package boilerless

import scala.annotation.tailrec
import scala.reflect.macros.whitebox

 /*
  * TODO prevent case traits -- convert to bare classes?
  * 
  * TODO add `override` to implicitly overriding vals?
  * 
  */
class Macros(val c: whitebox.Context) {
  import c.universe._
  import Flag._
  
  case class Params(debug: Boolean, unsealed: Boolean, notFinal: Boolean, interestedInWarnings: Boolean)
  val DefaultParams = Params(false, false, false, true)
  
  def getParams(args: Seq[Tree], scope: scala.Symbol) = {
    def globScp = scope == 'Global
    def caseScp = scope == 'Case
    (DefaultParams /: args) {
      case (ps, q"'Debug") if globScp => ps.copy(debug = true)
      case (ps, q"'Unsealed") if globScp => ps.copy(unsealed = true)
      case (ps, q"'NotInterested") => ps.copy(interestedInWarnings = false)
      case (ps, q"'NotFinal") if caseScp => ps.copy(notFinal = true)
      case (ps, a) => c.abort(a.pos, "Unrecognized parameter: "+showCode(a))
    }
  }
  val params = getParams(c.macroApplication match {
    case q"new enum(...$args).macroTransform(..$_)" => args.flatten
    case q"new enumeratum(...$args).macroTransform(..$_)" => args.flatten
    case q"new enumInFile($folder, $pckg)(...$args).macroTransform(..$_)" => args.flatten
    case q"$_.enumInFile($_, $_, $_)(..$args)($_)" => args
    case app => /*c.warning(app.pos, "Could not find parameters for this macro application.");*/ Nil
  }, 'Global)
  
  def debug(x: => Any, xs: Any*) { if (params.debug) println(x +: xs mkString " ") }
  
  
  def rmFlagsIn(from: FlagSet, rm: FlagSet): FlagSet = (from.asInstanceOf[Long] & ~rm.asInstanceOf[Long]).asInstanceOf[FlagSet]
  def mkPublic(fs: FlagSet): FlagSet = rmFlagsIn(fs, PRIVATE | PROTECTED | LOCAL)
  def mapFlags(m: Modifiers)(f: FlagSet => FlagSet) = Modifiers(f(m.flags), m.privateWithin, m.annotations)
  
  def isIgnore: Tree => Boolean = { case q"new ignore()" => true  case _ => false }
  
  
  def impl(annottees: Tree*): Tree = {
    
    val result = {
      annottees match {
          
        case (md: ModuleDef) :: Nil => c.abort(md.pos, "Expected a class definition here.")
          
        case ClassDef(mods, name, typs, temp) :: rest => // TODO handle rest
          
          /*// TODO: use?
          val paramss = temp.body flatMap {
            case DefDef(mods, termNames.CONSTRUCTOR, tparams, vparamss, tpt, rhs) => Some(v)
            case _ => None
          } match {
            case Nil => Nil
            case argss :: Nil => argss
            case multi => c.warning(temp.pos, s"Several constructors detected for $name. Picking the first one (YOLO)."); multi.head
          }*/
          
          def rmIgnore(m: Modifiers) = Modifiers(m.flags, m.privateWithin, m.annotations.indexWhere(isIgnore) match {
            case -1 => m.annotations
            case i => m.annotations.patch(i, Nil, 1) // leaves further @ignore annotations, possibly used later
          })
          
          val (otherDefs, movedDefs) = (temp.body map {
            case ModuleDef(mods0, name0, temp0) =>
              if (mods0.annotations.exists(isIgnore))
                   Left(ModuleDef(rmIgnore(mods0), name0, temp0))
              else Right(mods0, name0.toTypeName, None, temp0)
            case ClassDef(mods0, name0, typs0, temp0) =>
              if (mods0.annotations.exists(isIgnore))
                   Left(ClassDef(rmIgnore(mods0), name0, typs0, temp0))
              else Right(mods0, name0, Some(typs0), temp0)
            case d => Left(d)
          }).partition(_.isLeft) match { case(as,bs) => as.map(_.fold(identity,_ => ???)) -> bs.map(_.fold(_ => ???,identity)) }
          
          
          val extractedClasses = movedDefs map {
            case (mods0, name0, typs0, tmp @ Template(parents, self, body)) =>
              
              val caseParams = mods0.annotations collectFirst {
                case q"new options(...$args)" => getParams(args.flatten, 'Case)
              } getOrElse DefaultParams
              
              // Warn if the class could be an object:
              if (params.interestedInWarnings && caseParams.interestedInWarnings && !mods0.hasFlag(ABSTRACT)) typs0 foreach { ts =>
                if (ts.isEmpty) {
                  val clsParams = body collect { case d: MemberDef if d.mods.hasFlag(PARAMACCESSOR) => d }
                  if (clsParams.isEmpty) c.warning(tmp.pos, "Parameter-less class could be an object.")
                }
              }
              
              val isObject = typs0.isEmpty
              
              //debug(parents map (p => showRaw(p)))
              
              var newParents = parents.reverse flatMap {
                case tq"scala.AnyRef" | tq"scala.Product" | tq"scala.Serializable" => None
                case p => Some(p)
              }
              newParents ::= tq"scala.Serializable"
              newParents ::= tq"scala.Product"
              
              newParents = newParents.reverse // puts potential extended class back to first position
              
              val explicitExtends = newParents collectFirst {
                case Ident(`name`) =>
                case q"${`name`}(...$argss)" =>
                case q"${AppliedTypeTree(Ident(`name`), targs)}(...$argss)" =>
                case AppliedTypeTree(Ident(`name`), targs) =>
              } isDefined;
              
              val (typs1, newBody) = if (explicitExtends) {
                val r = typs0 getOrElse Nil
                r foreach {
                  case td @ TypeDef(_, TypeName("_"), Nil, rhs) =>
                    c.warning(td.pos, "Type parameter forwarding is not active if the parent class is extended explicitly.")
                  case _ =>
                }
                r -> body
              } else {
                
                val ((initTargs, initArgss), newBody) = {
                  @tailrec def rec(initExpr: Option[Tree], acc: List[Tree])(defs: List[Tree]): (Option[Tree], List[Tree]) = defs match {
                    case (d: DefTree) :: xs if d.name == termNames.CONSTRUCTOR => rec(initExpr, d :: acc)(xs)
                    case (d: ValDef) :: xs if d.mods.hasFlag(PARAMACCESSOR) => rec(initExpr, d :: acc)(xs)
                    case (d: MemberDef) :: xs if d.mods.hasFlag(SYNTHETIC) => rec(initExpr, d :: acc)(xs)
                    case (e @ q"super.${termNames.CONSTRUCTOR}[..$_](...$_)") :: xs => rec(initExpr, e :: acc)(xs)
                    case e :: xs if e isTerm  => rec(Some(e), acc)(xs)
                    //case d :: rest => rec(acc)(rest, canBeArgss)
                    case rest => 
                      //println("RETURN",initExpr,acc.reverse,rest)
                      //println("RETURN",rest.headOption,rest.headOption.map(_.getClass))//.asInstanceOf[MemberDef].mods)
                      (initExpr, acc.reverse ++ rest)
                  }
                  val (init, newBody) = rec(None, Nil)(body)
                  
                  //debug(init, newBody)
                  
                  (init match {
                    case Some(q"__") => None -> Nil
                    case Some(q"_[..$ts](...$as)") => Some(ts) -> as
                    case Some(q"$$[..$ts](...$as)") => Some(ts) -> as
                    case Some(e) => None -> ((e :: Nil) :: Nil)
                    case _ => None -> Nil
                  }) -> newBody
                }
                
                val (typs1, targs) = {
                  
                  val ts = typs0 match {
                    case Some(TypeDef(_, TypeName("_"), Nil, rhs) :: rest) => typs ::: rest
                    case Some(ts) => ts
                    case None => Nil
                  }
                  
                  val named = ts.map (_ name) toSet;
                  
                  val (vars, targs) = (typs map {
                    case TypeDef(m, n, tp, rhs) =>
                      if (tp.nonEmpty) c.warning(tp.head.pos, "Higher kinded type support has not been tested.")
        
                      val variance = if (m.hasFlag(COVARIANT)) Some(true)
                      else if (m.hasFlag(CONTRAVARIANT)) Some(false) else None
        
                      val (lo,hi) = rhs match {
                        case TypeBoundsTree(lo, hi) => (if (lo.isEmpty) None else Some(lo), if (hi.isEmpty) None else Some(hi))
                        case _ => (None, None)
                      }
        
                      (n -> (lo,hi)) -> (
                        if (named(n)) tq"$n"
                        else (lo,hi,variance) match {
                          case (Some(lo), _, Some(true)) => lo
                          case (None, _, Some(true)) => TypeTree(typeOf[Nothing])
                          case (_, Some(hi), Some(false)) => hi
                          case (_, None, Some(false)) => TypeTree(typeOf[Any])
                          case (loo, hio, None) =>
                            // Note: `TypeTree(WildcardType)` crashes the compiler
                            TypeBoundsTree(loo getOrElse tq"", hio getOrElse tq"") // Seems to be handled okay...
                        })
                    
                  }).unzip match {
                    case (vars, _) if initTargs.isDefined => vars.toMap -> initTargs.get
                    case (vars, targs) => vars.toMap -> targs
                  }
                  
                  val typs1 = ts map {
                    case td @ TypeDef(m, n, tp, rhs) =>
                      vars get n map {
                        case (lo,hi) => TypeDef(m, n, tp, TypeBoundsTree(lo getOrElse EmptyTree, hi getOrElse EmptyTree))
                      } getOrElse td
                  }
                  typs1 -> targs
                }
                
                newParents ::= q"${tq"$name[..$targs]"}(...$initArgss)"
                
                (typs1, newBody)
              }
              
              val temp1 = Template(newParents, self, newBody.map {
                case ValDef(m, n, t, v) if m.hasFlag(PARAMACCESSOR) =>
                  ValDef(Modifiers(mkPublic(m.flags) | CASEACCESSOR, m.privateWithin, m.annotations), n, t, v)
                case d => d
              })
              
              //val isEnum = mods0.annotations collectFirst { case q"new enum(...$args)" => args }
              
              val mods1 = Modifiers(
                mods0.flags
                  | CASE
                  | (if (caseParams.notFinal) NoFlags else FINAL),
                mods0.privateWithin, mods0.annotations)
              
             if (isObject) ModuleDef(mods1, name0.toTermName, temp1)
             else          ClassDef (mods1, name0,  typs1,    temp1)
          }
          
          val obj = rest match {
            case ModuleDef(mods0, name0, Template(parents, self, body)) :: Nil =>
              ModuleDef(mods0, name0, Template(parents, self, extractedClasses ++ body))
            case Nil => q"object ${name.toTermName} { ..${extractedClasses} }"
          }
          
          val newMods = Modifiers(
            rmFlagsIn(
              mods.flags
                | (if (params.unsealed) NoFlags else SEALED)
                | ABSTRACT, 
              CASE | FINAL), mods.privateWithin, mods.annotations)
          
          val cls = ClassDef(newMods, name, typs, Template(temp.parents, temp.self, otherDefs))
          
          q"$cls; $obj"
      }
    }
    
    debug("Generated:",showCode(result))
    result
  }
  
  
  def enumeratumImpl(annottees: Tree*): Tree = {
    import c.universe._
    import Flag._
    
    val q"${ClassDef(mods0, name0, typs0, temp0)}; ${ModuleDef(mods1, name1, temp1)}" = impl(annottees: _*)
    
    def rmAnyRef(ps: List[Tree]) = ps filter {case tq"scala.AnyRef" => false  case _ => true}
    
    val newBody = temp0.body map {
      case ValDef(m,TermName("entryName"),t,v) =>
        ValDef(Modifiers(rmFlagsIn(m.flags | OVERRIDE, PRIVATE), m.privateWithin, m.annotations),TermName("entryName"),t,v)
      case x => x
    }
    
    val r = q"""
    ${ClassDef(mods0, name0, typs0, Template(tq"_root_.enumeratum.EnumEntry" :: rmAnyRef(temp0.parents), temp0.self, newBody))}
    ${ModuleDef(mods1, name1, Template(tq"_root_.enumeratum.Enum[$name0]" :: rmAnyRef(temp1.parents), temp1.self,
      q"val values = findValues" :: temp1.body))}
    """
    debug("Finally Generated:",showCode(r))
    //debug("Finally Generated: "+(r))
    r
  }
  
  
  def genInFile(className: String, folderName: String, packageName: String)(args: Tree*)(annottees: List[Tree]): Tree = {
    
    val fileName = s"$folderName/$className.scala"
    
    val imports = c.enclosingPackage collect { // FIXME only collect in outer scopes
      case imp: Import => imp
    }
    
    val tree = impl(annottees: _*)
    
    val file = new java.io.File(fileName)
    
    debug(s"Writing to file: $file")
    
    try {
      val p = new java.io.PrintWriter(file)
      
      try p.println {
        val str = tree match {
          case q"$cls; $obj" =>
s"""/* Generated automatically with Boilerless. Changes made directly to this file will be lost. */

package $packageName
${imports map (i => "\n"+showCode(i)) mkString}

${showCode(cls)}

${showCode(obj)}
"""
        }
        debug(s"Writing string:\n$str")
        str
        
      } finally p.close()
      
    } catch {
      case e: java.io.IOException =>
        c.warning(c.enclosingPosition, s"Could not write to file $fileName: "+e)
    }
    
    // Q: actually write forwarders to the created file? --> no, as it may be in a different project
    q"class ${TypeName(className)}; object ${TermName(className)}"
  }
  
  
  def implInFile(annottees: Tree*): Tree = {
    import c.universe._
    
    val clsName = annottees match {
      case ClassDef(_, name, _, _) :: rest => name
    }
    
    c.macroApplication match {
      case q"new enumInFile($folder, $pckg)(...$args).macroTransform(..$_)" =>
        (folder,pckg) match {
          case (Literal(Constant(folderName: String)), Literal(Constant(packageName: String))) =>
            genInFile(clsName.toString, folderName, packageName)(args.flatten: _*)(annottees.toList)
          case _ => c.abort(folder.pos, "Strings passed to `enumInFile` should be literals.")
        }
    }
    
  }
  
  
  def defEnumInFileImpl(className: Tree, folderName: Tree, packageName: Tree)(args: Tree*)(code: Tree) = {
    (className, folderName, packageName, code) match {
      case (LitString(className), LitString(folderName), LitString(packageName), LitString(code)) =>
        c.parse(code) match {
          case q"${cls: ClassDef}"                    => genInFile(className, folderName, packageName)(args: _*)(cls :: Nil)
          case q"${cls: ClassDef}; ${mod: ModuleDef}" => genInFile(className, folderName, packageName)(args: _*)(cls :: mod :: Nil)
          case q"${mod: ModuleDef}"                   => genInFile(className, folderName, packageName)(args: _*)(mod :: Nil)
          case _ => ??? // TODO B/E
        }
    }
  }
  
  
  def annotIntoParam(annottees: Tree*) = {
    c.macroApplication match {
        
      case q"new open().macroTransform(..$_)" =>
        def mk(m: Modifiers) = mapFlags(m)(rmFlagsIn(_, FINAL | SEALED))
        q"${annottees.head match {
          case ClassDef(mods, name, tparams, impl) => ClassDef(mk(mods), name, tparams, impl)
          case ValDef(m, n, t, v) => ValDef(mk(m), n, t, v)
          case DefDef(m, n, tp, ass, t, v) => DefDef(mk(m), n, tp, ass, t, v)
          case x => c.abort(x.pos, "Cannot make this not final: "+showCode(x))
        }}; ..${annottees.tail}"
        
      case q"new concrete().macroTransform(..$_)" =>
        def mk(m: Modifiers) = mapFlags(m)(rmFlagsIn(_, ABSTRACT))
        q"${annottees.head match {
          case ClassDef(mods, name, tparams, impl) => ClassDef(mk(mods), name, tparams, impl)
          case ValDef(m, n, t, v) => ValDef(mk(m), n, t, v)
          case DefDef(m, n, tp, ass, t, v) => DefDef(mk(m), n, tp, ass, t, v)
          case x => c.abort(x.pos, "Cannot make this concrete (not abstract): "+showCode(x))
        }}; ..${annottees.tail}"
        
      case q"new notCase().macroTransform(..$_)" =>
        q"${annottees.head match {
          case ClassDef(mods, name, tparams, impl) => ClassDef(mapFlags(mods)(rmFlagsIn(_, CASE)), name, tparams, impl)
          case x => c.abort(x.pos, "Cannot make this not a case definition: "+showCode(x))
        }}; ..${annottees.tail}"
        
    }
  }
  
  
  
  
  object LitString {
    def unapply(x: Tree) = x match {
      case Literal(Constant(str: String)) => Some(str)
      case _ => c.abort(x.pos, "Required String literal, found: "+showCode(x))
    }
  }
  
  
}























