package boilerless

import scala.annotation.tailrec
import scala.reflect.macros.whitebox

 /*
  * TODO prevent case traits -- convert to bare classes?
  * 
  * TODO add `override` to implicitly overriding vals?
  * 
  * Note about the init syntax: neither `this[..](...)` nor `super[..](...)` are valid syntax.
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
          
        case ClassDef(baseMods, baseName, baseTParams, baseImpl) :: rest =>
          
          /*// TODO: use?
          val paramss = baseImpl.body flatMap {
            case DefDef(baseMods, termNames.CONSTRUCTOR, tparams, vparamss, tpt, rhs) => Some(v)
            case _ => None
          } match {
            case Nil => Nil
            case argss :: Nil => argss
            case multi => c.warning(baseImpl.pos, s"Several constructors detected for $baseName. Picking the first one (YOLO)."); multi.head
          }*/
          
          // Maps abstract definition names to the definitions themselves or None if they are overloaded
          val absDefs = baseImpl.body flatMap {
            case d @ DefDef(m, name, tparams, vparamss, tpt, rhs) if (m hasFlag ABSTRACT) || rhs.isEmpty => Some(name -> d)
            case d @ ValDef(m, name, tpt, rhs) if (m hasFlag ABSTRACT) || rhs.isEmpty =>  Some(name -> d)
            case _ => None
          } groupBy (_._1) map {
            case (name, (_, d) :: Nil) => name -> Some(d)
            case (name, _) => name -> None
          }
          //debug("Found abstract defs: "+absDefs)
          
          
          def rmIgnore(m: Modifiers) = Modifiers(m.flags, m.privateWithin, m.annotations.indexWhere(isIgnore) match {
            case -1 => m.annotations
            case i => m.annotations.patch(i, Nil, 1) // leaves further @ignore annotations, possibly used later (who knows?)
          })
          
          val (otherDefs, movedDefs) = (baseImpl.body map {
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
            case (mods, name, tparams, impl @ Template(parents, self, body)) =>
              
              val caseParams = mods.annotations collectFirst {
                case q"new options(...$args)" => getParams(args.flatten, 'Case)
              } getOrElse DefaultParams
              
              val doWarn  = params.interestedInWarnings && caseParams.interestedInWarnings
              
              // Warn if the class could be an object:
              if (doWarn && !mods.hasFlag(ABSTRACT)) tparams foreach { ts =>
                if (ts.isEmpty) {
                  val clsParams = body collect { case d: MemberDef if d.mods.hasFlag(PARAMACCESSOR) => d }
                  if (clsParams.isEmpty) c.warning(impl.pos, "Parameter-less class could be an object.")
                }
              }
              
              val isObject = tparams.isEmpty
              
              //debug(parents map (p => showRaw(p)))
              
              var newParents = parents.reverse flatMap {
                case tq"scala.AnyRef" | tq"scala.Product" | tq"scala.Serializable" => None
                case p => Some(p)
              }
              newParents ::= tq"scala.Serializable"
              newParents ::= tq"scala.Product"
              
              newParents = newParents.reverse // puts potential extended _class_ back to first position
              
              val explicitExtends = newParents collectFirst {
                case Ident(`baseName`) =>
                case q"${`baseName`}(...$argss)" =>
                case q"${AppliedTypeTree(Ident(`baseName`), targs)}(...$argss)" =>
                case AppliedTypeTree(Ident(`baseName`), targs) =>
              } isDefined;
              
              val expandedTParams = tparams match {
                case Some(TypeDef(_, TypeName("_"), Nil, rhs) :: rest) => baseTParams ::: rest
                case Some(ts) => ts
                case None => Nil
              }
              val expandedTParamName = expandedTParams.map (_ name) toSet;
              
              val (newTParams, newBody) = if (explicitExtends) {
                val r = tparams getOrElse Nil
                r foreach {
                  case td @ TypeDef(_, TypeName("_"), Nil, rhs) =>
                    c.warning(td.pos, "Type parameter forwarding is not active if the parent class is extended explicitly.")
                  case _ =>
                }
                r -> body
              } else {
                
                val ((explicitTArgs, initArgss), newBody) = {
                  @tailrec def rec(acc: List[Tree])(defs: List[Tree]): (Option[Tree], List[Tree]) = defs match {
                    case (d: DefTree) :: xs if d.name == termNames.CONSTRUCTOR => rec(d :: acc)(xs)
                    case (d: ValDef) :: xs if d.mods.hasFlag(PARAMACCESSOR) => rec(d :: acc)(xs)
                    case (d: MemberDef) :: xs if d.mods.hasFlag(SYNTHETIC) => rec(d :: acc)(xs)
                    case (e @ q"super.${termNames.CONSTRUCTOR}[..$_](...$_)") :: xs => rec(e :: acc)(xs)
                    case e :: xs if (e isTerm) && (e match { case q"$_ = $_" => false  case _ => true }) =>  // We don't want to misinterpret lightweight def implems
                      (Some(e), acc.reverse ++ xs)
                    case xs => (None, acc.reverse ++ xs)
                  }
                  val (init, newBody) = rec(Nil)(body)
                  
                  //debug(init, newBody)
                  
                  (init match {
                    case Some(q"__") => None -> Nil
                    case Some(q"_[..$ts](...$as)") => Some(ts) -> as
                    case Some(q"$$") => None -> Nil
                    case Some(q"$$[..$ts](...$as)") => Some(ts) -> as
                    case Some(e) => None -> ((e :: Nil) :: Nil)
                    case _ => None -> Nil
                  }) -> newBody
                }
                
                val (modTParams, initTArgs) = {
                  
                  val (variance_bounds, inferredTArgs) = (baseTParams map {
                    case TypeDef(m, n, tp, rhs) =>
                      if (tp.nonEmpty) c.warning(tp.head.pos, "Higher kinded type support has not been tested.")
        
                      val variance = if (m.hasFlag(COVARIANT)) Some(true)
                      else if (m.hasFlag(CONTRAVARIANT)) Some(false) else None
        
                      val (lo,hi) = rhs match {
                        case TypeBoundsTree(lo, hi) => (if (lo.isEmpty) None else Some(lo), if (hi.isEmpty) None else Some(hi))
                        case _ => (None, None)
                      }
        
                      (n -> (variance, lo, hi)) -> (
                        if (expandedTParamName(n)) tq"$n"
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
                    case (vars, _) if explicitTArgs.isDefined => vars.toMap -> explicitTArgs.get
                    case (vars, targs) => vars.toMap -> targs
                  }
                  
                  val typs = expandedTParams map {
                    case td @ TypeDef(m, n, tp, rhs) =>
                      variance_bounds get n map {
                        case (v,lo,hi) => TypeDef(mapFlags(m)(_ | (v match {
                          case Some(true) => COVARIANT
                          case Some(false) => CONTRAVARIANT
                          case None => NoFlags
                        })), n, tp, TypeBoundsTree(lo getOrElse EmptyTree, hi getOrElse EmptyTree))
                      } getOrElse td
                  }
                  typs -> inferredTArgs
                }
                
                // TODO make sure no shadowing
                // Type parameters of the parent that are not type parameters of the case are defined using a type alias
                val substituteTypes = (baseTParams zip initTArgs) flatMap {
                  case (td, _) if expandedTParamName(td.name) => None
                  case (TypeDef(m, n, tp, _), rhs) => Some(
                    /*if (m.hasFlag(CONTRAVARIANT))  q"private[this] type $n >: $rhs"
                    else if (m.hasFlag(COVARIANT)) q"private[this] type $n <: $rhs"
                    else*/      // ^ generates: Error: abstract member may not have private modifier;  does not make much sense anyway...
                    q"private[this] type $n =  $rhs" )
                }
                
                newParents ::= q"${tq"$baseName[..$initTArgs]"}(...$initArgss)"
                
                (modTParams, newBody ++ substituteTypes)  // Note: `substituteTypes ++ newBody` crashes the compiler as it expects some definitions to be up top
              }
              
              def mkProperDef(originalTree: Tree, defName: TermName, defParamss: List[List[Tree]], defTyp: Option[Tree], body: Tree) = {
                debug("Searching for abstract def",defName)
                absDefs get defName map {
                  case None => c.warning(originalTree.pos, s"Impossible to define overloaded abstract definition $defName."); originalTree
                  case Some(d) => val (tp,vp,rt,isVal) = d match { case DefDef(m,n,tp,vp,rt,_) => (tp,vp,rt,false)  case ValDef(m,n,rt,_) => (Nil,Nil,rt,true) }
                    // TODO handle TypedTree's  --  eg: f(x:Int): Int = x+1  -- ?
                    val dp = defParamss map (_ map { case Ident(n: TermName) => n  case tr => c.abort(tr.pos, "Unexpected parameter shape: `"+showCode(tr)+s"` in $defName.") })
                    if (vp.size != dp.size) c.abort(originalTree.pos, s"Parameter list number mismatch in  in $defName")
                    vp zip dp foreach {
                      case (vp, dp) =>
                        if (vp.size != dp.size) c.abort(originalTree.pos, s"Parameter number mismatch: (${dp mkString ","}) should have ${vp.size} parameter(s) in $defName.")
                        vp zip dp foreach { case (v, d) => if (v.name != d) c.abort(originalTree.pos, s"Argument name mismatch: '$d' should reflect '${v.name}' in $defName.") }
                    }
                    if (isVal) q"override val $defName: $rt = $body"
                    else q"override def $defName[..$tp](...$vp): $rt = $body"
                } getOrElse {
                  if (doWarn) c.warning(originalTree.pos, s"Found what looks like a lightweight implementation syntax: `$defName(...) = ${showCode(body)}`," + //`${showCode(originalTree)}`," +
                    s"but it correspond to no known method '$defName' in the parent class.")
                  originalTree
                }
              }
              
              val newImpl = Template(newParents, self, newBody.map {
                case ValDef(m, n, t, v) if m.hasFlag(PARAMACCESSOR) =>
                  ValDef(Modifiers(mkPublic(m.flags) | CASEACCESSOR, m.privateWithin, m.annotations), n, t, v)
                  
                case tr @ q"$rec = $body" =>
                  rec match {
                    case q"${n: TermName}(...$args)" => mkProperDef(tr, n, args, None, body)
                    case _ => c.abort(rec.pos, "Invalid lightweight implementation syntax. Expected `name(...args) = body`.")
                  }
                  
                case d => d
              })
              
              val newMods = Modifiers(
                mods.flags
                  | CASE
                  | (if (caseParams.notFinal) NoFlags else FINAL),
                mods.privateWithin, mods.annotations)
              
             if (isObject) ModuleDef(newMods, name.toTermName,  newImpl)
             else          ClassDef (newMods, name, newTParams, newImpl)
          }
          
          
          val obj = rest match {
            case ModuleDef(mods0, name0, Template(parents, self, body)) :: Nil =>
              ModuleDef(mods0, name0, Template(parents, self, extractedClasses ++ body))
            case Nil => q"object ${baseName.toTermName} { ..${extractedClasses} }"
          }
          
          val newMods = Modifiers(
            rmFlagsIn(
              baseMods.flags
                | (if (params.unsealed) NoFlags else SEALED)
                | ABSTRACT, 
              CASE | FINAL), baseMods.privateWithin, baseMods.annotations)
          
          val cls = ClassDef(newMods, baseName, baseTParams, Template(baseImpl.parents, baseImpl.self, otherDefs))
          
          q"$cls; $obj"
      }
    }
    
    //debug("Generated:",(result))
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























