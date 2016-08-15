# Boilerless: Beautiful Syntax for Sealed Class Hierarchies


## Introduction

Boilerless is a small utility that lets you write class hierarchies
with a lightweight syntax closer to how you define data types in other functional languages.
It has special support for Generalized Algebraic Data Types (GADT) and enums-like hierarchies.

Following is a short example showing how to write an `EitherOrBoth` data type.
The precise rules used to expand it are explained further below.

```scala
@enum class EitherOrBoth[+A,+B] {
  def fold[T](f: A => T, g: B => T)(m: (T,T) => T): T
  
  // Cases:
  class First [A](value: A)     { fold(f,g)(m) = f(value)         }
  class Second[B](value: B)     { fold(f,g)(m) = g(value)         }
  class Both[_](fst: A, snd: B) { fold(f,g)(m) = m(f(fst),g(snd)) }
}
```

Boilerless is based on [macro annotations](http://docs.scala-lang.org/overviews/macros/annotations.html),
which will expand at compile time into proper Scala code.
The code above will generate the equivalent of:

```scala
sealed abstract class EitherOrBoth[+A, +B] {
  def fold[T](f: A => T, g: B => T)(m: (T,T) => T): T
}
object EitherOrBoth {
  // Cases:
  case class First[+A](value: A) extends EitherOrBoth[A, Nothing] {
    private[this] type B = Nothing
    override def fold[T](f: A => T, g: B => T)(m: (T,T) => T): T = f(value)
  }
  case class Second[+B](value: B) extends EitherOrBoth[Nothing, B] {
    private[this] type A = Nothing
    override def fold[T](f: A => T, g: B => T)(m: (T,T) => T): T = g(value)
  }
  case class Both[+A, +B](fst: A, snd: B) extends EitherOrBoth[A, B] {
    override def fold[T](f: A => T, g: B => T)(m: (T,T) => T): T = m(f(fst),g(snd))
  }
}
```

**Note:** Macro annotations are not _officially_ supported in Scala.
Syntax highlighting may be broken in some IDE's.
However, Boilerless offers [alternatives](#ide-integration-and-file-generation-approach) to circumvent these problems.




## Functionalities


### Enumerations

<!-- If the parameters to be passed to the
As seen above, 
-->
Type and term parameters can be passed from case classes to the parent class implicitly
using the lightweight `_[..types](...args)` syntax inside the body of the case class.
Moreover, if that is the very first expression in the body and there are no types to pass,
the `_` can be ommitted. Therefore, one can write:
<!-- if that is the only expression  -->

```scala
@enum class State(entryName: String) {
   object Alabama {"AL"}
   object Alaska  {"AK"}
   
   object California { _("CA") }  // explicit initialization syntax
   
   // and so on and so forth.
}
```

Boilerless has special support for [enumeratum](http://github.com/lloydmeta/enumeratum).
By only changing `@enum` to `@enumeratum` in the code above,
the parent class is made to extend `enumeratum.EnumEntry`,
the case classes to extend `enumeratum.Enum[State]`,
and a `val values = findValues` field is added to the companion object:

```scala
@enumeratum class State(entryName: String) {
   object Alabama {"AL"}
   object Alaska {"AK"}
   // and so on and so forth.
}
assert(State.withName("AL") == State.Alabama)
```

You can see the code generated by the definitions above
[here](core/src/test/scala/boilerless/EnumeratumTests.scala#L9).



### Type Parameters Forwarding

As shown in the `EitherOrBoth` example above,
if there is no explicit `extends Parent[..](...)` clause nor `_[..](...)` initialization call, 
type parameters named the same as type parameters of the parent class are forwarded automatically.
Bounds and variance annotations for these parameters do not need to be repeated,
as they are copied from the parent class.
Parent type parameters not mentioned in the case class are passed to the parent class
as the lower bound if the parameter is covariant, the upper bound if it is contravariant,
and an existential otherwise.

One can also import all parent parameters with syntax `[_, ..]`,
i.e., first parameter named underscore `_`, possibly followed by more parameters.

Additionally, a private type is created in each case class for all parent type parameters
it does not mention, so that it can refer to it nonetheless
(see `EitherOrBoth` in cases `First` and `Second`).



### Nested Hierarchies

Nested hierarchies are naturally supported,
as macro annotations expand from the outermost to the innermost definition.
The following:

```scala
@enum class Level0(x: Int) {
  class Sub0(){0}
  class Sub1(x: Int){x}
  @enum class Level1(x: Int) { _(x)
    class SubSub0{1}
    class SubSub1(y: Int){y}
  }
}
```
... generates:
```scala
sealed abstract class Level0(x: Int)
object Level0 {
  case class Sub0() extends Level0(0)
  case class Sub1(x: Int) extends Level0(x)
  @enum case class Level1(x: Int) extends Level0(x) {
    class SubSub0 {1}
    class SubSub1(y: Int) {y}
  }
}
```
... which in turn generates:

```scala
sealed abstract class Level0(x: Int)
object Level0 {
  case class Sub0() extends Level0(0)
  case class Sub1(x: Int) extends Level0(x)
  sealed abstract class Level1(x: Int) extends Level0(x)
  object Level1 {
    case class SubSub0() extends Level1(1)
    case class SubSub1(y: Int) extends Level1(y)
  }
}
```


## Using Boilerless

Boilerless has only been made to work on Scala 2.11 yet.
More work is needed to port it to other versions.

To use Boilerless, you need to clone the repo and publish Boilerless locally with `sbt boilerless/publishLocal `.

Then, in your own project, enable the macro-paradise plugin and add the library dependency:

```scala
resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

libraryDependencies += "com.github.lptk" %% "boilerless" % boilerlessVersion
```

See [this example project](https://github.com/LPTK/Boilerless-Example).


## IDE Integration and File-Generation Approach

Some IDE's like Eclipse seem to support Boilerless remarkably well
– most type errors point to the right thing, and jump-to-definition is often approximately right.

Other IDE's like IntelliJ do not even try to understand macros.
To mitigate some of the IDE problems, you can make the companion object of the `@enum` class extend the class,
so the IDE will at least see the case classes.

Boilerless also provides an `@enumInFile(fileName, package)` macro that,
instead of expanding into the class trees, will write the result to a new Scala file [1].
The new file will be placed in `$folderName/ClassName.scala`, its package will be `$package`,
and imports found at macro call site will be placed at the top.

For example see [this tests file](macros/src/test/scala/boilerless/Templates.scala), which contains:

```scala
@enumInFile("core/src/test/scala/boilerless/gen", "boilerless.gen")
class Opt[+T] {  class Som[T](value: T);  object Non  }
```

The generated code can be found [here](core/src/test/scala/boilerless/gen/Opt.scala).


Arguments `folderName` and `package` should be string literals.
It is advised to set `folderName` to a folder belonging to a subproject
that depends on the project containing the `@enum` class, and _not the same project_.
This way, whenever you change the `@enum` class, it will re-expand first,
writing the result in the file located in the dependent project,
and that file will then be compiled as part of the dependent project.
<!-- If it was in the same project, you would have to compile twice. -->

**Note:** You _may_ still have to compile twice,
unless you use a special configuration or command
to explicitly ask `sbt` to compile the project containing the templates first,
like `sbt templates-project/compile main-project/run`.

If you do not want to use macro annotations,
a [def macro](http://docs.scala-lang.org/overviews/macros/overview.html)
version is also available as `genEnum(folderName, package)(){""" code """}`.

This functionality has only been tested with sbt `0.13.8` and Scala `2.11.8`.
It is known not to work in IntelliJ
(but a mere warning will be raised and the macro failure will not stop compilation).

[1] Something macros are not _supposed_ to do, but is very useful.



### Summary

Here is a summary of Boilerless' functionalities:

 - Make outer class `sealed abstract` and remove potential `final` and `case` modifiers.
 
 - Make inner classes and objects `final case` and move them to the companion object. 
 
 - Make inner classes extend outer class implicitly.
 
 - Forward type parameters with their bounds and variance if none are specified explicitly.
 
 - Pass type and term parameters to the parent class if specified with the `_[..](...)` syntax.
 
 - Create private aliases to the arguments passed for the outer class' type parameters,
 if they are not also inner class parameters.
 
 - Convert expressions of the form `f(...args) = body` found in inner class bodies
 to definitions of the corresponding abstract methods or values found in the outer class.




## Custom Options

Several options can be passed to `@enum` in order to customize its behavior.

 - `'Unseal` prevents making the `@enum` class sealed. 
 
 - `'NotInterested` removes warning like _"this class could be an object!"_.
  
 - `'Debug` enables debugging output,
 and allows to see what is generated by the macro expansion.

For example: `@enum('Unseal, 'Debug) class Enum { ... }`.

Annotate with `@ignore` a member definition to leave it untouched by Boilerless.

In addition, arbitrary classes, methods and objects (even outside of `@enum` hierarchies) 
may be modified after the fact with: 
 
 - `@notCase` to cancel a `case` modifier

 - `@open` to cancel a `final` or `sealed` modifier

 - `@concrete` to cancel an `abstract` modifier




## Known Limitations

### Syntax-Driven

Boilerless is completely syntax-driven,
as it operate before type-checking and name resolution.
As a consequence, if you extend the _parent class_ explicitly with `extends Base[..](...)`,
it is important to do so with the bare parent name (so Boilerless can detect it),
and not something like `extends my.package.Base[..](...)`.
<!-- As a consequence, it is important to use
`@enum` for nested enums (and not `@boilerless.enum`, for example).-->


### IDE support

Some IDE's like IntelliJ will likely not understand Boilerless' syntax and semantics,
so it may be good to turn inspections off for the specific definition files.
See also [this](#ide-integration-and-file-generation-approach) to circumvent the problem.



