package boilerless

import org.scalatest.FunSuite
import enumeratum._

class GenericTests extends FunSuite {
  
  def typed[T](x: T) = x
  
  
  test("Option") {
    
    @enum class Opt[+T] {
      class Som[T](value: T)
      object Non
    }
    
    typed[Opt[Nothing]](Opt.Non)
    typed[Opt[Int]](Opt.Non)
    typed[Opt[Int]](Opt.Som(42))
    
  }
  
  
  test("Either") {
    
    @enum class Either[+A,+B] {
      class Left[A](value: A)
      class Right[B](value: B)
    }
    import Either._
    
    typed[Either[Int,Nothing]](Left(42))
    typed[Either[Nothing,Int]](Right(42))
    typed[Either[Int,Int]](Left(42))
    typed[Either[Int,Int]](Right(42))
    
    typed[Either[Any,Any]](Right[Int](42))
    
  }
  
  
  test("EitherOrBoth") {
    
    @enum class EitherOrBoth[+A,+B] {
      //def fold[T](f: A => T, g: B => T): Either[T, (T,T)]
      class Left[A](value: A) //{ fold(f,g) = f(value) } // TODO 
      class Right[B](value: B)
      class Both[_](left: A, right: B)
    }
    import EitherOrBoth._
    
    
    
  }
  
  
  test("GADT") {
    
    @enum class Expr[+A] {
      //class AnyConst(value: A) extends ConstInt(value)
      //trait ConstInt(value: Int) extends Expr[Int]
      
      //class AnyConst2[_](value: A) extends Expr[A]
      
      class AnyConst[_](value: A)
      
      //class ConstInt(value: Int) extends Expr[Int]
      //class Abs[A,B](fun: A => B) extends Expr[A => B]
      //class App[A,B](fun: Expr[A => B], arg: A) extends Expr[A => B]
      
      // TODO
      class ConstInt(value: Int){$[Int]}
      class Abs[A,B](fun: A => B){$[A => B]}
      class App[A,B](fun: Expr[A => B], arg: A){$[B]}
      
      
    }
    import Expr._
    
    
    
  }
  
  
  test("Functions") {
    
    {
      @enum class Functions[-A,+B](val fun: A => B) {
        class IntFun(value: Int => Int) extends Functions[Int,Int](value)
        class IntFun2(value: Int => Int){ _[Int,Int](value) }
        object Inc{ _((x: Int) => x+1) }
        object Throws{ _((x: Any) => ???) }
        object Rejects{ _[Nothing,Any](x => x) }
      }
      import Functions._
      
      typed[Functions[Any, Nothing]](Throws)
      typed[Functions[Nothing, Any]](Rejects)
      
    }
    
    {
      @enum class ~>[-A,+B] {
        def fun: A => B
        
        // Cases:
        abstract class IntFun { _[Int,Int] }
        class Throws[A] { def fun = x => ??? }
        class Rejects[B] { def fun = x => ??? }
      }
      import ~>._
      
      typed[Int ~> Nothing]( Throws[Int]() )
      typed[Nothing ~> Int]( Rejects[Int]() )
      
    }
    
  }
  
  
  test("Inferred Existential Type Argument") {
    
    @enum class Enum[A] {
      @options('NotInterested) class Case0
      object Case1
    }
    typed[ Enum[_] ]{ Enum.Case0() }
    typed[ Enum[_] ]{ Enum.Case1 }
    
  }
  
  
  test("Propagated and Refined Bounds") {
    
    @enum class Enum[+S <: Seq[Any], -T >: List[Nothing]] {
      class A[S,T](s: S, t: T)
      class B[S, T, U /*>: T*/ <: S](u: U)
      class B2[_, U /*>: T*/ <: S](u: U)
      class C[S, T, U >: T /*<: S*/](u: U)
      class SC[_](s: S)
    }
    typed[ Enum[Seq[Any], List[Nothing]] ]{ Enum.B(collection.mutable.Seq(42)) }
    typed[ Enum[Seq[Any], List[Nothing]] ]{ Enum.C(Set(42)) }
    
    val e = typed[ Enum.SC[List[Int], List[Int]] ]{ Enum.SC(List(42)) }
    typed[ Enum.SC[Seq[Any], List[Nothing]] ]{ e }
    
    assertDoesNotCompile("Enum.B(Set(42))") // Error:(134, 10) inferred type arguments [scala.collection.immutable.Set[Int],List[Nothing],scala.collection.immutable.Set[Int]] do not conform to method apply's type parameter bounds [S <: Seq[Any],T >: List[Nothing],U <: S]
    
  }
  
  
  
}















