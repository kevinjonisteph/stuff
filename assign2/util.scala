package cs162.miniJS.util

import java.io._
import scala.io._
import cs162.miniJS.syntax._

// catamorphism over AST
object Fold {
  
  def createFold[A](
    thenA   : Seq[A] ⇒ A            ,
    assignA : (String, A) ⇒ A       ,
    whileA  : (A, A) ⇒ A            ,
    outputA : A ⇒ A                 ,
    numA    : BigInt ⇒ A            ,
    boolA   : Boolean ⇒ A           ,
    strA    : String ⇒ A            ,
    undefA  : () ⇒ A                ,
    varA    : String ⇒ A            ,
    unopA   : (Uop, A) ⇒ A          ,
    binopA  : (Bop, A, A) ⇒ A       ,
    ifA     : (A, A, A) ⇒ A         ,
    inA     : InputType ⇒ A         ,
    letA    : (Seq[String], A) ⇒ A
  ) = {
    def fold( t:Term ): A = t match {
      case Then( ts ) ⇒ thenA( ts map fold )
      case Assign( Var(x), e ) ⇒ assignA( x, fold(e) )
      case While( e, t ) ⇒ whileA( fold(e), fold(t) )
      case Output( e, l ) ⇒ outputA( fold(e) )
      case Num( n ) ⇒ numA( n )
      case Bool( b ) ⇒ boolA( b )
      case Str( s ) ⇒ strA( s )
      case Undef() ⇒ undefA()
      case Var( x ) ⇒ varA( x )
      case UnOp( op, e ) ⇒ unopA( op, fold(e) )
      case BinOp( op, e1, e2 ) ⇒ binopA( op, fold(e1), fold(e2) )
      case If( e, t1, t2 ) ⇒ ifA( fold(e), fold(t1), fold(t2) )
      case In( typ, l ) ⇒ inA( typ )
      case Let( xs, t ) ⇒ letA( xs map (_.x), fold(t) )
    }
    
    fold _
  }
  
}

object PrettyPrint {
  import Fold._

  var nodeid = 0
  def getid() = { nodeid += 1; nodeid }

  var out: PrintWriter = null

  def printDot( ast:Term ) {
    val dotify = createFold[Int]( 
      thenP, assignP, whileP, outputP, numP, boolP, strP, undefP,
      varP, unopP, binopP, ifP, inP, letP )

    out = new PrintWriter( 
      new BufferedWriter( 
	new FileWriter( "ast.dot" ) ) )
    
    out.println( "digraph AST {" )
    dotify( ast )
    out.println( "}" )

    out.close
  }

  def node( l:String ): Int = {
    val id = getid()
    out.println( id + " [label=\"" + l + "\"];" )
    id
  }

  def edge( e:Tuple2[Int,Int] ) =
    out.println( e._1 + " -> " + e._2 )

  def edgeL( e:Tuple2[Int,Int], l:String ) =
    out.println( e._1 + " -> " + e._2 + "[label=\"" + l + "\"]" )

  def thenP( ts:Seq[Int] ): Int = {
    val id = node( "seq" )
    ts map ( (i) ⇒ edge( id → i ) )
    id
  }

  def assignP( x:String, e:Int ): Int = {
    val id = node( ":=" )
    edge( id → node( x ) )
    edge( id → e )
    id
  }

  def whileP( e:Int, t:Int ): Int = {
    val id = node( "while" )
    edgeL( id → e, "G" )
    edgeL( id → t, "B" )
    id
  }

  def outputP( e:Int ): Int = {
    val id = node( "out" )
    edge( id → e )
    id
  }

  def numP( n:BigInt ): Int =
    node( n.toString )

  def boolP( b:Boolean ): Int =
    node( b.toString )

  def strP( s:String ): Int =
    node( s )

  def undefP(): Int =
    node( "undef" )

  def varP( x:String ): Int =
    node( x )

  def unopP( op :Uop, e:Int): Int = {
    val id = node( op.toString )
    edge( id → e )
    id
  }

  def binopP( op :Bop, e1:Int, e2:Int): Int = {
    val id = node( op.toString )
    edge( id → e1 )
    edge( id → e2 )
    id
  }

  def ifP( e:Int, t1:Int, t2:Int): Int = {
    val id = node( "if" )
    edgeL( id → e,  "G" )
    edgeL( id → t1, "TT" )
    edgeL( id → t2, "FF" )
    id
  }

  def inP( typ:InputType ): Int =
    node( "in:" + typ )

  def letP( xs:Seq[String], t:Int ): Int = {
    val id = node( "var" )
    xs map ( (x) ⇒ edge( id → node( x ) ) )
    edge( id → t )
    id
  }

}
