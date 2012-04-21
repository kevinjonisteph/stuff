import scala.io._

import cs162.miniJS.util._
import cs162.miniJS.syntax._
import cs162.miniJS.domains._
import cs162.miniJS.values._
import PrettyPrint._
import Value._
import Domains._

// the main entry point
object miniJS {

  def main(args:Array[String]) {
    // parse the given program
    val ast = ParseL.getAST( Source.fromFile( args(0) ).mkString )
    
    // either print the AST or evaluate it
    if ( args.length > 1 && args(1) == "--ast" ) 
      printDot( ast )
    else {
			pc.push(Public)
      inScope( Env() ) eval ast //Check this
		}
  }
  
}

// every term is evaluated inside a scope corresponding to a
// particular environment
case class inScope( ρ:Env ) {

  // evaluate a sequence of expressions returning a list of the
  // corresponding values
  def evalS( ts:Seq[Term] ): Seq[Storable] = 
    ts map( eval )

  // the main evaluation function
  def eval( t:Term): Storable = t match {
    case Then( ts ) ⇒ 
      evalS( ts ).last
    
    // notice that e is evaluated _before_ we check whether x is legal
    case Assign( Var(x), e ) ⇒ {
      val v = eval( e )
			if(pc.top.⊑(v.label)) {
      	σ( ρ( x ) ) = v
			}
			else
				throw insecure
    }
    
    case w @ While( e, t ) ⇒ {
      val v = eval( e )
			var s = v
      if ( v.T ) {
				pc.push(v.label.⊔(pc.top()))
				eval( t )
				s = eval( w )
			}
			else s = UndefV(pc.top())

			pc.pop
			s
    }

    case Output( e , l) ⇒ {
      val v = eval( e )
	
      if (v.label != Public)
				throw insecure
      
      println( v )
      UndefV(l)
    }
    
    case Num( n ) ⇒ 
      NumV( pc.top(), n )

    case Bool( b ) ⇒ 
      BoolV( pc.top(), b )
      
    case Str( s ) ⇒ 
      StrV( pc.top(), s )
      
    case Undef() ⇒ 
      UndefV(pc.top())

    case Var( x ) ⇒ 
      σ( ρ( x ) )

    case UnOp( op, e ) ⇒ {
      val v = eval( e )

      op match {
	case ⌞−⌟ ⇒ v neg
	case ⌞¬⌟ ⇒ v not
      }
    }
    
    case BinOp( op, e1, e2 ) ⇒ {
      val v1 = eval( e1 )
      val v2 = eval( e2 )

      op match {
	case ⌜+⌝ ⇒ v1 + (v2 , pc.top())//+( NumV(l, v1+ v2), l )
	case ⌜−⌝ ⇒ v1 − (v2 , pc.top())
	case ⌜×⌝ ⇒ v1 × (v2 , pc.top())
	case ⌜÷⌝ ⇒ v1 ÷ (v2 , pc.top())
	case ⌜∧⌝ ⇒ v1 ∧ (v2 , pc.top())
	case ⌜∨⌝ ⇒ v1 ∨ (v2 , pc.top())
	case ⌜=⌝ ⇒ v1 ≈ (v2 , pc.top())
	case ⌜≠⌝ ⇒ v1 ≠ (v2 , pc.top())
	case ⌜≤⌝ ⇒ v1 ≤ (v2 , pc.top())
	case ⌜<⌝ ⇒ v1 < (v2 , pc.top())
      }
    }
    
    case If( e, t1, t2 ) ⇒ {
      val v = eval( e )
			var s = v
      if ( v.T ) {
				pc.push(v.label.⊔(pc.top()))
				s = eval( t1 )
			}
			else {
				pc.push(v.label.⊔(pc.top()))
			 	s = eval( t2 )
			}
			pc.pop()
			s
    }
    
    case In( typ, l ) ⇒ 
      typ match {
	case NumIT ⇒ NumV(l, BigInt( Console.readLine() ) )
	case StrIT ⇒ StrV(l, Console.readLine() )
      }
    
    case Let( xs, t ) ⇒ 
    {
      val bindings = xs map ( _.x → ( σ += UndefV(pc.top()) ) )
      inScope( ρ ++ bindings ) eval t
    }
    
  }

}
