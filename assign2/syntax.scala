package cs162.miniJS.syntax

//---------- SECURITY LATTICE ----------

sealed abstract class Lattice {
  def ⊔( l:Lattice ): Lattice
  def ⊑( l:Lattice ): Boolean
}

case object Public extends Lattice {
  def ⊔( l:Lattice ) = l
  def ⊑( l:Lattice ) = true
}

case object Alice extends Lattice {
  def ⊔( l:Lattice ) = if ( Alice ⊑ l ) l else if (l ⊑ Alice) Alice else Secret
  def ⊑( l:Lattice ) = l == Alice || l == Secret
}

case object Bob extends Lattice {
  def ⊔( l:Lattice ) = if ( Bob ⊑ l ) l else if (l ⊑ Bob) Bob else Secret
  def ⊑( l:Lattice ) = l == Bob || l == Secret
}

case object Secret extends Lattice {
  def ⊔( l:Lattice ) = Secret
  def ⊑( l:Lattice ) = l == Secret
}

//---------- AST ----------

// language term (command or expression)
sealed abstract class Term

// commands
sealed abstract class Cmd extends Term
case class Then(ts:Seq[Term]) extends Cmd             // t1 ; t2 ; ...
case class Assign(x:Var, e:Exp) extends Cmd           // x := e
case class While(e:Exp, t:Term) extends Cmd           // while e t
case class Output(e:Exp, l:Lattice) extends Cmd       // output e l

// expressions
sealed abstract class Exp extends Term
case class Num(n:BigInt) extends Exp                  // n
case class Bool(b:Boolean) extends Exp                // b
case class Str(s:String) extends Exp                  // s
case class Undef() extends Exp                        // undef
case class Var(x:String) extends Exp                  // x
case class UnOp(op:Uop, e:Exp) extends Exp            // ⊖ e
case class BinOp(op:Bop, e1:Exp, e2:Exp) extends Exp  // e1 ⊕ e2
case class If(e:Exp, t1:Term, t2:Term) extends Exp    // if e t1 else t2
case class In(typ:InputType, l:Lattice) extends Exp   // input l typ
case class Let(xs:Seq[Var], t:Term) extends Exp       // var xs t

// input types
sealed abstract class InputType
case object NumIT extends InputType
case object StrIT extends InputType

// unary operators
sealed abstract class Uop
case object ⌞−⌟ extends Uop
case object ⌞¬⌟ extends Uop

// binary operators
sealed abstract class Bop
case object ⌜+⌝ extends Bop
case object ⌜−⌝ extends Bop
case object ⌜×⌝ extends Bop
case object ⌜÷⌝ extends Bop
case object ⌜∧⌝ extends Bop
case object ⌜∨⌝ extends Bop
case object ⌜=⌝ extends Bop
case object ⌜≠⌝ extends Bop
case object ⌜≤⌝ extends Bop
case object ⌜<⌝ extends Bop

//---------- PARSER ----------

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object ParseL extends StandardTokenParsers with PackratParsers {
  type P[T] = PackratParser[T]

  // reserved keywords
  lexical.reserved += ( "true", "false", "while", "output", "undef",
		        "if", "else", "input", "var", "in", "break",
		        "try", "catch", "finally", "throw", "str",
		        "num", "secret", "public", "alice", "bob" )

  lexical.delimiters += ( "+", "-", "*", "/", "!", "&", "|", "=",
			 "<=", "<", "{", "}", "(", ")", ":=", ";",
			 ",", ":", "<<", ">>", ".", "=>", "!=", ":>",
			 "[", "]" )
  
  // for debugging the parser: modify each rule you want to trace by
  // changing '= <pattern>' to '= "name" !!! <pattern>'. turn off the
  // debugging either by changing the !!! method to return p instead
  // of log(p)(name)
  implicit def toLogged( name:String ) = new { 
    def !!![T]( p:P[T] ) = p//log( p )( name )
  }

  // take the program as a string and return the corresponding AST
  // (or exit with an error message)
  def getAST( program:String ) = {
    // strip out comments
    val commentR = """<<((>?[^>]+)*)>>""".r
    val prog = commentR.replaceAllIn( program, "" )

    // parse the program
    val lexer = new lexical.Scanner( prog )
    val result = phrase( TermP )( lexer )

    // return result or a useful error message
    result match {
      case Success(ast,_) => ast
      case NoSuccess(msg, next) => { 
	println( "Parse error: " + msg )
	println( "At line " + next.pos.line + ", column " + next.pos.column )
	println( next.pos.longString )
	sys.exit(1) 
      }
    }
  }
  
  // terms (seqP promoted here for precedence issues)
  lazy val TermP: P[Term] = seqP

  lazy val seqP: P[Term] = "seq" !!! 
    repsep((CmdP | ExpP), ";") ^^ (Then)

  // commands
  lazy val CmdP: P[Cmd] = 
    ( assignP | whileP | outputP )

  // expressions (factored to E for precedence issues)
  lazy val ExpP: P[Exp] = 
    ( binopP | E )

  // expressions
  lazy val E: P[Exp] = (
      blockP
    | ifP                           
    | unopP
    | inputP
    | numP
    | boolP
    | strP
    | unitP
    | varP
    | "(" ~> ExpP <~ ")"
  )

  // assignment
  lazy val assignP: P[Assign] = "assign" !!! 
  varP ~ (":=" ~> ExpP) ^^ 
  { 
    case x ~ rhs ⇒ Assign(x, rhs) 
  }
  
  // while
  lazy val whileP: P[While] = "while" !!! 
  "while" ~ "(" ~> ExpP ~ (")" ~> (("{" ~> TermP <~ "}") | CmdP | ExpP)) ^^ 
  {
    case guard ~ body ⇒ While(guard, body) 
  }

  // output
  lazy val outputP: P[Output] = "output" !!! 
  "output" ~> latticeP ~ ExpP ^^
  {
    case level ~ out ⇒ Output(out, level)
  }

  // integer
  lazy val numP: P[Num] = "num" !!! (
      numericLit        ^^ ((n:String) ⇒ Num(BigInt(n)))
    | "-" ~> numericLit ^^ ((n:String) ⇒ Num(-BigInt(n)))
  )

  // boolean
  lazy val boolP: P[Bool] = "bool" !!! (
      "true"  ^^^ Bool(true)
    | "false" ^^^ Bool(false)
  )

  // string
  lazy val strP: P[Str] = "string" !!! 
  stringLit ^^ (Str)

  // unit
  lazy val unitP: P[Undef] = "undef" !!! 
  ("undef" ^^^ Undef())

  // variable
  lazy val varP: P[Var] = "var" !!! 
  ident ^^ (Var)

  // unary op
  lazy val unopP: P[UnOp] = "unop" !!! (
      notP
    | negP
  )

  // logical negation
  lazy val notP: P[UnOp] = "not" !!! 
  "!" ~> E ^^ 
  {
    case e ⇒ UnOp(⌞¬⌟, e)
  }

  // arithmetic negation
  lazy val negP: P[UnOp] = "neg" !!! 
  "-" ~> E ^^ 
  {
    case e ⇒ UnOp(⌞−⌟, e)
  }

  // binary operations
  lazy val binopP: P[BinOp] = "binop" !!! 
  E ~ bopP ~ ExpP ^^ 
  { 
    case e1 ~ bop ~ e2 ⇒ BinOp(bop, e1, e2) 
  }
  
  // binary operators
  lazy val bopP: P[Bop] = "bop" !!! (
      "+"  ^^^ ⌜+⌝
    | "-"  ^^^ ⌜−⌝
    | "*"  ^^^ ⌜×⌝
    | "/"  ^^^ ⌜÷⌝
    | "&"  ^^^ ⌜∧⌝
    | "|"  ^^^ ⌜∨⌝
    | "="  ^^^ ⌜=⌝
    | "!=" ^^^ ⌜≠⌝
    | "<=" ^^^ ⌜≤⌝
    | "<"  ^^^ ⌜<⌝
  )

  // if
  lazy val ifP: P[If] = "if" !!! 
  "if" ~ "(" ~> ExpP ~ (")" ~> (("{" ~> TermP <~ "}") | CmdP | ExpP)) ~ opt("else" ~> (("{" ~> TermP <~ "}") | CmdP | ExpP)) ^^
  { 
    case guard ~ tT ~ tFo ⇒ 
      tFo match { 
	case Some(tF) ⇒ If(guard, tT, tF) 
        case None ⇒ If(guard, tT, Undef())
      }
  }

  // input
  lazy val inputP: P[In] = "input" !!! 
  "input" ~> latticeP ~ typP ^^
  {
    case level ~ typ ⇒ In(typ, level)
  }
  
  // types
  lazy val typP: P[InputType] = (
      "num"  ^^^ NumIT
    | "str"  ^^^ StrIT
  )

  // block
  lazy val blockP: P[Let] = "block" !!! 
  "var" ~> repsep(varP, ",") ~ ("in" ~> (("{" ~> TermP <~ "}") | TermP)) ^^
  { case xs ~ t ⇒ Let(xs, t) }

  // security lattice level
  lazy val latticeP: P[Lattice] = "lattice" !!! (
      "public" ^^^ Public
    | "alice"  ^^^ Alice
    | "bob"    ^^^ Bob
    | "secret" ^^^ Secret
  )

}
