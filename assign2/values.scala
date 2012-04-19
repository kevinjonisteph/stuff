package cs162.miniJS.values

import cs162.miniJS.syntax._
import cs162.miniJS.domains._
import Value._
import Domains._

// language values
sealed abstract class Value

// companion object
object Value {

  /*
  // implicit conversions
  implicit def v2n( v:NumV )    : BigInt  = v.n
  implicit def v2b( v:BoolV )   : Boolean = v.b
  implicit def v2s( v:StrV )    : String  = v.s
  implicit def n2v( n:BigInt )  : NumV    = NumV( n )
  implicit def b2v( b:Boolean ) : BoolV   = BoolV( b )
  implicit def s2v( s:String )  : StrV    = StrV( s )
  */  

}

// storable values (non-exceptional)
sealed abstract class Storable( l:Lattice ) extends Value {
  
	def label = l
  def T: Boolean

  def + ( v:Storable, l:Lattice ): Storable
  def − ( v:Storable, l:Lattice ): Storable
  def × ( v:Storable, l:Lattice ): Storable
  def ÷ ( v:Storable, l:Lattice ): Storable
  def ≈ ( v:Storable, l:Lattice ): Storable
  def ≠ ( v:Storable, l:Lattice ): Storable
  def ≤ ( v:Storable, l:Lattice ): Storable
  def < ( v:Storable, l:Lattice ): Storable
  def ∧ ( v:Storable, l:Lattice ): Storable
  def ∨ ( v:Storable, l:Lattice ): Storable
  
  def neg: Storable
  def not: Storable
}

// numbers
case class NumV(level:Lattice, n:BigInt ) extends Storable (level:Lattice){
  def T = n != 0
  
  def + ( v:Storable, l:Lattice ) = v match {
    
    case NumV( _level, _n ) ⇒  NumV( _level.⊔(l), n + _n)
    case _ ⇒ throw undefined
  }
  def − ( v:Storable, l:Lattice ) = v match {
    case NumV( _level, _n ) ⇒ NumV( _level.⊔(l), n - _n)
    case _ ⇒ throw undefined
  }
  def × ( v:Storable, l:Lattice ) = v match {
    case NumV( _level, _n ) ⇒ NumV( _level.⊔(l), n * _n)
    case _ ⇒ throw undefined
  }
  def ÷ ( v:Storable, l:Lattice ) = v match {
    case NumV( _level, _n ) ⇒ if ( _n != 0 ) NumV( _level.⊔(l), n / _n) else throw undefined
    case _ ⇒ throw undefined
  }
  def ≈ ( v:Storable, l:Lattice ) = v match {
    case NumV( _level, _n ) ⇒ BoolV( _level.⊔(l), n == _n)
    case _ ⇒ BoolV( l, false)
  }
  def ≠ ( v:Storable, l:Lattice ) = v match {
    case NumV( _level, _n ) ⇒ BoolV( _level.⊔(l), n != _n)
    case _ ⇒ BoolV( l, true)
  }
  def ≤ ( v:Storable, l:Lattice ) = v match {
    case NumV( _level, _n ) ⇒ BoolV( _level.⊔(l), n <= _n)
    case _ ⇒ throw undefined
  }
  def < ( v:Storable, l:Lattice ) = v match {
    case NumV( _level, _n ) ⇒ BoolV( _level.⊔(l), n < _n)
    case _ ⇒ throw undefined
  }
  def ∧ ( v:Storable, l:Lattice ) = BoolV( l, this.T && v.T )
  def ∨ ( v:Storable, l:Lattice ) = BoolV( l, this.T || v.T )
  
  def neg = NumV( level, -n)
  def not = BoolV( level, !this.T)
  
  override def toString = n.toString
}
  
// booleans
case class BoolV(level:Lattice, b:Boolean ) extends Storable (level:Lattice){
  def T = b
  
  def + ( v:Storable, l:Lattice ) = throw undefined
  def − ( v:Storable, l:Lattice ) = throw undefined
  def × ( v:Storable, l:Lattice ) = throw undefined
  def ÷ ( v:Storable, l:Lattice ) = throw undefined
  def ≈ ( v:Storable, l:Lattice ) = v match {
    case BoolV( _level, _b ) ⇒ BoolV( _level.⊔(l), b == _b )
    case _ ⇒ BoolV( l, false)
  }
  def ≠ ( v:Storable, l:Lattice ) = v match {
    case BoolV( _level, _b ) ⇒ BoolV( _level.⊔(l), b != _b )
    case _ ⇒ BoolV( l, true)
  }
  def ≤ ( v:Storable, l:Lattice ) = throw undefined
  def < ( v:Storable, l:Lattice ) = throw undefined
  def ∧ ( v:Storable, l:Lattice ) = BoolV( l, this.T && v.T )
  def ∨ ( v:Storable, l:Lattice ) = BoolV( l, this.T || v.T )
  
  def neg = throw undefined
  def not = BoolV( level, !this.T)
  
  override def toString = b.toString
}

// strings
case class StrV(level:Lattice, s:String ) extends Storable(level:Lattice) {
  def T = s != ""
  
  def + ( v:Storable, l:Lattice ) = v match {
    case StrV( _level, _s ) ⇒  StrV( _level.⊔(l), s + _s )
    case _ ⇒ throw undefined
  }
  def − ( v:Storable, l:Lattice ) = throw undefined
  def × ( v:Storable, l:Lattice ) = throw undefined
  def ÷ ( v:Storable, l:Lattice ) = throw undefined
  def ≈ ( v:Storable, l:Lattice ) = v match {
    case StrV( _level, _s ) ⇒ BoolV( _level.⊔(l), s == _s )
    case _ ⇒ BoolV( l, false)
  }
  def ≠ ( v:Storable, l:Lattice ) = v match {
    case StrV( _level, _s ) ⇒ BoolV( _level.⊔(l), s != _s )
    case _ ⇒ BoolV( l, true)
  }
  def ≤ ( v:Storable, l:Lattice ) = v match {
    case StrV( _level, _s ) ⇒ BoolV( _level.⊔(l), s <= _s )
    case _ ⇒ throw undefined
  }
  def < ( v:Storable, l:Lattice ) = v match {
    case StrV( _level, _s ) ⇒ BoolV( _level.⊔(l), s < _s )
    case _ ⇒ throw undefined
  }
  def ∧ ( v:Storable, l:Lattice ) = BoolV( l, this.T && v.T)
  def ∨ ( v:Storable, l:Lattice ) = BoolV( l, this.T || v.T)
  
  def neg = throw undefined
  def not = BoolV( level, !this.T);
  
  override def toString = s
}

// undefined value
case class UndefV(level:Lattice) extends Storable(level:Lattice) {
  def T = false
  
  def + ( v:Storable, l:Lattice ) = throw undefined
  def − ( v:Storable, l:Lattice ) = throw undefined
  def × ( v:Storable, l:Lattice ) = throw undefined
  def ÷ ( v:Storable, l:Lattice ) = throw undefined
  def ≈ ( v:Storable, l:Lattice ) = v match {
    case UndefV(_level) ⇒ BoolV( _level.⊔(l) , true)
    case _ ⇒ BoolV( l , false)
  }
  def ≠ ( v:Storable, l:Lattice ) = v match {
    case UndefV(_level) ⇒ BoolV( _level.⊔(l) , false)
    case _ ⇒ BoolV( l , true)
  } 
  def ≤ ( v:Storable, l:Lattice ) = throw undefined
  def < ( v:Storable, l:Lattice ) = throw undefined
  def ∧ ( v:Storable, l:Lattice ) = BoolV( l, this.T && v.T)
  def ∨ ( v:Storable, l:Lattice ) = BoolV( l, this.T || v.T)
  
  def neg = throw undefined
  def not = BoolV( level, !this.T)
  
  override def toString = "undef"
}
