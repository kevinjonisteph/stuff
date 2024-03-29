package cs162.miniJS.domains

import scala.io._
import scala.collection.mutable.{ Map => MMap, Buffer , Stack}
import cs162.miniJS.syntax._
import cs162.miniJS.values._
import Value._
import Domains._

object Domains {

  // locations in the store
  type Address = Int

  // exception for impossible things
  object inconceivable extends Exception
  
  // exception for undefined behavior
  object undefined extends Exception

  // exception for insecure behavior
  object insecure extends Exception
  
}

// global store
object σ {

  // the actual store; an Address is an index into this buffer
  val store:Buffer[Storable] = Buffer()

  def apply( a:Address ): Storable = 
    if ( a < store.length ) store( a )
    else throw undefined

  def update( a:Address, v:Storable): Storable =
    if ( a < store.length ) {
      store( a ) = v
      UndefV(v.label)
    }
    else throw undefined

  def +=( v:Storable ): Address = {
    store += v
    store.length - 1
  }

  def ++=( vs:Seq[Storable] ): Seq[Address] = {
    val oldLength = store.length
    store ++= vs
    oldLength until store.length
  }

}

object pc {
	val stack = new Stack[Lattice]
	def top( ): Lattice =  stack.top
	def push( l:Lattice ) = { if(stack.isEmpty) 
														{ 
															stack.push(l) 
														} else { 
															stack.push(l.⊔(pc.top)) 
														}
													}
	def pop( ) = stack.pop
}

// environment
case class Env( env:MMap[String, Address] = MMap() ) {

  // retrieve a variable's address or throw exception if variable
  // isn't in the environment
  def apply( x:String ): Address =
    env get x match {
      case Some(a) ⇒ a
      case None ⇒ throw undefined
    }  

  def ++( bindings:Seq[(String, Address)] ): Env =
    Env( env ++ bindings )

}
