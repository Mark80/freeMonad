import sun.security.pkcs11.Secmod.TrustType

/**
  * Created by marco on 14/04/17.
  */
object Main {


  implicitly[TrueType =:= TrueType]
  implicitly[TrueType#Not =:= FalseType]
  implicitly[TrueType#Or[FalseType] =:= TrueType]



}


trait BoolType {

  type Not <: BoolType
  type Or[That <: BoolType] <: BoolType
  type And[That <: BoolType] <: BoolType


}

trait TrueType extends BoolType{

  override type Not = FalseType
  override type Or[That <: BoolType] = TrueType
  override type And[That <: BoolType] = That

}

trait FalseType extends BoolType{

  override type Not = TrueType
  override type Or[That <: BoolType] = That
  override type And[That <: BoolType] = FalseType


}