package org.jetbrains.plugins.scala.lang.psi.api.statements

import com.intellij.psi.{PsiMethod, PsiTypeParameter}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.{ScSyntheticFunction, ScSyntheticTypeParameter}
import org.jetbrains.plugins.scala.lang.psi.types.api.TypeParameterType
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult

trait ScReturnTypeHolder extends ScalaPsiElement with ScDeclaredElementsHolder {
  def declaredType: TypeResult
  def typeElement: Option[ScTypeElement]
  def superMethodAndSubstitutor: Option[(PsiMethod, ScSubstitutor)]
  def typeParameters: Seq[ScTypeParam]

  def getInheritedReturnType: Option[ScType] = {
    typeElement match {
      //TODO is 'returnType' really better here instead of declaredType?
      case Some(_) => this.declaredType.toOption
      case None =>
        val superReturnType = superMethodAndSubstitutor match {
          case Some((fun: ScFunction, subst)) =>
            val typeParamSubst =
              ScSubstitutor.bind(fun.typeParameters, typeParameters)(TypeParameterType(_))

            fun.returnType.toOption.map(typeParamSubst.followed(subst).subst)
          case Some((fun: ScSyntheticFunction, subst)) =>
            val typeParamSubst =
              ScSubstitutor.bind(fun.typeParameters, typeParameters)(TypeParameterType(_))

            Some(typeParamSubst.subst(fun.retType))
          case Some((fun: PsiMethod, subst)) =>
            val typeParamSubst =
              ScSubstitutor.bind(fun.getTypeParameters, typeParameters)(TypeParameterType(_))

            Some(typeParamSubst.followed(subst).subst(fun.getReturnType.toScType()))
          case _ => None
        }
        superReturnType
    }
  }
}
