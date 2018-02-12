package org.jetbrains.plugins.scala.lang.psi.api.statements

import com.intellij.psi.{PsiMethod, PsiTypeParameter}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.{ScSyntheticFunction, ScSyntheticTypeParameter}
import org.jetbrains.plugins.scala.lang.psi.types.api.TypeParameterType
import org.jetbrains.plugins.scala.lang.psi.types.{ScSubstitutor, ScType}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params._
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult

trait ScReturnTypeHolder extends ScalaPsiElement with ScDeclaredElementsHolder {
  def declaredType: TypeResult
  def typeElement: Option[ScTypeElement]
  def superMethodAndSubstitutor: Option[(PsiMethod, ScSubstitutor)]
  def typeParameters: Seq[ScTypeParam]

  def getInheritedReturnType: Option[ScType] = {
    typeElement match {
      case Some(_) => this.declaredType.toOption
      case None =>
        superMethodAndSubstitutor match {
          case Some((fun: ScFunction, subst)) =>
            var typeParamSubst = ScSubstitutor.empty
            fun.typeParameters.zip(typeParameters).foreach {
              case (oldParam: ScTypeParam, newParam: ScTypeParam) =>
                typeParamSubst = typeParamSubst.bindT(oldParam.nameAndId, TypeParameterType(newParam, Some(subst)))
            }
            fun.returnType.toOption.map(typeParamSubst.followed(subst).subst)
          case Some((fun: ScSyntheticFunction, subst)) =>
            var typeParamSubst = ScSubstitutor.empty
            fun.typeParameters.zip(typeParameters).foreach {
              case (oldParam: ScSyntheticTypeParameter, newParam: ScTypeParam) =>
                typeParamSubst = typeParamSubst.bindT(oldParam.nameAndId, TypeParameterType(newParam, Some(subst)))
            }
            Some(subst.subst(fun.retType))
          case Some((fun: PsiMethod, subst)) =>
            var typeParamSubst = ScSubstitutor.empty
            fun.getTypeParameters.zip(typeParameters).foreach {
              case (oldParam: PsiTypeParameter, newParam: ScTypeParam) =>
                typeParamSubst = typeParamSubst.bindT(oldParam.nameAndId, TypeParameterType(newParam, Some(subst)))
            }
            Some(typeParamSubst.followed(subst).subst(fun.getReturnType.toScType()))
          case _ => None
        }
    }
  }
}
