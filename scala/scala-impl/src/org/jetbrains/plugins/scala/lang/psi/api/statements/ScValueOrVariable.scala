package org.jetbrains.plugins.scala.lang.psi.api.statements

import com.intellij.psi.{PsiElement, PsiMethod}
import com.intellij.psi.tree.IElementType
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScBlockStatement
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypedDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScDocCommentOwner, ScMember}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.TypeDefinitionMembers
import org.jetbrains.plugins.scala.lang.psi.types.{PhysicalSignature, ScSubstitutor}
import org.jetbrains.plugins.scala.lang.psi.types.result.{Failure, TypeResult, Typeable}

/**
  * @author adkozlov
  */
trait ScValueOrVariable extends ScBlockStatement with ScMember with ScDocCommentOwner with ScDeclaredElementsHolder
  with ScAnnotationsHolder with ScCommentOwner with Typeable with ScReturnTypeHolder {
  def keywordToken: PsiElement = findFirstChildByType(keywordElementType)

  protected def keywordElementType: IElementType

  def declaredElements: Seq[ScTypedDefinition]

  def typeElement: Option[ScTypeElement]

  def typeParameters: Seq[ScTypeParam] = Seq.empty

  def superMethodAndSubstitutor: Option[(PsiMethod, ScSubstitutor)] = {
    if (containingClass != null && declaredNames.nonEmpty) {
      val sigs = TypeDefinitionMembers.getSignatures(containingClass).forName(declaredNames.head)._1
      val option = sigs.filter{ sig =>
        sig._1.paramLength.isEmpty || sig._1.paramLength.forall(_ == 0)
      }.headOption.map(_._2)
      if (option.isEmpty) return None
      option.get.primarySuper.filter(_.info.isInstanceOf[PhysicalSignature]).
        map(node => (node.info.asInstanceOf[PhysicalSignature].method, node.info.substitutor))
    } else None
  }

  def declaredType: TypeResult =
    typeElement match {
      case Some(element) => element.`type`()
      case _ => Failure("No declared type specified")
    }

  final def hasExplicitType: Boolean = typeElement.isDefined

  override protected def isSimilarMemberForNavigation(member: ScMember, isStrict: Boolean): Boolean = member match {
    case other: ScValueOrVariable =>
      for (thisName <- declaredNames;
           otherName <- other.declaredNames
           if thisName == otherName) {
        return true
      }
      super.isSimilarMemberForNavigation(member, isStrict)
    case _ => false
  }
}
