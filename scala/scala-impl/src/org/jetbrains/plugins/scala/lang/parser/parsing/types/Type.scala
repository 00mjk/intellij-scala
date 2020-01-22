package org.jetbrains.plugins.scala
package lang
package parser
package parsing
package types

import com.intellij.lang.PsiBuilder
import com.intellij.psi.tree.IElementType
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.parsing.builder.ScalaPsiBuilder

/*
 * Type ::= InfixType '=>' Type
 *        | '(' ['=>' Type] ')' => Type
 *        | InfixType [ExistentialClause]
 *        | _ SubtypeBounds
 *        | ? SubtypeBounds (Scala 3)
 * SubtypeBounds : == [>: Type] [<: Type]
 */
object Type extends Type {
  override protected def infixType = InfixType

  // TODO: handle changes for later Dotty versions https://dotty.epfl.ch/docs/reference/changed-features/wildcards.html
  //   In Scala 3.0, both _ and ? are legal names for wildcards.
  //   In Scala 3.1, _ is deprecated in favor of ? as a name for a wildcard. A -rewrite option is available to rewrite one to the other.
  //   In Scala 3.2, the meaning of _ changes from wildcard to placeholder for type parameter.
  //   The Scala 3.1 behavior is already available today under the -strict setting.
  def isWildcardStartToken(tokenType: IElementType)(implicit builder: ScalaPsiBuilder): Boolean =
    tokenType match {
      case ScalaTokenTypes.tQUESTION if builder.isScala3 => true
      case ScalaTokenTypes.tUNDER                        => true
      case _                                             => false
    }
}

trait Type {
  protected def infixType: InfixType

  def parse(builder: ScalaPsiBuilder, star: Boolean = false, isPattern: Boolean = false): Boolean = {
    implicit val b: ScalaPsiBuilder = builder
    val typeMarker = builder.mark

    if (infixType.parse(builder, star, isPattern)) {
      builder.getTokenType match {
        case ScalaTokenTypes.tFUNTYPE =>
          builder.advanceLexer() //Ate =>
          if (!parse(builder, isPattern = isPattern)) {
            builder.error(ScalaBundle.message("wrong.type"))
          }
          typeMarker.done(ScalaElementType.TYPE)
        case ScalaTokenTypes.kFOR_SOME =>
          ExistentialClause parse builder
          typeMarker.done(ScalaElementType.EXISTENTIAL_TYPE)
        case _ =>
          typeMarker.drop()
      }
      true
    } else if (parseWildcardType(typeMarker, isPattern)) {
      true
    } else {
      builder.getTokenType match {
        case ScalaTokenTypes.tIDENTIFIER if builder.getTokenText == "*" =>
          typeMarker.drop()
          true
        case _ =>
          typeMarker.drop()
          false
      }
    }
  }

  private def parseWildcardType(typeMarker: PsiBuilder.Marker, isPattern: Boolean)(implicit builder: ScalaPsiBuilder): Boolean = {
    if (!Type.isWildcardStartToken(builder.getTokenType))
      return false

    builder.advanceLexer() // eat _ or ?
    Bounds.parseSubtypeBounds()
    typeMarker.done(ScalaElementType.WILDCARD_TYPE)

    // TODO: looks like this is a dead code, no tests trigger breakpoint inside, leaving it just in case...
    builder.getTokenType match {
      case ScalaTokenTypes.tFUNTYPE =>
        val funMarker = typeMarker.precede()
        builder.advanceLexer() //Ate =>
        if (!parse(builder, isPattern = isPattern)) {
          builder error ScalaBundle.message("wrong.type")
        }
        funMarker.done(ScalaElementType.TYPE)
      case _ =>
    }

    true
  }
}