package org.jetbrains.plugins.scala
package lang
package parser
package parsing
package statements

import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.parsing.builder.ScalaPsiBuilder
import org.jetbrains.plugins.scala.lang.parser.parsing.expressions.{Ascription, ExprInIndentationRegion}
import org.jetbrains.plugins.scala.lang.parser.parsing.patterns.Pattern2InForDef

/*
 * Scala 2.12
 * PatDef ::= Pattern2 {',' Pattern2} [':' Type] '=' Expr
 *
 * Scala 3
 * PatDef ::= ids [‘:’ Type] ‘=’ Expr
 *          | Pattern2 [‘:’ Type | Ascription] ‘=’ Expr
 */
object PatDef extends ParsingRule {

  override def apply()(implicit builder: ScalaPsiBuilder): Boolean = {
    val patDefMarker = builder.mark

    def parsePatterns(): Boolean = {
      if (!Pattern2InForDef()) {
        return false
      }
      while (ScalaTokenTypes.tCOMMA.equals(builder.getTokenType)) {
        builder.checkedAdvanceLexer()
        if (!Pattern2InForDef()) {
          return false
        }
      }
      true
    }

    def parseTypeOrAnnotationAscription(): Boolean = {
      if (builder.getTokenType == ScalaTokenTypes.tCOLON) {
        Ascription()
        true
      } else {
        false
      }
    }

    def parseAssignExpression(): Boolean = {
      if (builder.getTokenType == ScalaTokenTypes.tASSIGN) {
        builder.checkedAdvanceLexer()

        if (!ExprInIndentationRegion()) {
          builder.error(ErrMsg("expression.expected"))
        }

        patDefMarker.drop()
        true
      } else {
        patDefMarker.rollbackTo()
        false
      }
    }


    val patternsMarker = builder.mark
    if (!parsePatterns()) {
      patternsMarker.rollbackTo()
      patDefMarker.drop()
      return false
    }
    patternsMarker.done(ScalaElementType.PATTERN_LIST)

    parseTypeOrAnnotationAscription()
    parseAssignExpression()
  }
}