package org.jetbrains.plugins.scala.lang.lexer

import java.io.Reader
import java.util.{Map => JMap}

import com.intellij.lexer.{MergingLexerAdapter, _XmlLexer, __XmlLexer}
import com.intellij.psi.tree.{IElementType, TokenSet}
import com.intellij.psi.xml.XmlTokenType
import org.jetbrains.plugins.scala.lang.parser.ScalaElementType

import com.intellij.util.containers.ContainerUtil

/**
 * User: Dmitry.Naydanov
 * Date: 15.04.15.
 */
object ScalaXmlTokenTypes {

  private val allTypes: JMap[String, ScalaTokenType] = ContainerUtil.newHashMap()
  
  private def create(name: String): ScalaTokenType = {
    val tp = new ScalaTokenType(name)
    allTypes.put(name, tp)
    tp
  }

  //let's not allocate Options in lexer
  private def getByName(elemType: IElementType): IElementType = {
    if (elemType == null || elemType.toString == null) return elemType

    val name = elemType.toString
    if (name == null) return elemType

    allTypes.get(name) match {
      case null  => elemType
      case subst => subst
    }
  }

  def substitute(elemType: IElementType): IElementType = getByName(elemType)

  def isSubstituted(elemType: IElementType): Boolean = allTypes.containsKey(elemType.toString)
  
  val XML_EQ = create("XML_EQ")

  val XML_ATTRIBUTE_VALUE_START_DELIMITER = create("XML_ATTRIBUTE_VALUE_START_DELIMITER")

  val XML_ATTRIBUTE_VALUE_TOKEN = create("XML_ATTRIBUTE_VALUE_TOKEN")

  val XML_ATTRIBUTE_VALUE_END_DELIMITER = create("XML_ATTRIBUTE_VALUE_END_DELIMITER")

  val XML_NAME = create("XML_NAME")

  val XML_TAG_NAME = create("XML_TAG_NAME")

  val XML_ATTRIBUTE_NAME = create("XML_ATTRIBUTE_NAME")

  val XML_WHITE_SPACE = create("XML_WHITESPACE")

  val XML_TAG_END = create("XML_TAG_END")

  val XML_CDATA_END = create("XML_CDATA_END")

  val XML_PI_END = create("XML_PI_END")

  val XML_EMPTY_ELEMENT_END = create("XML_EMPTY_ELEMENT_END")

  val XML_START_TAG_START = create("XML_START_TAG_START")

  val XML_END_TAG_START = create("XML_END_TAG_START")

  val XML_CDATA_START = create("XML_CDATA_START")

  val XML_PI_START = create("XML_PI_START")

  val XML_DATA_CHARACTERS = create("XML_DATA_CHARACTERS")

  val XML_COMMENT_CHARACTERS = create("XML_COMMENT_CHARACTERS")

  val XML_COMMENT_START = create("XML_COMMENT_START")

  val XML_COMMENT_END = create("XML_COMMENT_END")

  val XML_BAD_CHARACTER = create("XML_BAD_CHARACTER")

  val XML_CHAR_ENTITY_REF = create("XML_CHAR_ENTITY_REF")

  val XML_ENTITY_REF_TOKEN = create("XML_ENTITY_REF_TOKEN")

  val XML_TAG_CHARACTERS = create("XML_TAG_CHARACTERS")

  val XML_ELEMENTS = TokenSet.create(ScalaElementType.XML_PI, ScalaElementType.XML_ATTRIBUTE, ScalaElementType.XML_CD_SECT,
    ScalaElementType.XML_COMMENT, ScalaElementType.XML_ELEMENT, ScalaElementType.XML_EMPTY_TAG, ScalaElementType.XML_END_TAG,
    ScalaElementType.XML_EXPR, ScalaElementType.XML_PATTERN, ScalaElementType.XML_START_TAG,
    ScalaTokenTypesEx.SCALA_IN_XML_INJECTION_START, ScalaTokenTypesEx.SCALA_IN_XML_INJECTION_END, XML_EQ,
    XML_ATTRIBUTE_VALUE_START_DELIMITER, XML_NAME, XML_TAG_END, XML_CDATA_END, XML_PI_END, XML_EMPTY_ELEMENT_END,
    XML_START_TAG_START, XML_END_TAG_START, XML_CDATA_START, XML_PI_START, XML_DATA_CHARACTERS, XML_COMMENT_CHARACTERS)

  val XML_COMMENTS = TokenSet.create(XML_COMMENT_START, XML_COMMENT_CHARACTERS, XML_COMMENT_END)

  val XML_TOKENS_TO_MERGE = TokenSet.create(XML_DATA_CHARACTERS, XML_TAG_CHARACTERS, // merging can be performed in locateToken() => we need to merge both types of tokens
    XML_ATTRIBUTE_VALUE_TOKEN, XML_COMMENT_CHARACTERS, XmlTokenType.XML_DATA_CHARACTERS,
    XmlTokenType.XML_TAG_CHARACTERS, XmlTokenType.XML_ATTRIBUTE_VALUE_TOKEN, XmlTokenType.XML_PI_TARGET, XmlTokenType.XML_COMMENT_CHARACTERS)

  class PatchedXmlLexer extends MergingLexerAdapter(new _XmlLexer(new __XmlLexer(null.asInstanceOf[Reader]), false), XML_TOKENS_TO_MERGE) {
    override def getTokenType: IElementType = substitute(super.getTokenType)
  }
}
