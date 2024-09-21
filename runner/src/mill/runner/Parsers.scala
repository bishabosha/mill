package mill.runner

import mill.api.internal
import mill.util.Util.newLine

import scala.collection.mutable
import mill.runner.Scala3Parsers.Indenting.colon
import mill.runner.Scala3Parsers.Expressions.Expr

@internal
case class ImportTree(
    prefix: Seq[(String, Int)],
    mappings: Seq[(String, Option[String])],
    start: Int,
    end: Int
)

@internal
object Scala3Parsers {
  import fastparse._
  import NoWhitespace._

  object Lexer {
    import Expressions.BlockExpr

    def whiteSpace[_X: P] = P(CharIn("\u0020\u0009\u000D\u000A"))
    def ws[_X: P] = P(whiteSpace.rep)
    def upper[_X: P] = P(CharIn("A-Z$") | CharPred(_.isUpper))
    def lower[_X: P] = P(CharIn("a-z_") | CharPred(_.isLower))
    def letter[_X: P] = P(upper | lower)
    def digit[_X: P] = P(CharIn("0-9"))
    def paren[_X: P] = P(CharIn("()[]{}"))
    def delim[_X: P] = P(CharIn("`'\".;,"))
    def opchar[_X: P] = P(CharIn("!#%&*+-/:<=>?@\\^|~") | CharPred(c => c.getType == Character.MATH_SYMBOL || c.getType == Character.OTHER_SYMBOL))
    def printableChar[_X: P] = P(CharIn("\u0020-\u007E"))
    def hexDigit[_X: P] = P(CharIn("0-9a-fA-F"))
    def UnicodeEscape[_X: P] = P("\\" ~ "u" ~ hexDigit.rep(min = 4, max = 4))
    def charEscapeSeq[_X: P] = P("\\" ~ CharIn("btnfr"))
    def escapeSeq[_X: P] = P(UnicodeEscape | charEscapeSeq)

    def op[_X: P] = P(opchar.rep(1)).!
    def idrest[_X: P] = P((letter | digit).rep ~ ("_" ~ op).?).!
    def varid[_X: P] = P(lower ~ idrest).!
    def boundvarid[_X: P] = P(varid | "`" ~ varid ~ "`").!
    def alphaid[_X: P] = P(upper ~ idrest | varid).!
    def plainid[_X: P] = P(alphaid | op).!
    def id[_X: P] = P(plainid | "`" ~ (CharPred(c => c != '`' && c != '\n') | escapeSeq).rep ~ "`").!
    def quoteId[_X: P] = P("'" ~ alphaid).!
    def spliceId[_X: P] = P("$" ~ alphaid).!

    def decimalNumeral[_X: P] = P("0" | digit ~ (digit | "_" ~ digit).rep)
    def hexNumeral[_X: P] = P("0" ~ ("x" | "X") ~ hexDigit ~ (hexDigit | "_" ~ hexDigit).rep)
    def integerLiteral[_X: P] = P((decimalNumeral | hexNumeral) ~ CharIn("Ll").?)

    def exponentPart[_X: P] = ???// P(CharIn("Ee") ~ CharIn("+-").? ~ digit ~ (digit | "_" ~ digit).rep)
    def floatType[_X: P] = P(CharIn("FfDd"))
    def floatingPointLiteral[_X: P] = P(
      (decimalNumeral.? ~ "." ~ digit ~ (digit | "_" ~ digit).rep ~ exponentPart.? ~ floatType.?) |
      (decimalNumeral ~ exponentPart ~ floatType.?) |
      (decimalNumeral ~ floatType)
    )

    def booleanLiteral[_X: P] = P("true" | "false")

    def charNoQuoteOrNewline[_X: P] = P(CharPred(c => c != '\'' && c != '\n'))
    def characterLiteral[_X: P] = P("'" ~ (charNoQuoteOrNewline | escapeSeq) ~ "'")

    def charNoDoubleQuoteOrNewline[_X: P] = P(CharPred(c => c != '"' && c != '\n'))
    def stringElement[_X: P] = P(charNoDoubleQuoteOrNewline | escapeSeq)
    def stringLiteral[_X: P] = P("\"" ~ stringElement.rep ~ "\"" | "\"\"\"" ~ (("\"".rep(min = 0, max = 2) ~ charNoDoubleQuoteOrNewline).rep ~ "\"".rep(min = 0, max = 2)).rep ~ "\"\"\"")

    def interpolatedStringPart[_X: P]: P[?] = P(printableChar.!.map(_(0)).filter(c => c != '"' && c != '$' && c != '\\') | escape)
    def escape[_X: P] = P("$$" | "$\"" | "$" ~ alphaid | "$" ~ BlockExpr)
    def interpolatedString[_X: P] = P(alphaid ~ "\"" ~ (("\\" ~ interpolatedStringPart | "\\\\" | "\\\"").rep ~ "\"") | alphaid ~ "\"\"\"" ~ (("\"".rep(min = 0, max = 2) ~ CharPred(c => c != '"' && c != '$')).rep ~ "\"".rep(min = 0, max = 2)).rep ~ "\"\"\"")

    def comment[_X: P] = P("/*" ~ (!"*/" ~ AnyChar).rep ~ "*/" | "//" ~ (!"\n" ~ AnyChar).rep)

    def nl[_X: P] = P(CharIn("\n") | "\r\n")
    def semi[_X: P] = P(";" | nl.rep(1))
  }

  object Literals {
    import Lexer._

    def SimpleLiteral[_X: P] = P(
      ("-" ~ integerLiteral | integerLiteral) |
      ("-" ~ floatingPointLiteral | floatingPointLiteral) |
      booleanLiteral |
      characterLiteral |
      stringLiteral
    )

    def Literal[_X: P] = P(
      SimpleLiteral |
      interpolatedString |
      quoteId |
      "null"
    )

    def QualId[_X: P] = P(id ~ ("." ~ id).rep)
    def ids[_X: P] = P(id ~ ("," ~ id).rep)

    def SimpleRef[_X: P] = P(
      id |
      (id.? ~ "." ~ "this") |
      (id.? ~ "." ~ "super" ~ ClassQualifier.? ~ "." ~ id)
    )

    def ClassQualifier[_X: P] = P("[" ~ id ~ "]")
  }

  object Key {
    import scalaparse.syntax.Basic

    def W[$: P](s: String) = P( s ~ !CharPred(Basic.LetterDigitDollarUnderscore) )(s"`$s`", implicitly)
  }

  object Keywords {
    def abstractKW[$: P] = Key.W("abstract")
    def caseKW[$: P] = Key.W("case")
    def catchKW[$: P] = Key.W("catch")
    def classKW[$: P] = Key.W("class")
    def defKW[$: P] = Key.W("def")
    def doKW[$: P] = Key.W("do")
    def elseKW[$: P] = Key.W("else")
    def enumKW[$: P] = Key.W("enum")
    def exportKW[$: P] = Key.W("export")
    def extendsKW[$: P] = Key.W("extends")
    def falseKW[$: P] = Key.W("false")
    def finalKW[$: P] = Key.W("final")
    def finallyKW[$: P] = Key.W("finally")
    def forKW[$: P] = Key.W("for")
    def givenKW[$: P] = Key.W("given")
    def ifKW[$: P] = Key.W("if")
    def implicitKW[$: P] = Key.W("implicit")
    def importKW[$: P] = Key.W("import")
    def lazyKW[$: P] = Key.W("lazy")
    def matchKW[$: P] = Key.W("match")
    def newKW[$: P] = Key.W("new")
    def nullKW[$: P] = Key.W("null")
    def objectKW[$: P] = Key.W("object")
    def overrideKW[$: P] = Key.W("override")
    def packageKW[$: P] = Key.W("package")
    def privateKW[$: P] = Key.W("private")
    def protectedKW[$: P] = Key.W("protected")
    def returnKW[$: P] = Key.W("return")
    def sealedKW[$: P] = Key.W("sealed")
    def superKW[$: P] = Key.W("super")
    def thenKW[$: P] = Key.W("then")
    def throwKW[$: P] = Key.W("throw")
    def traitKW[$: P] = Key.W("trait")
    def trueKW[$: P] = Key.W("true")
    def tryKW[$: P] = Key.W("try")
    def typeKW[$: P] = Key.W("type")
    def valKW[$: P] = Key.W("val")
    def varKW[$: P] = Key.W("var")
    def whileKW[$: P] = Key.W("while")
    def withKW[$: P] = Key.W("with")
    def yieldKW[$: P] = Key.W("yield")
    def colonKW[$: P] = Key.W(":")
    def equalsKW[$: P] = Key.W("=")
    def leftArrowKW[$: P] = Key.W("<-")
    def rightArrowKW[$: P] = Key.W("=>")
    def subtypeKW[$: P] = Key.W("<:")
    def supertypeKW[$: P] = Key.W(">:")
    def hashKW[$: P] = Key.W("#")
    def atKW[$: P] = Key.W("@")
    def doubleRightArrowKW[$: P] = Key.W("=>>")
    def questionRightArrowKW[$: P] = Key.W("?=>")
  }

  object Types {
    import Keywords._
    import Indenting._
    import Lexer._
    import Expressions._
    import ParamParsers._
    import StatementParsers._
    import BindingAndImportParsers._
    import Literals._

    def Type[_X: P]: P[?] = P(
      FunType |
      HkTypeParamClause ~ Key.W("=>>") ~ Type |
      FunParamClause ~ Key.W("=>>") ~ Type |
      MatchType |
      InfixType
    )

    def FunType[_X: P] = P(
      FunTypeArgs ~ (Key.W("=>") | Key.W("?=>")) ~ Type |
      HkTypeParamClause ~ Key.W("=>") ~ Type
    )

    def FunTypeArgs[_X: P] = P(
      InfixType |
      "(" ~ FunArgTypes.? ~ ")" |
      FunParamClause
    )

    def FunParamClause[_X: P] = P(
      "(" ~ TypedFunParam.rep(sep = ",") ~ ")"
    )

    def TypedFunParam[_X: P] = P(
      id ~ colon ~ Type
    )

    def MatchType[_X: P] = P(
      InfixType ~ matchKW ~ block(TypeCaseClauses)
    )

    def InfixType[_X: P] = P(
      RefinedType ~ (id ~ nl.? ~ RefinedType).rep
    )

    def RefinedType[_X: P] = P(
      AnnotType ~ (nl.? ~ Refinement).rep
    )

    def AnnotType[_X: P] = P(
      SimpleType ~ Annotation.rep
    )

    def SimpleType[_X: P]: P[?] = P(
      SimpleLiteral |
      "?" ~ TypeBounds |
      id |
      Singleton ~ "." ~ id |
      Singleton ~ "." ~ Key.W("type") |
      "(" ~ Types.? ~ ")" |
      Refinement |
      SimpleType ~ TypeArgs |
      SimpleType ~ Key.W("#") ~ id
    )

    def Singleton[_X: P]: P[?] = P(
      SimpleRef |
      SimpleLiteral |
      Singleton ~ "." ~ id
    )

    def FunArgType[_X: P] = P(
      Type |
      Key.W("=>") ~ Type
    )

    def FunArgTypes[_X: P] = P(
      FunArgType.rep(sep = ",")
    )

    def ParamType[_X: P] = P(
      Key.W("=>").? ~ ParamValueType
    )

    def ParamValueType[_X: P] = P(
      Type ~ Key.W("*").?
    )

    def TypeArgs[_X: P] = P(
      "[" ~ Types ~ "]"
    )

    def Refinement[_X: P] = P(
      colonBlock {
        RefineDcl.? ~ (semi ~ RefineDcl.?).rep
      }
    )

    def TypeBounds[_X: P] = P(
      (Key.W(">:") ~ Type).? ~ (Key.W("<:") ~ Type).?
    )

    def TypeParamBounds[_X: P] = P(
      TypeBounds ~ (colon ~ Type).rep
    )

    def Types[_X: P] = P(
      Type.rep(sep = ",")
    )
  }

  object Expressions {
    import ParamParsers._
    import Indenting._
    import BindingAndImportParsers._
    import Lexer._
    import Keywords._
    import Literals._
    import Types._
    import Literals._
    import StatementParsers._
    import Expressions._


    def Expr[_X: P]: P[?] = P(
      FunParams ~ (Key.W("=>") | Key.W("?=>")) ~ Expr |
      HkTypeParamClause ~ Key.W("=>") ~ Expr |
      Expr1
    )

    def BlockResult[_X: P] = P(
      FunParams ~ (Key.W("=>") | Key.W("?=>")) ~ Block |
      HkTypeParamClause ~ Key.W("=>") ~ Block |
      Expr1
    )

    def FunParams[_X: P] = P(
      Bindings |
      id |
      "_"
    )

    def Expr1[_X: P] = P(
      Key.W("inline").? ~ Key.W("if") ~ "(" ~ Expr ~ ")" ~ nl.? ~ Expr ~ (semi.? ~ Key.W("else") ~ Expr).? |
      Key.W("inline").? ~ Key.W("if") ~ Expr ~ Key.W("then") ~ Expr ~ (semi.? ~ Key.W("else") ~ Expr).? |
      Key.W("while") ~ "(" ~ Expr ~ ")" ~ nl.? ~ Expr |
      Key.W("while") ~ Expr ~ Key.W("do") ~ Expr |
      Key.W("try") ~ Expr ~ Catches ~ (Key.W("finally") ~ Expr).? |
      Key.W("try") ~ Expr ~ (Key.W("finally") ~ Expr).? |
      Key.W("throw") ~ Expr |
      Key.W("return") ~ Expr.? |
      ForExpr |
      (SimpleExpr ~ ".").? ~ id ~ "=" ~ Expr |
      PrefixOperator ~ SimpleExpr ~ "=" ~ Expr |
      SimpleExpr ~ ArgumentExprs ~ "=" ~ Expr |
      PostfixExpr ~ Ascription.? |
      Key.W("inline") ~ InfixExpr ~ MatchClause
    )

    def Ascription[_X: P] = P(
      colon ~ InfixType |
      colon ~ Annotation.rep
    )

    def Catches[_X: P] = P(
      Key.W("catch") ~ (Expr | ExprCaseClause)
    )

    def PostfixExpr[_X: P]: P[?] = P(
      InfixExpr ~ id.?
    )

    def InfixExpr[_X: P]: P[?] = P(
      PrefixExpr |
      InfixExpr ~ id ~ nl.? ~ InfixExpr |
      InfixExpr ~ id ~ ColonArgument |
      InfixExpr ~ MatchClause
    )

    def MatchClause[_X: P] = P(
      Key.W("match") ~ block(CaseClauses)
    )

    def PrefixExpr[_X: P] = P(
      PrefixOperator.? ~ SimpleExpr
    )

    inline val foo = "-+~!"
    def PrefixOperator[_X: P] = P(
      CharIn(foo).!
    )

    def SimpleExpr[_X: P]: P[?] = P(
      SimpleRef |
      Literal |
      "_" |
      BlockExpr |
      ExprSplice |
      Quoted |
      Lexer.quoteId |
      Key.W("new") ~ ConstrApp ~ (Key.W("with") ~ ConstrApp).rep ~ TemplateBody.? |
      Key.W("new") ~ TemplateBody |
      "(" ~ ExprsInParens.? ~ ")" |
      SimpleExpr ~ "." ~ id |
      SimpleExpr ~ "." ~ MatchClause |
      SimpleExpr ~ TypeArgs |
      SimpleExpr ~ ArgumentExprs |
      SimpleExpr ~ ColonArgument
    )

    def ColonArgument[_X: P] = P(
      colon ~ LambdaStart.? ~ indent ~ (CaseClauses | Block) ~ outdent
    )

    def LambdaStart[_X: P] = P(
      FunParams ~ (Key.W("=>") | Key.W("?=>")) |
      HkTypeParamClause ~ Key.W("=>")
    )

    def Quoted[_X: P] = P(
      "'" ~ "{" ~ Block ~ "}" |
      "'" ~ "[" ~ TypeBlock ~ "]"
    )

    def ExprSplice[_X: P] = P(
      spliceId |
      "$" ~ "{" ~ Block ~ "}" |
      "$" ~ "{" ~ Pattern ~ "}"
    )

    def ExprsInParens[_X: P] = P(
      ExprInParens.rep(sep = ",")
    )

    def ExprInParens[_X: P] = P(
      PostfixExpr ~ colon ~ Type | Expr
    )

    def ParArgumentExprs[_X: P] = P(
      "(" ~ ExprsInParens.? ~ ")" |
      "(" ~ Key.W("using") ~ ExprsInParens ~ ")" |
      "(" ~ ExprsInParens.? ~ "," ~ PostfixExpr ~ "*" ~ ")"
    )

    def ArgumentExprs[_X: P] = P(
      ParArgumentExprs |
      BlockExpr
    )

    def BlockExpr[_X: P]: P[?] = P(
      block(CaseClauses | Block)
    )

    def Block[_X: P]: P[?] = P(
      (BlockStat ~ semi).rep ~ BlockResult.?
    )

    def BlockStat[_X: P] = P(
      Import |
      (Annotation.rep ~ nl.?) ~ LocalModifier.rep ~ Def |
      Extension |
      Expr1 |
      EndMarker
    )

    def TypeBlock[_X: P] = P(
      (TypeBlockStat ~ semi).rep ~ Type
    )

    def TypeBlockStat[_X: P] = P(
      Key.W("type") ~ nl.? ~ TypeDef
    )

    def ForExpr[_X: P] = P(
      Key.W("for") ~ "(" ~ Enumerators0 ~ ")" ~ nl.? ~ (Key.W("do") | Key.W("yield")) ~ Expr |
      Key.W("for") ~ "{" ~ Enumerators0 ~ "}" ~ nl.? ~ (Key.W("do") | Key.W("yield")) ~ Expr |
      Key.W("for") ~ Enumerators0 ~ (Key.W("do") | Key.W("yield")) ~ Expr
    )

    def Enumerators0[_X: P] = P(
      nl.? ~ Enumerators ~ semi.?
    )

    def Enumerators[_X: P] = P(
      Generator ~ (semi ~ Enumerator | Guard).rep
    )

    def Enumerator[_X: P] = P(
      Generator |
      Guard ~ Guard.rep |
      Pattern1 ~ "=" ~ Expr
    )

    def Generator[_X: P] = P(
      Key.W("case").? ~ Pattern1 ~ Key.W("<-") ~ Expr
    )

    def Guard[_X: P] = P(
      Key.W("if") ~ PostfixExpr
    )

    def CaseClauses[_X: P] = P(
      CaseClause.rep
    )

    def CaseClause[_X: P] = P(
      Key.W("case") ~ Pattern ~ Guard.? ~ Key.W("=>") ~ Block
    )

    def ExprCaseClause[_X: P] = P(
      Key.W("case") ~ Pattern ~ Guard.? ~ Key.W("=>") ~ Expr
    )

    def TypeCaseClauses[_X: P] = P(
      TypeCaseClause.rep
    )

    def TypeCaseClause[_X: P] = P(
      Key.W("case") ~ (InfixType | "_") ~ Key.W("=>") ~ Type ~ semi.?
    )

    def Pattern[_X: P]: P[?] = P(
      Pattern1.rep(sep = "|")
    )

    def Pattern1[_X: P] = P(
      PatVar ~ colon ~ RefinedType |
      ("-" ~ integerLiteral | integerLiteral) ~ colon ~ RefinedType |
      ("-" ~ floatingPointLiteral | floatingPointLiteral) ~ colon ~ RefinedType |
      Pattern2
    )

    def Pattern2[_X: P] = P(
      (id ~ "@").? ~ InfixPattern
    )

    def InfixPattern[_X: P] = P(
      SimplePattern ~ (id ~ nl.? ~ SimplePattern).rep
    )

    def SimplePattern[_X: P] = P(
      PatVar |
      Literal |
      "(" ~ Patterns.? ~ ")" |
      Quoted |
      SimplePattern1 ~ TypeArgs.? ~ ArgumentPatterns.? |
      Key.W("given") ~ RefinedType
    )

    def SimplePattern1[_X: P]: P[?] = P(
      SimpleRef |
      SimplePattern1 ~ "." ~ id
    )

    def PatVar[_X: P] = P(
      varid |
      "_"
    )

    def Patterns[_X: P] = P(
      Pattern.rep(sep = ",")
    )

    def ArgumentPatterns[_X: P] = P(
      "(" ~ Patterns.? ~ ")" |
      "(" ~ Patterns.? ~ "," ~ PatVar ~ "*" ~ ")"
    )
  }

  object BindingAndImportParsers {
    import Lexer._
    import Literals._
    import Keywords._
    import Types._
    import Expressions._

    // Bindings
    def Bindings[_X: P] = P("(" ~ Binding.rep(sep = ",") ~ ")")
    def Binding[_X: P] = P((id | "_") ~ (":" ~ Type).?)

    // Modifiers
    def Modifier[_X: P] = P(LocalModifier | AccessModifier | "override" | "opaque")
    def LocalModifier[_X: P] = P("abstract" | "final" | "sealed" | "open" | "implicit" | "lazy" | "inline" | "transparent" | "infix")
    def AccessModifier[_X: P] = P(("private" | "protected") ~ AccessQualifier.?)
    def AccessQualifier[_X: P] = P("[" ~ id ~ "]")

    // Annotations
    def Annotation[_X: P]: P[?] = P("@" ~ SimpleType ~ ParArgumentExprs.rep)

    // Imports and Exports
    def Import[_X: P] = P("import" ~ ImportExpr.rep(sep = ","))
    def Export[_X: P] = P("export" ~ ImportExpr.rep(sep = ","))
    def ImportExpr[_X: P] = P((SimpleRef ~ ("." ~ id).rep ~ "." ~ ImportSpec) | (SimpleRef ~ "as" ~ id))
    def ImportSpec[_X: P] = P(NamedSelector | WildCardSelector | "{" ~ ImportSelectors ~ "}")
    def NamedSelector[_X: P] = P(id ~ ("as" ~ (id | "_")).?)
    def WildCardSelector[_X: P] = P("*" | "given" ~ InfixType.?)
    def ImportSelectors[_X: P]: P[?] = P((NamedSelector ~ ("," ~ ImportSelectors).?) | (WildCardSelector ~ ("," ~ WildCardSelector).rep))

    // End Markers
    def EndMarker[_X: P] = P("end" ~ EndMarkerTag ~ nl)
    def EndMarkerTag[_X: P] = P(id | ifKW | whileKW | forKW | matchKW | tryKW | newKW | Key.W("this") | givenKW | Key.W("extension") | valKW)
  }

  object ParamParsers {
    import Lexer._
    import Keywords._
    import BindingAndImportParsers._
    import Types._

    def ClsTypeParamClause[_X: P] = P("[" ~ ClsTypeParam.rep(sep = ",") ~ "]")
    def ClsTypeParam[_X: P] = P(Annotation.rep ~ (CharIn("+-").? ~ id ~ HkTypeParamClause.? ~ TypeParamBounds))

    def TypTypeParamClause[_X: P] = P("[" ~ TypTypeParam.rep(sep = ",") ~ "]")
    def TypTypeParam[_X: P] = P(Annotation.rep ~ id ~ HkTypeParamClause.? ~ TypeBounds)

    def HkTypeParamClause[_X: P]: P[?] = P("[" ~ HkTypeParam.rep(sep = ",") ~ "]")
    def HkTypeParam[_X: P] = P(Annotation.rep ~ (CharIn("+-").? ~ (id ~ HkTypeParamClause.? | "_") ~ TypeBounds))

    def ClsParamClauses[_X: P] = P(ClsParamClause.rep ~ (nl.? ~ "(" ~ ("implicit".? ~ ClsParams).? ~ ")").?)
    def ClsParamClause[_X: P] = P(nl.? ~ "(" ~ (ClsParams | ("using" ~ (ClsParams | FunArgTypes))) ~ ")")
    def ClsParams[_X: P] = P(ClsParam.rep(sep = ","))
    def ClsParam[_X: P] = P(Annotation.rep ~ (Modifier.rep ~ ("val" | "var").? ~ Param))

    def DefParamClauses[_X: P] = P(DefParamClause.rep(sep = nl))
    def DefParamClause[_X: P] = P(DefTypeParamClause | DefTermParamClause | UsingParamClause)
    def TypelessClauses[_X: P] = P(TypelessClause.rep(sep = nl))
    def TypelessClause[_X: P] = P(DefTermParamClause | UsingParamClause)

    def DefTypeParamClause[_X: P] = P(nl.? ~ "[" ~ DefTypeParam.rep(sep = ",") ~ "]")
    def DefTypeParam[_X: P] = P(Annotation.rep ~ id ~ HkTypeParamClause.? ~ TypeParamBounds)
    def DefTermParamClause[_X: P] = P(nl.? ~ "(" ~ DefTermParams.? ~ ")")
    def UsingParamClause[_X: P] = P(nl.? ~ "(" ~ "using" ~ (DefTermParams | FunArgTypes) ~ ")")
    def DefImplicitClause[_X: P] = P(nl.? ~ "(" ~ "implicit" ~ DefTermParams ~ ")")

    def DefTermParams[_X: P] = P(DefTermParam.rep(sep = ","))
    def DefTermParam[_X: P] = P(Annotation.rep ~ ("inline".? ~ Param))
    def Param[_X: P] = P(id ~ ":" ~ ParamType ~ ("=" ~ Expr).?)

    def ParamType[_X: P]: P[?] = P(id ~ colon ~ ParamType ~ P(equalsKW ~ Expr).?)
  }

  object StatementParsers {
    import Lexer._
    import Literals._
    import Indenting._
    import Keywords._
    import Types._
    import Expressions._
    import BindingAndImportParsers._
    import ParamParsers._

    // Refine Declarations
    def RefineDcl[_X: P] = P("val" ~ ValDcl | "def" ~ DefDcl | "var" ~ ValDcl | "type" ~ nl.? ~ TypeDef)
    def ValDcl[_X: P] = P(ids ~ ":" ~ Type)
    def DefDcl[_X: P] = P(DefSig ~ ":" ~ Type)

    // Definitions
    def Def[_X: P]: P[?] = P("val" ~ PatDef | "var" ~ PatDef | "def" ~ DefDef | "type" ~ nl.? ~ TypeDef | TmplDef)
    def PatDef[_X: P] = P(ids ~ (":" ~ Type).? ~ ("=" ~ Expr).? | Pattern2 ~ (":" ~ Type).? ~ ("=" ~ Expr).?)
    def DefDef[_X: P] = P(DefSig ~ (":" ~ Type).? ~ ("=" ~ Expr).? | "this" ~ TypelessClauses ~ DefImplicitClause.? ~ "=" ~ ConstrExpr)
    def DefSig[_X: P] = P(id ~ DefParamClauses.? ~ DefImplicitClause.?)
    def TypeDef[_X: P] = P(id ~ HkTypeParamClause.? ~ FunParamClause.rep ~ TypeBounds ~ ("=" ~ Type).?)

    // Template Definitions
    def TmplDef[_X: P] = P(("case".? ~ "class" | "trait") ~ ClassDef | "case".? ~ "object" ~ ObjectDef | "enum" ~ EnumDef | "given" ~ GivenDef)
    def ClassDef[_X: P] = P(id ~ ClassConstr ~ Template.?)
    def ClassConstr[_X: P] = P(ClsTypeParamClause.? ~ ConstrMods.? ~ ClsParamClauses)
    def ConstrMods[_X: P] = P(Annotation.rep ~ AccessModifier.?)
    def ObjectDef[_X: P] = P(id ~ Template.?)
    def EnumDef[_X: P] = P(id ~ ClassConstr ~ InheritClauses ~ EnumBody)
    def GivenDef[_X: P] = P(GivenSig.? ~ (AnnotType ~ ("=" ~ Expr).? | StructuralInstance))
    def GivenSig[_X: P] = P(id.? ~ DefTypeParamClause.? ~ UsingParamClause.rep ~ ":")
    def GivenType[_X: P] = P(AnnotType ~ (id ~ nl.? ~ AnnotType).rep)
    def StructuralInstance[_X: P] = P(ConstrApp ~ (withKW ~ ConstrApp).rep ~ (withKW ~ WithTemplateBody).?)
    def Extension[_X: P] = P("extension" ~ DefTypeParamClause.? ~ UsingParamClause.rep ~ "(" ~ DefTermParam ~ ")" ~ UsingParamClause.rep ~ ExtMethods)
    def ExtMethods[_X: P] = P(ExtMethod | block(ExtMethod.rep(sep = semi)))
    def ExtMethod[_X: P] = P((Annotation.rep ~ nl.? ~ Modifier.rep ~ defKW ~ DefDef) | Export)
    def Template[_X: P] = P(InheritClauses ~ TemplateBody.?)
    def InheritClauses[_X: P] = P(("extends" ~ ConstrApps).? ~ ("derives" ~ QualId.rep(sep = ",")).?)
    def ConstrApps[_X: P] = P(ConstrApp.rep(sep = "," | withKW))
    def ConstrApp[_X: P] = P(SimpleType ~ Annotation.rep ~ ParArgumentExprs.rep)
    def ConstrExpr[_X: P] = P(SelfInvocation | block(SelfInvocation.rep(sep = semi)))
    def SelfInvocation[_X: P] = P("this" ~ ArgumentExprs.rep)

    def WithTemplateBody[_X: P] = P(block(SelfType.? ~ TemplateStat.rep(sep = semi)))
    def TemplateBody[_X: P]: P[?] = P(colonBlock(SelfType.? ~ TemplateStat.rep(sep = semi)))
    def TemplateStat[_X: P] = P(Import | Export | Annotation.rep ~ nl.? ~ Modifier.rep ~ Def | Extension | Expr1 | EndMarker | Pass)
    def SelfType[_X: P] = P(id ~ ":" ~ InfixType ~ "=>" | "this" ~ ":" ~ InfixType ~ "=>")

    def EnumBody[_X: P] = P(colonBlock(SelfType.? ~ EnumStat.rep(sep = semi)))
    def EnumStat[_X: P] = P(TemplateStat | Annotation.rep ~ nl.? ~ Modifier.rep ~ EnumCase)
    def EnumCase[_X: P] = P("case" ~ (id ~ ClassConstr ~ ("extends" ~ ConstrApps).? | ids))

    // Top-level Statements
    def TopStats[_X: P]: P[?] = P(TopStat.rep(sep = semi))
    def TopStat[_X: P] = P(Import | Export | Annotation.rep ~ nl.? ~ Modifier.rep ~ Def | Extension | Packaging | PackageObject | EndMarker | Pass)
    def Packaging[_X: P] = P("package" ~ QualId ~ colonBlock(TopStats))
    def PackageObject[_X: P] = P("package" ~ "object" ~ ObjectDef)

    def CompilationUnit[_X: P] = P(("package" ~ QualId ~ semi).rep ~ TopStats)
  }

  object Indenting {
    import Lexer._

    // Indentation handling
    var indentationStack: List[Int] = List(0)

    def indent[_X: P] = P {
      val currentIndent = indentationStack.head
      val nextIndent = P(ws ~ nl ~ " ".rep(1).!.map(_.length)).filter(_ > currentIndent)
      nextIndent.map { newIndent =>
        indentationStack = newIndent :: indentationStack
      }
    }

    def outdent[_X: P] = P {
      val currentIndent = indentationStack.head
      val nextIndent = P(ws ~ nl ~ " ".rep.!.map(_.length)).filter(_ < currentIndent)
      nextIndent.map { newIndent =>
        while (indentationStack.head > newIndent) {
          indentationStack = indentationStack.tail
        }
      }
    }

    def colon[_X: P] = P(":")
    def block[_X: P, T](ts: P[T]) = P("{" ~ ts ~ "}" | indent ~ ts ~ outdent)
    def colonBlock[_X: P, T](ts: P[T]) = P(nl.? ~ "{" ~ ts ~ "}" | colon ~ indent ~ ts ~ outdent)
  }
}

/**
 * Fastparse parser that extends the Scalaparse parser to handle `build.mill` and
 * other script files, and also for subsequently parsing any magic import
 * statements into [[ImportTree]] structures for the [[MillBuildRootModule]] to use
 */
@internal
object Parsers {
  import fastparse._
  import ScalaWhitespace._
  import scalaparse.Scala._

  def ImportSplitter[$: P]: P[Seq[ImportTree]] = {
    def IdParser = P((Id | Underscore).!).map(s => if (s(0) == '`') s.drop(1).dropRight(1) else s)
    def Selector: P[(String, Option[String])] = P(IdParser ~ (`=>` ~/ IdParser).?)
    def Selectors = P("{" ~/ Selector.rep(sep = ","./) ~ "}")
    def BulkImport = P(Underscore).map(_ => Seq("_" -> None))
    def Prefix = P((IdParser ~ Index).rep(1, sep = "."))
    def Suffix = P("." ~/ (BulkImport | Selectors))
    def ImportExpr: P[ImportTree] = {
      // Manually use `WL0` parser here, instead of relying on WhitespaceApi, as
      // we do not want the whitespace to be consumed even if the WL0 parser parses
      // to the end of the input (which is the default behavior for WhitespaceApi)
      P(Index ~~ Prefix ~~ (WL0 ~~ Suffix).? ~~ Index).map {
        case (start, idSeq, selectors, end) =>
          selectors match {
            case Some(selectors) => ImportTree(idSeq, selectors, start, end)
            case None => ImportTree(idSeq.init, Seq(idSeq.last._1 -> None), start, end)
          }
      }
    }
    P(`import` ~/ ImportExpr.rep(1, sep = ","./))
  }

  def Prelude[$: P] = P((Annot ~ OneNLMax).rep ~ (Mod ~/ Pass).rep)

  def TmplStat[$: P] = P(Import | Prelude ~ BlockDef | StatCtx.Expr)

  def HashBang[$: P] = P(Start ~~ "#!" ~~ CharsWhile(_ != '\n') ~~ "\n")
  // Do this funny ~~WS thing to make sure we capture the whitespace
  // together with each statement; otherwise, by default, it gets discarded.
  //
  // After each statement, there must either be `Semis`, a "}" marking the
  // end of the block, or the `End` of the input
  def StatementBlock[$: P]: P[Seq[String]] =
    P(Semis.? ~ (TmplStat ~~ WS ~~ (Semis | &("}") | End)).!.repX)

  def TopPkgSeq[$: P]: P[Seq[String]] =
    P(((scalaparse.Scala.`package` ~ QualId.!) ~~ !(WS ~ "{")).repX(1, Semis))

  def CompilationUnit[$: P]: P[(Option[Seq[String]], String, Seq[String])] =
    P(Semis.? ~ TopPkgSeq.? ~~ WL.! ~~ StatementBlock ~ WL ~ End)

  def parseImportHooksWithIndices(stmts: Seq[String]): Seq[(String, Seq[ImportTree])] = {
    val hookedStmts = mutable.Buffer.empty[(String, Seq[ImportTree])]
    for (stmt <- stmts) {
      // Call `fastparse.ParserInput.fromString` explicitly, to avoid generating a
      // lambda in the class body and making the we-do-not-load-fastparse-on-cached-scripts
      // test fail
      parse(fastparse.ParserInput.fromString(stmt), ImportSplitter(using _)) match {
        case f: Parsed.Failure => hookedStmts.append((stmt, Nil))
        case Parsed.Success(parsedTrees, _) =>
          val importTrees = mutable.Buffer.empty[ImportTree]
          for (importTree <- parsedTrees) {
            importTree.prefix match {
              case Seq((s"$$$rest", _), _*) => importTrees.append(importTree)
              case _ => // donothing
            }
          }
          hookedStmts.append((stmt, importTrees.toSeq))
      }
    }
    hookedStmts.toSeq
  }
  def formatFastparseError(fileName: String, rawCode: String, f: Parsed.Failure): String = {

    val lineColIndex = f.extra.input.prettyIndex(f.index)
    val expected = f.trace().failure.label
    val locationString = {
      val (first, last) = rawCode.splitAt(f.index)
      val lastSnippet = last.split(newLine).headOption.getOrElse("")
      val firstSnippet = first.reverse
        .split(newLine.reverse)
        .lift(0).getOrElse("").reverse
      firstSnippet + lastSnippet + newLine + (" " * firstSnippet.length) + "^"
    }
    s"$fileName:$lineColIndex expected $expected$newLine$locationString"
  }

  /**
   * Splits up a script file into its constituent blocks, each of which
   * is a tuple of (leading-whitespace, statements). Leading whitespace
   * is returned separately so we can later manipulate the statements e.g.
   * by adding `val res2 = ` without the whitespace getting in the way
   */
  def splitScript(rawCode: String, fileName: String): Either[String, (Seq[String], Seq[String])] = {
    parse(rawCode, CompilationUnit(using _)) match {
      case f: Parsed.Failure => Left(s"parse[0]: ${formatFastparseError(fileName, rawCode, f)}")
      case s: Parsed.Success[(Option[Seq[String]], String, Seq[String])] =>
        Right(s.value._1.toSeq.flatten -> (Seq(s.value._2) ++ s.value._3))
    }
  }
}
