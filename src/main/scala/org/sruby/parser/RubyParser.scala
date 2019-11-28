package org.sruby.parser

import org.sruby.exceptions.ParserError
import org.sruby.lexer._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

object RubyParser extends Parsers {
  // Public members
  override type Elem = Token

  val keywords = List("def", "end", "true", "false", "class", "if", "elsif", "unless", "else",
    "nil", "self")

  def apply(tokens: Seq[Token]): Either[ParserError, List[AST]] = {
    val reader = new TokenReader(tokens)

    phrase(repsep(definitionParser | expressionParser, SeparatorToken))(reader) match {
      case NoSuccess(msg, next) => Left(ParserError(msg))
      case Success(result, next) => Right(result)
    }
  }

  // Private methods
  private def expressionParser: Parser[AST] = ivarAssignmentParser |
    localVarAssignmentParser | conditionalParser | invocationParser | selfParser |
    ivarIdentifierParser | nonKeywordIdentifierParser | literalParser

  // scalastyle:off method.length
  private def definitionParser: Parser[AST] = {
    def klassDefParser: Parser[KlassDefNode] = {
      def klassTokenParser: Parser[Token] = elem(IdentifierToken("class"))
      def klassNameParser: Parser[ConstantNode] =
        klassTokenParser ~> constantParser <~ SeparatorToken

      (klassNameParser ~ repsepWithNewline(definitionParser) <~ endTokenParser) ^^ {
        case ConstantNode(name) ~ expressionParsers => KlassDefNode(name, expressionParsers)
      }
    }

    def methodDefParser: Parser[MethodDefNode] = {
      def defTokenParser: Parser[Token] = elem(IdentifierToken("def"))

      def paramListParser: Parser[List[IdentifierNode]] = OpeningParenthesisToken ~>
        repsep(nonKeywordIdentifierParser, CommaToken) <~ ClosingParenthesisToken

      def methodBodyParser: Parser[List[AST]] =
        repsepWithNewline(expressionParser) <~ endTokenParser

      def instanceMethodDefParser: Parser[InstanceMethodDefNode] = {
        def instanceMethodHeaderParser: Parser[IdentifierNode ~ Option[List[IdentifierNode]]] = {
          def methodNameParser: Parser[IdentifierNode] = defTokenParser ~>
            identifierParser

          methodNameParser ~ paramListParser.?
        }

        (instanceMethodHeaderParser ~ SeparatorToken ~ methodBodyParser) ^^ {
          case IdentifierNode(name) ~ Some(params) ~ _ ~ expressionParsers =>
            InstanceMethodDefNode(name, params.map(_.name), expressionParsers)
          case IdentifierNode(name) ~ None ~ _ ~ expressionParsers =>
            InstanceMethodDefNode(name, List.empty[Symbol], expressionParsers)
        }
      }

      def klassMethodDefParser: Parser[KlassMethodDefNode] = {
        def klassMethodHeaderParser: Parser[IdentifierNode ~ Option[List[IdentifierNode]]] = {
          def methodNameParser: Parser[IdentifierNode] = defTokenParser ~> selfParser ~>
            PeriodToken ~> identifierParser

          methodNameParser ~ paramListParser.?
        }

        (klassMethodHeaderParser ~ SeparatorToken ~ methodBodyParser) ^^ {
          case IdentifierNode(name) ~ Some(params) ~ _ ~ expressionParsers =>
            KlassMethodDefNode(name, params.map(_.name), expressionParsers)
          case IdentifierNode(name) ~ None ~ _ ~ expressionParsers =>
            KlassMethodDefNode(name, List.empty[Symbol], expressionParsers)
        }
      }

      instanceMethodDefParser.|[MethodDefNode](klassMethodDefParser)
    }

    klassDefParser | methodDefParser
  }
  // scalastyle:on method.length

  private def conditionalParser: Parser[ConditionalNode] = {
    def ifParser: Parser[IfNode] = {
      def ifTokenParser: Parser[Token] = elem(IdentifierToken("if"))
      def elseTokenParser: Parser[Token] = elem(IdentifierToken("else"))
      val predicateParser: Parser[AST] =
        ifTokenParser ~> expressionParser <~ SeparatorToken
      val elseParser: Parser[List[AST]] =
        elseTokenParser ~> SeparatorToken ~> repsepWithNewline(expressionParser)

      def elsifParser: Parser[List[AST] => IfNode] = {
        def elsifTokenParser: Parser[Token] = elem(IdentifierToken("elsif"))

        (elsifTokenParser ~> expressionParser ~ SeparatorToken ~
            repsepWithNewline(expressionParser)) ^^ {
          case predicate ~ _ ~ yesBranch =>
            (noBranch: List[AST]) => IfNode(predicate, yesBranch, noBranch)
        }
      }

      val build: (List[AST] => IfNode, List[AST]) => List[IfNode] =
        (acc, ifExp) => List(acc(ifExp))

      (predicateParser ~ repsepWithNewline(expressionParser) ~ rep(elsifParser) ~ elseParser.?
          <~ endTokenParser) ^^ {
        case predicate ~ yesBranch ~ elsifs ~ Some(noBranch) =>
          IfNode(predicate, yesBranch, elsifs.foldRight(noBranch)(build))
        case predicate ~ yesBranch ~ elsifs ~ None =>
          IfNode(predicate, yesBranch, elsifs.foldRight(List.empty[IfNode])(build))
      }
    }

    def unlessParser: Parser[UnlessNode] = {
      def unlessToken: Parser[Token] = elem(IdentifierToken("unless"))

      (unlessToken ~ expressionParser ~ SeparatorToken ~ repsepWithNewline(expressionParser) ~
          endTokenParser) ^^ {
        case _ ~ predicate ~ _ ~ expressionParsers ~ _ => UnlessNode(predicate, expressionParsers)
      }
    }

    ifParser.|[ConditionalNode](unlessParser)
  }

  // scalastyle:off method.length
  private def invocationParser: Parser[InvocationNode] = {
    def leftmostInvocationParser: Parser[InvocationNode] = {
      def innerInvocationParser: Parser[Option[AST] => InvocationNode] =
        (PeriodToken ~> messageParser(identifierParser) ~ innerInvocationParser.?) ^^ {
          case partialInvocation ~ None => partialInvocation
          case partialInvocation ~ Some(innerPartialInvocation) => (receiver: Option[AST]) =>
            innerPartialInvocation(Some(partialInvocation(receiver)))
        }

      def messageInvocationParser: Parser[InvocationNode] =
        (messageParser(nonKeywordIdentifierParser) ~ innerInvocationParser.?) ^^ {
          case partialInvocation ~ None => partialInvocation(None)
          case leftmostPartialInvocation ~ Some(partialInvocation) =>
            partialInvocation(Some(leftmostPartialInvocation(None)))
        }

      def nonMessageInvocationParser: Parser[InvocationNode] = {
        def nonMessageReceiverParser: Parser[AST] = literalParser | conditionalParser |
          ivarIdentifierParser | constantParser

        (nonMessageReceiverParser ~ innerInvocationParser) ^^ {
          case receiver ~ partialInvocation => partialInvocation(Some(receiver))
        }
      }

      messageInvocationParser | nonMessageInvocationParser
    }

    def messageParser(id: Parser[IdentifierNode]): Parser[Option[AST] => InvocationNode] = {
      def messageNoArgsParser: Parser[Option[AST] => InvocationNode] = id ^^ {
        case IdentifierNode(name) => (receiver: Option[AST]) => receiver match {
          case Some(r) => InvocationWithReceiverNode(r, name, Nil)
          case None => InvocationWithImplicitReceiverNode(name, Nil)
        }
      }

      def messageNoParensParser: Parser[Option[AST] => InvocationNode] =
        (id ~ rep1sep(expressionParser, CommaToken) ~> failure("no") |
            id ~ rep1sep(expressionParser, CommaToken)) ^^ {
          case IdentifierNode(name) ~ args => (receiver: Option[AST]) => receiver match {
            case Some(r) => InvocationWithReceiverNode(r, name, args)
            case None => InvocationWithImplicitReceiverNode(name, args)
          }
      }

      def messageParensParser: Parser[Option[AST] => InvocationNode] =
        (id ~ OpeningParenthesisToken ~ repsep(expressionParser, CommaToken)
            ~ ClosingParenthesisToken) ^^ {
          case IdentifierNode(name) ~ _ ~ args ~ _ =>
            (receiver: Option[AST]) => receiver match {
              case Some(r) => InvocationWithReceiverNode(r, name, args)
              case None => InvocationWithImplicitReceiverNode(name, args)
            }
        }

      messageParensParser | messageNoParensParser | messageNoArgsParser
    }

    leftmostInvocationParser
  }
  // scalastyle:on method.length

  private def ivarAssignmentParser: Parser[AST] =
    (IvarPrefixToken.~>[AST]((identifierParser.|[AST](constantParser))) ~ AssignerToken ~
        expressionParser) ^^ {
      case IdentifierNode(name) ~ _ ~ value => IvarAssignmentNode(name, value)
    }

  private def localVarAssignmentParser: Parser[AST] =
    (nonKeywordIdentifierParser ~ AssignerToken ~ expressionParser) ^^ {
      case IdentifierNode(name) ~ _ ~ value => LocalVarAssignmentNode(name, value)
    }

  private def selfParser: Parser[SelfNode.type] =
    elem(IdentifierToken("self")) ^^ { case _ => SelfNode }

  private def constantParser: Parser[ConstantNode] =
    accept("constant", { case ConstantToken(name) => ConstantNode(Symbol(name)) })

  private def identifierParser: Parser[IdentifierNode] =
    accept("identifier", { case IdentifierToken(name) => IdentifierNode(Symbol(name)) })

  private def nonKeywordIdentifierParser: Parser[IdentifierNode] = accept("identifier", {
    case IdentifierToken(name) if !keywords.contains(name) => IdentifierNode(Symbol(name))
  })

  private def ivarIdentifierParser: Parser[IvarIdentifierNode] =
    (IvarPrefixToken ~> identifierParser) ^^ {
      case IdentifierNode(name) => IvarIdentifierNode(name)
    }

  private def endTokenParser: Parser[Token] = elem(IdentifierToken("end"))

  private def literalParser: Parser[AST] = accept("literal", {
    case StringToken(s) => StringNode(s)
    case SymbolToken(s) => SymbolNode(Symbol(s))
    case IntegerToken(n) => IntegerNode(n)
    case FloatToken(n) => FloatNode(n)
    case IdentifierToken("true") => TrueNode
    case IdentifierToken("false") => FalseNode
    case IdentifierToken("nil") => NilNode
  })

  private def repsepWithNewline[A](parser: Parser[A]): Parser[List[A]] =
    (repsep(parser, SeparatorToken) <~ SeparatorToken).? ^^ {
      case Some(as) => as
      case None => List.empty[A]
    }
}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.headOption match {
    case Some(token) => token
    case None => throw new Exception("attempted to retrieve token from empty list")
  }

  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new TokenReader(tokens.drop(1))
}
