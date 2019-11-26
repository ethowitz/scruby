package org.sruby.parser

import org.sruby.exceptions.ParserError
import org.sruby.lexer._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

object RubyParser extends Parsers {
  // Types
  override type Elem = Token

  // Attributes
  val keywords = List("def", "end", "true", "false", "class", "if", "elsif", "unless", "else",
    "nil")


  // Public methods
  def apply(tokens: Seq[Token]): Either[ParserError, List[AST]] = {
    val reader = new TokenReader(tokens)

    phrase(repsep(definitionPattern | expressionPattern, SeparatorToken))(reader) match {
      case NoSuccess(msg, next) => Left(ParserError(msg))
      case Success(result, next) => Right(result)
    }
  }

  // Private methods
  private def expressionPattern: Parser[AST] = ivarAssignmentPattern |
    localVarAssignmentPattern | conditionalPattern | invocationPattern | ivarIdentifierPattern |
    nonKeywordIdentifierPattern | literalPattern

  private def definitionPattern: Parser[AST] = {
    def klassDefPattern: Parser[KlassDefNode] = {
      def klassTokenPattern: Parser[Token] = elem(IdentifierToken("class"))
      def klassNamePattern: Parser[ConstantNode] =
        klassTokenPattern ~> constantPattern <~ SeparatorToken

      (klassNamePattern ~ repsepWithNewline(definitionPattern) <~ endTokenPattern) ^^ {
        case ConstantNode(name) ~ expressionPatterns => KlassDefNode(name, expressionPatterns)
      }
    }

    def methodDefPattern: Parser[MethodDefNode] = {
      def methodHeaderPattern: Parser[IdentifierNode ~ Option[List[IdentifierNode]]] = {
        def defTokenPattern: Parser[Token] = elem(IdentifierToken("def"))
        def methodNamePattern: Parser[IdentifierNode] = defTokenPattern ~> identifierPattern
        def paramListPattern: Parser[List[IdentifierNode]] = OpeningParenthesisToken ~>
          repsep(nonKeywordIdentifierPattern, CommaToken) <~ ClosingParenthesisToken

        methodNamePattern ~ paramListPattern.?
      }

      def methodBodyPattern: Parser[List[AST]] =
        repsepWithNewline(expressionPattern) <~ endTokenPattern

      (methodHeaderPattern ~ SeparatorToken ~ methodBodyPattern) ^^ {
        case IdentifierNode(name) ~ Some(params) ~ _ ~ expressionPatterns =>
          MethodDefNode(name, params.map(_.name), expressionPatterns)
        case IdentifierNode(name) ~ None ~ _ ~ expressionPatterns =>
          MethodDefNode(name, List.empty[Symbol], expressionPatterns)
      }
    }

    klassDefPattern.|[AST](methodDefPattern)
  }

  private def conditionalPattern: Parser[AST] = {
    def ifPattern: Parser[IfNode] = {
      def ifTokenPattern: Parser[Token] = elem(IdentifierToken("if"))
      def elseTokenPattern: Parser[Token] = elem(IdentifierToken("else"))
      val predicatePattern: Parser[AST] =
        ifTokenPattern ~> expressionPattern <~ SeparatorToken
      val elsePattern: Parser[List[AST]] =
        elseTokenPattern ~> SeparatorToken ~> repsepWithNewline(expressionPattern)

      def elsifPattern: Parser[List[AST] => IfNode] = {
        def elsifTokenPattern: Parser[Token] = elem(IdentifierToken("elsif"))

        (elsifTokenPattern ~> expressionPattern ~ SeparatorToken ~
            repsepWithNewline(expressionPattern)) ^^ {
          case predicate ~ _ ~ yesBranch =>
            (noBranch: List[AST]) => IfNode(predicate, yesBranch, noBranch)
        }
      }

      val build: (List[AST] => IfNode, List[AST]) => List[IfNode] =
        (acc, ifExp) => List(acc(ifExp))

      (predicatePattern ~ repsepWithNewline(expressionPattern) ~ rep(elsifPattern) ~ elsePattern.?
          <~ endTokenPattern) ^^ {
        case predicate ~ yesBranch ~ elsifs ~ Some(noBranch) =>
          IfNode(predicate, yesBranch, elsifs.foldRight(noBranch)(build))
        case predicate ~ yesBranch ~ elsifs ~ None =>
          IfNode(predicate, yesBranch, elsifs.foldRight(List.empty[IfNode])(build))
      }
    }

    def unlessPattern: Parser[UnlessNode] = {
      def unlessToken: Parser[Token] = elem(IdentifierToken("unless"))

      (unlessToken ~ expressionPattern ~ SeparatorToken ~ repsepWithNewline(expressionPattern) ~
          endTokenPattern) ^^ {
        case _ ~ predicate ~ _ ~ expressionPatterns ~ _ => UnlessNode(predicate, expressionPatterns)
      }
    }

    ifPattern.|[AST](unlessPattern)
  }

  // scalastyle:off method.length
  private def invocationPattern: Parser[InvocationNode] = {
    def leftmostInvocationPattern: Parser[InvocationNode] = {
      def innerInvocationPattern: Parser[Option[AST] => InvocationNode] = {
        (PeriodToken ~> messagePattern(identifierPattern) ~ innerInvocationPattern.?) ^^ {
          case partialInvocation ~ None => partialInvocation
          case partialInvocation ~ Some(innerPartialInvocation) => (receiver: Option[AST]) =>
            innerPartialInvocation(Some(partialInvocation(receiver)))
        }
      }

      def messageInvocationPattern: Parser[InvocationNode] = {
        (messagePattern(nonKeywordIdentifierPattern) ~ innerInvocationPattern.?) ^^ {
          case partialInvocation ~ None => partialInvocation(None)
          case leftmostPartialInvocation ~ Some(partialInvocation) =>
            partialInvocation(Some(leftmostPartialInvocation(None)))
        }
      }

      def nonMessageInvocationPattern: Parser[InvocationNode] = {
        def nonMessageReceiverPattern: Parser[AST] = literalPattern | conditionalPattern |
          ivarIdentifierPattern | constantPattern

        (nonMessageReceiverPattern ~ innerInvocationPattern) ^^ {
          case receiver ~ partialInvocation => partialInvocation(Some(receiver))
        }
      }

      messageInvocationPattern | nonMessageInvocationPattern
    }

    def messagePattern(id: Parser[IdentifierNode]): Parser[Option[AST] => InvocationNode] = {
      def messageNoArgsPattern: Parser[Option[AST] => InvocationNode] = id ^^ {
        case IdentifierNode(name) => (receiver: Option[AST]) => receiver match {
          case Some(r) => InvocationWithReceiverNode(r, name, Nil)
          case None => InvocationWithImplicitReceiverNode(name, Nil)
        }
      }

      def messageNoParensPattern: Parser[Option[AST] => InvocationNode] = {
        (id ~ rep1sep(expressionPattern, CommaToken) ~> failure("no") |
            id ~ rep1sep(expressionPattern, CommaToken)) ^^ {
          case IdentifierNode(name) ~ args => (receiver: Option[AST]) => receiver match {
            case Some(r) => InvocationWithReceiverNode(r, name, args)
            case None => InvocationWithImplicitReceiverNode(name, args)
          }
        }
      }

      def messageParensPattern: Parser[Option[AST] => InvocationNode] = {
        (id ~ OpeningParenthesisToken ~ repsep(expressionPattern, CommaToken)
            ~ ClosingParenthesisToken) ^^ {
          case IdentifierNode(name) ~ _ ~ args ~ _ =>
            (receiver: Option[AST]) => receiver match {
              case Some(r) => InvocationWithReceiverNode(r, name, args)
              case None => InvocationWithImplicitReceiverNode(name, args)
            }
        }
      }

      messageParensPattern | messageNoParensPattern | messageNoArgsPattern
    }

    leftmostInvocationPattern
  }
  // scalastyle:on method.length

  private def ivarAssignmentPattern: Parser[AST] = {
    (IvarPrefixToken ~> (identifierPattern.|[AST](constantPattern)) ~ AssignerToken ~
        expressionPattern) ^^ {
      case IdentifierNode(name) ~ _ ~ value => IvarAssignmentNode(name, value)
    }
  }

  private def localVarAssignmentPattern: Parser[AST] =
    (nonKeywordIdentifierPattern ~ AssignerToken ~ expressionPattern) ^^ {
      case IdentifierNode(name) ~ _ ~ value => LocalVarAssignmentNode(name, value)
    }

  private def constantPattern: Parser[ConstantNode] =
    accept("constant", { case ConstantToken(name) => ConstantNode(Symbol(name)) })

  private def identifierPattern: Parser[IdentifierNode] =
    accept("identifier", { case IdentifierToken(name) => IdentifierNode(Symbol(name)) })

  private def nonKeywordIdentifierPattern: Parser[IdentifierNode] = accept("identifier", {
    case IdentifierToken(name) if !keywords.contains(name) => IdentifierNode(Symbol(name))
  })

  private def ivarIdentifierPattern: Parser[IvarIdentifierNode] = {
    (IvarPrefixToken ~> identifierPattern) ^^ {
      case IdentifierNode(name) => IvarIdentifierNode(name)
    }
  }

  private def endTokenPattern: Parser[Token] = elem(IdentifierToken("end"))

  private def literalPattern: Parser[AST] = {
    accept("literal", {
      case StringToken(s) => StringNode(s)
      case SymbolToken(s) => SymbolNode(Symbol(s))
      case IntegerToken(n) => IntegerNode(n)
      case FloatToken(n) => FloatNode(n)
      case IdentifierToken("true") => TrueNode
      case IdentifierToken("false") => FalseNode
      case IdentifierToken("nil") => NilNode
    })
  }

  private def repsepWithNewline[A](parser: Parser[A]): Parser[List[A]] = {
    (repsep(parser, SeparatorToken) <~ SeparatorToken).? ^^ {
      case Some(as) => as
      case None => List.empty[A]
    }
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
