/*
 * Copyright (C) 2014 Lymia Aluysia <lymiahugs@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is furnished
 * to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package moe.lymia.joustext

import ast._

object Parser extends scala.util.parsing.combinator.RegexParsers {
  def identifier = "[a-zA-Z_][a-zA-Z0-9_]*".r

  // Value
  object valueParsers {
    // Adapted from http://stackoverflow.com/a/11533809/1733590
    def variable   = "$" ~> identifier ^^ Value.Variable.apply
    def constant   = "-?[0-9]+".r ^^ (_.toInt) ^^ Value.Constant.apply

    def defer(f: (Value, Value) => Value) = (y: Value) => (x: Value) => f(x, y)
    def plus   = "+" ~> term   ^^ defer(Value.Add.apply)
    def minus  = "-" ~> term   ^^ defer(Value.Sub.apply)
    def times  = "*" ~> factor ^^ defer(Value.Mul.apply)
    def divide = "/" ~> factor ^^ defer(Value.Div.apply)
    def mod    = "%" ~> factor ^^ defer(Value.Mod.apply)

    def unNeg : Parser[Value] = (("-" ~> constant)          |
                                 ("-" ~> variable)          |
                                 ("-" ~> "(" ~> expr <~ ")")) ^^ (x => Value.Sub(Value.Constant(0), x))

    def join(parse: Value ~ Seq[Value=>Value]) = parse._2.foldLeft(parse._1)((a, f) => f(a))
    def expr  : Parser[Value] = term ~ (plus | minus).*           ^^ join
    def term  : Parser[Value] = factor ~ (times | divide | mod).* ^^ join
    def factor: Parser[Value] = constant | variable | unNeg | ("(" ~> expr <~ ")")
  }
  def value = valueParsers.constant | valueParsers.variable | ("(" ~> valueParsers.expr <~ ")")
  def expr  = valueParsers.expr

  // Predicate
  object predicateParsers {
    def comparison = ((expr <~ "==") ~ expr ^^ {case x~y => Predicate.Equals(x, y)}) |
                     ((expr <~ "!=") ~ expr ^^ {case x~y => Predicate.Not(Predicate.Equals(x, y))}) |
                     ((expr <~ "<" ) ~ expr ^^ {case x~y => Predicate.LessThan(x, y)}) |
                     ((expr <~ ">" ) ~ expr ^^ {case x~y => Predicate.GreaterThan(x, y)}) |
                     ((expr <~ "<=") ~ expr ^^ {case x~y => Predicate.Not(Predicate.GreaterThan(x, y))}) |
                     ((expr <~ ">=") ~ expr ^^ {case x~y => Predicate.Not(Predicate.LessThan(x, y))})
    def combination = ("!" ~> pred ^^ Predicate.Not.apply) |
                      ((term <~ "|") ~ pred ^^ {case x~y => Predicate.Or(x, y)}) |
                      ((term <~ "&") ~ pred ^^ {case x~y => Predicate.And(x, y)})
    def term = comparison | ("(" ~> pred <~ ")")
    def pred: Parser[Predicate] = comparison | combination | ("(" ~> pred <~ ")")
  }
  def pred = predicateParsers.pred

  // Basic instructions
  def basicInstruction = ("." ^^^ Noop  ) |
                         ("+" ^^^ IncMem) |
                         ("-" ^^^ DecMem) |
                         (">" ^^^ IncPtr) |
                         ("<" ^^^ DecPtr)

  def basicBlock       = "[" ~> block <~ "]" ^^ While.apply
  def repeatBlock      = ("(" ~> block <~ ")" <~ "*") ~ value ^^ {case x~y => Repeat(y, x)}

  // AST extensions
  def foreverBlock     = "(" ~> block <~ ")" <~ "*" <~ "-1" ^^ Forever.apply
  def ifBlock          =
    (("if" ~> "(" ~> pred <~ ")" <~ "{") ~ block <~ "}" <~ "else" <~ "{") ~ block <~ "}" ^^ {
      case pred~a~b => IfElse(pred, a, b)
    } |
    ("if" ~> "(" ~> pred <~ ")" <~ "{") ~ block <~ "}" ^^ {case x~y => IfElse(x,y,Seq())}
  def fromToBlock      =
    ((("for" ~> "(" ~> "$" ~> identifier <~ "in") ~ expr <~ "to") ~ expr <~ ")" <~ "{") ~ block <~ "}" ^^ {
      case id~from~to~block => FromTo(id, from, to, block)
    }

  def functionCall     =
    ("@" ~> identifier <~ "(") ~ repsep(expr, ",") <~ ")" ^^ {case x~y => FunctionInvocation(x, y)}
  def functionDef      =
    (("@" ~> identifier <~ "(") ~ repsep("$" ~> identifier, ",") <~ ")" <~ "{") ~ block <~ "}" ^^ {
      case id~param~block => Map(id -> Function(param, block))
    }
  def inlineFnDef      = functionDef ~ block ^^ {case x~y => LetIn(x, y)}

  def splice           = "local" ~> "{" ~> block <~ "}" ^^ Splice.apply
  def abort            = ("abort" ~> "\"[^\"]*\"".r ^^ (x => Abort(x.substring(1, x.length - 1)))) |
                         ("abort" ^^^ Abort("abort instruction encountered"))
  def terminate        = "terminate" ^^^ Terminate

  def invertBlock      = "invert" ~> "{" ~> block <~ "}" ^^ Invert.apply

  def comment          =
    "raw" ~> "\"[^\"]*\"".r ^^ (x => Raw(x.substring(1, x.length - 1))) |
    "raw" ~> "+margins" ~> "\"[^\"]*\"".r ^^ (x => Raw(x.substring(1, x.length - 1).stripMargin))

  def setCommand       = ("$" ~> identifier <~ "=") ~ expr ^^ {case x~y => Map(x -> y)}
  def inlineSetCommand = setCommand ~ block ^^ {case x~y => Assign(x, y)}

  def callCC           =
    ("callcc" ~> "(" ~> "@" ~> identifier <~ ")" <~ "{") ~ block <~ "}" ^^ {case x~y => CallCC(x, y)}
  def reset            = "reset" ~> "{" ~> block <~ "}" ^^ Reset.apply

  def instruction   : Parser[Instruction] = basicInstruction | basicBlock | foreverBlock | repeatBlock | ifBlock |
                                            fromToBlock | inlineFnDef | functionCall | splice | abort | comment |
                                            inlineSetCommand | invertBlock | reset | callCC | terminate
  def block         : Parser[Block]       = (instruction <~ ";".?).*

  def apply(s:String) = parseAll(block, s.replaceAll("//.*", "")) match {
    case Success(nodes, _)   => Left(nodes)
    case Error(err,next)     => Right("At line "+next.pos.line+", column "+next.pos.column+": "+err)
    case Failure(err,next)   => Right("At line "+next.pos.line+", column "+next.pos.column+": "+err)
  }
}
