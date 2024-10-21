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

import scala.language.{implicitConversions, reflectiveCalls}

// TODO Get rid of all this repetition of mapContents
object ast {
  class ASTException(s: String) extends RuntimeException(s)
  
  trait Instruction {
    def mapContents(f: Block => Block): Instruction
    def transverse(f: Instruction => Block): Instruction = mapContents(_.flatMap(f))
  }
  trait SimpleInstruction extends Instruction {
    def mapContents(f: Block => Block) = this
  }
  trait SimpleBlock extends Instruction { this: { def copy(block: Block): Instruction } =>
    val block: Block
    def mapContents(f: Block => Block) = copy(block = f(block))
  }

  type Block = Seq[Instruction]
  implicit final class BlockExt(block: Block) {
    def mapContents(f: Block => Block) = block.flatMap(_.mapContents(f))
    def transverse(f: Instruction => Block) = block.flatMap(f)
  }
  implicit def autoWrapBlock(i: Instruction): Block = Seq(i)

  case class StaticInstruction(s: String) extends SimpleInstruction
  val Noop   = StaticInstruction(".")
  val IncPtr = StaticInstruction(">")
  val DecPtr = StaticInstruction("<")
  val IncMem = StaticInstruction("+")
  val DecMem = StaticInstruction("-")

  val NullInstruction = StaticInstruction("")

  final case class Repeat(count: Value, block: Block) extends Instruction {
    def mapContents(f: Block => Block) = copy(block = f(block))
  }
  final case class While(block: Block) extends SimpleBlock
  final case class Forever(block: Block) extends SimpleBlock

  // value extensions
  enum Value {
    case Constant(i: Int)
    case Variable(s: String)
    case Add(a: Value, b: Value)
    case Sub(a: Value, b: Value)
    case Mul(a: Value, b: Value)
    case Div(a: Value, b: Value)
    case Mod(a: Value, b: Value)
    
    def asConstant: Int = this match {
      case Value.Constant(i) => i
      case x => sys.error(f"Value is not a constant: $x")
    }
  }
  
  // comparisons for compile time if/else
  enum Predicate {
    case Equals(a: Value, b: Value)
    case LessThan(a: Value, b: Value)
    case GreaterThan(a: Value, b: Value)
    case Not(v: Predicate)
    case And(a: Predicate, b: Predicate)
    case Or(a: Predicate, b: Predicate)
  }

  // comments, etc
  final case class Abort(reason : String) extends SimpleInstruction
  final case class Raw  (comment: String) extends SimpleInstruction

  case object Terminate extends SimpleInstruction

  // synthetic instructions
  trait SyntheticInstruction extends Instruction
  final case class Assign(vars: Map[String, Value], block: Block) extends SyntheticInstruction {
    def mapContents(f: Block => Block) = copy(block = f(block))
  }
  final case class IfElse(predicate: Predicate, ifClause: Block, elseClause: Block) extends SyntheticInstruction {
    def mapContents(f: Block => Block) =
      copy(ifClause = f(ifClause), elseClause = f(elseClause))
  }
  final case class FromTo(name: String, from: Value, to: Value, block: Block) extends SyntheticInstruction {
    def mapContents(f: Block => Block) = copy(block = f(block))
  }
  final case class Splice(block: Block) extends SyntheticInstruction with SimpleBlock
  final case class Invert(block: Block) extends SyntheticInstruction with SimpleBlock

  final case class CallCC(name: String, block: Block) extends SyntheticInstruction {
    def mapContents(f: Block => Block) = copy(block = f(block))
  }
  final case class Reset (block: Block) extends SyntheticInstruction with SimpleBlock

  // functions
  case class Function(params: Seq[String], body: Block)
  final case class LetIn(definitions: Map[String, Function], block: Block) extends SyntheticInstruction {
    def mapContents(f: Block => Block) = copy(block = f(block))
  }
  final case class FunctionInvocation(name: String, params: Seq[Value]) extends SimpleInstruction
}