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

  enum Instruction {
    // Basic instructions for BFJoust
    case Noop, IncPtr, DecPtr, IncMem, DecMem
    case Repeat(count: Value, block: Block)
    case While(block: Block)
    case Forever(block: Block)

    // Comments and similar
    case Abort(reason: String)
    case Raw(comment: String)
    case Terminate

    // BF Joust synthetic instructions
    case Assign(vars: Map[String, Value], block: Block)
    case IfElse(predicate: Predicate, ifClause: Block, elseClause: Block)
    case FromTo(name: String, from: Value, to: Value, block: Block)
    case Splice(block: Block)
    case Invert(block: Block)

    // Function-related synthetic instructions
    case CallCC(name: String, block: Block)
    case Reset(block: Block)
    case Function(params: Seq[String], body: Block)
    case LetIn(definitions: Map[String, Instruction.Function], block: Block)
    case FunctionInvocation(name: String, params: Seq[Value])

    // Internal continuation AST nodes
    case InvokeContinuation(name: String)
    case SavedCont(block: Block, state: (Instruction.SavedCont, Map[String, Block]))

    // methods
    def mapContents(f: Block => Block): Instruction = this match {
      case Instruction.Repeat(n, block) => Instruction.Repeat(n, f(block))
      case Instruction.While(block) => Instruction.While(f(block))
      case Instruction.Forever(block) => Instruction.Forever(f(block))
      case Instruction.Assign(vars, block) => Instruction.Assign(vars, f(block))
      case Instruction.IfElse(predicate, ifC, elseC) => Instruction.IfElse(predicate, f(ifC), f(elseC))
      case Instruction.FromTo(name, from, to, block) => Instruction.FromTo(name, from, to, f(block))
      case Instruction.Splice(block) => Instruction.Splice(f(block))
      case Instruction.Invert(block) => Instruction.Invert(f(block))
      case Instruction.CallCC(name, block) => Instruction.CallCC(name, f(block))
      case Instruction.Reset(block) => Instruction.Reset(f(block))
      case Instruction.Function(params, block) => Instruction.Function(params, f(block))
      case Instruction.LetIn(definitions, block) => Instruction.LetIn(definitions, f(block))
      case Instruction.SavedCont(block, state) => Instruction.SavedCont(f(block), state)
      case i => i
    }
    def transverse(f: Instruction => Block): Instruction = mapContents(_.flatMap(f))

  }

  type Block = Seq[Instruction]
  implicit final class BlockExt(block: Block) {
    def mapContents(f: Block => Block): Block = block.flatMap(_.mapContents(f))
    def transverse(f: Instruction => Block): Block = block.flatMap(f)
  }
  implicit def autoWrapBlock(i: Instruction): Block = Seq(i)

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
  
  enum Predicate {
    case Equals(a: Value, b: Value)
    case LessThan(a: Value, b: Value)
    case GreaterThan(a: Value, b: Value)
    case Not(v: Predicate)
    case And(a: Predicate, b: Predicate)
    case Or(a: Predicate, b: Predicate)
  }
}