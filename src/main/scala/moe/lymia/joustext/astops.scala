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

object astops {
  import ast._

  final case class GenerationOptions(maxCycles: Int = 100000, supportsForever: Boolean = true) {
    val forever = if(supportsForever) -1 else maxCycles
  }

  // AST Printing
  def generate(ast: Block)(implicit options: GenerationOptions) = {
    val buf = new java.lang.StringBuilder()
    printAst(ast, buf)
    buf.toString
  }
  def printAst(ast: Block, out: Appendable)(implicit options: GenerationOptions): Unit = ast foreach {
    case StaticInstruction(char) =>
      out.append(char)
    case Repeat(value, block) =>
      if(value.asConstant < 0) throw new ASTException("Negative repeat count!")
      else if(value.asConstant == 0) { /* do nothing */ }
      else if(value.asConstant == 1) printAst(block, out)
      else {
        out.append("(")
        printAst(block, out)
        out.append(")*")
        out.append(value.asConstant.toString)
      }
    case Forever(block) =>
      out.append("(")
      printAst(block, out)
      out.append(")*"+options.forever)
    case While(block) =>
      out.append("[")
      printAst(block, out)
      out.append("]")
    case Abort("eof") =>
      out.append("(.)*"+options.forever)
    case Abort(reason) =>
      out.append(",: "+reason+" (.)*"+options.forever+" :,")
    case Raw(text) => out.append(text)

    case x => throw new ASTException("Tried to generate unknown AST component: "+x.toString)
  }
}
