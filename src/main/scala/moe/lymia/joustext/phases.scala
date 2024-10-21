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

object phases {
  import ast._, astops._

  def doInvert(i: Block): Block = i.transverse {
    case Instruction.IncMem => Instruction.DecMem
    case Instruction.DecMem => Instruction.IncMem
    case x => x.transverse(x => doInvert(x))
  }
  def doSplice(i: Block): Block = i.transverse {
    case Instruction.Splice(x) => x.transverse(x => doSplice(x))
    case Instruction.Invert(x) => doInvert(x).transverse(x => doSplice(x))
    case x => x.transverse(x => doSplice(x))
  }

  // evaluate values, and various other compile-time macro stuff.
  final case class FunctionCallException(s: String) extends ASTException(s)
  final case class VariableException(s: String) extends ASTException(s)
  def evaluateValue(v: Value, vars: Map[String, Int]): Int = v match {
    case Value.Constant(x) => x
    case Value.Variable(x) =>
      if(!vars.contains(x)) throw VariableException("No such variable $"+x)
      vars(x)

    case Value.Add(x, y) => evaluateValue(x, vars) + evaluateValue(y, vars)
    case Value.Sub(x, y) => evaluateValue(x, vars) - evaluateValue(y, vars)
    case Value.Mul(x, y) => evaluateValue(x, vars) * evaluateValue(y, vars)
    case Value.Div(x, y) => evaluateValue(x, vars) / evaluateValue(y, vars)
    case Value.Mod(x, y) => evaluateValue(x, vars) % evaluateValue(y, vars)
  }
  def evaluatePredicate(p: Predicate, vars: Map[String, Int]): Boolean = p match {
    case Predicate.Equals     (a, b) => evaluateValue(a, vars) == evaluateValue(b, vars)
    case Predicate.GreaterThan(a, b) => evaluateValue(a, vars) >  evaluateValue(b, vars)
    case Predicate.LessThan   (a, b) => evaluateValue(a, vars) <  evaluateValue(b, vars)

    case Predicate.Not(v)    => !evaluatePredicate(v, vars)
    case Predicate.Or (a, b) => evaluatePredicate(a, vars) || evaluatePredicate(b, vars)
    case Predicate.And(a, b) => evaluatePredicate(a, vars) && evaluatePredicate(b, vars)
  }

  def evaluateExpressions(i: Block, vars: Map[String, Int], functions: Map[String, Option[Instruction.Function]])
                         (implicit options: GenerationOptions): Block = i.transverse {
    // function evaluation
    case Instruction.LetIn(definitions, block) =>
      evaluateExpressions(block, vars, functions ++ definitions.map(x => x.copy(_2 = Some(x._2))))
    case Instruction.CallCC(name, block) =>
      Instruction.CallCC(name, evaluateExpressions(block, vars, functions + ((name, None))))

    case Instruction.FunctionInvocation(name, params) =>
      if(!functions.contains(name)) throw FunctionCallException("No such function: "+name)
      functions(name) match {
        case Some(function) =>
          if(function.params.length != params.length)
            throw FunctionCallException("Called function "+name+" with "+params.length+" parameters. "+
              "("+function.params.length+" expected.)")

          val newValues = function.params.zip(params.map(x => evaluateValue(x, vars))).toMap
          evaluateExpressions(function.body, vars ++ newValues, functions)
        case None =>
          if(params.nonEmpty)
            throw FunctionCallException("Continuation "+name+" does not take parameters")
          Instruction.InvokeContinuation(name)
      }

    // assign
    case Instruction.Assign(values, block) =>
      evaluateExpressions(block, vars ++ values.map(x => (x._1, evaluateValue(x._2, vars))), functions)

    // reify stuff that uses values
    case Instruction.FromTo(name, from, to, block) =>
      (evaluateValue(from, vars) to evaluateValue(to, vars)) flatMap {v =>
        evaluateExpressions(block, vars + ((name, v)), functions)
      }
    case Instruction.Repeat(times, block) =>
      val value = evaluateValue(times, vars)
      if(value < 0) throw new ASTException("Repeat runs negative times!")
      Instruction.Repeat(Value.Constant(value), evaluateExpressions(block, vars, functions))
    case Instruction.IfElse(predicate, ifClause, elseClause) =>
      if(evaluatePredicate(predicate, vars)) evaluateExpressions(ifClause, vars, functions)
      else evaluateExpressions(elseClause, vars, functions)

    // fallback case
    case x => x.transverse(x => evaluateExpressions(x, vars, functions))
  }

  // Turn the complex functions into normal BF Joust code!
  // This is basically the core of JoustExt.
  val abort: Instruction.SavedCont =
    Instruction.SavedCont(Instruction.Abort("eof"), (null, Map()))
  def linearize(blk: Block, lastCont: Instruction.SavedCont = abort, conts: Map[String, Block] = Map()): Block = {
    blk.tails.foldLeft[(Boolean, Seq[Instruction])]((false, Seq[Instruction]())) {
      case ((ended, processed), i +: left) =>
        def continuation: Instruction.SavedCont =
          Instruction.SavedCont(left :+ lastCont, (lastCont, conts))
        def buildContinuation(headInst: Block): Instruction.SavedCont =
          Instruction.SavedCont((headInst ++ left) :+ lastCont, (lastCont, conts))
        def appendInstruction(newInst: Block) =
          (false, processed ++ newInst)

        if(ended) (true, processed)
        else i match {
          case `abort` => (true, processed ++ abort.block)
          case Instruction.Terminate =>
            (true, processed)
          case Instruction.SavedCont(block, (saved, oldConts)) =>
            (true, processed ++ linearize(block, saved, oldConts))
          case Instruction.Repeat(value, block) =>
            if(value.asConstant == 0) (ended, processed)
            else {
              val contObj = buildContinuation(Instruction.Repeat(Value.Constant(value.asConstant - 1), block))
              appendInstruction(Instruction.Repeat(value, linearize(block, contObj, conts)))
            }
          case Instruction.While(block) =>
            val contObj = buildContinuation(Instruction.While(block))
            appendInstruction(Instruction.While(linearize(block, contObj, conts)))
          case Instruction.Forever(block) =>
            val contObj = buildContinuation(Instruction.Forever(block))
            (true, processed :+ Instruction.Forever(linearize(block, contObj, conts)))
          case x: Instruction.Abort => (true, x)

          case Instruction.Reset(block) =>
            appendInstruction(linearize(block, abort, conts))
          case Instruction.CallCC(name, block) =>
            val currentCont = continuation
            val contBlock   = linearize(currentCont.block, currentCont.state._1, currentCont.state._2)
            val nextBlock   = linearize(block, currentCont, conts + ((name, contBlock)))

            appendInstruction(nextBlock)
          case Instruction.InvokeContinuation(name) =>
            if(!conts.contains(name)) throw new ASTException("Unknown continuation "+name)
            (true, processed ++ conts(name))

          case x => (false, processed :+ x)
        }
      case (out, _) => out
      }._2
  }

  // Optimization phases
  def isTerminating(i: Instruction) = i match {
    case _: Instruction.Forever => false
    case _: Instruction.Abort => false

    case _ => true
  }
  def cutNonTerminating(b: Block): Block = {
    val (nonTerm, dead) = b.span(isTerminating)
    if(dead.isEmpty) nonTerm.mapContents(cutNonTerminating)
    else nonTerm.mapContents(cutNonTerminating) :+ dead.head
  }

  def unwrapNonTerminating(b: Block): Block = b.flatMap {
    case Instruction.Forever(x)    if x.nonEmpty && !isTerminating(x.last) => unwrapNonTerminating(x)
    case Instruction.Repeat (_, x) if x.nonEmpty && !isTerminating(x.last) => unwrapNonTerminating(x)

    case x => x.mapContents(unwrapNonTerminating)
  }
  def dce(b: Block): Block = {
    val n = unwrapNonTerminating(cutNonTerminating(b))
    if(n!=b) dce(n)
    else n
  }

  // phase definitions
  type Phase = (Block, GenerationOptions) => Block
  final case class PhaseDef(shortName: String, description: String, fn: Phase)
  val phases = Seq(
    // Preprocessing phase
    PhaseDef("exprs"    , "Evaluates functions, from-to blocks, and the count for repeat blocks",
             (b, g) => evaluateExpressions(b, Map(), Map())(g)),
    PhaseDef("splice"   , "Processes Splice blocks", (b, g) => doSplice(b)),

    // Core compilation phase
    PhaseDef("linearize", "Transforms constructs such as if/else into BF Joust code", (b, g) => linearize(b)),

    // Optimization
    PhaseDef("dce"      , "Simple dead code elimination", (b, g) => dce(b)),

    // TODO: Optimize [a]a to .a (maybe?)
  )
  def runPhase(p: PhaseDef, b: Block)(implicit options: GenerationOptions) = p.fn(b, options)
}
