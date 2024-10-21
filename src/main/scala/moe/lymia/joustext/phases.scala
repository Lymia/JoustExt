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

import java.util.Random
import scala.annotation.tailrec

object phases {
  import ast._, astops._

  def doInvertActive(i: Block): Block = i.transverse {
    case Instruction.IncMem => Instruction.DecMem
    case Instruction.DecMem => Instruction.IncMem
    case Instruction.Invert(x) => doInvert(x)
    case x => x.transverse(x => doInvertActive(x))
  }
  def doInvert(i: Block): Block = i.transverse {
    case Instruction.Invert(x) => doInvertActive(x)
    case x => x.transverse(x => doInvert(x))
  }

  def doSplice(i: Block): Block = i.transverse {
    case Instruction.Splice(x) => x.transverse(x => doSplice(x))
    case x => x.transverse(x => doSplice(x))
  }

  // evaluate values, and various other compile-time macro stuff.
  final case class FunctionCallException(s: String) extends ASTException(s)
  final case class VariableException(s: String) extends ASTException(s)

  case class RngPhase(isEarly: Boolean, rng: Random) {
    def now = copy(isEarly = false)
  }

  private def evaluateBinOp(a: Value, b: Value, vars: Map[String, Int],
                            ap: (Int, Int) => Int, fail: (Value, Value) => Value)
                           (implicit rng: RngPhase): Value =
    (evaluateValue(a, vars), evaluateValue(b, vars)) match {
      case (Value.Constant(a), Value.Constant(b)) => Value.Constant(ap(a, b))
      case (a, b) => fail(a, b)
    }
  def evaluateValue(v: Value, vars: Map[String, Int])(implicit rng: RngPhase): Value = v match {
    case Value.Constant(x) => Value.Constant(x)
    case Value.Variable(x) =>
      if(!vars.contains(x)) throw VariableException("No such variable $"+x)
      Value.Constant(vars(x))

    case Value.Add(x, y) => evaluateBinOp(x, y, vars, _ + _, Value.Add.apply)
    case Value.Sub(x, y) => evaluateBinOp(x, y, vars, _ - _, Value.Sub.apply)
    case Value.Mul(x, y) => evaluateBinOp(x, y, vars, _ * _, Value.Mul.apply)
    case Value.Div(x, y) => evaluateBinOp(x, y, vars, _ / _, Value.Div.apply)
    case Value.Mod(x, y) => evaluateBinOp(x, y, vars, _ % _, Value.Mod.apply)
    case Value.RandomBetween(x, y) =>
      if (rng.isEarly) Value.RandomBetween(evaluateValue(x, vars), evaluateValue(y, vars))
      else Value.Constant(rng.rng.nextInt(evaluateValue(x, vars).asConstant, evaluateValue(y, vars).asConstant + 1))
  }
  def evaluatePredicate(p: Predicate, vars: Map[String, Int])(implicit rng: RngPhase): Boolean = p match {
    case Predicate.Equals     (a, b) => evaluateValue(a, vars).asConstant == evaluateValue(b, vars).asConstant
    case Predicate.GreaterThan(a, b) => evaluateValue(a, vars).asConstant >  evaluateValue(b, vars).asConstant
    case Predicate.LessThan   (a, b) => evaluateValue(a, vars).asConstant <  evaluateValue(b, vars).asConstant

    case Predicate.Not(v)    => !evaluatePredicate(v, vars)
    case Predicate.Or (a, b) => evaluatePredicate(a, vars) || evaluatePredicate(b, vars)
    case Predicate.And(a, b) => evaluatePredicate(a, vars) && evaluatePredicate(b, vars)
  }

  def evaluateExpressions(i: Block, vars: Map[String, Int], functions: Map[String, Option[Instruction.Function]])
                         (implicit options: GenerationOptions, rng: RngPhase): Block = i.transverse {
    // function evaluation
    case Instruction.LetIn(definitions, block) =>
      evaluateExpressions(block, vars, functions ++ definitions.map(x => x.copy(_2 = Some(x._2))))
    case Instruction.Reset(block) =>
      if (!rng.isEarly) throw new ASTException("Cannot use continuation commands in defer.")
      Instruction.Reset(evaluateExpressions(block, vars, functions))
    case Instruction.CallCC(name, block) =>
      if (!rng.isEarly) throw new ASTException("Cannot use continuation commands in defer.")
      Instruction.CallCC(name, evaluateExpressions(block, vars, functions + ((name, None))))

    case Instruction.FunctionInvocation(name, params) =>
      if(!functions.contains(name)) throw FunctionCallException("No such function: "+name)
      functions(name) match {
        case Some(function) =>
          if(function.params.length != params.length)
            throw FunctionCallException("Called function "+name+" with "+params.length+" parameters. "+
              "("+function.params.length+" expected.)")

          val newValues = function.params.zip(params.map(x => evaluateValue(x, vars)(rng.now).asConstant)).toMap
          evaluateExpressions(function.body, vars ++ newValues, functions)
        case None =>
          if(params.nonEmpty)
            throw FunctionCallException("Continuation "+name+" does not take parameters")
          Instruction.InvokeContinuation(name)
      }

    // assign
    case Instruction.Assign(values, block) =>
      val newValues = values.map(x => (x._1, evaluateValue(x._2, vars)(rng.now).asConstant))
      evaluateExpressions(block, vars ++ newValues, functions)

    // reify stuff that uses values
    case Instruction.FromTo(name, from, to, block) =>
      (evaluateValue(from, vars)(rng.now).asConstant to evaluateValue(to, vars)(rng.now).asConstant) flatMap {v =>
        evaluateExpressions(block, vars + ((name, v)), functions)
      }
    case Instruction.Repeat(times, block) => evaluateValue(times, vars) match {
      case Value.Constant(value) => {
        if (value < 0) throw new ASTException("Repeat runs negative times!")
        Instruction.Repeat(Value.Constant(value), evaluateExpressions(block, vars, functions))
      }
      case x => Instruction.Repeat(x, evaluateExpressions(block, vars, functions))
    }
    case Instruction.IfElse(predicate, ifClause, elseClause) =>
      if(evaluatePredicate(predicate, vars)(rng.now)) evaluateExpressions(ifClause, vars, functions)
      else evaluateExpressions(elseClause, vars, functions)

    // defer blocks
    case Instruction.Defer(block) =>
      if (rng.isEarly) Instruction.Defer(block)
      else evaluateExpressions(block, vars, functions)

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
          // simple instructions
          case `abort` => (true, processed ++ abort.block)
          case Instruction.Terminate =>
            (true, processed)
          case Instruction.SavedCont(block, (saved, oldConts)) =>
            (true, processed ++ linearize(block, saved, oldConts))
          case Instruction.While(block) =>
            val contObj = buildContinuation(Instruction.While(block))
            appendInstruction(Instruction.While(linearize(block, contObj, conts)))
          case Instruction.Forever(block) =>
            val contObj = buildContinuation(Instruction.Forever(block))
            (true, processed :+ Instruction.Forever(linearize(block, contObj, conts)))
          case x: Instruction.Abort => (true, x)

          // deferred instructions
          case Instruction.Repeat(Value.Constant(x), block) =>
            if (x == 0) (ended, processed)
            else {
              val contObj = buildContinuation(Instruction.Repeat(Value.Constant(x - 1), block))
              appendInstruction(Instruction.Repeat(Value.Constant(x), linearize(block, contObj, conts)))
            }
          case Instruction.Repeat(value, block) =>
            val nextLoop = Value.Sub(value, Value.Constant(1))
            val contObj = buildContinuation(Instruction.Repeat(nextLoop, block))
            appendInstruction(Instruction.Repeat(value, linearize(block, contObj, conts)))

          // call/cc instructions
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

  @tailrec
  def dce(b: Block): Block = {
    val n = unwrapNonTerminating(cutNonTerminating(b))
    if(n!=b) dce(n)
    else n
  }

  def exprsPhase(isEarly: Boolean, b: Block, opts: GenerationOptions) = {
    val rng = new Random()
    rng.setSeed(opts.source.hashCode  )
    val result = evaluateExpressions(b, Map(), Map())(opts, RngPhase(isEarly, rng))
    result
  }

  // phase definitions
  type Phase = (Block, GenerationOptions) => Block
  final case class PhaseDef(shortName: String, description: String, fn: Phase)
  val phases = Seq(
    // Core compilation phase
    PhaseDef("splice", "Processes Splice blocks", (b, g) => doSplice(b)),
    PhaseDef("early_exprs", "Evaluates expressions (early pass)", (b, g) => exprsPhase(true, b, g)),
    PhaseDef("invert", "Processes Invert blocks", (b, g) => doInvert(b)),
    PhaseDef("continuations", "Transforms constructs such as if/else into BF Joust code", (b, g) => linearize(b)),
    PhaseDef("late_exprs", "Evaluates expressions (late pass)", (b, g) => exprsPhase(false, b, g)),

    // Optimization
    PhaseDef("dce", "Simple dead code elimination", (b, g) => dce(b)),

    // TODO: Optimize [a]a to .a (maybe?)
  )
  def runPhase(p: PhaseDef, b: Block)(implicit options: GenerationOptions) = p.fn(b, options)
}
