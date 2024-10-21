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
    case IncMem => DecMem
    case DecMem => IncMem
    case x => x.transverse(x => doInvert(x))
  }
  def doSplice(i: Block): Block = i.transverse {
    case Splice(x) => x.transverse(x => doSplice(x))
    case Invert(x) => doInvert(x).transverse(x => doSplice(x))
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
  final case class InvokeContinuation(name: String) extends SimpleInstruction

  // TODO: Optimize this to generate continuations only once per call/cc block.
  def evaluateExpressions(i: Block, vars: Map[String, Int], functions: Map[String, Option[Function]])
                         (implicit options: GenerationOptions): Block = i.transverse {
    // function evaluation
    case LetIn(definitions, block) =>
      evaluateExpressions(block, vars, functions ++ definitions.map(x => x.copy(_2 = Some(x._2))))
    case CallCC(name, block) =>
      CallCC(name, evaluateExpressions(block, vars, functions + ((name, None))))

    case FunctionInvocation(name, params) =>
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
          InvokeContinuation(name)
      }

    // assign
    case Assign(values, block) =>
      evaluateExpressions(block, vars ++ values.map(x => (x._1, evaluateValue(x._2, vars))), functions)

    // reify stuff that uses values
    case FromTo(name, from, to, block) =>
      (evaluateValue(from, vars) to evaluateValue(to, vars)) flatMap {v =>
        evaluateExpressions(block, vars + ((name, v)), functions)
      }
    case Repeat(times, block) =>
      val value = evaluateValue(times, vars)
      if(value < 0) throw new ASTException("Repeat runs negative times!")
      Repeat(Value.Constant(value), evaluateExpressions(block, vars, functions))
    case IfElse(predicate, ifClause, elseClause) =>
      if(evaluatePredicate(predicate, vars)) evaluateExpressions(ifClause, vars, functions)
      else evaluateExpressions(elseClause, vars, functions)

    // fallback case
    case x => x.transverse(x => evaluateExpressions(x, vars, functions))
  }

  // Turn the complex functions into normal BF Joust code!
  // This is basically the core of JoustExt.

  final case class SavedCont(
    block: Block, state: (SavedCont, Map[String, Block])) extends SyntheticInstruction {

    def mapContents(f: Block => Block) = copy(block = f(block))
    def debugPrint(): Unit = {
      astops.printAst(linearize(block, state._1, state._2), System.out)(GenerationOptions(supportsForever = true))
      System.out.println()
    }
  }
  val abort = SavedCont(Abort("eof"), (null, Map()))

  // Minimum execution time -- used to figure out when to stop expanding some constructs.
  def linearize(blk: Block, lastCont: SavedCont = abort, conts: Map[String, Block] = Map()): Block = {
    blk.tails.foldLeft[(Boolean, Seq[Instruction])]((false, Seq[Instruction]())) {
      case ((ended, processed), i +: left) =>
        def continuation = SavedCont(left :+ lastCont, (lastCont, conts))
        def buildContinuation(headInst: Block) =
          SavedCont((headInst ++ left) :+ lastCont, (lastCont, conts))
        def appendInstruction(newInst: Block) =
          (false, processed ++ newInst)

        if(ended) (true, processed)
        else i match {
          case `abort` => (true, processed ++ abort.block)
          case Terminate =>
            (true, processed)
          case SavedCont(block, (saved, oldConts)) =>
            (true, processed ++ linearize(block, saved, oldConts))
          case Repeat(value, block) =>
            if(value.asConstant == 0) (ended, processed)
            else {
              val blockContinuation = buildContinuation(Repeat(Value.Constant(value.asConstant - 1), block))
              appendInstruction(Repeat(value, linearize(block, blockContinuation, conts)))
            }
          case While(block) =>
            appendInstruction(While(linearize(block, buildContinuation(While(block)), conts)))
          case Forever(block) =>
            (true, processed :+ Forever(linearize(block, buildContinuation(Forever(block)), conts)))
          case x: Abort => (true, x)

          case Reset(block) =>
            appendInstruction(linearize(block, abort, conts))
          case CallCC(name, block) =>
            val currentCont = continuation
            val contBlock   = linearize(currentCont.block, currentCont.state._1, currentCont.state._2)
            val nextBlock   = linearize(block, currentCont, conts + ((name, contBlock)))

            appendInstruction(nextBlock)
          case InvokeContinuation(name) =>
            if(!conts.contains(name)) throw new ASTException("Unknown continuation "+name)
            (true, processed ++ conts(name))

          case x => (false, processed :+ x)
        }
      case (out, _) => out
      }._2
  }

  // Optimization phases
  def isTerminating(i: Instruction) = i match {
    case _: Forever => false
    case _: Abort   => false

    case _ => true
  }
  def cutNonTerminating(b: Block): Block = {
    val (nonTerm, dead) = b.span(isTerminating)
    if(dead.isEmpty) nonTerm.mapContents(cutNonTerminating)
    else nonTerm.mapContents(cutNonTerminating) :+ dead.head
  }

  def unwrapNonTerminating(b: Block): Block = b.flatMap {
    case Forever(x)    if x.nonEmpty && !isTerminating(x.last) => unwrapNonTerminating(x)
    case Repeat (_, x) if x.nonEmpty && !isTerminating(x.last) => unwrapNonTerminating(x)

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
    PhaseDef("linearize", "Transforms constructs such as if/else into BF Joust code", (b, g) => linearize(b))

    // Optimization
    // PhaseDef("dce"      , "Simple dead code elimination", (b, g) => dce(b))
    // TODO: Optimize [a]a to .a (maybe?)
  )
  def runPhase(p: PhaseDef, b: Block)(implicit options: GenerationOptions) = p.fn(b, options)
}
