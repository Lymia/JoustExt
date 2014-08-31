package moe.lymia.joustext

object phases {
  import ast._, astextension._, astops._

  def doSplice(i: Block): Block = i.transverse {
    case Splice(x) => x.transverse(x => doSplice(x))
    case x => x.transverse(x => doSplice(x))
  }

  // TODO: Make this use the tick count to be able to always resolve functions.
  final case class FunctionCallException(s: String) extends ASTException(s)
  final case class VariableException(s: String) extends ASTException(s)
  def evaluateValue(v: Value, vars: Map[String, Int]): Int = v match {
    case Constant(x) => x
    case Variable(x) =>
      if(!vars.contains(x)) throw VariableException("No such variable $"+x)
      vars.get(x).get

    case Add(x, y) => evaluateValue(x, vars) + evaluateValue(y, vars)
    case Sub(x, y) => evaluateValue(x, vars) - evaluateValue(y, vars)
    case Mul(x, y) => evaluateValue(x, vars) * evaluateValue(y, vars)
    case Div(x, y) => evaluateValue(x, vars) / evaluateValue(y, vars)
  }
  def evaluateExpressions(i: Block, vars: Map[String, Int], functions: Map[String, Function])
                         (implicit options: GenerationOptions): Block = i.transverse {
    // function evaluation
    case LetIn(definitions, block) =>
      evaluateExpressions(block, vars, functions ++ definitions)
    case FunctionInvocation(name, params) =>
      if(!functions.contains(name)) throw FunctionCallException("No such function: "+name)
      val function = functions.get(name).get
      if(function.params.length != params.length)
        throw FunctionCallException("Called function "+name+" with "+params.length+" parameters. "+
                                    "("+function.params.length+" expected.)")

      val newValues = function.params.zip(params.map(x => evaluateValue(x, vars))).toMap
      evaluateExpressions(function.body, vars ++ newValues, functions)

    // reify stuff that uses values
    case FromTo(name, from, to, block) =>
      (evaluateValue(from, vars) to evaluateValue(to, vars)) flatMap {v =>
        evaluateExpressions(block, vars + ((name, v)), functions)
      }
    case Repeat(times, block) =>
      Repeat(evaluateValue(times, vars), evaluateExpressions(block, vars, functions))

    // fallback case
    case x => x.transverse(x => evaluateExpressions(x, vars, functions))
  }

  type Phase = (Block, GenerationOptions) => Block
  final case class PhaseDef(shortName: String, description: String, fn: Phase)
  val phases = Seq(
    PhaseDef("splice", "Processes Splice blocks", (b, g) => doSplice(b)),
    PhaseDef("exprs" , "Evaluates functions, from-to blocks, and the count for repeat blocks",
             (b, g) => evaluateExpressions(b, Map(), Map())(g))
  )
  def runPhase(p: PhaseDef, b: Block)(implicit options: GenerationOptions) = p.fn(b, options)
}
