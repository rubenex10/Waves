package com.wavesplatform.lang.v1.evaluator

import cats.implicits._
import com.wavesplatform.lang.ExprEvaluator.{LetExecResult, LetLogCallback, Log, LogItem}
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.LoggedEvaluationContext.Lenses._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.{ExecutionError, ExprEvaluator, TrampolinedExecResult}

import scala.collection.mutable.ListBuffer

object EvaluatorV1 extends ExprEvaluator {

  override type V = V1.type
  override val version: V = V1

  private def evalLetBlock(let: LET, inner: EXPR): EvalM[EVALUATED] =
    for {
      ctx <- get[LoggedEvaluationContext, ExecutionError]
      blockEvaluation = evalExpr(let.value)
      lazyBlock       = LazyVal(blockEvaluation.ter(ctx), ctx.l(let.name))
      result <- id {
        modify[LoggedEvaluationContext, ExecutionError](lets.modify(_)(_.updated(let.name, lazyBlock)))
          .flatMap(_ => evalExpr(inner))
      }
    } yield result

  private def evalFuncBlock(func: FUNC, inner: EXPR): EvalM[EVALUATED] = {
    val funcHeader = FunctionHeader.User(func.name)
    val function   = UserFunction(func.name, null, s"user defined function '${func.name}'", func.args.map(n => (n, null, n)): _*)(func.body)
    for {
      ctx <- get[LoggedEvaluationContext, ExecutionError]
      result <- id {
        modify[LoggedEvaluationContext, ExecutionError](funcs.modify(_)(_.updated(funcHeader, function)))
          .flatMap(_ => evalExpr(inner))
      }
    } yield result
  }

  private def evalRef(key: String): EvalM[EVALUATED] =
    get[LoggedEvaluationContext, ExecutionError] flatMap { ctx =>
      lets.get(ctx).get(key) match {
        case Some(lzy) => liftTER[EVALUATED](lzy.value.value)
        case None      => raiseError[LoggedEvaluationContext, ExecutionError, EVALUATED](s"A definition of '$key' not found")
      }
    }

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): EvalM[EVALUATED] =
    evalExpr(cond) flatMap {
      case TRUE  => evalExpr(ifTrue)
      case FALSE => evalExpr(ifFalse)
    }

  private def evalGetter(expr: EXPR, field: String): EvalM[EVALUATED] =
    evalExpr(expr).map(_.asInstanceOf[CaseObj]) map {
      _.fields(field)
    }

  private def evalFunctionCall(header: FunctionHeader, args: List[EXPR]): EvalM[EVALUATED] =
    for {
      ctx <- get[LoggedEvaluationContext, ExecutionError]
      result <- funcs
        .get(ctx)
        .get(header)
        .map {
          case func: UserFunction =>
            args
              .traverse[EvalM, EVALUATED](evalExpr)
              .flatMap { args =>
                val letDefsWithArgs = args.zip(func.signature.args).foldLeft(ctx.ec.letDefs) {
                  case (r, (argValue, (argName, _))) => r + (argName -> LazyVal(argValue.pure[TrampolinedExecResult], ctx.l("(arg)" + argName)))
                }
                id {
                  set(LoggedEvaluationContext.Lenses.lets.set(ctx)(letDefsWithArgs)).flatMap(_ => evalExpr(func.ev))
                }
              }
          case func: NativeFunction =>
            args
              .traverse[EvalM, EVALUATED] { x =>
                evalExpr(x)
              }
              .map(func.eval)
              .flatMap(r => liftTER[EVALUATED](r.value))
        }
        .orElse(
          // no such function, try data constructor
          header match {
            case FunctionHeader.User(typeName) =>
              types.get(ctx).get(typeName).collect {
                case t @ CaseType(_, fields) =>
                  args
                    .traverse[EvalM, EVALUATED](a => evalExpr(a))
                    .map(argValues => CaseObj(t.typeRef, fields.map(_._1).zip(argValues).toMap))
              }
            case _ => None
          }
        )
        .getOrElse(raiseError[LoggedEvaluationContext, ExecutionError, EVALUATED](s"function '$header' not found"))
    } yield result

  private def evalExpr(t: EXPR): EvalM[EVALUATED] = t match {
    case BLOCKV1(let, inner) => evalLetBlock(let, inner)
    case BLOCKV2(dec, inner) =>
      dec match {
        case l: LET  => evalLetBlock(l, inner)
        case f: FUNC => evalFuncBlock(f, inner)
      }
    case REF(str)                    => evalRef(str)
    case c @ CONST_LONG(v)           => c.pure[EvalM]
    case c @ CONST_BYTEVECTOR(v)     => c.pure[EvalM]
    case c @ CONST_STRING(v)         => c.pure[EvalM]
    case c @ TRUE                    => c.pure[EvalM]
    case c @ FALSE                   => c.pure[EvalM]
    case IF(cond, t1, t2)            => evalIF(cond, t1, t2)
    case GETTER(expr, field)         => evalGetter(expr, field)
    case FUNCTION_CALL(header, args) => evalFunctionCall(header, args)
  }

  def applywithLogging[A](c: EvaluationContext, expr: EXPR): (Log, Either[ExecutionError, A]) = {
    val log = ListBuffer[LogItem]()
    val r   = ap(c, expr, (str: String) => (v: LetExecResult) => log.append((str, v)))
    (log.toList, r)
  }

  def apply[A](c: EvaluationContext, expr: EXPR): Either[ExecutionError, A] = ap(c, expr, _ => _ => ())

  private def ap[A <: EVALUATED](c: EvaluationContext, expr: EXPR, llc: LetLogCallback): Either[ExecutionError, A] = {
    val lec = LoggedEvaluationContext(llc, c)
    evalExpr(expr)
      .map(_.asInstanceOf[A])
      .run(lec)
      .value
      ._2
  }

}
