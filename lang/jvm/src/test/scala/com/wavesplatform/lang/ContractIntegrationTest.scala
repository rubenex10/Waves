package com.wavesplatform.lang

import cats.syntax.monoid._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.{NoShrink, sampleTypes}
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invocation
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, ContractResult}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.domain.DataItem
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ContractIntegrationTest extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink {

  val ctx: CTX =
    PureContext.build(StdLibVersion.V3) |+|
      CTX(sampleTypes, Map.empty, Array.empty) |+|
      WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), false)

  property("Simple test") {
    parseCompileAndEvaluate(
      """
        |
        |func fooHelper2() = {
        |   false
        |}
        |
        |func fooHelper() = {
        |   fooHelper2() || false
        |}
        |
        |@Callable(invocation)
        |func foo(a:ByteStr) = {
        |  let x = invocation.caller.bytes
        |  if (fooHelper())
        |    then WriteSet(List(DataEntry("b", 1), DataEntry("sender", x)))
        |    else WriteSet(List(DataEntry("a", a), DataEntry("sender", x)))
        |}
        |
        |@Verifier(t)
        |func verify() = {
        |  true
        |}
        |
      """.stripMargin,
      "foo"
    ).explicitGet() shouldBe ContractResult(
      List(
        DataItem.Bin("a", ByteStr.empty),
        DataItem.Bin("sender", ByteStr.empty)
      ),
      List()
    )
  }

  /*property("Same name of function's argument and annotation's argument") {
    parseCompileAndEvaluate(
      """
        |@Callable(i)
        |func some(i: Int) = {
        |
        |   if (i.contractAddress == "abc") then
        |      WriteSet(List(DataEntry("a", "a")))
        |   else
        |      WriteSet(List(DataEntry("a", "b")))
        |}
      """.stripMargin,
      "some",
      List(Terms.CONST_LONG(5))
    ).explicitGet() shouldBe ContractResult( //TODO ловить ошибку
      List(
        DataItem.Str("a", "a")
      ),
      List()
    )
  }*/

  property("Same name of function's argument and annotation's argument") {
    parseCompileAndEvaluate(
      """
        |@Callable(i)
        |func some(j: String, i: Int) = {
        |   if (i < 3) then
        |      WriteSet(List(DataEntry("a", "a")))
        |   else
        |      WriteSet(List(DataEntry("a", "b")))
        |}
      """.stripMargin,
      "some",
      List(Terms.CONST_STRING("abc"), Terms.CONST_LONG(5))
    ).explicitGet() shouldBe ContractResult( //TODO ловить ошибку
      List(
        DataItem.Str("a", "a")
      ),
      List()
    )
  }

  def parseCompileAndEvaluate(script: String,
                              func: String,
                              args: List[Terms.EXPR] = List(Terms.CONST_BYTESTR(ByteStr.empty))): Either[ExecutionError, ContractResult] = {
    val parsed   = Parser.parseContract(script).get.value
    val compiled = ContractCompiler(ctx.compilerContext, parsed).explicitGet()

    ContractEvaluator(
      ctx.evaluationContext,
      compiled,
      Invocation(Terms.FUNCTION_CALL(FunctionHeader.User(func), args), ByteStr.empty, None, ByteStr.empty)
    )
  }

}
