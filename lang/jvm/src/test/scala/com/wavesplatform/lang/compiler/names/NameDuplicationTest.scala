package com.wavesplatform.lang.compiler.names
import cats.kernel.Monoid
import com.wavesplatform.lang.Common.{NoShrink, produce}
import com.wavesplatform.lang.compiler.compilerContext
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.CompilerContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.{Common, StdLibVersion}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class NameDuplicationTest extends FreeSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  val ctx: CompilerContext =
    Monoid.combine(compilerContext, WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), isTokenContext = false).compilerContext)

  "Contract compilation" - {

    "should succeed when" - {

      "annotation bindings and args of another func have the same name" in {
        compileOf("""
             |@Callable(x)
             |func foo(i: Int) = {
             |    WriteSet(List(DataEntry("a", "a")))
             |}
             |
             |@Callable(i)
             |func bar(x: Int) = {
             |    WriteSet(List(DataEntry("a", "a")))
             |}
             |""") shouldBe 'right
      }

      "declaration vars and func args have the same name" in {
        compileOf("""
             |let x = 42
             |
             |@Callable(i)
             |func some(x: String) = {
             |    WriteSet(List(DataEntry("a", x)))
             |}
             |""") shouldBe 'right
      }

    }

    "should fail when" - {

      "user functions have the same name" in {
        compileOf("""
                    |func sameName() = {
                    |   true
                    |}
                    |
                    |func sameName() = {
                    |   1
                    |}
                    |""") should produce("Contract functions must have unique names")
      }

      "user and callable functions have the same name" in {
        compileOf("""
                    |func sameName() = {
                    |   true
                    |}
                    |
                    |@Callable(i)
                    |func sameName() = {
                    |   WriteSet(List(DataEntry("a", "a")))
                    |}
                    |""") should produce("Contract functions must have unique names")
      }

      "user and verifier functions have the same name" in {
        compileOf("""
                    |func sameName() = {
                    |   true
                    |}
                    |
                    |@Verifier(i)
                    |func sameName() = {
                    |   true
                    |}
                    |""") should produce("Contract functions must have unique names")
      }

      "callable functions have the same name" in {
        compileOf("""
             |@Callable(i)
             |func sameName() = {
             |   WriteSet(List(DataEntry("a", "a")))
             |}
             |
             |@Callable(i)
             |func sameName() = {
             |   WriteSet(List(DataEntry("b", "b")))
             |}
             |""") should produce("Contract functions must have unique names")
      }

      "callable and verifier functions have the same name" in {
        compileOf("""
             |@Callable(i)
             |func sameName() = {
             |   WriteSet(List(DataEntry("a", "a")))
             |}
             |
             |@Verifier(i)
             |func sameName() = {
             |   true
             |}
             |""") should produce("Contract functions must have unique names")
      }

      "declaration and annotation bindings has the same name" in {
        compileOf("""
             |let x = 42
             |
             |@Callable(x)
             |func some(i: Int) = {
             |    WriteSet(List(DataEntry("a", "a")))
             |}
             |""") should produce("already defined")
      }

      "annotation bindings and func args has the same name" in {
        compileOf("""
             |@Callable(i)
             |func some(i: Int) = {
             |   if (i.contractAddress == "abc") then
             |      WriteSet(List(DataEntry("a", "a")))
             |   else
             |      WriteSet(List(DataEntry("a", "b")))
             |}
             |""") should produce("override annotation bindings")
      }

      "test1" in {
        compileOf("""
             |let x = 42
             |
             |func x() = {
             |   true
             |}
             |""") should produce("already defined")
      }

      "test2" in {
        compileOf("""
             |let x = 42
             |
             |@Callable(i)
             |func x() = {
             |   WriteSet(List(DataEntry("a", "a")))
             |}
             |""") should produce("already defined")
      }

      "test3" in {
        compileOf("""
             |let x = 42
             |
             |@Verifier(i)
             |func x() = {
             |   WriteSet(List(DataEntry("a", "a")))
             |}
             |""") should produce("already defined")
      }

      "test4" in {
        compileOf("""
             |let x = 42
             |
             |@Verifier(i)
             |func x() = {
             |   WriteSet(List(DataEntry("a", "a")))
             |}
             |""") should produce("already defined")
      }

    }

  }

  def compileOf(script: String): Either[String, Contract] = {
    val expr = Parser.parseContract(script.stripMargin).get.value
    compiler.ContractCompiler(ctx, expr)
  }

}
