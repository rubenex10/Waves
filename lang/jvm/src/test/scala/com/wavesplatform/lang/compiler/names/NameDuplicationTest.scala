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

      "these have the same name" - {

        "constant and user function" in {
          compileOf("""
                      |let x = 42
                      |
                      |func x() = {
                      |   true
                      |}
                      |""") should produce("already defined")
        }

        "constant and callable function" in {
          compileOf("""
                      |let x = 42
                      |
                      |@Callable(i)
                      |func x() = {
                      |   WriteSet(List(DataEntry("a", "a")))
                      |}
                      |""") should produce("already defined")
        }

        "constant and verifier function" in {
          compileOf("""
                      |let x = 42
                      |
                      |@Verifier(i)
                      |func x() = {
                      |   WriteSet(List(DataEntry("a", "a")))
                      |}
                      |""") should produce("already defined")
        }

        "two user functions" in {
          compileOf("""
                    |func sameName() = {
                    |   true
                    |}
                    |
                    |func sameName() = {
                    |   1
                    |}
                    |""") should produce("already defined")
        }

        "user and callable functions" in {
          compileOf("""
                    |func sameName() = {
                    |   true
                    |}
                    |
                    |@Callable(i)
                    |func sameName() = {
                    |   WriteSet(List(DataEntry("a", "a")))
                    |}
                    |""") should produce("already defined")
        }

        "user and verifier functions" in {
          compileOf("""
                    |func sameName() = {
                    |   true
                    |}
                    |
                    |@Verifier(i)
                    |func sameName() = {
                    |   true
                    |}
                    |""") should produce("already defined")
        }

        "two callable functions" in {
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
             |""") should produce("already defined")
        }

        "callable and verifier functions" in {
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
             |""") should produce("already defined")
        }

        "constant and annotation binding" in {
          compileOf("""
             |let x = 42
             |
             |@Callable(x)
             |func some(i: Int) = {
             |    WriteSet(List(DataEntry("a", "a")))
             |}
             |""") should produce("already defined")
        }

        "annotation binding and function argument" in {
          compileOf("""
             |@Callable(i)
             |func some(s: String, i: Int) = {
             |   if (i.contractAddress == "abc") then
             |      WriteSet(List(DataEntry("a", "a")))
             |   else
             |      WriteSet(List(DataEntry("a", "b")))
             |}
             |""") should produce("override annotation bindings")
        }

      }

    }

  }

  def compileOf(script: String): Either[String, Contract] = {
    val expr = Parser.parseContract(script.stripMargin).get.value
    compiler.ContractCompiler(ctx, expr)
  }

}
