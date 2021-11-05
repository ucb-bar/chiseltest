// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.backends.smt

import chiseltest.formal.FormalTag
import org.scalatest.flatspec.AnyFlatSpec

class SMTLibResponseParserSpec extends AnyFlatSpec {
  behavior.of("SMTLibResponseParser")

  def in(value: String): String = s"((in $value))"

  it should "parse an array with default value in binary" taggedAs FormalTag in {
    val array = "((as const (Array (_ BitVec 5) (_ BitVec 32))) #b00000000000000000000000000110011)"
    val expected = Seq((None, BigInt(0x33)))
    assert(SMTLibResponseParser.parseMemValue(in(array)) == expected)
  }

  private val base = "((as const (Array (_ BitVec 5) (_ BitVec 32))) #x00000033)"

  it should "parse an array with default value in hex" taggedAs FormalTag in {
    val expected = Seq((None, BigInt(0x33)))
    assert(SMTLibResponseParser.parseMemValue(in(base)) == expected)
  }

  private val store = s"(store $base #b01110 #x00000000)"

  it should "parse a store" taggedAs FormalTag in {
    val expected = Seq((None, BigInt(0x33)), (Some(BigInt(0xe)), BigInt(0)))
    assert(SMTLibResponseParser.parseMemValue(in(store)) == expected)
  }

  it should "parse a two stores" taggedAs FormalTag in {
    val store2 = s"(store $store #b01110 #x00000011)"
    val expected = Seq((None, BigInt(0x33)), (Some(BigInt(0xe)), BigInt(0)), (Some(BigInt(0xe)), BigInt(0x11)))
    assert(SMTLibResponseParser.parseMemValue(in(store2)) == expected)
  }

  // Z3 uses lets when doing multiple stores
  it should "parse a let" taggedAs FormalTag in {
    val innerStore = s"(store a!1 #b01110 #x00000011)"
    val letA1 = s"(let ((a!1 $store)) $innerStore)"
    val expected = Seq((None, BigInt(0x33)), (Some(BigInt(0xe)), BigInt(0)), (Some(BigInt(0xe)), BigInt(0x11)))
    assert(SMTLibResponseParser.parseMemValue(in(letA1)) == expected)
  }

  it should "parse a simple lambda" taggedAs FormalTag in {
    val array = "(lambda ((x!1 (_ BitVec 5))) (= x!1 #b00000))"
    val expected = Seq((None, BigInt(0)), (Some(BigInt(0)), BigInt(1)))
    assert(SMTLibResponseParser.parseMemValue(in(array)) == expected)
  }

  it should "parse a value represented as (_ bv0 32)" taggedAs FormalTag in {
    def z3Answer(v: Int) = s"(((count_f s0) (_ bv$v 32)))"
    assert(SMTLibResponseParser.parseValue(z3Answer(0)).get == 0)
    assert(SMTLibResponseParser.parseValue(z3Answer(1234567)).get == 1234567)
  }

  it should "parse an array with default value in the (_ bvX X) format" taggedAs FormalTag in {
    // we got this from CVC4
    val array = "((as const (Array (_ BitVec 2) (_ BitVec 8))) (_ bv0 8))"
    val expected = Seq((None, BigInt(0)))
    assert(SMTLibResponseParser.parseMemValue(in(array)) == expected)
  }

  it should "parse a store with a value in the (_ bvX X) format" taggedAs FormalTag in {
    // we got this from CVC4
    val array = "(store ((as const (Array (_ BitVec 5) (_ BitVec 8))) (_ bv0 8)) (_ bv0 5) (_ bv1 8))"
    val expected = Seq((None, BigInt(0)), (Some(BigInt(0)), BigInt(1)))
    assert(SMTLibResponseParser.parseMemValue(in(array)) == expected)
  }
}
