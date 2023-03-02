; from a chiseltest test
; we expect to see two distince coverage count for the two instances of SubModule1:
; c0.cover_0 and c1.cover_0
circuit CoverageTestModule :
  module SubModule1 :
    input clock : Clock
    input reset : UInt<1>
    input a : UInt<3>

    node _T = gt(a, UInt<3>("h4")) @[Test1Module.scala 45:45]
    cover(clock, _T, UInt<1>("h1"), "user coverage 2") @[Test1Module.scala 45:42]

  module CoverageTestModule :
    input clock : Clock
    input reset : UInt<1>
    input a : UInt<3>
    output b : UInt<3>

    inst c0 of SubModule1 @[Test1Module.scala 36:20]
    inst c1 of SubModule1 @[Test1Module.scala 38:20]
    node _T = eq(a, UInt<3>("h4")) @[Test1Module.scala 13:10]
    node _GEN_0 = mux(_T, UInt<1>("h1"), UInt<1>("h0")) @[Test1Module.scala 13:19 Test1Module.scala 14:7 Test1Module.scala 11:5]
    node _T_1 = lt(UInt<3>("h5"), a) @[Test1Module.scala 17:12]
    node _GEN_1 = mux(_T_1, UInt<2>("h2"), UInt<2>("h3")) @[Test1Module.scala 17:17 Test1Module.scala 18:7 Test1Module.scala 20:7]
    node _T_2 = eq(a, UInt<1>("h0")) @[Test1Module.scala 23:10]
    node _T_3 = eq(a, UInt<1>("h1")) @[Test1Module.scala 27:10]
    reg cover_chain_en : UInt<3>, clock with :
      reset => (UInt<1>("h0"), cover_chain_en) @[Test1Module.scala 32:31]
    ; changed (a - 4) to (a + 5) such that the cover statement inside c1 becomes true when a=0
    node _c1_a_T = add(a, UInt<3>("h5")) @[Test1Module.scala 39:15]
    node _c1_a_T_1 = tail(_c1_a_T, 1) @[Test1Module.scala 39:15]
    b <= _GEN_1
    cover_chain_en <= mux(reset, UInt<3>("h0"), a) @[Test1Module.scala 32:31 Test1Module.scala 32:31 Test1Module.scala 33:18]
    c0.clock <= clock
    c0.reset <= reset
    c0.a <= a @[Test1Module.scala 37:10]
    c1.clock <= clock
    c1.reset <= reset
    c1.a <= _c1_a_T_1 @[Test1Module.scala 39:10]
    cover(clock, UInt<1>("h1"), and(and(UInt<1>("h1"), _T_2), UInt<1>("h1")), "user coverage") @[Test1Module.scala 24:44]