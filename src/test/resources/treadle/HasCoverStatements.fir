circuit HasCoverStatements :
  module HasCoverStatements :
    input clock : Clock
    input reset : UInt<1>
    output out : UInt<1>

    reg out_reg : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[VerificationSpec.scala 29:24]
    node _out_reg_T = eq(out_reg, UInt<1>("h0")) @[VerificationSpec.scala 30:14]
    out_reg <= _out_reg_T @[VerificationSpec.scala 30:11]
    cover(clock, out_reg, UInt<1>("h1"), "register 0 cover") @[VerificationSpec.scala 42:19]
    node _out_T = asClock(out_reg) @[VerificationSpec.scala 32:11]
    reg out_reg_1 : UInt<1>, _out_T with :
      reset => (reset, UInt<1>("h0")) @[VerificationSpec.scala 29:24]
    node _out_reg_T_1 = eq(out_reg_1, UInt<1>("h0")) @[VerificationSpec.scala 30:14]
    out_reg_1 <= _out_reg_T_1 @[VerificationSpec.scala 30:11]
    cover(_out_T, out_reg_1, UInt<1>("h1"), "register 1 cover") @[VerificationSpec.scala 52:19]
    node _out_T_1 = asClock(out_reg_1) @[VerificationSpec.scala 32:11]
    reg out_reg_2 : UInt<1>, _out_T_1 with :
      reset => (reset, UInt<1>("h0")) @[VerificationSpec.scala 29:24]
    node _out_reg_T_2 = eq(out_reg_2, UInt<1>("h0")) @[VerificationSpec.scala 30:14]
    out_reg_2 <= _out_reg_T_2 @[VerificationSpec.scala 30:11]
    cover(_out_T_1, out_reg_2, UInt<1>("h1"), "register 2 cover") @[VerificationSpec.scala 62:19]
    node _out_T_2 = asClock(out_reg_2) @[VerificationSpec.scala 32:11]
    reg out_reg_3 : UInt<1>, _out_T_2 with :
      reset => (reset, UInt<1>("h0")) @[VerificationSpec.scala 29:24]
    node _out_reg_T_3 = eq(out_reg_3, UInt<1>("h0")) @[VerificationSpec.scala 30:14]
    out_reg_3 <= _out_reg_T_3 @[VerificationSpec.scala 30:11]
    cover(_out_T_2, out_reg_3, UInt<1>("h1"), "register 3 cover") @[VerificationSpec.scala 72:19]
    node _out_T_3 = asClock(out_reg_3) @[VerificationSpec.scala 32:11]
    node _out_T_4 = asUInt(_out_T_3) @[VerificationSpec.scala 34:5]
    node _out_T_5 = bits(_out_T_4, 0, 0) @[VerificationSpec.scala 34:5]
    out <= _out_T_5 @[VerificationSpec.scala 27:7]
    inst c of child
    c.clock <= clock
    c.in <= out_reg_3

  module child:
    input clock : Clock
    input in : UInt<1>
    cover(clock, not(in), UInt(1), "cov0") : cov0
    cover(clock, not(not(in)), UInt(1), "cov1") : cov1_this_is_custom
