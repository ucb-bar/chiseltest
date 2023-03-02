circuit Top :
    module Level1 :
        input clk : Clock
        input reset : UInt<1>
        input in1   : UInt<16>
        output out1 : UInt<16>
        output out2 : UInt<16>
        output out3 : UInt<16>

        reg reg1 : UInt<16>, clk

        reg1 <= in1
        out1 <= reg1

        inst level2 of Level2
        level2.clk <= clk
        level2.reset <= reset

        level2.in1 <= in1
        out2 <= level2.out2
        out3 <= level2.out3

    module Level2 :
        input clk : Clock
        input reset : UInt<1>
        input in1 : UInt<16>
        output out2 : UInt<16>
        output out3 : UInt<16>

        reg reg2 : UInt<16>, clk

        reg2 <= in1
        out2 <= reg2

        inst level3 of Level3
        level3.clk <= clk
        level3.reset <= reset

        level3.in1 <= in1
        out3 <= level3.out3

    module Level3 :
        input clk : Clock
        input reset : UInt<1>
        input in1 : UInt<16>
        output out3 : UInt<16>

        reg reg3 : UInt<16>, clk

        reg3 <= in1
        out3 <= in1

    module Top :
        input clk : Clock
        input reset : UInt<1>
        input in1 : UInt<16>
        output out1 : UInt<16>
        output out2 : UInt<16>
        output out3 : UInt<16>

        inst level1 of Level1
        level1.clk <= clk
        level1.reset <= reset

        level1.in1 <= in1
        out1 <= level1.out1
        out2 <= level1.out2
        out3 <= level1.out3




