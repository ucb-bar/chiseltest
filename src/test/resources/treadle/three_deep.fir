circuit Top :
    module Level1 :
        input clk : Clock
        input reset : UInt<1>
        input addr : UInt<16>
        output data : UInt<16>

        inst level2 of Level2
        level2.clk <= clk
        level2.reset <= reset

        level2.addr <= addr
        data <= level2.data

    module Level2 :
        input clk : Clock
        input reset : UInt<1>
        input addr : UInt<16>
        output data : UInt<16>

        inst memory of Level3
        memory.clk <= clk
        memory.reset <= reset

        memory.addr <= addr
        data <= memory.data

    module Level3 :
        input clk : Clock
        input reset : UInt<1>
        input addr : UInt<16>
        output data : UInt<16>

        data <= addr

    module Top :
        input clk : Clock
        input reset : UInt<1>
        input addr : UInt<16>
        output data : UInt<16>

        inst level1 of Level1
        level1.clk <= clk
        level1.reset <= reset

        level1.addr <= addr
        data <= level1.data




