circuit ReadMem:
  module ReadMem:
    input clock : Clock
    input reset : UInt<1>
    input addr : UInt<2>
    output value : UInt<8>

    mem m:
      data-type => UInt<8>
      depth => 3
      reader => r
      read-latency => 1
      write-latency => 1
      read-under-write => new

    m.r.clk <= clock
    m.r.en <= UInt(1)
    m.r.addr <= addr
    value <= m.r.data
