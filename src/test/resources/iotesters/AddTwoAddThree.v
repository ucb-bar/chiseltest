// SPDX-License-Identifier: Apache-2.0

module BBAddTwo(
    input  [15:0] in,
    output reg [15:0] out
);
  always @* begin
    out = in + 2;
  end
endmodule

module BBAddThree(
    input  [15:0] in,
    output reg [15:0] out
);
  always @* begin
    out = in + 3;
  end
endmodule
