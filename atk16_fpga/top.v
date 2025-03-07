`default_nettype none

`define PH_COPY_IMG 1'b0
`define PH_RUN_IMG  1'b1

module top(
    input wire SYSCLK,
    input wire BUT1,
    output wire LED1,

    output SRAM_CS, // inverted
    output SRAM_OE, // inverted
    output SRAM_WE, // inverted
    output [17:0] SA,
    inout  [15:0] SD,

    input  [3:0] INT
);
    reg phase = `PH_COPY_IMG;

    cu cu_inst(
        .clk(SYSCLK),
        .rst(BUT1),

        .sram_cs_n(SRAM_CS),
        .sram_rd_n(SRAM_OE),
        .sram_wr_n(SRAM_WE),
        .sram_addr(SA[15:0]),
        .sram_in(SD),
        .sram_out(SD),

        .int_lines(INT)
    );

    assign SA[17:16] = 2'b0;
    assign LED1 = BUT1;

endmodule
