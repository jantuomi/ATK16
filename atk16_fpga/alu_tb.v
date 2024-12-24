`default_nettype none
`define DUMPSTR(x) `"x.vcd`"

`define PLUS  3'd0
`define MINUS 3'd1
`define AND   3'd2
`define OR    3'd3
`define XOR   3'd4
`define SHL   3'd5
`define SHR   3'd6
`define SAR   3'd7

`define TESTCASE(ma, msel, mb, mexpected, mflags) \
    sel      <= `msel; \
    a        <= ma; \
    b        <= mb; \
    #5 if (result != $unsigned(mexpected) || flags !== mflags) begin \
        failed <= 1; \
        $write("\033[0;31m[FAIL]\033[0m "); \
        $display("%d msel %0d = %0d, flags %04b", a, b, result, flags); \
        $display("_             expected result %0d", $unsigned(mexpected)); \
        $display("_             expected flags  %04b", mflags); \
    end \
    else begin \
        $write("\033[0;32m[PASS]\033[0m "); \
        $display("%d msel %0d = %0d, flags %04b", a, b, result, flags); \
    end

`timescale 10 ns / 100 ps
module alu_tb();

    reg  [2:0]  sel;
    reg  [15:0] a, b;
    wire [15:0] result;
    wire [3:0]  flags;

    alu dut (
        .sel(sel),
        .a(a),
        .b(b),
        .result(result),
        .flags(flags)
    );

    reg failed;
    initial begin
        failed <= 0;
        $dumpfile(`DUMPSTR(`VCD_OUTPUT));
        $dumpvars(0, alu_tb);

        `TESTCASE(10, PLUS, 20, 30, 4'b0000)
        `TESTCASE(-1, PLUS, 1, 0, 4'b1001)
        `TESTCASE(16'h7fff, PLUS, 16'h7fff, 16'hfffe, 4'b0110)
        `TESTCASE(30, MINUS, 30, 0, 4'b0001)
        `TESTCASE(30, MINUS, 40, -16'd10, 4'b1010)
        `TESTCASE(16'h7fff, MINUS, 16'h7fff, 0, 4'b0001)
        `TESTCASE(16'h8000, MINUS, 16'h1, 16'h7fff, 4'b0100)
        `TESTCASE(3'b110, AND, 3'b101, 3'b100, 4'b0000)
        `TESTCASE(3'b110, OR, 3'b101, 3'b111, 4'b0000)
        `TESTCASE(3'b110, XOR, 3'b101, 3'b011, 4'b0000)
        `TESTCASE(16'b110, SHL, 16'd3, 16'b110000, 4'b0000)
        `TESTCASE(16'b1100_0000_0000_0000, SHR, 16'd3, 16'b0001_1000_0000_0000, 4'b0000)
        `TESTCASE(16'b1100_0000_0000_0000, SAR, 16'd3, 16'b1111_1000_0000_0000, 4'b0010)

        #5
        if (failed) begin
            $display("🛑 \033[0;31mTest suite failed\033[0m");
            $fatal(1);
        end else
            $display("✅ \033[0;32mTest suite passed\033[0m");
        $finish;
    end
endmodule
