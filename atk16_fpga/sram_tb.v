`default_nettype none
`define DUMPSTR(x) `"x.vcd`"
`timescale 1 ns / 100 ps

module sram_tb();

    reg clk, cs_n, wr_n, rd_n;
    reg [17:0] addr;
    reg [15:0] data_in;
    wire [15:0] data_out;

    sram dut(
        .cs_n(cs_n),
        .wr_n(wr_n),
        .rd_n(rd_n),
        .addr(addr),
        .data_in(data_in),
        .data_out(data_out)
    );

    wire [15:0] debug_mem100;
    assign debug_mem100 = dut.mem[100];

    reg failed;
    initial begin
        failed = 0;
        $dumpfile(`DUMPSTR(`VCD_OUTPUT));
        $dumpvars(0, sram_tb);

        cs_n = 1;
        wr_n = 1;
        rd_n = 1;
        addr = 18'd0;
        data_in = 16'h0;
        #10

        // test write
        cs_n = 0;
        wr_n = 0;
        addr = 18'd100;
        data_in = 16'h1234;

        #5
        if (debug_mem100 === 16'hx) begin
            $display("\033[0;32m[PASS]\033[0m Write: data is invalid at time: %0d ns", $time);
        end else begin
            $display("\033[0;31m[FAIL]\033[0m Write: data is valid too early, got %04h at time %0d ns", debug_mem100, $time);
            failed = 1;
        end

        #5
        if (debug_mem100 == 16'h1234) begin
            $display("\033[0;32m[PASS]\033[0m Write: data is written as expected");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m Write: data is %04h (expected 1234)", debug_mem100);
            failed = 1;
        end

        cs_n = 1;
        wr_n = 1;
        #100;

        // test read
        cs_n = 0;
        rd_n = 0;
        addr = 18'd100;

        #5
        if (data_out === 16'hx) begin
            $display("\033[0;32m[PASS]\033[0m Read: data is invalid at time: %0d ns", $time);
        end else begin
            $display("\033[0;31m[FAIL]\033[0m Read: data is valid too early, got %04h at time %0d ns", data_out, $time);
            failed = 1;
        end

        #5
        if (data_out == 16'h1234) begin
            $display("\033[0;32m[PASS]\033[0m Read: data is available as expected");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m Read: data is %04h (expected 1234)", data_out);
            failed = 1;
        end

        #100

        if (!failed) begin
            $display("\033[0;32m[PASS]\033[0m All tests passed");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m Some tests failed");
        end
        $finish;
    end

endmodule
