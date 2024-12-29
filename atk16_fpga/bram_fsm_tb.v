`default_nettype none
`define DUMPSTR(x) `"x.vcd`"
`timescale 10 ns / 100 ps

module bram_fsm_tb();

    reg clk, rst, read_en, write_en;
    reg [15:0] addr, data_in;
    wire done;
    wire [15:0] data_out;

    bram_fsm dut(
        .clk(clk),
        .addr(addr),
        .data_in(data_in),
        .read_en(read_en),
        .write_en(write_en),
        .data_out(data_out),
        .done(done)
    );

    // Clock generation
    always #5 clk = ~clk;  // 100 MHz clock

    reg failed;
    initial begin
        failed = 0;
        $dumpfile(`DUMPSTR(`VCD_OUTPUT));
        $dumpvars(0, bram_fsm_tb);

        clk = 0;
        rst = 0;
        read_en = 0;
        write_en = 0;
        #10

        // test read
        dut.bram_inst.mem[0] = 16'hffff;
        addr = 16'd0;
        read_en = 1;
        write_en = 0;

        while (!done) #10;

        if (data_out == 16'hffff && done == 1) begin
            $display("\033[0;32m[PASS]\033[0m Read: data is available and done is 1 as expected");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m Read: done is %d (expected 1), data is %04h (expected ffff)", done, data_out);
            failed = 1;
        end

        read_en = 0;
        write_en = 0;
        #10

        // test write
        addr = 16'd1;
        data_in = 16'hffff;
        read_en = 0;
        write_en = 1;

        while (!done) #10;

        if (dut.bram_inst.mem[1] == 16'hffff && done == 1) begin
            $display("\033[0;32m[PASS]\033[0m Write: data is available and done is 1 as expected");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m Write: done is %d (expected 0), data is %04h (expected FFFF)", done, data_out);
            failed = 1;
        end

        read_en = 0;
        write_en = 0;

        #5
        if (failed) begin
            $display("🛑 \033[0;31mTest suite failed\033[0m");
            $fatal(1);
        end else
            $display("✅ \033[0;32mTest suite passed\033[0m");

        $finish;
    end

endmodule
