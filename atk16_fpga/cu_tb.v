`default_nettype none
`define DUMPSTR(x) `"x.vcd`"
`timescale 10 ns / 100 ps

module cu_tb();

    reg clk, rst;

    cu dut(
        .clk(clk),
        .rst(rst)
    );

    // Clock generation
    always #5 clk = ~clk;  // 100 MHz clock

    reg failed;
    initial begin
        failed = 0;
        $dumpfile(`DUMPSTR(`VCD_OUTPUT));
        $dumpvars(0, cu_tb);

        clk = 0;
        rst = 0;

        #10 // Wait for reset phase to finish

        // test ALR
        dut.regbank[0] = 16'd10;
        dut.regbank[1] = 16'd20;
        dut.mem_fsm_inst.bram_inst.mem[0] = { 4'b0000, 3'd2, 3'd0, 3'd1, 3'd0 }; // ALR RC, RA, RB, S=PLUS

        #200 if (dut.regbank[2] == 16'd30) begin
            $display("\033[0;32m[PASS]\033[0m ALR ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m ALR not working, target reg contains %04h", dut.regbank[2]);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test LDR, absolute addressing
        dut.mem_fsm_inst.bram_inst.mem[16'd100] = 16'hffff;
        dut.regbank[1] = 16'd100;
        dut.mem_fsm_inst.bram_inst.mem[16'd0] = { 4'b0010, 3'd0, 3'd1, 4'd0, 1'd1, 1'd1 }; // LDR RA, RB, absolute, direct

        #200 if (dut.regbank[0] == 16'hffff) begin
            $display("\033[0;32m[PASS]\033[0m LDR absolute ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m LDR absolute not working, target reg contains %04h", dut.regbank[0]);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test LDR, relative addressing
        dut.mem_fsm_inst.bram_inst.mem[16'd10] = 16'hffff;
        dut.regbank[1] = 16'd10;
        dut.mem_fsm_inst.bram_inst.mem[16'd0] = { 4'b0010, 3'd0, 3'd1, 4'd0, 1'd0, 1'd1 }; // LDR RA, RB, relative, direct

        #200 if (dut.regbank[0] == 16'hffff) begin
            $display("\033[0;32m[PASS]\033[0m LDR relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m LDR relative not working, target reg contains %04h", dut.regbank[0]);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        #5
        if (failed) begin
            $display("🛑 \033[0;31mTest suite failed\033[0m");
            //$fatal(1);
        end else
            $display("✅ \033[0;32mTest suite passed\033[0m");

        $finish;
    end

endmodule
