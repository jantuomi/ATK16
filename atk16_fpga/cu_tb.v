`default_nettype none
`define DUMPSTR(x) `"x.vcd`"
`timescale 1 ns / 100 ps

module cu_tb();

    reg clk, rst;
    reg [3:0] int_lines;

    cu dut(
        .clk(clk),
        .rst(rst),
        .int_lines(int_lines)
    );

    // Clock generation
    always #5 clk = ~clk;  // 100 MHz clock

    reg failed;
    reg [15:0] expected;
    initial begin
        failed = 0;
        $dumpfile(`DUMPSTR(`VCD_OUTPUT));
        $dumpvars(0, cu_tb);

        clk = 0;
        rst = 0;
        int_lines = 4'b0000;

        #10 // Wait for reset phase to finish

        // set up halt instruction at PC = 1, used by all tests
        dut.sram_inst.mem[1] = { 4'b1111, 12'd0 }; // HLT

        // test ALR
        dut.regbank[0] = 16'd10;
        dut.regbank[1] = 16'd20;
        dut.sram_inst.mem[0] = { 4'b0000, 3'd2, 3'd0, 3'd1, 3'd0 }; // ALR RC, RA, RB, S=PLUS
        expected = 16'd30;

        #200 if (dut.regbank[2] == expected) begin
            $display("\033[0;32m[PASS]\033[0m ALR ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m ALR not working, target reg contains %04h, expected %04h", dut.regbank[2], expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test ALI
        dut.regbank[0] = 16'd10;
        dut.sram_inst.mem[0] = { 4'b0001, 3'd2, 3'd0, 3'd7, 3'd0 }; // ALI RC, RA, 7, S=PLUS
        expected = 16'd17;

        #200 if (dut.regbank[2] == expected) begin
            $display("\033[0;32m[PASS]\033[0m ALI ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m ALI not working, target reg contains %04h, expected %04h", dut.regbank[2], expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test LDR, absolute addressing
        dut.sram_inst.mem[16'd100] = 16'hffff;
        dut.regbank[1] = 16'd100;
        dut.sram_inst.mem[16'd0] = { 4'b0010, 3'd0, 3'd1, 4'd0, 1'd1, 1'd1 }; // LDR RA, RB, absolute, direct
        expected = 16'hffff;

        #200 if (dut.regbank[0] == expected) begin
            $display("\033[0;32m[PASS]\033[0m LDR direct absolute ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m LDR direct absolute not working, target reg contains %04h, expected %04h", dut.regbank[0], expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test LDR, relative addressing
        dut.sram_inst.mem[16'd10] = 16'hffff;
        dut.regbank[1] = 16'd10;
        dut.sram_inst.mem[16'd0] = { 4'b0010, 3'd0, 3'd1, 4'd0, 1'd0, 1'd1 }; // LDR RA, RB, relative, direct
        expected = 16'hffff;

        #200 if (dut.regbank[0] == expected) begin
            $display("\033[0;32m[PASS]\033[0m LDR direct relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m LDR direct relative not working, target reg contains %04h, expected %04h", dut.regbank[0], expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test LDR, absolute addressing, indirect
        dut.sram_inst.mem[16'd100] = 16'd200;
        dut.sram_inst.mem[16'd200] = 16'hffff;
        dut.regbank[1] = 16'd100;
        dut.sram_inst.mem[16'd0] = { 4'b0010, 3'd0, 3'd1, 4'd0, 1'd1, 1'd0 }; // LDR RA, RB, absolute, indirect
        expected = 16'hffff;

        #200 if (dut.regbank[0] == expected) begin
            $display("\033[0;32m[PASS]\033[0m LDR indirect absolute ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m LDR indirect absolute not working, target reg contains %04h, expected %04h", dut.regbank[0], expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test LDR, relative addressing, indirect
        dut.sram_inst.mem[16'd10] = 16'd20;
        dut.sram_inst.mem[16'd20] = 16'hffff;
        dut.regbank[1] = 16'd10;
        dut.sram_inst.mem[16'd0] = { 4'b0010, 3'd0, 3'd1, 4'd0, 1'd0, 1'd0 }; // LDR RA, RB, relative, indirect
        expected = 16'hffff;

        #200 if (dut.regbank[0] == expected) begin
            $display("\033[0;32m[PASS]\033[0m LDR indirect relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m LDR indirect relative not working, target reg contains %04h, expected %04h", dut.regbank[0], expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test STR, absolute addressing, direct
        dut.regbank[0] = 16'hfafa;
        dut.regbank[1] = 16'd100;
        dut.sram_inst.mem[16'd0] = { 4'b0011, 3'd0, 3'd1, 4'd0, 1'd1, 1'd1 }; // STR RB, RA, absolute, direct
        expected = 16'hfafa;

        #200 if (dut.sram_inst.mem[16'd100] == expected) begin
            $display("\033[0;32m[PASS]\033[0m STR direct absolute ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m STR direct absolute not working, memory location contains %04h, expected %04h", dut.sram_inst.mem[16'd100], expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test STR, relative addressing, direct
        dut.regbank[0] = 16'hafaf;
        dut.regbank[1] = 16'd10;
        dut.sram_inst.mem[16'd0] = { 4'b0011, 3'd0, 3'd1, 4'd0, 1'd0, 1'd1 }; // STR RB, RA, relative, direct
        expected = 16'hafaf;

        #200 if (dut.sram_inst.mem[16'd10] == expected) begin
            $display("\033[0;32m[PASS]\033[0m STR direct relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m STR direct relative not working, memory location contains %04h, expected %04h", dut.sram_inst.mem[16'd10], expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test STR, absolute addressing, indirect
        dut.regbank[0] = 16'hfafa;
        dut.regbank[1] = 16'd100;
        dut.sram_inst.mem[16'd100] = 16'd200;
        dut.sram_inst.mem[16'd0] = { 4'b0011, 3'd0, 3'd1, 4'd0, 1'd1, 1'd0 }; // STR RB, RA, absolute, indirect
        expected = 16'hfafa;

        #200 if (dut.sram_inst.mem[16'd200] == expected) begin
            $display("\033[0;32m[PASS]\033[0m STR indirect absolute ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m STR indirect absolute not working, memory location contains %04h, expected %04h", dut.sram_inst.mem[16'd200], expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test STR, relative addressing, indirect
        dut.regbank[0] = 16'hafaf;
        dut.regbank[1] = -16'd10;
        dut.sram_inst.mem[16'd10] = 16'd20;
        dut.sram_inst.mem[16'd20] = { 4'b0011, 3'd0, 3'd1, 4'd0, 1'd0, 1'd0 }; // STR RB, RA, relative, indirect
        dut.pc = 16'd20;
        expected = 16'hafaf;

        #200 if (dut.sram_inst.mem[16'd20] == expected) begin
            $display("\033[0;32m[PASS]\033[0m STR indirect relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m STR indirect relative not working, memory location contains %04h, expected %04h", dut.sram_inst.mem[16'd20], expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test LDI, absolute addressing, direct
        dut.sram_inst.mem[16'd0] = { 4'b0100, 3'd0, 7'd123, 1'd1, 1'd1 }; // LDI RA, 1, absolute, direct
        expected = 16'd123;

        #200 if (dut.regbank[0] == expected) begin
            $display("\033[0;32m[PASS]\033[0m LDI direct absolute ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m LDI direct absolute not working, target reg contains %04h, expected %04h", dut.regbank[0], expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test LDI, relative addressing, direct
        dut.sram_inst.mem[16'd150] = { 4'b0100, 3'd0, -7'd20, 1'd0, 1'd1 }; // LDI RA, 1, relative, direct
        dut.pc = 16'd150;
        expected = 16'd130;

        #200 if (dut.regbank[0] == expected) begin
            $display("\033[0;32m[PASS]\033[0m LDI direct relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m LDI direct relative not working, target reg contains %04h, expected %04h", dut.regbank[0], expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test LDI, absolute addressing, indirect
        dut.sram_inst.mem[16'd0] = { 4'b0100, 3'd0, 7'd50, 1'd1, 1'd0 }; // LDI RA, 1, absolute, indirect
        dut.sram_inst.mem[16'd50] = 16'd123;
        expected = 16'd123;

        #200 if (dut.regbank[0] == expected) begin
            $display("\033[0;32m[PASS]\033[0m LDI indirect absolute ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m LDI indirect absolute not working, target reg contains %04h, expected %04h", dut.regbank[0], expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test LDI, relative addressing, indirect
        dut.sram_inst.mem[16'd150] = { 4'b0100, 3'd0, -7'd20, 1'd0, 1'd0 }; // LDI RA, 1, relative, indirect
        dut.sram_inst.mem[16'd130] = 16'd123;
        dut.pc = 16'd150;
        expected = 16'd123;

        #200 if (dut.regbank[0] == expected) begin
            $display("\033[0;32m[PASS]\033[0m LDI indirect relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m LDI indirect relative not working, target reg contains %04h, expected %04h", dut.regbank[0], expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test JPR, absolute addressing, direct
        dut.regbank[0] = 16'd100;
        dut.sram_inst.mem[16'd0] = { 4'b0101, 1'd1, 1'd1, 3'd0, 7'd0 }; // JPR RA, absolute, direct
        dut.sram_inst.mem[16'd100] = { 4'b1111, 12'd0 }; // HLT
        expected = 16'd101; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m JPR direct absolute ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m JPR direct absolute not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test JPR, relative addressing, direct
        dut.regbank[0] = 16'd50;
        dut.sram_inst.mem[16'd0] = { 4'b0101, 1'd0, 1'd1, 3'd0, 7'd0 }; // JPR RA, relative, direct
        dut.sram_inst.mem[16'd50] = { 4'b1111, 12'd0 }; // HLT
        expected = 16'd51; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m JPR direct relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m JPR direct relative not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test JPR, absolute addressing, indirect
        dut.regbank[0] = 16'd100;
        dut.sram_inst.mem[16'd100] = 16'd200;
        dut.sram_inst.mem[16'd0] = { 4'b0101, 1'd1, 1'd0, 3'd0, 7'd0 }; // JPR RA, absolute, indirect
        dut.sram_inst.mem[16'd200] = { 4'b1111, 12'd0 }; // HLT
        expected = 16'd201; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m JPR indirect absolute ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m JPR indirect absolute not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test JPR, relative addressing, indirect
        dut.regbank[0] = 16'd50;
        dut.sram_inst.mem[16'd60] = 16'd100;
        dut.sram_inst.mem[16'd10] = { 4'b0101, 1'd0, 1'd0, 3'd0, 7'd0 }; // JPR RA, relative, indirect
        dut.sram_inst.mem[16'd100] = { 4'b1111, 12'd0 }; // HLT
        dut.pc = 16'd10;
        expected = 16'd101; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m JPR indirect relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m JPR indirect relative not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test JPI, absolute addressing, direct
        dut.sram_inst.mem[16'd0] = { 4'b0110, 1'd1, 1'd1, 10'd123 }; // JPI 123, absolute, direct
        dut.sram_inst.mem[16'd123] = { 4'b1111, 12'd0 }; // HLT
        expected = 16'd124; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m JPI direct absolute ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m JPI direct absolute not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test JPI, relative addressing, direct
        dut.sram_inst.mem[16'd30] = { 4'b0110, 1'd0, 1'd1, -10'd20 }; // JPI -20, relative, direct
        dut.sram_inst.mem[16'd10] = { 4'b1111, 12'd0 }; // HLT
        dut.pc = 16'd30;
        expected = 16'd11; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m JPI direct relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m JPI direct relative not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test JPI, absolute addressing, indirect
        dut.sram_inst.mem[16'd10] = { 4'b0110, 1'd1, 1'd0, 10'd50 }; // JPI 50, absolute, indirect
        dut.sram_inst.mem[16'd50] = 16'd123;
        dut.sram_inst.mem[16'd123] = { 4'b1111, 12'd0 }; // HLT
        dut.pc = 16'd10;
        expected = 16'd124; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m JPI indirect absolute ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m JPI indirect absolute not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test JPI, relative addressing, indirect
        dut.sram_inst.mem[16'd30] = { 4'b0110, 1'd0, 1'd0, -10'd20 }; // JPI -20, relative, indirect
        dut.sram_inst.mem[16'd10] = 16'd50;
        dut.sram_inst.mem[16'd50] = { 4'b1111, 12'd0 }; // HLT
        dut.pc = 16'd30;
        expected = 16'd51; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m JPI indirect relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m JPI indirect relative not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test BRR, absolute addressing, direct, true branch
        dut.regbank[0] = 16'd100;
        dut.regbank[2] = 16'd110;
        dut.sram_inst.mem[16'd0]   = { 4'b0000, 3'd1, 3'd0, 3'd0, 3'd1 }; // ALR RB, RA, RA, S=MINUS
        dut.sram_inst.mem[16'd1]   = { 4'b0111, 1'd1, 1'd1, 2'd0, 3'd2, 5'd0 }; // BRR RC, absolute, direct, F=ZERO
        dut.sram_inst.mem[16'd2]   = { 4'b1111, 12'd0 }; // HLT
        dut.sram_inst.mem[16'd110] = { 4'b1111, 12'd0 }; // HLT
        expected = 16'd111; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m BRR direct absolute true ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m BRR direct absolute true not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test BRR, absolute addressing, direct, false branch
        dut.regbank[0] = 16'd100;
        dut.regbank[2] = 16'd110;
        dut.sram_inst.mem[16'd0] = { 4'b0000, 3'd1, 3'd0, 3'd0, 3'd1 }; // ALR RB, RA, RA, S=MINUS
        dut.sram_inst.mem[16'd1] = { 4'b0111, 1'd1, 1'd1, 2'd1, 3'd2, 5'd0 }; // BRR RC, absolute, direct, F=NEGATIVE
        dut.sram_inst.mem[16'd2] = { 4'b1111, 12'd0 }; // HLT
        dut.sram_inst.mem[16'd110] = { 4'b1111, 12'd0 }; // HLT
        expected = 16'd3; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m BRR direct absolute false ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m BRR direct absolute false not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test BRR, relative addressing, direct
        dut.regbank[0] = 16'd100;
        dut.regbank[2] = 16'd109;
        dut.pc = 16'd10;
        dut.sram_inst.mem[16'd10]  = { 4'b0000, 3'd1, 3'd0, 3'd0, 3'd1 }; // ALR RB, RA, RA, S=MINUS
        dut.sram_inst.mem[16'd11]  = { 4'b0111, 1'd0, 1'd1, 2'd0, 3'd2, 5'd0 }; // BRR RC, relative, direct, F=ZERO
        dut.sram_inst.mem[16'd12]  = { 4'b1111, 12'd0 }; // HLT
        dut.sram_inst.mem[16'd120] = { 4'b1111, 12'd0 }; // HLT
        expected = 16'd121; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m BRR direct relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m BRR direct relative not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test BRR, absolute addressing, indirect
        dut.regbank[0] = 16'd100;
        dut.regbank[2] = 16'd110;
        dut.sram_inst.mem[16'd110] = 16'd200;
        dut.sram_inst.mem[16'd0]   = { 4'b0000, 3'd1, 3'd0, 3'd0, 3'd1 }; // ALR RB, RA, RA, S=MINUS
        dut.sram_inst.mem[16'd1]   = { 4'b0111, 1'd1, 1'd0, 2'd0, 3'd2, 5'd0 }; // BRR RC, absolute, indirect, F=ZERO
        dut.sram_inst.mem[16'd2]   = { 4'b1111, 12'd0 }; // HLT
        dut.sram_inst.mem[16'd200] = { 4'b1111, 12'd0 }; // HLT
        expected = 16'd201; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m BRR indirect absolute ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m BRR indirect absolute not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test BRR, relative addressing, indirect
        dut.regbank[0] = 16'd100;
        dut.regbank[2] = 16'd109;
        dut.pc = 16'd10;
        dut.sram_inst.mem[16'd10]  = { 4'b0000, 3'd1, 3'd0, 3'd0, 3'd1 }; // ALR RB, RA, RA, S=MINUS
        dut.sram_inst.mem[16'd11]  = { 4'b0111, 1'd0, 1'd0, 2'd0, 3'd2, 5'd0 }; // BRR RC, relative, indirect, F=ZERO
        dut.sram_inst.mem[16'd12]  = { 4'b1111, 12'd0 }; // HLT
        dut.sram_inst.mem[16'd120] = 16'd130;
        dut.sram_inst.mem[16'd130] = { 4'b1111, 12'd0 }; // HLT
        expected = 16'd131; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m BRR indirect relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m BRR indirect relative not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test BRI, absolute addressing, direct, true branch
        dut.regbank[0] = 16'd100;
        dut.sram_inst.mem[16'd0]   = { 4'b0000, 3'd1, 3'd0, 3'd0, 3'd1 }; // ALR RB, RA, RA, S=MINUS
        dut.sram_inst.mem[16'd1]   = { 4'b1000, 1'd1, 1'd1, 2'd0, 8'd100 }; // BRI 100, absolute, direct, F=ZERO
        dut.sram_inst.mem[16'd2]   = { 4'b1111, 12'd0 }; // HLT
        dut.sram_inst.mem[16'd100] = { 4'b1111, 12'd0 }; // HLT
        expected = 16'd101; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m BRI direct absolute true ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m BRI direct absolute true not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test BRI, absolute addressing, direct, false branch
        dut.regbank[0] = 16'd100;
        dut.sram_inst.mem[16'd0]   = { 4'b0000, 3'd1, 3'd0, 3'd0, 3'd1 }; // ALR RB, RA, RA, S=MINUS
        dut.sram_inst.mem[16'd1]   = { 4'b1000, 1'd1, 1'd1, 2'd1, 8'd100 }; // BRI 100, absolute, direct, F=NEGATIVE
        dut.sram_inst.mem[16'd2]   = { 4'b1111, 12'd0 }; // HLT
        dut.sram_inst.mem[16'd100] = { 4'b1111, 12'd0 }; // HLT
        expected = 16'd3; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m BRI direct absolute false ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m BRI direct absolute false not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test BRI, relative addressing, direct
        dut.regbank[0] = 16'd100;
        dut.pc = 16'd10;
        dut.sram_inst.mem[16'd10]  = { 4'b0000, 3'd1, 3'd0, 3'd0, 3'd1 }; // ALR RB, RA, RA, S=MINUS
        dut.sram_inst.mem[16'd11]  = { 4'b1000, 1'd0, 1'd1, 2'd0, 8'd9 }; // BRI 9, relative, direct, F=ZERO
        dut.sram_inst.mem[16'd12]  = { 4'b1111, 12'd0 }; // HLT
        dut.sram_inst.mem[16'd20]  = { 4'b1111, 12'd0 }; // HLT
        expected = 16'd21; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m BRI direct relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m BRI direct relative not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test BRI, absolute addressing, indirect
        dut.regbank[0] = 16'd100;
        dut.sram_inst.mem[16'd100] = 16'd200;
        dut.sram_inst.mem[16'd0]   = { 4'b0000, 3'd1, 3'd0, 3'd0, 3'd1 }; // ALR RB, RA, RA, S=MINUS
        dut.sram_inst.mem[16'd1]   = { 4'b1000, 1'd1, 1'd0, 2'd0, 8'd100 }; // BRI 100, absolute, indirect, F=ZERO
        dut.sram_inst.mem[16'd2]   = { 4'b1111, 12'd0 }; // HLT
        dut.sram_inst.mem[16'd200] = { 4'b1111, 12'd0 }; // HLT
        expected = 16'd201; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m BRI indirect absolute ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m BRI indirect absolute not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        // test BRI, relative addressing, indirect
        dut.regbank[0] = 16'd100;
        dut.pc = 16'd10;
        dut.sram_inst.mem[16'd10]  = { 4'b0000, 3'd1, 3'd0, 3'd0, 3'd1 }; // ALR RB, RA, RA, S=MINUS
        dut.sram_inst.mem[16'd11]  = { 4'b1000, 1'd0, 1'd0, 2'd0, 8'd9 }; // BRI 9, relative, indirect, F=ZERO
        dut.sram_inst.mem[16'd12]  = { 4'b1111, 12'd0 }; // HLT
        dut.sram_inst.mem[16'd20]  = 16'd130;
        dut.sram_inst.mem[16'd130] = { 4'b1111, 12'd0 }; // HLT
        expected = 16'd131; // PC ends up at &HLT + 1

        #200 if (dut.pc == expected) begin
            $display("\033[0;32m[PASS]\033[0m BRI indirect relative ok");
        end else begin
            $display("\033[0;31m[FAIL]\033[0m BRI indirect relative not working, PC contains %04h, expected: %04h", dut.pc, expected);
            failed = 1;
        end
        rst = 1; #10 rst = 0; #10

        #200
        if (failed) begin
            $display("🛑 \033[0;31mTest suite failed\033[0m");
            //$fatal(1);
        end else
            $display("✅ \033[0;32mTest suite passed\033[0m");

        $finish;
    end

endmodule
