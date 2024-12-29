`default_nettype none

`define PH_RESET   3'd0
`define PH_FETCH   3'd1
`define PH_DECODE  3'd2
`define PH_EXECUTE 3'd3

`define FL_CARRY    2'd3
`define FL_OVERFLOW 2'd2
`define FL_NEGATIVE 2'd1
`define FL_ZERO     2'd0

// Legend:
// T = target register select
// L = left operand register select
// R = right operand register select
// I = immediate operand
// S = ALU operation select
// F = flag select
// A = absolute (1) / relative (0) addressing
// D = direct (1) / indirect (0) load/store
// X = unused

// ALR (arithmetic-logic, register)
// 0000 TTTL LLRR RSSS
`define OP_ALR     4'b0000
// ALI (arithmetic-logic, immediate)
// 0001 TTTL LLII ISSS
`define OP_ALI     4'b0001
// LDR  (load by register)
// 0010 TTTR RRXX XXAD
`define OP_LDR     4'b0010
// STR (store to memory)
// 0011 XXXL LLRR RXAD
`define OP_STR     4'b0011
// LDI (load by immediate)
// 0100 TTTI IIII IIAD
`define OP_LDI     4'b0100
// JPR (jump by register)
// 0101 ADRR RXXX XXXX
`define OP_JPR     4'b0101
// JPI (jump by immediate)
// 0110 ADII IIII IIII
`define OP_JPI     4'b0110
// BRR (branch by register)
// 0111 ADFF RRRX XXXX
`define OP_BRR     4'b0111
// BRI (branch by immediate)
// 1000 ADFF IIII IIII
`define OP_BRI     4'b1000
// LPC (load PC+1)
// 1001 TTTX XXXX XXXX
`define OP_LPC     4'b1001
// RTI (return from interrupt)
// 1010 XXXX XXXX XXXX
`define OP_RTI     4'b1010
`define OP_NOP1    4'b1011
`define OP_NOP2    4'b1100
`define OP_NOP3    4'b1101
`define OP_NOP4    4'b1110
// HLT (halt)
// 1111 XXXX XXXX XXXX
`define OP_HLT     4'b1111

module cu(
    input wire clk,
    input wire rst,
    input wire [3:0] int_lines
);
    reg  [2:0]  alu_sel;
    reg  [15:0] alu_a;
    reg  [15:0] alu_b;
    wire [15:0] alu_result;
    wire [3:0]  alu_flags;
    reg  [15:0] regbank [0:7];
    reg  [15:0] decoded_tmp;
    reg  [2:0]  phase = `PH_RESET;
    reg  [15:0] pc;
    reg  [15:0] ir;
    reg  halted = 0;

    // interrupts
    reg [15:0] int_pc;
    reg [3:0]  int_line_regs;
    reg int_en = 1;

    reg  [15:0] mem_addr;
    reg  [15:0] mem_data_in;
    reg  mem_read_en, mem_write_en;
    wire mem_done;
    wire [15:0] mem_data_out;
    bram_fsm bram_fsm_inst(
        .clk(clk),
        .addr(mem_addr),
        .data_in(mem_data_in),
        .read_en(mem_read_en),
        .write_en(mem_write_en),
        .data_out(mem_data_out),
        .done(mem_done)
    );

    alu alu_inst(
        .sel(alu_sel),
        .a(alu_a),
        .b(alu_b),
        .result(alu_result),
        .flags(alu_flags)
    );

    // Named reg wires for debugging purposes
    wire [15:0] ra, rb, rc, rd, re, rf, rg, rh;
    assign ra = regbank[0];
    assign rb = regbank[1];
    assign rc = regbank[2];
    assign rd = regbank[3];
    assign re = regbank[4];
    assign rf = regbank[5];
    assign rg = regbank[6];
    assign rh = regbank[7];

    always @(posedge int_lines[0]) int_line_regs[0] <= int_lines[0];
    always @(posedge int_lines[1]) int_line_regs[1] <= int_lines[1];
    always @(posedge int_lines[2]) int_line_regs[2] <= int_lines[2];
    always @(posedge int_lines[3]) int_line_regs[3] <= int_lines[3];

    integer i;
    always @(posedge clk or posedge rst) begin
        // Handle reset button
        if (rst) begin
            phase  <= `PH_RESET;
            halted <= 0;
        end

        else if (halted) begin
            // Do nothing
        end

        else if (int_en && int_line_regs != 4'b00) begin
            int_en <= 0;
            int_pc <= pc;
            casez (int_line_regs)
                4'bzzz1: begin
                    pc               <= 16'h10;
                    int_line_regs[0] <= 0;
                end
                4'bzz1z: begin
                    pc               <= 16'h11;
                    int_line_regs[1] <= 0;
                end
                4'bz1zz: begin
                    pc               <= 16'h12;
                    int_line_regs[2] <= 0;
                end
                4'b1zzz: begin
                    pc               <= 16'h13;
                    int_line_regs[3] <= 0;
                end
            endcase
        end

        else if (phase == `PH_RESET) begin
            phase <= `PH_FETCH;

            pc <= 16'd0;
            mem_read_en   <= 0;
            mem_write_en  <= 0;

            for (i = 0; i < 8; i = i + 1) begin
                regbank[i] <= 16'd0;
            end
        end

        // Fetch stage
        else if (phase == `PH_FETCH) begin
            mem_read_en  <= 1;
            mem_addr     <= pc;

            if (mem_done) begin
                mem_read_en  <= 0;
                ir           <= mem_data_out;
                phase        <= `PH_DECODE;
                pc           <= pc + 16'd1;
            end
        end

        // Decode stage
        else if (phase == `PH_DECODE) begin
            case (ir[15:12])
                `OP_ALR: begin
                    alu_sel <= ir[2:0];              // select ALU operation
                    alu_a   <= regbank[ir[8:6]];       // read operand A
                    alu_b   <= regbank[ir[5:3]];       // read operand B
                    phase   <= `PH_EXECUTE;
                end
                `OP_ALI: begin
                    alu_sel <= ir[2:0];              // select ALU operation
                    alu_a   <= regbank[ir[8:6]];       // read operand A
                    alu_b   <= ir[5:3];                // read immediate operand
                    phase   <= `PH_EXECUTE;
                end
                `OP_LDR: begin
                    // ir[1] = A = absolute (1) / relative (0) addressing
                    // ir[0] = D = direct (1) / indirect (0) store
                    // set target reg value to address, dereference in execute phase
                    if (ir[1] == 1 && ir[0] == 1) begin       // addressing mode: absolute, direct
                        decoded_tmp <= regbank[ir[8:6]];
                        phase <= `PH_EXECUTE;
                    end
                    else if (ir[1] == 0 && ir[0] == 1) begin  // addressing mode: pc relative, direct
                        decoded_tmp <= pc + regbank[ir[8:6]] - 1;
                        phase <= `PH_EXECUTE;
                    end
                    else if (ir[1] == 1 && ir[0] == 0) begin  // addressing mode: absolute, indirect
                        mem_read_en    <= 1;
                        mem_addr       <= regbank[ir[8:6]];

                        if (mem_done) begin
                            decoded_tmp       <= mem_data_out;
                            mem_read_en       <= 0;
                            phase             <= `PH_EXECUTE;
                        end
                    end
                    else if (ir[1] == 0 && ir[0] == 0) begin // addressing mode: pc relative, indirect
                        mem_read_en    <= 1;
                        mem_addr       <= pc + regbank[ir[8:6]] - 1;

                        if (mem_done) begin
                            decoded_tmp       <= mem_data_out;
                            mem_read_en       <= 0;
                            phase             <= `PH_EXECUTE;
                        end
                    end
                end
                `OP_STR: begin
                    // ir[1] = A = absolute (1) / relative (0) addressing
                    // ir[0] = D = direct (1) / indirect (0) store
                    // ir[5:3] = data reg
                    // ir[8:6] = address reg
                    // decoded_tmp will be the address to write to
                    if (ir[1] == 1 && ir[0] == 1) begin       // addressing mode: absolute, direct
                        decoded_tmp <= regbank[ir[8:6]];
                        phase <= `PH_EXECUTE;
                    end
                    else if (ir[1] == 0 && ir[0] == 1) begin  // addressing mode: pc relative, direct
                        decoded_tmp <= pc + regbank[ir[8:6]] - 1;
                        phase <= `PH_EXECUTE;
                    end
                    else if (ir[1] == 1 && ir[0] == 0) begin  // addressing mode: absolute, indirect
                        mem_read_en  <= 1;
                        mem_addr     <= regbank[ir[8:6]];

                        if (mem_done) begin
                            decoded_tmp  <= mem_data_out;
                            mem_read_en  <= 0;
                            phase        <= `PH_EXECUTE;
                        end
                    end
                    else if (ir[1] == 0 && ir[0] == 0) begin // addressing mode: pc relative, indirect
                        mem_read_en  <= 1;
                        mem_addr     <= pc + regbank[ir[8:6]] - 1;

                        if (mem_done) begin
                            decoded_tmp  <= mem_data_out;
                            mem_read_en  <= 0;
                            phase        <= `PH_EXECUTE;
                        end
                    end
                end
                `OP_LDI: begin
                    // ir[1] = A = absolute (1) / relative (0) addressing
                    // ir[0] = D = direct (1) / indirect (0) load
                    // ir[8:2] = immediate
                    // ir[11:9] = target reg
                    if (ir[1] == 1) begin // addressing mode: absolute
                        decoded_tmp <= ir[8:2];
                    end
                    else if (ir[1] == 0) begin // addressing mode: pc relative
                        decoded_tmp <= pc + {{9{ir[8]}}, ir[8:2]} - 1;
                    end
                    phase <= `PH_EXECUTE;
                end
                `OP_JPR: begin
                    // ir[11] = A = absolute (1) / relative (0) addressing
                    // ir[10] = D = direct (1) / indirect (0) load
                    // ir[9:6] = address reg
                    if (ir[11] == 1 && ir[10] == 1) begin // addressing mode: absolute, direct
                        pc          <= regbank[ir[9:6]];
                        phase       <= `PH_FETCH;
                    end
                    else if (ir[11] == 0 && ir[10] == 1) begin // addressing mode: pc relative, direct
                        pc          <= pc + regbank[ir[9:6]] - 1;
                        phase       <= `PH_FETCH;
                    end
                    else if (ir[11] == 1 && ir[10] == 0) begin // addressing mode: absolute, indirect
                        mem_read_en  <= 1;
                        mem_addr     <= regbank[ir[9:6]];

                        if (mem_done) begin
                            pc          <= mem_data_out;
                            mem_read_en <= 0;
                            phase       <= `PH_FETCH;
                        end
                    end
                    else if (ir[11] == 0 && ir[10] == 0) begin // addressing mode: pc relative, indirect
                        mem_read_en  <= 1;
                        mem_addr     <= pc + regbank[ir[9:6]] - 1;

                        if (mem_done) begin
                            pc          <= mem_data_out;
                            mem_read_en <= 0;
                            phase       <= `PH_FETCH;
                        end
                    end
                end
                `OP_JPI: begin
                    // ir[11] = A = absolute (1) / relative (0) addressing
                    // ir[10] = D = direct (1) / indirect (0) load
                    // ir[9:0] = immediate
                    if (ir[11] == 1 && ir[10] == 1) begin // addressing mode: absolute, direct
                        pc          <= {6'd0, ir[9:0]};
                        phase       <= `PH_FETCH;
                    end
                    else if (ir[11] == 0 && ir[10] == 1) begin // addressing mode: pc relative, direct
                        pc          <= pc + {{6{ir[9]}}, ir[9:0]} - 1;
                        phase       <= `PH_FETCH;
                    end
                    else if (ir[11] == 1 && ir[10] == 0) begin // addressing mode: absolute, indirect
                        mem_read_en  <= 1;
                        mem_addr     <= {6'd0, ir[9:0]};

                        if (mem_done) begin
                            pc          <= mem_data_out;
                            mem_read_en <= 0;
                            phase       <= `PH_FETCH;
                        end
                    end
                    else if (ir[11] == 0 && ir[10] == 0) begin // addressing mode: pc relative, indirect
                        mem_read_en  <= 1;
                        mem_addr     <= pc + {{6{ir[9]}}, ir[9:0]} - 1;

                        if (mem_done) begin
                            pc          <= mem_data_out;
                            mem_read_en <= 0;
                            phase       <= `PH_FETCH;
                        end
                    end
                end
                `OP_BRR: begin
                    // ir[11] = A = absolute (1) / relative (0) addressing
                    // ir[10] = direct (1) / indirect (0) branch
                    // ir[9:8] = flag select
                    // ir[7:5] = address reg
                    if (ir[11] == 1 && ir[10] == 1) begin // addressing mode: absolute, direct
                        decoded_tmp <= regbank[ir[7:5]];
                        phase       <= `PH_EXECUTE;
                    end
                    else if (ir[11] == 0 && ir[10] == 1) begin // addressing mode: pc relative, direct
                        decoded_tmp <= pc + regbank[ir[7:5]] - 1;
                        phase       <= `PH_EXECUTE;
                    end
                    else if (ir[11] == 1 && ir[10] == 0) begin // addressing mode: absolute, indirect
                        mem_read_en  <= 1;
                        mem_addr     <= regbank[ir[7:5]];

                        if (mem_done) begin
                            decoded_tmp <= mem_data_out;
                            mem_read_en <= 0;
                            phase       <= `PH_EXECUTE;
                        end
                    end
                    else if (ir[11] == 0 && ir[10] == 0) begin // addressing mode: pc relative, indirect
                        mem_read_en  <= 1;
                        mem_addr     <= pc + regbank[ir[7:5]] - 1;

                        if (mem_done) begin
                            decoded_tmp <= mem_data_out;
                            mem_read_en <= 0;
                            phase       <= `PH_EXECUTE;
                        end
                    end
                end
                `OP_BRI: begin
                    // ir[11] = A = absolute (1) / relative (0) addressing
                    // ir[10] = direct (1) / indirect (0) branch
                    // ir[9:8] = flag select
                    // ir[7:0] = immediate
                    if (ir[11] == 1 && ir[10] == 1) begin // addressing mode: absolute, direct
                        decoded_tmp <= {8'd0, ir[7:0]};
                        phase       <= `PH_EXECUTE;
                    end
                    else if (ir[11] == 0 && ir[10] == 1) begin // addressing mode: pc relative, direct
                        decoded_tmp <= pc + {{8{ir[7]}}, ir[7:0]} - 1;
                        phase       <= `PH_EXECUTE;
                    end
                    else if (ir[11] == 1 && ir[10] == 0) begin // addressing mode: absolute, indirect
                        mem_read_en  <= 1;
                        mem_addr     <= {8'd0, ir[7:0]};

                        if (mem_done) begin
                            decoded_tmp <= mem_data_out;
                            mem_read_en <= 0;
                            phase       <= `PH_EXECUTE;
                        end
                    end
                    else if (ir[11] == 0 && ir[10] == 0) begin // addressing mode: pc relative, indirect
                        mem_read_en  <= 1;
                        mem_addr     <= pc + {{8{ir[7]}}, ir[7:0]} - 1;

                        if (mem_done) begin
                            decoded_tmp <= mem_data_out;
                            mem_read_en <= 0;
                            phase       <= `PH_EXECUTE;
                        end
                    end
                end
                `OP_LPC: begin
                    // ir[11:9] = target reg
                    regbank[ir[11:9]] <= pc;
                    phase <= `PH_FETCH;
                end
                `OP_RTI: begin
                    pc     <= int_pc;
                    int_en <= 1;
                    phase  <= `PH_FETCH;
                end
                `OP_HLT: begin
                    halted <= 1;
                end
                default: begin
                    `ifdef __SYNTHESIS__
                    $fatal(1, "decode: unhandled opcode %04b", ir[15:12]);
                    `endif
                end
            endcase
        end

        // Execute stage
        else if (phase == `PH_EXECUTE) begin
            case (ir[15:12])
                `OP_ALR, `OP_ALI: begin
                    regbank[ir[11:9]] <= alu_result; // write result
                    phase <= `PH_FETCH;
                end
                `OP_LDR: begin
                    mem_read_en  <= 1;
                    mem_addr     <= decoded_tmp;

                    if (mem_done) begin
                        regbank[ir[11:9]] <= mem_data_out; // dereference address
                        mem_read_en       <= 0;
                        phase             <= `PH_FETCH;
                    end
                end
                `OP_STR: begin
                    mem_write_en  <= 1;
                    mem_addr      <= decoded_tmp;
                    mem_data_in   <= regbank[ir[5:3]];

                    if (mem_done) begin
                        mem_write_en <= 0;
                        phase        <= `PH_FETCH;
                    end
                end
                `OP_LDI: begin
                    // ir[1] = A = absolute (1) / relative (0) addressing
                    // ir[0] = D = direct (1) / indirect (0) load
                    // ir[8:2] = immediate
                    // ir[11:9] = target reg
                    // decoded_tmp = immediate value that takes absolute/relative to account
                    if (ir[0] == 1) begin       // direct
                        regbank[ir[11:9]] <= decoded_tmp;
                        phase <= `PH_FETCH;
                    end
                    else if (ir[0] == 0) begin  // indirect
                        mem_read_en  <= 1;
                        mem_addr     <= decoded_tmp;

                        if (mem_done) begin
                            regbank[ir[11:9]] <= mem_data_out;
                            mem_read_en       <= 0;
                            phase             <= `PH_FETCH;
                        end
                    end
                end
                `OP_BRR, `OP_BRI: begin
                    // ir[9:8] = flag select
                    // branch if selected flag (0..3) is set (i.e. when anded with flags is non-zero)
                    if ((1 << ir[9:8]) & alu_flags != 4'd0) begin
                        pc <= decoded_tmp;
                    end
                    phase <= `PH_FETCH;
                end
                default: begin
                    `ifdef __SYNTHESIS__
                    $fatal(1, "execute: unhandled opcode %04b", ir[15:12]);
                    `endif
                end
            endcase
        end
    end
endmodule
