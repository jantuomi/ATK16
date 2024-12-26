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
// A = absolute (1) / relative (0) addressing
// D = direct (1) / indirect (0) load
// M = immediate mode (1) / register mode (0)

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
// 0011 XXXL LLRR RMAD
`define OP_STR     4'b0011
// LDI (load by immediate)
// 0100 TTTD IIII IIII
`define OP_LDI     4'b0100

module cu(
    input wire clk,
    input wire rst
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

    reg  [15:0] mem_addr;
    reg  [15:0] mem_data_in;
    reg  mem_read_en, mem_write_en;
    wire mem_done;
    wire [15:0] mem_data_out;
    mem_fsm mem_fsm_inst(
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

    integer i;
    always @(posedge clk or posedge rst) begin
        // Handle reset button
        if (rst) begin
            phase <= `PH_RESET;
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
            mem_write_en <= 0;
            mem_addr        <= pc;

            if (mem_done) begin
                mem_read_en  <= 0;
                mem_write_en <= 0;
                ir              <= mem_data_out;
                phase           <= `PH_DECODE;
                pc              <= pc + 16'd1;
            end
        end

        // Decode stage
        else if (phase == `PH_DECODE) begin
            phase <= `PH_EXECUTE;

            case (ir[15:12])
                `OP_ALR: begin
                    alu_sel <= ir[2:0];              // select ALU operation
                    alu_a <= regbank[ir[8:6]];       // read operand A
                    alu_b <= regbank[ir[5:3]];       // read operand B
                end
                `OP_ALI: begin
                    alu_sel <= ir[2:0];              // select ALU operation
                    alu_a <= regbank[ir[8:6]];       // read operand A
                    alu_b <= ir[5:3];                // read immediate operand
                end
                `OP_LDR: begin
                    // set target reg value to address, dereference in execute phase
                    if (ir[1] == 1 && ir[0] == 1)       // addressing mode: absolute, direct
                        decoded_tmp <= regbank[ir[8:6]];
                    else if (ir[1] == 0 && ir[0] == 1)  // addressing mode: pc relative, direct
                        decoded_tmp <= regbank[ir[8:6]] + pc - 1;
                    /*else if (ir[1] == 1 && ir[0] == 0) begin  // addressing mode: absolute, indirect
                        bram_wr   <= 0;
                        bram_rd   <= 1;
                        bram_addr <= regbank[ir[8:6]];
                        decoded_tmp <= bram_data_out;
                    end
                    else if (ir[1] == 0 && ir[0] == 0) begin // addressing mode: pc relative, indirect
                        bram_wr   <= 0;
                        bram_rd   <= 1;
                        bram_addr <= regbank[ir[8:6]] + pc - 1;
                        decoded_tmp <= bram_data_out;
                    end*/
                end
                `OP_STR: begin
                    // ir[2] = M = immediate mode (1) / register mode (0)
                    // ir[1] = A = absolute (1) / relative (0) addressing
                    // ir[0] = D = direct (1) / indirect (0) load
                    // TODO
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
                    //$display("ir[11:9] %d, reg val: %d, mem_data_out: %d, mem_done: %d\n", ir[11:9], regbank[ir[11:9]], mem_data_out, mem_done);
                    mem_read_en  <= 1;
                    mem_write_en <= 0;
                    mem_addr     <= decoded_tmp;

                    if (mem_done) begin
                        regbank[ir[11:9]] <= mem_data_out; // dereference address
                        mem_read_en       <= 0;
                        mem_write_en      <= 0;
                        phase             <= `PH_FETCH;
                    end
                end
                `OP_STR: begin
                    // TODO
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
