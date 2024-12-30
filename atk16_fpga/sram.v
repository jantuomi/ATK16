`default_nettype none
`timescale 1 ns / 100 ps

// This is a memory model of the SRAM chip on the iCE40HX8K-EVB.
// Relevant specs:
// - 256Kx16 capacity
// - 18-bit address bus, 16-bit data bus
// - 10 ns read or write cycle time
// - 3 ns output hold from address change
module sram(
    input wire cs_n,
    input wire wr_n,
    input wire rd_n,
    input wire [17:0] addr,
    input wire [15:0] data_in,
    output reg [15:0] data_out
);
    reg [15:0] mem [0:262143];  // 256K x 16 memory

    always @(cs_n or wr_n or rd_n or addr or data_in) begin
        if (cs_n == 1'b1) begin
            data_out <= #3 16'hx;
        end
        if (cs_n == 1'b0 && wr_n == 1'b0) begin
            mem[addr] <= #3 16'hx;
            mem[addr] <= #7 data_in;
        end
        if (cs_n == 1'b0 && rd_n == 1'b0) begin
            data_out <= #3 16'hx;
            data_out <= #7 mem[addr];
        end
    end
endmodule
