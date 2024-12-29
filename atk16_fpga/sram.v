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
    reg [17:0] last_addr = -1;
    reg [15:0] last_data_in = -1;
    reg last_wr_n, last_rd_n, last_cs_n = 0;
    reg trigger = 0;

    always @(posedge trigger) begin
        if (cs_n == 1'b0) begin
            if (wr_n == 1'b0) begin
                mem[addr] = 16'hx;
                //$display("wrote %04h at time %0d ns", mem[addr], $time);
                #7 mem[addr] = data_in;
                //$display("wrote %04h at time %0d ns", mem[addr], $time);
            end
            if (rd_n == 1'b0) begin
                data_out = 16'hx;
                //$display("read %04h at time %0d ns", data_out, $time);
                #7 data_out = mem[addr];
                //$display("read %04h at time %0d ns", data_out, $time);
            end
        end
    end

    always @(*) begin
        if (addr != last_addr ||
            data_in != last_data_in ||
            wr_n != last_wr_n ||
            rd_n != last_rd_n ||
            cs_n != last_cs_n
        ) begin
            //$display("SRAM: addr=%h data_in=%h", addr, data_in);
            last_addr = addr;
            last_data_in = data_in;
            last_wr_n = wr_n;
            last_rd_n = rd_n;
            last_cs_n = cs_n;
            trigger = 0;
            #3 trigger = 1;
        end
    end

endmodule
