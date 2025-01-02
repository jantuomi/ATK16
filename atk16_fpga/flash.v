`timescale 1ns / 1ps

// This is a memory model of the flash memory chip on the iCE40HX8K-EVB.
// Relevant specs:
// - 2MB space (not implemented entirely in simulation)
// - An SPI interface
module flash(
    input wire cs,    // Chip select, active low
    input wire clk,   // SPI clock
    input wire mosi,  // Master Out Slave In
    output reg miso,  // Master In Slave Out
    input wire rst_n  // Active low reset
);

    reg [7:0] memory [0:2047];  // A smaller memory space for simplicity
    reg [7:0] command;
    reg [10:0] address;
    reg [7:0] temp_data;
    reg [3:0] bit_count;
    reg write_enable;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            miso <= 1'b0;
            command <= 8'b0;
            address <= 11'b0;
            temp_data <= 8'b0;
            bit_count <= 4'b0;
            write_enable <= 1'b0;
        end else if (!cs) begin
            if (bit_count < 8) begin
                // First byte is the command
                command[7 - bit_count] <= mosi;
            end else if (bit_count < 19) begin
                // Next 11 bits are the address
                address[18 - bit_count] <= mosi;
            end else if (command == 8'h02 && write_enable) begin
                // Handle write command
                if (bit_count < 28) begin
                    temp_data[27 - bit_count] <= mosi;
                    if (bit_count == 27) begin
                        memory[address] <= temp_data;
                    end
                end
            end else if (command == 8'h03) begin
                // Handle read command
                if (bit_count == 19) begin
                    temp_data <= memory[address];
                end
                if (bit_count >= 20) begin
                    miso <= temp_data[27 - bit_count];
                end
            end
            bit_count <= bit_count + 1'b1;
        end else begin
            bit_count <= 0;
            miso <= 1'bz; // Tristate when not active
        end
    end
endmodule
