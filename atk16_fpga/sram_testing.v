`define SRAM_ON 0
`define SRAM_OFF 1

`define MODE_NONE 0
`define MODE_READ 1
`define MODE_WRITE 2

module sram_testing (
    input wire SYSCLK,
    output wire LED1,
    output wire LED2,
    output wire SRAM_CS,
    output wire SRAM_OE,
    output wire SRAM_WE,
    output reg [17:0] SA,  // SRAM address bus
    inout wire [15:0] SD  // SRAM data bus
);

    reg clk = 0;
    always @(posedge SYSCLK) begin
        clk <= ~clk;
    end

    // Enable SRAM chip select
    assign SRAM_CS = `SRAM_ON;

    wire [15:0] address = 16'hdead;
    wire [15:0] write_datum = 16'hbeef;
    reg  [15:0] read_datum;

    assign LED1 = read_datum == write_datum;
    assign LED2 = LED1;

    reg  [ 1:0] mode = `MODE_NONE;
    reg  [15:0] SD_write;
    wire [15:0] SD_read;
    assign SD = mode == `MODE_WRITE ? SD_write : 16'hz;
    assign SD_read = mode == `MODE_READ ? SD : 16'hz;
    assign SRAM_WE = mode == `MODE_WRITE ? `SRAM_ON : `SRAM_OFF;
    assign SRAM_OE = mode == `MODE_READ ? `SRAM_ON : `SRAM_OFF;

    // write and read back datum from SRAM
    reg [15:0] state = 0;
    always @(posedge clk) begin
        case (state)
            // write the data
            0: begin
                mode <= `MODE_WRITE;
                SA <= {2'b0, address};
                state <= state + 1;
            end
            1: begin
                SD_write <= write_datum;
                state <= state + 1;
            end
            // write something else to jumble SA and SD, in 2 cycles
            2: begin
                SA <= {2'b0, address} + 10;
                state <= state + 1;
            end
            3: begin
                SD_write <= 16'hcafe;
                state <= state + 1;
            end
            // read in the jumbled data
            4: begin
                mode  <= `MODE_READ;
                state <= state + 1;
            end
            5: begin
                read_datum <= SD_read;
                state <= state + 1;
            end
            // read back the original datum
            6: begin
                SA <= {2'b0, address};
                state <= state + 1;
            end
            7: begin
                read_datum <= SD_read;
                state <= state + 1;
            end
            default: begin
                // halt
            end
        endcase
    end

endmodule
