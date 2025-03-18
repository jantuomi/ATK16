// UART transmitter module.
// Transmits a frame: start bit (0), 8 data bits (LSB first), stop bit (1)
// at a baud rate determined by CLKS_PER_BIT (here: 10417 for 100MHz/9600)
module uart_tx (
    input            clk,         // system clock: 100 MHz
    input            start,       // one-cycle pulse to start transmission
    input      [7:0] data,        // data byte to send
    output reg       tx = 1'b1,   // serial TX line
    output reg       busy = 1'b0  // high while transmitting the byte
);

    // CLK / baudrate cycles per bit.
    //parameter CLKS_PER_BIT = 10417; // 9600 baud rate
    parameter CLKS_PER_BIT = 868;  // 115200 baud rate

    reg [13:0] clk_count = 14'b0;  // counter for baud tick (14 bits is enough)
    reg [ 3:0] bit_index = 4'b0;  // counts from 0 to 9 (10 bits total: start, 8 data, stop)
    reg [ 9:0] tx_frame;  // complete frame: {stop bit, data[7:0], start bit}

    always @(posedge clk) begin
        if (!busy) begin
            if (start) begin
                // Load frame: start bit (0), 8 data bits (LSB first), stop bit (1)
                tx_frame  <= {1'b1, data, 1'b0};
                busy      <= 1'b1;
                clk_count <= 0;
                bit_index <= 0;
                tx        <= 1'b0;  // send start bit immediately
            end else begin
                tx <= 1'b1;  // remain idle
            end
        end else begin
            // When busy, count clocks for each bit period.
            if (clk_count < CLKS_PER_BIT - 1) clk_count <= clk_count + 1;
            else begin
                clk_count <= 0;
                bit_index <= bit_index + 1;
                if (bit_index < 9) tx <= tx_frame[bit_index+1];
                else begin
                    busy <= 1'b0;
                    tx   <= 1'b1;  // return to idle
                end
            end
        end
    end
endmodule
