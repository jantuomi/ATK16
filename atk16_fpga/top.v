// UART transmitter module.
// Transmits a frame: start bit (0), 8 data bits (LSB first), stop bit (1)
// at a baud rate determined by CLKS_PER_BIT (here: 10417 for 100MHz/9600)
module uart_tx (
    input            clk,    // system clock: 100 MHz
    input            rst,    // synchronous reset (active high)
    input            start,  // one-cycle pulse to start transmission
    input      [7:0] data,   // data byte to send
    output reg       tx,     // serial output
    output reg       busy    // high while transmitting the byte
);
    // 100e6/9600 ≈ 10417 cycles per bit.
    parameter CLKS_PER_BIT = 10417;

    reg [13:0] clk_count;  // counter for baud tick (14 bits is enough)
    reg [ 3:0] bit_index;  // counts from 0 to 9 (10 bits total: start, 8 data, stop)
    reg [ 9:0] tx_frame;  // complete frame: {stop bit, data[7:0], start bit}

    always @(posedge clk) begin
        if (rst) begin
            busy      <= 1'b0;
            tx        <= 1'b1;  // idle state is high
            clk_count <= 0;
            bit_index <= 0;
        end else begin
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
    end
endmodule

// Top-level module for the iCE40 HX8K FPGA.
// This module instantiates the UART transmitter and sends the bytes for
// "Hello" and a newline character, then stops.
module top (
    input  SYSCLK,     // 100 MHz system clock
    input  SERIAL_RX,  // Unused in this design; available as a pin
    output SERIAL_TX   // UART transmit output
);

    // Create a simple synchronous reset signal.
    // Here, we use a counter that holds reset high for a few hundred cycles.
    reg [7:0] reset_cnt;
    reg sys_rst;
    always @(posedge SYSCLK) begin
        if (reset_cnt != 8'd255) begin
            reset_cnt <= reset_cnt + 1;
            sys_rst   <= 1'b1;
        end else begin
            sys_rst <= 1'b0;
        end
    end

    // ROM for the message "Hello\n" (6 bytes)
    reg [7:0] message[0:5];
    initial begin
        message[0] = "H";  // ASCII 72
        message[1] = "e";  // ASCII 101
        message[2] = "l";  // ASCII 108
        message[3] = "l";  // ASCII 108
        message[4] = "o";  // ASCII 111
        message[5] = "\n";  // ASCII 10 (newline)
    end

    // Pointer to index through the message.
    reg [2:0] index;
    // One-cycle pulse to trigger transmission.
    reg start_tx;
    // Data byte to be transmitted.
    reg [7:0] data_to_send;
    // Busy flag from the transmitter.
    wire tx_busy;

    // State machine states.
    localparam IDLE = 2'd0, WAIT_BUSY = 2'd1, WAIT_IDLE = 2'd2, DONE = 2'd3;
    reg [1:0] state;

    // Synchronous state machine with reset.
    always @(posedge SYSCLK) begin
        if (sys_rst) begin
            // Initialize on reset.
            state <= IDLE;
            index <= 0;
            start_tx <= 1'b0;
            data_to_send <= 8'd0;
        end else begin
            case (state)
                IDLE: begin
                    start_tx <= 1'b0;
                    if (index < 3'd6) begin
                        if (!tx_busy) begin
                            data_to_send <= message[index];
                            start_tx <= 1'b1;  // trigger transmission
                            state <= WAIT_BUSY;
                        end
                    end else begin
                        state <= DONE;
                    end
                end

                // After issuing the start pulse, wait for tx_busy to go high.
                WAIT_BUSY: begin
                    start_tx <= 1'b0;  // ensure pulse is only one cycle.
                    if (tx_busy) state <= WAIT_IDLE;
                end

                // Wait for the transmitter to finish sending the byte.
                WAIT_IDLE: begin
                    if (!tx_busy) begin
                        index <= index + 1;  // move to next byte
                        state <= IDLE;
                    end
                end

                DONE: begin
                    start_tx <= 1'b0;
                    // Remain in DONE state; no further transmissions.
                end

                default: state <= IDLE;
            endcase
        end
    end

    // Instantiate the UART transmitter.
    uart_tx uart_inst (
        .clk  (SYSCLK),
        .rst  (sys_rst),    // Use our synchronous reset signal.
        .start(start_tx),   // One-cycle pulse to start transmission.
        .data (data_to_send),
        .tx   (SERIAL_TX),
        .busy (tx_busy)
    );

endmodule
