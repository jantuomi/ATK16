module uart_rx (
    input  wire       clk,   // System clock: 100 MHz
    input  wire       rx,    // Serial RX line
    output reg  [7:0] data,  // Received data byte
    output reg        done   // Goes high for one clock cycle when a frame is received
);

    // CLK / baudrate cycles per bit.
    //parameter CLKS_PER_BIT = 10417; // 9600 baud rate
    parameter CLKS_PER_BIT = 868;  // 115200 baud rate

    // Define states for the state machine.
    localparam STATE_IDLE = 2'd0;
    localparam STATE_START = 2'd1;
    localparam STATE_DATA = 2'd2;
    localparam STATE_STOP = 2'd3;

    reg [ 1:0] state = STATE_IDLE;
    reg [13:0] clk_count = 14'd0;
    reg [ 2:0] bit_index = 3'd0;  // Will count 0 to 7 for the 8 data bits.
    reg [ 7:0] rx_shift_reg = 8'd0;

    // Synchronous state machine.
    always @(posedge clk) begin
        case (state)
            STATE_IDLE: begin
                done <= 1'b0;
                clk_count <= 14'd0;
                bit_index <= 3'd0;
                if (rx == 1'b0) begin
                    // Detected start bit. Go to START state.
                    state <= STATE_START;
                end else begin
                    state <= STATE_IDLE;
                end
            end

            STATE_START: begin
                // Wait for half a bit period, then sample the start bit.
                if (clk_count == (CLKS_PER_BIT - 1) / 2) begin
                    // Sample start bit: it must be 0.
                    if (rx == 1'b0) begin
                        clk_count <= 14'd0;
                        state <= STATE_DATA;
                    end else begin
                        // False start; return to idle.
                        state <= STATE_IDLE;
                    end
                end else begin
                    clk_count <= clk_count + 1;
                end
            end

            STATE_DATA: begin
                // Wait for a full bit period then sample the data bit.
                if (clk_count < CLKS_PER_BIT - 1) begin
                    clk_count <= clk_count + 1;
                end else begin
                    clk_count <= 14'd0;
                    // Sample the current data bit.
                    rx_shift_reg[bit_index] <= rx;
                    if (bit_index == 3'd7) begin
                        state <= STATE_STOP;
                    end else begin
                        bit_index <= bit_index + 1;
                    end
                end
            end

            STATE_STOP: begin
                // Wait one bit period for the stop bit.
                if (clk_count < CLKS_PER_BIT - 1) begin
                    clk_count <= clk_count + 1;
                end else begin
                    clk_count <= 14'd0;
                    // Optionally, you could check that rx is high (stop bit).
                    data      <= rx_shift_reg;  // Latch the received data.
                    done      <= 1'b1;  // Signal that a frame has been received.
                    state     <= STATE_IDLE;  // Go back to idle for the next frame.
                end
            end

            default: state <= STATE_IDLE;
        endcase
    end

endmodule
