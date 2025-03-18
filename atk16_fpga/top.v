// Top-level module for the iCE40 HX8K FPGA.
// This module instantiates the UART transmitter and sends the bytes for
// "Hello" and a newline character, then stops.
module top (
    input  SYSCLK,     // 100 MHz system clock
    input  SERIAL_RX,   // UART receive input
    output SERIAL_TX,   // UART transmit output
    output LED1,
    output LED2
);

    // One-cycle pulse to trigger transmission.
    reg start_tx = 1'b0;
    // Data byte to be transmitted.
    reg [7:0] data_to_send = 8'b0;
    // Busy flag from the transmitter.
    wire tx_busy;

    reg [7:0] letter_a = 8'd97;

    // State machine states.
    localparam WAIT_RX_BUSY = 3'd0;
    localparam WAIT_RX_DONE = 3'd1;
    localparam START_TX_1 = 3'd2;
    localparam START_TX_2 = 3'd3;
    localparam WAIT_TX_DONE = 3'd4;

    reg [2:0] state = WAIT_RX_BUSY;

    // Synchronous state machine with reset.
    always @(posedge SYSCLK) begin
        case (state)
            WAIT_RX_BUSY: begin
                if (!rx_done) begin
                    state <= WAIT_RX_DONE;
                end
            end

            WAIT_RX_DONE: begin
                if (rx_done) begin
                    state <= START_TX_1;
                end
            end

            START_TX_1: begin
                if (!tx_busy) begin
                    //data_to_send <= letter_a;
                    data_to_send <= data_to_receive;
                    start_tx <= 1'b1;  // trigger transmission
                    state <= START_TX_2;
                end
            end

            // After issuing the start pulse, wait for tx_busy to go high.
            START_TX_2: begin
                start_tx <= 1'b0;  // ensure pulse is only one cycle.
                state <= WAIT_TX_DONE;
            end

            // Wait for the transmitter to finish sending the byte.
            WAIT_TX_DONE: begin
                if (!tx_busy) begin
                    state <= WAIT_RX_BUSY;
                end
            end

            default: state <= WAIT_RX_BUSY;
        endcase

    end

    // Instantiate the UART transmitter.
    uart_tx uart_inst (
        .clk  (SYSCLK),
        .start(start_tx),   // One-cycle pulse to start transmission.
        .data (data_to_send),
        .tx   (SERIAL_TX),
        .busy (tx_busy)
    );

    wire [7:0] data_to_receive;
    wire rx_done;

    uart_rx uart_rx_inst (
        .clk (SYSCLK),
        .rx  (SERIAL_RX),
        .data(data_to_receive),
        .done(rx_done)
    );

    assign LED1 = data_to_receive == letter_a ? 1 : 0;
    assign LED2 = LED1;

endmodule
