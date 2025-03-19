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

    // State machine states.
    localparam RX_START = 0;
    localparam WAIT_RX_DONE = 1;
    localparam START_TX = 2;
    localparam TX_IN_PROG = 3;
    localparam WAIT_TX_DONE = 4;
    localparam SEND_STRING_WELCOME = 5;
    localparam RECEIVE_PROGRAM_DATA = 6;

    reg [7:0] state = SEND_STRING_WELCOME;

    // SEND_WELCOME data
    `STR_welcome
    reg [7:0] welcome_index = 0;

    // Synchronous state machine with reset.
    always @(posedge SYSCLK) begin
        case (state)
            // STATES FOR RX
            RX_START: begin
                if (!rx_done) begin
                    state <= WAIT_RX_DONE;
                end
            end

            WAIT_RX_DONE: begin
                if (rx_done) begin
                    state <= state_after_rx;
                end
            end

            // STATES FOR TX
            START_TX: begin
                if (!tx_busy) begin
                    start_tx <= 1'b1;  // trigger transmission
                    state <= TX_IN_PROG;
                end
            end

            // After issuing the start pulse, wait for tx_busy to go high.
            TX_IN_PROG: begin
                start_tx <= 1'b0;  // ensure pulse is only one cycle.
                state <= WAIT_TX_DONE;
            end

            // Wait for the transmitter to finish sending the byte.
            WAIT_TX_DONE: begin
                if (!tx_busy) begin
                    state <= state_after_tx;
                end
            end

            SEND_STRING_WELCOME: begin
                if (welcome_index < 8'd`LEN_welcome) begin
                    data_tx <= str_welcome[welcome_index];
                    welcome_index <= welcome_index + 1;
                    state <= START_TX;
                    state_after_tx <= SEND_STRING_WELCOME;
                end else begin
                    welcome_index <= 0;
                    state <= RECEIVE_PROGRAM_DATA;
                    state_after_tx <= RX_START;
                end
            end

            RECEIVE_PROGRAM_DATA: begin
                // TODO write data to flash
                state <= RX_START;
                state_after_rx <= RECEIVE_PROGRAM_DATA;
            end

            default: state <= RX_START;
        endcase

    end

    // One-cycle pulse to trigger transmission.
    reg        start_tx = 1'b0;
    // Data byte to be transmitted.
    reg  [7:0] data_tx = 8'b0;
    // Busy flag from the transmitter.
    wire       tx_busy;
    // State to transition to after TX done.
    reg  [7:0] state_after_tx = RX_START;

    // Instantiate the UART transmitter.
    uart_tx uart_inst (
        .clk  (SYSCLK),
        .start(start_tx),   // One-cycle pulse to start transmission.
        .data (data_tx),
        .tx   (SERIAL_TX),
        .busy (tx_busy)
    );

    wire [7:0] data_rx;
    wire rx_done;
    reg [7:0] state_after_rx = RECEIVE_PROGRAM_DATA;

    uart_rx uart_rx_inst (
        .clk (SYSCLK),
        .rx  (SERIAL_RX),
        .data(data_rx),
        .done(rx_done)
    );

    assign LED1 = 1;
    assign LED2 = LED1;

endmodule
