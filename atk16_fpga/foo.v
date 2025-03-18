// Top-level module
module foo (
    LED1,
    LED2,
    BUT1,
    BUT2,
    SYSCLK
);
    output LED1;
    output LED2;
    input BUT1;
    input BUT2;
    input SYSCLK;

    // 2-bit counter register
    reg [1:0] val;
    initial val = 2'b00;

    // Wires for debounced button signals
    wire db_but1;
    wire db_but2;

    // Instantiate debounce modules for each button.
    // Adjust the DEBOUNCE_LIMIT parameter as needed based on SYSCLK frequency.
    debounce #(
        .DEBOUNCE_LIMIT(500000)
    ) debounce_but1 (
        .clk(SYSCLK),
        .button(BUT1),
        .db_out(db_but1)
    );

    debounce #(
        .DEBOUNCE_LIMIT(500000)
    ) debounce_but2 (
        .clk(SYSCLK),
        .button(BUT2),
        .db_out(db_but2)
    );

    // Edge detection registers to trigger a single count change per button press.
    reg prev_db_but1;
    reg prev_db_but2;
    initial begin
        prev_db_but1 = 1'b0;
        prev_db_but2 = 1'b0;
    end

    // Drive LEDs from the counter bits.
    assign LED1 = val[0];
    assign LED2 = val[1];

    // Synchronous logic to detect rising edges and update counter.
    always @(posedge SYSCLK) begin
        // If a rising edge is detected on db_but1, increment the counter.
        if (db_but1 && !prev_db_but1) val <= val + 1;
        // Else, if a rising edge is detected on db_but2, decrement the counter.
        else if (db_but2 && !prev_db_but2) val <= val - 1;

        // Store the current debounced states for edge detection on the next clock.
        prev_db_but1 <= db_but1;
        prev_db_but2 <= db_but2;
    end

endmodule

// Debounce module written in plain Verilog.
module debounce (
    clk,
    button,
    db_out
);
    parameter DEBOUNCE_LIMIT = 500000;  // Adjust as needed for your clock frequency.
    input clk;
    input button;
    output db_out;
    reg db_out;

    reg [31:0] counter;  // Counter width chosen to comfortably count up to DEBOUNCE_LIMIT.
    reg button_sync;

    initial begin
        counter     = 32'd0;
        db_out      = 1'b0;
        button_sync = 1'b0;
    end

    always @(posedge clk) begin
        // First, synchronize the raw button input to the clock domain.
        button_sync <= button;

        // If the synchronized value equals the debounced output,
        // reset the counter.
        if (button_sync == db_out) counter <= 32'd0;
        else begin
            // Otherwise, increment the counter.
            counter <= counter + 1;
            // If the counter reaches the limit, update the debounced output.
            if (counter >= DEBOUNCE_LIMIT) begin
                db_out  <= button_sync;
                counter <= 32'd0;
            end
        end
    end

endmodule
