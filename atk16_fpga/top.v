`default_nettype none

module top(
    input wire SYSCLK,
    input wire BUT1,
    output wire LED1
);

    cu cu_inst(
        .clk(SYSCLK),
        .rst(BUT1)
    );

    assign LED1 = BUT1;

endmodule
