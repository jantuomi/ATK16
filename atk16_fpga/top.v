module top(
    input wire SYSCLK,
    output wire LED1
);

    assign LED1 = SYSCLK;

    wire [15:0] result;
    wire [3:0]  flags;
    alu alu_inst(
        .sel(3'd0),
        .a(16'd0),
        .b(16'd0),
        .result(result),
        .flags(flags)
    );

endmodule
