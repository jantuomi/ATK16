`default_nettype none

module alu (
    input wire [2:0] sel,
    input wire [15:0] a,
    input wire [15:0] b,
    output reg [15:0] result,
    output reg [3:0] flags // { carry, overflow, negative, zero }
);
    always @(*) begin
        flags[2] = 0;
        flags[3] = 0;

        case (sel)
            3'd0: begin
                {flags[3], result} = a + b;                         // compute carry and result
                flags[2] = (a[15] == b[15] && result[15] != a[15]); // compute overflow
            end
            3'd1: begin
                {flags[3], result} = a - b;                         // compute carry and result
                flags[2] = (a[15] != b[15] && result[15] != a[15]); // compute overflow
            end
            3'd2: result = a & b;
            3'd3: result = a | b;
            3'd4: result = a ^ b;
            3'd5: result = a << b;
            3'd6: result = a >> b;
            3'd7: result = $signed(a) >>> b;
        endcase

        flags[1] = ($signed(result) < 0);
        flags[0] = (result == 0);
    end

endmodule
