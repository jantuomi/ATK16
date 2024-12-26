`default_nettype none

`define ST_IDLE   3'd0
`define ST_READ1  3'd1
`define ST_READ2  3'd2
`define ST_WRITE1 3'd4
`define ST_WRITE2 3'd5
`define ST_END    3'd7

module mem_fsm(
    input clk,
    input [15:0] addr,
    input [15:0] data_in,
    input read_en,
    input write_en,

    output wire [15:0] data_out,
    output reg done
);
    reg wr, rd;
    reg [15:0] bram_addr;
    reg [2:0] state = 0;
    bram bram_inst(
        .clk(clk),
        .addr(bram_addr),
        .cs_n(0),
        .wr_n(~wr),
        .rd_n(~rd),
        .bram_data_in(data_in),
        .bram_data_out(data_out)
    );

    always @(posedge clk) begin
        case (state)
            `ST_IDLE: begin
                done <= 0;
                if (~done && read_en) begin
                    state <= `ST_READ1;
                    bram_addr <= addr;
                    rd <= 1;
                    wr <= 0;
                end
                else if (~done && write_en) begin
                    state <= `ST_WRITE1;
                    bram_addr <= addr;
                    rd <= 0;
                    wr <= 1;
                end
            end
            `ST_READ1: begin
                state <= `ST_READ2;
            end
            `ST_READ2: begin
                state <= `ST_IDLE;
                done <= 1;
            end
            `ST_WRITE1: begin
                state <= `ST_WRITE2;
            end
            `ST_WRITE2: begin
                state <= `ST_IDLE;
                done <= 1;
            end
        endcase
    end

endmodule
