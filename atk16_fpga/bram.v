`default_nettype none

/*
 * Copyright 2020 Brian O'Dell
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

module bram (
    input clk,
    input [15:0] addr,
    input cs_n,
    input wr_n,
    input rd_n,
    input [15:0] bram_data_in,
    output reg [15:0] bram_data_out
);

    reg [15:0] mem [16'hEFFF:0];

    always @(posedge clk)
        if (cs_n == 1'b0) begin
            if (wr_n == 1'b0) mem[addr] <= bram_data_in;
            if (rd_n == 1'b0) bram_data_out <= mem[addr];
        end
    endmodule
