module flash_to_ram(
    // SRAM interface
    output sram_cs_n,
    output sram_rd_n,
    output sram_wr_n,
    output [17:0] sram_addr,
    output [15:0] sram_in,
    input  [15:0] sram_out,

    // SPI master interface
    output wire spi_cs_n,   // Chip select, active low
    output wire spi_clk,    // SPI clock
    output wire spi_mosi,   // Master Out Slave In
    input  wire spi_miso,   // Master In Slave Out
    output wire spi_rst_n   // Active low reset
);

    // Copy 128KB from flash address 2^20 onwards to SRAM address 0
    // 1. Send a read message to the flash
    // 2. Wait for the flash to respond
    // 3. Write the data to SRAM
    // 4. Repeat until 128KB has been copied




endmodule
