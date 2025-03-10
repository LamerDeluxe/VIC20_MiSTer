//============================================================================
//  VIC20
//
//  Port to MiSTer
//  Copyright (C) 2017-2019 Sorgelig
//
//  This program is free software; you can redistribute it and/or modify it
//  under the terms of the GNU General Public License as published by the Free
//  Software Foundation; either version 2 of the License, or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//============================================================================

module emu
(
	//Master input clock
	input         CLK_50M,

	//Async reset from top-level module.
	//Can be used as initial reset.
	input         RESET,

	//Must be passed to hps_io module
	inout  [48:0] HPS_BUS,

	//Base video clock. Usually equals to CLK_SYS.
	output        CLK_VIDEO,

	//Multiple resolutions are supported using different CE_PIXEL rates.
	//Must be based on CLK_VIDEO
	output        CE_PIXEL,

	//Video aspect ratio for HDMI. Most retro systems have ratio 4:3.
	//if VIDEO_ARX[12] or VIDEO_ARY[12] is set then [11:0] contains scaled size instead of aspect ratio.
	output [12:0] VIDEO_ARX,
	output [12:0] VIDEO_ARY,

	output  [7:0] VGA_R,
	output  [7:0] VGA_G,
	output  [7:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,
	output        VGA_DE,    // = ~(VBlank | HBlank)
	output        VGA_F1,
	output [1:0]  VGA_SL,
	output        VGA_SCALER, // Force VGA scaler
	output        VGA_DISABLE, // analog out is off

	input  [11:0] HDMI_WIDTH,
	input  [11:0] HDMI_HEIGHT,
	output        HDMI_FREEZE,

`ifdef MISTER_FB
	// Use framebuffer in DDRAM
	// FB_FORMAT:
	//    [2:0] : 011=8bpp(palette) 100=16bpp 101=24bpp 110=32bpp
	//    [3]   : 0=16bits 565 1=16bits 1555
	//    [4]   : 0=RGB  1=BGR (for 16/24/32 modes)
	//
	// FB_STRIDE either 0 (rounded to 256 bytes) or multiple of pixel size (in bytes)
	output        FB_EN,
	output  [4:0] FB_FORMAT,
	output [11:0] FB_WIDTH,
	output [11:0] FB_HEIGHT,
	output [31:0] FB_BASE,
	output [13:0] FB_STRIDE,
	input         FB_VBL,
	input         FB_LL,
	output        FB_FORCE_BLANK,

`ifdef MISTER_FB_PALETTE
	// Palette control for 8bit modes.
	// Ignored for other video modes.
	output        FB_PAL_CLK,
	output  [7:0] FB_PAL_ADDR,
	output [23:0] FB_PAL_DOUT,
	input  [23:0] FB_PAL_DIN,
	output        FB_PAL_WR,
`endif
`endif

	output        LED_USER,  // 1 - ON, 0 - OFF.

	// b[1]: 0 - LED status is system status OR'd with b[0]
	//       1 - LED status is controled solely by b[0]
	// hint: supply 2'b00 to let the system control the LED.
	output  [1:0] LED_POWER,
	output  [1:0] LED_DISK,

	// I/O board button press simulation (active high)
	// b[1]: user button
	// b[0]: osd button
	output  [1:0] BUTTONS,

	input         CLK_AUDIO, // 24.576 MHz
	output [15:0] AUDIO_L,
	output [15:0] AUDIO_R,
	output        AUDIO_S,   // 1 - signed audio samples, 0 - unsigned
	output  [1:0] AUDIO_MIX, // 0 - no mix, 1 - 25%, 2 - 50%, 3 - 100% (mono)

	//ADC
	inout   [3:0] ADC_BUS,

	//SD-SPI
	output        SD_SCK,
	output        SD_MOSI,
	input         SD_MISO,
	output        SD_CS,
	input         SD_CD,

	//High latency DDR3 RAM interface
	//Use for non-critical time purposes
	output        DDRAM_CLK,
	input         DDRAM_BUSY,
	output  [7:0] DDRAM_BURSTCNT,
	output [28:0] DDRAM_ADDR,
	input  [63:0] DDRAM_DOUT,
	input         DDRAM_DOUT_READY,
	output        DDRAM_RD,
	output [63:0] DDRAM_DIN,
	output  [7:0] DDRAM_BE,
	output        DDRAM_WE,

	//SDRAM interface with lower latency
	output        SDRAM_CLK,
	output        SDRAM_CKE,
	output [12:0] SDRAM_A,
	output  [1:0] SDRAM_BA,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nCS,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nWE,

`ifdef MISTER_DUAL_SDRAM
	//Secondary SDRAM
	//Set all output SDRAM_* signals to Z ASAP if SDRAM2_EN is 0
	input         SDRAM2_EN,
	output        SDRAM2_CLK,
	output [12:0] SDRAM2_A,
	output  [1:0] SDRAM2_BA,
	inout  [15:0] SDRAM2_DQ,
	output        SDRAM2_nCS,
	output        SDRAM2_nCAS,
	output        SDRAM2_nRAS,
	output        SDRAM2_nWE,
`endif

	input         UART_CTS,
	output        UART_RTS,
	input         UART_RXD,
	output        UART_TXD,
	output        UART_DTR,
	input         UART_DSR,

	// Open-drain User port.
	// 0 - D+/RX
	// 1 - D-/TX
	// 2..6 - USR2..USR6
	// Set USER_OUT to 1 to read from USER_IN.
	input   [6:0] USER_IN,
	output  [6:0] USER_OUT,

	input         OSD_STATUS
);

assign {UART_RTS, UART_TXD, UART_DTR} = 0;
assign {SD_SCK, SD_MOSI, SD_CS} = 'Z;
 
assign LED_USER  = ioctl_download | |led_disk | tape_led | mc_nvram_dirty;
assign LED_DISK  = 0;
assign LED_POWER = 0;
assign BUTTONS   = 0;
assign VGA_SCALER= 0;
assign VGA_DISABLE = 0;
assign HDMI_FREEZE = 0;


// Status Bit Map:
//              Upper                          Lower
// 0         1         2         3          4         5         6
// 01234567890123456789012345678901 23456789012345678901234567890123
// 0123456789ABCDEFGHIJKLMNOPQRSTUV 0123456789ABCDEFGHIJKLMNOPQRSTUV
// X XXX XXXXXXXXXXXXXXXXXXXXXXXXXx X


`include "build_id.v" 
parameter CONF_STR = {
	"VIC20;;",
	"S1,D64G64,Mount #8;",
	"S2,D64G64,Mount #9;",
	"-;",
	"F1,PRGCRTCT?TAP,Load;",
	"FS2,CRTROM,Load Megacart;",
	"h4RS,Save NVRAM;",
	"h4OT,Autosave,Off,On;",
	"-;",
	"h3RG,Tape Play/Pause;",
	"h3RI,Tape Unload;",
	"h3OH,Tape Sound,Off,On;",
	"h3OM,Tape Autoplay,Yes,No;",
	"h3-;",
	"OJK,Aspect ratio,Original,Full Screen,[ARC1],[ARC2];",
	"O24,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%,CRT 75%;",
	"OCD,Screen center,Both,None,Horz,Vert;",
	"OE,TV mode,PAL,NTSC;",
	"H2d1ON,Vertical Crop,No,Yes;",
	"h2d1ONO,Vertical Crop,No,270,216;",
	"OPQ,Scale,Normal,V-Integer,Narrower HV-Integer,Wider HV-Integer;",
	"-;",
	"H4O6,ExtRAM $0400(3KB),Off,On;",
	"H4O7,ExtRAM $2000(8KB),Off,On;",
	"H4O8,ExtRAM $4000(8KB),Off,On;",
	"H4O9,ExtRAM $6000(8KB),Off,On;",
	"H4OA,ExtRAM $A000(8KB),Off,On;",
	"H4-;",
	"OL,External IEC,Disabled,Enabled;",
	"OB,Cart is writable,No,Yes;", 
	"-;",
	"OF,Kernal,Loadable,Standard;",
	"FC6,ROM,Load Kernal;",
	"-;",
	"OU,Swap paddles,No,Yes;", // 30
	"OV,Controller,Joystick,Paddle;", // 31
	"o0,Paddle Range, Clamp, Repeat;", // 32
	"-;",
	"R0,Reset;",
	"RR,Reset & Detach Cartridge;",
	"J,Fire,Paddle Fire|P;",

// PADDLE START
	"jn,A,L|P;",
	"I,",
	"Paddle A Assigned,",
	"Paddle B Assigned,",
// PADDLE END

	"V,v",`BUILD_DATE
};

wire [4:0] extram = {status[10:6]};

wire load_prg = (ioctl_index == 'h01);
wire load_crt = (ioctl_index == 'h41);
wire load_ct  = (ioctl_index == 'h81);
wire load_tap = (ioctl_index == 'hC1);
wire load_rom = (ioctl_index == 'h06);
wire load_mc  = (ioctl_index[4:0] == 2);

/////////////////  CLOCKS  ////////////////////////

wire pal = ~status[14];

wire clk_sys;
wire pll_locked;

pll pll
(
	.refclk(CLK_50M),
	.reconfig_to_pll(reconfig_to_pll),
	.reconfig_from_pll(reconfig_from_pll),
	.outclk_0(clk_sys),
	.outclk_1(CLK_VIDEO),
	.locked(pll_locked)
);

wire [63:0] reconfig_to_pll;
wire [63:0] reconfig_from_pll;
wire        cfg_waitrequest;
reg         cfg_write;
reg   [5:0] cfg_address;
reg  [31:0] cfg_data;

pll_cfg pll_cfg
(
	.mgmt_clk(CLK_50M),
	.mgmt_reset(0),
	.mgmt_waitrequest(cfg_waitrequest),
	.mgmt_read(0),
	.mgmt_readdata(),
	.mgmt_write(cfg_write),
	.mgmt_address(cfg_address),
	.mgmt_writedata(cfg_data),
	.reconfig_to_pll(reconfig_to_pll),
	.reconfig_from_pll(reconfig_from_pll)
);

reg tv_reset = 0;
always @(posedge CLK_50M) begin
	reg pald = 0, pald2 = 0;
	reg [2:0] state = 0;

	pald  <= pal;
	pald2 <= pald;

	cfg_write <= 0;
	if(pald2 != pald) state <= 1;

	if(!cfg_waitrequest) begin
		if(state) state<=state+1'd1;
		case(state)
			0: tv_reset <= 0;
			1: begin
					tv_reset <= 1;
					cfg_address <= 0;
					cfg_data <= 0;
					cfg_write <= 1;
				end
			3: begin
					cfg_address <= 7;
					cfg_data <= pald2 ? 3999921597 : 702796321;
					cfg_write <= 1;
				end
			5: begin
					cfg_address <= 2;
					cfg_data <= 0;
					cfg_write <= 1;
				end
		endcase
	end
end

reg v20_en = 0;
always @(negedge clk_sys) begin
	reg [1:0] div = 0;

	div <= div + 1'd1;
	v20_en <= !div;
end


/////////////////  HPS  ///////////////////////////

wire [63:0] status;
wire  [1:0] buttons;

wire [10:0] ps2_key;

wire        ioctl_download;
wire  [7:0] ioctl_index;
wire        ioctl_wr;
reg         ioctl_wait;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;
wire [31:0] ioctl_file_ext;
wire        forced_scandoubler;

wire [31:0] sd_lba[3];
wire  [5:0] sd_blk_cnt[3];
wire  [2:0] sd_rd;
wire  [2:0] sd_wr;
wire  [2:0] sd_ack;
wire [13:0] sd_buff_addr;
wire  [7:0] sd_buff_dout;
wire  [7:0] sd_buff_din[3];
wire        sd_buff_wr;
wire  [2:0] img_mounted;
wire        img_readonly;
wire [31:0] img_size;

wire [21:0] gamma_bus;

// PADDLE START

wire [15:0] joy0,joy1,joy2,joy3;
wire [15:0] joya_0,joya_1,joya_2,joya_3,joyar_0,joyar_1,joyar_2,joyar_3;
wire  [7:0] pd_0,pd_1,pd_2,pd_3;
wire [24:0] ps2_mouse;

logic info_req;
logic [7:0] info;
logic pad0_assigned, pad1_assigned, pad2_assigned, pad3_assigned;

// PADDLE END

hps_io #(.CONF_STR(CONF_STR), .VDNUM(3), .BLKSZ(1)) hps_io
(
	.clk_sys(clk_sys),
	.HPS_BUS(HPS_BUS),

	.buttons(buttons),
	.status(status),
	.status_menumask({mc_loaded,tap_loaded,en1080p,|vcrop,1'b0}),
	.forced_scandoubler(forced_scandoubler),
	.gamma_bus(gamma_bus),
	
	.ps2_key(ps2_key),

	.ioctl_download(ioctl_download),
	.ioctl_index(ioctl_index),
	.ioctl_wr(ioctl_wr),
	.ioctl_addr(ioctl_addr),
	.ioctl_dout(ioctl_dout),
	.ioctl_file_ext(ioctl_file_ext),
	.ioctl_wait(ioctl_wait),

	.sd_lba(sd_lba),
	.sd_blk_cnt(sd_blk_cnt),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),
	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din(sd_buff_din),
	.sd_buff_wr(sd_buff_wr),
	.img_mounted(img_mounted),
	.img_readonly(img_readonly),
	.img_size(img_size),

// PADDLE_START
	
	.joystick_0         (joy0),
	.joystick_1         (joy1),
	.joystick_2         (joy2),
	.joystick_3         (joy3),
	.joystick_l_analog_0  (joya_0),
	.joystick_l_analog_1  (joya_1),
	.joystick_l_analog_2  (joya_2),
	.joystick_l_analog_3  (joya_3),
	.joystick_r_analog_0  (joyar_0),
	.joystick_r_analog_1  (joyar_1),
	.joystick_r_analog_2  (joyar_2),
	.joystick_r_analog_3  (joyar_3),
	.paddle_0           (pd_0),
	.paddle_1           (pd_1),
	.paddle_2           (pd_2),
	.paddle_3           (pd_3),

	.info_req           (info_req),
	.info               (info),

	.ps2_mouse          (ps2_mouse)
	
// PADDLE_END	
);

/////////////////  RESET  /////////////////////////

wire sys_reset = RESET | status[0] | status[27] | buttons[1];
wire reset = sys_reset | cart_reset | mc_reset;

////////////////  LOADING  ////////////////////////

reg  [15:0] dl_addr;
reg   [7:0] dl_data;
reg         dl_wr;
reg         cart_reset = 0;
reg   [4:0] cart_blk = 0;

always @(posedge clk_sys) begin
	reg        old_download = 0;
	reg  [3:0] state = 0;
	reg [15:0] addr;

	dl_wr <= 0;
	old_download <= ioctl_download;

	if(ioctl_download && load_prg) begin
		state <= 0;
		if(ioctl_wr) begin
			     if(ioctl_addr == 0) addr[7:0]  <= ioctl_dout;
			else if(ioctl_addr == 1) addr[15:8] <= ioctl_dout;
			else begin
				if(addr<'hA000) begin
					dl_addr <= addr;
					dl_data <= ioctl_dout;
					dl_wr   <= 1;
					addr    <= addr + 1'd1;
				end
			end
		end
	end

	if(old_download && ~ioctl_download && load_prg) state <= 1;
	if(state) state <= state + 1'd1;

	case(state)
		 1: begin dl_addr <= 16'h2d; dl_data <= addr[7:0];  dl_wr <= 1; end
		 3: begin dl_addr <= 16'h2e; dl_data <= addr[15:8]; dl_wr <= 1; end
		 5: begin dl_addr <= 16'h2f; dl_data <= addr[7:0];  dl_wr <= 1; end
		 7: begin dl_addr <= 16'h30; dl_data <= addr[15:8]; dl_wr <= 1; end
		 9: begin dl_addr <= 16'h31; dl_data <= addr[7:0];  dl_wr <= 1; end
		11: begin dl_addr <= 16'h32; dl_data <= addr[15:8]; dl_wr <= 1; end
		13: begin dl_addr <= 16'hae; dl_data <= addr[7:0];  dl_wr <= 1; end
		15: begin dl_addr <= 16'haf; dl_data <= addr[15:8]; dl_wr <= 1; end
	endcase

	if(ioctl_download && load_rom) begin
		state <= 0;
		if(ioctl_wr) begin
			if(ioctl_addr>='h4000 && ioctl_addr<'h8000) begin
				dl_addr <= ioctl_addr[15:0] + 16'h8000;
				dl_data <= ioctl_dout;
				dl_wr   <= 1;
			end
		end
	end

	if(ioctl_download && (load_crt || load_ct)) begin
		if(ioctl_wr) begin
				  if(ioctl_addr == 0 && load_crt) addr[7:0]  <= ioctl_dout;
			else if(ioctl_addr == 1 && load_crt) addr[15:8] <= ioctl_dout;
			else if(addr < 'hC000) begin
				if(addr[15:13] == 3'b000) cart_blk[0] <= 1;
				if(addr[15:13] == 3'b001) cart_blk[1] <= 1;
				if(addr[15:13] == 3'b010) cart_blk[2] <= 1;
				if(addr[15:13] == 3'b011) cart_blk[3] <= 1;
				if(addr[15:13] == 3'b101) cart_blk[4] <= 1;
				dl_addr <= addr;
				dl_data <= ioctl_dout;
				dl_wr   <= 1;
				addr    <= addr + 1'd1;
			end
		end
	end

	if((old_download ^ ioctl_download) & (load_crt | load_mc)) cart_reset <= ioctl_download;
	if(status[27]) {cart_reset, cart_blk} <= 0;
	if(ioctl_download & load_mc) cart_blk <= 0;

	if(~old_download & ioctl_download & load_ct) begin
		if(ioctl_file_ext[7:0] >= "2" && ioctl_file_ext[7:0] <= "9") addr <= {ioctl_file_ext[3:0],     12'h000};
		if(ioctl_file_ext[7:0] >= "A" && ioctl_file_ext[7:0] <= "B") addr <= {ioctl_file_ext[3:0]+4'd9,12'h000};
	end
end

///////////////////////////////////////////////////

reg mc_loaded = 0;
always @(posedge clk_sys) begin
	if(status[27] | (ioctl_download & load_crt)) mc_loaded <= 0;
	if(ioctl_download & load_mc) mc_loaded <= 1;
end

wire        extmem_sel;
wire        p2_h;

wire        mc_reset;
wire [22:0] mc_addr;
wire        mc_wr_n;
wire        mc_nvram_sel;
wire        mc_rom_sel;

wire        vic_wr_n;
wire        vic_io2_sel;
wire        vic_io3_sel;
wire        vic_blk123_sel;
wire        vic_blk5_sel;
wire        vic_ram123_sel;
wire  [7:0] vic_data;
wire [15:0] vic_addr;

megacart mc
(
	.clk(clk_sys),
	.reset_n(mc_loaded & ~sys_reset & ~cart_reset),

	.vic_addr(vic_addr),
	.vic_wr_n(vic_wr_n),
	.vic_io2_sel(vic_io2_sel),
	.vic_io3_sel(vic_io3_sel),
	.vic_blk123_sel(vic_blk123_sel),
	.vic_blk5_sel(vic_blk5_sel),
	.vic_ram123_sel(vic_ram123_sel),
	.vic_data(vic_data),

	.mc_addr(mc_addr),
	.mc_wr_n(mc_wr_n),
	.mc_nvram_sel(mc_nvram_sel),
	.mc_soft_reset(mc_reset)
);

reg ioctl_wr_d;
always @(posedge clk_sys) ioctl_wr_d <= ioctl_wr;

wire [7:0] sdram_out;
sdram ram
(
    .*,
    .clk(clk_sys),
    .init(~pll_locked),
    .clkref(ioctl_download ? ioctl_wr : p2_h),

    .dout(sdram_out),
    .din ((ioctl_download & load_mc) ? ioctl_dout : vic_data),
    .addr((ioctl_download & load_mc) ? ioctl_addr : mc_addr),
    .we  ((ioctl_download & load_mc) ? ioctl_wr_d : (~mc_nvram_sel & extmem_sel & ~mc_wr_n)),
    .oe  ((ioctl_download & load_mc) ? 1'b0       : (~mc_nvram_sel & extmem_sel & mc_wr_n))
);

wire [7:0] mc_nvram_out;
gen_dpram #(13, 8) mc_nvram
(
	.clock_a(clk_sys),
	.address_a(vic_addr),
	.data_a(vic_data),
	.wren_a(mc_nvram_sel & ~mc_wr_n),
	.q_a(mc_nvram_out),

	.clock_b(clk_sys),
	.address_b(sd_buff_addr),
	.data_b(sd_buff_dout),
	.wren_b(sd_buff_wr & sd_ack[0]),
	.q_b(sd_buff_din[0])
);

assign sd_blk_cnt[0] = 31;
assign sd_lba[0] = 0;

reg mc_nvram_dirty;
always @(posedge clk_sys) begin
	reg old_mounted;
	reg old_status, old_flg;

	if(sd_ack[0] | ~mc_loaded) mc_nvram_dirty <= 0;
	if(mc_nvram_sel & ~mc_wr_n & mc_loaded) mc_nvram_dirty <= 1;
	
	old_mounted <= img_mounted[0];
	if(old_mounted & ~img_mounted[0]) sd_rd[0] <= 1;
	
	old_flg <= status[28];
	old_status <= OSD_STATUS;
	if(((~old_status & OSD_STATUS & status[29]) | (~old_flg & status[28])) & mc_nvram_dirty) sd_wr[0] <= 1;
	
	if(sd_ack[0]) {sd_rd[0], sd_wr[0]} <= 0;
end

wire [7:0] mc_data = mc_nvram_sel ? mc_nvram_out : sdram_out;

///////////////////////////////////////////////////


///// PADDLE
assign info_req = pad0_assigned | pad1_assigned;

always_comb begin
	info = 8'd0;
	if (pad0_assigned)
		info = 8'd1;
	if (pad1_assigned)
		info = 8'd2;
end

wire porta_type = status[31];//, portb_type = status[31];
logic [3:0] pad_b;
logic [3:0] is_paddle;
logic [7:0] pad_ax[4];
logic [15:0] joya_a, joya_b, joya_c, joya_d;
wire [3:0] paddle_mask = porta_type == 1'd1 ? 4'b0011 : 4'b0000;

paddle_chooser paddles
(
	.clk        (clk_sys),
	.reset      (reset),
	.mask       (paddle_mask),
	.enable0    (1'b1),
	.enable1    (1'b1),
	.use_multi  (1'b1),
	.mouse      (ps2_mouse),
	.analog     ({joya_3, joya_2, joya_1, joya_0}),
	.paddle     ({pd_3, pd_2, pd_1, pd_0}),
	.buttons_in ({joy3[4], joy2[4], joy1[4], joy0[4]}),
	.alt_b_in   ({joy3[5], joy2[5], joy1[5], joy0[5]}),
	.paddle_range (status[32]),

	.assigned   ({pad3_assigned, pad2_assigned, pad1_assigned, pad0_assigned}),
	.pd_out     ({pad_ax[3], pad_ax[2], pad_ax[1], pad_ax[0]}),
	.paddle_but (pad_b),
	.is_paddle  (is_paddle)
);


logic [7:0] paddle_scaled[2]; 

paddle_scaler ps0 (clk_sys, reset || ~paddle_mask[0], pad_ax[0][7:0], paddle_scaled[0]);
paddle_scaler ps1 (clk_sys, reset || ~paddle_mask[1], pad_ax[1][7:0], paddle_scaled[1]);

/////

wire		   paddle_swap = status[30];
wire [1:0]	port_type = status[31];
wire [15:0] joy = joy0 | joy1 | joy2 | joy3;
wire        firebutton = (~is_paddle[0] ? joy0[4] : 1'b0) | (~is_paddle[1] ? joy1[4] : 1'b0) | (~is_paddle[2] ? joy2[4] : 1'b0) | (~is_paddle[3] ? joy3[4] : 1'b0);

// joystick directions combined with paddle buttons (paddle a is right, paddle b is left)
wire [1:0] paddle_buttons = paddle_swap ? {pad_b[0], pad_b[1]} : {pad_b[1], pad_b[0]};
wire [1:0] joy_horizontal = {joy[0], joy[1]} | paddle_buttons;

// paddle analog values
//wire [7:0] paddle_a = paddle_swap ? pad_ax[1][7:0] : pad_ax[0][7:0];
//wire [7:0] paddle_b = paddle_swap ? pad_ax[0][7:0] : pad_ax[1][7:0];

wire [7:0] paddle_a = paddle_swap ? paddle_scaled[1][7:0] : paddle_scaled[0][7:0];
wire [7:0] paddle_b = paddle_swap ? paddle_scaled[0][7:0] : paddle_scaled[1][7:0];

reg [10:0] v20_key;
always @(posedge clk_sys) begin
	reg [10:0] key;
	
	key <= ps2_key;
	v20_key <= key;
end

reg rom_std;
always @(posedge clk_sys) if(reset) rom_std <= status[15];

VIC20 VIC20
(
	.i_sysclk(clk_sys),
	.i_sysclk_en(v20_en),
	.i_reset(reset|tv_reset),
	.o_p2h(p2_h),

	//IEC
	.atn_o(v20_iec_atn_o),
	.clk_o(v20_iec_clk_o),
	.data_o(v20_iec_data_o),
	.clk_i(c1541_iec_clk_o & ext_iec_clk),
	.data_i(c1541_iec_data_o & ext_iec_data),
	
	.i_joy(~{joy_horizontal, joy[2], joy[3]}),
	.i_fire(~firebutton),
	.i_potx(paddle_a),
	.i_poty(paddle_b),

	.i_ram_ext_ro(mc_loaded ? 5'b00000 : (cart_blk & ~{5{status[11]}})),
	.i_ram_ext   (mc_loaded ? 5'b11111 : (extram|cart_blk)),

	.o_ce_pix(i_ce_pix),
	.o_video_r(r),
	.o_video_g(g),
	.o_video_b(b),
	.o_hsync(hs),
	.o_vsync(vs),
	.o_hblank(hblank),
	.o_vblank(vblank),
	.i_center(status[13:12]+2'b11),
	.i_pal(pal),
	.i_wide(wide),

	.i_extmem_en(mc_loaded),
	.o_extmem_sel(extmem_sel),
	.o_extmem_r_wn(vic_wr_n),
	.o_extmem_addr(vic_addr),
	.o_extmem_data(vic_data),
	.i_extmem_data(mc_data),
	.o_io2_sel(vic_io2_sel),
	.o_io3_sel(vic_io3_sel),
	.o_blk123_sel(vic_blk123_sel),
	.o_blk5_sel(vic_blk5_sel),
	.o_ram123_sel(vic_ram123_sel),

	.ps2_key(v20_key),
	.tape_play(key_play),

	.o_audio(audio),

	.cass_read(tape_adc_act ? ~tape_adc : cass_read),
	.cass_motor(cass_motor),
	.cass_sw(~tape_adc_act & cass_sense),

	.rom_std(rom_std),
	.conf_clk(clk_sys),
	.conf_ai(dl_addr),
	.conf_di(dl_data),
	.conf_wr(dl_wr)
);

wire v20_iec_atn_o;
wire v20_iec_data_o;
wire v20_iec_clk_o;

wire [5:0] audio;

assign AUDIO_L = {1'b0,audio, 9'd0} + {cass_aud,10'd0};
assign AUDIO_R = AUDIO_L;
assign AUDIO_MIX = 0;
assign AUDIO_S = 0;

wire hs, vs, hblank, vblank, i_ce_pix;
wire [3:0] r,g,b;

wire [2:0] scale = status[4:2];
wire [2:0] sl = scale ? scale - 1'd1 : 3'd0;

wire ce_sd;
assign VGA_F1 = 0;
assign VGA_SL = sl[1:0];

reg ce_pix;
always @(posedge CLK_VIDEO) begin
	reg old_ce;
	
	old_ce <= i_ce_pix;
	ce_pix <= ~old_ce & i_ce_pix;
end


wire [3:0] R,G,B;
wire VSync,HSync,HBlank,VBlank;

reg hs_o, vs_o;
always @(posedge CLK_VIDEO) begin
	if(ce_pix) begin
		hs_o <= ~hs;
		if(~hs_o & ~hs) vs_o <= ~vs;
	end
end

video_cleaner video_cleaner
(
	.clk_vid(CLK_VIDEO),
	.ce_pix(ce_pix),

	.R(r),
	.G(g),
	.B(b),

	.HSync(hs_o),
	.VSync(vs_o),
	.HBlank(hblank),
	.VBlank(vblank),

	// video output signals
	.VGA_R(R),
	.VGA_G(G),
	.VGA_B(B),
	.VGA_VS(VSync),
	.VGA_HS(HSync),

	// optional aligned blank
	.HBlank_out(HBlank),
	.VBlank_out(VBlank)
); 

reg [9:0] vcrop;
reg wide;
always @(posedge CLK_VIDEO) begin
	vcrop <= 0;
	wide <= 0;
	if(HDMI_WIDTH >= (HDMI_HEIGHT + HDMI_HEIGHT[11:1]) && !forced_scandoubler && !scale) begin
		if(HDMI_HEIGHT == 480)  vcrop <= 240;
		if(HDMI_HEIGHT == 600)  begin vcrop <= 200; wide <= vcrop_en; end
		if(HDMI_HEIGHT == 720)  vcrop <= 240;
		if(HDMI_HEIGHT == 768)  vcrop <= 256; // NTSC mode has 245 visible lines only!
		if(HDMI_HEIGHT == 800)  begin vcrop <= 200; wide <= vcrop_en; end
		if(HDMI_HEIGHT == 1080) vcrop <= (~pal | status[24]) ? 10'd216 : 10'd270;
		if(HDMI_HEIGHT == 1200) vcrop <= 240;
	end
end

reg en1080p;
always @(posedge CLK_VIDEO) en1080p <= (HDMI_WIDTH == 1920) && (HDMI_HEIGHT == 1080);

wire [1:0] ar = status[20:19];
wire vcrop_en = en1080p ? |status[24:23] : status[23];
wire vga_de;
video_freak video_freak
(
	.*,
	.VGA_DE_IN(vga_de),
	.ARX((!ar) ? ((wide & pal) ? 12'd330 : (wide & ~pal) ? 12'd390 : 12'd400) : (ar - 1'd1)),
	.ARY((!ar) ? 12'd300 : 12'd0),
	.CROP_SIZE(vcrop_en ? vcrop : 10'd0),
	.CROP_OFF(0),
	.SCALE(status[26:25])
);

video_mixer #(256, 1, 1) mixer
(
	.*,
	.hq2x(scale == 1),
	.scandoubler(scale || forced_scandoubler),
	.freeze_sync(),
	.VGA_DE(vga_de)
);

///////////////////////////////////////////////////

wire [1:0] led_disk;

wire c1541_iec_data_o;
wire c1541_iec_clk_o;

c1541_multi #(.PARPORT(0)) c1541
(
	.clk(clk_sys),
	.reset({reset|tv_reset | ~drive_mounted[1], reset|tv_reset | ~drive_mounted[0]}),
	.ce(ce_c1541),

	.img_mounted(img_mounted[2:1]),
	.img_readonly(img_readonly),
	.img_size(img_size),

	.gcr_mode(2'b11),

	.led(led_disk),

	.iec_atn_i(v20_iec_atn_o),
	.iec_data_i(v20_iec_data_o & ext_iec_data),
	.iec_clk_i(v20_iec_clk_o & ext_iec_clk),
	.iec_data_o(c1541_iec_data_o),
	.iec_clk_o(c1541_iec_clk_o),

	.clk_sys(clk_sys),

	.sd_lba(sd_lba[1:2]),
	.sd_blk_cnt(sd_blk_cnt[1:2]),
	.sd_rd(sd_rd[2:1]),
	.sd_wr(sd_wr[2:1]),
	.sd_ack(sd_ack[2:1]),
	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din(sd_buff_din[1:2]),
	.sd_buff_wr(sd_buff_wr),

	.rom_addr(ioctl_addr[13:0]),
	.rom_data(ioctl_dout),
	.rom_wr(ioctl_wr && (ioctl_addr[24:14] == 0) && load_rom),
	.rom_std(rom_std)
);

reg [1:0] drive_mounted = 0;
always @(posedge clk_sys) begin 
	if(img_mounted[1]) drive_mounted[0] <= |img_size;
	if(img_mounted[2]) drive_mounted[1] <= |img_size;
end

reg ce_c1541;
always @(negedge clk_sys) begin
	int sum = 0;
	int msum;
	
	msum <= pal ? 35468944 : 32727260;

	ce_c1541 <= 0;
	sum = sum + 16000000;
	if(sum >= msum) begin
		sum = sum - msum;
		ce_c1541 <= 1;
	end
end

wire ext_iec_en   = status[21];
wire ext_iec_clk  = USER_IN[2] | ~ext_iec_en;
wire ext_iec_data = USER_IN[4] | ~ext_iec_en;

assign USER_OUT[0] = 1;
assign USER_OUT[1] = 1;
assign USER_OUT[2] = (v20_iec_clk_o & c1541_iec_clk_o)  | ~ext_iec_en;
assign USER_OUT[3] = ~(reset|tv_reset) | ~ext_iec_en;
assign USER_OUT[4] = (v20_iec_data_o & c1541_iec_data_o) | ~ext_iec_en;
assign USER_OUT[5] = v20_iec_atn_o | ~ext_iec_en;
assign USER_OUT[6] = 1;

/////////////////////////////////////////////////

assign DDRAM_CLK = clk_sys;
ddram ddram
(
	.*,
	.addr((ioctl_download & load_tap) ? ioctl_addr : tap_play_addr),
	.dout(tap_data),
	.din(ioctl_dout),
	.we(tap_wr),
	.rd(tap_rd),
	.ready(tap_data_ready)
);

reg tap_wr;
reg [1:0] tap_version;
always @(posedge clk_sys) begin
	reg old_reset;

	old_reset <= reset;
	if(~old_reset && reset) ioctl_wait <= 0;

	tap_wr <= 0;
	if(ioctl_wr & load_tap) begin
		ioctl_wait <= 1;
		tap_wr <= 1;
		if (ioctl_addr == 'h0C) tap_version <= ioctl_dout[1:0];
	end
	else if(~tap_wr & ioctl_wait & tap_data_ready) begin
		ioctl_wait <= 0;
	end
end

reg        tap_rd;
wire       tap_finish;
reg [24:0] tap_play_addr;
reg [24:0] tap_last_addr;
wire [7:0] tap_data;
wire       tap_data_ready;
wire       tap_reset = reset | (ioctl_download & load_tap) | status[18] | tap_finish | (cass_run & ((tap_last_addr - tap_play_addr) < 80));
reg        tap_wrreq;
wire       tap_wrfull;
wire       tap_loaded = (tap_play_addr < tap_last_addr);
wire       cass_sense;
wire       key_play;
reg        tap_autoplay = 0;

always @(posedge clk_sys) begin
	reg tap_cycle = 0;

	if(tap_reset) begin
		//C1530 module requires one more byte at the end due to fifo early check.
		tap_last_addr <= (ioctl_download & load_tap) ? ioctl_addr+2'd2 : 25'd0;
		tap_play_addr <= 0;
		tap_rd <= 0;
		tap_cycle <= 0;
		tap_autoplay <= ioctl_download & load_tap & ~status[22];
	end
	else begin
		tap_rd <= 0;
		tap_wrreq <= 0;
		tap_autoplay <= 0;

		if(~tap_rd & ~tap_wrreq) begin
			if(tap_cycle) begin
				if(tap_data_ready) begin
					tap_play_addr <= tap_play_addr + 1'd1;
					tap_cycle <= 0;
					tap_wrreq <= 1;
				end
			end
			else begin
				if(~tap_wrfull & tap_loaded) begin
					tap_rd <= 1;
					tap_cycle <= 1;
				end
			end
		end
	end
end

reg [26:0] act_cnt;
always @(posedge clk_sys) act_cnt <= act_cnt + (cass_sense ? 4'd1 : 4'd8);
wire tape_led = tap_loaded && (act_cnt[26] ? ((cass_sense | ~cass_motor) && act_cnt[25:18] > act_cnt[7:0]) : act_cnt[25:18] <= act_cnt[7:0]);

wire cass_motor;
wire cass_run;
wire cass_read;
wire cass_aud = cass_read & status[17] & ~cass_sense & ~cass_motor;

c1530 c1530
(
	.clk32(clk_sys),
	.restart_tape(tap_reset),
	
	.wav_mode(0),
	.tap_version(tap_version),

	.host_tap_in(tap_data),
	.host_tap_wrreq(tap_wrreq),
	.tap_fifo_wrfull(tap_wrfull),
	.tap_fifo_error(tap_finish),

	.osd_play_stop_toggle(status[16]|key_play|tap_autoplay),
	.cass_motor(cass_motor),
	.cass_sense(cass_sense),
	.cass_read(cass_read),
	.cass_run(cass_run),
	.ear_input(0)
);

wire tape_adc, tape_adc_act;
ltc2308_tape #(.CLK_RATE(35468944)) ltc2308_tape
(
  .clk(clk_sys),
  .ADC_BUS(ADC_BUS),
  .dout(tape_adc),
  .active(tape_adc_act)
);

endmodule
