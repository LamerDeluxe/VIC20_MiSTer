// k7800 (c) by Jamie Blanks

// k7800 is licensed under a
// Creative Commons Attribution-NonCommercial 4.0 International License.

// You should have received a copy of the license along with this
// work. If not, see http://creativecommons.org/licenses/by-nc/4.0/.

module paddle_select
(
	input       clk,
	input [7:0] value,
	output      select
);
	logic [7:0] last_value;
	always_ff @(posedge clk) begin
		last_value <= value;
		if (last_value != value && (&value[7:6] || ~|value[7:6]))
			select <= 1;
		else
			select <= 0;
	end
endmodule

module paddle_chooser
(
	input   logic                   clk,            // System clock
	input   logic                   reset,          // Cold Reset
	input   logic   [3:0]           mask,           // Mask of enabled paddles
	input   logic                   enable0,        // Port 0 Enable
	input   logic                   enable1,        // Port 1 Enable
	input   logic                   use_multi,      // Allows the same joystick to assign multiple axis

	input   logic   [24:0]          mouse,          // PS2 Mouse Input
	input   logic   [3:0][15:0]     analog,         // Analog stick for controller X
	input   logic   [3:0][7:0]      paddle,         // Spinner input X
	input   logic   [3:0]           buttons_in,
	input   logic   [3:0]           alt_b_in,

	input	  logic                   paddle_range,	// clamped or repeating (mouse/spinner) 

	output  logic   [3:0]           assigned,       // Momentary input assigned signal
	output  logic   [3:0][7:0]      pd_out,         // Paddle output data
	output  logic   [3:0][3:0]      paddle_type,    // Paddle output data type
	output  logic   [3:0]           paddle_but,		// Paddle buttons
	output  logic   [3:0]           is_paddle       // Paddle detected on this port
);

	// Determinations:
	// Mouse -- Button pressed
	// Spinner/Analog -- Movement to extremes

	logic [3:0] xs_assigned, ys_assigned, pdi_assigned, paddle_assigned, output_assigned;
	logic mouse_assigned;
	logic [1:0] types[4];
	logic [1:0] index[4];
	logic [3:0] analog_axis;
	logic mouse_button;
	logic [3:0][7:0] xs_unsigned;
	logic [3:0][7:0] ys_unsigned;
	logic [3:0] xs_select, ys_select, p_select;

	assign xs_unsigned[0] = {~analog[0][7], analog[0][6:0]};
	assign xs_unsigned[1] = {~analog[1][7], analog[1][6:0]};
	assign xs_unsigned[2] = {~analog[2][7], analog[2][6:0]};
	assign xs_unsigned[3] = {~analog[3][7], analog[3][6:0]};

	assign ys_unsigned[0] = {~analog[0][15], analog[0][14:8]};
	assign ys_unsigned[1] = {~analog[1][15], analog[1][14:8]};
	assign ys_unsigned[2] = {~analog[2][15], analog[2][14:8]};
	assign ys_unsigned[3] = {~analog[3][15], analog[3][14:8]};

	paddle_select pdjx0(clk, xs_unsigned[0], xs_select[0]);
	paddle_select pdjx1(clk, xs_unsigned[1], xs_select[1]);
	paddle_select pdjx2(clk, xs_unsigned[2], xs_select[2]);
	paddle_select pdjx3(clk, xs_unsigned[3], xs_select[3]);

	paddle_select pdjy0(clk, ys_unsigned[0], ys_select[0]);
	paddle_select pdjy1(clk, ys_unsigned[1], ys_select[1]);
	paddle_select pdjy2(clk, ys_unsigned[2], ys_select[2]);
	paddle_select pdjy3(clk, ys_unsigned[3], ys_select[3]);

	paddle_select pdp0(clk, paddle[0], p_select[0]);
	paddle_select pdp1(clk, paddle[1], p_select[1]);
	paddle_select pdp2(clk, paddle[2], p_select[2]);
	paddle_select pdp3(clk, paddle[3], p_select[3]);

	logic [3:0][15:0] old_analog;
	logic [3:0][7:0] old_paddle;
	logic [3:0] use_alt_buttons;
	logic old_stb;
	reg  signed [8:0] mx = 0;
	wire signed [8:0] mdx = {mouse[4],mouse[15:8]};
	wire signed [8:0] mdx2 = (mdx > 64) || (mouse[6] && ~mouse[4]) ? 9'd64 : (mdx < -64) || (mouse[6] && mouse[4]) ? -8'd64 : mdx;
	wire signed [8:0] nmx = mx + mdx2;

	assign mouse_button = mouse[0];

	always_comb begin
		paddle_but = 4'b0000;
		for (logic [2:0] x = 0; x < 3'd4; x = x + 2'd1) begin
			pd_out[x] = 8'd0;
			if (output_assigned[x]) begin
				case (paddle_type[x])
					0: begin pd_out[x] = ~paddle[index[x]]; paddle_but[x] = buttons_in[index[x]]; end
					1: begin pd_out[x] = ~(analog_axis[x] ? xs_unsigned[index[x]][7:0] : ys_unsigned[index[x]][7:0]); paddle_but[x] = ~use_alt_buttons[x] ? buttons_in[index[x]] : alt_b_in[index[x]]; end
					2: begin pd_out[x] = ~{~mx[7], mx[6:0]}; paddle_but[x] = mouse_button; end // | ((~xs_assigned[0] & ~ys_assigned[0]) ? buttons_in[0] : 1'b0); end
					default: ;
				endcase
			end
		end
	end

	always @(posedge clk) begin
		reg current_assign;
		assigned <= '0;
		current_assign = 0; // NOTE: This is blocking!
		for (logic [2:0] y = 0; y < 3'd4; y = y + 2'd1) begin
			if (~output_assigned[y] && ((y==0 || ~mask[y[1:0]-1'd1]) ? 1'b1 : output_assigned[y[1:0]-1'd1]) && mask[y] && ~current_assign) begin
				for (logic [2:0] x = 0; x < 3'd4; x = x + 2'd1) begin
					if (xs_select[x] && ~xs_assigned[x]) begin
						assigned[y] <= 1;
						xs_assigned[x] <= 1;
						current_assign = 1;
						ys_assigned[x] <= ~use_multi | ys_assigned[x];
						output_assigned[y] <= 1;
						paddle_type[y] <= 1;
						index[y] <= x[1:0];
						analog_axis[y] <= 1;
						is_paddle[x] <= 1;
					end
				end
				for (logic [2:0] x = 0; x < 3'd4; x = x + 2'd1) begin
					if (ys_select[x] && ~ys_assigned[x]) begin
						assigned[y] <= 1;
						ys_assigned[x] <= 1;
						xs_assigned[x] <= ~use_multi | xs_assigned[x];
						current_assign = 1;
						output_assigned[y] <= 1;
						use_alt_buttons[y] <= use_multi;
						paddle_type[y] <= 1;
						analog_axis[y] <= 0;
						index[y] <= x[1:0];
						is_paddle[x] <= 1;
					end
				end
				for (logic [2:0] x = 0; x < 3'd4; x = x + 2'd1) begin
					if (p_select[x] && ~pdi_assigned[x]) begin
						assigned[y] <= 1;
						pdi_assigned[x] <= 1;
						current_assign = 1;
						output_assigned[y] <= 1;
						paddle_type[y] <= 0;
						index[y] <= x[1:0];
						is_paddle[x] <= 1;
					end
				end
				if (~mouse_assigned && mouse_button) begin
					assigned[y] <= 1;
					mouse_assigned <= 1;
					current_assign = 1;
					output_assigned[y] <= 1;
					paddle_type[y] <= 2;
					index[y] <= 0;
				end
			end
		end
		old_stb <= mouse[24];
		if(old_stb != mouse[24]) begin
			mx <= paddle_range ? nmx[7:0] : (nmx < -128 ? -9'd128 : (nmx > 127 ? 9'd127 : nmx));
		end

		if (reset) begin
			mouse_assigned <= 0;
			assigned <= '0;
			index <= '{2'd0, 2'd0, 2'd0, 2'd0};
			pdi_assigned <= '0;
			xs_assigned <= '0;
			ys_assigned <= '0;
			output_assigned <= '0;
			use_alt_buttons <= '0;
			paddle_type <= '0;
			is_paddle <= '0;
		end
	end
endmodule

module paddle_scaler (
	input clk,
	input reset,
	input [7:0] paddle_value,
	output logic [7:0] scaled_value
);

	logic [7:0] min_value, max_value, range;

	// Fixed-point 8.8 1/n table (255 values, no division by zero)
	logic [15:0] fixed_point_divide [0:254] = '{
		16'hFFFF, 16'h7FFF, 16'h5555, 16'h3FFF, 16'h3333, 16'h2AAA, 16'h2492, 16'h1FFF,
		16'h1C71, 16'h1999, 16'h1745, 16'h1555, 16'h13B1, 16'h1249, 16'h1111, 16'hFFF,
		16'hF0F, 16'hE38, 16'hD79, 16'hCCC, 16'hC30, 16'hBA2, 16'hB21, 16'hAAA,
		16'hA3D, 16'h9D8, 16'h97B, 16'h924, 16'h8D3, 16'h888, 16'h842, 16'h7FF,
		16'h7C1, 16'h787, 16'h750, 16'h71C, 16'h6EB, 16'h6BC, 16'h690, 16'h666,
		16'h63E, 16'h618, 16'h5F4, 16'h5D1, 16'h5B0, 16'h590, 16'h572, 16'h555,
		16'h539, 16'h51E, 16'h505, 16'h4EC, 16'h4D4, 16'h4BD, 16'h4A7, 16'h492,
		16'h47D, 16'h469, 16'h456, 16'h444, 16'h432, 16'h421, 16'h410, 16'h3FF,
		16'h3F0, 16'h3E0, 16'h3D2, 16'h3C3, 16'h3B5, 16'h3A8, 16'h39B, 16'h38E,
		16'h381, 16'h375, 16'h369, 16'h35E, 16'h353, 16'h348, 16'h33D, 16'h333,
		16'h329, 16'h31F, 16'h315, 16'h30C, 16'h303, 16'h2FA, 16'h2F1, 16'h2E8,
		16'h2E0, 16'h2D8, 16'h2D0, 16'h2C8, 16'h2C0, 16'h2B9, 16'h2B1, 16'h2AA,
		16'h2A3, 16'h29C, 16'h295, 16'h28F, 16'h288, 16'h282, 16'h27C, 16'h276,
		16'h270, 16'h26A, 16'h264, 16'h25E, 16'h259, 16'h253, 16'h24E, 16'h249,
		16'h243, 16'h23E, 16'h239, 16'h234, 16'h230, 16'h22B, 16'h226, 16'h222,
		16'h21D, 16'h219, 16'h214, 16'h210, 16'h20C, 16'h208, 16'h204, 16'h1FF,
		16'h1FC, 16'h1F8, 16'h1F4, 16'h1F0, 16'h1EC, 16'h1E9, 16'h1E5, 16'h1E1,
		16'h1DE, 16'h1DA, 16'h1D7, 16'h1D4, 16'h1D0, 16'h1CD, 16'h1CA, 16'h1C7,
		16'h1C3, 16'h1C0, 16'h1BD, 16'h1BA, 16'h1B7, 16'h1B4, 16'h1B2, 16'h1AF,
		16'h1AC, 16'h1A9, 16'h1A6, 16'h1A4, 16'h1A1, 16'h19E, 16'h19C, 16'h199,
		16'h197, 16'h194, 16'h192, 16'h18F, 16'h18D, 16'h18A, 16'h188, 16'h186,
		16'h183, 16'h181, 16'h17F, 16'h17D, 16'h17A, 16'h178, 16'h176, 16'h174,
		16'h172, 16'h170, 16'h16E, 16'h16C, 16'h16A, 16'h168, 16'h166, 16'h164,
		16'h162, 16'h160, 16'h15E, 16'h15C, 16'h15A, 16'h158, 16'h157, 16'h155,
		16'h153, 16'h151, 16'h150, 16'h14E, 16'h14C, 16'h14A, 16'h149, 16'h147,
		16'h146, 16'h144, 16'h142, 16'h141, 16'h13F, 16'h13E, 16'h13C, 16'h13B,
		16'h139, 16'h138, 16'h136, 16'h135, 16'h133, 16'h132, 16'h130, 16'h12F,
		16'h12E, 16'h12C, 16'h12B, 16'h129, 16'h128, 16'h127, 16'h125, 16'h124,
		16'h123, 16'h121, 16'h120, 16'h11F, 16'h11E, 16'h11C, 16'h11B, 16'h11A,
		16'h119, 16'h118, 16'h116, 16'h115, 16'h114, 16'h113, 16'h112, 16'h111,
		16'h10F, 16'h10E, 16'h10D, 16'h10C, 16'h10B, 16'h10A, 16'h109, 16'h108,
		16'h107, 16'h106, 16'h105, 16'h104, 16'h103, 16'h102, 16'h101
	};

	always @(posedge clk) begin

		if (~reset) begin
			if (min_value > paddle_value) min_value <= paddle_value;
			if (max_value < paddle_value) max_value <= paddle_value;
			range <= max_value - min_value;
		end
		else begin
			// Initialize at a safe range, to have a smooth range adjustment experience
			min_value <= 32;
			max_value <= 64;
			range <= max_value - min_value;	
		end
		
		scaled_value = ((paddle_value - min_value) * fixed_point_divide[range - 8'd1]) >> 4'd8;
	end
endmodule