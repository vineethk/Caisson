module lease(timer,data1,data2,out1,out2,clk,reset);
input[7:0] timer;
input data1,data2;
output[7:0] out1,out2;
reg mode;
reg[7:0] temp1,temp2;
reg[7:0] local_timer;

input clk;
input reset;
reg[1:0] cur_state;

reg[7:0] out1_wout,out2_wout;
wire mode_win;
reg mode_wout;
wire[7:0] temp1_win;
reg[7:0] temp1_wout;
wire[7:0] temp2_win;
reg[7:0] temp2_wout;
wire[7:0] local_timer_win;
reg[7:0] local_timer_wout;
wire[1:0] cur_state_win;
reg[1:0] cur_state_wout;

reg condition1;
reg data1_data2_group_w;
reg[7:0] temp1_temp2_group_w;

assign out1 = out1_wout;
assign out2 = out2_wout;
assign mode_win = mode;
assign temp1_win = temp1;
assign temp2_win = temp2;
assign local_timer_win = local_timer;
assign cur_state_win = cur_state;

always @ (posedge clk)
begin
	mode <= mode_wout;
	temp1 <= temp1_wout;
	temp2 <= temp2_wout;
	local_timer <= local_timer_wout;
	cur_state <= cur_state_wout;
end

always @ (*)
begin
	mode_wout = mode_win;
	temp1_wout = temp1_win;
	temp2_wout = temp2_win;
	local_timer_wout = local_timer_win;	
	cur_state_wout = cur_state_win;
	if (reset) begin
		mode_wout = 0;
		temp1_wout = 0;
		temp2_wout = 0;
		local_timer_wout = 0;			
		cur_state_wout = 0;
	end 
	else begin
		case (cur_state_win)
			0:	begin					
					if (timer > 0) begin
						local_timer_wout = timer;
						if (mode_win == 0) begin
							mode_wout = 1;
							temp1_wout = 0;
							cur_state_wout = 1;
							condition1 = 0;							
						end 
						else begin
							mode_wout = 0;
							temp2_wout = 0;
							cur_state_wout = 1;
							condition1 = 1;							
						end
					end
				end
			1:	begin
					data1_data2_group_w = condition1?data2:data1;
					temp1_temp2_group_w = condition1?temp2_win:temp1_win;					
					if (local_timer_win == 0) begin
						if (condition1) begin
							out2_wout = temp1_temp2_group_w;
						end
						else begin
							out1_wout = temp1_temp2_group_w;
						end								
						cur_state_wout = 0;
					end
					else begin
						local_timer_wout = local_timer_win - 1;
						if (data1_data2_group_w == 0) begin
							cur_state_wout = 2;
						end 
						else begin
							if (condition1) begin
								temp2_wout = temp1_temp2_group_w + 1;
							end
							else begin
								temp1_wout = temp1_temp2_group_w + 1;
							end								
						end
				  	end
				end
			2:	begin
					data1_data2_group_w = condition1?data2:data1;
					temp1_temp2_group_w = condition1?temp2_win:temp1_win;											
					if (local_timer_win == 0) begin
						if (condition1) begin
							out2_wout = temp1_temp2_group_w;
						end
						else begin
							out1_wout = temp1_temp2_group_w;
						end								
						cur_state_wout = 0;
					end
					else begin
						local_timer_wout = local_timer_win - 1;
						if (data1_data2_group_w == 0) begin
							cur_state_wout = 1;
						end 
						else begin
							if (condition1) begin
								temp2_wout = temp1_temp2_group_w + 2;
							end
							else begin
								temp1_wout = temp1_temp2_group_w + 2;
							end								
						end
				  	end
				end	
			default: ;
		endcase
	end
end
endmodule
