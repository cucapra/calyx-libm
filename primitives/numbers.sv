// verilator lint_off MULTITOP
// verilator lint_off WIDTH

module num_rpad #(
    parameter int IN_WIDTH  = 32,
    parameter int OUT_WIDTH = 32
) (
    input  logic [ IN_WIDTH-1:0] in,
    output logic [OUT_WIDTH-1:0] out
);
  localparam int EXTEND = OUT_WIDTH - IN_WIDTH;

  assign out = {in, {EXTEND{1'b0}}};
endmodule

module num_sneg #(
    parameter int WIDTH = 32
) (
    input  logic signed [WIDTH-1:0] in,
    output logic signed [WIDTH-1:0] out
);
  assign out = -in;
endmodule

module num_sabs #(
    parameter int WIDTH = 32
) (
    input  logic signed [WIDTH-1:0] in,
    output logic signed [WIDTH-1:0] out
);
  assign out = in[WIDTH-1] ? -in : in;
endmodule

module num_min #(
    parameter int WIDTH = 32
) (
    input  logic [WIDTH-1:0] left,
    input  logic [WIDTH-1:0] right,
    output logic [WIDTH-1:0] out
);
  assign out = (left < right) ? left : right;
endmodule

module num_max #(
    parameter int WIDTH = 32
) (
    input  logic [WIDTH-1:0] left,
    input  logic [WIDTH-1:0] right,
    output logic [WIDTH-1:0] out
);
  assign out = (left > right) ? left : right;
endmodule

module num_smin #(
    parameter int WIDTH = 32
) (
    input  logic signed [WIDTH-1:0] left,
    input  logic signed [WIDTH-1:0] right,
    output logic signed [WIDTH-1:0] out
);
  assign out = (left < right) ? left : right;
endmodule

module num_smax #(
    parameter int WIDTH = 32
) (
    input  logic signed [WIDTH-1:0] left,
    input  logic signed [WIDTH-1:0] right,
    output logic signed [WIDTH-1:0] out
);
  assign out = (left > right) ? left : right;
endmodule

module num_ssignbit #(
    parameter int WIDTH = 32
) (
    input logic signed [WIDTH-1:0] in,
    output logic out
);
  assign out = in[WIDTH-1];
endmodule

module num_sadd #(
    parameter int LEFT_WIDTH = 32,
    parameter int LEFT_LSB = 0,
    parameter int RIGHT_WIDTH = 32,
    parameter int RIGHT_LSB = 0,
    parameter int OUT_WIDTH = 33
) (
    input  logic signed [ LEFT_WIDTH-1:0] left,
    input  logic signed [RIGHT_WIDTH-1:0] right,
    output logic signed [  OUT_WIDTH-1:0] out
);
  logic signed [  LEFT_LSB+LEFT_WIDTH-1:0] left_aligned;
  logic signed [RIGHT_LSB+RIGHT_WIDTH-1:0] right_aligned;

  assign left_aligned = {left, {LEFT_LSB{1'b0}}};
  assign right_aligned = {right, {RIGHT_LSB{1'b0}}};

  assign out = left_aligned + right_aligned;
endmodule

module num_smul #(
    parameter int LEFT_WIDTH = 32,
    parameter int RIGHT_WIDTH = 32,
    parameter int OUT_WIDTH = 64,
    parameter int OUT_LSB = 0
) (
    input  logic signed [ LEFT_WIDTH-1:0] left,
    input  logic signed [RIGHT_WIDTH-1:0] right,
    input  logic                          go,
    input  logic                          reset,
    input  logic                          clk,
    output logic signed [  OUT_WIDTH-1:0] out,
    output logic                          done
);
  logic [1:0] done_buffer;

  assign done = done_buffer[1];

  logic signed [       LEFT_WIDTH-1:0] left_tmp;
  logic signed [      RIGHT_WIDTH-1:0] right_tmp;
  logic signed [OUT_LSB+OUT_WIDTH-1:0] out_tmp;

  assign out = out_tmp[OUT_LSB+OUT_WIDTH-1:OUT_LSB];

  always_ff @(posedge clk) begin
    if (go) begin
      done_buffer[0] <= 1;
      done_buffer[1] <= done_buffer[0];
    end else begin
      done_buffer <= 0;
    end
  end

  always_ff @(posedge clk) begin
    if (!reset && go) begin
      left_tmp  <= left;
      right_tmp <= right;
    end else begin
      left_tmp  <= 0;
      right_tmp <= 0;
    end
  end

  always_ff @(posedge clk) begin
    if (reset) begin
      out_tmp <= 0;
    end else if (go) begin
      out_tmp <= left_tmp * right_tmp;
    end else begin
      out_tmp <= out_tmp;
    end
  end
endmodule
