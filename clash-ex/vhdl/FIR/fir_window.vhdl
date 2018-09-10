-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.fir_types.all;

entity fir_window is
  port(-- clock
       clk    : in std_logic;
       -- asynchronous reset: active high
       rst    : in std_logic;
       x      : in signed(15 downto 0);
       result : out fir_types.array_of_signed_16(0 to 3));
end;

architecture structural of fir_window is
  signal \#x_app_arg\            : fir_types.array_of_signed_16(0 to 2);
  signal \#x_app_arg_0\          : fir_types.array_of_signed_16(0 to 2);
  signal x_0                     : fir_types.array_of_signed_16(0 to 2);
  signal \#vec\                  : fir_types.array_of_signed_16(0 to 3);
  signal \#x_app_arg_projection\ : fir_types.tup2_0;
begin
  \#vec\ <= (fir_types.array_of_signed_16'(fir_types.array_of_signed_16'(fir_types.array_of_signed_16'(0 => x)) & fir_types.array_of_signed_16'(x_0)));

  \#x_app_arg_projection\ <= (\#vec\(0 to 3-1),\#vec\(3 to \#vec\'high));

  \#x_app_arg\ <= \#x_app_arg_projection\.tup2_0_sel0;

  -- register begin 
  fir_window_register : process(clk,rst)
  begin
    if rst = '1' then
      \#x_app_arg_0\ <= (fir_types.array_of_signed_16'(0 to 3-1 =>  to_signed(0,16) ))
      -- pragma translate_off
      after 1 ps
      -- pragma translate_on
      ;
    elsif rising_edge(clk) then
      \#x_app_arg_0\ <= \#x_app_arg\
      -- pragma translate_off
      after 1 ps
      -- pragma translate_on
      ;
    end if;
  end process;
  -- register end

  x_0 <= \#x_app_arg_0\;

  result <= fir_types.array_of_signed_16'(signed'(x) & x_0);
end;

