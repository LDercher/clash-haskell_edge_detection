-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.mac_types.all;

entity mac_topentity is
  port(-- clock
       clk      : in std_logic;
       -- asynchronous reset: active high
       rst      : in std_logic;
       \#arg_0\ : in signed(8 downto 0);
       \#arg_1\ : in signed(8 downto 0);
       acc      : out signed(8 downto 0));
end;

architecture structural of mac_topentity is
  signal \#acc_rec\ : signed(8 downto 0);
  signal x          : signed(8 downto 0);
  signal y          : signed(8 downto 0);
  signal x_0        : signed(8 downto 0);
  signal \#arg\     : mac_types.tup2;
begin
  \#arg\ <= ( tup2_sel0 => \#arg_0\
            , tup2_sel1 => \#arg_1\ );

  -- register begin 
  mac_topentity_register : process(clk,rst)
  begin
    if rst = '1' then
      \#acc_rec\ <= to_signed(0,9)
      -- pragma translate_off
      after 1 ps
      -- pragma translate_on
      ;
    elsif rising_edge(clk) then
      \#acc_rec\ <= x_0
      -- pragma translate_off
      after 1 ps
      -- pragma translate_on
      ;
    end if;
  end process;
  -- register end

  x <= \#arg\.tup2_sel0;

  y <= \#arg\.tup2_sel1;

  x_0 <= \#acc_rec\ + (resize(x * y, 9));

  acc <= \#acc_rec\;
end;

