-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.gaussian_types.all;

entity gaussian_topentity is
  port(img    : in std_logic;
       result : out std_logic);
end;

architecture structural of gaussian_topentity is
  signal \#i\ : signed(63 downto 0);
begin
  \#i\ <= to_signed(1,64);

  result <= '1';
end;

