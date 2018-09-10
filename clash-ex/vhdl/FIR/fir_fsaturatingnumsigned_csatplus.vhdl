-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.fir_types.all;

entity fir_fsaturatingnumsigned_csatplus is
  port(a           : in signed(15 downto 0);
       b           : in signed(15 downto 0);
       \#case_alt\ : out signed(15 downto 0));
end;

architecture structural of fir_fsaturatingnumsigned_csatplus is
  signal r                           : signed(16 downto 0);
  signal \#app_arg\                  : std_logic_vector(16 downto 0);
  signal \r'\                        : std_logic_vector(15 downto 0);
  signal \#case_alt_0\               : signed(15 downto 0);
  signal \#r'_projection\            : fir_types.tup2;
  signal \#case_alt_0_selection_res\ : boolean;
  signal \#bv\                       : std_logic_vector(15 downto 0);
  signal \#bv_0\                     : std_logic_vector(15 downto 0);
  signal \#i\                        : signed(63 downto 0);
  signal \#case_alt_selection_res\   : boolean;
  signal \#i_0\                      : signed(63 downto 0);
begin
  r <= resize(a,17) + resize(b,17);

  \#app_arg\ <= std_logic_vector(r);

  \#r'_projection\ <= (\#app_arg\(\#app_arg\'high downto 16),\#app_arg\(16-1 downto 0));

  \r'\ <= \#r'_projection\.tup2_sel1;

  \#bv\ <= (std_logic_vector(a));

  \#bv_0\ <= (std_logic_vector(b));

  \#i\ <= to_signed(0,64);

  \#case_alt_0_selection_res\ <= (( \#bv\(\#bv\'high) ) and ( \#bv_0\(\#bv_0\'high) )) = '0';

  \#case_alt_0\ <= to_signed(32767,16) when \#case_alt_0_selection_res\ else
                   to_signed(-32768,16);

  \#i_0\ <= to_signed(0,64);

  \#case_alt_selection_res\ <= (( \#app_arg\(\#app_arg\'high) ) xor ( \r'\(\r'\'high) )) = '0';

  \#case_alt\ <= signed(\r'\) when \#case_alt_selection_res\ else
                 \#case_alt_0\;
end;

