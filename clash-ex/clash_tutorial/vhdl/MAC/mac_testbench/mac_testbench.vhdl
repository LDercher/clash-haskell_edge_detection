-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.mac_testbench_types.all;
library mac_topentity;

entity mac_testbench is
  port(\MAC.testBench\ : out boolean);
end;

architecture structural of mac_testbench is
  -- MAC.hs:23:1-9
  signal \#MAC.testBench_rec\ : boolean;
  -- MAC.hs:28:5-7
  signal \MAC.testBench_clk\  : std_logic;
  signal \#app_arg\           : signed(8 downto 0);
  signal \#app_arg_0\         : mac_testbench_types.tup2_0;
  signal \#app_arg_1\         : std_logic;
  signal input_0              : std_logic;
  signal input_1              : std_logic;
  signal input_2              : mac_testbench_types.tup2_0;
  signal input_2_0            : signed(8 downto 0);
  signal input_2_1            : signed(8 downto 0);
  signal result               : signed(8 downto 0);
begin
  mac_outputverifier_mactestbench_rec : entity mac_outputverifier
    port map
      ( result => \#MAC.testBench_rec\
      , clk    => \MAC.testBench_clk\
      , rst    => \#app_arg_1\
      , i      => \#app_arg\ );

  -- pragma translate_off
  clkgen : process is
    constant half_period : time := 100000 ps / 2;
  begin
    \MAC.testBench_clk\ <= '0';
    wait for 3000 ps;
    while (not \#MAC.testBench_rec\) loop
      \MAC.testBench_clk\ <= not \MAC.testBench_clk\;
      wait for half_period;
      \MAC.testBench_clk\ <= not \MAC.testBench_clk\;
      wait for half_period;
    end loop;
    wait;
  end process;
  -- pragma translate_on

  input_0 <= \MAC.testBench_clk\;

  input_1 <= \#app_arg_1\;

  input_2 <= \#app_arg_0\;

  input_2_0 <= input_2.tup2_0_sel0;

  input_2_1 <= input_2.tup2_0_sel1;

  mac_topentity_app_arg : entity mac_topentity.mac_topentity
    port map
      ( clk      => input_0
      , rst      => input_1
      , \#arg_0\ => input_2_0
      , \#arg_1\ => input_2_1
      , acc      => result );

  \#app_arg\ <= result;

  mac_stimuligenerator_app_arg_0 : entity mac_stimuligenerator
    port map
      ( result => \#app_arg_0\
      , clk    => \MAC.testBench_clk\
      , rst    => \#app_arg_1\ );

  -- pragma translate_off
  \#app_arg_1\ <= '1',
             '0' after 2000 ps;
  -- pragma translate_on

  \MAC.testBench\ <= \#MAC.testBench_rec\;
end;

