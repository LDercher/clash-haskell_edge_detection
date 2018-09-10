-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.fir_testbench_types.all;
library fir_topentity;

entity fir_testbench is
  port(\FIR.testBench\ : out boolean);
end;

architecture structural of fir_testbench is
  -- FIR.hs:29:1-9
  signal \#FIR.testBench_rec\ : boolean;
  -- FIR.hs:34:5-7
  signal \FIR.testBench_clk\  : std_logic;
  signal \#app_arg\           : signed(15 downto 0);
  signal \#app_arg_0\         : signed(15 downto 0);
  signal \#app_arg_1\         : std_logic;
  signal input_0              : std_logic;
  signal input_1              : std_logic;
  signal input_2              : signed(15 downto 0);
  signal result               : signed(15 downto 0);
begin
  fir_outputverifier_firtestbench_rec : entity fir_outputverifier
    port map
      ( result => \#FIR.testBench_rec\
      , clk    => \FIR.testBench_clk\
      , rst    => \#app_arg_1\
      , i      => \#app_arg\ );

  -- pragma translate_off
  clkgen : process is
    constant half_period : time := 100000 ps / 2;
  begin
    \FIR.testBench_clk\ <= '0';
    wait for 3000 ps;
    while (not \#FIR.testBench_rec\) loop
      \FIR.testBench_clk\ <= not \FIR.testBench_clk\;
      wait for half_period;
      \FIR.testBench_clk\ <= not \FIR.testBench_clk\;
      wait for half_period;
    end loop;
    wait;
  end process;
  -- pragma translate_on

  input_0 <= \FIR.testBench_clk\;

  input_1 <= \#app_arg_1\;

  input_2 <= \#app_arg_0\;

  fir_topentity_app_arg : entity fir_topentity.fir_topentity
    port map
      ( clk    => input_0
      , rst    => input_1
      , eta1   => input_2
      , result => result );

  \#app_arg\ <= result;

  fir_stimuligenerator_app_arg_0 : entity fir_stimuligenerator
    port map
      ( result => \#app_arg_0\
      , clk    => \FIR.testBench_clk\
      , rst    => \#app_arg_1\ );

  -- pragma translate_off
  \#app_arg_1\ <= '1',
             '0' after 2000 ps;
  -- pragma translate_on

  \FIR.testBench\ <= \#FIR.testBench_rec\;
end;

