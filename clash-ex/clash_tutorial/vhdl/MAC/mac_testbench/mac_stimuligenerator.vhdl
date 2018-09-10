-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.mac_testbench_types.all;

entity mac_stimuligenerator is
  port(-- clock
       clk    : in std_logic;
       -- asynchronous reset: active high
       rst    : in std_logic;
       result : out mac_testbench_types.tup2_0);
end;

architecture structural of mac_stimuligenerator is
  signal \#tup_app_arg\               : unsigned(1 downto 0);
  signal s                            : unsigned(1 downto 0);
  signal wild                         : signed(63 downto 0);
  signal \#tup_app_arg_selection_res\ : boolean;
  signal \#vec\                       : mac_testbench_types.array_of_tup2_0(0 to 3);
begin
  \#tup_app_arg_selection_res\ <= s < to_unsigned(3,2);

  \#tup_app_arg\ <= s + to_unsigned(1,2) when \#tup_app_arg_selection_res\ else
                    s;

  \#vec\ <= mac_testbench_types.array_of_tup2_0'( ( tup2_0_sel0 => to_signed(1,9)
                                                , tup2_0_sel1 => to_signed(1,9) )
                                                , ( tup2_0_sel0 => to_signed(2,9)
                                                , tup2_0_sel1 => to_signed(2,9) )
                                                , ( tup2_0_sel0 => to_signed(3,9)
                                                , tup2_0_sel1 => to_signed(3,9) )
                                                , ( tup2_0_sel0 => to_signed(4,9)
                                                , tup2_0_sel1 => to_signed(4,9) ) );

  -- index begin
  indexvec : block
    signal vec_index : integer range 0 to 4-1;
  begin
    vec_index <= to_integer((wild))
    -- pragma translate_off
                 mod 4
    -- pragma translate_on
                 ;
    result <= \#vec\(vec_index);
  end block;
  -- index end

  -- register begin 
  mac_stimuligenerator_register : process(clk,rst)
  begin
    if rst = '1' then
      s <= to_unsigned(0,2)
      -- pragma translate_off
      after 1 ps
      -- pragma translate_on
      ;
    elsif rising_edge(clk) then
      s <= \#tup_app_arg\
      -- pragma translate_off
      after 1 ps
      -- pragma translate_on
      ;
    end if;
  end process;
  -- register end

  wild <= (signed(std_logic_vector(resize(s,64))));
end;

