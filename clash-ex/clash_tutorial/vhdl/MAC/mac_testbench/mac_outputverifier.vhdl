-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.mac_testbench_types.all;

entity mac_outputverifier is
  port(-- clock
       clk    : in std_logic;
       -- asynchronous reset: active high
       rst    : in std_logic;
       i      : in signed(8 downto 0);
       result : out boolean);
end;

architecture structural of mac_outputverifier is
  signal \#app_arg\                   : boolean;
  signal x                            : signed(8 downto 0);
  signal x_0                          : boolean;
  signal x_1                          : mac_testbench_types.tup2;
  signal \#tup_app_arg\               : unsigned(1 downto 0);
  signal result_0                     : signed(8 downto 0);
  signal s                            : unsigned(1 downto 0);
  signal wild                         : signed(63 downto 0);
  signal \#tup_app_arg_selection_res\ : boolean;
  signal \#vec\                       : mac_testbench_types.array_of_signed_9(0 to 3);
begin
  -- assert begin
  assert_r : block
    -- pragma translate_off
    function slv2string (slv : std_logic_vector) return STRING is
       variable result : string (1 to slv'length);
       variable res_l : string (1 to 3);
       variable r : integer;
     begin
       r := 1;
       for i in slv'range loop
          res_l := std_logic'image(slv(i));
          result(r) := res_l(2);
          r := r + 1;
       end loop;
       return result;
    end;
    signal actual : signed(8 downto 0);
    signal expected : signed(8 downto 0);

    -- pragma translate_on
  begin
    -- pragma translate_off
    actual <= i;
    expected <= x;
    process(clk,rst) is
    begin
      if (rising_edge(clk) or falling_edge(rst)) then
        assert (std_match(toSLV(actual),toSLV(expected))) report (("outputVerifier") & ", expected: " & slv2string(toSLV(expected)) & ", actual: " & slv2string(toSLV(actual))) severity error;
      end if;
    end process;
    -- pragma translate_on

    result <= \#app_arg\;
  end block;
  -- assert end

  -- register begin 
  mac_outputverifier_register : process(clk,rst)
  begin
    if rst = '1' then
      \#app_arg\ <= false
      -- pragma translate_off
      after 1 ps
      -- pragma translate_on
      ;
    elsif rising_edge(clk) then
      \#app_arg\ <= x_0
      -- pragma translate_off
      after 1 ps
      -- pragma translate_on
      ;
    end if;
  end process;
  -- register end

  x <= x_1.tup2_sel0;

  x_0 <= x_1.tup2_sel1;

  x_1 <= ( tup2_sel0 => result_0
         , tup2_sel1 => s = to_unsigned(3,2) );

  \#tup_app_arg_selection_res\ <= s < to_unsigned(3,2);

  \#tup_app_arg\ <= s + to_unsigned(1,2) when \#tup_app_arg_selection_res\ else
                    s;

  \#vec\ <= mac_testbench_types.array_of_signed_9'( to_signed(0,9)
                                                  , to_signed(1,9)
                                                  , to_signed(5,9)
                                                  , to_signed(14,9) );

  -- index begin
  indexvec : block
    signal vec_index : integer range 0 to 4-1;
  begin
    vec_index <= to_integer((wild))
    -- pragma translate_off
                 mod 4
    -- pragma translate_on
                 ;
    result_0 <= \#vec\(vec_index);
  end block;
  -- index end

  -- register begin 
  mac_outputverifier_register_0 : process(clk,rst)
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

