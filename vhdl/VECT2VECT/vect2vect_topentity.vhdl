-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.vect2vect_types.all;

entity vect2vect_topentity is
  port(arr_0    : in signed(63 downto 0);
       arr_1    : in signed(63 downto 0);
       arr_2    : in signed(63 downto 0);
       arr_3    : in signed(63 downto 0);
       arr_4    : in signed(63 downto 0);
       arr_5    : in signed(63 downto 0);
       arr_6    : in signed(63 downto 0);
       arr_7    : in signed(63 downto 0);
       arr_8    : in signed(63 downto 0);
       arr_9    : in signed(63 downto 0);
       result_0 : out signed(63 downto 0);
       result_1 : out signed(63 downto 0);
       result_2 : out signed(63 downto 0);
       result_3 : out signed(63 downto 0);
       result_4 : out signed(63 downto 0);
       result_5 : out signed(63 downto 0);
       result_6 : out signed(63 downto 0);
       result_7 : out signed(63 downto 0);
       result_8 : out signed(63 downto 0);
       result_9 : out signed(63 downto 0));
end;

architecture structural of vect2vect_topentity is
  signal arr    : vect2vect_types.array_of_signed_64(0 to 9);
  signal result : vect2vect_types.array_of_signed_64(0 to 9);
begin
  arr <= vect2vect_types.array_of_signed_64'( arr_0
                                            , arr_1
                                            , arr_2
                                            , arr_3
                                            , arr_4
                                            , arr_5
                                            , arr_6
                                            , arr_7
                                            , arr_8
                                            , arr_9 );

  -- map begin
  map_r : for i in result'range generate
  begin
    result(i) <= arr(i) + to_signed(1,64);
  end generate;
  -- map end

  result_0 <= result(0);

  result_1 <= result(1);

  result_2 <= result(2);

  result_3 <= result(3);

  result_4 <= result(4);

  result_5 <= result(5);

  result_6 <= result(6);

  result_7 <= result(7);

  result_8 <= result(8);

  result_9 <= result(9);
end;

