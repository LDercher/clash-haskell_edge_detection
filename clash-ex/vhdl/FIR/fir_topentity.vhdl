-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.fir_types.all;

entity fir_topentity is
  port(-- clock
       clk    : in std_logic;
       -- asynchronous reset: active high
       rst    : in std_logic;
       eta1   : in signed(15 downto 0);
       result : out signed(15 downto 0));
end;

architecture structural of fir_topentity is
  signal \#app_arg\ : fir_types.array_of_signed_16(0 to 3);
  signal bs         : fir_types.array_of_signed_16(0 to 3);
  signal \#vec1\    : fir_types.array_of_signed_16(0 to 3);
begin
  \#vec1\ <= fir_types.array_of_signed_16'( to_signed(2,16)
                                          , to_signed(3,16)
                                          , to_signed(-2,16)
                                          , to_signed(8,16) );

  -- zipWith begin
  zipwith : for i in \#app_arg\'range generate
  begin
    fir_fsaturatingnumsigned_csatmult_0 : entity fir_fsaturatingnumsigned_csatmult
      port map
        ( \#case_alt\ => \#app_arg\(i)
        , a           => \#vec1\(i)
        , b           => bs(i) );
  end generate;
  -- zipWith end

  fir_window_bs : entity fir_window
    port map
      ( result => bs
      , clk    => clk
      , rst    => rst
      , x      => eta1 );

  -- fold begin
  fold : block
    -- given a level and a depth, calculate the corresponding index into the
    -- intermediate array
    function depth2index (levels,depth : in natural) return natural is
    begin
      return (2 ** levels - 2 ** depth);
    end function;

    signal intermediate : fir_types.array_of_signed_16(0 to (2*4)-2);
    constant levels : natural := natural (ceil (log2 (real (4))));
  begin
    -- put input array into the first half of the intermediate array
    intermediate(0 to 4-1) <= \#app_arg\;

    -- Create the tree of instantiated components
    make_tree : if levels /= 0 generate
      tree_depth : for d in levels-1 downto 0 generate
        tree_depth_loop : for i_0 in 0 to (natural(2**d) - 1) generate
          fir_fsaturatingnumsigned_csatplus_1 : entity fir_fsaturatingnumsigned_csatplus
      port map
        ( \#case_alt\ => intermediate(depth2index(levels+1,d+1)+i_0)
        , a           => intermediate(depth2index(levels+1,d+2)+(2*i_0))
        , b           => intermediate(depth2index(levels+1,d+2)+(2*i_0)+1) );
        end generate;
      end generate;
    end generate;

    -- The last element of the intermediate array holds the result
    result <= intermediate((2*4)-2);
  end block;
  -- fold end
end;

