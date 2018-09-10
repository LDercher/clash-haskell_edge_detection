-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.fir_types.all;

entity fir_fsaturatingnumsigned_csatmult is
  port(a           : in signed(15 downto 0);
       b           : in signed(15 downto 0);
       \#case_alt\ : out signed(15 downto 0));
end;

architecture structural of fir_fsaturatingnumsigned_csatmult is
  signal \rR\                        : std_logic_vector(15 downto 0);
  signal \rL\                        : std_logic_vector(15 downto 0);
  signal \#case_scrut\               : fir_types.tup2_1;
  signal \#case_alt_0\               : signed(15 downto 0);
  signal \#app_arg\                  : std_logic;
  signal \#app_arg_0\                : std_logic;
  signal \#app_arg_1\                : std_logic_vector(16 downto 0);
  signal \#case_alt_selection_res\   : boolean;
  signal \#i\                        : signed(63 downto 0);
  signal \#bv\                       : std_logic_vector(31 downto 0);
  signal \#case_alt_0_selection_res\ : boolean;
  signal \#i_0\                      : signed(63 downto 0);
begin
  \rR\ <= \#case_scrut\.tup2_1_sel1;

  \rL\ <= \#case_scrut\.tup2_1_sel0;

  \#i\ <= to_signed(1,64);

  \#case_alt_selection_res\ <= ((not \#app_arg_0\) or \#app_arg\) = '1';

  \#case_alt\ <= signed(\rR\) when \#case_alt_selection_res\ else
                 \#case_alt_0\;

  \#bv\ <= (std_logic_vector((a * b)));

  \#case_scrut\ <= (\#bv\(\#bv\'high downto 16),\#bv\(16-1 downto 0));

  \#i_0\ <= to_signed(0,64);

  \#case_alt_0_selection_res\ <= ( \rL\(\rL\'high) ) = '0';

  \#case_alt_0\ <= to_signed(32767,16) when \#case_alt_0_selection_res\ else
                   to_signed(-32768,16);

  -- reduceAnd begin
  reduceand : block
    function and_reduce (arg : std_logic_vector) return std_logic is
      variable upper, lower : std_logic;
      variable half         : integer;
      variable argi         : std_logic_vector (arg'length - 1 downto 0);
      variable result       : std_logic;
    begin
      if (arg'length < 1) then
        result := '1';
      else
        argi := arg;
        if (argi'length = 1) then
          result := argi(argi'left);
        else
          half   := (argi'length + 1) / 2; -- lsb-biased tree
          upper  := and_reduce (argi (argi'left downto half));
          lower  := and_reduce (argi (half - 1 downto argi'right));
          result := upper and lower;
        end if;
      end if;
      return result;
    end;
  begin
    \#app_arg\ <= and_reduce(\#app_arg_1\);
  end block;
  -- reduceAnd end

  -- reduceOr begin
  reduceor : block
    function or_reduce (arg : std_logic_vector) return std_logic is
      variable upper, lower : std_logic;
      variable half         : integer;
      variable argi         : std_logic_vector (arg'length - 1 downto 0);
      variable result       : std_logic;
    begin
      if (arg'length < 1) then
        result := '0';
      else
        argi := arg;
        if (argi'length = 1) then
          result := argi(argi'left);
        else
          half   := (argi'length + 1) / 2; -- lsb-biased tree
          upper  := or_reduce (argi (argi'left downto half));
          lower  := or_reduce (argi (half - 1 downto argi'right));
          result := upper or lower;
        end if;
      end if;
      return result;
    end;
  begin
    \#app_arg_0\ <= or_reduce(\#app_arg_1\);
  end block;
  -- reduceOr end

  \#app_arg_1\ <= std_logic_vector'(std_logic_vector'((std_logic_vector'(0 => ( \rR\(\rR\'high) )))) & std_logic_vector'(\rL\));
end;

