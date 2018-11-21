library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package vect2vect_types is

  type array_of_signed_64 is array (integer range <>) of signed(63 downto 0);
  function toSLV (s : in signed) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return signed;
  function toSLV (value :  vect2vect_types.array_of_signed_64) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return vect2vect_types.array_of_signed_64;
end;

package body vect2vect_types is
  function toSLV (s : in signed) return std_logic_vector is
  begin
    return std_logic_vector(s);
  end;
  function fromSLV (slv : in std_logic_vector) return signed is
  begin
    return signed(slv);
  end;
  function toSLV (value :  vect2vect_types.array_of_signed_64) return std_logic_vector is
    alias ivalue    : vect2vect_types.array_of_signed_64(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 64);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 64) + 1 to i*64) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return vect2vect_types.array_of_signed_64 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : vect2vect_types.array_of_signed_64(0 to slv'length / 64 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 64 to (i+1) * 64 - 1));
    end loop;
    return result;
  end;
end;

