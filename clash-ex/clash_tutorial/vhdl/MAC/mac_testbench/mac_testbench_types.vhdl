library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package mac_testbench_types is

  type array_of_signed_9 is array (integer range <>) of signed(8 downto 0);


  type tup2 is record
    tup2_sel0 : signed(8 downto 0);
    tup2_sel1 : boolean;
  end record;
  type tup2_0 is record
    tup2_0_sel0 : signed(8 downto 0);
    tup2_0_sel1 : signed(8 downto 0);
  end record;


  type array_of_tup2_0 is array (integer range <>) of mac_testbench_types.tup2_0;
  function toSLV (s : in signed) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return signed;
  function toSLV (value :  mac_testbench_types.array_of_signed_9) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return mac_testbench_types.array_of_signed_9;
  function toSLV (u : in unsigned) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return unsigned;
  function toSLV (b : in boolean) return std_logic_vector;
  function fromSLV (sl : in std_logic_vector) return boolean;
  function tagToEnum (s : in signed) return boolean;
  function dataToTag (b : in boolean) return signed;
  function toSLV (p : mac_testbench_types.tup2) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return mac_testbench_types.tup2;
  function toSLV (p : mac_testbench_types.tup2_0) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return mac_testbench_types.tup2_0;
  function toSLV (sl : in std_logic) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic;
  function toSLV (value :  mac_testbench_types.array_of_tup2_0) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return mac_testbench_types.array_of_tup2_0;
end;

package body mac_testbench_types is
  function toSLV (s : in signed) return std_logic_vector is
  begin
    return std_logic_vector(s);
  end;
  function fromSLV (slv : in std_logic_vector) return signed is
  begin
    return signed(slv);
  end;
  function toSLV (value :  mac_testbench_types.array_of_signed_9) return std_logic_vector is
    alias ivalue    : mac_testbench_types.array_of_signed_9(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 9);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 9) + 1 to i*9) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return mac_testbench_types.array_of_signed_9 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : mac_testbench_types.array_of_signed_9(0 to slv'length / 9 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 9 to (i+1) * 9 - 1));
    end loop;
    return result;
  end;
  function toSLV (u : in unsigned) return std_logic_vector is
  begin
    return std_logic_vector(u);
  end;
  function fromSLV (slv : in std_logic_vector) return unsigned is
  begin
    return unsigned(slv);
  end;
  function toSLV (b : in boolean) return std_logic_vector is
  begin
    if b then
      return "1";
    else
      return "0";
    end if;
  end;
  function fromSLV (sl : in std_logic_vector) return boolean is
  begin
    if sl = "1" then
      return true;
    else
      return false;
    end if;
  end;
  function tagToEnum (s : in signed) return boolean is
  begin
    if s = to_signed(0,64) then
      return false;
    else
      return true;
    end if;
  end;
  function dataToTag (b : in boolean) return signed is
  begin
    if b then
      return to_signed(1,64);
    else
      return to_signed(0,64);
    end if;
  end;
  function toSLV (p : mac_testbench_types.tup2) return std_logic_vector is
  begin
    return (toSLV(p.tup2_sel0) & toSLV(p.tup2_sel1));
  end;
  function fromSLV (slv : in std_logic_vector) return mac_testbench_types.tup2 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 8)),fromSLV(islv(9 to 9)));
  end;
  function toSLV (p : mac_testbench_types.tup2_0) return std_logic_vector is
  begin
    return (toSLV(p.tup2_0_sel0) & toSLV(p.tup2_0_sel1));
  end;
  function fromSLV (slv : in std_logic_vector) return mac_testbench_types.tup2_0 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 8)),fromSLV(islv(9 to 17)));
  end;
  function toSLV (sl : in std_logic) return std_logic_vector is
  begin
    return std_logic_vector'(0 => sl);
  end;
  function fromSLV (slv : in std_logic_vector) return std_logic is
    alias islv : std_logic_vector (0 to slv'length - 1) is slv;
  begin
    return islv(0);
  end;
  function toSLV (value :  mac_testbench_types.array_of_tup2_0) return std_logic_vector is
    alias ivalue    : mac_testbench_types.array_of_tup2_0(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 18);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 18) + 1 to i*18) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return mac_testbench_types.array_of_tup2_0 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : mac_testbench_types.array_of_tup2_0(0 to slv'length / 18 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 18 to (i+1) * 18 - 1));
    end loop;
    return result;
  end;
end;

