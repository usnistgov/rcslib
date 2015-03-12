

with Nml;
with Nml_Msg;
with Nml_Test_Format_N_Ada;
use Nml_Test_Format_N_Ada;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Unchecked_Conversion;
with Interfaces.C;
use Interfaces.C;
with Ada.Command_Line;

procedure Nml_Test_Dl_Write_Ada is
   Connection1 :  Nml.NmlConnection_Access;
   Tst_Msg :  Nml_Test_Format_N_Ada.Test_Message_Access;
   Ok : Integer := 0;
begin
   if Ada.Command_Line.Argument_Count < 4 then
      Ada.Text_IO.Put("usage: buffername processname cfgsource lastvar");
      Ada.Text_IO.New_Line;
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      return;
   end if;
   Connection1 := Nml.CreateConnection(Nml_Test_Format_N_Ada.Format'Access,
                                       Ada.Command_Line.Argument(1),
                                       Ada.Command_Line.Argument(2),
                                       Ada.Command_Line.Argument(3));

   if True /= Nml.Valid(Connection1) then
      Nml.Free(Connection1);
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      return;
   end if;

   Tst_Msg := new Nml_Test_Format_N_Ada.Test_Message;

   Tst_Msg.I := 67;
   Tst_Msg.Ia(1) := 61;
   Tst_Msg.Ia(4) := 64;
   Tst_Msg.Ida_Length := 3;
   Tst_Msg.Ida(1) := 51;
   Tst_Msg.Ida(3) := 53;
   Tst_Msg.Ida(4) := 54;
   Tst_Msg.Ida(8) := 58;

   -- These will be sent.
   Tst_Msg.etd := xxx;
   Tst_Msg.etd2 := uuu;
   Tst_Msg.enumtestvar := aa;
   Tst_Msg.enumtestvar := aa;
   Tst_Msg.Enum_Array(1) := aa;
   Tst_Msg.Enum_Array(2) := bb;
   Tst_Msg.Enum_Array(3) := bb;
   Tst_Msg.Enum_Array(4) := bb;
   Tst_Msg.Enum_Array(5) := bb;
   Tst_Msg.enumtest_dla_length := 3;
   Tst_Msg.Enumtest_Dla(1) := aa;
   Tst_Msg.Enumtest_Dla(2) := bb;
   Tst_Msg.Enumtest_Dla(3) := aa;

   -- These will NOT be sent.
   Tst_Msg.Enumtest_Dla(4) := bb;
   Tst_Msg.Enumtest_Dla(5) := aa;
   Tst_Msg.Enumtest_Dla(6) := bb;
   Tst_Msg.Enumtest_Dla(7) := aa;

   -- These will be sent.
   Tst_Msg.f  := -(1.0E-37);
   Tst_Msg.Three_D_Array(Tst_Msg.Three_D_Array'Last) := 3.33;
   Tst_Msg.cda_length := 3;
   Tst_Msg.Cda(1) := '0';
   Tst_Msg.Cda(2) := '1';
   Tst_Msg.Cda(3) := nul;

   -- This string  will NOT be sent.
   Tst_Msg.Cda(4) := '2';
   Tst_Msg.Cda(5) := '3';
   Tst_Msg.Cda(6) := '4';
   Tst_Msg.Cda(7) := '5';
   Tst_Msg.Cda(8) := nul;


   -- This will be sent.
   Tst_Msg.sda_length:=1;
   Tst_Msg.Sda(1).c:= 'x';

   -- This will NOT be sent.
   Tst_Msg.Sda(2).c := 'y';

   -- Skip the Test some integer length features.
   Tst_Msg.do_int_size_test:=False;

   Tst_Msg.dda_length:=0;
   Tst_Msg.false_bool :=False;
   Tst_Msg.true_bool:=True;

   Tst_Msg.sminusone := -1;
   Tst_Msg.iminusone := -1;
   Tst_Msg.lminusone := -1;
   Tst_Msg.fminusone := -1.0;
   Tst_Msg.dminusone := -1.0;

   -- This will be sent.
   Tst_Msg.lastvar:= Interfaces.C.Long'Value(Ada.Command_Line.Argument(4));

   Ok := Nml.Write(Connection1,Nml_Msg.NmlMsg_Access(Tst_Msg));
   Nml.Free(Connection1);
   Nml_Test_Format_N_Ada.Free(Tst_Msg);

   if 0 /= Ok then
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
   end if;

end Nml_Test_Dl_Write_Ada;
