with Nml;
with Nml_Msg; use Nml_Msg;
with Nml_Test_Format_N_Ada;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Unchecked_Conversion;
with Interfaces.C;
with Ada.Command_Line;

procedure Nmltest_Write_Ada is
   Connection1 :  Nml.NmlConnection_Access;
   Msg :  Nml_Test_Format_N_Ada.Test_Message_Access;
   Ok : Integer := 0;
begin
   if Ada.Command_Line.Argument_Count < 3 then
      Ada.Text_IO.Put("usage: buffername processname cfgsource");
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

   Msg := new Nml_Test_Format_N_Ada.Test_Message;
   Msg.I := 67;
   Msg.Ia(1) := 61;
   Msg.Ia(4) := 64;
   Msg.Ida_Length := 3;
   Msg.Ida(1) := 51;
   Msg.Ida(3) := 53;
   Msg.Ida(4) := 54;
   Msg.Ida(8) := 58;

   --Msg.AnotherInt := 67;
   --Msg.AnIntArray(1):=1;
   --Msg.AnIntArray(2):=2;
   --Msg.AnIntArray(3):=3;
   --Msg.AnIntArray(4):=4;
   --Msg.AnIntArray(5):=5;
   --Msg.AnIntArray(10):=10;

   --Msg.AnIntDla_Length:=3;
   --Msg.AnIntDla(1):=1;
   --Msg.AnIntDla(2):=2;
   --Msg.AnIntDla(3):=3;
   --Msg.AnIntDla(4):=4;
   --Msg.AnIntDla(5):=5;
   --Msg.AnIntDla(10):=10;

   Ok := Nml.Write(Connection1,NmlMsg_Access(Msg));
   Nml.Free(Connection1);
   Nml_Test_Format_N_Ada.Free(Msg);
end Nmltest_Write_Ada;
