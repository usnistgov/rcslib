with Nml;
with Nml_Test_Format_N_Ada;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Unchecked_Conversion;
with Interfaces.C;
with Ada.Command_Line;

procedure Nml_TEst_Server_Ada is
   Connection1 :  Nml.NmlConnection_Access;
begin
   if Ada.Command_Line.Argument_Count < 3 then
      Ada.Text_IO.Put("usage: buffername processname cfgsource");
      Ada.Text_IO.New_Line;
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      return;
   end if;

   Nml.Set_To_Server;

   Connection1 := Nml.CreateConnection(Nml_Test_Format_N_Ada.Format'Access,
                                       Ada.Command_Line.Argument(1),
                                       Ada.Command_Line.Argument(2),
                                       Ada.Command_Line.Argument(3));

   if True /= Nml.Valid(Connection1) then
      Nml.Free(Connection1);
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      return;
   end if;

   -- Nml.Run_Servers; does not return until someone presses Control-C or
   -- the process is sent SIGINT;
   Nml.Run_Servers;


   Nml.Free(Connection1);

end Nml_Test_Server_Ada;
