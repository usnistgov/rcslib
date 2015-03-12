with Nml;
with Nml_Test_Format_N_Ada;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with Unchecked_Conversion;
with Interfaces.C;

procedure Nmltest_Read_Ada is
   Connection2 :  Nml.NmlConnection_Access;
   Read_Msg : Nml_Test_Format_N_Ada.Test_Message_Access;
   Ok : Integer := 0;
   Msg_Type : Integer;

begin
   if Ada.Command_Line.Argument_Count < 3 then
      Ada.Text_IO.Put("usage: buffername processname cfgsource");
      Ada.Text_IO.New_Line;
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      return;
   end if;

   Connection2 := Nml.CreateConnection(Nml_Test_Format_N_Ada.Format'Access,
                                       Ada.Command_Line.Argument(1),
                                       Ada.Command_Line.Argument(2),
                                       Ada.Command_Line.Argument(3));

   if True /= Nml.Valid(Connection2) then
      Nml.Free(Connection2);
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      return;
   end if;

   Msg_Type := Nml.Read(Connection2);
   if Msg_Type = Nml_Test_Format_N_Ada.TEST_MESSAGE_TYPE then
      Read_Msg := Nml_Test_Format_N_Ada.NmlMsg_To_Test_Message(Nml.Get_Address(Connection2));
      Ada.Text_IO.Put("Read_Msg.i=");
      Ada.Integer_Text_IO.Put(Integer(Read_Msg.i));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Read_Msg.Ia(1)=");
      Ada.Integer_Text_IO.Put(Integer(Read_Msg.Ia(1)));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Read_Msg.Ia(4)=");
      Ada.Integer_Text_IO.Put(Integer(Read_Msg.Ia(4)));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Read_Msg.Ida_Length=");
      Ada.Integer_Text_IO.Put(Integer(Read_Msg.Ida_Length));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Read_Msg.Ida(1)=");
      Ada.Integer_Text_IO.Put(Integer(Read_Msg.Ida(1)));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Read_Msg.Ida(2)=");
      Ada.Integer_Text_IO.Put(Integer(Read_Msg.Ida(2)));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Read_Msg.Ida(3)=");
      Ada.Integer_Text_IO.Put(Integer(Read_Msg.Ida(3)));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Read_Msg.Ida(4)=");
      Ada.Integer_Text_IO.Put(Integer(Read_Msg.Ida(4)));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("Read_Msg.Ida(8)=");
      Ada.Integer_Text_IO.Put(Integer(Read_Msg.Ida(8)));
      Ada.Text_IO.New_Line;
      --Ada.Text_IO.Put("Read_Msg.AnotherIntArray(2)=");
      --Ada.Integer_Text_IO.Put(Integer(Read_Msg.AnIntArray(2)));
      --Ada.Text_IO.New_Line;
      --Ada.Text_IO.Put("Read_Msg.AnotherIntArray(3)=");
      --Ada.Integer_Text_IO.Put(Integer(Read_Msg.AnIntArray(3)));
      --Ada.Text_IO.New_Line;
      --Ada.Text_IO.Put("Read_Msg.AnotherIntArray(10)=");
      --Ada.Integer_Text_IO.Put(Integer(Read_Msg.AnIntArray(10)));
      --Ada.Text_IO.New_Line;
      --Ada.Text_IO.Put("Read_Msg.AnotherIntDla_Length=");
      --Ada.Integer_Text_IO.Put(Integer(Read_Msg.AnIntDla_Length));
      --Ada.Text_IO.New_Line;
      --Ada.Text_IO.Put("Read_Msg.AnotherIntDla(1)=");
      --Ada.Integer_Text_IO.Put(Integer(Read_Msg.AnIntDla(1)));
      --Ada.Text_IO.New_Line;
      --Ada.Text_IO.Put("Read_Msg.AnotherIntDla(2)=");
      --Ada.Integer_Text_IO.Put(Integer(Read_Msg.AnIntDla(2)));
      --Ada.Text_IO.New_Line;
      --Ada.Text_IO.Put("Read_Msg.AnotherIntDla(3)=");
      --Ada.Integer_Text_IO.Put(Integer(Read_Msg.AnIntDla(3)));
      --Ada.Text_IO.New_Line;
      --Ada.Text_IO.Put("Read_Msg.AnotherIntDla(4)=");
      --Ada.Integer_Text_IO.Put(Integer(Read_Msg.AnIntDla(4)));
      --Ada.Text_IO.New_Line;
      --Ada.Text_IO.Put("Read_Msg.AnotherIntDla(10)=");
      --Ada.Integer_Text_IO.Put(Integer(Read_Msg.AnIntDla(10)));
      --Ada.Text_IO.New_Line;
   end if;
   Nml.Free(Connection2);

end Nmltest_Read_Ada;
