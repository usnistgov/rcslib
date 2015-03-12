with Nml;
use Nml;
with Mynmlmsg;
use Mynmlmsg;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Unchecked_Conversion;
with Interfaces.C;

procedure Nmltest is
   Connection1 :  NmlConnection_Access;
   Connection2 :  NmlConnection_Access;
   Read_Msg : Mynmlmsg_Type_Access;
   Msg :  Mynmlmsg_Type_Access;
   Ok : Integer := 0;
   Msg_Type : Integer;

begin
   Msg := new Mynmlmsg_Type;
   Msg.AnotherInt := 67;
   Connection1 := Nml.CreateConnection(Mynmlmsg.Format'Access,"b1","proc","nmlcfgsvr::::options=neutral=1");
   Connection2 := Nml.CreateConnection(Mynmlmsg.Format'Access,"b1","proc","nmlcfgsvr::::options=neutral=1");
   Ok := Nml.Write(Connection1,NmlMsg_Access(Msg));
   Msg_Type := Nml.Read(Connection2);
   if Msg_Type = 1001 then
      Read_Msg := Mynmlmsg.NmlMsg_Access_To_Mynmlmsg_Type_Access(Nml.Get_Address(Connection2));
      Ada.Text_IO.Put("Read_Msg.AnotherInt=");
      Ada.Integer_Text_IO.Put(Integer(Read_Msg.AnotherInt));
      Ada.Text_IO.New_Line;
   end if;
   Nml.Free(Connection2);
   Nml.Free(Connection1);

end Nmltest;
