
with Unchecked_Conversion;
with Ada;
with Nml;
use Nml;
with Ada.Text_IO, Ada.Integer_Text_IO;
with Interfaces.C;
use Interfaces.C;
with Ada.Finalization;
use Ada.Finalization;

package body Mynmlmsg is

   function Format(Param_1 : in long;
                   param_2 : in NmlMsg_Access;
                   Param_3 : in Cms_Access)
                  return Interfaces.C.Int is
      ReturnValue : int :=0;
   begin
      Ada.Text_IO.Put("param_1=");
      Ada.Integer_Text_IO.Put(Integer(Param_1));
      Ada.Text_IO.New_Line;
      if Param_2 /= null then
         Ada.Text_IO.Put("param_2.NmlType=");
         Ada.Integer_Text_IO.Put(Integer(Param_2.NmlType));
         Ada.Text_IO.New_Line;
         if Param_1 = 1001 then
            UpdateMynmlmsg_Type(Param_3,
                                NmlMsg_Access_To_Mynmlmsg_Type_Access(Param_2));
         end if;
      end if;
      return ReturnValue;
   end Format;

   procedure UpdateMynmlmsg_Type(Cms : in NML.Cms_Access ;
                                 Msg : in Mynmlmsg_Type_Access ) is
   begin
      CmsUpdateInt(Cms,"AnotherInt",Msg.AnotherInt);
      CmsUpdateIntArray(Cms,"AnIntArray",Msg.AnIntArray,10);
      CmsUpdateInt(Cms,"AnIntDla_Lenght",Msg.AnIntDla_Length);
      CmsUpdateIntDla(Cms,"AnIntDla",Msg.AnIntDla,Msg.AnIntDla_Length,10);
   end UpdateMynmlmsg_Type;

   procedure Initialize(Object : in out Mynmlmsg_Type) is
   begin
      Object.NmlType := 1001;
      Object.Size := Mynmlmsg_Type'Size;
   end Initialize;

end Mynmlmsg;
