with Nml;
with Interfaces.C;
use Interfaces.C;
with Ada.Finalization;
use Ada.Finalization;
with Unchecked_Conversion;
with Unchecked_Deallocation;

package Mynmlmsg is

   type Mynmlmsg_Type is new Nml.NmlMsg with
      record
         AnotherInt : Int;
         AnIntArray : Nml.Int_Array(1..10);
         AnIntDla_Length : Int;
         AnIntDla : Nml.Int_Array(1..10);
      end record;

   type Mynmlmsg_Type_Access is access Mynmlmsg_Type;


   function NmlMsg_Access_To_Mynmlmsg_Type_Access is new
     Unchecked_Conversion(Nml.NmlMsg_Access,Mynmlmsg_Type_Access);

   procedure UpdateMynmlmsg_Type(Cms : in NML.Cms_Access ;
                                 Msg : in Mynmlmsg_Type_Access );

   procedure Initialize(Object : in out Mynmlmsg_Type);

   function Format(Param_1 : in long;
                   Param_2 : in Nml.NmlMsg_Access;
                   Param_3 : in Nml.Cms_Access)
                  return int;

   procedure Free is new Unchecked_Deallocation(Mynmlmsg_Type,Mynmlmsg_Type_Access);

end Mynmlmsg;
