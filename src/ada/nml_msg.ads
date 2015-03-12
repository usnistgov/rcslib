with Interfaces.C; use Interfaces.C;
with Unchecked_Deallocation;
with Unchecked_Conversion;
with Ada.Finalization;
use Ada.Finalization;
with Cms;

package Nml_Msg is

   type NmlMsg is new Limited_Controlled with
      record
         NmlType : Interfaces.C.long;
         Size : Interfaces.C.Size_t;
      end record;

   pragma Convention(C,NmlMsg);

   type NmlMsg_Access is access all  NmlMsg'Class;
   type NmlMsg_DirectAccess is access all NmlMsg;
   type NmlMsg_Array is array(Integer range <>) of NmlMsg;

   function NmlMsg_Access_To_Limited_Controlled_Access is new Unchecked_Conversion(NmlMsg_Access,Cms.Limited_Controlled_Access);

   procedure Initialize(Msg : in out NmlMsg);

   procedure Free is new Unchecked_Deallocation(NmlMsg,NmlMsg_DirectAccess);


   type Rcs_Cmd_Msg is new NmlMsg with
      record
         Serial_Number : Interfaces.C.int;
      end record;

   pragma Convention(C,Rcs_Cmd_Msg);


   type Rcs_Cmd_Msg_Access is access all  Rcs_Cmd_Msg'Class;
   type Rcs_Cmd_Msg_DirectAccess is access all Rcs_Cmd_Msg;
   type Rcs_Cmd_Msg_Array is array(Integer range <>) of Rcs_Cmd_Msg;

   function Rcs_Cmd_Msg_Access_To_Limited_Controlled_Access is new Unchecked_Conversion(Rcs_Cmd_Msg_Access,Cms.Limited_Controlled_Access);

   procedure Initialize(Msg : in out Rcs_Cmd_Msg);
   procedure Update_Internal_Rcs_Cmd_Msg(Cms_Ptr : in Cms.Cms_Access ; Msg : in out Rcs_Cmd_Msg);

   procedure Free is new Unchecked_Deallocation(Rcs_Cmd_Msg,Rcs_Cmd_Msg_DirectAccess);

   type RCS_STATUS is (
                       UNINITIALIZED_STATUS,
                       RCS_DONE,
                       RCS_EXEC,
                       RCS_ERROR,
                       Bad_Rcs_Status_Value
                       );
   type RCS_STATUS_Array is array(Integer range <>) of RCS_STATUS;

   type Rcs_Stat_Msg is new NmlMsg with
      record
          Command_Type : Interfaces.C.long;
          Echo_Serial_Number : Interfaces.C.int;
          Status : RCS_STATUS;
          State : Interfaces.C.int;
          Line : Interfaces.C.int;
          Source_Line :  Interfaces.C.int;
          Source_File : Interfaces.C.Char_Array(1..64);
      end record;

   pragma Convention(C,Rcs_Stat_Msg);

   type Rcs_Stat_Msg_Access is access all  Rcs_Stat_Msg'Class;
   type Rcs_Stat_Msg_DirectAccess is access all Rcs_Stat_Msg;
   type Rcs_Stat_Msg_Array is array(Integer range <>) of Rcs_Stat_Msg;

   function Rcs_Stat_Msg_Access_To_Limited_Controlled_Access is new Unchecked_Conversion(Rcs_Stat_Msg_Access,Cms.Limited_Controlled_Access);

   procedure Initialize(Msg : in out Rcs_Stat_Msg);
   procedure Update_Internal_Rcs_Stat_Msg(Cms_Ptr : in Cms.Cms_Access ; Msg : in out Rcs_Stat_Msg);
   procedure Free is new Unchecked_Deallocation(Rcs_Stat_Msg,Rcs_Stat_Msg_DirectAccess);

   type RCS_ADMIN_STATE is (
                            Bad_RCS_ADMIN_STATE_Value,
                            RCS_ADMIN_ZERO,
                            ADMIN_INITIALIZED, -- 2
                            ADMIN_UNINITIALIZED, -- 1
                            ADMIN_SHUT_DOWN --3
                            );
   type RCS_ADMIN_STATE_Array is array(Integer range <>) of RCS_ADMIN_STATE;


   type time_tracker is
      record
         count : int;
         last : double;
         now : double;
         start : double;
         elapsed : double;
         min : double;
         max : double;
         avg : double;
      end record;

   type time_tracker_Access is access all time_tracker;
   procedure Update_Internal_time_tracker(Cms_Ptr : in Cms.Cms_Access; Msg : in out time_tracker);
   procedure Free is new Unchecked_Deallocation(time_tracker,time_tracker_Access);
   type time_tracker_Array is array(Integer range <>) of time_tracker;

   type RCS_STAT_MSG_V2 is new RCS_STAT_MSG with
      record
         admin_state : RCS_ADMIN_STATE;
         tt : time_tracker;
         message_length : int;
         message : Char_Array(1..80); -- NML_DYNAMIC_LENGTH_ARRAY --
      end record;

   type RCS_STAT_MSG_V2_Access is access all RCS_STAT_MSG_V2;
   procedure Update_Internal_RCS_STAT_MSG_V2(Cms_Ptr : in Cms.Cms_Access; Msg : in out RCS_STAT_MSG_V2);
   procedure Free is new Unchecked_Deallocation(RCS_STAT_MSG_V2,RCS_STAT_MSG_V2_Access);
   type RCS_STAT_MSG_V2_Array is array(Integer range <>) of RCS_STAT_MSG_V2;

end Nml_Msg;
