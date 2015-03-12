
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Interfaces.C, Interfaces.C.Strings;
use Interfaces.C, Interfaces.C.Strings;
with Ada.Finalization;
use Ada.Finalization;
with Unchecked_Deallocation;
with Unchecked_Conversion;

package body Nml is

   pragma Linker_Options("-lrcs");


   function Nml_New(Func : in Format_Callback_Func;
                    Bufname, ProcessName,Cfgfile : in Interfaces.C.Strings.Chars_Ptr)
                   return Nml_C_T;

   pragma Import(C,Nml_New,"nml_new");

   procedure Nml_Set_Msg_Base_Offset(Nml : Nml_C_T; S: Interfaces.C.Int);
   pragma Import(C,Nml_Set_Msg_Base_Offset,"nml_set_msg_base_offset");

   procedure Nml_Cms_Pointer_Check_Disable(Nml : Nml_C_T);
   pragma Import(C,Nml_Cms_Pointer_Check_Disable,"nml_cms_pointer_check_disable");

   function CreateConnection(NewCallBackFunction : in Format_Callback_Func; BufferName,ProcessName,ConfigSource: in String) return NmlConnection_Access is
      Nca : NmlConnection_Access;
      Bn : Interfaces.C.Strings.Chars_Ptr;
      Pn : Interfaces.C.Strings.Chars_Ptr;
      Cf : Interfaces.C.Strings.Chars_Ptr;
      M:NmlMsg;
   begin
      Bn := Interfaces.C.Strings.New_String(BufferName);
      Pn := Interfaces.C.Strings.New_String(ProcessName);
      Cf := Interfaces.C.Strings.New_String(ConfigSource);
      Nca := new NmlConnection;
      Nca.CallBackFunc := NewCallBackFunction;
      Nca.Nml_C_Ptr := Nml_New(NewCallBackFunction,Bn,Pn,Cf);
      Nml_Cms_Pointer_Check_Disable(Nca.Nml_C_Ptr);
      -- Nml_Set_Msg_Base_Offset(Nca.Nml_C_Ptr,(M'Size-M.NmlType'Position)/8);
      Interfaces.C.Strings.Free(Bn);
      Interfaces.C.Strings.Free(Pn);
      Interfaces.C.Strings.Free(Cf);
      return Nca;
   end CreateConnection;


   function Nml_Valid(Nml : in Nml_C_T) return Interfaces.C.Int;

   pragma Import(C,Nml_Valid,"nml_valid");

   function Valid(Connection : in NmlConnection_Access) return Boolean is
      Ok_Int :Interfaces.C.Int;
      OK : Integer;
   begin
      if null = Connection then
         return False;
      end if;
      Ok_Int := Nml_Valid(Connection.NML_C_Ptr);
      OK := Integer(Ok_Int);
      if OK = 1 then
         return True;
      else
         return False;
      end if;
   end Valid;

   procedure Nml_Set_To_Server_C;
   pragma Import(C,Nml_Set_To_Server_C,"nml_set_to_server_c");

   procedure Set_To_Server is
   begin
      Nml_Set_To_Server_C;
   end Set_To_Server;


   function Nml_Write(Nml : in Nml_C_T;
                      Message : in NmlMsg_Access;
                      NmlType : in Interfaces.C.Long;
                      Size : in Interfaces.C.Size_T)
                     return Interfaces.C.Int;

   pragma Import(C,Nml_Write,"nml_write");

   function Write(Connection : in NmlConnection_Access; Message : in NmlMsg_Access) return Integer is
      ReturnValue : Integer := 0;
      I : Interfaces.C.int;
   begin
      I := Nml_Write(Connection.NML_C_Ptr,Message,Message.NmlType,Message.Size);
      returnvalue := Integer(I);
      return ReturnValue;
   end Write;


   function Nml_Write_If_Read(Nml : in Nml_C_T;
                      Message : in NmlMsg_Access;
                      NmlType : in Interfaces.C.Long;
                      Size : in Interfaces.C.Size_T)
                     return Interfaces.C.Int;

   pragma Import(C,Nml_Write_If_Read,"nml_write_if_read");

   function Write_If_Read(Connection : in NmlConnection_Access; Message : in NmlMsg_Access) return Integer is
      ReturnValue : Integer := 0;
      I : Interfaces.C.int;
   begin
      I := Nml_Write_If_Read(Connection.NML_C_Ptr,Message,Message.NmlType,Message.Size);
      returnvalue := Integer(I);
      return ReturnValue;
   end Write_If_Read;

   function Nml_Read(Nml : in Nml_C_T)
                     return Interfaces.C.Int;

   pragma Import(C,Nml_Read,"nml_read");

   function Read(Connection : in NmlConnection_Access) return Integer is
      ReturnValue : Integer := 0;
      I : Interfaces.C.int;
   begin
      I := Nml_Read(Connection.NML_C_Ptr);
      returnvalue := Integer(I);
      return ReturnValue;
   end Read;


   function Nml_Blocking_Read(Nml : in Nml_C_T;
                              Timeout : in Interfaces.C.Double)
                             return Interfaces.C.Int;

   pragma Import(C,Nml_Blocking_Read,"nml_blocking_read");

   function Blocking_Read(Connection : in NmlConnection_Access;
                          Timeout : in Interfaces.C.Double)
                         return Integer is
      ReturnValue : Integer := 0;
      I : Interfaces.C.int;
   begin
      I := Nml_Blocking_Read(Connection.NML_C_Ptr,Timeout);
      returnvalue := Integer(I);
      return ReturnValue;
   end Blocking_Read;


   function Nml_Peek(Nml : in Nml_C_T)
                    return Interfaces.C.Int;

   pragma Import(C,Nml_Peek,"nml_peek");

   function Peek(Connection : in NmlConnection_Access) return Integer is
      ReturnValue : Integer := 0;
      I : Interfaces.C.int;
   begin
      I := Nml_Peek(Connection.NML_C_Ptr);
      returnvalue := Integer(I);
      return ReturnValue;
   end Peek;


   function Nml_Get_Msg_Count(Nml : in Nml_C_T)
                    return Interfaces.C.Int;

   pragma Import(C,Nml_Get_Msg_Count,"nml_get_msg_count");

   function Get_Msg_Count(Connection : in NmlConnection_Access) return Integer is
      ReturnValue : Integer := 0;
      I : Interfaces.C.int;
   begin
      I := Nml_Get_Msg_Count(Connection.NML_C_Ptr);
      returnvalue := Integer(I);
      return ReturnValue;
   end Get_Msg_Count;

   function Nml_Get_Queue_Length(Nml : in Nml_C_T)
                    return Interfaces.C.Int;

   pragma Import(C,Nml_Get_Queue_Length,"nml_get_queue_length");

   function Get_Queue_Length(Connection : in NmlConnection_Access) return Integer is
      ReturnValue : Integer := 0;
      I : Interfaces.C.int;
   begin
      I := Nml_Get_Queue_Length(Connection.NML_C_Ptr);
      returnvalue := Integer(I);
      return ReturnValue;
   end Get_Queue_Length;

   function Nml_Get_Space_Available(Nml : in Nml_C_T)
                    return Interfaces.C.Int;

   pragma Import(C,Nml_Get_Space_Available,"nml_get_space_available");

   function Get_Space_Available(Connection : in NmlConnection_Access) return Integer is
      ReturnValue : Integer := 0;
      I : Interfaces.C.int;
   begin
      I := Nml_Peek(Connection.NML_C_Ptr);
      returnvalue := Integer(I);
      return ReturnValue;
   end Get_Space_Available;


   function Nml_Get_Address(Nml : in Nml_C_T)
                       return NmlMsg_Access;

   pragma Import(C,Nml_Get_Address,"nml_get_address");

   function Get_Address(Connection : in NmlConnection_Access) return NmlMsg_Access is
   begin
      return  Nml_Get_Address(Connection.NML_C_Ptr);
   end Get_Address;

   procedure Run_Nml_Servers_C;
   pragma Import(C,Run_Nml_Servers_C,"run_nml_servers_c");

   procedure Run_Servers is
   begin
      Run_Nml_Servers_C;
   end Run_Servers;

   procedure Nml_Free(Nml : in Nml_C_T);
   pragma Import(C,Nml_Free,"nml_free");

   procedure Finalize(Object : in out NmlConnection) is
   begin
      if Object.NML_C_Ptr /= null then
         Nml_Free(Object.NML_C_Ptr);
         Object.NML_C_Ptr := null;
      end if;
   end Finalize;


   function Nml_Xml_Schema_Save_As(Connection : in NmlConnection_Access;
                                   File_Name : in Interfaces.C.Strings.Chars_Ptr )
                                  return Interfaces.C.Int;

   pragma Import(C,Nml_Xml_Schema_Save_As,"nml_xml_schema_save_as");

   procedure Save_Xml_Schema(Connection : in NmlConnection_Access;
                             File_Name : in String) is
      C_Ptr : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String(File_Name);
      Ok : Interfaces.C.Int := 0;
   begin
      Ok := Nml_Xml_Schema_Save_As(Connection,C_Ptr);
      Interfaces.C.Strings.Free(C_Ptr);
   end Save_Xml_Schema;


   function Nml_Xml_Msg_Save_As(Nml : in Nml_C_T;
                      Message : in NmlMsg_Access;
                      NmlType : in Interfaces.C.Long;
                      Size : in Interfaces.C.Size_T;
                      ConfigFile : in Interfaces.C.Strings.Chars_Ptr)
                     return Interfaces.C.Int;

   pragma Import(C,Nml_Xml_Msg_Save_As,"nml_xml_msg_save_as");

   procedure Save_Xml_Msg(Connection : in NmlConnection_Access;
                         Message : in NmlMsg_Access;
                         File_Name : in String) is
      ReturnValue : Integer := 0;
      I : Interfaces.C.int;
      C_Ptr : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String(File_Name);
   begin
      I := Nml_Xml_Msg_Save_As(Connection.NML_C_Ptr,Message,Message.NmlType,Message.Size,C_Ptr);
      returnvalue := Integer(I);
   end Save_Xml_Msg;

   function nml_xml_msg_read_from_file(
                                       Nml : in Nml_C_T;
                                       File_Name : Interfaces.C.Strings.Chars_Ptr
                                      ) return Interfaces.C.Int;

   pragma Import(C,Nml_Xml_Msg_Read_From_File,"nml_xml_msg_read_from_file");


   function Read_Xml_Msg(Connection : in NmlConnection_Access;
                         File_Name : in String)
                        return Integer is
      C_Ptr : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String(File_Name);
      Ok_Int : Interfaces.C.Int;
      Ok : Integer;
   begin
      Ok_Int := Nml_Xml_Msg_Read_From_File(Connection.NML_C_Ptr,C_Ptr);
      Ok := Integer(Ok_Int);
      return Ok;
   end Read_Xml_Msg;

   function Int_To_Enum_NML_ERROR_TYPE(enum_int: in int) return NML_ERROR_TYPE is
   begin
      case enum_int is
         when 9 =>      return NML_INVALID_CONSTRUCTOR_ARG;
         when 6 =>      return NML_NO_MASTER_ERROR;
         when 11        =>      return NML_NO_FORMAT_FUNCTION;
         when 2 =>      return NML_TIMED_OUT;
         when 3 =>      return NML_INVALID_CONFIGURATION;
         when 4 =>      return NML_FORMAT_ERROR;
         when 12        =>      return NML_OUT_OF_MEMORY_ERROR;
         when 10        =>      return NML_INTERRUPTED_OPERATION;
         when 5 =>      return NML_INTERNAL_CMS_ERROR;
         when 0 =>      return NML_NO_ERROR;
         when 7 =>      return NML_INVALID_MESSAGE_ERROR;
         when 1 =>      return NML_BUFFER_NOT_READ;
         when 8 =>      return NML_QUEUE_FULL_ERROR;
         when others    =>      return Bad_NML_ERROR_TYPE_Value;
      end case;
   end Int_To_Enum_NML_ERROR_TYPE;

   function Nml_Get_Last_Error_Type(Nml : in Nml_C_T)
                                   return Interfaces.C.Int;

   pragma Import(C,Nml_Get_Last_Error_Type,"nml_get_last_error_type");

   function Get_Last_Error_Type(Connection : in NmlConnection_Access)
                               return NML_ERROR_TYPE is
   begin
      return Int_To_Enum_NML_ERROR_TYPE(Nml_Get_Last_Error_Type(Connection.NML_C_Ptr));
   end Get_Last_Error_Type;

   procedure Nml_Set_Print_Destination_C(Dest_Int : Int);

   pragma Import(C,Nml_Set_Print_Destination_C,"nml_set_print_destination_c");

   procedure Set_Print_Destination( T :  RCS_PRINT_DESTINATION_TYPE) is
   begin
      case T is
         when RCS_PRINT_TO_STDOUT => Nml_Set_Print_Destination_C(0);
         when RCS_PRINT_TO_STDERR => Nml_Set_Print_Destination_C(1);
         when RCS_PRINT_TO_NULL => Nml_Set_Print_Destination_C(2);
         when RCS_PRINT_TO_FILE => Nml_Set_Print_Destination_C(4);
      end case;
   end Set_Print_Destination;

   procedure Nml_Set_Print_File_C(Fn : Interfaces.C.Strings.Chars_Ptr);

   pragma Import(C,Nml_Set_Print_File_C,"nml_set_print_file_c");

   procedure Set_Print_File(File_Name :String) is
   begin
      Nml_Set_Print_File_C(Interfaces.C.Strings.New_String(File_Name));
   end Set_Print_File;

   procedure Nml_Debug_On_C;
   pragma Import(C,Nml_Debug_On_C,"nml_debug_on_c");

   procedure Debug_On is
   begin
      Nml_Debug_On_C;
   end Debug_On;

   procedure Nml_Debug_Off_C;
   pragma Import(C,Nml_Debug_Off_C,"nml_debug_off_c");

   procedure Debug_Off is
   begin
      Nml_Debug_Off_C;
   end Debug_Off;


end Nml;



