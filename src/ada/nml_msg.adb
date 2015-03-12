with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Strings;
use Interfaces.C.Strings;
with Cms;

package body Nml_Msg is

   procedure Initialize(Msg : in out NmlMsg) is
   begin
      Msg.Size := NmlMsg'Size/8;
   end Initialize;


   Enum_RCS_STATUS_Name_List : constant Char_Array(1..105) := (
                                                               'R','C','S','_','D','O','N','E',nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,
                                                               'R','C','S','_','E','R','R','O','R',nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,
                                                               'R','C','S','_','E','X','E','C',nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,
                                                               'U','N','I','N','I','T','I','A','L','I','Z','E','D','_','S','T','A','T','U','S',nul,
                                                               nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul
                                                               );
   Enum_RCS_STATUS_Int_List : constant Cms.Int_Array(1..5) := (
                                                               1, -- RCS_DONE
                                                               3, -- RCS_ERROR
                                                               2, -- RCS_EXEC
                                                               -1, -- UNINITIALIZED_STATUS
                                                               -1
                                                               );
        enum_RCS_STATUS_RCS_DONE_Key_Name : constant Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String("RCS_DONE");
        enum_RCS_STATUS_RCS_ERROR_Key_Name : constant Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String("RCS_ERROR");
        enum_RCS_STATUS_RCS_EXEC_Key_Name : constant Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String("RCS_EXEC");
        enum_RCS_STATUS_UNINITIALIZED_STATUS_Key_Name : constant Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String("UNINITIALIZED_STATUS");

        function Enum_RCS_STATUS_Symbol_Lookup(enum_int : in long) return Interfaces.C.Strings.chars_ptr;
        pragma Export(C,Enum_RCS_STATUS_Symbol_Lookup,"ada_RCS_STATUS_stat_msg_n_ada_symbol_lookup");

        function Enum_RCS_STATUS_Symbol_Lookup(enum_int: in long) return Interfaces.C.Strings.chars_ptr is
        begin
                case enum_int is
                        when 1  =>      return enum_RCS_STATUS_RCS_DONE_Key_Name; -- RCS_DONE
                        when 3  =>      return enum_RCS_STATUS_RCS_ERROR_Key_Name; -- RCS_ERROR
                        when 2  =>      return enum_RCS_STATUS_RCS_EXEC_Key_Name; -- RCS_EXEC
                        when -1 =>      return enum_RCS_STATUS_UNINITIALIZED_STATUS_Key_Name; -- UNINITIALIZED_STATUS
                        when others     =>      return Null_Ptr;
                end case;
        end Enum_RCS_STATUS_Symbol_Lookup;

        function Enum_RCS_STATUS_To_Int(enum_val: in RCS_STATUS) return int is
        begin
                case enum_val is
                        when RCS_EXEC   =>      return 2;
                        when RCS_DONE   =>      return 1;
                        when RCS_ERROR  =>      return 3;
                        when UNINITIALIZED_STATUS       =>      return -1;
                        when Bad_Rcs_Status_Value       =>      return -2;
                end case;
        end Enum_RCS_STATUS_To_Int;

        function Int_To_Enum_RCS_STATUS(enum_int: in int) return RCS_STATUS is
        begin
                case enum_int is
                        when 2  =>      return RCS_EXEC;
                        when 1  =>      return RCS_DONE;
                        when 3  =>      return RCS_ERROR;
                        when -1 =>      return UNINITIALIZED_STATUS;
                        when others     =>      return Bad_RCS_STATUS_Value;
                end case;
        end Int_To_Enum_RCS_STATUS;

        Enum_RCS_STATUS_Info : constant Cms.Cms_Enum_Info_Access := Cms.New_Cms_Enum_Info(
                        "RCS_STATUS",
                        Enum_RCS_STATUS_Name_List,
                        Enum_RCS_STATUS_Int_List,
                        21,
                        4,
                        Enum_RCS_STATUS_Symbol_Lookup'Access);


        -- Every NMLmsg type needs an update and an initialize function.

        procedure Update_Internal_RCS_STAT_MSG(Cms_Ptr : in Cms.Cms_Access; Msg : in out RCS_STAT_MSG) is
        begin
                Cms.Begin_Class(Cms_Ptr,"RCS_STAT_MSG","");
                Cms.Update_Long(Cms_Ptr,"command_type",Msg.Command_Type);
                Cms.Update_Int(Cms_Ptr, "echo_serial_number", Msg.echo_serial_number);
                Msg.status := Int_To_Enum_RCS_STATUS(
                        Cms.Update_Enumeration(Cms_Ptr, "status", Enum_RCS_STATUS_To_Int(Msg.status), Enum_RCS_STATUS_Info));
                Cms.Update_Int(Cms_Ptr, "state", Msg.state);
                Cms.Update_Int(Cms_Ptr, "line", Msg.line);
                Cms.Update_Int(Cms_Ptr, "source_line", Msg.source_line);
                Cms.Update_Char_Array(Cms_Ptr, "source_file", Msg.source_file,64);
                Cms.End_Class(Cms_Ptr,"RCS_STAT_MSG","");
        end Update_Internal_RCS_STAT_MSG;

        procedure Initialize(Msg : in out RCS_STAT_MSG) is
        begin
           Msg.Command_Type := 0;
           Msg.Echo_Serial_Number := 0;
           Msg.Status := UNINITIALIZED_STATUS;
           Msg.State := 0;
           Msg.Line := 0;
           Msg.Source_Line := -1;
        end Initialize;

        procedure Update_Internal_RCS_CMD_MSG(Cms_Ptr : in Cms.Cms_Access; Msg : in out RCS_CMD_MSG) is
        begin
                Cms.Begin_Class(Cms_Ptr,"RCS_CMD_MSG","");
                Cms.Update_Int(Cms_Ptr, "serial_number", Msg.serial_number);
                Cms.End_Class(Cms_Ptr,"RCS_CMD_MSG","");
        end Update_Internal_RCS_CMD_MSG;

        procedure Initialize(Msg : in out RCS_CMD_MSG) is
        begin
           Msg.Serial_Number := 0;
        end Initialize;


                Enum_RCS_ADMIN_STATE_Name_List : constant Char_Array(1..100) := (
                'A','D','M','I','N','_','I','N','I','T','I','A','L','I','Z','E','D',nul,nul,nul,
                'A','D','M','I','N','_','S','H','U','T','_','D','O','W','N',nul,nul,nul,nul,nul,
                'A','D','M','I','N','_','U','N','I','N','I','T','I','A','L','I','Z','E','D',nul,
                'R','C','S','_','A','D','M','I','N','_','Z','E','R','O',nul,nul,nul,nul,nul,nul,
                nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul,nul
                );
        Enum_RCS_ADMIN_STATE_Int_List : constant Cms.Int_Array(1..5) := (
                2, -- ADMIN_INITIALIZED
                3, -- ADMIN_SHUT_DOWN
                1, -- ADMIN_UNINITIALIZED
                0, -- RCS_ADMIN_ZERO
                -1
                );
        enum_RCS_ADMIN_STATE_ADMIN_INITIALIZED_Key_Name : constant Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String("ADMIN_INITIALIZED");
        enum_RCS_ADMIN_STATE_ADMIN_SHUT_DOWN_Key_Name : constant Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String("ADMIN_SHUT_DOWN");
        enum_RCS_ADMIN_STATE_ADMIN_UNINITIALIZED_Key_Name : constant Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String("ADMIN_UNINITIALIZED");
        enum_RCS_ADMIN_STATE_RCS_ADMIN_ZERO_Key_Name : constant Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String("RCS_ADMIN_ZERO");
        function Enum_RCS_ADMIN_STATE_Symbol_Lookup(enum_int : in long) return Interfaces.C.Strings.chars_ptr;
        pragma Export(C,Enum_RCS_ADMIN_STATE_Symbol_Lookup,"ada_RCS_ADMIN_STATE_stat_msg_v2_n_ada_symbol_lookup");

        function Enum_RCS_ADMIN_STATE_Symbol_Lookup(enum_int: in long) return Interfaces.C.Strings.chars_ptr is
        begin
                case enum_int is
                        when 2  =>      return enum_RCS_ADMIN_STATE_ADMIN_INITIALIZED_Key_Name; -- ADMIN_INITIALIZED
                        when 3  =>      return enum_RCS_ADMIN_STATE_ADMIN_SHUT_DOWN_Key_Name; -- ADMIN_SHUT_DOWN
                        when 1  =>      return enum_RCS_ADMIN_STATE_ADMIN_UNINITIALIZED_Key_Name; -- ADMIN_UNINITIALIZED
                        when 0  =>      return enum_RCS_ADMIN_STATE_RCS_ADMIN_ZERO_Key_Name; -- RCS_ADMIN_ZERO
                        when others     =>      return Null_Ptr;
                end case;
        end Enum_RCS_ADMIN_STATE_Symbol_Lookup;

        function Enum_RCS_ADMIN_STATE_To_Int(enum_val: in RCS_ADMIN_STATE) return int is
        begin
                case enum_val is
                        when ADMIN_INITIALIZED  =>      return 2;
                        when ADMIN_UNINITIALIZED        =>      return 1;
                        when ADMIN_SHUT_DOWN    =>      return 3;
                        when RCS_ADMIN_ZERO     =>      return 0;
                        when Bad_RCS_ADMIN_STATE_Value  =>      return -1;
                end case;
        end Enum_RCS_ADMIN_STATE_To_Int;

        function Int_To_Enum_RCS_ADMIN_STATE(enum_int: in int) return RCS_ADMIN_STATE is
        begin
                case enum_int is
                        when 2  =>      return ADMIN_INITIALIZED;
                        when 1  =>      return ADMIN_UNINITIALIZED;
                        when 3  =>      return ADMIN_SHUT_DOWN;
                        when 0  =>      return RCS_ADMIN_ZERO;
                        when others     =>      return Bad_RCS_ADMIN_STATE_Value;
                end case;
        end Int_To_Enum_RCS_ADMIN_STATE;

        Enum_RCS_ADMIN_STATE_Info : constant Cms.Cms_Enum_Info_Access := Cms.New_Cms_Enum_Info(
                        "RCS_ADMIN_STATE",
                        Enum_RCS_ADMIN_STATE_Name_List,
                        Enum_RCS_ADMIN_STATE_Int_List,
                        20,
                        5,
                        Enum_RCS_ADMIN_STATE_Symbol_Lookup'Access);


                procedure Update_RCS_STAT_MSG_V2(Cms_Ptr : in Cms.Cms_Access; Msg : in RCS_STAT_MSG_V2_Access) is
        begin
                Cms.Begin_Class(Cms_Ptr,"RCS_STAT_MSG_V2","");
                Cms.Begin_Base_Class(Cms_Ptr,"RCS_STAT_MSG");
                Update_Internal_RCS_STAT_MSG(Cms_Ptr, RCS_STAT_MSG(Msg.all));
                Cms.End_Base_Class(Cms_Ptr,"RCS_STAT_MSG");
                Msg.admin_state := Int_To_Enum_RCS_ADMIN_STATE(
                        Cms.Update_Enumeration(Cms_Ptr, "admin_state", Enum_RCS_ADMIN_STATE_To_Int(Msg.admin_state), Enum_RCS_ADMIN_STATE_Info));
                Cms.Begin_Class_Var(Cms_Ptr,"tt");
                Update_Internal_time_tracker(Cms_Ptr,Msg.tt);
                Cms.End_Class_Var(Cms_Ptr,"tt");
                Cms.Update_Dla_Length(Cms_Ptr,"message_length", Msg.message_Length);
                Cms.Update_Char_Dla(Cms_Ptr, "message", Msg.message,Msg.message_length,80);
                Cms.End_Class(Cms_Ptr,"RCS_STAT_MSG_V2","");
        end Update_RCS_STAT_MSG_V2;

        procedure Update_Internal_RCS_STAT_MSG_V2(Cms_Ptr : in Cms.Cms_Access; Msg : in out RCS_STAT_MSG_V2) is
        begin
                Cms.Begin_Class(Cms_Ptr,"RCS_STAT_MSG_V2","");
                Cms.Begin_Base_Class(Cms_Ptr,"RCS_STAT_MSG");
                Update_Internal_RCS_STAT_MSG(Cms_Ptr, RCS_STAT_MSG(Msg));
                Cms.End_Base_Class(Cms_Ptr,"RCS_STAT_MSG");
                Msg.admin_state := Int_To_Enum_RCS_ADMIN_STATE(
                        Cms.Update_Enumeration(Cms_Ptr, "admin_state", Enum_RCS_ADMIN_STATE_To_Int(Msg.admin_state), Enum_RCS_ADMIN_STATE_Info));
                Cms.Begin_Class_Var(Cms_Ptr,"tt");
                Update_Internal_time_tracker(Cms_Ptr,Msg.tt);
                Cms.End_Class_Var(Cms_Ptr,"tt");
                Cms.Update_Dla_Length(Cms_Ptr,"message_length", Msg.message_Length);
                Cms.Update_Char_Dla(Cms_Ptr, "message", Msg.message,Msg.message_length,80);
                Cms.End_Class(Cms_Ptr,"RCS_STAT_MSG_V2","");
        end Update_Internal_RCS_STAT_MSG_V2;

        procedure Update_time_tracker(Cms_Ptr : in Cms.Cms_Access; Msg : in time_tracker_Access) is
        begin
                Cms.Begin_Class(Cms_Ptr,"time_tracker","");
                Cms.Update_Int(Cms_Ptr, "count", Msg.count);
                Cms.Update_Double(Cms_Ptr, "last", Msg.last);
                Cms.Update_Double(Cms_Ptr, "now", Msg.now);
                Cms.Update_Double(Cms_Ptr, "start", Msg.start);
                Cms.Update_Double(Cms_Ptr, "elapsed", Msg.elapsed);
                Cms.Update_Double(Cms_Ptr, "min", Msg.min);
                Cms.Update_Double(Cms_Ptr, "max", Msg.max);
                Cms.Update_Double(Cms_Ptr, "avg", Msg.avg);
                Cms.End_Class(Cms_Ptr,"time_tracker","");
        end Update_time_tracker;

        procedure Update_Internal_time_tracker(Cms_Ptr : in Cms.Cms_Access; Msg : in out time_tracker) is
        begin
                Cms.Begin_Class(Cms_Ptr,"time_tracker","");
                Cms.Update_Int(Cms_Ptr, "count", Msg.count);
                Cms.Update_Double(Cms_Ptr, "last", Msg.last);
                Cms.Update_Double(Cms_Ptr, "now", Msg.now);
                Cms.Update_Double(Cms_Ptr, "start", Msg.start);
                Cms.Update_Double(Cms_Ptr, "elapsed", Msg.elapsed);
                Cms.Update_Double(Cms_Ptr, "min", Msg.min);
                Cms.Update_Double(Cms_Ptr, "max", Msg.max);
                Cms.Update_Double(Cms_Ptr, "avg", Msg.avg);
                Cms.End_Class(Cms_Ptr,"time_tracker","");
        end Update_Internal_time_tracker;


end Nml_Msg;
