
with Interfaces.C;
with Interfaces.C.Strings;
use Interfaces.C;
with Ada.Finalization;
use Ada.Finalization;
with Unchecked_Deallocation;
with Unchecked_Conversion;
with Cms; use Cms;
with Nml_Msg; use Nml_Msg;

package Nml is

   -- NML_C and NML_C_T should only be used internally within the nml package,
   -- but I could't avoid definding them here.
   type NML_C is private;
   type Nml_C_T is access NML_C;

   -- The callback function definition that will convert from or to
   -- Native Ada representation of NMLmsg from/to neutral repepresentations
   -- like XDR, XML etc.
   -- Functions of this type are generally automatically generated using
   -- the NML CodeGenerator.
   type Format_Callback_Func is access function(Nml_Type : in long;
                                                Msg : in NmlMsg_Access;
                                                Cms_Ptr : in Cms.Cms_Access)
                                               return int;
   pragma Convention(C,Format_Callback_Func);

   -- Users shouldn't really use this either but we need
   -- to define it here so we can instantiate the Free function.
   type NmlConnection is new Limited_Controlled with
      record
         CallBackFunc : Format_Callback_Func;
         Nml_C_Ptr : Nml_C_T;
      end record;

   type NmlConnection_Access is access all NmlConnection;

   -- Release resources associated with an NML connection.
   procedure Finalize(Object : in out NmlConnection);
   procedure Free is new Unchecked_Deallocation(NmlConnection,NmlConnection_Access);

   -- This should generally be called before creating any NMLConnections
   -- if you plan to call Run_Servers.
   procedure Set_To_Server;

   -- Run a processs that allows remote processes to access connections
   -- previously created by the local process.
   -- This never returns.
   procedure Run_Servers;

   -- Write the specified message to the connected buffer even if it would overwrite a previous unread message.
   function Write(Connection : in NmlConnection_Access; Message : in NmlMsg_Access) return Integer;

   -- Write the specified message to the connected buffer unless
   -- the last message has not yet been read..
   function Write_If_Read(Connection : in NmlConnection_Access; Message : in NmlMsg_Access) return Integer;

   -- Read the message from the connected buffer to a local area and return
   -- its type or 0 if no new messages have been written to that buffer or -1
   -- if some sort of comm error occured.
   function Read(Connection : in NmlConnection_Access) return Integer;

   -- Read a messag but wait up to Timeout seconds for new data to be
   -- written to the buffer. If the timeout is less than zero wait forever.
   function Blocking_Read(Connection : in NmlConnection_Access;
                          Timeout : Interfaces.C.Double ) return Integer;

   -- Same as Read except do not change was_read flag or remove
   -- messages from a queued buffer.
   function Peek(Connection : in NmlConnection_Access) return Integer;

   -- Get the location that the last message read was stored in.
   function Get_Address(Connection : in NmlConnection_Access) return NmlMsg_Access;

   -- Check to see if the given connection is valid.
   function Valid(Connection : in NmlConnection_Access) return Boolean;

   -- Get the number of messages that have ever been written to the
   -- buffer we connected to. (Including messages that have since been overwritten or removed from the queue.)
   function Get_Msg_Count(Connection : in NmlConnection_Access) return Integer;

   -- Get the number of messages currently stored in the queue or 0 if
   -- this is not a queued buffer.
   function Get_Queue_Length(Connection : in NmlConnection_Access) return Integer;

   -- Get the amount of space in bytes that is available to store new messages
   function Get_Space_Available(Connection : in NmlConnection_Access) return Integer;

   -- Store an XML Schema corresponding to the current set of messages in a file
   procedure Save_Xml_Schema(Connection : in NmlConnection_Access;
                             File_Name : in String);


   -- Store the Message in a file in XML format.
   procedure Save_Xml_Msg(Connection : in NmlConnection_Access;
                          Message : in NmlMsg_Access;
                          File_Name : in String);

   -- Read a file in XML format, store it in local memory and return the
   -- type of message or -1 if it could not be read.
   function Read_Xml_Msg(Connection : in NmlConnection_Access;
                         File_Name : in String)
                        return Integer;

   pragma Linker_Options("-lrcs");

   -- Create a connection to a buffer, and if necessary create the buffer.
   -- The NewCallBackFunction is the function that will convert to/from
   -- an native Ada format from/to XDR or XML etc.
   -- BufferName is the buffer I want to connect to.
   -- ProcessName is the name of the process calling this.
   -- ConfigSource is either a configuration file name or the location of an
   -- nmlcfgsvr.
   function CreateConnection(NewCallBackFunction : in Format_Callback_Func;
                             BufferName,ProcessName,ConfigSource: in String)
                            return NmlConnection_Access;

   type RCS_PRINT_DESTINATION_TYPE is (
                                       RCS_PRINT_TO_STDOUT,
                                       RCS_PRINT_TO_STDERR,
                                       RCS_PRINT_TO_NULL,
                                       RCS_PRINT_TO_FILE);

   -- Specify what to do with debug and error messages generated within the
   -- NML/CMS and miscellaneous RCS functions. Affects all NML connections.
   procedure Set_Print_Destination( T :  RCS_PRINT_DESTINATION_TYPE);


   -- Set a filename that will store debug and error messages
   -- It has no effect if Set_Print_Destination(RCS_PRINT_TO_FILE)
   -- is not called. Affects all NML connections.
   procedure Set_Print_File(File_Name :String);


   type NML_ERROR_TYPE is (
                           -- You should not have needed to check the error type
                           -- the last function succeeded.
                           NML_NO_ERROR,

                           -- Write_if_read fails if the last message was not read.
                           NML_BUFFER_NOT_READ,

                           -- something took more time than allowed.
                           NML_TIMED_OUT,

                           -- The configuration file or the data sent from
                           -- a configuration server was invalid.
                           NML_INVALID_CONFIGURATION,

                           -- An error occured while running format
                           -- could be a problem with the CodeGenerator,
                           -- a version mismatch, you forgot to regenerate,recompile or relink something after a header was changed.
                           NML_FORMAT_ERROR,

                           -- You will need to examine the logged text error messages to figure out what went wrong here.
                           NML_INTERNAL_CMS_ERROR,

                           -- You need to start the master process first.
                           NML_NO_MASTER_ERROR,

                           -- An attempt to write or read a message not in the
                           -- dictionary(format function) or not properly constructructed.
                           NML_INVALID_MESSAGE_ERROR,

                           -- The queue is full.
                           NML_QUEUE_FULL_ERROR,

                           -- bad constructor argument.
                           NML_INVALID_CONSTRUCTOR_ARG,

                           -- Either someone called NML::interrupt_operation()
                           -- from a C++ part of the program or signal was caught.
                           NML_INTERRUPTED_OPERATION,

                           -- Invalid or missing format function.
                           NML_NO_FORMAT_FUNCTION,

                           -- Some things that need dynamically allocated
                           -- memory and run out of memory throw exceptions others return
                           -- and set this error code.
                           -- If at all possible I try to avoid allocation
                           -- and deallocation everywhere except CreateConnection and the Finalizer
                           NML_OUT_OF_MEMORY_ERROR,

                           -- I suppose they are all bad but this one means
                           -- the error_type integer itself was not one of
                           -- the values that was assigned a meaning.
                           -- This probably means the C++ code and Ada code is out of sync.
                           Bad_NML_ERROR_TYPE_Value
                           );

   function Get_Last_Error_Type(Connection : in NmlConnection_Access)
                               return NML_ERROR_TYPE;

   -- Start logging debugging messages. Affects all NML channels.
   -- The C++ library is by default configured to never log debug messages
   -- whether you call this or not. See --enable-debug_print option to configure.
   procedure Debug_On;

   -- Stop logging debugging messages. Affects all NML channels.
   procedure Debug_Off;

private

   -- Don't look at me. I'm a dummy.
   type NML_C is
      record
         Dummy : Integer;
      end record;

end Nml;


