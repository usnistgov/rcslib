with Interfaces.C;
with Interfaces.C.Strings;
use Interfaces.C;
with Ada.Finalization;
use Ada.Finalization;
with Unchecked_Deallocation;

package Cms is

   type Cms is tagged private;
   type Cms_Access is access all Cms'Class;

   type Cms_Enum_Info is private;
   type Cms_Enum_Info_Access is access all Cms_Enum_Info;

   type Short_Array is array(Integer range <>) of Interfaces.C.Short;
   type Int_Array is array(Integer range <>) of Interfaces.C.Int;
   type Long_Array is array(Integer range <>) of Interfaces.C.Long;

   type Unsigned_Char_Array is array(Integer range <>) of Interfaces.C.Unsigned_Char;
   type Unsigned_Short_Array is array(Integer range <>) of Interfaces.C.Unsigned_Short;
   type Unsigned_Array is array(Integer range <>) of Interfaces.C.Unsigned;
   type Unsigned_Long_Array is array(Integer range <>) of Interfaces.C.Unsigned_Long;


   type C_Float_Array is array(Integer range <>) of Interfaces.C.C_Float;
   type Double_Array is array(Integer range <>) of Interfaces.C.Double;
   type Long_Double_Array is array(Integer range <>) of Interfaces.C.Long_Double;

   type Boolean_Array is array(Integer range <>) of Boolean;
   type Signed_Char_Array is array(Integer range <>) of Interfaces.C.Signed_Char;

   type Size_T_Array is array(Integer range <>) of Interfaces.C.Size_T;

   type Symbol_Lookup_Callback is access function(Nml_Type : in long)
                                                 return Interfaces.C.Strings.Chars_Ptr;

   type Limited_Controlled_Access is access all Limited_Controlled;

   pragma Convention(C,Symbol_Lookup_Callback);

   function New_Cms_Enum_Info(Name : in String;
                              NameList : in Char_Array;
                              IntList : in Int_Array;
                              MaxNameLen : in Int ;
                              ListLen : in Int;
                              Symbol_Lookup : in Symbol_Lookup_Callback
                             ) return Cms_Enum_Info_Access;

   function Return_Symbol(Ca : Char_Array) return Interfaces.C.Strings.Chars_Ptr;

   function Check_Type_Info (Ca : in Cms_Access;
                              Nml_Type : in Long;
                              Message : in Limited_Controlled_Access;
                              NameSpace : in String;
                              Slc : in Symbol_Lookup_Callback;
                              NameList : in Interfaces.C.Char_Array;
                              IdList : in Long_Array;
                              SizeList : in Size_T_Array;
                              List_Length : in Long;
                              Max_Name_Length: in Long) return Long;


   procedure Update_Long(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Long);
   procedure Update_Long_Array(Ca : in Cms_Access; Name : in String; X : in out Long_Array; Len : in Interfaces.C.Unsigned );
   procedure Update_Long_Dla(Ca : in Cms_Access; Name : in String; X : in out Long_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int );

   procedure Update_Dla_Length(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Int);
   procedure Update_Int(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Int);
   procedure Update_Int_Array(Ca : in Cms_Access; Name : in String; X : in out Int_Array; Len : in Interfaces.C.Unsigned );
   procedure Update_Int_Dla(Ca : in Cms_Access; Name : in String; X : in out Int_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int );

   procedure Update_Short(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Short);
   procedure Update_Short_Array(Ca : in Cms_Access; Name : in String; X : in out Short_Array; Len : in Interfaces.C.Unsigned );
   procedure Update_Short_Dla(Ca : in Cms_Access; Name : in String; X : in out Short_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int );

   procedure Update_Char(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Char);
   procedure Update_Char_Array(Ca : in Cms_Access; Name : in String; X : in out Char_Array; Len : in Interfaces.C.Unsigned );
   procedure Update_Char_Dla(Ca : in Cms_Access; Name : in String; X : in out Char_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int );


   procedure Update_Unsigned_Long(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Unsigned_Long);
   procedure Update_Unsigned_Long_Array(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Long_Array; Len : in Interfaces.C.Unsigned );
   procedure Update_Unsigned_Long_Dla(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Long_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int );

   procedure Update_Unsigned(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Unsigned);
   procedure Update_Unsigned_Array(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Array; Len : in Interfaces.C.Unsigned );
   procedure Update_Unsigned_Dla(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int );

   procedure Update_Unsigned_Short(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Unsigned_Short);
   procedure Update_Unsigned_Short_Array(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Short_Array; Len : in Interfaces.C.Unsigned );
   procedure Update_Unsigned_Short_Dla(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Short_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int );

   procedure Update_Unsigned_Char(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Unsigned_Char);
   procedure Update_Unsigned_Char_Array(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Char_Array; Len : in Interfaces.C.Unsigned );
   procedure Update_Unsigned_Char_Dla(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Char_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int );

   procedure Update_C_Float(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.C_Float);
   procedure Update_C_Float_Array(Ca : in Cms_Access; Name : in String; X : in out C_Float_Array; Len : in Interfaces.C.Unsigned );
   procedure Update_C_Float_Dla(Ca : in Cms_Access; Name : in String; X : in out C_Float_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int );

   procedure Update_Double(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Double);
   procedure Update_Double_Array(Ca : in Cms_Access; Name : in String; X : in out Double_Array; Len : in Interfaces.C.Unsigned );
   procedure Update_Double_Dla(Ca : in Cms_Access; Name : in String; X : in out Double_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int );

   procedure Update_Boolean(Ca : in Cms_Access; Name : in String; X : in out Boolean);
   procedure Update_Boolean_Array(Ca : in Cms_Access; Name : in String; X : in out Boolean_Array; Len : in Interfaces.C.Unsigned );
   -- C++ and C versions also missing equivalent of cms_update_bool_dla.
   -- procedure Update_Boolean_Dla(Ca : in Cms_Access; Name : in String; X : in out Boolean_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int );

   procedure  Begin_Class(Ca : in Cms_Access; Name : in String; Parent : in String);
   procedure  End_Class(Ca : in Cms_Access; Name : in String; Parent : in String);

   procedure  Begin_Base_Class(Ca : in Cms_Access; Name : in String);
   procedure  End_Base_Class(Ca : in Cms_Access; Name : in String);

   procedure  Begin_Class_Var(Ca : in Cms_Access; Name : in String);
   procedure  End_Class_Var(Ca : in Cms_Access; Name : in String);

   procedure  Begin_Struct_Array_Elem (Ca : in Cms_Access;Name : in String; Index : Interfaces.C.Int);
   procedure  End_Struct_Array_Elem (Ca : in Cms_Access;Name : in String; Index : Interfaces.C.Int);
   procedure  Begin_Struct_Dynamic_Array (Ca : in Cms_Access;
                                                Name : in String; Len : in out Interfaces.C.Int; Maxlen : Interfaces.C.Int);
   procedure  End_Struct_Dynamic_Array (Ca : in Cms_Access;
                                        Name : in String; Len : in out Interfaces.C.Int; Maxlen : Interfaces.C.Int);

   function Update_Enumeration(Ca : in Cms_Access;
                               Name : in String;
                               X : in Interfaces.C.Int;
                               Info : in Cms_Enum_Info_Access) return Int;

   procedure Begin_Enumeration_Array (Ca : in Cms_Access;
                                      Name : in String;
                                      Info : in Cms_Enum_Info_Access;
                                      Len : in Interfaces.C.Unsigned);

   procedure Begin_Enumeration_DLA (Ca : in Cms_Access;
                                    Name : in String;
                                    Info : in Cms_Enum_Info_Access;
                                    Len : in out Interfaces.C.Int;
                                    MaxLen : in Interfaces.C.Int);

   function Update_Enumeration_Array_Elem(Ca : in Cms_Access;
                                          Name : in String;
                                          X : in Interfaces.C.Int;
                                          Elem : in Interfaces.C.Int) return Int;

   procedure End_Enumeration_Array (Ca : in Cms_Access;
                                    Name : in String;
                                    Info : in Cms_Enum_Info_Access;
                                    Len : in Interfaces.C.Unsigned);

   procedure End_Enumeration_DLA (Ca : in Cms_Access;
                                  Name : in String;
                                  Info : in Cms_Enum_Info_Access;
                                  Len : in out Interfaces.C.Int;
                                  MaxLen : in Interfaces.C.Int);

private

   type Cms is tagged
      record
         Dummy : Integer;
      end record;


   type Cms_Enum_Info_C  is tagged
      record
         Dummy : Integer;
      end record;

   type Cms_Enum_Info_C_Access is access all Cms_Enum_Info_C;

   type Cms_Enum_Info is
      record
         Symbol_Lookup : Symbol_Lookup_Callback;
         Info : Cms_Enum_Info_C_Access;
      end record;

end Cms;

