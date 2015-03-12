-- with Ada.Text_IO;
-- with Ada.Integer_Text_IO;
with Ada.Finalization;
use Ada.Finalization;
with Unchecked_Deallocation;
with Unchecked_Conversion;

package body Cms is

   pragma Linker_Options("-lrcs");

   function Cms_Check_Type_Info (Ca : in Cms_Access;
                                 Nml_Type : in Long;
                                 Message : in Limited_Controlled_Access;
                                 NameSpace : in Interfaces.C.Char_Array;
                                 Slc : in Symbol_Lookup_Callback;
                                 NameList : in Interfaces.C.Char_Array;
                                 IdList : in Long_Array;
                                 SizeList : in Size_T_Array;
                                 List_Length : in Long;
                                 Max_Name_Length: in Long) return Long;

   pragma Import(C,Cms_Check_Type_Info);


   function Check_Type_Info (Ca : in Cms_Access;
                              Nml_Type : in Long;
                              Message : in Limited_Controlled_Access;
                              NameSpace : in String;
                              Slc : in Symbol_Lookup_Callback;
                              NameList : in Interfaces.C.Char_Array;
                              IdList : in Long_Array;
                              SizeList : in Size_T_Array;
                              List_Length : in Long;
                              Max_Name_Length: in Long) return Long is
      Rv : Interfaces.C.Long;
   begin
      Rv := Cms_Check_Type_Info(Ca,Nml_Type,Message,To_C(NameSpace),Slc,NameList,IdList,SizeList,List_Length,Max_Name_Length);
      return Rv;
   end Check_Type_Info;


   -- Chars_Ptr already defined in Interfaces.C.Strings;
   type Ints_Ptr is access all Interfaces.C.Int;
   type Longs_Ptr is access all Interfaces.C.Long;
   type Shorts_Ptr is access all Interfaces.C.Short;
   type Unsigneds_Ptr is access all Interfaces.C.Unsigned;
   type Unsigned_Longs_Ptr is access all Interfaces.C.Unsigned_Long;
   type Unsigned_Shorts_Ptr is access all Interfaces.C.Unsigned_Short;
   type Unsigned_Chars_Ptr is access all Interfaces.C.Unsigned_Char;
   type C_Floats_Ptr is access all Interfaces.C.C_float;
   type Doubles_Ptr is access all Interfaces.C.double;
   type Booleans_Ptr is access all Interfaces.C.Signed_Char;
   type Signed_Chars_Ptr is access all Interfaces.C.Signed_Char;

   procedure Free is new Unchecked_Deallocation(Interfaces.C.Int, Ints_Ptr);
   procedure Free is new Unchecked_Deallocation(Interfaces.C.Long, Longs_Ptr);
   procedure Free is new Unchecked_Deallocation(Interfaces.C.Short, Shorts_Ptr);
   procedure Free is new Unchecked_Deallocation(Interfaces.C.Unsigned, Unsigneds_Ptr);
   procedure Free is new Unchecked_Deallocation(Interfaces.C.Unsigned_Long, Unsigned_Longs_Ptr);
   procedure Free is new Unchecked_Deallocation(Interfaces.C.Unsigned_Short, Unsigned_Shorts_Ptr);
   procedure Free is new Unchecked_Deallocation(Interfaces.C.Unsigned_Char, Unsigned_Chars_Ptr);
   procedure Free is new Unchecked_Deallocation(Interfaces.C.C_Float, C_Floats_Ptr);
   procedure Free is new Unchecked_Deallocation(Interfaces.C.Double, Doubles_Ptr);
   procedure Free is new Unchecked_Deallocation(Interfaces.C.Signed_Char, Booleans_Ptr);
   procedure Free is new Unchecked_Deallocation(Interfaces.C.Signed_Char, Signed_Chars_Ptr);


   procedure Cms_Update_Dla_Length(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in Ints_Ptr);
   pragma Import(C,Cms_Update_Dla_Length);

   procedure Update_Dla_Length(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Int) is
      Xptr : Ints_Ptr;
   begin
      Xptr := new Interfaces.C.Int;
      Xptr.all := X;
      -- Ada.Text_IO.Put("CmsUpdateInt X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      Cms_Update_Dla_Length(Ca,To_C(Name),Xptr);
      X := Xptr.all;
      -- Ada.Text_IO.Put("CmsUpdateInt X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      Free(Xptr);
   end Update_Dla_Length;

   -- Begin Int
   procedure Cms_Update_Int(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in Ints_Ptr);
   pragma Import(C,Cms_Update_Int);

   procedure Update_Int(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Int) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      Xptr : Ints_Ptr;
   begin
      Xptr := new Interfaces.C.Int;
      Xptr.all := X;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      -- Ada.Text_IO.Put("CmsUpdateInt X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      Cms_Update_Int(Ca,To_C(Name),Xptr);
      X := Xptr.all;
      -- Ada.Text_IO.Put("CmsUpdateInt X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
      Free(Xptr);
   end Update_Int;

   procedure Cms_Update_Int_Array(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Int_Array; Len : in Interfaces.C.Unsigned );
   pragma Import (C,Cms_Update_Int_Array);

   procedure Update_Int_Array(Ca : in Cms_Access; Name : in String; X : in out Int_Array; Len : in Interfaces.C.Unsigned ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
   begin
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Int_Array(Ca,To_C(Name),X,Len);
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Int_Array;

   procedure Cms_Update_Int_Dla(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Int_Array; Len : Ints_Ptr; MaxLen : in Interfaces.C.Int );
   pragma Import (C,Cms_Update_Int_Dla);

   procedure Update_Int_Dla(Ca : in Cms_Access; Name : in String; X : in out Int_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      LENptr : Ints_Ptr;
   begin
      LENptr := new Interfaces.C.Int;
      LENptr.all := Len;
      -- Ada.Text_IO.Put("CmsUpdateIntDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Int_Dla(Ca,To_C(Name),X,Lenptr,Maxlen);
      Len := LENptr.all;
      Free(LENptr);
      -- Ada.Text_IO.Put("CmsUpdateIntDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Int_Dla;
   -- End int

   -- Begin long
   procedure Cms_Update_Long(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in Longs_Ptr);
   pragma Import(C,Cms_Update_Long);

   procedure Update_Long(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Long) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      Xptr : Longs_Ptr;
   begin
      Xptr := new Interfaces.C.Long;
      Xptr.all := X;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      -- Ada.Text_IO.Put("CmsUpdateLong X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      Cms_Update_Long(Ca,To_C(Name),Xptr);
      X := Xptr.all;
      -- Ada.Text_IO.Put("CmsUpdateLong X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
      Free(Xptr);
   end Update_Long;

   procedure Cms_Update_Long_Array(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Long_Array; Len : in Interfaces.C.Unsigned );
   pragma Import (C,Cms_Update_Long_Array);

   procedure Update_Long_Array(Ca : in Cms_Access; Name : in String; X : in out Long_Array; Len : in Interfaces.C.Unsigned ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
   begin
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Long_Array(Ca,To_C(Name),X,Len);
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Long_Array;

   procedure Cms_Update_Long_Dla(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Long_Array; Len : Ints_Ptr; MaxLen : in Interfaces.C.Int );
   pragma Import (C,Cms_Update_Long_Dla);

   procedure Update_Long_Dla(Ca : in Cms_Access; Name : in String; X : in out Long_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      LENptr : Ints_Ptr;
   begin
      LENptr := new Interfaces.C.Int;
      LENptr.all := Len;
      -- Ada.Text_IO.Put("CmsUpdateLongDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Long_Dla(Ca,To_C(Name),X,Lenptr,Maxlen);
      Len := LENptr.all;
      Free(LENptr);
      -- Ada.Text_IO.Put("CmsUpdateLongDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Long_Dla;
   -- End long

   -- Begin short
   procedure Cms_Update_Short(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in Shorts_Ptr);
   pragma Import(C,Cms_Update_Short);

   procedure Update_Short(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Short) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      Xptr : Shorts_Ptr;
   begin
      Xptr := new Interfaces.C.Short;
      Xptr.all := X;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      -- Ada.Text_IO.Put("CmsUpdateShort X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      Cms_Update_Short(Ca,To_C(Name),Xptr);
      X := Xptr.all;
      -- Ada.Text_IO.Put("CmsUpdateShort X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
      Free(Xptr);
   end Update_Short;

   procedure Cms_Update_Short_Array(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Short_Array; Len : in Interfaces.C.Unsigned );
   pragma Import (C,Cms_Update_Short_Array);

   procedure Update_Short_Array(Ca : in Cms_Access; Name : in String; X : in out Short_Array; Len : in Interfaces.C.Unsigned ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
   begin
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Short_Array(Ca,To_C(Name),X,Len);
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Short_Array;

   procedure Cms_Update_Short_Dla(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Short_Array; Len : Ints_Ptr; MaxLen : in Interfaces.C.Int );
   pragma Import (C,Cms_Update_Short_Dla);

   procedure Update_Short_Dla(Ca : in Cms_Access; Name : in String; X : in out Short_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      LENptr : Ints_Ptr;
   begin
      LENptr := new Interfaces.C.Int;
      LENptr.all := Len;
      -- Ada.Text_IO.Put("CmsUpdateShortDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Short_Dla(Ca,To_C(Name),X,Lenptr,Maxlen);
      Len := LENptr.all;
      Free(LENptr);
      -- Ada.Text_IO.Put("CmsUpdateShortDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Short_Dla;
   -- End short


   -- Begin char
   procedure Cms_Update_Char(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in Signed_Chars_Ptr);
   pragma Import(C,Cms_Update_Char);


   function Char_To_Signed_Char is new Unchecked_Conversion(Interfaces.C.Char, Interfaces.C.Signed_Char);
   function Signed_Char_To_Char is new Unchecked_Conversion(Interfaces.C.Signed_Char, Interfaces.C.Char);

   procedure Update_Char(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Char) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      Xptr : Signed_Chars_Ptr;
      Xa : Interfaces.C.Strings.Char_Array_Access := new Interfaces.C.Char_Array(1..2);
   begin
      Xptr := new Interfaces.C.Signed_Char;
      Xptr.all := Char_To_Signed_Char(X);
      -- Nc := Interfaces.C.Strings.New_String(Name);
      -- Ada.Text_IO.Put("CmsUpdateChar X=");
      -- Ada.Text_IO.Put(Character(X));
      -- Ada.Text_IO.New_Line;
      Cms_Update_Char(Ca,To_C(Name),Xptr);
      X := Signed_Char_To_Char(Xptr.all);
      -- Ada.Text_IO.Put("CmsUpdateChar X=");
      -- Ada.Text_IO.Put(Character(X));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
      Free(Xptr);
   end Update_Char;

   procedure Cms_Update_Char_Array(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Char_Array; Len : in Interfaces.C.Unsigned );
   pragma Import (C,Cms_Update_Char_Array);

   procedure Update_Char_Array(Ca : in Cms_Access; Name : in String; X : in out Char_Array; Len : in Interfaces.C.Unsigned ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
   begin
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Char_Array(Ca,To_C(Name),X,Len);
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Char_Array;

   procedure Cms_Update_Char_Dla(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Char_Array; Len : Ints_Ptr; MaxLen : in Interfaces.C.Int );
   pragma Import (C,Cms_Update_Char_Dla);

   procedure Update_Char_Dla(Ca : in Cms_Access; Name : in String; X : in out Char_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      LENptr : Ints_Ptr;
   begin
      LENptr := new Interfaces.C.Int;
      LENptr.all := Len;
      -- Ada.Text_IO.Put("CmsUpdateCharDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Char_Dla(Ca,To_C(Name),X,Lenptr,Maxlen);
      Len := LENptr.all;
      Free(LENptr);
      -- Ada.Text_IO.Put("CmsUpdateCharDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Char_Dla;
   -- End char

    -- Begin unsigned
   procedure Cms_Update_Unsigned_Int(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in Unsigneds_Ptr);
   pragma Import(C,Cms_Update_Unsigned_Int);

   procedure Update_Unsigned(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Unsigned) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      Xptr : Unsigneds_Ptr;
   begin
      Xptr := new Interfaces.C.Unsigned;
      Xptr.all := X;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      -- Ada.Text_IO.Put("CmsUpdateUnsigned X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      Cms_Update_Unsigned_Int(Ca,To_C(Name),Xptr);
      X := Xptr.all;
      -- Ada.Text_IO.Put("CmsUpdateUnsigned X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
      Free(Xptr);
   end Update_Unsigned;

   procedure Cms_Update_Unsigned_Int_Array(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Unsigned_Array; Len : in Interfaces.C.Unsigned );
   pragma Import (C,Cms_Update_Unsigned_Int_Array);

   procedure Update_Unsigned_Array(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Array; Len : in Interfaces.C.Unsigned ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
   begin
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Unsigned_Int_Array(Ca,To_C(Name),X,Len);
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Unsigned_Array;

   procedure Cms_Update_Unsigned_Int_Dla(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Unsigned_Array; Len : Ints_Ptr; MaxLen : in Interfaces.C.Int );
   pragma Import (C,Cms_Update_Unsigned_Int_Dla);

   procedure Update_Unsigned_Dla(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      LENptr : Ints_Ptr;
   begin
      LENptr := new Interfaces.C.Int;
      LENptr.all := Len;
      -- Ada.Text_IO.Put("CmsUpdateUnsignedDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Unsigned_Int_Dla(Ca,To_C(Name),X,Lenptr,Maxlen);
      Len := LENptr.all;
      Free(LENptr);
      -- Ada.Text_IO.Put("CmsUpdateUnsignedDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Unsigned_Dla;
   -- End unsigned

   -- Begin unsigned_long
   procedure Cms_Update_Unsigned_Long(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in Unsigned_Longs_Ptr);
   pragma Import(C,Cms_Update_Unsigned_Long);

   procedure Update_Unsigned_Long(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Unsigned_Long) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      Xptr : Unsigned_Longs_Ptr;
   begin
      Xptr := new Interfaces.C.Unsigned_Long;
      Xptr.all := X;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      -- Ada.Text_IO.Put("CmsUpdateUnsigned_Long X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      Cms_Update_Unsigned_Long(Ca,To_C(Name),Xptr);
      X := Xptr.all;
      -- Ada.Text_IO.Put("CmsUpdateUnsigned_Long X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
      Free(Xptr);
   end Update_Unsigned_Long;

   procedure Cms_Update_Unsigned_Long_Array(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Unsigned_Long_Array; Len : in Interfaces.C.Unsigned );
   pragma Import (C,Cms_Update_Unsigned_Long_Array);

   procedure Update_Unsigned_Long_Array(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Long_Array; Len : in Interfaces.C.Unsigned ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
   begin
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Unsigned_Long_Array(Ca,To_C(Name),X,Len);
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Unsigned_Long_Array;

   procedure Cms_Update_Unsigned_Long_Dla(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Unsigned_Long_Array; Len : Ints_Ptr; MaxLen : in Interfaces.C.Int );
   pragma Import (C,Cms_Update_Unsigned_Long_Dla);

   procedure Update_Unsigned_Long_Dla(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Long_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      LENptr : Ints_Ptr;
   begin
      LENptr := new Interfaces.C.Int;
      LENptr.all := Len;
      -- Ada.Text_IO.Put("CmsUpdateUnsigned_LongDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Unsigned_Long_Dla(Ca,To_C(Name),X,Lenptr,Maxlen);
      Len := LENptr.all;
      Free(LENptr);
      -- Ada.Text_IO.Put("CmsUpdateUnsigned_LongDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Unsigned_Long_Dla;
   -- End unsigned_long


   -- Begin unsigned_short
   procedure Cms_Update_Unsigned_Short(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in Unsigned_Shorts_Ptr);
   pragma Import(C,Cms_Update_Unsigned_Short);

   procedure Update_Unsigned_Short(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Unsigned_Short) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      Xptr : Unsigned_Shorts_Ptr;
   begin
      Xptr := new Interfaces.C.Unsigned_Short;
      Xptr.all := X;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      -- Ada.Text_IO.Put("CmsUpdateUnsigned_Short X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      Cms_Update_Unsigned_Short(Ca,To_C(Name),Xptr);
      X := Xptr.all;
      -- Ada.Text_IO.Put("CmsUpdateUnsigned_Short X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
      Free(Xptr);
   end Update_Unsigned_Short;

   procedure Cms_Update_Unsigned_Short_Array(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Unsigned_Short_Array; Len : in Interfaces.C.Unsigned );
   pragma Import (C,Cms_Update_Unsigned_Short_Array);

   procedure Update_Unsigned_Short_Array(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Short_Array; Len : in Interfaces.C.Unsigned ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
   begin
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Unsigned_Short_Array(Ca,To_C(Name),X,Len);
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Unsigned_Short_Array;

   procedure Cms_Update_Unsigned_Short_Dla(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Unsigned_Short_Array; Len : Ints_Ptr; MaxLen : in Interfaces.C.Int );
   pragma Import (C,Cms_Update_Unsigned_Short_Dla);

   procedure Update_Unsigned_Short_Dla(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Short_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      LENptr : Ints_Ptr;
   begin
      LENptr := new Interfaces.C.Int;
      LENptr.all := Len;
      -- Ada.Text_IO.Put("CmsUpdateUnsigned_ShortDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Unsigned_Short_Dla(Ca,To_C(Name),X,Lenptr,Maxlen);
      Len := LENptr.all;
      Free(LENptr);
      -- Ada.Text_IO.Put("CmsUpdateUnsigned_ShortDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Unsigned_Short_Dla;
   -- End unsigned_short


   -- Begin unsigned_char
   procedure Cms_Update_Unsigned_Char(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in Unsigned_Chars_Ptr);
   pragma Import(C,Cms_Update_Unsigned_Char);

   procedure Update_Unsigned_Char(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Unsigned_Char) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      Xptr : Unsigned_Chars_Ptr;
   begin
      Xptr := new Interfaces.C.Unsigned_Char;
      Xptr.all := X;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      -- Ada.Text_IO.Put("CmsUpdateUnsigned_Char X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      Cms_Update_Unsigned_Char(Ca,To_C(Name),Xptr);
      X := Xptr.all;
      -- Ada.Text_IO.Put("CmsUpdateUnsigned_Char X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
      Free(Xptr);
   end Update_Unsigned_Char;

   procedure Cms_Update_Unsigned_Char_Array(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Unsigned_Char_Array; Len : in Interfaces.C.Unsigned );
   pragma Import (C,Cms_Update_Unsigned_Char_Array);

   procedure Update_Unsigned_Char_Array(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Char_Array; Len : in Interfaces.C.Unsigned ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
   begin
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Unsigned_Char_Array(Ca,To_C(Name),X,Len);
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Unsigned_Char_Array;

   procedure Cms_Update_Unsigned_Char_Dla(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Unsigned_Char_Array; Len : Ints_Ptr; MaxLen : in Interfaces.C.Int );
   pragma Import (C,Cms_Update_Unsigned_Char_Dla);

   procedure Update_Unsigned_Char_Dla(Ca : in Cms_Access; Name : in String; X : in out Unsigned_Char_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      LENptr : Ints_Ptr;
   begin
      LENptr := new Interfaces.C.Int;
      LENptr.all := Len;
      -- Ada.Text_IO.Put("CmsUpdateUnsigned_CharDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Unsigned_Char_Dla(Ca,To_C(Name),X,Lenptr,Maxlen);
      Len := LENptr.all;
      Free(LENptr);
      -- Ada.Text_IO.Put("CmsUpdateUnsigned_CharDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Unsigned_Char_Dla;
   -- End unsigned_char

   -- Begin c_float
   procedure Cms_Update_Float(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in C_Floats_Ptr);
   pragma Import(C,Cms_Update_Float);

   procedure Update_C_Float(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.C_Float) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      Xptr : C_Floats_Ptr;
   begin
      Xptr := new Interfaces.C.C_Float;
      Xptr.all := X;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      -- Ada.Text_IO.Put("CmsUpdateC_Float X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      Cms_Update_Float(Ca,To_C(Name),Xptr);
      X := Xptr.all;
      -- Ada.Text_IO.Put("CmsUpdateC_Float X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
      Free(Xptr);
   end Update_C_Float;

   procedure Cms_Update_Float_Array(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out C_Float_Array; Len : in Interfaces.C.Unsigned );
   pragma Import (C,Cms_Update_Float_Array);

   procedure Update_C_Float_Array(Ca : in Cms_Access; Name : in String; X : in out C_Float_Array; Len : in Interfaces.C.Unsigned ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
   begin
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Float_Array(Ca,To_C(Name),X,Len);
      -- Interfaces.C.Strings.Free(Nc)
   end Update_C_Float_Array;

   procedure Cms_Update_Float_Dla(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out C_Float_Array; Len : Ints_Ptr; MaxLen : in Interfaces.C.Int );
   pragma Import (C,Cms_Update_Float_Dla);

   procedure Update_C_Float_Dla(Ca : in Cms_Access; Name : in String; X : in out C_Float_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      LENptr : Ints_Ptr;
   begin
      LENptr := new Interfaces.C.Int;
      LENptr.all := Len;
      -- Ada.Text_IO.Put("CmsUpdateC_FloatDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Float_Dla(Ca,To_C(Name),X,Lenptr,Maxlen);
      Len := LENptr.all;
      Free(LENptr);
      -- Ada.Text_IO.Put("CmsUpdateC_FloatDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
   end Update_C_Float_Dla;
   -- End c_float


   -- Begin double
   procedure Cms_Update_Double(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in Doubles_Ptr);
   pragma Import(C,Cms_Update_Double);

   procedure Update_Double(Ca : in Cms_Access; Name : in String; X : in out Interfaces.C.Double) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      Xptr : Doubles_Ptr;
   begin
      Xptr := new Interfaces.C.Double;
      Xptr.all := X;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      -- Ada.Text_IO.Put("CmsUpdateDouble X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      Cms_Update_Double(Ca,To_C(Name),Xptr);
      X := Xptr.all;
      -- Ada.Text_IO.Put("CmsUpdateDouble X=");
      -- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
      Free(Xptr);
   end Update_Double;

   procedure Cms_Update_Double_Array(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Double_Array; Len : in Interfaces.C.Unsigned );
   pragma Import (C,Cms_Update_Double_Array);

   procedure Update_Double_Array(Ca : in Cms_Access; Name : in String; X : in out Double_Array; Len : in Interfaces.C.Unsigned ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
   begin
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Double_Array(Ca,To_C(Name),X,Len);
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Double_Array;

   procedure Cms_Update_Double_Dla(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Double_Array; Len : Ints_Ptr; MaxLen : in Interfaces.C.Int );
   pragma Import (C,Cms_Update_Double_Dla);

   procedure Update_Double_Dla(Ca : in Cms_Access; Name : in String; X : in out Double_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      LENptr : Ints_Ptr;
   begin
      LENptr := new Interfaces.C.Int;
      LENptr.all := Len;
      -- Ada.Text_IO.Put("CmsUpdateDoubleDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Double_Dla(Ca,To_C(Name),X,Lenptr,Maxlen);
      Len := LENptr.all;
      Free(LENptr);
      -- Ada.Text_IO.Put("CmsUpdateDoubleDla Len=");
      -- Ada.Integer_Text_IO.Put(Integer(Len));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Double_Dla;
   -- End double


   -- Begin boolean
   procedure Cms_Update_Bool(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in Booleans_Ptr);
   pragma Import(C,Cms_Update_Bool);

   procedure Update_Boolean(Ca : in Cms_Access; Name : in String; X : in out Boolean) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      Xptr : Booleans_Ptr;
   begin
      Xptr := new Interfaces.C.Signed_Char;
      if True /= X then
         Xptr.all := 0;
      else
         Xptr.all := 1;
      end if;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      -- Ada.Text_IO.Put("CmsUpdateBoolean X=");
      ---- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      Cms_Update_Bool(Ca,To_C(Name),Xptr);
      if 0 /= Xptr.all then
         X := True;
      else
         X := False;
      end if;
      -- Ada.Text_IO.Put("CmsUpdateBoolean X=");
      ---- Ada.Integer_Text_IO.Put(Integer(X));
      -- Ada.Text_IO.New_Line;
      -- Interfaces.C.Strings.Free(Nc)
      Free(Xptr);
   end Update_Boolean;

   procedure Cms_Update_Bool_Array(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Signed_Char_Array; Len : in Interfaces.C.Unsigned );
   pragma Import (C,Cms_Update_Bool_Array);

   procedure Update_Boolean_Array(Ca : in Cms_Access; Name : in String; X : in out Boolean_Array; Len : in Interfaces.C.Unsigned ) is
      -- Nc : Interfaces.C.Strings.Chars_Ptr;
      Xptr : Signed_Char_Array(X'Range);
   begin
      for I in X'Range
      loop
         if True /= X(I) then
            Xptr(I) := 0;
         else
            Xptr(I) := 1;
         end if;
      end loop;
      -- Nc := Interfaces.C.Strings.New_String(Name);
      Cms_Update_Bool_Array(Ca,To_C(Name),Xptr,Len);
      for I in X'Range
      loop
         if 0 /= Xptr(I) then
            X(I) := True;
         else
            X(I) := False;
         end if;
      end loop;
      -- Interfaces.C.Strings.Free(Nc)
   end Update_Boolean_Array;

   -- C++ and C versions also missing equivalent of cms_update_bool_dla.
   -- procedure Cms_Update_Bool_Dla(Ca : in Cms_Access; Name : in Interfaces.C.Char_Array; X : in out Signed_Char_Array; Len : Ints_Ptr; MaxLen : in Interfaces.C.Int );
   -- pragma Import (C,Cms_Update_Bool_Dla);
   --
   -- procedure CmsUpdateBooleanDla(Ca : in Cms_Access; Name : in String; X : in out Boolean_Array; Len : in out Interfaces.C.Int; Maxlen : in Interfaces.C.Int ) is
   -- -- Nc : Interfaces.C.Strings.Chars_Ptr;
   -- LENptr : Ints_Ptr;
   -- Xptr : Signed_Char_Array(X'Range);
-- begin
   -- for I in X'Range
   -- loop
   -- if True /= X(I) then
   -- Xptr(I) := 0;
-- else
   -- Xptr(I) := 1;
-- end if;
-- end loop;
   -- LENptr := new Interfaces.C.Int;
   -- LENptr.all := Len;
   -- -- Ada.Text_IO.Put("CmsUpdateBooleanDla Len=");
   -- -- Ada.Integer_Text_IO.Put(Integer(Len));
   -- -- Ada.Text_IO.New_Line;
   -- -- Nc := Interfaces.C.Strings.New_String(Name);
   -- Cms_Update_Boolean_Dla(Ca,To_C(Name),Xptr,Lenptr,Maxlen);
   -- Len := LENptr.all;
   -- Free(LENptr);
   -- -- Ada.Text_IO.Put("CmsUpdateBooleanDla Len=");
   -- -- Ada.Integer_Text_IO.Put(Integer(Len));
   -- -- Ada.Text_IO.New_Line;
   -- for I in X'Range
   -- loop
   -- if 0 /= Xptr(I) then
   -- X(I) := True;
-- else
   -- X(I) := False;
-- end if;
-- end loop;
   -- -- Interfaces.C.Strings.Free(Nc)
-- end CmsUpdateBooleanDla;
   -- End boolean

   function Return_Symbol(Ca : Char_Array) return Interfaces.C.Strings.Chars_Ptr is
      Rv: Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_Char_Array(Ca);
      --S : String := Interfaces.C.Strings.Value(Rv);
   begin
      return Rv;
   end Return_Symbol;

   procedure Cms_Begin_Class(Ca : in Cms_Access; Name : Interfaces.C.Strings.Chars_Ptr; Parent : Interfaces.C.Strings.Chars_Ptr);

   pragma Import(C,Cms_Begin_Class);

   procedure Begin_Class(Ca : in Cms_Access; Name : in String; Parent : in String) is
      Np : Interfaces.C.Strings.Chars_Ptr;
      Pp : Interfaces.C.Strings.Chars_Ptr;
   begin
      Np := Interfaces.C.Strings.New_String(Name);
      Pp := Interfaces.C.Strings.New_String(Parent);
      Cms_Begin_Class(Ca,Np,Pp);
      Interfaces.C.Strings.Free(Np);
      Interfaces.C.Strings.Free(Pp);
   end Begin_Class;

   procedure Cms_End_Class(Ca : in Cms_Access; Name : Interfaces.C.Strings.Chars_Ptr; Parent : Interfaces.C.Strings.Chars_Ptr);

   pragma Import(C,Cms_End_Class);

   procedure End_Class(Ca : in Cms_Access; Name : in String; Parent : in String) is
      Np : Interfaces.C.Strings.Chars_Ptr;
      Pp : Interfaces.C.Strings.Chars_Ptr;
   begin
      Np := Interfaces.C.Strings.New_String(Name);
      Pp := Interfaces.C.Strings.New_String(Parent);
      Cms_End_Class(Ca,Np,Pp);
      Interfaces.C.Strings.Free(Np);
      Interfaces.C.Strings.Free(Pp);
   end End_Class;

   procedure Cms_Begin_Base_Class(Ca : in Cms_Access; Name : Interfaces.C.Strings.Chars_Ptr);

   pragma Import(C,Cms_Begin_Base_Class);

   procedure Begin_Base_Class(Ca : in Cms_Access; Name : in String) is
      Np : Interfaces.C.Strings.Chars_Ptr;
   begin
      Np := Interfaces.C.Strings.New_String(Name);
      Cms_Begin_Base_Class(Ca,Np);
      Interfaces.C.Strings.Free(Np);
   end Begin_Base_Class;

   procedure Cms_End_Base_Class(Ca : in Cms_Access; Name : Interfaces.C.Strings.Chars_Ptr);

   pragma Import(C,Cms_End_Base_Class);

   procedure End_Base_Class(Ca : in Cms_Access; Name : in String) is
      Np : Interfaces.C.Strings.Chars_Ptr;
   begin
      Np := Interfaces.C.Strings.New_String(Name);
      Cms_End_Base_Class(Ca,Np);
      Interfaces.C.Strings.Free(Np);
   end End_Base_Class;


   procedure Cms_Begin_Class_Var(Ca : in Cms_Access; Name : Interfaces.C.Strings.Chars_Ptr);

   pragma Import(C,Cms_Begin_Class_Var);

   procedure Begin_Class_Var(Ca : in Cms_Access; Name : in String) is
      Np : Interfaces.C.Strings.Chars_Ptr;
   begin
      Np := Interfaces.C.Strings.New_String(Name);
      Cms_Begin_Class_Var(Ca,Np);
      Interfaces.C.Strings.Free(Np);
   end Begin_Class_Var;

   procedure Cms_End_Class_Var(Ca : in Cms_Access; Name : Interfaces.C.Strings.Chars_Ptr);

   pragma Import(C,Cms_End_Class_Var);

   procedure End_Class_Var(Ca : in Cms_Access; Name : in String) is
      Np : Interfaces.C.Strings.Chars_Ptr;
   begin
      Np := Interfaces.C.Strings.New_String(Name);
      Cms_End_Class_Var(Ca,Np);
      Interfaces.C.Strings.Free(Np);
   end End_Class_Var;

   procedure Cms_Begin_Struct_Array_Elem(Ca : in Cms_Access; Name : Interfaces.C.Strings.Chars_Ptr; Index :Interfaces.C.Int);

   pragma Import(C,Cms_Begin_Struct_Array_Elem);

   procedure Begin_Struct_Array_Elem(Ca : in Cms_Access; Name : in String; Index :Interfaces.C.Int) is
      Np : Interfaces.C.Strings.Chars_Ptr;
   begin
      Np := Interfaces.C.Strings.New_String(Name);
      Cms_Begin_Struct_Array_Elem(Ca,Np,Index);
      Interfaces.C.Strings.Free(Np);
   end Begin_Struct_Array_Elem;

   procedure Cms_End_Struct_Array_Elem(Ca : in Cms_Access; Name : Interfaces.C.Strings.Chars_Ptr; Index :Interfaces.C.Int);

   pragma Import(C,Cms_End_Struct_Array_Elem);

   procedure End_Struct_Array_Elem(Ca : in Cms_Access; Name : in String; Index :Interfaces.C.Int) is
      Np : Interfaces.C.Strings.Chars_Ptr;
   begin
      Np := Interfaces.C.Strings.New_String(Name);
      Cms_End_Struct_Array_Elem(Ca,Np,Index);
      Interfaces.C.Strings.Free(Np);
   end End_Struct_Array_Elem;


   procedure Cms_Begin_Struct_Dynamic_Array(Ca : in Cms_Access; Name : Interfaces.C.Strings.Chars_Ptr; Len : Ints_Ptr;
                                                                                                       Maxlen :Interfaces.C.Int);

   pragma Import(C,Cms_Begin_Struct_Dynamic_Array);

   procedure Begin_Struct_Dynamic_Array(Ca : in Cms_Access; Name : in String; Len : in out Interfaces.C.Int;
                                                                           Maxlen : Interfaces.C.Int) is
      Np : Interfaces.C.Strings.Chars_Ptr;
      LenPtr : Ints_Ptr;
   begin
      LenPtr := new Interfaces.C.Int;
      LenPtr.all := Len;
      Np := Interfaces.C.Strings.New_String(Name);
      Cms_Begin_Struct_Dynamic_Array(Ca,Np,LenPtr,Maxlen);
      Len := LenPtr.all;
      Free(LenPtr);
      Interfaces.C.Strings.Free(Np);
   end Begin_Struct_Dynamic_Array;


   procedure Cms_End_Struct_Dynamic_Array(Ca : in Cms_Access; Name : Interfaces.C.Strings.Chars_Ptr; Len : Ints_Ptr;
                                                                                                       Maxlen : Interfaces.C.Int);

   pragma Import(C,Cms_End_Struct_Dynamic_Array);

   procedure End_Struct_Dynamic_Array(Ca : in Cms_Access; Name : in String; Len : in out Interfaces.C.Int;
                                                                           Maxlen : Interfaces.C.Int) is
      Np : Interfaces.C.Strings.Chars_Ptr;
      LenPtr : Ints_Ptr;
   begin
      LenPtr := new Interfaces.C.Int;
      LenPtr.all := Len;
      Np := Interfaces.C.Strings.New_String(Name);
      Cms_End_Struct_Dynamic_Array(Ca,Np,LenPtr,Maxlen);
      Len := LenPtr.all;
      Free(LenPtr);
      Interfaces.C.Strings.Free(Np);
   end End_Struct_Dynamic_Array;

   function Cms_Create_Cms_Enum_Info(NamePtr : in Interfaces.C.Strings.Chars_Ptr;
                                     NameList : in Char_Array;
                                     IntListPtr : in Int_Array;
                                     MaxNameLen : in Int ;
                                     ListLen : in Int;
                                     Symbol_Lookup : in Symbol_Lookup_Callback
                                    ) return Cms_Enum_Info_C_Access;

   pragma Import(C,Cms_Create_Cms_Enum_Info);

   function New_Cms_Enum_Info(Name : in String;
                              NameList : in Char_Array;
                              IntList : in Int_Array;
                              MaxNameLen : in Int ;
                              ListLen : in Int;
                              Symbol_Lookup : in Symbol_Lookup_Callback)
                             return Cms_Enum_Info_Access is
      NamePtr : Interfaces.C.Strings.Chars_Ptr;
      --NameListPtr : Interfaces.C.Strings.Chars_Ptr;
      Ceia : Cms_Enum_Info_Access := new Cms_Enum_Info;
   begin
      NamePtr := Interfaces.C.Strings.New_String(Name);
      --NameListPtr := Interfaces.C.Strings.To_Chars_Ptr(NameList);
      Ceia.Info := Cms_Create_Cms_Enum_Info(NamePtr,NameList, IntList,MaxNameLen,ListLen,Symbol_Lookup);
      return Ceia;
   end New_Cms_Enum_Info;


   function Cms_Update_Enumeration(Ca : in Cms_Access;
                                   Name : in Interfaces.C.Char_Array;
                                   X : in Interfaces.C.Int;
                                   Xaddr : in Ints_Ptr;
                                   Info : in Cms_Enum_Info_C_Access) return Int;

   pragma Import(C,Cms_Update_Enumeration);

   function Update_Enumeration(Ca : in Cms_Access;
                               Name : in String;
                               X : in Interfaces.C.Int;
                               Info : in Cms_Enum_Info_Access) return Int is
      Xptr : Ints_Ptr;
      -- NamePtr : Interfaces.C.Strings.Chars_Ptr;
      Rv : Int;
   begin
      Xptr := new Interfaces.C.Int;
      Xptr.all := X;
      -- NamePtr := Interfaces.C.Strings.New_String(Name);
      Rv := Cms_Update_Enumeration(Ca,To_C(Name),Xptr.all,Xptr,Info.Info);
      Free(Xptr);
      -- Interfaces.C.Strings.Free(NamePtr);
      return Rv;
   end Update_Enumeration;


   procedure Cms_Begin_Enumeration_Array (Ca : in Cms_Access;
                                          Name : in Interfaces.C.Char_Array;
                                          Info : in Cms_Enum_Info_C_Access;
                                          Len : in Interfaces.C.Unsigned);

   pragma Import(C,Cms_Begin_Enumeration_Array);

   -- EnumArrayNamePtr: Interfaces.C.Strings.Chars_Ptr := Null_Ptr;

   procedure Begin_Enumeration_Array (Ca : in Cms_Access;
                                      Name : in String;
                                      Info : in Cms_Enum_Info_Access;
                                      Len : in Interfaces.C.Unsigned) is
   begin
      -- if EnumArrayNamePtr /= Null_Ptr then
          -- Interfaces.C.Strings.Free(EnumArrayNamePtr);
          -- EnumArrayNamePtr := Null_Ptr;
      -- end if;
      -- EnumArrayNamePtr :=Interfaces.C.Strings.New_String(Name);
      Cms_Begin_Enumeration_Array(Ca,To_C(Name),Info.Info,Len);
      -- Interfaces.C.Strings.Free(NamePtr);
   end Begin_Enumeration_Array;

   procedure Cms_Begin_Enumeration_DLA (Ca : in Cms_Access;
                                    Name : in Interfaces.C.Char_Array;
                                    Info : in Cms_Enum_Info_C_Access;
                                    Len : in Ints_Ptr;
                                    MaxLen : in Interfaces.C.Int);

   pragma Import(C,Cms_Begin_Enumeration_Dla);

   procedure Begin_Enumeration_Dla(Ca : in Cms_Access;
                                    Name : in String;
                                    Info : in Cms_Enum_Info_Access;
                                    Len : in out Interfaces.C.Int;
                                    MaxLen : in Interfaces.C.Int) is
     LenPtr : Ints_Ptr;
   begin
      -- if EnumArrayNamePtr /= Null_Ptr then
         -- Interfaces.C.Strings.Free(EnumArrayNamePtr);
         -- EnumArrayNamePtr := Null_Ptr;
      -- end if;
      -- EnumArrayNamePtr :=Interfaces.C.Strings.New_String(Name);
      LenPtr := new Interfaces.C.Int;
      LenPtr.all := Len;
      Cms_Begin_Enumeration_Dla(Ca,To_C(Name),Info.Info,LenPtr,MaxLen);
      --Interfaces.C.Strings.Free(NamePtr);
      Free(LenPtr);
   end Begin_Enumeration_Dla;

   function Cms_Update_Enumeration_Array_Elem(Ca : in Cms_Access;
                                              X : in Interfaces.C.Int;
                                              Xptr : in Ints_Ptr;
                                              Elem : in Interfaces.C.Int) return Int;

   pragma Import(C,Cms_Update_Enumeration_Array_Elem);

   function Update_Enumeration_Array_Elem(Ca : in Cms_Access;
                                          Name : in String;
                                          X : in Interfaces.C.Int;
                                          Elem : in Interfaces.C.Int) return Int is
      Xptr : Ints_Ptr;
      Rv : Int;
   begin
      Xptr := new Interfaces.C.Int;
      Xptr.all := X;
      Rv := Cms_Update_Enumeration_Array_Elem(Ca,X,Xptr,Elem);
      Free(Xptr);
      return Rv;
   end Update_Enumeration_Array_Elem;




   procedure Cms_End_Enumeration_Array (Ca : in Cms_Access;
                                          Name : in Interfaces.C.Char_Array;
                                          Info : in Cms_Enum_Info_C_Access;
                                          Len : in Interfaces.C.Unsigned);

   pragma Import(C,Cms_End_Enumeration_Array);

   procedure End_Enumeration_Array (Ca : in Cms_Access;
                                      Name : in String;
                                      Info : in Cms_Enum_Info_Access;
                                      Len : in Interfaces.C.Unsigned) is
      --NamePtr: Interfaces.C.Strings.Chars_Ptr;
   begin
      --NamePtr :=Interfaces.C.Strings.New_String(Name);
      Cms_End_Enumeration_Array(Ca,To_C(Name),Info.Info,Len);
      --Interfaces.C.Strings.Free(NamePtr);
      --if EnumArrayNamePtr /= Null_Ptr then
        -- Interfaces.C.Strings.Free(EnumArrayNamePtr);
        -- EnumArrayNamePtr := Null_Ptr;
      --end if;
   end End_Enumeration_Array;

   procedure Cms_End_Enumeration_DLA (Ca : in Cms_Access;
                                    Name : in Interfaces.C.Char_Array;
                                    Info : in Cms_Enum_Info_C_Access;
                                    Len : in Ints_Ptr;
                                    MaxLen : in Interfaces.C.Int);

   pragma Import(C,Cms_End_Enumeration_Dla);

   procedure End_Enumeration_DLA (Ca : in Cms_Access;
                                    Name : in String;
                                    Info : in Cms_Enum_Info_Access;
                                    Len : in out Interfaces.C.Int;
                                    MaxLen : in Interfaces.C.Int) is
     --NamePtr: Interfaces.C.Strings.Chars_Ptr;
     LenPtr : Ints_Ptr;
   begin
      --NamePtr :=Interfaces.C.Strings.New_String(Name);
      LenPtr := new Interfaces.C.Int;
      LenPtr.all := Len;
      Cms_End_Enumeration_Dla(Ca,To_C(Name),Info.Info,LenPtr,MaxLen);
      --Interfaces.C.Strings.Free(NamePtr);
      Free(LenPtr);
      --if EnumArrayNamePtr /= Null_Ptr then
      --   Interfaces.C.Strings.Free(EnumArrayNamePtr);
      --   EnumArrayNamePtr := Null_Ptr;
      --end if;
   end End_Enumeration_Dla;

end Cms;

