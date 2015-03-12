with Nml;
with Nml_Test_Format_N_Ada;
use Nml_Test_Format_N_Ada;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with Unchecked_Conversion;
with Interfaces.C;
use Interfaces.C;

procedure Nml_Test_Dl_Read_Ada is
   Connection2 :  Nml.NmlConnection_Access;
   Read_Msg : Nml_Test_Format_N_Ada.Test_Message_Access;
   Ok : Integer := 0;
   Msg_Type : Integer;
   Bads : Integer := 0;
   Expected_Last_Var : Long;
begin
   if Ada.Command_Line.Argument_Count < 4 then
      Ada.Text_IO.Put("usage: buffername processname cfgsource lastvar");
      Ada.Text_IO.New_Line;
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      return;
   end if;
   Expected_Last_Var := Long'Value(Ada.Command_Line.Argument(4));

   Connection2 := Nml.CreateConnection(Nml_Test_Format_N_Ada.Format'Access,
                                       Ada.Command_Line.Argument(1),
                                       Ada.Command_Line.Argument(2),
                                       Ada.Command_Line.Argument(3));

   if Not Nml.Valid(Connection2) then
      Nml.Free(Connection2);
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      return;
   end if;

   Msg_Type := Nml.Read(Connection2);
   if Msg_Type = Nml_Test_Format_N_Ada.TEST_MESSAGE_TYPE then
      Read_Msg := Nml_Test_Format_N_Ada.NmlMsg_To_Test_Message(Nml.Get_Address(Connection2));
      Ada.Text_IO.Put_Line("Read_Msg.i=" & Int'Image(Read_Msg.I));
      Ada.Text_IO.Put_Line("Read_Msg.Ia(1)=" & Int'Image(Read_Msg.Ia(1)));
      Ada.Text_IO.Put_Line("Read_Msg.Ia(2)=" & Int'Image(Read_Msg.Ia(2)));
      Ada.Text_IO.Put_Line("Read_Msg.Ia(3)=" & Int'Image(Read_Msg.Ia(3)));
      Ada.Text_IO.Put_Line("Read_Msg.Ia(4)=" & Int'Image(Read_Msg.Ia(4)));
      Ada.Text_IO.Put_Line("Read_Msg.Ida_Length=" & Int'Image(Read_Msg.Ida_Length) );
      Ada.Text_IO.Put_Line("Read_Msg.Ida(1)=" & Int'Image(Read_Msg.Ida(1)) );
      Ada.Text_IO.Put_Line("Read_Msg.Ida(2)=" & Int'Image(Read_Msg.Ida(2)) );
      Ada.Text_IO.Put_Line("Read_Msg.Ida(3)=" & Int'Image(Read_Msg.Ida(3)) );
      Ada.Text_IO.Put_Line("Read_Msg.Ida(4)=" & Int'Image(Read_Msg.Ida(4)) );
      Ada.Text_IO.Put_Line("Read_Msg.Ida(5)=" & Int'Image(Read_Msg.Ida(5)) );
      Ada.Text_IO.Put_Line("Read_Msg.Ida(6)=" & Int'Image(Read_Msg.Ida(6)) );
      Ada.Text_IO.Put_Line("Read_Msg.Ida(7)=" & Int'Image(Read_Msg.Ida(7)) );
      Ada.Text_IO.Put_Line("Read_Msg.Ida(8)=" & Int'Image(Read_Msg.Ida(8)) );

      Bads := 0;
      if Read_Msg.lastvar /= Expected_Last_Var then
         Ada.Text_IO.Put(Ada.Text_IO.Current_Error,"expected_last_var(" & Long'Image(Expected_Last_Var) & ") /= Read_Msg.last_var(" & Long'Image(Read_Msg.lastvar) & ")");
         Ada.Text_IO.New_Line(Ada.Text_IO.Current_Error);
         Nml.Free(Connection2);
         Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
         return;
      end if;


      -- This should have been sent and be aa
      Ada.Text_IO.Put_Line("Read_Msg.enumtestvar=" & Enumtest'Image(Read_Msg.enumtestvar));
      if aa = Read_Msg.Enumtestvar then
         Ada.Text_IO.Put_Line("GOOD (aa = Read_Msg.enumtestvar)");
      else
         Ada.Text_IO.Put_Line("BAD (aa /= Read_Msg.enumtestvar)");
         bads := bads +1;
      end if;



      -- This should have been sent and be bb
      Ada.Text_IO.Put_Line("Read_Msg.enum_array(5)=" & Enumtest'Image(Read_Msg.Enum_Array(5)));
      if bb = Read_Msg.Enum_Array(5) then
         Ada.Text_IO.Put_Line("GOOD (bb = Read_Msg.enum_array[5])");
      else
         Ada.Text_IO.Put_Line("BAD (bb /= Read_Msg.enum_array[5])");
         bads := bads +1;
      end if;



      -- This should have been sent and be 3
      Ada.Text_IO.Put_Line("Read_Msg.enumtest_dla_length=" & Int'Image(Read_Msg.Enumtest_Dla_Length));
      if 3 = Read_Msg.Enumtest_Dla_Length then
         Ada.Text_IO.Put_Line("GOOD (3 = Read_Msg.enumtest_dla_length)");
      else
         Ada.Text_IO.Put_Line("BAD (3 /= Read_Msg.enumtest_dla_length)");
         bads := bads +1;
      end if;


      -- Elements 0,1,and 2 of the dynamic length array should have been sent, elements 3,4,5, and 6 should not.
      Ada.Text_IO.Put_Line("Read_Msg.enumtest_dla(3)=" & Enumtest'Image(Read_Msg.Enumtest_Dla(3)));
      if aa = Read_Msg.Enumtest_Dla(3) then
         Ada.Text_IO.Put_Line("GOOD (aa = Read_Msg.enumtest_dla(3))");
      else
         Ada.Text_IO.Put_Line("BAD (aa /= Read_Msg.enumtest_dla(3))");
         bads := bads +1;
      end if;


      -- The write sets this to bb but it should not have been sent because of the low value of  enumtest_dla_length
      Ada.Text_IO.Put_Line("Read_Msg.enumtest_dla(4)=" & Enumtest'Image(Read_Msg.enumtest_dla(4)));
      if bb /= Read_Msg.enumtest_dla(4) then
         Ada.Text_IO.Put_Line("GOOD (bb /= Read_Msg.enumtest_dla(4)) Unnecessary data was not sent.");
      else
         Ada.Text_IO.Put_Line("BAD (bb = Read_Msg.enumtest_dla(4)) Unnecessary data was sent.");
         bads := bads +1;
      end if;


      --this should have been sent and should be 3.33
      Ada.Text_IO.Put_Line("Read_Msg.three_d_array(Read_Msg.three_d_array'Last)=" & Double'Image(Read_Msg.three_d_array(Read_Msg.three_d_array'Last)));
      if (Read_Msg.three_d_array(Read_Msg.three_d_array'Last) - 3.33) > -1.0E-15 and (Read_Msg.three_d_array(Read_Msg.three_d_array'Last) - 3.33)  < 1.0E-15 then
         Ada.Text_IO.Put_Line("GOOD");
      else
         Ada.Text_IO.Put_Line("BAD should have been 3.33");
         bads := bads +1;
      end if;


      -- This should have been sent and should be 01
      Ada.Text_IO.Put_Line("tst_msg.cda_length=" & Int'Image(Read_Msg.Cda_Length));
      if Read_Msg.cda_length = 3 then
         Ada.Text_IO.Put_Line("GOOD");
      else
         Ada.Text_IO.Put_Line("BAD should have been 3.");
         bads := bads +1;
      end if;


      -- This should have been sent and should be 01
      Ada.Text_IO.Put_Line("Read_Msg.cda=" & To_Ada(Read_Msg.Cda));
      if To_Ada(Read_Msg.Cda) = "01" then
         Ada.Text_IO.Put_Line("GOOD");
      else
         Ada.Text_IO.Put_Line("BAD should have been 01.");
         bads := bads +1;
      end if;


      -- This string should NOT have been sent.
      -- avoid a possible pointer fault since really the value of cda[7] is undefined.
      Read_Msg.Cda(8) := nul;
      Ada.Text_IO.Put_Line("Read_Msg.cda+3 = " & To_Ada(Read_Msg.Cda(3..8)));
      if To_Ada(Read_Msg.Cda(3..8)) /= "2345" then
         Ada.Text_IO.Put_Line("GOOD unnecessary data was NOT transmitted.");
      else
         Ada.Text_IO.Put_Line("BAD unnecessary data was transmitted.");
         bads := bads +1;
      end if;


      -- this should have been sent
      Ada.Text_IO.Put_Line("Read_Msg.sda_length=" & Int'Image(Read_Msg.Sda_Length));
      if Read_Msg.sda_length=1 then
         Ada.Text_IO.Put_Line("GOOD");
      else
         Ada.Text_IO.Put_Line("BAD");
         bads := bads +1;
      end if;


      -- this should have been sent and be x
      Ada.Text_IO.Put_Line("Read_Msg.sda(1).c=" & Char'Image(Read_Msg.sda(1).C));
      if Read_Msg.sda(1).c = 'x' then
         Ada.Text_IO.Put_Line("GOOD");
      else
         Ada.Text_IO.Put_Line("BAD");
         bads := bads +1;
      end if;


      -- this should NOT have been sent.
      Ada.Text_IO.Put_Line("Read_Msg.sda(4).c=" & Char'Image(Read_Msg.sda(4).C));
      if Read_Msg.sda(4).c /= 'y' then
         Ada.Text_IO.Put_Line("GOOD unneccessary data was NOT sent.");
      else
         Ada.Text_IO.Put_Line("BAD unneccessary data was sent.");
         bads := bads +1;
      end if;



      if Read_Msg.True_Bool then
         Ada.Text_IO.Put_Line("GOOD Read_Msg.true_bool is true");
      else
         Ada.Text_IO.Put_Line("BAD Read_Msg.true_bool is false");
         bads := bads +1;
      end if;


      if Not Read_Msg.False_Bool then
         Ada.Text_IO.Put_Line("GOOD Read_Msg.false_bool is false");
      else
         Ada.Text_IO.Put_Line("BAD Read_Msg.false_bool is true");
         bads := bads +1;
      end if;


      if Read_Msg.sminusone = -1 then
         Ada.Text_IO.Put_Line("GOOD  Read_Msg.sminusone equals -1.");
      else
         Ada.Text_IO.Put_Line("BAD Read_Msg.sminusone=" & Short'Image(Read_Msg.Sminusone));
         bads := bads +1;
      end if;


      if Read_Msg.iminusone = -1 then
         Ada.Text_IO.Put_Line("GOOD  Read_Msg.iminusone equals -1.");
      else
         Ada.Text_IO.Put_Line("BAD Read_Msg.iminusone=" & Int'Image(Read_Msg.Iminusone));
         bads := bads +1;
      end if;


      if Read_Msg.lminusone = -1 then
         Ada.Text_IO.Put_Line("GOOD  Read_Msg.lminusone equals -1.");
      else
         Ada.Text_IO.Put_Line("BAD Read_Msg.lminusone=" & Long'Image(Read_Msg.Lminusone));
         bads := bads +1;
      end if;


      if Read_Msg.fminusone = C_Float(-1.0) then
         Ada.Text_IO.Put_Line("GOOD  Read_Msg.fminusone equals -1.");
      else
         Ada.Text_IO.Put_Line("BAD Read_Msg.fminusone=" & C_Float'Image(Read_Msg.Fminusone));
         bads := bads +1;
      end if;


      if Read_Msg.dminusone = Double(-1.0) then
         Ada.Text_IO.Put_Line("GOOD  Read_Msg.dminusone equals -1.");
      else
         Ada.Text_IO.Put_Line("BAD Read_Msg.dminusone=" & Double'Image(Read_Msg.Dminusone));
         bads := bads +1;
      end if;


      Ada.Text_IO.Put_Line("bads="& Integer'Image(Bads));

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

   if Bads > 0 then
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
   end if;

end Nml_Test_Dl_Read_Ada;
