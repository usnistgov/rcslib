/*
*	New Java File starts here.
*	This file should be named TEST_MESSAGE.java
*	Automatically generated by NML CodeGen Java Applet.
*	from nml_test_format.hh:208
*	with command line arguments :  generate_for_all_langs=true HHFile=nml_test_format.hh
*
*	.gen script :
*		0:load nml_test_format.hh
*		1:clear
*		2:select_from_file nml_test_format.hh
*		3:generate C protos>nml_test_format_c_n.h
*		4:generate C format>nml_test_format_c_n.c
*		5:generate C update>nml_test_format_c_n.c
*		6:generate C++ protos>nml_test_format_n_codegen_protos.hh
*		7:generate C++ format>nml_test_format_n.cc
*		8:generate C++ update>nml_test_format_n.cc
*		9:generate C++ constructor>nml_test_format_n.cc
*		10:generate Java dict>nml_test_format_MsgDict.java
*		11:generate Java classes >*
*		12:generate Ada spec>nml_test_format_n_ada.ads
*		13:generate Ada body>nml_test_format_n_ada.adb
*		14:exit
*
*/

// Import all NML and posemath interfaces
import rcs.nml.*;
import rcs.posemath.*;

/*
*	Class definition for TEST_MESSAGE
*	Automatically generated by NML CodeGen Java Applet.
*/
public class TEST_MESSAGE extends TEST_MESSAGE_BASE implements Cloneable
{
	public byte byte_to_messup_msg = 0;
	public long first_count = 0;
	public struct_from_other_header sfoh = new struct_from_other_header();
	public boolean b = false;
	public byte c = 0;
	public double d = 0;
	public int i = 0;
	public float f = 0;
	public long l = 0;
	public long ul = 0;
	public fwLaserStruct fw = new fwLaserStruct();

	public static NML_ENUM_INFO nml_enum_info_for_enumtest_typedef=null;
	public int etd =0; /* enum enumtest_typedef : zzz=0 */

	public static NML_ENUM_INFO nml_enum_info_for_enumtest_typedef2=null;
	public int etd2 =0; /* enum enumtest_typedef2 : www=0 */
	public byte big_array[] = new byte[1000];
	public boolean bool_array[] = new boolean[2];
	public int ia[] = new int[2];
	public byte ca[] = new byte[2];
	public float fa[] = new float[2];
	public double da[] = new double[2];
	public double two_d_array[] = new double[4];
	public double three_d_array[] = new double[8];
	public float f_pi = (float) 3.14159265;/* set by default comment */
	public int cda_length = 0;
	public byte cda[] = new byte[8]; /* NML_DYNAMIC_LENGTH_ARRAY */
	public double seventysevenpointseven = (double) 77.7;/* set by default comment */
	public int ida_length = 0;
	public int ida[] = new int[8]; /* NML_DYNAMIC_LENGTH_ARRAY */
	public double eightyeightpointeight = (double) 88.8;/* set by default comment */
	public int fda_length = 0;
	public float fda[] = new float[8]; /* NML_DYNAMIC_LENGTH_ARRAY */
	public int dda_length = 0;
	public double dda[] = new double[8]; /* NML_DYNAMIC_LENGTH_ARRAY */
	public teststruct s = new teststruct();
	public teststruct_td2 s_td2 = new teststruct_td2();
	public teststruct sa[] = new teststruct[2];
	public double d_pi = (double) 3.14159265;/* set by default comment */
	public int sda_length = 0;
	public teststruct sda[] = new teststruct[2]; /* NML_DYNAMIC_LENGTH_ARRAY */

	public static NML_ENUM_INFO nml_enum_info_for_enumtest=null;
	public int enumtestvar =0; /* enum enumtest : a=0 */
	public int enum_array[] = new int[5];
	public int enumtest_dla_length = 0;
	public int enumtest_dla[] = new int[7]; /* NML_DYNAMIC_LENGTH_ARRAY */
	public PM_CARTESIAN cart = new PM_CARTESIAN();
	public PM_CARTESIAN cart_array[] = new PM_CARTESIAN[3];
	public int cart_dla_length = 0;
	public PM_CARTESIAN cart_dla[] = new PM_CARTESIAN[5]; /* NML_DYNAMIC_LENGTH_ARRAY */
	public boolean do_int_size_test = false;
	public short smin = 0;
	public short smax = 0;
	public int i_smin = 0;
	public int i_smax = 0;
	public int imin = 0;
	public int imax = 0;
	public long l_imin = 0;
	public long l_imax = 0;
	public long lmin = 0;
	public long lmax = 0;
	public short usmax = 0;
	public int ui_usmax = 0;
	public int uimax = 0;
	public long ul_uimax = 0;
	public long ulmax = 0;
	public double d_ulmax = 0;
	public double d_lmin = 0;
	public double d_lmax = 0;
	public short s_array[] = new short[3];
	public int i_array[] = new int[3];
	public long l_array[] = new long[3];
	public short us_array[] = new short[2];
	public int ui_array[] = new int[2];
	public long ul_array[] = new long[2];
	public boolean false_bool = false;
	public boolean true_bool = false;
	public short sminusone = 0;
	public int iminusone = 0;
	public long lminusone = 0;
	public float fminusone = 0;
	public double dminusone = 0;
	public long last_count = 0;
	public teststruct teststruct_2d_array[] = new teststruct[4];
	public long lastvar = 0;


	// Constructor 
	public TEST_MESSAGE()
	{
		super(101);

		if(nml_enum_info_for_enumtest_typedef==null)
		{
			nml_enum_info_for_enumtest_typedef = new NML_ENUM_INFO();
			nml_enum_info_for_enumtest_typedef.name="enumtest_typedef";
			nml_enum_info_for_enumtest_typedef.string_to_int_hash = new java.util.Hashtable();
			nml_enum_info_for_enumtest_typedef.int_to_string_hash = new java.util.Hashtable();
			Integer I_enumtest_typedef=null;
			String Str_enumtest_typedef=null;
			I_enumtest_typedef= new Integer(2);
			Str_enumtest_typedef= "xxx";
			nml_enum_info_for_enumtest_typedef.int_to_string_hash.put(I_enumtest_typedef,Str_enumtest_typedef);
			nml_enum_info_for_enumtest_typedef.string_to_int_hash.put(Str_enumtest_typedef,I_enumtest_typedef);
			I_enumtest_typedef= new Integer(0);
			Str_enumtest_typedef= "zzz";
			nml_enum_info_for_enumtest_typedef.int_to_string_hash.put(I_enumtest_typedef,Str_enumtest_typedef);
			nml_enum_info_for_enumtest_typedef.string_to_int_hash.put(Str_enumtest_typedef,I_enumtest_typedef);
			I_enumtest_typedef= new Integer(1);
			Str_enumtest_typedef= "yyy";
			nml_enum_info_for_enumtest_typedef.int_to_string_hash.put(I_enumtest_typedef,Str_enumtest_typedef);
			nml_enum_info_for_enumtest_typedef.string_to_int_hash.put(Str_enumtest_typedef,I_enumtest_typedef);
		}


		if(nml_enum_info_for_enumtest_typedef2==null)
		{
			nml_enum_info_for_enumtest_typedef2 = new NML_ENUM_INFO();
			nml_enum_info_for_enumtest_typedef2.name="enumtest_typedef2";
			nml_enum_info_for_enumtest_typedef2.string_to_int_hash = new java.util.Hashtable();
			nml_enum_info_for_enumtest_typedef2.int_to_string_hash = new java.util.Hashtable();
			Integer I_enumtest_typedef2=null;
			String Str_enumtest_typedef2=null;
			I_enumtest_typedef2= new Integer(2);
			Str_enumtest_typedef2= "uuu";
			nml_enum_info_for_enumtest_typedef2.int_to_string_hash.put(I_enumtest_typedef2,Str_enumtest_typedef2);
			nml_enum_info_for_enumtest_typedef2.string_to_int_hash.put(Str_enumtest_typedef2,I_enumtest_typedef2);
			I_enumtest_typedef2= new Integer(0);
			Str_enumtest_typedef2= "www";
			nml_enum_info_for_enumtest_typedef2.int_to_string_hash.put(I_enumtest_typedef2,Str_enumtest_typedef2);
			nml_enum_info_for_enumtest_typedef2.string_to_int_hash.put(Str_enumtest_typedef2,I_enumtest_typedef2);
			I_enumtest_typedef2= new Integer(1);
			Str_enumtest_typedef2= "vvv";
			nml_enum_info_for_enumtest_typedef2.int_to_string_hash.put(I_enumtest_typedef2,Str_enumtest_typedef2);
			nml_enum_info_for_enumtest_typedef2.string_to_int_hash.put(Str_enumtest_typedef2,I_enumtest_typedef2);
		}

		for(int i_big_array = 0; i_big_array < 1000; i_big_array++ )
		{
			big_array[i_big_array]  = 0;
		}
		for(int i_bool_array = 0; i_bool_array < 2; i_bool_array++ )
		{
			bool_array[i_bool_array]  = false;
		}
		for(int i_ia = 0; i_ia < 2; i_ia++ )
		{
			ia[i_ia]  = 0;
		}
		for(int i_ca = 0; i_ca < 2; i_ca++ )
		{
			ca[i_ca]  = 0;
		}
		for(int i_fa = 0; i_fa < 2; i_fa++ )
		{
			fa[i_fa]  = 0;
		}
		for(int i_da = 0; i_da < 2; i_da++ )
		{
			da[i_da]  = 0;
		}
		for(int i_two_d_array = 0; i_two_d_array < 4; i_two_d_array++ )
		{
			two_d_array[i_two_d_array]  = 0;
		}
		for(int i_three_d_array = 0; i_three_d_array < 8; i_three_d_array++ )
		{
			three_d_array[i_three_d_array]  = 0;
		}
		for(int i_cda = 0; i_cda < 8; i_cda++ )
		{
			cda[i_cda]  = 0;
		}
		for(int i_ida = 0; i_ida < 8; i_ida++ )
		{
			ida[i_ida]  = 0;
		}
		for(int i_fda = 0; i_fda < 8; i_fda++ )
		{
			fda[i_fda]  = 0;
		}
		for(int i_dda = 0; i_dda < 8; i_dda++ )
		{
			dda[i_dda]  = 0;
		}
		for(int i_sa = 0; i_sa < 2; i_sa++ )
		{
			sa[i_sa] = new teststruct();
		}
		for(int i_sda = 0; i_sda < 2; i_sda++ )
		{
			sda[i_sda] = new teststruct();
		}

		if(nml_enum_info_for_enumtest==null)
		{
			nml_enum_info_for_enumtest = new NML_ENUM_INFO();
			nml_enum_info_for_enumtest.name="enumtest";
			nml_enum_info_for_enumtest.string_to_int_hash = new java.util.Hashtable();
			nml_enum_info_for_enumtest.int_to_string_hash = new java.util.Hashtable();
			Integer I_enumtest=null;
			String Str_enumtest=null;
			I_enumtest= new Integer(77);
			Str_enumtest= "dd";
			nml_enum_info_for_enumtest.int_to_string_hash.put(I_enumtest,Str_enumtest);
			nml_enum_info_for_enumtest.string_to_int_hash.put(Str_enumtest,I_enumtest);
			I_enumtest= new Integer(1);
			Str_enumtest= "b";
			nml_enum_info_for_enumtest.int_to_string_hash.put(I_enumtest,Str_enumtest);
			nml_enum_info_for_enumtest.string_to_int_hash.put(Str_enumtest,I_enumtest);
			I_enumtest= new Integer(0);
			Str_enumtest= "a";
			nml_enum_info_for_enumtest.int_to_string_hash.put(I_enumtest,Str_enumtest);
			nml_enum_info_for_enumtest.string_to_int_hash.put(Str_enumtest,I_enumtest);
			I_enumtest= new Integer(88);
			Str_enumtest= "e";
			nml_enum_info_for_enumtest.int_to_string_hash.put(I_enumtest,Str_enumtest);
			nml_enum_info_for_enumtest.string_to_int_hash.put(Str_enumtest,I_enumtest);
			I_enumtest= new Integer(2);
			Str_enumtest= "aa";
			nml_enum_info_for_enumtest.int_to_string_hash.put(I_enumtest,Str_enumtest);
			nml_enum_info_for_enumtest.string_to_int_hash.put(Str_enumtest,I_enumtest);
			I_enumtest= new Integer(3);
			Str_enumtest= "bb";
			nml_enum_info_for_enumtest.int_to_string_hash.put(I_enumtest,Str_enumtest);
			nml_enum_info_for_enumtest.string_to_int_hash.put(Str_enumtest,I_enumtest);
			I_enumtest= new Integer(99);
			Str_enumtest= "ccc";
			nml_enum_info_for_enumtest.int_to_string_hash.put(I_enumtest,Str_enumtest);
			nml_enum_info_for_enumtest.string_to_int_hash.put(Str_enumtest,I_enumtest);
		}

		for(int i_enum_array = 0; i_enum_array < 5; i_enum_array++ )
		{
			enum_array[i_enum_array]  = 0; /* enum enumtest : a=0 */
		}
		for(int i_enumtest_dla = 0; i_enumtest_dla < 7; i_enumtest_dla++ )
		{
			enumtest_dla[i_enumtest_dla]  = 0; /* enum enumtest : a=0 */
		}
		for(int i_cart_array = 0; i_cart_array < 3; i_cart_array++ )
		{
			cart_array[i_cart_array] = new PM_CARTESIAN();
		}
		for(int i_cart_dla = 0; i_cart_dla < 5; i_cart_dla++ )
		{
			cart_dla[i_cart_dla] = new PM_CARTESIAN();
		}
		for(int i_s_array = 0; i_s_array < 3; i_s_array++ )
		{
			s_array[i_s_array]  = 0;
		}
		for(int i_i_array = 0; i_i_array < 3; i_i_array++ )
		{
			i_array[i_i_array]  = 0;
		}
		for(int i_l_array = 0; i_l_array < 3; i_l_array++ )
		{
			l_array[i_l_array]  = 0;
		}
		for(int i_us_array = 0; i_us_array < 2; i_us_array++ )
		{
			us_array[i_us_array]  = 0;
		}
		for(int i_ui_array = 0; i_ui_array < 2; i_ui_array++ )
		{
			ui_array[i_ui_array]  = 0;
		}
		for(int i_ul_array = 0; i_ul_array < 2; i_ul_array++ )
		{
			ul_array[i_ul_array]  = 0;
		}
		for(int i_teststruct_2d_array = 0; i_teststruct_2d_array < 4; i_teststruct_2d_array++ )
		{
			teststruct_2d_array[i_teststruct_2d_array] = new teststruct();
		}

	}



	// Constructor that should be used by any classes that extend this class. 
	protected TEST_MESSAGE(int _type)
	{
		super(_type);

		if(nml_enum_info_for_enumtest_typedef==null)
		{
			nml_enum_info_for_enumtest_typedef = new NML_ENUM_INFO();
			nml_enum_info_for_enumtest_typedef.name="enumtest_typedef";
			nml_enum_info_for_enumtest_typedef.string_to_int_hash = new java.util.Hashtable();
			nml_enum_info_for_enumtest_typedef.int_to_string_hash = new java.util.Hashtable();
			Integer I_enumtest_typedef=null;
			String Str_enumtest_typedef=null;
			I_enumtest_typedef= new Integer(2);
			Str_enumtest_typedef= "xxx";
			nml_enum_info_for_enumtest_typedef.int_to_string_hash.put(I_enumtest_typedef,Str_enumtest_typedef);
			nml_enum_info_for_enumtest_typedef.string_to_int_hash.put(Str_enumtest_typedef,I_enumtest_typedef);
			I_enumtest_typedef= new Integer(0);
			Str_enumtest_typedef= "zzz";
			nml_enum_info_for_enumtest_typedef.int_to_string_hash.put(I_enumtest_typedef,Str_enumtest_typedef);
			nml_enum_info_for_enumtest_typedef.string_to_int_hash.put(Str_enumtest_typedef,I_enumtest_typedef);
			I_enumtest_typedef= new Integer(1);
			Str_enumtest_typedef= "yyy";
			nml_enum_info_for_enumtest_typedef.int_to_string_hash.put(I_enumtest_typedef,Str_enumtest_typedef);
			nml_enum_info_for_enumtest_typedef.string_to_int_hash.put(Str_enumtest_typedef,I_enumtest_typedef);
		}


		if(nml_enum_info_for_enumtest_typedef2==null)
		{
			nml_enum_info_for_enumtest_typedef2 = new NML_ENUM_INFO();
			nml_enum_info_for_enumtest_typedef2.name="enumtest_typedef2";
			nml_enum_info_for_enumtest_typedef2.string_to_int_hash = new java.util.Hashtable();
			nml_enum_info_for_enumtest_typedef2.int_to_string_hash = new java.util.Hashtable();
			Integer I_enumtest_typedef2=null;
			String Str_enumtest_typedef2=null;
			I_enumtest_typedef2= new Integer(2);
			Str_enumtest_typedef2= "uuu";
			nml_enum_info_for_enumtest_typedef2.int_to_string_hash.put(I_enumtest_typedef2,Str_enumtest_typedef2);
			nml_enum_info_for_enumtest_typedef2.string_to_int_hash.put(Str_enumtest_typedef2,I_enumtest_typedef2);
			I_enumtest_typedef2= new Integer(0);
			Str_enumtest_typedef2= "www";
			nml_enum_info_for_enumtest_typedef2.int_to_string_hash.put(I_enumtest_typedef2,Str_enumtest_typedef2);
			nml_enum_info_for_enumtest_typedef2.string_to_int_hash.put(Str_enumtest_typedef2,I_enumtest_typedef2);
			I_enumtest_typedef2= new Integer(1);
			Str_enumtest_typedef2= "vvv";
			nml_enum_info_for_enumtest_typedef2.int_to_string_hash.put(I_enumtest_typedef2,Str_enumtest_typedef2);
			nml_enum_info_for_enumtest_typedef2.string_to_int_hash.put(Str_enumtest_typedef2,I_enumtest_typedef2);
		}

		for(int i_big_array = 0; i_big_array < 1000; i_big_array++ )
		{
			big_array[i_big_array]  = 0;
		}
		for(int i_bool_array = 0; i_bool_array < 2; i_bool_array++ )
		{
			bool_array[i_bool_array]  = false;
		}
		for(int i_ia = 0; i_ia < 2; i_ia++ )
		{
			ia[i_ia]  = 0;
		}
		for(int i_ca = 0; i_ca < 2; i_ca++ )
		{
			ca[i_ca]  = 0;
		}
		for(int i_fa = 0; i_fa < 2; i_fa++ )
		{
			fa[i_fa]  = 0;
		}
		for(int i_da = 0; i_da < 2; i_da++ )
		{
			da[i_da]  = 0;
		}
		for(int i_two_d_array = 0; i_two_d_array < 4; i_two_d_array++ )
		{
			two_d_array[i_two_d_array]  = 0;
		}
		for(int i_three_d_array = 0; i_three_d_array < 8; i_three_d_array++ )
		{
			three_d_array[i_three_d_array]  = 0;
		}
		for(int i_cda = 0; i_cda < 8; i_cda++ )
		{
			cda[i_cda]  = 0;
		}
		for(int i_ida = 0; i_ida < 8; i_ida++ )
		{
			ida[i_ida]  = 0;
		}
		for(int i_fda = 0; i_fda < 8; i_fda++ )
		{
			fda[i_fda]  = 0;
		}
		for(int i_dda = 0; i_dda < 8; i_dda++ )
		{
			dda[i_dda]  = 0;
		}
		for(int i_sa = 0; i_sa < 2; i_sa++ )
		{
			sa[i_sa] = new teststruct();
		}
		for(int i_sda = 0; i_sda < 2; i_sda++ )
		{
			sda[i_sda] = new teststruct();
		}

		if(nml_enum_info_for_enumtest==null)
		{
			nml_enum_info_for_enumtest = new NML_ENUM_INFO();
			nml_enum_info_for_enumtest.name="enumtest";
			nml_enum_info_for_enumtest.string_to_int_hash = new java.util.Hashtable();
			nml_enum_info_for_enumtest.int_to_string_hash = new java.util.Hashtable();
			Integer I_enumtest=null;
			String Str_enumtest=null;
			I_enumtest= new Integer(77);
			Str_enumtest= "dd";
			nml_enum_info_for_enumtest.int_to_string_hash.put(I_enumtest,Str_enumtest);
			nml_enum_info_for_enumtest.string_to_int_hash.put(Str_enumtest,I_enumtest);
			I_enumtest= new Integer(1);
			Str_enumtest= "b";
			nml_enum_info_for_enumtest.int_to_string_hash.put(I_enumtest,Str_enumtest);
			nml_enum_info_for_enumtest.string_to_int_hash.put(Str_enumtest,I_enumtest);
			I_enumtest= new Integer(0);
			Str_enumtest= "a";
			nml_enum_info_for_enumtest.int_to_string_hash.put(I_enumtest,Str_enumtest);
			nml_enum_info_for_enumtest.string_to_int_hash.put(Str_enumtest,I_enumtest);
			I_enumtest= new Integer(88);
			Str_enumtest= "e";
			nml_enum_info_for_enumtest.int_to_string_hash.put(I_enumtest,Str_enumtest);
			nml_enum_info_for_enumtest.string_to_int_hash.put(Str_enumtest,I_enumtest);
			I_enumtest= new Integer(2);
			Str_enumtest= "aa";
			nml_enum_info_for_enumtest.int_to_string_hash.put(I_enumtest,Str_enumtest);
			nml_enum_info_for_enumtest.string_to_int_hash.put(Str_enumtest,I_enumtest);
			I_enumtest= new Integer(3);
			Str_enumtest= "bb";
			nml_enum_info_for_enumtest.int_to_string_hash.put(I_enumtest,Str_enumtest);
			nml_enum_info_for_enumtest.string_to_int_hash.put(Str_enumtest,I_enumtest);
			I_enumtest= new Integer(99);
			Str_enumtest= "ccc";
			nml_enum_info_for_enumtest.int_to_string_hash.put(I_enumtest,Str_enumtest);
			nml_enum_info_for_enumtest.string_to_int_hash.put(Str_enumtest,I_enumtest);
		}

		for(int i_enum_array = 0; i_enum_array < 5; i_enum_array++ )
		{
			enum_array[i_enum_array]  = 0; /* enum enumtest : a=0 */
		}
		for(int i_enumtest_dla = 0; i_enumtest_dla < 7; i_enumtest_dla++ )
		{
			enumtest_dla[i_enumtest_dla]  = 0; /* enum enumtest : a=0 */
		}
		for(int i_cart_array = 0; i_cart_array < 3; i_cart_array++ )
		{
			cart_array[i_cart_array] = new PM_CARTESIAN();
		}
		for(int i_cart_dla = 0; i_cart_dla < 5; i_cart_dla++ )
		{
			cart_dla[i_cart_dla] = new PM_CARTESIAN();
		}
		for(int i_s_array = 0; i_s_array < 3; i_s_array++ )
		{
			s_array[i_s_array]  = 0;
		}
		for(int i_i_array = 0; i_i_array < 3; i_i_array++ )
		{
			i_array[i_i_array]  = 0;
		}
		for(int i_l_array = 0; i_l_array < 3; i_l_array++ )
		{
			l_array[i_l_array]  = 0;
		}
		for(int i_us_array = 0; i_us_array < 2; i_us_array++ )
		{
			us_array[i_us_array]  = 0;
		}
		for(int i_ui_array = 0; i_ui_array < 2; i_ui_array++ )
		{
			ui_array[i_ui_array]  = 0;
		}
		for(int i_ul_array = 0; i_ul_array < 2; i_ul_array++ )
		{
			ul_array[i_ul_array]  = 0;
		}
		for(int i_teststruct_2d_array = 0; i_teststruct_2d_array < 4; i_teststruct_2d_array++ )
		{
			teststruct_2d_array[i_teststruct_2d_array] = new teststruct();
		}

	}


	public void update(NMLFormatConverter nml_fc)
	{

		nml_fc.beginClass("TEST_MESSAGE","TEST_MESSAGE_BASE");

		nml_fc.beginBaseClass("TEST_MESSAGE_BASE");

		super.update(nml_fc);

		nml_fc.endBaseClass("TEST_MESSAGE_BASE");
		byte_to_messup_msg = nml_fc.update_with_name("byte_to_messup_msg",byte_to_messup_msg);
		first_count = nml_fc.update_with_name("first_count",first_count);
		nml_fc.beginClassVar("sfoh");
		sfoh.update(nml_fc);
		nml_fc.endClassVar("sfoh");
		b = nml_fc.update_with_name("b",b);
		c = nml_fc.update_with_name("c",c);
		d = nml_fc.update_with_name("d",d);
		i = nml_fc.update_with_name("i",i);
		f = nml_fc.update_with_name("f",f);
		l = nml_fc.update_with_name("l",l);
		ul = nml_fc.update_unsigned_with_name("ul",ul);
		nml_fc.beginClassVar("fw");
		fw.update(nml_fc);
		nml_fc.endClassVar("fw");
		etd = nml_fc.update_enumeration_with_name("etd",etd,nml_enum_info_for_enumtest_typedef);
		etd2 = nml_fc.update_enumeration_with_name("etd2",etd2,nml_enum_info_for_enumtest_typedef2);
		nml_fc.update_with_name("big_array",big_array,1000);
		nml_fc.update_with_name("bool_array",bool_array,2);
		nml_fc.update_with_name("ia",ia,2);
		nml_fc.update_with_name("ca",ca,2);
		nml_fc.update_with_name("fa",fa,2);
		nml_fc.update_with_name("da",da,2);
		nml_fc.update_with_name("two_d_array",two_d_array,4);
		nml_fc.update_with_name("three_d_array",three_d_array,8);
		nml_fc.next_update_default("3.14159265");
		f_pi = nml_fc.update_with_name("f_pi",f_pi);
		cda_length = nml_fc.update_dla_length_with_name("cda_length",cda_length);
		nml_fc.update_with_name("cda",cda,cda_length);
		nml_fc.next_update_default("77.7");
		seventysevenpointseven = nml_fc.update_with_name("seventysevenpointseven",seventysevenpointseven);
		ida_length = nml_fc.update_dla_length_with_name("ida_length",ida_length);
		nml_fc.update_with_name("ida",ida,ida_length);
		nml_fc.next_update_default("88.8");
		eightyeightpointeight = nml_fc.update_with_name("eightyeightpointeight",eightyeightpointeight);
		fda_length = nml_fc.update_dla_length_with_name("fda_length",fda_length);
		nml_fc.update_with_name("fda",fda,fda_length);
		dda_length = nml_fc.update_dla_length_with_name("dda_length",dda_length);
		nml_fc.update_with_name("dda",dda,dda_length);
		nml_fc.beginClassVar("s");
		s.update(nml_fc);
		nml_fc.endClassVar("s");
		nml_fc.beginClassVar("s_td2");
		s_td2.update(nml_fc);
		nml_fc.endClassVar("s_td2");
		for(int i_sa = 0; i_sa < 2; i_sa++ )
		{
			nml_fc.beginClassArrayElem("sa",i_sa);
			sa[i_sa].update(nml_fc);

			nml_fc.endClassArrayElem("sa",i_sa);
		}
		nml_fc.next_update_default("3.14159265");
		d_pi = nml_fc.update_with_name("d_pi",d_pi);
		sda_length = nml_fc.update_dla_length_with_name("sda_length",sda_length);
		for(int i_sda = 0; i_sda < sda_length; i_sda++ )
		{
			nml_fc.beginClassArrayElem("sda",i_sda);
			sda[i_sda].update(nml_fc);

			nml_fc.endClassArrayElem("sda",i_sda);
		}
		enumtestvar = nml_fc.update_enumeration_with_name("enumtestvar",enumtestvar,nml_enum_info_for_enumtest);
		nml_fc.update_enumeration_array_with_name("enum_array",enum_array,5,nml_enum_info_for_enumtest);
		enumtest_dla_length = nml_fc.update_dla_length_with_name("enumtest_dla_length",enumtest_dla_length);
		nml_fc.update_enumeration_array_with_name("enumtest_dla",enumtest_dla,enumtest_dla_length,nml_enum_info_for_enumtest);
		nml_fc.beginClassVar("cart");
		cart.update(nml_fc);
		nml_fc.endClassVar("cart");
		for(int i_cart_array = 0; i_cart_array < 3; i_cart_array++ )
		{
			nml_fc.beginClassArrayElem("cart_array",i_cart_array);
			cart_array[i_cart_array].update(nml_fc);

			nml_fc.endClassArrayElem("cart_array",i_cart_array);
		}
		cart_dla_length = nml_fc.update_dla_length_with_name("cart_dla_length",cart_dla_length);
		for(int i_cart_dla = 0; i_cart_dla < cart_dla_length; i_cart_dla++ )
		{
			nml_fc.beginClassArrayElem("cart_dla",i_cart_dla);
			cart_dla[i_cart_dla].update(nml_fc);

			nml_fc.endClassArrayElem("cart_dla",i_cart_dla);
		}
		do_int_size_test = nml_fc.update_with_name("do_int_size_test",do_int_size_test);
		smin = nml_fc.update_with_name("smin",smin);
		smax = nml_fc.update_with_name("smax",smax);
		i_smin = nml_fc.update_with_name("i_smin",i_smin);
		i_smax = nml_fc.update_with_name("i_smax",i_smax);
		imin = nml_fc.update_with_name("imin",imin);
		imax = nml_fc.update_with_name("imax",imax);
		l_imin = nml_fc.update_with_name("l_imin",l_imin);
		l_imax = nml_fc.update_with_name("l_imax",l_imax);
		lmin = nml_fc.update_with_name("lmin",lmin);
		lmax = nml_fc.update_with_name("lmax",lmax);
		usmax = nml_fc.update_unsigned_with_name("usmax",usmax);
		ui_usmax = nml_fc.update_unsigned_with_name("ui_usmax",ui_usmax);
		uimax = nml_fc.update_unsigned_with_name("uimax",uimax);
		ul_uimax = nml_fc.update_unsigned_with_name("ul_uimax",ul_uimax);
		ulmax = nml_fc.update_unsigned_with_name("ulmax",ulmax);
		d_ulmax = nml_fc.update_with_name("d_ulmax",d_ulmax);
		d_lmin = nml_fc.update_with_name("d_lmin",d_lmin);
		d_lmax = nml_fc.update_with_name("d_lmax",d_lmax);
		nml_fc.update_with_name("s_array",s_array,3);
		nml_fc.update_with_name("i_array",i_array,3);
		nml_fc.update_with_name("l_array",l_array,3);
		nml_fc.update_unsigned_with_name("us_array",us_array,2);
		nml_fc.update_unsigned_with_name("ui_array",ui_array,2);
		nml_fc.update_unsigned_with_name("ul_array",ul_array,2);
		false_bool = nml_fc.update_with_name("false_bool",false_bool);
		true_bool = nml_fc.update_with_name("true_bool",true_bool);
		sminusone = nml_fc.update_with_name("sminusone",sminusone);
		iminusone = nml_fc.update_with_name("iminusone",iminusone);
		lminusone = nml_fc.update_with_name("lminusone",lminusone);
		fminusone = nml_fc.update_with_name("fminusone",fminusone);
		dminusone = nml_fc.update_with_name("dminusone",dminusone);
		last_count = nml_fc.update_with_name("last_count",last_count);
		for(int i_teststruct_2d_array = 0; i_teststruct_2d_array < 4; i_teststruct_2d_array++ )
		{
			nml_fc.beginClassArrayElem("teststruct_2d_array",i_teststruct_2d_array);
			teststruct_2d_array[i_teststruct_2d_array].update(nml_fc);

			nml_fc.endClassArrayElem("teststruct_2d_array",i_teststruct_2d_array);
		}
		lastvar = nml_fc.update_with_name("lastvar",lastvar);

		nml_fc.endClass("TEST_MESSAGE","TEST_MESSAGE_BASE");

	}


	public TEST_MESSAGE clone() throws CloneNotSupportedException
	{
		TEST_MESSAGE cloned_object = (TEST_MESSAGE) super.clone();
		if(this.sfoh != null) {
			cloned_object.sfoh = (struct_from_other_header ) this.sfoh.clone();
		}
		if(this.fw != null) {
			cloned_object.fw = (fwLaserStruct ) this.fw.clone();
		}
		if(this.big_array != null) {
			cloned_object.big_array = (byte []) this.big_array.clone();
		}
		if(this.bool_array != null) {
			cloned_object.bool_array = (boolean []) this.bool_array.clone();
		}
		if(this.ia != null) {
			cloned_object.ia = (int []) this.ia.clone();
		}
		if(this.ca != null) {
			cloned_object.ca = (byte []) this.ca.clone();
		}
		if(this.fa != null) {
			cloned_object.fa = (float []) this.fa.clone();
		}
		if(this.da != null) {
			cloned_object.da = (double []) this.da.clone();
		}
		if(this.two_d_array != null) {
			cloned_object.two_d_array = (double []) this.two_d_array.clone();
		}
		if(this.three_d_array != null) {
			cloned_object.three_d_array = (double []) this.three_d_array.clone();
		}
		if(this.cda != null) {
			cloned_object.cda = (byte []) this.cda.clone();
		}
		if(this.ida != null) {
			cloned_object.ida = (int []) this.ida.clone();
		}
		if(this.fda != null) {
			cloned_object.fda = (float []) this.fda.clone();
		}
		if(this.dda != null) {
			cloned_object.dda = (double []) this.dda.clone();
		}
		if(this.s != null) {
			cloned_object.s = (teststruct ) this.s.clone();
		}
		if(this.s_td2 != null) {
			cloned_object.s_td2 = (teststruct_td2 ) this.s_td2.clone();
		}
		if(this.sa != null) {
			cloned_object.sa = (teststruct []) this.sa.clone();
			for(int i_sa=0; i_sa < this.sa.length; i_sa++) {
				if(this.sa[i_sa] != null) {
					cloned_object.sa[i_sa] = (teststruct) this.sa[i_sa].clone();
				}
			}
		}
		if(this.sda != null) {
			cloned_object.sda = (teststruct []) this.sda.clone();
			for(int i_sda=0; i_sda < this.sda.length; i_sda++) {
				if(this.sda[i_sda] != null) {
					cloned_object.sda[i_sda] = (teststruct) this.sda[i_sda].clone();
				}
			}
		}
		if(this.enum_array != null) {
			cloned_object.enum_array = (int []) this.enum_array.clone();
		}
		if(this.enumtest_dla != null) {
			cloned_object.enumtest_dla = (int []) this.enumtest_dla.clone();
		}
		if(this.cart != null) {
			cloned_object.cart = (PM_CARTESIAN ) this.cart.clone();
		}
		if(this.cart_array != null) {
			cloned_object.cart_array = (PM_CARTESIAN []) this.cart_array.clone();
			for(int i_cart_array=0; i_cart_array < this.cart_array.length; i_cart_array++) {
				if(this.cart_array[i_cart_array] != null) {
					cloned_object.cart_array[i_cart_array] = (PM_CARTESIAN) this.cart_array[i_cart_array].clone();
				}
			}
		}
		if(this.cart_dla != null) {
			cloned_object.cart_dla = (PM_CARTESIAN []) this.cart_dla.clone();
			for(int i_cart_dla=0; i_cart_dla < this.cart_dla.length; i_cart_dla++) {
				if(this.cart_dla[i_cart_dla] != null) {
					cloned_object.cart_dla[i_cart_dla] = (PM_CARTESIAN) this.cart_dla[i_cart_dla].clone();
				}
			}
		}
		if(this.s_array != null) {
			cloned_object.s_array = (short []) this.s_array.clone();
		}
		if(this.i_array != null) {
			cloned_object.i_array = (int []) this.i_array.clone();
		}
		if(this.l_array != null) {
			cloned_object.l_array = (long []) this.l_array.clone();
		}
		if(this.us_array != null) {
			cloned_object.us_array = (short []) this.us_array.clone();
		}
		if(this.ui_array != null) {
			cloned_object.ui_array = (int []) this.ui_array.clone();
		}
		if(this.ul_array != null) {
			cloned_object.ul_array = (long []) this.ul_array.clone();
		}
		if(this.teststruct_2d_array != null) {
			cloned_object.teststruct_2d_array = (teststruct []) this.teststruct_2d_array.clone();
			for(int i_teststruct_2d_array=0; i_teststruct_2d_array < this.teststruct_2d_array.length; i_teststruct_2d_array++) {
				if(this.teststruct_2d_array[i_teststruct_2d_array] != null) {
					cloned_object.teststruct_2d_array[i_teststruct_2d_array] = (teststruct) this.teststruct_2d_array[i_teststruct_2d_array].clone();
				}
			}
		}
		return cloned_object;

	}


	/* Getters and Setters */
	public byte getbyte_to_messup_msg() {
		return (byte_to_messup_msg);
	}
	public void setbyte_to_messup_msg(byte _byte_to_messup_msg)  {
		this.byte_to_messup_msg=_byte_to_messup_msg;
	}

	public long getfirst_count() {
		return (first_count);
	}
	public void setfirst_count(long _first_count)  {
		this.first_count=_first_count;
	}

	public struct_from_other_header getsfoh() {
		return (sfoh);
	}
	public void setsfoh(struct_from_other_header _sfoh)  {
		this.sfoh=_sfoh;
	}

	public boolean getb() {
		return (b);
	}
	public void setb(boolean _b)  {
		this.b=_b;
	}

	public byte getc() {
		return (c);
	}
	public void setc(byte _c)  {
		this.c=_c;
	}

	public double getd() {
		return (d);
	}
	public void setd(double _d)  {
		this.d=_d;
	}

	public int geti() {
		return (i);
	}
	public void seti(int _i)  {
		this.i=_i;
	}

	public float getf() {
		return (f);
	}
	public void setf(float _f)  {
		this.f=_f;
	}

	public long getl() {
		return (l);
	}
	public void setl(long _l)  {
		this.l=_l;
	}

	public long getul() {
		return (ul);
	}
	public void setul(long _ul)  {
		this.ul=_ul;
	}

	public fwLaserStruct getfw() {
		return (fw);
	}
	public void setfw(fwLaserStruct _fw)  {
		this.fw=_fw;
	}

	public NML_ENUM_INFO getnml_enum_info_for_enumtest_typedef() {
		return (nml_enum_info_for_enumtest_typedef);
	}
	public void setnml_enum_info_for_enumtest_typedef(NML_ENUM_INFO _nml_enum_info_for_enumtest_typedef)  {
		this.nml_enum_info_for_enumtest_typedef=_nml_enum_info_for_enumtest_typedef;
	}

	public int getetd() {
		return (etd);
	}
	public void setetd(int _etd)  {
		this.etd=_etd;
	}

	public NML_ENUM_INFO getnml_enum_info_for_enumtest_typedef2() {
		return (nml_enum_info_for_enumtest_typedef2);
	}
	public void setnml_enum_info_for_enumtest_typedef2(NML_ENUM_INFO _nml_enum_info_for_enumtest_typedef2)  {
		this.nml_enum_info_for_enumtest_typedef2=_nml_enum_info_for_enumtest_typedef2;
	}

	public int getetd2() {
		return (etd2);
	}
	public void setetd2(int _etd2)  {
		this.etd2=_etd2;
	}

	public byte [] getbig_array() {
		return (big_array);
	}
	public void setbig_array(byte [] _big_array)  {
		this.big_array=_big_array;
	}

	public boolean [] getbool_array() {
		return (bool_array);
	}
	public void setbool_array(boolean [] _bool_array)  {
		this.bool_array=_bool_array;
	}

	public int [] getia() {
		return (ia);
	}
	public void setia(int [] _ia)  {
		this.ia=_ia;
	}

	public byte [] getca() {
		return (ca);
	}
	public void setca(byte [] _ca)  {
		this.ca=_ca;
	}

	public float [] getfa() {
		return (fa);
	}
	public void setfa(float [] _fa)  {
		this.fa=_fa;
	}

	public double [] getda() {
		return (da);
	}
	public void setda(double [] _da)  {
		this.da=_da;
	}

	public double [] gettwo_d_array() {
		return (two_d_array);
	}
	public void settwo_d_array(double [] _two_d_array)  {
		this.two_d_array=_two_d_array;
	}

	public double [] getthree_d_array() {
		return (three_d_array);
	}
	public void setthree_d_array(double [] _three_d_array)  {
		this.three_d_array=_three_d_array;
	}

	public float getf_pi() {
		return (f_pi);
	}
	public void setf_pi(float _f_pi)  {
		this.f_pi=_f_pi;
	}

	public int getcda_length() {
		return (cda_length);
	}
	public void setcda_length(int _cda_length)  {
		this.cda_length=_cda_length;
	}

	public byte [] getcda() {
		return (cda);
	}
	public void setcda(byte [] _cda)  {
		this.cda=_cda;
	}

	public double getseventysevenpointseven() {
		return (seventysevenpointseven);
	}
	public void setseventysevenpointseven(double _seventysevenpointseven)  {
		this.seventysevenpointseven=_seventysevenpointseven;
	}

	public int getida_length() {
		return (ida_length);
	}
	public void setida_length(int _ida_length)  {
		this.ida_length=_ida_length;
	}

	public int [] getida() {
		return (ida);
	}
	public void setida(int [] _ida)  {
		this.ida=_ida;
	}

	public double geteightyeightpointeight() {
		return (eightyeightpointeight);
	}
	public void seteightyeightpointeight(double _eightyeightpointeight)  {
		this.eightyeightpointeight=_eightyeightpointeight;
	}

	public int getfda_length() {
		return (fda_length);
	}
	public void setfda_length(int _fda_length)  {
		this.fda_length=_fda_length;
	}

	public float [] getfda() {
		return (fda);
	}
	public void setfda(float [] _fda)  {
		this.fda=_fda;
	}

	public int getdda_length() {
		return (dda_length);
	}
	public void setdda_length(int _dda_length)  {
		this.dda_length=_dda_length;
	}

	public double [] getdda() {
		return (dda);
	}
	public void setdda(double [] _dda)  {
		this.dda=_dda;
	}

	public teststruct gets() {
		return (s);
	}
	public void sets(teststruct _s)  {
		this.s=_s;
	}

	public teststruct_td2 gets_td2() {
		return (s_td2);
	}
	public void sets_td2(teststruct_td2 _s_td2)  {
		this.s_td2=_s_td2;
	}

	public teststruct [] getsa() {
		return (sa);
	}
	public void setsa(teststruct [] _sa)  {
		this.sa=_sa;
	}

	public double getd_pi() {
		return (d_pi);
	}
	public void setd_pi(double _d_pi)  {
		this.d_pi=_d_pi;
	}

	public int getsda_length() {
		return (sda_length);
	}
	public void setsda_length(int _sda_length)  {
		this.sda_length=_sda_length;
	}

	public teststruct [] getsda() {
		return (sda);
	}
	public void setsda(teststruct [] _sda)  {
		this.sda=_sda;
	}

	public NML_ENUM_INFO getnml_enum_info_for_enumtest() {
		return (nml_enum_info_for_enumtest);
	}
	public void setnml_enum_info_for_enumtest(NML_ENUM_INFO _nml_enum_info_for_enumtest)  {
		this.nml_enum_info_for_enumtest=_nml_enum_info_for_enumtest;
	}

	public int getenumtestvar() {
		return (enumtestvar);
	}
	public void setenumtestvar(int _enumtestvar)  {
		this.enumtestvar=_enumtestvar;
	}

	public int [] getenum_array() {
		return (enum_array);
	}
	public void setenum_array(int [] _enum_array)  {
		this.enum_array=_enum_array;
	}

	public int getenumtest_dla_length() {
		return (enumtest_dla_length);
	}
	public void setenumtest_dla_length(int _enumtest_dla_length)  {
		this.enumtest_dla_length=_enumtest_dla_length;
	}

	public int [] getenumtest_dla() {
		return (enumtest_dla);
	}
	public void setenumtest_dla(int [] _enumtest_dla)  {
		this.enumtest_dla=_enumtest_dla;
	}

	public PM_CARTESIAN getcart() {
		return (cart);
	}
	public void setcart(PM_CARTESIAN _cart)  {
		this.cart=_cart;
	}

	public PM_CARTESIAN [] getcart_array() {
		return (cart_array);
	}
	public void setcart_array(PM_CARTESIAN [] _cart_array)  {
		this.cart_array=_cart_array;
	}

	public int getcart_dla_length() {
		return (cart_dla_length);
	}
	public void setcart_dla_length(int _cart_dla_length)  {
		this.cart_dla_length=_cart_dla_length;
	}

	public PM_CARTESIAN [] getcart_dla() {
		return (cart_dla);
	}
	public void setcart_dla(PM_CARTESIAN [] _cart_dla)  {
		this.cart_dla=_cart_dla;
	}

	public boolean getdo_int_size_test() {
		return (do_int_size_test);
	}
	public void setdo_int_size_test(boolean _do_int_size_test)  {
		this.do_int_size_test=_do_int_size_test;
	}

	public short getsmin() {
		return (smin);
	}
	public void setsmin(short _smin)  {
		this.smin=_smin;
	}

	public short getsmax() {
		return (smax);
	}
	public void setsmax(short _smax)  {
		this.smax=_smax;
	}

	public int geti_smin() {
		return (i_smin);
	}
	public void seti_smin(int _i_smin)  {
		this.i_smin=_i_smin;
	}

	public int geti_smax() {
		return (i_smax);
	}
	public void seti_smax(int _i_smax)  {
		this.i_smax=_i_smax;
	}

	public int getimin() {
		return (imin);
	}
	public void setimin(int _imin)  {
		this.imin=_imin;
	}

	public int getimax() {
		return (imax);
	}
	public void setimax(int _imax)  {
		this.imax=_imax;
	}

	public long getl_imin() {
		return (l_imin);
	}
	public void setl_imin(long _l_imin)  {
		this.l_imin=_l_imin;
	}

	public long getl_imax() {
		return (l_imax);
	}
	public void setl_imax(long _l_imax)  {
		this.l_imax=_l_imax;
	}

	public long getlmin() {
		return (lmin);
	}
	public void setlmin(long _lmin)  {
		this.lmin=_lmin;
	}

	public long getlmax() {
		return (lmax);
	}
	public void setlmax(long _lmax)  {
		this.lmax=_lmax;
	}

	public short getusmax() {
		return (usmax);
	}
	public void setusmax(short _usmax)  {
		this.usmax=_usmax;
	}

	public int getui_usmax() {
		return (ui_usmax);
	}
	public void setui_usmax(int _ui_usmax)  {
		this.ui_usmax=_ui_usmax;
	}

	public int getuimax() {
		return (uimax);
	}
	public void setuimax(int _uimax)  {
		this.uimax=_uimax;
	}

	public long getul_uimax() {
		return (ul_uimax);
	}
	public void setul_uimax(long _ul_uimax)  {
		this.ul_uimax=_ul_uimax;
	}

	public long getulmax() {
		return (ulmax);
	}
	public void setulmax(long _ulmax)  {
		this.ulmax=_ulmax;
	}

	public double getd_ulmax() {
		return (d_ulmax);
	}
	public void setd_ulmax(double _d_ulmax)  {
		this.d_ulmax=_d_ulmax;
	}

	public double getd_lmin() {
		return (d_lmin);
	}
	public void setd_lmin(double _d_lmin)  {
		this.d_lmin=_d_lmin;
	}

	public double getd_lmax() {
		return (d_lmax);
	}
	public void setd_lmax(double _d_lmax)  {
		this.d_lmax=_d_lmax;
	}

	public short [] gets_array() {
		return (s_array);
	}
	public void sets_array(short [] _s_array)  {
		this.s_array=_s_array;
	}

	public int [] geti_array() {
		return (i_array);
	}
	public void seti_array(int [] _i_array)  {
		this.i_array=_i_array;
	}

	public long [] getl_array() {
		return (l_array);
	}
	public void setl_array(long [] _l_array)  {
		this.l_array=_l_array;
	}

	public short [] getus_array() {
		return (us_array);
	}
	public void setus_array(short [] _us_array)  {
		this.us_array=_us_array;
	}

	public int [] getui_array() {
		return (ui_array);
	}
	public void setui_array(int [] _ui_array)  {
		this.ui_array=_ui_array;
	}

	public long [] getul_array() {
		return (ul_array);
	}
	public void setul_array(long [] _ul_array)  {
		this.ul_array=_ul_array;
	}

	public boolean getfalse_bool() {
		return (false_bool);
	}
	public void setfalse_bool(boolean _false_bool)  {
		this.false_bool=_false_bool;
	}

	public boolean gettrue_bool() {
		return (true_bool);
	}
	public void settrue_bool(boolean _true_bool)  {
		this.true_bool=_true_bool;
	}

	public short getsminusone() {
		return (sminusone);
	}
	public void setsminusone(short _sminusone)  {
		this.sminusone=_sminusone;
	}

	public int getiminusone() {
		return (iminusone);
	}
	public void setiminusone(int _iminusone)  {
		this.iminusone=_iminusone;
	}

	public long getlminusone() {
		return (lminusone);
	}
	public void setlminusone(long _lminusone)  {
		this.lminusone=_lminusone;
	}

	public float getfminusone() {
		return (fminusone);
	}
	public void setfminusone(float _fminusone)  {
		this.fminusone=_fminusone;
	}

	public double getdminusone() {
		return (dminusone);
	}
	public void setdminusone(double _dminusone)  {
		this.dminusone=_dminusone;
	}

	public long getlast_count() {
		return (last_count);
	}
	public void setlast_count(long _last_count)  {
		this.last_count=_last_count;
	}

	public teststruct [] getteststruct_2d_array() {
		return (teststruct_2d_array);
	}
	public void setteststruct_2d_array(teststruct [] _teststruct_2d_array)  {
		this.teststruct_2d_array=_teststruct_2d_array;
	}

	public long getlastvar() {
		return (lastvar);
	}
	public void setlastvar(long _lastvar)  {
		this.lastvar=_lastvar;
	}


}

