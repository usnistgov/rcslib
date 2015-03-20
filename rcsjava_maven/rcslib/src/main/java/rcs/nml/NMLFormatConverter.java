/* 
The NIST RCS (Real-time Control Systems) 
 library is public domain software, however it is preferred
 that the following disclaimers be attached.

Software Copywrite/Warranty Disclaimer

   This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST Real-Time Control System software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.



*/ 

package rcs.nml;

/*
*
* This interface was added  to eliminate a circular dependancy.
* Previously NMLmsg depended on NMLFormatConverter and NMLFormat
*/

/**
* interface each NMLmsg should use to implement it's update function.
*/
public abstract  class NMLFormatConverter
{
    /**
     * Message being updated.
     */
    public Object msg_to_update = null;
    /**
     * The type of message being converted.
     */
    public int msg_type = 0;
    public String type_string=null;
    public String bufName=null;

    public boolean stat_msg_updated = false;
    public boolean cmd_msg_updated = false;
    public boolean error_in_update = false;
    public boolean always_update_stat_msg=false;
    public boolean always_update_cmd_msg=false;
    public boolean add_array_indexes_to_name=true;
    public boolean sending_short=false;

    public String error_in_update_string=null;

    public boolean tokens_not_used_warning_given=false;
    public boolean bytes_not_used_warning_given=false;

    protected int array_val=-1;
    public void set_array_val(int _array_val)
    {
	array_val=_array_val;
    }

    protected int classVarArrayIndex = -1;
    public void set_classVarArrayIndex(int _classVarArrayIndex) {
	classVarArrayIndex = _classVarArrayIndex;
    }

    public abstract String getVersionString();

    public abstract void SetErrorInUpdate(String str);
    public abstract void SetBufName(String bname);
    public abstract void add_to_output_string(String s);
    public abstract void throw_away_token();

    public abstract boolean  get_decoding();
    public abstract boolean get_use_string();

    public abstract  boolean update(boolean x);
    public abstract  void update(boolean x[],int num_elements);
    public abstract  byte update(byte x);
    public abstract  void update(byte x[],int num_elements);
    public abstract  char update(char x);
    public abstract  void update(char x[],int num_elements);
    public abstract  short update(short x);
    public abstract  void update(short x[],int num_elements);
    public abstract  int update(int x);
    public abstract  void update(int x[],int num_elements);
    public abstract  long update(long x);
    public abstract  void update(long x[],int num_elements);
    public abstract  long update_ll(long x);
    public abstract  void update_ll(long x[],int num_elements);
    public abstract  float update(float x);
    public abstract  void update(float x[],int num_elements);
    public abstract  double update(double x);
    public abstract  void update(double x[],int num_elements);

    public abstract  boolean update_with_name(String name,boolean x);
    public abstract  void update_with_name(String name,boolean x[],int num_elements);

    public abstract  byte update_with_name(String name,byte x);
    public abstract  void update_with_name(String name,byte x[],int num_elements);
    public abstract  char update_with_name(String name,char x);
    public abstract  void update_with_name(String name,char x[],int num_elements);
    public abstract  short update_with_name(String name,short x);
    public abstract  void update_with_name(String name,short x[],int num_elements);
    public abstract  int update_with_name(String name,int x);
    public abstract  void update_with_name(String name,int x[],int num_elements);
    public abstract  long update_with_name(String name,long x);
    public abstract  void update_with_name(String name,long x[],int num_elements);
    public abstract  long update_ll_with_name(String name,long x);
    public abstract  void update_ll_with_name(String name,long x[],int num_elements);
    public abstract  float update_with_name(String name,float x);
    public abstract  void update_with_name(String name,float x[],int num_elements);
    public abstract  double update_with_name(String name,double x);
    public abstract  void update_with_name(String name,double x[], int num_elements);


    public abstract  void update_unsigned(byte x[],int num_elements);
    public abstract  char update_unsigned(char x);
    public abstract  void update_unsigned(char x[],int num_elements);
    public abstract  short update_unsigned(short x);
    public abstract  void update_unsigned(short x[],int num_elements);
    public abstract  int update_unsigned(int x);
    public abstract  void update_unsigned(int x[],int num_elements);
    public abstract  long update_unsigned(long x);
    public abstract  void update_unsigned(long x[],int num_elements);
    public abstract  long update_unsigned_ll(long x);
    public abstract  void update_unsigned_ll(long x[],int num_elements);

    public abstract  byte update_unsigned_with_name(String name,byte x);
    public abstract  void update_unsigned_with_name(String name,byte x[],int num_elements);
    public abstract  char update_unsigned_with_name(String name,char x);
    public abstract  void update_unsigned_with_name(String name,char x[],int num_elements);
    public abstract  short update_unsigned_with_name(String name,short x);
    public abstract  void update_unsigned_with_name(String name,short x[],int num_elements);
    public abstract  int update_unsigned_with_name(String name,int x);
    public abstract  void update_unsigned_with_name(String name,int x[],int num_elements);
    public abstract  long update_unsigned_with_name(String name,long x);
    public abstract  void update_unsigned_with_name(String name,long x[],int num_elements);
    public abstract  long update_unsigned_ll_with_name(String name,long x);
    public abstract  void update_unsigned_ll_with_name(String name,long x[],int num_elements);


    public abstract void beginClass(String name, String base);
    public abstract void endClass(String name, String base);

    public abstract void beginBaseClass(String name);
    public abstract void endBaseClass(String name);

    public abstract void beginClassVar(String name);
    public abstract void endClassVar(String name);

    public abstract void beginClassArrayElem(String name, int elemnum);
    public abstract void endClassArrayElem(String name, int elemnum);

    public abstract int update_enumeration_with_name(String name,  
						     int enumin, 
						     NML_ENUM_INFO info);

    public abstract void update_enumeration_array_with_name(String name,  
							   int enumin[], int num_elements,
							   NML_ENUM_INFO info);
 

    public abstract int update_attribute_enumeration_with_name(String name,  
						     int enumin, 
						     NML_ENUM_INFO info);
 
    public abstract  boolean update_attribute_with_name(String  name,boolean x);
    public abstract  byte update_attribute_with_name(String  name,byte x);
    public abstract  void update_attribute_with_name(String  name,byte x[],int num_elements);
    public abstract  char update_attribute_with_name(String  name,char x);
    public abstract  short update_attribute_with_name(String  name,short x);
    public abstract  int update_attribute_with_name(String  name,int x);
    public abstract  long update_attribute_with_name(String  name,long x);
    public abstract  float update_attribute_with_name(String  name,float x);
    public abstract  double update_attribute_with_name(String  name,double x);

    public abstract  void update_attribute_with_name(String  name,char x[],int num_elements);
    public abstract  void update_attribute_with_name(String  name,short x[],int num_elements);
    public abstract  void update_attribute_with_name(String  name,int x[],int num_elements);
    public abstract  void update_attribute_with_name(String  name,long x[],int num_elements);
    public abstract  void update_attribute_with_name(String  name,float x[],int num_elements);
    public abstract  void update_attribute_with_name(String  name,double x[],int num_elements);

    public abstract int check_type_info(NML_ENUM_INFO info);
    
    public abstract int get_length_of_unbounded(String typename, String varname, Object []oarray);


    public abstract  byte [] update_unbounded_attribute_with_name(String name,byte []x);
    public abstract  byte [] update_unbounded_with_name(String name,byte []x);
    public abstract  short [] update_unbounded_with_name(String name,short []x);
    public abstract  int [] update_unbounded_with_name(String name,int []x);
    public abstract  long [] update_unbounded_with_name(String name,long []x);
    public abstract  byte [] update_unbounded_unsigned_with_name(String name,byte []x);
    public abstract  short [] update_unbounded_unsigned_with_name(String name,short []x);
    public abstract  int [] update_unbounded_unsigned_with_name(String name,int []x);
    public abstract  long [] update_unbounded_unsigned_with_name(String name,long []x);
    public abstract  float [] update_unbounded_with_name(String name,float []x);    
    public abstract  double [] update_unbounded_with_name(String name,double []x);
    
    public abstract  int update_dla_length_with_name(String name,int x);

    public abstract  void next_update_default(String s);


    public abstract void update_CMS_TIME(CMS_TIME time);
    public abstract void update_CMS_DATE(CMS_DATE date);
    public abstract int get_token_count();
    public abstract void set_diagnostics_mode(boolean dm);
    public abstract boolean get_diagnostics_mode();
    public abstract void set_diagnostics_mode_string_max(int len);
    public abstract int get_diagnostics_mode_string_max();
    public abstract long getPos();
    
    
    public String toString()
    {
	return super.toString()+ " = "+getClass().getName()+" {\n"+
	    "msg_type="+msg_type+";\n"+
	    "msg_to_update="+msg_to_update+";\n"+
	    "stat_msg_updated="+stat_msg_updated+";\n"+
	    "cmd_msg_updated="+cmd_msg_updated+";\n"+
	    "error_in_update="+error_in_update+";\n}";
    }

}
