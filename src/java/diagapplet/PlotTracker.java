/* 
The NIST RCS (Real-time Control Systems) 
 library is protected  domain software, however it is preferred
 that the following disclaimers be attached.

Software Copywrite/Warranty Disclaimer

   This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
protected  domain. NIST Real-Time Control System software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modifiedS
versions bear some notice that they have been modified.



*/ 

package diagapplet;

import diagapplet.plotter.PlotData;
import diagapplet.CodeGen.ModuleInfo;
import diagapplet.CodeGen.BufferInfo;


/*
 *
 * PlotTracker
 *
 */
 class PlotTracker
{
    protected  String structName=null;
    protected  String varName=null;
    protected  long msg_type;
    protected  ModuleInfo module = null;
    protected  int var_number = 0;
    protected  boolean is_cmd_value = false;
    protected  boolean is_aux_channel = false;
    protected  BufferInfo auxBufferInfo = null;
    protected  String aux_channel_name = null;
    protected  PlotData plot_data;
    protected  boolean  array_type=false;
    protected  int min_var_num=0;
    protected  int max_var_num=0;
    protected  int skip_size=0;
    protected  boolean ndla=false;
    protected  int ndla_length_var_num=0;
    protected  int cur_ndla_len=0;
    protected  int cur_max_var_num=0;
    protected  int array_size;
    protected  boolean using_nml_single_var_log=false;
    protected  int nml_single_var_log_number=-1;
    protected  rcs.nml.NMLConnectionInterface nml_for_get_single_var_log=null;
    protected  double last_x;
    protected  double last_y;
    protected  double mean;
    protected  double stddev;
    protected  double integral;
    protected  double derivmean;
    protected  int point_count;
    protected  int last_compare_index;
    protected  String single_var_log_name=null;
    protected  boolean need_nml_single_var_log_resetup=false;
    protected  boolean cur_ndla_len_var_found=false;
    protected  String vname=null;
    protected  boolean updated=false;
    protected  int last_new_data_count;
    protected  String name;
    protected  long last_reconnect_time_millis=0;
    
    PlotTracker()
    {
	last_reconnect_time_millis = System.currentTimeMillis();
    }
    
    public String toString()
    {
	try
	    {
		String s = super.toString()+"\n";
		s += "\t{\n";
		s += "\tmsg_type="+msg_type+",\n";
		s += "\tvar_number="+var_number+",\n";
		s += "\tis_cmd_value="+is_cmd_value+",\n";
		s += "\tis_aux_channel="+is_aux_channel+",\n";
		s += "\tarray_type="+array_type+",\n";
		s += "\tmin_var_num="+min_var_num+",\n";
		s += "\tmax_var_num="+max_var_num+",\n";
		s += "\tskip_size="+skip_size+",\n";
		s += "\tndla="+ndla+",\n";
		s += "\tndla_length_var_num="+ndla_length_var_num + ",\n";
		s += "\tcur_ndla_len="+cur_ndla_len+",\n";
		s += "\tcur_ndla_len_var_found="+cur_ndla_len_var_found+",\n";
		s += "\tcur_max_var_num="+cur_max_var_num+",\n";
		s += "\tusing_nml_single_var_log="+using_nml_single_var_log+",\n";
		s += "\tnml_single_var_log_number="+ nml_single_var_log_number+",\n";
		s += "\tnml_for_get_single_var_log="+nml_for_get_single_var_log+", single_var_log_name="+single_var_log_name+"\n";
		if(module != null)
		    {
			s+= " module.Name="+module.Name+",";
		    }
		if(plot_data != null)
		    {
			s+= " plot_data.name="+plot_data.name;
		    }
		s += "\n";
		s += "plot_data= {\n\t"+plot_data+"\n},\n";
		s += "\n";
		s += "auxBufferInfo= {\n\t"+auxBufferInfo+"\n}\n";
		s += "\n";
		s +="} ";
		return s;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return "";
    }
}
