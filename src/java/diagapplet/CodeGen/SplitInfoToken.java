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

package diagapplet.CodeGen;

class SplitInfoToken
{
    public String orig_info_token=null;
    public String cpp_type=null;
    public String variable_name=null;
    public String array_suffix=null;
    public int num_dims=0;
    public int dims[]=null;
    public int dims_mult=1;
    public int lastSpaceIndex = -1;
    public int cpp_type_length=0;
    public boolean ndla=false;
    public boolean nula=false;
    public boolean enum_flag=false;
    public boolean class_flag=false;
    public static final  String ndla_string = "NML_DYNAMIC_LENGTH_ARRAY";
    public static final  String nula_string = "DECLARE_NML_UNBOUNDED_ARRAY";

    public SplitInfoToken(String info_token)
    {
	try
	    {
		orig_info_token = info_token;
		info_token = info_token.trim();
		int nml_dynamic_length_array_index = info_token.indexOf(ndla_string);
		if(nml_dynamic_length_array_index >= 0)
		    {
			ndla=true;
			info_token = info_token.substring(nml_dynamic_length_array_index+ndla_string.length());
		    }
                int nml_unbounded_length_array_index = info_token.indexOf(nula_string);
		if(nml_unbounded_length_array_index >= 0)
		    {
			nula=true;
			info_token = info_token.substring(nml_dynamic_length_array_index+nula_string.length());
		    }
		cpp_type = null;
		while(true)
		    {
			if(info_token.charAt(0) != ' ' &&
			   info_token.charAt(0) != '\t' &&
			   info_token.charAt(0) != '\r' &&
			   info_token.charAt(0) != '\n')
			    {
				break;
			    }
			if(info_token.length() < 2)
			    {
				break;
			    }
			info_token = info_token.substring(1);
		    }
		if(info_token.length() < 2)
		    {
			return;
		    }
		int l_squareParamIndex=-1;
		int r_squareParamIndex=-1;
		l_squareParamIndex = info_token.indexOf('[');
		if(l_squareParamIndex > 0)
		    {
			array_suffix = info_token.substring(l_squareParamIndex);
			info_token = info_token.substring(0, l_squareParamIndex);
			num_dims=0;
			dims_mult=1;
			l_squareParamIndex = array_suffix.indexOf('[');
			r_squareParamIndex = array_suffix.indexOf(']');
			while(l_squareParamIndex >= 0 && r_squareParamIndex > l_squareParamIndex+1)
			    {
				l_squareParamIndex = array_suffix.indexOf('[',r_squareParamIndex);
				r_squareParamIndex = array_suffix.indexOf(']',r_squareParamIndex+1);
				num_dims++;
			    }
			if(num_dims > 0)
			    {
				dims= new int[num_dims];
			    }
			l_squareParamIndex = array_suffix.indexOf('[');
			r_squareParamIndex = array_suffix.indexOf(']');
			int i = 0;
			while(l_squareParamIndex >= 0 && r_squareParamIndex > l_squareParamIndex+1)
			    {
				String dim_str = array_suffix.substring(l_squareParamIndex+1,r_squareParamIndex);
				dims[i] = rcs.utils.StrToInt.convert(dim_str);
				dims_mult *= dims[i];
				i++;
				l_squareParamIndex = array_suffix.indexOf('[',r_squareParamIndex);
				r_squareParamIndex = array_suffix.indexOf(']',r_squareParamIndex+1);
			    }
		    }
		while(true)
		    {
			char last_char = info_token.charAt(info_token.length()-1);
			if(last_char  != ' ' &&
			   last_char != '\t' &&
			   last_char != '\r' &&
			   last_char != '\n')
			    {
				break;
			    }
			if(info_token.length() < 2)
			    {
				break;
			    }
			info_token = info_token.substring(0, info_token.length()-1);
		    }
		if(info_token.length() < 2)
		    {
			System.err.println("Invalid variable definition ("+orig_info_token+")");
			System.err.println("\t\t-- info_token ("+info_token+") should be longer.");
			return;
		    }
		lastSpaceIndex = info_token.lastIndexOf(' ');
		if(lastSpaceIndex < 0 || lastSpaceIndex >= (info_token.length()-1))
		    {
			System.err.println("Invalid variable definition ("+orig_info_token+")");
			System.err.println("\t\t-- info_token ("+info_token+") needs a space. *6");
			return;
		    }
		variable_name = info_token.substring(lastSpaceIndex+1);
		cpp_type = info_token.substring(0,lastSpaceIndex);
		while(true)
		    {
			if(cpp_type.charAt(0) != ' ' &&
			   cpp_type.charAt(0) != '\t' &&
			   cpp_type.charAt(0) != '\r' &&
			   cpp_type.charAt(0) != '\n')
			    {
				break;
			    }
			if(cpp_type.length() < 2)
			    {
				break;
			    }
			cpp_type = cpp_type.substring(1);
		    }
		cpp_type_length = cpp_type.length();
		while(true)
		    {
			if(cpp_type.charAt(cpp_type_length-1) != ' ' &&
			   cpp_type.charAt(cpp_type_length-1) != '\t' &&
			   cpp_type.charAt(cpp_type_length-1) != '\r' &&
			   cpp_type.charAt(cpp_type_length-1) != '\n')
			    {
				break;
			    }
			if(cpp_type.length() < 2)
			    {
				break;
			    }
			cpp_type = cpp_type.substring(0,cpp_type_length-1);
			cpp_type_length = cpp_type.length();
		    }
		if(cpp_type.indexOf('*') >= 0 ||
		   cpp_type.indexOf('&') >=0 )
		    {
			System.err.println("Invalid variable definition ("+orig_info_token+")");
			System.err.println("\t\t-- cpp_type ("+cpp_type+") appears to be a pointer or reference.");
			return;
		    }
		if(cpp_type.indexOf('*') >= 0 ||
		   cpp_type.indexOf('?') >= 0  ||
		   cpp_type.indexOf('-') >= 0 ||
		   cpp_type.indexOf('\\') >= 0 ||
		   cpp_type.indexOf('/') >= 0 ||
		   cpp_type.indexOf('+') >= 0 ||
		   cpp_type.indexOf('=') >= 0 ||
		   cpp_type.indexOf('<') >= 0 || cpp_type.indexOf('>') >= 0 ||
		   cpp_type.indexOf('[') >= 0 ||  cpp_type.indexOf(']') >= 0 ||
		   cpp_type.indexOf('(') >= 0 ||  cpp_type.indexOf(')') >= 0 ||
		   cpp_type.indexOf('{') >= 0 ||  cpp_type.indexOf('}') >= 0 ||
		   cpp_type.indexOf(',') >= 0 ||
		   cpp_type.indexOf('&') >= 0)
		    {
			System.err.println("Invalid variable definition ("+orig_info_token+")");
			System.err.println("\t\t-- cpp_type ("+cpp_type+") contains illegal character.");

			return;
		    }
		if(cpp_type.startsWith(ndla_string))
		    {
			ndla=true;
			cpp_type=cpp_type.substring(ndla_string.length());
			while(true)
			    {
				if(cpp_type.charAt(0) != ' ' &&
				   cpp_type.charAt(0) != '\t' &&
				   cpp_type.charAt(0) != '\r' &&
				   cpp_type.charAt(0) != '\n')
				    {
					break;
				    }
				if(cpp_type.length() < 2)
				    {
					break;
				    }
				cpp_type = cpp_type.substring(1);
			    }
		    }
		if(cpp_type.startsWith("enum "))
		    {
			enum_flag=true;
			cpp_type=cpp_type.substring(5);			
			while(true)
			    {
				if(cpp_type.charAt(0) != ' ' &&
				   cpp_type.charAt(0) != '\t' &&
				   cpp_type.charAt(0) != '\r' &&
				   cpp_type.charAt(0) != '\n')
				    {
					break;
				    }
				if(cpp_type.length() < 2)
				    {
					break;
				    }
				cpp_type = cpp_type.substring(1);
			    }
		    }
		if(cpp_type.startsWith("class "))
		    {
			class_flag=true;
			cpp_type=cpp_type.substring(6);			
			while(true)
			    {
				if(cpp_type.charAt(0) != ' ' &&
				   cpp_type.charAt(0) != '\t' &&
				   cpp_type.charAt(0) != '\r' &&
				   cpp_type.charAt(0) != '\n')
				    {
					break;
				    }
				if(cpp_type.length() < 2)
				    {
					break;
				    }
				cpp_type = cpp_type.substring(1);
			    }
		    }
		if(cpp_type.startsWith("struct "))
		    {
			class_flag=true;
			cpp_type=cpp_type.substring(7);			
			while(true)
			    {
				if(cpp_type.charAt(0) != ' ' &&
				   cpp_type.charAt(0) != '\t' &&
				   cpp_type.charAt(0) != '\r' &&
				   cpp_type.charAt(0) != '\n')
				    {
					break;
				    }
				if(cpp_type.length() < 2)
				    {
					break;
				    }
				cpp_type = cpp_type.substring(1);
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }
}
