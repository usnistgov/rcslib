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

import java.util.StringTokenizer;
import java.io.IOException;

/**
* The DISPFormatConverter converts NML message classes to simple
* text strings. Most users should not use it directly.
*
* <pre>
* Related Documentation:
* <A HREF="http://isd.cme.nist.gov/proj/rcs_lib">RCS Library</a>, <A HREF="http://isd.cme.nist.gov/proj/rcs_lib/NMLjava.html">NML Programmers Guide (Java Version)</a>
*
*
* </pre>
*
* @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
*/
class DISPFormatConverter extends NMLFormatConverterBase
{
  protected byte temp_input_bytes[] = null;
  String token;

  public String toString()
  {
    return super.toString()+ " = "+getClass().getName()+" {\n"+
      "temp_input_bytes="+temp_input_bytes+";\n"+
      "token="+token+";\n}";
  }

  protected void write_string_without_zero_end(String str)  throws IOException
  {
    if(debug_on)
      {
        rcs.nml.debugInfo.debugPrintStream.println("write_string_without_zero_end("+str+") called.");
      }
    if(null != output_stream)
	{
	    byte b[] = str.getBytes();
	    output_stream.write(b);
	}
  }

  protected long improved_parse_long(String str)
  {
    try
      {
        while(str.length() > 1 &&
              (str.charAt(0) == ' ' || str.charAt(0) == '\t' || str.charAt(0) == '+')
              )
          {
            str = str.substring(1);
          }
        if(str.length() < 1)
          {
            throw new Exception();
          }
        Long L = Long.valueOf(str);
        return L.longValue();
      }
    catch(Exception e)
      {
        System.err.println("Can't parse "+str +" in "+input_string);
        e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
      }
    return 0;
  }

  protected int improved_parse_int(String str)
  {
    try
      {
        while(str.length() > 1 &&
              (str.charAt(0) == ' ' || str.charAt(0) == '\t' || str.charAt(0) == '+')
              )
          {
            str = str.substring(1);
          }
        if(str.length() < 1)
          {
            throw new Exception();
          }
	if(updating_unsigned)
	    {
		long ltmp = Long.parseLong(str);
		return (int) ltmp;
	    }
        Integer I = Integer.valueOf(str);
        return I.intValue();
      }
    catch(Exception e)
      {
        if(debug_on)
          {
            rcs.nml.debugInfo.debugPrintStream.println("token = "+token);
            rcs.nml.debugInfo.debugPrintStream.println("tokens_taken = "+tokens_taken);
            rcs.nml.debugInfo.debugPrintStream.println("tokens left = "+input_string_tokenizer.countTokens());
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
          }
        else
          {
            if(!error_in_update)
              {
                SetErrorInUpdate("Can't parse "+str +" in "+input_string);
                e.printStackTrace();
              }
          }
      }
    return 0;
  }

  private int tokens_taken = 0;
  protected void make_it_use_string()
  {
    try
      {
        if(!use_string)
          {
            tokens_taken = 0;
            if(decoding)
              {
                if(bytes_in_input_stream_known && bytes_in_input_stream > 0)
                  {
                    temp_input_bytes = new byte[bytes_in_input_stream];
                    if(debug_on)
                      {
                        rcs.nml.debugInfo.debugPrintStream.println("DISPFormatConverter.make_it_use_string() reading "+bytes_in_input_stream+" bytes from input_stream.");
                      }
                    input_stream.readFully(temp_input_bytes, 0, bytes_in_input_stream);
                    bytes_in_input_stream = 0;
                    input_string = new String(temp_input_bytes);
                  }
                else
                  {
                    temp_input_bytes = new byte[2048];
                    input_string = "";
                    if(debug_on)
                      {
                        rcs.nml.debugInfo.debugPrintStream.println("DISPFormatConverter.make_it_use_string() reading ? bytes from input_stream.");
                      }
                    while(input_stream.read(temp_input_bytes,0,2048) > 0)
                      {
                        input_string += new String(temp_input_bytes);
                      }
                  }
                if(debug_on)
                  {
                    rcs.nml.debugInfo.debugPrintStream.println("Checking for double commas");
                    rcs.nml.debugInfo.debugPrintStream.println(input_string);
                  }
                int dc_index = input_string.indexOf(",,");
                while(dc_index > 0)
                  {
                    if(debug_on)
                      {
                        rcs.nml.debugInfo.debugPrintStream.println("Breaking String");
                        rcs.nml.debugInfo.debugPrintStream.println(input_string.substring(0,dc_index));
                        rcs.nml.debugInfo.debugPrintStream.println(input_string.substring(dc_index+2));
                      }
                    input_string = input_string.substring(0,dc_index)+",(null),"+input_string.substring(dc_index+2);
                    if(debug_on)
                      {
                        rcs.nml.debugInfo.debugPrintStream.println(input_string);
                      }
                    dc_index = input_string.indexOf(",,");
                  }
                input_string_tokenizer = new StringTokenizer(input_string,",");
              }
          }
        if(!decoding)
          {
            if(null == output_string_buffer)
              {
                output_string_buffer = new StringBuffer();
              }
          }
      }
    catch(Exception e)
      {
        if(debug_on)
          {
            rcs.nml.debugInfo.debugPrintStream.println("token = "+token);
            rcs.nml.debugInfo.debugPrintStream.println("tokens_taken = "+tokens_taken);
            rcs.nml.debugInfo.debugPrintStream.println("tokens left = "+input_string_tokenizer.countTokens());
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
          }
        else
          {
            if(!error_in_update)
              {
                e.printStackTrace();
		SetErrorInUpdate(e.toString());
              }
          }
      }
    use_string = true;
    if(decoding)
      {
        if(debug_on)
          {
            rcs.nml.debugInfo.debugPrintStream.println(input_string);
            rcs.nml.debugInfo.debugPrintStream.println("token = "+token);
            rcs.nml.debugInfo.debugPrintStream.println("tokens_taken = "+tokens_taken);
            rcs.nml.debugInfo.debugPrintStream.println("tokens left = "+input_string_tokenizer.countTokens());
          }
        tokens_taken++;
      }
    else
      {
        if(debug_on)
          {
            rcs.nml.debugInfo.debugPrintStream.println("output_string = "+output_string_buffer.toString());
          }
      }

  }

  public byte update(byte x)
  {
    try
      {
	 if(debug_on)
	     {
		 rcs.nml.debugInfo.debugPrintStream.println("x="+x+",decoding="+decoding+",output_string="+output_string_buffer.toString());
	     }
        if(error_in_update)
          {
            return x;
          }
        make_it_use_string();
        if(decoding)
          {
            token = input_string_tokenizer.nextToken();
	    if(debug_on)
		{
		    rcs.nml.debugInfo.debugPrintStream.println("token="+token);
		}
            if(token != null)
              {
                if(token.charAt(0) == '\\')
                  {
                    x = (byte) Integer.valueOf(token.substring(1)).intValue();
                  }
                else
                  {
                    x = (byte) token.charAt(0);
                  }
              }
          }
        else
          {
	      int xi = (int)x;
	      if(xi < 0)
		  {
		      xi+=256;
		  }
	      String ts = "\\"+Integer.toString(xi)+",";
	      if(x >= 0 && x < 10)
		  {
		      ts = "\\00"+Integer.toString(xi)+",";
		  }
	      else if(x >= 0 && x < 100)
		  {
		      ts = "\\0"+Integer.toString(xi)+",";
		  }
	      
	      write_string_without_zero_end(ts);
	      if(null != output_string_buffer)
		  {
		      output_string_buffer.append(ts);
		      raw_data_size = output_string_buffer.length();
		  }
          }
	if(debug_on)
	    {
		rcs.nml.debugInfo.debugPrintStream.println("x="+x+",decoding="+decoding+",output_string="+output_string_buffer.toString());
	    }
      }
    catch(Exception e)
      {
        if(debug_on)
          {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            rcs.nml.debugInfo.debugPrintStream.println("token = "+token);
            rcs.nml.debugInfo.debugPrintStream.println("tokens_taken = "+tokens_taken);
	    if(input_string_tokenizer != null)
		{
		    rcs.nml.debugInfo.debugPrintStream.println("tokens left = "+input_string_tokenizer.countTokens());
		}
          }
        else
          {
            if(!error_in_update)
              {
                e.printStackTrace();
		SetErrorInUpdate(e.toString());
              }
          }
      }
    return x;
  }


  public void update(byte x[],int num_elements)
  {
    int i;
    try
      {
        if(error_in_update)
          {
            return;
          }
        make_it_use_string();
        if(decoding)
          {
            token = input_string_tokenizer.nextToken();
	    if(debug_on)
		{
		    rcs.nml.debugInfo.debugPrintStream.println("token="+token);
		}
            if(token != null)
              {
                int bytes_to_get  = token.length();
                if(bytes_to_get >=num_elements)
                  {
                    bytes_to_get = num_elements;
                  }
                byte temp_bytes[] = token.getBytes();
                for(i = 0; i < temp_bytes.length && i < x.length; i++)
                  {
                    x[i] = temp_bytes[i];
                  }
                if(bytes_to_get < num_elements)
                  {
                    x[bytes_to_get] = 0;
                  }
              }
          }
        else
          {
            for(i = 0; i < num_elements && i < x.length; i++)
              {
                if(x[i] == 0)
                  {
                    break;
                  }
              }
            if(i > 0)
              {
                if(debug_on)
                  {
                    rcs.nml.debugInfo.debugPrintStream.println("updating string: length ="+i);
                  }
                token = new String(x,0,i);
                if(debug_on)
                  {
                    rcs.nml.debugInfo.debugPrintStream.println("updating string ="+token);
                  }
                output_string_buffer.append(token);
		output_string_buffer.append(',');
                if(null != output_stream)
                  {
                    write_string_without_zero_end(token+",");
                    raw_data_size = output_string_buffer.length();
                  }
              }
            else
              {
		  if(debug_on)
		      {
			  rcs.nml.debugInfo.debugPrintStream.println("empty string updating string ="+token+",output_string="+output_string_buffer.toString());
		      }
                output_string_buffer.append(',');
                if(null != output_stream)
                  {
                    write_string_without_zero_end(",");
                    raw_data_size = output_string_buffer.length();
                  }
              }
          }
      }
    catch(Exception e)
      {
        if(debug_on)
          {
            rcs.nml.debugInfo.debugPrintStream.println("token = "+token);
            rcs.nml.debugInfo.debugPrintStream.println("tokens_taken = "+tokens_taken);
              rcs.nml.debugInfo.debugPrintStream.println("tokens left = "+input_string_tokenizer.countTokens());
              e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
          }
        else
          {
            if(!error_in_update)
              {
                e.printStackTrace();
		SetErrorInUpdate(e.toString());
              }
          }
      }
  }

  public char update(char x)
  {
    try
      {
        if(error_in_update)
          {
            return x;
          }
        make_it_use_string();
        if(decoding)
          {
            token = input_string_tokenizer.nextToken();
            if(token != null)
              {
		  if(token.charAt(0) == '\\')
                  {
                    x = (char) Integer.valueOf(token.substring(1)).intValue();
                  }
                else
                  {
		      char ch_array[] = new char[1];
		      token.getChars(0,1,ch_array,0);
		      x = ch_array[0];
		  }
              }
          }
        else
          {
	      String stemp= "\\0"+Integer.toString(x)+",";
	      output_string_buffer.append(stemp);
	      if(null != output_stream)
		  {
		      write_string_without_zero_end(stemp);
		      raw_data_size = output_string_buffer.length();
		  }
          }
      }
    catch(Exception e)
      {
        if(debug_on)
          {
              rcs.nml.debugInfo.debugPrintStream.println("token = "+token);
              rcs.nml.debugInfo.debugPrintStream.println("tokens_taken = "+tokens_taken);
              rcs.nml.debugInfo.debugPrintStream.println("tokens left = "+input_string_tokenizer.countTokens());
              e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
          }
        else
          {
            if(!error_in_update)
              {
                e.printStackTrace();
		SetErrorInUpdate(e.toString());
              }
          }
      }
    return x;
  }


  public void update(char x[],int num_elements)
  {
    int i;
    try
      {
        if(error_in_update)
          {
            return;
          }
        make_it_use_string();
        if(decoding)
          {
            token = input_string_tokenizer.nextToken();
            if(token != null)
              {
                int chars_to_get  = token.length();
                if(chars_to_get >= num_elements)
                  {
                    chars_to_get = num_elements;
                  }
                token.getChars(0, chars_to_get,x,0);
              }
          }
        else
          {
            for(i = 0; i < num_elements; i++)
              {
                if(x[i] == 0)
                  {
                    break;
                  }
              }
            token = new String(x,0,i);
            output_string_buffer.append(token);
	    output_string_buffer.append(',');
            if(null != output_stream)
              {
                write_string_without_zero_end(token+",");
                raw_data_size = output_string_buffer.length();
              }
          }
      }
    catch(Exception e)
      {
        if(debug_on)
          {
              rcs.nml.debugInfo.debugPrintStream.println("token = "+token);
              rcs.nml.debugInfo.debugPrintStream.println("tokens_taken = "+tokens_taken);
              rcs.nml.debugInfo.debugPrintStream.println("tokens left = "+input_string_tokenizer.countTokens());
              e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
          }
        else
          {
            if(!error_in_update)
              {
                e.printStackTrace();
		SetErrorInUpdate(e.toString());
              }
          }
      }
  }

  public short update(short x)
  {
    try
      {
        if(error_in_update)
          {
            return x;
          }
        make_it_use_string();
        if(decoding)
          {
            token = input_string_tokenizer.nextToken();
            if(token != null)
              {
                x = (short) improved_parse_int(token);
              }
          }
        else
          {
            output_string_buffer.append(x);
	    output_string_buffer.append(',');
            if(null != output_stream)
              {
                write_string_without_zero_end(x+",");
                raw_data_size = output_string_buffer.length();
              }
          }
      }
    catch(Exception e)
      {
        if(debug_on)
          {
              rcs.nml.debugInfo.debugPrintStream.println("token = "+token);
              rcs.nml.debugInfo.debugPrintStream.println("tokens_taken = "+tokens_taken);
              rcs.nml.debugInfo.debugPrintStream.println("tokens left = "+input_string_tokenizer.countTokens());
              e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
          }
        else
          {
            if(!error_in_update)
              {
                e.printStackTrace();
		SetErrorInUpdate(e.toString());                
              }
          }
      }
    return x;
  }

  public void update(short x[],int num_elements)
  {
    int i;
    for(i = 0; i < num_elements; i++)
      {
        x[i] = update(x[i]);
      }
  }

  public int update(int x)
  {
    try
      {
        if(error_in_update)
          {
            return x;
          }
        make_it_use_string();
        if(decoding)
          {
            token = input_string_tokenizer.nextToken();
            if(token != null)
              {
                x = improved_parse_int(token);
              }
          }
        else
          {
            output_string_buffer.append(x);
	    output_string_buffer.append(',');
            if(null != output_stream)
              {
                write_string_without_zero_end(x+",");
                raw_data_size = output_string_buffer.length();
              }
          }
      }
    catch(Exception e)
      {
        if(debug_on)
          {
              rcs.nml.debugInfo.debugPrintStream.println("token = "+token);
              rcs.nml.debugInfo.debugPrintStream.println("tokens_taken = "+tokens_taken);
              rcs.nml.debugInfo.debugPrintStream.println("tokens left = "+input_string_tokenizer.countTokens());
              e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
          }
        else
          {
            if(!error_in_update)
              {
                e.printStackTrace();
		SetErrorInUpdate(e.toString());                
              }
          }
      }
    return x;
  }

  public void update(int x[],int num_elements)
  {
    int i;
    for(i = 0; i < num_elements; i++)
      {
        /*if(debug_on)
          {
            rcs.nml.debugInfo.debugPrintStream.println("tokens_taken = "+tokens_taken);
            rcs.nml.debugInfo.debugPrintStream.println("tokens left = "+input_string_tokenizer.countTokens());
          }      */
        x[i] = update(x[i]);
      }
  }

  public long update(long x)
  {
    if(error_in_update)
      {
        return x;
      }
    try
      {
        make_it_use_string();
        if(decoding)
          {
            token = input_string_tokenizer.nextToken();
            if(token != null)
              {
                x = improved_parse_long(token);
              }
          }
        else
          {
            output_string_buffer.append(x);
	    output_string_buffer.append(',');
            if(null != output_stream)
              {
                write_string_without_zero_end(x+",");
                raw_data_size = output_string_buffer.length();
              }
          }
      }
    catch(Exception e)
      {
        if(debug_on)
          {
              rcs.nml.debugInfo.debugPrintStream.println("token = "+token);
              rcs.nml.debugInfo.debugPrintStream.println("tokens_taken = "+tokens_taken);
              rcs.nml.debugInfo.debugPrintStream.println("tokens left = "+input_string_tokenizer.countTokens());
              e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
          }
        else
          {
            if(!error_in_update)
              {
                e.printStackTrace();
		SetErrorInUpdate(e.toString());                
              }
          }
      }
    return x;
  }

  public void update(long x[],int num_elements)
  {
    int i;
    for(i = 0; i < num_elements; i++)
      {
        x[i] = update(x[i]);
      }
  }

  public float update(float x)
  {
    try
      {
        if(error_in_update)
          {
            return x;
          }
        make_it_use_string();
        if(decoding)
          {
            token = input_string_tokenizer.nextToken();
            if(token != null)
              {
                x = Float.valueOf(token).floatValue();
              }
          }
        else
          {
            output_string_buffer.append(x);
	    output_string_buffer.append(',');
            if(null != output_stream)
              {
                write_string_without_zero_end(x+",");
                raw_data_size = output_string_buffer.length();
              }
          }
      }
    catch(Exception e)
      {
        if(debug_on)
          {
              rcs.nml.debugInfo.debugPrintStream.println("token = "+token);
              rcs.nml.debugInfo.debugPrintStream.println("tokens_taken = "+tokens_taken);
              rcs.nml.debugInfo.debugPrintStream.println("tokens left = "+input_string_tokenizer.countTokens());
              e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
          }
        else
          {
            if(!error_in_update)
              {
                e.printStackTrace();
		SetErrorInUpdate(e.toString());                
              }
          }
      }
    return x;
  }

  public void update(float x[],int num_elements)
  {
    int i;
    for(i = 0; i < num_elements; i++)
      {
        x[i] = update(x[i]);
      }
  }


  public double update(double x)
  {
    try
      {
        if(error_in_update)
          {
            return x;
          }
        make_it_use_string();
        if(decoding)
          {
            token = input_string_tokenizer.nextToken();
            if(token != null)
              {
                x = Double.valueOf(token).doubleValue();
              }
          }
        else
          {
            output_string_buffer.append(x);
	    output_string_buffer.append(',');
            if(null != output_stream)
              {
                write_string_without_zero_end(x+",");
                raw_data_size = output_string_buffer.length();
              }
          }
      }
    catch(Exception e)
      {
        if(debug_on)
          {
              rcs.nml.debugInfo.debugPrintStream.println("token = "+token);
              rcs.nml.debugInfo.debugPrintStream.println("tokens_taken = "+tokens_taken);
              rcs.nml.debugInfo.debugPrintStream.println("tokens left = "+input_string_tokenizer.countTokens());
              e.printStackTrace(rcs.nml.debugInfo.debugPrintStream );
          }
        else
          {
            if(!error_in_update)
              {
                e.printStackTrace();
		SetErrorInUpdate(e.toString());                
              }
          }
      }
    return x;
  }

  public void update(double x[],int num_elements)
  {
    int i;
    for(i = 0; i < num_elements; i++)
      {
        x[i] = update(x[i]);
      }
  }
}
