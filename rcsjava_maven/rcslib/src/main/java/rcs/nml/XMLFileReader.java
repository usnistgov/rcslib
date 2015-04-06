/*
 * The NIST RCS (Real-time Control Systems) 
 *  library is public domain software, however it is preferred
 *  that the following disclaimers be attached.
 * 
 * Software Copywrite/Warranty Disclaimer
 * 
 *    This software was developed at the National Institute of Standards and
 * Technology by employees of the Federal Government in the course of their
 * official duties. Pursuant to title 17 Section 105 of the United States
 * Code this software is not subject to copyright protection and is in the
 * public domain. NIST Real-Time Control System software is an experimental
 * system. NIST assumes no responsibility whatsoever for its use by other
 * parties, and makes no guarantees, expressed or implied, about its
 * quality, reliability, or any other characteristic. We would appreciate
 * acknowledgement if the software is used. This software can be
 * redistributed and/or modified freely provided that any derivative works
 * bear some notice that they are derived from it, and any modified
 * versions bear some notice that they have been modified.
 * 
 */
package rcs.nml;

import java.io.File;
import java.io.FileInputStream;

/**
 *
 * @author Will Shackleford 
 */
public class XMLFileReader {

    private XMLFormatConverter converter = null;

    public void set_add_array_indexes_to_name(boolean _add_array_indexes_to_name) {
	converter.add_array_indexes_to_name = _add_array_indexes_to_name;
    }

    public XMLFileReader(NMLMessageDictionary _dict) {
	converter = new XMLFormatConverter();
	converter.SetMessageDictionary(_dict);
    }

    public NMLmsg ReadFile(final String fileName) {
	return ReadFile(new File(fileName));
    }

    public NMLmsg ReadFile(final File curFile) {
	FileInputStream fis = null;
	byte raw_data[] = null;
	NMLmsg tmpMsg = null;
	try {
	    fis = new FileInputStream(curFile);
	    raw_data = new byte[fis.available()];
	    fis.read(raw_data);
	    fis.close();
	    fis = null;
	    tmpMsg = converter.convertRawDataToMsg(raw_data, 0, raw_data.length);
	} catch (Exception except) {
	    except.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    rcs.nml.debugInfo.debugPrintStream.println("Can't ReadFile(" + curFile + ")");
	} finally {
	    if (null != fis) {
		try {
		    fis.close();
		} catch (Exception ex2) {
		    ex2.printStackTrace();
		}
		fis = null;
	    }
	    raw_data = null;
	}
	return tmpMsg;
    }

//    public NMLmsg ReadFile(final File curFile, int max_len)
//    {
//	FileInputStream fis = null;
//	byte raw_data[] = null;
//	NMLmsg tmpMsg = null;
//	try
//	{
//            boolean hide_errors_orig = converter.hide_errors;
//	    fis = new FileInputStream(curFile);
//            int len = fis.available();
//            if(len > max_len)
//            {
//                converter.hide_errors = true;
//                len = max_len;
//            }
//	    raw_data = new byte[len];
//	    fis.read(raw_data);
//	    fis.close();
//	    fis=null;
//	    tmpMsg = converter.convertRawDataToMsg(raw_data, 0, raw_data.length);
//            converter.hide_errors = hide_errors_orig;
//	}
//	catch(Exception except)
//	{
//	    except.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
//            rcs.nml.debugInfo.debugPrintStream.println("Can't ReadFile("+curFile+")");
//	}
//	finally
//	{
//	    if(null != fis)
//	    {
//		try
//		{
//		    fis.close();
//		}
//		catch(Exception ex2)
//		{
//		    ex2.printStackTrace();
//		}
//		fis = null;
//	    }
//	    raw_data = null;
//	}	
//	return tmpMsg;
//    }
    public String convertMsgToString(final NMLmsg inMsg) {
	return converter.convertMsgToString(inMsg);
    }
}
