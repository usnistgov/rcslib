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
import java.io.FileOutputStream;

/**
 *
 * 
 */
/**
 * Class that allows the writing of packed files directly without using
 * an NMLConnection.
 *
 * @author Will Shackleford
 */
public class PackedFileWriter {

	private PackedFormatConverter converter = null;

	public PackedFileWriter(NMLMessageDictionary _dict, boolean _l64_mode) {
		converter = new PackedFormatConverter(_l64_mode);
		converter.SetMessageDictionary(_dict);
	}

	public void WriteFile(final String fileName, NMLmsg _msg) {
		WriteFile(new File(fileName), _msg);
	}

	public void WriteFile(final File curFile, NMLmsg _msg) {
		FileOutputStream fos = null;
		byte raw_data[] = null;
		try {
			int raw_data_estimated_size = (int) converter.GetMessageDictionary().getEstimatedSize(_msg.type);
			raw_data = new byte[raw_data_estimated_size];
			converter.set_do_not_print_diag_overflow_err(true);
			int len = converter.convertMsgToRawData(raw_data, raw_data.length, _msg);
			while(len == -1 && converter.get_raw_data_size() > raw_data_estimated_size)
			{
				raw_data_estimated_size = converter.get_raw_data_size();
				System.out.println("raw_data_estimated_size = " + raw_data_estimated_size);
				raw_data = new byte[raw_data_estimated_size];
				converter.set_do_not_print_diag_overflow_err(true);
				len = converter.convertMsgToRawData(raw_data, raw_data.length, _msg);
			}
			fos = new FileOutputStream(curFile);
			fos.write(raw_data,0,len);
			fos.close();
		} catch (Exception except) {
			except.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
			rcs.nml.debugInfo.debugPrintStream.println("Can't WriteFile(" + curFile + ")");
		} finally {
			if (null != fos) {
				try {
					fos.close();
				} catch (Exception ex2) {
					ex2.printStackTrace();
				}
				fos = null;
			}
			raw_data = null;
		}
	}

	public NMLmsg convertStringToMsg(final String str) {
		return converter.convertStringToMsg(str);
	}
}
