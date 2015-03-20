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

/*
 *       New Java File starts here.
 *       This file should be named errlogMsgDict.java
 */
package rcs.nml;

/**
 * Implements an NML message dictionary for a set of messages
 * usually used for logging errors and/or communication
 * with a generic operator interface.
 */
public class errlogMsgDict implements NMLMessageDictionary {

    public long getEstimatedSize(int _type) {
        return 0;
    }

    public long getMaxEstimatedSize() {
        return 0;
    }  
    
    
    // Define an object of every message class.
    NML_DISPLAY NML_DISPLAY_object = new NML_DISPLAY();
    NML_ERROR NML_ERROR_object = new NML_ERROR();
    NML_TEXT NML_TEXT_object = new NML_TEXT();    // ID Type Constants
    public static final int NML_DISPLAY_TYPE = 3;
    public static final int NML_ERROR_TYPE = 1;
    public static final int NML_TEXT_TYPE = 2;
    // NML Format Function
    public int formatMsg(NMLFormatConverter nml_fc) {
        switch (nml_fc.msg_type) {
            case NML_DISPLAY_TYPE:
                nml_fc.msg_to_update = NML_DISPLAY_object;
                NML_DISPLAY_object.update(nml_fc);
                break;
            case NML_ERROR_TYPE:
                nml_fc.msg_to_update = NML_ERROR_object;
                NML_ERROR_object.update(nml_fc);
                break;
            case NML_TEXT_TYPE:
                nml_fc.msg_to_update = NML_TEXT_object;
                NML_TEXT_object.update(nml_fc);
                break;
            default:
                return (-1);
        }
        return (0);
    }
}
