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

/**
* The NMLMessageDictionary interface is used to create
* custom message sets for NML. It's primary function is
* to match an integer type identifier with a particular
* message class and update the class.
*
* @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
*
*/
public interface NMLMessageDictionary
{
        /**
        * This function should use NMLfc.type to select an
        * object of the appropriate class, set NMLfc.msg_to_update
        * to it, and call the objects update(NMLFormatConverter) function.
        *
        * @param  NMLfc a format converter object which provides methods
        *                 for updating all the basic types and is used to update
        *                 the message, member-by-member.
        *
        * @return The function should return 0 if it is successful, -1
        *                       otherwise.
        */
        int formatMsg(rcs.nml.NMLFormatConverter NMLfc);

        long getEstimatedSize(int _type);
        long getMaxEstimatedSize();
        
}
