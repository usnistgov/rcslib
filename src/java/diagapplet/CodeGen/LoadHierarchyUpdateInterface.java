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


/**
 * An interface used to update a graphical progress bar and status text with information
 * on the progress loading a hierarchy, configuration file etc.
 * @author Will Shackleford
 */
public interface LoadHierarchyUpdateInterface
{
    /**
     * Update the progress bar that task name has completed part of total.
     * @param name name of file to be loaded/task to be completed
     * @param part lines of the file read or number of subparts of task completed
     * @param total total number of lines in file to real or total subtasks to be accomplished.
     */
    public void update_main(String name, int part, int total);
    
    /**
     * Update a second  progress bar with details of a subtask.
     * @param name name of file to be loaded/task to be completed
     * @param part lines of the file read or number of subparts of task completed
     * @param total total number of lines in file to real or total subtasks to be accomplished.
     */
    public void update_internal(String name, int part, int total);
    
    
    /**
     * Send notice that the task is completed/file read etc.
     */
    public void done();
}
