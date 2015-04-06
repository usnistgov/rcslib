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

import java.util.Hashtable;


/**
 * Interface to the ModuleInfo class, used to reduce circlular dependancies.
 * 
 * @author Will Shackleford
 */
public interface ModuleInfoInterface
{
    /**
     * Was the module created from a C++ header file? (As opposed to a module section of a 
     * hierarchy file.)
     * @return created_from_header
     */
    public boolean get_created_from_header();
    
    /**
     * Get the name property.
     * @return name
     */
    public String getName();
    
    /**
     * Get the name of the C++ format function.
     * @return format function name
     */
    public String getFormatFunction();
    
    /**
     * Get the name of the C++ format function.
     * @return format function name
     */
    public String getSymbolLookup();
    
    /**
     * Get the list of messages that can be sent or recieved to auxilliary buffers associated
     * with this module.
     * @return AuxMessages vector.
     */
    public java.util.Vector getAuxMessages();

    /**
     * Get the list of filters of the form buffername:pattern to select available messages for a buffer.
     * @return AuxMessages vector.
     */
    public java.util.Vector getAuxAvailableMessageFilters();

    public void AddAllAuxMessages();
    
    /**
     * Get the NMLConnectionCreator associated with this module.
     * @return nml_creator
     */
    public rcs.nml.NMLConnectionCreatorInterface get_nml_creator();
    
    
    /**
     * Get a Hashtable that matches long identifiers to StructureTypeInfo objects
     * used to resolve conflicts when more than one  StructureTypeInfo is associated with
     * the same id in the global hashtable.
     * @return conflict_m_structInfoHashTable
     */
    public Hashtable get_conflict_m_structInfoHashTable();

}
