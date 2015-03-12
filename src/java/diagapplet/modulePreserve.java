/*
 * modulePreserve.java
 *
 * Created on January 15, 2007, 6:20 PM
 *
 *
 * The NIST RCS (Real-time Control Systems)
 * library is public domain software, however it is preferred
 * that the following disclaimers be attached.
 *
 * Software Copywrite/Warranty Disclaimer
 *
 *   This software was developed at the National Institute of Standards and
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
package diagapplet;

import java.util.Hashtable;

/**
 * Stores all of the information related to one module in the hierarchy
 * kept between sessions by the diagnostics tool.
 * Everything is accessed though getter/setter functions to be compatible with
 * XMLDecoder/XMLEncoder as the files are saved in XML
 * @author shackle
 */
public class modulePreserve {

    /** Creates a new instance of modulePreserve */
    public modulePreserve() {
    }
    /**
     * Holds value of property name.
     */
    private String name;

    /**
     * Getter for property name.
     * @return Value of property name.
     */
    public String getName() {
        return this.name;
    }

    /**
     * Setter for property name.
     * @param name New value of property name.
     */
    public void setName(String name) {
        this.name = name;
    }
    /**
     * Holds value of property PreviousCommands.
     */
    private Hashtable<Long, String> PreviousCommands;

    /**
     * Getter for property PreviousCommands.
     * @return Value of property PreviousCommands.
     */
    public Hashtable<Long, String> getPreviousCommands() {
        return this.PreviousCommands;
    }

    /**
     * Setter for property PreviousCommands.
     * @param _PreviousCommands New value of property PreviousCommands.
     */
    public void setPreviousCommands(Hashtable<Long, String> _PreviousCommands) {
        this.PreviousCommands = new Hashtable<Long, String>();
        for (Long key : _PreviousCommands.keySet()) {
            String s = _PreviousCommands.get(key);
            this.PreviousCommands.put(key, s);
        }
    }
    /**
     * Holds value of property lastSelectedCommandIndex.
     */
    private int lastSelectedCommandIndex;

    /**
     * Getter for property lastSelectedCommandIndex.
     * @return Value of property lastSelectedCommandIndex.
     */
    public int getLastSelectedCommandIndex() {
        return this.lastSelectedCommandIndex;
    }

    /**
     * Setter for property lastSelectedCommandIndex.
     * @param lastSelectedCommandIndex New value of property lastSelectedCommandIndex.
     */
    public void setLastSelectedCommandIndex(int lastSelectedCommandIndex) {
        this.lastSelectedCommandIndex = lastSelectedCommandIndex;
    }
}
