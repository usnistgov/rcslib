/*
 * auxBufferPreserve.java
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
 * Stores all of the information related to one auxBuffer in the hierarchy
 * kept between sessions by the diagnostics tool.
 * Everything is accessed though getter/setter functions to be compatible with
 * XMLDecoder/XMLEncoder as the files are saved in XML
 * @author shackle
 */
public class auxBufferPreserve {

    /** Creates a new instance of auxBufferPreserve */
    public auxBufferPreserve() {
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
     * Holds value of property PreviousMessages.
     */
    private Hashtable<Long, String> PreviousMessages;

    /**
     * Getter for property PreviousMessages.
     * @return Value of property PreviousMessages.
     */
    public Hashtable<Long, String> getPreviousMessages() {
        return this.PreviousMessages;
    }

    /**
     * Setter for property PreviousMessages.
     * @param _PreviousMessages New value of property PreviousMessages.
     */
    public void setPreviousMessages(Hashtable<Long, String> _PreviousMessages) {
        this.PreviousMessages = new Hashtable<Long, String>();
        for (Long key : _PreviousMessages.keySet()) {
            String s = _PreviousMessages.get(key);
            this.PreviousMessages.put(key, s);
        }
    }
    /**
     * Holds value of property lastSelectedMessageIndex.
     */
    private int lastSelectedMessageIndex;

    /**
     * Getter for property lastSelectedMessageIndex.
     * @return Value of property lastSelectedMessageIndex.
     */
    public int getLastSelectedMessageIndex() {
        return this.lastSelectedMessageIndex;
    }

    /**
     * Setter for property lastSelectedMessageIndex.
     * @param lastSelectedMessageIndex New value of property lastSelectedMessageIndex.
     */
    public void setLastSelectedMessageIndex(int lastSelectedMessageIndex) {
        this.lastSelectedMessageIndex = lastSelectedMessageIndex;
    }
}
