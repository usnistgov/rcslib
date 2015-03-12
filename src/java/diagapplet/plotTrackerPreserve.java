/*
 * plotTrackerPreserve.java
 *
 * Created on January 16, 2007, 6:39 PM
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

/**
 * Stores all of the information kept between sessions about a single plot by the diagnostics tool.
 * Everything is accessed though getter/setter functions to be compatible with
 * XMLDecoder/XMLEncoder as the files are saved in XML
 * @author shackle
 */
public class plotTrackerPreserve {

    @Override
    public String toString() {
        return super.toString() + ";\n" +
                "bufferName=" + bufferName + ";\n" +
                "isArray=" + isArray + ";\n" +
                "isAux=" + isAux + ";\n" +
                "isCmd=" + isCmd + ";\n" +
                "moduleName=" + moduleName + ";\n" +
                "msgType=" + msgType + ";\n" +
                "name=" + name + ";\n" +
                "stiName=" + stiName + ";\n" +
                "variableName=" + variableName + ";\n" +
                "variableNumber=" + variableNumber + ";\n";
    }

    /** Creates a new instance of plotTrackerPreserve */
    public plotTrackerPreserve() {
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
     * Holds value of property isAux.
     */
    private boolean isAux;

    /**
     * Getter for property isAux.
     * @return Value of property isAux.
     */
    public boolean isIsAux() {
        return this.isAux;
    }

    /**
     * Setter for property isAux.
     * @param isAux New value of property isAux.
     */
    public void setIsAux(boolean isAux) {
        this.isAux = isAux;
    }
    /**
     * Holds value of property isCmd.
     */
    private boolean isCmd;

    /**
     * Getter for property isCmd.
     * @return Value of property isCmd.
     */
    public boolean isIsCmd() {
        return this.isCmd;
    }

    /**
     * Setter for property isCmd.
     * @param isCmd New value of property isCmd.
     */
    public void setIsCmd(boolean isCmd) {
        this.isCmd = isCmd;
    }
    /**
     * Holds value of property isArray.
     */
    private boolean isArray;

    /**
     * Getter for property isArray.
     * @return Value of property isArray.
     */
    public boolean isIsArray() {
        return this.isArray;
    }

    /**
     * Setter for property isArray.
     * @param isArray New value of property isArray.
     */
    public void setIsArray(boolean isArray) {
        this.isArray = isArray;
    }
    /**
     * Holds value of property moduleName.
     */
    private String moduleName;

    /**
     * Getter for property moduleName.
     * @return Value of property moduleName.
     */
    public String getModuleName() {
        return this.moduleName;
    }

    /**
     * Setter for property moduleName.
     * @param moduleName New value of property moduleName.
     */
    public void setModuleName(String moduleName) {
        this.moduleName = moduleName;
    }
    /**
     * Holds value of property BufferName.
     */
    private String bufferName;

    /**
     * Getter for property BufferName.
     * @return Value of property BufferName.
     */
    public String getBufferName() {
        return this.bufferName;
    }

    /**
     * Setter for property bufferName.
     * @param bufferName New value of property bufferName.
     */
    public void setBufferName(String bufferName) {
        this.bufferName = bufferName;
    }
    /**
     * Holds value of property variableNumber.
     */
    private int variableNumber;

    /**
     * Getter for property variableNumber.
     * @return Value of property variableNumber.
     */
    public int getVariableNumber() {
        return this.variableNumber;
    }

    /**
     * Setter for property variableNumber.
     * @param variableNumber New value of property variableNumber.
     */
    public void setVariableNumber(int variableNumber) {
        this.variableNumber = variableNumber;
    }
    /**
     * Holds value of property variableName.
     */
    private String variableName;

    /**
     * Getter for property variableName.
     * @return Value of property variableName.
     */
    public String getVariableName() {
        return this.variableName;
    }

    /**
     * Setter for property variableName.
     * @param variableName New value of property variableName.
     */
    public void setVariableName(String variableName) {
        this.variableName = variableName;
    }
    /**
     * Holds value of property stiName.
     */
    private String stiName;

    /**
     * Getter for property stiName.
     * @return Value of property stiName.
     */
    public String getStiName() {
        return this.stiName;
    }

    /**
     * Setter for property stiName.
     * @param stiName New value of property stiName.
     */
    public void setStiName(String stiName) {
        this.stiName = stiName;
    }
    /**
     * Holds value of property msgType.
     */
    private long msgType;

    /**
     * Getter for property msgType.
     * @return Value of property msgType.
     */
    public long getMsgType() {
        return this.msgType;
    }

    /**
     * Setter for property msgType.
     * @param msgType New value of property msgType.
     */
    public void setMsgType(long msgType) {
        this.msgType = msgType;
    }
}
