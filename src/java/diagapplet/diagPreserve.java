/*
 * diagPreserve.java
 *
 * Created on January 1, 2007, 2:36 PM
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

import java.io.Serializable;
import java.util.Vector;

/**
 * Stores all of the information kept between sessions by the diagnostics tool.
 * Everything is accessed though getter/setter functions to be compatible with
 * XMLDecoder/XMLEncoder as the files are saved in XML
 * @author shackle
 */
public class diagPreserve
        implements Serializable {

    /**
     *   Unique id for this class, Exception implements Serializable interface see documentation
     * for serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613939L;

    protected boolean Automatically_Keep_and_Use_PlotSets;

    /**
     * Get the value of Automatically_Keep_and_Use_PlotSets
     *
     * @return the value of Automatically_Keep_and_Use_PlotSets
     */
    public boolean isAutomatically_Keep_and_Use_PlotSets() {
        return Automatically_Keep_and_Use_PlotSets;
    }

    /**
     * Set the value of Automatically_Keep_and_Use_PlotSets
     *
     * @param Automatically_Keep_and_Use_PlotSets new value of Automatically_Keep_and_Use_PlotSets
     */
    public void setAutomatically_Keep_and_Use_PlotSets(boolean Automatically_Keep_and_Use_PlotSets) {
        this.Automatically_Keep_and_Use_PlotSets = Automatically_Keep_and_Use_PlotSets;
    }

    /**
     * Holds the value of the recentNmlConfigFileVector property.
     * List of recent NML file names.
     */
    private Vector<String> recentNmlConfigFileVector = new Vector<String>();
    /**
     * Holds the value of the connected property.
     * Is/Should the diag tool automatically connect to the NML buffers?
     * (It may be over written with a setting on the command line or in the diagnostics
     * config file.)
     */
    private boolean connected = false;

    /**
     * Get Recent NML Config Files.
     * @return Vector of strings of names of previously used NML config files.
     */
    public Vector<String> get_recentNmlConfigFileVector() {
        return recentNmlConfigFileVector;
    }

    /**
     * Set The vector of strings of NML config file names.
     * @param _recentNmlConfigFileVector recent nml files vector
     */
    public void set_recentNmlConfigFileVector(Vector<String> _recentNmlConfigFileVector) {
        recentNmlConfigFileVector = _recentNmlConfigFileVector;
    }

    /**
     * Set a flag so the diagnostics too  will reconnect or not when next restarted.
     * (It may be over written with a setting on the command line or in the diagnostics
     * config file.)
     * @param _connected is the system connected
     */
    public void set_connected(boolean _connected) {
        connected = _connected;
    }

    /**
     * Get the value of connected flag from last run.
     * @return Was the tool connected when state last saved?
     */
    public boolean get_connected() {
        return connected;
    }

    /**
     * Get the value of connected flag from last run.
     * (redundant with get_connected.)
     * @return  Was the tool connected when state last saved?
     */
    public boolean is_connected() {
        return connected;
    }

    /** Creates a new instance of diagPreserve */
    public diagPreserve() {
    }
    /**
     * Holds value of property selectedModule.
     * The name of the last selected module.
     */
    private String selectedModule;

    /**
     * Getter for property selectedModule.
     * @return Value of property selectedModule ( the name of the last selected module. )
     */
    public String getSelectedModule() {
        return this.selectedModule;
    }

    /**
     * Setter for property selectedModule.
     * @param selectedModule New value of property selectedModule (the name of the selected module).
     */
    public void setSelectedModule(String selectedModule) {
        this.selectedModule = selectedModule;
    }

    /**
     * Holds value of property ModulesVector.
     * Vector containing one modulePreserve object for each module in the hierarchy.
     */
    private Vector<modulePreserve> ModulesVector;

    /**
     * Getter for property ModulesVector.
     * @return Value of property ModulesVector. 
     * (Vector containing one modulePreserve object for each module in the hierarchy.)
     */
    public Vector<modulePreserve> getModulesVector() {
        return this.ModulesVector;
    }

    /**
     * Setter for property ModulesVector.
     * @param ModulesVector New value of property ModulesVector.
     * (Vector containing one modulePreserve object for each module in the hierarchy.)
     */
    public void setModulesVector(Vector<modulePreserve> ModulesVector) {
        this.ModulesVector = ModulesVector;
    }



    /**
     * Holds value of property ModulesVector.
     * Vector containing one modulePreserve object for each module in the hierarchy.
     */
    private Vector<auxBufferPreserve> AuxBuffersVector;


    /**
     * Holds value of property selectedAuxChannel.
     * (The name of the last selected auxilliary channel.)
     */
    private String selectedAuxChannel;

    /**
     * Getter for property selectedAuxChannel.
     * @return Value of property selectedAuxChannel.
     */
    public String getSelectedAuxChannel() {
        return this.selectedAuxChannel;
    }

    /**
     * Setter for property selectedAuxChannel.
     * @param selectedAuxChannel New value of property selectedAuxChannel.
     */
    public void setSelectedAuxChannel(String selectedAuxChannel) {
        this.selectedAuxChannel = selectedAuxChannel;
    }
    /**
     * Holds value of property selectedTab.
     * 
     */
    private int selectedTab;

    /**
     * Getter for property selectedTab.
     * @return Value of property selectedTab.
     */
    public int getSelectedTab() {
        return this.selectedTab;
    }

    /**
     * Setter for property selectedTab.
     * @param selectedTab New value of property selectedTab.
     */
    public void setSelectedTab(int selectedTab) {
        this.selectedTab = selectedTab;
    }
    /**
     * Holds value of property plotSet.
     */
    private plotSetPreserve plotSet;

    /**
     * Getter for property plotSet.
     * @return Value of property plotSet.
     */
    public plotSetPreserve getPlotSet() {
        return this.plotSet;
    }

    /**
     * Setter for property plotSet.
     * @param plotSet New value of property plotSet.
     */
    public void setPlotSet(plotSetPreserve plotSet) {
        this.plotSet = plotSet;
    }
    /**
     * Holds value of property recentPlotSets.
     */
    private Vector<String> recentPlotSets;

    /**
     * Getter for property recentPlotSets.
     * @return Value of property recentPlotSets.
     */
    public Vector<String> getRecentPlotSets() {
        return this.recentPlotSets;
    }

    /**
     * Setter for property recentPlotSets.
     * @param recentPlotSets New value of property recentPlotSets.
     */
    public void setRecentPlotSets(Vector<String> recentPlotSets) {
        this.recentPlotSets = recentPlotSets;
    }
    /**
     * Holds value of property moved.
     */
    private boolean moved;

    /**
     * Getter for property moved.
     * @return Value of property moved.
     */
    public boolean isMoved() {
        return this.moved;
    }

    /**
     * Setter for property moved.
     * @param moved New value of property moved.
     */
    public void setMoved(boolean moved) {
        this.moved = moved;
    }
    /**
     * Holds value of property x.
     */
    private int x;

    /**
     * Getter for property x.
     * @return Value of property x.
     */
    public int getX() {
        return this.x;
    }

    /**
     * Setter for property x.
     * @param x New value of property x.
     */
    public void setX(int x) {
        this.x = x;
    }
    /**
     * Holds value of property y.
     */
    private int y;

    /**
     * Getter for property y.
     * @return Value of property y.
     */
    public int getY() {
        return this.y;
    }

    /**
     * Setter for property y.
     * @param y New value of property y.
     */
    public void setY(int y) {
        this.y = y;
    }
    /**
     * Holds value of property resized.
     */
    private boolean resized;

    /**
     * Getter for property resized.
     * @return Value of property resized.
     */
    public boolean isResized() {
        return this.resized;
    }

    /**
     * Setter for property resized.
     * @param resized New value of property resized.
     */
    public void setResized(boolean resized) {
        this.resized = resized;
    }
    /**
     * Holds value of property width.
     */
    private int width;

    /**
     * Getter for property width.
     * @return Value of property width.
     */
    public int getWidth() {
        return this.width;
    }

    /**
     * Setter for property width.
     * @param width New value of property width.
     */
    public void setWidth(int width) {
        this.width = width;
    }
    /**
     * Holds value of property height.
     */
    private int height;

    /**
     * Getter for property height.
     * @return Value of property height.
     */
    public int getHeight() {
        return this.height;
    }

    /**
     * Setter for property height.
     * @param height New value of property height.
     */
    public void setHeight(int height) {
        this.height = height;
    }
    /**
     * Holds value of property defaultNMLConfigFile.
     */
    private String defaultNMLConfigFile;

    /**
     * Getter for property defaultNMLConfigFile.
     * @return Value of property defaultNMLConfigFile.
     */
    public String getDefaultNMLConfigFile() {
        return this.defaultNMLConfigFile;
    }

    /**
     * Setter for property defaultNMLConfigFile.
     * @param defaultNMLConfigFile New value of property defaultNMLConfigFile.
     */
    public void setDefaultNMLConfigFile(String defaultNMLConfigFile) {
        this.defaultNMLConfigFile = defaultNMLConfigFile;
    }
    /**
     * Holds value of property window_state.
     */
    private int window_state;

    /**
     * Getter for property window_state.
     * @return Value of property window_state.
     */
    public int getWindow_state() {
        return this.window_state;
    }

    /**
     * Setter for property window_state.
     * @param window_state New value of property window_state.
     */
    public void setWindow_state(int window_state) {
        this.window_state = window_state;
    }
    /**
     * Holds value of property auto_connect_disconnect.
     */
    private boolean auto_connect_disconnect;

    /**
     * Getter for property auto_connect_disconnect.
     * @return Value of property auto_connect_disconnect.
     */
    public boolean isAuto_connect_disconnect() {
        return this.auto_connect_disconnect;
    }

    /**
     * Setter for property auto_connect_disconnect.
     * @param auto_connect_disconnect New value of property auto_connect_disconnect.
     */
    public void setAuto_connect_disconnect(boolean auto_connect_disconnect) {
        this.auto_connect_disconnect = auto_connect_disconnect;
    }

    /**
     * @return the AuxBuffersVector
     */
    public Vector<auxBufferPreserve> getAuxBuffersVector() {
	return AuxBuffersVector;
    }

    /**
     * @param AuxBuffersVector the AuxBuffersVector to set
     */
    public void setAuxBuffersVector(Vector<auxBufferPreserve> AuxBuffersVector) {
	this.AuxBuffersVector = AuxBuffersVector;
    }
}
