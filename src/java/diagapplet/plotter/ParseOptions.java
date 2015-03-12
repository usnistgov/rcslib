package diagapplet.plotter;

import java.util.Arrays;

/**
 *
 * @author Will Shackleford<shackle@nist.gov>
 */
public class ParseOptions implements Cloneable {

    private double scale = 1.0;

    /**
     * Get the value of scale
     *
     * @return the value of scale
     */
    public double getScale() {
        return scale;
    }

    /**
     * Set the value of scale
     *
     * @param scale new value of scale
     */
    public void setScale(double scale) {
        this.scale = scale;
    }

    private String filterPattern = "";

    /**
     * Get the value of filterPattern
     *
     * @return the value of filterPattern
     */
    public String getFilterPattern() {
        return filterPattern;
    }

    /**
     * Set the value of filterPattern
     *
     * @param filterPattern new value of filterPattern
     */
    public void setFilterPattern(String filterPattern) {
        this.filterPattern = filterPattern;
    }

    private boolean plotVersusLineNumber = false;

    /**
     * Get the value of plotVersusLineNumber
     *
     * @return the value of plotVersusLineNumber
     */
    public boolean isPlotVersusLineNumber() {
        return plotVersusLineNumber;
    }

    /**
     * Set the value of plotVersusLineNumber
     *
     * @param plotVersusLineNumber new value of plotVersusLineNumber
     */
    public void setPlotVersusLineNumber(boolean plotVersusLineNumber) {
        this.plotVersusLineNumber = plotVersusLineNumber;
    }

    private String fieldSeperator = ",";

    /**
     * Get the value of fieldSeperator
     *
     * @return the value of fieldSeperator
     */
    public String getFieldSeperator() {
        return fieldSeperator;
    }

    /**
     * Set the value of fieldSeperator
     *
     * @param fieldSeperator new value of fieldSeperator
     */
    public void setFieldSeperator(String fieldSeperator) {
        this.fieldSeperator = fieldSeperator;
    }

        private String firstLine;

    /**
     * Get the value of firstLine
     *
     * @return the value of firstLine
     */
    public String getFirstLine() {
        return firstLine;
    }

    /**
     * Set the value of firstLine
     *
     * @param firstLine new value of firstLine
     */
    public void setFirstLine(String firstLine) {
        this.firstLine = firstLine;
    }

    private int[] fieldsToShow;

    /**
     * Get the value of fieldsToShow
     *
     * @return the value of fieldsToShow
     */
    public int[] getFieldsToShow() {
        return fieldsToShow;
    }

    /**
     * Set the value of fieldsToShow
     *
     * @param fieldsToShow new value of fieldsToShow
     */
    public void setFieldsToShow(int[] fieldsToShow) {
        this.fieldsToShow = fieldsToShow;
    }

    @Override
    public ParseOptions clone() {
        ParseOptions po = new ParseOptions();
        po.fieldSeperator = this.fieldSeperator;
        po.filterPattern = this.filterPattern;
        po.plotVersusLineNumber = this.plotVersusLineNumber;
        po.fieldsToShow = this.fieldsToShow == null ? null : Arrays.copyOf(this.fieldsToShow, this.fieldsToShow.length);
        return po;
    }
}
