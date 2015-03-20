package rcs.nml;

import java.io.File;

/**
 * Class representing one entry in an indexed log.
 * @author shackle
 */
public class LogTimeEntry {


    /**
     * Constructor to initialize all properties.
     * @param _FileNameOrURL file name or URL to load data from
     * @param _TimeStamp timestamp in seconds since 1970
     * @param _Offset offset in file for this data
     * @param _MaxLen bytes starting at offset that should be read for this message.
     */
    public LogTimeEntry(String _FileNameOrURL,
            double _TimeStamp,
            int _Offset,
            int _MaxLen) {
        FileNameOrURL = _FileNameOrURL;
        File _f = new File(FileNameOrURL);
        if(_f.exists() && _f.canRead()) {
            this.setF(_f);
        }
        TimeStamp = _TimeStamp;
        Offset = _Offset;
        MaxLen = _MaxLen;
    }

    public String toString() {
        return FileNameOrURL+","+TimeStamp+","+Offset+","+MaxLen;
    }

    protected File F;

    /**
     * Get the value of F
     *
     * @return the value of F
     */
    public File getF() {
        return F;
    }

    /**
     * Set the value of F
     *
     * @param F new value of F
     */
    public void setF(File F) {
        this.F = F;
    }


    private final String FileNameOrURL;

    /**
     * Get the value of FileNameOrURL
     *
     * @return the value of FileNameOrURL
     */
    public String getFileNameOrURL() {
        return FileNameOrURL;
    }

    private final double TimeStamp;

    /**
     * Get the value of TimeStamp
     *
     * @return the value of TimeStamp
     */
    public double getTimeStamp() {
        return TimeStamp;
    }

    private final int Offset;

    /**
     * Get the value of Offset
     *
     * @return the value of Offset
     */
    public int getOffset() {
        return Offset;
    }

    private final int MaxLen;

    /**
     * Get the value of MaxLen
     *
     * @return the value of MaxLen
     */
    public int getMaxLen() {
        return MaxLen;
    }
}
