package rcs.nml;

/**
 * Interface passed to tell indexer how to get a timestamp
 * double seconds since epoch 12:00 AM Jan 1, 1970
 * @author shackle
 */
public interface MsgToTimeStamp {

    /**
     * Return the timestamp that should be  associated with this message
     * used for indexing log files. Unfortunately different messages use different
     * variables for this and some with different epochs/units etc.
     * @param msg message to extract timestamp from
     * @return time in seconds associated with this message since Jan 1,1970.
     */
    public double getTimestampFromMessage(rcs.nml.NMLmsg msg);
    
}
