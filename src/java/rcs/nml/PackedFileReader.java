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
 /*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package rcs.nml;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Class that allows the reading of packed files directly without using an
 * NMLConnection.
 *
 * @author Will Shackleford
 */
public class PackedFileReader {

    private PackedFormatConverter converter = null;

    /**
     * Create a PackedFileReader for reading log files of a given set of types.
     *
     * @param _dict MessageDictionary with auto-generated format functions to
     * parse this specific file types.
     * @param _l64_mode should longs be stored or expected to be 64-bit instead
     * of 32-bit
     */
    public PackedFileReader(NMLMessageDictionary _dict, boolean _l64_mode) {
        converter = new PackedFormatConverter(_l64_mode);
        converter.SetMessageDictionary(_dict);
    }

    /**
     * Read the entire file or URL and parse it into an NMLmsg of the
     * appropriate type.
     *
     * @param fileName the path to a file, or a filename in the current
     * directory or a URL beginning with "http://"
     * @return message from given file.
     */
    public NMLmsg ReadFile(final String fileName) throws FileNotFoundException {
        File f = new File(fileName);
        if (f.exists()) {
            return ReadFile(f);
        } else if (fileName.startsWith("http://")) {
            try {
                URL url_object = new URL(fileName);
                URLConnection url_connection = url_object.openConnection();
                InputStream is = url_connection.getInputStream();
                int bytes_read = 0;
                byte raw_data[] = null;
                int read_ret = -1;
                final int content_length = url_connection.getContentLength();
                raw_data = new byte[content_length];
                read_ret = is.read(raw_data);
                if (read_ret > 0) {
                    bytes_read += read_ret;
                }
                while (bytes_read < content_length) {
                    read_ret = is.read(raw_data, bytes_read,
                            raw_data.length - bytes_read);
                    if (read_ret > 0) {
                        bytes_read += read_ret;
                    } else {
                        break;
                    }
                }
                is.close();
                is = null;
                url_connection = null;
                url_object = null;
                return converter.convertRawDataToMsg(raw_data, 0, raw_data.length);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        throw new FileNotFoundException("Can not find \""+fileName+"\"");
    }

    /**
     * Read the first maxlen bytes out of a file and convert it to an NMLmsg. It
     * is typically used to extract just a timestamp in one of the first
     * variables of a large message.
     *
     * @param fileName fileName or path or URL to read.
     * @param maxlen number of bytes to actually read.
     * @return message from file. (variables stored after maxlen will be all
     * zero.)
     */
    public NMLmsg ReadFileNameStringMaxlen(final String fileName, final int maxlen) throws FileNotFoundException {
        File f = new File(fileName);
        if (f.exists()) {
            return ReadFileObjectMaxLen(f, maxlen);
        } else if (fileName.startsWith("http://")) {
            try {
                URL url_object = new URL(fileName);
                URLConnection url_connection = url_object.openConnection();
                InputStream is = url_connection.getInputStream();
                int bytes_read = 0;
                byte raw_data[] = null;
                int read_ret = -1;
                final int content_length = Math.max(maxlen, url_connection.getContentLength());
                raw_data = new byte[content_length];
                read_ret = is.read(raw_data);
                if (read_ret > 0) {
                    bytes_read += read_ret;
                }
                while (bytes_read < content_length) {
                    read_ret = is.read(raw_data, bytes_read, raw_data.length - bytes_read);
                    if (read_ret > 0) {
                        bytes_read += read_ret;
                    } else {
                        break;
                    }
                }
                is.close();
                is = null;
                url_connection = null;
                url_object = null;
                return converter.convertRawDataToMsg(raw_data, 0, raw_data.length);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        throw new FileNotFoundException("Can not find \""+fileName+"\"");
    }

    /**
     * Read a section of a large file containing many messages.
     *
     * @param fileName filename or path to file or URL starting with "http://"
     * @param offset number of bytes from beginning of file to location of the
     * desired message
     * @param maxlen maximum number of bytes in the message
     * @throws Exception throws exception of offset or max_len exceeds the size
     * of file or
     * @return message from the file.
     */
    public NMLmsg ReadFileSection(final String fileName,
                                  final int offset,
                                  final int maxlen) throws Exception {
        File f = new File(fileName);
        if (f.exists()) {
            return ReadFileSection(f,
                    offset,
                    maxlen);
        } else if (fileName.startsWith("http://")) {
            URLConnection url_connection = null;
            InputStream is = null;
            try {
                URL url_object = new URL(fileName);
                url_connection = url_object.openConnection();
                is = url_connection.getInputStream();
                int bytes_read = 0;
                byte raw_data[] = null;
                int read_ret = -1;
                int cl_from_url_con = url_connection.getContentLength();
                if (cl_from_url_con <= offset) {
                    throw new Exception("Insufficent ContentLength : " + cl_from_url_con + " offset=" + offset);
                }
                int cl_after_offset = cl_from_url_con - offset;
                final int content_length = Math.max(maxlen, cl_after_offset);
                raw_data = new byte[content_length];
                is.skip(offset);
                read_ret = is.read(raw_data);
                if (read_ret > 0) {
                    bytes_read += read_ret;
                }
                while (bytes_read < content_length) {
                    read_ret = is.read(raw_data, bytes_read, raw_data.length - bytes_read);
                    if (read_ret > 0) {
                        bytes_read += read_ret;
                    } else {
                        break;
                    }
                }
                is.close();
                is = null;
                url_connection = null;
                url_object = null;
                return converter.convertRawDataToMsg(raw_data, 0, raw_data.length);
            } finally {
                if (null != is) {
                    is.close();
                    is = null;
                }
                url_connection = null;
            }
        }
        return null;
    }

    /**
     * Read the entire message in the pointed to file and convert it to an
     * NMLmsg.
     *
     * @param f the file to read.
     * @return message from File.
     */
    public NMLmsg ReadFile(final File f) throws FileNotFoundException {
        if(null == f) {
            throw new IllegalArgumentException("File f="+f);
        }
        if(!f.exists()) {
            throw new FileNotFoundException("File "+f+" does not exist.");
        }
        FileInputStream fis = null;
        byte raw_data[] = null;
        NMLmsg tmpMsg = null;
        try {
            fis = new FileInputStream(f);
            raw_data = new byte[fis.available()];
            fis.read(raw_data);
            fis.close();
            fis = null;
            tmpMsg = converter.convertRawDataToMsg(raw_data, 0, raw_data.length);
        } catch (Exception except) {
            except.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            rcs.nml.debugInfo.debugPrintStream.println("Can't ReadFile(" + f + ")");
        } finally {
            if (null != fis) {
                try {
                    fis.close();
                } catch (Exception ex2) {
                    ex2.printStackTrace();
                }
                fis = null;
            }
            raw_data = null;
        }
        return tmpMsg;
    }

    /**
     * Read the first max_len bytes from a file and convert it to an NML
     * message. It is typically used to get a timestamp which is one of the
     * first few variables in a large message. Variables from more than max_len
     * bytes into the file will be left zero.
     *
     * @param f file to read
     * @param max_len maximum number of bytes to read.
     * @return the message for the given file with variables after max_len left
     * zero.
     */
    public NMLmsg ReadFileObjectMaxLen(final File f, int max_len) throws FileNotFoundException {
        if(null == f) {
            throw new IllegalArgumentException("File f="+f);
        }
        if(!f.exists()) {
            throw new FileNotFoundException("File "+f+" does not exist.");
        }
        FileInputStream fis = null;
        byte raw_data[] = null;
        NMLmsg tmpMsg = null;
        try {
            boolean hide_errors_orig = converter.hide_errors;
            fis = new FileInputStream(f);
            int len = fis.available();
            if (len > max_len) {
                converter.hide_errors = true;
                len = max_len;
            }
            raw_data = new byte[len];
            fis.read(raw_data);
            fis.close();
            fis = null;
            tmpMsg = converter.convertRawDataToMsg(raw_data, 0, raw_data.length);
            converter.hide_errors = hide_errors_orig;
        } catch (Exception except) {
            except.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            rcs.nml.debugInfo.debugPrintStream.println("Can't ReadFile(" + f + ")");
        } finally {
            if (null != fis) {
                try {
                    fis.close();
                } catch (Exception ex2) {
                    ex2.printStackTrace();
                }
                fis = null;
            }
            raw_data = null;
        }
        return tmpMsg;
    }

    /**
     * Read an indexFile as created by the logRecorder into an integer array.
     *
     * @param indexFileName name of the index file.
     * @param littleEndian was the log recorder run on a littleEndian platform
     * (probably true)
     * @return integer array with the indexes
     * @throws Exception occurs if the file does not exist or can not be read.
     */
    static public int[] ReadIndexes(String indexFileName,
                                    boolean littleEndian) throws Exception {
        return ReadIndexes(new File(indexFileName),
                littleEndian);
    }

    /**
     * Read an indexFile as created by the logRecorder into an integer array.
     *
     * @param indexFile file to read
     * @param littleEndian was the log recorder run on a littleEndian platform
     * (probably true)
     * @return integer array with the indexes
     * @throws Exception occurs if the file does not exist or can not be read.
     */
    static public int[] ReadIndexes(File indexFile,
                                    boolean littleEndian) throws Exception {
        LinkedList<Integer> lli = new LinkedList<Integer>();
        FileInputStream fis = new FileInputStream(indexFile);
        int ia[] = null;
        byte ba[] = new byte[4];
        while (fis.available() >= 4) {
            fis.read(ba);
            int i;
            if (littleEndian) {
                i = (((int) ba[3] & 0xFF) << 24)
                        | (((int) ba[2] & 0xFF) << 16)
                        | (((int) ba[1] & 0xFF) << 8)
                        | ((int) ba[0] & 0xFF);
            } else {
                i = (((int) ba[0] & 0xFF) << 24)
                        | (((int) ba[1] & 0xFF) << 16)
                        | (((int) ba[2] & 0xFF) << 8)
                        | ((int) ba[3] & 0xFF);
            }
            lli.add(i);
        }
        ia = new int[lli.size()];
        for (int i = 0; i < lli.size(); i++) {
            ia[i] = lli.get(i).intValue();
        }
        return ia;
    }

    /**
     * Create a list to use as an index into a large ".COMBINED" file with
     * multiple messages in it using the ".index" file created by the
     * logRecorder at the same time
     *
     * @param combinedFile large file with multiple messages to read
     * @param indexFile index file with list of position offsets only
     * @param indexIsLittleEndian true if the logRecorder was run on a
     * little-endian platform
     * @param maxlen number of bytes of each message that needs to be read to
     * get the time stamp.
     * @param mtts class to get timestamp out of custom message types.
     * @return list of LogTimeEntry objects useful for finding a message at a
     * given time.
     * @throws Exception are thrown if one of files does not exist or is not
     * readable.
     */
    public List<rcs.nml.LogTimeEntry> createLogTimeEntryList(File combinedFile,
                                                             File indexFile,
                                                             boolean indexIsLittleEndian,
                                                             int maxlen,
                                                             rcs.nml.MsgToTimeStamp mtts) throws Exception {

        int indexes_ia[] = ReadIndexes(indexFile, indexIsLittleEndian);
        if (null == indexes_ia || indexes_ia.length < 1) {
            throw new Exception("Bad index file : " + indexFile);
        }
        ArrayList<rcs.nml.LogTimeEntry> l
                = new ArrayList<rcs.nml.LogTimeEntry>(indexes_ia.length + 1);
        NMLmsg msg0 = ReadFileSection(combinedFile, 0, maxlen);
        double ts = mtts.getTimestampFromMessage(msg0);
        String cp = combinedFile.getCanonicalPath();
        int indexNum = 0;
        if (indexes_ia[0] == 0) {
            indexNum++;
        }
        LogTimeEntry lte0 = new LogTimeEntry(cp, ts, 0, indexes_ia[indexNum]);
        l.add(lte0);
        int pos = indexes_ia[indexNum];
        final int cfl = (int) combinedFile.length();
        while (indexNum < indexes_ia.length && pos < cfl) {
            int ml = maxlen;
            NMLmsg msg = ReadFileSection(combinedFile, pos, maxlen);
            ts = mtts.getTimestampFromMessage(msg);
            int ms = 0;
            if (indexNum < indexes_ia.length - 1) {
                int nextPos = indexes_ia[indexNum + 1];
                ms = nextPos - pos;
            } else {
                ms = cfl - pos;
            }
            LogTimeEntry lte = new LogTimeEntry(cp, ts, pos, ms);
            l.add(lte);
            pos = indexes_ia[indexNum];
            indexNum++;
        }
        return l;
    }

    public static void main(String args[]) {
        try {
            System.out.println("args[0]=" + args[0]);
            int ia[] = ReadIndexes(args[0], true);
            System.out.println("ia = " + Arrays.toString(ia));
            for (int i = 1; i < ia.length; i++) {
                System.out.println("ia[i] = " + ia[i]);
                System.out.println("ia[i]-ia[i-1] = " + (ia[i] - ia[i - 1]));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Read a message from a large ".COMBINED" file that contains multiple
     * messages.
     *
     * @param f file to read
     * @param offset offset in bytes to the desired message
     * @param max_len number of bytes to read from desired message
     * @return message read from that part of the file.
     * @throws Exception throws exception of offset or max_len exceeds the size
     * of file or file is not readable.
     */
    public NMLmsg ReadFileSection(final File f,
                                  int offset,
                                  int max_len) throws Exception {
        byte raw_data[] = null;
        NMLmsg tmpMsg = null;
        RandomAccessFile raf = null;
        try {
            if (offset < 0) {
                throw new Exception(this.getClass().getCanonicalName() + ".ReadFileSection() -- offset less than zero.");
            }
            if (offset + max_len > f.length()) {
                throw new Exception(this.getClass().getCanonicalName() + ".ReadFileSection() : offset(" + offset + ") + max_len(" + max_len + ") = " + (offset + max_len) + " exceeds file length of " + f.length() + " for " + f.getCanonicalPath());
            }
            boolean hide_errors_orig = converter.hide_errors;
            raf = new RandomAccessFile(f, "r");
            int len = (int) (raf.length() - offset);
            if (len > max_len) {
                converter.hide_errors = true;
                len = max_len;
            }
            raw_data = new byte[len];
            raf.seek(offset);
            raf.readFully(raw_data, 0, len);
            tmpMsg = converter.convertRawDataToMsg(raw_data, 0, raw_data.length);
            converter.hide_errors = hide_errors_orig;
        } catch (Exception except) {
            rcs.nml.debugInfo.debugPrintStream.println("Can't ReadFile(" + f + ")");
            throw except;
        } finally {
            if (null != raf) {
                try {
                    raf.close();
                } catch (Exception ex2) {
                    ex2.printStackTrace();
                }
                raf = null;
            }
            raw_data = null;
        }
        return tmpMsg;
    }

    /**
     * Use FormatConverter to get a string representation of the message.
     *
     * @param inMsg message to convert
     * @return converted string
     */
    public String convertMsgToString(final NMLmsg inMsg) {
        return converter.convertMsgToString(inMsg);
    }
}
