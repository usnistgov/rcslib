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
package rcs.nml;

import java.io.*;
import java.util.*;

import rcs.utils.CorrectedPipedInputStream;
import rcs.utils.CorrectedPipedOutputStream;
import rcs.utils.StackTracePrinter;

/**
 * This class is the base class for all classes used by NML to convert
 * local data types to some neutral format usable by many different types of
 * hosts.
 *
 * @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
 */
public abstract class NMLFormatConverterBase extends NMLFormatConverter {

    public String getVersionString() {
        return "NMLFormatConverterBase : Nov 1, 2011 9:55";
    }
    /**
     * True when the data from the network is being converted into a message in
     * this hosts format, false when a message on this host is being converted to
     * a neutral format.
     */
    protected NMLFormatConvertErrCallbackInterface nfceci = null;

    public void SetFormatConvertErrCallback(NMLFormatConvertErrCallbackInterface new_nfceci) {
        nfceci = new_nfceci;
    }
    public boolean decoding = false;
    public boolean first_format_error = true;
    public boolean updating_unsigned = false;
    public boolean check_unsigneds = false;
    public boolean hide_errors = false;
    protected boolean first_update_error_occured = false;
    protected boolean use_string = false;
    protected boolean diagnostics_mode = false; // limit strings to manageable sizes.
    protected int diagnostics_mode_string_max = 80;

    public long getPos() {
        if(this.decoding) {
            return this.raw_data_size - this.bytes_in_input_stream;
        } else {
            return this.raw_data_size;
        }
    }
    
    public void set_diagnostics_mode(boolean dm) {
        this.diagnostics_mode = dm;
    }

    public boolean get_diagnostics_mode() {
        return this.diagnostics_mode;
    }

    public void set_diagnostics_mode_string_max(int len) {
        this.diagnostics_mode_string_max = len;
    }

    public int get_diagnostics_mode_string_max() {
        return this.diagnostics_mode_string_max;
    }
    public static boolean debug_on = rcs.nml.debugInfo.debug_on;
    protected StringBuffer output_string_buffer = null;
    protected String input_string = null;
    protected String next_default = null;
    protected DataInputStream input_stream = null;
    protected StringTokenizer input_string_tokenizer = null;
    protected DataOutputStream output_stream = null;
    protected long msg_size = 0;
    protected int raw_data_size = 0;
    protected NMLMessageDictionary msg_dict = null;
    protected int bytes_in_input_stream;
    protected boolean bytes_in_input_stream_known = true;
    protected String var_name = null;
    public Vector var_name_list = null;
    public static final boolean save_var_names = false;
    public String class_name = null;
    public String base_class_name = null;
    public String class_var_name = null;
    int var_number = 0;

    public void SetBufName(String bname) {
        bufName = bname;
    }

    public void SetMessageDictionary(NMLMessageDictionary new_dict) {
        msg_dict = new_dict;
        var_name = null;
    }

    public int get_token_count() {
        if (decoding) {
            if (null == input_string_tokenizer) {
                return 0;
            }
            return input_string_tokenizer.countTokens();
        } else {
            String temp_output_string = output_string_buffer.toString();
            StringTokenizer tempTokenizer = new StringTokenizer(temp_output_string, ",");
            return tempTokenizer.countTokens();
        }
    }

    public void add_to_output_string(String s) {
        output_string_buffer.append(s);
    }

    public boolean get_decoding() {
        return decoding;
    }

    public boolean get_use_string() {
        return use_string;
    }

    public void SetErrorInUpdate(String s) {
        try {
            first_update_error_occured = true;
            Throwable t = new Throwable();
            error_in_update_string = StackTracePrinter.ThrowableToShortList(t) + s;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void throw_away_token() {
        try {
            if (null != input_string_tokenizer && input_string_tokenizer.hasMoreTokens()) {
                input_string_tokenizer.nextToken();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Function that should be called before every message or string is parsed.
     */
    public void rewind() {
        first_update_error_occured = false;
        var_name_list = null;
        var_name = null;
        var_number = 0;
        updating_unsigned = false;
        next_default = null;
        msg_size = 0;
        raw_data_size = 0;
        bytes_in_input_stream = 0;
        bytes_in_input_stream_known = false;
        if (null == output_string_buffer) {
            output_string_buffer = new StringBuffer();
        }
        output_string_buffer.setLength(0);
        input_string = "";
        input_stream = null;
        input_string_tokenizer = null;
        output_stream = null;
        stat_msg_updated = false;
        cmd_msg_updated = false;
        error_in_update = false;
        error_in_update_string = null;
    }

    public void start_updates() {
    }

    public NMLMessageDictionary GetMessageDictionary() {
        return msg_dict;
    }

    protected NMLmsg convertRawDataToMsg(byte b[], int offset, int size) {
        try {
            error_in_update = false;
            rewind();
            if (b.length < size) {
                System.err.println("NMLFormatConverter.convertRawDataToMsg() , passed an invalid array of length " + b.length + " to hold " + size + " bytes");
                return null;
            }
            decoding = true;
            use_string = false;
            raw_data_size = size;
            CorrectedPipedInputStream pipe_in = new CorrectedPipedInputStream();
            CorrectedPipedOutputStream pipe_out = new CorrectedPipedOutputStream(pipe_in);
            input_stream = new DataInputStream(pipe_in);
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("convertRawDataToMsg(), Writing " + size + " bytes to pipe");
                // rcs.nml.debugInfo.debugPrintStream.println("b = "+new String(b,0,size));
            }
            pipe_out.write(b, offset, size);
            bytes_in_input_stream = size;
            raw_data_size = size;
            bytes_in_input_stream_known = true;
            start_updates();
            if (error_in_update) {
                return null;
            }
            long msg_type_long_in = (long) msg_type;
            long msg_type_long_out = 0;
            msg_type_long_out = update_with_name("type", msg_type_long_in);
            if (msg_type_long_out > Integer.MAX_VALUE || msg_type_long_out <= 0) {
                throw new Exception("Bad msg_type=" + msg_type_long_out);
            }
            msg_type = (int) msg_type_long_out;
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("convertRawDataToMsg(), msg_type =" + msg_type);
            }
            msg_size = update_with_name("size", msg_size);
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("convertRawDataToMsg(), msg_size =" + msg_size);
            }
            if (null == msg_dict) {
                System.err.println("No Message Dictionary!!!");
                return null;
            }
            if (msg_dict.formatMsg(this) < 0) {
                System.err.println("Format Message Error, msg_type = " + msg_type + ", msg_dict = " + msg_dict + ", formatter = " + this);
                if (first_format_error && !hide_errors) {
                    Thread.dumpStack();
                    first_format_error = false;
                }
                return null;
            }
            if (bytes_in_input_stream != 0 && !bytes_not_used_warning_given && !hide_errors) {
                if (!bytes_not_used_warning_given) {
                    System.err.println("WARNING : NMLFormatConverterBase : Not all bytes (" + bytes_in_input_stream + ")  were used in decoding a message. Check that C++ and Java update functions were correctly written/generated and compatible.");
                }
                if (nfceci != null) {
                    nfceci.bytesNotUsed(msg_dict, bytes_in_input_stream, bufName, bytes_not_used_warning_given);
                }
                bytes_not_used_warning_given = true;
            }
            return (NMLmsg) msg_to_update;
        } catch (Exception e) {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
        }
        return null;
    }

    protected NMLmsg convertStringToMsg(String str) {
        try {
            rewind();
            if (null == str) {
                return null;
            }
            decoding = true;
            use_string = true;
            bytes_in_input_stream = 0;
            bytes_in_input_stream_known = false;
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("NMLFormatConverter.convertStringToMsg(" + str + ") called.");
            }
            input_string = str;
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("Checking for double commas");
                rcs.nml.debugInfo.debugPrintStream.println(input_string);
            }
            int dc_index = input_string.indexOf(",,");
            while (dc_index > 0) {
                if (debug_on) {
                    rcs.nml.debugInfo.debugPrintStream.println("Breaking String");
                    rcs.nml.debugInfo.debugPrintStream.println(input_string.substring(0, dc_index));
                    if (dc_index < input_string.length() - 2) {
                        rcs.nml.debugInfo.debugPrintStream.println(input_string.substring(dc_index + 2));
                    }
                }
                if (dc_index < input_string.length() - 2) {
                    input_string = input_string.substring(0, dc_index) + ",(null)," + input_string.substring(dc_index + 2);
                } else {
                    input_string = input_string.substring(0, dc_index) + ",(null),";
                }
                if (debug_on) {
                    rcs.nml.debugInfo.debugPrintStream.println(input_string);
                }
                dc_index = input_string.indexOf(",,");
            }
            input_string_tokenizer = new StringTokenizer(input_string, ",");
            start_updates();
            long msg_type_long_in = (long) msg_type;
            long msg_type_long_out = 0;
            msg_type_long_out = update_with_name("type", msg_type_long_in);
            if (msg_type_long_out > Integer.MAX_VALUE || msg_type_long_out <= 0) {
                throw new Exception("Bad msg_type=" + msg_type_long_out);
            }
            msg_type = (int) msg_type_long_out;
            msg_size = update_with_name("size", msg_size);
            if (null == msg_dict) {
                System.err.println("No Message Dictionary!!!");
                return null;
            }
            if (msg_dict.formatMsg(this) < 0) {
                System.err.println("Format Message Error, msg_type = " + msg_type + ", msg_dict = " + msg_dict + ", formatter = " + this);
                if (first_format_error) {
                    Thread.dumpStack();
                    first_format_error = false;
                }
                return null;
            }
            if (input_string_tokenizer.countTokens() != 0) {
                if (!tokens_not_used_warning_given) {
                    System.err.println("WARNING: NMLFormatConverterBase not all tokens(" + input_string_tokenizer.countTokens() + ") were used in converting a string. Check that the C++ and Java update functions were correctly generated/written and that they are compatible.");
                }
                if (null != nfceci) {
                    nfceci.tokensNotUsed(msg_dict, input_string_tokenizer.countTokens(), input_string, tokens_not_used_warning_given);
                }
                tokens_not_used_warning_given = true;
            }
            if (debug_on) {
                NMLmsg NMLmsg_to_update = (NMLmsg) msg_to_update;
                rcs.nml.debugInfo.debugPrintStream.println("NMLFormatConverter.convertStringToMsg: msg_type = " + NMLmsg_to_update.type + ", msg_size = " + NMLmsg_to_update.size);
            }
            return (NMLmsg) msg_to_update;
        } catch (Exception e) {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
        }
        return null;
    }
    private boolean do_not_print_diag_overflow_err = false;

    public void set_do_not_print_diag_overflow_err(boolean _do_not_print_diag_overflow_err) {
        this.do_not_print_diag_overflow_err = _do_not_print_diag_overflow_err;
    }

    public int get_raw_data_size() {
        return this.raw_data_size;
    }

    protected int convertMsgToRawData(byte b[], int size, NMLmsg msg) {
        try {
            if (null == b) {
                throw new Exception("convertMsgToRawData: byte array is null");
            }
            rewind();
            decoding = false;
            use_string = false;
            CorrectedPipedInputStream pipe_in = new CorrectedPipedInputStream();
            CorrectedPipedOutputStream pipe_out = new CorrectedPipedOutputStream(pipe_in);
            output_stream = new DataOutputStream(pipe_out);
            msg_to_update = msg;
            msg_type = msg.type;
            msg_size = msg.size;
            bytes_in_input_stream = 0;
            raw_data_size = 0;
            bytes_in_input_stream_known = false;
            start_updates();
            long msg_type_long_in = (long) msg_type;
            long msg_type_long_out = 0;
            msg_type_long_out = update_with_name("type", msg_type_long_in);
            if (msg_type_long_out > Integer.MAX_VALUE || msg_type_long_out <= 0) {
                throw new Exception("Bad msg_type=" + msg_type_long_out);
            }
            msg_type = (int) msg_type_long_out;
            msg_size = update_with_name("size", msg_size);
            if (msg_type > 0 && msg.type == 0) {
                msg.type = msg_type;
            }
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("NMLFormatConverterBase: msg_type_long_in=" + msg_type_long_in + ",msg_type_long_out=" + msg_type_long_out + ", msg.update(this) msg=" + msg + ", this=" + this);
            }
            msg.update(this);
            if (raw_data_size > size) {
                if (!do_not_print_diag_overflow_err) {
                    rcs.nml.debugInfo.debugPrintStream.println("Error: Message is too large or buffer is too small. raw_data_size(" + raw_data_size + ") > size(" + size + ")");
                }
                do_not_print_diag_overflow_err = false;
                return -1;
            }
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("convertMsgToRawData(), Reading " + raw_data_size + " bytes from pipe");
            }
            output_stream.flush();
            for (int i = 0; i < b.length && i < raw_data_size; i++) {
                b[i] = 0;
            }
            pipe_in.read(b, 0, raw_data_size);
            if (debug_on) {
                //rcs.nml.debugInfo.debugPrintStream.println("b = "+new String(b,0,raw_data_size));
            }
            do_not_print_diag_overflow_err = false;
            return raw_data_size;
        } catch (Exception e) {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
        }
        return -1;
    }

    protected String convertMsgToString(NMLmsg msg) {
        try {
            rewind();
            if (null == msg) {
                return null;
            }
            if (null == output_string_buffer) {
                output_string_buffer = new StringBuffer(((int) (4 * msg.size)));
            } else {
                output_string_buffer.ensureCapacity(((int) (4 * msg.size)));
            }
            decoding = false;
            use_string = true;
            msg_to_update = msg;
            msg_type = msg.type;
            msg_size = msg.size;
            bytes_in_input_stream = 0;
            bytes_in_input_stream_known = false;
            start_updates();
            long msg_type_long_in = (long) msg_type;
            long msg_type_long_out = 0;
            msg_type_long_out = update_with_name("type", msg_type_long_in);
            if (msg_type_long_out > Integer.MAX_VALUE || msg_type_long_out <= 0) {
                throw new Exception("Bad msg_type=" + msg_type_long_out);
            }
            msg_type = (int) msg_type_long_out;
            msg_size = update_with_name("size", msg_size);
            output_string_buffer.ensureCapacity(((int) (4 * msg_size)));
            msg.update(this);
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("convertMsgToString, output_string =" + output_string_buffer.toString());
            }
            return output_string_buffer.toString();
        } catch (Exception e) {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
        }
        return null;
    }

    protected NMLmsg getMessageFromInputStream(DataInputStream dis) {
        try {
            rewind();
            input_stream = dis;
            decoding = true;
            use_string = false;
            start_updates();
            long msg_type_long_in = (long) msg_type;
            long msg_type_long_out = 0;
            msg_type_long_out = update_with_name("type", msg_type_long_in);
            if (msg_type_long_out > Integer.MAX_VALUE || msg_type_long_out <= 0) {
                throw new Exception("Bad msg_type=" + msg_type_long_out);
            }
            msg_type = (int) msg_type_long_out;
            msg_size = update_with_name("size", msg_size);
            bytes_in_input_stream = -1;
            if (null == msg_dict) {
                System.err.println("No Message Dictionary!!!");
                return null;
            }
            if (msg_dict.formatMsg(this) < 0) {
                System.err.println("Format Message Error, msg_type = " + msg_type + ", msg_dict = " + msg_dict + ", formatter = " + this);
                if (first_format_error) {
                    Thread.dumpStack();
                    first_format_error = false;
                }
                return null;
            }
            return (NMLmsg) msg_to_update;
        } catch (Exception e) {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
        }
        return null;
    }

    protected int sendMsgToOutputStream(DataOutputStream dos, NMLmsg msg) {
        try {
            rewind();
            decoding = false;
            use_string = false;
            bytes_in_input_stream = 0;
            output_stream = dos;
            msg_to_update = msg;
            start_updates();
            msg.update(this);
            return raw_data_size;
        } catch (Exception e) {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
        }
        return -1;
    }

    public boolean update(boolean x) {
        byte b;
        if (x) {
            b = 1;
        } else {
            b = 0;
        }
        if (debug_on) {
            rcs.nml.debugInfo.debugPrintStream.println("NMLFormatConverterBase(boolean x=" + x + ") : byte b=" + b);
        }
        b = update(b);
        if (b == 0) {
            x = false;
        } else {
            x = true;
        }
        if (debug_on) {
            rcs.nml.debugInfo.debugPrintStream.println("NMLFormatConverterBase(boolean x=" + x + ") : byte b=" + b);
        }
        return x;
    }

    public void update(boolean x[], int num_elements) {
        if (null != x) {
            for (int i = 0; i < x.length; i++) {
                x[i] = update(x[i]);
            }
        }
    }

    public abstract byte update(byte x);

    public abstract void update(byte x[], int num_elements);

    public abstract char update(char x);

    public abstract void update(char x[], int num_elements);

    public abstract short update(short x);

    public abstract void update(short x[], int num_elements);

    public abstract int update(int x);

    public abstract void update(int x[], int num_elements);

    public abstract long update(long x);

    public abstract void update(long x[], int num_elements);

    public abstract float update(float x);

    public abstract void update(float x[], int num_elements);

    public abstract double update(double x);

    public abstract void update(double x[], int num_elements);

    public void beginClass(String name, String base) {
        class_name = name;
        base_class_name = base;
    }

    public void endClass(String name, String base) {
        class_name = null;
        base_class_name = null;
    }

    public void beginBaseClass(String name) {
    }

    public void endBaseClass(String name) {
    }

    public void beginClassVar(String name) {
        class_var_name = class_name;
    }

    public void endClassVar(String name) {
        class_var_name = null;
    }

    public void beginClassArrayElem(String name, int elemnum) {
    }

    public void endClassArrayElem(String name, int elemnum) {
    }

    public int check_type_info(NML_ENUM_INFO info) {
        return (msg_type);
    }

    public int update_enumeration_with_name(String name,
            int enumin,
            NML_ENUM_INFO info) {
        enumin = update_with_name(name, enumin);
        return enumin;
    }

    public void update_enumeration_array_with_name(String name,
            int enumin[], int num_elements,
            NML_ENUM_INFO info) {
        update_with_name(name, enumin, num_elements);
    }

    public int update_attribute_enumeration_with_name(String name,
            int enumin,
            NML_ENUM_INFO info) {
        enumin = update_attribute_with_name(name, enumin);
        return enumin;
    }

    // check unsigned in
    private void cui(long l) throws Exception {
        updating_unsigned = true;
        if (!decoding && !error_in_update && l < 0) {
            SetErrorInUpdate("unsigned value " + l + " is negative");
            throw new Exception("unsigned value " + l + " is negative");
        }
    }

    // check unsigned out
    private void cuo(long l) throws Exception {
        updating_unsigned = true;
        if (check_unsigneds && !decoding && !error_in_update && l < 0) {
            throw new Exception("unsigned value " + l + " is negative");
        }
    }

    // check unsigned in
    private void cuin(String name, long l) throws Exception {
        updating_unsigned = true;
        if (check_unsigneds && !decoding && !error_in_update && l < 0) {
            SetErrorInUpdate("unsigned value " + l + " is negative for variable" + name);
            throw new Exception("unsigned value " + l + " is negative for variable" + name);
        }
    }

    // check unsigned out
    private void cuon(String name, long l) throws Exception {
        updating_unsigned = true;
        if (check_unsigneds && !decoding && !error_in_update && l < 0) {
            throw new Exception("unsigned value " + l + " is negative for variable" + name);
        }
    }

    public byte update_unsigned(byte x) {
        try {
            cui((long) x);
            x = update(x);
            cuo((long) x);
        } catch (Exception e) {
            e.printStackTrace();
            x = 0;
            updating_unsigned = false;
        }
        return (x);
    }

    public void update_unsigned(byte x[], int num_elements) {
        for (int i = 0; i < num_elements; i++) {
            x[i] = update_unsigned(x[i]);
        }
    }

    public char update_unsigned(char x) {
        try {
            cui((long) x);
            x = update(x);
            cuo((long) x);
        } catch (Exception e) {
            e.printStackTrace();
            x = 0;
        } finally {
            updating_unsigned = false;
        }
        return (x);
    }

    public void update_unsigned(char x[], int num_elements) {
        for (int i = 0; i < num_elements; i++) {
            x[i] = update_unsigned(x[i]);
        }
    }

    public short update_unsigned(short x) {
        try {
            cui((long) x);
            x = update(x);
            cuo((long) x);
        } catch (Exception e) {
            e.printStackTrace();
            x = 0;
        } finally {
            updating_unsigned = false;
        }
        return (x);
    }

    public void update_unsigned(short x[], int num_elements) {
        for (int i = 0; i < num_elements; i++) {
            x[i] = update_unsigned(x[i]);
        }
    }

    public int update_unsigned(int x) {
        try {
            cui((long) x);
            x = update(x);
            cuo((long) x);
        } catch (Exception e) {
            e.printStackTrace();
            x = 0;
        } finally {
            updating_unsigned = false;
        }
        return (x);
    }

    public void update_unsigned(int x[], int num_elements) {
        for (int i = 0; i < num_elements; i++) {
            x[i] = update_unsigned(x[i]);
        }
    }

    public long update_unsigned(long x) {
        try {
            cui(x);
            x = update(x);
            cuo(x);
        } catch (Exception e) {
            e.printStackTrace();
            x = 0;
        } finally {
            updating_unsigned = false;
        }
        return (x);
    }

    public void update_unsigned(long x[], int num_elements) {
        for (int i = 0; i < num_elements; i++) {
            x[i] = update_unsigned(x[i]);
        }
    }

    public byte update_unsigned_with_name(String name, byte x) {
        try {
            cuin(name, (long) x);
            x = update_with_name(name, x);
            cuon(name, (long) x);
        } catch (Exception e) {
            e.printStackTrace();
            x = 0;
        } finally {
            updating_unsigned = false;
        }
        return (x);
    }

    public void update_unsigned_with_name(String name, byte x[], int num_elements) {
        try {
            updating_unsigned = true;
            update_with_name(name, x, num_elements);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            updating_unsigned = false;
        }
    }

    public char update_unsigned_with_name(String name, char x) {
        try {
            cuin(name, (long) x);
            x = update_with_name(name, x);
            cuon(name, (long) x);
        } catch (Exception e) {
            e.printStackTrace();
            x = 0;
        } finally {
            updating_unsigned = false;
        }
        return (x);
    }

    public void update_unsigned_with_name(String name, char x[], int num_elements) {
        try {
            updating_unsigned = true;
            update_with_name(name, x, num_elements);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            updating_unsigned = false;
        }
    }

    public short update_unsigned_with_name(String name, short x) {
        try {
            cuin(name, (long) x);
            x = update_with_name(name, x);
            cuon(name, (long) x);
        } catch (Exception e) {
            e.printStackTrace();
            x = 0;
        } finally {
            updating_unsigned = false;
        }
        return (x);
    }

    public void update_unsigned_with_name(String name, short x[], int num_elements) {
        for (int i = 0; i < num_elements; i++) {
            if (add_array_indexes_to_name) {
                x[i] = update_unsigned_with_name(name + "-" + i, x[i]);
            } else {
                x[i] = update_unsigned_with_name(name, x[i]);
            }
        }
    }

    public int update_unsigned_with_name(String name, int x) {
        try {
            cuin(name, (long) x);
            x = update_with_name(name, x);
            cuon(name, (long) x);
        } catch (Exception e) {
            e.printStackTrace();
            x = 0;
        } finally {
            updating_unsigned = false;
        }
        return (x);
    }

    public void update_unsigned_with_name(String name, int x[], int num_elements) {
        for (int i = 0; i < num_elements; i++) {
            if (add_array_indexes_to_name) {
                x[i] = update_unsigned_with_name(name + "-" + i, x[i]);
            } else {
                x[i] = update_unsigned_with_name(name, x[i]);
            }
        }
    }

    public long update_unsigned_with_name(String name, long x) {
        try {
            cuin(name, x);
            x = update_with_name(name, x);
            cuon(name, x);
        } catch (Exception e) {
            e.printStackTrace();
            x = 0;
        } finally {
            updating_unsigned = false;
        }
        return (x);
    }

    public void update_unsigned_with_name(String name, long x[], int num_elements) {
        for (int i = 0; i < num_elements; i++) {
            if (add_array_indexes_to_name) {
                x[i] = update_unsigned_with_name(name + "-" + i, x[i]);
            } else {
                x[i] = update_unsigned_with_name(name, x[i]);
            }
        }
    }

    //    @SuppressWarnings("unchecked")
    public void update_pre_check(String name) {
        try {
            var_name = name;
            var_number++;
            if (save_var_names) {
                if (null == var_name_list) {
                    var_name_list = new Vector();
                }
                var_name_list.add(name);
            }
            if (decoding && !error_in_update && use_string && !input_string_tokenizer.hasMoreTokens()) {
                if (!sending_short) {
                    System.err.println("update_with_name(name=" + name + ") input_string_tokenizer out of tokens\n");
                    Thread.dumpStack();
                }
                SetErrorInUpdate("update_with_name(name=" + name + ") input_string_tokenizer out of tokens\n");
                error_in_update = true;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public boolean update_with_name(String name, boolean x) {
        update_pre_check(name);
        return update(x);
    }

    public void update_with_name(String name, boolean x[], int num_elements) {
        update_pre_check(name);
        if (null != x) {
            for (int i = 0; i < x.length; i++) {
                x[i] = update_with_name(name, x[i]);
            }
        }
    }

    public byte update_with_name(String name, byte x) {
        update_pre_check(name);
        return update(x);
    }

    public void update_with_name(String name, byte x[], int num_elements) {
        update_pre_check(name);
        update(x, num_elements);
    }

    public char update_with_name(String name, char x) {
        update_pre_check(name);
        return update(x);
    }

    public void update_with_name(String name, char x[], int num_elements) {
        update_pre_check(name);
        update(x, num_elements);
    }

    public short update_with_name(String name, short x) {
        update_pre_check(name);
        return update(x);
    }

    public void update_with_name(String name, short x[], int num_elements) {
        update_pre_check(name);
        update(x, num_elements);
    }

    public int update_with_name(String name, int x) {
        update_pre_check(name);
        return update(x);
    }

    public void update_with_name(String name, int x[], int num_elements) {
        update_pre_check(name);
        update(x, num_elements);
    }

    public long update_with_name(String name, long x) {
        update_pre_check(name);
        return update(x);
    }

    public void update_with_name(String name, long x[], int num_elements) {
        update_pre_check(name);
        update(x, num_elements);
    }

    public float update_with_name(String name, float x) {
        update_pre_check(name);
        return update(x);
    }

    public void update_with_name(String name, float x[], int num_elements) {
        update_pre_check(name);
        update(x, num_elements);
    }

    public double update_with_name(String name, double x) {
        update_pre_check(name);
        return update(x);
    }

    public void update_with_name(String name, double x[], int num_elements) {
        update_pre_check(name);
        update(x, num_elements);
    }

    public boolean update_attribute_with_name(String name, boolean x) {
        byte b;
        if (x) {
            b = 0;
        } else {
            b = 1;
        }
        b = update_attribute_with_name(name, b);
        if (b == 0) {
            x = false;
        } else {
            x = true;
        }
        return x;
    }

    public byte update_attribute_with_name(String name, byte x) {
        update_pre_check(name);
        return update(x);
    }

    public void update_attribute_with_name(String name, byte x[], int num_elements) {
        update_pre_check(name);
        update(x, num_elements);
    }

    public char update_attribute_with_name(String name, char x) {
        update_pre_check(name);
        return update(x);
    }

    public short update_attribute_with_name(String name, short x) {
        update_pre_check(name);
        return update(x);
    }

    public int update_attribute_with_name(String name, int x) {
        update_pre_check(name);
        return update(x);
    }

    public long update_attribute_with_name(String name, long x) {
        update_pre_check(name);
        return update(x);
    }

    public float update_attribute_with_name(String name, float x) {
        update_pre_check(name);
        return update(x);
    }

    public double update_attribute_with_name(String name, double x) {
        update_pre_check(name);
        return update(x);
    }

    public void update_attribute_with_name(String name, char x[], int num_elements) {
        update_with_name(name, x, num_elements);
    }

    public void update_attribute_with_name(String name, short x[], int num_elements) {
        update_with_name(name, x, num_elements);
    }

    public void update_attribute_with_name(String name, int x[], int num_elements) {
        update_with_name(name, x, num_elements);
    }

    public void update_attribute_with_name(String name, long x[], int num_elements) {
        update_with_name(name, x, num_elements);
    }

    public void update_attribute_with_name(String name, float x[], int num_elements) {
        update_with_name(name, x, num_elements);
    }

    public void update_attribute_with_name(String name, double x[], int num_elements) {
        update_with_name(name, x, num_elements);
    }

    public int get_length_of_unbounded(String typename, String varname, Object[] oarray) {
        int len = 0;
        if (oarray != null) {
            len = oarray.length;
        }
        if (decoding) {
            if (bytes_in_input_stream < 8 && bytes_in_input_stream_known) {
                return 0;
            }
        }
        len = update_with_name(varname + "_length", len);
        return len;
    }

    public byte[] update_unbounded_with_name(String name, byte[] x) {
        int len = 0;
        if (x != null) {
            len = x.length;
        }
        len = update_with_name(name + "_length", len);
        if (len <= 0) {
            return null;
        }
        if (null == x || len != x.length) {
            x = new byte[len];
        }
        update_with_name(name, x, x.length);
        return (x);
    }

    public short[] update_unbounded_with_name(String name, short[] x) {
        int len = 0;
        if (x != null) {
            len = x.length;
        }
        len = update_with_name(name + "_length", len);
        if (len <= 0) {
            return null;
        }
        if (null == x || len != x.length) {
            x = new short[len];
        }
        update_with_name(name, x, x.length);
        return (x);
    }

    public int[] update_unbounded_with_name(String name, int[] x) {
        int len = 0;
        if (x != null) {
            len = x.length;
        }
        len = update_with_name(name + "_length", len);
        if (len <= 0) {
            return null;
        }
        if (null == x || len != x.length) {
            x = new int[len];
        }
        update_with_name(name, x, x.length);
        return (x);
    }

    public long[] update_unbounded_with_name(String name, long[] x) {
        int len = 0;
        if (x != null) {
            len = x.length;
        }
        len = update_with_name(name + "_length", len);
        if (len <= 0) {
            return null;
        }
        if (null == x || len != x.length) {
            x = new long[len];
        }
        update_with_name(name, x, x.length);
        return (x);
    }

    public byte[] update_unbounded_unsigned_with_name(String name, byte[] x) {
        int len = 0;
//        Thread.dumpStack();
        if (x != null) {
            len = x.length;
        }
        len = update_with_name(name + "_length", len);
        if (len <= 0) {
            return null;
        }
        if (null == x || len != x.length) {
            x = new byte[len];
        }
        update_unsigned_with_name(name, x, x.length);
//        Thread.dumpStack();
        return (x);
    }

    public short[] update_unbounded_unsigned_with_name(String name, short[] x) {
        int len = 0;
        if (x != null) {
            len = x.length;
        }
        len = update_with_name(name + "_length", len);
        if (len <= 0) {
            return null;
        }
        if (null == x || len != x.length) {
            x = new short[len];
        }
        update_unsigned_with_name(name, x, x.length);
        return (x);
    }

    public int[] update_unbounded_unsigned_with_name(String name, int[] x) {
        int len = 0;
        if (x != null) {
            len = x.length;
        }
        len = update_with_name(name + "_length", len);
        if (len <= 0) {
            return null;
        }
        if (null == x || len != x.length) {
            x = new int[len];
        }
        update_unsigned_with_name(name, x, x.length);
        return (x);
    }

    public long[] update_unbounded_unsigned_with_name(String name, long[] x) {
        int len = 0;
        if (x != null) {
            len = x.length;
        }
        len = update_with_name(name + "_length", len);
        if (len <= 0) {
            return null;
        }
        if (null == x || len != x.length) {
            x = new long[len];
        }
        update_unsigned_with_name(name, x, x.length);
        return (x);
    }

    public float[] update_unbounded_with_name(String name, float[] x) {
        int len = 0;
        if (x != null) {
            len = x.length;
        }
        len = update_with_name(name + "_length", len);
        if (len <= 0) {
            return null;
        }
        if (null == x || len != x.length) {
            x = new float[len];
        }
        update_with_name(name, x, x.length);
        return (x);
    }

    public double[] update_unbounded_with_name(String name, double[] x) {
        int len = 0;
        if (x != null) {
            len = x.length;
        }
        len = update_with_name(name + "_length", len);
        if (len <= 0) {
            return null;
        }
        if (null == x || len != x.length) {
            x = new double[len];
        }
        update_with_name(name, x, x.length);
        return (x);
    }

    public int update_dla_length_with_name(String name, int x) {
        if (error_in_update) {
            return 0;
        }
        return update_with_name(name, x);
    }

    public void update_CMS_TIME(CMS_TIME time) {
        time.update_for_non_xml(this);
    }

    public void update_CMS_DATE(CMS_DATE date) {
        date.update_for_non_xml(this);
    }

    public byte[] update_unbounded_attribute_with_name(String name, byte[] x) {
        return update_unbounded_with_name(name, x);
    }

    public void next_update_default(String s) {
        next_default = s;
    }
    
    public long update_ll(long x) {
        System.out.println("x = " + x);
        int xh = (int) ((x >> 32));
        int xl = (int) (x % (1L << 32));
        long xhl = xh;
        long xll = xl;
        xhl = update(xhl);
        xll = update(xll) % (1L << 32);
        x = (xhl << 32) | (xll & (0xFFFFFFFFL));
        return x;
    }
    
    public void update_ll(long x[],int num_elements) {
        for(int i = 0; i < x.length && i < num_elements; i++) {
            x[i] = update_ll(x[i]);
        }
    }
    
    public long update_unsigned_ll(long x) {
        try {
            updating_unsigned = true;
            x = update_ll(x);
        } catch (Exception e) {
            e.printStackTrace();
            x = 0;
        } finally {
            updating_unsigned = false;
        }
        return (x);
    }
    
    
    public void update_unsigned_ll(long x[],int num_elements) {
        for(int i = 0; i < x.length && i < num_elements; i++) {
            x[i] = update_unsigned_ll(x[i]);
        }
    }
    
    public long update_unsigned_ll_with_name(String name, long x) {
        try {
            cuin(name, x);
            x = update_ll_with_name(name, x);
            cuon(name, x);
        } catch (Exception e) {
            e.printStackTrace();
            x = 0;
        } finally {
            updating_unsigned = false;
        }
        return (x);
    }

    public void update_unsigned_ll_with_name(String name, long x[], int num_elements) {
        for (int i = 0; i < num_elements; i++) {
            if (add_array_indexes_to_name) {
                x[i] = update_unsigned_ll_with_name(name + "-" + i, x[i]);
            } else {
                x[i] = update_unsigned_ll_with_name(name, x[i]);
            }
        }
    }
    
    public long update_ll_with_name(String name, long x) {
        try {
            update_pre_check(name);
            x = update_ll(x);
        } catch (Exception e) {
            e.printStackTrace();
            x = 0;
        }
        return (x);
    }

    public void update_ll_with_name(String name, long x[], int num_elements) {
        for (int i = 0; i < num_elements; i++) {
            if (add_array_indexes_to_name) {
                x[i] = update_ll_with_name(name + "-" + i, x[i]);
            } else {
                x[i] = update_ll_with_name(name, x[i]);
            }
        }
    }


}
