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

import java.util.Arrays;

/**
 * The PackedFormatConverter converts NML message classes to Packed.
 * Most users should not use it directly.
 *
 * <pre>
 * Related Documentation:
 * <A HREF="http://isd.cme.nist.gov/proj/rcs_lib">RCS Library</a>, <A HREF="http://isd.cme.nist.gov/proj/rcs_lib/NMLjava.html">NML Programmers Guide (Java Version)</a>
 *
 * </pre>
 *
 * @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
 */
public class PackedFormatConverter extends NMLFormatConverterBase {

    public static byte align_bytes[] = new byte[4];
    public static boolean auto_align = false;
    boolean data_is_little_endian = true;
    public boolean data_has_64bit_longs = false;
    public boolean l64_mode = false;

    public PackedFormatConverter(boolean _l64_mode) {
        super();
        l64_mode = _l64_mode;
    }

    public void HandleErrorInUpdate(Exception e) {
        if (hide_errors) {
            error_in_update = true;
            return;
        }
        if (nfceci != null) {
            nfceci.miscError(msg_dict, e);
        } else if (!error_in_update) {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            if (System.err != rcs.nml.debugInfo.debugPrintStream) {
                e.printStackTrace();
            }
        }
        if (!error_in_update) {
            SetErrorInUpdate(e.toString());
            if (null != var_name_list) {
                System.err.println("var_name_list=" + var_name_list);
            }
            System.err.println("var_number=" + var_number);
            System.err.println("class_name=" + class_name);
            System.err.println("class_var_name=" + class_var_name);
            System.err.println("var_name=" + this.var_name);
            error_in_update = true;
        }
    }

    public String toString() {
        return super.toString() + " = " + getClass().getName() + " {\n"
                + "auto_align=" + auto_align + ";\n}";
    }

    public void align(int align_factor) throws Exception {
        if (false) {
            rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.PackedFormatConverter.align(" + align_factor + ")");
        }
        if (!use_string && auto_align) {
            if (decoding) {
                int bytes_to_waste = align_factor - ((raw_data_size - bytes_in_input_stream) % align_factor);
                if (debug_on) {
                    rcs.nml.debugInfo.debugPrintStream.println("int bytes_to_waste(=" + bytes_to_waste + ") = align_factor(=" + align_factor + ") - ((raw_data_size(=" + raw_data_size + ") - bytes_in_input_stream(=" + bytes_in_input_stream + "))%align_factor(=" + align_factor + "));");
                }

                if (bytes_to_waste < align_factor && bytes_to_waste > 0) {
                    waste(bytes_to_waste);
                }
            } else {
                int bytes_to_waste = align_factor - (raw_data_size % align_factor);
                if (debug_on) {
                    rcs.nml.debugInfo.debugPrintStream.println("int bytes_to_waste(=" + bytes_to_waste + ") = align_factor(=" + align_factor + ") - (raw_data_size(=" + raw_data_size + ")%align_factor(=" + align_factor + "));");
                }
                if (bytes_to_waste < align_factor && bytes_to_waste > 0) {
                    waste(bytes_to_waste);
                }
            }
        }
    }

    public void waste(int bytes_to_waste) throws Exception {
        if (debug_on) {
            rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.PackedFormatConverter.waste(" + bytes_to_waste + ")");
        }
        if (!use_string && bytes_to_waste > 0) {
            if (decoding) {
                if (debug_on) {
                    rcs.nml.debugInfo.debugPrintStream.println("bytes_in_input_stream(=" + bytes_in_input_stream + ") -= bytes_to_waste(=" + bytes_to_waste + ")");
                }
                if (bytes_in_input_stream < bytes_to_waste && bytes_in_input_stream_known) {
                    throw new Exception("PackedFormatConverter neads to read " + bytes_to_waste + " but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized. , msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name + ",bufName=" + bufName);
                }
                input_stream.read(align_bytes, 0, bytes_to_waste);
                bytes_in_input_stream -= bytes_to_waste;
            } else {
                raw_data_size += bytes_to_waste;
                if (debug_on) {
                    rcs.nml.debugInfo.debugPrintStream.println("raw_data_size(=" + raw_data_size + ") += bytes_to_waste(=" + bytes_to_waste + ")");
                }
                output_stream.write(align_bytes, 0, bytes_to_waste);
            }
        }
    }

    public void start_updates() {
        try {
            super.start_updates();
            error_in_update = false;
            data_is_little_endian = false;
            if (!use_string) {
                byte b;
                if (decoding) {
                    if (bytes_in_input_stream < 1 && bytes_in_input_stream_known) {
                        throw new Exception("PackedFormatConverter neads to read 4 bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized. , msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name + ",bufName=" + bufName);
                    }
                    b = input_stream.readByte();
                    switch (b) {
                        case 'L':
                            data_is_little_endian = true;
                            data_has_64bit_longs = false;
                            break;

                        case 'B':
                            data_is_little_endian = false;
                            data_has_64bit_longs = false;
                            break;

                        case 'l':
                            data_is_little_endian = true;
                            data_has_64bit_longs = true;
                            break;

                        case 'b':
                            data_is_little_endian = false;
                            data_has_64bit_longs = true;
                            break;

                        default:
                            int ib = (int) b;
                            char cb = (char) b;
                            throw new Exception("Bad endian indicating byte = " + ib + " or " + cb);
                    }
                    bytes_in_input_stream--;
                    if (debug_on) {
                        rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.PackedFormatConvert : b=" + b + ", data_is_little_endian=" + data_is_little_endian + ", bytes_in_input_stream=" + bytes_in_input_stream);
                    }
                } else {
                    if (l64_mode) {
                        b = (byte) 'b';
                        data_has_64bit_longs = true;
                    } else {
                        b = (byte) 'B';
                        data_has_64bit_longs = false;
                    }
                    output_stream.writeByte(b);
                    raw_data_size++;
                }
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
    }

    public boolean update(boolean x) {
        byte xb_in = 0;
        byte xb_out = 0;
        if (x) {
            xb_in = 1;
        } else {
            xb_in = 0;
        }
        if (debug_on) {
            rcs.nml.debugInfo.debugPrintStream.println("PackedFormatConverterBase(boolean x=" + x + ") : byte xb_in=" + xb_in);
        }
        xb_out = update(xb_in);
        if (xb_out == 0) {
            x = false;
        } else {
            x = true;
        }
        if (debug_on) {
            rcs.nml.debugInfo.debugPrintStream.println("PackedFormatConverterBase(boolean x=" + x + ") : byte xb_out=" + xb_out);
        }
        return x;
    }

    public byte update(byte x) {
        byte x_out = x;
        try {
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("x_out = " + x_out + " = rcs.nml.PackedFormatConverter.update( byte " + x + ") bytes_in_input_stream=" + bytes_in_input_stream + ", raw_data_size=" + raw_data_size + ", use_string=" + use_string + ", decoding=" + decoding);
            }
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < 1 && bytes_in_input_stream_known) {
                        throw new Exception("PackedFormatConverter neads to read 1 byte but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized. , msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name + ",bufName=" + bufName);
                    }
                    if (bytes_in_input_stream >= 1 || !bytes_in_input_stream_known) {
                        x_out = input_stream.readByte();
                        bytes_in_input_stream -= 1;
                    }
                } else {
                    output_stream.writeByte(x);
                    raw_data_size++;
                }
            } else {
                String token;
                if (decoding) {
                    token = input_string_tokenizer.nextToken();
                    if (debug_on) {
                        rcs.nml.debugInfo.debugPrintStream.println("token=" + token);
                    }
                    if (token != null) {
                        if (token.endsWith(".0")) {
                            token = token.substring(0, token.length() - 2);
                        }
                        x_out = Byte.valueOf(token).byteValue();
                    }
                } else {
                    if (updating_unsigned && x < 0) {
                        int iux = ((int) x) + 256;
                        output_string_buffer.append(iux);
                        output_string_buffer.append(',');
                    } else {
                        int xi = x;
                        output_string_buffer.append(xi);
                        output_string_buffer.append(',');
                    }
                }
            }
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("x_out = " + x_out + " = rcs.nml.PackedFormatConverter.update( byte " + x + ") bytes_in_input_stream=" + bytes_in_input_stream + ", raw_data_size=" + raw_data_size + ", output_string_buffer=" + output_string_buffer);
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
        return x_out;
    }

    public void update(byte x[], int num_elements) {
        int i;
        try {
            if (num_elements == 0) {
                return;
            }
            if (num_elements < 0) {
                throw new Exception("PackedFormatConverter num_elements = " + num_elements);
            }
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < num_elements && bytes_in_input_stream_known) {
                        throw new Exception("PackedFormatConverter neads to read " + (num_elements + 4) + " bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized. , msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name + ",bufName=" + bufName);
                    }
                    if (bytes_in_input_stream >= num_elements || !bytes_in_input_stream_known) {
                        try {
                            input_stream.read(x, 0, num_elements);
                        } catch (Exception e) {
                            System.err.println("update(byte x[],int num_elements) failed. -- x.length=" + x.length + ", num_elements=" + num_elements + ", bytes_in_input_stream=" + bytes_in_input_stream);
                            throw e;
                        }
                        bytes_in_input_stream -= (num_elements);
                    }
                } else {
                    if (x.length < num_elements) {
                        output_stream.write(x, 0, x.length);
                        int diff = num_elements - x.length;
                        byte diffbuf[] = new byte[diff];
                        output_stream.write(diffbuf, 0, diff);
                    } else {
                        output_stream.write(x, 0, num_elements);
                    }
                    raw_data_size += num_elements;
                }
            } else {
                String token;
                if (decoding) {
                    token = input_string_tokenizer.nextToken();
                    if (token != null) {
                        int bytes_to_get = token.length();
                        if (bytes_to_get >= num_elements) {
                            bytes_to_get = num_elements;
                        }
                        byte temp_bytes[] = token.getBytes();
                        for (i = 0; i < x.length && i < temp_bytes.length; i++) {
                            x[i] = temp_bytes[i];
                        }
                        for (int j = temp_bytes.length; j < x.length; j++) {
                            x[j] = 0;
                        }
                    }
                } else {
                    for (i = 0; i < num_elements; i++) {
                        if (x[i] == 0) {
                            break;
                        }
                    }
                    if (i > 0) {
                        if (debug_on) {
                            rcs.nml.debugInfo.debugPrintStream.println("updating string: length =" + i);
                        }
                        if (diagnostics_mode && i > this.diagnostics_mode_string_max) {
                            token = new String(x, 0, this.diagnostics_mode_string_max);
                        } else {
                            token = new String(x, 0, i);
                        }
                        if (this.diagnostics_mode) {
                            if (token.length() > 512) {
                                token = token.substring(0, 512);
                            }
                            for (int tp = 0; tp < token.length(); tp++) {
                                char tc = token.charAt(tp);
                                if (updating_unsigned
                                        || (!Character.isLetter(tc) && !Character.isDigit(tc)
                                        && punct_string.indexOf(tc) < 0)) {
                                    String escape_string = "\\0" + Integer.toOctalString(((int) tc) & 0xff);
                                    token = token.substring(0, tp) + escape_string + token.substring(tp + 1);
                                    tp += escape_string.length() - 1;
                                }
                            }
                        }
                        if (debug_on) {
                            rcs.nml.debugInfo.debugPrintStream.println("updating string =" + token);
                        }
                        if (token.length() < 1) {
                            output_string_buffer.append("(null),");
                        } else {
                            output_string_buffer.append(token);
                            output_string_buffer.append(',');
                        }
                    } else {
                        output_string_buffer.append("(null),");
                    }
                }
            }
            // if(debug_on)
//           {
//               rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.XDRFormatConverter.update( byte [] "+x+" \""+(new String(x,0,num_elements))+"\" , num_elements="+num_elements+")");
// 	      Thread.dumpStack();
//           }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
    }

    public char update(char x) {
        try {
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < 1 && bytes_in_input_stream_known) {
                        throw new Exception("PackedFormatConverter neads to read 1 bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized. , msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name + ",bufName=" + bufName);
                    }

                    if (bytes_in_input_stream >= 1 || !bytes_in_input_stream_known) {
                        x = (char) input_stream.readByte();
                        bytes_in_input_stream--;
                    }
                } else {
                    output_stream.writeByte(((byte) x));
                    raw_data_size++;
                }
            } else {
                String token;
                if (decoding) {
                    token = input_string_tokenizer.nextToken();
                    if (token != null) {
                        char ch_array[] = new char[1];
                        token.getChars(0, 1, ch_array, 0);
                        x = ch_array[0];
                    }
                } else {
                    if (updating_unsigned && x < 0) {
                        int iux = ((int) x) + 256;
                        output_string_buffer.append(iux);
                        output_string_buffer.append(',');
                    } else {
                        output_string_buffer.append(x);
                        output_string_buffer.append(',');
                    }
                }
            }
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.PackedFormatConverter.update( char " + x + ")");
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
        return x;
    }
    private static final String punct_string = new String("~!@#$%^&*(),./<>?;\':\\\"[]{}|-=_+ \t");

    public void update(char x[], int num_elements) {
        int i;
        try {
            if (num_elements == 0) {
                return;
            }
            if (num_elements < 0) {
                throw new Exception("PackedFormatConverter num_elements = " + num_elements);
            }
            align(4);
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < num_elements && bytes_in_input_stream_known) {
                        throw new Exception("PackedFormatConverter neads to read " + (num_elements + 4) + " bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized. , msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name + ",bufName=" + bufName);
                    }
                    if (bytes_in_input_stream >= num_elements || !bytes_in_input_stream_known) {
                        for (i = 0; i < num_elements; i++) {
                            x[i] = (char) input_stream.readByte();
                        }
                        bytes_in_input_stream -= (num_elements);
                    }
                } else {
                    for (i = 0; i < num_elements; i++) {
                        output_stream.writeByte(((byte) (0xff & x[i])));
                    }
                    raw_data_size += num_elements + 4;
                }
            } else {
                String token;
                if (decoding) {
                    token = input_string_tokenizer.nextToken();
                    if (token != null) {
                        int chars_to_get = token.length();
                        if (chars_to_get >= num_elements) {
                            chars_to_get = num_elements;
                        }
                        token.getChars(0, chars_to_get, x, 0);
                        for (int j = chars_to_get; j < x.length; j++) {
                            x[j] = 0;
                        }
                    }
                } else {
                    for (i = 0; i < num_elements; i++) {
                        if (x[i] == 0) {
                            break;
                        }
                    }
                    if (diagnostics_mode && i > this.diagnostics_mode_string_max) {
                        token = new String(x, 0, this.diagnostics_mode_string_max);
                    } else {
                        token = new String(x, 0, i);
                    }
                    if (this.diagnostics_mode) {
                        if (token.length() > 512) {
                            token = token.substring(0, 512);
                        }
                        for (int tp = 0; tp < token.length(); tp++) {
                            char tc = token.charAt(tp);
                            if (updating_unsigned || (!Character.isLetter(tc) && !Character.isDigit(tc)
                                    && punct_string.indexOf(tc) < 0)) {
                                String escape_string = "\\0" + Integer.toOctalString(((int) tc) & 0xff);
                                token = token.substring(0, tp) + escape_string + token.substring(tp + 1);
                                tp += escape_string.length() - 1;
                            }
                        }
                    }
                    if (token.length() < 1) {
                        output_string_buffer.append("(null),");
                    } else {
                        output_string_buffer.append(token);
                        output_string_buffer.append(',');
                    }
                }
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
    }

    public short update(short x) {
        try {
            align(4);
            //waste(2);
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < 2 && bytes_in_input_stream_known) {
                        throw new Exception("PackedFormatConverter neads to read 2 bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized. , msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name + ",bufName=" + bufName);
                    }

                    if (bytes_in_input_stream >= 2 || !bytes_in_input_stream_known) {
                        if (data_is_little_endian) {
                            byte ba[] = new byte[2];
                            input_stream.readFully(ba, 0, 2);
                            x = (short) ((ba[1] << 8) | (ba[0] & 0xff));
                        } else {
                            x = input_stream.readShort();
                        }
                        bytes_in_input_stream -= 2;
                    }
                } else {
                    output_stream.writeShort(x);
                    raw_data_size += 2;
                }
            } else {
                String token;
                if (decoding) {
                    token = input_string_tokenizer.nextToken();
                    if (token != null) {
                        if (token.endsWith(".0")) {
                            token = token.substring(0, token.length() - 2);
                        }
                        x = Short.valueOf(token).shortValue();
                    }
                } else {
                    if (updating_unsigned && x < 0) {
                        int iux = x + 65536;
                        output_string_buffer.append(iux);
                        output_string_buffer.append(',');
                    } else {
                        output_string_buffer.append(x);
                        output_string_buffer.append(',');
                    }
                }
            }
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.PackedFormatConverter.update( short " + x + ")");
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
        return x;
    }

    public void update(short x[], int num_elements) {
        try {
            int i;
            align(4);
            for (i = 0; i < num_elements; i++) {
                if (Thread.currentThread().isInterrupted()) {
                    throw new InterruptedException();
                }
                x[i] = update(x[i]);
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
    }

    public int update(int x) {
        try {
            align(4);
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < 4 && bytes_in_input_stream_known) {
                        throw new Exception("PackedFormatConverter neads to read 4 bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized. , msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name + ",bufName=" + bufName);
                    }
                    if (bytes_in_input_stream >= 4 || !bytes_in_input_stream_known) {
                        if (data_is_little_endian) {
                            byte ba[] = new byte[4];
                            input_stream.readFully(ba, 0, 4);
                            x = (((int) ba[3] & 0xFF) << 24) | (((int) ba[2] & 0xFF) << 16) | (((int) ba[1] & 0xFF) << 8) | ((int) ba[0] & 0xFF);
                        } else {
                            x = input_stream.readInt();
                        }
                        bytes_in_input_stream -= 4;
                    }
                } else {
                    output_stream.writeInt(x);
                    raw_data_size += 4;
                }
            } else {
                String token;
                if (decoding) {
                    token = input_string_tokenizer.nextToken();
                    if (token != null) {
                        if (token.endsWith(".0")) {
                            token = token.substring(0, token.length() - 2);
                        }
                        x = Integer.valueOf(token).intValue();
                    }
                } else {
                    if (updating_unsigned && x < 0) {
                        long lux = x + 4294967296L;
                        output_string_buffer.append(lux);
                        output_string_buffer.append(',');
                    } else {
                        output_string_buffer.append(x);
                        output_string_buffer.append(',');
                    }
                }
            }
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.PackedFormatConverter.update( int " + x + ")");
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
            x=0;
        }
        return x;
    }

    public void update(int x[], int num_elements) {
        try {
            int i;
            for (i = 0; i < num_elements; i++) {
                if (Thread.currentThread().isInterrupted()) {
                    throw new InterruptedException();
                }
                x[i] = update(x[i]);
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
    }

    private long l64_update(long x) {
        try {
            long orig_x = 0;
            if (debug_on) {
                orig_x = x;
            }
            align(8);
            if (decoding) {
                if (bytes_in_input_stream < 8 && bytes_in_input_stream_known) {
                    throw new Exception("PackedFormatConverter neads to read 8 bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized. , msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name + ",bufName=" + bufName);
                }
                if (bytes_in_input_stream >= 8 || !bytes_in_input_stream_known) {
                    if (data_is_little_endian) {
                        byte ba[] = new byte[8];
                        input_stream.readFully(ba, 0, 8);
                        x = (((long) ba[7] & 0xFF) << 56) | (((long) ba[6] & 0xFF) << 48) | (((long) ba[5] & 0xFF) << 40) | (((long) ba[4] & 0xFF) << 32) | (((long) ba[3] & 0xFF) << 24) | (((long) ba[2] & 0xFF) << 16) | (((long) ba[1] & 0xFF) << 8) | ((long) ba[0] & 0xFF);
                    } else {
                        x = input_stream.readLong();
                    }
                    bytes_in_input_stream -= 8;
                }
            } else {
                output_stream.writeLong(x);
                raw_data_size += 8;
            }
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.PackedFormatConverter.l64_update( long " + x + ") {orig_x=" + orig_x + ", bytes_in_input_stream=" + bytes_in_input_stream + ", raw_data_size=" + raw_data_size + "}");
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
        return (x);
    }

    public long update(long x) {
        try {
            if (data_has_64bit_longs && !use_string) {
                return l64_update(x);
            }
            if (!use_string) {
                long x_orig = x;
                int ixin = (int) x;
                int ixout = 0;
                ixout = update(ixin);
                x = (long) ixout;
                if ((x_orig > Integer.MAX_VALUE || x_orig < Integer.MIN_VALUE) && !decoding) {
                    throw new Exception("64bit long(" + x_orig + ") was too big to be encoded in 32bits");
                }
            } else {
                String token;
                if (decoding) {
                    token = input_string_tokenizer.nextToken();
                    if (token != null) {
                        if (token.endsWith(".0")) {
                            token = token.substring(0, token.length() - 2);
                        }
                        x = Long.valueOf(token).longValue();
                    }
                } else {
                    if (updating_unsigned && x < 0) {
                        long lux = (x) + 4294967296L;
                        output_string_buffer.append(lux);
                        output_string_buffer.append(',');
                    } else {
                        output_string_buffer.append(x);
                        output_string_buffer.append(',');
                    }
                }
            }
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.PackedFormatConverter.update( long " + x + ")");
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
        return x;
    }

    public void update(long x[], int num_elements) {
        try {
            int i;
            for (i = 0; i < num_elements; i++) {
                if (Thread.currentThread().isInterrupted()) {
                    throw new InterruptedException();
                }
                x[i] = update(x[i]);
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
    }

    public float update(float x) {
        try {
            align(4);
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < 4 && bytes_in_input_stream_known) {
                        throw new Exception("PackedFormatConverter neads to read 4 bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized. , msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name + ",bufName=" + bufName);
                    }
                    if (bytes_in_input_stream >= 4 || !bytes_in_input_stream_known) {
                        if (data_is_little_endian) {
                            byte ba[] = new byte[4];
                            int bits;
                            input_stream.readFully(ba, 0, 4);
                            bits = (((int) ba[3] & 0xFF) << 24) | (((int) ba[2] & 0xFF) << 16) | (((int) ba[1] & 0xFF) << 8) | ((int) ba[0] & 0xFF);
                            x = Float.intBitsToFloat(bits);
                        } else {
                            x = input_stream.readFloat();
                        }
                        bytes_in_input_stream -= 4;
                    }
                } else {
                    output_stream.writeFloat(x);
                    raw_data_size += 4;
                }
            } else {
                String token;
                if (decoding) {
                    token = input_string_tokenizer.nextToken();
                    if (token != null) {
                        x = Float.valueOf(token).floatValue();
                    }
                } else {
                    output_string_buffer.append(x);
                    output_string_buffer.append(',');
                }
            }
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.PackedFormatConverter.update( float " + x + ")");
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
        return x;
    }

    public void update(float x[], int num_elements) {
        try {
            if (num_elements > x.length) {
                num_elements = x.length;
            }
            if (use_string) {
                for (int i = 0; i < num_elements; i++) {
                    x[i] = update(x[i]);
                }
                return;
            }
            final int sz = 4 * num_elements;
            align(4);
            if (decoding) {
                if (bytes_in_input_stream < sz && bytes_in_input_stream_known) {
                    throw new Exception("PackedFormatConverter neads to read " + sz + " bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized. , msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name + ",bufName=" + bufName);
                }
                if (data_is_little_endian) {
                    byte ba[] = new byte[sz];
                    input_stream.readFully(ba, 0, sz);
                    for (int i = 0; i < num_elements; i++) {
                        int bits;
                        int off = i * 4;
                        bits = (((int) ba[off + 3] & 0xFF) << 24) | (((int) ba[off + 2] & 0xFF) << 16) | (((int) ba[off + 1] & 0xFF) << 8) | ((int) ba[off + 0] & 0xFF);
                        x[i] = Float.intBitsToFloat(bits);
                    }
                } else {
                    for (int i = 0; i < num_elements; i++) {
                        x[i] = input_stream.readFloat();
                    }
                }
                bytes_in_input_stream -= sz;
            } else {
                for (int i = 0; i < num_elements; i++) {
                    output_stream.writeFloat(x[i]);
                }
                raw_data_size += sz;
            }
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.PackedFormatConverter.update( float " + x + ")");
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
    }

    public double update(double x) {
        try {
            align(4);
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < 8 && bytes_in_input_stream_known) {
                        throw new Exception("PackedFormatConverter neads to read 8 bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized. , msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name + ",bufName=" + bufName);
                    }
                    if (bytes_in_input_stream >= 8 || !bytes_in_input_stream_known) {
                        if (data_is_little_endian) {
                            byte ba[] = new byte[8];
                            long bits;
                            input_stream.readFully(ba, 0, 8);
                            bits = (((long) ba[7] & 0xFF) << 56) | (((long) ba[6] & 0xFF) << 48) + (((long) ba[5] & 0xFF) << 40) | (((long) ba[4] & 0xFF) << 32) | (((long) ba[3] & 0xFF) << 24) | (((long) ba[2] & 0xFF) << 16) + (((long) ba[1] & 0xFF) << 8) | ((long) ba[0] & 0xFF);
                            x = Double.longBitsToDouble(bits);
                        } else {
                            x = input_stream.readDouble();
                        }
                        bytes_in_input_stream -= 8;
                    }
                } else {
                    output_stream.writeDouble(x);
                    raw_data_size += 8;
                }
            } else {
                String token;
                if (decoding) {
                    token = input_string_tokenizer.nextToken();
                    if (token != null) {
                        x = Double.valueOf(token).doubleValue();
                    }
                } else {
                    output_string_buffer.append(x);
                    output_string_buffer.append(',');
                }
            }
            if (debug_on) {
                rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.PackedFormatConverter.update( double " + x + ")");
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
        return x;
    }

    public void update(double x[], int num_elements) {
        try {
            int i;
            for (i = 0; i < num_elements; i++) {
                if (Thread.currentThread().isInterrupted()) {
                    throw new InterruptedException();
                }
                x[i] = update(x[i]);
            }
        } catch (Exception e) {
            HandleErrorInUpdate(e);
        }
    }

    public byte[] convertMsgToRawData(NMLmsg _msg) throws Exception {
        byte raw_data[] = null;
        int raw_data_estimated_size = (int) this.GetMessageDictionary().getEstimatedSize(_msg.type);
        raw_data = new byte[raw_data_estimated_size];
        this.set_do_not_print_diag_overflow_err(true);
        int len = this.convertMsgToRawData(raw_data, raw_data.length, _msg);
        while (len == -1 && this.get_raw_data_size() > raw_data_estimated_size) {
            raw_data_estimated_size = this.get_raw_data_size();
//            System.out.println("raw_data_estimated_size = " + raw_data_estimated_size);
            raw_data = new byte[raw_data_estimated_size];
            this.set_do_not_print_diag_overflow_err(true);
            len = this.convertMsgToRawData(raw_data, raw_data.length, _msg);
        }
        raw_data = Arrays.copyOf(raw_data, this.get_raw_data_size());
        return raw_data;
    }
}
