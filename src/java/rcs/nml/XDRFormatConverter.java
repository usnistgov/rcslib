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

/**
 * The XDRFormatConverter converts NML message classes to XDR.
 * XDR stands for eXternal Data Representation.
 * Most users should not use it directly.
 *
 * <pre>
 * Related Documentation:
 * <A HREF="http://isd.cme.nist.gov/proj/rcs_lib">RCS Library</a>, <A HREF="http://isd.cme.nist.gov/proj/rcs_lib/NMLjava.html">NML Programmers Guide (Java Version)</a>
 *
 *
 * </pre>
 *
 * @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
 */
public class XDRFormatConverter extends NMLFormatConverterBase {

    public static byte align_bytes[] = new byte[4];
    public static boolean auto_align = true;
    public static boolean debug_on = false;

    public String toString() {
        return super.toString() + " = " + getClass().getName() + " {\n" +
                "auto_align=" + auto_align + ";\n}";
    }

    public void align(int align_factor) throws Exception {
//        if(raw_data_size > 100)
//        {
//        System.out.println("align_factor = " + align_factor);
//        System.out.println("bytes_in_input_stream = " + bytes_in_input_stream);
//        System.out.println("var_name = " + var_name);
//        int bytes_used = raw_data_size - bytes_in_input_stream;
//        System.out.println("bytes_used = " + bytes_used);
//        if(bytes_used > 2000)
//        {
//            System.exit(0);
//        }
//        }
        if (debug_on) {
            rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.XDRFormatConverter.align(" + align_factor + ")");
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
            rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.XDRFormatConverter.waste(" + bytes_to_waste + ")");
        }
        if (!use_string && bytes_to_waste > 0) {
            if (decoding) {
                if (debug_on) {
                    rcs.nml.debugInfo.debugPrintStream.println("bytes_in_input_stream(=" + bytes_in_input_stream + ") -= bytes_to_waste(=" + bytes_to_waste + ")");
                }
                if (bytes_in_input_stream < bytes_to_waste && bytes_in_input_stream_known) {
                    throw new Exception("XDRFormatConverter neads to read " + bytes_to_waste + " but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized., msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name);
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

    public boolean update(boolean x) {
        int xi_in = 0;
        int xi_out = 0;
        if (x) {
            xi_in = 1;
        } else {
            xi_in = 0;
        }
        if (debug_on) {
            rcs.nml.debugInfo.debugPrintStream.println("XDRFormatConverterBase(boolean x=" + x + ") : int xi_in=" + xi_in);
        }
        xi_out = update(xi_in);
        if (xi_out == 0) {
            x = false;
        } else {
            x = true;
        }
        if (debug_on) {
            rcs.nml.debugInfo.debugPrintStream.println("XDRFormatConverterBase(boolean x=" + x + ") : int xi_out=" + xi_out);
        }
        return x;
    }

    public byte update(byte x) {
        try {
            if (first_update_error_occured) {
                return x;
            }
            if (Thread.currentThread().isInterrupted()) {
                first_update_error_occured = true;
                return x;
            }
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < 4 && bytes_in_input_stream_known) {
                        throw new Exception("XDRFormatConverter neads to read 4 bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized., msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name);
                    }
                    if (bytes_in_input_stream >= 4 || !bytes_in_input_stream_known) {
                        x = (byte) input_stream.readInt();
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
                        x = (byte) Integer.valueOf(token).intValue();
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
        // if(debug_on)
//           {
//             rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.XDRFormatConverter.update( byte "+x+")");
//             Thread.dumpStack();
//           }
        } catch (Exception e) {
            if (nfceci != null) {
                nfceci.miscError(msg_dict);
            }
            if (!error_in_update) {
                e.printStackTrace();
                SetErrorInUpdate(e.toString());
            }
        }
        return x;
    }

    public void update(byte x[], int num_elements) {
        int i;
        try {
//             System.out.println("x = " + x);
//             System.out.println("num_elements = " + num_elements);
//            System.out.println("var_name = " + var_name);
//            System.out.println("bytes_in_input_stream = " + bytes_in_input_stream);
            
            if (first_update_error_occured) {
                return;
            }
            if (Thread.currentThread().isInterrupted()) {
                first_update_error_occured = true;
                return;
            }
            align(4);
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < num_elements + 4 && bytes_in_input_stream_known) {
                        throw new Exception("XDRFormatConverter neads to read " + (num_elements + 4) + " bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized., msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name);
                    }
                    if (bytes_in_input_stream >= num_elements + 4 || !bytes_in_input_stream_known) {
                        int num_elements_check = input_stream.readInt();
                        bytes_in_input_stream -= 4;
                        if (num_elements_check != num_elements) {
                            throw new Exception("XDRFormatConverter.update(byte b[],int): num_elements_check(" + num_elements_check + " != num_elements(" + num_elements + ")\nCheck that C++ and Java update functions are synchronized., msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name+", bytes_in_input_stream="+bytes_in_input_stream);
                        }
                        input_stream.read(x, 0, num_elements);
                        bytes_in_input_stream -= (num_elements);
                    }
                } else {
                    output_stream.writeInt(num_elements);
                    if (x.length < num_elements) {
                        output_stream.write(x, 0, x.length);
                        int diff = num_elements - x.length;
                        byte diffbuf[] = new byte[diff];
                        output_stream.write(diffbuf, 0, diff);
                    } else {
                        output_stream.write(x, 0, num_elements);
                    }
                    raw_data_size += num_elements + 4;
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
                            if (first_update_error_occured) {
                                return;
                            }
                            if (Thread.currentThread().isInterrupted()) {
                                first_update_error_occured = true;
                                return;
                            }
                            x[i] = temp_bytes[i];
                        }
                        for (int j = temp_bytes.length; j < x.length; j++) {
                            if (first_update_error_occured) {
                                return;
                            }
                            if (Thread.currentThread().isInterrupted()) {
                                first_update_error_occured = true;
                                return;
                            }
                            x[j] = 0;
                        }
                    }
                } else {
                    for (i = 0; i < num_elements; i++) {
                        if (x[i] == 0) {
                            break;
                        }
                        if (x[i] == ',' || x[i] < 0) {
                            x[i] = '?';
                        }
                    }
                    if (i > 0) {
                        if (debug_on) {
                            rcs.nml.debugInfo.debugPrintStream.println("updating string: length =" + i);
                        }
                        token = new String(x, 0, i);
                        if (debug_on) {
                            rcs.nml.debugInfo.debugPrintStream.println("updating string =" + token);
                        }
                        if (token.length() < 1) {
                            token = "(null)";
                        }
                        output_string_buffer.append(token);
                        output_string_buffer.append(',');
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
            if (!error_in_update) {
                e.printStackTrace();
                SetErrorInUpdate(e.toString());
            }
            if (nfceci != null) {
                nfceci.miscError(msg_dict);
            }
        }
    }

    public char update(char x) {
        try {
            if (first_update_error_occured) {
                return x;
            }
            if (Thread.currentThread().isInterrupted()) {
                first_update_error_occured = true;
                return x;
            }
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < 1 && bytes_in_input_stream_known) {
                        throw new Exception("XDRFormatConverter neads to read 1 bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized., msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name);
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
        // if(debug_on)
//           {
//               rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.XDRFormatConverter.update( char "+x+")");
//             Thread.dumpStack();
//           }
        } catch (Exception e) {
            if (nfceci != null) {
                nfceci.miscError(msg_dict);
            }
            if (!error_in_update) {
                e.printStackTrace();
                SetErrorInUpdate(e.toString());
            }
        }
        return x;
    }

    public void update(char x[], int num_elements) {
        int i;
        try {
            if (first_update_error_occured) {
                return;
            }
            if (Thread.currentThread().isInterrupted()) {
                first_update_error_occured = true;
                return;
            }
            align(4);
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < num_elements + 4 && bytes_in_input_stream_known) {
                        throw new Exception("XDRFormatConverter neads to read " + (num_elements + 4) + " bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized., msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name);
                    }
                    if (bytes_in_input_stream >= num_elements + 4 || !bytes_in_input_stream_known) {
                        int num_elements_check = input_stream.readInt();
                        bytes_in_input_stream -= 4;
                        if (num_elements_check != num_elements) {
                            throw new Exception("XDRFormatConverter.update(char [],int): num_elements_check(" + num_elements_check + " != num_elements(" + num_elements + ")");
                        }
                        for (i = 0; i < num_elements; i++) {
                            x[i] = (char) input_stream.readByte();
                        }
                        bytes_in_input_stream -= (num_elements);
                    }
                } else {
                    output_stream.writeInt(num_elements);
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
                        if (x[i] == ',' || x[i] < 0) {
                            x[i] = '?';
                        }
                    }
                    token = new String(x, 0, i);
                    if (token.length() < 1) {
                        token = "(null)";
                    }
                    output_string_buffer.append(token);
                    output_string_buffer.append(',');
                }
            }
        } catch (Exception e) {
            if (nfceci != null) {
                nfceci.miscError(msg_dict);
            }
            if (!error_in_update) {
                e.printStackTrace();
                SetErrorInUpdate(e.toString());
            }
        }
    }

    public short update(short x) {
        try {
            if (first_update_error_occured) {
                return x;
            }
            if (Thread.currentThread().isInterrupted()) {
                first_update_error_occured = true;
                return x;
            }
            align(4);
            waste(2);
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < 2 && bytes_in_input_stream_known) {
                        throw new Exception("XDRFormatConverter neads to read 2 bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized., msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name);
                    }

                    if (bytes_in_input_stream >= 2 || !bytes_in_input_stream_known) {
                        x = input_stream.readShort();
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
                        x = (short) Integer.valueOf(token).intValue();
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
        // if(debug_on)
//           {
//               rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.XDRFormatConverter.update( short "+x+")");
//             Thread.dumpStack();
//           }
        } catch (Exception e) {
            if (nfceci != null) {
                nfceci.miscError(msg_dict);
            }
            if (!error_in_update) {
                e.printStackTrace();
                SetErrorInUpdate(e.toString());
            }
        }
        return x;
    }

    public void update(short x[], int num_elements) {
        try {
            int i;
            if (first_update_error_occured) {
                return;
            }
            if (Thread.currentThread().isInterrupted()) {
                first_update_error_occured = true;
                return;
            }
            align(4);
            for (i = 0; i < num_elements; i++) {
                if (first_update_error_occured) {
                    return;
                }
                if (Thread.currentThread().isInterrupted()) {
                    first_update_error_occured = true;
                    return;
                }
                x[i] = update(x[i]);
            }
        } catch (Exception e) {
            if (nfceci != null) {
                nfceci.miscError(msg_dict);
            }
            if (!error_in_update) {
                e.printStackTrace();
                SetErrorInUpdate(e.toString());
            }
        }
    }

    public int update(int x) {
        try {
//            System.out.println("x = " + x);
//            System.out.println("var_name = " + var_name);
//            System.out.println("bytes_in_input_stream = " + bytes_in_input_stream);
            if (first_update_error_occured) {
                return x;
            }
            if (Thread.currentThread().isInterrupted()) {
                first_update_error_occured = true;
                return x;
            }
            align(4);
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < 4 && bytes_in_input_stream_known) {
                        throw new Exception("XDRFormatConverter neads to read 4 bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized., msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name);
                    }
                    if (bytes_in_input_stream >= 4 || !bytes_in_input_stream_known) {
                        x = input_stream.readInt();
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
        // if(debug_on)
//           {
//               rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.XDRFormatConverter.update( int "+x+")");
//             Thread.dumpStack();
//           }
        } catch (Exception e) {
            if (nfceci != null) {
                nfceci.miscError(msg_dict);
            }
            if (!error_in_update) {
                e.printStackTrace();
                SetErrorInUpdate(e.toString());
            }
            first_update_error_occured = true;
        }
        return x;
    }

    public void update(int x[], int num_elements) {
        int i;
        for (i = 0; i < num_elements; i++) {
            if (first_update_error_occured) {
                return;
            }
            if (Thread.currentThread().isInterrupted()) {
                first_update_error_occured = true;
                return;
            }
            x[i] = update(x[i]);
        }
    }

    public long update(long x) {
        try {
            if (first_update_error_occured) {
                return x;
            }
            if (Thread.currentThread().isInterrupted()) {
                first_update_error_occured = true;
                return x;
            }
            align(4);
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < 4 && bytes_in_input_stream_known) {
                        throw new Exception("XDRFormatConverter neads to read 4 bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized., msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name);
                    }
                    if (bytes_in_input_stream >= 4 || !bytes_in_input_stream_known) {
                        x = (long) input_stream.readInt();
                        bytes_in_input_stream -= 4;
                    }
                } else {
                    output_stream.writeInt((int) x);
                    raw_data_size += 4;
                }
            } else {
                String token;
                if (decoding) {
                    token = input_string_tokenizer.nextToken();
                    if (token != null) {
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
        // if(debug_on)
//           {
//               rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.XDRFormatConverter.update( long "+x+")");
//             Thread.dumpStack();
//           }
        } catch (Exception e) {
            if (nfceci != null) {
                nfceci.miscError(msg_dict);
            }
            if (!error_in_update) {
                e.printStackTrace();
                SetErrorInUpdate(e.toString());
            }
        }
        return x;
    }

    public void update(long x[], int num_elements) {
        int i;
        for (i = 0; i < num_elements; i++) {
            if (first_update_error_occured) {
                return;
            }
            if (Thread.currentThread().isInterrupted()) {
                first_update_error_occured = true;
                return;
            }
            x[i] = update(x[i]);
        }
    }

    public float update(float x) {
        try {
            if (first_update_error_occured) {
                return x;
            }
            if (Thread.currentThread().isInterrupted()) {
                first_update_error_occured = true;
                return x;
            }
            align(4);
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < 4 && bytes_in_input_stream_known) {
                        throw new Exception("XDRFormatConverter neads to read 4 bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized., msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name);
                    }
                    if (bytes_in_input_stream >= 4 || !bytes_in_input_stream_known) {
                        x = input_stream.readFloat();
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
        //  if(debug_on)
//           {
//               rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.XDRFormatConverter.update( float "+x+")");
//             Thread.dumpStack();
//           }
        } catch (Exception e) {
            if (nfceci != null) {
                nfceci.miscError(msg_dict);
            }
            if (!error_in_update) {
                e.printStackTrace();
                SetErrorInUpdate(e.toString());
            }
        }
        return x;
    }

    public void update(float x[], int num_elements) {
        int i;
        for (i = 0; i < num_elements; i++) {
            if (first_update_error_occured) {
                return;
            }
            if (Thread.currentThread().isInterrupted()) {
                first_update_error_occured = true;
                return;
            }
            x[i] = update(x[i]);
        }
    }

    public double update(double x) {
        try {
            if (first_update_error_occured) {
                return x;
            }
            if (Thread.currentThread().isInterrupted()) {
                first_update_error_occured = true;
                return x;
            }
            align(4);
            if (!use_string) {
                if (decoding) {
                    if (bytes_in_input_stream < 8 && bytes_in_input_stream_known) {
                        throw new Exception("XDRFormatConverter neads to read 8 bytes but only " + bytes_in_input_stream + " bytes are available. \nCheck that C++ and Java update functions are synchronized., msg_size=" + msg_size + ",raw_data_size=" + raw_data_size + ",var_name=" + var_name);
                    }

                    if (bytes_in_input_stream >= 8 || !bytes_in_input_stream_known) {
                        x = input_stream.readDouble();
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
        // if(debug_on)
//           {
//               rcs.nml.debugInfo.debugPrintStream.println("rcs.nml.XDRFormatConverter.update( double "+x+")");
//             Thread.dumpStack();
//           }
        } catch (Exception e) {
            if (nfceci != null) {
                nfceci.miscError(msg_dict);
            }
            if (!error_in_update) {
                e.printStackTrace();
                SetErrorInUpdate(e.toString());
            }
        }
        return x;
    }

    public void update(double x[], int num_elements) {
        int i;
        for (i = 0; i < num_elements; i++) {
            if (first_update_error_occured) {
                return;
            }
            if (Thread.currentThread().isInterrupted()) {
                first_update_error_occured = true;
                return;
            }
            x[i] = update(x[i]);
        }
    }
}
