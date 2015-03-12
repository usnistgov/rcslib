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

class nmlperf {

    public boolean read_test = false;
    public boolean write_test = false;
    public boolean add_delay = false;
    public boolean debug_on = false;

    static public void main(String args[]) {
        nmlperf nmlperf_obj = new nmlperf();
        nmlperf_obj.run_test(args);
    }

    public void run_test(String args[]) {
        rcs.nml.debugInfo.debugPrintStream.println(" NML Performance Test (Java version) $Id: nmlperf.java 1603 2010-03-19 20:41:36Z shackle $");
        //rcs.nml.debugInfo.debugPrintStream.println(rcs.RCS_VERSION.info_string);
        rcs.nml.debugInfo.debugPrintStream.println("Press <ENTER> or <RETURN> to end the test and view results.");
        for (int i = 0; i < args.length; i++) {
            if (args[i].toUpperCase().startsWith("-DEBUG")) {

                //NMLConnection.set_config_debug_on(true);

//            NMLConnection.set_read_debug_on(true);
//            NMLConnection.set_write_debug_on(true);
                NMLFormatConverterBase.debug_on = true;
                NML_PERFORMANCE_TEST_MSG.debug_on = true;
                debug_on = true;
            }
            if (args[i].toUpperCase().startsWith("-READ")) {
                read_test = true;
            }
            if (args[i].toUpperCase().startsWith("-WRITE")) {
                write_test = true;
            }
            if (args[i].toUpperCase().startsWith("-DELAY")) {
                add_delay = true;
            }
        }

        if (!write_test && !read_test) {
            rcs.nml.debugInfo.debugPrintStream.println("Usage: [-WRITE|-READ] <-DEBUG> <-DELAY>");
            System.exit(-1);
        }
        long start_millis = System.currentTimeMillis();
        long messages_received = 0;
        long messages_sent = 0;
        long null_returns = 0;
        long cycles = 0;
        long max_millis = 0;
        long min_millis = 2 ^ 31;
        NML_PERFORMANCE_TEST_MSG.default_array_length = 2000;
        NML_PERFORMANCE_TEST_MSG testMsgToSend = new NML_PERFORMANCE_TEST_MSG();
        NML_PERFORMANCE_TEST_MSG testMsgRead = null;

        NMLConnection nml_for_read = null;
        NMLConnection nml_for_write = null;

        try {
            if (read_test) {
                nml_for_read = new NMLConnection(new perftypeMsgDict(), "testbuf", "nmlperf_java_read", "test.nml");
                if (debug_on) {
                    nml_for_read.set_read_debug_on(debug_on);
                }
            }
            if (write_test) {
                nml_for_write = new NMLConnection(new perftypeMsgDict(), "testbuf", "nmlperf_java_write", "test.nml");
                if (debug_on) {
                    nml_for_write.set_write_debug_on(debug_on);
                }
            }
            rcs.nml.debugInfo.debugPrintStream.println("");
            rcs.nml.debugInfo.debugPrintStream.println("Starting test(s) . . .");
            rcs.nml.debugInfo.debugPrintStream.println("");
            while (read_test || write_test) {
                long amillis = System.currentTimeMillis();
                if (debug_on) {
                    rcs.nml.debugInfo.debugPrintStream.println("amillis = " + amillis);
                }
                if (write_test) {
                    testMsgToSend.test_type++;
                    if (testMsgToSend.test_type >= 6) {
                        testMsgToSend.test_type = 0;
                    }
                    if (debug_on) {
                        rcs.nml.debugInfo.debugPrintStream.println("");
                        rcs.nml.debugInfo.debugPrintStream.println("test_type=" + testMsgToSend.test_type);
                        rcs.nml.debugInfo.debugPrintStream.println("nml_for_write.write(testMsgToSend)");
                    }
                    if (nml_for_write.write(testMsgToSend) < 0) {
                        break;
                    }
                    messages_sent++;
                }
                if (read_test) {
                    if (debug_on) {
                        rcs.nml.debugInfo.debugPrintStream.println("");
                        rcs.nml.debugInfo.debugPrintStream.println("nml_for_read.peek()");
                    }
                    testMsgRead = (NML_PERFORMANCE_TEST_MSG) nml_for_read.peek();
                    if (null != testMsgRead) {
                        messages_received++;
                    } else {
                        null_returns++;
                    }
                }
                long bmillis = System.currentTimeMillis();
                long diff_millis = bmillis - amillis;
                if (debug_on) {
                    rcs.nml.debugInfo.debugPrintStream.println("bmillis = " + bmillis);
                    rcs.nml.debugInfo.debugPrintStream.println("diff_millis = " + diff_millis);
                }
                if (max_millis < diff_millis) {
                    max_millis = diff_millis;
                }
                if (min_millis > diff_millis) {
                    min_millis = diff_millis;
                }

                cycles++;
                if (add_delay) {
                    Thread.sleep(10);
                }
                if (System.in.available() != 0) {
                    break;
                }
            }
            if (null != nml_for_read) {
                if (debug_on) {
                    rcs.nml.debugInfo.debugPrintStream.println("nml_for_read.disconnect()");
                }
                nml_for_read.disconnect();
                nml_for_read = null;
            }
            if (null != nml_for_write) {
                if (debug_on) {
                    rcs.nml.debugInfo.debugPrintStream.println("nml_for_write.disconnect()");
                }
                nml_for_write.disconnect();
                nml_for_write = null;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        long end_millis = System.currentTimeMillis();
        double test_time = (double) (end_millis - start_millis) / 1000.0;
        rcs.nml.debugInfo.debugPrintStream.println("RESULTS:");
        rcs.nml.debugInfo.debugPrintStream.println("The test took " + test_time + " seconds.");
        if (write_test) {
            rcs.nml.debugInfo.debugPrintStream.println(messages_sent + " messages were sent.");
        }
        if (read_test) {
            rcs.nml.debugInfo.debugPrintStream.println(messages_received + " messages were received.\n");
            rcs.nml.debugInfo.debugPrintStream.println("NMLConnection.read returned null (indicating no new data) " + null_returns + " times.");
        }
        rcs.nml.debugInfo.debugPrintStream.println("The maximum time to call an NML function was " + max_millis + " milliseconds.");
        rcs.nml.debugInfo.debugPrintStream.println("The minimum time to call an NML function was " + min_millis + " milliseconds.");
        double delay_time = 0.0;
        if (add_delay) {
            delay_time = cycles * 0.01;
            rcs.nml.debugInfo.debugPrintStream.println(cycles + " 10ms delays were inserted totalling " + delay_time + " seconds");
        }
        double avg_call_time = (test_time / cycles);
        if (add_delay) {
            avg_call_time -= 0.01;
        }
        rcs.nml.debugInfo.debugPrintStream.println("The average time required to call an NML function  was " + avg_call_time + " seconds.");
        if (read_test) {
            double avg_msg_time = test_time / messages_received;
            rcs.nml.debugInfo.debugPrintStream.println("The average time between receiving new messages was " + avg_msg_time + " seconds.");
        }
        System.exit(0);
    }
}
