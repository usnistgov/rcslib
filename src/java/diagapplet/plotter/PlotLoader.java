/*
 * PlotLoader.java
 *
 * Created on December 31, 2006, 7:47 AM
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
package diagapplet.plotter;

import diagapplet.utils.URLLoadInfoPanelInterface;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Hashtable;
import java.util.List;
import java.util.StringTokenizer;
import java.util.Vector;
import javax.swing.JOptionPane;
import javax.swing.ProgressMonitor;

/**
 *
 * @author shackle
 */
class PlotLoader {

    private int unnamed_num = 0;
    private int point_number = 0;
    private Hashtable options_hash_table = null;
    private String url_string = null;
    private Vector extra_pds_vector = null;
    private int max_points_per_plot = 0;
    private double max_x = Double.NEGATIVE_INFINITY;
    private double min_x = Double.POSITIVE_INFINITY;
    private double max_y = Double.NEGATIVE_INFINITY;
    private double min_y = Double.POSITIVE_INFINITY;
    private URLLoadInfoPanelInterface load_info_panel = null;
    private PlotGraphJPanel plotGraphJPanel = null;
    private ParseOptions parseOptions = null;

    /**
     * Get the value of parseOptions
     *
     * @return the value of parseOptions
     */
    public ParseOptions getParseOptions() {
        return parseOptions;
    }

    /**
     * Set the value of parseOptions
     *
     * @param parseOptions new value of parseOptions
     */
    public void setParseOptions(ParseOptions parseOptions) {
        this.parseOptions = parseOptions;
        if (null != parseOptions) {
            String new_field_separator = parseOptions.getFieldSeperator();
            if (null != new_field_separator && new_field_separator.length() > 0) {
                FIELD_SEPARATOR = new_field_separator;
            }
            String new_line_filter_pattern = parseOptions.getFilterPattern();
            if (null != new_line_filter_pattern) {
                this.setLineFilterPattern(new_line_filter_pattern);
            }
            this.plot_verses_line_number = parseOptions.isPlotVersusLineNumber();
        }
    }

    public void set_load_info_panel(URLLoadInfoPanelInterface _load_info_panel) {
        load_info_panel = _load_info_panel;
    }

    public URLLoadInfoPanelInterface get_load_info_panel() {
        return load_info_panel;
    }

    public void set_options_hash_table(Hashtable _options_hash_table) {
        options_hash_table = _options_hash_table;
    }

    public Hashtable get_options_hash_table() {
        return options_hash_table;
    }

    public int get_max_points_per_plot() {
        return max_points_per_plot;
    }

    public void set_max_points_per_plot(int _max_points_per_plot) {
        max_points_per_plot = _max_points_per_plot;
    }

    public Vector get_extra_pds_vector() {
        return extra_pds_vector;
    }

    public void set_extra_pds_vector(Vector _extra_pds_vector) {
        extra_pds_vector = _extra_pds_vector;
    }

    public void set_max_x(double _max_x) {
        max_x = _max_x;
    }

    public double get_max_x() {
        return max_x;
    }

    public void set_min_x(double _min_x) {
        min_x = _min_x;
    }

    public double get_min_x() {
        return min_x;
    }

    public void set_max_y(double _max_y) {
        max_y = _max_y;
    }

    public double get_max_y() {
        return max_y;
    }

    public void set_min_y(double _min_y) {
        min_y = _min_y;
    }

    public double get_min_y() {
        return min_y;
    }

    /**
     * Creates a new instance of PlotLoader
     */
    public PlotLoader(PlotGraphJPanel _plotGraphJPanel) {
        try {
            this.setPlotGraphJPanel(_plotGraphJPanel);
            String max_lines_per_file_string = System.getProperty("max_lines");
            if (null != max_lines_per_file_string) {
                this.max_lines_per_file = Integer.valueOf(max_lines_per_file_string);
            }
        } catch (Exception e) {
        }
    }

    private void AddPlot(PlotData pd, String name) {
        plotGraphJPanel.AddPlot(pd, name.replace(' ', '_'));
    }

    private void AddPointToPlot(PlotData pd, double x, double y, boolean connected, double pre_f_x, double pre_f_y) {
        if (x == pd.last_x && y == pd.last_y) {
            PlotPoint pp = pd.getPlotPointAt(pd.last_added_plot_point_index);
            if (pp != null && pp.pre_f_x == pre_f_x && pp.pre_f_y == pre_f_y) {
                return;
            }
        }
        plotGraphJPanel.AddPointToPlot(pd, x, y, connected, pre_f_x, pre_f_y);
    }
    private static boolean plot_verses_line_number = false;
    private static boolean plot_verses_line_number_locked = false;

    public static void lock_value_for_plot_versus_line_number(
            boolean _plot_verses_line_number,
            boolean _plot_verses_line_number_locked) {
        PlotLoader.plot_verses_line_number = _plot_verses_line_number;
        PlotLoader.plot_verses_line_number_locked = _plot_verses_line_number_locked;
    }
    private boolean first_line = true;
    private int line = 0;
    private boolean over_line_max = false;
    private int skip_coords[] = null;
    private int coords_map[] = null;
    private boolean is_hex[] = null;

    static public void set_plot_verses_line_number(boolean _plot_verses_line_number) {
        plot_verses_line_number = _plot_verses_line_number;
    }
    private static boolean use_reverse_line_number = false;

    static public void set_use_reverse_line_number(boolean _use_reverse_line_number) {
        use_reverse_line_number = _use_reverse_line_number;
    }
    private static int start_at_line_number = -1;

    static public void set_start_at_line_number(int _start_at_line_number) {
        start_at_line_number = _start_at_line_number;
        System.out.println("start_at_line_number = " + start_at_line_number);
    }
    private static int end_at_line_number = -1;

    static public void set_end_at_line_number(int _end_at_line_number) {
        end_at_line_number = _end_at_line_number;
    }

    private void ParseExtraCoordString(PlotData extra_pd, List<PlotData> extra_coord_plot_vector,
            final String extra_coord_string, final int coord_num, final double x, final boolean connected) throws Exception {
        double extra_coord = 0.0;
        double y = 0.0;

        final char first_char = extra_coord_string.charAt(0);
        if (first_char == '"'
                || (!Character.isDigit(first_char)
                && first_char != '+' && first_char != '.' && first_char != '-')) {
            return;
        }

        if ((null != is_hex && is_hex.length > coord_num && is_hex[coord_num]) || extra_coord_string.matches("0[0-9,A-F,a-f,x][0-9,A-F,a-f]*[A-F,a-f][0-9,A-F,a-f]*")) {
            long l = Long.parseLong(extra_coord_string, 16);
            if (null == is_hex || is_hex.length <= coord_num) {
                boolean new_is_hex[] = new boolean[coord_num + 1];
                if (null != is_hex) {
                    for (int j = 0; j < is_hex.length; j++) {
                        new_is_hex[j] = is_hex[j];
                    }
                }
                is_hex = new_is_hex;
            }
            if (!is_hex[coord_num]) {
                if (PlotterCommon.debug_on) {
                    PlotterCommon.DebugPrint("Value in field " + coord_num + " of " + extra_coord_string + " appears to be hexadecimal the field will be treeted as hex.");
                }
                is_hex[coord_num] = true;
            }
            extra_coord = (double) l;
        } else {
            extra_coord = this.parseDoubleValue(extra_coord_string);
        }
        if (null != parseOptions) {
            extra_coord *= parseOptions.getScale();
        }
        if (extra_coord_plot_vector.size() > coord_num - 2) {
            extra_pd = (PlotData) extra_coord_plot_vector.get(coord_num - 2);
        }
        if (null == extra_pd) {
            extra_pd = new PlotData();
        }
//        System.out.println("extra_pd.name="+extra_pd.name+", string="+extra_coord_string);
        y = extra_coord;
        if (y > max_y) {
            max_y = y;
        }
        if (y < min_y) {
            min_y = y;
        }
        AddPointToPlot(extra_pd, x, y, connected, x, y);
        if (extra_coord_plot_vector.size() < (coord_num - 1)) {
            extra_coord_plot_vector.add(extra_pd);
        } else {
            extra_coord_plot_vector.set(coord_num - 2, extra_pd);
        }
    }
    private static boolean FIELD_SEPARATOR_SET = false;
    private static String FIELD_SEPARATOR = ",";

    public static void setFieldSeparator(String _FIELD_SEPARATOR) {
        FIELD_SEPARATOR = _FIELD_SEPARATOR;
        if (PlotterCommon.debug_on) {
            PlotterCommon.DebugPrint("_FIELD_SEPARATOR = " + _FIELD_SEPARATOR);
        }
        FIELD_SEPARATOR_SET = true;
    }

    private void ParseFirstLine(PlotData pd_to_use, List<PlotData> extra_coord_plot_vector, final String parseString) throws Exception {
        skip_coords = null;
        Vector<Integer> skip_coords_vector = new Vector<Integer>();
        Vector<Integer> coord_map_vector = new Vector<Integer>();
        if (!FIELD_SEPARATOR_SET && !cmd_line_mode) {
            String new_field_separator = null;
            if (null != parseOptions) {
                new_field_separator = parseOptions.getFieldSeperator();
            } else {
                new_field_separator = JOptionPane.showInputDialog("Field Separator", FIELD_SEPARATOR);
            }
            if (null != new_field_separator && new_field_separator.length() > 0) {
                FIELD_SEPARATOR = new_field_separator;
            }
            FIELD_SEPARATOR_SET = true;
        }
        StringTokenizer tokenizer = new StringTokenizer(parseString, FIELD_SEPARATOR);
        if (!tokenizer.hasMoreTokens()) {
            return;
        }
        if (!this.plot_verses_line_number && !this.url_string_ends_in_xy) {
            String first_string = tokenizer.nextToken();
            if (null != first_string) {
                first_string = first_string.trim();
            }
            if (!tokenizer.hasMoreTokens()) {
                if (pd_to_use.name != null && pd_to_use.name.length() > 1) {
                    return;
                }
                if (url_string != null && url_string.length() > 1) {
                    pd_to_use.name = url_string + "._" + first_string;
                } else {
                    pd_to_use.name = "unnamed_" + unnamed_num + "._" + first_string;
                }
                pd_to_use.heading_string = first_string;
                AddPlot(pd_to_use, pd_to_use.name);
                return;
            }
            String second_string = tokenizer.nextToken();
            if (null != second_string) {
                second_string = second_string.trim();
            }
            if (url_string != null && url_string.length() > 1) {
                pd_to_use.name = url_string + "._" + second_string;
            } else {
                if (pd_to_use.name == null || pd_to_use.name.length() < 1) {
                    pd_to_use.name = "unnamed_" + unnamed_num + "._" + second_string;
                }
            }
            pd_to_use.heading_string = first_string;
            AddPlot(pd_to_use, pd_to_use.name);
            if (!tokenizer.hasMoreTokens()) {
                return;
            }
        }
        int extra_coord_pos = 0;
        while (tokenizer.hasMoreTokens()) {
            extra_coord_pos++;
            String tok = tokenizer.nextToken();
            if (null != tok) {
                tok = tok.trim();
            }
            if (extra_coord_pos > extra_coord_plot_vector.size()) {
                if (this.fieldSelectPatternEnabled) {
                    if (!tok.matches(this.fieldSelectPattern)) {
                        skip_coords_vector.add(Integer.valueOf(extra_coord_pos + 1));
                        coord_map_vector.add(Integer.valueOf(-3));
                        System.err.println("Field " + tok + " does NOT match  pattern " + this.fieldSelectPattern + ".");
                        continue;
                    } else {
                        coord_map_vector.add(Integer.valueOf(extra_coord_plot_vector.size()));
                        System.err.println("Field " + tok + " matches  pattern " + this.fieldSelectPattern + ". ");

                    }
                }
                PlotData extra_coord_plot_data = new PlotData();
//                if (extra_coord_plot_data.name != null && extra_coord_plot_data.name.length() > 1) {
//                    return;
//                }
                if (url_string != null && url_string.length() > 1) {
                    extra_coord_plot_data.name = url_string + "._" + tok;
                } else {
                    extra_coord_plot_data.name = "unnamed_" + unnamed_num + "._" + tok;
                }
                extra_coord_plot_data.heading_string = tok;
                AddPlot(extra_coord_plot_data, extra_coord_plot_data.name);
                extra_coord_plot_vector.add(extra_coord_plot_data);
                if (this.fieldSelectPatternEnabled) {
                    System.err.println(extra_coord_plot_vector.size() + " of " + extra_coord_pos + " fields selected.");
                }
            }
        }
        if (null != skip_coords_vector && skip_coords_vector.size() > 0) {
            skip_coords = new int[skip_coords_vector.size()];
            coords_map = new int[extra_coord_pos + 2];
            for (int i = 0; i < skip_coords_vector.size(); i++) {
                skip_coords[i] = skip_coords_vector.elementAt(i).intValue();
            }
            coords_map[0] = -1;
            coords_map[1] = -1;
            for (int i = 0; i < coord_map_vector.size(); i++) {
                coords_map[i + 2] = coord_map_vector.elementAt(i).intValue() + 2;
            }
        }
//        System.out.println("skip_coords="+skip_coords);
    }

    private boolean skip_coord_check(int coord) {
        if (null == skip_coords) {
            return false;
        }
        for (int i = 0; i < skip_coords.length; i++) {
            //System.out.println("i="+i+", skip_coord[i]="+skip_coords[i]+",coord="+coord);
            if (coord == skip_coords[i]) {
                return true;
            }
            if (coord < skip_coords[i]) {
                return false;
            }
        }
        return false;
    }
    private double last_time_val = -1.0;
    private static double time_val_diff_min = 0.001;
    private static boolean check_time_val_diff = false;
    private boolean time_val_checked = false;
    static private int time_skip_count = 0;

    static void set_time_diff_min(double _time_diff_min) {
        time_val_diff_min = _time_diff_min;
        check_time_val_diff = true;
        time_skip_count = 0;
    }

    private boolean check_time_val(double time_val) {
        if (time_val_checked || !check_time_val_diff) {
            return false;
        }
        time_val_checked = true;
        double diff = time_val - last_time_val;
        if (diff > 0 && diff < time_val_diff_min) {
            time_skip_count++;
            return true;
        }
        last_time_val = time_val;
        return false;
    }

    static public String getDateFormatString() {
        return df_string;
    }

    static public void setDateFormatString(String new_df_string) {
        df_string = new_df_string;
        df = new SimpleDateFormat(df_string);
        df.setLenient(true);
    }

    static private String df_string = "MM/dd/yy HH:mm:ss";
    static private SimpleDateFormat df
            = new SimpleDateFormat(df_string);

    static private final String df2_string = "yyyy-MM-dd";
    static private final SimpleDateFormat df2
            = new SimpleDateFormat(df2_string);
    static private boolean first_parse_double_error_occured = false;

    static double parseDoubleValue(String s) throws Exception {
        s = s.trim();
        if (s.matches("[0-9]+[.][0-9]+[.][0-9]+")) {
            String sa[] = s.split("[.]");
            if (sa.length == 3) {
                return 60.0 * Integer.valueOf(sa[0]) + 1.0 * Integer.valueOf(sa[1]) + 1e-3 * Integer.valueOf(sa[2]);
            }
        }
        Exception e1 = null;
        try {
            return Double.valueOf(s);

        } catch (Exception e) {
            e1 = e;
        }
        try {
            df.setLenient(true);
            //df.setTimeZone(TimeZone.getTimeZone("EDT"));
            String dtstring = s;
            String sfracstring = "";
            double sfrac = 0;
            String pattern = ".*[.][0-9][0-9][0-9]$";
            if (PlotterCommon.debug_on) {
                PlotterCommon.DebugPrint(df.format(new Date()));
            }
            if (s.matches(pattern)) {
                dtstring = s.substring(0, s.length() - 4);
                sfracstring = s.substring(s.length() - 4);
                if (PlotterCommon.debug_on) {
                    PlotterCommon.DebugPrint("sfracstring = " + sfracstring);
                }
                sfrac = Double.valueOf(sfracstring);
            }
            double d1 = Double.NaN;
            try {
                long l1 = df.parse(dtstring).getTime();
                d1 = ((double) (1e-3 * l1)) + sfrac - 3600.0;
            } catch (Exception e) {

            }
            if (Double.isNaN(d1)) {
                try {
                    long l2 = df2.parse(dtstring).getTime();
                    d1 = ((double) (1e-3 * l2)) + sfrac - 3600.0;

                } catch (Exception exception) {
                }
            }
            if (Double.isNaN(d1)) {
                d1 = (double) s.hashCode();
            }
            if (PlotterCommon.debug_on) {
                PlotterCommon.DebugPrint(String.format("parseDoubleValue(%s) returning %20.3f\n", s, d1));
            }

            return d1;
        } catch (Exception e) {
            if (!first_parse_double_error_occured) {
                e.printStackTrace();
                first_parse_double_error_occured = true;
            }
            if (null != e1) {
                throw e1;
            }
        }
        return Double.NEGATIVE_INFINITY;
    }
    private int pre_counted_line_count = -1;

    @SuppressWarnings("unchecked")
    private void ParseString(PlotData pd_to_use,
            List<PlotData> extra_coord_plot_vector,
            String parseString) throws Exception {
        boolean connected = true;
        PlotData extra_pd = null;
        time_val_checked = false;
        if (this.fieldSelectPatternEnabled) {
            this.plot_verses_line_number = true;
        }
        if (first_line) {
            line = 0;
            over_line_max = false;
        } else {
            line++;
            if (over_line_max) {
                return;
            }
            if (line > this.max_lines_per_file && this.max_lines_per_file >= 0) {
                over_line_max = true;
                throw new Exception("Line limit exceeded. " + line + " > " + max_lines_per_file);
            }
        }
        if (PlotterCommon.debug_on) {
            PlotterCommon.DebugPrint("ParseString( pd_to_use=" + pd_to_use + ", parseString=" + parseString + ")");
            PlotterCommon.DebugPrint("pd_to_use.name=" + pd_to_use.name);
        }
        if (first_line
                && (Character.isDigit(parseString.charAt(0))
                || parseString.charAt(0) == '+'
                || parseString.charAt(0) == '-'
                || parseString.charAt(0) == '.')) {
            StringTokenizer st = new StringTokenizer(parseString, this.FIELD_SEPARATOR);
            String fakeFirstLine = "";
            int token_num = 0;
            while (st.hasMoreTokens()) {
                token_num++;
                fakeFirstLine += "field" + token_num;
                if (st.hasMoreTokens()) {
                    fakeFirstLine += this.FIELD_SEPARATOR.charAt(0);
                }
                st.nextToken();
            }
            if (token_num == 2 && url_string_ends_in_xy) {
                fakeFirstLine = "x,y";
            }
            ParseFirstLine(pd_to_use, extra_coord_plot_vector, fakeFirstLine);
        }
        if ((parseString.charAt(0) == '"' || first_line)
                && !Character.isDigit(parseString.charAt(0))
                && parseString.charAt(0) != '+'
                && parseString.charAt(0) != '-'
                && parseString.charAt(0) != '.') {
            boolean starts_with_quote = false;
            if (parseString.charAt(0) == '"') {
                starts_with_quote = true;
                parseString = parseString.substring(1);
                int qindex = parseString.indexOf('"');
                if (qindex > 0) {
                    parseString = parseString.substring(0, qindex);
                }
                pd_to_use = new PlotData();
                pd_to_use.name = parseString;
            }
            if (parseString.length() < 1) {
                return;
            }
            if (starts_with_quote) {
                if (pd_to_use.name.length() > 0 || pd_to_use.current_size > 0) {
                    if (pd_to_use.name.length() < 1) {
                        unnamed_num++;
                        if (url_string != null && url_string.length() > 1) {
                            pd_to_use.name = url_string + unnamed_num;
                        } else {
                            pd_to_use.name = "unnamed_" + unnamed_num;
                        }
                    }
                    AddPlot(pd_to_use, pd_to_use.name);
                    if (null != extra_coord_plot_vector) {
                        for (int i = 0; i < extra_coord_plot_vector.size(); i++) {
                            extra_pd = (PlotData) extra_coord_plot_vector.get(i);
                            if (null != extra_pd.heading_string) {
                                extra_pd.name = extra_pd.heading_string;
                            } else {

                                extra_pd.name = pd_to_use.name + "_coord_" + i;
                            }
                            AddPlot(extra_pd, extra_pd.name);
                        }
                        extra_coord_plot_vector = null;
                        extra_pd = null;
                    }
                    pd_to_use = new PlotData();
                }
                pd_to_use.name = parseString;
                point_number = 0;
            } else if (first_line) {
                ParseFirstLine(pd_to_use, extra_coord_plot_vector, parseString);
                first_line = false;
                point_number = 0;
                return;
            }
            return;
        }
        first_line = false;
        StringTokenizer tokenizer = new StringTokenizer(parseString, FIELD_SEPARATOR);
        if (!tokenizer.hasMoreTokens()) {
            return;
        }
        double first_coord = 0.0;
        double second_coord = 0.0;
        double x = 0.0;
        double y = 0.0;
        point_number++;

        if (point_number < start_at_line_number) {
//            System.err.println("Skipping point_number="+point_number);
//            System.err.println("start_at_line_number="+start_at_line_number);
            return;
        }
        if (point_number > end_at_line_number && end_at_line_number > 0) {
//            System.err.println("Skipping point_number="+point_number);
//            System.err.println("end_at_line_number="+end_at_line_number);
            return;
        }
//        System.out.println("point_number=" + point_number + " , parseString=" + parseString);

        if (!this.plot_verses_line_number && !this.url_string_ends_in_xy) {
            try {
                String first_string = tokenizer.nextToken();
                if (null != first_string) {
                    first_string = first_string.trim();
                }
                first_coord = parseDoubleValue(first_string);
                if (null != parseOptions) {
                    first_coord *= parseOptions.getScale();
                }
                if (check_time_val(first_coord)) {
                    return;
                }
            } catch (Exception e) {
                return;
            }
            if (!tokenizer.hasMoreTokens()) {
                second_coord = first_coord;
                first_coord = point_number;
                if (use_reverse_line_number) {
                    first_coord = this.counted_lines - point_number;
                }
            } else {
                try {
                    String second_string = tokenizer.nextToken();
                    if (null != second_string) {
                        second_string = second_string.trim();
                    }
                    second_coord = parseDoubleValue(second_string);
                    if (null != parseOptions) {
                        second_coord *= parseOptions.getScale();
                    }
                    if (check_time_val(second_coord)) {
                        return;
                    }
                } catch (Exception e) {
                    second_coord = first_coord;
                    first_coord = point_number;
                    if (use_reverse_line_number) {
                        first_coord = this.counted_lines - point_number;
                    }
                }
            }
        } else {
            first_coord = point_number;
            if (use_reverse_line_number) {
                first_coord = this.counted_lines - point_number;
            }
        }
        if (pd_to_use.coordinate_type == PlotterCommon.POLAR_COORD_TYPE) {
            double r = first_coord;
            double theta = second_coord;
        } else {
            x = first_coord;
            y = second_coord;
        }
        if (!this.fieldSelectPatternEnabled || !skip_coord_check(0)) {
            AddPointToPlot(pd_to_use, x, y, connected, x, y);
        }
        int coord_num = 2;
        if (extra_coord_plot_vector == null) {
            return;
        }
        while (tokenizer.hasMoreTokens()) {
            String extra_coord_string = null;
            try {
                extra_coord_string = tokenizer.nextToken();
                if (null != extra_coord_string) {
                    extra_coord_string = extra_coord_string.trim();
                }
            } catch (Exception e) {
                break;
            }
            try {
                if (coord_num == 2 && time_val_checked
                        && check_time_val(Double.valueOf(extra_coord_string))) {
                    return;
                }
            } catch (Exception exception) {
                exception.printStackTrace();
            }
            int coord_to_use = coord_num;
            if (!this.fieldSelectPatternEnabled
                    || (coord_num < coords_map.length
                    && (coord_to_use = coords_map[coord_num]) >= 2)) {
                ParseExtraCoordString(extra_pd, extra_coord_plot_vector, extra_coord_string, coord_to_use, x, connected);
            }
            extra_pd = null;
            coord_num++;
        }

    }
    private boolean count_lines_first = true;
    private boolean use_fixed_plotdata = false;
    private int counted_lines = -1;
    private int skipped_lines = 0;
    private int lines_to_skip = 0;

    private void CountLines(BufferedReader reader) throws Exception {
        while (null != reader.readLine()) {
            counted_lines++;
        }
    }

    private void SkipLines(BufferedReader reader, int lines_to_skip) throws Exception {
        skipped_lines = 0;
        while (null != reader.readLine() && skipped_lines < lines_to_skip) {
            skipped_lines++;
        }
    }
    private int lines_read = 0;

    public void Reload() {
        lines_to_skip = 0;
        if (lines_read < counted_lines && lines_read > 100) {
            lines_to_skip = lines_read - 2;
        }
        if (PlotterCommon.debug_on) {
            PlotterCommon.DebugPrint("lines_read=" + lines_read + ", counted_lines=" + counted_lines + ", lines_to_skip=" + lines_to_skip);
        }
        LoadURL(url_string, null);
    }
    boolean url_string_ends_in_xy = false;
    protected String LineFilterPattern;
    private static String ForcedLineFilterPattern = "";
    private static boolean use_forced_line_filter_pattern = false;

    public static void setForcedLineFilterPattern(String _ForcedLineFilterPattern,
            boolean _use_forced_line_filter_pattern) {
        PlotLoader.ForcedLineFilterPattern = _ForcedLineFilterPattern;
        PlotLoader.use_forced_line_filter_pattern = _use_forced_line_filter_pattern;
    }

    /**
     * Get the value of LineFilterPattern
     *
     * @return the value of LineFilterPattern
     */
    public String getLineFilterPattern() {
        return LineFilterPattern;
    }

    /**
     * Set the value of LineFilterPattern
     *
     * @param LineFilterPattern new value of LineFilterPattern
     */
    public void setLineFilterPattern(String LineFilterPattern) {
        this.LineFilterPattern = LineFilterPattern;
    }
    public static boolean cmd_line_mode = false;
    public static int filenum = 0;

    public List<PlotData> LoadURL(String _url_string, ProgressMonitor pm) {
        byte b[] = new byte[32768];
        ArrayList<PlotData> extra_coords_plots_vector = new ArrayList<PlotData>();
        long load_url_start_time = System.currentTimeMillis();
        try {
            PlotData plot_data_to_add = null;
            URL newURL = null;
            URLConnection newURLConnection = null;
            File newFile = null;
            FileInputStream newFileInputStream = null;
            BufferedReader reader = null;
            PlotData extra_pd = null;
            url_string = _url_string;
            counted_lines = -1;
            long bytes_read = 0;
            long mem_used_start = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
            if (Thread.interrupted()) {
                return null;
            }
            if (use_reverse_line_number) {
                this.count_lines_first = true;
            }

            if (url_string.startsWith("http:") || url_string.startsWith("ftp:")) {
                newURL = new URL(url_string);
                newURLConnection = newURL.openConnection();
                newURLConnection.setUseCaches(false);
                reader = new BufferedReader(new InputStreamReader(newURLConnection.getInputStream()));
                try {
                    if (null != load_info_panel) {
                        load_info_panel.set_content_length(newURLConnection.getContentLength());
                    }
                    if (null != pm) {
                        pm.setMaximum((int) newURLConnection.getContentLength());
                    }
                } catch (Exception e) {
                    System.err.println("Error initializing load_info_panel.");
                    e.printStackTrace();
                }
            } else {
                newFile = new File(url_string);
                System.out.println("Loading " + newFile.getAbsolutePath());
                newFileInputStream = new FileInputStream(url_string);
                reader = new BufferedReader(new InputStreamReader(newFileInputStream));
                if (count_lines_first) {
                    CountLines(reader);
                    reader.close();
                    newFileInputStream.close();
                    newFileInputStream = new FileInputStream(url_string);
                    reader = new BufferedReader(new InputStreamReader(newFileInputStream));
                    if (use_fixed_plotdata) {
                        PlotData.fixed_size = counted_lines;
                        plotGraphJPanel.max_points_per_plot = counted_lines;
                    }
                }
                try {
                    if (null != load_info_panel) {
                        load_info_panel.set_content_length((int) newFile.length());
                    }
                    if (null != pm) {
                        pm.setMaximum((int) newFile.length());
                    }
                } catch (Exception e) {
                    System.err.println("Error initializing load_info_panel.");
                    e.printStackTrace();
                }
            }

            if (!cmd_line_mode && filenum == 0) {
                if (PlotLoader.use_forced_line_filter_pattern) {
                    this.setLineFilterPattern(PlotLoader.ForcedLineFilterPattern);
                } else {
                    String new_line_filter_pattern = null;
                    if (null != parseOptions) {
                        new_line_filter_pattern = parseOptions.getFilterPattern();
                    } else {
                        new_line_filter_pattern = JOptionPane.showInputDialog("Pattern to filter lines",
                                this.LineFilterPattern);
                    }
                    if (null != new_line_filter_pattern) {
                        this.setLineFilterPattern(new_line_filter_pattern);
                    }
                }
                if (!PlotLoader.plot_verses_line_number_locked) {
                    if (null != parseOptions) {
                        this.plot_verses_line_number = parseOptions.isPlotVersusLineNumber();
                    } else {
                        Boolean BV[] = new Boolean[2];
                        BV[0] = Boolean.FALSE;
                        BV[1] = Boolean.TRUE;
                        Boolean B = (Boolean) JOptionPane.showInputDialog(plotGraphJPanel,
                                "Plot values versus line number?", "Plotter Query",
                                JOptionPane.QUESTION_MESSAGE,
                                null, BV, Boolean.valueOf(this.plot_verses_line_number));
                        if (B != null) {
                            this.plot_verses_line_number = B.booleanValue();
                        }
                    }
                }
            }
            first_line = true;
            point_number = 0;
            url_string_ends_in_xy = _url_string.endsWith(".xy");
            if (url_string_ends_in_xy) {
                plot_verses_line_number = true;
            }

            String parseString = null;
            boolean using_ndc8_workaround = false;
            if (lines_to_skip > 0) {
                plot_data_to_add = new PlotData();
                if (first_line && _url_string.endsWith(".xyz")
                        && (Character.isDigit(parseString.charAt(0)) || parseString.charAt(0) == '-' || parseString.charAt(0) == '+')) {
                    ParseString(plot_data_to_add, extra_coords_plots_vector, "X, Y, Z\n");
                } else {
                    parseString = reader.readLine();

                    if (null != parseString) {
                        parseString = parseString.trim();
                    }
                    ParseString(plot_data_to_add, extra_coords_plots_vector, parseString);
                }
                SkipLines(reader, lines_to_skip);
                lines_read = 1 + lines_to_skip;
                if (PlotterCommon.debug_on) {
                    PlotterCommon.DebugPrint("skipped_lines=" + skipped_lines);
                }
            } else {
                skipped_lines = 0;
            }

            try {
                int lines_between_updates = 20;
                if (null != load_info_panel) {
                    lines_between_updates = load_info_panel.get_content_length() / 1000;
                }
                if (lines_between_updates < 20) {
                    lines_between_updates = 20;
                }
                if (null == plot_data_to_add) {
                    plot_data_to_add = new PlotData();
                }
                try {
                    if (null != load_info_panel) {
                        load_info_panel.set_bytes_read(0);
                        load_info_panel.set_URLname(url_string);
                    }
                } catch (Exception e) {
                    System.err.println("Error initializing load_info_panel.");
                    e.printStackTrace();
                }
                //StringBuffer tempBuffer = new StringBuffer();
                while (true) {
                    if (lines_read > skipped_lines) {
                        long memleft = Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory() + Runtime.getRuntime().freeMemory();
//                        long mem_used = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
//                        long mem_used_new = mem_used - mem_used_start;
//                        long mem_used_per_line = (mem_used_new) / (lines_read - skipped_lines);
//                        System.out.println("lines_read= "+lines_read+", memleft = "+memleft+", mem_used="+mem_used+", mem_used_new="+mem_used_new+", mem_used_per_line="+mem_used_per_line);
                        if (lines_read - skipped_lines > 50000
                                && memleft < 10000000) {
                            this.max_lines_per_file = lines_read / 2;
                            return extra_coords_plots_vector;
                        }
                    }
                    if (Thread.interrupted()) {
                        return null;
                    }
                    try {
                        parseString = reader.readLine();
                        if (null != parseString) {
                            parseString = parseString.trim();
                            // Some idiot must be manually editing many of the files produced by
                            // NDC8 Vehicle Application Designer
                            // They instert month;..;min but in the header and then only 
                            // put these fields in the first line. It is completely unneccessary and
                            //  breaks any reasonable program for easy plotting.
                            final String ndc8_workaround = "month;day;year;hour;min;,m.s.ms;";

                            if (lines_read == 0 && parseString.startsWith(ndc8_workaround)) {
                                parseString = "time;" + parseString.substring(ndc8_workaround.length());
                                using_ndc8_workaround = true;
                            }
                            if (lines_read == 1 && using_ndc8_workaround) {
                                parseString = parseString.substring(parseString.indexOf(";") + 1);
                                parseString = parseString.substring(parseString.indexOf(";") + 1);
                                parseString = parseString.substring(parseString.indexOf(";") + 1);
                                parseString = parseString.substring(parseString.indexOf(";") + 1);
                                parseString = parseString.substring(parseString.indexOf(";") + 1);
                            }
                            if (using_ndc8_workaround && parseString.startsWith(",")) {
                                parseString = parseString.substring(1);
                            }
                        }
                    } catch (java.lang.OutOfMemoryError oome) {
                        this.max_lines_per_file = lines_read / 2;
//                        oome.printStackTrace();
                        return null;
                    }
                    if (null == parseString) {
                        break;
                    }
                    if (parseString.length() < 1) {
                        continue;
                    }
                    if (null != this.LineFilterPattern && this.LineFilterPattern.length() > 0) {
                        if (!parseString.matches(this.LineFilterPattern)) {
                            continue;
                        }
                    }
//                    if(!parseString.endsWith("\n") && !parseString.endsWith("\n\r"))
//                    {
//                        tempBuffer.append(parseString+"\n");
//                    }
//                    else
//                    {
//                        tempBuffer.append(parseString);
//                    }
                    try {
                        // NOT a breakpoint

                        if (lines_read % (lines_between_updates) == 0) {

                            if (load_info_panel != null) {
                                load_info_panel.inc_bytes_read(parseString.length());
                                load_info_panel.updateDisplay();

                            }
//                            Thread.sleep(10);
                        }

                    } catch (Exception e) {
                        System.err.println("Error updating load_info_panel.");
                        e.printStackTrace();
                    }
//                    String s=parseString;
//                    if(s.startsWith("##! "))
//                    {
//                        s = s.substring(4);
//                    }
//                    if(
//                            s.charAt(0) == '#' ||
//                            s.charAt(0) == '!' ||
//                            s.startsWith("plot -") ||
//                            s.startsWith("pause") ||
//                            s.equals("e"))
//                    {
//                        continue;
//                    }
                    if (parseString.charAt(0) == '"' && parseString.endsWith("\"") && parseString.length() > 2 && parseString.indexOf(' ') < 0 && parseString.indexOf(',') < 0 && parseString.indexOf('\t') < 0) {
                        String newname = parseString.substring(1, parseString.length() - 1);
                        if (newname.compareTo(plot_data_to_add.name) != 0) {
                            if (plot_data_to_add.current_size > 0) {
                                plotGraphJPanel.AddPlot(plot_data_to_add, plot_data_to_add.name);
                                plot_data_to_add = new PlotData();
                                plot_data_to_add.name = newname;
                            } else {
                                plot_data_to_add.name = newname;
                            }
                        }
                    } else {
                        try {
                            if (first_line && _url_string.endsWith(".xyz")
                                    && (Character.isDigit(parseString.charAt(0)) || parseString.charAt(0) == '-' || parseString.charAt(0) == '+')) {
                                ParseString(plot_data_to_add, extra_coords_plots_vector, "X, Y, Z\n");
                            }
                            ParseString(plot_data_to_add, extra_coords_plots_vector, parseString);
                        } catch (java.lang.OutOfMemoryError oome) {
                            this.max_lines_per_file = lines_read / 2;
//                            oome.printStackTrace();
                            return null;
                        }
                    }
                    bytes_read += parseString.length();
                    if (null != pm) {
                        pm.setProgress((int) bytes_read);
                        if (pm.isCanceled()) {
                            return null;
                        }
                    }
                    lines_read++;
                }

//		if(plot_data_to_add.name.length() > 0 || plot_data_to_add.current_size > 0)
//		{
//		    if(plot_data_to_add.name.length() < 1)
//		    {
//			unnamed_num++;
//			if(url_string != null && url_string.length() > 1)
//			{
//			    plot_data_to_add.name = url_string+unnamed_num;
//			}
//			else
//			{
//			    plot_data_to_add.name = "unnamed_"+unnamed_num;
//			}
//		    }
//		    AddPlot(plot_data_to_add,plot_data_to_add.name);
//		    if(null != extra_pds_vector)
//		    {
//			for(int i = 0; i < extra_pds_vector.size(); i++)
//			{
//			    extra_pd = (PlotData) extra_pds_vector.elementAt(i);
//			    extra_pd.name = plot_data_to_add.name+"_coord_"+(i+2);
//			    AddPlot(extra_pd,extra_pd.name);
//			}
//			extra_pds_vector=null;
//			extra_pd=null;
//		    }
//		}
            } finally {
                if (null != reader) {
                    reader.close();
                    reader = null;
                }
                if (null != newFileInputStream) {
                    newFileInputStream.close();
                    newFileInputStream = null;
                }
            }

            //PlotterCommon.DebugPrint2("PlotGraph.plots= "+PlotGraph.plots);
            // if(PlotGraph.keyVector != null)
// 	    {
// 		//PlotterCommon.DebugPrint2("PlotGraph.plots.size() = "+PlotGraph.plots.size());
// 		int max_now=0;
// 		for(int ki = 0 ; ki < PlotGraph.keyVector.size(); ki++)
// 		{
// 		    PlotData plot_data = (PlotData) PlotGraph.keyVector.elementAt(ki);
// 		    if(PlotterCommon.debug_on )
// 		    {
// 			PlotterCommon.DebugPrint("plot_data.name="+plot_data.name+", plot_data.v.size() g= " +plot_data.v_size());
// 		    }
// 		    if(plot_data.v_size() > max_now)
// 		    {
// 			max_now = plot_data.v_size();
// 		    }
// 		}
// 		if(PlotterCommon.debug_on )
// 		{
// 		    PlotterCommon.DebugPrint("max_now="+max_now);
// 		}
// 		if(max_now > max_points_per_plot)
// 		{
// 		    set_max_points_per_plot(max_now);
// 		}
//	    }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            b = null;
            System.gc();
        }
        if (null != pm) {
            pm.close();
        }
        long load_url_stop_time = System.currentTimeMillis();
        long load_url_time = load_url_stop_time - load_url_start_time;
        if (PlotterCommon.debug_on) {
            PlotterCommon.DebugPrint("load_url_time= " + load_url_time);
            if (time_skip_count > 0) {
                PlotterCommon.DebugPrint("time_skip_count = " + time_skip_count);
            }
        }
        b = null;
        //ListPlots();
        return extra_coords_plots_vector;
    }
    /**
     * Holds value of property max_lines_per_file.
     */
    private int max_lines_per_file = -1;

    /**
     * Getter for property max_lines_per_file.
     *
     * @return Value of property max_lines_per_file.
     */
    public int getMax_lines_per_file() {
        return this.max_lines_per_file;
    }

    /**
     * Setter for property max_lines_per_file.
     *
     * @param max_lines_per_file New value of property max_lines_per_file.
     */
    public void setMax_lines_per_file(int max_lines_per_file) {
        this.max_lines_per_file = max_lines_per_file;
    }
    /**
     * Holds value of property fieldSelectPattern.
     */
    private String fieldSelectPattern;

    /**
     * Getter for property fieldSelectPattern.
     *
     * @return Value of property fieldSelectPattern.
     */
    public String getFieldSelectPattern() {
        return this.fieldSelectPattern;
    }

    /**
     * Setter for property fieldSelectPattern.
     *
     * @param fieldSelectPattern New value of property fieldSelectPattern.
     */
    public void setFieldSelectPattern(String fieldSelectPattern) {
        this.fieldSelectPattern = fieldSelectPattern;
    }
    /**
     * Holds value of property fieldSelectPatternEnabled.
     */
    private boolean fieldSelectPatternEnabled;

    /**
     * Getter for property fieldSelectPatternEnabled.
     *
     * @return Value of property fieldSelectPatternEnabled.
     */
    public boolean isFieldSelectPatternEnabled() {
        return this.fieldSelectPatternEnabled;
    }

    /**
     * Setter for property fieldSelectPatternEnabled.
     *
     * @param fieldSelectPatternEnabled New value of property
     * fieldSelectPatternEnabled.
     */
    public void setFieldSelectPatternEnabled(boolean fieldSelectPatternEnabled) {
        this.fieldSelectPatternEnabled = fieldSelectPatternEnabled;
    }

    /**
     * @return the plotGraphJPanel
     */
    public PlotGraphJPanel getPlotGraphJPanel() {
        return plotGraphJPanel;
    }

    /**
     * @param plotGraphJPanel the plotGraphJPanel to set
     */
    public void setPlotGraphJPanel(PlotGraphJPanel plotGraphJPanel) {
        this.plotGraphJPanel = plotGraphJPanel;
    }
}
