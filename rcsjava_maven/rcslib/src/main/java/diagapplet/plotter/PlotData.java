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
package diagapplet.plotter;

import java.util.Vector;
import java.awt.Color;
import java.util.Iterator;
import javax.swing.JOptionPane;

/**
 * Class containing all of the data for a single plot.
 * @author Will Shackleford
 */
public class PlotData {

    private static int count = 0;
    public final int num = ++count;
    private Vector<PlotPoint> v = null;
    private PlotPoint plot_point_array[] = null;
    private Color line_color = null;
    private Color point_color = null;
    public String name = "";
    public String short_name = "";
    public String add_to_short_key = "";
    public String add_to_key = "";
    public double integral = 0;
    public double stddev = 0;
    public double derivmean = 0;
    public double mean = 0;
    public int point_count = 0;
    public double last_x;
    public double last_y;
    public int last_compare_index = 0;
    public boolean array_type = false;
    public boolean delete_me = false;
    String line_style = null;
    int line_width;
    int coordinate_type;
    public boolean mark_points = true;
    public int current_size = 0;
    public int v_offset = 0;
    public int p2_count = 0;
    public String heading_string = null;
    private boolean show[] = null;
    public boolean painting = false;
    public PlotPoint last_pp_set = null;
    public int last_pp_set_index = -1;
    public PlotData y_plot_data = null;
    public boolean no_key = false;
    public boolean print_v = false;
    public boolean is_y_plot = false;
    public double max_x = Double.NEGATIVE_INFINITY;
    public double min_x = Double.POSITIVE_INFINITY;
    public double max_y = Double.NEGATIVE_INFINITY;
    public double min_y = Double.POSITIVE_INFINITY;
    public double max_orig_x = Double.NEGATIVE_INFINITY;
    public double min_orig_x = Double.POSITIVE_INFINITY;
    public double max_orig_y = Double.NEGATIVE_INFINITY;
    public double min_orig_y = Double.POSITIVE_INFINITY;
    public double max_pre_f_x = Double.NEGATIVE_INFINITY;
    public double min_pre_f_x = Double.POSITIVE_INFINITY;
    public double max_pre_f_y = Double.NEGATIVE_INFINITY;
    public double min_pre_f_y = Double.POSITIVE_INFINITY;
    public double min_time = Double.POSITIVE_INFINITY;
    public double max_time = Double.NEGATIVE_INFINITY;
    static public int fixed_size = -1;

    public int get_num_points() {
        if (fixed_size > 0) {
            return fixed_size;
        }
        if (current_size > v.size()) {
            current_size = v.size();
        }
        return current_size;
    }

    public void AddX(double ax) {
        for (int i = 0; i < v.size(); i++) {
            PlotPoint pp = v.elementAt(i);
            pp.orig_x += ax;
            pp.pre_f_x += ax;
            v.set(i, pp);
        }
        this.RecheckAllPoints();
    }


    public PlotData() {
        if (fixed_size > 0) {
            plot_point_array = new PlotPoint[fixed_size];
            current_size = fixed_size;
        } else {
            v = new Vector<PlotPoint>();
        }
        show = new boolean[16];
        for (int i = 0; i < show.length; i++) {
            show[i] = true;
        }
        if(unnamed_pd > 0) {
            this.name = "_"+unnamed_pd;
        }
        unnamed_pd++;
    }
    
    private static int unnamed_pd = 0;
    
    private boolean contains_infinite = false;

    private void PrintContainsInfiniteError() {
        contains_infinite = true;
        String errS = this.name+ " contains infinite or NAN data.";
        System.err.println(errS);
        JOptionPane.showMessageDialog(null, errS);
    }

    public void CheckPoint(PlotPoint pp) {
        if(Double.isInfinite(pp.orig_x) || Double.isNaN(pp.orig_x) || Double.isInfinite(-pp.orig_x)) {
            this.infinite_points++;
            if(!contains_infinite) {
                PrintContainsInfiniteError();
            }
            return;
        }
        if(Double.isInfinite(pp.orig_y) || Double.isNaN(pp.orig_y) || Double.isInfinite(-pp.orig_y)) {
            this.infinite_points++;
            if(!contains_infinite) {
                PrintContainsInfiniteError();
            }
            return;
        }
        if(Double.isInfinite(pp.pre_f_x) || Double.isNaN(pp.pre_f_x) || Double.isInfinite(-pp.pre_f_x)) {
            this.infinite_points++;
            if(!contains_infinite) {
                PrintContainsInfiniteError();
            }
            return;
        }
        if(Double.isInfinite(pp.pre_f_y) || Double.isNaN(pp.pre_f_y) || Double.isInfinite(-pp.pre_f_y)) {
            this.infinite_points++;
            if(!contains_infinite) {
                PrintContainsInfiniteError();
            }
            return;
        }
        points_checked++;
        if (max_orig_x < pp.orig_x) {
            max_orig_x = pp.orig_x;
        }
        if (min_orig_x > pp.orig_x) {
            min_orig_x = pp.orig_x;
        }
        if (max_orig_y < pp.orig_y) {
            max_orig_y = pp.orig_y;
        }
        if (min_orig_y > pp.orig_y) {
            min_orig_y = pp.orig_y;
        }
        if (max_pre_f_x < pp.pre_f_x) {
            max_pre_f_x = pp.pre_f_x;
        }
        if (min_pre_f_x > pp.pre_f_x) {
            min_pre_f_x = pp.pre_f_x;
        }
        if (max_pre_f_y < pp.pre_f_y) {
            max_pre_f_y = pp.pre_f_y;
        }
        if (min_pre_f_y > pp.pre_f_y) {
            min_pre_f_y = pp.pre_f_y;
        }
        if (max_x < pp.x) {
            max_x = pp.x;
        }
        if (min_x > pp.x) {
            min_x = pp.x;
        }
        if (max_y < pp.y) {
            max_y = pp.y;
        }
        if (min_y > pp.y) {
            min_y = pp.y;
        }
//	if(max_time < pp.time)
//	{
//	    max_time=pp.time;
//	}
//	if(min_time > pp.time)
//	{
//	    min_time=pp.time;
//	}
    }

    public void set_size(int size) {
        if (fixed_size <= 0) {
            if (null != v) {
                v = new Vector<PlotPoint>();
            }
            if (v.size() > size) {
                v.setSize(size);
            }
            if (current_size > size) {
                current_size = size;
            }
        }
    }
    
    public int points_checked = 0;
    public int infinite_points = 0;

    public void RecheckAllPoints() {
        max_x = Double.NEGATIVE_INFINITY;
        min_x = Double.POSITIVE_INFINITY;
        max_y = Double.NEGATIVE_INFINITY;
        min_y = Double.POSITIVE_INFINITY;

        max_orig_x = Double.NEGATIVE_INFINITY;
        min_orig_x = Double.POSITIVE_INFINITY;
        max_orig_y = Double.NEGATIVE_INFINITY;
        min_orig_y = Double.POSITIVE_INFINITY;

        max_pre_f_x = Double.NEGATIVE_INFINITY;
        min_pre_f_x = Double.POSITIVE_INFINITY;
        max_pre_f_y = Double.NEGATIVE_INFINITY;
        min_pre_f_y = Double.POSITIVE_INFINITY;

        min_time = Double.POSITIVE_INFINITY;
        max_time = Double.NEGATIVE_INFINITY;
        this.points_checked = 0;
        this.infinite_points = 0;
        
        if (fixed_size > 0) {
            for (int i = 0; i < plot_point_array.length; i++) {
                PlotPoint pp = plot_point_array[i];
                CheckPoint(pp);
            }
        } else {
            for (int i = 0; i < v.size(); i++) {
                PlotPoint pp = v.elementAt(i);
                CheckPoint(pp);
            }
        }
    }

    public String toString() {
        String s = super.toString() + " {\n";
        s += "\tname=" + name + "\n";
        s += "\tshort_name=" + short_name + "\n";
        s += "\tcurrent_size=" + current_size + "\n";
        s += "\tv_offset=" + v_offset + "\n";
        s += "\tpainting=" + painting + "\n";
        s += "\tp2_count=" + p2_count + "\n";
        s += "\tlast_pp_set=" + last_pp_set + "\n";
        s += "\tlast_pp_set_index=" + last_pp_set_index + "\n";
        if (v != null) {
            s += "\t\tv.size()=" + v.size() + "\n";
            if (print_v || v.size() < 4) {
                for (int i = 0; i < v.size(); i++) {
                    PlotPoint pp = v.elementAt(i);
                    s += "\tv.elementAt(" + i + ") =" + pp + "\n";
                }
            }
            s += "\tmax_orig_x=" + max_orig_x + "\n";
            s += "\tmin_orig_x=" + min_orig_x + "\n";
            s += "\tmax_orig_y=" + max_orig_y + "\n";
            s += "\tmin_orig_y=" + min_orig_y + "\n";
            s += "\tmax_pre_f_x=" + max_pre_f_x + "\n";
            s += "\tmin_pre_f_x=" + min_pre_f_x + "\n";
            s += "\tmax_pre_f_y=" + max_pre_f_y + "\n";
            s += "\tmin_pre_f_y=" + min_pre_f_y + "\n";
            s += "\tmax_x=" + max_x + "\n";
            s += "\tmin_x=" + min_x + "\n";
            s += "\tmax_y=" + max_y + "\n";
            s += "\tmin_y=" + min_y + "\n";
            s += "\tmin_time=" + min_time + "\n";
            s += "\tmax_time=" + max_time + "\n";
        } else {
            s += "\tv=null\n";
        }
        s += "\t#End of PlotData(name="+name+")\n";
        s += "}\n";
        return s;
    }

    @SuppressWarnings("unchecked")
    public void setPlotPointAt(PlotPoint pp, int i) {
        //pp.time=System.currentTimeMillis();
        CheckPoint(pp);
        if (fixed_size > 0) {
            if (plot_point_array.length > i && i >= 0) {
                plot_point_array[i] = pp;
            }
        } else {
            if (v.size() <= i) {
                last_pp_set_index = v.size();
                v.add(pp);
                current_size = v.size();
            } else {
                v.setElementAt(pp, i);
                last_pp_set_index = i;
            }
        }
        last_pp_set = pp;
    }
    int last_added_plot_point_index = 0;

    public void addPlotPoint(PlotPoint pp) {
        if (fixed_size > 0) {
            last_added_plot_point_index++;
            last_added_plot_point_index %= fixed_size;
            setPlotPointAt(pp, last_added_plot_point_index);
            return;
        }
        //pp.time=System.currentTimeMillis();
        last_pp_set_index = v.size();
        CheckPoint(pp);
        v.addElement(pp);
        last_pp_set = pp;
    }
    public int v_out_of_range_count = 0;

//    public final List<PlotPoint> get_v()
//    {
//        if(fixed_size > 0)
//        {
//            v = new Vector<PlotPoint>().copyInto(plot_point_array)
//            return Collections.unmodifiableList();
//        }
//        else
//        {
//            return Collections.unmodifiableList(v);
//        }
//    }
    public PlotPoint getPlotPointAt(int i) {
        if (fixed_size > 0) {
            if (null == plot_point_array[i]) {
                plot_point_array[i] = new PlotPoint();
            }
            return plot_point_array[i];
        }
        try {
            return v.elementAt(i);
        } catch (Exception e) {
            v_out_of_range_count++;
            if (v_out_of_range_count < 2) {
                System.err.println("i=" + i);
                System.err.println("this=" + this);
                e.printStackTrace();
            } else {
                System.err.println("PlotData.getPlotPointAt() : v_out_of_range_count=" + v_out_of_range_count + ", i=" + i + ", name=" + name + ", current_size=" + current_size);
            }
        }
        return null;
    }

    public int v_size() {
        if (fixed_size > 0) {
            return fixed_size;
        }
        if (null == v) {
            current_size = 0;
            return 0;
        }
        if (current_size > v.size()) {
            current_size = v.size();
        }
        return v.size();
    }

    public void set_v(Vector<PlotPoint> new_v) {
        v = new_v;
    }

    public void clear_v() {
        if (null != v) {
            v.removeAllElements();
        }
        v_offset = 0;
        current_size = 0;
        p2_count = 0;
        last_pp_set = null;
        last_pp_set_index = -1;
    }

    /**
     * @return the line_color
     */
    public Color getLine_color() {
        return line_color;
    }

    /**
     * @param line_color the line_color to set
     */
    public void setLine_color(Color line_color) {
        this.line_color = line_color;
    }

    public boolean isVisible() {
        if (null == show) {
            return false;
        }
        for (int i = 0; i < show.length; i++) {
            if (show[i]) {
                return true;
            }
        }
        return false;
    }

        private int pointSize = 4;

    /**
     * Get the value of pointSize
     *
     * @return the value of pointSize
     */
    public int getPointSize() {
        return pointSize;
    }

    /**
     * Set the value of pointSize
     *
     * @param pointSize new value of pointSize
     */
    public void setPointSize(int pointSize) {
        this.pointSize = pointSize;
    }

    /**
     * @return the point_color
     */
    public Color getPoint_color() {
        return point_color;
    }

    /**
     * @param point_color the point_color to set
     */
    public void setPoint_color(Color point_color) {
        this.point_color = point_color;
    }

    /**
     * Return whether plotter i should show this data.
     * @param i index of plotter to get show value of
     * @return the show
     */
    public boolean getShow(int i) {
        if (null == show || show.length <= i || i < 0) {
            return false;
        }
        return show[i];
    }

    public void setShowAll(int total_plotters, boolean _show) {
        if (show == null || show.length < total_plotters) {
            this.show = new boolean[total_plotters];
        }
        for (int i = 0; i < show.length; i++) {
            this.show[i] = _show;
        }
//        PlotGraphJPanel.ResetColors();
    }

    /**
     * Make this data set visible on plotter number i
     * @param i which panel to change show state for
     * @param _show the show to set
     */
    public void setShow(int i, boolean _show) {
        if (i < 0) {
            throw new IllegalArgumentException();
        }
        if (null == show || show.length <= i) {
            boolean new_show[] = new boolean[i + 1];
            if (null != show) {
                System.arraycopy(show, 0, new_show, 0, show.length);
            }
        }
        if (this.show[i] != _show) {
            this.show[i] = _show;
//            PlotGraphJPanel.ResetColors();
        }
    }

    public String getStatsString() throws Exception {
        double y_at_max_x = -1;
        double y_at_min_x = -1;
        double x_at_min_y = -1;
        double x_at_max_y = -1;
        double min_x = Double.POSITIVE_INFINITY;
        double max_x = Double.NEGATIVE_INFINITY;
        double total_x = 0;
        double min_y = Double.POSITIVE_INFINITY;
        double max_y = Double.NEGATIVE_INFINITY;
        double total_y = 0;
        double total_xy = 0;
        int count = 0;
        StringBuffer sb = new StringBuffer();
        Iterator<PlotPoint> it = v.iterator();
        while (it.hasNext()) {
            PlotPoint pp = it.next();
            if (pp == null) {
                continue;
            }
            total_x += pp.orig_x;
            total_y += pp.orig_y;
            total_xy = pp.orig_x * pp.orig_y;
            count++;
            if (min_x > pp.orig_x) {
                min_x = pp.orig_x;
                y_at_min_x = pp.orig_y;
            }
            if (max_x < pp.orig_x) {
                max_x = pp.orig_x;
                y_at_max_x = pp.orig_y;
            }
            if (min_y > pp.orig_y) {
                min_y = pp.orig_y;
                x_at_min_y = pp.orig_x;
            }
            if (max_y < pp.orig_y) {
                max_y = pp.orig_y;
                x_at_max_y = pp.orig_x;
            }
        }
        double max_x_minus_min_x = max_x - min_x;
        double max_y_minus_min_y = max_y - min_y;
        double mean_x = total_x / count;
        double mean_y = total_y / count;
        double total_x_minus_mean_squared = 0;
        double total_y_minus_mean_squared = 0;
        double sum_xy_minus_means = 0;
        it = v.iterator();
        while (it.hasNext()) {
            PlotPoint pp = it.next();
            if (pp == null) {
                continue;
            }
            double x_minus_mean = pp.orig_x - mean_x;
            total_x_minus_mean_squared += x_minus_mean * x_minus_mean;
            double y_minus_mean = pp.orig_y - mean_y;
            total_y_minus_mean_squared += y_minus_mean * y_minus_mean;
            sum_xy_minus_means = x_minus_mean * y_minus_mean;
        }
        double variance_x = total_x_minus_mean_squared / count;
        double variance_y = total_y_minus_mean_squared / count;
        double std_dev_x = Math.sqrt(variance_x);
        double std_dev_y = Math.sqrt(variance_y);
        double xy_correlation = sum_xy_minus_means / (count * std_dev_x * std_dev_y);
        sb.append(this.name + ":\n");
        sb.append(String.format("\t%20s:\t%+.4g\n", "min_x", min_x));
        sb.append(String.format("\t%20s:\t%+.4g\n", "max_x", max_x));
        sb.append(String.format("\t%20s:\t%+.4g\n", "max_x_minus_min_x", max_x_minus_min_x));
        sb.append(String.format("\t%20s:\t%+.4g\n", "mean_x", mean_x));
        sb.append(String.format("\t%20s:\t%+.4g\n", "variance_x", variance_x));
        sb.append(String.format("\t%20s:\t%+.4g\n", "std_dev_x", std_dev_x));
        sb.append(String.format("\t%20s:\t%+.4g\n", "min_y", min_y));
        sb.append(String.format("\t%20s:\t%+.4g\n", "max_y", max_y));
        sb.append(String.format("\t%20s:\t%+.4g\n", "max_y_minus_min_y", max_y_minus_min_y));
        sb.append(String.format("\t%20s:\t%+.4g\n", "mean_y", mean_y));
        sb.append(String.format("\t%20s:\t%+.4g\n", "variance_y", variance_y));
        sb.append(String.format("\t%20s:\t%+.4g\n", "std_dev_y", std_dev_y));
        sb.append(String.format("\t%20s:\t%+.4g\n", "xy_correlation", xy_correlation));
        sb.append(String.format("\t%20s:\t%+.4g\n", "x_at_min_y", x_at_min_y));
        sb.append(String.format("\t%20s:\t%+.4g\n", "x_at_max_y", x_at_max_y));
        sb.append(String.format("\t%20s:\t%+.4g\n", "y_at_min_x", y_at_min_x));
        sb.append(String.format("\t%20s:\t%+.4g\n", "y_at_max_x", y_at_max_x));
        sb.append("\n");
        return sb.toString();
    }
}
