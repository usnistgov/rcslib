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
import java.util.Hashtable;
import java.util.Enumeration;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.PrintStream;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.StringTokenizer;
import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 * JPanel for the actual graph without the controls around the outside.
 * @author Will Shackleford
 */
public class PlotGraphJPanel
        extends JPanel //implements MouseListener, MouseMotionListener
{

    /**
     *   Unique id for this class, Exception implements Serializable interface see documentation
     * for serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613905L;
    public boolean use_buffer = false;
    public int max_points_per_plot = -1; // -1 = no limit
    PlotGraphScreenMap screen_map = null;
    PlotGraphScreenMap array_mode_screen_map = null;
    public Object SyncObject = new Object();
    public int plotter_num = -1;
    private int max_scroll_x = 0;
    private int max_scroll_y = 0;
    public int repaint_count = 0;
    public int paint_count = 0;
    boolean repaint_just_issued = false;
    String line_style = "11011";
    int line_width = 1;
    int last_plots_to_show = 0;
    public int repaint_max_tm_millis = 500;
    private boolean array_mode = false;
    public boolean show_lines = true;
    public volatile boolean show_key = true;
    public volatile boolean k2_mode = false;
    public volatile boolean s_mode = false;
    public volatile boolean c_mode = true;
    public volatile boolean l_mode = false;
    public volatile boolean e_mode = false;
    public volatile boolean xy_mode = false;
    public boolean repaint_needed = true;
    public boolean rescale_to_selected_rectangle_needed = false;
    public boolean show_grid = true;
    public boolean show_axis = true;
    public boolean label_grid = true;
    public boolean mark_points = true;
    public boolean label_points = false;
    public Color axis_color = Color.WHITE;
    public Color grid_color = Color.CYAN;
    public Color back_color = Color.BLACK;
    public Hashtable<String, PlotData> plots = null;
    public boolean m_color = true;
    public volatile boolean show_rect = false;
    Rectangle selected_rectangle = null;
    public static final int ANGLE_DEGREE_UNITS = 1;
    public static final int ANGLE_RAD_UNITS = 2;
    public static final int ANGLE_RADPI_UNITS = 3;
    public int angle_unit_type = ANGLE_DEGREE_UNITS;
    public int radius_lines = 8;
    public int angle_lines = 8;
    public Color zero_rad_color = null;
    public static DecimalFormat df_noexponent = null;
    public static DecimalFormat df_exponent = null;
    public boolean set_l_mode_on_paint = false;
    private Color current_line_color;
    private Color current_point_color;
    JFrame fullScreenJFrame = null;
    public boolean is_full_screen = false;

    public void CopySettings(PlotGraphJPanel pgjp_to_copy) {
        this.array_mode_screen_map = pgjp_to_copy.array_mode_screen_map;
        this.screen_map = pgjp_to_copy.screen_map;
        this.show_axis = pgjp_to_copy.show_axis;
        this.show_grid = pgjp_to_copy.show_grid;
        this.show_key = pgjp_to_copy.show_key;
        this.s_mode = pgjp_to_copy.s_mode;
        this.array_mode = pgjp_to_copy.array_mode;
        this.show_lines = pgjp_to_copy.show_lines;
        this.e_mode = pgjp_to_copy.e_mode;
        this.l_mode = pgjp_to_copy.l_mode;
        this.label_grid = pgjp_to_copy.label_grid;
        this.label_points = pgjp_to_copy.label_points;
        this.show_rect = pgjp_to_copy.show_rect;
        this.selected_rectangle = pgjp_to_copy.selected_rectangle;
        this.c_mode = pgjp_to_copy.c_mode;
        this.k2_mode = pgjp_to_copy.k2_mode;
        this.mark_points = pgjp_to_copy.mark_points;
        this.xy_mode = pgjp_to_copy.xy_mode;
        this.plotter_num = pgjp_to_copy.plotter_num;
    }

    public void SetKeyListener(KeyListener kl) {
        KeyListener kls[] = this.getKeyListeners();
        for (KeyListener kl_to_remove : kls) {
            this.removeKeyListener(kl_to_remove);
        }
        this.addKeyListener(kl);
    }

    public void refresh() {
//	if (this.is_full_screen && fullScreenJPanel != null) {
//	    Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
//	    fullScreenJPanel.SetImage(getImage(d));
//	    fullScreenJPanel.repaint();
//	} else {
//	    repaint();
//	}
        repaint();
    }

    public void HandleResize() {
        PlotGraphScreenMap cur_screen_map = this.screen_map;
        if (this.get_array_mode()) {
            cur_screen_map = this.array_mode_screen_map;
        }
        Dimension d = null;
        if (!this.is_full_screen) {
            d = this.getSize();
        } else {
            d = Toolkit.getDefaultToolkit().getScreenSize();
        }
        this.screen_map.set_screen_w_x_h(d.width, d.height);
        this.array_mode_screen_map.set_screen_w_x_h(d.width, d.height);
        if (this.e_mode) {
            cur_screen_map.equalizeAxis();
        }
        refresh();
    }

    public void SaveOptions() {
        try {
            File f = new File(System.getProperty("user.home"), ".PlotGraphJPanel");
            FileOutputStream fos = new FileOutputStream(f);
            PrintStream ps = new PrintStream(fos);
            ps.println("back_color=" + String.format("0x%6X",0xFFFFFF&back_color.getRGB()));
            ps.println("grid_color=" + String.format("0x%6X",0xFFFFFF&grid_color.getRGB()));
            ps.println("axis_color=" + String.format("0x%6X",0xFFFFFF&axis_color.getRGB()));
            ps.close();
            ps = null;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void LoadOptions() {
        try {
            File f = new File(System.getProperty("user.home"), ".PlotGraphJPanel");
            if (f.exists() && f.canRead()) {
                FileReader fr = new FileReader(f);
                BufferedReader br = new BufferedReader(fr);
                String line = br.readLine();
                while (line != null) {
                    if (line.startsWith("back_color=0x")) {
                        back_color = new Color(Long.valueOf(line.substring(13).trim(),16).intValue());
                    } else if (line.startsWith("grid_color=0x")) {
                        grid_color = new Color(Long.valueOf(line.substring(13).trim(),16).intValue());
                    } else if (line.startsWith("axis_color=0x")) {
                        axis_color = new Color(Long.valueOf(line.substring(13).trim(),16).intValue());
                    } else if (line.startsWith("back_color=")) {
                        back_color = new Color(Integer.valueOf(line.substring(11)).intValue());
                    } else if (line.startsWith("grid_color=")) {
                        grid_color = new Color(Integer.valueOf(line.substring(11)).intValue());
                    } else if (line.startsWith("axis_color=")) {
                        axis_color = new Color(Integer.valueOf(line.substring(11)).intValue());
                    }
                    line = br.readLine();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
//    static {
//        LoadOptions();
//    }
    private BufferedImage bi = null;

    public BufferedImage getImage() {
        if (bi == null
                || bi.getWidth() != this.getWidth()
                || bi.getHeight() != this.getHeight()) {
            bi = new BufferedImage(this.getWidth(), this.getHeight(), BufferedImage.TYPE_3BYTE_BGR);
        }
        Graphics g = bi.getGraphics();
        if (null != back_color) {
            g.setColor(back_color);
            g.fillRect(0, 0, bi.getWidth(), bi.getHeight());
        }
        this.paintAll(g);
        return bi;
    }

    public BufferedImage getImage(final Dimension d) {
        return getImage(d.width, d.height);
    }

    public BufferedImage getImage(final int _width, final int _height) {

        PlotGraphScreenMap cur_screen_map = screen_map;
        if (array_mode) {
            cur_screen_map = array_mode_screen_map;
        }
        final int orig_width = cur_screen_map.get_screen_width();
        final int orig_height = cur_screen_map.get_screen_height();
        cur_screen_map.set_screen_w_x_h(_width, _height);
        if (this.e_mode) {
            cur_screen_map.equalizeAxis();
        }
//		System.out.println("cur_screen_map.get_screen_width() = " + cur_screen_map.get_screen_width());
//	System.out.println("cur_screen_map.get_x_scale() = " + cur_screen_map.get_x_scale());
//	System.out.println("cur_screen_map.get_y_scale() = " + cur_screen_map.get_y_scale());
        try {
            if (bi == null
                    || bi.getWidth() != _width
                    || bi.getHeight() != _height) {
                bi = new BufferedImage(_width, _height, BufferedImage.TYPE_3BYTE_BGR);
            }
            Graphics g = bi.getGraphics();
            if (null != back_color) {
                g.setColor(back_color);
                g.fillRect(0, 0, bi.getWidth(), bi.getHeight());
            }
            this.paintAll(g);
            return bi;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        } finally {
            cur_screen_map.set_screen_w_x_h(orig_width, orig_height);
        }
    }

    static {
        try {
            df_noexponent = new DecimalFormat("###.###");
            try {
                df_exponent = new DecimalFormat("0.####E0");
            } catch (Exception e) {
                df_exponent = df_noexponent;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public String toString() {
        String s = super.toString() + "\n";

        s += "plotter_num  =" + plotter_num + "\n";
        s += "max_scroll_x=" + max_scroll_x + "\n";
        s += "max_scroll_y=" + max_scroll_y + "\n";
        s += "repaint_count=" + repaint_count + "\n";
        s += "paint_count=" + paint_count + "\n";
        s += "max_points_per_plot=" + max_points_per_plot + "\n";
        s += "c_mode=" + c_mode + "\n";
        s += "s_mode=" + s_mode + "\n";
        s += "l_mode=" + l_mode + "\n";
        if (screen_map != null) {
            s += "screen_map=" + screen_map + "\n";
        } else {
            s += "screen_map=null;";
        }
        if (array_mode_screen_map != null) {
            s += "array_mode_screen_map=" + array_mode_screen_map + "\n";
        } else {
            s += "array_mode_screen_map=null;";
        }
        return s;
    }

    static public void DebugPrint2(String s) {
        try {
            Throwable t = new Throwable();
            StackTraceElement ste[] = t.getStackTrace();
            System.out.println(ste[1].getFileName() + ":" + ste[1].getLineNumber() + " " + s);
            System.out.println("time=" + System.currentTimeMillis());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void DebugPrint(String s) {
        try {
            if (!PlotterCommon.debug_on) {
                return;
            }
            Throwable t = new Throwable();
            StackTraceElement ste[] = t.getStackTrace();
            System.out.println(ste[1].getFileName() + ":" + ste[1].getLineNumber() + " " + s);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void ErrorPrint(String s) {
        try {
            Throwable t = new Throwable();
            StackTraceElement ste[] = t.getStackTrace();
            if (PlotterCommon.debug_on) {
                System.out.println("ErrorPrint + " + ste[1].getFileName() + ":" + ste[1].getLineNumber() + " " + s);
            }
            System.err.println(ste[1].getFileName() + ":" + ste[1].getLineNumber() + " " + s);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static String format_double(double d) {
        try {
            if (Math.abs(d) <= 0.001 || Math.abs(d) >= 10000.0) {
                if (null == df_exponent) {
                    return Double.toString(d);
                }
                return df_exponent.format(d);
            } else {
                if (null == df_noexponent) {
                    return Double.toString(d);
                }
                return df_noexponent.format(d);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return Double.toString(d);
    }

    public void set_array_mode(boolean _array_mode) {
        array_mode = _array_mode;
    }

    public boolean get_array_mode() {
        return array_mode;
    }

    public void Zoom(double scale_factor) {
        if (array_mode) {
            array_mode_screen_map.zoom_x(scale_factor);
            if (!s_mode) {
                array_mode_screen_map.zoom_y(scale_factor);
            }
            if (e_mode) {
                array_mode_screen_map.equalizeAxis();
            }
        } else {
            screen_map.zoom_x(scale_factor);
            if (!s_mode) {
                screen_map.zoom_y(scale_factor);
            }
            if (e_mode) {
                screen_map.equalizeAxis();
            }
        }
    }

    public void ZoomOut() {
        Zoom(2.0);
    }

    public void ZoomIn() {
        Zoom(0.5);
    }

    public void ScrollRight() {
        double min_orig_x = Double.POSITIVE_INFINITY;
        double max_orig_x = Double.NEGATIVE_INFINITY;
        int nump = 0;

        if (array_mode) {
            return;
        }
        for (Object obj : plots.values()) {
            PlotData pd = (PlotData) obj;
            if (pd.array_type != array_mode) {
                continue;
            }
            if (min_orig_x > pd.min_orig_x) {
                min_orig_x = pd.min_orig_x;
            }
            if (max_orig_x < pd.max_orig_x) {
                max_orig_x = pd.max_orig_x;
            }
            nump++;
        }
        if (nump > 0) {
            screen_map.set_abs_x_min(min_orig_x);
            screen_map.set_abs_x_max(max_orig_x);
            double x_diff = screen_map.get_x_max() - screen_map.get_x_min();
            screen_map.set_x_show_area(max_orig_x - x_diff, max_orig_x);
            //screen_map.set_x_show_area(min_orig_x,max_orig_x);
        }
        if (e_mode) {
            screen_map.equalizeAxis();
        }
    }

    public void FitToGraph() {
        double min_orig_x = Double.POSITIVE_INFINITY;
        double max_orig_x = Double.NEGATIVE_INFINITY;
        double min_orig_y = Double.POSITIVE_INFINITY;
        double max_orig_y = Double.NEGATIVE_INFINITY;
        int nump = 0;

        PlotData min_x_pd = null;
        PlotData max_x_pd = null;
        PlotData min_y_pd = null;
        PlotData max_y_pd = null;

        for (Object obj : plots.values()) {
            PlotData pd = (PlotData) obj;
            if (pd.array_type != array_mode) {
                continue;
            }
            if (!pd.getShow(plotter_num)) {
                continue;
            }
            pd.RecheckAllPoints();
            if (min_orig_x > pd.min_orig_x) {
                min_x_pd = pd;
                min_orig_x = pd.min_orig_x;
            }
            if (max_orig_x < pd.max_orig_x) {
                max_orig_x = pd.max_orig_x;
                max_x_pd = pd;
            }
            if (min_orig_y > pd.min_orig_y) {
                min_orig_y = pd.min_orig_y;
                min_y_pd = pd;
            }
            if (max_orig_y < pd.max_orig_y) {
                max_orig_y = pd.max_orig_y;
                max_y_pd = pd;
            }
            nump++;
        }
	if(nump > 1 && PlotterCommon.debug_on)
	{
	    PlotterCommon.DebugPrint("min_orig_x="+min_orig_x);
	    PlotterCommon.DebugPrint("min_x_pd="+min_x_pd);
	    PlotterCommon.DebugPrint("max_orig_x="+max_orig_x);
	    PlotterCommon.DebugPrint("max_x_pd="+max_x_pd);
	    PlotterCommon.DebugPrint("min_orig_y="+min_orig_y);
	    PlotterCommon.DebugPrint("min_y_pd="+min_y_pd);
	    PlotterCommon.DebugPrint("min_orig_y="+max_orig_y);
	    PlotterCommon.DebugPrint("max_y_pd="+max_y_pd);
	}
//        double diff_x = max_orig_x - min_orig_x;
//        double diff_y = max_orig_y - min_orig_y;
        if (nump > 0) {
            if (array_mode) {
                array_mode_screen_map.set_abs_x_min(min_orig_x);
                array_mode_screen_map.set_abs_x_max(max_orig_x);
                array_mode_screen_map.set_x_show_area(min_orig_x, max_orig_x);
                if (!s_mode) {
                    array_mode_screen_map.set_abs_y_min(min_orig_y);
                    array_mode_screen_map.set_abs_y_max(max_orig_y);
                    array_mode_screen_map.set_y_show_area(min_orig_y, max_orig_y);
                }
                if (e_mode) {
                    array_mode_screen_map.equalizeAxis();
                }
            } else {
                screen_map.set_abs_x_min(min_orig_x);
                screen_map.set_abs_x_max(max_orig_x);
                screen_map.set_x_show_area(min_orig_x, max_orig_x);
                if (!s_mode) {
                    screen_map.set_abs_y_min(min_orig_y);
                    screen_map.set_abs_y_max(max_orig_y);
                    screen_map.set_y_show_area(min_orig_y, max_orig_y);
                }
                if (e_mode) {
                    screen_map.equalizeAxis();
                }
            }
        }
    }

    public void FitY() {
        double min_orig_y = Double.POSITIVE_INFINITY;
        double max_orig_y = Double.NEGATIVE_INFINITY;
        int nump = 0;
        if (s_mode || array_mode) {
            return;
        }

        for (Object obj : plots.values()) {
            PlotData pd = (PlotData) obj;
            if (pd.array_type != array_mode) {
                continue;
            }
            if (!pd.getShow(plotter_num)) {
                continue;
            }
            pd.RecheckAllPoints();
            if (min_orig_y > pd.min_orig_y) {
                min_orig_y = pd.min_orig_y;
            }
            if (max_orig_y < pd.max_orig_y) {
                max_orig_y = pd.max_orig_y;
            }
            nump++;
        }
        if (nump > 0) {
            if (!s_mode) {
                screen_map.set_abs_y_min(min_orig_y);
                screen_map.set_abs_y_max(max_orig_y);
                screen_map.set_y_show_area(min_orig_y, max_orig_y);
                if (e_mode) {
                    screen_map.equalizeAxis();
                }
            }
        }
    }

    public PlotGraphJPanel() {
        LoadOptions();
        Dimension d = getSize();
        if (d.width > 1 && d.height > 1) {
            screen_map = new PlotGraphScreenMap(d.width, d.height);
            array_mode_screen_map = new PlotGraphScreenMap(d.width, d.height);
        } else {
            screen_map = new PlotGraphScreenMap(600, 300);
            array_mode_screen_map = new PlotGraphScreenMap(600, 300);
        }

        //      System.out.println("start_width="+start_width+",start_height="+start_height+", getSize() = "+getSize());
        //Thread.dumpStack();
        try {
            Font f = getFont();
            if (f != null) {
                if (f.getSize() < 12) {
                    setFont(f.deriveFont(12));
                }
            } else {
                setFont(new Font("SanSerif", Font.PLAIN, 20));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        repaint_needed = true;
        image_needs_to_be_updated = true;
        repaint_just_issued = false;
        selected_rectangle = new Rectangle();
        show_rect = false;
        //	changeScale(x_min, x_max, scroll_width, y_min, y_max, scroll_height);
        if (null == plots) {
            plots = new Hashtable<String, PlotData>();
        }
        if (use_buffer) {
            bufferImage = createImage(d.width, d.height);
        }
    }
    int plot_clicking_on = -1;
    long time_of_last_click = 0;

    public void SetReverseX(boolean _new_reverse_x) {
        if (null != this.screen_map) {
            this.screen_map.SetReverseX(_new_reverse_x);
        }
    }

    public void mouseClicked(MouseEvent evt) {
        int x = evt.getX();
        int y = evt.getY();
        if (show_key && !s_mode) {
            if (x > 30 && x < 70 && y > 10 && y < (keyVector.size() + 1) * 25 + 25) {
                int new_plot_clicking_on = (y - 15) / 25;
                //System.out.println("new_plot_clicking_on="+new_plot_clicking_on+", keyVector.size()="+keyVector.size());
                if (new_plot_clicking_on < 0) {
                    new_plot_clicking_on = 0;
                }
                if (new_plot_clicking_on > keyVector.size() - 1) {
                    new_plot_clicking_on = keyVector.size() - 1;
                }
                PlotData pd = keyVector.get(new_plot_clicking_on);
                if (null == pd) {
                    return;
                }
                pd.setShow(plotter_num, !pd.getShow(plotter_num));
                //System.out.println("pd.show[plotter_num  ="+plotter_num  +"] = "+pd.show[plotter_num  ]);
            }
        }
        plot_clicking_on = -1;
    }

    public void mouseEntered(MouseEvent evt) {
    }

    public void mouseExited(MouseEvent evt) {
    }

    public void mousePressed(MouseEvent evt) {
        int x = evt.getX();
        int y = evt.getY();
        last_x = x;
        last_y = y;
        repaint_needed = true;
        selected_rectangle.x = x;
        selected_rectangle.y = y;
        selected_rectangle.width = 0;
        selected_rectangle.height = 0;
        show_rect = true;
    }

    public void mouseMoved(MouseEvent evt) {
        last_x = evt.getX();
        last_y = evt.getY();
    }

    public void mouseDragged(MouseEvent evt) {
        int x = evt.getX();
        int y = evt.getY();
        last_x = x;
        last_y = y;
        int temp;
        if (PlotterCommon.debug_on) {
            DebugPrint("mouseDragged(" + evt + ") x=" + x + ", y=" + y);
        }
        repaint_needed = true;
        if (x > selected_rectangle.x) {
            selected_rectangle.width = x - selected_rectangle.x;
        } else {
            temp = selected_rectangle.x;
            selected_rectangle.x = x;
            selected_rectangle.width += temp - selected_rectangle.x;
        }
        if (y > selected_rectangle.y) {
            selected_rectangle.height = y - selected_rectangle.y;
        } else {
            temp = selected_rectangle.y;
            selected_rectangle.y = y;
            selected_rectangle.height += temp - selected_rectangle.y;
        }
        show_rect = true;
        refresh();
    }

    public void mouseReleased(MouseEvent evt) {
        int x = evt.getX();
        int y = evt.getY();
        last_x = x;
        last_y = y;
        if (PlotterCommon.debug_on) {
            DebugPrint("mouseReleased(" + evt + ") x=" + x + ", y=" + y);
        }
        if (show_rect && selected_rectangle.width > 5 && selected_rectangle.height > 5) {
            rescale_to_selected_rectangle_needed = true;
            show_rect = false;
            repaint_just_issued = false;
            //System.out.println("mouseUp setting repaint_needed = true");
            repaint_needed = true;
            return;
        }
        show_rect = false;
        image_needs_to_be_updated = true;
        refresh();
    }

    public void ResetMinXToZero() {
        try {
            this.FitToGraph();
            double x_min = this.screen_map.get_abs_x_min();
            for (PlotData pd : this.plots.values()) {
                pd.AddX(-x_min);
            }
            this.FitToGraph();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public boolean ColorTooClosePreviousPlot(Color c1, long min_diff) {
        try {
            if (null == plots) {
                return false;
            }
            //long min_diff = 400 / (plots.size() + 1);
            for (PlotData pd : plots.values()) {
                if (ColorsTooClose(c1, pd.getLine_color(), min_diff)
                        || ColorsTooClose(c1, pd.getPoint_color(), min_diff)) {
                    return true;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean ColorsTooClose(Color c1, Color c2, long min_diff) {
        try {
            if (null == c2) {
                return false;
            }
            long red_diff = Math.abs(c1.getRed() - c2.getRed());
            long green_diff = Math.abs(c1.getGreen() - c2.getGreen());
            long blue_diff = Math.abs((c1.getBlue() - c2.getBlue()));
            long total_diff = red_diff + blue_diff + green_diff;
//			if (min_diff > 25) {
//				System.out.println("c1 = " + c1);
//				System.out.println("c2 = " + c2);
//				System.out.println("min_diff = " + min_diff);
//				if (true) {
//					System.out.print("red_diff=" + red_diff + ", green_diff=" + green_diff + ", blue_diff=" + blue_diff + ", total_diff=" + total_diff + "\n");
//				}
//			}
            if (total_diff < min_diff) {
                return true;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public void AddPlot(PlotData pd, String name) {
        if (null == name || clearing_plots) {
            return;
        }
        if (name.length() < 1) {
            return;
        }
        pd.name = name;
        AddPlot(pd);
    }

    @SuppressWarnings("unchecked")
    public void AddPlot(PlotData pd) {
        if (PlotterCommon.debug_on) {
            DebugPrint("Adding  plot " + pd + ", " + pd.name);
            DebugPrint(" to " + plots);
            //Thread.dumpStack();
        }
        final String name = pd.name;
        if (null == pd) {
            return;
        }
        pd.short_name = pd.name;
        current_line_color = PlotterCommon.nextColor();
        Color orig_color_line_color = current_line_color;
        int tries = 0;
        while (ColorsTooClose(current_line_color, back_color, 500 - tries * 5)
                || ColorsTooClose(current_line_color, grid_color, 50 - tries)
                || ColorsTooClose(current_line_color, axis_color, 50 - tries)
                || ColorTooClosePreviousPlot(current_line_color, (800 / (plots.size() + 1 + (tries / 5))))) {
            current_line_color = PlotterCommon.nextColor();
            tries++;
            if (current_line_color == orig_color_line_color || tries > 40) {
                break;
            }
        }
        current_point_color = current_line_color;
        if (null == plots) {
            plots = new Hashtable();
        }
        if (!plots.containsKey(name)) {
            if (PlotterCommon.debug_on) {
                DebugPrint("Adding  plot " + name);
            }
            if (null == pd.getLine_color()) {
                pd.setLine_color(current_line_color);
            }
            if (null == pd.getPoint_color()) {
                pd.setPoint_color(current_point_color);
            }
            if (pd.line_width < 1) {
                pd.line_width = (plots.size() % 4) + 1;
            }
        } else {
            PlotData old_value = plots.get(name);
            if (null == pd.getLine_color()) {
                pd.setLine_color(old_value.getLine_color());
            }
            if (null == pd.getPoint_color()) {
                pd.setPoint_color(old_value.getPoint_color());
            }
            if (pd.line_width < 1) {
                pd.line_width = old_value.line_width;
            }
            if (null == pd.line_style) {
                pd.line_style = old_value.line_style;
            }
        }
        AddPlotStep2(pd);
        if (PlotterCommon.debug_on) {
            DebugPrint("Added plot " + pd + ", " + name);
        }
    }

    private void AddPlotStep2(PlotData pd) {
        if (PlotterCommon.debug_on) {
            DebugPrint("pd =" + pd + ", name=" + pd.name + ", plots=" + plots);
        }
        plots.put(pd.name, pd);
        keyVector = new ArrayList<PlotData>();
        Enumeration plots_enum = plots.elements();

        while (plots_enum.hasMoreElements()) {
            PlotData pd2 = (PlotData) plots_enum.nextElement();
            //DebugPrint2("pd2="+pd2+", pd2.name="+pd2.name);
            keyVector.add(pd2);
        }
        if (null == keyVectorComparator) {
            this.setupKeyVectorComparator();
        }
        Collections.sort(keyVector, keyVectorComparator);
        String commonStart = keyVector.get(0).name;
        Iterator<PlotData> itr = keyVector.iterator();
        while (commonStart.length() > 0 && itr.hasNext()) {
            PlotData pdk = itr.next();
            pdk.short_name = pdk.name;
            String n = pdk.short_name;
            while (commonStart.length() > 0) {
                if (!n.startsWith(commonStart)) {
                    commonStart = commonStart.substring(0, commonStart.length() - 1);
                } else {
                    break;
                }
            }
        }
        while (commonStart.length() > 0) {
            char c = commonStart.charAt(commonStart.length() - 1);
            if (!Character.isLetter(c)) {
                break;
            }
            if (commonStart.length() > 1 && Character.isUpperCase(c)) {
                char cl = commonStart.charAt(commonStart.length() - 2);
                if (!Character.isUpperCase(cl)) {
                    commonStart = commonStart.substring(0, commonStart.length() - 1);
                    break;
                }
            }
            commonStart = commonStart.substring(0, commonStart.length() - 1);
        }
        int csl = commonStart.length();
        if(PlotterCommon.debug_on) {
            DebugPrint("commonStart = " + commonStart);
            DebugPrint("csl = " + csl);
        }
        itr = keyVector.iterator();
        while (itr.hasNext()) {
            PlotData pdk = itr.next();
            if (csl > 0) {
                pdk.short_name = pdk.name.substring(csl);
            } else {
                pdk.short_name = pdk.name;
            }
            if (PlotterCommon.debug_on) {
                DebugPrint("pdk.name = " + pdk.name);
                DebugPrint("pdk.short_name = " + pdk.short_name);
            }
        }
        ResetColors();
    }

    public void ResetColors() {
        int num_visible = 0;
        if (null == keyVector) {
            return;
        }
        if (null == keyVectorComparator) {
            this.setupKeyVectorComparator();
        }
        Collections.sort(keyVector, keyVectorComparator);
        for (int i = 0; i < keyVector.size(); i++) {
            PlotData pdk = keyVector.get(i);
            if (pdk.isVisible()) {
                num_visible++;
                if (PlotterCommon.debug_on) {
                    DebugPrint(pdk.name + " is visible");
                    DebugPrint("num_visible = " + num_visible);
                }
            }
        }
        if (num_visible <= 0) {
            return;
        }
        float h = 0;
        for (int i = 0; i < keyVector.size(); i++) {
            PlotData pdk = keyVector.get(i);
            if (pdk.isVisible()) {
                h += (1f / ((float) num_visible));
                if (PlotterCommon.debug_on) {
                    System.out.println("Hue for " + pdk.name + " = " + h);
                }
                Color c = new Color(Color.HSBtoRGB(h, 1.0f, 1.0f));
                pdk.setLine_color(c);
                pdk.setPoint_color(c);
            }
        }
    }

    public PlotData GetPlot(String name) {
        try {
            if (null != plots) {
                return plots.get(name);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public void RemoveAllPlots() {
        plots = new Hashtable<String, PlotData>();
        keyVector = new ArrayList<PlotData>();
    }
    boolean clearing_plots = false;

    public void ClearAllData() {
        try {
            clearing_plots = true;
            if (null != keyVector) {
                for (int ki = 0; ki < keyVector.size(); ki++) {
                    PlotData plot_data = keyVector.get(ki);
                    plot_data.clear_v();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        clearing_plots = false;
    }

    public void RemovePlot(String name) {
        plots.remove(name);
        if (null != keyVector) {
            keyVector = new ArrayList<PlotData>();
            Enumeration plots_enum = plots.elements();
            while (plots_enum.hasMoreElements()) {
                PlotData pd2 = (PlotData) plots_enum.nextElement();
                keyVector.add(pd2);
            }
        }
        image_needs_to_be_updated = true;
    }

    public void AddPointToPlot(PlotData pd, double x, double y, boolean connected, double pre_f_x, double pre_f_y) {
        if (clearing_plots) {
            return;
        }
        if (pd.delete_me) {
            return;
        }
        // for(int i = 0; i < dplotter.dplotter_vector.size(); i++)
// 	{
// 	    dplotter dp = (dplotter) dplotter.dplotter_vector.elementAt(i);
// 	    if(null != dp)
// 	    {
// 		if(null != dp.plot_graph)
// 		{
// 		    dp.plot_graph.repaint_needed = true;
// 		    dp.plot_graph.repaint_just_issued = false;
// 		}
// 	    }
// 	}
        if (max_points_per_plot <= 0 || max_points_per_plot > pd.v_size()) {
            pd.addPlotPoint(new PlotPoint((int) x, (int) y, x, y, pre_f_x, pre_f_y));
            pd.current_size++;
        } else {
            int v_index = (pd.v_offset + max_points_per_plot) % pd.v_size();
            PlotPoint pp = pd.getPlotPointAt(v_index);
            pp.x = (int) x;
            pp.y = (int) y;
            pp.orig_x = x;
            pp.orig_y = y;
            pp.pre_f_x = pre_f_x;
            pp.pre_f_y = pre_f_y;
            //pp.connected = connected;
            pd.setPlotPointAt(pp, v_index);
            if (pd.current_size >= pd.v_size()) {
                pd.current_size = pd.v_size();
            }
            if (pd.current_size >= max_points_per_plot) {
                pd.v_offset = (pd.v_offset + 1) % pd.v_size();
            } else {
                pd.current_size++;
            }
        }
    }

    public void AddPointToArrayPlot(PlotData pd, int index, double x, double y, double pre_f_x, double pre_f_y) {
        if (clearing_plots) {
            return;
        }
        if (pd.delete_me) {
            return;
        }
        // for(int i = 0; i < dplotter.dplotter_vector.size(); i++)
// 	{
// 	    dplotter dp = (dplotter) dplotter.dplotter_vector.elementAt(i);
// 	    if(null != dp)
// 	    {
// 		if(null != dp.plot_graph)
// 		{
// 		    dp.plot_graph.repaint_needed = true;
// 		    dp.plot_graph.repaint_just_issued = false;
// 		}
// 	    }
// 	}
        PlotPoint pp = null;
        if (pd.v_size() > index) {
            pp = pd.getPlotPointAt(index);
        }
        if (null == pp) {
            pp = new PlotPoint((int) x, (int) y, x, y, pre_f_x, pre_f_y);
        } else {
            pp.x = (int) x;
            pp.y = (int) y;
            pp.orig_x = x;
            pp.orig_y = y;
            pp.pre_f_x = pre_f_x;
            pp.pre_f_y = pre_f_y;
            // pp.connected = true;
        }
        pd.setPlotPointAt(pp, index);
    }

    public void equalizeAxis() {
        image_needs_to_be_updated = true;
        try {
            if (array_mode) {
                array_mode_screen_map.equalizeAxis();
            } else {
                screen_map.equalizeAxis();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void UpdateDisplay(boolean force_repaint) {
        if (force_repaint) {
            image_needs_to_be_updated = true;
            repaint_needed = true;
        }

        if (array_mode) {
            if (array_mode_screen_map.get_changed()) {
                image_needs_to_be_updated = true;
                repaint_needed = true;
                array_mode_screen_map.clear_changed();
            }
        } else {
            if (screen_map.get_changed()) {
                image_needs_to_be_updated = true;
                repaint_needed = true;
                screen_map.clear_changed();
            }
        }
        if (show_rect && use_buffer) {
            repaint_just_issued = false;
            repaint_count++;
            repaint(repaint_max_tm_millis);
            return;
        }
        if (repaint_needed && !repaint_just_issued) {
            if (!show_rect) {
                image_needs_to_be_updated = true;
                repaint_just_issued = true;
            }
            repaint_count++;
            repaint(repaint_max_tm_millis);
        }
    }
    public ArrayList<PlotData> keyVector = null;
    private String order_array[] = null;
    Comparator<PlotData> keyVectorComparator = null;

    void setupKeyVectorComparator() {
        keyVectorComparator = new Comparator<PlotData>() {

            @Override
            public int compare(PlotData o1, PlotData o2) {
                int o1_pos = 0;
                int o2_pos = 0;
                if (null != order_array) {
                    o1_pos = order_array.length + 1;
                    o2_pos = order_array.length + 1;
                    for (int i = 0; i < order_array.length; i++) {
                        String order_pattern = order_array[i];
                        if (o1.short_name.matches(order_pattern)
                                && o1_pos > i) {
                            o1_pos = i;
                        }
                        if (o2.short_name.matches(order_pattern)
                                && o2_pos > i) {
                            o2_pos = i;
                        }
                        if (o1_pos < i && o2_pos < i) {
                            break;
                        }
                    }
                }
                if (PlotterCommon.debug_on) {
                    DebugPrint("o1_pos=" + o1_pos + ",o1.short_name=" + o1.short_name);
                    DebugPrint("o2_pos=" + o2_pos + ",o2.short_name=" + o2.short_name);
                }
                int ret = (o2_pos - o1_pos);
                if (o1_pos == o2_pos) {
                    ret = o2.short_name.compareTo(o1.short_name);
                }
                if (PlotterCommon.debug_on) {
                    DebugPrint("keyVectorComparator returning " + ret);
                }
                return ret;
            }
        };
    }

    public void setPlotOrder(String s) {
        if(PlotterCommon.debug_on) {
            DebugPrint("setPlotOrder(" + s + ") called.");
        }
        if (null == keyVector) {
            return;
        }
        order_array = null;
        if (null != s) {
            StringTokenizer st = new StringTokenizer(s, ", ");
            order_array = new String[st.countTokens()];
            for (int i = 0; i < order_array.length && st.hasMoreTokens(); i++) {
                order_array[i] = st.nextToken();
            }
        }
        if (PlotterCommon.debug_on) {
            DebugPrint("starting keyVector order.");
            for (int i = 0; i < keyVector.size(); i++) {
                DebugPrint("keyVector.get(" + i + ").short_name = " + keyVector.get(i).short_name);
            }
            DebugPrint("");
        }
        setupKeyVectorComparator();
        Collections.sort(keyVector, keyVectorComparator);
        if (PlotterCommon.debug_on) {
            DebugPrint("new keyVector order.");
            for (int i = 0; i < keyVector.size(); i++) {
                DebugPrint("keyVector.get(" + i + ").short_name = " + keyVector.get(i).short_name);
            }
            DebugPrint("");
        }
    }
    int last_x;
    int last_y;
    public String extra_sh_str = null;
    public String short_extra_sh_str = null;

    private void paintKey(Graphics g) {
        try {
            if (null == keyVector) {
                return;
            }
            if (m_color) {
                g.setColor(Color.white);
            } else {
                g.setColor(Color.black);
            }
            int y_pos = 20;
            int x_pos = 30;
            if (plots != null) {
                for (int i = 0; i < keyVector.size(); i++) {
                    String shStr = " (hidden)";
                    PlotData pd = keyVector.get(i);
                    if (pd.no_key) {
                        continue;
                    }
                    boolean show_this_plot = false;
                    if (pd.getShow(plotter_num)) {
                        shStr = " (visible)";
                        show_this_plot = true;
                    }
                    if (null != pd.add_to_key) {
                        shStr += pd.add_to_key;
                    }
                    if (null != extra_sh_str) {
                        shStr += extra_sh_str;
                    }
                    if (pd.v_size() < 1 || pd.name.length() < 1) {
                        continue;
                    }
                    if (show_this_plot) {
                        if (c_mode) {
                            if (m_color) {
                                g.setColor(pd.getLine_color());
                            }
                            g.drawLine(x_pos, y_pos, x_pos + 40, y_pos);
                        }
                        if (m_color) {
                            g.setColor(pd.getPoint_color());
                        }
                        int ptSz = pd.getPointSize();
                        g.fillRect(x_pos + 20-ptSz/2, y_pos - ptSz/2, ptSz, ptSz);
                    } else if (c_mode) {
                        if (m_color && axis_color != null) {
                            g.setColor(axis_color);
                        }
                        g.drawLine(x_pos + 10, y_pos, x_pos + 30, y_pos);
                    }
                    if (m_color) {
                        g.setColor(pd.getPoint_color());
                    }
                    if(this.m_use_shortname) {
                        g.drawString(pd.short_name + shStr, x_pos + 50, y_pos + 5);
                    } else {
                        g.drawString(pd.name + shStr, x_pos + 50, y_pos + 5);
                    }
                    y_pos += 25;
                }
            }
            PlotGraphScreenMap cur_screen_map = screen_map;
            if (array_mode) {
                cur_screen_map = array_mode_screen_map;
            }
            double last_x_pos = cur_screen_map.get_x_value(last_x);
            double last_y_pos = cur_screen_map.get_y_value(last_y);
            if (PlotterCommon.debug_on) {
                DebugPrint("last_x=" + last_x + ", last_x_pos=" + last_x_pos);
                DebugPrint("last_y=" + last_y + ", last_y_pos=" + last_y_pos);
            }
            String pos_string = null;

            // spacing("+cur_screen_map.get_x_grid_spacing()+","+cur_screen_map.get_y_grid_spacing()+")",
            if (k2_mode) {
                if (m_color && axis_color != null) {
                    g.setColor(axis_color);
                }
                if (show_rect) {
                    pos_string = "{" + cur_screen_map.get_x_value(selected_rectangle.x) + ", " + cur_screen_map.get_y_value(selected_rectangle.y) + "}  (" + last_x_pos + ", " + last_y_pos + ")";
                } else {
                    pos_string = "(" + last_x_pos + ", " + last_y_pos + ")";
                }
                g.drawString(pos_string, x_pos + 50, y_pos + 5);
                y_pos += 25;
                g.drawString("grid (" + cur_screen_map.get_x_grid() + "," + cur_screen_map.get_y_grid() + ") : scale(" + cur_screen_map.get_x_scale() + ", " + cur_screen_map.get_y_scale() + ")",
                        x_pos + 50, y_pos + 5);

                y_pos += 25;
                g.drawString("abs_x (" + cur_screen_map.get_abs_x_min() + "," + cur_screen_map.get_abs_x_max() + ") : abs_y(" + cur_screen_map.get_abs_y_min() + ", " + cur_screen_map.get_abs_y_max() + ")", x_pos + 50, y_pos + 5);
                y_pos += 25;
                g.drawString("x (" + cur_screen_map.get_x_min() + "," + cur_screen_map.get_x_max() + ") : y(" + cur_screen_map.get_y_min() + ", " + cur_screen_map.get_y_max() + ")", x_pos + 50, y_pos + 5);
                y_pos += 25;
                g.drawString("scroll_x=" + cur_screen_map.get_scroll_x() + ", scroll_width=" + cur_screen_map.get_scroll_width() + ", scroll_y=" + cur_screen_map.get_scroll_y() + ", scroll_height= " + cur_screen_map.get_scroll_height() + ")", x_pos + 50, y_pos + 5);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    private boolean m_use_shortname = true;
    
    public void setUseShortname(boolean _UseShortname) {
        this.m_use_shortname = _UseShortname;
        this.repaint();
    }
    
    public boolean isUseShortnama(boolean m_use_shortname) {
        return this.m_use_shortname;
    }
    
    int last_x1;
    int last_x2;
    int last_y1;
    int last_y2;
    static final boolean UseOrigDrawLine = true;//    private void EnhancedDrawLine(Graphics  g,int x1, int y1, int x2, int y2)
//    {
//	int temp;
//	int x_low;
//	int x_high;
//	int x_pos;
//	int y_low;
//	int y_high;
//	int y_pos;
//	int x_diff;
//	int y_diff;
//	int i;
//	double m;
//	double inverse_m;
//	int starting_i = 0;
//	try
//	{
//	    if(x1 == last_x1 && x2 == last_x2 && y1 == last_y1 && y2 == last_y2)
//	    {
//		return;
//	    }
//	    if(UseOrigDrawLine)
//	    {
//		g.drawLine(x1,y1,x2,y2);
//		return;
//	    }
//	    PlotGraphScreenMap cur_screen_map = screen_map;
//	    if(array_mode)
//	    {
//		cur_screen_map = array_mode_screen_map;
//	    }
//	    last_x1 = x1;
//	    last_x2 = x2;
//	    last_y1 = y1;
//	    last_y2 = y2;
//	    if(x2 > x1)
//	    {
//		x_low = x1;
//		x_high = x2;
//	    }
//	    else
//	    {
//		x_low = x2;
//		x_high = x1;
//	    }
//	    if(y2 > y1)
//	    {
//		y_low = y1;
//		y_high = y2;
//	    }
//	    else
//	    {
//		y_low = y2;
//		y_high = y1;
//	    }
//	    x_diff = x_high - x_low;
//	    y_diff = y_high - y_low;
//	    
//	    if(x_diff < 3 && y_diff < 3)
//	    {
//		return;
//	    }
//	    // Handle vertical line and sindgle point case separately.
//	    if(x_diff < 1)
//	    {
//		// Handle sindgle point case separately.
//		if(y_diff < 1)
//		{
//		    g.fillRect(x_low,y_low,1,1);
//		    return;
//		}
//		g.fillRect(x_low,y_low,line_width,y_diff);
//		return;
//	    }
//	    // Handle horizontal line case separately.
//	    if(y_diff < 1)
//	    {
//		g.fillRect(x_low,y_low,x_diff,line_width);
//		return;
//	    }
//	    
//	    if(y_diff > x_diff)
//	    {
//		// Swap the points to make the higher point (x1,y1)
//		if(y1 > y2)
//		{
//		    temp = x2;
//		    x2 = x1;
//		    x1 = temp;
//		    temp = y2;
//		    y2 = y1;
//		    y1 = temp;
//		}
//		if(y1 > 0)
//		{
//		    starting_i = 0;
//		}
//		else
//		{
//		    starting_i = -y1;
//		}
//		for(i = starting_i; i < y_diff; i++)
//		{
//		    if(line_style != null)
//		    {
//			if(line_style.charAt(i%line_style.length()) == '0')
//			{
//			    continue;
//			}
//		    }
//		    inverse_m = ((double)(x2 - x1))/((double)(y2 -y1));
//		    y_pos = y1 + i;
//		    if(y_pos > cur_screen_map.get_screen_height())
//		    {
//			break;
//		    }
//		    x_pos = (int) (x1 + inverse_m*i);
//		    if(x_pos > cur_screen_map.get_screen_width()+line_width/2 && inverse_m > 0)
//		    {
//			break;
//		    }
//		    if(x_pos < -line_width/2 && inverse_m < 0)
//		    {
//			break;
//		    }
//		    g.fillRect(x_pos-line_width/2,y_pos,line_width,1);
//		}
//	    }
//	    else
//	    {
//		// Swap the points to make the one to the left (x1,y1)
//		if(x1 > x2)
//		{
//		    temp = x2;
//		    x2 = x1;
//		    x1 = temp;
//		    temp = y2;
//		    y2 = y1;
//		    y1 = temp;
//		}
//		if(x1 > 0)
//		{
//		    starting_i = 0;
//		}
//		else
//		{
//		    starting_i = -x1;
//		}
//		for(i = starting_i; i < x_diff; i++)
//		{
//		    if(line_style != null)
//		    {
//			if(line_style.charAt(i%line_style.length()) == '0')
//			{
//			    continue;
//			}
//		    }
//		    m = ((double)(y2 -y1))/ ((double)(x2 - x1));
//		    x_pos = x1 + i;
//		    if(x_pos > cur_screen_map.get_screen_width())
//		    {
//			break;
//		    }
//		    y_pos = (int) (y1 + m*i);
//		    if(y_pos > cur_screen_map.get_screen_height()+line_width/2 && m > 0)
//		    {
//			break;
//		    }
//		    if(x_pos < -line_width/2 && m < 0)
//		    {
//			break;
//		    }
//		    g.fillRect(x_pos,y_pos-line_width/2,1,line_width);
//		}
//	    }
//	}
//	catch(Exception e)
//	{
//	    e.printStackTrace();
//	}
//    }
    Font font = null;
    FontMetrics fm = null;
    String label_string = null;
    double theta_min_last = 0.0;
    double theta_max_last = 0.0;
    double r_min_last = 0.0;
    double r_max_last = 0.0;
    double r_spacing = 1.0;
    int scale_change = 2;
    private boolean first_paint_cart_grid = true;
    private boolean first_bad_dims = true;
    private boolean first_bad_num_y_lines = true;
    private boolean first_bad_num_x_lines = true;
    protected double X_Grid;
    public static final String PROP_X_GRID = "X_Grid";

    /**
     * Get the value of X_Grid
     *
     * @return the value of X_Grid
     */
    public double getX_Grid() {
        return X_Grid;
    }

    /**
     * Set the value of X_Grid
     *
     * @param X_Grid new value of X_Grid
     */
    public void setX_Grid(double X_Grid) {
        double oldX_Grid = this.X_Grid;
        this.X_Grid = X_Grid;
        this.firePropertyChange(PROP_X_GRID, oldX_Grid, X_Grid);
    }
    protected double Y_Grid;
    public static final String PROP_Y_GRID = "Y_Grid";

    /**
     * Get the value of Y_Grid
     *
     * @return the value of Y_Grid
     */
    public double getY_Grid() {
        return Y_Grid;
    }

    public void setForced_Y_Grid(double _y_grid) {
        this.setY_Grid(_y_grid);
        if(!array_mode && this.screen_map != null) {
            this.screen_map.set_y_grid_forced_value(_y_grid);
        } else if(this.array_mode_screen_map != null) {
            this.array_mode_screen_map.set_y_grid_forced_value(_y_grid);
        }
    }
    
    public void setForced_X_Grid(double _x_grid) {
        this.setX_Grid(_x_grid);
        if(!array_mode && this.screen_map != null) {
            this.screen_map.set_x_grid_forced_value(_x_grid);
        } else if(this.array_mode_screen_map != null) {
            this.array_mode_screen_map.set_x_grid_forced_value(_x_grid);
        }
    }
    
    /**
     * Set the value of Y_Grid
     *
     * @param Y_Grid new value of Y_Grid
     */
    public void setY_Grid(double Y_Grid) {
        double oldY_Grid = this.Y_Grid;
        this.Y_Grid = Y_Grid;
        this.firePropertyChange(PROP_Y_GRID, oldY_Grid, Y_Grid);
    }

    public void paintCartesianGrid(Graphics g) {
        try {
            if (m_color && grid_color != null) {
                g.setColor(grid_color);
            }
            PlotGraphScreenMap cur_screen_map = screen_map;
            if (array_mode) {
                cur_screen_map = array_mode_screen_map;
            }
            final int x_grid_spacing = cur_screen_map.get_x_grid_spacing();
            double x_value = cur_screen_map.get_starting_x_grid_value();
            final double x_grid = cur_screen_map.get_x_grid();
            if (x_grid != this.X_Grid) {
                this.setX_Grid(x_grid);
            }
            final int width = cur_screen_map.get_screen_width();
            final int height = cur_screen_map.get_screen_height();

            final int y_grid_spacing = cur_screen_map.get_y_grid_spacing();
            double y_value = cur_screen_map.get_starting_y_grid_value();
            final double y_grid = cur_screen_map.get_y_grid();
            if (y_grid != this.Y_Grid && !s_mode) {
                this.setY_Grid(y_grid);
            }
            boolean reset_grid_color = false;
            if (width < 10 || width > 200000) {
                if (first_bad_dims) {
                    Thread.dumpStack();
                    System.err.println("Can't paint cartesian grid with width=" + width + ", and x_grid_spacing=" + x_grid_spacing);
                    first_bad_dims = false;
                }
                return;
            }
            if (height < 10 || height > 200000) {
                if (first_bad_dims) {
                    System.err.println("Can't paint cartesian grid with height=" + height + ", and y_grid_spacing=" + y_grid_spacing);
                    first_bad_dims = false;
                }
                return;
            }
            final int x_grid_start = cur_screen_map.get_starting_x_grid_screen_pos();
            int x_lines_drawn = 0;
            for (int x_pos = x_grid_start; x_pos < 2 * width && x_pos > -2 * width; x_pos += x_grid_spacing, x_value += x_grid) {
                x_lines_drawn++;
                if (x_lines_drawn > 200) {
                    if (first_bad_num_x_lines) {
                        System.err.println("Bad num x lines : Can't paint cartesian grid with width=" + width + ", and x_grid_spacing=" + x_grid_spacing);
                        first_bad_num_x_lines = false;
                    }
                    return;
                }
                if (m_color && axis_color != null && show_axis && !s_mode && Math.abs(x_value / x_grid) < 1e-6) {
                    g.setColor(axis_color);
                    reset_grid_color = true;
                }
                g.drawLine(x_pos, 0, x_pos, height);
                if (reset_grid_color && m_color && grid_color != null) {
                    g.setColor(grid_color);
                }
                reset_grid_color = false;
                if (label_grid && Math.abs(x_grid_spacing) > 20) {
                    g.drawString(format_double(x_value), x_pos + 5, height - 5);
                }
            }
            if (!s_mode) {
                final int y_start_grid = cur_screen_map.get_starting_y_grid_screen_pos();
                int y_lines_drawn = 0;
                for (int y_pos = y_start_grid;
                        y_pos >= -2 * height && y_pos < height * 2; y_pos -= y_grid_spacing, y_value += y_grid) {
                    y_lines_drawn++;
                    if (y_lines_drawn > 200) {
                        if (first_bad_num_y_lines) {
                            System.err.println("Too many y_lines -- Can't paint cartesian grid with height=" + height + ", y_grid_spacing=" + y_grid_spacing + ", and y_start_grid=" + y_start_grid);
                            first_bad_num_y_lines = false;
                        }
                        return;
                    }
                    if (m_color && axis_color != null && show_axis && !s_mode && Math.abs(y_value / y_grid) < 1e-6) {
                        g.setColor(axis_color);
                        reset_grid_color = true;
                    }
                    g.drawLine(0, y_pos, width, y_pos);
                    if (reset_grid_color && m_color && grid_color != null) {
                        g.setColor(grid_color);
                    }
                    reset_grid_color = false;
                    if (label_grid && y_grid_spacing > 20) {
                        g.drawString(format_double(y_value), 5, y_pos - 5);
                    }
                }
                if (y_lines_drawn < 2) {
                    if (first_bad_num_y_lines) {
                        System.err.println("Too few y_lines -- Can't paint cartesian grid with height=" + height + ", y_grid_spacing=" + y_grid_spacing + " and y_start_grid=" + y_start_grid);
                        first_bad_num_y_lines = false;
                    }
                    return;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        first_paint_cart_grid = false;
    }
    Image bufferImage = null;
    boolean image_needs_to_be_updated = true;

    protected void paintComponent(Graphics g) {
        if (!this.isVisible()) {
            return;
        }
        if (null != back_color) {
            setBackground(back_color);
        }
        super.paintComponent(g);
        this.paintAll(g);
    }

    public void paintAll(Graphics g) {

        if (PlotterCommon.debug_on) {
            DebugPrint("paint() called. paint_count = " + paint_count);
        }
        PlotGraphScreenMap cur_screen_map = screen_map;
        if (array_mode) {
            cur_screen_map = array_mode_screen_map;
        }

        if (clearing_plots) {
            return;
        }


        if (use_buffer) {
            if (null == bufferImage) {
                image_needs_to_be_updated = true;
            }
            if (image_needs_to_be_updated) {
                if (null == bufferImage) {
                    // System.out.println("creating new buffer Image");
                    bufferImage = createImage(cur_screen_map.get_screen_width(), cur_screen_map.get_screen_height());
                }
                Graphics image_g = bufferImage.getGraphics();
                if (null == image_g) {
                    // System.out.println("bufferImage.getGraphics = null");
                    paintGraph(g);
                    paintDraggingRect(g);
                    return;
                }
                // System.out.println("Painting image.");
                image_g.clearRect(0, 0, cur_screen_map.get_screen_width(), cur_screen_map.get_screen_height());
                paintGraph(image_g);
                image_needs_to_be_updated = false;
            }
            if (null != bufferImage) {
                g.drawImage(bufferImage, 0, 0, cur_screen_map.get_screen_width(), cur_screen_map.get_screen_height(), null);
            }
        } else {
            paintGraph(g);
        }
        paintDraggingRect(g);
    }
    private int point_size_limit = 20000;

    public void set_point_size_limit(int _new_point_size_limit) {
        point_size_limit = _new_point_size_limit;
    }

    public int get_point_size_limit() {
        return point_size_limit;
    }

    public void paintDraggingRect(Graphics g) {
        try {
            // System.out.println("paintDraggingRect() called. show_rect="+show_rect);
            if (show_rect) {
                if (m_color && axis_color != null) {
                    g.setColor(axis_color);
                }
                g.drawRect(selected_rectangle.x, selected_rectangle.y, selected_rectangle.width, selected_rectangle.height);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }    //long last_pg_time=0;
    private int last_height = -1;

    public void paintGraph(Graphics g) {

        try {
            if (clearing_plots) {
                return;
            }

            if (null == keyVector) {
                return;
            }
            PlotGraphScreenMap cur_screen_map = screen_map;
            if (array_mode) {
                cur_screen_map = array_mode_screen_map;
            }
            repaint_just_issued = false;
            repaint_needed = false;
            paint_count++;
            if (PlotterCommon.debug_on) {
                DebugPrint("Repainting PlotGraph plotter_num   = " + plotter_num);
            }
            synchronized (SyncObject) {
                if (m_color) {
                    g.setColor(Color.white);
                } else {
                    g.setColor(Color.black);
                }
                if (show_grid) {
                    paintCartesianGrid(g);
                }
                if (show_key && !s_mode) {
                    paintKey(g);
                }

                if (plots != null) {
                    if (PlotterCommon.debug_on) {
                        DebugPrint("Repainting PlotGraph plots = " + plots);
                    }
                    int plots_to_show = 0;
                    int width = cur_screen_map.get_screen_width();
                    int height = cur_screen_map.get_screen_height();
                    if (s_mode) {
                        for (int ki = 0; ki < keyVector.size(); ki++) {
                            PlotData plot_data = keyVector.get(ki);
                            if (!plot_data.getShow(plotter_num) || plot_data.array_type != array_mode) {
                                continue;
                            }
                            plots_to_show++;
                        }
                        if (plots_to_show < max_plots_to_show || max_plots_to_show < 1) {
                            cur_screen_map.set_num_y_sections(plots_to_show);
                        } else {
                            cur_screen_map.set_num_y_sections(max_plots_to_show);
                        }
                    } else {
                        cur_screen_map.set_num_y_sections(0);
                    }
                    int plot_number = 0;
                    for (int ki = 0; ki < keyVector.size() && !clearing_plots; ki++) {
                        if (plots_to_show > max_plots_to_show && max_plots_to_show > 0 && (ki / max_plots_to_show) != plot_group_number) {
                            continue;
                        }
                        PlotData plot_data = keyVector.get(ki);
                        try {
                            synchronized (plot_data) {
                                plot_data.painting = true;
                                if (plotter_num > 0 && PlotterCommon.debug_on) {
                                    DebugPrint("Repainting PlotGraph plot_data = " + plot_data);
                                }
                                if (!plot_data.getShow(plotter_num) || plot_data.array_type != array_mode) {
                                    continue;
                                }
                                if (plot_data.line_width > 0) {
                                    line_width = plot_data.line_width;
                                }
                                line_style = plot_data.line_style;
                                PlotPoint last_point = null;
                                int j = 0;
                                if (plotter_num > 0 && PlotterCommon.debug_on) {
                                    //System.out.println("Repainting PlotGraph orig_scroll_x = "+orig_scroll_x);
                                    //System.out.println("Repainting PlotGraph orig_scroll_y = "+orig_scroll_y);
                                    DebugPrint("Repainting PlotGraph cur_screen_map.get_screen_width() = " + cur_screen_map.get_screen_width());
                                    DebugPrint("Repainting PlotGraph cur_screen_map.get_screen_height() = " + cur_screen_map.get_screen_height());
                                }
                                int v_size = plot_data.v_size();
                                double this_plot_section_y_min = Double.MAX_VALUE;
                                double this_plot_section_y_max = -Double.MAX_VALUE;
                                last_plots_to_show = plots_to_show;
                                if (s_mode) {
                                    for (j = 0; j < v_size && j < plot_data.current_size && !clearing_plots; j++) {
                                        //int v_index = (j + plot_data.v_offset)%v_size;
                                        PlotPoint current_point = plot_data.getPlotPointAt(j);
                                        if(Double.isInfinite(current_point.orig_x)
                                                || Double.isInfinite(-current_point.orig_x)
                                                || Double.isNaN(current_point.orig_x)
                                                || Double.isInfinite(current_point.orig_y)
                                                || Double.isInfinite(-current_point.orig_y)
                                                || Double.isNaN(current_point.orig_y)){
                                            continue;
                                        }
                                        current_point.x = cur_screen_map.get_screen_x(current_point.orig_x);
                                        if (current_point.x >= 0
                                                && current_point.x <= width) {
                                            if (current_point.orig_y > this_plot_section_y_max) {
                                                this_plot_section_y_max = current_point.orig_y;
                                            }
                                            if (current_point.orig_y < this_plot_section_y_min) {
                                                this_plot_section_y_min = current_point.orig_y;
                                            }
                                        }
                                    }
                                    cur_screen_map.set_y_section_show_area(plot_number,
                                            this_plot_section_y_min, this_plot_section_y_max);
                                }

                                if (plot_data.mark_points && plot_data.current_size > point_size_limit && m_color) {
                                    g.setColor(plot_data.getPoint_color());
                                }
                                for (j = 0; j < v_size && j < plot_data.current_size && !clearing_plots; j++) {
                                    int v_index = (j + plot_data.v_offset) % v_size;
                                    PlotPoint current_point = plot_data.getPlotPointAt(v_index);
                                    if (null == current_point) {
                                        continue;
                                    }
                                    current_point.x = cur_screen_map.get_screen_x(current_point.orig_x);
                                    current_point.y = cur_screen_map.get_section_screen_y(plot_number, current_point.orig_y);
                                    //							System.out.println("p="+current_point);
                                    if (plotter_num > 0 && PlotterCommon.debug_on) {
                                        DebugPrint("Repainting PlotGraph current_point.x = " + current_point.x);
                                        DebugPrint("Repainting PlotGraph current_point.y = " + current_point.y);
                                    }

                                    if ((current_point.x < 0 || current_point.x > width) && !xy_mode) {
                                        last_point = current_point;
                                        continue;
                                    }
                                    if (plot_data.current_size > point_size_limit || !plot_data.mark_points) {
                                        g.drawRect(current_point.x,
                                                current_point.y,
                                                0, 0);
                                        if (null != last_point && show_lines && c_mode) {
                                            if (last_point.x < 0 || last_point.x > width) {
                                                last_point = current_point;
                                                continue;
                                            }
                                            if(Math.abs(last_point.x -current_point.x) > 2
                                                    || Math.abs(last_point.y - current_point.y) > 2)
                                            {
                                                g.drawLine(last_point.x, last_point.y,
                                                    current_point.x, current_point.y);
                                            }
                                        }
                                        last_point = current_point;
                                    } else {
                                        if (m_color) {
                                            g.setColor(plot_data.getLine_color());
                                        }
                                        if (null != last_point && show_lines && c_mode) {
                                            if (last_point.x < 0 || last_point.x > width) {
                                                last_point = current_point;
                                                continue;
                                            }
                                            if(Math.abs(last_point.x -current_point.x) > 2
                                                    || Math.abs(last_point.y - current_point.y) > 2)
                                            {
                                                g.drawLine(last_point.x, last_point.y,
                                                    current_point.x, current_point.y);
                                            }
                                        }
                                        if (plot_data.mark_points) {
                                            if (m_color) {
                                                g.setColor(plot_data.getPoint_color());
                                            }
                                            int ptSz = plot_data.getPointSize();
                                            int fr_x = current_point.x - ptSz/2;
                                            int fr_y = current_point.y - ptSz/2;
                                            g.fillRect(fr_x,
                                                    fr_y,
                                                    ptSz, ptSz);
                                        }
                                        if (label_points) {

                                            if (null != last_point) {
                                                if (Math.abs(current_point.x - last_point.x) < 30 && Math.abs(current_point.y - last_point.y) < 10) {
                                                    last_point = current_point;
                                                    continue;
                                                }
                                                if (label_string != null) {
                                                    if (null == font) {
                                                        font = g.getFont();
                                                        fm = g.getFontMetrics(font);
                                                    }
                                                    if (null != fm) {
                                                        if (Math.abs(current_point.x - last_point.x) < fm.stringWidth(label_string) && Math.abs(current_point.y - last_point.y) < fm.getHeight()) {
                                                            last_point = current_point;
                                                            continue;
                                                        }
                                                    }
                                                }
                                            }
                                          label_string = "(" + current_point.orig_x + "," + current_point.orig_y + ")";
                                          g.drawString(label_string, current_point.x + 5, current_point.y - 5);
                                        }
                                        last_point = current_point;
                                    }
                                }
                                if (s_mode) {
                                    int this_plot_section_y_offset = cur_screen_map.get_section_screen_y_line_pose(plot_number);
                                    int top_label_y_pos = this_plot_section_y_offset - (height / plots_to_show) + 20;
                                    int bottom_label_y_pos = this_plot_section_y_offset - 15;

//DebugPrint2("plot_number="+plot_number+", this_plot_section_y_offset="+this_plot_section_y_offset+", top_label_y_pos="+top_label_y_pos+", height="+height+", plots_to_show="+plots_to_show+", bottom_label_y_pos="+bottom_label_y_pos+", plot_data.short_name="+plot_data.short_name);
                                    String slabel = "";
                                    if (m_color && axis_color != null) {
                                        g.setColor(axis_color);
                                    }
                                    if (show_key && !s_mode) {
                                        slabel = "> " + this_plot_section_y_min;
                                    } else {
                                        String tail = "";
                                        if (null != short_extra_sh_str) {
                                            tail += short_extra_sh_str;
                                        }
                                        if (null != plot_data.add_to_short_key) {
                                            tail += plot_data.add_to_short_key;
                                        }
                                        slabel = "> " + this_plot_section_y_min + " : " + plot_data.short_name + tail;
                                    }
                                    if (height / plots_to_show > 50 || (max_plots_to_show > 1 && height / max_plots_to_show > 50)) {
                                        g.drawString("< " + this_plot_section_y_max, 5, top_label_y_pos);
                                    } else {
                                        slabel += " < " + this_plot_section_y_max;
                                    }
                                    g.drawString(slabel, 5, bottom_label_y_pos);
                                    g.drawLine(0,
                                            this_plot_section_y_offset,
                                            width,
                                            this_plot_section_y_offset);
                                }
                            }
                            plot_number++;
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                        plot_data.painting = false;
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    public void SetOuterArea(double min_x, double max_x, double min_y, double max_y) {
        if (array_mode) {
            array_mode_screen_map.set_abs_x_min(min_x);
            array_mode_screen_map.set_abs_x_max(max_x);
            array_mode_screen_map.set_abs_y_max(max_y);
            array_mode_screen_map.set_abs_y_min(min_y);
            if (e_mode) {
                array_mode_screen_map.equalizeAxis();
            }
        } else {
            screen_map.set_abs_x_min(min_x);
            screen_map.set_abs_x_max(max_x);
            screen_map.set_abs_y_max(max_y);
            screen_map.set_abs_y_min(min_y);
            if (e_mode) {
                screen_map.equalizeAxis();
            }
        }
    }

    public void SetInnerArea(double min_x, double max_x, double min_y, double max_y) {
        if (array_mode) {
            array_mode_screen_map.set_x_show_area(min_x, max_x);
            array_mode_screen_map.set_y_show_area(min_y, max_y);
            if (e_mode) {
                array_mode_screen_map.equalizeAxis();
            }
        } else {
            screen_map.set_x_show_area(min_x, max_x);
            screen_map.set_y_show_area(min_y, max_y);
            if (e_mode) {
                screen_map.equalizeAxis();
            }
        }
    }
    private int plot_group_number = 0;

    public void set_plot_group_number(int _plot_group_number) {
        this.plot_group_number = _plot_group_number;
    }
    /**
     * Holds value of property max_plots_to_show.
     */
    private int max_plots_to_show;

    /**
     * Getter for property max_plots_to_show.
     * @return Value of property max_plots_to_show.
     */
    public int getMax_plots_to_show() {
        return this.max_plots_to_show;
    }

    /**
     * Setter for property max_plots_to_show.
     * @param max_plots_to_show New value of property max_plots_to_show.
     */
    public void setMax_plots_to_show(int max_plots_to_show) {
        this.max_plots_to_show = max_plots_to_show;
    }
}
