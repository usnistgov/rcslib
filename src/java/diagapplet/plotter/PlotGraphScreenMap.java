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

class PlotGraphScreenMap {

    private int screen_height;
    private int screen_width;
    private double show_area_x_mid;
    private double show_area_y_mid;
    private double show_area_x_min;
    private double show_area_y_min;
    private double show_area_x_max;
    private double show_area_y_max;
    private double show_area_x_range;
    private double show_area_y_range;
    private double abs_x_min;
    private double abs_x_max;
    private double abs_y_min;
    private double abs_y_max;
    private double abs_x_range;
    private double abs_y_range;
    private double x_scale;
    private double y_scale;
    private double x_over_y_scale_ratio;
    private double x_grid;
    private double y_grid;
    private int x_grid_spacing;
    private int y_grid_spacing;
    private java.awt.Dimension m_prefDim;
    private boolean changed;
    private int changed_count;
    private int starting_x_grid_screen_pos;
    private int starting_y_grid_screen_pos;
    private double starting_x_grid_value;
    private double starting_y_grid_value;
    private int ending_x_grid_screen_pos;
    private int ending_y_grid_screen_pos;
    private double ending_x_grid_value;
    private double ending_y_grid_value;
    private int x_grid_diff;
    private int y_grid_diff;
    private int x_grid_count;
    private int y_grid_count;
    private int num_y_sections;
    private int y_section_inner_height;
    private int y_section_outer_height;
    private double section_y_min[];
    private double section_y_max[];
    private double section_y_range[];
    private double section_y_scale[];
    private int scroll_x;
    private int scroll_y;
    private int scroll_width;
    private int scroll_height;
    private double abs_to_show_x_min_diff;
    private double abs_to_show_y_min_diff;
    private boolean reverse_x = false;

    public void SetReverseX(boolean _new_reverse_x) {
        this.reverse_x = _new_reverse_x;
        this.changed_count++;
        this.changed = true;
    }

    public int get_changed_count() {
        return changed_count;
    }

    public String toString() {
        String s = super.toString() + " PlotGraphScreenMap{\n";
        s += "\tscreen_height=" + screen_height + ",\n";
        s += "\tscreen_width=" + screen_width + ",\n";
        s += "\tshow_area_x_mid=" + show_area_x_mid + ",\n";
        s += "\tshow_area_x_min=" + show_area_x_min + ",\n";
        s += "\tshow_area_x_max=" + show_area_x_max + ",\n";
        s += "\tshow_area_x_range=" + show_area_x_range + ",\n";
        s += "\tshow_area_y_mid=" + show_area_y_mid + ",\n";
        s += "\tshow_area_y_min=" + show_area_y_min + ",\n";
        s += "\tshow_area_y_max=" + show_area_y_max + ",\n";
        s += "\tshow_area_y_range=" + show_area_y_range + ",\n";
        s += "\tabs_x_min=" + abs_x_min + ",\n";
        s += "\tabs_x_max=" + abs_x_max + ",\n";
        s += "\tabs_x_range=" + abs_x_range + ",\n";
        s += "\tabs_y_min=" + abs_y_min + ",\n";
        s += "\tabs_y_max=" + abs_y_max + ",\n";
        s += "\tabs_y_range=" + abs_y_range + ",\n";
        s += "\tx_scale=" + x_scale + ",\n";
        s += "\ty_scale=" + y_scale + ",\n";
        s += "\tx_over_y_scale_ratio=" + x_over_y_scale_ratio + ",\n";
        s += "\tx_grid=" + x_grid + ",\n";
        s += "\ty_grid=" + y_grid + ",\n";
        s += "\tx_grid_spacing=" + x_grid_spacing + ",\n";
        s += "\ty_grid_spacing=" + y_grid_spacing + ",\n";
        s += "\tm_prefDim=" + m_prefDim + ",\n";
        s += "\tchanged=" + changed + ",\n";
        s += "\tchanged_count=" + changed_count + ",\n";
        s += "\tstarting_x_grid_screen_pos=" + starting_x_grid_screen_pos + ",\n";
        s += "\tstarting_y_grid_screen_pos=" + starting_y_grid_screen_pos + ",\n";
        s += "\tstarting_x_grid_value=" + starting_x_grid_value + ",\n";
        s += "\tstarting_y_grid_value=" + starting_y_grid_value + ",\n";
        s += "\tending_x_grid_screen_pos=" + ending_x_grid_screen_pos + ",\n";
        s += "\tending_y_grid_screen_pos=" + ending_y_grid_screen_pos + ",\n";
        s += "\tending_x_grid_value=" + ending_x_grid_value + ",\n";
        s += "\tending_y_grid_value=" + ending_y_grid_value + ",\n";
        s += "\tx_grid_diff=" + x_grid_diff + ",\n";
        s += "\tx_grid_count=" + x_grid_count + ",\n";
        s += "\ty_grid_diff=" + y_grid_diff + ",\n";
        s += "\ty_grid_count=" + y_grid_count + ",\n";
        s += "}  \n";
        return s;
    }

    public void clear_changed() {
        changed = false;
    }

    public boolean get_changed() {
        return changed;
    }

    public PlotGraphScreenMap(int _screen_width, int _screen_height) {
        screen_height = _screen_height;
        screen_width = _screen_width;
        show_area_x_min = 0;
        show_area_y_min = 0;
        show_area_x_max = 1.0;
        show_area_y_max = 1.0;
        show_area_x_range = 1.0;
        show_area_y_range = 1.0;
        num_y_sections = 0;
        recalculate();
    }

    double log10(double x) {
        return Math.log(x) / Math.log(10);
    }
    
    public boolean use_x_grid_forced_value = false;
    public boolean use_y_grid_forced_value = false;
    public double x_grid_forced_value = 1.0;
    public double y_grid_forced_value = 1.0;

    public void set_x_grid_forced_value(double  _x_grid_forced_value) {
        if(_x_grid_forced_value > 0) {
            use_x_grid_forced_value = true;
            this.x_grid_forced_value = _x_grid_forced_value;
        } else {
            use_x_grid_forced_value = false;
        }
        recalculate();
    }
    
    public void set_y_grid_forced_value(double  _y_grid_forced_value) {
        if(_y_grid_forced_value > 0) {
            use_y_grid_forced_value = true;
            this.y_grid_forced_value = _y_grid_forced_value;
        } else {
            use_y_grid_forced_value = false;
        }
        recalculate();
    }
    
    private void recalculate() {
        if (null == m_prefDim) {
            m_prefDim = new java.awt.Dimension(screen_width, screen_height);
        }
        if (screen_height < 1 || screen_width < 1) {
            changed = true;
            return;
        }
        x_scale = screen_width / show_area_x_range;
        y_scale = screen_height / show_area_y_range;
        x_over_y_scale_ratio = (x_scale / y_scale);
        int xli = (int) log10(show_area_x_range / 5.0);
        if (xli != 0) {
            x_grid = Math.pow(10, xli);
        } else {
            x_grid = 1.0;
        }
        int yli = (int) log10(show_area_y_range / 5.0);
        if (yli != 0) {
            y_grid = Math.pow(10, yli);
        } else {
            y_grid = 1.0;
        }
        x_grid_spacing = (int) (x_grid * x_scale);
        while (x_grid_spacing < 50) {
            x_grid *= 2.0;
            x_grid_spacing = (int) (x_grid * x_scale);
        }
        while (x_grid_spacing > (screen_width / 4)) {
            x_grid *= 0.5;
            x_grid_spacing = (int) (x_grid * x_scale);
        }
        if(use_x_grid_forced_value) {
            if(x_grid >= x_grid_forced_value/20.0 && x_grid <= x_grid_forced_value*20) {
                x_grid = x_grid_forced_value;
                x_grid_spacing = (int) (x_grid * x_scale);
            } else {
                use_x_grid_forced_value = false;
            }
        }

        y_grid_spacing = (int) (y_grid * y_scale);
        while (y_grid_spacing < 25) {
            y_grid *= 2.0;
            y_grid_spacing = (int) (y_grid * y_scale);
        }
        while (y_grid_spacing > (screen_height / 4)) {
            y_grid *= 0.5;
            y_grid_spacing = (int) (y_grid * y_scale);
        }
        if(use_y_grid_forced_value) {
            if(y_grid >= y_grid_forced_value/20.0 && y_grid <= y_grid_forced_value*20) {
                y_grid = y_grid_forced_value;
                y_grid_spacing = (int) (y_grid * y_scale);
            } else {
                use_y_grid_forced_value = false;
            }
        }
        
        show_area_x_mid = (show_area_x_min + show_area_x_max) / 2.0;
        show_area_y_mid = (show_area_y_min + show_area_y_max) / 2.0;
//		if(!this.reverse_x) {
        starting_x_grid_value = (Math.floor(show_area_x_min / x_grid)) * x_grid;
//		}
//		else {
//			starting_x_grid_value = (Math.floor(show_area_x_max / x_grid)) * x_grid;
//		}
        starting_x_grid_screen_pos = get_screen_x(starting_x_grid_value);
        starting_y_grid_value = (Math.floor(show_area_y_min / y_grid)) * y_grid;
        starting_y_grid_screen_pos = get_screen_y(starting_y_grid_value);
        ending_x_grid_value = ((int) (show_area_x_max / x_grid)) * x_grid;
        ending_x_grid_screen_pos = get_screen_x(ending_x_grid_value);
        ending_y_grid_value = ((int) (show_area_y_max / y_grid)) * y_grid;
        ending_y_grid_screen_pos = get_screen_y(ending_y_grid_value);
        x_grid_diff = ending_x_grid_screen_pos - starting_x_grid_screen_pos;
        y_grid_diff = starting_y_grid_screen_pos - ending_y_grid_screen_pos;
        x_grid_count = x_grid_diff / x_grid_spacing;
        y_grid_count = y_grid_diff / y_grid_spacing;
        abs_x_range = abs_x_max - abs_x_min;
        abs_y_range = abs_y_max - abs_y_min;
        scroll_width = (int) ((abs_x_range - show_area_x_range) * x_scale);
        scroll_height = (int) ((abs_y_range - show_area_y_range) * y_scale);
        abs_to_show_x_min_diff = show_area_x_min - abs_x_min;
        abs_to_show_y_min_diff = show_area_y_min - abs_y_min;
        scroll_x = (int) (abs_to_show_x_min_diff * x_scale);
        scroll_y = (int) (scroll_height - (abs_to_show_y_min_diff * y_scale));
        if (scroll_height <= screen_height + 1) {
            scroll_y = 0;
            scroll_height = screen_height;
        }
        if (scroll_width <= screen_width + 1) {
            scroll_y = 0;
            scroll_width = screen_width;
        }
        changed = true;
        changed_count++;
    }

    public double get_abs_x_min() {
        return abs_x_min;
    }

    public double get_abs_x_max() {
        return abs_x_max;
    }

    public double get_abs_y_min() {
        return abs_y_min;
    }

    public double get_abs_y_max() {
        return abs_y_max;
    }

    public int get_scroll_width() {
        return scroll_width;
    }

    public int get_scroll_height() {
        return scroll_height;
    }

    public void set_scroll_x(int new_scroll_x) {
        if (scroll_x != new_scroll_x) {
            double new_show_area_x_min = new_scroll_x / x_scale + abs_x_min;
            double new_show_area_x_max = new_show_area_x_min + show_area_x_range;
            set_x_show_area(new_show_area_x_min, new_show_area_x_max);
        }
    }

    public void set_scroll_y(int new_scroll_y) {
        if (scroll_y != new_scroll_y) {
            double new_show_area_y_min = (scroll_height - new_scroll_y) / y_scale + abs_y_min;
            double new_show_area_y_max = new_show_area_y_min + show_area_y_range;
            set_y_show_area(new_show_area_y_min, new_show_area_y_max);
            // if(scroll_y != new_scroll_y)
//                 {
//                         System.err.println("scroll_y not set properly. "+scroll_y+" != " +new_scroll_y);
//                 }
        }
    }

    public int get_scroll_x() {
        return scroll_x;
    }

    public int get_scroll_y() {
        return scroll_y;
    }

    public void equalizeAxis() {
        if (Math.abs((x_scale - y_scale) / (x_scale + y_scale)) < 1e-12) {
            return;
        }
        recalculate();
        double x_over_y_scale_ratio = (x_scale / y_scale);
        if (x_over_y_scale_ratio > 1.0) {
            show_area_x_mid = (show_area_x_min + show_area_x_max) / 2.0;
            double new_show_area_x_range = show_area_x_range * x_over_y_scale_ratio;
            double new_show_area_x_min = show_area_x_mid - new_show_area_x_range / 2.0;
            double new_show_area_x_max = show_area_x_mid + new_show_area_x_range / 2.0;
            set_x_show_area(new_show_area_x_min, new_show_area_x_max);
        } else {
            show_area_y_mid = (show_area_y_min + show_area_y_max) / 2.0;
            double new_show_area_y_range = show_area_y_range / x_over_y_scale_ratio;
            double new_show_area_y_min = show_area_y_mid - new_show_area_y_range / 2.0;
            double new_show_area_y_max = show_area_y_mid + new_show_area_y_range / 2.0;
            set_y_show_area(new_show_area_y_min, new_show_area_y_max);
        }
        recalculate();
    }

    public void set_screen_w_x_h(int _screen_width, int _screen_height) {
        if (_screen_width != screen_width || _screen_height != screen_height) {
            try {
                screen_height = _screen_height;
                screen_width = _screen_width;
                m_prefDim = new java.awt.Dimension(screen_width, screen_height);
                if (num_y_sections < 2) {
                    y_section_inner_height = screen_height - 8;
                    y_section_outer_height = screen_height;
                } else {
                    y_section_inner_height = screen_height / num_y_sections - 8;
                    y_section_outer_height = screen_height / num_y_sections;
                }
                for (int i = 0; i < num_y_sections; i++) {
                    if(Math.abs(section_y_range[i]) < Double.MIN_NORMAL ) {
                        section_y_scale[i] = Double.MAX_VALUE/(2*y_section_inner_height);
                    } else {
                        section_y_scale[i] = y_section_inner_height / section_y_range[i];
                    }
                }
                recalculate();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public void set_abs_x_min(double _x_min) {
        if (abs_x_min != _x_min) {
            abs_x_min = _x_min;
            recalculate();
        }
    }

    public void set_abs_x_max(double _x_max) {
        if (abs_x_max != _x_max) {
            abs_x_max = _x_max;
            recalculate();
        }
    }

    public void set_abs_y_min(double _y_min) {
        if (abs_y_min != _y_min) {
            abs_y_min = _y_min;
            recalculate();
        }
    }

    public void set_abs_y_max(double _y_max) {
        if (abs_y_max != _y_max) {
            abs_y_max = _y_max;
            recalculate();
        }
    }

    public void zoom_x(double _x_zoom) {
        double new_show_area_x_mid = (show_area_x_min + show_area_x_max) / 2.0;
        double new_show_are_x_range = show_area_x_range * _x_zoom;
        double new_show_area_x_min = new_show_area_x_mid - new_show_are_x_range / 2.0;
        double new_show_area_x_max = new_show_area_x_mid + new_show_are_x_range / 2.0;
        set_x_show_area(new_show_area_x_min, new_show_area_x_max);
    }

    public void zoom_y(double _y_zoom) {
        double new_show_area_y_mid = (show_area_y_min + show_area_y_max) / 2.0;
        double new_show_area_y_range = show_area_y_range * _y_zoom;
        double new_show_area_y_min = new_show_area_y_mid - new_show_area_y_range / 2.0;
        double new_show_area_y_max = new_show_area_y_mid + new_show_area_y_range / 2.0;
        set_y_show_area(new_show_area_y_min, new_show_area_y_max);
    }

    public void move_to_new_x_show_area_max(double _x_max) {
        if (show_area_x_max < _x_max) {
            double new_show_area_x_min = _x_max - show_area_x_range;
            double new_show_area_x_max = _x_max;
            set_x_show_area(new_show_area_x_min, new_show_area_x_max);
        }
    }

    public void set_x_show_area(double _x_min, double _x_max) {
        if (this.reverse_x && _x_min > _x_max) {
            final double temp = _x_min;
            _x_min = _x_max;
            _x_max = temp;
        }
        if (_x_min >= _x_max
                || Double.isInfinite(_x_min)
                || Double.isNaN(_x_min)
                || Double.isInfinite(_x_max)
                || Double.isNaN(_x_max)) {
            return;
        }
        if (show_area_x_min != _x_min
                || show_area_x_max != _x_max) {
            show_area_x_min = _x_min;
            if (show_area_x_min < abs_x_min) {
                abs_x_min = show_area_x_min;
            }
            show_area_x_max = _x_max;
            if (show_area_x_max > abs_x_max) {
                abs_x_max = show_area_x_max;
            }
            show_area_x_range = show_area_x_max - show_area_x_min;
            if (show_area_x_range < 1e-12) {
                show_area_x_range = 1e-12;
            }
            recalculate();
        }
    }

    public void set_y_show_area(double _y_min, double _y_max) {
        if (_y_min >= _y_max
                || Double.isInfinite(_y_min)
                || Double.isNaN(_y_min)
                || Double.isInfinite(_y_max)
                || Double.isNaN(_y_max)) {
            return;
        }
        if (show_area_y_min != _y_min
                || show_area_y_max != _y_max) {
            show_area_y_min = _y_min;
            if (show_area_y_min < abs_y_min) {
                abs_y_min = show_area_y_min;
            }
            show_area_y_max = _y_max;
            if (show_area_y_max > abs_y_max) {
                abs_y_max = show_area_y_max;
            }
            show_area_y_range = show_area_y_max - show_area_y_min;
            if (show_area_y_range < 1e-12) {
                show_area_y_range = 1e-12;
            }
            recalculate();
        }
    }

    public void update_horz_scrollbar(java.awt.Scrollbar horz_scroll_bar) {
        horz_scroll_bar.setValues(scroll_x, 1, 0, scroll_width);
        horz_scroll_bar.setUnitIncrement(screen_width / 4);
        horz_scroll_bar.setBlockIncrement(screen_width / 2);
        if (scroll_width <= screen_width + 1) {
            horz_scroll_bar.setValues(0, screen_width, 0, screen_width);
            horz_scroll_bar.setEnabled(false);
        } else {
            horz_scroll_bar.setEnabled(true);
        }
    }

    public void update_vert_scrollbar(java.awt.Scrollbar vert_scroll_bar) {
        vert_scroll_bar.setValues(scroll_y, 1, 0, scroll_height);
        vert_scroll_bar.setUnitIncrement(screen_height / 4);
        vert_scroll_bar.setBlockIncrement(screen_height / 2);
        if (scroll_height <= screen_height + 1) {
            vert_scroll_bar.setValues(0, 1, 0, screen_height);
            vert_scroll_bar.setEnabled(false);
        } else {
            vert_scroll_bar.setEnabled(true);
        }
    }

    public void update_horz_scrollbar(javax.swing.JScrollBar horz_scroll_bar) {
        horz_scroll_bar.setValues(scroll_x, 1, 0, scroll_width);
        horz_scroll_bar.setUnitIncrement(screen_width / 4);
        horz_scroll_bar.setBlockIncrement(screen_width / 2);
        if (scroll_width <= screen_width + 1) {
            horz_scroll_bar.setValues(0, screen_width, 0, screen_width);
            horz_scroll_bar.setEnabled(false);
        } else {
            horz_scroll_bar.setEnabled(true);
        }
    }

    public void update_vert_scrollbar(javax.swing.JScrollBar vert_scroll_bar) {
        vert_scroll_bar.setValues(scroll_y, 1, 0, scroll_height);
        vert_scroll_bar.setUnitIncrement(screen_height / 4);
        vert_scroll_bar.setBlockIncrement(screen_height / 2);
        if (scroll_height <= screen_height + 1) {
            vert_scroll_bar.setValues(0, 1, 0, screen_height);
            vert_scroll_bar.setEnabled(false);
        } else {
            vert_scroll_bar.setEnabled(true);
        }
    }

    public double get_x_scale() {
        return x_scale;
    }

    public double get_y_scale() {
        return y_scale;
    }

    public void set_y_section_show_area(int section_number, double _y_min, double _y_max) {
        if (section_number < 0
                || section_number >= num_y_sections) {
            return;
        }
        if (section_y_min[section_number] != _y_min || section_y_max[section_number] != _y_max) {
            section_y_min[section_number] = _y_min;
            section_y_max[section_number] = _y_max;
            section_y_range[section_number] = _y_max - _y_min;
            if (section_y_range[section_number] < 1e-12) {
                section_y_range[section_number] = 1e-12;
            }
            section_y_scale[section_number] = y_section_inner_height / section_y_range[section_number];
            changed = true;
            changed_count++;
        }
    }

    public void set_num_y_sections(int _num_y_sections) {
        if (num_y_sections != _num_y_sections) {
            if (null == section_y_min || section_y_min.length < _num_y_sections) {
                section_y_min = new double[_num_y_sections];
            }
            if (null == section_y_max || section_y_max.length < _num_y_sections) {
                section_y_max = new double[_num_y_sections];
            }
            if (null == section_y_range || section_y_range.length < _num_y_sections) {
                section_y_range = new double[_num_y_sections];
            }
            if (null == section_y_scale || section_y_scale.length < _num_y_sections) {
                section_y_scale = new double[_num_y_sections];
            }
            num_y_sections = _num_y_sections;
            if (screen_height > 4 * num_y_sections) {
                if (num_y_sections < 2) {
                    y_section_inner_height = screen_height - 4;
                    y_section_outer_height = screen_height;
                } else {
                    y_section_inner_height = (screen_height - 4 * num_y_sections) / num_y_sections;
                    y_section_outer_height = screen_height / num_y_sections;
                }
            } else if (screen_height > 2 * num_y_sections) {
                if (num_y_sections < 2) {
                    y_section_inner_height = screen_height - 2;
                    y_section_outer_height = screen_height;
                } else {
                    y_section_inner_height = (screen_height - 2 * num_y_sections) / num_y_sections;
                    y_section_outer_height = screen_height / num_y_sections;
                }
            } else {
                if (num_y_sections < 2) {
                    y_section_inner_height = screen_height;
                    y_section_outer_height = screen_height;
                } else {
                    y_section_inner_height = screen_height / num_y_sections;
                    y_section_outer_height = screen_height / num_y_sections;
                }
            }
            changed = true;
            changed_count++;
        }
    }

    public int get_screen_x(double x) {
        if (!this.reverse_x) {
            return (int) ((x - show_area_x_min) * x_scale);
        } else {
            return (int) ((show_area_x_max - x) * x_scale);
        }
    }

    public int get_screen_y(double y) {
        double ys = (y - show_area_y_min) * y_scale;
        return (int) (screen_height - ys);
    }

    public int get_section_screen_y_line_pose(int section_number) {
        int y_line_pose = screen_height - section_number * y_section_outer_height;
        // System.out.println("screen_height="+screen_height+",section_number="+section_number+", y_line_pose="+y_line_pose);
// 	System.out.println("section_y_max["+section_number+"]="+section_y_max[section_number]+", get_section_screen_y(section_y_max[section_number])="+
// 			   get_section_screen_y(section_number,section_y_max[section_number]));
// 	System.out.println("section_y_min["+section_number+"]="+section_y_min[section_number]+", get_section_screen_y(section_y_min[section_number])="+
//			   get_section_screen_y(section_number,section_y_min[section_number]));
        return (y_line_pose);
    }

    public int get_section_screen_y(int section_number, double y) {
        if (num_y_sections < 1) {
            return get_screen_y(y);
        }
        if (PlotterCommon.debug_on) {
            if (section_number >= num_y_sections || section_number < 0) {
                PlotterCommon.DebugPrint("section_number=" + section_number + ",num_y_sections=" + num_y_sections);
            }
        }
        return (int) (screen_height - section_number * y_section_outer_height - 4 - ((y - section_y_min[section_number]) * section_y_scale[section_number]));
    }

    public double get_x_value(int screen_x) {
        if (!this.reverse_x) {
            return (screen_x / x_scale + show_area_x_min);
        } else {
            return (show_area_x_max - screen_x / x_scale);
        }
    }

    public double get_y_value(int screen_y) {
        return (screen_height - screen_y) / y_scale + show_area_y_min;
    }

    public double get_x_grid() {
        return x_grid;
    }

    public double get_y_grid() {
        return y_grid;
    }

    public int get_x_grid_spacing() {
        if (!this.reverse_x) {
            return x_grid_spacing;
        } else {
            return -x_grid_spacing;
        }
    }

    public int get_y_grid_spacing() {
        return y_grid_spacing;
    }

    public java.awt.Dimension get_dimension() {
        return m_prefDim;
    }

    public int get_screen_height() {
        return screen_height;
    }

    public int get_screen_width() {
        return screen_width;
    }

    public double get_x_min() {
        return show_area_x_min;
    }

    public double get_x_max() {
        return show_area_x_max;
    }

    public double get_y_min() {
        return show_area_y_min;
    }

    public double get_y_max() {
        return show_area_y_max;
    }

    public int get_starting_x_grid_screen_pos() {
        return starting_x_grid_screen_pos;
    }

    public int get_starting_y_grid_screen_pos() {
        return starting_y_grid_screen_pos;
    }

    public double get_starting_x_grid_value() {
        return starting_x_grid_value;
    }

    public double get_starting_y_grid_value() {
        return starting_y_grid_value;
    }
}
