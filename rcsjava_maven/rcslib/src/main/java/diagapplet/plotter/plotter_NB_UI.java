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
 * plotter_NB_UI.java
 *
 * Created on December 31, 2006, 6:53 AM
 */
package diagapplet.plotter;

import diagapplet.utils.SaveImage;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GraphicsDevice;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.lang.reflect.Field;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JColorChooser;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.ProgressMonitor;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;

/**
 * Contain JPanel with PlotGraphJPanel in the center surrounded by several
 * controls.
 *
 * @author shackle
 */
public class plotter_NB_UI extends javax.swing.JPanel {

    private static final long serialVersionUID = 2613932L;
    private static int total_plotters = 0;
    private boolean paused = false;
    private boolean clearing_plots = false;
    private boolean mouse_down = false;
    private static final int FUNC_CHOICE_NORMAL = 0;
    private static final int FUNC_CHOICE_VS = 1;
    private static final int FUNC_CHOICE_XY = 2;
    private static final int FUNC_CHOICE_SMOOTH = 3;
    private static final int FUNC_CHOICE_DEVIATION = 4;
    private static final int FUNC_CHOICE_DERIVATIVE = 5;
    private static final int FUNC_CHOICE_INTEGRAL = 6;
    private static final int FUNC_CHOICE_DIFF = 7;
    private static final int FUNC_CHOICE_PPDIFF = 8;
    private static final int FUNC_CHOICE_PPDIFFMODPI = 9;
    private static final int FUNC_CHOICE_NEGATIVEX = 10;
    private static final int FUNC_CHOICE_XZ = 11;
    private static final int FUNC_CHOICE_YZ = 12;
    private static final int FUNC_CHOICE_SINGLE = 13;
    private static final DecimalFormat dcFormat = new DecimalFormat("####.###");
    private File last_dir = null;
    private boolean point_added_since_check_recalc_plots = false;

    public static int getTotalPlotters() {
        return total_plotters;
    }

    /**
     * Creates new form plotter_NB_UI
     */
    public plotter_NB_UI() {
        initComponents();
        this.cur_pgjp = this.plotGraphJPanel1;
        plotGraphJPanel1.plotter_num = total_plotters;
        total_plotters++;
        jButtonBackground.setBackground(this.plotGraphJPanel1.back_color);
        jButtonBackground.setForeground(this.plotGraphJPanel1.back_color);
        jButtonGrid.setBackground(this.plotGraphJPanel1.grid_color);
        jButtonGrid.setForeground(this.plotGraphJPanel1.grid_color);
        jButtonAxis.setBackground(this.plotGraphJPanel1.axis_color);
        jButtonAxis.setForeground(this.plotGraphJPanel1.axis_color);

        this.plotGraphJPanel1.addPropertyChangeListener(this.plotGraphJPanel1.PROP_X_GRID, new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent evt) {
//		System.out.println("evt = " + evt);
//		String name = evt.getPropertyName();
//		System.out.println("name = " + name);
                final double x_grid = (Double) (evt.getNewValue());
                //jLabelXScale.setText(String.format("X Scale:%9.3g/div", x_grid));
                jTextFieldXScale.setText(String.format("%9.3g", x_grid));
            }
        });

        this.plotGraphJPanel1.addPropertyChangeListener(this.plotGraphJPanel1.PROP_Y_GRID, new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent evt) {
//		System.out.println("evt = " + evt);
//		String name = evt.getPropertyName();
//		System.out.println("name = " + name);
                final double y_grid = (Double) (evt.getNewValue());
                //jLabelYScale.setText(String.format("Y Scale:%9.3g/div", y_grid));
                jTextFieldYScale.setText(String.format("%9.3g", y_grid));
            }
        });
        //SetupOptionsTable();
        this.plotGraphJPanel1.label_points = this.jCheckBoxLabelPoints.isSelected();
    }
    private boolean setup_options_table_done = false;

    private void SetupOptionsTable() {
        try {
            setup_options_table_done = true;
            jTableOptions.getColumnModel().getColumn(1).setCellEditor(new ColorEditor());
            jTableOptions.getColumnModel().getColumn(2).setCellEditor(new ColorEditor());
            jTableOptions.getColumnModel().getColumn(1).setCellRenderer(new ColorRenderer(false));
            jTableOptions.getColumnModel().getColumn(2).setCellRenderer(new ColorRenderer(false));
            AddOptionsTableListener();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public boolean get_array_mode() {
        return plotGraphJPanel1.get_array_mode();
    }

    public void FitY() {
        plotGraphJPanel1.FitY();
        UpdateScrollBarsTextFields();
    }

    public void SetEqualizeAxis(boolean _e_mode) {
        this.jToggleButtonEqualizeAxis.setSelected(_e_mode);
        this.set_e_mode(_e_mode);
    }

    @SuppressWarnings("unchecked")
    private void AddOptionsTableListener() {
        jTableOptions.getModel().addTableModelListener(new TableModelListener() {
            @Override
            public void tableChanged(TableModelEvent e) {
                try {
                    int row = e.getFirstRow();
                    int column = e.getColumn();
                    if (row < 0 || row >= jTableOptions.getRowCount()) {
                        return;
                    }
                    PlotData pd = plotGraphJPanel1.plots.get((String) jTableOptions.getValueAt(row, 5));
                    if (null == pd) {
                        return;
                    }
                    switch (column) {
                        case 1:
                            pd.setLine_color((Color) jTableOptions.getValueAt(row, column));
                            break;

                        case 2:
                            pd.setPoint_color((Color) jTableOptions.getValueAt(row, column));
                            break;

                        case 3:
                            pd.setShow(plotGraphJPanel1.plotter_num, ((Boolean) jTableOptions.getValueAt(row, column)).booleanValue());
                            break;

                        case 4:
                            pd.delete_me = ((Boolean) jTableOptions.getValueAt(row, column)).booleanValue();
                            break;

                        case 6:
                            pd.setPointSize((Integer) jTableOptions.getValueAt(row, column));
                            break;
                    }
                    refresh();
                } catch (Exception except) {
                    except.printStackTrace();
                }
            }
        });
    }

    public BufferedImage plotToImage() {
        return this.plotGraphJPanel1.getImage();
    }

    public BufferedImage plotToImage(int width, int height) {
        return this.plotGraphJPanel1.getImage(width, height);
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jFrameOptions = new javax.swing.JFrame();
        jPanel1 = new javax.swing.JPanel();
        jLabel5 = new javax.swing.JLabel();
        jButtonBackground = new javax.swing.JButton();
        jLabel6 = new javax.swing.JLabel();
        jButtonGrid = new javax.swing.JButton();
        jCheckBoxK2 = new javax.swing.JCheckBox();
        jScrollPanelOptonsTable = new javax.swing.JScrollPane();
        jTableOptions = new javax.swing.JTable();
        jCheckBoxShowGrid = new javax.swing.JCheckBox();
        jButtonCloseOptions = new javax.swing.JButton();
        jButtonDeleteMarked = new javax.swing.JButton();
        jCheckBoxReverseX = new javax.swing.JCheckBox();
        jLabel7 = new javax.swing.JLabel();
        jButtonAxis = new javax.swing.JButton();
        jButtonShowAll = new javax.swing.JButton();
        jButtonHideAll = new javax.swing.JButton();
        jCheckBoxApplyAbsY = new javax.swing.JCheckBox();
        jCheckBoxUseShortNames = new javax.swing.JCheckBox();
        jFrameData = new javax.swing.JFrame();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTableData = new javax.swing.JTable();
        jButtonDataClose = new javax.swing.JButton();
        jButtonDataSave = new javax.swing.JButton();
        jToolBar1 = new javax.swing.JToolBar();
        jButtonZoomIn = new javax.swing.JButton();
        jButtonZoomOut = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();
        jToggleButtonPause = new javax.swing.JToggleButton();
        jToggleButtonKey = new javax.swing.JToggleButton();
        jButtonClear = new javax.swing.JButton();
        jToggleButtonSplit = new javax.swing.JToggleButton();
        jToggleButtonCMode = new javax.swing.JToggleButton();
        jToggleButtonArrayMode = new javax.swing.JToggleButton();
        jToggleButtonLockDisplay = new javax.swing.JToggleButton();
        jToggleButtonEqualizeAxis = new javax.swing.JToggleButton();
        jComboBoxFunc = new javax.swing.JComboBox();
        jSpinnerFuncArg = new javax.swing.JSpinner();
        jButtonShowData = new javax.swing.JButton();
        jButtonOptions = new javax.swing.JButton();
        jButtonStats = new javax.swing.JButton();
        jScrollBarVert = new javax.swing.JScrollBar();
        plotGraphJPanel1 = new diagapplet.plotter.PlotGraphJPanel();
        jLabel1 = new javax.swing.JLabel();
        jScrollBarHorz = new javax.swing.JScrollBar();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jTextFieldYMax = new javax.swing.JTextField();
        jTextFieldYMin = new javax.swing.JTextField();
        jTextFieldXMin = new javax.swing.JTextField();
        jTextFieldXMax = new javax.swing.JTextField();
        jSpinnerPlotGroup = new javax.swing.JSpinner();
        jLabelPlotGroup = new javax.swing.JLabel();
        jSpinnerPlotsPerGroup = new javax.swing.JSpinner();
        jLabelPlotsPerGroup = new javax.swing.JLabel();
        jButtonClearDataOnly = new javax.swing.JButton();
        jLabelYScale = new javax.swing.JLabel();
        jLabelXScale = new javax.swing.JLabel();
        jLabelXMaxMin = new javax.swing.JLabel();
        jCheckBoxLabelPoints = new javax.swing.JCheckBox();
        jTextFieldYScale = new javax.swing.JTextField();
        jLabel8 = new javax.swing.JLabel();
        jTextFieldXScale = new javax.swing.JTextField();
        jLabel9 = new javax.swing.JLabel();

        jFrameOptions.setTitle("Plotter Options");
        jFrameOptions.setMinimumSize(new java.awt.Dimension(600, 400));

        jPanel1.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        jPanel1.setMinimumSize(new java.awt.Dimension(200, 200));

        jLabel5.setText("Background:");

        jButtonBackground.setText("BACKGROUND");
        jButtonBackground.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonBackgroundActionPerformed(evt);
            }
        });

        jLabel6.setText("Grid:");

        jButtonGrid.setText("GRID");
        jButtonGrid.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonGridActionPerformed(evt);
            }
        });

        jCheckBoxK2.setText("Extended Key");
        jCheckBoxK2.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        jCheckBoxK2.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                jCheckBoxK2ItemStateChanged(evt);
            }
        });

        jTableOptions.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {

            },
            new String [] {
                "Plot Name (short)", "Line Color", "Point Color", "Show", "Delete", "Full Name", "Point Size"
            }
        ) {
            Class[] types = new Class [] {
                java.lang.String.class, java.lang.Object.class, java.lang.Object.class, java.lang.Boolean.class, java.lang.Boolean.class, java.lang.String.class, java.lang.Integer.class
            };
            boolean[] canEdit = new boolean [] {
                false, true, true, true, true, false, true
            };

            public Class getColumnClass(int columnIndex) {
                return types [columnIndex];
            }

            public boolean isCellEditable(int rowIndex, int columnIndex) {
                return canEdit [columnIndex];
            }
        });
        jScrollPanelOptonsTable.setViewportView(jTableOptions);

        jCheckBoxShowGrid.setSelected(true);
        jCheckBoxShowGrid.setText("Show Grid");
        jCheckBoxShowGrid.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        jCheckBoxShowGrid.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                jCheckBoxShowGridItemStateChanged(evt);
            }
        });

        jButtonCloseOptions.setText("Close");
        jButtonCloseOptions.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonCloseOptionsActionPerformed(evt);
            }
        });

        jButtonDeleteMarked.setText("Delete Marked Plots");
        jButtonDeleteMarked.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonDeleteMarkedActionPerformed(evt);
            }
        });

        jCheckBoxReverseX.setText("Reverse X");
        jCheckBoxReverseX.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                jCheckBoxReverseXItemStateChanged(evt);
            }
        });

        jLabel7.setText("Axis:");

        jButtonAxis.setText("AXIS");
        jButtonAxis.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonAxisActionPerformed(evt);
            }
        });

        jButtonShowAll.setText("Show All");
        jButtonShowAll.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonShowAllActionPerformed(evt);
            }
        });

        jButtonHideAll.setText("Hide All");
        jButtonHideAll.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonHideAllActionPerformed(evt);
            }
        });

        jCheckBoxApplyAbsY.setText("Apply Abs(y)");
        jCheckBoxApplyAbsY.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxApplyAbsYActionPerformed(evt);
            }
        });

        jCheckBoxUseShortNames.setText("Use Short Names");
        jCheckBoxUseShortNames.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxUseShortNamesActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jScrollPanelOptonsTable, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 757, Short.MAX_VALUE)
                    .addComponent(jButtonCloseOptions)
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jLabel5)
                                    .addComponent(jLabel6)
                                    .addComponent(jLabel7))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                    .addComponent(jButtonAxis, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jButtonGrid, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jButtonBackground, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jCheckBoxK2)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jCheckBoxShowGrid)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jCheckBoxUseShortNames, javax.swing.GroupLayout.PREFERRED_SIZE, 107, javax.swing.GroupLayout.PREFERRED_SIZE)))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(jCheckBoxReverseX)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                    .addComponent(jButtonShowAll)
                                    .addComponent(jButtonHideAll))
                                .addGap(9, 9, 9)
                                .addComponent(jButtonDeleteMarked))
                            .addComponent(jCheckBoxApplyAbsY))))
                .addContainerGap())
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonBackground, jButtonGrid});

        jPanel1Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonCloseOptions, jButtonDeleteMarked});

        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel5)
                    .addComponent(jButtonBackground)
                    .addComponent(jCheckBoxReverseX))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel6)
                            .addComponent(jButtonGrid))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel7)
                            .addComponent(jButtonAxis))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jCheckBoxK2)
                            .addComponent(jCheckBoxShowGrid)
                            .addComponent(jCheckBoxUseShortNames)))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jCheckBoxApplyAbsY)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonShowAll)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jButtonDeleteMarked)
                            .addComponent(jButtonHideAll))))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPanelOptonsTable, javax.swing.GroupLayout.DEFAULT_SIZE, 173, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonCloseOptions)
                .addGap(8, 8, 8))
        );

        javax.swing.GroupLayout jFrameOptionsLayout = new javax.swing.GroupLayout(jFrameOptions.getContentPane());
        jFrameOptions.getContentPane().setLayout(jFrameOptionsLayout);
        jFrameOptionsLayout.setHorizontalGroup(
            jFrameOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jFrameOptionsLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jFrameOptionsLayout.setVerticalGroup(
            jFrameOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jFrameOptionsLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jFrameData.setMinimumSize(new java.awt.Dimension(300, 300));

        jTableData.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jScrollPane1.setViewportView(jTableData);

        jButtonDataClose.setText("Close");
        jButtonDataClose.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonDataCloseActionPerformed(evt);
            }
        });

        jButtonDataSave.setText("Save");
        jButtonDataSave.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonDataSaveActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jFrameDataLayout = new javax.swing.GroupLayout(jFrameData.getContentPane());
        jFrameData.getContentPane().setLayout(jFrameDataLayout);
        jFrameDataLayout.setHorizontalGroup(
            jFrameDataLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jFrameDataLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jFrameDataLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 463, Short.MAX_VALUE)
                    .addGroup(jFrameDataLayout.createSequentialGroup()
                        .addComponent(jButtonDataSave)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonDataClose)))
                .addContainerGap())
        );
        jFrameDataLayout.setVerticalGroup(
            jFrameDataLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jFrameDataLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 270, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jFrameDataLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonDataClose)
                    .addComponent(jButtonDataSave))
                .addContainerGap())
        );

        jButtonZoomIn.setText("ZoomIn");
        jButtonZoomIn.setToolTipText("Zoom In");
        jButtonZoomIn.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonZoomInActionPerformed(evt);
            }
        });
        jToolBar1.add(jButtonZoomIn);

        jButtonZoomOut.setText("ZoomOut");
        jButtonZoomOut.setToolTipText("Zoom Out");
        jButtonZoomOut.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonZoomOutActionPerformed(evt);
            }
        });
        jToolBar1.add(jButtonZoomOut);

        jButton1.setText("Fit ");
        jButton1.setToolTipText("Fit Plot To Window");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });
        jToolBar1.add(jButton1);

        jToggleButtonPause.setText("Pause");
        jToggleButtonPause.setToolTipText("Pause Plotting");
        jToggleButtonPause.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                jToggleButtonPauseItemStateChanged(evt);
            }
        });
        jToolBar1.add(jToggleButtonPause);

        jToggleButtonKey.setSelected(true);
        jToggleButtonKey.setText("Key");
        jToggleButtonKey.setToolTipText("Show/Hide Key");
        jToggleButtonKey.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                jToggleButtonKeyItemStateChanged(evt);
            }
        });
        jToolBar1.add(jToggleButtonKey);

        jButtonClear.setText("Clear");
        jButtonClear.setToolTipText("Clear Data and Stop Plotting");
        jButtonClear.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonClearActionPerformed(evt);
            }
        });
        jToolBar1.add(jButtonClear);

        jToggleButtonSplit.setText("Split");
        jToggleButtonSplit.setToolTipText("Split/Combine Plots ");
        jToggleButtonSplit.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                jToggleButtonSplitItemStateChanged(evt);
            }
        });
        jToolBar1.add(jToggleButtonSplit);

        jToggleButtonCMode.setSelected(true);
        jToggleButtonCMode.setText("C");
        jToggleButtonCMode.setToolTipText("Enable/Disable Connecting Points with Lines");
        jToggleButtonCMode.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                jToggleButtonCModeItemStateChanged(evt);
            }
        });
        jToolBar1.add(jToggleButtonCMode);

        jToggleButtonArrayMode.setText("A");
        jToggleButtonArrayMode.setToolTipText("Show Array/Single Variables");
        jToggleButtonArrayMode.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jToggleButtonArrayModeActionPerformed(evt);
            }
        });
        jToolBar1.add(jToggleButtonArrayMode);

        jToggleButtonLockDisplay.setText("L");
        jToggleButtonLockDisplay.setToolTipText("Lock XY Ranges -- Stop Auto Fitting");
        jToolBar1.add(jToggleButtonLockDisplay);

        jToggleButtonEqualizeAxis.setText("E");
        jToggleButtonEqualizeAxis.setToolTipText("Equalize X and Y scales");
        jToggleButtonEqualizeAxis.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jToggleButtonEqualizeAxisActionPerformed(evt);
            }
        });
        jToolBar1.add(jToggleButtonEqualizeAxis);

        jComboBoxFunc.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Normal", "Each vs selected.", "X vs Y", "Smooth", "Deviation", "Derivative", "Integral", "Diff", "Plot to Plot Diff", "Plot to Plot Diff Mod PI", "Negative X", "X vs. Z", "Y vs. Z", "Single" }));
        jComboBoxFunc.setToolTipText("Select Plot Style or Apply function to data");
        jComboBoxFunc.setMinimumSize(new java.awt.Dimension(0, 24));
        jComboBoxFunc.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                jComboBoxFuncItemStateChanged(evt);
            }
        });
        jToolBar1.add(jComboBoxFunc);

        jSpinnerFuncArg.setToolTipText("Applied Function Parameter");
        jSpinnerFuncArg.setMinimumSize(new java.awt.Dimension(0, 20));
        jSpinnerFuncArg.setPreferredSize(new java.awt.Dimension(60, 20));
        jSpinnerFuncArg.addChangeListener(new javax.swing.event.ChangeListener() {
            public void stateChanged(javax.swing.event.ChangeEvent evt) {
                jSpinnerFuncArgStateChanged(evt);
            }
        });
        jToolBar1.add(jSpinnerFuncArg);

        jButtonShowData.setText("Data");
        jButtonShowData.setToolTipText("Show Data in Table or save data to text file.");
        jButtonShowData.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonShowDataActionPerformed(evt);
            }
        });
        jToolBar1.add(jButtonShowData);

        jButtonOptions.setText("Options");
        jButtonOptions.setToolTipText("Change colors and other options");
        jButtonOptions.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonOptionsActionPerformed(evt);
            }
        });
        jToolBar1.add(jButtonOptions);

        jButtonStats.setText("Stats");
        jButtonStats.setFocusable(false);
        jButtonStats.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonStats.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButtonStats.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonStatsActionPerformed(evt);
            }
        });
        jToolBar1.add(jButtonStats);

        jScrollBarVert.addAdjustmentListener(new java.awt.event.AdjustmentListener() {
            public void adjustmentValueChanged(java.awt.event.AdjustmentEvent evt) {
                jScrollBarVertAdjustmentValueChanged(evt);
            }
        });

        plotGraphJPanel1.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                plotGraphJPanel1MouseClicked(evt);
            }
            public void mousePressed(java.awt.event.MouseEvent evt) {
                plotGraphJPanel1MousePressed(evt);
            }
            public void mouseReleased(java.awt.event.MouseEvent evt) {
                plotGraphJPanel1MouseReleased(evt);
            }
        });
        plotGraphJPanel1.addComponentListener(new java.awt.event.ComponentAdapter() {
            public void componentResized(java.awt.event.ComponentEvent evt) {
                plotGraphJPanel1ComponentResized(evt);
            }
        });
        plotGraphJPanel1.addMouseMotionListener(new java.awt.event.MouseMotionAdapter() {
            public void mouseDragged(java.awt.event.MouseEvent evt) {
                plotGraphJPanel1MouseDragged(evt);
            }
        });

        javax.swing.GroupLayout plotGraphJPanel1Layout = new javax.swing.GroupLayout(plotGraphJPanel1);
        plotGraphJPanel1.setLayout(plotGraphJPanel1Layout);
        plotGraphJPanel1Layout.setHorizontalGroup(
            plotGraphJPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        plotGraphJPanel1Layout.setVerticalGroup(
            plotGraphJPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 226, Short.MAX_VALUE)
        );

        jLabel1.setText("Y max:");

        jScrollBarHorz.setOrientation(javax.swing.JScrollBar.HORIZONTAL);
        jScrollBarHorz.addAdjustmentListener(new java.awt.event.AdjustmentListener() {
            public void adjustmentValueChanged(java.awt.event.AdjustmentEvent evt) {
                jScrollBarHorzAdjustmentValueChanged(evt);
            }
        });

        jLabel2.setText("X min:");

        jLabel3.setText("Y min:");

        jLabel4.setText("X Max:");

        jTextFieldYMax.setText("1.0");
        jTextFieldYMax.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jTextFieldYMaxActionPerformed(evt);
            }
        });

        jTextFieldYMin.setText("-1.0");
        jTextFieldYMin.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jTextFieldYMinActionPerformed(evt);
            }
        });

        jTextFieldXMin.setText("-1.0");
        jTextFieldXMin.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jTextFieldXMinActionPerformed(evt);
            }
        });

        jTextFieldXMax.setText("1.0");
        jTextFieldXMax.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jTextFieldXMaxActionPerformed(evt);
            }
        });

        jSpinnerPlotGroup.addChangeListener(new javax.swing.event.ChangeListener() {
            public void stateChanged(javax.swing.event.ChangeEvent evt) {
                jSpinnerPlotGroupStateChanged(evt);
            }
        });

        jLabelPlotGroup.setText("Plot Group:");

        jSpinnerPlotsPerGroup.addChangeListener(new javax.swing.event.ChangeListener() {
            public void stateChanged(javax.swing.event.ChangeEvent evt) {
                jSpinnerPlotsPerGroupStateChanged(evt);
            }
        });

        jLabelPlotsPerGroup.setText("Plots per Group:");

        jButtonClearDataOnly.setText("Clear Data Only");
        jButtonClearDataOnly.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonClearDataOnlyActionPerformed(evt);
            }
        });

        jLabelYScale.setText("Y Grid :  ");

        jLabelXScale.setText("X Grid:  ");

        jLabelXMaxMin.setText(" X Max-Min:  -----.--- ");

        jCheckBoxLabelPoints.setText("Label Points");
        jCheckBoxLabelPoints.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxLabelPointsActionPerformed(evt);
            }
        });

        jTextFieldYScale.setText("1.000 ");
        jTextFieldYScale.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jTextFieldYScaleActionPerformed(evt);
            }
        });

        jLabel8.setText("/div");

        jTextFieldXScale.setText("1.000 ");
        jTextFieldXScale.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jTextFieldXScaleActionPerformed(evt);
            }
        });

        jLabel9.setText("/div");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jToolBar1, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                                .addComponent(plotGraphJPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jScrollBarVert, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(jLabel1)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jTextFieldYMax, javax.swing.GroupLayout.PREFERRED_SIZE, 94, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabelYScale)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jTextFieldYScale, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabel8)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(jCheckBoxLabelPoints)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonClearDataOnly)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabelPlotsPerGroup)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jSpinnerPlotsPerGroup, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabelPlotGroup)
                                .addGap(4, 4, 4)
                                .addComponent(jSpinnerPlotGroup, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addGap(32, 32, 32)))
                        .addContainerGap())
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(jLabel3)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldYMin, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel2)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldXMin, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabelXScale)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldXScale, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(1, 1, 1)
                        .addComponent(jLabel9)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabelXMaxMin)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jLabel4)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldXMax, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addContainerGap())
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jScrollBarHorz, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addGap(35, 35, 35))))
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jTextFieldXMax, jTextFieldXMin, jTextFieldYMax, jTextFieldYMin});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jToolBar1, javax.swing.GroupLayout.PREFERRED_SIZE, 25, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldYMax, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jSpinnerPlotGroup, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabelPlotGroup)
                    .addComponent(jSpinnerPlotsPerGroup, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabelPlotsPerGroup)
                    .addComponent(jButtonClearDataOnly)
                    .addComponent(jLabelYScale)
                    .addComponent(jCheckBoxLabelPoints)
                    .addComponent(jTextFieldYScale, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel8))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollBarVert, javax.swing.GroupLayout.DEFAULT_SIZE, 226, Short.MAX_VALUE)
                    .addComponent(plotGraphJPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollBarHorz, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel3)
                        .addComponent(jTextFieldYMin, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(jLabel2)
                        .addComponent(jTextFieldXMin, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(jLabelXScale)
                        .addComponent(jLabelXMaxMin)
                        .addComponent(jTextFieldXScale, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(jLabel9))
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jTextFieldXMax, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(jLabel4)))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    private void jSpinnerPlotsPerGroupStateChanged(javax.swing.event.ChangeEvent evt)//GEN-FIRST:event_jSpinnerPlotsPerGroupStateChanged
    {//GEN-HEADEREND:event_jSpinnerPlotsPerGroupStateChanged
        plotGraphJPanel1.setMax_plots_to_show(((Integer) this.jSpinnerPlotsPerGroup.getValue()).intValue());
        refresh();
    }//GEN-LAST:event_jSpinnerPlotsPerGroupStateChanged

    private void jSpinnerPlotGroupStateChanged(javax.swing.event.ChangeEvent evt)//GEN-FIRST:event_jSpinnerPlotGroupStateChanged
    {//GEN-HEADEREND:event_jSpinnerPlotGroupStateChanged
        plotGraphJPanel1.set_plot_group_number(((Integer) this.jSpinnerPlotGroup.getValue()).intValue());
        refresh();
    }//GEN-LAST:event_jSpinnerPlotGroupStateChanged
    private boolean auto_fit_to_graph = true;

    public void set_auto_fit_to_graph(final boolean _auto_fit_to_graph) {
        this.auto_fit_to_graph = _auto_fit_to_graph;
    }

    private void set_e_mode(boolean _emode) {
        if (_emode != plotGraphJPanel1.e_mode) {
            plotGraphJPanel1.e_mode = _emode;
            if (plotGraphJPanel1.e_mode) {
                if (plotGraphJPanel1.get_array_mode()) {
                    plotGraphJPanel1.array_mode_screen_map.equalizeAxis();
                } else {
                    plotGraphJPanel1.screen_map.equalizeAxis();
                }
                UpdateScrollBarsTextFields();
                refresh();
            } else {
                plotGraphJPanel1.FitToGraph();
                UpdateScrollBarsTextFields();
                refresh();
            }
        }
    }
    private void jToggleButtonEqualizeAxisActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jToggleButtonEqualizeAxisActionPerformed
    {//GEN-HEADEREND:event_jToggleButtonEqualizeAxisActionPerformed
        this.set_e_mode(jToggleButtonEqualizeAxis.isSelected());
    }//GEN-LAST:event_jToggleButtonEqualizeAxisActionPerformed

    private void jToggleButtonArrayModeActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jToggleButtonArrayModeActionPerformed
    {//GEN-HEADEREND:event_jToggleButtonArrayModeActionPerformed
        plotGraphJPanel1.set_array_mode(jToggleButtonArrayMode.isSelected());
        last_scrollbar_changed_count = -1;
        UpdateScrollBarsTextFields();
        mouse_down = false;
    }//GEN-LAST:event_jToggleButtonArrayModeActionPerformed

    private void jButtonDataSaveActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonDataSaveActionPerformed
    {//GEN-HEADEREND:event_jButtonDataSaveActionPerformed
        javax.swing.filechooser.FileFilter filter = new javax.swing.filechooser.FileFilter() {
            @Override
            public boolean accept(File f) {
                return (!f.isHidden() && !f.getName().startsWith("."));
            }

            @Override
            public String getDescription() {
                return "Plot Data Files";
            }
        };
        JFileChooser chooser = new JFileChooser();
        if (last_dir != null) {
            chooser.setCurrentDirectory(last_dir);
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }
        chooser.setFileFilter(filter);
        int returnVal = chooser.showSaveDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            System.out.println("You chose to open this file: "
                    + chooser.getSelectedFile().getPath());
            last_dir = chooser.getCurrentDirectory();
            SaveFile(chooser.getSelectedFile().getPath());
        }
    }//GEN-LAST:event_jButtonDataSaveActionPerformed

    private void jButtonDataCloseActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonDataCloseActionPerformed
    {//GEN-HEADEREND:event_jButtonDataCloseActionPerformed
        jFrameData.setVisible(false);
    }//GEN-LAST:event_jButtonDataCloseActionPerformed

    /**
     * @return the plot_order
     */
    public String getPlot_order() {
        return plot_order;
    }

    /**
     * @param plot_order the plot_order to set
     */
    public void setPlot_order(String plot_order) {
        this.plot_order = plot_order;
        if (null != this.plotGraphJPanel1) {
            this.plotGraphJPanel1.setPlotOrder(plot_order);
        }
    }

    private class ixy {

        public ixy(int _i, double _x, double _y) {
            i = _i;
            x = _x;
            y = _y;
        }
        public int i;
        public double x;
        public double y;
    }
    private Vector<ixy> ixy_vector = null;
    int ixy_columns = 0;
    private Vector<String> colvec = null;

    private void LoadDataIntoIxyVector() {
        ixy_vector = new Vector<ixy>();
        colvec = new Vector<String>();
        colvec.add("");
        colvec.add("X-Axis");
        ArrayList<PlotData> plots_list = new ArrayList<PlotData>(this.plotGraphJPanel1.plots.values());
        Collections.sort(plots_list, new Comparator<PlotData>() {
            @Override
            public int compare(PlotData o1, PlotData o2) {
                return o1.short_name.compareTo(o2.short_name);
            }
        });
        for (int i = 0; i < plots_list.size(); i++) {
            PlotData pd = plots_list.get(i);
            colvec.add(pd.short_name);
            for (int j = 0; j < pd.get_num_points(); j++) {
                PlotPoint pp = pd.getPlotPointAt(j);
                ixy_vector.add(new ixy(i + 1, pp.orig_x, pp.orig_y));
            }
        }
        ixy_columns = plots_list.size();
    }
    protected boolean Use_Interpolation = false;

    /**
     * Get the value of Use_Interpolation
     *
     * @return the value of Use_Interpolation
     */
    public boolean isUse_Interpolation() {
        return Use_Interpolation;
    }

    /**
     * Set the value of Use_Interpolation
     *
     * @param Use_Interpolation new value of Use_Interpolation
     */
    public void setUse_Interpolation(boolean Use_Interpolation) {
        this.Use_Interpolation = Use_Interpolation;
    }

    private Double[][] LoadDataInto2DArray() {
        Collections.sort(ixy_vector,
                new Comparator<ixy>() {
                    @Override
                    public int compare(ixy ixy1, ixy ixy2) {
                        if (ixy2.x < ixy1.x) {
                            return 1;
                        } else if (ixy2.x > ixy1.x) {
                            return -1;
                        }
                        return 0;
                    }
                });
        int ixy_rows = 0;
        double last_x = Double.NEGATIVE_INFINITY;
        for (ixy d : ixy_vector) {
            if (ixy_rows < 0 || d.x > last_x) {
                ixy_rows++;
            }
            last_x = d.x;
        }
        int dar = -1;
        Double last_value[] = new Double[ixy_columns + 2];
        Double last_time[] = new Double[ixy_columns + 2];
        Double da[][] = new Double[ixy_rows][ixy_columns + 2];
        for (int ixy_vector_i = 0; ixy_vector_i < ixy_vector.size(); ixy_vector_i++) {
            ixy d = ixy_vector.get(ixy_vector_i);
            if (dar < 0 || d.x > da[dar][1].doubleValue()) {
                dar++;
            }
            if (dar >= ixy_rows) {
                System.err.println("dar=" + dar + ", ixy_vector_i=" + ixy_vector_i + ", ixy_rows=" + ixy_rows);
                break;
            }
            da[dar][0] = (double) dar;
            da[dar][1] = Double.valueOf(d.x);
            da[dar][d.i + 1] = Double.valueOf(d.y);
            int last_dar = dar - 1;
            while (last_dar >= 0 && da[last_dar][d.i + 1] == null) {
                Double Dy = Double.valueOf(d.y);
                if (this.Use_Interpolation
                        && null != last_time[d.i + 1]
                        && null != last_value[d.i + 1]) {
                    Dy = last_value[d.i + 1] + (Dy - last_value[d.i + 1]) * (da[last_dar][1] - last_time[d.i + 1]) / (d.x - last_time[d.i + 1]);
                }
                da[last_dar][d.i + 1] = Dy;
                last_dar--;
            }
            last_time[d.i + 1] = da[dar][1];
            last_value[d.i + 1] = da[dar][d.i + 1];
        }
        for (int i = 2; i < ixy_columns + 2; i++) {
            for (int j = ixy_rows - 1; j >= 0 && da[j][i] == null && last_value[i] != null; j--) {
                da[j][i] = last_value[i];
            }
        }
        return da;
    }

    private void LoadDataIntoTable() {
        LoadDataIntoIxyVector();
        Double da[][] = LoadDataInto2DArray();
        ((DefaultTableModel) jTableData.getModel()).setDataVector(da, colvec.toArray());
    }

    public void SaveFile(String fileName) {
        try {
            LoadDataIntoIxyVector();
            Double d2Da[][] = LoadDataInto2DArray();
            FileOutputStream fos = new FileOutputStream(fileName);
            PrintStream ps = new PrintStream(fos);
            for (int i = 1; i < colvec.size(); i++) {
                ps.print(colvec.elementAt(i));
                if (i < colvec.size() - 1) {
                    ps.print(",");
                } else {
                    ps.println();
                }
            }
            for (Double da[] : d2Da) {
                for (int i = 1; i < da.length; i++) {
                    ps.printf("%.13g", da[i]);
                    if (i < da.length - 1) {
                        ps.print(",");
                    }
                }
                ps.println();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void ShowData() {
        this.jFrameData.setVisible(false);
        ((DefaultTableModel) jTableData.getModel()).setDataVector(new Vector(), new Vector());
        switch (JOptionPane.showConfirmDialog(this.getParent(), "Use Interpolation ?")) {
            case JOptionPane.CANCEL_OPTION:
                return;

            case JOptionPane.YES_OPTION:
                this.setUse_Interpolation(true);
                break;

            case JOptionPane.NO_OPTION:
                this.setUse_Interpolation(false);
                break;
        }
        this.jFrameData.setVisible(true);
        LoadDataIntoTable();
    }

    private void jButtonShowDataActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonShowDataActionPerformed
    {//GEN-HEADEREND:event_jButtonShowDataActionPerformed
        this.ShowData();
    }//GEN-LAST:event_jButtonShowDataActionPerformed

    private void jCheckBoxK2ItemStateChanged(java.awt.event.ItemEvent evt)//GEN-FIRST:event_jCheckBoxK2ItemStateChanged
    {//GEN-HEADEREND:event_jCheckBoxK2ItemStateChanged
        plotGraphJPanel1.k2_mode = jCheckBoxK2.isSelected();
        if (plotGraphJPanel1.k2_mode) {
            plotGraphJPanel1.show_key = true;
            jToggleButtonKey.setSelected(true);
        }
        refresh();
        mouse_down = false;
    }//GEN-LAST:event_jCheckBoxK2ItemStateChanged

    private void jButtonCloseOptionsActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonCloseOptionsActionPerformed
    {//GEN-HEADEREND:event_jButtonCloseOptionsActionPerformed
        jFrameOptions.setVisible(false);
        mouse_down = false;
    }//GEN-LAST:event_jButtonCloseOptionsActionPerformed

    private void jCheckBoxShowGridItemStateChanged(java.awt.event.ItemEvent evt)//GEN-FIRST:event_jCheckBoxShowGridItemStateChanged
    {//GEN-HEADEREND:event_jCheckBoxShowGridItemStateChanged
        plotGraphJPanel1.show_grid = jCheckBoxShowGrid.isSelected();
        refresh();
        mouse_down = false;
    }//GEN-LAST:event_jCheckBoxShowGridItemStateChanged

    private void DeleteMarkedPlots() {
        synchronized (plotGraphJPanel1) {
            boolean deleteme_found = true;
            while (deleteme_found) {
                deleteme_found = false;
                for (PlotData pd : this.plotGraphJPanel1.plots.values()) {
                    if (pd.delete_me) {
                        plotGraphJPanel1.RemovePlot(pd.name);
                        deleteme_found = true;
                        break;
                    }
                }
            }
            if (!this.setup_options_table_done) {
                this.SetupOptionsTable();
            }
            for (int i = 0; i < jTableOptions.getRowCount(); i++) {
                if (((Boolean) jTableOptions.getValueAt(i, 4)).booleanValue()) {
                    ((DefaultTableModel) jTableOptions.getModel()).removeRow(i);
                }
            }
            refresh();
        }
    }

    private void jButtonDeleteMarkedActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonDeleteMarkedActionPerformed
    {//GEN-HEADEREND:event_jButtonDeleteMarkedActionPerformed
        DeleteMarkedPlots();
    }//GEN-LAST:event_jButtonDeleteMarkedActionPerformed

    public void AddDeleteActionListener(ActionListener al) {
        jButtonDeleteMarked.addActionListener(al);
    }

    private void jButtonGridActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonGridActionPerformed
    {//GEN-HEADEREND:event_jButtonGridActionPerformed
        this.plotGraphJPanel1.grid_color = JColorChooser.showDialog(
                this,
                "Choose Grid Color",
                this.plotGraphJPanel1.grid_color);
        jButtonGrid.setBackground(this.plotGraphJPanel1.grid_color);
        jButtonGrid.setForeground(this.plotGraphJPanel1.grid_color);
        refresh();
        this.plotGraphJPanel1.SaveOptions();
    }//GEN-LAST:event_jButtonGridActionPerformed

    @SuppressWarnings("static-access")
    private void jButtonBackgroundActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonBackgroundActionPerformed
    {//GEN-HEADEREND:event_jButtonBackgroundActionPerformed
        this.plotGraphJPanel1.back_color = JColorChooser.showDialog(
                this,
                "Choose Background Color",
                this.plotGraphJPanel1.back_color);
        jButtonBackground.setBackground(this.plotGraphJPanel1.back_color);
        jButtonBackground.setForeground(plotGraphJPanel1.back_color);
        refresh();
        this.plotGraphJPanel1.SaveOptions();
    }//GEN-LAST:event_jButtonBackgroundActionPerformed

    public void ShowOptions() {
        this.InitOptionsTable();
        jFrameOptions.setVisible(true);
    }

    private void jButtonOptionsActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonOptionsActionPerformed
    {//GEN-HEADEREND:event_jButtonOptionsActionPerformed
//        if (!this.setup_options_table_done) {
//            this.SetupOptionsTable();
//        }
        this.ShowOptions();
    }//GEN-LAST:event_jButtonOptionsActionPerformed

    private void jTextFieldXMaxActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jTextFieldXMaxActionPerformed
    {//GEN-HEADEREND:event_jTextFieldXMaxActionPerformed
        try {
            PlotGraphScreenMap cur_screen_map = plotGraphJPanel1.screen_map;
            if (plotGraphJPanel1.get_array_mode()) {
                cur_screen_map = plotGraphJPanel1.array_mode_screen_map;
            }
            double new_value = Double.valueOf(jTextFieldXMax.getText()).doubleValue();
            double old_value = cur_screen_map.get_x_max();
            if (new_value != old_value) {
                cur_screen_map.set_x_show_area(cur_screen_map.get_x_min(), new_value);
                if (plotGraphJPanel1.e_mode) {
                    cur_screen_map.equalizeAxis();
                }
                UpdateScrollBars();
                refresh();
            }

            this.jToggleButtonLockDisplay.setSelected(true);
            mouse_down = false;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }//GEN-LAST:event_jTextFieldXMaxActionPerformed

    private void jTextFieldXMinActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jTextFieldXMinActionPerformed
    {//GEN-HEADEREND:event_jTextFieldXMinActionPerformed
        try {
            PlotGraphScreenMap cur_screen_map = plotGraphJPanel1.screen_map;
            if (plotGraphJPanel1.get_array_mode()) {
                cur_screen_map = plotGraphJPanel1.array_mode_screen_map;
            }
            double new_value = Double.valueOf(jTextFieldXMin.getText()).doubleValue();
            double old_value = cur_screen_map.get_x_min();
            if (new_value != old_value) {
                cur_screen_map.set_x_show_area(new_value, cur_screen_map.get_x_max());
                if (plotGraphJPanel1.e_mode) {
                    cur_screen_map.equalizeAxis();
                }
                UpdateScrollBars();
                refresh();
            }
            this.jToggleButtonLockDisplay.setSelected(true);
            mouse_down = false;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }//GEN-LAST:event_jTextFieldXMinActionPerformed

    private void jTextFieldYMinActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jTextFieldYMinActionPerformed
    {//GEN-HEADEREND:event_jTextFieldYMinActionPerformed
        try {
            PlotGraphScreenMap cur_screen_map = plotGraphJPanel1.screen_map;
            if (plotGraphJPanel1.get_array_mode()) {
                cur_screen_map = plotGraphJPanel1.array_mode_screen_map;
            }
            double new_value = Double.valueOf(jTextFieldYMin.getText()).doubleValue();
            double old_value = cur_screen_map.get_y_min();
            if (new_value != old_value) {
                cur_screen_map.set_y_show_area(new_value, cur_screen_map.get_y_max());
                if (plotGraphJPanel1.e_mode) {
                    cur_screen_map.equalizeAxis();
                }
                UpdateScrollBars();
                refresh();
            }
            this.jToggleButtonLockDisplay.setSelected(true);
            mouse_down = false;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }//GEN-LAST:event_jTextFieldYMinActionPerformed

    private void jTextFieldYMaxActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jTextFieldYMaxActionPerformed
    {//GEN-HEADEREND:event_jTextFieldYMaxActionPerformed
        try {
            PlotGraphScreenMap cur_screen_map = plotGraphJPanel1.screen_map;
            if (plotGraphJPanel1.get_array_mode()) {
                cur_screen_map = plotGraphJPanel1.array_mode_screen_map;
            }
            double new_value = Double.valueOf(jTextFieldYMax.getText()).doubleValue();
            double old_value = cur_screen_map.get_y_max();
            if (new_value != old_value) {
                cur_screen_map.set_y_show_area(cur_screen_map.get_y_min(), new_value);
                if (plotGraphJPanel1.e_mode) {
                    cur_screen_map.equalizeAxis();
                }
                UpdateScrollBars();
                refresh();
            }
            this.jToggleButtonLockDisplay.setSelected(true);
            mouse_down = false;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }//GEN-LAST:event_jTextFieldYMaxActionPerformed

    private void jSpinnerFuncArgStateChanged(javax.swing.event.ChangeEvent evt)//GEN-FIRST:event_jSpinnerFuncArgStateChanged
    {//GEN-HEADEREND:event_jSpinnerFuncArgStateChanged
        java.awt.EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                RecalculatePlots();
                FitToGraph();
                refresh();
            }
        });

    }//GEN-LAST:event_jSpinnerFuncArgStateChanged

    public void checkComboBoxFunc(int new_function_selected) {
        plotGraphJPanel1.xy_mode
                = (new_function_selected == FUNC_CHOICE_XY
                || new_function_selected == FUNC_CHOICE_XZ
                || new_function_selected == FUNC_CHOICE_YZ);

        if (new_function_selected < 0 || new_function_selected > jComboBoxFunc.getMaximumRowCount()) {
            new_function_selected = jComboBoxFunc.getSelectedIndex();
        } else if (new_function_selected != jComboBoxFunc.getSelectedIndex()) {
            jComboBoxFunc.setSelectedIndex(new_function_selected);
        }
        if (function_selected != new_function_selected) {
            if ((function_selected == FUNC_CHOICE_XY
                    || function_selected == FUNC_CHOICE_XZ
                    || function_selected == FUNC_CHOICE_YZ)
                    && !this.jToggleButtonSplit.isEnabled()) {
                plotGraphJPanel1.e_mode = true;
                plotGraphJPanel1.s_mode = this.jToggleButtonSplit.isSelected();
                if (!plotGraphJPanel1.s_mode) {
                    plotGraphJPanel1.xy_mode = true;
                }
                this.jToggleButtonSplit.setEnabled(true);
            }
            function_selected = new_function_selected;
            if (function_selected == FUNC_CHOICE_XY
                    || function_selected == FUNC_CHOICE_XZ
                    || function_selected == FUNC_CHOICE_YZ) {
                plotGraphJPanel1.e_mode = true;
//                plotGraphJPanel1.s_mode=false;
//                this.jToggleButtonSplit.setEnabled(false);
            } else {
                plotGraphJPanel1.e_mode = false;
            }
            jToggleButtonEqualizeAxis.setSelected(plotGraphJPanel1.e_mode);
            java.awt.EventQueue.invokeLater(new Runnable() {
                @Override
                public void run() {
                    RecalculatePlots();
                    plotGraphJPanel1.ResetColors();
                    FitToGraph();
                    refresh();
                }
            });
        }
        if (plotGraphJPanel1.xy_mode) {
            jToggleButtonSplit.setSelected(false);
            setSplit(false);
            jToggleButtonSplit.setSelected(false);
        }
    }

    private void jComboBoxFuncItemStateChanged(java.awt.event.ItemEvent evt)//GEN-FIRST:event_jComboBoxFuncItemStateChanged
    {//GEN-HEADEREND:event_jComboBoxFuncItemStateChanged
        int new_function_selected = jComboBoxFunc.getSelectedIndex();
        if (new_function_selected == this.FUNC_CHOICE_PPDIFF
                && function_selected != new_function_selected) {
            PlotData currentPlot = this.getFunctionArgPlot();
            String names[] = this.getPlotNames();
            String current_plot_name = names[0];
            if (currentPlot != null) {
                current_plot_name = currentPlot.name;
            }
            String selected_name = (String) JOptionPane.showInputDialog(this,
                    "Select plot to compare",
                    "plotter query",
                    JOptionPane.QUESTION_MESSAGE,
                    null,
                    names,
                    current_plot_name);
            if (selected_name != null) {
                PlotData pd = this.getPlotByName(selected_name);
                this.jSpinnerFuncArg.setValue(Integer.valueOf(pd.num));
            }
        }
        checkComboBoxFunc(new_function_selected);
    }//GEN-LAST:event_jComboBoxFuncItemStateChanged

    private void jToggleButtonCModeItemStateChanged(java.awt.event.ItemEvent evt)//GEN-FIRST:event_jToggleButtonCModeItemStateChanged
    {//GEN-HEADEREND:event_jToggleButtonCModeItemStateChanged
        plotGraphJPanel1.c_mode = jToggleButtonCMode.isSelected();
        refresh();
    }//GEN-LAST:event_jToggleButtonCModeItemStateChanged

    public void setSplit(boolean _s_mode) {
        try {
            if (_s_mode && this.nosplit) {
                _s_mode = false;
            }
            if (jToggleButtonSplit.isSelected() != _s_mode) {
                jToggleButtonSplit.setSelected(_s_mode);
            }
            plotGraphJPanel1.s_mode = _s_mode;
            this.jTextFieldYMax.setEnabled(!plotGraphJPanel1.s_mode);
            this.jTextFieldYMin.setEnabled(!plotGraphJPanel1.s_mode);
            refresh();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void jToggleButtonSplitItemStateChanged(java.awt.event.ItemEvent evt)//GEN-FIRST:event_jToggleButtonSplitItemStateChanged
    {//GEN-HEADEREND:event_jToggleButtonSplitItemStateChanged
        setSplit(jToggleButtonSplit.isSelected());
    }//GEN-LAST:event_jToggleButtonSplitItemStateChanged

    public void Clear() {
        clearing_plots = true;
        try {
            this.plotGraphJPanel1.ClearAllData();
            this.plotGraphJPanel1.RemoveAllPlots();
            refresh();
            if (null != jTableOptions) {
                if (!this.setup_options_table_done) {
                    this.SetupOptionsTable();
                }
                ((DefaultTableModel) jTableOptions.getModel()).setRowCount(0);
            }
            if (null != jTableData) {
                ((DefaultTableModel) jTableData.getModel()).setRowCount(0);
            }
            this.jToggleButtonLockDisplay.setSelected(false);
            jComboBoxFunc.setSelectedIndex(0);
            jToggleButtonPause.setSelected(false);
            mouse_down = false;
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            clearing_plots = false;
        }
    }
    private void jButtonClearActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonClearActionPerformed
    {//GEN-HEADEREND:event_jButtonClearActionPerformed
        Clear();
    }//GEN-LAST:event_jButtonClearActionPerformed

    public void addClearActionListener(java.awt.event.ActionListener _al) {
        jButtonClear.addActionListener(_al);
    }
    private void jToggleButtonPauseItemStateChanged(java.awt.event.ItemEvent evt)//GEN-FIRST:event_jToggleButtonPauseItemStateChanged
    {//GEN-HEADEREND:event_jToggleButtonPauseItemStateChanged
        paused = jToggleButtonPause.isSelected();
        mouse_down = false;
    }//GEN-LAST:event_jToggleButtonPauseItemStateChanged

    public void setShowKey(boolean _b) {
        plotGraphJPanel1.show_key = _b;
        refresh();
        if (_b != jToggleButtonKey.isSelected()) {
            jToggleButtonKey.setSelected(_b);
        }
    }
    private void jToggleButtonKeyItemStateChanged(java.awt.event.ItemEvent evt)//GEN-FIRST:event_jToggleButtonKeyItemStateChanged
    {//GEN-HEADEREND:event_jToggleButtonKeyItemStateChanged
        setShowKey(jToggleButtonKey.isSelected());
    }//GEN-LAST:event_jToggleButtonKeyItemStateChanged

    public void FitToGraph() {
        if (this.jToggleButtonLockDisplay.isSelected()) {
            return;
        }
        cur_pgjp.FitToGraph();
        UpdateScrollBarsTextFields();
        refresh();
    }

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButton1ActionPerformed
    {//GEN-HEADEREND:event_jButton1ActionPerformed
        this.jToggleButtonLockDisplay.setSelected(false);
        FitToGraph();
    }//GEN-LAST:event_jButton1ActionPerformed

    private void jButtonZoomOutActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonZoomOutActionPerformed
    {//GEN-HEADEREND:event_jButtonZoomOutActionPerformed
        this.jToggleButtonLockDisplay.setSelected(false);
        ZoomOut();
        this.jToggleButtonLockDisplay.setSelected(true);
    }//GEN-LAST:event_jButtonZoomOutActionPerformed

    public void ZoomOut() {
        if (this.jToggleButtonLockDisplay.isSelected()) {
            return;
        }
        plotGraphJPanel1.ZoomOut();
        UpdateScrollBarsTextFields();
        refresh();
    }

    public void ZoomIn() {
        plotGraphJPanel1.ZoomIn();
        UpdateScrollBarsTextFields();
        refresh();
    }

    private void jButtonZoomInActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonZoomInActionPerformed
    {//GEN-HEADEREND:event_jButtonZoomInActionPerformed
        this.jToggleButtonLockDisplay.setSelected(false);
        ZoomIn();
        this.jToggleButtonLockDisplay.setSelected(true);
    }//GEN-LAST:event_jButtonZoomInActionPerformed
    JPopupMenu jpop = null;
    javax.swing.JCheckBoxMenuItem jpopFullScreenCheckboxMenuItem = null;
    JMenuItem jpopNoFullScreenMenuItem = null;
    JMenuItem jpopSaveImageMenuItem = null;
    JMenuItem jpopSetZoomMenuItem = null;
    JMenuItem jpopSetFitMenuItem = null;

    public BufferedImage getImage() {
        return this.plotGraphJPanel1.getImage();
    }

    public BufferedImage getImage(Dimension d) {
        return this.plotGraphJPanel1.getImage(d);
    }

    static private void HideMe(Container c) {
        Container p = c.getParent();
        if (null != p) {
            HideMe(p);
        } else {
            c.setVisible(false);
        }
    }

    static private void ShowMe(Container c) {
        Container p = c.getParent();
        if (null != p) {
            ShowMe(p);
        } else {
            c.setVisible(true);
            try {
                JFrame jf = (JFrame) c;
                jf.pack();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private void ShowMe() {
        Container p = this.getParent();
        if (null != p) {
            ShowMe(p);
        }
        this.plotGraphJPanel1.setVisible(true);
        setVisible(true);
    }

    private void HideMe() {
        Container p = this.getParent();
        if (null != p) {
            HideMe(p);
        }
        setVisible(false);
        this.plotGraphJPanel1.setVisible(true);
    }

    public void SetKeyListener(KeyListener kl) {
        KeyListener kls[] = this.getKeyListeners();
        for (KeyListener kl_to_remove : kls) {
            this.removeKeyListener(kl_to_remove);
        }
        this.addKeyListener(kl);
    }

    private void ShowFullScreen() {

        if (PlotterCommon.debug_on) {
            PlotterCommon.DebugPrint("ShowFullScreen");
        }
        this.plotGraphJPanel1.fullScreenJFrame = new JFrame();
        this.plotGraphJPanel1.fullScreenJFrame.setUndecorated(true);
        this.fullScreenPlotGraphJPanel = new PlotGraphJPanel();

        //this.plotGraphJPanel1 = new PlotGraphJPanel();
//	this.plotGraphJPanel1.fullScreenJPanel = new fullScreenJPanelClass();
//	Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
//	this.plotGraphJPanel1.fullScreenJPanel.setSize(d);
//	this.plotGraphJPanel1.fullScreenJPanel.SetImage(getImage(d));
        this.fullScreenPlotGraphJPanel.addMouseListener(new java.awt.event.MouseAdapter() {
            @Override
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                plotGraphJPanel1MouseClicked(evt);
            }

            @Override
            public void mousePressed(java.awt.event.MouseEvent evt) {
                plotGraphJPanel1MousePressed(evt);
            }

            @Override
            public void mouseReleased(java.awt.event.MouseEvent evt) {
                plotGraphJPanel1MouseReleased(evt);
            }
        });

        this.fullScreenPlotGraphJPanel.addMouseMotionListener(new java.awt.event.MouseMotionAdapter() {
            public void mouseDragged(java.awt.event.MouseEvent evt) {
                plotGraphJPanel1MouseDragged(evt);
            }
        });
        KeyListener kl = new KeyListener() {
            private void HandleKey(final int key_code) {
                switch (key_code) {
                    case KeyEvent.VK_ESCAPE:
                        ClearFullScreen();
                        break;
                    case KeyEvent.VK_LEFT:
                        ScrollLeftInc();
                        break;
                    case KeyEvent.VK_RIGHT:
                        ScrollRightInc();
                        break;
                    case KeyEvent.VK_UP:
                        ScrollUpInc();
                        break;
                    case KeyEvent.VK_DOWN:
                        ScrollDownInc();
                        break;
                }
            }

            @Override
            public void keyTyped(KeyEvent e) {
                HandleKey(e.getKeyCode());
            }

            @Override
            public void keyPressed(KeyEvent e) {
                HandleKey(e.getKeyCode());
            }

            @Override
            public void keyReleased(KeyEvent e) {
            }
        };

        this.fullScreenPlotGraphJPanel.SetKeyListener(kl);
        this.fullScreenPlotGraphJPanel.CopySettings(plotGraphJPanel1);
        this.cur_pgjp = this.fullScreenPlotGraphJPanel;
        this.plotGraphJPanel1.fullScreenJFrame.addKeyListener(kl);

//	this.plotGraphJPanel1.fullScreenJFrame.add(this.plotGraphJPanel1.fullScreenJPanel);
        this.plotGraphJPanel1.fullScreenJFrame.add(this.fullScreenPlotGraphJPanel);
        this.plotGraphJPanel1.fullScreenJFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        GraphicsDevice gd = this.getGraphicsConfiguration().getDevice();

        HideMe();

        gd.setFullScreenWindow(this.plotGraphJPanel1.fullScreenJFrame);
        this.fullScreenPlotGraphJPanel.is_full_screen = true;
        this.fullScreenPlotGraphJPanel.HandleResize();

    }

    private void ClearFullScreen() {
        if (PlotterCommon.debug_on) {
            PlotterCommon.DebugPrint("ClearFullScreen");
        }
        if (null != this.plotGraphJPanel1.fullScreenJFrame) {
            this.plotGraphJPanel1.fullScreenJFrame.dispose();
            this.plotGraphJPanel1.fullScreenJFrame = null;
        }
        if (null != this.fullScreenPlotGraphJPanel) {
            this.fullScreenPlotGraphJPanel.setVisible(false);
            this.fullScreenPlotGraphJPanel = null;
        }

        this.plotGraphJPanel1.is_full_screen = false;
        this.jpopFullScreenCheckboxMenuItem.setSelected(false);
        GraphicsDevice gd = this.getGraphicsConfiguration().getDevice();
        gd.setFullScreenWindow(null);
//	this.plotGraphJPanel1.fullScreenJPanel = null;
        ShowMe();
        this.cur_pgjp = this.plotGraphJPanel1;
        this.plotGraphJPanel1.HandleResize();
    }

    private void popup_create() {
        jpop = new JPopupMenu();
        jpopFullScreenCheckboxMenuItem
                = new JCheckBoxMenuItem("Full Screen");
        jpopFullScreenCheckboxMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if (e.getSource() == jpopFullScreenCheckboxMenuItem) {
                    if (PlotterCommon.debug_on) {
                        PlotterCommon.DebugPrint("jpopFullScreenCheckboxMenuItem.isSelected()=" + jpopFullScreenCheckboxMenuItem.isSelected());
                    }
                    if (!cur_pgjp.is_full_screen) {
                        ShowFullScreen();
                    } else {
                        ClearFullScreen();
                    }

                    if (cur_pgjp.is_full_screen != jpopFullScreenCheckboxMenuItem.isSelected()) {
                        jpopFullScreenCheckboxMenuItem.setSelected(cur_pgjp.is_full_screen);
                    }
                }
            }
        });
        jpop.add(this.jpopFullScreenCheckboxMenuItem);
        jpopSaveImageMenuItem
                = new JMenuItem("Save Image As ...");
        jpopSaveImageMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                SaveImage.SaveImageAs(getImage(), getParent());
            }
        });
        jpop.add(jpopSaveImageMenuItem);

        this.jpopSetFitMenuItem = new JMenuItem("Fit");
        jpopSetFitMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                jToggleButtonLockDisplay.setSelected(false);
                FitToGraph();
            }
        });
        jpop.add(this.jpopSetFitMenuItem);
    }

    private void popup_show(java.awt.event.MouseEvent evt) {
        try {
            if (PlotterCommon.debug_on) {
                PlotterCommon.DebugPrint("evt=" + evt);
            }
            if (null == jpop) {
                popup_create();
            }
            if (null != jpop) {
                jpop.show(evt.getComponent(), evt.getX(), evt.getY());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void plotGraphJPanel1MouseClicked(java.awt.event.MouseEvent evt)//GEN-FIRST:event_plotGraphJPanel1MouseClicked
    {//GEN-HEADEREND:event_plotGraphJPanel1MouseClicked
        if (evt.isPopupTrigger()) {
            plotGraphJPanel1.show_rect = false;
            mouse_down
                    = false;
            popup_show(evt);
            return;
        }
        plotGraphJPanel1.mouseClicked(evt);
        this.jToggleButtonLockDisplay.setSelected(true);
        refresh();
        mouse_down  = false;
    }//GEN-LAST:event_plotGraphJPanel1MouseClicked
    private boolean last_press_was_popup = false;

    private void plotGraphJPanel1MousePressed(java.awt.event.MouseEvent evt)//GEN-FIRST:event_plotGraphJPanel1MousePressed
    {//GEN-HEADEREND:event_plotGraphJPanel1MousePressed
        if (evt.isPopupTrigger()) {
            plotGraphJPanel1.show_rect = false;
            mouse_down = false;
            popup_show(evt);
            last_press_was_popup = true;
            return;
        }
        plotGraphJPanel1.mousePressed(evt);
        this.jToggleButtonLockDisplay.setSelected(true);
        refresh();
        mouse_down= false;
        last_press_was_popup= false;

    }//GEN-LAST:event_plotGraphJPanel1MousePressed

    private void plotGraphJPanel1MouseReleased(java.awt.event.MouseEvent evt)//GEN-FIRST:event_plotGraphJPanel1MouseReleased
    {//GEN-HEADEREND:event_plotGraphJPanel1MouseReleased
        if (evt.isPopupTrigger()) {
            plotGraphJPanel1.show_rect = false;
            mouse_down = false;
            popup_show(evt);
            last_press_was_popup = true;
            return;
        }
        if (last_press_was_popup) {
            last_press_was_popup = false;
            return;
        }
        cur_pgjp.mouseReleased(evt);
        if (cur_pgjp.rescale_to_selected_rectangle_needed) {
            this.jToggleButtonLockDisplay.setSelected(true);
            SetScaleToSelectedRect();
        }
        refresh();
        mouse_down = false;
    }//GEN-LAST:event_plotGraphJPanel1MouseReleased

    public void SetGraphLimits(double new_x_show_area_min,
            double new_x_show_area_max,
            double new_y_show_area_min,
            double new_y_show_area_max) {
        PlotGraphScreenMap cur_screen_map = plotGraphJPanel1.screen_map;
        if (plotGraphJPanel1.get_array_mode()) {
            cur_screen_map = plotGraphJPanel1.array_mode_screen_map;
        }

        cur_screen_map.set_x_show_area(new_x_show_area_min, new_x_show_area_max);
        if (!plotGraphJPanel1.s_mode) {
            cur_screen_map.set_y_show_area(new_y_show_area_min, new_y_show_area_max);
            if (plotGraphJPanel1.e_mode) {
                cur_screen_map.equalizeAxis();
            }

        }
        UpdateScrollBarsTextFields();
    }

    private void SetScaleToSelectedRect() {
        PlotGraphScreenMap cur_screen_map = cur_pgjp.screen_map;
        if (plotGraphJPanel1.get_array_mode()) {
            cur_screen_map = cur_pgjp.array_mode_screen_map;
        }
        double new_x_show_area_min = cur_screen_map.get_x_value(cur_pgjp.selected_rectangle.x);
        int new_max_x_pos = plotGraphJPanel1.selected_rectangle.x + cur_pgjp.selected_rectangle.width;
        double new_x_show_area_max = cur_screen_map.get_x_value(new_max_x_pos);
        int new_min_y_pos = plotGraphJPanel1.selected_rectangle.y + cur_pgjp.selected_rectangle.height;
        double new_y_show_area_min = cur_screen_map.get_y_value(new_min_y_pos);
        double new_y_show_area_max = cur_screen_map.get_y_value(cur_pgjp.selected_rectangle.y);
        SetGraphLimits(new_x_show_area_min,
                new_x_show_area_max,
                new_y_show_area_min,
                new_y_show_area_max);
        mouse_down = false;
    }

    private void plotGraphJPanel1MouseDragged(java.awt.event.MouseEvent evt)//GEN-FIRST:event_plotGraphJPanel1MouseDragged
    {//GEN-HEADEREND:event_plotGraphJPanel1MouseDragged
        this.jToggleButtonLockDisplay.setSelected(true);
        this.cur_pgjp.mouseDragged(evt);
        mouse_down = true;
    }//GEN-LAST:event_plotGraphJPanel1MouseDragged
    private int last_xTextField_changed_count = -1;

    public double getXMin() {
        return plotGraphJPanel1.screen_map.get_x_min();
    }

    public double getXMax() {
        return plotGraphJPanel1.screen_map.get_x_max();
    }

    public double getYMin() {
        return plotGraphJPanel1.screen_map.get_y_min();
    }

    public double getYMax() {
        return plotGraphJPanel1.screen_map.get_x_max();
    }

    private void UpdateXTextFields() {
        PlotGraphScreenMap cur_screen_map = plotGraphJPanel1.screen_map;
        if (plotGraphJPanel1.get_array_mode()) {
            cur_screen_map = plotGraphJPanel1.array_mode_screen_map;
        }

        try {

            if (last_xTextField_changed_count != cur_screen_map.get_changed_count()) {
                last_xTextField_changed_count = cur_screen_map.get_changed_count();
                String old_text = jTextFieldXMin.getText();
                double old_value = Double.valueOf(old_text).doubleValue();
                if (old_value != cur_screen_map.get_x_min()) {
                    String new_text = dcFormat.format(cur_screen_map.get_x_min());
                    if (old_text.compareTo(new_text) != 0) {
                        jTextFieldXMin.setText(new_text);
                    }

                }
                old_text = jTextFieldXMax.getText();
                old_value = Double.valueOf(old_text).doubleValue();
                if (old_value != cur_screen_map.get_x_max()) {
                    String new_text = dcFormat.format(cur_screen_map.get_x_max());
                    if (old_text.compareTo(new_text) != 0) {
                        jTextFieldXMax.setText(new_text);
                    }
                }
                if (this.plotGraphJPanel1.s_mode) {
                    this.jLabelYScale.setText(" ----------- ");
                }
                final double x_max_min = cur_screen_map.get_x_max() - cur_screen_map.get_x_min();
                this.jLabelXMaxMin.setText(String.format("X Max-Min:%9.3g", x_max_min));
            }
        } finally {
            last_xTextField_changed_count = cur_screen_map.get_changed_count();
        }

    }
    private int last_yTextField_changed_count = -1;

    private void UpdateYTextFields() {
        PlotGraphScreenMap cur_screen_map = plotGraphJPanel1.screen_map;
        if (plotGraphJPanel1.get_array_mode()) {
            cur_screen_map = plotGraphJPanel1.array_mode_screen_map;
        }

        try {

            if (last_yTextField_changed_count != cur_screen_map.get_changed_count() && !plotGraphJPanel1.s_mode) {
                last_yTextField_changed_count = cur_screen_map.get_changed_count();
                String old_text = jTextFieldYMin.getText();
                double old_value = Double.valueOf(old_text).doubleValue();
                if (old_value != cur_screen_map.get_y_min()) {
                    String new_text = dcFormat.format(cur_screen_map.get_y_min());
                    if (old_text.compareTo(new_text) != 0) {
                        jTextFieldYMin.setText(new_text);
                    }
// ((TextFieldNumberModel) jTextFieldYMax.getModel()).setMinimum(new Double(cur_screen_map.get_y_min()+1e-9));

                }
                old_text = jTextFieldYMax.getText();
                old_value
                        = Double.valueOf(old_text).doubleValue();
                if (old_value != cur_screen_map.get_y_max()) {
                    String new_text = dcFormat.format(cur_screen_map.get_y_max());
                    if (old_text.compareTo(new_text) != 0) {
                        jTextFieldYMax.setText(new_text);
                    }
// ((TextFieldNumberModel) jTextFieldYMin.getModel()).setMaximum(new Double(cur_screen_map.get_y_max()-1e-9));

                }
            }
        } finally {
            last_yTextField_changed_count = cur_screen_map.get_changed_count();
        }

    }

    private void UpdateTextFields() {
        UpdateXTextFields();
        UpdateYTextFields();

    }
    private int last_scrollbar_changed_count = -1;

    private void UpdateScrollBars() {
        PlotGraphScreenMap cur_screen_map = plotGraphJPanel1.screen_map;
        if (plotGraphJPanel1.get_array_mode()) {
            cur_screen_map = plotGraphJPanel1.array_mode_screen_map;
        }

        if (last_scrollbar_changed_count != cur_screen_map.get_changed_count()) {
            last_scrollbar_changed_count = cur_screen_map.get_changed_count();
            cur_screen_map.update_horz_scrollbar(jScrollBarHorz);
            cur_screen_map.update_vert_scrollbar(jScrollBarVert);
        }

    }

    private void UpdateScrollBarsTextFields() {
        UpdateScrollBars();
        UpdateTextFields();

    }

    private void plotGraphJPanel1ComponentResized(java.awt.event.ComponentEvent evt)//GEN-FIRST:event_plotGraphJPanel1ComponentResized
    {//GEN-HEADEREND:event_plotGraphJPanel1ComponentResized
        plotGraphJPanel1.HandleResize();
    }//GEN-LAST:event_plotGraphJPanel1ComponentResized

    private void ScrollLeftInc() {
        PlotGraphScreenMap cur_screen_map = plotGraphJPanel1.screen_map;
        int old_x = cur_screen_map.get_scroll_x();
        cur_screen_map.set_scroll_x(old_x + 10);
        UpdateXTextFields();

        refresh();

    }

    private void ScrollRightInc() {
        PlotGraphScreenMap cur_screen_map = plotGraphJPanel1.screen_map;
        int old_x = cur_screen_map.get_scroll_x();
        cur_screen_map.set_scroll_x(old_x - 10);
        UpdateXTextFields();

        refresh();

    }

    private void ScrollUpInc() {
        PlotGraphScreenMap cur_screen_map = plotGraphJPanel1.screen_map;
        int old_y = cur_screen_map.get_scroll_y();
        cur_screen_map.set_scroll_y(old_y + 10);
        UpdateXTextFields();

        refresh();

    }

    private void ScrollDownInc() {
        PlotGraphScreenMap cur_screen_map = plotGraphJPanel1.screen_map;
        int old_y = cur_screen_map.get_scroll_y();
        cur_screen_map.set_scroll_y(old_y - 10);
        UpdateXTextFields();

        refresh();

    }

    private void jScrollBarHorzAdjustmentValueChanged(java.awt.event.AdjustmentEvent evt)//GEN-FIRST:event_jScrollBarHorzAdjustmentValueChanged
    {//GEN-HEADEREND:event_jScrollBarHorzAdjustmentValueChanged
        PlotGraphScreenMap cur_screen_map = plotGraphJPanel1.screen_map;
        if (plotGraphJPanel1.get_array_mode()) {
            cur_screen_map = plotGraphJPanel1.array_mode_screen_map;
        }

        int x = jScrollBarHorz.getValue();
        if (jScrollBarHorz.isEnabled()
                && x != cur_screen_map.get_scroll_x()) {
            cur_screen_map.set_scroll_x(x);
            UpdateXTextFields();
            refresh();
        }
    }//GEN-LAST:event_jScrollBarHorzAdjustmentValueChanged

    private void jScrollBarVertAdjustmentValueChanged(java.awt.event.AdjustmentEvent evt)//GEN-FIRST:event_jScrollBarVertAdjustmentValueChanged
    {//GEN-HEADEREND:event_jScrollBarVertAdjustmentValueChanged
        PlotGraphScreenMap cur_screen_map = plotGraphJPanel1.screen_map;
        if (plotGraphJPanel1.get_array_mode()) {
            cur_screen_map = plotGraphJPanel1.array_mode_screen_map;
        }

        int y = jScrollBarVert.getValue();
        if (jScrollBarVert.isEnabled()
                && y != cur_screen_map.get_scroll_y()) {
            cur_screen_map.set_scroll_y(y);
            UpdateYTextFields();

            refresh();

        }
    }//GEN-LAST:event_jScrollBarVertAdjustmentValueChanged

    public void SetReverseX(boolean _new_reverse_x) {
        if (null != this.jCheckBoxReverseX
                && this.jCheckBoxReverseX.isSelected() != _new_reverse_x) {
            this.jCheckBoxReverseX.setSelected(_new_reverse_x);
        }

        if (null != this.plotGraphJPanel1) {
            this.plotGraphJPanel1.SetReverseX(_new_reverse_x);
        }

        this.FitToGraph();
    }

	private void jCheckBoxReverseXItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_jCheckBoxReverseXItemStateChanged
            SetReverseX(this.jCheckBoxReverseX.isSelected());
	}//GEN-LAST:event_jCheckBoxReverseXItemStateChanged

	private void jButtonClearDataOnlyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonClearDataOnlyActionPerformed
            this.plotGraphJPanel1.ClearAllData();
            jToggleButtonPause.setSelected(false);
	}//GEN-LAST:event_jButtonClearDataOnlyActionPerformed

	private void jButtonAxisActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonAxisActionPerformed
            this.plotGraphJPanel1.axis_color = JColorChooser.showDialog(
                    this,
                    "Choose Axis Color",
                    this.plotGraphJPanel1.axis_color);
            jButtonAxis.setBackground(this.plotGraphJPanel1.axis_color);
            jButtonAxis.setForeground(this.plotGraphJPanel1.axis_color);
            refresh();

            this.plotGraphJPanel1.SaveOptions();
	}//GEN-LAST:event_jButtonAxisActionPerformed

    public String ComputeStatsString() throws Exception {
        StringBuffer sb = new StringBuffer();
        Iterator<PlotData> it = this.plotGraphJPanel1.plots.values().iterator();
        while (it.hasNext()) {
            PlotData pd = it.next();
            if (!pd.getShow(this.plotGraphJPanel1.plotter_num)) {
                continue;
            }
            sb.append(pd.getStatsString());
            if (pd.y_plot_data != null) {
                sb.append("** " + pd.y_plot_data.name + " is y_data for " + pd.name + "\n");
                sb.append(pd.y_plot_data.getStatsString());
                sb.append("** " + pd.y_plot_data.name + " is y_data for " + pd.name + "\n");
            }
        }
        return sb.toString();
    }

        private void jButtonStatsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonStatsActionPerformed
            try {
                String s = this.ComputeStatsString();
                StatsTextJFrame stats_jf = new StatsTextJFrame(s);
                System.out.println(s);
            } catch (Exception exception) {
                exception.printStackTrace();
            }
        }//GEN-LAST:event_jButtonStatsActionPerformed

        private void jButtonShowAllActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonShowAllActionPerformed
            for (PlotData pd : this.plotGraphJPanel1.plots.values()) {
                pd.setShow(this.plotGraphJPanel1.plotter_num, true);
            }
            this.InitOptionsTable();
        }//GEN-LAST:event_jButtonShowAllActionPerformed

        private void jButtonHideAllActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonHideAllActionPerformed
            for (PlotData pd : this.plotGraphJPanel1.plots.values()) {
                pd.setShow(this.plotGraphJPanel1.plotter_num, false);
            }
            this.InitOptionsTable();
        }//GEN-LAST:event_jButtonHideAllActionPerformed

        private void jCheckBoxApplyAbsYActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCheckBoxApplyAbsYActionPerformed
            this.setApply_absolute_value(this.jCheckBoxApplyAbsY.isSelected());
            this.RecalculatePlots();
        }//GEN-LAST:event_jCheckBoxApplyAbsYActionPerformed

    private void jCheckBoxLabelPointsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCheckBoxLabelPointsActionPerformed
        this.plotGraphJPanel1.label_points = this.jCheckBoxLabelPoints.isSelected();
        this.plotGraphJPanel1.repaint();
    }//GEN-LAST:event_jCheckBoxLabelPointsActionPerformed

    private void jCheckBoxUseShortNamesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCheckBoxUseShortNamesActionPerformed
        this.plotGraphJPanel1.setUseShortname(this.jCheckBoxUseShortNames.isSelected());
    }//GEN-LAST:event_jCheckBoxUseShortNamesActionPerformed

    private void jTextFieldYScaleActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jTextFieldYScaleActionPerformed
        String vs = "";
        try {
            vs = this.jTextFieldYScale.getText();
            double d = Double.valueOf(vs);
            this.plotGraphJPanel1.setForced_Y_Grid(d);
            this.plotGraphJPanel1.repaint();
        } catch (NumberFormatException numberFormatException) {
            numberFormatException.printStackTrace();
            JOptionPane.showMessageDialog(this, "YScale field (" + vs + ") does not have a valid number format.");
        }
    }//GEN-LAST:event_jTextFieldYScaleActionPerformed

    private void jTextFieldXScaleActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jTextFieldXScaleActionPerformed
        String vs = "";
        try {
            vs = this.jTextFieldXScale.getText();
            double d = Double.valueOf(vs);
            this.plotGraphJPanel1.setForced_X_Grid(d);
            this.plotGraphJPanel1.repaint();
        } catch (NumberFormatException numberFormatException) {
            numberFormatException.printStackTrace();
            JOptionPane.showMessageDialog(this, "XScale field (" + vs + ") does not have a valid number format.");
        }
    }//GEN-LAST:event_jTextFieldXScaleActionPerformed
    private PlotLoader pl = null;

    public void setUseShortname(boolean _UseShortName) {
        if (null != this.jCheckBoxUseShortNames) {
            this.jCheckBoxUseShortNames.setSelected(_UseShortName);
        }
        this.plotGraphJPanel1.setUseShortname(_UseShortName);
    }

    public void SaveStatsFile(File f) {
        try {
            String s = this.ComputeStatsString();
            PrintStream ps = new PrintStream(new FileOutputStream(f));
            ps.println(s);
            ps.close();
        } catch (Exception exception) {
            exception.printStackTrace();
        }
    }

    private void InitOptionsTable() {
        if (!this.setup_options_table_done) {
            this.SetupOptionsTable();
        }

        ((DefaultTableModel) jTableOptions.getModel()).setRowCount(0);
        for (PlotData pd : this.plotGraphJPanel1.plots.values()) {
            this.AddPlotToOptionsTable(pd);
        }
        if (this.jCheckBoxApplyAbsY != null
                && this.jCheckBoxApplyAbsY.isSelected() != apply_absolute_value) {
            this.jCheckBoxApplyAbsY.setSelected(apply_absolute_value);
        }
    }

    public void ReloadFile() {
        this.Clear();
        if (null != pl) {
            pl.Reload();
        }

        if (null != this.plotGraphJPanel1
                && null != this.plotGraphJPanel1.plots
                && this.plotGraphJPanel1.plots.size() > 3
                && this.jComboBoxFunc.getSelectedIndex() == 0) {
            this.jToggleButtonSplit.setSelected(true);
            this.plotGraphJPanel1.s_mode = true;
            this.jTextFieldYMax.setEnabled(!plotGraphJPanel1.s_mode);
            this.jTextFieldYMin.setEnabled(!plotGraphJPanel1.s_mode);
            refresh();

        }

        this.last_function_selected = -1;
        this.checkComboBoxFunc(this.function_selected);
        InitOptionsTable();

        RecalculatePlots();

    }

    public int get_num_plots() {
        return this.plotGraphJPanel1.plots.size();
    }
    diagapplet.utils.URlLoadInfoFrame loadInfoFrame = null;

    /**
     * Loads two arrays into a single plot and displays it. The two arrays
     * should be the same length or points will only be plotted up to the
     * shorter of the two.
     *
     * @param name -- name of the plot
     * @param xA -- x axis array
     * @param yA -- y axis array
     */
    public void LoadXYFloatArrays(String name, float xA[], float yA[]) {
        //Clear();
        if (xA == null) {
            this.LoadFloatArray(name, yA);
            return;
        }
        PlotData pd = new PlotData();
        pd.name = name;
        this.plotGraphJPanel1.AddPlot(pd, name);
        for (int i = 0; i
                < xA.length && i < yA.length; i++) {
            this.plotGraphJPanel1.AddPointToPlot(pd, xA[i], yA[i], true, xA[i], yA[i]);

        }

        RecalculatePlots();
        if (auto_fit_to_graph) {
            FitToGraph();
        }

        pd.setShowAll(total_plotters, false);
        pd.setShow(this.plotGraphJPanel1.plotter_num, true);
        refresh();
    }

    /**
     * Loads two arrays into a single plot and displays it. The two arrays
     * should be the same length or points will only be plotted up to the
     * shorter of the two.
     *
     * @param name -- name of the plot
     * @param xA -- x axis array
     * @param yA -- y axis array
     */
    public void LoadXYDoubleArrays(String name, double xA[], double yA[]) {
        //Clear();
        if (xA == null) {
            this.LoadDoubleArray(name, yA);
            return;
        }
        PlotData pd = new PlotData();
        pd.name = name;
        this.plotGraphJPanel1.AddPlot(pd, name);
        for (int i = 0; i
                < xA.length && i < yA.length; i++) {
            this.plotGraphJPanel1.AddPointToPlot(pd, xA[i], yA[i], true, xA[i], yA[i]);

        }

        RecalculatePlots();
        if (auto_fit_to_graph) {
            FitToGraph();
        }

        pd.setShowAll(total_plotters, false);
        pd.setShow(this.plotGraphJPanel1.plotter_num, true);
        refresh();
    }

    /**
     * Loads an arrays into a single plot and displays it.
     *
     * @param name -- name of the plot
     * @param fa -- float array to plot.
     */
    public void LoadFloatArray(String name, float fa[]) {
        //Clear();
        PlotData pd = new PlotData();
        pd.name = name;
        this.plotGraphJPanel1.AddPlot(pd, name);
        for (int i = 0; i < fa.length; i++) {
            this.plotGraphJPanel1.AddPointToPlot(pd, (float) i, fa[i], true, (float) i, fa[i]);
        }

        RecalculatePlots();
        if (auto_fit_to_graph) {
            FitToGraph();
        }
        pd.setShowAll(total_plotters, false);
        pd.setShow(this.plotGraphJPanel1.plotter_num, true);
        refresh();
    }

    /**
     * Loads an arrays into a single plot and displays it.
     *
     * @param name -- name of the plot
     * @param da -- double array to plot.
     */
    public void LoadDoubleArray(String name, double da[]) {
        //Clear();
        PlotData pd = new PlotData();
        pd.name = name;
        this.plotGraphJPanel1.AddPlot(pd, name);
        for (int i = 0; i < da.length; i++) {
            this.plotGraphJPanel1.AddPointToPlot(pd, (float) i, da[i], true, (float) i, da[i]);
        }

        RecalculatePlots();
        if (auto_fit_to_graph) {
            FitToGraph();
        }
        pd.setShowAll(total_plotters, false);
        pd.setShow(this.plotGraphJPanel1.plotter_num, true);
        refresh();
    }
    private boolean nosplit = false;

    /**
     * Get the value of nosplit
     *
     * @return the value of nosplit
     */
    public boolean isNosplit() {
        return nosplit;
    }

    /**
     * Set the value of nosplit
     *
     * @param nosplit new value of nosplit
     */
    public void setNosplit(boolean nosplit) {
        if (nosplit) {
            if (null != plotGraphJPanel1) {
                plotGraphJPanel1.s_mode = false;
            }
            if (null != this.jToggleButtonSplit) {
                this.jToggleButtonSplit.setSelected(false);
                this.jToggleButtonSplit.setEnabled(false);
            }
            this.setSplit(false);
        } else {
            if (null != this.jToggleButtonSplit) {
                this.jToggleButtonSplit.setEnabled(true);
            }
        }
        this.nosplit = nosplit;
    }

    public void setParseOptions(ParseOptions po) {
        if (pl == null) {
            pl = new PlotLoader(this.plotGraphJPanel1);
        }
        pl.setParseOptions(po);
    }

    public void loadFile(String fileName) {
        List<PlotData> new_plots = null;
        try {
            try {
                if (pl == null) {
                    pl = new PlotLoader(this.plotGraphJPanel1);
                }

                if (this.isVisible()) {
                    if (null == loadInfoFrame) {
                        loadInfoFrame = new diagapplet.utils.URlLoadInfoFrame();
                    }

                    java.awt.EventQueue.invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            if (null != loadInfoFrame) {
                                loadInfoFrame.setVisible(true);
                            }

                        }
                    });
                }

                if (null != loadInfoFrame) {
                    pl.set_load_info_panel(loadInfoFrame.get_uRLLoadInfoPanel());
                }

                //ProgressMonitor pm = new ProgressMonitor(this,fileName,fileName,0,0);
                new_plots = pl.LoadURL(fileName, null);
                pl.set_load_info_panel(null);
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                if (null != loadInfoFrame) {
                    loadInfoFrame.setVisible(false);
                    java.awt.EventQueue.invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            if (null != loadInfoFrame) {
                                loadInfoFrame.setVisible(false);
                                loadInfoFrame.dispose();
                                loadInfoFrame
                                        = null;
                            }

                        }
                    });
                }

            }
            if (null != this.plotGraphJPanel1
                    && null != this.plotGraphJPanel1.plots
                    && this.plotGraphJPanel1.plots.size() > 3
                    && this.jComboBoxFunc.getSelectedIndex() == 0
                    && !fileName.endsWith(".xy")
                    && !this.nosplit) {
                this.jToggleButtonSplit.setSelected(true);
                this.plotGraphJPanel1.s_mode = true;
                this.jTextFieldYMax.setEnabled(!plotGraphJPanel1.s_mode);
                this.jTextFieldYMin.setEnabled(!plotGraphJPanel1.s_mode);
                refresh();

            }

            this.last_function_selected = -1;
            this.checkComboBoxFunc(this.function_selected);
            InitOptionsTable();

            RecalculatePlots();
            ParseOptions po = pl.getParseOptions();
            if (null != po && null != po.getFieldsToShow() && null != new_plots) {
                for (int i = 0; i < new_plots.size(); i++) {
                    PlotData pd = new_plots.get(i);
                    pd.setShowAll(plotter_NB_UI.getTotalPlotters(), false);
                }
                int fields[] = po.getFieldsToShow();
                int adjusti = po.isPlotVersusLineNumber() ? 0 : 2;
                for (int i = 0; i < fields.length; i++) {
                    if (fields[i] - adjusti < 0 || fields[i] - adjusti > new_plots.size()) {
                        continue;
                    }
                    PlotData pd = new_plots.get(fields[i] - adjusti);
                    pd.setShowAll(plotter_NB_UI.getTotalPlotters(), true);
                }
            }
            int changing_plots = 0;
            int unchanging_plots = 0;
            for (PlotData pd : this.plotGraphJPanel1.plots.values()) {
                if (pd.max_orig_y > pd.min_orig_y
                        && pd.max_orig_x > pd.min_orig_x) {
                    changing_plots++;
                } else {
                    unchanging_plots++;
                }
            }
            if (unchanging_plots > 0 && changing_plots > 2) {
                System.out.println("Hiding unchanging_plots = " + unchanging_plots);
                for (PlotData pd : plotGraphJPanel1.plots.values()) {
                    if (pd.max_orig_y == pd.min_orig_y) {
                        System.out.println("Hiding " + pd.short_name + " max_orig_y == min_orig_y :" + pd.max_orig_y);
                        pd.setShowAll(1, false);
                    } else if (pd.max_orig_x == pd.min_orig_x) {
                        System.out.println("Hiding " + pd.short_name + " max_orig_x == min_orig_x :" + pd.max_orig_x);
                        pd.setShowAll(1, false);
                    }
                }
            }
        } catch (java.lang.OutOfMemoryError oome) {
            oome.printStackTrace();
            System.out.println("max=" + Runtime.getRuntime().maxMemory() + ", total=" + Runtime.getRuntime().totalMemory() + ", free=" + Runtime.getRuntime().freeMemory());
        }

    }

    public void ForceRecheckComboFunc() {
        int fs = this.function_selected;
        this.last_function_selected = -1;
        this.function_selected = -1;
        this.checkComboBoxFunc(fs);
    }

    public void ScrollRight() {
        if (this.jToggleButtonLockDisplay.isSelected()) {
            return;
        }

        plotGraphJPanel1.ScrollRight();
        UpdateScrollBarsTextFields();

        refresh();

    }

    public boolean isLocked() {
        return this.jToggleButtonLockDisplay.isSelected() || mouse_down;
    }

    private void AddPlotToOptionsTable(PlotData pd) {
        if (!this.setup_options_table_done) {
            this.SetupOptionsTable();
        }
        Object rowObject[] = new Object[7];
        rowObject[0] = pd.short_name;
        rowObject[1] = pd.getLine_color();
        rowObject[2] = pd.getPoint_color();
        rowObject[3] = Boolean.valueOf(pd.getShow(plotGraphJPanel1.plotter_num));
        rowObject[4] = Boolean.valueOf(false);
        rowObject[5] = pd.name;
        rowObject[6] = pd.getPointSize();
        ((DefaultTableModel) jTableOptions.getModel()).addRow(rowObject);
        if (null != this.plotGraphJPanel1) {
            this.plotGraphJPanel1.setPlotOrder(plot_order);
        }
    }

    public void AddPlot(PlotData pd, String name) {
        pd.setShowAll(total_plotters, true);
        plotGraphJPanel1.set_array_mode(false);
        jToggleButtonArrayMode.setSelected(false);
        this.plotGraphJPanel1.AddPlot(pd, name);
        AddPlotToOptionsTable(pd);
    }

    public void AddPlot(PlotData pd) {
        pd.setShowAll(total_plotters, true);
        plotGraphJPanel1.set_array_mode(false);
        jToggleButtonArrayMode.setSelected(false);
        this.plotGraphJPanel1.AddPlot(pd);
        AddPlotToOptionsTable(pd);
    }

    public void AddArrayPlot(PlotData pd, String name) {
        pd.setShowAll(total_plotters, true);
        pd.array_type = true;
        plotGraphJPanel1.set_array_mode(true);
        jToggleButtonArrayMode.setSelected(true);
        pd.setShow(plotGraphJPanel1.plotter_num, true);
        this.plotGraphJPanel1.AddPlot(pd, name);
        AddPlotToOptionsTable(pd);
    }

    public void refresh() {
        if (null != this.fullScreenPlotGraphJPanel) {
            this.fullScreenPlotGraphJPanel.refresh();
        } else {
            this.plotGraphJPanel1.refresh();
        }
    }

    public void AddPointToPlot(PlotData pd, double pre_f_x, double pre_f_y, boolean connected) {
        double x = this.apply_function_to_point_x(pd, pre_f_x, pre_f_y);
        double y = this.apply_function_to_point(pd, pre_f_y, pre_f_y);
        this.plotGraphJPanel1.AddPointToPlot(pd, x, y, connected, pre_f_x, pre_f_y);
        point_added_since_check_recalc_plots = true;
    }

    public void AddPointToArrayPlot(PlotData pd, int index, double pre_f_y) {
        double x = this.apply_function_to_point_x(pd, (double) index, pre_f_y);
        double y = this.apply_function_to_point(pd, (double) index, pre_f_y);
        this.plotGraphJPanel1.AddPointToArrayPlot(pd, index, x, y, (double) index, pre_f_y);
        point_added_since_check_recalc_plots = true;
    }

    public void AddPointToPlot(PlotData pd, double x, double y, boolean connected, double pre_f_x, double pre_f_y) {
        this.plotGraphJPanel1.AddPointToPlot(pd, x, y, connected, pre_f_x, pre_f_y);
        point_added_since_check_recalc_plots = true;
    }

    public void SetOuterArea(double min_x, double max_x, double min_y, double max_y) {
        plotGraphJPanel1.SetOuterArea(min_x, max_x, min_y, max_y);
        UpdateScrollBarsTextFields();
        refresh();
    }

    public void SetInnerArea(double min_x, double max_x, double min_y, double max_y) {
        plotGraphJPanel1.SetInnerArea(min_x, max_x, min_y, max_y);
        UpdateScrollBarsTextFields();
        refresh();
    }

    public void setGraphFunctionNormal() {
        setGraphFunction(FUNC_CHOICE_NORMAL);
    }

    public void setGraphFunctionVS() {
        setGraphFunction(FUNC_CHOICE_VS);
    }

    public void setGraphFunctionXY() {
        setGraphFunction(FUNC_CHOICE_XY);
    }

    public void setGraphFunctionXZ() {
        setGraphFunction(FUNC_CHOICE_XZ);
    }

    public void setGraphFunctionYZ() {
        setGraphFunction(FUNC_CHOICE_YZ);
    }

    public void setGraphFunctionSingle() {
        setGraphFunction(FUNC_CHOICE_SINGLE);
    }

    public void setGraphFunctionSMOOTH() {
        setGraphFunction(FUNC_CHOICE_SMOOTH);
    }

    public void setGraphFunctionDEVIATION() {
        setGraphFunction(FUNC_CHOICE_DEVIATION);
    }

    public void setGraphFunctionDERIVATIVE() {
        setGraphFunction(FUNC_CHOICE_DERIVATIVE);
    }

    public void setGraphFunctionINTEGRAL() {
        setGraphFunction(FUNC_CHOICE_INTEGRAL);
    }

    public void setGraphFunctionDIFF() {
        setGraphFunction(FUNC_CHOICE_DIFF);
    }

    public void setGraphFunctionPPDIFF() {
        setGraphFunction(FUNC_CHOICE_PPDIFF);
    }

    public void setGraphFunctionPPDIFFMODPI() {
        setGraphFunction(FUNC_CHOICE_PPDIFFMODPI);
    }

    public void setGraphFunctionNEGATIVEX() {
        setGraphFunction(FUNC_CHOICE_NEGATIVEX);
    }

    public void setGraphFunction(String function_name) {
        for (int i = 0; i
                < jComboBoxFunc.getItemCount(); i++) {
            String s = jComboBoxFunc.getItemAt(i).toString();
            if (s.toUpperCase().startsWith(function_name.toUpperCase())) {
                jComboBoxFunc.setSelectedIndex(i);
                if (i == FUNC_CHOICE_XY) {
                    PlotLoader.set_plot_verses_line_number(true);
                    this.jToggleButtonSplit.setSelected(false);
                    this.plotGraphJPanel1.s_mode = false;
                    this.jTextFieldYMax.setEnabled(!plotGraphJPanel1.s_mode);
                    this.jTextFieldYMin.setEnabled(!plotGraphJPanel1.s_mode);
                    refresh();

                }

                return;
            }

        }
        System.err.println("setGraphFunction: " + function_name + " was not recognized.");
        System.err.println("Possible Values:");
        for (int i = 0; i
                < jComboBoxFunc.getItemCount(); i++) {
            String s = jComboBoxFunc.getItemAt(i).toString();
            System.err.println(s);
        }

    }

    public void setGraphFunction(int i) {
        jComboBoxFunc.setSelectedIndex(i);
        if (i == FUNC_CHOICE_XY) {
            this.jToggleButtonSplit.setSelected(false);
            this.plotGraphJPanel1.s_mode = false;
            this.jTextFieldYMax.setEnabled(!plotGraphJPanel1.s_mode);
            this.jTextFieldYMin.setEnabled(!plotGraphJPanel1.s_mode);
            refresh();
        }
    }

    public int getGraphFunction() {
        return jComboBoxFunc.getSelectedIndex();
    }
    private boolean recalculating_plots = false;
    private PlotData plot_data_to_compare = null;
    private int last_function_selected = -1;
    private int function_selected = -1;
    private int function_argument = -1;

    public int getFuncArg() {
        try {
            SpinnerNumberModel snm = (SpinnerNumberModel) jSpinnerFuncArg.getModel();
            function_argument
                    = snm.getNumber().intValue();
        } catch (Exception e) {
            e.printStackTrace();
        }

        return function_argument;
    }

    public void setFuncArg(String s) {
        if (Character.isDigit(s.charAt(0))) {
            int i = Integer.valueOf(s).intValue();
            setFuncArg(i);
        }

        int count = 0;
        for (PlotData pd : this.plotGraphJPanel1.plots.values()) {
            count++;
            if (null == pd) {
                break;
            }
            if (pd.short_name.toUpperCase().indexOf(s.toUpperCase()) >= 0) {
                if (PlotterCommon.debug_on) {
                    PlotterCommon.DebugPrint("setFuncArg(" + s + ") calling setFuncArg(" + pd.num + ") :pd.short_name=" + pd.short_name);
                }
                setFuncArg(pd.num);
                return;
            }
        }
        System.err.println("setFuncArg(" + s + ") plot not recognized.");
        System.err.println("Possible values :");
        for (PlotData pd : this.plotGraphJPanel1.plots.values()) {
            if (null == pd) {
                break;
            }
            System.err.println(pd.short_name);
        }
    }

    public void setFuncArg(int _func_arg) {
        try {
            SpinnerNumberModel snm = (SpinnerNumberModel) jSpinnerFuncArg.getModel();
            snm.setValue(Integer.valueOf(_func_arg));
            function_argument
                    = snm.getNumber().intValue();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    private boolean checkXyzNameMatch(PlotData pd, String pd_name_upcase, String s1, String s2) {
        if (pd_name_upcase.endsWith(s1)) {
            String y_name = pd_name_upcase.substring(0, pd_name_upcase.length() - s1.length()) + s2;
            y_name = y_name.toUpperCase();
            for (PlotData pd_for_find_y : this.plotGraphJPanel1.plots.values()) {
                if (pd_for_find_y.name.toUpperCase().compareTo(y_name) == 0) {
                    pd.y_plot_data = pd_for_find_y;
                    System.out.println(pd.name + " matches " + pd_for_find_y.name);
                    break;
                }
            }
            if (null == pd.y_plot_data) {
                PlotterCommon.ErrorPrint("No plot " + s1 + " vs. " + s2 + " match for " + pd_name_upcase + " -- need " + y_name);
            }
            return true;
        } else if (pd_name_upcase.endsWith(s1 + "[]")) {
            String y_name = pd_name_upcase.substring(0, pd_name_upcase.length() - s1.length() - 2) + s2 + "[]";
            y_name = y_name.toUpperCase();
            for (PlotData pd_for_find_y : this.plotGraphJPanel1.plots.values()) {
                if (pd_for_find_y.name.toUpperCase().compareTo(y_name) == 0) {
                    pd.y_plot_data = pd_for_find_y;
                    break;
                }
            }
            if (null == pd.y_plot_data) {
                PlotterCommon.ErrorPrint("No plot " + s1 + " vs. " + s2 + " match for " + pd_name_upcase + " -- need " + y_name);
            }
            return true;
        }

        return false;
    }

    private void Find_XY_YPlotData(PlotData pd) {
        String pd_name_upcase = pd.name.toUpperCase();
        if (!checkXyzNameMatch(pd, pd_name_upcase, "Y", "X")
            && !checkXyzNameMatch(pd, pd_name_upcase, "Y[MM]", "X[MM]")
                && !checkXyzNameMatch(pd, pd_name_upcase, "NORTH", "EAST")
                && !checkXyzNameMatch(pd, pd_name_upcase, "N", "E")) {
            if (!pd_name_upcase.endsWith("X")
                    && !pd_name_upcase.endsWith("X[]")
                    && !pd_name_upcase.endsWith("X[MM]")
                    && !pd_name_upcase.endsWith("N")
                    && !pd_name_upcase.endsWith("N[]")
                    && !pd_name_upcase.endsWith("NORTH")
                    && !pd_name_upcase.endsWith("NORTH[]")
                    && !pd_name_upcase.endsWith("Z")
                    && !pd_name_upcase.endsWith("Z[]")
                    && !pd_name_upcase.endsWith("ALTITUDE")
                    && !pd_name_upcase.endsWith("ALTITUDE[]")) {
                PlotterCommon.ErrorPrint("No plot X vs. Y match for " + pd.name);
            }
        }
    }

    private void Find_XZ_YPlotData(PlotData pd) {
        String pd_name_upcase = pd.name.toUpperCase();
        if (!checkXyzNameMatch(pd, pd_name_upcase, "Z", "X")
                && !checkXyzNameMatch(pd, pd_name_upcase, "ALTITUDE", "EAST")) {
            if (!pd_name_upcase.endsWith("X")
                    && !pd_name_upcase.endsWith("X[]")
                    && !pd_name_upcase.endsWith("N")
                    && !pd_name_upcase.endsWith("N[]")
                    && !pd_name_upcase.endsWith("NORTH")
                    && !pd_name_upcase.endsWith("NORTH[]")
                    && !pd_name_upcase.endsWith("Y")
                    && !pd_name_upcase.endsWith("Y[]")
                    && !pd_name_upcase.endsWith("E")
                    && !pd_name_upcase.endsWith("E[]")
                    && !pd_name_upcase.endsWith("EAST")
                    && !pd_name_upcase.endsWith("EAST[]")) {
                PlotterCommon.ErrorPrint("No plot X vs. Z match for " + pd.name);
            }
        }
    }

    private void Find_YZ_YPlotData(PlotData pd) {
        String pd_name_upcase = pd.name.toUpperCase();
        if (!checkXyzNameMatch(pd, pd_name_upcase, "Z", "Y")
                && !checkXyzNameMatch(pd, pd_name_upcase, "ALTITUDE", "NORTH")) {
            if (!pd_name_upcase.endsWith("X")
                    && !pd_name_upcase.endsWith("X[]")
                    && !pd_name_upcase.endsWith("N")
                    && !pd_name_upcase.endsWith("N[]")
                    && !pd_name_upcase.endsWith("NORTH")
                    && !pd_name_upcase.endsWith("NORTH[]")
                    && !pd_name_upcase.endsWith("Y")
                    && !pd_name_upcase.endsWith("Y[]")
                    && !pd_name_upcase.endsWith("E")
                    && !pd_name_upcase.endsWith("E[]")
                    && !pd_name_upcase.endsWith("EAST")
                    && !pd_name_upcase.endsWith("EAST[]")) {
                PlotterCommon.ErrorPrint("No plot X vs. Z match for " + pd.name);
            }

        }
    }

    private void FindYPlotData(PlotData pd) {
        try {
            if (null == pd) {
                return;
            }

            if (null != pd.y_plot_data) {
                pd.y_plot_data.is_y_plot = false;
            }

            pd.y_plot_data = null;
            if (function_selected != FUNC_CHOICE_XY
                    && function_selected != FUNC_CHOICE_XZ
                    && function_selected != FUNC_CHOICE_YZ) {
                return;
            }

            if (pd.name.toUpperCase().indexOf("_VEL") >= 0
                    || pd.name.toUpperCase().indexOf(".VEL") >= 0
                    || pd.name.toUpperCase().startsWith("VEL")) {
                return;
            }

            if (pd.name.toUpperCase().indexOf("_ACC") >= 0
                    || pd.name.toUpperCase().indexOf(".ACC") >= 0
                    || pd.name.toUpperCase().startsWith("ACC")) {
                return;
            }

            switch (function_selected) {
                case FUNC_CHOICE_XY:
                    Find_XY_YPlotData(pd);
                    break;

                case FUNC_CHOICE_XZ:
                    Find_XZ_YPlotData(pd);
                    break;

                case FUNC_CHOICE_YZ:
                    Find_YZ_YPlotData(pd);
                    break;

            }

            if (null != pd.y_plot_data) {
                pd.y_plot_data.is_y_plot = true;
                pd.is_y_plot = false;
                if (PlotterCommon.debug_on) {
                    PlotterCommon.DebugPrint("Plot pd " + pd.name + " has y_plot_data " + pd.y_plot_data.name);
                }
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    protected boolean apply_absolute_value = false;

    /**
     * Get the value of apply_absolute_value
     *
     * @return the value of apply_absolute_value
     */
    public boolean isApply_absolute_value() {
        return apply_absolute_value;
    }

    /**
     * Set the value of apply_absolute_value
     *
     * @param apply_absolute_value new value of apply_absolute_value
     */
    public void setApply_absolute_value(boolean apply_absolute_value) {
        if (this.jCheckBoxApplyAbsY != null
                && this.jCheckBoxApplyAbsY.isSelected() != apply_absolute_value) {
            this.jCheckBoxApplyAbsY.setSelected(apply_absolute_value);
        }
        this.apply_absolute_value = apply_absolute_value;
    }

    private double apply_function_to_point(PlotData pd, double x, double y) {
        try {

            double fy = y;
            pd.point_count++;
            if (pd.y_plot_data != null) {
                plot_data_to_compare = pd.y_plot_data;
            }

            if (function_selected == FUNC_CHOICE_SMOOTH) {
                int window = function_argument;
                //DebugPrint("funcArgScrollbar.getValue() = "+funcArgScrollbar.getValue());
                if (window > pd.get_num_points()) {
                    window = pd.get_num_points();
                }

                if (window > pd.point_count) {
                    window = pd.point_count;
                }

                if (window > 0) {
                    pd.mean = pd.mean + (y - pd.mean) / window;
                } else {
                    pd.mean = y;
                }

                fy = pd.mean;
            } else if (function_selected == FUNC_CHOICE_DEVIATION) {
                int window = function_argument;
                double s = (y - pd.mean) * (y - pd.mean);
                //DebugPrint("funcArgScrollbar.getValue() = "+funcArgScrollbar.getValue());
                if (window > pd.get_num_points()) {
                    window = pd.get_num_points();
                }

                if (window > pd.point_count) {
                    window = pd.point_count;
                }

                if (window > 0) {
                    pd.mean = pd.mean + (y - pd.mean) / window;
                } else {
                    pd.mean = y;
                }

                if (window > 1) {
                    pd.stddev = (s + pd.stddev * function_argument) / (function_argument + 1);
                } else {
                    pd.stddev = 0;
                }

                fy = pd.stddev;
            } else if (function_selected == FUNC_CHOICE_DERIVATIVE) {
                double fy1 = y;
                if (Math.abs(x - pd.last_x) < 1E-9 || pd.point_count < 2) {
                    fy1 = 0;
                } else {
                    fy1 = (y - pd.last_y) / (x - pd.last_x);
                }

                int window = function_argument;
                //DebugPrint("funcArgScrollbar.getValue() = "+funcArgScrollbar.getValue());
                if (window > pd.get_num_points()) {
                    window = pd.get_num_points();
                }

                if (window > pd.point_count) {
                    window = pd.point_count;
                }

                if (window > 0) {
                    pd.derivmean = pd.derivmean + (fy1 - pd.derivmean) / window;
                } else {
                    pd.derivmean = fy1;
                }

                fy = pd.derivmean;
                pd.last_x = x;
                pd.last_y = y;
            } else if (function_selected == FUNC_CHOICE_INTEGRAL) {
                if (pd.get_num_points() > 1) {
                    pd.integral += y;
                } else {
                    pd.integral = y;
                }

                fy = pd.integral;
            } else if (function_selected == FUNC_CHOICE_DIFF) {
                double fy1 = y;
                if (pd.point_count < 2) {
                    fy1 = 0;
                } else {
                    fy1 = (y - pd.last_y);
                }

                int window = function_argument;
                //DebugPrint("funcArgScrollbar.getValue() = "+funcArgScrollbar.getValue());
                if (window > pd.get_num_points()) {
                    window = pd.get_num_points();
                }

                if (window > pd.point_count) {
                    window = pd.point_count;
                }

                if (window > 0) {
                    pd.derivmean = pd.derivmean + (fy1 - pd.derivmean) / window;
                } else {
                    pd.derivmean = fy1;
                }

                fy = pd.derivmean;
                pd.last_x = x;
                pd.last_y = y;
            } else if (function_selected == FUNC_CHOICE_PPDIFF
                    && null != plot_data_to_compare
                    && plot_data_to_compare.v_size() > 2) {
                pd.last_compare_index = pd.last_compare_index % (plot_data_to_compare.v_size() - 1);
                PlotPoint p1 = plot_data_to_compare.getPlotPointAt(pd.last_compare_index);
                PlotPoint p2 = plot_data_to_compare.getPlotPointAt(pd.last_compare_index + 1);
                int k = 1;
                while (Math.abs(p2.pre_f_x - p1.pre_f_x) < 1e-6 * Math.abs(p1.pre_f_x + p2.pre_f_x)
                        && k < plot_data_to_compare.v_size() - 1 - pd.last_compare_index) {
                    k++;
                    p2 = plot_data_to_compare.getPlotPointAt(pd.last_compare_index + k);
                }
                int j = 0;
                int v_index = pd.last_compare_index;
                if (p1.pre_f_x <= p2.pre_f_x && x > p1.pre_f_x) {
                    j = 0;
                    for (; x > p1.pre_f_x && j < plot_data_to_compare.v_size(); j++) {
                        v_index = (j + pd.last_compare_index) % (plot_data_to_compare.v_size());
                        p1 = plot_data_to_compare.getPlotPointAt(v_index);
                    }
                } else if (p1.pre_f_x > p2.pre_f_x && x < p1.pre_f_x) {
                    j = 0;
                    for (; x < p1.pre_f_x && j < plot_data_to_compare.v_size(); j++) {
                        v_index = (j + pd.last_compare_index) % (plot_data_to_compare.v_size());
                        p1 = plot_data_to_compare.getPlotPointAt(v_index);
                    }
                } else if (p1.pre_f_x <= p2.pre_f_x && x < p1.pre_f_x) {
                    j = plot_data_to_compare.v_size() - 1;
                    for (; x < p1.pre_f_x && j > 0; j--) {
                        v_index = (j + pd.last_compare_index) % (plot_data_to_compare.v_size());
                        p1 = plot_data_to_compare.getPlotPointAt(v_index);
                    }
                } else if (p1.pre_f_x > p2.pre_f_x && x > p1.pre_f_x) {
                    j = plot_data_to_compare.v_size() - 1;
                    for (; x > p1.pre_f_x && j > 0; j--) {
                        v_index = (j + pd.last_compare_index) % (plot_data_to_compare.v_size());
                        p1 = plot_data_to_compare.getPlotPointAt(v_index);
                    }
                }
                PlotPoint p1n = p1;
                if (v_index < plot_data_to_compare.v_size() - 1) {
                    p1n = plot_data_to_compare.getPlotPointAt(v_index + 1);
                }
                PlotPoint p1l = p1;
                if (v_index > 0) {
                    p1l = plot_data_to_compare.getPlotPointAt(v_index - 1);
                }
                fy = y - p1.pre_f_y;
                if (Math.abs(x - p1l.pre_f_x) < Math.abs(x - p1n.pre_f_x)) {
                    if (p1.pre_f_x != p1l.pre_f_x) {
                        double efy = p1.pre_f_y + (p1l.pre_f_y - p1.pre_f_y) * (x - p1.pre_f_x) / (p1l.pre_f_x - p1.pre_f_x);
                        fy = y - efy;
                    }
                } else {
                    if (p1.pre_f_x != p1n.pre_f_x) {
                        double efy = p1.pre_f_y + (p1n.pre_f_y - p1.pre_f_y) * (x - p1.pre_f_x) / (p1n.pre_f_x - p1.pre_f_x);
                        fy = y - efy;
                    }
                }

                pd.last_compare_index = v_index;
            } else if (function_selected == FUNC_CHOICE_PPDIFFMODPI
                    && null != plot_data_to_compare
                    && plot_data_to_compare.v_size() > 2) {
                pd.last_compare_index = pd.last_compare_index % (plot_data_to_compare.v_size() - 1);
                PlotPoint p1 = plot_data_to_compare.getPlotPointAt(pd.last_compare_index);
                PlotPoint p2 = plot_data_to_compare.getPlotPointAt(pd.last_compare_index + 1);
                int j = 0;
                int v_index = pd.last_compare_index;
                if (p1.pre_f_x < p2.pre_f_x && x > p1.pre_f_x) {
                    j = 0;
                    for (; x
                            > p1.pre_f_x && j < plot_data_to_compare.v_size(); j++) {
                        v_index = (j + pd.last_compare_index) % (plot_data_to_compare.v_size());
                        p1
                                = plot_data_to_compare.getPlotPointAt(v_index);
                    }

                } else if (p1.pre_f_x > p2.pre_f_x && x < p1.pre_f_x) {
                    j = 0;
                    for (; x
                            < p1.pre_f_x && j < plot_data_to_compare.v_size(); j++) {
                        v_index = (j + pd.last_compare_index) % (plot_data_to_compare.v_size());
                        p1
                                = plot_data_to_compare.getPlotPointAt(v_index);
                    }

                } else if (p1.pre_f_x < p2.pre_f_x && x < p1.pre_f_x) {
                    j = plot_data_to_compare.v_size() - 1;
                    for (; x
                            < p1.pre_f_x && j > 0; j--) {
                        v_index = (j + pd.last_compare_index) % (plot_data_to_compare.v_size());
                        p1
                                = plot_data_to_compare.getPlotPointAt(v_index);
                    }

                } else if (p1.pre_f_x > p2.pre_f_x && x > p1.pre_f_x) {
                    j = plot_data_to_compare.v_size() - 1;
                    for (; x
                            > p1.pre_f_x && j > 0; j--) {
                        v_index = (j + pd.last_compare_index) % (plot_data_to_compare.v_size());
                        p1
                                = plot_data_to_compare.getPlotPointAt(v_index);
                    }

                }
                fy = y - p1.pre_f_y;
                if (fy > Math.PI) {
                    fy -= 2 * Math.PI * Math.floor((fy + Math.PI) / (2 * Math.PI));
                } else if (fy < -Math.PI) {
                    fy += 2 * Math.PI * Math.floor((Math.abs(fy) + Math.PI) / (2 * Math.PI));
                }

                pd.last_compare_index = v_index;
            } else if (function_selected == FUNC_CHOICE_NEGATIVEX) {
                if (this.apply_absolute_value) {
                    return Math.abs(fy);
                }
                return fy;
            }
            if (this.apply_absolute_value) {
                return Math.abs(fy);
            }
            return fy;
        } catch (Exception e) {
            e.printStackTrace();
        }
        if (this.apply_absolute_value) {
            return Math.abs(y);
        }
        return y;
    }

    private double apply_function_to_point_x(PlotData pd, double x, double y) {
        try {
            double fx = x;
            if (pd.y_plot_data != null) {
                plot_data_to_compare = pd.y_plot_data;
            }

            final boolean func_xyz_type_selected
                    = function_selected == FUNC_CHOICE_XY
                    || function_selected == FUNC_CHOICE_XZ
                    || function_selected == FUNC_CHOICE_YZ;

            if (func_xyz_type_selected
                    && pd.is_y_plot) {
                fx = x;
                return x;
            }
            if (pd == plot_data_to_compare) {
                fx = x;
                return x;
            }
            if ((function_selected == FUNC_CHOICE_VS
                    || func_xyz_type_selected)
                    && null != plot_data_to_compare
                    && plot_data_to_compare.v_size() > 2) {
                PlotPoint p1 = null;
                PlotPoint p2 = null;
                int j = 0;
                int v_size_0 = plot_data_to_compare.v_size();
                pd.last_compare_index
                        = pd.last_compare_index % (plot_data_to_compare.v_size() - 1);
                int v_index = pd.last_compare_index;
                double min_diff = 0;
                int min_diff_index = pd.last_compare_index;
                PlotPoint min_diff_point = null;
                if (pd.array_type) {
                    min_diff_index = v_index = (int) x;
                    if (v_index >= v_size_0 - 1) {
                        min_diff_index = v_index = v_size_0 - 1;
                    }
                    min_diff_point = plot_data_to_compare.getPlotPointAt(v_index);
                } else {
                    p1 = plot_data_to_compare.getPlotPointAt(pd.last_compare_index);
                    p2 = plot_data_to_compare.getPlotPointAt(pd.last_compare_index + 1);
                    min_diff = Math.abs(p1.pre_f_x - x);
                    min_diff_point = p1;
                    if (p1.pre_f_x <= p2.pre_f_x && x > p1.pre_f_x) {
                        j = 0;
                        for (; x > p1.pre_f_x && j < plot_data_to_compare.v_size(); j++) {
                            v_index = (j + pd.last_compare_index) % (plot_data_to_compare.v_size());
                            p1 = plot_data_to_compare.getPlotPointAt(v_index);
                            double diff = Math.abs(p1.pre_f_x - x);
                            if (diff < min_diff) {
                                min_diff = diff;
                                min_diff_index = v_index;
                                min_diff_point = p1;
                            }
                        }
                    } else if (p1.pre_f_x >= p2.pre_f_x && x < p1.pre_f_x) {
                        j = 0;
                        for (; x < p1.pre_f_x && j < plot_data_to_compare.v_size(); j++) {
                            v_index = (j + pd.last_compare_index) % (plot_data_to_compare.v_size());
                            p1 = plot_data_to_compare.getPlotPointAt(v_index);
                            double diff = Math.abs(p1.pre_f_x - x);
                            if (diff < min_diff) {
                                min_diff = diff;
                                min_diff_index = v_index;
                                min_diff_point = p1;
                            }
                        }
                    } else if (p1.pre_f_x <= p2.pre_f_x && x < p1.pre_f_x) {
                        j = plot_data_to_compare.v_size() - 1;
                        for (; x < p1.pre_f_x && j > 0; j--) {
                            v_index = (j + pd.last_compare_index) % (plot_data_to_compare.v_size());
                            p1 = plot_data_to_compare.getPlotPointAt(v_index);
                            double diff = Math.abs(p1.pre_f_x - x);
                            if (diff < min_diff) {
                                min_diff = diff;
                                min_diff_index = v_index;
                                min_diff_point = p1;
                            }
                        }
                    } else if (p1.pre_f_x >= p2.pre_f_x && x > p1.pre_f_x) {
                        j = plot_data_to_compare.v_size() - 1;
                        for (; x > p1.pre_f_x && j > 0; j--) {
                            v_index = (j + pd.last_compare_index) % (plot_data_to_compare.v_size());
                            p1 = plot_data_to_compare.getPlotPointAt(v_index);
                            double diff = Math.abs(p1.pre_f_x - x);
                            if (diff < min_diff) {
                                min_diff = diff;
                                min_diff_index = v_index;
                                min_diff_point = p1;
                            }

                        }
                    }
                }

                fx = min_diff_point.pre_f_y;
                pd.last_compare_index = min_diff_index;
            } else if (function_selected == FUNC_CHOICE_NEGATIVEX) {
                return -fx;
            }

            return fx;
        } catch (Exception e) {
            e.printStackTrace();
        }

        return x;
    }

    public void CheckRecalcPlots() {
        if (get_paused()) {
            return;
        }

        if (plotGraphJPanel1.get_array_mode()
                && function_selected != FUNC_CHOICE_NORMAL
                && point_added_since_check_recalc_plots) {
            RecalculatePlots();
        }

        point_added_since_check_recalc_plots = false;
    }
    private boolean func_choice_single_cleared_s_mode = false;

    String[] getPlotNames() {
        String names[] = new String[this.plotGraphJPanel1.plots.size()];
        Iterator<PlotData> pd_it = this.plotGraphJPanel1.plots.values().iterator();
        for (int i = 0; i < names.length && pd_it.hasNext(); i++) {
            PlotData pd = pd_it.next();
            if (null != pd) {
                names[i] = pd.name;
            }
        }
        return names;
    }

    PlotData getPlotByName(String name) {
        for (PlotData pd : this.plotGraphJPanel1.plots.values()) {
            if (null == pd) {
                break;
            }

            if (pd.name.compareTo(name) == 0) {
                return pd;
            }
        }
        return null;
    }

    PlotData getFunctionArgPlot() {
        for (PlotData pd : this.plotGraphJPanel1.plots.values()) {
            if (null == pd) {
                break;
            }

            if (pd.num == function_argument) {
                return pd;
            }
        }
        return null;
    }

    private void RecalculatePlots() {
        try {
            point_added_since_check_recalc_plots = false;
            function_selected
                    = jComboBoxFunc.getSelectedIndex();
            getFuncArg();

            if (null == this.plotGraphJPanel1.keyVector || clearing_plots) {
                return;
            }

            if (function_selected == FUNC_CHOICE_SINGLE) {
                int count = 0;
                for (PlotData pd : this.plotGraphJPanel1.plots.values()) {
                    count++;
                    if (null == pd) {
                        break;
                    }

                    boolean this_plot_selected = (pd.num == function_argument);
                    pd.setShowAll(total_plotters, this_plot_selected);
                    pd.no_key = !this_plot_selected;
                }

                if (last_function_selected != FUNC_CHOICE_SINGLE) {
                    func_choice_single_cleared_s_mode = plotGraphJPanel1.s_mode;
                    if (func_choice_single_cleared_s_mode) {
                        plotGraphJPanel1.s_mode = false;
                        this.jTextFieldYMax.setEnabled(true);
                        this.jTextFieldYMin.setEnabled(true);
                        this.jToggleButtonSplit.setSelected(false);
                        this.jToggleButtonSplit.setEnabled(false);
                    }

                }
            } else if (last_function_selected == FUNC_CHOICE_SINGLE) {
                int count = 0;
                for (PlotData pd : this.plotGraphJPanel1.plots.values()) {
                    count++;
                    if (null == pd) {
                        break;
                    }
                    pd.setShowAll(total_plotters, true);

                }
                if (func_choice_single_cleared_s_mode) {
                    plotGraphJPanel1.s_mode = true;
                    this.jTextFieldYMax.setEnabled(false);
                    this.jTextFieldYMin.setEnabled(false);
                    this.jToggleButtonSplit.setSelected(true);
                    this.jToggleButtonSplit.setEnabled(true);
                    func_choice_single_cleared_s_mode
                            = false;
                }

            }
            recalculating_plots = true;
            synchronized (this.plotGraphJPanel1.SyncObject) {
                if (null != plot_data_to_compare) {
                    plot_data_to_compare.setShowAll(total_plotters, true);
                }
                plot_data_to_compare = null;
                this.plotGraphJPanel1.extra_sh_str = null;
                this.plotGraphJPanel1.short_extra_sh_str = null;
                if (last_function_selected != function_selected) {
                    if (function_selected == FUNC_CHOICE_NORMAL
                            || function_selected == FUNC_CHOICE_SMOOTH) {
                        for (PlotData pd : this.plotGraphJPanel1.plots.values()) {
                            pd.setShow(plotGraphJPanel1.plotter_num, true);
                            pd.no_key = false;
                        }
                    }
                }
                if (function_selected == FUNC_CHOICE_VS
                        || function_selected == FUNC_CHOICE_PPDIFF
                        || function_selected == FUNC_CHOICE_PPDIFFMODPI) {
                    int count = 0;
                    for (PlotData pd : this.plotGraphJPanel1.plots.values()) {
                        count++;
                        if (null == pd) {
                            break;
                        }

                        if (pd.num == function_argument) {
                            plot_data_to_compare = pd;
                            pd.setShowAll(total_plotters, false);
                            plot_data_to_compare.no_key = true;
                            if (function_selected == FUNC_CHOICE_PPDIFF) {
                                this.plotGraphJPanel1.extra_sh_str = " - " + plot_data_to_compare.name;
                                this.plotGraphJPanel1.short_extra_sh_str = " - " + plot_data_to_compare.short_name;
                            } else if (function_selected == FUNC_CHOICE_PPDIFFMODPI) {
                                this.plotGraphJPanel1.extra_sh_str = " - " + plot_data_to_compare.name + " % 2PI";
                                this.plotGraphJPanel1.short_extra_sh_str = " - " + plot_data_to_compare.short_name + " % 2PI";
                            } else if (function_selected == FUNC_CHOICE_VS) {
                                this.plotGraphJPanel1.extra_sh_str = " vs. " + plot_data_to_compare.name;
                                this.plotGraphJPanel1.short_extra_sh_str = " vs. " + plot_data_to_compare.short_name;
                            } else {
                                this.plotGraphJPanel1.extra_sh_str = "";
                                this.plotGraphJPanel1.short_extra_sh_str = "";
                            }
                            break;
                        }
                    }
                }
                final boolean func_xyz_type_selected
                        = function_selected == FUNC_CHOICE_XY
                        || function_selected == FUNC_CHOICE_XZ
                        || function_selected == FUNC_CHOICE_YZ;

                List<PlotData> plot_values = new ArrayList<PlotData>(this.plotGraphJPanel1.plots.values());
                for (int plot_values_i = 0; plot_values_i < plot_values.size(); plot_values_i++) {
                    try {
                        PlotData pd = plot_values.get(plot_values_i);
                        if (pd == null) {
                            continue;
                        }

                        pd.integral = 0;
                        pd.stddev = 0;
                        pd.derivmean = 0;
                        pd.point_count = 0;
                        pd.mean = 0.0;
                        pd.y_plot_data = null;
                        pd.add_to_key = "";
                        pd.add_to_short_key = "";

                        if (func_xyz_type_selected) {
                            plot_data_to_compare = null;
                            FindYPlotData(pd);
                            if (null == pd.y_plot_data) {
                                pd.setShowAll(total_plotters, false);
                                pd.no_key = true;
                                continue;

                            }

                            if (PlotterCommon.debug_on) {
                                PlotterCommon.DebugPrint("pd.name=" + pd.name + ", pd.y_plot_data.name=" + pd.y_plot_data.name);
                            }

                            this.plotGraphJPanel1.extra_sh_str = " vs. " + pd.y_plot_data.short_name;
                            this.plotGraphJPanel1.short_extra_sh_str = " vs. " + pd.y_plot_data.short_name;
                        } else {
                            pd.y_plot_data = null;
                        }

                        if (func_xyz_type_selected) {
                            plot_data_to_compare = pd.y_plot_data;
                            if (null == pd.y_plot_data || pd.v_size() < 3) {
                                pd.setShowAll(total_plotters, false);
                                pd.no_key = true;
                                continue;
                            }

                            if (PlotterCommon.debug_on) {
                                PlotterCommon.DebugPrint("pd.name=" + pd.name + ", pd.y_plot_data.name=" + pd.y_plot_data.name);
                            }
//plotGraphJPanel1.extra_sh_str = " vs. " + pd.y_plot_data.short_name;
//plotGraphJPanel1.short_extra_sh_str = " vs. "+pd.y_plot_data.short_name;

                            this.plotGraphJPanel1.extra_sh_str = "";
                            this.plotGraphJPanel1.short_extra_sh_str = "";
                            pd.add_to_key = " vs. " + pd.y_plot_data.short_name;
                            pd.add_to_short_key = " vs. " + pd.y_plot_data.short_name;
                        }

                        synchronized (pd) {
                            PlotData current_pd = pd;
                            for (int i = 0; i < current_pd.current_size && i < current_pd.v_size(); i++) {
                                int v_index = (i + current_pd.v_offset) % current_pd.current_size;
                                if (v_index >= current_pd.v_size()) {
                                    continue;
                                }

                                PlotPoint p = current_pd.getPlotPointAt(v_index);
                                p.orig_y = apply_function_to_point(pd, p.pre_f_x, p.pre_f_y);
                                p.orig_x = apply_function_to_point_x(pd, p.pre_f_x, p.pre_f_y);
                                p.y = (int) p.orig_y;
                                p.x = (int) p.orig_x;
                                current_pd.setPlotPointAt(p, v_index);
                            }

                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }

                }
                for (PlotData plot_data : this.plotGraphJPanel1.keyVector) {
                    try {
                        plot_data.integral = 0;
                        plot_data.stddev = 0;
                        plot_data.derivmean = 0;
                        plot_data.point_count = 0;
                        if (func_xyz_type_selected && null == plot_data.y_plot_data) {
                            continue;
                        }

                        synchronized (plot_data) {
                            for (int i = 0; i < plot_data.current_size; i++) {
                                int v_index = (i + plot_data.v_offset) % plot_data.current_size;
                                PlotPoint p = plot_data.getPlotPointAt(v_index);
                                p.orig_y = apply_function_to_point(plot_data, p.pre_f_x, p.pre_f_y);
                                p.orig_x = apply_function_to_point_x(plot_data, p.pre_f_x, p.pre_f_y);
                                //DebugPrint("p.orig_y = "+p.orig_y+" = apply_function_to_point("+pt+","+p.orig_x+","+p.pre_f_y+") : function_selected="+function_selected);
                                p.x = (int) p.orig_x;
                                p.y = (int) p.orig_y;
                                plot_data.setPlotPointAt(p, v_index);
                            }
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
                if (last_function_selected != function_selected
                        || function_selected == FUNC_CHOICE_VS
                        || func_xyz_type_selected
                        || function_selected == FUNC_CHOICE_PPDIFF
                        || function_selected == FUNC_CHOICE_PPDIFFMODPI
                        || function_selected == FUNC_CHOICE_NEGATIVEX) {
                    last_function_selected = function_selected;
                }
            }
            for (PlotData pd : this.plotGraphJPanel1.plots.values()) {
                pd.RecheckAllPoints();
            }

            point_added_since_check_recalc_plots = false;
        } catch (Exception e) {
            e.printStackTrace();
        }

        recalculating_plots = false;
    }

    public void ResetMinXToZero() {
        this.FitToGraph();
        this.plotGraphJPanel1.ResetMinXToZero();
        this.FitToGraph();
    }

    public boolean get_paused() {
        return paused || mouse_down;
    }
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButtonAxis;
    private javax.swing.JButton jButtonBackground;
    private javax.swing.JButton jButtonClear;
    private javax.swing.JButton jButtonClearDataOnly;
    private javax.swing.JButton jButtonCloseOptions;
    private javax.swing.JButton jButtonDataClose;
    private javax.swing.JButton jButtonDataSave;
    private javax.swing.JButton jButtonDeleteMarked;
    private javax.swing.JButton jButtonGrid;
    private javax.swing.JButton jButtonHideAll;
    private javax.swing.JButton jButtonOptions;
    private javax.swing.JButton jButtonShowAll;
    private javax.swing.JButton jButtonShowData;
    private javax.swing.JButton jButtonStats;
    private javax.swing.JButton jButtonZoomIn;
    private javax.swing.JButton jButtonZoomOut;
    private javax.swing.JCheckBox jCheckBoxApplyAbsY;
    private javax.swing.JCheckBox jCheckBoxK2;
    private javax.swing.JCheckBox jCheckBoxLabelPoints;
    private javax.swing.JCheckBox jCheckBoxReverseX;
    private javax.swing.JCheckBox jCheckBoxShowGrid;
    private javax.swing.JCheckBox jCheckBoxUseShortNames;
    private javax.swing.JComboBox jComboBoxFunc;
    private javax.swing.JFrame jFrameData;
    private javax.swing.JFrame jFrameOptions;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JLabel jLabelPlotGroup;
    private javax.swing.JLabel jLabelPlotsPerGroup;
    private javax.swing.JLabel jLabelXMaxMin;
    private javax.swing.JLabel jLabelXScale;
    private javax.swing.JLabel jLabelYScale;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollBar jScrollBarHorz;
    private javax.swing.JScrollBar jScrollBarVert;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPanelOptonsTable;
    private javax.swing.JSpinner jSpinnerFuncArg;
    private javax.swing.JSpinner jSpinnerPlotGroup;
    private javax.swing.JSpinner jSpinnerPlotsPerGroup;
    private javax.swing.JTable jTableData;
    private javax.swing.JTable jTableOptions;
    private javax.swing.JTextField jTextFieldXMax;
    private javax.swing.JTextField jTextFieldXMin;
    private javax.swing.JTextField jTextFieldXScale;
    private javax.swing.JTextField jTextFieldYMax;
    private javax.swing.JTextField jTextFieldYMin;
    private javax.swing.JTextField jTextFieldYScale;
    private javax.swing.JToggleButton jToggleButtonArrayMode;
    private javax.swing.JToggleButton jToggleButtonCMode;
    private javax.swing.JToggleButton jToggleButtonEqualizeAxis;
    private javax.swing.JToggleButton jToggleButtonKey;
    private javax.swing.JToggleButton jToggleButtonLockDisplay;
    private javax.swing.JToggleButton jToggleButtonPause;
    private javax.swing.JToggleButton jToggleButtonSplit;
    private javax.swing.JToolBar jToolBar1;
    private diagapplet.plotter.PlotGraphJPanel plotGraphJPanel1;
    // End of variables declaration//GEN-END:variables
    private diagapplet.plotter.PlotGraphJPanel fullScreenPlotGraphJPanel = null;
    private diagapplet.plotter.PlotGraphJPanel cur_pgjp = null;
    private String plot_order = null;

    public void set_point_size_limit(int _new_point_size_limit) {
        this.plotGraphJPanel1.set_point_size_limit(_new_point_size_limit);
    }

    public void setFieldSelectPattern(String _pattern) {
        if (null == this.pl) {
            this.pl = new PlotLoader(this.plotGraphJPanel1);
        }

        this.pl.setFieldSelectPattern(_pattern);
        this.pl.setFieldSelectPatternEnabled(true);
    }

    public static void setForcedLineFilterPattern(String _ForcedLineFilterPattern,
            boolean _use_forced_line_filter_pattern) {
        PlotLoader.setForcedLineFilterPattern(_ForcedLineFilterPattern,
                _use_forced_line_filter_pattern);
    }

    public static void lock_value_for_plot_versus_line_number(
            boolean _plot_verses_line_number,
            boolean _plot_verses_line_number_locked) {
        PlotLoader.lock_value_for_plot_versus_line_number(_plot_verses_line_number,
                _plot_verses_line_number_locked);
    }

    public void LoadObjectsList(String prefix, List l)
            throws NoSuchFieldException {
        //Clear();
        Object o = l.get(0);
        Class c = o.getClass();
        Field fa[] = c.getFields();
        for (Field f : fa) {
            double x = 1.0;
            if (!Double.class.isAssignableFrom(f.getType())
                    && !double.class.isAssignableFrom(f.getType())) {
                continue;
            }
            PlotData pd = new PlotData();
            pd.name = prefix + f.getName();
            this.plotGraphJPanel1.AddPlot(pd, pd.name);
            try {
                for (Object o2 : l) {
                    x += 1.0;
                    double y = f.getDouble(o2);
                    this.plotGraphJPanel1.AddPointToPlot(pd, x, y, true, x, y);
                }
                pd.setShowAll(total_plotters, false);
                pd.setShow(this.plotGraphJPanel1.plotter_num, true);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        RecalculatePlots();
        if (auto_fit_to_graph) {
            FitToGraph();
        }
        refresh();
    }

    public void LoadXYObjectsList(String prefix, String xname, List l)
            throws NoSuchFieldException {
        //Clear();
        Object o = l.get(0);
        Class c = o.getClass();
        Field fa[] = c.getFields();
        Field xfield = c.getField(xname);
        for (Field f : fa) {
            if (f.getName().compareTo(xname) == 0) {
                continue;
            }
            if (!Double.class.isAssignableFrom(f.getType())
                    && !double.class.isAssignableFrom(f.getType())) {
                continue;
            }
            PlotData pd = new PlotData();
            pd.name = prefix + f.getName();
            this.plotGraphJPanel1.AddPlot(pd, pd.name);
            try {
                for (Object o2 : l) {
                    double x = xfield.getDouble(o2);
                    double y = f.getDouble(o2);
                    this.plotGraphJPanel1.AddPointToPlot(pd, x, y, true, x, y);
                }
                pd.setShowAll(total_plotters, false);
                pd.setShow(this.plotGraphJPanel1.plotter_num, true);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        RecalculatePlots();
        if (auto_fit_to_graph) {
            FitToGraph();
        }
        refresh();
    }
}
