package diagapplet.plotter;

import java.awt.Dialog;
import java.awt.Frame;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import javax.swing.JDialog;
import javax.swing.table.DefaultTableModel;

/**
 *
 * @author Will Shackleford
 */
public class ParseOptionsJPanel extends javax.swing.JPanel {

    /**
     * Creates new form ParseOptionsJPanel
     */
    public ParseOptionsJPanel() {
        initComponents();
    }

    private List<String> lines;
    private String loaded_first_line_fa[] = null;

    private boolean headingsChanged(String fa[]) {
        if (null == loaded_first_line_fa) {
            return true;
        }
        if (loaded_first_line_fa.length != fa.length) {
            return true;
        }
        for (int i = 0; i < fa.length; i++) {
            if (loaded_first_line_fa[i].compareTo(fa[i]) != 0) {
                return true;
            }
        }
        return false;
    }

    String first_line;

    @SuppressWarnings("deprecation")
    public void updateTable() {
        String sep = this.jTextFieldFieldSeperator.getText();
        if (sep.length() == 1) {
            sep = "[" + sep + "]";
        }

        first_line = lines.get(0);
        String first_line_fa[] = first_line.split(sep);
        int field_indexes_to_show[] = null;
        if (headingsChanged(first_line_fa)) {
            this.jListFieldsToShow.setListData(first_line_fa);
            this.loaded_first_line_fa = first_line_fa;
            field_indexes_to_show = getSelectedIndexes(first_line);
            if (null == field_indexes_to_show || field_indexes_to_show.length < 1) {
                if (first_line_fa.length >= 3) {
                    field_indexes_to_show = new int[]{0, 1, 2};
                } else if (first_line_fa.length >= 2) {
                    field_indexes_to_show = new int[]{0, 1};
                } else if (first_line_fa.length >= 1) {
                    field_indexes_to_show = new int[]{0};
                }
            }
            this.jListFieldsToShow.setSelectedIndices(field_indexes_to_show);
        } else {
            field_indexes_to_show = this.jListFieldsToShow.getSelectedIndices();
        }
        List<String> headingsList = new LinkedList<String>();
        headingsList.add("Line_Number");
        List<String> l = new LinkedList<String>();
        Object selected_values[] = this.jListFieldsToShow.getSelectedValues();
        for(int i = 0; i < selected_values.length; i++) {
            l.add(selected_values[i].toString());
        }
        headingsList.addAll(l);
        String headings[] = headingsList.toArray(new String[headingsList.size()]);
        DefaultTableModel tm = new DefaultTableModel(headings, lines.size() - 1);
        boolean bad_index_warn_given = false;
        for (int i = 1; i < lines.size(); i++) {
            String line = lines.get(i);
            String fa[] = line.split(sep);
            tm.setValueAt(i, i - 1, 0);
            for (int j = 0; j < field_indexes_to_show.length; j++) {
                int fa_index = field_indexes_to_show[j];
                if(fa_index >= fa.length) {
                    if(!bad_index_warn_given) {
                        System.err.println("first_line="+first_line);
                        System.err.println("first_line_fa="+Arrays.toString(first_line_fa));
                        System.err.println("headings="+Arrays.toString(headings));
                        System.err.println("line="+line);
                        System.err.println("line number = "+i);
                        System.err.println("fa_index = "+ fa_index);
                        System.err.println("fa.length = "+ fa.length);
                        Thread.dumpStack();
                        bad_index_warn_given = true;
                    }
                    tm.setValueAt(Double.NaN,i-1,j+1);
                    continue;
                }
                tm.setValueAt(fa[fa_index], i - 1, j + 1);
            }
        }
        this.jTable1.setModel(tm);
        this.repaint();
    }

    public void setLines(List<String> _lines) {
        this.lines = _lines;
        if (null == lines || lines.size() < 1) {
            return;
        }
        String first_line = lines.get(0);
        if (this.jTextFieldFieldSeperator.getText().compareTo(",") == 0
                && first_line.indexOf(",") < 0 && first_line.indexOf(";") > 0) {
            this.jTextFieldFieldSeperator.setText(";");
        }
        this.updateTable();
    }
    JDialog dialog = null;

    static final File prevParseSelectsFile = new File(System.getProperty("user.home"),
            ".plotter_select_fields.txt");

    static private Map<String, ParseOptions> selectsMap = null;

    private static void saveSelectsMap() {
        if (null != selectsMap) {
            PrintStream ps =null;
            try {
                ps = new PrintStream(new FileOutputStream(prevParseSelectsFile));
                for (String line : selectsMap.keySet()) {
                    ParseOptions po = selectsMap.get(line);
                    if (null == po) {
                        continue;
                    }
                    ps.println("firstLine=" + line);
                    int ia[] = po.getFieldsToShow();
                    if (null != ia && ia.length > 0 && line.length() > 0) {
                        ps.println("fields_to_show=" + Arrays.toString(ia));
                    }
                    ps.println("plotVersusLineNumber=" + po.isPlotVersusLineNumber());
                    ps.println("fieldSeperator=" + po.getFieldSeperator());
                    if (Math.abs(po.getScale() - 1.0) > 1 - 6) {
                        ps.println("scale=" + po.getScale());
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                if(null != ps) {
                    try { ps.close(); } catch(Exception e){};
                }
            }
        }
    }

    private static void loadSelectsMap() {
        selectsMap = new HashMap<String, ParseOptions>();
        if (!prevParseSelectsFile.exists()) {
            return;
        }
        BufferedReader br = null;
        try {
            br = new BufferedReader(new FileReader(prevParseSelectsFile));
            String line = null;
            ParseOptions po = null;
            while (null != (line = br.readLine())) {
                line = line.trim();
                if (line.startsWith("firstLine=")) {
                    String fline = line.substring("firstLine=".length()).trim();
                    if (null != po) {
                        selectsMap.put(po.getFirstLine(), po);
                    }
                    po = new ParseOptions();
                    po.setFirstLine(fline);
                }
                if (po == null) {
                    po = new ParseOptions();
                }
                if (line.startsWith("fieldSeperator=")) {
                    String fsline = line.substring("fieldSeperator=".length()).trim();
                    po.setFieldSeperator(fsline);
                }
                if (line.startsWith("plotVersusLineNumber=")) {
                    String pvln_line = line.substring("plotVersusLineNumber=".length()).trim();
                    po.setPlotVersusLineNumber(Boolean.valueOf(pvln_line));
                }
                if (line.startsWith("fields_to_show=")) {
                    String array_line = line.substring("fields_to_show=".length()).trim();
                    if (null == array_line) {
                        break;
                    }
                    array_line = array_line.trim();
                    if (line.length() < 1
                            || array_line.length() < 1) {
                        continue;
                    }
                    if (array_line.startsWith("[")) {
                        array_line = array_line.substring(1);
                    } else {
                        System.err.println("expected line to start with [" + line + " in " + prevParseSelectsFile);
                        continue;
                    }
                    if (array_line.endsWith("]")) {
                        array_line = array_line.substring(0, array_line.length() - 1);
                    } else {
                        System.err.println("expected line to end with ]" + line + " in " + prevParseSelectsFile);
                        continue;
                    }
                    String fa[] = array_line.split("[\\[\\],;\t ]+");
                    int ia[] = new int[fa.length];
                    for (int i = 0; i < ia.length; i++) {
                        ia[i] = Integer.valueOf(fa[i]);
                    }
                    po.setFieldsToShow(ia);
                }
            }
            if (null != po) {
                selectsMap.put(po.getFirstLine(), po);
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if(null != br) {
                try { br.close(); } catch(Exception e) {}
            }
        }
    }

    static private ParseOptions getPrevParseOptions(String first_line) {
        if (null == selectsMap) {
            loadSelectsMap();
        }
        return selectsMap.get(first_line.trim());
    }

    static private int[] getSelectedIndexes(String first_line) {
        if (null == selectsMap) {
            loadSelectsMap();
        }
        ParseOptions po = selectsMap.get(first_line.trim());
        if (null != po) {
            return po.getFieldsToShow();
        }
        return null;
    }

    private ParseOptions parseOptions = null;

    public ParseOptions getParseOptions() {
        if (parseOptions == null) {
            parseOptions = new ParseOptions();
        }
        parseOptions.setFieldSeperator(this.jTextFieldFieldSeperator.getText());
        parseOptions.setFilterPattern(this.jTextFieldFilterPattern.getText());
        parseOptions.setPlotVersusLineNumber(this.jCheckBoxPlotVersusLineNumber.isSelected());
        parseOptions.setFieldsToShow(this.jListFieldsToShow.getSelectedIndices());
        if (this.jCheckBoxTab.isSelected()) {
            parseOptions.setFieldSeperator("\t");
        }
        try {
            double scale = java.lang.Double.valueOf(this.jTextFieldScale.getText());
            if (Math.abs(scale - 1) < 1e-9) {
                parseOptions.setScale(1);
            } else {
                parseOptions.setScale(scale);
            }
        } catch (Exception e) {

        }
        return parseOptions;
    }

    public void setParseOptions(ParseOptions opts_in) {
        this.jCheckBoxPlotVersusLineNumber.setSelected(opts_in.isPlotVersusLineNumber());
        this.jTextFieldFieldSeperator.setText(opts_in.getFieldSeperator());
        this.jTextFieldFilterPattern.setText(opts_in.getFilterPattern());
    }

    static public ParseOptions ask(ParseOptions opts_in, Frame parent, List<String> _lines) {

        JDialog dialog = new JDialog(parent, Dialog.ModalityType.APPLICATION_MODAL);
        ParseOptionsJPanel panel = new ParseOptionsJPanel();
        ParseOptions prev_options = getPrevParseOptions(_lines.get(0));
        if (null != opts_in) {
            panel.setParseOptions(opts_in);
        } else if (null != prev_options) {
            panel.setParseOptions(prev_options);
        }
        panel.dialog = dialog;
        panel.setLines(_lines);
        dialog.add(panel);
        dialog.pack();
        dialog.setVisible(true);
        if (panel.cancelled) {
            return null;
        }
        panel.updateSelectsMap();
        saveSelectsMap();
        ParseOptions opts_out = panel.getParseOptions();
        return opts_out;
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel1 = new javax.swing.JLabel();
        jTextFieldFilterPattern = new javax.swing.JTextField();
        jLabel2 = new javax.swing.JLabel();
        jTextFieldFieldSeperator = new javax.swing.JTextField();
        jButtonOk = new javax.swing.JButton();
        jCheckBoxPlotVersusLineNumber = new javax.swing.JCheckBox();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        jCheckBoxTab = new javax.swing.JCheckBox();
        jLabel3 = new javax.swing.JLabel();
        jTextFieldScale = new javax.swing.JTextField();
        jScrollPane2 = new javax.swing.JScrollPane();
        jListFieldsToShow = new javax.swing.JList();
        jLabel4 = new javax.swing.JLabel();
        jButtonCancel = new javax.swing.JButton();

        jLabel1.setText("Filter Pattern:");

        jLabel2.setText("Field Seperator: ");

        jTextFieldFieldSeperator.setText(",");
        jTextFieldFieldSeperator.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jTextFieldFieldSeperatorActionPerformed(evt);
            }
        });

        jButtonOk.setText("OK");
        jButtonOk.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonOkActionPerformed(evt);
            }
        });

        jCheckBoxPlotVersusLineNumber.setText("Plot Versus Line Number");

        jTable1.setModel(new javax.swing.table.DefaultTableModel(
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
        jScrollPane1.setViewportView(jTable1);

        jCheckBoxTab.setText("Tab");
        jCheckBoxTab.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxTabActionPerformed(evt);
            }
        });

        jLabel3.setText("Scale:");

        jTextFieldScale.setText("1.0");

        jListFieldsToShow.setModel(new javax.swing.AbstractListModel() {
            String[] strings = { "Item 1", "Item 2", "Item 3", "Item 4", "Item 5" };
            public int getSize() { return strings.length; }
            public Object getElementAt(int i) { return strings[i]; }
        });
        jListFieldsToShow.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
            public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
                jListFieldsToShowValueChanged(evt);
            }
        });
        jScrollPane2.setViewportView(jListFieldsToShow);

        jLabel4.setText("Fields To Show:");

        jButtonCancel.setText("Cancel");
        jButtonCancel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonCancelActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonCancel)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonOk))
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(layout.createSequentialGroup()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jLabel2)
                                    .addComponent(jLabel1))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jTextFieldFilterPattern)
                                    .addGroup(layout.createSequentialGroup()
                                        .addComponent(jTextFieldFieldSeperator, javax.swing.GroupLayout.PREFERRED_SIZE, 157, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                        .addComponent(jCheckBoxTab)
                                        .addGap(0, 416, Short.MAX_VALUE))))
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(jCheckBoxPlotVersusLineNumber)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabel3)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jTextFieldScale, javax.swing.GroupLayout.PREFERRED_SIZE, 76, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addGap(0, 0, Short.MAX_VALUE))
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 523, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addGroup(layout.createSequentialGroup()
                                        .addComponent(jLabel4)
                                        .addGap(0, 0, Short.MAX_VALUE))
                                    .addComponent(jScrollPane2))))
                        .addContainerGap())))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jCheckBoxPlotVersusLineNumber)
                            .addComponent(jLabel3)))
                    .addGroup(layout.createSequentialGroup()
                        .addGap(13, 13, 13)
                        .addComponent(jTextFieldScale, javax.swing.GroupLayout.PREFERRED_SIZE, 22, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldFilterPattern, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jCheckBoxTab)
                    .addComponent(jLabel4)
                    .addComponent(jTextFieldFieldSeperator, javax.swing.GroupLayout.PREFERRED_SIZE, 23, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 278, Short.MAX_VALUE)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 278, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonOk)
                    .addComponent(jButtonCancel))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    private void jTextFieldFieldSeperatorActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jTextFieldFieldSeperatorActionPerformed
        this.updateTable();
    }//GEN-LAST:event_jTextFieldFieldSeperatorActionPerformed

    public boolean cancelled = true;
    private void jButtonOkActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonOkActionPerformed
        cancelled = false;
        if (null != dialog) {
            dialog.setVisible(false);
        }

    }//GEN-LAST:event_jButtonOkActionPerformed

    private void jCheckBoxTabActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCheckBoxTabActionPerformed
        if (this.jCheckBoxTab.isSelected()) {
            this.jTextFieldFieldSeperator.setText("[\t]");
            this.jTextFieldFieldSeperator.setEditable(false);
        } else {
            this.jTextFieldFieldSeperator.setText(",");
            this.jTextFieldFieldSeperator.setEditable(true);
        }
        this.updateTable();
    }//GEN-LAST:event_jCheckBoxTabActionPerformed

    private void updateSelectsMap() {
        if (null == selectsMap) {
            selectsMap = new HashMap<String, ParseOptions>();
        }
        selectsMap.put(first_line, this.getParseOptions());
    }

    private void jListFieldsToShowValueChanged(javax.swing.event.ListSelectionEvent evt) {//GEN-FIRST:event_jListFieldsToShowValueChanged
        this.updateTable();
        this.updateSelectsMap();
    }//GEN-LAST:event_jListFieldsToShowValueChanged

    private void jButtonCancelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonCancelActionPerformed
        if (null != dialog) {
            dialog.setVisible(false);
        }
    }//GEN-LAST:event_jButtonCancelActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonCancel;
    private javax.swing.JButton jButtonOk;
    private javax.swing.JCheckBox jCheckBoxPlotVersusLineNumber;
    private javax.swing.JCheckBox jCheckBoxTab;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JList jListFieldsToShow;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JTable jTable1;
    private javax.swing.JTextField jTextFieldFieldSeperator;
    private javax.swing.JTextField jTextFieldFilterPattern;
    private javax.swing.JTextField jTextFieldScale;
    // End of variables declaration//GEN-END:variables
}
