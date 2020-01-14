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
 * HierarchyLoad_NB_UI.java
 *
 * Created on December 9, 2006, 6:01 PM
 */
package diagapplet;

import diagapplet.CodeGen.CodeGenCommonInterface;
import java.util.List;
import javax.swing.SwingWorker;

/**
 *
 * @author  shackle
 */
class HierarchyLoad_NB_UI extends javax.swing.JFrame {

    /**
     *   Unique id for this class, Exception implements Serializable interface see documentation
     * for serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613941L;

    /** Creates new form HierarchyLoad_NB_UI */
    public HierarchyLoad_NB_UI() {
        this.setAlwaysOnTop(true);
        initComponents();
        this.setAlwaysOnTop(true);
    }

    private class HierarchyLoadUpdateInfo {

        @Override
        public String toString()
        {
            return super.toString()+",internal="+internal+", name="+name+", part="+part+", total="+total+" ";
        }
        boolean internal;
        String name;
        int part;
        int total;
    }
    
    private HierarchyLoadUpdateInfo last_main_HierarchyLoadUpdateInfo=null;
    private HierarchyLoadUpdateInfo last_internal_HierarchyLoadUpdateInfo=null;
    
    @Override
    public String toString()
    {
        String s = super.toString()+"\n\tisVisible()="+isVisible()+"\n";
        s += "\tlast_main_last_main_HierarchyLoadUpdateInfo="+last_main_HierarchyLoadUpdateInfo+"\n";
        s += "\tlast_internal_HierarchyLoadUpdateInfo="+last_internal_HierarchyLoadUpdateInfo+"\n";
        s += "\tneed_main_update="+need_main_update+"\n";
        s += "\tneed_internal_update=" + need_internal_update + "\n";
        s += "\tswing_worker=" + swing_worker + "\n";
        s += "\tsw_bg_return_count="+sw_bg_return_count+"\n";
        s += "\tsw_done_count="+sw_done_count+"\n";
        s += "\tpublish_in_count="+publish_in_count+"\n";
        s += "\tprocess_call_count="+process_call_count+"\n";
        s += "\tpublish_out_count="+publish_out_count+"\n";
        if(null != swing_worker)
        {
            s += "\tswing_worker.getState()=" + swing_worker.getState() + "\n";
        }
        return s;
    }
    
    diagapplet.CodeGen.LoadHierarchyUpdateInterface lhui =null;
    private boolean need_main_update = true;
    private boolean need_internal_update = true;
    private SwingWorker swing_worker=null;

    private void DisplayHierarchyLoadUpdateInfo(final HierarchyLoadUpdateInfo hlui) {
        if (!hlui.internal) {
            jTextFieldMainFile.setText(hlui.name);
            jProgressBarMain.setValue(hlui.part);
            jProgressBarMain.setMaximum(hlui.total);
            need_internal_update=true;
        } else {
            jTextFieldInternalFile.setText(hlui.name);
            jProgressBarInternalFile.setValue(hlui.part);
            jProgressBarInternalFile.setMaximum(hlui.total);
            need_main_update=true;
        }
        this.toFront();
    }

    private abstract class HierarchyLoadUpdateInfoPublisher {

        abstract public void publish_hlui(HierarchyLoadUpdateInfo hlui);
    }

    private int publish_in_count=0;
    private int publish_out_count=0;
    private int sw_done_count=0;
    private int sw_bg_return_count=0;
    private int process_call_count=0;
    
    private Void LoadHierarchy_Background_Do(final HierarchyLoadUpdateInfoPublisher hluip, final CodeGenCommonInterface cgc, final String hierarchy_name) {
        try {
            lhui =
                    diagapplet.CodeGen.ModuleInfo.lhui =
                    diagapplet.CodeGen.CodeGenCommon.lhui =
                    new diagapplet.CodeGen.LoadHierarchyUpdateInterface() {

                        @Override
                        public void update_main(final String name, final int part, final int total) {
                            if(last_main_HierarchyLoadUpdateInfo == null ||
                                        name.compareTo(last_main_HierarchyLoadUpdateInfo.name) != 0) {
                                diagapplet.utils.DiagError.println("\tloading " + name + " from line "+part);
                                need_main_update=true;
                                //Thread.dumpStack();
//                                try
//                                {
//                                    Thread.sleep(10);
//                                }
//                                catch(Exception ex)
//                                {
//                                    ex.printStackTrace();
//                                    diagapplet.utils.DiagError.println("Exception: " + ex);
//                                    diagapplet.CodeGen.ModuleInfo.interrupt_loading = true;
//                                    cgc.set_interrupt_loading(true);
//                                }
                            }
                            if (need_main_update) {
                                HierarchyLoadUpdateInfo hlui = new HierarchyLoadUpdateInfo();
                                hlui.name = name;
                                hlui.part = part;
                                hlui.total = total;
                                hlui.internal = false;
                                last_main_HierarchyLoadUpdateInfo=hlui;
                                need_main_update=false;
                                hluip.publish_hlui(hlui);
                            }
                        }

                        @Override
                        public void update_internal(final String name, final int part, final int total) {
                            if(last_internal_HierarchyLoadUpdateInfo == null ||
                                        name.compareTo(last_internal_HierarchyLoadUpdateInfo.name) != 0) {
                                diagapplet.utils.DiagError.println("\t\tloading " + name + " from line "+part);
                                need_internal_update=true;
                                //Thread.dumpStack();
//                                try
//                                {
//                                    Thread.sleep(10);
//                                }
//                                catch(Exception ex)
//                                {
//                                    ex.printStackTrace();
//                                    diagapplet.utils.DiagError.println("Exception: " + ex);
//                                    diagapplet.CodeGen.ModuleInfo.interrupt_loading = true;
//                                    cgc.set_interrupt_loading(true);
//                                }
                            }
                            if (need_internal_update) {
                                HierarchyLoadUpdateInfo hlui = new HierarchyLoadUpdateInfo();
                                hlui.name = name;
                                hlui.part = part;
                                hlui.total = total;
                                hlui.internal = true;
                                last_internal_HierarchyLoadUpdateInfo=hlui;
                                need_internal_update=false;
                                hluip.publish_hlui(hlui);
                                
                            }
                        }

                        @Override
                        public void done() {
                        //java.awt.EventQueue.invokeLater(r);
                        }
                    };
            cgc.set_m_hierarchyFile(hierarchy_name);
            cgc.LoadHierarchy();
        } catch (Exception ex) {
            diagapplet.utils.DiagError.println("\tException Caught during load hierarchy: "+ex);                                        
            ex.printStackTrace();
        }
        return null;
    }

    private boolean interrupted_message_printed=false;
    private boolean cancelled_message_printed=false;
   
    
    public SwingWorker CreateLoadHierarchySwingWorker(final diagapplet.CodeGen.CodeGenCommonInterface cgc,
            final String hierarchy_name,
            final Runnable r) {

        this.setAlwaysOnTop(true);
        this.setTitle("Loading "+hierarchy_name+" . . .");
        this.toFront();
        need_main_update=true;
        need_internal_update=true;
        interrupted_message_printed=false;
        cancelled_message_printed=false;
        SwingWorker backgroundSwingWorkerToReturn =
                new SwingWorker<Void, HierarchyLoadUpdateInfo>() {

                    @Override
                    public Void doInBackground() {
                        try
                        {
                        HierarchyLoadUpdateInfoPublisher hluip = new HierarchyLoadUpdateInfoPublisher() {

                            @Override
                            public void publish_hlui(HierarchyLoadUpdateInfo hlui) {
                                if(Thread.interrupted())
                                {
                                    if(!interrupted_message_printed)
                                    {
                                        diagapplet.utils.DiagError.println("\tLoading hierarchy detects : Thread.interrupted()=true.\n");
                                        interrupted_message_printed=true;
                                    }
                                    return;
                                }
                                else if(isCancelled())
                                {
                                    if(!cancelled_message_printed)
                                    {
                                        diagapplet.utils.DiagError.println("\tLoading hierarchy detects : isCancelled()=true.\n");
                                        cancelled_message_printed=true;
                                    }
                                }
                                else
                                {
                                    publish(hlui);
                                    publish_in_count++;
                                    try {
                                        Thread.sleep(10);
                                    } catch (Exception ex) {
                                        ex.printStackTrace();
                                        diagapplet.utils.DiagError.println("Exception: " + ex);
                                        diagapplet.CodeGen.ModuleInfo.interrupt_loading = true;
                                        cgc.set_interrupt_loading(true);
                                    }
                                }
                            }
                        };
                        LoadHierarchy_Background_Do(hluip, cgc, hierarchy_name);
                        }
                        catch(Throwable e)
                        {
                            e.printStackTrace();
                        }
                        sw_bg_return_count++;
                        return null;                        
                    }

                    @Override
                    public void process(List<HierarchyLoadUpdateInfo> hlui_list) {
                        //System.err.println("publishing load results hlui_list="+hlui_list);
                        process_call_count++;
                        try {
                            int count=0;
                            for (HierarchyLoadUpdateInfo hlui : hlui_list) {
                                publish_out_count++;
                                DisplayHierarchyLoadUpdateInfo(hlui);
                                if (isCancelled() || isDone()) {
                                    return;
                                }
                                if(Thread.interrupted())
                                {
                                    Thread.currentThread().interrupt();
                                    return;
                                }
                                count++;
                                if(count > 4)
                                {
                                    return;
                                }
                            }
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }

                    @Override
                    protected void done() {
                        sw_done_count++;
                        diagapplet.utils.DiagError.println("done stage 1");
                        try
                        {
                            r.run();
                        }
                        catch(Exception e){
                            diagapplet.utils.DiagError.println("\tException : "+e);
                            e.printStackTrace();
                        }
                    }
        };
        this.swing_worker = backgroundSwingWorkerToReturn;
        return backgroundSwingWorkerToReturn;
//	cgc.set_debug_on(true);

    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents()
    {

        jLabelMainFile = new javax.swing.JLabel();
        jTextFieldMainFile = new javax.swing.JTextField();
        jProgressBarMain = new javax.swing.JProgressBar();
        jLabelInternalFile = new javax.swing.JLabel();
        jTextFieldInternalFile = new javax.swing.JTextField();
        jProgressBarInternalFile = new javax.swing.JProgressBar();
        jButtonCancel = new javax.swing.JButton();

        setTitle("Loading ...");

        jLabelMainFile.setText("Main File:");

        jTextFieldMainFile.setEditable(false);

        jLabelInternalFile.setText("Internal File:");

        jTextFieldInternalFile.setEditable(false);

        jButtonCancel.setText("Cancel");
        jButtonCancel.addActionListener(new java.awt.event.ActionListener()
        {
            public void actionPerformed(java.awt.event.ActionEvent evt)
            {
                jButtonCancelActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabelMainFile)
                    .addComponent(jLabelInternalFile))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextFieldInternalFile, javax.swing.GroupLayout.DEFAULT_SIZE, 315, Short.MAX_VALUE)
                    .addComponent(jTextFieldMainFile, javax.swing.GroupLayout.DEFAULT_SIZE, 315, Short.MAX_VALUE))
                .addContainerGap())
            .addGroup(layout.createSequentialGroup()
                .addGap(12, 12, 12)
                .addComponent(jProgressBarMain, javax.swing.GroupLayout.DEFAULT_SIZE, 407, Short.MAX_VALUE)
                .addContainerGap(44, Short.MAX_VALUE))
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jProgressBarInternalFile, javax.swing.GroupLayout.DEFAULT_SIZE, 439, Short.MAX_VALUE)
                .addContainerGap())
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap(379, Short.MAX_VALUE)
                .addComponent(jButtonCancel)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabelMainFile)
                    .addComponent(jTextFieldMainFile, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jProgressBarMain, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabelInternalFile)
                    .addComponent(jTextFieldInternalFile, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jProgressBarInternalFile, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 10, Short.MAX_VALUE)
                .addComponent(jButtonCancel)
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jButtonCancelActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonCancelActionPerformed
    {//GEN-HEADEREND:event_jButtonCancelActionPerformed
       try
       {
           if(null != swing_worker)
           {
               swing_worker.cancel(true);
           }
       }
       catch(Exception ex)
       {
           ex.printStackTrace();
       }
    }//GEN-LAST:event_jButtonCancelActionPerformed
    /**
     * @param args the command lineNumber arguments
     */
    public static void main(String args[]) {
        java.awt.EventQueue.invokeLater(new Runnable() {

            public void run() {
                new HierarchyLoad_NB_UI().setVisible(true);
            }
        });
    }
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonCancel;
    private javax.swing.JLabel jLabelInternalFile;
    private javax.swing.JLabel jLabelMainFile;
    private javax.swing.JProgressBar jProgressBarInternalFile;
    private javax.swing.JProgressBar jProgressBarMain;
    private javax.swing.JTextField jTextFieldInternalFile;
    private javax.swing.JTextField jTextFieldMainFile;
    // End of variables declaration//GEN-END:variables
}
