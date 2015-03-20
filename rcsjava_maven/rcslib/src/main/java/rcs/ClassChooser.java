package rcs;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import javax.swing.JOptionPane;

/**
 *
 * @author shackle
 */


public class ClassChooser {
    
    public static void main(String args[]) {
        try {
            String tmp_args[]= args;
            String tmp_subclass=null;
            if(args.length > 1) {
                if(0 == "--subclass".compareTo(args[0])) {
                    tmp_subclass = args[1];
                    tmp_args = Arrays.copyOfRange(args, 2, args.length);
                }
            }
            final String subclass = tmp_subclass;
            final String args_copy[] = tmp_args;
            String args_combined = "";
            for (String arg : args_copy) {
                args_combined = args_combined + " " + arg;
            }
            args_combined = args_combined.trim();
            final String args_combinedf = args_combined;
            final String choices[] = { "Plotter", "Diagnostics","Design","CodeGenCmdLine"};
            java.awt.EventQueue.invokeAndWait(new Runnable() {
                @Override
                public void run() {
                    String choice = subclass;
                    String new_args_combined =args_combinedf;
                    if(choice == null) {
                    choice = (String) JOptionPane.showInputDialog(
                            null, // parent component
                            "Subclass to run:", // message
                            "rcsjava", // title
                            JOptionPane.QUESTION_MESSAGE, // message type
                            null, //icon
                            choices, // selection values
                            choices[0] // initial selection value
                            );
                        new_args_combined = JOptionPane.showInputDialog(null, "Arguments: ", args_combinedf);
                    }
                    final String new_args[] = new_args_combined.trim().length()>0?new_args_combined.split(" +"):args_copy;
                    if(choice.compareTo(choices[1]) == 0) {
                        Thread t = new Thread(new Runnable() {

                            @Override
                            public void run() {
                                diagapplet.diag_NB_UI.main(new_args);
                            }
                        });
                        t.start();
                    } else if(choice.compareTo(choices[0]) == 0) {
                        Thread t = new Thread(new Runnable() {

                            @Override
                            public void run() {
                                diagapplet.plotter.plotterJFrame.main(new_args);
                            }
                        });
                        t.start();
                    }else if(choice.compareTo(choices[2]) == 0) {
                        Thread t = new Thread(new Runnable() {

                            @Override
                            public void run() {
                                rcsdesign.rcsDesign.main(new_args);
                            }
                        });
                        t.start();
                    } else if(choice.compareTo(choices[3]) == 0) {
                        Thread t = new Thread(new Runnable() {

                            @Override
                            public void run() {
                                diagapplet.CodeGen.CodeGenCmdLine.main(new_args);
                            }
                        });
                        t.start();
                    }
                }
            });
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
