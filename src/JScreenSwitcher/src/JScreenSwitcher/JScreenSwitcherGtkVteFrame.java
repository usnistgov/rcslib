package JScreenSwitcher;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.TimerTask;
import org.gnu.gdk.Color;
import org.gnu.gtk.Gtk;
import org.gnu.gtk.event.ButtonEvent;
import org.gnu.gtk.event.ButtonListener;
import org.gnu.gtk.event.ComboBoxEvent;
import org.gnu.gtk.event.ComboBoxListener;
import org.gnu.gtk.event.LifeCycleEvent;
import org.gnu.gtk.event.LifeCycleListener;

public class JScreenSwitcherGtkVteFrame
{
    
    org.gnu.gtk.ComboBox combo=null;
    org.gnu.gnomevte.Terminal terminal=null;
    String cmd_to_send=null;
    
    public static String args_copy=null;
    public boolean need_first_detach_cntl_A=false;
    public boolean  need_detach_colon=false;
    public boolean need_detach_string=false;
    public boolean cmd_to_send_sent=false;
    java.util.Timer tmr = null;
    org.gnu.gtk.CheckButton pauseCheckButton =  null;
    
    public void SendCmdOut()
    {
	try
	{
	    if(cmd_to_send != null)
		{
		    if(need_first_detach_cntl_A)
		    {
			System.out.println("sending ^A");
			terminal.feedChild("\001");
			need_detach_colon=true;
			need_detach_string=false;
			need_first_detach_cntl_A=false;
			return;
		    }
		    else if(need_detach_colon)
		    {
			System.out.println("sending :");
			terminal.feedChild(":");
			need_detach_string=true;
			need_detach_colon=false;
			need_first_detach_cntl_A=false;
			return;
		    }
		    else if(need_detach_string)
		    {
			System.out.println("sending detach");
			terminal.feedChild("detach\n");
			need_detach_colon=false;
			need_detach_string=false;
			need_first_detach_cntl_A=false;
			return;
		    }
		    String c = cmd_to_send;
		    System.out.println("Send command "+c);
		    cmd_to_send=null;
		    terminal.feedChild(c);
		    if(null != pauseCheckButton)
		    {
			pauseCheckButton.setState(false);
		    }
		    if(tmr != null)
		    {
			tmr.cancel();
			tmr = null;
		    }
		}
	}
	catch(Exception e)
	{
	    e.printStackTrace();
	}
    }
    
    public JScreenSwitcherGtkVteFrame()
    {
	
	org.gnu.gtk.Window window = new org.gnu.gtk.Window(
		org.gnu.gtk.WindowType.TOPLEVEL);
	window.setBorderWidth(10);
	window.setTitle("Screen Switcher");
	window.addListener(new LifeCycleListener()
	{
	    public void lifeCycleEvent(LifeCycleEvent event)
	    {
	    }
	    
	    public boolean lifeCycleQuery(LifeCycleEvent event)
	    {
		if (event.isOfType(LifeCycleEvent.Type.DESTROY)
		|| event.isOfType(LifeCycleEvent.Type.DELETE))
		{
		    try
		    {
			if(null != terminal)
			{
			    terminal.feedChild("\001\004");
			}
		    }
		    catch(Exception e)
		    {
			e.printStackTrace();
		    }
		    Gtk.mainQuit();
		}
		return false;
	    }
	});
	
	window.setDefaultSize(300, 300);
	org.gnu.gtk.VBox vbox = new org.gnu.gtk.VBox(false,4);
	
	combo = new org.gnu.gtk.ComboBox();
	
	try
	{
//	    System.out.println("Executing screen -ls");
//	    terminal.feed("Executing screen -ls\n");
	    Process p = Runtime.getRuntime().exec("screen -ls");
//	    System.out.println("p="+p);
//	    terminal.feed("p="+p+"\n");
	    p.waitFor();
	    p.getInputStream();
	    BufferedReader b_r = new BufferedReader(new InputStreamReader(p.getInputStream()));
	    
	    while(b_r.ready())
	    {
		String s = b_r.readLine();
//		terminal.feed(s+"\n");
		System.out.println(s);
		int dindex = s.indexOf("(Detached)");
		if(dindex > 0 )
		{
		    String screen = s.substring(0,dindex).trim();
		    combo.appendText("screen -r "+screen);
		}
		else
		{
		    dindex = s.indexOf("(Attached)");
		    if(dindex > 0 )
		    {
			String screen = s.substring(0,dindex).trim();
			combo.appendText("screen -r -x "+screen);
		    }
		}
	    }
	}
	catch(Exception e)
	{
	    e.printStackTrace();
	}
	combo.setActive(0);
	
//(combo.getActiveText(),null,null);
//("/bin/bash",null,null);
	//terminal.setScrollbackLines(2000);
	combo.addListener(new ComboBoxListener()
	{
	    public void comboBoxEvent(ComboBoxEvent comboBoxEvent)
	    {
		try
		{
		    
		    if(!need_detach_colon && ! need_detach_string && null != terminal)
		    {
			terminal.feedChild("\001\033");
			need_first_detach_cntl_A=true;
			need_detach_colon=false;
			need_detach_string=false;
		    }
		    //terminal.reset(true,true);
		    //Thread.sleep(5000);
		    cmd_to_send = combo.getActiveText()+"\n";
		    if(tmr != null)
		    {
			tmr.cancel();
			tmr = null;
		    }
		    tmr = new java.util.Timer();
		    tmr.schedule(new TimerTask()
		    {
			public void run()
			{
			    SendCmdOut();
			}
		    },250,250);
		    //terminal.feedChild(";\npwd\ndata\n"+combo.getActiveText()+"\n");//,null,null,false,false,false);
		}
		catch(Exception e)
		{
		    e.printStackTrace();
		}
	    }
	});
	vbox.packStart(combo);
	org.gnu.gtk.HBox hbox = new org.gnu.gtk.HBox(false,4);
	pauseCheckButton = new org.gnu.gtk.CheckButton();
	pauseCheckButton.setLabel("Pause");
	org.gnu.gtk.Button pageUpButton = new org.gnu.gtk.Button();
	pageUpButton.setLabel("PageUp");
	org.gnu.gtk.Button pageDownButton = new org.gnu.gtk.Button();
	pageDownButton.setLabel("PageDown");
	pauseCheckButton.addListener(new ButtonListener()
	{
	    public void buttonEvent(ButtonEvent buttonEvent)
	    {
		if(((org.gnu.gtk.CheckButton) buttonEvent.getSource()).getState())
		{
		    terminal.feedChild("\001\033");
		}
		else
		{
		    terminal.feedChild("\033");
		}
	    }
	});
	hbox.packStart(pauseCheckButton);
	
	
	vbox.packStart(hbox);
	terminal = org.gnu.gnomevte.Terminal.terminalAndShell(); 
//	terminal.addListener(new TerminalListener()
//	{
//	    public void terminalEvent(TerminalEvent terminalEvent)
//	    {
//		SendCmdOut();
//	    }
//	});
	
	if(combo.getActiveText() != null && combo.getActiveText().length() > 0)
	{
	    terminal.feedChild(combo.getActiveText()+"\n");
	}
	need_detach_colon=false;
	need_detach_string=false;
	cmd_to_send=null;
	//terminal.forkCommand("/bin/bash",null,null,false,false,false);
//        frame.setLabelAlign(1.0);
//        frame.setShadow(ShadowType.ETCHED_OUT);
	vbox.packStart(terminal);
	window.add(vbox);
	window.showAll();
	terminal.setBackgroudColor(Color.WHITE);
	terminal.setForegroundColor(Color.BLACK);
	
    }
    
    public static void main(String[] args)
    {
	// Initialize GTK
	Gtk.init(args);
	JScreenSwitcherGtkVteFrame f = new JScreenSwitcherGtkVteFrame();
	for(String arg: args)
	{
	    f.terminal.feedChild(arg+"\n");
	}
	Gtk.main();
    }
}
