//******************************************************************************
// socapplet.java:	Applet
//
//******************************************************************************
import java.applet.*;
import java.awt.*;
import java.net.*;
import java.io.*;
import CountButton;



//==============================================================================
// Main Class for applet socapplet
//
//==============================================================================
public class socapplet extends Applet implements Runnable
{
 		Socket timeSocket = null;
        DataOutputStream timeOs = null;
        DataInputStream timeIs = null;
		String time_reply_string = null;
		Socket controlSocket = null;
        DataOutputStream controlOs = null;
        DataInputStream controlIs = null;
		String control_reply_string = null;
		boolean time_request_made = false;
		CountButton start_button = null;
		int last_start_button_count = 0;
		CountButton stop_button = null;
		int last_stop_button_count = 0;
		CountButton reset_button = null;
		int last_reset_button_count = 0;
		double time = 0.0;
		Label time_label = null;
	// Members for applet parameters
    // <type>       <MemberVar>    = <Default Value>
    //--------------------------------------------------------------------------
	private String m_host = "windale";
	private int m_time_port = 3005;
	private int m_control_port = 3006;


    // Parameter names.  To change a name of a parameter, you need only make
	// a single change.  Simply modify the value of the parameter string below.
    //--------------------------------------------------------------------------
	private final String PARAM_host = "host";
	private final String PARAM_time_port = "time_port";
	private final String PARAM_control_port = "control_port";


	
	 // THREAD SUPPORT:
	//		m_socapplet	is the Thread object for the applet
	//--------------------------------------------------------------------------
	Thread	 m_socapplet = null;



	// socapplet Class Constructor
	//--------------------------------------------------------------------------
	public socapplet()
	{
		// TODO: Add constructor code here
	}

	// APPLET INFO SUPPORT:
	//		The getAppletInfo() method returns a string describing the applet's
	// author, copyright date, or miscellaneous information.
    //--------------------------------------------------------------------------
	public String getAppletInfo()
	{
		return "Name: socapplet\r\n" +
		       "Author: Will Shackleford\r\n" +
		       "Created with Microsoft Visual J++ Version 1.0";
	}

	// PARAMETER SUPPORT
	//		The getParameterInfo() method returns an array of strings describing
	// the parameters understood by this applet.
	//
    // emcjava Parameter Information:
    //  { "Name", "Type", "Description" },
    //--------------------------------------------------------------------------
	public String[][] getParameterInfo()
	{
		String[][] info =
		{
			{ PARAM_host, "String", "Host Running NML Server" },
			{ PARAM_time_port, "int", "Port on which NML server listens for timebuf." },
			{ PARAM_control_port, "int", "Port on which NML server listens for controlbuf." },
		};
		return info;		
	}

	// The init() method is called by the AWT when an applet is first loaded or
	// reloaded.  Override this method to perform whatever initialization your
	// applet needs, such as initializing data structures, loading images or
	// fonts, creating frame windows, setting the layout manager, or adding UI
	// components.
    //--------------------------------------------------------------------------
	public void init()
	{
        String param;
		// host: Host Running  NML Server
		//--------------------------------------------------------------
		param = getParameter(PARAM_host);
		if (param != null)
			m_host = param;

		// port: Port on which  NML server listens.
		//--------------------------------------------------------------
		param = getParameter(PARAM_time_port);
		if (param != null)
			m_time_port = Integer.parseInt(param);
	  
		// port: Port on which  NML server listens.
		//--------------------------------------------------------------
		param = getParameter(PARAM_control_port);
		if (param != null)
			m_control_port = Integer.parseInt(param);

// If you use a ResourceWizard-generated "time creator" class to
        // arrange times in your applet, you may want to call its
        // Createtimes() method from within this method. Remove the following
        // call to resize() before adding the call to Createtimes();
        // Createtimes() does its own resizing.
        //----------------------------------------------------------------------
		resize(320, 240);

		// TODO: Place additional initialization code here
	       try {
            timeSocket = new Socket(m_host, m_time_port);
            timeOs = new DataOutputStream(timeSocket.getOutputStream());
            timeIs = new DataInputStream(timeSocket.getInputStream());
            controlSocket = new Socket(m_host, m_control_port);
            controlOs = new DataOutputStream(controlSocket.getOutputStream());
            controlIs = new DataInputStream(controlSocket.getInputStream());
        } catch (UnknownHostException e) {
            //g.drawString("Don't know about host: dopey",10,20);
        } catch (IOException e) {
            //g.drawString("Couldn't get I/O for the connection to: taranis", 10,20);
        }
		time_reply_string = "ERR: No data yet.";
		control_reply_string = "ERR: No data yet.";
		start_button = new CountButton("Start");
		add(start_button);
		stop_button = new CountButton("Stop");
		add(stop_button);
		reset_button = new CountButton("Reset");
		add(reset_button);
		//setBackground(new Color(0,0,255));
		setBackground(new Color(255,255,255));
		time_label = new Label("Time: 0.0000");
		//Font label_font = time_label.getFont();
		//time_label.setFont(new Font(label_font.getName(), label_font.BOLD,20));
		add(time_label);
		

}

	// Place additional applet clean up code here.  destroy() is called when
	// when you applet is terminating and being unloaded.
	//-------------------------------------------------------------------------
	public void destroy()
	{
		// TODO: Place applet cleanup code here
		try{

			if(timeOs != null)
			{
				timeOs.close();
				timeOs = null;
			}
			if(timeIs != null);
			{
				timeIs.close();
				timeIs = null;
			}
			if(timeSocket != null)
			{
				timeSocket.close();
				timeSocket = null;
			}
			if(controlOs != null)
			{
				controlOs.close();
				controlOs = null;
			}
			if(controlIs != null);
			{
				controlIs.close();
				controlIs = null;
			}
			if(controlSocket != null)
			{
				controlSocket.close();
				controlSocket = null;
			}
		} catch (IOException e) {
            //g.drawString("Couldn't get I/O for the connection to: taranis", 10,20);
        }

}

	// socapplet Paint Handler
	//--------------------------------------------------------------------------
	public void paint(Graphics g)
	{
		// TODO: Place applet paint code here
		//g.drawString("Running: " + Math.random(), 10, 20);
 	//DataInputStream stdIn = new DataInputStream(System.in);

 
        if (timeSocket != null && timeOs != null && timeIs != null 
			&& controlSocket != null && controlOs != null && controlIs != null) {
            try {
                String userInput;
				
                //while ((userInput = stdIn.readLine()) != null) {
                if(!time_request_made)
				{
					//g.drawString("Sending Request. . .", 40,60);
					userInput = "read:\n";
					timeOs.writeBytes(userInput);
                    //os.writeByte('\n');
					time_request_made = true;
					//g.drawString("done.", 80,60);
				}
				else
				{
					//g.drawString("Recieving reply. . .", 40,60);
					time_reply_string = 	timeIs.readLine();
					time_request_made = false;
					//g.drawString("done.", 80,60);
				}
				//g.drawString("echo: " + time_reply_string, 20, 40);
				//g.drawString("Start count " + start_button.count, 20, 80);
				//g.drawString("Stop count " + stop_button.count, 20, 100);
				//g.drawString("Reset count " + reset_button.count, 20, 120);
				if(!time_reply_string.startsWith("ERR"))
				{
					int time_index = time_reply_string.lastIndexOf(",");
					time_index =   time_reply_string.lastIndexOf(",",time_index-1);
					String time_string = time_reply_string.substring(time_index+1);
					Double dtime = new Double(0).valueOf(time_string);
					time = dtime.doubleValue();
					//g.setColor(new Color(255,255,255));
					//g.drawString( dtime.toString(), 10,60);
					time_label.setText(dtime.toString());
				}
				if(start_button.count != last_start_button_count)
				{
					controlOs.writeBytes("write: 2001\n");
					last_start_button_count = start_button.count;
				}
				if(stop_button.count != last_stop_button_count)
				{
					controlOs.writeBytes("write: 2002\n");
					last_stop_button_count = stop_button.count;
				}
				if(reset_button.count != last_reset_button_count)
				{
					controlOs.writeBytes("write: 2003\n");
					last_reset_button_count = reset_button.count;
				}
				
            } catch (IOException e) {
                g.drawString("I/O failed "+e.getMessage(), 10,20);
            }
        }
		else
		{
		    g.drawString("null Socket or stream.", 20, 40);
		}       //
 
	}

	//		The start() method is called when the page containing the applet
	// first appears on the screen. The AppletWizard's initial implementation
	// of this method starts execution of the applet's thread.
	//--------------------------------------------------------------------------
	public void start()
	{
		if (m_socapplet == null)
		{
			m_socapplet = new Thread(this);
			m_socapplet.start();
		}
		// TODO: Place additional applet start code here
	}
	
	//		The stop() method is called when the page containing the applet is
	// no longer on the screen. The AppletWizard's initial implementation of
	// this method stops execution of the applet's thread.
	//--------------------------------------------------------------------------
	public void stop()
	{
		if (m_socapplet != null)
		{
			m_socapplet.stop();
			m_socapplet = null;
		}

		// TODO: Place additional applet stop code here
	}

	// THREAD SUPPORT
	//		The run() method is called when the applet's thread is started. If
	// your applet performs any ongoing activities without waiting for user
	// input, the code for implementing that behavior typically goes here. For
	// example, for an applet that performs animation, the run() method times
	// the display of images.
	//--------------------------------------------------------------------------
	public void run()
	{
		while (true)
		{
			try
			{
				repaint();
				// TODO:  Add additional thread-specific code here
				Thread.sleep(50);
			}
			catch (InterruptedException e)
			{
				// TODO: Place exception-handling code here in case an
				//       InterruptedException is thrown by Thread.sleep(),
				//		 meaning that another thread has interrupted this one
				stop();
			}
		}
	}



	// TODO: Place additional applet code here

}
