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


package diagapplet;


import java.awt.*;
import java.awt.event.*;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;
import java.util.StringTokenizer;
import diagapplet.CodeGen.ModuleInfo;
import diagapplet.CodeGen.StructureTypeInfo;
import diagapplet.CodeGen.STI_TokenizerInterface;
import diagapplet.utils.URLLoadInfoPanelInterface;

/**
 * Heavy weight panel component used in old original diagnostics applet and current RCS Design Tool.
 * New diagnostics tool uses a JPaintablePanel with HierarchyDraw as a replacement.
 * @author shackle
 */
public class HierarchyPanel extends Panel implements MouseListener, MouseMotionListener, ComponentListener
{

    /**
     *   Unique id for this class, Exception implements Serializable interface see documentation
     * for serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613910L;
    
    volatile diagapplet.diag_update_interface diag_update_object=null;

    ModuleInfo moduleShowingCommands = null;
    public static boolean debug_on = false;
    public static boolean updating_hierarchy = true;
    static boolean interrupt_loading=false;
    boolean force_next_repaint=true;

    volatile boolean painting=false;
    int highlighted_command;
    long new_threshold = 1500;
    static Vector modules_by_generation = null;
    static Vector modules = null;
    int max_number_in_generation = 0;
    int max_generations = -1;
    int current_module =0;
    public int scroll_x =0;
    public int scroll_y =0;
    int lastx = -1;
    int lasty = -1;
    int min_y = -1;
    int max_y = -1;
    int min_x = -1;
    int max_x = -1;
    boolean use_color = true;
    public static int MODULE_WIDTH = 160;
    public static int MODULE_HEIGHT = 60;
    public static int MODULE_XOFFSET = -10;
    public static int MODULE_YOFFSET = -15;
    public static int MODULE_X_SPACING = 50;
    public static int MODULE_Y_SPACING = 40;
    public static int MAX_HEIGHT=1200;
    public static int MAX_WIDTH=1500;
    public static int MIN_HEIGHT=100;
    public static int MIN_WIDTH=100;
    static boolean partial_paint=false;
    volatile int  paint_hierarchy_count=0;

    volatile int mouse_clicked_count=0;
    volatile int  last_mouse_clicked_count=0;
    volatile int  mouse_clicked_x=0;
    volatile int  mouse_clicked_y=0;

    volatile int  mouse_pressed_count=0;
    volatile int  last_mouse_pressed_count=0;
    volatile int  mouse_pressed_x=0;
    volatile int  mouse_pressed_y=0;

    volatile int  mouse_released_count=0;
    volatile int  last_mouse_released_count=0;
    volatile int  mouse_released_x=0;
    volatile int  mouse_released_y=0;

    volatile int  mouse_dragged_count=0;
    volatile int  last_mouse_dragged_count=0;
    volatile int  mouse_dragged_x=0;
    volatile int  mouse_dragged_y=0;

    Dimension d = null;
    Image buffer_image = null;
    static boolean use_buffer_image=false;
    diagapplet.utils.FastListPanelInterface modulesList = null;
    public Scrollbar horzScrollbar = null;
    public Scrollbar vertScrollbar = null;
    public boolean design_mode = false;
    int repaint_count= 10;
    boolean list_modules_by_number = false;

    protected diagapplet.utils.CountList modulesCountList = null;

    static void ErrorPrint(String s)
    {
	try
	    {
		Throwable t = new Throwable();
		StackTraceElement ste[] = t.getStackTrace();
		if(debug_on)
		    {
			System.out.println("ErrorPrint + "+ste[1].getFileName()+":"+ste[1].getLineNumber()+" "+s);
		    }
		System.err.println(ste[1].getFileName()+":"+ste[1].getLineNumber()+" "+s);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }    


    static void DebugPrint(String s)
    {
	 try
	     {
		 if(!debug_on)
		     {
			 return;
		     }
		 Throwable t = new Throwable();
		 StackTraceElement ste[] = t.getStackTrace();
		 System.out.println(ste[1].getFileName()+":"+ste[1].getLineNumber()+" "+s);
	     }
	 catch(Exception e)
	     {
		 e.printStackTrace();
	     }
     }    

    static void DebugPrint2(String s)
    {
	 try
	     {
		 Throwable t = new Throwable();
		 StackTraceElement ste[] = t.getStackTrace();
		 System.out.println(ste[1].getFileName()+":"+ste[1].getLineNumber()+" "+s);
	     }
	 catch(Exception e)
	     {
		 e.printStackTrace();
	     }
     }    
    
    public void setCountList(diagapplet.utils.CountList cli)
    {
	modulesCountList = cli;
    }
    
    diagapplet.utils.CountListInterface getCountList()
    {
	return modulesCountList;
    }
    
    int getModulesListItemCount()
    {
	if(null != modulesList)
	    {
		return modulesList.getItemCount();
	    }
	else if(null != modulesCountList)
	    {
		return modulesCountList.getItemCount();
	    }	
	return 0;
    }

    String getModulesListItem(int k)
    {
	if(null != modulesList)
	    {
		return modulesList.getItem(k);
	    }
	else if(null != modulesCountList)
	    {
		return modulesCountList.getItem(k);
	    }	
	return null;
    }
	
    void modulesListSelect(int k)
    {
	if(null != modulesList)
	    {
		modulesList.select(k);
	    }
	else if(null != modulesCountList)
	    {
		modulesCountList.select(k);
	    }	
    }

    public String getModulesListSelectedItem()
    {
	if(null != modulesList)
	    {
		return modulesList.getSelectedItem();
	    }
	else if(null != modulesCountList)
	    {
		return modulesCountList.getSelectedItem();
	    }
	return null;
    }

	
    public void monitored_repaint()
    {
	try
	    {
		if(debug_on)
		    {
			Thread.dumpStack();
		    }
		repaint();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }
    public void clear()
    {
	if(null != modules_by_generation)
	    {
		modules_by_generation.removeAllElements();
		modules_by_generation = null;
	    }
	if(null != modules)
	    {
		Enumeration moduleEnumeration = modules.elements();
		while(moduleEnumeration.hasMoreElements())
		    {
			ModuleInfo modInfo = (ModuleInfo) moduleEnumeration.nextElement();
			modInfo.generation = 0;
			modInfo.max_children_in_generation =0;
			modInfo.x = 0;
			modInfo.y = 0;
			modInfo.parent = null;
			modInfo.positioned = false;
		    }
		modules.removeAllElements();
		modules = null;
	    }
	reset();
	System.runFinalization();
	modules  = new Vector();
	modules_by_generation = new Vector();
    }



    public HierarchyPanel(int new_width, int new_height)
    {
	this();
	if(debug_on)
	    {
		System.out.println("HierarchyPanel("+new_width+", "+new_height+") called.");
	    }
	if(new_width > MAX_WIDTH)
	    {
		new_width = MAX_WIDTH;
	    }
	if(new_height > MAX_HEIGHT )
	    {
		new_height = MAX_HEIGHT;
	    }
	if(new_width < MIN_WIDTH)
	    {
		new_width = MIN_WIDTH;
	    }
	if(new_height < MIN_HEIGHT )
	    {
		new_height = MIN_HEIGHT;
	    }
	d = new Dimension(new_width, new_height);
	setSize(new_width, new_height);
	addMouseListener(this);
	addMouseMotionListener(this);
	addComponentListener(this);
	setBackground(Color.BLUE);
    }

    @SuppressWarnings("unchecked")
    public HierarchyPanel()
    {
	if(null == modules_by_generation)
	    {
		modules_by_generation = new Vector();
		modules_by_generation.addElement(new Vector());
	    }
	if(null == modules)
	    {
		modules = new Vector();
	    }
	use_color = true;
	reset();
    }

    public void reset()
    {
	max_number_in_generation = 0;
	max_generations = -1;
	current_module =0;
	scroll_x =0;
	scroll_y =0;
	lastx = -1;
	lasty = -1;
	min_y = -1;
	max_y = -1;
	min_x = -1;
	max_x = -1;
    }
    public Dimension getPreferredSize()
    {
	return d;
    }

    public Dimension getMinimumSize()
    {
	return d;
    }

    public void mouseEntered(MouseEvent evt)
    {
    }
    public void mouseExited(MouseEvent evt)
    {
    }

    public int computeMaxY()
    {
	try
	    {
		int maxy = d.height*2;
		ModuleInfo maxy_module=null;
		if(null != modules)
		    {
			for(int i = 0; i < modules.size(); i++)
			    {
				ModuleInfo module = (ModuleInfo) modules.elementAt(i);
				if(maxy < module.y)
				    {
					maxy = module.y;
					if(debug_on)
					    {
						maxy_module = module;
					    }
				    }
			    }
		    }
		maxy += 2*Math.abs(MODULE_YOFFSET) + 2*Math.abs(MODULE_HEIGHT);
		if(debug_on)
		    {
			DebugPrint("HierarchyPanel.computeMaxY() returning "+maxy+" : maxy_module="+maxy_module+".");
		    }
		return maxy;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return max_y;
    }


    public int computeMaxX()
    {
	try
	    {
		int maxx = d.width*2;
		ModuleInfo maxx_module=null;
		if(null != modules)
		    {
			for(int i = 0; i < modules.size(); i++)
			    {
				ModuleInfo module = (ModuleInfo) modules.elementAt(i);
				if(maxx < module.x)
				    {
					if(debug_on)
					    {
						maxx_module=module;
					    }
					maxx = module.x;
				    }
			    }
		    }
		maxx += 2*Math.abs(MODULE_XOFFSET) + 2*Math.abs(MODULE_WIDTH);
		if(debug_on)
		    {
			DebugPrint("HierarchyPanel.computeMaxX() returning "+maxx+" : maxx_module="+maxx_module+".");
		    }
		return maxx;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return max_x;
    }

    public void mouseClicked(MouseEvent  evt)
    {
	try
	    {
		int x = evt.getX();
		int y = evt.getY();
		mouse_clicked_x = x;
		mouse_clicked_y = y;
		mouse_clicked_count++;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public void mousePressed(MouseEvent  evt)
    {
	try
	    {
		int x = evt.getX();
		int y = evt.getY();
		mouse_pressed_count++;
		mouse_pressed_x = x;
		mouse_pressed_y = y;
		last_mouse_dragged_count = mouse_dragged_count;

		// Compute the current x and y offsets
		if(debug_on)
		    {
			DebugPrint("mouseDown event occurred at ("+x+", "+y+") : modules.size() = "+modules.size()+" : size="+getSize());
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }
    
    void handle_mousePressed()
    {
	try
	    {
		int x = mouse_pressed_x;
		int y = mouse_pressed_y;
		last_mouse_pressed_count = mouse_pressed_count;
		last_mouse_dragged_count = mouse_dragged_count;
		if(debug_on)
		    {
			DebugPrint("modulesList="+modulesList);
			DebugPrint("modulesCountList="+modulesCountList);
			DebugPrint("handle_mousePressed: mouse_pressed_count="+mouse_pressed_count+",x="+x+",y="+y+",moduleShowingCommands="+moduleShowingCommands);
		    }
		Rectangle rect = getBounds();

		int xoffset = rect.x -scroll_x;
		int yoffset = rect.y -scroll_y;

		moduleShowingCommands = null;

		// See if the mouse came down inside one of the modules
		for(int i = 0; i < modules.size(); i++)
		    {
			ModuleInfo module = (ModuleInfo) modules.elementAt(i);
			if(null == module)
			    {
				continue;
			    }
			if(debug_on)
			    {
				DebugPrint("HierarchyPanel.mousePressed() module.Name="+module.Name+": x(="+x+") > xoffset(="+xoffset+") + module.x(="+module.x+")+MODULE_XOFFSET(="+MODULE_XOFFSET+") ["+(xoffset + module.x+MODULE_XOFFSET)+"] is "+(x > xoffset + module.x+MODULE_XOFFSET));
				DebugPrint("HierarchyPanel.mousePressed() module.Name="+module.Name+": x(="+x+") < xoffset(="+xoffset+") + module.x(="+module.x+")+MODULE_XOFFSET(="+MODULE_XOFFSET+") + MODULE_WIDTH(="+MODULE_WIDTH+") ["+(xoffset + module.x+MODULE_XOFFSET + MODULE_WIDTH)+"] is "+(x < xoffset + module.x+MODULE_XOFFSET + MODULE_WIDTH));
				DebugPrint("HierarchyPanel.mousePressed() module.Name="+module.Name+": y(="+y+") > yoffset(="+yoffset+") + module.y(="+module.y+")+MODULE_YOFFSET(="+MODULE_YOFFSET+") ["+(yoffset + module.y+MODULE_YOFFSET)+"] is "+(y > yoffset + module.y+MODULE_YOFFSET));
				DebugPrint("HierarchyPanel.mousePressed() module.Name="+module.Name+": y(="+y+") < yoffset(="+yoffset+") + module.y(="+module.y+")+MODULE_YOFFSET(="+MODULE_YOFFSET+") + MODULE_HEIGHT(="+MODULE_HEIGHT+") ["+(yoffset + module.y+MODULE_YOFFSET + MODULE_HEIGHT)+"] is "+(y < yoffset + module.y+MODULE_YOFFSET + MODULE_HEIGHT));
			    }
			if( x > xoffset + module.x+MODULE_XOFFSET &&
			    x < xoffset + module.x+MODULE_XOFFSET + MODULE_WIDTH &&
			    y > yoffset + module.y+MODULE_YOFFSET &&
			    y < yoffset + module.y+MODULE_YOFFSET + MODULE_HEIGHT)
			    {
				moduleShowingCommands = module;
				highlighted_command = 0;
				for(int k = 0; k < getModulesListItemCount(); k++)
				    {
					String item = getModulesListItem(k);
					if(null == item)
					    {
						continue;
					    }
					if(item.equals(module.Name))
					    {
						if(debug_on)
						    {
							DebugPrint("HierarchyPanel.mousePressed() selecting module "+module+" at position "+k+" in modulesList.");
						    }
						modulesListSelect(k);
						if(modulesCountList != null)
						    {
							ItemEvent select_event = new ItemEvent(modulesCountList, ItemEvent.ITEM_STATE_CHANGED, Integer.valueOf(k),  ItemEvent.SELECTED);
							modulesCountList.dispatchEvent(select_event);
						    }
					    }
				    }
				if(design_mode)
				    {
					scroll_x = module.x - getSize().width/2+rect.x;
					scroll_y = module.y +MODULE_YOFFSET - MODULE_HEIGHT+rect.y;
					scroll_y += MODULE_YOFFSET - MODULE_HEIGHT;
					if(null != horzScrollbar)
					    {
						if(debug_on)
						    {
							DebugPrint("HierachyPanel.mousePressed calling horzScrollbar.setValue("+scroll_x+")");
						    }
						horzScrollbar.setValue(scroll_x);
					    }
					if(null != vertScrollbar)
					    {
						if(debug_on)
						    {
							DebugPrint("HierachyPanel.mousePressed calling vertScrollbar.setValue("+scroll_y+")");
						    }
						vertScrollbar.setValue(scroll_y);
					    }
				    }
				break;
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	repaint();
    }





    void handle_mouseClicked()
    {
	try
	    {
		int x = mouse_clicked_x;
		int y = mouse_clicked_y;
		last_mouse_clicked_count = mouse_clicked_count;
		if(debug_on)
		    {
			DebugPrint("modulesList="+modulesList);
			DebugPrint("modulesCountList="+modulesCountList);
			DebugPrint("handle_mouseClicked: mouse_clicked_count="+mouse_clicked_count+",x="+x+",y="+y+",moduleShowingCommands="+moduleShowingCommands);
		    }
		Rectangle rect = getBounds();

		int xoffset = rect.x -scroll_x;
		int yoffset = rect.y -scroll_y;

		moduleShowingCommands = null;

		// See if the mouse came down inside one of the modules
		for(int i = 0; i < modules.size(); i++)
		    {
			ModuleInfo module = (ModuleInfo) modules.elementAt(i);
			if(null == module)
			    {
				continue;
			    }
			if(debug_on)
			    {
				DebugPrint("HierarchyPanel.mouseClicked() module.Name="+module.Name+": x(="+x+") > xoffset(="+xoffset+") + module.x(="+module.x+")+MODULE_XOFFSET(="+MODULE_XOFFSET+") ["+(xoffset + module.x+MODULE_XOFFSET)+"] is "+(x > xoffset + module.x+MODULE_XOFFSET));
				DebugPrint("HierarchyPanel.mouseClicked() module.Name="+module.Name+": x(="+x+") < xoffset(="+xoffset+") + module.x(="+module.x+")+MODULE_XOFFSET(="+MODULE_XOFFSET+") + MODULE_WIDTH(="+MODULE_WIDTH+") ["+(xoffset + module.x+MODULE_XOFFSET + MODULE_WIDTH)+"] is "+(x < xoffset + module.x+MODULE_XOFFSET + MODULE_WIDTH));
				DebugPrint("HierarchyPanel.mouseClicked() module.Name="+module.Name+": y(="+y+") > yoffset(="+yoffset+") + module.y(="+module.y+")+MODULE_YOFFSET(="+MODULE_YOFFSET+") ["+(yoffset + module.y+MODULE_YOFFSET)+"] is "+(y > yoffset + module.y+MODULE_YOFFSET));
				DebugPrint("HierarchyPanel.mouseClicked() module.Name="+module.Name+": y(="+y+") < yoffset(="+yoffset+") + module.y(="+module.y+")+MODULE_YOFFSET(="+MODULE_YOFFSET+") + MODULE_HEIGHT(="+MODULE_HEIGHT+") ["+(yoffset + module.y+MODULE_YOFFSET + MODULE_HEIGHT)+"] is "+(y < yoffset + module.y+MODULE_YOFFSET + MODULE_HEIGHT));
			    }
			if( x > xoffset + module.x+MODULE_XOFFSET &&
			    x < xoffset + module.x+MODULE_XOFFSET + MODULE_WIDTH &&
			    y > yoffset + module.y+MODULE_YOFFSET &&
			    y < yoffset + module.y+MODULE_YOFFSET + MODULE_HEIGHT)
			    {
				highlighted_command = 0;
				for(int k = 0; k < getModulesListItemCount(); k++)
				    {
					String item = getModulesListItem(k);
					if(null == item)
					    {
						continue;
					    }
					if(item.equals(module.Name))
					    {
						if(debug_on)
						    {
							DebugPrint("HierarchyPanel.mouseClicked() selecting module "+module+" at position "+k+" in modulesList.");
						    }
						modulesListSelect(k);
						if(modulesCountList != null)
						    {
							ItemEvent select_event = new ItemEvent(modulesCountList, ItemEvent.ITEM_STATE_CHANGED, Integer.valueOf(k),  ItemEvent.SELECTED);
							modulesCountList.dispatchEvent(select_event);
						    }
					    }
				    }
				if(design_mode)
				    {
					scroll_x = module.x - getSize().width/2+rect.x;
					scroll_y = module.y +MODULE_YOFFSET - MODULE_HEIGHT+rect.y;
					scroll_y += MODULE_YOFFSET - MODULE_HEIGHT;
					if(null != horzScrollbar)
					    {
						if(debug_on)
						    {
							DebugPrint("HierachyPanel.mouseClicked calling horzScrollbar.setValue("+scroll_x+")");
						    }
						horzScrollbar.setValue(scroll_x);
					    }
					if(null != vertScrollbar)
					    {
						if(debug_on)
						    {
							DebugPrint("HierachyPanel.mouseClicked calling vertScrollbar.setValue("+scroll_y+")");
						    }
						vertScrollbar.setValue(scroll_y);
					    }
				    }
				break;
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	repaint();
    }

    public void mouseMoved(MouseEvent evt)
    {
    }

    public void  mouseDragged(MouseEvent  evt)
    {
	try
	    {
		int x = evt.getX();
		int y = evt.getY();
		mouse_dragged_count++;
		mouse_dragged_x = x;
		mouse_dragged_y = y;
		// Compute the current x and y offsets
		if(debug_on)
		    {
			DebugPrint("mouseDrag event occurred at ("+x+", "+y+")");
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }
    
    public void handle_mouseDragged()
    {
	try
	    {
		int x = mouse_dragged_x;
		int y = mouse_dragged_y;
		last_mouse_dragged_count = mouse_dragged_count;
		Rectangle rect = getBounds();

		int xoffset = rect.x -scroll_x;
		int yoffset = rect.y -scroll_y;

		if(debug_on)
		    {
			DebugPrint("handle_mouseDragged: mouse_dragged_count="+mouse_dragged_count+",x="+x+",y="+y+",moduleShowingCommands="+moduleShowingCommands);
		    }

		// See if the mouse dragged inside one of the modules commands
		if(moduleShowingCommands != null)
		    {
			for(int i = 0; i < moduleShowingCommands.cmdsAvailable.size(); i++)
			    {
				int cmd_height = (int) (1.5*(getFontMetrics(getFont()).getHeight()));
				if(debug_on)
				    {
					DebugPrint("cmd_height="+cmd_height);
				    }
				if( x > xoffset + moduleShowingCommands.x+MODULE_XOFFSET &&
				    x < xoffset + moduleShowingCommands.x+MODULE_XOFFSET + MODULE_WIDTH &&
				    y > yoffset + moduleShowingCommands.y+MODULE_YOFFSET + MODULE_HEIGHT + i*cmd_height &&
				    y < yoffset + moduleShowingCommands.y+MODULE_YOFFSET + MODULE_HEIGHT + (i+1)*cmd_height)
				    {
					if(highlighted_command != i)
					    {
						highlighted_command = i;
						repaint();
					    }
					break;
				    }
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    public void mouseReleased(MouseEvent  evt)
    {
	try
	    {
		// Compute the current x and y offsets
		int x = evt.getX();
		int y = evt.getY();
		last_mouse_dragged_count = mouse_dragged_count;
		mouse_released_x = x;
		mouse_released_y = y;
		mouse_released_count++;
		if(debug_on)
		    {
			DebugPrint("mouseUp event occurred at ("+x+", "+y+") : moduleShowingCommands="+moduleShowingCommands);
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }
    
    void handle_mouseReleased()
    {
	try
	    {
		int x = mouse_released_x;
		int y = mouse_released_y;
		Rectangle rect = getBounds();
		int xoffset = rect.x -scroll_x;
		int yoffset = rect.y -scroll_y;
		last_mouse_released_count = mouse_released_count;
		last_mouse_dragged_count = mouse_dragged_count;
		last_mouse_pressed_count = mouse_pressed_count;

		if(debug_on)
		    {
			DebugPrint("handle_mouseReleased: mouse_released_count="+mouse_released_count+",x="+x+",y="+y+",moduleShowingCommands="+moduleShowingCommands);
		    }
		// See if the mouse dragged inside one of the modules commands
		if(moduleShowingCommands != null)
		    {
			if(debug_on)
			    {
				DebugPrint("moduleShowingCommands.cmdsAvailable.size()="+moduleShowingCommands.cmdsAvailable.size());
			    }
			for(int i = 0; i < moduleShowingCommands.cmdsAvailable.size(); i++)
			    {
				if(debug_on)
				    {
					DebugPrint("x("+x+") > xoffset + moduleShowingCommands.x+MODULE_XOFFSET("+(xoffset + moduleShowingCommands.x+MODULE_XOFFSET)+") &&  x("+x+") < xoffset + moduleShowingCommands.x+MODULE_XOFFSET + MODULE_WIDTH("+(xoffset + moduleShowingCommands.x+MODULE_XOFFSET + MODULE_WIDTH)+") &&   y("+y+") > yoffset + moduleShowingCommands.y+MODULE_YOFFSET + MODULE_HEIGHT + i*15("+(yoffset + moduleShowingCommands.y+MODULE_YOFFSET + MODULE_HEIGHT+ i*15)+") &&  (  y("+y+") < yoffset + moduleShowingCommands.y+MODULE_YOFFSET + MODULE_HEIGHT + (i+1)*15) ("+(yoffset + moduleShowingCommands.y+MODULE_YOFFSET + MODULE_HEIGHT + (i+1)*15)+") || i("+i+") >= moduleShowingCommands.cmdsAvailable.size()-1 ("+(moduleShowingCommands.cmdsAvailable.size()-1)+") ");
				    }
				if( x > xoffset + moduleShowingCommands.x+MODULE_XOFFSET &&
				    x < xoffset + moduleShowingCommands.x+MODULE_XOFFSET + MODULE_WIDTH &&
				    y > yoffset + moduleShowingCommands.y+MODULE_YOFFSET + MODULE_HEIGHT + i*15 &&
				    ((y < yoffset + moduleShowingCommands.y+MODULE_YOFFSET + MODULE_HEIGHT + (i+1)*15) ||
				     i >= moduleShowingCommands.cmdsAvailable.size()-1)
				    )
				    {
					String command_to_send_name = (String) moduleShowingCommands.cmdsAvailable.elementAt(i);
					int eq_index = command_to_send_name.indexOf('=');
					if(eq_index < 0)
					    {
						break;
					    }
					command_to_send_name = command_to_send_name.substring(eq_index+1);
					long cmd_id = Long.valueOf(command_to_send_name).longValue();
					if(debug_on)
					    {
						DebugPrint("moduleShowingCommands.Name="+moduleShowingCommands.Name+", moduleShowingCommands.echo_serial_number="+moduleShowingCommands.serial_number+",  moduleShowingCommands.echo_serial_number="+ moduleShowingCommands.echo_serial_number+",cmd_id="+cmd_id);
					    }
					moduleShowingCommands.serial_number++;
					int serial_number = moduleShowingCommands.serial_number;
					if(moduleShowingCommands.echo_serial_number+1 > serial_number)
					    {
						serial_number = moduleShowingCommands.echo_serial_number+1;
						moduleShowingCommands.serial_number = moduleShowingCommands.echo_serial_number+1;
					    }
					if(serial_number < 2)
					    {
						moduleShowingCommands.serial_number = 2;
						serial_number=2;
					    }
						
					StringTokenizer data_tokenizer = null;
					try
					    {
						//DebugPrint("Retrieving PrevCmd. Id = "+cmd_id);
						String prevCmdString = (String) moduleShowingCommands.previous_commands.get(Long.valueOf(cmd_id));
						if(debug_on)
						    {
							DebugPrint("prevCmdString="+prevCmdString);
						    }
						//DebugPrint("Retrieving PrevCmd. : "+prevCmdString);
						if(null != prevCmdString)
						    {
							data_tokenizer = new StringTokenizer(prevCmdString,",");
							data_tokenizer.nextToken();
							data_tokenizer.nextToken();
							data_tokenizer.nextToken();
						    }
					    }
					catch(Exception e)
					    {
						e.printStackTrace();
					    }
					String string_to_send = cmd_id+",0,"+serial_number+",";
					if(null != data_tokenizer)
					    {
						while(data_tokenizer.hasMoreTokens())
						    {
							string_to_send += data_tokenizer.nextToken() + ",";
						    }
					    }
					moduleShowingCommands.sending_short_string=true;
					if(debug_on)
					    {
						DebugPrint("string_to_send="+string_to_send);
					    }
					//diagappletMain.sending_command=true;
					int starting_cmd_count = moduleShowingCommands.new_cmd_count;
					moduleShowingCommands.writeCmd(string_to_send);
					moduleShowingCommands.sending_short_string=false;
					for(int ik= 0 ; ik < 50; ik++)
					    {
						if(moduleShowingCommands.new_cmd_count > starting_cmd_count)
						    {
							break;
						    }
						// if(!diagappletMain.sending_command)
// 						    {
// 							break;
// 						    }
						moduleShowingCommands.updateCmdData();
						try
						    {
							Thread.sleep(10);
						    }
						catch(Exception e)
						    {
							break;
						    }
					    }
					int starting_stat_count = moduleShowingCommands.new_stat_count;

					for(int ij= 0 ; ij < 50; ij++)
					    {
						if(moduleShowingCommands.new_stat_count > starting_stat_count)
						    {
							break;
						    }
						// if(!diagappletMain.sending_command)
// 						    {
// 							break;
// 						    }
						moduleShowingCommands.updateStatData();
						try
						    {
							Thread.sleep(10);
						    }
						catch(Exception e)
						    {
							break;
						    }
					    }
					//diagappletMain.sending_command=false;
					force_next_repaint=true;
					break;
				    }
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	moduleShowingCommands = null;
	highlighted_command = 0;
	repaint();
    }

    long last_notify_millis=0;
    long last_paint_millis=0;

    public boolean UpdateDisplay(boolean force_repaint )
    {
	ModuleInfo moduleToUpdate;
	boolean statFound = false;
	boolean negative_one_status_found = false;
	boolean repaint_needed = force_repaint;
	int start_module;
	int modules_checked = 0;
	// if(diagappletMain.sending_command)
	// 	    {
	// 		try
	// 		    {
	// 			Thread.sleep(10);
	// 		    }
	// 		catch(Exception e)
	// 		    {
	// 		    }
	// 		return false;
	// 	    }
	if(repaint_count > 0)
	    {
		repaint_needed = force_repaint = true;
	    }
	if(mouse_pressed_count > mouse_released_count &&
	   mouse_pressed_count > last_mouse_pressed_count)
	    {
		handle_mousePressed();
	    }
	else if(mouse_released_count > last_mouse_released_count)
	    {
		handle_mouseReleased();
	    }
	else if(mouse_dragged_count > last_mouse_dragged_count)
	    {
		handle_mouseDragged();
	    }
	else if(mouse_clicked_count > last_mouse_clicked_count)
	    {
		handle_mouseClicked();
	    }

	if(null == modules)
	    {
		if(painting)
		    {
			return repaint_needed;
		    }
		if(repaint_needed)
		    {
			Graphics cur_g = getGraphics();
			if(null != cur_g)
			    {
				paintHierarchy(getGraphics());
				if(!painting)
				    {
					try{synchronized(cur_g){cur_g.notifyAll();}}catch(Exception eNotify){}
				    }
			    }
			else
			    {
				repaint();
			    }
		    }
		return repaint_needed;
	    }
	if(modules.size() < 1)
	    {
		if(painting)
		    {
			return repaint_needed;
		    }
		if(repaint_needed)
		    {
			Graphics cur_g = getGraphics();
			if(null != cur_g)
			    {
				paintHierarchy(getGraphics());
				if(!painting)
				    {
					try{synchronized(cur_g){cur_g.notifyAll();}}catch(Exception eNotify){}
				    }
			    }
			else
			    {
				repaint();
			    }
		    }
		return repaint_needed;
	    }
	if(current_module > 0)
	    {
		start_module = current_module -1;
	    }
	else
	    {
		start_module = modules.size()-1;
	    }

	if(debug_on)
	    {
		DebugPrint("HierarchyPanel.UpdateDisplay() called with lastx="+lastx+",scroll_x="+scroll_x+",lasty="+lasty+",scroll_y="+scroll_y+", size="+getSize());
	    }
	if(lastx != scroll_x)
	    {
		repaint_needed = true;
		lastx = scroll_x;
	    }
	if(lasty != scroll_y)
	    {
		repaint_needed = true;
		lasty = scroll_y;
	    }

	while(
	      (current_module != start_module || modules_checked < 1)
	      )
	    {
		try
		    {
			if(debug_on)
			    {
				DebugPrint("in-loop: current_module="+current_module+", start_module="+start_module+",modules_checked="+modules_checked);
			    }
			current_module++;
			modules_checked ++;
			if(current_module >= modules.size())
			    {
				current_module = 0;
			    }
			if(current_module < 0)
			    {
				current_module = 0;
			    }
			moduleToUpdate = (ModuleInfo) modules.elementAt(current_module);
			if(null == moduleToUpdate)
			    {
				break;
			    }
			if(!moduleToUpdate.is_connected)
			    {
				continue;
			    }
			if(debug_on)
			    {
				DebugPrint("HierarchyPanel.update(): moduleToUpdate.Name = "+moduleToUpdate.Name);
				DebugPrint("HierarchyPanel.update(): updateCmdData()");
			    }
			if(moduleToUpdate != moduleShowingCommands)
			    {
				moduleToUpdate.updateCmdData();
			    }
			if(debug_on)
			    {
				DebugPrint("HierarchyPanel.update(): updateStatData()");
			    }
			moduleToUpdate.updateStatData();
			long cur_time = System.currentTimeMillis();
			moduleToUpdate.new_command = false;
			if(moduleToUpdate.last_serial_number_displayed != moduleToUpdate.serial_number)
			    {
				repaint_needed = true;
				moduleToUpdate.last_new_command_time = cur_time;
				moduleToUpdate.new_command = true;
			    }
			if(cur_time - moduleToUpdate.last_new_command_time < new_threshold
			   && cur_time >= moduleToUpdate.last_new_command_time)
			    {
				moduleToUpdate.new_command = true;
			    }
			if(moduleToUpdate.new_command != moduleToUpdate.new_command_drawn)
			    {
				repaint_needed = true;
			    }
			moduleToUpdate.new_status = false;
			if(moduleToUpdate.last_echo_serial_number_displayed != moduleToUpdate.echo_serial_number)
			    {
				repaint_needed = true;
				moduleToUpdate.last_new_stat_time = cur_time;
				moduleToUpdate.new_status = true;
			    }
			if(cur_time - moduleToUpdate.last_new_stat_time < new_threshold
			   && cur_time >= moduleToUpdate.last_new_stat_time)
			    {
				moduleToUpdate.new_status = true;
			    }
			if(moduleToUpdate.new_status != moduleToUpdate.new_status_drawn)
			    {
				//DebugPrint("module = "+moduleToUpdate.Name+", new_status = "+moduleToUpdate.new_status+", new_status_drawn = "+moduleToUpdate.new_status_drawn);
				//DebugPrint("module = "+moduleToUpdate.Name+", last_new_stat_time = "+moduleToUpdate.last_new_stat_time+", cur_time = "+cur_time+", diff ="+(cur_time-moduleToUpdate.last_new_stat_time));
				repaint_needed = true;
			    }
			if(null != moduleToUpdate.cmdData)
			    {
				int index = moduleToUpdate.cmdData.indexOf(',');
				if(index > 0)
				    {
					String idString = moduleToUpdate.cmdData.substring(0,index);
					while(true)
					    {
						if(idString.length() < 1)
						    {
							break;
						    }
						if(idString.charAt(0) != ' ' &&
						   idString.charAt(0) != '+')
						    {
							break;
						    }
						idString = idString.substring(1);
					    }
					Long idLong = Long.valueOf(rcs.utils.StrToLong.convert(idString));
					if(moduleToUpdate != null &&
					   ModuleInfo.m_cmd_structInfoHashTable != null)
					    {
						StructureTypeInfo cmdInfo = 
						    (StructureTypeInfo) ModuleInfo.m_cmd_structInfoHashTable.get(idLong);
						if(cmdInfo.conflicts)
						    {
							if(null != moduleToUpdate && 
							   null != moduleToUpdate.get_conflict_m_structInfoHashTable() &&
							   null != moduleToUpdate.get_conflict_m_structInfoHashTable().get(idLong))
							    {
								cmdInfo =(StructureTypeInfo) moduleToUpdate.get_conflict_m_structInfoHashTable().get(idLong);
							    }
						    }
						if(null != cmdInfo)
						    {
							moduleToUpdate.cmd_msg_type = idLong.longValue();
							if(null != cmdInfo.getName())
							    {
								if(null ==  moduleToUpdate.lastCmdName)
								    {
									moduleToUpdate.lastCmdName = cmdInfo.getName();
									repaint_needed = true;
								    }
								else if(0 != cmdInfo.getName().compareTo(moduleToUpdate.lastCmdName))
								    {
									moduleToUpdate.lastCmdName = cmdInfo.getName();
									repaint_needed = true;
								    }
							    }
							else
							    {
								if(null ==  moduleToUpdate.lastCmdName)
								    {
									moduleToUpdate.lastCmdName = idString;
									repaint_needed = true;
								    }
								else if(0 != idString.compareTo(moduleToUpdate.lastCmdName))
								    {
									moduleToUpdate.lastCmdName = idString;
									repaint_needed = true;
								    }
							    }

						    }
						else
						    {
							if(null ==      moduleToUpdate.lastCmdName)
							    {
								moduleToUpdate.lastCmdName = idString;
								repaint_needed = true;
							    }
							else if(0 != idString.compareTo(moduleToUpdate.lastCmdName))
							    {
								moduleToUpdate.lastCmdName = idString;
								repaint_needed = true;
							    }
						    }
					    }
				    }
			    }

			// STATUS
			if(null != moduleToUpdate.statData)
			    {
				negative_one_status_found = false;
				int dcIndex = -1;
				while(-1 != (dcIndex = moduleToUpdate.statData.indexOf(",,")))
				    {
					String firstPartString = moduleToUpdate.statData.substring(0,dcIndex);
					String secondPartString = "";
					if(dcIndex+2 <  moduleToUpdate.statData.length())
					    {
						secondPartString = moduleToUpdate.statData.substring(dcIndex+2);
					    }
					moduleToUpdate.statData = firstPartString + ",(null)," + secondPartString;
				    }
				int index = moduleToUpdate.statData.indexOf(',');
				if(index > 0)
				    {
					String idString = moduleToUpdate.statData.substring(0,index);
					while(true)
					    {
						if(idString.length() < 1)
						    {
							break;
						    }
						if(idString.charAt(0) != ' ' &&
						   idString.charAt(0) != '+')
						    {
							break;
						    }
						idString = idString.substring(1);
					    }
					Long idLong = Long.valueOf(0);
					try
					    {
						idLong = Long.valueOf(rcs.utils.StrToLong.convert(idString));
					    }
					catch(Exception e)
					    {
						System.err.println("Invalid idString in statData "+moduleToUpdate.statData);
						e.printStackTrace();
						break;
					    }
					if(null == ModuleInfo.m_stat_structInfoHashTable)
					    {
						ModuleInfo.m_stat_structInfoHashTable = ModuleInfo.m_structInfoHashTable;
					    }
					StructureTypeInfo statInfo = (StructureTypeInfo) ModuleInfo.m_stat_structInfoHashTable.get(idLong);
					if(null != statInfo)
					    {
						if(statInfo.conflicts)
						    {
							if(null != moduleToUpdate && 
							   null != moduleToUpdate.get_conflict_m_structInfoHashTable() &&
							   null != moduleToUpdate.get_conflict_m_structInfoHashTable().get(idLong))
							    {
								statInfo =(StructureTypeInfo) moduleToUpdate.get_conflict_m_structInfoHashTable().get(idLong);
							    }
						    }
						moduleToUpdate.stat_msg_type = idLong.longValue();
						moduleToUpdate.status_type_name = statInfo.getName();
						StringTokenizer dataTokenizer =
						    new StringTokenizer(moduleToUpdate.statData,",");
						statInfo.startInfoTokens();
						String dataString = dataTokenizer.nextToken();
						dataString = dataTokenizer.nextToken();
						if(debug_on)
						    {
							DebugPrint("HierarchyPanel.update(): statInfo.RawInfo = "+statInfo.RawInfo);
							DebugPrint("HierarchyPanel.update(): statInfo.HiddenInfo = "+statInfo.HiddenInfo);
							DebugPrint("HierarchyPanel.update(): moduleToUpdate.statData = "+moduleToUpdate.statData);
						    }
						STI_TokenizerInterface stiti = statInfo.getInfoTokenizer();
						while(dataTokenizer.hasMoreTokens() && stiti.hasMoreTokens() && !statFound)
						    {
							dataString = dataTokenizer.nextToken();
							if(dataString == null)
							    {
								break;
							    }
							String infoString = stiti.nextToken();
							if(infoString == null)
							    {
								break;
							    }
							if(infoString.endsWith(" status")  || infoString.endsWith(".status"))
							    {
								if(debug_on)
								    {
									DebugPrint("HierarchyPanel.update(): infoString = "+infoString+", dataString = "+dataString);
								    }
								while(true)
								    {
									if(dataString.length() < 1)
									    {
										break;
									    }
									if(dataString.charAt(0) != ' ' &&
									   dataString.charAt(0) != '+')
									    {
										break;
									    }
									dataString = dataString.substring(1);
								    }
								Long statLong = Long.valueOf(0);
								try
								    {
									statLong = Long.valueOf(rcs.utils.StrToLong.convert(dataString));
									switch(statLong.intValue())
									    {
									    case -1:
										negative_one_status_found = true;
										statFound=false;
										break;

									    case 0:
										//DebugPrint("HierarchyPanel.update(): this status variable is 0 so keep looking.");
										statFound = false;
										break;
									    case 1:
										if(null == moduleToUpdate.lastStatName)
										    {
											moduleToUpdate.lastStatName = "DONE";
											repaint_needed = true;
										    }
										else if(0 != moduleToUpdate.lastStatName.compareTo("DONE"))
										    {
											moduleToUpdate.lastStatName = "DONE";
											repaint_needed = true;
										    }
										statFound = true;
										break;

									    case 2:
										if(null == moduleToUpdate.lastStatName)
										    {
											moduleToUpdate.lastStatName = "EXEC";
											repaint_needed = true;
										    }
										else if(0 != moduleToUpdate.lastStatName.compareTo("EXEC"))
										    {
											moduleToUpdate.lastStatName = "EXEC";
											repaint_needed = true;
										    }
										statFound = true;
										break;

									    case 3:
										if(null == moduleToUpdate.lastStatName)
										    {
											moduleToUpdate.lastStatName = "ERROR";
											repaint_needed = true;
										    }
										else if(0 != moduleToUpdate.lastStatName.compareTo("ERROR"))
										    {
											moduleToUpdate.lastStatName = "ERROR";
											repaint_needed = true;
										    }
										statFound = true;
										break;
									    default:
										statFound = true;
										if(null == moduleToUpdate.lastStatName)
										    {
											moduleToUpdate.lastStatName = dataString;
											repaint_needed = true;
										    }
										else if(0 != moduleToUpdate.lastStatName.compareTo(dataString))
										    {
											moduleToUpdate.lastStatName = dataString;
											repaint_needed = true;
										    }
										break;
									    }
								    }
								catch(Exception e)
								    {
									e.printStackTrace();
								    }
							    }
						    }
						if(negative_one_status_found && !statFound)
						    {
							if(null == moduleToUpdate.lastStatName)
							    {
								moduleToUpdate.lastStatName = "UNINITIALIZED";
								repaint_needed = true;
							    }
							else if(0 != moduleToUpdate.lastStatName.compareTo("UNINITIALIZED"))
							    {
								moduleToUpdate.lastStatName = "UNINITIALIZED";
								repaint_needed = true;
							    }
						    }
					    }
					else
					    {
						String temp;
						temp = "unknown NML msg("+idString+")";
						if(null == moduleToUpdate.lastStatName)
						    {
							moduleToUpdate.lastStatName = temp;
							repaint_needed = true;
						    }
						else if(0 != moduleToUpdate.lastStatName.compareTo(temp))
						    {
							moduleToUpdate.lastStatName = temp;
							repaint_needed = true;
						    }
					    }
				    }
			    }
		    }
		catch(Exception e)
		    {
			System.err.println("Error updating module = "+current_module);
			e.printStackTrace();
			break;
		    }
	    }
	if(debug_on)
	    {
		DebugPrint("finished: current_module="+current_module+", start_module="+start_module+",modules_checked="+modules_checked);
	    }
	if(painting)
	    {
		return repaint_needed;
	    }
	if(repaint_needed || 
	   (System.currentTimeMillis() - last_notify_millis > 100)  ||
	   (System.currentTimeMillis() - last_paint_millis > 5000))
	    {
		if(force_repaint || force_next_repaint ||
		   (System.currentTimeMillis() - last_paint_millis > 5000))
		    {
			repaint();
		    }
		else
		    {
			Graphics cur_g = getGraphics();
			if(null != cur_g)
			    {
				partial_paint=
				    ((repaint_needed && paint_hierarchy_count%5 != 0) || 
				     (moduleShowingCommands != null));
				if(debug_on)
				    {
					DebugPrint("partial_paint="+partial_paint);
				    }
				paintHierarchy(getGraphics());
				partial_paint=false;
				if(System.currentTimeMillis() - last_notify_millis > 100 && !painting)
				    {
					try{synchronized(cur_g){cur_g.notifyAll();}
					last_notify_millis=System.currentTimeMillis();}catch(Exception eNotify){}
				    }
			    }
			else
			    {
				repaint();
			    }
		    }
	    }
	return repaint_needed;
    }
    public Frame parentFrame = null;

    boolean printing = false;
    public void print()
    {
	try
	    {
		DebugPrint("Printing Hierarchy . . .");
		if(null == parentFrame)
		    {
			try
			    {
				parentFrame = (Frame) getParent();
			    }
			catch(ClassCastException e)
			    {
				parentFrame = new Frame();
			    }
		    }
		int orig_scroll_x = scroll_x;
		int orig_scroll_y = scroll_y;
		PrintJob pj = Toolkit.getDefaultToolkit().getPrintJob(parentFrame, "print1", null);
		if(null == pj)
		    {
			DebugPrint("PrintJob is null. (Canceled?)");
			return;
		    }
		Dimension page_dim = pj.getPageDimension();
		DebugPrint("Printing Hierarchy "+page_dim.width+"x"+page_dim.height);
		int page_width = page_dim.width;
		int page_height = page_dim.height;
		int pages = 0;
		if (pj != null)
		    {
			for(scroll_x = min_x; scroll_x < max_x; scroll_x += (page_width*0.95) )
			    {
				for(scroll_y = min_y; scroll_y < max_y; scroll_y += (page_height*0.95) )
				    {
					Graphics g = pj.getGraphics();
					if(null == g.getFont())
					    {
						g.setFont(this.getFont());
					    }
					printing = true;
					boolean orig_use_color = use_color;
					use_color = false;
					updating_hierarchy=false;
					paint(g);
					use_color = orig_use_color;
					printing = false;
					g.dispose();
					pages++;
					DebugPrint(pages+" pages printed.");
				    }
			    }
			DebugPrint("Hierarchy Printed.");
			pj.end();
		    }
		scroll_x = orig_scroll_x;
		scroll_y = orig_scroll_y;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }
    Dimension last_temp_d = null;

    public void paint(Graphics g)
    {
	try
	    {
		partial_paint=false;
		paintHierarchy(g);
		if(null != modules && modules.size() > 0)
		    {
			force_next_repaint=false;
		    }
		last_notify_millis=System.currentTimeMillis();
		last_paint_millis = last_notify_millis;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public void paintHierarchy(Graphics g)
    {
	try
	    {
		if(g == null)
		    {
			return;
		    }
		long m1,m2,m3,m4;
		Graphics current_g = g;
		long cur_time = System.currentTimeMillis();
		m1 = cur_time;
		m2= cur_time;
		m3=cur_time;
		if(updating_hierarchy || painting)
		    {
			return;
		    }
		painting=true;
		if(null == modules)
		    {
			return;
		    }
		paint_hierarchy_count++;
		if(debug_on)
		    {
			DebugPrint("paint_hierarchy_count="+paint_hierarchy_count);
		    }
		if(repaint_count > 0)
		    {
			repaint_count --;
		    }
		Rectangle rect = getBounds();

		int xoffset = rect.x -scroll_x;
		Dimension temp_d = getSize();
		int yoffset = rect.y -scroll_y;
		
		if(use_buffer_image)
		    {
			m1 = System.currentTimeMillis();
			if(null != buffer_image)
			    {
				g.drawImage(buffer_image, 0,0, temp_d.width, temp_d.height,this);
			    }
			m2 = System.currentTimeMillis();
			if(buffer_image == null || 
			   (last_temp_d != null && (last_temp_d.width != temp_d.width || last_temp_d.height != temp_d.height)))
			    {
				buffer_image =  createImage(temp_d.width, temp_d.height);
			    }
			m3 = System.currentTimeMillis();
			current_g = buffer_image.getGraphics();
			last_temp_d = temp_d;
		    }
		current_g.setColor(Color.black);
		for(int i = 0; i < modules.size(); i++)
		    {
			ModuleInfo module = (ModuleInfo) modules.elementAt(i);
			if(null == module)
			    {
				continue;
			    }
			if(debug_on)
			    {
				DebugPrint("1227: i="+i+",partial_paint="+partial_paint);
			    }
			if(partial_paint && i == 0)
			    {
				module = (ModuleInfo) modules.elementAt(current_module);
			    }
			else if(partial_paint && i == 1 && 
				null != moduleShowingCommands && 
				moduleShowingCommands != ((ModuleInfo) modules.elementAt(current_module)) )
			    {
				module = moduleShowingCommands;
			    }
			else if(partial_paint && i > 0)
			    {
				break;
			    }
			module.line_to_sub_drawn = false;
			if((!module.new_status && !module.new_command 
			   && module.echo_serial_number == module.last_echo_serial_number_displayed) 
			   || !use_color || printing)
			    {
				current_g.drawRect(xoffset + module.x+MODULE_XOFFSET-1, yoffset + module.y+MODULE_YOFFSET-1, MODULE_WIDTH+1,MODULE_HEIGHT+1);
				if(null != module.parent)
				    {
					current_g.drawLine(xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET,
							   yoffset + module.y+MODULE_YOFFSET,
							   xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET,
							   yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2);

					current_g.drawLine(xoffset + module.x+MODULE_WIDTH/2 +MODULE_XOFFSET,
							   yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2,
							   xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET,
							   yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2);

					current_g.drawLine(xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET,
							   yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2,
							   xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET,
							   yoffset + module.parent.y+MODULE_YOFFSET+MODULE_HEIGHT);
				    }
			    }
		    }
		if(use_color)
		    {
			current_g.setColor(Color.magenta);
		    }

		if(null == modules)
		    {
			return;
		    }
		for(int i = 0; i < modules.size(); i++)
		    {
			ModuleInfo module = (ModuleInfo) modules.elementAt(i);
			
			if(debug_on)
			    {
				DebugPrint("1278: i="+i+",partial_paint="+partial_paint);
			    }

			if(partial_paint && i == 0)
			    {
				module = (ModuleInfo) modules.elementAt(current_module);
			    }
			else if(partial_paint && i == 1 && 
				null != moduleShowingCommands && 
				moduleShowingCommands != ((ModuleInfo) modules.elementAt(current_module)) )
			    {
				module = moduleShowingCommands;
			    }
			else if(partial_paint && i > 0)
			    {
				break;
			    }
			if(null == module)
			    {
				continue;
			    }
			if(null == module.ModulesReadingAuxOutput)
			    {
				continue;
			    }
			for(int j = 0; j < module.ModulesReadingAuxOutput.size(); j++)
			    {
				ModuleInfo auxmodule = (ModuleInfo) module.ModulesReadingAuxOutput.elementAt(j);
				current_g.drawLine(xoffset + auxmodule.x+MODULE_WIDTH/2+MODULE_XOFFSET, yoffset + auxmodule.y+MODULE_YOFFSET,
					   xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET,    yoffset + module.y+MODULE_YOFFSET+MODULE_HEIGHT);
			    }
		    }

		for(int i = 0; i < modules.size(); i++)
		    {
			ModuleInfo module = (ModuleInfo) modules.elementAt(i);
			
			if(debug_on)
			    {
				DebugPrint("1317: i="+i+",partial_paint="+partial_paint);
			    }
			if(partial_paint && i == 0)
			    {
				module = (ModuleInfo) modules.elementAt(current_module);
			    }
			else if(partial_paint && i == 1 && 
				null != moduleShowingCommands && 
				moduleShowingCommands != ((ModuleInfo) modules.elementAt(current_module)) )
			    {
				module = moduleShowingCommands;
			    }
			else if(partial_paint && i > 0)
			    {
				break;
			    }

			if(null == module)
			    {
				continue;
			    }
			if(partial_paint && use_color)
			    {
				current_g.setColor(Color.blue);
				current_g.fillRect(xoffset + module.x+MODULE_XOFFSET, 
						   yoffset + module.y+MODULE_YOFFSET -getFontMetrics(getFont()).getHeight(), 
						   MODULE_WIDTH,
						   getFontMetrics(getFont()).getHeight());
			    }
			if(use_color && module.is_connected)
			    {
				boolean new_command = module.new_command;
				boolean new_status = module.new_status;
				if(module.last_serial_number_displayed != module.serial_number)
				    {
					current_g.setColor(Color.red);
					module.last_serial_number_displayed = module.serial_number;
					module.last_new_command_time = cur_time;
					new_command = true;
				    }
				if(cur_time - module.last_new_command_time < new_threshold
				   && cur_time > module.last_new_command_time)
				    {
					current_g.setColor(Color.red);
					new_command = true;
				    }
				module.last_serial_number_displayed = module.serial_number;
				if(!printing)
				    {
					current_g.drawString(String.valueOf(module.serial_number), 
						     xoffset + module.x,                  
						     yoffset + module.y+MODULE_YOFFSET);
				    }
				if(new_command && null != module.parent)
				    {
					current_g.drawRect(xoffset + module.x+MODULE_XOFFSET-1, yoffset + module.y+MODULE_YOFFSET-1, MODULE_WIDTH+1,MODULE_HEIGHT+1);
					current_g.drawLine(xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET-1,                  yoffset + module.y+MODULE_YOFFSET,
						   xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET-1,                  yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2);
					current_g.drawLine(xoffset + module.x+MODULE_WIDTH/2 +MODULE_XOFFSET,                   yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2-1,
						   xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET,     yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2);
					current_g.drawLine(xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET-1,   yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2-1,
						   xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET-1,   yoffset + module.parent.y+MODULE_YOFFSET+MODULE_HEIGHT);
				    }
				current_g.setColor(Color.black);
				if(module.last_echo_serial_number_displayed != module.echo_serial_number)
				    {
					current_g.setColor(Color.yellow);
					module.last_echo_serial_number_displayed = module.echo_serial_number;
					module.last_new_echo_serial_time = cur_time;
					new_status = true;
				    }
				if(cur_time - module.last_new_echo_serial_time < new_threshold
				   && cur_time > module.last_new_echo_serial_time)
				    {
					current_g.setColor(Color.yellow);
					new_status = true;
				    }
				module.last_echo_serial_number_displayed = module.echo_serial_number;
				if(!printing)
				    {
					current_g.drawString(String.valueOf(module.echo_serial_number),
						     xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET+5,
						     yoffset + module.y+MODULE_YOFFSET);
				    }
				if(cur_time - module.last_new_stat_time < new_threshold
				   && cur_time > module.last_new_stat_time)
				    {
					current_g.setColor(Color.yellow);
					new_status = true;
				    }
				if(new_status && !new_command && null != module.parent)
				    {
					current_g.drawLine(xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET,                    yoffset + module.y+MODULE_YOFFSET,
						   xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET,                    yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2);
					current_g.drawLine(xoffset + module.x+MODULE_WIDTH/2 +MODULE_XOFFSET,                   yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2,
						   xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET,     yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2);
					current_g.drawLine(xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET,     yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2,
						   xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET,     yoffset + module.parent.y+MODULE_YOFFSET+MODULE_HEIGHT);
				    }
				module.new_status_drawn = module.new_status = new_status;
				module.new_command_drawn = module.new_command = new_command;
			    }
			else
			    {
				module.last_serial_number_displayed = module.serial_number;
				module.last_echo_serial_number_displayed = module.echo_serial_number;
				module.new_status_drawn = module.new_status;
				module.new_command_drawn = module.new_command;
				if(!printing)
				    {
					current_g.drawString(String.valueOf(module.echo_serial_number),
						     xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET+5,
							     yoffset + module.y+MODULE_YOFFSET);
					current_g.drawString(String.valueOf(module.serial_number),
						     xoffset + module.x,
						     yoffset + module.y+MODULE_YOFFSET);
				    }
			    }
			boolean module_done_or_executing=false;
			if(!use_color)
			    {
				current_g.setColor(Color.white);
			    }
			else
			    {
				if(module.lastStatName == null)
				    {
					current_g.setColor(Color.pink);
				    }
				else if(module.lastStatName.startsWith("ERROR"))
				    {
					current_g.setColor(Color.red);
				    }
				else if(module.lastStatName.startsWith("EXEC"))
				    {
					current_g.setColor(Color.green);
					module_done_or_executing=true;
				    }
				else if(module.lastStatName.startsWith("DONE"))
				    {
					current_g.setColor(Color.white);
					module_done_or_executing=true;
				    }
				else 
				    {
					current_g.setColor(Color.pink);
				    }
			    }
			if( (cur_time - module.last_new_stat_time) > (1000 + new_threshold)
			   && module.last_new_stat_time < last_notify_millis 
			   && module_done_or_executing)
			    {
				current_g.setColor(Color.yellow);
			    }
			if(!module.is_connected)
			    {
				current_g.setColor(Color.lightGray);
			    }
			boolean is_selected = false;
			if(null != modulesList)
			    {
				if(module.Name.equals(getModulesListSelectedItem()))
				    {
					is_selected = true;
				    }
			    }
			Color textColor = Color.black;
			if(is_selected && !printing)
			    {
				Color curColor = current_g.getColor();
				current_g.setColor(Color.darkGray);
				current_g.fillRect(xoffset + module.x+MODULE_XOFFSET-(MODULE_X_SPACING/4), 
					   yoffset + module.y+MODULE_YOFFSET-(MODULE_Y_SPACING/4), 
					   MODULE_WIDTH + (MODULE_X_SPACING/2),
					   MODULE_HEIGHT + (MODULE_Y_SPACING/2));
				current_g.setColor(curColor);
			    }

			if(!printing)
			    {
				current_g.fillRect(xoffset + module.x+MODULE_XOFFSET, yoffset + module.y+MODULE_YOFFSET, MODULE_WIDTH,MODULE_HEIGHT);
			    }
			else
			    {
				current_g.drawRect(xoffset + module.x+MODULE_XOFFSET, yoffset + module.y+MODULE_YOFFSET, MODULE_WIDTH,MODULE_HEIGHT);
			    }
			current_g.setColor(textColor);
			if(use_color && module.is_connected)
			    {
				boolean new_command = module.new_command;
				boolean new_status = module.new_status;
				if(module.last_serial_number_displayed != module.serial_number)
				    {
					current_g.setColor(Color.red);
					module.last_serial_number_displayed = module.serial_number;
					module.last_new_command_time = cur_time;
					new_command = true;
				    }
				if(cur_time - module.last_new_command_time < new_threshold
				   && cur_time > module.last_new_command_time)
				    {
					current_g.setColor(Color.red);
					new_command = true;
				    }
				module.last_serial_number_displayed = module.serial_number;
				if(!printing)
				    {
					current_g.drawString(String.valueOf(module.serial_number), 
						     xoffset + module.x,                  
						     yoffset + module.y+MODULE_YOFFSET);
				    }
				if(new_command && null != module.parent)
				    {
					current_g.drawRect(xoffset + module.x+MODULE_XOFFSET-1, yoffset + module.y+MODULE_YOFFSET-1, MODULE_WIDTH+1,MODULE_HEIGHT+1);
					current_g.drawLine(xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET-1,                  yoffset + module.y+MODULE_YOFFSET,
						   xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET-1,                  yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2);
					current_g.drawLine(xoffset + module.x+MODULE_WIDTH/2 +MODULE_XOFFSET,                   yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2-1,
						   xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET,     yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2);
					current_g.drawLine(xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET-1,   yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2-1,
						   xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET-1,   yoffset + module.parent.y+MODULE_YOFFSET+MODULE_HEIGHT);
				    }
				current_g.setColor(Color.black);
				if(module.last_echo_serial_number_displayed != module.echo_serial_number)
				    {
					current_g.setColor(Color.yellow);
					module.last_echo_serial_number_displayed = module.echo_serial_number;
					module.last_new_echo_serial_time = cur_time;
					new_status = true;
				    }
				if(cur_time - module.last_new_echo_serial_time < new_threshold
				   && cur_time > module.last_new_echo_serial_time)
				    {
					current_g.setColor(Color.yellow);
					new_status = true;
				    }
				module.last_echo_serial_number_displayed = module.echo_serial_number;
				if(!printing)
				    {
					current_g.drawString(String.valueOf(module.echo_serial_number), 
						     xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET+5,
						     yoffset + module.y+MODULE_YOFFSET);
				    }
				if(cur_time - module.last_new_stat_time < new_threshold
				   && cur_time > module.last_new_stat_time)
				    {
					current_g.setColor(Color.yellow);
					new_status = true;
				    }
				if(new_status && !new_command && null != module.parent)
				    {
					current_g.drawLine(xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET,                    yoffset + module.y+MODULE_YOFFSET,
						   xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET,                    yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2);
					current_g.drawLine(xoffset + module.x+MODULE_WIDTH/2 +MODULE_XOFFSET,                   yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2,
						   xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET,     yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2);
					current_g.drawLine(xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET,     yoffset + (module.y + module.parent.y)/2+MODULE_YOFFSET+MODULE_HEIGHT/2,
						   xoffset + module.parent.x+MODULE_WIDTH/2+MODULE_XOFFSET,     yoffset + module.parent.y+MODULE_YOFFSET+MODULE_HEIGHT);
				    }
				module.new_status_drawn = module.new_status = new_status;
				module.new_command_drawn = module.new_command = new_command;
			    }
			else
			    {
				module.last_serial_number_displayed = module.serial_number;
				module.last_echo_serial_number_displayed = module.echo_serial_number;
				module.new_status_drawn = module.new_status;
				module.new_command_drawn = module.new_command;
				if(!printing)
				    {
					current_g.drawString(String.valueOf(module.echo_serial_number), xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET+5,
						     yoffset + module.y+MODULE_YOFFSET);
					current_g.drawString(String.valueOf(module.serial_number), xoffset + module.x,
						     yoffset + module.y+MODULE_YOFFSET);
				    }
			    }
			current_g.setColor(textColor);
			try
			    {
				current_g.drawString(module.Name,xoffset + module.x , 
					     yoffset + module.y + (getFontMetrics(getFont()).getHeight())/2);
				if(null != module.lastCmdName)
				    {
					String cmd_name= module.lastCmdName;
					if(null != module.common_cmd_base &&
					   cmd_name.length() > module.common_cmd_base.length() &&
					   cmd_name.substring(0,module.common_cmd_base.length()).equalsIgnoreCase(module.common_cmd_base))
					    {
						cmd_name = cmd_name.substring(module.common_cmd_base.length());
					    }
					else if(cmd_name.length() > module.Name.length() &&
						cmd_name.substring(0,module.Name.length()).equalsIgnoreCase(module.Name))
					    {
						cmd_name = cmd_name.substring(module.Name.length());
					    }
					if(cmd_name.startsWith("_"))
					    {
						cmd_name = cmd_name.substring(1);
					    }
					current_g.drawString(cmd_name,xoffset + module.x , yoffset + module.y+(2*(getFontMetrics(getFont()).getHeight())));
				    }
				if(null != module.lastStatName)
				    {
					current_g.drawString(module.lastStatName,xoffset + module.x , yoffset + module.y+((int) 3.5*(getFontMetrics(getFont()).getHeight())));
				    }
			    }
			catch(Exception e)
			    {
				e.printStackTrace();
			    }
		    }

		if(null != moduleShowingCommands)
		    {
			int cmd_height = (int) (1.5*(getFontMetrics(getFont()).getHeight()));
			if(debug_on)
			    {
				DebugPrint("cmd_height="+cmd_height);
			    }
			for(int cmd_num = 0; cmd_num < moduleShowingCommands.cmdsAvailable.size(); cmd_num++)
			    {
				String command_string = (String) moduleShowingCommands.cmdsAvailable.elementAt(cmd_num);
				if(highlighted_command != cmd_num)
				    {
					current_g.setColor(Color.white);
				    }
				else
				    {
					current_g.setColor(Color.black);
				    }
				current_g.fillRect(xoffset + moduleShowingCommands.x+MODULE_XOFFSET, yoffset + moduleShowingCommands.y+MODULE_YOFFSET+MODULE_HEIGHT+cmd_num*cmd_height, MODULE_WIDTH,cmd_height);
				if(highlighted_command != cmd_num)
				    {
					current_g.setColor(Color.black);
				    }
				else
				    {
					current_g.setColor(Color.white);
				    }
				current_g.drawRect(xoffset + moduleShowingCommands.x+MODULE_XOFFSET, yoffset + moduleShowingCommands.y+MODULE_YOFFSET+MODULE_HEIGHT+cmd_num*cmd_height, MODULE_WIDTH,cmd_height);
				try
				    {
					if(null != moduleShowingCommands.common_cmd_base &&
					   command_string.length() > moduleShowingCommands.common_cmd_base.length() &&
					   command_string.substring(0,moduleShowingCommands.common_cmd_base.length()).equalsIgnoreCase(moduleShowingCommands.common_cmd_base))
					    {
						command_string = command_string.substring(moduleShowingCommands.common_cmd_base.length());
					    }
					else if(command_string.length() > moduleShowingCommands.Name.length() &&
					   command_string.substring(0,moduleShowingCommands.Name.length()).equalsIgnoreCase(moduleShowingCommands.Name))
					    {
						command_string = command_string.substring(moduleShowingCommands.Name.length());
					    }
					if(command_string.startsWith("_"))
					    {
						command_string = command_string.substring(1);
					    }
					if(command_string.indexOf('=') > 2 )
					    {
						command_string = command_string.substring(0,command_string.indexOf('='));
					    }
					current_g.drawString(command_string, 
						     xoffset + moduleShowingCommands.x+MODULE_XOFFSET+5, 
						     yoffset + moduleShowingCommands.y+MODULE_YOFFSET+MODULE_HEIGHT+(cmd_num+1)*cmd_height-2);
				    }
				catch(Exception e)
				    {
					e.printStackTrace();
				    }
			    }
		    }

		for(int i = 0; i < modules.size(); i++)
		    {
			ModuleInfo module = (ModuleInfo) modules.elementAt(i);
			
			if(debug_on)
			    {
				DebugPrint("1670: i="+i+",partial_paint="+partial_paint);
			    }

			if(partial_paint && i == 0)
			    {
				module = (ModuleInfo) modules.elementAt(current_module);
			    }
			else if(partial_paint && i == 1 && 
				null != moduleShowingCommands && 
				moduleShowingCommands != ((ModuleInfo) modules.elementAt(current_module)) )
			    {
				module = moduleShowingCommands;
			    }
			else if(partial_paint && i > 0)
			    {
				break;
			    }

			if(null == module)
			    {
				continue;
			    }
			if(null == module.ModulesReadingAuxOutput)
			    {
				continue;
			    }
			line_style="11100000";
			if(use_color)
			    {
				current_g.setColor(Color.magenta);
			    }
			for(int j = 0; j < module.ModulesReadingAuxOutput.size(); j++)
			    {
				ModuleInfo auxmodule = (ModuleInfo) module.ModulesReadingAuxOutput.elementAt(j);
				EnhancedDrawLine(current_g,xoffset + auxmodule.x+MODULE_WIDTH/2+MODULE_XOFFSET, yoffset + auxmodule.y+MODULE_YOFFSET,
						 xoffset + module.x+MODULE_WIDTH/2+MODULE_XOFFSET,      yoffset + module.y+MODULE_YOFFSET+MODULE_HEIGHT);
			    }
		    }
		if(debug_on)
		    {
			m4 =  System.currentTimeMillis();
			DebugPrint("HierarchyPanel.paint() time ="+(m4-m1)+" ms.");
		    }

	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	partial_paint=false;
	painting=false;
    }


    int line_width = 1;
    String line_style=null;

    private void EnhancedDrawLine(Graphics  g,int x1, int y1, int x2, int y2)
    {
	int temp;
	int x_low;
	int x_high;
	int x_pos;
	int y_low;
	int y_high;
	int y_pos;
	int x_diff;
	int y_diff;
	int i;
	double m;
	double inverse_m;
	int starting_i = 0;
	try
	    {
		if(x2 > x1)
		    {
			x_low = x1;
			x_high = x2;
		    }
		else
		    {
			x_low = x2;
			x_high = x1;
		    }
		if(y2 > y1)
		    {
			y_low = y1;
			y_high = y2;
		    }
		else
		    {
			y_low = y2;
			y_high = y1;
		    }
		x_diff = x_high - x_low;
		y_diff = y_high - y_low;

		// Handle vertical lineNumber and sindgle point case separately.
		if(x_diff < 1)
		    {
			// Handle sindgle point case separately.
			if(y_diff < 1)
			    {
				g.fillRect(x_low,y_low,1,1);
				return;
			    }
			g.fillRect(x_low,y_low,line_width,y_diff);
			return;
		    }
		// Handle horizontal lineNumber case separately.
		if(y_diff < 1)
		    {
			g.fillRect(x_low,y_low,x_diff,line_width);
			return;
		    }

		if(y_diff > x_diff)
		    {
			// Swap the points to make the higher point (x1,y1)
			if(y1 > y2)
			    {
				temp = x2;
				x2 = x1;
				x1 = temp;
				temp = y2;
				y2 = y1;
				y1 = temp;
			    }
			if(y1 > 0)
			    {
				starting_i = 0;
			    }
			else
			    {
				starting_i = -y1;
			    }
			for(i = starting_i; i < y_diff; i++)
			    {
				if(line_style != null)
				    {
					if(line_style.charAt(i%line_style.length()) == '0')
					    {
						continue;
					    }
				    }
				inverse_m = ((double)(x2 - x1))/((double)(y2 -y1));
				y_pos = y1 + i;
				if(y_pos > d.height)
				    {
					break;
				    }
				x_pos = (int) (x1 + inverse_m*i);
				if(x_pos > d.width+line_width/2 && inverse_m > 0)
				    {
					break;
				    }
				if(x_pos < -line_width/2 && inverse_m < 0)
				    {
					break;
				    }
				g.fillRect(x_pos-line_width/2,y_pos,line_width,1);
			    }
		    }
		else
		    {
			// Swap the points to make the one to the left (x1,y1)
			if(x1 > x2)
			    {
				temp = x2;
				x2 = x1;
				x1 = temp;
				temp = y2;
				y2 = y1;
				y1 = temp;
			    }
			if(x1 > 0)
			    {
				starting_i = 0;
			    }
			else
			    {
				starting_i = -x1;
			    }
			for(i = starting_i; i < x_diff; i++)
			    {
				if(line_style != null)
				    {
					if(line_style.charAt(i%line_style.length()) == '0')
					    {
						continue;
					    }
				    }
				m = ((double)(y2 -y1))/ ((double)(x2 - x1));
				x_pos = x1 + i;
				if(x_pos > d.width)
				    {
					break;
				    }
				y_pos = (int) (y1 + m*i);
				if(y_pos > d.height+line_width/2 && m > 0)
				    {
					break;
				    }
				if(x_pos < -line_width/2 && m < 0)
				    {
					break;
				    }
				g.fillRect(x_pos,y_pos-line_width/2,1,line_width);
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    private ModuleInfo FindParent(String child, Hashtable modulesHashTable)
      {
	  String moduleName;
	  ModuleInfo moduleInfo;
	  try
	      {
		  if(debug_on)
		      {
			  DebugPrint("FindParent("+child+") called.");
		      }
		  if(modulesHashTable.size() < 1)
		      {
			  return null;
		      }
		  int number_of_modules = getModulesListItemCount();
		  for(int i = 0; i < number_of_modules; i++)
		      {
			  moduleName = getModulesListItem(i);
			  DebugPrint("moduleName="+moduleName);
			  if(null == moduleName)
			      {
				  break;
			      }
			  moduleInfo = (ModuleInfo) modulesHashTable.get(moduleName);
			  if(null == moduleInfo)
			      {
				  continue;
			      }
			  int number_of_children = moduleInfo.children_names.size();
			  if(debug_on)
			      {
				  DebugPrint("moduleInfo="+moduleInfo);
				  DebugPrint("moduleInfo.children_names="+moduleInfo.children_names);
			      }
			  for(int ii = 0; ii < number_of_children; ii++)
			      {
				  if(debug_on)
				      {
					  DebugPrint("moduleInfo.children_names.elementAt("+ii+")="+moduleInfo.children_names.elementAt(ii));
				      }
				  String test_string = (String) moduleInfo.children_names.elementAt(ii);
				  if(0 == test_string.compareTo(child))
				      {
					  if(debug_on)
					      {
						  DebugPrint("FindParent("+child+") returning "+moduleInfo+".");
					      }
					  return moduleInfo;
				      }
			      }
		      }
		  if(debug_on)
		      {
			  DebugPrint("FindParent("+child+") returning null.");
		      }
		  return null;
	      }
	  catch(Exception e)
	      {
		  e.printStackTrace();
	      }
	  return null;
      }


    @SuppressWarnings("unchecked")
     public void FindAllParents(Hashtable modulesHashTable, URLLoadInfoPanelInterface loadingPanel)
      {
	  String moduleName;
	  ModuleInfo parent;
	  ModuleInfo module;
	  //	  boolean orig_debug_on=debug_on;
	  try
	      {
		  //debug_on=true;
		  if(debug_on)
		      {
			  DebugPrint("FindAllParents called.");
		      }
                  int modulesHashTable_size = modulesHashTable.size();
		  if(modulesHashTable_size < 1)
		      {
			  return;
		      }
		  max_x = 0;
		  int number_of_modules = getModulesListItemCount();
		  if(null != loadingPanel)
		      {
			  loadingPanel.set_URLname("Sorting Modules . . . ");
			  loadingPanel.set_bytes_read(0);
			  loadingPanel.set_content_length(number_of_modules);
			  loadingPanel.repaint();
		      }
		  if(null != modules_by_generation)
		      {
			  modules_by_generation.removeAllElements();
			  modules_by_generation = null;
		      }
		  modules_by_generation = new Vector();
		  max_generations=-1;
		  for(int ij = 0; ij < number_of_modules; ij++)
		      {
			  if(interrupt_loading)
			      {
				  return;
			      }
			  moduleName = getModulesListItem(ij);
			  if(null == moduleName)
			      {
				  break;
			      }
			  module = (ModuleInfo) modulesHashTable.get(moduleName);
			  if(null == module)
			      {
				  continue;
			      }
			  if(!module.preset_x)
			      {
				  module.x = 0;
			      }
			  if(!module.preset_y)
			      {
				  module.y = 0;
			      }
			  module.generation = 0;
		      }
		  for(int i = 0; i < number_of_modules; i++)
		      {
			  if(interrupt_loading)
			      {
				  return;
			      }
			  moduleName = getModulesListItem(i);
			  if(null == moduleName)
			      {
				  break;
			      }
			  parent = FindParent(moduleName, modulesHashTable);
			  module = (ModuleInfo) modulesHashTable.get(moduleName);
			  if(null == module)
			      {
				  continue;
			      }
			  if(parent != null)
			      {
				  module.generation = parent.generation + 1;
				  module.parent = parent;
			      }
			  if(module.generation > max_generations)
			      {
				  for(int ii = max_generations; ii < module.generation+1; ii++)
				      {
					  modules_by_generation.addElement(new Vector());
				      }
				  max_generations = module.generation;
			      }
			  if(modules_by_generation.size() > module.generation)
			      {
				  Vector generation_vector = (Vector) modules_by_generation.elementAt(module.generation);
				  generation_vector.addElement(module);
				  modules.addElement(module);
				  if(generation_vector.size() > max_number_in_generation)
				      {
					  max_number_in_generation = generation_vector.size();
				      }
			      }
			  if(null != loadingPanel)
			      {
				  loadingPanel.set_bytes_read(loadingPanel.get_bytes_read()+1);
				  loadingPanel.set_content_length(number_of_modules);
				  loadingPanel.repaint();
			      }
		      }
		  boolean need_correction = true;
		  while(need_correction)
		      {
			  need_correction = false;
			  for(int i = 0; i < number_of_modules; i++)
			      {
				  moduleName = getModulesListItem(i);
				  if(null == moduleName)
				      {
					  continue;
				      }
				  module = (ModuleInfo) modulesHashTable.get(moduleName);
				  if(null == module)
				      {
					  continue;
				      }
				  if(null == module.parent)
				      {
					  module.generation = 0;
					  continue;
				      }
				  if(module.generation < module.parent.generation + 1)
				      {
					  module.generation = module.parent.generation + 1;
					  need_correction = true;
					  break;
				      }
			      }
		      }
		  if(modules_by_generation.size() > 0)
		      {
			  Vector first_generation_vector = (Vector) modules_by_generation.elementAt(0);
			  if(null == first_generation_vector)
			      {
				  return;
			      }
			  if(null != loadingPanel)
			      {
				  loadingPanel.set_URLname("Positioning Modules . . . ");
				  loadingPanel.set_bytes_read(0);
				  loadingPanel.set_content_length(number_of_modules);
				  loadingPanel.repaint();
			      }
			  int xpos = 100;
			  for(int iii = 0; iii < first_generation_vector.size(); iii++)
			      {
				  if(interrupt_loading)
				      {
					  return;
				      }
				  module = (ModuleInfo) first_generation_vector.elementAt(iii);
				  if(null != module)
				      {
					  xpos = PositionChildren(module,xpos,modulesHashTable,loadingPanel);
				      }
				  xpos = (int) (max_x + MODULE_WIDTH*1.5);
			      }
		      }
		  for(int i = 0; i < number_of_modules; i++)
		      {
			  moduleName = getModulesListItem(i);
			  if(null == moduleName)
			      {
				  continue;
			      }
			  module = (ModuleInfo) modulesHashTable.get(moduleName);
			  if(null == module)
			      {
				  continue;
			      }
			  if(null == module.AuxOutputNames)
			      {
				  continue;
			      }
			  if(null == module.ModulesReadingAuxOutput)
			      {
				  module.ModulesReadingAuxOutput = new Vector();
			      }
			  module.ModulesReadingAuxOutput.removeAllElements();
			  for(int j = 0; j < module.AuxOutputNames.size(); j++)
			      {
				  String auxOutput = (String) module.AuxOutputNames.elementAt(j);
				  if(debug_on)
				      {
					  DebugPrint(module.Name+" writes to auxiliary channel "+auxOutput);
				      }
				  for(int k = 0; k < number_of_modules; k++)
				      {
					  String auxmoduleName = getModulesListItem(k);
					  if(null == auxmoduleName)
					      {
						  continue;
					      }
					  ModuleInfo auxmodule = (ModuleInfo) modulesHashTable.get(auxmoduleName);
					  if(null == auxmodule)
					      {
						  continue;
					      }
					  if(null == auxmodule.AuxInputNames)
					      {
						  continue;
					      }
					  for(int l = 0; l < auxmodule.AuxInputNames.size(); l++)
					      {
						  String auxInput = (String) auxmodule.AuxInputNames.elementAt(l);
						  if(debug_on)
						      {
							  DebugPrint(auxmodule.Name+" reads from auxiliary channel "+auxInput);
						      }
						  if(auxInput.equals(auxOutput))
						      {
							  DebugPrint(module.Name+" and "+auxmodule.Name+" share auxiliary channel "+auxInput);
							  module.ModulesReadingAuxOutput.addElement(auxmodule);
							  break;
						      }
					      }
				      }
			      }
		      }
		  max_x += MODULE_WIDTH + MODULE_XOFFSET;
		  if(min_x > -(MODULE_XOFFSET))
		      {
			  min_x += MODULE_XOFFSET;
		      }
		  else
		      {
			  min_x = 0;
		      }

		  max_y += MODULE_HEIGHT + MODULE_YOFFSET;
		  if(min_y > -(MODULE_YOFFSET))
		      {
			  min_y += MODULE_YOFFSET;
		      }
		  else
		      {
			  min_y = 0;
		      }
		  Dimension d = getPreferredSize();
		  if(debug_on)
		      {
			  DebugPrint("Setting up Hierarchy Panel.");
			  DebugPrint("m_hierarchyInnerPanel.preferredSize().width = "+d.width);
			  DebugPrint("m_hierarchyInnerPanel.preferredSize().height = "+d.height);
			  DebugPrint("min_x = "+min_x);
			  DebugPrint("max_x = "+max_x);
			  DebugPrint("min_y = "+min_y);
			  DebugPrint("max_y = "+max_y);
		      }
		  int hscroll_current = min_x-10;
		  int hscroll_max = max_x +20+2*MODULE_WIDTH+ MODULE_XOFFSET;
		  if(hscroll_max < hscroll_current+20+2*MODULE_WIDTH+ MODULE_XOFFSET)
		      {
			  hscroll_max = hscroll_current+20+2*MODULE_WIDTH+ MODULE_XOFFSET;
		      }
		  int hscroll_min = min_x - 10+MODULE_XOFFSET;
		  int hscroll_visible = d.width;
		  if(hscroll_visible > (hscroll_max - hscroll_min)/2)
		      {
			  hscroll_visible = (hscroll_max - hscroll_min)/2;
		      }
		  if(hscroll_current < hscroll_min)
		      {
			  hscroll_current = hscroll_min;
		      }
		  int hmaxx = computeMaxX();
		  if(hscroll_max < hmaxx)
		      {
			  hscroll_max = hmaxx;
		      }
		  if(debug_on)
		      {
			  DebugPrint("hscroll_current = "+hscroll_current);
			  DebugPrint("hscroll_visible = "+hscroll_visible);
			  DebugPrint("hscroll_min = "+hscroll_min);
			  DebugPrint("hscroll_max = "+hscroll_max);
		      }
		  if(null != horzScrollbar)
		      {
			  if(debug_on)
			      {
				  DebugPrint("HierarchyPanel.FindAllParents calling horzScrollbar.setValues("+hscroll_current+", "+hscroll_visible+", "+(hscroll_min-hscroll_visible)+", "+(hscroll_max+hscroll_visible)+");");
			      }
			  horzScrollbar.setValues(hscroll_current, hscroll_visible,(hscroll_min-hscroll_visible),hscroll_max+hscroll_visible);
			  scroll_x = horzScrollbar.getValue();
		      }
		  else
		      {
			  System.err.println("horzScrollbar is null.");
		      }
		  int vscroll_current = min_y-25;
		  int vscroll_max = max_y +10+ MODULE_HEIGHT+ MODULE_YOFFSET;
		  if(vscroll_max < vscroll_current+20+ MODULE_HEIGHT+ MODULE_YOFFSET)
		      {
			  vscroll_max = vscroll_current+20+ MODULE_HEIGHT+ MODULE_YOFFSET;
		      }
		  int vscroll_min = min_y - 10+ MODULE_YOFFSET;
		  int vscroll_visible = d.height;
		  if(vscroll_visible > (vscroll_max - vscroll_min)/2)
		      {
			  vscroll_visible = (vscroll_max - vscroll_min)/2;
		      }
		  if(vscroll_current < vscroll_min)
		      {
			  vscroll_current = vscroll_min;
		      }
		  int hmaxy = computeMaxY();
		  if(vscroll_max < hmaxy)
		      {
			  vscroll_max = hmaxy;
		      }
		  if(debug_on)
		      {
			  DebugPrint("vscroll_current = "+vscroll_current);
			  DebugPrint("vscroll_visible = "+vscroll_visible);
			  DebugPrint("vscroll_min = "+vscroll_min);
			  DebugPrint("vscroll_max = "+vscroll_max);
		      }
		  if(null != vertScrollbar)
		      {
			  if(debug_on)
			      {
				  DebugPrint("HierarchyPanel.FindAllParents calling vertScrollbar.setValues("+vscroll_current+", "+vscroll_visible+", "+(vscroll_min-vscroll_visible)+", "+(vscroll_max+vscroll_visible)+");");
			      }
			  vertScrollbar.setValues(vscroll_current,vscroll_visible,(vscroll_min-vscroll_visible),(vscroll_max+vscroll_visible));
			  scroll_y = vertScrollbar.getValue();
		      }
		  else
		      {
			  System.err.println("vertScrollbar is null.");
		      }
		  if(debug_on)
		      {
			  DebugPrint("FindAllParents returning.");
			  DebugPrint("diag_update_object="+diag_update_object);
		      }
		  if(null != diag_update_object)
		      {
			  diag_update_object.set_need_update();
		      }
	      }
	  catch(Exception e)
	      {
		  e.printStackTrace();
	      }
	  //debug_on=orig_debug_on;
      }

    private int PositionChildren(ModuleInfo module, int x, Hashtable modulesHashTable, URLLoadInfoPanelInterface loadingPanel )
      {
	  int begin_x = x;
	  try
	      {
		  if(debug_on)
		      {
			  DebugPrint("PositionChildren("+module+","+x+" ...) called.");
		      }
		  if(interrupt_loading)
		      {
			  return 0;
		      }
		  for(int i = 0; i < module.children_names.size(); i++)
		      {
			  String child_name = (String) module.children_names.elementAt(i);
			  if(module.IsAncestor(child_name))
			      {
				  ErrorPrint("module "+module+" has child with same name as ancestor : "+child_name);
				  break;
			      }
			  if(null == child_name)
			      {
				  break;
			      }
			  ModuleInfo child_module = (ModuleInfo) modulesHashTable.get(child_name);
			  if(null == child_module)
			      {
				  continue;
			      }
			  if(i > 0)
			      {
				  x = PositionChildren(child_module,max_x+MODULE_WIDTH+MODULE_X_SPACING, modulesHashTable,loadingPanel);
			      }
			  else
			      {
				  x = PositionChildren(child_module,max_x+MODULE_WIDTH+MODULE_X_SPACING, modulesHashTable,loadingPanel);
			      }
			  if(i == 0)
			      {
				  begin_x = x;
			      }
		      }
		  int end_x = x;
		  if(!module.preset_x)
		      {
			  module.x = (begin_x + end_x)/2;
		      }
		  if(!module.preset_y)
		      {
			  module.y = module.generation * ( MODULE_HEIGHT+ MODULE_Y_SPACING) + MODULE_Y_SPACING;
		      }
		  if(min_x < 0 ||min_x > module.x)
		      {
			  min_x = module.x;
		      }
		  if(max_x < 0 || max_x < module.x)
		      {
			  max_x = module.x;
		      }
		  if(min_y < 0 || min_y > module.y)
		      {
			  min_y = module.y;
		      }
		  if(max_y < 0 || max_y < module.y)
		      {
			  max_y = module.y;
		      }
		  if(null != loadingPanel)
		      {
			  loadingPanel.set_bytes_read(loadingPanel.get_bytes_read()+1);
			  loadingPanel.repaint();
		      }
		  if(debug_on)
		      {
			  //Thread.dumpStack();
			  DebugPrint("PositionChildren("+module+","+x+" ...) returning "+module.x+" .");
		      }
		  return module.x;
	      }
	  catch(Exception e)
	      {
		  e.printStackTrace();
	      }
	  return 0;
      }
    
    public void componentResized(ComponentEvent evt)
    {
	if(debug_on)
	    {
		Thread.dumpStack();
	    }
    }

  public void componentShown(ComponentEvent evt)
  {
	if(debug_on)
	    {
		Thread.dumpStack();
	    }
  }

  public void componentHidden(ComponentEvent evt)
  {
	if(debug_on)
	    {
		Thread.dumpStack();
	    }
  }

  public void componentMoved(ComponentEvent evt)
  {
 	if(debug_on)
	    {
		Thread.dumpStack();
	    }
  }


}
