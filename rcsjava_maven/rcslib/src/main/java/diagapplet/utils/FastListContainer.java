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

package diagapplet.utils;

import java.awt.*;
import java.awt.event.*;


/**
 * Container that adds scrollbars to FastListPanel.
 * @author Will Shackleford {@literal <william.shackleford@nist.gov>}
 */
public class FastListContainer extends Panel implements AdjustmentListener
{
    FastListPanel flpanel = null;
    Panel innerPanel = null;
    GridBagLayout innerLayout = null;
    FlowLayout lm = null;
    Scrollbar sideScrollbar = null;
    Scrollbar bottomScrollbar = null;


    /**
     *   Unique id for this class, Exception implements Serializable interface see documentation
     * for serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613900L;



    public FastListContainer(FastListPanel internal_flpanel)
    {
	super();
	try
	    {
		lm  = new FlowLayout(FlowLayout.LEFT);
		setLayout(lm);
		innerPanel = new Panel();
		innerLayout = new GridBagLayout();
		innerPanel.setLayout(innerLayout);
		flpanel = internal_flpanel;
		GridBagConstraints c = new GridBagConstraints();
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		innerPanel.add(flpanel);
		innerLayout.setConstraints(flpanel, c);
		flpanel.myContainer = this;
		bottomScrollbar = new Scrollbar(Scrollbar.HORIZONTAL);
		c.gridx = 0;
		c.gridy = 1;
		c.gridwidth = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		innerPanel.add(bottomScrollbar);
		innerLayout.setConstraints(bottomScrollbar, c);
		bottomScrollbar.addAdjustmentListener(this);
		sideScrollbar = new Scrollbar(Scrollbar.VERTICAL);
		c.gridx = 1;
		c.gridy = 0;
		c.gridwidth = 1;
		c.fill = GridBagConstraints.VERTICAL;
		innerPanel.add(sideScrollbar);
		innerLayout.setConstraints(sideScrollbar, c);
		sideScrollbar.addAdjustmentListener(this);
		add(innerPanel);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public void UpdateScrollbars()
    {
	try
	    {
		if(null == sideScrollbar || null == flpanel)
		    {
			return;
		    }
		int new_max_side = ((flpanel.countItems()/flpanel.lines_visible)+1)*flpanel.lines_visible;
		if(flpanel.start_line != sideScrollbar.getValue() || flpanel.lines_visible != sideScrollbar.getVisibleAmount() ||
		   0 != sideScrollbar.getMinimum() || new_max_side != sideScrollbar.getMaximum())
		    {
			sideScrollbar.setValues(flpanel.start_line, flpanel.lines_visible,0,
						new_max_side);
		    }
		// System.out.println("sideScrollbar.setValues("+flpanel.start_line+", "+flpanel.lines_visible+
		// ",0,"+flpanel.countItems()+");");
		int new_max_bottom = ((flpanel.maxlinelength/flpanel.chars_visible)+1)*flpanel.chars_visible;
		if(new_max_bottom != bottomScrollbar.getMaximum() || 
		   flpanel.char_offset !=  bottomScrollbar.getValue() ||
		   0 != bottomScrollbar.getMinimum() ||
		   flpanel.chars_visible !=  bottomScrollbar.getVisibleAmount())
		    {
			bottomScrollbar.setValues(flpanel.char_offset, flpanel.chars_visible,0,new_max_bottom);
		    }
		// System.out.println("bottomScrollbar.setValues("+flpanel.char_offset+", "+flpanel.chars_visible+
		// ",0,"+flpanel.maxlinelength+");");
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    int last_sideScrollbar_value = 0;
    int last_bottomScrollbar_value = 0;

    public void adjustmentValueChanged(AdjustmentEvent evt)
    {
	try
	    {
		if(evt.getSource() == sideScrollbar)
		    {
			int temp = evt.getValue();
			if(temp != last_sideScrollbar_value)
			    {
				last_sideScrollbar_value = temp;
				flpanel.start_line = sideScrollbar.getValue();
				if(flpanel.start_line > flpanel.countItems())
				    {
					flpanel.start_line = flpanel.countItems() - flpanel.countItems()%flpanel.lines_visible;
				    }
				flpanel.list_changed=true;
				flpanel.repaint_needed = true;
				flpanel.repaint();
				sideScrollbar.setValues(flpanel.start_line, flpanel.lines_visible,0,flpanel.countItems());
				// System.out.println("sideScrollbar.setValues("+flpanel.start_line+", "+flpanel.lines_visible+
				// ",0,"+flpanel.countItems()+");");
			    }
			return;
		    }
		if(evt.getSource() == bottomScrollbar)
		    {
			int temp = evt.getValue();
			if(temp != last_bottomScrollbar_value)
			    {
				last_bottomScrollbar_value = temp;
				flpanel.char_offset = bottomScrollbar.getValue();
				if(flpanel.char_offset > flpanel.maxlinelength)
				    {
					flpanel.char_offset = flpanel.maxlinelength - flpanel.maxlinelength%flpanel.chars_visible;
				    }
				flpanel.repaint_needed = true;
				flpanel.repaint();
				bottomScrollbar.setValues(flpanel.char_offset, flpanel.chars_visible,0,flpanel.maxlinelength);
				// System.out.println("bottomScrollbar.setValues("+flpanel.char_offset+", "+flpanel.chars_visible+
				// ",0,"+flpanel.maxlinelength+");");
			    }
			return;
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

}
