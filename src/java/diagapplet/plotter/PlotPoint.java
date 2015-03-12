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

/*
 *
 * PlotPoint
 *
 */
class PlotPoint
{
    //public boolean connected =  true;
    public double orig_x = 0.0;
    public double orig_y = 0.0;
    public double pre_f_y = 0.0;
    public double pre_f_x = 0.0;
    public int x =0;
    public int y =0;
    //public long time=0;
    

    public PlotPoint(int x_param, int y_param, double orig_x_param, double orig_y_param, double pre_f_x_param, double pre_f_y_param)
    {
	x = x_param;
	y = y_param;
	orig_x = orig_x_param;
	orig_y = orig_y_param;
	//connected = connected_param;
	pre_f_x = pre_f_x_param;
	pre_f_y  = pre_f_y_param;
    }
    
    public PlotPoint()
    {
        
    }
    
    
    
    public String toString()
    {
	return super.toString()+ " { orig_x="+orig_x+", orig_y="+orig_y+", pre_f_x="+pre_f_x+", pre_fy_y="+pre_f_y+" x="+x+", y="+y+" }";
    }
}
