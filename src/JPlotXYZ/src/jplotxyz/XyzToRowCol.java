/*
 * XyzToRowCol.java
 *
 * Created on July 5, 2007, 10:49 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package jplotxyz;

/**
 *
 * @author shackle
 */
public class XyzToRowCol
{
    
    /** Creates a new instance of XyzToRowCol */
    public XyzToRowCol()
    {
    }

	/**
	 * Holds value of property viewPos.
	 */
	private Xyz viewPos;

	/**
	 * Getter for property viewPos.
	 * @return Value of property viewPos.
	 */
	public Xyz getViewPos()
	{
		return this.viewPos;
	}

	/**
	 * Setter for property viewPos.
	 * @param viewPos New value of property viewPos.
	 */
	public void setViewPos(Xyz viewPos)
	{
		this.viewPos = viewPos;
	}

	/**
	 * Holds value of property viewOrientation.
	 */
	private RollPitchYaw viewOrientation;

	/**
	 * Getter for property viewOrientation.
	 * @return Value of property viewOrientation.
	 */
	public RollPitchYaw getViewOrientation()
	{
		return this.viewOrientation;
	}

	/**
	 * Setter for property viewOrientation.
	 * @param viewOrientation New value of property viewOrientation.
	 */
	public void setViewOrientation(RollPitchYaw viewOrientation)
	{
		this.viewOrientation = viewOrientation;
	}

	/**
	 * Holds value of property imageDimension.
	 */
	private java.awt.Dimension imageDimension;

	/**
	 * Getter for property imageDimension.
	 * @return Value of property imageDimension.
	 */
	public java.awt.Dimension getImageDimension()
	{
		return this.imageDimension;
	}

	/**
	 * Setter for property imageDimension.
	 * @param imageDimension New value of property imageDimension.
	 */
	public void setImageDimension(java.awt.Dimension imageDimension)
	{
		this.imageDimension = imageDimension;
	}

	/**
	 * Holds value of property horzAngularRange.
	 */
	private double horzAngularRange;

	/**
	 * Getter for property horzAngularRange.
	 * @return Value of property horzAngularRange.
	 */
	public double getHorzAngularRange()
	{
		return this.horzAngularRange;
	}

	/**
	 * Setter for property horzAngularRange.
	 * @param horzAngularRange New value of property horzAngularRange.
	 */
	public void setHorzAngularRange(double horzAngularRange)
	{
		this.horzAngularRange = horzAngularRange;
	}

	/**
	 * Holds value of property vertAngularRange.
	 */
	private double vertAngularRange;

	/**
	 * Getter for property vertAngularRange.
	 * @return Value of property vertAngularRange.
	 */
	public double getVertAngularRange()
	{
		return this.vertAngularRange;
	}

	/**
	 * Setter for property vertAngularRange.
	 * @param vertAngularRange New value of property vertAngularRange.
	 */
	public void setVertAngularRange(double vertAngularRange)
	{
		this.vertAngularRange = vertAngularRange;
	}

	public RowCol convertXyzToRowCol(Xyz xyz)
	{
	    
	    Xyz xyz_diff = Xyz.diff(xyz,this.viewPos);
	    RollPitchYaw rpy = new RollPitchYaw(xyz_diff);
	    RowCol result = new RowCol();
	    result.setCol((int) ((rpy.getYaw() - this.viewOrientation.getYaw())*this.imageDimension.width/this.horzAngularRange));
	    result.setRow((int) ((rpy.getPitch() - this.viewOrientation.getPitch())*this.imageDimension.height/this.vertAngularRange));
	    return result;
	}
    
}
