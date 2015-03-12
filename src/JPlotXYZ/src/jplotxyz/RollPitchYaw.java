/*
 * RollPitchYaw.java
 *
 * Created on July 5, 2007, 11:01 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package jplotxyz;

/**
 *
 * @author shackle
 */
public class RollPitchYaw
{
    
    /** Creates a new instance of RollPitchYaw */
    public RollPitchYaw()
    {
    }

	/**
	 * Holds value of property roll.
	 */
	private double roll;

	/**
	 * Getter for property roll.
	 * @return Value of property roll.
	 */
	public double getRoll()
	{
		return this.roll;
	}

	/**
	 * Setter for property roll.
	 * @param roll New value of property roll.
	 */
	public void setRoll(double roll)
	{
		this.roll = roll;
	}

	/**
	 * Holds value of property pitch.
	 */
	private double pitch;

	/**
	 * Getter for property pitch.
	 * @return Value of property pitch.
	 */
	public double getPitch()
	{
		return this.pitch;
	}

	/**
	 * Setter for property pitch.
	 * @param pitch New value of property pitch.
	 */
	public void setPitch(double pitch)
	{
		this.pitch = pitch;
	}

	/**
	 * Holds value of property yaw.
	 */
	private double yaw;

	/**
	 * Getter for property yaw.
	 * @return Value of property yaw.
	 */
	public double getYaw()
	{
		return this.yaw;
	}

	/**
	 * Setter for property yaw.
	 * @param yaw New value of property yaw.
	 */
	public void setYaw(double yaw)
	{
		this.yaw = yaw;
	}

	public RollPitchYaw(double roll, double pitch, double yaw)
	{
	    setRoll(roll);
	    setPitch(pitch);
	    setYaw(yaw);
	}

	public RollPitchYaw(Xyz xyz)
	{
	    setRoll(0);
	    double x = xyz.getX();
	    double y = xyz.getY();
	    double z = xyz.getZ();
	    double xy_mag = Math.sqrt(x*x+y*y);
	    double pitch = Math.atan2(z,xy_mag);
	    double yaw = Math.atan2(y,x);
	    setPitch(pitch);
	    setYaw(yaw);
	}
    
}
