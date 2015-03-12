/*
 * Xyz.java
 *
 * Created on July 5, 2007, 10:50 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package jplotxyz;

/**
 *
 * @author shackle
 */
public class Xyz
{
    
    /** Creates a new instance of Xyz */
    public Xyz()
    {
    }

	/**
	 * Holds value of property x.
	 */
	private double x;

	/**
	 * Getter for property x.
	 * @return Value of property x.
	 */
	public double getX()
	{
		return this.x;
	}

	/**
	 * Setter for property x.
	 * @param x New value of property x.
	 */
	public void setX(double x)
	{
		this.x = x;
	}

	/**
	 * Holds value of property y.
	 */
	private double y;

	/**
	 * Getter for property y.
	 * @return Value of property y.
	 */
	public double getY()
	{
		return this.y;
	}

	/**
	 * Setter for property y.
	 * @param y New value of property y.
	 */
	public void setY(double y)
	{
		this.y = y;
	}

	/**
	 * Holds value of property z.
	 */
	private double z;

	/**
	 * Getter for property z.
	 * @return Value of property z.
	 */
	public double getZ()
	{
		return this.z;
	}

	/**
	 * Setter for property z.
	 * @param z New value of property z.
	 */
	public void setZ(double z)
	{
		this.z = z;
	}

	public Xyz(double x, double y, double z)
	{
	    setX(x);
	    setY(y);
	    setZ(z);
	}

	public static Xyz diff(Xyz xyz1, Xyz xyz2)
	{
	    Xyz result = new Xyz();
	    result.setX(xyz1.x - xyz2.x);
	    result.setY(xyz1.y - xyz2.y);
	    result.setZ(xyz1.z - xyz2.z);
	    return result;
	}
    
}
