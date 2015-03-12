/*
 * RowCol.java
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
public class RowCol
{
    
    /** Creates a new instance of RowCol */
    public RowCol()
    {
    }

	/**
	 * Holds value of property row.
	 */
	private int row;

	/**
	 * Getter for property row.
	 * @return Value of property row.
	 */
	public int getRow()
	{
		return this.row;
	}

	/**
	 * Setter for property row.
	 * @param row New value of property row.
	 */
	public void setRow(int row)
	{
		this.row = row;
	}

	/**
	 * Holds value of property col.
	 */
	private int col;

	/**
	 * Getter for property col.
	 * @return Value of property col.
	 */
	public int getCol()
	{
		return this.col;
	}

	/**
	 * Setter for property col.
	 * @param col New value of property col.
	 */
	public void setCol(int col)
	{
		this.col = col;
	}

	public RowCol(int row, int col)
	{
	    setRow(row);
	    setCol(col);
	}
    
}
