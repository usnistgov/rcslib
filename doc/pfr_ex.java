

import rcs.nml.PackedFileReader;

public class pfr_ex
{    
    static public void main(String args[])
    {
	PackedFileReader pfr = new PackedFileReader(new exampleMsgDict(),false);
	EXAMPLE_MSG ex_msg = (EXAMPLE_MSG) pfr.ReadFile(args[0]);
	System.out.println("ex_msg.f="+ex_msg.f);	
	System.out.println("ex_msg.c="+Character.toString((char)ex_msg.c));	
	System.out.println("ex_msg.i="+ex_msg.i);
    }
}
