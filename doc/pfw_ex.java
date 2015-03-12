

import rcs.nml.PackedFileWriter;
import rcs.nml.NMLConnection;

public class pfw_ex
{    
    static public void main(String args[]) throws Exception
    {
	NMLConnection nml = new NMLConnection(new exampleMsgDict(),
					     "ex_buf1",
					     "pfw_ex",
					     "ex_cfg.nml");
	PackedFileWriter pfw = new PackedFileWriter(new exampleMsgDict(),false);
	pfw.WriteFile(args[0],nml.read());
    }
}
