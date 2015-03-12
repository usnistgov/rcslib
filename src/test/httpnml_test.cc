#include "clock.hh"
#include "clock_n_codegen_protos.hh"


#include "httpnml.hh"
#include <stdio.h>

int main(int argc, const char **argv)
{
  const char *url = "http://localhost:8090/clock_buf.xml";
  if(argc > 1)
    {
      url = argv[1];
    }

  HttpXmlNml hxn(MY_CLOCK_format,
		 url);
		 

  while(1)
    {
      MY_CLOCK *clk = (MY_CLOCK *) hxn.readMsg();
      if(clk)
	{
	  printf("%02d:%02d:%02.0f\n",
		 clk->now.hours,
		 clk->now.minutes,
		 clk->now.seconds);
	}
      esleep(1.0);
   }
}

