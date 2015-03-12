#include <iostream>
#include <stdio.h>
#include "clock.hh"
#include "clock_n_codegen_protos.hh"

#include "httpnml.hh"

using namespace std;

int main(int argc, const char **argv)
{
  HttpXmlNml hxn(MY_CLOCK_format,"http://localhost:8090/clock_buf.xml");

  while(1)
    {
      MY_CLOCK *clk = (MY_CLOCK *) hxn.readMsg();
      if(clk)
	{
	  printf("%02d:%02d:%02.0f\n",clk->now.hours,clk->now.minutes,clk->now.seconds);
	}
      sleep(1);
   }
}
