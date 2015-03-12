/* nml_ex3.cc */
#include "rcs.hh"
#include "nml_ex1.hh"
   
#include <string.h>
#include <stdlib.h>

#include <signal.h>
				
int bReaderQuit = 0;


void bReaderSigintHandler(int sig)
{
  bReaderQuit = 1;
}
#ifdef VXWORKS 
extern "C" void bReader( double timeout=-1.0);

void bReader( double timeout)
#else
int main(int argc, char **argv)
#endif
{
#ifndef VXWORKS
  double timeout = -1.0;
  if(argc > 1)
    {
      timeout = strtod(argv[1],NULL);
    }
  int print_flag = 0;
  if(argc > 2)
    {
      print_flag = strtol(argv[2],0,0);
      set_rcs_print_flag(print_flag);
      cms_print_queue_free_space=1;
    }
#endif
  max_rcs_errors_to_print=-1;

  int count =0;
  int scount = 0;

  NML example_nml(ex_format, "ex_buf1", "bReader", TEST_CFG);
  EXAMPLE_MSG *example_msg_ptr;
  signal(SIGINT, bReaderSigintHandler);
  bReaderQuit = 0;
  
  printf("bReader(%f) started . . .\n",timeout);
  double start_time = etime();

  while(!bReaderQuit)
    {
      NMLTYPE readval =0;
      if(timeout > 1e-6 || timeout < -1e-6)
	{
	  readval = example_nml.blocking_read(timeout);
	}
      else
	{
	  readval = example_nml.read();
	}
      switch(readval)
	{
	case -1:
	  rcs_print( "\nA communications error occurred.\n");
	  esleep(0.5);
	  break;

	case 0:

	  /* The buffer contains the same message */
	  /* you read last time. */
	  rcs_print("\nNo new data.\n");
	  break;

	case EXAMPLE_MSG_TYPE:
	  example_msg_ptr = (EXAMPLE_MSG *)example_nml.get_address();
	  /*rcs_print(" We have a new example message. \n");
	    rcs_print(" The value of its members are:\n ");
	    rcs_print(" f=%f, c=%c, i=%d\n ", 
	    example_msg_ptr->f,
	    example_msg_ptr->c,
	    example_msg_ptr->i); */
	  count++;
	  double curtime = etime() - start_time;
	  double freq = (count - scount)/curtime;

#if 0
	  printf("example_nml.get_msg_count() = %d\n", example_nml.get_msg_count());
	  printf("example_nml.get_space_available() = %d\n", example_nml.get_space_available());
	  printf("example_nml.get_queue_length() = %d\n", example_nml.get_queue_length());
#endif
	  rcs_print("%d %d\t\t%lf\n",count,example_msg_ptr->i,freq);
	  if(count%100  == 1)
	    {
	      start_time = etime();
	      scount = count;
	    }
	  break;
	}
      esleep(0.01);
    }
}
