/* nml_ex4.cc */
#include "rcs.hh"
#include "nml_ex1.hh"

#include <string.h>
#include <stdlib.h>


static double f = 1.1;
static char c = 'a';
static int i = 0;

#ifdef VXWORKS
extern "C" void bWriter();

void bWriter()
#else
int main(int argc, char **argv)
#endif
{
 #ifndef VXWORKS
    int print_flag = 0;
  if(argc > 2)
    {
      print_flag = strtol(argv[2],0,0);
      set_rcs_print_flag(print_flag);
      cms_print_queue_free_space=1;
    }
#endif

  NML example_nml(ex_format, "ex_buf1", "bWriter", TEST_CFG);
  EXAMPLE_MSG example_msg;
  printf("bWriter started . . .\n");

  max_rcs_errors_to_print=-1;
   for(int j = 0; j < 10000; j++)
    {
      f *= 1.01;
      c++;
      if(c > 'z')
	{
	  c = 'a';
	}
      i++;
      if(f > 1000.0)
	{
	  f = 1;
	}
      example_msg.f = f;
      example_msg.c = c;
      example_msg.i = i;
      example_msg.da_length = (j%5)+4;
      if(example_nml.write(example_msg) )
	{
	  printf("example_nml.write() failed.\n");
	}
#if 0
      printf("example_nml.get_msg_count() = %d\n", example_nml.get_msg_count());
      printf("example_nml.get_space_available() = %d\n", example_nml.get_space_available());
      printf("example_nml.get_queue_length() = %d\n", example_nml.get_queue_length());
#endif
      esleep(0.03);
    }
}




