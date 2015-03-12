

#include "rcs.hh"
#include "nml_unbounded_array_test_msg.hh"
#include <stdio.h>
#include <stdlib.h>

static bool quit = false;

static void
my_sigint_handler(int)
{
  quit = true;
}

int i;

#define assign(var,value)				\
  var = (typeof((var))) value;


#define NML_TEST_UBA_ALLOC(var,len)	                         \
  {                                                              \
  if(!var)                                                       \
    {                                                            \
      void *vp = malloc(len*sizeof(*var));                       \
      var = (typeof((var))) vp;                                  \
      var##_length = len;		                         \
      var##_size_allocated = len; 	                         \
    }                                                            \
  else                                                           \
    {                                                            \
      void *vp = realloc((void *)var,len*sizeof(*var));	         \
      var = (typeof((var))) vp;                                  \
      var##_length = len;		                         \
      var##_size_allocated = len; 	                         \
    }                                                            \
  }                                                              \




#define NML_TEST_UBA_FREE(var)		     \
  if(var)                                    \
    {                                        \
      free(var);                             \
    }                                        \
  var=0;                                     \
  var##_length=0;                            \
  var##_size_allocated=0;                    \
      
int
main(int argc, const char **argv)
{
  const char *bufname =0;
  const char *procname =0;
  const char *configfile =0;

  if(argc !=  4)
    {
      fprintf(stderr,"%s usage : bufname procname configfile repeat_count repeat_delay\n", argv[0]);
      fprintf(stderr,"set repeat count to -1 to repeat indefinitely.\n");
      exit(1);
    }
  bufname = argv[1];
  procname = argv[2];
  configfile = argv[3];


  NML *nmlP = new NML(nml_unbounded_array_test_msg_format,
		      bufname, procname, configfile);
  run_nml_servers();
}


  


  
