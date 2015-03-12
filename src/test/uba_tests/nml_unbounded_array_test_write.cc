

#include "rcs.hh"
#include "nml_unbounded_array_test_msg.hh"
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

static bool quit = false;
static const char *arg0="";

static void
my_sigint_handler(int sig)
{ 
  printf("%s: Recieved signal %d\n",arg0,sig);
  quit = true;
}

int i;

      
int
main(int argc, const char **argv)
{
  int repeat_count = 0;
  double repeat_delay = 0.2;
  const char *bufname =0;
  const char *procname =0;
  const char *configfile =0;
  
  arg0 = argv[0];
  if(argc !=  6)
    {
      fprintf(stderr,"%s usage : bufname procname configfile repeat_count repeat_delay\n", argv[0]);
      fprintf(stderr,"set repeat count to -1 to repeat indefinitely.\n");
      exit(1);
    }
  bufname = argv[1];
  procname = argv[2];
  configfile = argv[3];
  repeat_count = atoi(argv[4]);
  repeat_delay = atof(argv[5]);

  printf("%s: Connecting to NML buffer %s using process name %s and configuration %s\n", arg0,
	 bufname,procname,configfile);

  NML *nmlP = new NML(nml_unbounded_array_test_msg_format,
		      bufname, procname, configfile);
  if(!nmlP->valid())
    {
      fprintf(stderr,"Invalid NML connection exiting.\n");
      delete nmlP;
      exit(1);
    }
  printf("%s: Connected with bufline:\n%s\n",arg0, nmlP->cms->BufferLine);
  printf("%s: ... and  procline:\n%s\n",arg0, nmlP->cms->ProcessLine);

  NML_UNBOUNDED_ARRAY_TEST_MSG *testMsgP = 
    new NML_UNBOUNDED_ARRAY_TEST_MSG();
  

  // Examples using macros
  NML_UNBOUNDED_ALLOC(testMsgP->s2_m.c_ubP,10);
  NML_UNBOUNDED_ALLOC(testMsgP->s2_m.s1_in_s2_ubP,5);
  NML_UNBOUNDED_ALLOC(testMsgP->s2_ubP,10);
  NML_UNBOUNDED_ALLOC(testMsgP->s2_ubP[0].c_ubP,10);

  //example allocation without macro
  testMsgP->f_ubP = (float *)  malloc(sizeof(float)*5);
  memset(testMsgP->f_ubP,0,sizeof(float)*5);
  testMsgP->f_ubP_length = 1; // we allocated space for 5 but are only sending 1.
  testMsgP->f_ubP_size_allocated = 5;
  testMsgP->f_ubP[0] = 0.25;

  
  strcpy(testMsgP->s2_m.c_ubP,"string");
  strcpy(testMsgP->s2_ubP[0].c_ubP,"string2");

  NML_UNBOUNDED_ARRAY_TEST_MSG2 *testMsg2P = 
    new NML_UNBOUNDED_ARRAY_TEST_MSG2();
  

  // Examples using macros
  NML_UNBOUNDED_ALLOC(testMsg2P->s2_m.c_ubP,10);
  NML_UNBOUNDED_ALLOC(testMsg2P->s2_m.s1_in_s2_ubP,5);
  NML_UNBOUNDED_ALLOC(testMsg2P->s2_ubP,10);
  NML_UNBOUNDED_ALLOC(testMsg2P->s2_ubP[0].c_ubP,10);

  //example allocation without macro
  testMsg2P->f_ubP = (float *)  malloc(sizeof(float)*5);
  memset(testMsg2P->f_ubP,0,sizeof(float)*5);
  testMsg2P->f_ubP_length = 1; // we allocated space for 5 but are only sending 1.
  testMsg2P->f_ubP_size_allocated = 5;
  testMsg2P->f_ubP[0] = 0.25;

  
  strcpy(testMsg2P->s2_m.c_ubP,"string");
  strcpy(testMsg2P->s2_ubP[0].c_ubP,"string2");

  signal(SIGINT,my_sigint_handler);

  do
    {
      nmlP->write(testMsgP);
      esleep(repeat_delay);
      nmlP->write(testMsg2P);

      if(repeat_count > 0)
	{
	  repeat_count--;
	  printf("%s: %d more writes left.\n",arg0,repeat_count);
	}
      esleep(repeat_delay);

    } while( !quit && repeat_count != 0);

  printf("%s: Cleaning up.\n",arg0);
    
  NML_UNBOUNDED_FREE(testMsgP->s2_m.c_ubP);
  NML_UNBOUNDED_FREE(testMsgP->s2_m.s1_in_s2_ubP);
  NML_UNBOUNDED_FREE(testMsgP->s2_ubP[0].c_ubP);
  NML_UNBOUNDED_FREE(testMsgP->s2_ubP);

  // deallocate without the macro
  if(testMsgP->f_ubP)
    {
      free(testMsgP->f_ubP);
      testMsgP->f_ubP = 0;
      testMsgP->f_ubP_length = 0;
      testMsgP->f_ubP_size_allocated = 0;
    }

  NML_UNBOUNDED_FREE(testMsg2P->s2_m.c_ubP);
  NML_UNBOUNDED_FREE(testMsg2P->s2_m.s1_in_s2_ubP);
  NML_UNBOUNDED_FREE(testMsg2P->s2_ubP[0].c_ubP);
  NML_UNBOUNDED_FREE(testMsg2P->s2_ubP);

  // deallocate without the macro
  if(testMsg2P->f_ubP)
    {
      free(testMsg2P->f_ubP);
      testMsg2P->f_ubP = 0;
      testMsg2P->f_ubP_length = 0;
      testMsg2P->f_ubP_size_allocated = 0;
    }

  delete testMsgP;
  delete testMsg2P;
  delete nmlP;
  return 0;
}


  


  
