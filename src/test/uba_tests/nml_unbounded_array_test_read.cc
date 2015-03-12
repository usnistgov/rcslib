

#include "rcs.hh"
#include "nml_unbounded_array_test_msg.hh"
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

static bool quit = false;

static void
my_sigint_handler(int)
{
  quit = true;
}

      
int
main(int argc, const char **argv)
{
  int repeat_count = 0;
  double repeat_delay = 0.2;
  const char *bufname =0;
  const char *procname =0;
  const char *configfile =0;

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

  NML *nmlP = new NML(nml_unbounded_array_test_msg_format,
		      bufname, procname, configfile);

  bool error_occured = false;
  NML_UNBOUNDED_ARRAY_TEST_MSG *testMsgP = 0;
  NML_UNBOUNDED_ARRAY_TEST_MSG2 *testMsg2P = 0;

  signal(SIGINT,my_sigint_handler);

  do
    {
      switch(nmlP->read())
	{
	case 0:
	  // no new data -- ignore.
	  break;

	case -1:
	  // comm error
	  error_occured = true;
	  quit=true;
	  break;

	case NML_UNBOUNDED_ARRAY_TEST_MSG_TYPE:
	  testMsg2P = 0;
	  testMsgP = 
	    (NML_UNBOUNDED_ARRAY_TEST_MSG *) nmlP->get_address();
	  printf("testMsgP->s2_m.c_ubP=%p, testMsgP->s2_m.c_ubP_length=%d, testMsgP->s2_m.c_ubP_size_allocated=%d\n",
		 testMsgP->s2_m.c_ubP,testMsgP->s2_m.c_ubP_length, testMsgP->s2_m.c_ubP_size_allocated);
	  if(testMsgP->s2_m.c_ubP)
	    {
	      printf("testMsgP->s2_m.c_ubP=%s\n",
		     testMsgP->s2_m.c_ubP);
	    }
	  printf("testMsgP->s2_m.s1_in_s2_ubP=%p, testMsgP->s2_m.s1_in_s2_ubP_length=%d, testMsgP->s2_m.s1_in_s2_ubP_size_allocated=%d\n",
		 testMsgP->s2_m.s1_in_s2_ubP,testMsgP->s2_m.s1_in_s2_ubP_length, testMsgP->s2_m.s1_in_s2_ubP_size_allocated);
	  printf("testMsgP->s2_ubP=%p, testMsgP->s2_ubP_length=%d, testMsgP->s2_ubP_size_allocated=%d\n",
		 testMsgP->s2_ubP,testMsgP->s2_ubP_length, testMsgP->s2_ubP_size_allocated);
	  if(testMsgP->s2_ubP)
	    {
	      printf("testMsgP->s2_ubP[0].c_ubP=%p, testMsgP->s2_ubP[0].c_ubP_length=%d, testMsgP->s2_ubP[0].c_ubP_size_allocated=%d\n",
		 testMsgP->s2_ubP[0].c_ubP,testMsgP->s2_ubP[0].c_ubP_length, testMsgP->s2_ubP[0].c_ubP_size_allocated);
	      if(testMsgP->s2_ubP[0].c_ubP)
		{
		  printf("testMsgP->s2_ubP[0].c_ubP=%s\n",
			 testMsgP->s2_ubP[0].c_ubP);
		  NML_UNBOUNDED_FREE(testMsgP->s2_ubP[0].c_ubP);
		}
	    }
	  printf("testMsgP->f_ubP=%p, testMsgP->f_ubP_length=%d, testMsgP->f_ubP_size_allocated=%d\n",
		 testMsgP->f_ubP,testMsgP->f_ubP_length, testMsgP->f_ubP_size_allocated);
	  if(testMsgP->f_ubP)
	    {
	      printf("testMsgP->f_ubP[0]=%f\n",
		     testMsgP->f_ubP[0]);
	    }
	  break;


	case NML_UNBOUNDED_ARRAY_TEST_MSG2_TYPE:
	  testMsgP = 0;
	  testMsg2P = 
	    (NML_UNBOUNDED_ARRAY_TEST_MSG2 *) nmlP->get_address();
	  printf("testMsg2P->s2_m.c_ubP=%p, testMsg2P->s2_m.c_ubP_length=%d, testMsg2P->s2_m.c_ubP_size_allocated=%d\n",
		 testMsg2P->s2_m.c_ubP,testMsg2P->s2_m.c_ubP_length, testMsg2P->s2_m.c_ubP_size_allocated);
	  if(testMsg2P->s2_m.c_ubP)
	    {
	      printf("testMsg2P->s2_m.c_ubP=%s\n",
		     testMsg2P->s2_m.c_ubP);
	    }
	  printf("testMsg2P->s2_m.s1_in_s2_ubP=%p, testMsg2P->s2_m.s1_in_s2_ubP_length=%d, testMsg2P->s2_m.s1_in_s2_ubP_size_allocated=%d\n",
		 testMsg2P->s2_m.s1_in_s2_ubP,testMsg2P->s2_m.s1_in_s2_ubP_length, testMsg2P->s2_m.s1_in_s2_ubP_size_allocated);
	  printf("testMsg2P->s2_ubP=%p, testMsg2P->s2_ubP_length=%d, testMsg2P->s2_ubP_size_allocated=%d\n",
		 testMsg2P->s2_ubP,testMsg2P->s2_ubP_length, testMsg2P->s2_ubP_size_allocated);
	  if(testMsg2P->s2_ubP)
	    {
	      printf("testMsg2P->s2_ubP[0].c_ubP=%p, testMsg2P->s2_ubP[0].c_ubP_length=%d, testMsg2P->s2_ubP[0].c_ubP_size_allocated=%d\n",
		 testMsg2P->s2_ubP[0].c_ubP,testMsg2P->s2_ubP[0].c_ubP_length, testMsg2P->s2_ubP[0].c_ubP_size_allocated);
	      if(testMsg2P->s2_ubP[0].c_ubP)
		{
		  printf("testMsg2P->s2_ubP[0].c_ubP=%s\n",
			 testMsg2P->s2_ubP[0].c_ubP);
		  NML_UNBOUNDED_FREE(testMsg2P->s2_ubP[0].c_ubP);
		}
	    }
	  printf("testMsg2P->f_ubP=%p, testMsg2P->f_ubP_length=%d, testMsg2P->f_ubP_size_allocated=%d\n",
		 testMsg2P->f_ubP,testMsg2P->f_ubP_length, testMsg2P->f_ubP_size_allocated);
	  if(testMsg2P->f_ubP)
	    {
	      printf("testMsg2P->f_ubP[0]=%f\n",
		     testMsg2P->f_ubP[0]);
	    }
	  break;
	  
	default:
	  fprintf(stderr,"Unknown message type %ld\n",
		  nmlP->get_address()->type);
	  error_occured=true;
	  quit=true;
	}

      if(repeat_count > 0)
	{
	  repeat_count--;
	}
      esleep(repeat_delay);
    } while( !quit && repeat_count != 0);



  // These dealloctions are not really necessary as the memory will
  // be freed when we exit anyway but within a larger program 
  // that was going to go on to do other things not deleting them 
  // will cause a memory leak.
  if(testMsgP)
    {
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
    }


  if(testMsg2P)
    {
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
    }

  delete nmlP;
  if(!error_occured)
    {
      return 0;
    }
  return 1;
}


  


  
