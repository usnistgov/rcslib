

#include "rcs.hh"		// Required for all RCS library applications.
#include "nmlset.hh"		// Declare the NMLSET,NMLDOMAINSET and NMLDOMAINSET_MEMBER classes.

#include "nml_test_format.hh"	// Specific message classes for these examples.

// Standard C/C++ headers
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>


// Setup Control-C signal handler for quitting this application.
static void (*old_sigint_handler)(int) =0;
static bool sigint_occured=false;

static void sigint_handler(int sig)
{
  if(old_sigint_handler && old_sigint_handler != SIG_IGN &&
     old_sigint_handler != SIG_DFL)
    {
      (*old_sigint_handler)(sig);
    }
  sigint_occured=true;
}

 
int
main(int argc, const char **argv)
{
  // Parse/handle  the command line arguments.
  if(argc < 4)
    {
      fprintf(stderr,"nml_test_nmlset usage: bufferlist procname cfgsvrlist [buffersneeded] [maxcycles]\n");
      exit(1);
    }
  int bufsneeded = -1;
  if(argc >= 5)
    {
      bufsneeded = strtol(argv[4],0,0);
      printf("bufsneeded=%d\n",bufsneeded);
    }
  int maxcycles = -1;
  if(argc >= 6)
    {
      maxcycles = strtol(argv[5],0,0);
      printf("maxcycles=%d\n",maxcycles);
    }
  int bufsread = 0;
  int cycles = 0;
  bool error_occured=false;
 

  // Create an NMLSET class object that will be used to
  // monitor all buffers in the ; delimited list in argv[1] on
  // nmlcfgsvr's listed in the ; delimeted list in argv[3],
  // pass argv[2] as our ProcessName.
  NMLSET s(nml_test_format,argv[1],argv[2],argv[3]);
  printf("NMLSET s(nml_test_format,argv[1]=%s,argv[2]=%s,argv[3]=%s);\n",
	 argv[1],argv[2],argv[3]);

  int *lastnum = 0;
  if(bufsneeded > 0)
    {
      lastnum = new int[bufsneeded];
    }
  for(int k = 0; k < bufsneeded; k++)
    {
      lastnum[k] = 0;
    }

  // Setup the signal handler so we can gracefully quit when
  // someone presses Control-C.
  old_sigint_handler = signal(SIGINT,sigint_handler);
  
  while(!sigint_occured)
    {
      cycles++;
      if(maxcycles > 0 && cycles > maxcycles)
	{
	  break;
	}
      printf("cycles=%d\n",cycles);
      printf("\n**********************************************************\n");

      // Check all of the configuration servers to see if there are new
      // buffers that match our criterea. If a server won't respond after
      // 1.0 seconds go on to the next one or return if it is the last one.
      s.update_set(1.0);
      printf("s..update_set(1.0);\n");

      // NMLSETS are divided into NMLDOMAINSETS, there could be an
      // infinite number, so we run though the list, by calling s.get_first_domainset() and then s.get_next_domainset() until it returns NULL.
      NMLDOMAINSET *nd = s.get_first_domainset();
      printf("NMLDOMAINSET *nd = s.get_first_domainset();\nnd=%p\n",
	     (void*)nd);
      while(nd)
	{
	  printf("nd->getdomainname() = %s\n",nd->getdomainname());
	  printf("nd->cfgsvrname() = %s\n",nd->getcfgsvrname());
	  
	  // Each NMLDOMAINSET has a fixed randomly accessible list of 
	  // members. The member id's correspond to the position of the
	  // buffer name in the bufferlist passed when the top level NMLSET
	  // object was created.
	  for(int i=0; i <= nd->get_max_member_id(); i++)
	    {
	      NMLDOMAINSET_MEMBER *nm = nd->get_member(i);
	      printf("NMLDOMAINSET_MEMBER *nm = nd->get_member(i=%d);\nnm=%p\n",i,nm);

	      if(nm)
		{
		  if(nm->cms && nm->cms->BufferName)
		    {
		      printf("nm->cms->BufferName=%s\n",nm->cms->BufferName);
		    }
		  NMLTYPE t;

		  // NMLDOMAINSET_MEMBER is publically derived from NML
		  // so we can do anything with nm that we could do 
		  // with an NML *, including read, write, peek, get_address()
		  // etc.
		  t = nm->peek();
		  if(t < 0)
		    {
		      error_occured=true;
		    }
		  printf("nm->peek() returned %d\n",t);
		  if(t == TEST_MESSAGE_TYPE)
		    {
		      TEST_MESSAGE *tm = (TEST_MESSAGE *) nm->get_address();
		      printf("TEST_MESSAGE *tm = (TEST_MESSAGE *) nm->get_address();\ntm=%p\n",(void *)tm);
		      printf("tm->i = %d, tm->lastvar=%d\n",tm->i,tm->lastvar);
		      int j=0;
		      if(tm->lastvar > 0 && bufsneeded > 0)
			{
			  for(j =0; j < bufsread; j++)
			    {
			      printf("j=%d,lastnum[j]=%d,bufsread=%d\n",
				     j,lastnum[j],bufsread);
			      if(lastnum[j] == tm->lastvar)
				{
				  break;
				}
			    }
			  printf("j=%d,lastnum[j]=%d,bufsread=%d\n",
				 j,lastnum[j],bufsread);
			  if(lastnum[j] == 0 && j == bufsread)
			    {
			      printf("new buffer found. -- bufsread=%d, j=%d,lastnum[j]=%d,tm->lastvar=%d\n",
				     bufsread,j,lastnum[j],tm->lastvar);
			      lastnum[j] = tm->lastvar;
			      bufsread++;
			      if(bufsread >= bufsneeded)
				{
				  break;
				}
			    }
			}
		    }
		  else
		    {
		      // Deleting an NMLDOMAINSET_MEMBER removes it from
		      // its parent NMLDOMAINSET.
		      printf("delete nm;\n");
		      delete nm;
		    }
		}
	    }
	  if(bufsneeded > 0 && bufsread >= bufsneeded)
	    {
	      break;
	    }
	  nd = s.get_next_domainset();
	  printf("NMLDOMAINSET *nd = s.get_next_domainset();\nnd=%p\n",
		 (void*)nd);
	}
      if(bufsneeded > 0 && bufsread >= bufsneeded)
	{
	  break;
	}
      printf("esleep(2.5);\n");
      fflush(stdout);
      esleep(2.5);
    }

  // clean up the signal handler.
  signal(SIGINT,old_sigint_handler);


  // Set the return value so we can use this program in test scripts.
  if(error_occured)
    {
      exit(2);
    }
  if(cycles > maxcycles && maxcycles > 0)
    {
      exit(3);
    }
  exit(0);
}

