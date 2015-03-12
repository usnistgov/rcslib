/*
#   ,---------------------------------------------------------------------
#   | DISCLAIMER
#   |---------------------------------------------------------------------
#   | This software was developed at the National Institute of Standards  
#   | and Technology by employees of the Federal Government in the course
#   | of their official duties. Pursuant to title 17 Section 105 of the   
#   | United States Code this software is not subject to copyright        
#   | protection and is in the public domain. NIST's [intelligent         
#   | mobility] software is anexperimental system. NIST assumes no       
#   | responsibility whatsoever for its use by other parties, and makes no
#   | guarantees, expressed or implied, about its quality, reliability, or
#   | any other characteristic. We would appreciate acknowledgement if the
#   | software is used. This software can be redistributed and/or modified
#   | freely provided that any derivative works bear some notice that they
#   | are derived from it, and any modified versions bear some notice that
#   | they have been modified.                                            
#   `---------------------------------------------------------------------
*/

#include "rcs.hh"

#include "log_data_format.hh"

//#include "muChameleonData.hh"
#include "labjackData.hh"
#include "LadarData.hh"
#include "LadarCmd.hh"
#include "logRecorderCmd.hh"
#include "logRecorderSts.hh"

#ifdef HAVE_MOAST
#include "moastLogRecordSuperCmd.hh"
#include "moastLogRecordSuperSts.hh"
#include "amMobJA.hh"
#include "primMobJA.hh"
#include "servoMobJA.hh"
#include "primDebug.hh"
#include "primSP.hh"
#include "sensorData.hh"
#include "navDataExt.hh"
#include "atrvTune.hh"
#include "nav200Dbg.hh"
#endif

#include "logPlaybackCmd.hh"
#include "logPlaybackSts.hh"
#include "sr3000Cmd.hh"
#include "sr3000Sts.hh"
#include "PMDCmd.hh"
#include "PMDSts.hh"
#include "CanestaCmd.hh"
#include "CanestaSts.hh"
#include "LadarPatches.hh"
// #include "sensorData.hh"
		
#include "navdata_v2.hh"
//#include "sickData_v3.hh"
#include "sickData.hh"
#include "sonarData.hh"
#include "playerPosData.hh"
#include "PointCloud.hh"
#include "range3DMsg.hh"

#include "fwCameraLog_v2.hh"
#include "fwCameraData_v2.hh"
		
#include "sicksafetyData.hh"
#include "SkyTrax_TCPMsg.hh"

#include <stdio.h>
#include <stdlib.h>

static bool debug=false;

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

const char *THIS_PROCESS_NAME="ccrada_nml_svr";

char *CONFIG_NML=0;

int ignored_count=0;
int err_count=0;
int ok_count = 0;

static void inline 
OPEN_STAT_CHANNEL(NML_FORMAT_PTR _format_function_ptr, 
		  const char *buffer_name)
{
  if(debug) printf("Opening RCS_STAT_CHANNEL  %s.\n", buffer_name);
  RCS_STAT_CHANNEL *statChan = 
    new RCS_STAT_CHANNEL(_format_function_ptr,buffer_name,THIS_PROCESS_NAME,CONFIG_NML);
  if(statChan && 
     (statChan->error_type == NML_IGNORED_REMOTE ||
      statChan->error_type == NML_IGNORED_NO_BUFLINE ))
    {
      delete statChan;
      statChan=0;
      if(debug) printf("%s ignored.\n",buffer_name);
      ignored_count++;
    }
  else if(!statChan || !statChan->valid())
    {
      delete statChan;
      statChan=0;
      if(debug) printf("%s failed.\n",buffer_name);
      err_count++;
    }
  else
    {
      if(debug) printf("%s ok.\n",buffer_name);
      ok_count++;
    }      
}

static void inline 
OPEN_CMD_CHANNEL(NML_FORMAT_PTR _format_function_ptr, 
		  const char *buffer_name)
{
  if(debug) printf("Opening RCS_CMD_CHANNEL  %s.\n", buffer_name);
  RCS_CMD_CHANNEL *cmdChan = 
    new RCS_CMD_CHANNEL(_format_function_ptr,buffer_name,THIS_PROCESS_NAME,CONFIG_NML);
  if(cmdChan && 
     (cmdChan->error_type == NML_IGNORED_REMOTE ||
      cmdChan->error_type == NML_IGNORED_NO_BUFLINE))
    {
      delete cmdChan;
      cmdChan=0;
      if(debug) printf("%s ignored.\n",buffer_name);
      ignored_count++;
    }
  else if(!cmdChan || !cmdChan->valid())
    {
     delete cmdChan;
      cmdChan=0;
      if(debug) printf("%s failed.\n",buffer_name);
      err_count++;
    }
  else
    {
      if(debug) printf("%s ok.\n",buffer_name);
      ok_count++;
    }      
}


static void inline 
OPEN_NML(NML_FORMAT_PTR _format_function_ptr, 
		  const char *buffer_name)
{
  if(debug) printf("Opening NML %s.\n", buffer_name);
  NML *nmlChan = 
    new NML(_format_function_ptr,buffer_name,THIS_PROCESS_NAME,CONFIG_NML);
  if(nmlChan && 
     ( nmlChan->error_type == NML_IGNORED_REMOTE ||
       nmlChan->error_type == NML_IGNORED_NO_BUFLINE))
    {
      delete nmlChan;
      nmlChan=0;
      if(debug) printf("%s ignored.\n",buffer_name);
      ignored_count++;
    }
  else if(!nmlChan || !nmlChan->valid())
    {
      delete nmlChan;
      nmlChan=0;
      if(debug) printf("%s failed.\n",buffer_name);
      err_count++;
    }
  else
    {
      if(debug) printf("%s ok.\n",buffer_name);
      ok_count++;
    }      
}

static const char * const  NML_SVR_ID="$Id:$";
const char *RUNNING_FILE="ccrada_nml_svr.running";

static const char **_argv=0;
static int _argc=0;

static void
touch_running_file()
{
  char *RUNNING_FILE_ENV=getenv("RUNNING_FILE");
  if(RUNNING_FILE_ENV)
    {
      RUNNING_FILE=RUNNING_FILE_ENV;
    }
  FILE *f = fopen(RUNNING_FILE,"w");
  if(!f)
    {
      fprintf(stderr,"Can't open %s -- %s \n",
	      RUNNING_FILE,strerror(errno));
      return;
    }
  else
    {
      if(_argv && _argv[0]) {
	fprintf(f,"prog=\t%s\n", _argv[0]);
	fprintf(f,"cmd_line=\t%s", _argv[0]);
	for(int i=1; i < _argc; i++)
	  {
	    fprintf(f," %s", _argv[i]);
	  }
	fprintf(f,"\n");
	fprintf(f,"%s compiled from %s at %s on %s with id %s starting ...\n",
		_argv[0],__FILE__, __DATE__,__TIME__, NML_SVR_ID);
      }
      fprintf(f,"\n%d ignored, %d failed, %d ok\n",
	      ignored_count, err_count, ok_count);
      fflush(f);
      fclose(f);
    }
}

struct named_format_struct
{
  const char *name;
  NML_FORMAT_PTR _format_function_ptr;
};

static struct named_format_struct format_array[] =
  { 
    { "LadarData", LadarData_format },
    { "LadarCmd", LadarCmd_format },
    { "labjackData", labjackData_format },
    //    { "sensorData", sensorData_format },
    { "fwCameraData", fwCameraData_format },
    { "fwCameraLog_v2", fwCameraLog_v2_format },
    { "navdata_v2", navdata_v2_format },
    { "sickData", sickData_format },
    { "playerPosData", playerPosData_format },
    { "sonarData", sonarData_format },
    { "logRecorderCmd", logRecorderCmd_format },
    { "logRecorderSts", logRecorderSts_format },
    { "logPlaybackCmd", logPlaybackCmd_format },
    { "logPlaybackSts", logPlaybackSts_format },
    { "PointCloud", PointCloud_format },
    { "range3DMsg", range3DMsg_format },
    { "sensorData", sensorData_format },
    { "log_data", log_data_format },
  };
  
static const size_t format_array_len = 
  sizeof(format_array)/sizeof(format_array[0]);
  
static void printhelp() 
{
  fprintf(stderr,"\n");
  fprintf(stderr,"ccrada_nml_svr help:\n");
  fprintf(stderr,"options: \n");
  fprintf(stderr,"\t--help\tPrint this help.\n");
  fprintf(stderr,"\t--stat\tUse RCS_STAT_CHANNEL() for subsequent buffers.\n");
  fprintf(stderr,"\t--cmd\tUse RCS_CMD_CHANNEL() for subsequent buffers.\n");
  fprintf(stderr,"\t--generic\tUse NML() rather than RCS_CMD_CHANNEL or RCS_STAT_CHANNEL  for folllowing buffers.\n");
  fprintf(stderr,"\t--nmlproc <procname>\tSet the process name for subsequent buffers.\n");
  fprintf(stderr,"\t\tDefaults to \"ccrada_nml_svr\"\n");
  fprintf(stderr,"\t--running_file <filename>\tSet the name of a file that is created so scripts can know the server is up and running.\n");
  fprintf(stderr,"\t\tDefaults to \"ccrada_nml_svr.running\"\n");

  fprintf(stderr,"\t--format <formatname>\tSet the format function for subsequent buffers\n");
  fprintf(stderr,"\tValid formats include:\n");
  for(int j = 0; j < format_array_len; j++) {
    fprintf(stderr,"\t\t%s\n",format_array[j].name);
  };
  fprintf(stderr,"\n");
  fprintf(stderr,"The environment variable CONFIG_NML\n");
  fprintf(stderr,"should be set to the name of the configuration file or server to use\n");
  fprintf(stderr,"\n");
  fprintf(stderr,"To see details as it starts up:\n");
  fprintf(stderr,"setenv DEBUG 1\n");
  fprintf(stderr,"\n");

  // fprinff(stderr,"Example:\n");
  // fprinff(stderr,"./ccrada_nml_svr --format LadarData kinect\n");
}

int 
main(int argc, const char** argv)
{
  _argv = argv;
  _argc = argc;

  CONFIG_NML = getenv("CONFIG_NML");
  if (!CONFIG_NML)
    {
      fprintf(stderr,"%s: Please set the CONFIG_NML environment variable.\n",
	      argv[0]);
      printhelp();
      exit(1);
    }

  printf("%s compiled from %s at %s on %s with id %s starting ...\n",
	 argv[0],__FILE__, __DATE__,__TIME__, NML_SVR_ID);

  debug = (getenv("DEBUG") != 0);

  nmlSetToServer();
  nmlSetIgnoreRemote();
  nmlSetIgnoreNoBufline();
  load_nml_config_file(CONFIG_NML);

  enum {
    cmd,stat,generic
  } buftype = generic;

  NML_FORMAT_PTR _format_function_ptr = LadarData_format;

  for(int i = 0; i < argc; i++) {
    if(!strcmp(argv[i],"--help")) {
      printhelp();
    } else if(!strcmp(argv[i],"--cmd")) {
      buftype = cmd;
    }
    else if(!strcmp(argv[i],"--stat")) {
      buftype = stat;
    }
    else if(!strcmp(argv[i],"--generic")) {
      buftype = generic;
    }
    else if(!strcmp(argv[i],"--nmlproc")) {
      i++;
      THIS_PROCESS_NAME = argv[i];
    }
    else if(!strcmp(argv[i],"--running_file")) {
      i++;
      RUNNING_FILE= argv[i];
    }
    else if(!strcmp(argv[i],"--format")) {
      i++;
      bool format_found = false;
      for(int j = 0; j < format_array_len; j++) {
	if(!strcmp(argv[i],format_array[j].name)) {
	  _format_function_ptr = format_array[j]._format_function_ptr;
	  format_found = true;
	  break;
	} 
      }
      if(!format_found) {
	fprintf(stderr,"%s: Unrecognized format %s\n",
		argv[0],argv[i]);
	printhelp();
	exit(1);
      }
    } else if(argv[i][0] == '-') {
      fprintf(stderr,"%s: Unrecognized argument %s\n",
	      argv[0],argv[i]);
      printhelp();
      exit(1);
    }
    switch(buftype) {
    case generic:
      OPEN_NML(_format_function_ptr,argv[i]);
      break;

    case cmd:
      OPEN_CMD_CHANNEL(_format_function_ptr,argv[i]);
      break;

    case stat:
      OPEN_STAT_CHANNEL(_format_function_ptr,argv[i]);
      break;
    }
  }

  if(ok_count > 0)
    {
      printf("%s compiled from file %s on %s at %s with id %s running nml servers . . .\n",
	     argv[0],__FILE__,__DATE__, __TIME__,NML_SVR_ID);

      run_nml_servers_with_func(touch_running_file);
    }
  else
    {
      printf("%s compiled from file %s on %s at %s with id %s exiting because no buffers are ok.\n",
	     argv[0],__FILE__, __DATE__, __TIME__,NML_SVR_ID);
    }

  nml_cleanup();
  return 0;
}
