/* 
The NIST RCS (Real-time Control Systems) 
 library is public domain software, however it is preferred
 that the following disclaimers be attached.

Software Copywrite/Warranty Disclaimer

   This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST Real-Time Control System software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.

*/ 

#define NEED_VPRINT 1

#if HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"

#else
#include "rcs_prnt_no_config.h"
#endif

#include "linklist.hh"
#include "rcs_prnt.hh"

#ifdef DEBUG_MEMORY
#include "dbg_mem.h"
#else
#ifndef DEBUG_MALLOC
#define DEBUG_MALLOC malloc
#endif
#ifndef DEBUG_CALLOC
#define DEBUG_CALLOC calloc
#endif
#ifndef DEBUG_REALLOC
#define DEBUG_REALLOC realloc
#endif
#ifndef DEBUG_FREE
#define DEBUG_FREE free
#endif
#endif
// DEBUG_MEMORY

#ifndef _TIMER_H
extern "C" double etime (void);
#endif

class RCS_LINKED_LIST *rcs_print_list = NULL;
char **rcs_lines_table = NULL;

const char *rcs_print_tag=0;

void set_rcs_print_tag(const char *new_tag)
{
  rcs_print_tag = new_tag;
}

#ifdef DISABLE_RCS_PRINT
void setRcsDoNotPrintTimeoutErrors(int)
{
}

int getRcsDoNotPrintTimeoutErrors(void)
{
  return 1;
}

#else

static int rcs_do_not_print_timeout_errors=0;

void setRcsDoNotPrintTimeoutErrors(int _new_val)
{
  rcs_do_not_print_timeout_errors=_new_val;
}

int getRcsDoNotPrintTimeoutErrors(void)
{
  return rcs_do_not_print_timeout_errors;
}
#endif


#ifndef DISABLE_RCS_PRINT

// The following came from rpatel@gdrs.com Rcslib_Tweaks_for_GDRS.doc 
// Monday Nov. 27, 2006
#ifndef DEFAULT_RCS_PRINT_DESTINATION
#define DEFAULT_RCS_PRINT_DESTINATION RCS_PRINT_TO_STDOUT
#endif

static enum RCS_PRINT_DESTINATION_TYPE rcs_print_destination = 
  DEFAULT_RCS_PRINT_DESTINATION;

static bool print_start_time_set=false;
static double print_start_time=0.0;

#endif


int rcs_print_do_flush=0;

void  set_rcs_print_do_flush(int _do_flush)
{
  rcs_print_do_flush = _do_flush;
}

#if defined(MS_WINDOWS_API) && defined(HAVE_ALLOC_CONSOLE)
BOOL AllocConsoleCalled = FALSE;
BOOL AllocConsoleRetVal = FALSE;
HANDLE OutputHandle = NULL;
#endif

#if 0 
static pthread_t deferred_stdout_processing_thread=0;
static pthread_cond_t deferred_stdout_cond = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t deferred_stdout_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif


int max_rcs_errors_to_print = 30;
int rcs_errors_printed = 0;

bool pause_on_rcs_error=false;
bool abort_on_rcs_error=false;

void
set_abort_on_rcs_error(int _do_abort)
{
  init_rcs_print_mode_flags();
  abort_on_rcs_error = (bool) (_do_abort != 0);
}

void
set_pause_on_rcs_error(int _do_pause)
{
  init_rcs_print_mode_flags();
  pause_on_rcs_error = (bool) (_do_pause != 0);
}


bool rcs_print_mode_flags_set = false;
unsigned long rcs_print_mode_flags = PRINT_RCS_ERRORS;
#ifndef DISABLE_RCS_DEBUG_PRINT
char rcs_debugging_enabled =1;
#endif

FILE *rcs_print_file_stream = NULL;
char rcs_print_file_name[80] = "rcs_out.txt";

char last_error_bufs[4][100];
int error_bufs_initialized = 0;
int last_error_buf_filled = 0;

#if !defined(HAVE_CONFIG_H) && !defined(NO_VSNPRINTF) && !defined(HAVE_VSNPRINTF) && !defined(VXWORKS)
#define HAVE_VSNPRINTF 1
#endif


void
set_rcs_print_destination (RCS_PRINT_DESTINATION_TYPE _dest)
{
#ifndef DISABLE_RCS_DEBUG_PRINT
  if(!rcs_print_mode_flags_set)
    {
      init_rcs_print_mode_flags();
    }
#endif
  if (rcs_print_destination == RCS_PRINT_TO_NULL)
    {
      rcs_errors_printed = 0;
    }
  rcs_print_destination = _dest;
  rcs_print_tag=0;
}

RCS_PRINT_DESTINATION_TYPE
get_rcs_print_destination ()
{
  return (rcs_print_destination);
}

char **
get_rcs_lines_table ()
{
  return (rcs_lines_table);
}

class RCS_LINKED_LIST *
get_rcs_print_list ()
{
  return (rcs_print_list);
}

int
get_rcs_print_list_size ()
{
  if (NULL != rcs_print_list)
    {
      return (rcs_print_list->list_size);
    }
  else
    {
      return (-1);
    }
}
void
set_rcs_print_list_sizing (int _new_max_size,
			   LIST_SIZING_MODE _new_sizing_mode)
{
  if (NULL == rcs_print_list)
    {
      rcs_print_list = new RCS_LINKED_LIST;
    }
  if (NULL != rcs_print_list)
    {
      rcs_print_list->set_list_sizing_mode (_new_max_size, _new_sizing_mode);
    }
  rcs_print_tag=0;
}

void
clean_print_list ()
{
  if (NULL != rcs_print_list)
    {
      delete rcs_print_list;
      rcs_print_list = NULL;
    }
}

void
output_print_list (int output_func (char *))
{
  if (NULL != rcs_print_list)
    {
      char *string_from_list;
      string_from_list = (char *) rcs_print_list->get_head ();
      while (NULL != string_from_list)
	{
	  if (output_func (string_from_list) != EOF)
	    {
	      break;
	    }
	  string_from_list = (char *) rcs_print_list->get_next ();
	}
    }
}

int
count_characters_in_print_list ()
{
  int count = 0;
  if (NULL != rcs_print_list)
    {
      char *string_from_list;
      string_from_list = (char *) rcs_print_list->get_head ();
      while (NULL != string_from_list)
	{
	  count += (int) strlen (string_from_list);
	  string_from_list = (char *) rcs_print_list->get_next ();
	}
    }
  return (count);
}

int
count_lines_in_print_list ()
{
  int count = 1;
  if (NULL != rcs_print_list)
    {
      char *string_from_list;
      string_from_list = (char *) rcs_print_list->get_head ();
      while (NULL != string_from_list)
	{
	  char *line;
	  line = strchr (string_from_list, '\n');
	  while (NULL != line)
	    {
	      count++;
	      line = strchr (line + 1, '\n');
	    }
	  string_from_list = (char *) rcs_print_list->get_next ();
	}
    }
  return (count);
}

void
convert_print_list_to_lines ()
{
  char *temp_buf = NULL;
  static int last_id_converted = -1;
  if (NULL != rcs_print_list)
    {
      char *string_from_list;
      if (-1 == last_id_converted)
	{
	  string_from_list = (char *) rcs_print_list->get_head ();
	}
      else
	{
	  string_from_list =
	    (char *) rcs_print_list->get_first_newer (last_id_converted);
	}
      while (NULL != string_from_list)
	{
	  char *next_line;
	  next_line = strchr (string_from_list, '\n');
	  if (NULL == next_line)
	    {
	      if (NULL == temp_buf)
		{
		  temp_buf =
		    (char *) DEBUG_MALLOC (strlen (string_from_list) + 1);
		  strcpy (temp_buf, string_from_list);
		}
	      else
		{
		  temp_buf = (char *) DEBUG_REALLOC (temp_buf,
						     strlen (temp_buf)
						     +
						     strlen (string_from_list)
						     + 1);
		  strcat (temp_buf, string_from_list);
		}
	      rcs_print_list->delete_current_node ();
	    }
	  else
	    {
	      if (temp_buf != NULL)
		{
		  temp_buf = (char *) DEBUG_REALLOC (temp_buf,
						     strlen (temp_buf)
						     +
						     strlen (string_from_list)
						     + 1);
		  strcat (temp_buf, string_from_list);
		  rcs_print_list->delete_current_node ();
		  rcs_print_list->store_after_current_node (temp_buf,
							    strlen (temp_buf)
							    + 1, 1);
		  DEBUG_FREE (temp_buf);
		  temp_buf = NULL;
		}
	      else if (next_line[1] != 0)
		{
		  rcs_print_list->store_after_current_node (next_line + 1,
							    strlen (next_line
								    + 1) + 1,
							    1);
		  next_line[1] = 0;
		}
	    }
	  string_from_list = (char *) rcs_print_list->get_next ();
	}
    }
  last_id_converted = rcs_print_list->get_newest_id ();
  if (temp_buf != NULL)
    {
      rcs_print_list->store_at_tail (temp_buf, strlen (temp_buf) + 1, 1);
      DEBUG_FREE (temp_buf);
      temp_buf = NULL;
    }
}

void
update_lines_table ()
{
  if (NULL != rcs_lines_table)
    {
      DEBUG_FREE (rcs_lines_table);
      rcs_lines_table = NULL;
    }
  if (NULL != rcs_print_list)
    {
      convert_print_list_to_lines ();
      rcs_lines_table = (char **) DEBUG_MALLOC (sizeof (char *)
							*
							rcs_print_list->
							list_size);
      if (NULL != rcs_print_list)
	{
	  char *string_from_list;
	  string_from_list = (char *) rcs_print_list->get_head ();
	  int i = 0;
	  while (NULL != string_from_list)
	    {
	      rcs_lines_table[i] = string_from_list;
	      i++;
	      string_from_list = (char *) rcs_print_list->get_next ();
	    }
	}
    }
}

char *
strip_control_characters (char * _dest, char * _src)
{
  static char line_buffer[255];
  char *destination;
  char *control_char_loc;
  if (NULL == _dest)
    {
      destination = line_buffer;
      if (strlen (_src) < 255)
	{
	  strcpy (line_buffer, _src);
	}
      else
	{
	  if (NULL == strpbrk (_src, "\n\r\t\b"))
	    {
	      return (_src);
	    }
	  else
	    {
	      return (NULL);
	    }
	}
    }
  else
    {
      destination = _dest;
      if (_dest != _src)
	{
#ifndef __CENTERLINE__		/*  CenterLine messed up string.h for C++ so that memmove will be undefined. */
	  memmove (_dest, _src, strlen (_src));
#else
	  strcpy (line_buffer, _src);
	  strcpy (_dest, line_buffer);
#endif
	}
    }
  control_char_loc = strpbrk (destination, "\n\r\t\b");
  while (NULL != control_char_loc)
    {
      *control_char_loc = ' ';	/* Replace control character with SPACE */
      control_char_loc = strpbrk (control_char_loc, "\n\r\t\b");
    }
  return (destination);

}


void
bad_char_to_print (char *ptr)
{
  if (ptr)
    {
      *ptr = '?';
    }
}

static int
rcs_write_string (const char * _str, int save_string)
{
  int retval = EOF;
  if (NULL != _str)
    {
      if (0 == _str[0])
	{
	  return (0);
	}

      if (save_string)
	{
	  if (!error_bufs_initialized)
	    {
	      memset (last_error_bufs[0], 0, 100);
	      memset (last_error_bufs[1], 0, 100);
	      memset (last_error_bufs[2], 0, 100);
	      memset (last_error_bufs[3], 0, 100);
	      error_bufs_initialized = 1;
	    }
	  last_error_buf_filled++;
	  last_error_buf_filled %= 4;
	  strncpy (last_error_bufs[last_error_buf_filled], _str, 99);
	}

      switch (rcs_print_destination)
	{
	  // The following came from rpatel@gdrs.com Rcslib_Tweaks_for_GDRS.doc 
	  // Monday Nov. 27, 2006
#if defined(WIN32) && !defined(UNDER_CE)
	case RCS_PRINT_TO_WIN32_DEBUGGER:
	  OutputDebugString(_str);
	  break;
#endif

#ifdef VXWORKS
	case RCS_PRINT_TO_LOGGER:
	  retval = logMsg ((char *)_str, 0, 0, 0, 0, 0, 0);
	  break;
#else
	case RCS_PRINT_TO_LOGGER:
#endif

	case RCS_PRINT_TO_STDOUT:
#if defined(MS_WINDOWS_API) && defined(HAVE_ALLOC_CONSOLE)
	  if (!AllocConsoleCalled)
	    {
	      AllocConsoleRetVal = AllocConsole ();
	      AllocConsoleCalled = TRUE;
	    }
	  OutputHandle = GetStdHandle (STD_OUTPUT_HANDLE);
	  if (NULL == OutputHandle)
	    {
	      retval = fputs (_str, stdout);
	      if(rcs_print_do_flush)
		{
		  fflush (stdout);
		}
	    }
	  else
	    {
	      DWORD bytes_written;
	      WriteConsole (OutputHandle, _str, strlen (_str),
			    (LPDWORD) & bytes_written, NULL);
	      retval = (int) bytes_written;
	    }
	  // MS_WINDOWS_API && HAVE_ALLOC_CONSOLE
#else
	  retval = fputs (_str, stdout);
	  if(rcs_print_do_flush)
	    {
	      fflush (stdout);
	    }
#endif
	  break;

	case RCS_PRINT_TO_STDERR:
	  retval = fputs (_str, stderr);
	  if(rcs_print_do_flush)
	    {
	      fflush (stderr);
	    }
	  break;

	case RCS_PRINT_TO_LIST:
	  if (NULL == rcs_print_list)
	    {
	      rcs_print_list = new RCS_LINKED_LIST();
	      if (NULL != rcs_print_list)
		{
		  rcs_print_list->set_list_sizing_mode (256,
							DELETE_FROM_HEAD);
		}
	    }
	  if (NULL != rcs_print_list)
	    {
	      retval = (int) strlen (_str);
	      if (-1 ==
		  rcs_print_list->store_at_tail ((void *)_str,
						 (retval + 1), 1))
		{
		  retval = EOF;
		}
	    }
	  break;

	case RCS_PRINT_TO_NULL:
	  retval = (int) strlen (_str);
	  break;

	case RCS_PRINT_TO_FILE:
	  if (NULL == rcs_print_file_stream)
	    {
	      if (NULL == rcs_print_file_name)
		{
		  return EOF;
		}
	      rcs_print_file_stream = fopen (rcs_print_file_name, "a+");
	    }
	  if (NULL == rcs_print_file_stream)
	    {
	      return EOF;
	    }
	  retval = fputs (_str, rcs_print_file_stream);
	  if(rcs_print_do_flush)
	    {
	      fflush (rcs_print_file_stream);
	    }
	  break;

#if  defined(MS_WINDOWS_API) && defined(HAVE_MESSAGE_BOX)
	case RCS_PRINT_TO_MESSAGE_BOX:
#ifdef UNICODE
	  wchar_t wstr2[256];
	  RCS_CE_ASCII_TO_UNICODE (wstr2, _str, 256);
	  retval = strlen (_str);
	  if (IDOK != MessageBox (NULL, wstr2, TEXT ("RCS Message"), MB_OK))
	    {
	      retval = EOF;
	    }
#else
	  retval = strlen (_str);
	  if (IDOK != MessageBox (NULL, _str, "RCS Message", MB_OK))
	    {
	      retval = EOF;
	    }
#endif
	  break;
	  // MS_WINDOWS_API && HAVE_MESSAGE_BOX
#endif

	default:
	  break;
	}
    }
  return (retval);
}

int
rcs_vprint (const char * _fmt, va_list _args, int save_string)
{
  static char temp_string[4096];
  char *ptr = 0;
  char *endptr = 0;

  if (NULL == _fmt)
    {
      return (EOF);
    }

#ifdef HAVE_VSNPRINTF
  if (EOF == (int) vsnprintf (temp_string, sizeof (temp_string), _fmt, _args))
    {
      return (EOF);
    }
#else
#ifdef HAVE__VSNPRINTF
  if (EOF ==
      (int) _vsnprintf (temp_string, sizeof (temp_string), _fmt, _args))
    {
      return (EOF);
    }
#else
  /* Return EOF if we are likely  overflow temp_string. */
  if (strlen (_fmt) > sizeof (temp_string) / 2)
    {
      return (EOF);
    }
  if (EOF == (int) vsprintf (temp_string, _fmt, _args))
    {
      return (EOF);
    }
#endif
#endif

  ptr = temp_string;
  endptr = temp_string + sizeof (temp_string);
  while (*ptr && ptr < endptr)
    {
      if (!isprint (*ptr))
	{
	  if (!isspace (*ptr))
	    {
	      bad_char_to_print (ptr);
	    }
	}
      ptr++;
    }
  *ptr = 0;
  return (rcs_write_string (temp_string,save_string));
}

int
rcs_puts (const char * _str)
{
  int retval, retval2;
  if(!_str)
    {
      return EOF;
    }
  retval = rcs_write_string (_str,0);
  if (retval != EOF)
    {
      retval2 = rcs_write_string ("\n",0);
      if (retval2 != EOF)
	{
	  retval += retval;
	}
      else
	{
	  retval = EOF;
	}
    }
  return (retval);
}

#if 0
//  POSIX_THREADS
static bool process_deffered_stdout_quit=false;

void * process_deferred_stdout(void *_arg)
{
  while(!process_deffered_stdout_quit)
    {
      pthread_mutex_lock(&deferred_stdout_mutex);
      pthread_cond_wait(&deferred_stdout_cond,&deferred_stdout_mutex);
      pthread_mutex_unlock(&deferred_stdout_mutex);
      if (NULL != rcs_print_list)
	{
	  char *string_from_list;
	  string_from_list = (char *) rcs_print_list->get_head ();
	  while (NULL != string_from_list)
	    {
	      if (fputs(string_from_list,stdout) != EOF)
		{
		  rcs_print_list->delete_current_node();
		  break;
		}
	      rcs_print_list->delete_current_node();
	      string_from_list = (char *) rcs_print_list->get_next ();
	    }
	}
    }
  return(0);
};

#endif


void
close_rcs_printing ()
{
  switch (rcs_print_destination)
    {
    case RCS_PRINT_TO_LIST:
      clean_print_list ();
      break;

    case RCS_PRINT_TO_FILE:
      if (NULL != rcs_print_file_stream)
	{
	  fclose (rcs_print_file_stream);
	  rcs_print_file_stream = NULL;
	}
      break;
    default:
      break;
    }
  return;
}

int
set_rcs_print_file (const char * _file_name)
{
  if (_file_name == NULL)
    {
      return -1;
    }
  if (strlen (_file_name) > 80)
    {
      return -1;
    }
  strcpy (rcs_print_file_name, _file_name);
  if (NULL != rcs_print_file_stream)
    {
      fclose (rcs_print_file_stream);
    }
  rcs_print_file_stream = fopen (rcs_print_file_name, "a+");
  if (NULL == rcs_print_file_stream)
    {
      return -1;
    }
  fflush(stdout);
  fflush(stderr);
  fflush(rcs_print_file_stream);
  return 0;
}

static char rcs_print_temp_buffer[1024];

int
rcs_print (const char * _fmt, ...)
{
  int retval;
  va_list args;
  if (strlen (_fmt) > 250)
    {
      return EOF;
    }
  va_start (args, _fmt);


#ifdef HAVE_VSNPRINTF
  if (EOF == (int) vsnprintf (rcs_print_temp_buffer, sizeof (rcs_print_temp_buffer), _fmt, args))
    {
      return (EOF);
    }
#else
#ifdef HAVE__VSNPRINTF
  if (EOF == (int) _vsnprintf (rcs_print_temp_buffer, sizeof (rcs_print_temp_buffer), _fmt, args))
    {
      return (EOF);
    }
#else

  retval = vsprintf (rcs_print_temp_buffer, _fmt, args);
  va_end (args);
  if (retval == (EOF))
    {
      return EOF;
    }
#endif
#endif
  rcs_print_temp_buffer[(sizeof (rcs_print_temp_buffer) - 1)] = 0;
  retval = rcs_write_string (rcs_print_temp_buffer,0);
  return (retval);
}

#ifndef DO_NOT_USE_RCS_PRINT_ERROR_NEW

static const char *rcs_error_filename = NULL;
static int rcs_error_linenum = -1;
int
set_print_rcs_error_info (const char *file, int line)
{
  rcs_error_filename = file;
  rcs_error_linenum = line;
  return (0);
}


static bool rcs_print_error_flags_initialized=false;

static void
init_rcs_print_error_flags()
{
  if(!rcs_print_error_flags_initialized)
    {
      rcs_print_error_flags_initialized=true;
#if HAVE_GETENV
      const char *env =0;
      env = getenv("RCS_PRINT_DEST");
      if(env)
	{
	  if(!strcmp(env,"STDOUT")  || !strcmp(env,"stdout"))
	    {
	      set_rcs_print_destination(RCS_PRINT_TO_STDOUT);
	    }
	  else if(!strcmp(env,"STDERR") || !strcmp(env,"stderr"))
	    {
	      set_rcs_print_destination(RCS_PRINT_TO_STDERR);
	    }
	  else if(strlen(env) < 200)
	    {
	      set_rcs_print_destination(RCS_PRINT_TO_FILE);
	      set_rcs_print_file(strdup(env));
	    }
	}
      env = getenv("PAUSE_ON_RCS_ERROR");
      if(env)
	{
	  set_rcs_print_do_flush(1);
	  pause_on_rcs_error=true;
	}
      env = getenv("ABORT_ON_RCS_ERROR");
      if(env)
	{
	  set_rcs_print_do_flush(1);
	  abort_on_rcs_error=true;
	}
      env = getenv("DO_NOT_PRINT_TIMEOUT_ERRORS");
      if(env)
	{
	  setRcsDoNotPrintTimeoutErrors(1);
	}
#endif
    }
}

int
print_rcs_error_new (const char * _fmt, ...)
{
  int retval = 0;
  va_list args;
  va_start (args, _fmt);
  retval = vprint_rcs_error_new(_fmt,args);
  va_end(args);
  return retval;
}

int
vprint_rcs_error_new(const char *_fmt, va_list args)
{
  int retval=0;
#if defined(HAVE_PID_T) && !defined(MS_WINDOWS_API) && !defined(VXWORKS)
  pid_t pid=0;
#else
  long pid = 0;
#endif
#ifdef POSIX_THREADS
  pthread_t pthread_id=0;
#endif
#ifdef MS_WINDOWS_API
  pid = GetCurrentProcessId ();
#else
#ifndef VXWORKS
  pid = getpid ();
#else
  pid = (long) taskIdSelf ();
#endif
#endif
#ifdef POSIX_THREADS
  pthread_id=pthread_self();
#endif
  int orig_do_flush;
  
  if(!rcs_print_error_flags_initialized)
    {
      init_rcs_print_error_flags();
    }


  if ((rcs_print_mode_flags & PRINT_RCS_ERRORS)
      && ((max_rcs_errors_to_print >= rcs_errors_printed)
	  || max_rcs_errors_to_print < 0))
    {
      orig_do_flush = rcs_print_do_flush;
      if (NULL != rcs_error_filename && rcs_error_linenum > 0)
	{
	  if(!print_start_time_set)
	    {
	      print_start_time_set=true;
	      print_start_time = etime();
	      print_start_time = ((long) print_start_time/3600)*3600.0;
	    }
	  rcs_print_do_flush=1;
#ifdef POSIX_THREADS
	  rcs_print ("(time=%4.4f,pid=%ld,thread=%ld): ", (etime() - print_start_time), (long)pid,(long) pthread_id);
#else
	  rcs_print ("(time=%4.4f,pid=%ld): ", (etime() - print_start_time), (long)pid);
#endif
	  rcs_print ("%s %d: !ERROR! ", rcs_error_filename,
		     rcs_error_linenum);
	  if(rcs_print_tag)
	    {
	      rcs_print(" <%s> ",rcs_print_tag);
	      rcs_print_tag=0;
	    }
	  rcs_error_filename = NULL;
	  rcs_error_linenum = -1;
	}
      retval = rcs_vprint (_fmt, args, 1);
      if (max_rcs_errors_to_print == rcs_errors_printed &&
	  max_rcs_errors_to_print >= 0)
	{
	  rcs_print ("\nMaximum number of errors to print exceeded!\n");
	}
      rcs_print_do_flush = orig_do_flush;
    }
  if (rcs_print_destination != RCS_PRINT_TO_NULL)
    {
      rcs_errors_printed++;
    }
  if(pause_on_rcs_error)
    {
      char dumbbuf[256];
      rcs_print("\nPausing due to error. Press <ENTER> to continue.\n");
      fgets(dumbbuf,sizeof(dumbbuf),stdin);
    }
#ifdef HAVE_ABORT
  if(abort_on_rcs_error)
    {
      abort();
    }
#endif
  return (retval);
}

int
print_rcs_warning_new (const char * _fmt, ...)
{
  int retval = 0;
#if defined(HAVE_PID_T) && !defined(MS_WINDOWS_API) && !defined(VXWORKS)
  pid_t pid=0;
#else
  long pid = 0;
#endif
#ifdef POSIX_THREADS
  pthread_t pthread_id=0;
#endif
#ifdef MS_WINDOWS_API
  pid = GetCurrentProcessId ();
#else
#ifndef VXWORKS
  pid = getpid ();
#else
  pid = (long) taskIdSelf ();
#endif
#endif 
#ifdef POSIX_THREADS
  pthread_id=pthread_self();
#endif

  va_list args;
  va_start (args, _fmt);
  if ((rcs_print_mode_flags & PRINT_RCS_ERRORS)
      && ((max_rcs_errors_to_print >= rcs_errors_printed)
	  || max_rcs_errors_to_print < 0))
    {
      if (NULL != rcs_error_filename && rcs_error_linenum > 0)
	{
	  if(!print_start_time_set)
	    {
	      print_start_time_set=true;
	      print_start_time = etime();
	      print_start_time = ((long) print_start_time/3600)*3600.0;
	    }
#ifdef POSIX_THREADS
	  rcs_print ("(time=%4.4f,pid=%ld,thread=%ld): ", (etime() - print_start_time), (long)pid,(long) pthread_id);
#else
	  rcs_print ("(time=%4.4f,pid=%ld): ", (etime() - print_start_time), (long)pid);
#endif

	  rcs_print ("%s %d: !WARNING! ", rcs_error_filename,
		     rcs_error_linenum);
	  if(rcs_print_tag)
	    {
	      rcs_print(" <%s> ",rcs_print_tag);
	      rcs_print_tag=0;
	    }
	  rcs_error_filename = NULL;
	  rcs_error_linenum = -1;
	}
      retval = rcs_vprint (_fmt, args, 0);
      if (max_rcs_errors_to_print == rcs_errors_printed &&
	  max_rcs_errors_to_print >= 0)
	{
	  rcs_print ("\nMaximum number of errors to print exceeded!\n");
	}
    }
  if (rcs_print_destination != RCS_PRINT_TO_NULL)
    {
      rcs_errors_printed++;
    }
  va_end (args);
  return (retval);
}

#endif

#ifdef rcs_print_debug
#undef rcs_print_debug
#endif

#ifndef DISABLE_RCS_DEBUG_PRINT

#ifndef DO_NOT_USE_RCS_PRINT_DEBUG_NEW


int rcs_print_debug (long flag_to_check, const char * _fmt, ...);


static const char *rcs_debug_filename = NULL;
static int rcs_debug_linenum = -1;
int
set_print_rcs_debug_info (const char *file, int line)
{
  rcs_debug_filename = file;
  rcs_debug_linenum = line;
  return (0);
}

static void init_rcs_print_error_flags();

int
init_rcs_print_mode_flags(void)
{
  if(!rcs_print_mode_flags_set)
    {
      rcs_debugging_enabled=false;
      rcs_print_mode_flags_set=true;
#if HAVE_GETENV
      const char *env =0;
      init_rcs_print_error_flags();
      env = getenv("RCS_PRINT_DEST");
      if(env)
	{
	  if(!strcmp(env,"STDOUT")  || !strcmp(env,"stdout"))
	    {
	      set_rcs_print_destination(RCS_PRINT_TO_STDOUT);
	    }
	  else if(!strcmp(env,"STDERR") || !strcmp(env,"stderr"))
	    {
	      set_rcs_print_destination(RCS_PRINT_TO_STDERR);
	    }
	  else if(strlen(env) < 200)
	    {
	      set_rcs_print_destination(RCS_PRINT_TO_FILE);
	      set_rcs_print_file(strdup(env));
	    }
	}
      env = getenv("DEBUG_RCSLIB");
      if(env)
	{
	  max_rcs_errors_to_print=-1;
	  set_rcs_print_flag(0xFFFFFFFF);
	  set_rcs_print_do_flush(1);
	}
      env = getenv("RCS_PRINT_DO_FLUSH");
      if(env)
	{
	  set_rcs_print_do_flush(1);
	}	  
      env = getenv("RCS_PRINT_FLAGS");
      if(env)
	{
	  unsigned long envflags = strtoul(env,0,0);
	  clear_rcs_print_flag(0xFFFFFFFF);
	  set_rcs_print_flag(envflags);
	}
#endif
    }
  return 0;
}


int
print_rcs_debug_new (long flag_to_check, const char * _fmt, ...)
{
  int retval = 0;
  va_list args;

  if(!rcs_print_mode_flags_set)
    {
      init_rcs_print_mode_flags();

    }

  va_start (args, _fmt);
  if (rcs_print_mode_flags & flag_to_check)
    {
      if (NULL != rcs_debug_filename && rcs_debug_linenum > 0)
	{
	  rcs_print_debug (flag_to_check, "%s %d: !DEBUG! ",
			   rcs_debug_filename, rcs_debug_linenum);
	  rcs_debug_filename = NULL;
	  rcs_debug_linenum = -1;
	}
      retval = rcs_vprint (_fmt, args, 0);
    }
  va_end (args);
  return (retval);
}

#endif


int
rcs_print_debug (long flag_to_check, const char * _fmt, ...)
{
  int retval = 0;
#if defined(HAVE_PID_T) && !defined(MS_WINDOWS_API) && !defined(VXWORKS)
  pid_t pid=0;
#else
  long pid = 0;
#endif
#ifdef POSIX_THREADS
  pthread_t pthread_id=0;
#endif
  va_list args;
  va_start (args, _fmt);

  if (flag_to_check & rcs_print_mode_flags)
    {
#ifdef MS_WINDOWS_API
      pid = GetCurrentProcessId ();
#else
#ifndef VXWORKS
      pid = getpid ();
#else
      pid = (long) taskIdSelf ();
#endif
#endif 
#ifdef POSIX_THREADS
      pthread_id=pthread_self();
#endif
      if(!print_start_time_set)
	{
	  print_start_time_set=true;
	  print_start_time = etime();
	  print_start_time = ((long) print_start_time/3600)*3600.0;
	}
#ifdef POSIX_THREADS
      rcs_print ("(time=%4.4f,pid=%ld,thread=%ld): ", (etime() - print_start_time), (long)pid,(long) pthread_id);
#else
      rcs_print ("(time=%4.4f,pid=%ld): ", (etime() - print_start_time), (long)pid);
#endif
      retval = rcs_vprint (_fmt, args, 0);
    }
  va_end (args);
  return (retval);
}

#endif
// #ifndef DISABLE_RCS_DEBUG_PRINT

void
set_rcs_print_flag (unsigned long flag_to_set)
{
  rcs_print_mode_flags_set =true;
  rcs_print_mode_flags |= flag_to_set;
  unsigned long debug_masked_flag_to_set = flag_to_set & ~(1);
  if (0 != debug_masked_flag_to_set)
    {
#ifndef DISABLE_RCS_DEBUG_PRINT
      rcs_debugging_enabled =1;
#endif
      set_rcs_print_do_flush(1);
    }
}

void
clear_rcs_print_flag (unsigned long flag_to_clear)
{
  rcs_print_mode_flags_set =true;
  rcs_print_mode_flags &= ~(flag_to_clear);
  unsigned long debug_masked_flag = rcs_print_mode_flags & ~(1);
  if (0 == debug_masked_flag)
    {
#ifndef DISABLE_RCS_DEBUG_PRINT
      rcs_debugging_enabled = 0;
#endif
    }
}

int
rcs_print_sys_error (int error_source,const char * _fmt, ...)
{
  static char temp_string[256];
  static char message_string[512];
  va_list args;
  va_start (args, _fmt);


  if (NULL == _fmt)
    {
      return (EOF);
    }
  if (strlen (_fmt) > 200)	/* Might overflow temp_string. */
    {
      return (EOF);
    }
  if (EOF == (int) vsprintf (temp_string, _fmt, args))
    {
      return (EOF);
    }
  va_end (args);

#if defined(HAVE_PID_T) && !defined(MS_WINDOWS_API) && !defined(VXWORKS)
  pid_t pid=0;
#else
  long pid = 0;
#endif
#ifdef POSIX_THREADS
  pthread_t pthread_id=0;
#endif

#ifdef MS_WINDOWS_API
  pid = GetCurrentProcessId ();
#else
#ifndef VXWORKS
  pid = getpid ();
#else
  pid = (long) taskIdSelf ();
#endif
#endif 

#ifdef POSIX_THREADS
  pthread_id=pthread_self();
#endif

  if (max_rcs_errors_to_print == rcs_errors_printed &&
      max_rcs_errors_to_print >= 0)
    {
      rcs_print ("\nMaximum number of errors to print exceeded!\n");
    }
  rcs_errors_printed++;
  if (max_rcs_errors_to_print <= rcs_errors_printed &&
      max_rcs_errors_to_print >= 0)
    {
      return (EOF);
    }
  if(!print_start_time_set)
    {
      print_start_time_set=true;
      print_start_time = etime();
      print_start_time = ((long) print_start_time/3600)*3600.0;
    }
#ifdef POSIX_THREADS
  rcs_print ("(time=%4.4f,pid=%ld,thread=%ld): ", (etime() - print_start_time), (long)pid,(long) pthread_id);
#else
  rcs_print ("(time=%4.4f,pid=%ld): ", (etime() - print_start_time), (long)pid);
#endif
  switch (error_source)
    {
    case ERRNO_ERROR_SOURCE:
      SNPRINTF_FUNC ( SNPRINTF_ARGS(message_string,sizeof(message_string)),
		      "%s %d %s\n", temp_string, errno,
		      strerror (errno));
      rcs_puts (message_string);
      break;

#ifdef MS_WINDOWS_API
    case GETLASTERROR_ERROR_SOURCE:
      {
	char *msgBuf = NULL;
	DWORD errCode = GetLastError ();
	FormatMessage (FORMAT_MESSAGE_ALLOCATE_BUFFER |
		       FORMAT_MESSAGE_FROM_SYSTEM, NULL, errCode,
		       MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
		       (LPTSTR) & msgBuf, 0, NULL);
	if (NULL != msgBuf)
	  {
	    SNPRINTF_FUNC ( SNPRINTF_ARGS(message_string,sizeof(message_string)),
			    
			    "%s %ld %s\n", temp_string, errCode, msgBuf);
	  }
	else
	  {
	    SNPRINTF_FUNC ( SNPRINTF_ARGS(message_string,sizeof(message_string)),
			    "%s %ld\n", temp_string, errCode);
	  }
      }
      rcs_puts (message_string);
      break;

#ifdef HAVE_WSAGETLASTERROR
    case WSAGETLASTERROR_ERROR_SOURCE:
      {
	char *msgBuf = NULL;
	DWORD errCode = WSAGetLastError ();
	FormatMessage (FORMAT_MESSAGE_ALLOCATE_BUFFER |
		       FORMAT_MESSAGE_FROM_SYSTEM, NULL, errCode,
		       MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
		       (LPTSTR) & msgBuf, 0, NULL);
	if (NULL != msgBuf)
	  {
	    SNPRINTF_FUNC ( SNPRINTF_ARGS(message_string,sizeof(message_string)),
			    "%s %d %s\n", temp_string, errCode,
			    msgBuf);
	  }
	else
	  {
	    SNPRINTF_FUNC ( SNPRINTF_ARGS(message_string,sizeof(message_string)),
			    "%s %d\n", temp_string, errCode);
	  }
      }
      rcs_puts (message_string);
      break;
#endif

#endif

    default:
      rcs_puts (temp_string);
      break;
    }
  if(pause_on_rcs_error)
    {
      char dumbbuf[256];
      rcs_print("\nPausing due to error. Press <ENTER> to continue.\n");
      fgets(dumbbuf,sizeof(dumbbuf),stdin);
    }
#ifdef HAVE_ABORT
  if(abort_on_rcs_error)
    {
      abort();
    }
#endif
  return ((int) strlen (temp_string));
}


#ifdef rcs_print_error
#undef rcs_print_error
#endif
extern "C"
{
  int rcs_print_error (char * _fmt, ...);
}

int
rcs_print_error (char * _fmt, ...)
{
  int retval = 0;
  va_list args;
#if defined(HAVE_PID_T) && !defined(MS_WINDOWS_API) && !defined(VXWORKS)
  pid_t pid=0;
#else
  long pid = 0;
#endif
#ifdef POSIX_THREADS
  pthread_t pthread_id=0;
#endif
#ifdef MS_WINDOWS_API
  pid = GetCurrentProcessId ();
#else
#ifndef VXWORKS
  pid = getpid ();
#else
  pid = (long) taskIdSelf ();
#endif
#endif 

  va_start (args, _fmt);
  if ((rcs_print_mode_flags & PRINT_RCS_ERRORS)
      && ((max_rcs_errors_to_print >= rcs_errors_printed)
	  || max_rcs_errors_to_print < 0))
    {
      if(!print_start_time_set)
	{
	  print_start_time_set=true;
	  print_start_time = etime();
	  print_start_time = ((long) print_start_time/3600)*3600.0;
	}
#ifdef POSIX_THREADS
      rcs_print ("(time=%4.4f,pid=%ld,thread=%ld): ", (etime() - print_start_time), (long)pid,(long) pthread_id);
#else
      rcs_print ("(time=%4.4f,pid=%ld): ", (etime() - print_start_time), (long)pid);
#endif
      retval = rcs_vprint (_fmt, args, 1);
      if (max_rcs_errors_to_print == rcs_errors_printed &&
	  max_rcs_errors_to_print >= 0)
	{
	  rcs_print ("\nMaximum number of errors to print exceeded!\n");
	}
    }
  if (rcs_print_destination != RCS_PRINT_TO_NULL)
    {
      rcs_errors_printed++;
    }
  va_end (args);
  if(pause_on_rcs_error)
    {
      char dumbbuf[256];
      rcs_print("\nPausing due to error. Press <ENTER> to continue.\n");
      fgets(dumbbuf,sizeof(dumbbuf),stdin);
    }
#ifdef HAVE_ABORT
  if(abort_on_rcs_error)
    {
      abort();
    }
#endif
  return (retval);
}

int rcs_fputs(const char *_str)
{
  if(_str)
    {
      return rcs_write_string(_str,0);
    }
  else
    {
      return EOF;
    }
}
