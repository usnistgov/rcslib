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

/************************************************************************
File: FILEMEM.cc
Purpose: Provides a derived class of CMS that reads and writes
messages to a plain text file. The file size can be limited from
the NML configuration file.
**********************************************************************/

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_FILEMEM)


#if HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "filemem_no_config.h"
#endif

#include "filemem.hh"		// class FILEMEM
#include "cms.hh"		// class CMS
#include "rcs_prnt.hh"		// rcs_print_error()
#include "timer.hh"		// etime()

/* rw-rw-r-- permissions */
#ifndef MS_WINDOWS_API
#ifdef S_IRUSR
#define MODE (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#else
#define MODE (0777)
#endif
#else
#define MODE (0)
#endif

class FILEMEM_NONPORT_INTERNALS
{
  friend class FILEMEM;

  fpos_t last_read_pos;
  fpos_t last_write_pos;
#ifdef FILEMEM_USE_SEMAPHORES
  key_t key;			/* key for sem */
  RCS_SEMAPHORE *sem;		/* semaphore */
#endif

#ifdef USE_FCNTL_FILE_LOCKING
  struct flock read_lock;
  struct flock write_lock;
  int input_fd;
  int output_fd;
#endif

};

FILEMEM::FILEMEM (const char *bufline, const char *procline, int set_to_server,
		  int set_to_master):
  CMS (bufline, procline, set_to_server),
  input_file_pos(0),
  max_output_messages(0),
  in(0),
  out(0),
  input_is_stdin(0),
  output_is_stdout(0),
  last_read_time(0),
  read_time(0),
  last_write_time(0),
  write_time(0),
  wait_period(0),
  wait_start(0),
  write_cycle(0),
  write_count(0),
  read_cycle(0),
  add_waits(0),
  internals(0),
  write_file_length(0)
{
  char *infile_name_string = (char *) NULL;
  char *outfile_name_string = (char *) NULL;
  char *end_name = (char *) NULL;

  internals = new FILEMEM_NONPORT_INTERNALS();

  int master;
  master = is_local_master;
  if (1 == set_to_master)
    {
      master = 1;
    }
  else if (-1 == set_to_master)
    {
      master = 0;
    }

  in = (FILE *) NULL;
  out = (FILE *) NULL;
  write_time = 0;
  last_write_time = 0;
  read_time = 0;
  last_read_time = 0;
  wait_period = 0;
  wait_start = 0;
  write_count = 0;
  write_cycle = 0;
  read_cycle = 0;
  add_waits = 0;
  write_file_length = 0;
  input_is_stdin = 0;
  output_is_stdout = 0;

  if (CMS_DISPLAY_ASCII_ENCODING != neutral_encoding_method || !neutral)
    {
      rcs_print_error
	("The neutral_encoding_method must be ASCII_DISPLAY_ENCODING and the buffer must be neutral format to use FILEMEM.\n");
      status = CMS_CONFIG_ERROR;
    }

#ifdef FILEMEM_USE_SEMAPHORES
  /* Save parameters from configuration file. */
  if (sscanf (bufline, "%*s %*s %*s %*s %*d %*s %*s %*d %d", &internals->key) != 1)
    {
      rcs_print_error ("FILEMEM: Invalid configuration file format.\n");
      return;
    }
#endif

  if (NULL != strstr (procline, "add_waits")
      || NULL != strstr (bufline, "add_waits"))
    {
      add_waits = 1;
    }


  outfile_name_string = strstr (procline, "out=");
  if (NULL == outfile_name_string)
    {
      outfile_name_string = strstr (bufline, "out=");
    }
  if (NULL != outfile_name_string)
    {
      strcpy (outfile_name, outfile_name_string + 4);
      end_name = strpbrk (outfile_name, " \t\n\r,;=");
      if (NULL != end_name)
	{
	  *end_name = 0;
	}
      if (!strcmp (outfile_name, "stdout"))
	{
	  output_is_stdout = 1;
	  out = stdout;
	}
      else
	{
	  out = fopen (outfile_name, "w+");
	  if (out == NULL || ((long) out) == -1)
	    {
	      rcs_print_error ("FILEMEM: Can not open %s. (error = %d,%s)\n",
			       outfile_name, errno, strerror (errno));
	      status = CMS_CREATE_ERROR;
	      return;
	    }
	}
    }
  else
    {
      strcpy (outfile_name, "stdout");
      output_is_stdout = 1;
      out = stdout;
    }

  infile_name_string = strstr (procline, "in=");
  if (NULL == infile_name_string)
    {
      infile_name_string = strstr (bufline, "in=");
    }
  if (NULL != infile_name_string)
    {
      strcpy (infile_name, infile_name_string + 3);

      end_name = strpbrk (infile_name, " \t\n\r,;=");
      if (NULL != end_name)
	{
	  *end_name = 0;
	}
      if (!strcmp (infile_name, "stdin"))
	{
	  in = stdin;
	}
      else
	{
	  in = fopen (infile_name, "r");
	  if (in == NULL || ((long) in) == -1)
	    {
	      rcs_print_error ("FILEMEM: Can not open %s. (error = %d,%s)\n",
			       infile_name, errno, strerror (errno));
	      status = CMS_CREATE_ERROR;
	      return;
	    }
	}
    }
  else
    {
      strcpy (infile_name, "stdin");
      in = stdin;
    }

  max_output_messages = -1;
  char *ptr;
  ptr = strstr (procline, "max_out=");
  if (NULL == ptr)
    {
      ptr = strstr (bufline, "max_out=");
    }
  if (NULL != ptr)
    {
      max_output_messages = strtol (ptr + 8, (char **) NULL, 0);
    }

#ifdef FILEMEM_USE_SEMAPHORES
  if (is_local_master)
    {
      internals->sem = new RCS_SEMAPHORE (internals->key, RCS_SEMAPHORE_CREATE, -1, (int) MODE, 1);
      if (NULL == internals->sem)
	{
	  rcs_print_error ("CMS: couldn't create RCS_SEMAPHORE.\n");
	  rcs_print_error (" Possibly out of memory?\n");
	  status = CMS_CREATE_ERROR;
	  return;
	}
      if (!internals->sem->valid ())
	{
	  rcs_print_error ("CMS: RCS_SEMAPHORE is invalid.\n");
	  status = CMS_MISC_ERROR;
	  return;
	}
    }
  else
    {
      internals->sem = new RCS_SEMAPHORE (internals->key, RCS_SEMAPHORE_NOCREATE, -1);
      if (NULL == internals->sem)
	{
	  rcs_print_error ("CMS: couldn't create RCS_SEMAPHORE.\n");
	  rcs_print_error (" Possibly out of memory?\n");
	  status = CMS_CREATE_ERROR;
	  return;
	}
      if (!internals->sem->valid ())
	{
	  rcs_print_error ("CMS: RCS_SEMAPHORE is invalid.\n");
	  status = CMS_MISC_ERROR;
	  return;
	}
    }
#endif



  fgetpos (in, &internals->last_read_pos);
  fgetpos (out, &internals->last_write_pos);

}


FILEMEM::~FILEMEM ()
{
  if (NULL != in && strcmp (infile_name, "stdin"))
    {
      fclose (in);
      in = (FILE *) NULL;
    }
  if (NULL != out && strcmp (outfile_name, "stdout"))
    {
      fclose (out);
      out = (FILE *) NULL;
    }
#ifdef FILEMEM_USE_SEMAPHORES
  if (NULL != internals->sem)
    {
      if (is_local_master || delete_totally)
	{
	  internals->sem->setflag (RCS_SEMAPHORE_CREATE);
	}
      else
	{
	  internals->sem->setflag (RCS_SEMAPHORE_NOCREATE);
	}
      delete internals->sem;
      internals->sem = (RCS_SEMAPHORE *) NULL;
    }
#endif
  if(internals)
    {
      delete internals;
      internals=0;
    }
}

CMS_STATUS FILEMEM::clear ()
{
  return status;
}

int
FILEMEM::check_if_read ()
{
  return 1;
}

CMS_STATUS FILEMEM::read ()
{
  char *
    ptr;
  read_time = etime ();
  if (read_time - wait_start < wait_period && last_read_time > 0
      && wait_start > 0)
    {
      return (status = CMS_READ_OLD);
    }
  wait_period = 0;
  if (NULL == in)
    {
      reopen_input ();
    }

  if (NULL == in)
    {
      rcs_print_error ("FILE *in=NULL\n");
      return (status = CMS_MISC_ERROR);
    }
  if (lock_input () < 0)
    {
      return (status = CMS_MISC_ERROR);
    }

  while (!feof (in))
    {
      memset (input_buffer, 0, FILEMEM_INPUT_BUFFER_SIZE);
      fgetpos (in, &internals->last_read_pos);
      fgets (input_buffer, FILEMEM_INPUT_BUFFER_SIZE, in);
      if (!strncmp (input_buffer, "MSG", 3))
	{
	  ptr = strstr (input_buffer, ">");
	  if (NULL == ptr)
	    {
	      rcs_print_error ("FILEMEM: Badly formatted message string.\n");
	      unlock_input ();
	      return (status = CMS_MISC_ERROR);
	    }
	  if (0 == *ptr)
	    {
	      rcs_print_error ("FILEMEM: Badly formatted message string.\n");
	      unlock_input ();
	      return (status = CMS_MISC_ERROR);
	    }
	  strcpy ((char *) encoded_data, ptr + 1);
	  last_read_time = read_time;
	  unlock_input ();
	  close_input ();
	  return (status = CMS_READ_OK);
	}
      if (!strncmp (input_buffer, "WAIT", 4))
	{
	  ptr = strstr (input_buffer, ">");
	  if (NULL == ptr)
	    {
	      rcs_print_error ("FILEMEM: Badly formatted message string.\n");
	      unlock_input ();
	      return (status = CMS_MISC_ERROR);
	    }
	  if (0 == *ptr)
	    {
	      rcs_print_error ("FILEMEM: Badly formatted message string.\n");
	      return (status = CMS_MISC_ERROR);
	    }
	  errno = 0;
	  wait_period = strtod (ptr + 1, (char **) NULL);
	  if (errno != 0)
	    {
	      rcs_print_error ("FILEMEM: Badly formatted message string.\n");
	      unlock_input ();
	      return (status = CMS_MISC_ERROR);
	    }
	  wait_start = last_read_time;
	  if (read_time - wait_start < wait_period && last_read_time > 0
	      && wait_start > 0)
	    {
	      unlock_input ();
	      close_input ();
	      return (status = CMS_READ_OLD);
	    }
	  last_read_time = read_time;
	}
      if (!strncmp (input_buffer, "REWIND", 5))
	{
	  fseek (in, 0, SEEK_SET);
	}
      if (!strncmp (input_buffer, "END", 3))
	{
	  fsetpos (in, &internals->last_read_pos);
	  unlock_input ();
	  close_input ();
	  return (status = CMS_READ_OLD);
	}
    }
  unlock_input ();
  close_input ();
  last_read_time = read_time;
  return (status = CMS_READ_OLD);
}

int
FILEMEM::lock_input ()
{
  if (input_is_stdin)
    {
      return 0;
    }
#ifdef FILEMEM_USE_SEMAPHORES
  if (NULL == internals->sem)
    {
      return -1;
    }
  return internals->sem->wait ();
#endif


#ifdef USE_FCNTL_FILE_LOCKING
  internals->input_fd = fileno (in);
  internals->read_lock.l_type = F_RDLCK;
  internals->read_lock.l_start = 0;
  internals->read_lock.l_whence = SEEK_SET;
  internals->read_lock.l_len = 0;
  while (1)
    {
      switch (fcntl (internals->input_fd, F_SETLK, &internals->read_lock))
	{
	case -1:
	  if (errno == EACCES || errno == EAGAIN)
	    {
	      continue;
	    }
	  rcs_print_error
	    ("FILEMEM: Can not lock input file %s. (errno = %d) %s\n",
	     infile_name, errno, strerror (errno));
	  return -1;
	default:
	  return 0;
	}
    }
#endif
  return 0;
}

int
FILEMEM::unlock_input ()
{
  if (input_is_stdin)
    {
      return 0;
    }
#ifdef FILEMEM_USE_SEMAPHORES
  internals->sem->post ();
  return 0;
#endif

#ifdef USE_FCNTL_FILE_LOCKING
  internals->read_lock.l_type = F_UNLCK;
  fcntl (internals->input_fd, F_SETLK, &internals->read_lock);
#endif
  return 0;
}

int
FILEMEM::lock_output ()
{
  if (output_is_stdout)
    {
      return 0;
    }

#ifdef FILEMEM_USE_SEMAPHORES
  if (NULL == internals->sem)
    {
      return -1;
    }
  return internals->sem->wait ();
#endif

#ifdef USE_FCNTL_FILE_LOCKING
  internals->output_fd = fileno (out);
  internals->write_lock.l_type = F_WRLCK;
  internals->write_lock.l_start = 0;
  internals->write_lock.l_whence = SEEK_SET;
  internals->write_lock.l_len = 0;
  while (1)
    {
      switch (fcntl (internals->output_fd, F_SETLK, &internals->write_lock))
	{
	case -1:
	  if (errno == EACCES || errno == EAGAIN)
	    {
	      continue;
	    }
	  rcs_print_error
	    ("FILEMEM: Can not lock input file %s. (errno = %d) %s\n",
	     infile_name, errno, strerror (errno));
	  return -1;
	default:
	  return 0;
	}
    }
#endif
  return 0;
}

int
FILEMEM::unlock_output ()
{
  if (output_is_stdout)
    {
      return 0;
    }
#ifdef FILEMEM_USE_SEMAPHORES
  internals->sem->post ();
  return 0;
#endif

#ifdef USE_FCNTL_FILE_LOCKING
  internals->write_lock.l_type = F_UNLCK;
  fcntl (internals->output_fd, F_SETLK, &internals->write_lock);
#endif
  return 0;
}


CMS_STATUS FILEMEM::peek ()
{
  return read ();
}

void
FILEMEM::close_input ()
{
  if (NULL != in && strcmp (infile_name, "stdin"))
    {
      input_file_pos = ftell (in);
      fseek (in, 0, SEEK_SET);
      fgets (input_buffer, FILEMEM_INPUT_BUFFER_SIZE, in);
      if (!strncmp (input_buffer, "CYCLE", 5))
	{
	  char *ptr;
	  long file_cycle;
	  ptr = strstr (input_buffer, ">");
	  file_cycle = strtol (ptr + 1, (char **) NULL, 0);
	  if (file_cycle != read_cycle)
	    {
	      read_cycle = file_cycle;
	      fgetpos (in, &internals->last_read_pos);
	      return;
	    }
	}
      fclose (in);
      in = (FILE *) NULL;
    }
}

void
FILEMEM::reopen_input ()
{
  if (NULL == in)
    {
      if (!strcmp (infile_name, "stdin"))
	{
	  in = stdin;
	}
      else
	{
	  in = fopen (infile_name, "r");
	  if ((long) in == -1 || in == NULL)
	    {
	      in = (FILE *) NULL;
	      return;
	    }
	  fgets (input_buffer, FILEMEM_INPUT_BUFFER_SIZE, in);
	  if (!strncmp (input_buffer, "CYCLE", 5))
	    {
	      char *ptr;
	      long file_cycle;
	      ptr = strstr (input_buffer, ">");
	      file_cycle = strtol (ptr + 1, (char **) NULL, 0);
	      if (file_cycle > read_cycle)
		{
		  read_cycle = file_cycle;
		  fgetpos (in, &internals->last_read_pos);
		  return;
		}
	    }
	  fseek (in, input_file_pos, SEEK_SET);
	}
    }
}

CMS_STATUS FILEMEM::write (
			   __unused_parameter__ void *user_data)
{
  char
    temp_buffer[2048];
  long
    current_pos;
  long
    dist_to_end;
  long
    output_size;

  if (NULL == out)
    {
      rcs_print_error ("FILE *out=NULL\n");
      return (status = CMS_MISC_ERROR);
    }
  if (lock_output () < 0)
    {
      return (status = CMS_MISC_ERROR);
    }
  write_time = etime ();
  write_count++;
  fsetpos (out, &internals->last_write_pos);
  if (write_count > max_output_messages &&
      max_output_messages > 0 && strcmp (outfile_name, "stdout") != 0)
    {
      write_count = 0;
      write_cycle++;
      fprintf (out, "\nREWIND>%d\n", write_cycle);
      current_pos = ftell (out);
      if (current_pos > write_file_length)
	{
	  write_file_length = current_pos;
	}
      dist_to_end = write_file_length - current_pos;
      if (dist_to_end > 0)
	{
	  output_size = dist_to_end < 2048 ? dist_to_end : 2048;
	  if (output_size <= 0)
	    {
	      rcs_print_error ("FILEMEM: Bad output size %ld.\n", output_size);
	      unlock_output ();
	      return (status = CMS_MISC_ERROR);
	    }
	  size_t
	    unsigned_output_size = (size_t)
	    output_size;
	  memset (temp_buffer, '#', output_size);
	  size_t
	    fwrite_ret =
	    fwrite (temp_buffer, 1, output_size, out);
	  if (fwrite_ret < unsigned_output_size)
	    {
	      rcs_print_error
		("FILEMEM: Can not write to %s. (error = %d,%s)\n",
		 outfile_name, errno, strerror (errno));
	      unlock_output ();
	      return (status = CMS_MISC_ERROR);
	    }
	}
      fseek (out, 0, SEEK_SET);
      if (fprintf
	  (out,
	   "CYCLE>%d\n# Returned to the beginning of the file after %d messages.\n",
	   write_cycle, (int) (write_cycle * max_output_messages)) < 0)
	{
	  rcs_print_error ("FILEMEM: Can not write to %s. (error = %d,%s)\n",
			   outfile_name, errno, strerror (errno));
	  unlock_output ();
	  return (status = CMS_MISC_ERROR);
	}
      fgetpos (out, &internals->last_write_pos);

    }
  if (fprintf (out, "\n# time=%f, count=%d, cycle=%d",
	       write_time, write_count, write_cycle) < 0)
    {
      rcs_print_error ("FILEMEM: Can not write to %s. (error = %d,%s)\n",
		       outfile_name, errno, strerror (errno));
      unlock_output ();
      return (status = CMS_MISC_ERROR);
    }
  if (add_waits)
    {
      if (fprintf (out, "\nWAIT> %f",
		   last_write_time > 0 ? write_time - last_write_time : 0.0) <
	  0)
	{
	  rcs_print_error ("FILEMEM: Can not write to %s. (error = %d,%s)\n",
			   outfile_name, errno, strerror (errno));
	  unlock_output ();
	  return (status = CMS_MISC_ERROR);
	}
    }
  if (fprintf (out, "\nMSG>%s", (char *) encoded_data) < 0)
    {
      rcs_print_error ("FILEMEM: Can not write to %s. (error = %d,%s)\n",
		       outfile_name, errno, strerror (errno));
      unlock_output ();
      return (status = CMS_MISC_ERROR);
    }
  last_write_time = write_time;

  fgetpos (out, &internals->last_write_pos);
  if (fprintf (out, "\nEND>\n#") < 0)
    {
      rcs_print_error ("FILEMEM: Can not write to %s. (error = %d,%s)\n",
		       outfile_name, errno, strerror (errno));
      unlock_output ();
      return (status = CMS_MISC_ERROR);
    }
  current_pos = ftell (out);
  if (current_pos > write_file_length)
    {
      write_file_length = current_pos;
    }
  if (fflush (out))
    {
      rcs_print_error ("FILEMEM: Error Flushing output buffer.\n");
      unlock_output ();
      return (status = CMS_MISC_ERROR);
    }
  unlock_output ();
  return (status = CMS_WRITE_OK);
}

CMS_STATUS FILEMEM::write_if_read (void *user_data)
{
  return write (user_data);
}

//Private copy constructor and = operator to prevent copying.
// only cms_cfg copy functions can be used to copy CMS objects.
FILEMEM::FILEMEM(
		 __unused_parameter__ const FILEMEM &_cms_ref):
  CMS (0),
  input_file_pos(0),
  max_output_messages(0),
  in(0),
  out(0),
  input_is_stdin(0),
  output_is_stdout(0),
  last_read_time(0),
  read_time(0),
  last_write_time(0),
  write_time(0),
  wait_period(0),
  wait_start(0),
  write_cycle(0),
  write_count(0),
  read_cycle(0),
  add_waits(0),
  internals(0),
  write_file_length(0)
{
  rcs_print_error("FILEMEM copy constructor should never be called.\n");
}
  
FILEMEM &
FILEMEM::operator=(
		   __unused_parameter__ const FILEMEM &_cms_ref)
{
  rcs_print_error("FILEMEM::operator= should never be called.\n");
  return(*this);
}


//  defined(ENABLE_RCS_FILEMEM)

#else
#include "rcs_empty_source"
#endif


