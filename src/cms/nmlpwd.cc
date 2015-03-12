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


/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1


#include "crypt2.hh"

#if HAVE_CONFIG_H

#include "rcs_cms.h"

#else

#include <stdio.h>		/* printf(), fgets(), fputs() */
#include <string.h>		/* strncpy(), strpbrk(), strcat() */
#include <stdlib.h>		/* srand(), rand(), getpass() */
#include <errno.h>		// errno
#include <ctype.h>		/* isgraph() */

#ifdef linux
#include <pwd.h>		// getpass()
#endif


#endif

#ifdef VXWORKS
#ifdef __cplusplus
extern "C" int nmlpwd ();
#endif

int
nmlpwd ()
{
  int argc = 1;
  char **argv = NULL;
#else

int
main (int argc, char **argv)
{
#endif
  char old_passwd_file[256];
  char new_passwd_file[256];
  char user_passwd[16];
  char user_logname[16];
  char temp_buffer[256];
  char *crypt_ret;
  char encrypted_passwd[16];
  int allow_read = 0;
  int allow_write = 0;
  int seed;
  int i;
  char new_line[256];
  char salt[3];
  int user_logname_length;
  int user_found = 0;
  char *end_line = NULL;
  char *getpass_ret = NULL;

  FILE *fp_old = NULL;
  FILE *fp_new = NULL;

  if (argc > 1)
    {
      strncpy (old_passwd_file, argv[1], 256);
    }
  else
    {
      printf ("Old Password File:(type none to create a file from scratch)");
      fgets (old_passwd_file, 256, stdin);
    }
  end_line = strpbrk (old_passwd_file, "\r\n");
  if (end_line != NULL)
    {
      *end_line = 0;
    }
  if (old_passwd_file[0] != 0)
    {
      if (strcmp (old_passwd_file, "none"))
	{
	  fp_old = fopen (old_passwd_file, "r");
	  if (fp_old == NULL)
	    {
	      fprintf (stderr, "Can not open %s. errno = %d -- %s\n",
		       old_passwd_file, errno, strerror (errno));
	      exit (-1);
	    }
	}
    }

  if (argc > 1)
    {
      strncpy (new_passwd_file, argv[1], 256);
    }
  else
    {
      printf ("New Password File:");
      fgets (new_passwd_file, 256, stdin);
    }
  end_line = strpbrk (new_passwd_file, "\r\n");
  if (end_line != NULL)
    {
      *end_line = 0;
    }

  if (!strcmp (new_passwd_file, old_passwd_file))
    {
      fprintf (stderr,
	       "New passwd file must be different from old passwd file.\n");
      exit (-1);
    }

  fp_new = fopen (new_passwd_file, "w");
  if (NULL == fp_new)
    {
      fprintf (stderr, "Can not open/create %s. errno = %d -- %s\n",
	       new_passwd_file, errno, strerror (errno));
      exit (-1);
    }
  if (argc > 3)
    {
      strncpy (user_logname, argv[3], 16);
    }
  else
    {
      printf ("Login name:");
      fgets (user_logname, 16, stdin);
    }
  end_line = strpbrk (user_logname, "\r\n");
  if (end_line != NULL)
    {
      *end_line = 0;
    }
  user_logname_length = strlen (user_logname);
  if (user_logname_length < 2)
    {
      fprintf (stderr, "Login name %s is too short.\n", user_logname);
      exit (-1);
    }

  if (argc > 4)
    {
      strncpy (user_passwd, argv[4], 16);
    }
  else
    {
      while (1)
	{
	  getpass_ret = getpass ("Password:");
	  if (NULL == getpass_ret)
	    {
	      fprintf (stderr, "getpass() failed. errno = %d -- %s\n",
		       errno, strerror (errno));
	      exit (-1);
	    }
	  strncpy (user_passwd, getpass_ret, 16);
	  getpass_ret = getpass ("Retype Password:");
	  if (!strcmp (getpass_ret, user_passwd))
	    {
	      break;
	    }
	  printf ("Passwd did not match.\n");
	}
    }
  end_line = strpbrk (user_passwd, "\r\n");
  if (end_line != NULL)
    {
      *end_line = 0;
    }

  printf ("Should %s be allowed to read?(y/n)", user_logname);
  fgets (temp_buffer, 256, stdin);
  allow_read = (temp_buffer[0] == 'y' || temp_buffer[0] == 'Y');


  printf ("Should %s be allowed to write?(y/n)", user_logname);
  fgets (temp_buffer, 256, stdin);
  allow_write = (temp_buffer[0] == 'y' || temp_buffer[0] == 'Y');

  if (strlen (user_passwd) > 1)
    {
      seed = 1000;

      for (i = 0; i < 256 && new_passwd_file[i]; i++)
	{
	  seed += new_passwd_file[i];
	}

      for (i = 0; i < 16 && user_logname[i]; i++)
	{
	  seed += user_logname[i];
	}

      for (i = 0; i < 16 && user_passwd[i]; i++)
	{
	  seed += user_passwd[i];
	}
      srand (seed);
      salt[0] = rand () % 128;
      while (!isgraph (salt[0])
	     && salt[0] != ' '
	     && salt[0] != '\t'
	     && salt[0] != '\r'
	     && salt[0] != '\n' && salt[0] != ':' && salt[0] != 0)
	{
	  salt[0] = rand () % 128;
	}
      salt[1] = rand () % 128;
      while (!isgraph (salt[1])
	     && salt[1] != ' '
	     && salt[1] != '\t'
	     && salt[1] != '\r'
	     && salt[1] != '\n' && salt[1] != ':' && salt[1] != 1)
	{
	  salt[1] = rand () % 128;
	}
      salt[2] = 0;

      crypt_ret = rcs_crypt (user_passwd, salt);
      if (NULL == crypt_ret)
	{
	  fprintf (stderr, "crypt failed.\n");
	  exit (-1);
	}
      strncpy (encrypted_passwd, crypt_ret, 16);

      sprintf (new_line, "%s:%s:", user_logname, encrypted_passwd);
    }
  else
    {
      sprintf (new_line, "%s::", user_logname);
    }
  if (allow_read)
    {
      strcat (new_line, " read=true,");
    }
  else
    {
      strcat (new_line, " read=false,");
    }
  if (allow_write)
    {
      strcat (new_line, " write=true");
    }
  else
    {
      strcat (new_line, " write=false");
    }

  printf ("New line will be:\n");
  puts (new_line);

  if (NULL != fp_old)
    {
      while (!feof (fp_old))
	{
	  memset (temp_buffer, 0, 256);
	  fgets (temp_buffer, 256, fp_old);
	  if (!temp_buffer[0])
	    {
	      continue;
	    }
	  if (!strncmp (temp_buffer, user_logname, user_logname_length))
	    {
	      if (!user_found)
		{
		  fputs (new_line, fp_new);
		  fputs ("\n", fp_new);
		}
	      user_found = 1;
	    }
	  else
	    {
	      fputs (temp_buffer, fp_new);
	    }
	}
    }
  if (!user_found)
    {
      fputs (new_line, fp_new);
      fputs ("\n", fp_new);
    }
}
