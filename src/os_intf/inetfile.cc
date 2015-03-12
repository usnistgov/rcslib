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

/**********************************************************************
* FILE: inetfile.cc
* Purpose: This file provides a common interface to load files over
* the internet using either the libwww directly from the World Wide Web
* or with the simpler Microsoft Internet SDK
***********************************************************************/

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "inetfile_no_config.h"
#endif

#include "rcs_prnt.hh"
#include "inetfile.hh"

HINTERNET hInternetSession = NULL;


class INET_FILE
{
public:
  INET_FILE ();
  ~INET_FILE ();
#ifndef NO_STDIO
  FILE *fp;
#endif
  HINTERNET hURL;
  HANDLE hFile;
  BOOL url_read;
  DWORD bytes_read;
  int end_of_file;
  int last_buffer_read;
  int bytes_in_buffer;
  int bytes_left_in_buffer;
  char buffer[4096];
  char url[4096];
  char *ptr_in_buffer;

private:
    INET_FILE (INET_FILE & ifile)
  {
  };				// Don't copy me.
};

INET_FILE::~INET_FILE ()
{
#ifndef NO_STDIO
  if (NULL != fp)
    {
      fclose (fp);
      fp = NULL;
    }
#endif
}

INET_FILE::INET_FILE ()
{
#ifndef NO_STDIO
  fp = NULL;
#endif
  hURL = NULL;
  hFile = INVALID_HANDLE_VALUE;
  bytes_in_buffer = 0;
  bytes_left_in_buffer = 0;
  end_of_file = 0;
  url_read = FALSE;
  last_buffer_read = 0;
  ptr_in_buffer = buffer;
  bytes_read = 0;
  memset (url, 0, 4096);
  memset (buffer, 0, 4096);
}




int RCS_EXPORT
inet_file_init (const char *agent_name, char *agent_version, int debug)
{
  if (hInternetSession == NULL)
    {
      if (NULL == agent_name)
	{
	  agent_name = "InetFileAgent";
	}
      hInternetSession = InternetOpen (agent_name,	// agent name
				       0,	//INTERNET_OPEN_TYPE_PRECONFIG// access type
				       NULL,	// proxy servers
				       NULL,	// proxy bypass
				       0);	// flags
    }
  if (hInternetSession == NULL)
    {
      rcs_print_error ("Can not open internet session. (Error =%d)\n",
		       GetLastError ());
      return -1;
    }
  return 0;
}

INET_FILE *
inet_file_open (const char *url, const char *type)
{
  HINTERNET hURL;
  INET_FILE *ifp;
#ifndef NO_STDIO
  FILE *fp;
#endif

  if (url == NULL)
    {
      return NULL;
    }

  if (strncmp (url, "http:", 5) && strncmp (url, "ftp:", 4)
      && strncmp (url, "https:", 6) && strncmp (url, "file:", 5))
    {
#ifndef NO_STDIO
      fp = fopen (url, type);
      if (NULL != fp)
	{
	  ifp = new INET_FILE;
	  strncpy (ifp->url, url, 4096);
	  ifp->fp = fp;
	  return ifp;
	}
      return NULL;
#else
      HANDLE hFile = CreateFile (url,
				 GENERIC_READ,	// Open for reading
				 FILE_SHARE_READ,	// Share for reading
				 NULL,	// No security
				 OPEN_EXISTING,	// Existing file only
				 FILE_ATTRIBUTE_NORMAL,	// Normal file
				 NULL);	// No template file
      if (hFile != INVALID_HANDLE_VALUE)
	{
	  ifp = new INET_FILE;
	  strncpy (ifp->url, url, 4096);
	  ifp->hFile = hFile;
	  return ifp;
	}
      // Process error ...
      return NULL;
#endif
    }


  if (hInternetSession == NULL)
    {
      if (inet_file_init (NULL, NULL, 0) < 0)
	{
	  return NULL;
	}
    }
  hURL = InternetOpenUrl (hInternetSession,
			  url,
			  NULL,
			  0,
			  INTERNET_FLAG_EXISTING_CONNECT |
			  INTERNET_FLAG_RELOAD, 0);
  if (hURL == NULL)
    {
      rcs_print_error ("Can not open URL %s. (Error =%d)\n",
		       url, GetLastError ());
      return NULL;
    }

  ifp = new INET_FILE;
  if (NULL != ifp)
    {
      strncpy (ifp->url, url, 4096);
    }
  return ifp;
}


char *
inet_file_gets (char *str, int maxlen, INET_FILE * inet_file)
{
  int bytes_in_str = 0;
  char *ptr_in_str = str;
  int end_of_line = 0;
  if (NULL == inet_file || NULL == str)
    {
      return NULL;
    }
#ifndef NO_STDIO
  if (NULL != inet_file->fp)
    {
      return (fgets (str, maxlen, inet_file->fp));
    }
#endif


  memset (str, 0, maxlen);
  while (!end_of_line && bytes_in_str < maxlen - 1 && !inet_file->end_of_file)
    {
      if (inet_file->bytes_left_in_buffer > 0)
	{
	  if (*inet_file->ptr_in_buffer == '\n')
	    {
	      *ptr_in_str = 0;
	      end_of_line = 1;
	      inet_file->ptr_in_buffer++;
	      inet_file->bytes_left_in_buffer--;
	      break;
	    }
	  else if (*inet_file->ptr_in_buffer == '\r')
	    {
	      inet_file->ptr_in_buffer++;
	      inet_file->bytes_left_in_buffer--;
	    }
	  else if (*inet_file->ptr_in_buffer == 0)
	    {
	      inet_file->bytes_left_in_buffer = 0;
	      inet_file->bytes_in_buffer = 0;
	      inet_file->ptr_in_buffer = inet_file->buffer;
	    }
	  else
	    {
	      *ptr_in_str = *inet_file->ptr_in_buffer;
	      ptr_in_str++;
	      bytes_in_str++;
	      inet_file->ptr_in_buffer++;
	      inet_file->bytes_left_in_buffer--;
	    }
	}
      else
	{
	  if (inet_file->last_buffer_read)
	    {
	      inet_file->end_of_file = 1;
	      *ptr_in_str = 0;
	      inet_file->bytes_left_in_buffer = 0;
	      inet_file->bytes_in_buffer = 0;
	      inet_file->ptr_in_buffer = inet_file->buffer;
	    }
	  else
	    {
	      memset (inet_file->buffer, 0, 4096);
	      if (NULL != inet_file->hURL)
		{
		  inet_file->url_read = InternetReadFile (inet_file->hURL,
							  inet_file->buffer,
							  4095,
							  &inet_file->
							  bytes_read);
		  if (!inet_file->url_read)
		    {
		      rcs_print_error
			("Error from InternetReadFile of %s. (error = %d)\n",
			 inet_file->url, GetLastError ());
		      inet_file->end_of_file = 1;
		      *ptr_in_str = 0;
		      return NULL;
		    }
		}
	      else if (INVALID_HANDLE_VALUE != inet_file->hFile)
		{
		  inet_file->url_read = ReadFile (inet_file->hFile,
						  inet_file->buffer,
						  4095,
						  &inet_file->bytes_read,
						  NULL);
		  if (!inet_file->url_read)
		    {
		      rcs_print_error
			("Error from ReadFile of %s. (error = %d)\n",
			 inet_file->url, GetLastError ());
		      inet_file->end_of_file = 1;
		      *ptr_in_str = 0;
		      return NULL;
		    }
		}
	      else
		{
		  return NULL;
		}
	      if (inet_file->bytes_read < 4095)
		{
		  inet_file->last_buffer_read = 1;
		}
	      inet_file->bytes_left_in_buffer = inet_file->bytes_read;
	      inet_file->bytes_in_buffer = inet_file->bytes_read;
	      inet_file->ptr_in_buffer = inet_file->buffer;
	    }
	}
    }
  return str;
}

int
inet_file_eof (INET_FILE * inet_file)
{
  if (NULL == inet_file)
    {
      return 1;
    }
#ifndef NO_STDIO
  if (NULL != inet_file->fp)
    {
      return (feof (inet_file->fp));
    }
#endif
  return inet_file->end_of_file;
}


int
inet_file_close (INET_FILE * inet_file)
{
  if (NULL != inet_file)
    {
#ifndef NO_STDIO
      if (NULL != inet_file->fp)
	{
	  fclose (inet_file->fp);
	  inet_file->fp = NULL;
	}
#endif
      if (NULL != inet_file->hURL)
	{
	  InternetCloseHandle (inet_file->hURL);
	  inet_file->hURL = NULL;
	}
      if (INVALID_HANDLE_VALUE != inet_file->hFile)
	{
	  CloseHandle (inet_file->hFile);
	  inet_file->hFile = INVALID_HANDLE_VALUE;
	}
      inet_file->end_of_file = 1;
      inet_file->ptr_in_buffer = inet_file->buffer;
      inet_file->bytes_left_in_buffer = 0;
      delete inet_file;
    }
  return 0;
}



int
inet_file_exit ()
{
  if (NULL != hInternetSession)
    {
      InternetCloseHandle (hInternetSession);
      hInternetSession = NULL;
    }
  return 0;
}


int
inet_file_rewind (INET_FILE * ifp)
{
  if (NULL == ifp)
    {
      return -1;
    }
#ifndef NO_STDIO
  if (ifp->fp != NULL)
    {
      rewind (ifp->fp);
      return 0;
    }
#endif

  if (INVALID_HANDLE_VALUE != ifp->hFile)
    {
      CloseHandle (ifp->hFile);
      ifp->hFile = CreateFile (ifp->url, GENERIC_READ,	// Open for reading
			       FILE_SHARE_READ,	// Share for reading
			       NULL,	// No security
			       OPEN_EXISTING,	// Existing file only
			       FILE_ATTRIBUTE_NORMAL,	// Normal file
			       NULL);	// No template file
      ifp->bytes_in_buffer = 0;
      ifp->bytes_left_in_buffer = 0;
      ifp->end_of_file = 0;
      ifp->url_read = FALSE;
      ifp->last_buffer_read = 0;
      ifp->ptr_in_buffer = ifp->buffer;
      ifp->bytes_read = 0;
      memset (ifp->buffer, 0, 4096);
      return 0;
    }
  else if (NULL != ifp->hURL)
    {
      InternetCloseHandle (ifp->hURL);
      ifp->hURL = InternetOpenUrl (hInternetSession,
				   ifp->url,
				   NULL,
				   0,
				   INTERNET_FLAG_EXISTING_CONNECT |
				   INTERNET_FLAG_RELOAD, 0);
      if (ifp->hURL == NULL)
	{
	  rcs_print_error ("Can not open URL %s. (Error =%d)\n",
			   ifp->url, GetLastError ());
	  return -1;
	}
    }
  ifp->bytes_in_buffer = 0;
  ifp->bytes_left_in_buffer = 0;
  ifp->end_of_file = 0;
  ifp->url_read = FALSE;
  ifp->last_buffer_read = 0;
  ifp->ptr_in_buffer = ifp->buffer;
  ifp->bytes_read = 0;
  memset (ifp->buffer, 0, 4096);
  return 0;
}


#else



#include "inetfile.hh"

#ifndef NO_STDIO
#include <stdio.h>
#endif
#include <string.h>
#include <stdlib.h>

#ifdef HAVE_LIBDL
#ifndef  DYNAMICALLY_LOAD_LIBWWW
#define DYNAMICALLY_LOAD_LIBWWW
#endif
#endif

#ifndef DYNAMICALLY_LOAD_LIBWWW
#include "WWWLib.h"
#include "WWWHTTP.h"
#include "WWWInit.h"
#define DL(x) x
#else
extern "C"
{
#define CORE_TRACE      (WWWTRACE & SHOW_CORE_TRACE)
#define SHOW_CORE_TRACE 0x2000
#define SHOW_STREAM_TRACE 0x40
#define SHOW_PROTOCOL_TRACE 0x80
#define PARSE_ALL               31
#define BOOL char
#define HT_FREE(pointer)        {ptr_to_HTMemory_free((pointer));((pointer))=NULL;}
#define WWW_SOURCE      ptr_to_HTAtom_for("*/*")

  typedef void *HTRequest;
  typedef void *HTList;
   ;
#ifdef DEBUG
  static int *WWW_TraceFlag = NULL;	/* In DLLs, we need the indirection */
#define WWWTRACE        (*WWW_TraceFlag)
#else
  static int WWW_TraceFlag = 0;
#define WWWTRACE        (WWW_TraceFlag)
#endif				/* DEBUG */
#define YES             (BOOL)1


  typedef struct _HTAtom HTAtom;
  struct _HTAtom
  {
    HTAtom *next;
  };				/* struct _HTAtom */
  typedef void *HTAnchor;
  typedef void *HTStream;
  typedef void *HTChunk;
  typedef void *HTFormat;
#define DL(x) ptr_to_##x

  static void (*ptr_to_HTMemory_free) (void *) = NULL;
  static HTRequest *(*ptr_to_HTRequest_new) (void) = NULL;
  static BOOL (*ptr_to_HTLibInit) (const char *AppName,
				   const char *AppVersion) = NULL;
  static HTList *(*ptr_to_HTList_new) (void) = NULL;
  static void (*ptr_to_HTTransportInit) (void) = NULL;
  static void (*ptr_to_HTProtocolInit) (void) = NULL;
  static void (*ptr_to_HTNetInit) (void) = NULL;
  static void (*ptr_to_HTConverterInit) (HTList * conversions) = NULL;
  static void (*ptr_to_HTFormat_setConversion) (HTList * list) = NULL;
  static void (*ptr_to_HTEncoderInit) (HTList * encodings) = NULL;
  static void (*ptr_to_HTFormat_setTransferCoding) (HTList * encodings) =
    NULL;
  static void (*ptr_to_HTMIMEInit) (void) = NULL;
  static HTAtom *(*ptr_to_HTAtom_for) (const char *string) = NULL;
  static void (*ptr_to_HTRequest_setOutputStream) (HTRequest * request,
						   HTStream * output);
  static void (*ptr_to_HTRequest_setPreemptive) (HTRequest * request,
						 BOOL mode) = NULL;
  static char *(*ptr_to_HTGetCurrentDirectoryURL) (void) = NULL;
  static void (*ptr_to_HTRequest_setOutputFormat) (HTRequest * request,
						   HTFormat format) = NULL;
  static char *(*ptr_to_HTParse) (const char *aName, const char *relatedName,
				  int wanted) = NULL;
  static HTAnchor *(*ptr_to_HTAnchor_findAddress) (const char *address) =
    NULL;
  static HTChunk *(*ptr_to_HTLoadAnchorToChunk) (HTAnchor * anchor,
						 HTRequest * request) = NULL;
  static char *(*ptr_to_HTChunk_toCString) (HTChunk * ch) = NULL;
  static void (*ptr_to_HTRequest_delete) (HTRequest * request) = NULL;
  static void (*ptr_to_HTFormat_deleteAll) (void) = NULL;
  static BOOL (*ptr_to_HTLibTerminate) (void) = NULL;

  typedef void (*HTMemory_free_func_type) (void *);
  typedef HTRequest *(*HTRequest_new_func_type) (void);
  typedef BOOL (*HTLibInit_func_type) (const char *AppName,
				       const char *AppVersion);
  typedef HTList *(*HTList_new_func_type) (void);
  typedef void (*HTTransportInit_func_type) (void);
  typedef void (*HTProtocolInit_func_type) (void);
  typedef void (*HTNetInit_func_type) (void);
  typedef void (*HTConverterInit_func_type) (HTList * conversions);
  typedef void (*HTFormat_setConversion_func_type) (HTList * list);
  typedef void (*HTEncoderInit_func_type) (HTList * encodings);
  typedef void (*HTFormat_setTransferCoding_func_type) (HTList * encodings);
  typedef void (*HTMIMEInit_func_type) (void);
  typedef HTAtom *(*HTAtom_for_func_type) (const char *string);
  typedef void (*HTRequest_setOutputStream_func_type) (HTRequest * request,
						       HTStream * output);
  typedef void (*HTRequest_setPreemptive_func_type) (HTRequest * request,
						     BOOL mode);
  typedef char *(*HTGetCurrentDirectoryURL_func_type) (void);
  typedef void (*HTRequest_setOutputFormat_func_type) (HTRequest * request,
						       HTFormat format);
  typedef char *(*HTParse_func_type) (const char *aName,
				      const char *relatedName, int wanted);
  typedef HTAnchor *(*HTAnchor_findAddress_func_type) (const char *address);
  typedef HTChunk *(*HTLoadAnchorToChunk_func_type) (HTAnchor * anchor,
						     HTRequest * request);
  typedef char *(*HTChunk_toCString_func_type) (HTChunk * ch);
  typedef void (*HTRequest_delete_func_type) (HTRequest * request);
  typedef void (*HTFormat_deleteAll_func_type) (void);
  typedef BOOL (*HTLibTerminate_func_type) (void);

  static int load_libwww_status = 0;

#include <dlfcn.h>

  int load_libwww_functions (void *handle)
  {
    ptr_to_HTMemory_free =
      (HTMemory_free_func_type) dlsym (handle, "HTMemory_free");
    if (NULL == ptr_to_HTMemory_free)
      {
	return -1;
      }
    ptr_to_HTRequest_new =
      (HTRequest_new_func_type) dlsym (handle, "HTRequest_new");
    if (NULL == ptr_to_HTRequest_new)
      {
	return -1;
      }
    ptr_to_HTLibInit = (HTLibInit_func_type) dlsym (handle, "HTLibInit");
    if (NULL == ptr_to_HTLibInit)
      {
	return -1;
      }
    ptr_to_HTList_new = (HTList_new_func_type) dlsym (handle, "HTList_new");
    if (NULL == ptr_to_HTList_new)
      {
	return -1;
      }

    ptr_to_HTTransportInit =
      (HTTransportInit_func_type) dlsym (handle, "HTTransportInit");
    if (NULL == ptr_to_HTTransportInit)
      {
	return -1;
      }

    ptr_to_HTProtocolInit =
      (HTProtocolInit_func_type) dlsym (handle, "HTProtocolInit");
    if (NULL == ptr_to_HTProtocolInit)
      {
	return -1;
      }


    ptr_to_HTNetInit = (HTNetInit_func_type) dlsym (handle, "HTNetInit");
    if (NULL == ptr_to_HTNetInit)
      {
	return -1;
      }

    ptr_to_HTConverterInit =
      (HTConverterInit_func_type) dlsym (handle, "HTConverterInit");
    if (NULL == ptr_to_HTConverterInit)
      {
	return -1;
      }


    ptr_to_HTFormat_setConversion =
      (HTFormat_setConversion_func_type) dlsym (handle,
						"HTFormat_setConversion");
    if (NULL == ptr_to_HTFormat_setConversion)
      {
	return -1;
      }


    ptr_to_HTEncoderInit =
      (HTEncoderInit_func_type) dlsym (handle, "HTEncoderInit");
    if (NULL == ptr_to_HTEncoderInit)
      {
	return -1;
      }

    ptr_to_HTFormat_setTransferCoding =
      (HTFormat_setTransferCoding_func_type) dlsym (handle,
						    "HTFormat_setTransferCoding");
    if (NULL == ptr_to_HTFormat_setTransferCoding)
      {
	return -1;
      }

    ptr_to_HTMIMEInit = (HTMIMEInit_func_type) dlsym (handle, "HTMIMEInit");
    if (NULL == ptr_to_HTMIMEInit)
      {
	return -1;
      }

    ptr_to_HTAtom_for = (HTAtom_for_func_type) dlsym (handle, "HTAtom_for");
    if (NULL == ptr_to_HTAtom_for)
      {
	return -1;
      }

    ptr_to_HTRequest_setOutputStream =
      (HTRequest_setOutputStream_func_type) dlsym (handle,
						   "HTRequest_setOutputStream");
    if (NULL == ptr_to_HTRequest_setOutputStream)
      {
	return -1;
      }

    ptr_to_HTRequest_setPreemptive =
      (HTRequest_setPreemptive_func_type) dlsym (handle,
						 "HTRequest_setPreemptive");
    if (NULL == ptr_to_HTRequest_setPreemptive)
      {
	return -1;
      }

    ptr_to_HTGetCurrentDirectoryURL =
      (HTGetCurrentDirectoryURL_func_type) dlsym (handle,
						  "HTGetCurrentDirectoryURL");
    if (NULL == ptr_to_HTGetCurrentDirectoryURL)
      {
	return -1;
      }

    ptr_to_HTRequest_setOutputFormat =
      (HTRequest_setOutputFormat_func_type) dlsym (handle,
						   "HTRequest_setOutputFormat");
    if (NULL == ptr_to_HTRequest_setOutputFormat)
      {
	return -1;
      }


    ptr_to_HTParse = (HTParse_func_type) dlsym (handle, "HTParse");
    if (NULL == ptr_to_HTParse)
      {
	return -1;
      }


    ptr_to_HTAnchor_findAddress =
      (HTAnchor_findAddress_func_type) dlsym (handle, "HTAnchor_findAddress");
    if (NULL == ptr_to_HTAnchor_findAddress)
      {
	return -1;
      }


    ptr_to_HTLoadAnchorToChunk =
      (HTLoadAnchorToChunk_func_type) dlsym (handle, "HTLoadAnchorToChunk");
    if (NULL == ptr_to_HTLoadAnchorToChunk)
      {
	return -1;
      }


    ptr_to_HTChunk_toCString =
      (HTChunk_toCString_func_type) dlsym (handle, "HTChunk_toCString");
    if (NULL == ptr_to_HTChunk_toCString)
      {
	return -1;
      }


    ptr_to_HTRequest_delete =
      (HTRequest_delete_func_type) dlsym (handle, "HTRequest_delete");
    if (NULL == ptr_to_HTRequest_delete)
      {
	return -1;
      }


    ptr_to_HTFormat_deleteAll =
      (HTFormat_deleteAll_func_type) dlsym (handle, "HTFormat_deleteAll");
    if (NULL == ptr_to_HTFormat_deleteAll)
      {
	return -1;
      }


    ptr_to_HTLibTerminate =
      (HTLibTerminate_func_type) dlsym (handle, "HTLibTerminate");
    if (NULL == ptr_to_HTLibTerminate)
      {
	return -1;
      }
    return 0;
  }

  static void *libwww_handle = NULL;
  int show_load_libwww_errors = 1;

  int load_libwww ()
  {
    char libwww_name[256];
    char *ldpath = getenv ("LD_LIBRARY_PATH");
    libwww_handle = dlopen (NULL, RTLD_LAZY | RTLD_GLOBAL);
    memset (libwww_name, 0, 256);
    if (NULL != libwww_handle)
      {
	if (load_libwww_functions (libwww_handle) == 0)
	  {
	    load_libwww_status = 1;
	    return 0;
	  }
      }
    else
      {
	fprintf (stderr, "dlopen failed. %s\n", dlerror ());
      }
    libwww_handle = dlopen ("libwww.so", RTLD_LAZY | RTLD_GLOBAL);
    if (NULL != libwww_handle)
      {
	if (load_libwww_functions (libwww_handle) == 0)
	  {
	    load_libwww_status = 1;
	    return 0;
	  }
      }
    else
      {
	fprintf (stderr, "dlopen(%s) failed. %s\n", libwww_name, dlerror ());
      }

    if (NULL != ldpath)
      {
	char *dir = strtok (ldpath, ";:,");
	while (NULL != dir)
	  {
	    if (*dir == 0)
	      {
		break;
	      }
	    strncpy (libwww_name, dir, 256);
	    strncat (libwww_name, "/libwww.so", 256 - strlen (dir));
	    libwww_handle = dlopen (libwww_name, RTLD_LAZY | RTLD_GLOBAL);
	    if (NULL != libwww_handle)
	      {
		if (load_libwww_functions (libwww_handle) == 0)
		  {
		    load_libwww_status = 1;
		    return 0;
		  }
	      }
	    else
	      {
		fprintf (stderr, "dlopen(%s) failed. %s\n", libwww_name,
			 dlerror ());
	      }
	    dir = strtok (NULL, ";:,");
	  }
      }

    strncpy (libwww_name, "/isd/proj/rcslib/plat/" PLATNAME "/lib", 256);
    strcat (libwww_name, "/libwww.so");
    libwww_handle = dlopen (libwww_name, RTLD_LAZY | RTLD_GLOBAL);
    if (NULL != libwww_handle)
      {
	if (load_libwww_functions (libwww_handle) == 0)
	  {
	    load_libwww_status = 1;
	    return 0;
	  }
      }
    else
      {
	fprintf (stderr, "dlopen(%s) failed. %s\n", libwww_name, dlerror ());
      }
    strncpy (libwww_name, "/usr/local/rcslib/plat/" PLATNAME "/lib", 256);
    strcat (libwww_name, "/libwww.so");
    libwww_handle = dlopen (libwww_name, RTLD_LAZY | RTLD_GLOBAL);
    if (NULL != libwww_handle)
      {
	if (load_libwww_functions (libwww_handle) == 0)
	  {
	    load_libwww_status = 1;
	    return 0;
	  }
      }
    else
      {
	fprintf (stderr, "dlopen(%s) failed. %s\n", libwww_name, dlerror ());
      }
    strncpy (libwww_name, "/usr/local/nist/rcslib/plat/" PLATNAME "/lib",
	     256);
    strcat (libwww_name, "/libwww.so");
    libwww_handle = dlopen (libwww_name, RTLD_LAZY | RTLD_GLOBAL);
    if (NULL != libwww_handle)
      {
	if (load_libwww_functions (libwww_handle) == 0)
	  {
	    load_libwww_status = 1;
	    return 0;
	  }
      }
    else
      {
	fprintf (stderr, "dlopen(%s) failed. %s\n", libwww_name, dlerror ());
      }
    return (load_libwww_status = -1);
  }


}



#endif


static HTRequest *request = NULL;
static HTList *converters = NULL;	/* List of converters */
static HTList *encodings = NULL;	/* List of encodings */

class INET_FILE
{
public:
  INET_FILE ();
  ~INET_FILE ();
  char *buffer;
  char *current_line;
  FILE *local_fp;

private:
    INET_FILE (INET_FILE & ifile);	// Don't copy me.
};


INET_FILE::INET_FILE ()
{
  buffer = NULL;
  current_line = NULL;
  local_fp = NULL;
}

INET_FILE::~INET_FILE ()
{
  if (NULL != local_fp)
    {
      fclose (local_fp);
      local_fp = NULL;
    }
  else if (NULL != buffer)
    {
      HT_FREE (buffer);
      buffer = NULL;
    }
  current_line = NULL;
}


int
inet_file_init (const char *agent_name, char *agent_version, int debug)
{
#ifdef DYNAMICALLY_LOAD_LIBWWW
  switch (load_libwww_status)
    {
    case 0:
      if (load_libwww () == -1)
	{
	  fprintf (stderr, "Can't load www library.\n");
	  return (-1);
	}
      break;
    case 1:
      break;

    case -1:
    default:
      return (-1);
    }
#endif

  if (NULL == request)
    {
      request = DL (HTRequest_new ());
    }
  if (NULL == converters)
    {
      converters = DL (HTList_new ());	/* List of converters */
    }
  if (NULL == encodings)
    {
      encodings = DL (HTList_new ());	/* List of encodings */
    }

  if (NULL == agent_name)
    {
      agent_name = "InetFileAgent";
    }
  if (NULL == agent_version)
    {
      agent_version = "1.0";
    }
  /* Initialize libwww core */
  DL (HTLibInit (agent_name, agent_version));

  /* Turn on TRACE so we can see what is going on */
  if (debug)
    {
      WWWTRACE = SHOW_CORE_TRACE + SHOW_STREAM_TRACE + SHOW_PROTOCOL_TRACE;
    }

  /* Register the default set of transport protocols */
  DL (HTTransportInit ());

  /* Register the default set of protocol modules */
  DL (HTProtocolInit ());

  /* Register the default set of BEFORE and AFTER callback functions */
  DL (HTNetInit ());

  /* Register the default set of converters */
  DL (HTConverterInit (converters));
  DL (HTFormat_setConversion (converters));

  /* Register the default set of transfer encodings and decoders */
  DL (HTEncoderInit (encodings));
  DL (HTFormat_setTransferCoding (encodings));

  /* Register the default set of MIME header parsers */
  DL (HTMIMEInit ());

  /* Set up the request and pass it to the Library */
  DL (HTRequest_setOutputFormat (request, WWW_SOURCE));
  DL (HTRequest_setPreemptive (request, YES));

  inet_file_initialized = 1;

  /* I don't know how to detect an error so I always return zero. */
  return 0;
}


INET_FILE *
inet_file_open (const char *url, char *type)
{
  HTChunk *chunk = NULL;
  char *string = NULL;
  INET_FILE *inet_file = NULL;

  if (strncmp (url, "http:", 5) && strncmp (url, "ftp:", 4)
      && strncmp (url, "https:", 6) && strncmp (url, "file:", 5))
    {
      inet_file = new INET_FILE ();
      inet_file->local_fp = fopen (url, type);
      if (NULL == inet_file->local_fp)
	{
	  delete inet_file;
	  return NULL;
	}
      return inet_file;
    }

  if (!inet_file_initialized)
    {
      inet_file_init (NULL, NULL, 0);
    }

  if (NULL == url || NULL == request || NULL == converters ||
      NULL == encodings)
    {
      if (NULL != inet_file)
	{
	  delete inet_file;
	}
      return NULL;
    }

  char *cwd = DL (HTGetCurrentDirectoryURL ());
  char *absolute_url = DL (HTParse (url, cwd, PARSE_ALL));
  if (NULL == absolute_url)
    {
      if (NULL != inet_file)
	{
	  delete inet_file;
	}
      return NULL;
    }
  HTAnchor *anchor = DL (HTAnchor_findAddress (absolute_url));
  chunk = DL (HTLoadAnchorToChunk (anchor, request));
  HT_FREE (absolute_url);
  HT_FREE (cwd);

  /* If chunk != NULL then we have the data */
  if (chunk == NULL)
    {
      if (NULL != inet_file)
	{
	  delete inet_file;
	}
      return NULL;
    }
  string = DL (HTChunk_toCString (chunk));
  if (string == NULL)
    {
      if (NULL != inet_file)
	{
	  delete inet_file;
	}
      return NULL;
    }
  if (*string == 0)
    {
      if (NULL != inet_file)
	{
	  delete inet_file;
	}
      return NULL;
    }
  inet_file = new INET_FILE;
  if (NULL == inet_file)
    {
      return NULL;
    }
  inet_file->buffer = string;
  inet_file->current_line = inet_file->buffer;
  inet_file->local_fp = NULL;

  return inet_file;
}


char *
inet_file_gets (char *str, int maxlen, INET_FILE * inet_file)
{
  unsigned long bytes_to_copy;
  char *next_line;
  if (NULL == str || NULL == inet_file)
    {
      return NULL;
    }
  if (NULL != inet_file->local_fp)
    {
      return fgets (str, maxlen, inet_file->local_fp);
    }
  bytes_to_copy = 0;
  next_line = strchr (inet_file->current_line, '\n');
  if (next_line != NULL)
    {
      bytes_to_copy =
	((unsigned long) next_line) -
	((unsigned long) inet_file->current_line);
    }
  else
    {
      bytes_to_copy = strlen (inet_file->current_line);
    }
  bytes_to_copy =
    ((int) bytes_to_copy) > ((int) maxlen - 1) ? maxlen - 1 : bytes_to_copy;
  strncpy (str, inet_file->current_line, bytes_to_copy);
  str[bytes_to_copy] = 0;
  if (next_line != NULL)
    {
      inet_file->current_line = next_line + 1;
    }
  else
    {
      inet_file->current_line = NULL;
    }
  return str;
}

int
inet_file_close (INET_FILE * inet_file)
{
  if (NULL != inet_file)
    {
      if (NULL != inet_file->local_fp)
	{
	  fclose (inet_file->local_fp);
	  inet_file->local_fp = NULL;
	}
      else if (NULL != inet_file->buffer)
	{
	  HT_FREE (inet_file->buffer);
	  inet_file->buffer = NULL;
	}
      delete inet_file;
    }
  return 0;
}

int
inet_file_eof (INET_FILE * inet_file)
{
  if (NULL == inet_file)
    {
      return 1;
    }
  if (NULL != inet_file->local_fp)
    {
      return feof (inet_file->local_fp);
    }
  if (NULL == inet_file->current_line || NULL == inet_file->buffer)
    {
      return 1;
    }
  return !(*inet_file->current_line);
}


int
inet_file_exit ()
{
#ifdef DYNAMICALLY_LOAD_LIBWWW
  if (load_libwww_status != 1)
    {
      return -1;
    }
#endif

  /* Clean up the request */
  DL (HTRequest_delete (request));
  DL (HTFormat_deleteAll ());

  /* Terminate the Library */
  DL (HTLibTerminate ());

  request = NULL;
  converters = NULL;
  encodings = NULL;
  inet_file_initialized = 0;

#ifdef DYNAMICALLY_LOAD_LIBWWW
  if (NULL != libwww_handle)
    {
      dlclose (libwww_handle);
      libwww_handle = NULL;
    }
#endif

  return 0;
}

int
inet_file_rewind (INET_FILE * ifp)
{
  if (NULL != ifp->local_fp)
    {
      rewind ((FILE *) ifp->local_fp);
      return 0;
    }
  else
    {
      ifp->current_line = ifp->buffer;
    }
  return 0;
}

/* !MS_WINDOWS_API */
#endif 
