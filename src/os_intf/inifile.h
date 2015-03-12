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

#ifndef INIFILE_H
#define INIFILE_H

/*
  inifile.h

  Decls for INI file format functions
*/

#ifdef INIFILE_USE_INET_FILES
#ifdef __cplusplus
class INET_FILE;
#else
#ifndef INET_FILE
#define INET_FILE void
#endif
#endif

/* INIFILE_USE_INET_FILES */
#endif

#ifdef __cplusplus
extern "C" {
#if 0
}
#endif
#endif

#define INIFILE_MAX_LINELEN 256	/* max number of chars in a line */

#define COMMENT_CHAR ';'	/* signifies a comment */

#define INI_OK      0
#define INI_DEFAULT 1
#define INI_INVALID 2

typedef struct {
  char tag[INIFILE_MAX_LINELEN];
  char rest[INIFILE_MAX_LINELEN];
} INIFILE_ENTRY;

/*
   Positions file at line after section tag. Returns 0 if section found;
   -1 if not.
   */
extern int
findSection (void *fp, const char *section);

extern const char *iniFind (void *fp,	/* already opened file ptr */
			    const char *tag,	/* string to find */
			    const char *section);	/* section it's in */

extern int iniSection (void *fp,	/* already opened file ptr */
		       const char *section,	/* section you want */
		       INIFILE_ENTRY array[],	/* entries to fill */
		       int max);	/* how many you can hold */

#ifdef __cplusplus
#if 0
{
#endif
}
#endif

#ifdef __cplusplus

class INIFILE_PRIVATE_DATA;

class INIFILE {
 public:
  INIFILE ();
  INIFILE (const char *path);
  ~INIFILE ();

  int open (const char *path);
  int close ();
  int isSection(const char * section);
  const char *find (const char *tag, const char *section_to_find = 0);
  int section (const char *section_to_load, INIFILE_ENTRY array[], int max);
  int valid ();

 private:
  INIFILE_PRIVATE_DATA *ipd;
  
 private:
  /* prevent copying. */
  INIFILE(const INIFILE &);
  INIFILE &operator=(const INIFILE &);
};

/* C++ part */
#endif 

/* INIFILE_H */
#endif

/*
  Modification history:

  $Log$
  Revision 1.3  2005/06/21 13:25:34  proctor
  Added INI_OK,DEFAULT,INVALID


  24-Feb-1998  FMP replaced __CPLUSPLUS__ with __cplusplus
  18-Dec-1997  FMP set up as C/C++ split for emcnml directory
  24-Apr-1997  FMP split into C and C++ parts
*/
