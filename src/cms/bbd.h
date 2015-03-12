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

/****************************************************************************/
/*  bbd.h

    5/13/93    murphy  commented out BBD_SOLE_READER, not implemented
   Jan 1 1992  murphy  created
*****************************************************************************/

#ifndef INC_BBD_H
#define INC_BBD_H

#ifdef VXWORKS
#include "vxWorks.h"
#include "semLib.h"

/* parameter defs */
/*  bbd types */
typedef enum
{
  BBD_NULL = 0,
/*  BBD_SOLE_READER = 1,  NOT implmented */
  BBD_MULT_READER = 2,
  BBD_SOLE_WRITER = 3,
  BBD_MULT_WRITER = 4
}
BBD_TYPE;

/* read write types */
typedef enum
{
  BBD_PLAIN_READ = 1,
  BBD_FRESH_READ = 2,
  BBD_SPY_READ = 3
}
BBD_READ_TYPE;

typedef enum
{
  BBD_PLAIN_WRITE = 1,
  BBD_WAIT_WRITE = 2
}
BBD_WRITE_TYPE;

#ifndef BBD_TYPEDEFED
#define BBD_TYPEDEFED
typedef void *BBD;
#endif

/* function declarations */
#ifdef  __STDC__

IMPORT BBD bbdConnect (char *bbdName, BBD_TYPE bbdType, int bbdSize);
IMPORT STATUS bbdRead (BBD id, void *data, BBD_READ_TYPE readType,
		       int timeout);
IMPORT STATUS bbdClear (BBD id);
IMPORT STATUS bbdWrite (BBD id, void *data, BBD_WRITE_TYPE writeType, ...);
  /* If BBD_WRITE_TYPE == BBD_WAIT_WRITE, then last arg = timeout */
IMPORT STATUS bbdDelete (BBD id);
IMPORT char *bbdName (BBD id);
IMPORT BBD bbdNameToId (char *name);
IMPORT STATUS bbdShow (BBD id, char *name);

#else

IMPORT BBD bbdConnect ();
IMPORT int bbdRead ();
IMPORT int bbdClear ();
IMPORT int bbdWrite ();
IMPORT STATUS bbdDelete ();
IMPORT char *bbdName ();
IMPORT BBD bbdNameToId ();
IMPORT int bbdShow ();

#endif /* __STDC__ */

/* bbd structures:  writer and reader */
#define BBD_NAME_SIZE 100

typedef struct bbdWriter
{
  BBD_TYPE type;		/* BBD_SOLE_WRITER or BBD_MULT_WRITER */
  SEM_ID sem;			/* semaphore for bbd */
  char name[BBD_NAME_SIZE];	/* name of the bulletin board */
  char *buf;			/* pointer to data buffer */
  int size;			/* size of data */
  int writeCnt;			/* message count of current write */
  int readCnt;			/* message count of last read */
  SEM_ID waitingToWriteSem;	/* sem: last data has been read */
  SEM_ID waitingToReadSem;	/* sem: new data has been writen.  Always
				 * remains in the 'taken' state.  SemFlush
				 * does not change the state. */
  int taskId;			/* who has write priveledge */
  struct bbdReader *readers;	/* link to reader list */
  struct bbdWriter *next;	/* link to next bulletin board in list */
}
BBD_WRITER;

typedef struct bbdReader
{
  BBD_TYPE type;		/* BBD_SOLE_READER or BBD_MULT_READER */
  BBD_WRITER *bbd;		/* link to bulletin board structure */
  int readCnt;			/* message count of last read */
  int taskId;			/* task id of owner */
  struct bbdReader *next;	/* link to next reader in list */
}
BBD_READER;

/* VXWORKS */
#endif

  /* INC_BBD_H */
#endif

