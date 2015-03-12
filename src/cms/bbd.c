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



/**************************************************************************

This is a collection of routines designed to provide a simple
communications mechanism between multiple processes running under VxWorks.
They provide the user with a flexible and sometimes reliable means of passing
data between, and synchronizing, independently spawned processes.  At the
same time the routines take care of the bookkeeping associated with
maintaining the timers and semiphores necessary to ensure reliablility
and mutually exclusive access to the data being exchanged.


History:
03/20/98  murphy:  clean up code
07/28/92  murphy:  free data buffer in bbdDeleteWriterStruct()
03/30/92  murphy:  change bbdShow to print task names.  Added task Id to
                   readrer struct.
03/21/92  murphy:  Added NULL id check on user routines.  Change type to
                   BBD_NULL in bbdDelete().  Added bbdName().  Gave
                   bbdWrite() a variable arg list.
03/10/92  murphy:  Added bbdHelpHelpStr.
03/05/92  murphy:  Created.


The major routines are bbdConnect, bbdRead, bbdWrite, bbdClear, and bbdDelete.
Shell help routines are bbdHelp and bbdShow.

BBD
bbdConnect(name, type, size)
  This connects to a bbd with the given name, creating it if it doesn't
  exist, and returns a bbd id.  The id is used in subsequent calls of read,
  write, etc.  The size of the data buffer (in bytes) must be the same for
  all tasks that use the named bbd.  bbd's with different names can have
  different sizes.  The type must be one of the following

  BBD_SOLE_READER only this task can read from the bbd (not implemented)
  BBD_MULT_READER one of many possible readers.  Each task should connect
                  with its own BBD.
  BBD_SOLE_WRITER only this task can write to the bbd.  If other tasks had
                  previously connected as writers, then bbdWrite() will
                  return ERROR when those tasks try to write to the bbd.
                  Each task can have its own BBD or multiple tasks can use
                  the same BBD if each task reconnects after another task
                  used the BBD.
  BBD_MULT_WRITER one of many possible writers.  Any other tasks that
                  connected as a writer can now write to the bbd.  Each task
                  can have its own BBD or multiple tasks can use the same BBD.

  Normally, a task connects once at initialization, uses the id for either
  reads or writes, and may then delete a reader bbd when the task is ending.
-----------------------

STATUS
bbdRead(id, dataPt, readType, timeout)
  This copies the data from the bbd into the data buffer pointed to by
  dataPt.  The bbd must have been opened as a reader.  The read type must
  be one of the following

  BBD_PLAIN_READ copies data, fresh or not.  If no data has ever been
                 written to the bbd, this will block (up to timeout) for
                 another task to write.
  BBD_FRESH_READ waits (up to timeout) until new data has been writen to
                 the bbd.  If the data is fresh, bbdRead() returns without
                 blocking.  This option insures that another task has written
                 to the bbd at least once since the current task last read
                 from the bbd.  This option does NOT insure that no other
                 task has read the data as each reader maintains its own
                 "freshness" flags.  Timeout can be a value, NO_WAIT, or
                 WAIT_FOREVER.
  BBD_SPY_READ   like a BBD_PLAIN_READ but does not set any flags used for
                 synchronization.  This allows a monitoring program to
                 read a snapshot of the data without disturbing the tasks
                 that are using the bbd for synchronization.

  Returns OK or ERROR if bad id or it waited more than timeout for fresh
  data or for the first data (if the bbd was originally empty).
-----------------------

STATUS
bbdWrite(id, dataPt, writeType [,timeout])
  This writes data to the bbd.  The bbd must have been opened as a writer.
  The writeType can be one of the following

  BBD_PLAIN_WRITE writes to the bbd regardless if the old data has been read
                  or not.  "timeout" is not needed.
  BBD_WAIT_WRITE  waits (up to timeout) until the data last writen to the
                  bbd has been read by at least one reader.  Currently,
                  there is no means to ensure that all readers have read
                  the old data.  Timeout can be a value, NO_WAIT, or
                  WAIT_FOREVER.

  Returns OK or ERROR if bad id, it waited more than timeout, or if the
  bbd is of type BBD_SOLE_WRITER and the task is not the owner.
-----------------------

STATUS
bbdClear(id)
  This clears the bbd for both reader bbds and writer bbds.

  For readers, it is like a read with no wait and no data transfer. This
  would be used to make sure subsequent reads with the fresh option get data
  writen after the flush.

  For writers: If sole_writer: flush old buffer and make it look like no one
  has ever writen to the bbd.   All readers (plain, fresh, or spy) will
  block or return ERROR until the writer puts data into the buffer.  If
  multi_writer:  make it look like someone just read.

  Returns OK or ERROR if bad id;
-----------------------

STATUS
bbdDelete(id)
  Deletes a reader bbd and frees memory.  Currently, there is no way to
  delete a writer bbd.
  New memory (16 bytes) is allocated every time a reader connects to a bbd.
  This is a small amount of memory, but if a task continuously connects as
  a reader it will eventually run out of memory unless it calls bbdDelete.
  When a writer connects to a bbd, the old data structure is used and no
  new memmory is allocated.

  Returns OK or ERROR if bad id;
-----------------------

char   *
bbdName(BBD id)
  returns pointer to the name of the bbd.  NO NOT change the name.

  Returns pointer or NULL if bad id;
-----------------------

BBD
bbdNameToId(char   *name)
  returns BBD writer id that matches the given name.

  Returns bbd or NULL if bad name;
-----------------------

bbdHelp
  Prints help info to the screen.
-----------------------

bbdShow [id, name]
  This a diagnostic routine called from the shell that prints info on the
  given bbd.  if id = 0, the name is used.  If the name is 0, a list of all
  the bbds is displayed.
-----------------------


Function Declarations:

BBD    bbdConnect(char *bbdName, BBD_TYPE bbdType, int bbdSize);
STATUS bbdRead(BBD id, void *data, BBD_READ_TYPE readType, int timeout);
STATUS bbdClear(BBD id);
STATUS bbdWrite(BBD id, void *data, BBD_WRITE_TYPE writeType, ...);
STATUS bbdDelete(BBD id);
char * bbdName(BBD id);
BBD    bbdNameToId(char *name);
int    bbdShow(BBD id, char *name);
------------------------

Examples:
Send command to retro;

retroSendCmd(RET_COMMAND *cmdBuf)
{
  static BBD id = NULL;

  if(ERROR == bbdWrite(id, cmdBuf, BBD_PLAIN_WRITE))
  {
    id = bbdConnect("retroCommand", BBD_SOLE_WRITER, sizeof(RET_COMMAND));
    if(ERROR == bbdWrite(id, cmdBuf, BBD_PLAIN_WRITE))
    {
      logMsg("ERROR:  Can't write to retroCommand BBD\n",
      0,0,0,
      0,0,0);
      return ERROR;
    }
  }
  return OK;
}

Read retro command;

***************************************************************************
***************************************************************************/


#include "vxWorks.h"
#include "semLib.h"
#include "memLib.h"
#include "taskLib.h"
#include "stdarg.h"
#include "fioLib.h"
#include "stdio.h"
#include "logLib.h"
#include "string.h"

#include "bbd.h"
#include "dbg_mem.h"		/* DEBUG_MALLOC,DEBUG_FREE,DEBUG_CALLOC */

/* typedef struct bbdWriter{ . . . } BBD_WRITER and
 * typedef struct bbdReader{ . . . } BBD_READER moved from here to
 * bbd.h so they could be used by bbdmem.cc  on 27-Oct-1997 by WPS
 */



#define BBD_MASTER (BBD_WRITER *) NULL


/* Declarations for LOCAL functions */
LOCAL void bbdLock (BBD_WRITER * id);
LOCAL void bbdUnlock (BBD_WRITER * id);
LOCAL BBD_WRITER *bbdCreateWriterStruct (char *bbdName, BBD_TYPE bbdType,
					 int bufSize);
LOCAL BOOL bbdDeleteWriterStruct (BBD_WRITER * id);
LOCAL BBD_READER *bbdCreateReaderStruct (char *name, BBD_TYPE type,
					 int bufSize, BBD_WRITER * bbd);
LOCAL BBD_WRITER *bbdFindWriter (char *name);
LOCAL BBD_WRITER *bbdLinkWriter (BBD_WRITER * id);
LOCAL BBD_WRITER *bbdConnectWriter (BBD_WRITER * id, BBD_TYPE type,
				    int bufSize);
STATUS bbdDelete (BBD id);


LOCAL BBD_WRITER *bbdList = NULL;
LOCAL SEM_ID bbdMasterSem = NULL;	/* master bulletin board semaphore */


/*************************************************************************/
LOCAL void
bbdSuspend (char *format, ...)
{
  char st[300];

  va_list args;
  va_start (args, format);

  vsprintf (st, format, args);
  logMsg (st, 0, 0, 0, 0, 0, 0);
  logMsg ("Suspending task\n", 0, 0, 0, 0, 0, 0);
  taskSuspend (0);

  va_end (args);
}


/*************************************************************************/
/* These routines take and give the semaphores that prevents two different
tasks from accessing or modifying the data structure at the same time.
bbdLock(BBD_WRITER *id) takes the semaphore and bbdUnlock(BBD_WRITER *id)
gives the semaphore back to the system.  If id == BBD_MASTER, then bbdMasterSem
is used.  If bbdMasterSem does not exist, it is created. */

LOCAL void
bbdLock (BBD_WRITER * id)
{
  char st[100 + BBD_NAME_SIZE];

  if (id == BBD_MASTER)
    {
      if (bbdMasterSem == NULL)
	{
	  if (NULL == (bbdMasterSem = semMCreate (SEM_Q_PRIORITY |
						  SEM_DELETE_SAFE |
						  SEM_INVERSION_SAFE)))
	    {
	      bbdSuspend ("Can't create BBD master semaphore!\n");
	    }
	}
      if (semTake (bbdMasterSem, 100) == ERROR)
	{
	  bbdSuspend ("Bulletin Board master semaphore timed out!\n");

	}
    }
  else
    {
      if (semTake (id->sem, 100) == ERROR)
	{
	  sprintf (st, "BBD >%s< semaphore timed out!\n", id->name);
	  bbdSuspend (st);
	}
    }
}

LOCAL void
bbdUnlock (BBD_WRITER * id)
{
  if (id == BBD_MASTER)
    semGive (bbdMasterSem);
  else
    semGive (id->sem);
}


/*************************************************************************/
/* This routine connects a writer to the named bulletin board and
returns the id that the program will use to identify the bulletin
board in future calls to bulletin board routines.  If it cannot find
the bulletin board it will create one with the given name.  The size
must be equal to that of an existing buffer of that name.
*/

/* Create a new bulletin board structure. */

LOCAL BBD_WRITER *
bbdCreateWriterStruct (char *bbdName, BBD_TYPE bbdType, int bufSize)
{
  BBD_WRITER *id;
  /* create bbd */
  if (NULL == (id = (BBD_WRITER *) DEBUG_MALLOC (sizeof (BBD_WRITER))))
    bbdSuspend ("Can't allocate memory for BBD!\n");
  id->type = bbdType;
  id->sem =
    semMCreate (SEM_Q_PRIORITY | SEM_DELETE_SAFE | SEM_INVERSION_SAFE);
  strncpy (id->name, bbdName, BBD_NAME_SIZE);
  id->name[BBD_NAME_SIZE] = 0;	/* make sure it ends */
  if (NULL == (id->buf = (char *) DEBUG_MALLOC (bufSize)))
    bbdSuspend ("Can't allocate memory for BBD data buffer!\n");

  id->size = bufSize;
  id->writeCnt = 0;
  id->readCnt = 0;
  id->waitingToWriteSem = semBCreate (SEM_Q_FIFO, SEM_FULL);
  id->waitingToReadSem = semBCreate (SEM_Q_FIFO, SEM_EMPTY);
  id->taskId = taskIdSelf ();
  id->readers = NULL;		/* no readers yet */
  /* id->next is set when bbd is added to the list */

  /* check semaphores */
  if (NULL == id->sem ||
      NULL == id->waitingToWriteSem || NULL == id->waitingToReadSem)
    {
      bbdSuspend ("Can't create BBD semaphores!\n");
    }

  return id;
}

/*------------------------------------------------------------------------*/
/* Delete a writer struct NOT on the list of bbd's. */
LOCAL BOOL
bbdDeleteWriterStruct (BBD_WRITER * id)
{
  BOOL stat;
  if (id == NULL)
    return ERROR;
  DEBUG_FREE ((char *) id->buf);
  stat = semDelete (id->sem) &&
    semDelete (id->waitingToWriteSem) && semDelete (id->waitingToReadSem);

  DEBUG_FREE ((char *) id);	/* do this last */
  return stat;
}


/*------------------------------------------------------------------------*/
LOCAL BBD_READER *
bbdCreateReaderStruct (char *bbdName, BBD_TYPE bbdType, int bufSize,
		       BBD_WRITER * bbd)
{
  BBD_READER *id;
  /* check that reader's buffer is same size */
  if (bufSize != bbd->size)
    bbdSuspend ("Can't attach bbd reader to \"%s\".\n"
		"Wrong buffer size %d. Must be %d\n",
		bbd->name, bufSize, bbd->size);

  /* Create reader struct */
  if (NULL == (id = (BBD_READER *) DEBUG_MALLOC (sizeof (BBD_READER))))
    bbdSuspend ("Can't allocate memory for reader BBD!\n");

  id->type = bbdType;
  id->bbd = bbd;
  id->readCnt = 0;
  id->taskId = taskIdSelf ();

  bbdLock (bbd);
  {
    /* add reader to start of reader list */
    id->next = bbd->readers;
    bbd->readers = id;
  }
  bbdUnlock (bbd);
  return id;
}

/*------------------------------------------------------------------------*/
/* returns writer id of BBD with matching name, NULL if no match */
LOCAL BBD_WRITER *
bbdFindWriter (char *name)
{
  BBD_WRITER *tId;
  for (tId = bbdList; tId != NULL; tId = tId->next)
    {
      if (0 == strncmp (tId->name, name, BBD_NAME_SIZE - 1))
	break;			/* we have a match */
    }
  return tId;
}

/*----------------------------------------------------------------------*/
/* Links BBD to the bbdList.  This first checks to make sure another task
 * has not linked another BBD with the same name.  If this has happened,
 * the routine connects to the existing BBD.  Returns the id of the final
 * bbd.
 */
LOCAL BBD_WRITER *
bbdLinkWriter (BBD_WRITER * id)
{
  BBD_WRITER *tId;
  /* add bbd to start of list */
  bbdLock (BBD_MASTER);
  {
    /*
     * make sure no one added a bbd with the same name while we weren't
     * locked
     */
    if (NULL == (tId = bbdFindWriter (id->name)))
      {
	id->next = bbdList;	/* None there, add ours to the list */
	bbdList = id;
      }
  }
  bbdUnlock (BBD_MASTER);
  if (tId != NULL)
    {
      /* Another task created the BBD, connect to it, and delete our BBD */
      bbdConnectWriter (tId, id->type, id->size);
      (void) bbdDeleteWriterStruct (id);
      id = tId;
    }
  return id;
}

/*------------------------------------------------------------------------*/
/* Connect to an existing BBD. */
LOCAL BBD_WRITER *
bbdConnectWriter (BBD_WRITER * id, BBD_TYPE type, int bufSize)
{
  BBD_READER *readId;
  /* Make sure bbd exists. */
  if (NULL == id)
    bbdSuspend ("Cannot connect to NULL bbd.\n");

  /* can't change type to multiwriter.  No way to inform original task */
  /*
   * So ??  I don't know why I protected against this.
   *
   * if (id->type == BBD_SOLE_WRITER && type == BBD_MULT_WRITER)
   * bbdSuspend("Can't change BBD type to multiwriter.\n");
   */

  /* Don't change size */
  if (id->size != bufSize)
    bbdSuspend ("Can't attach bbd writter to \"%s\".\n"
		"Wrong buffer size %d. Must be %d\n",
		id->name, bufSize, id->size);

  /* Looks good, update bbd for new writer */
  bbdLock (id);
  {
    id->type = type;
    id->taskId = taskIdSelf ();
    if (type == BBD_SOLE_WRITER)
      {
	/*
	 * New sole_writer.  Make readers that want to wait for fresh data wait
	 * on the new writer.  Flush all read buffers and reset semaphores
	 */
	for (readId = id->readers; readId != NULL; readId = readId->next)
	  {
	    /* make reader think he has already read the existing data buf */
	    readId->readCnt = id->writeCnt;
	  }
	id->readCnt = id->writeCnt;
	/*
	 * Free all that want to write so that they can see that a new
	 * sole_writer has control.
	 */
	semFlush (id->waitingToWriteSem);
      }
  }
  bbdUnlock (id);
  return id;
}

/*------------------------------------------------------------------------*/
/* returns id of BBD, read or write.  Creates one if it doesn't exist */
BBD
bbdConnect (char *bbdName, BBD_TYPE bbdType, int bufSize)
{
  BBD id;
  BBD_WRITER *wId;
  switch (bbdType)
    {
    case BBD_SOLE_WRITER:
    case BBD_MULT_WRITER:
      if (NULL == (wId = bbdFindWriter (bbdName)))
	{
	  /* write bbd does not exist, create one and attach it to list. */
	  wId = bbdCreateWriterStruct (bbdName, bbdType, bufSize);
	  wId = bbdLinkWriter (wId);
	}
      else
	{
	  /* Connect to BBD */
	  bbdConnectWriter (wId, bbdType, bufSize);
	}
      id = (BBD) wId;
      break;
    case BBD_MULT_READER:
      if (NULL == (wId = bbdFindWriter (bbdName)))
	{
	  /* bbd writer does not exist, create one and attach it to list.
	     The following comment is old thinking:
	     Type BBD_MULT_WRITER can be changed to BBD_SOLE_WRITER
	     by writer if needed.  But can't go the other way. */
	  wId = bbdCreateWriterStruct (bbdName, BBD_MULT_WRITER, bufSize);
	  wId = bbdLinkWriter (wId);
	}
      id = (BBD) bbdCreateReaderStruct (bbdName, bbdType, bufSize, wId);
      break;
    default:
      bbdSuspend ("Can't connect.  Unknown BBD type.\n");
    }
  return id;
}

/*------------------------------------------------------------------------*/
/* deleat bbd reader */
STATUS
bbdDelete (BBD id)
{
  STATUS stat;
  BBD_WRITER *wId;
  BBD_READER *rId;

  if (NULL == id)
    return ERROR;

  switch (*(BBD_TYPE *) id)	/* bbd type */
    {
    case BBD_MULT_READER:
      stat = ERROR;
      wId = ((BBD_READER *) id)->bbd;
      /* Try to remove id from reader list */
      bbdLock (wId);
      {
	/* Is it the first reader on the list */
	if (wId->readers == (BBD_READER *) id)
	  {
	    wId->readers = wId->readers->next;	/* remove it */
	    stat = OK;
	  }
	else
	  {
	    /* is id on the list */
	    for (rId = wId->readers; rId != NULL; rId = rId->next)
	      {
		if (rId->next == (BBD_READER *) id)
		  {
		    /* found reader one up on the reader list.  Remove id */
		    rId->next = ((BBD_READER *) id)->next;
		    stat = OK;
		    break;
		  }
	      }
	  }
      }
      bbdUnlock (wId);
      if (OK == stat)
	{
	  /* clear the type */
	  (*(BBD_TYPE *) id) = BBD_NULL;
	  /* try to free memory */
	  DEBUG_FREE ((char *) id);
	}
      break;
    default:
      stat = ERROR;
      break;
    }
  return stat;
}


/**************************************************************************/
/* READ and WRITE functions. */

/*-----------------------------------------------------------------------*/
/*
 * bbdClear.
 *
 * For readers, like a read with no wait and no data transfer. Used to make sure
 * subsequent read with the fresh option get data writen after the flush.
 *
 * For writers: If sole_writer: flush old buffer and make it look like no one
 * has ever writen to the bbd.   All readers (plain, fresh, or spy) will
 * block or return ERROR until the writer puts data into the buffer.  If
 * multi_writer:  make it look like someone just read.
 *
 * Returns OK or ERROR if bad id;
 */

STATUS
bbdClear (BBD id)
{
  STATUS stat;
  BBD_WRITER *wId;
  BBD_READER *rId;

  if (NULL == id)
    return ERROR;

  switch (*(BBD_TYPE *) id)	/* bbd type */
    {
    case BBD_MULT_READER:
      stat = OK;
      wId = ((BBD_READER *) id)->bbd;
      bbdLock (wId);
      {
	/* Update message cnt to LOOK like we read the current data */
	((BBD_READER *) id)->readCnt = wId->readCnt = wId->writeCnt;
	semGive (wId->waitingToWriteSem);
      }
      bbdUnlock (wId);
      break;
    case BBD_MULT_WRITER:
      stat = OK;
      wId = (BBD_WRITER *) id;
      bbdLock (wId);
      {
	/* Update message cnt to LOOK like someone just read the current data */
	wId->readCnt = wId->writeCnt;
	semGive (wId->waitingToWriteSem);
      }
      bbdUnlock (wId);
      break;

    case BBD_SOLE_WRITER:
      stat = OK;
      wId = (BBD_WRITER *) id;
      bbdLock (wId);
      {
	if (wId->taskId != taskIdSelf ())
	  {
	    /* can't flush someone else's bbd */
	    stat = ERROR;
	  }
	else
	  {
	    /* Update bbd to look like no data has ever been writen */
	    wId->readCnt = wId->writeCnt = 0;
	    for (rId = wId->readers; rId != NULL; rId = rId->next)
	      {
		rId->readCnt = 0;
	      }
	    semGive (wId->waitingToWriteSem);
	  }
      }
      bbdUnlock (wId);
      break;

    default:
      stat = ERROR;
      break;
    }
  return stat;
}

/*-----------------------------------------------------------------------*/
/*
 * Read buffer.
 *
 * If no data in buf, wait timeout for data
 *
 * Returns OK or ERROR if timeout or bad id type;
 */

STATUS
bbdRead (BBD id, void *buf, BBD_READ_TYPE type, int timeout)
{
  STATUS stat;
  BBD_WRITER *wId;
  BBD_READER *rId;

  if (NULL == id)
    return ERROR;

  switch (*(BBD_TYPE *) id)	/* bbd type */
    {
    case BBD_MULT_READER:
      rId = (BBD_READER *) id;
      wId = rId->bbd;
      stat = OK;
      /*
       * Is data available?  TaskLock to prevent a writer from writing after I
       * check the counters but before I do a semTake.  If a writer writes after
       * the taskUnlock but before the bbdLock, that's ok I'll get even fresher
       * data. The writer MUST first copy the data, then set the counters and
       * then do a semFlush.
       */
      taskLock ();		/* Be carefull with this. */
      {
	if (wId->writeCnt == 0)
	  {
	    /* Wait till there is some data in the buffer */
	    stat = semTake (wId->waitingToReadSem, timeout);
	  }
	else if (type == BBD_FRESH_READ && wId->writeCnt <= rId->readCnt)
	  {
	    /* Wait till someone writes new data */
	    stat = semTake (wId->waitingToReadSem, timeout);
	  }
      }
      taskUnlock ();
      if (stat == OK)
	{
	  bbdLock (wId);
	  {
	    bcopy (wId->buf, buf, wId->size);	/* Copy data */
	    /* If not spying, update message cnt */
	    if (type != BBD_SPY_READ)
	      {
		wId->readCnt = wId->writeCnt;
		rId->readCnt = wId->writeCnt;
		semGive (wId->waitingToWriteSem);	/* don't flush */
	      }
	  }
	  bbdUnlock (wId);
	}
      break;
    default:
      stat = ERROR;
      break;
    }
  return stat;
}



/*-----------------------------------------------------------------------*/
/*
 * Write buffer.
 * Returns OK or ERROR if sole writer and not the current owner.
 */

LOCAL STATUS
bbdPlainWrite (BBD_WRITER * id, void *buf)
{
  STATUS stat;

  stat = OK;
  bbdLock (id);
  {
    if (BBD_SOLE_WRITER == id->type && taskIdSelf () != id->taskId)
      {
	/* Not the owner. */
	stat = ERROR;
	if (id->writeCnt <= id->readCnt)
	  {
	    /*
	     * I am not the owner and can't write.  I may have taken the
	     * semaphore that the real owner needs.  Give it back.
	     */
	    semGive (id->waitingToWriteSem);

	  }
      }
    else
      {
	/*
	 * The writer MUST first copy the data, then set the counters and
	 * then do a semFlush.  This is so the blocking read will work
	 * correctly.
	 */
	bcopy (buf, id->buf, id->size);	/* Copy data */
	/* Update message cnt */
	id->writeCnt++;
	semTake (id->waitingToWriteSem, NO_WAIT);
	semFlush (id->waitingToReadSem);	/* Flush so all can read */
      }
    bbdUnlock (id);

    return stat;
  }
}

STATUS
bbdWrite (BBD id, void *buf, BBD_WRITE_TYPE type, ...)
{
  STATUS stat;
  BBD_WRITER *wId;
  int timeout;
  va_list ap;			/* timout included on blocking write */

  if (NULL == id)
    return ERROR;

  switch (*(BBD_TYPE *) id)	/* bbd type */
    {
    case BBD_SOLE_WRITER:
    case BBD_MULT_WRITER:
      wId = (BBD_WRITER *) id;
      if (BBD_WAIT_WRITE == type)
	{
	  va_start (ap, type);
	  timeout = va_arg (ap, int);	/* get timeout from argument list */
	  /* wait till someone reads the buffer so we can write */
	  stat = semTake (wId->waitingToWriteSem, timeout);
	  if (ERROR != stat)
	    {
	      bbdLock (wId);
	      {
		if (wId->writeCnt > wId->readCnt)
		  {
		    /*
		     * someone wrote before we could bbdLock.  The code should be
		     * changed to wait for another reader to read the data.  The code
		     * would need to keep track of how long the semTake took, then
		     * bbdUnlock and continue waiting until all of timeout has been
		     * used before reporting an error.
		     */
		    stat = ERROR;
		  }
		else
		  {
		    stat = bbdPlainWrite (wId, buf);
		  }
	      }
	      bbdUnlock (wId);
	    }
	  va_end (ap);
	}
      else			/* type == BBD_PLAIN_WRITE */
	{
	  stat = bbdPlainWrite (wId, buf);
	}

      break;
    default:
      stat = ERROR;
      break;
    }
  return stat;
}

/************************************************************************/
/* BBD help commands
   void bbdHelp()         prints command summary
   void bbdShow(id,name)  prints status of bbd.  If id = 0, overall status
*************************************************************************/
char bbdHelpHelpStr[] = "\
bbdHelp                         Msg passing between tasks on one cpu\n";

LOCAL char bbdHelpString[] = "\
bbdConnect name,type,size       Connects to a bbd \n\
bbdClear   id                   Flush data buffer \n\
bbdRead    id,buf,type,timeout  Read from bbd \n\
bbdWrite   id,buf,type[,timeout]Write to bbd \n\
bbdName    id                   Returns name of bbd \n\
bbdNameToId name                Returns id of writer\n\
bbdShow    [id [,name]]         Show info on bbds. If id=0, uses name\n\
\n\
bbd types:   multi-reader = 2, sole_writer = 3, multi-writer = 4 \n\
read types:  plain = 1, only fresh data = 2, spy = 3 \n\
write types: plain = 1, blocking = 2 \n\
";

/*----------------------------------------------------------------------*/
int
bbdHelp ()
{
  printf (bbdHelpString);
  return 0;
}

/*----------------------------------------------------------------------*/
char *
bbdName (BBD id)
{
  if (NULL == id)
    return NULL;

  switch (*(BBD_TYPE *) id)	/* bbd type */
    {
    case BBD_SOLE_WRITER:
    case BBD_MULT_WRITER:
      return ((BBD_WRITER *) id)->name;
    case BBD_MULT_READER:
      return ((BBD_READER *) id)->bbd->name;
    default:
      break;
    }
  return NULL;
}

/*----------------------------------------------------------------------*/
BBD
bbdNameToId (char *name)
{
  return (BBD) bbdFindWriter (name);
}

/*----------------------------------------------------------------------*/
LOCAL void
bbdPrintTaskName (int taskId, int maxChars)
{
  int len, i;
  char *name;
  name = taskName (taskId);
  if (NULL != name)
    len = strlen (name);
  else
    {
      name = "***";		/* task does not exist */
      len = 3;
    }
  for (i = maxChars - len; i > 0; i--)
    printf (" ");
  if (maxChars < len)
    for (i = 0; i < maxChars; i++)
      printf ("%c", name[i]);	/* print spaces */
  else
    for (i = 0; i < len; i++)
      printf ("%c", name[i]);	/* print name */
}

/*----------------------------------------------------------------------*/
LOCAL STATUS
bbdShowReaders (BBD_WRITER * wId)
{
  int cnt;
  BBD_READER *rId, *r;

  if (BBD_SOLE_WRITER != wId->type && BBD_MULT_WRITER != wId->type)
    return ERROR;
  if (NULL == wId->readers)
    printf ("no readers\n----------\n");
  for (rId = wId->readers; NULL != rId;)
    {
      /* print readers */
    /** printing format:
      id   0xffff0000 0xffff0000 0xffff0000 0xffff0000 0xffff0000 0xffff0000
      task tttttttttt aaaaaaaaaa ssssssssss kkkkkkkkkk task names 1234567890
      cnt          23         23         23         23         23         23
    */
      printf ("id   ");
      for (cnt = 0, r = rId; NULL != r && cnt != 6; cnt++, r = r->next)
	printf ("%10p ", r);
      printf ("\ntask ");
      for (cnt = 0, r = rId; NULL != r && cnt != 6; cnt++, r = r->next)
	{
	  bbdPrintTaskName (r->taskId, 10);
	  printf (" ");
	}
      printf ("\ncnt  ");
      for (cnt = 0, r = rId; NULL != r && cnt != 6; cnt++, r = r->next)
	printf ("%10d ", r->readCnt);

      for (printf ("\n-----"); cnt--; printf ("-----------"));
      printf ("\n");
      rId = r;
    }
  return OK;
}

/*----------------------------------------------------------------------*/
LOCAL STATUS
bbdShowWriter (BBD_WRITER * wId)
{
  char *name, st[61];

  if (BBD_SOLE_WRITER != wId->type && BBD_MULT_WRITER != wId->type)
    return ERROR;

  if (BBD_SOLE_WRITER == wId->type)
    printf ("Sole writer:  ");
  else
    printf ("Mult writer:  ");
  strncpy (st, wId->name, 60);
  st[60] = 0;
  printf ("\"%s\"\n", st);
  printf ("bbd semaphore     (%p) is ", wId->sem);
  if (ERROR == semTake (wId->sem, NO_WAIT))
    printf ("blocked\n");
  else
    {
      printf ("clear\n");
      semGive (wId->sem);
    }
  printf ("waitingToWriteSem (%p) is ", wId->waitingToWriteSem);
  if (ERROR == semTake (wId->waitingToWriteSem, NO_WAIT))
    printf ("blocked\n");
  else
    {
      printf ("clear\n");
      semGive (wId->waitingToWriteSem);
    }
  printf ("waitingToReadSem  (%p)\n", wId->waitingToReadSem);
  printf ("Buffer:        %p\n", wId->buf);
  printf ("Size:          %d\n", wId->size);
  printf ("Owner Task:    0x%x: ", wId->taskId);
  if (NULL != (name = taskName (wId->taskId)))
    {
      printf (name);
      printf ("\n");
    }
  printf ("Write Cnt:     %d\n", wId->writeCnt);
  printf ("Read Cnt:      %d\n", wId->readCnt);
  return OK;
}


/*----------------------------------------------------------------------*/
int
bbdShow (BBD id, char *name)
{
  char st[100];
  BBD_WRITER *wId, *wId2;
  BBD_READER *rId;

  if (NULL == id && NULL != name)
    {
      id = bbdFindWriter (name);
      if (NULL == id)
	{
	  printf ("Bad name or id\n");
	  name = NULL;
	}
    }

  if (id == NULL)
    {
      printf ("Master semaphore (%p) is ", bbdMasterSem);
      if (NULL != bbdMasterSem)
	{
	  if (ERROR == semTake (bbdMasterSem, NO_WAIT))
	    printf ("blocked\n");
	  else
	    {
	      printf ("clear\n");
	      semGive (bbdMasterSem);
	    }
	}
      else
	printf ("NOT initialized\n");

      /* print bbd list backwards (in order of creation) */
      printf ("  write id|write task|name  => read tasks\n");
      for (wId = NULL; wId != bbdList; wId = wId2)
	{
	  for (wId2 = bbdList; wId2->next != wId; wId2 = wId2->next);
	  strncpy (st, wId2->name, 60);
	  st[60] = 0;
	  printf ("%10p ", wId2);
	  bbdPrintTaskName (wId2->taskId, 10);
	  printf (" | \"%s\"\n        => ", st);

	  /* print reader list forward */
	  rId = wId2->readers;
	  if (NULL == rId)
	    printf ("0");
	  else
	    {
	      for (; rId != NULL; rId = rId->next)
		{
		  if (NULL == (name = taskName (rId->taskId)))
		    printf ("*** ");
		  else
		    printf ("%s ", name);
		}
	    }
	  printf ("\n");

	}
      return 0;
    }

  switch (*(BBD_TYPE *) id)	/* bbd type */
    {
    case BBD_SOLE_WRITER:
    case BBD_MULT_WRITER:
      bbdShowWriter ((BBD_WRITER *) id);
      printf ("\n");
      bbdShowReaders ((BBD_WRITER *) id);
      break;
    case BBD_MULT_READER:
      rId = (BBD_READER *) id;
      wId = rId->bbd;
      printf ("Mult Reader:  ");
      strncpy (st, wId->name, 60);
      st[60] = 0;
      printf ("\"%s\"\n", st);
      printf ("bbd semaphore (%p) is ", wId->sem);
      if (ERROR == semTake (wId->sem, NO_WAIT))
	printf ("blocked\n");
      else
	{
	  printf ("clear\n");
	  semGive (wId->sem);
	}
      printf ("Writer id:  %p\n", wId);
      printf ("Write Cnt:  %d\n", wId->writeCnt);
      printf ("Read Cnt:   %d\n", rId->readCnt);
      if (NULL == (name = taskName (rId->taskId)))
	printf ("Task name: ***\nTask Id: 0x%x\n", rId->taskId);
      else
	printf ("Task name: %s\n", name);

      break;
    default:
      if (NULL == name)
	bbdShow (NULL, (char *) id);	/* maybe the id is a name: bbdShow "mybbd" */
      else
	bbdShow (NULL, name);
      break;
    }
  return 0;
}
