
/*
  These 68000 assembly language routines implement spinlock-style
  mutual exlusion via a 16-bit short integer, whose address is shared
  between processes.
  */

/*
  Modification History:

  16-Mar-1994  Fred Proctor removed 'xbuffer' code, since this was
  causing horrible problems with subsequent calls to other functions.
  We'll use memcpy() instead.

  14-Mar-1994  Fred Proctor copied from communication_adaprims.s, part
  of the NML protocols written by John Michaloski and others.
  */

/*
  The four semaphore operations defined in this file form the basis
  for multiprocessing data transfers. This machine-dependent code is
  written for use with the Motorola 680x0 family microprocessors. The
  semaphore rules used by the code (discussed below) allow the
  reader_writer package to be used in any system in which all other
  processors follow the same semaphore rules. This file can be compiled
  with the reader_writer.c file to create a complete reader_writer for
  680x0 family processors. The x_buffer procedure breaks the data
  transfer into the largest possible number of 32-bit transfers, sending
  bytes until a long boundary is reached.

  OBJECTS DEFINED

  SEMAPHORE

  A 16-bit integer (short) which is used to control access to a
  resource, see COMMENTS below.

  OPERATIONS

  Increment_read_status
  Increment a semaphore to acquire a buffer for reading.

  Decrement_read_status
  Decrement a previously acquired semaphore when reading is complete.

  Decrement_write_status
  Set a semaphore to -1 to lock it for writing.

  Increment_write_status
  Release a semaphore from -1 when writing is complete.

  x_buffer
  Routine to efficiently transfer a block of data from one memory location to
  another. This can be used for user transfers.

  COMMENTS

  Data buffers are protected in a multiprocessor environment by numeric
  semaphores. A semaphore indicates when a data data buffer is being
  accessed by its numeric value. It is the reponsibility of the process
  accessing the data to properly check and update the semaphore when
  accessing data.  For a given data buffer, only one process can access
  the buffer while it is being written. However, multiple readers are
  allowed on the buffer when there is no writer.

  Semaphore Rules:

  A positive semaphore means that data is available to be read. The
  value of a positive semaphore indicates the number of readers
  currently active.  A semaphore value of zero indicates that no readers
  or writers are currently accessing the data buffer. A reader or a
  writer may gain access when the semaphore is zero.  A negative
  semaphore indicates that either the data or the semaphore itself is
  being modified.  Processes that encounter a negative semaphore (set
  sign bit) should not attempt to access or modify the semaphore or the
  data.  A process must wait until a semaphore is non|negative before
  taking any action on the semaphore. The only process allowed to modify
  a negative semaphore is the process that made it negative.  Only one
  writer is allowed on the data at a time. A process seeking to write
  data must determine that the semaphore is zero, and set it to a
  negative value (typically -1) during the write operation, and set it
  back to zero when the operation is complete.  A process seeking to
  read data must determine that the semaphore is non-negative, increment
  it, read the data, and then decrement the semaphore.  All semaphore
  accesses must be indivisible. This means that the access must be
  completed in a single operation such that no other process can
  interfere with the semaphore between the start of the operation and
  the end of the operation. Typically, this capability is provided by
  special machine instructions such as TAS for the 680x0 family
  processors, or LOCK BTS for Intel 80x86 family processors. (Note that
  this means that semaphores may take on negative values when there are
  no writers. A process seeking access to a semaphore can set the sign
  bit of the semaphore in a test-and-set operation. Any process which
  sets a sign bit in this manner is responsible for returning the
  semaphore to an uncorrupted state. No other process will be able to
  modify the semaphore until this occurs.)  Given the above rules for
  semaphores, processes reading and writing data require four routines
  for securely modifying semaphores, acquire_read_access,
  release_read_access, acquire_write_access, release_write_access.
  These functions are defined in the appropriate high-level language. In
  this file, the machine code to perform the actual indivisible
  operations on the semaphore is provided. The Motorola 680x0 TAS (Test
  and Set) instruction is used to provide secure, uninterrupted
  modification of the semaphore. The machine code procedures defined
  here return 0 to indicate success (rather than non-zero TRUE value).
  This is to allow for signaling of different failure modes, only some
  of which are fatal. The return value is returned in the D0 register
  which is normally used to indicate the return value of an
  integer-valued function.
  */

/*
  increment semaphore to indicate reader
  function returns:
  0 => success, semaphore incremented
  1 => failure, semaphore in transition, writer, or too many readers
  */

.globl  _increment_read_status

  _increment_read_status:

  movl  sp@(0x4),a0             /* move semaphore address into a0 */
  moveq   #1,d0                 /* set d0 to indicate failure on return */
  tas   a0@                     /* test and set the indirect address in a0 */
  bmi   inc_read_done           /* if negative, someone else is modifying
                                   semaphore */
  movw  a0@,d1                  /* otherwise, semaphore is now owned by this
                                   routine */
  bclr  #15,d1                  /* reset tas bit in register copy of
                                   semaphore */
  cmpw  #0x7ff0,d1              /* too many readers? (limited by 16-bit
                                   semaphore size) */
  bge   inc_read_free           /* if so, give up semaphore, and try later */
  addqw #1,d1                   /* else increment reader count (positive) */
  moveq #0,d0                   /* now will return 0 (FALSE) for success */

 inc_read_free:
  movw  d1,a0@                  /* release semaphore for others */

 inc_read_done:
  rts

/*
  decrement semaphore to free reader
  function returns:
  0 => semaphore decremented
  1 => semaphore in transition
  -1 => semaphore has invalid value
  */

  .globl _decrement_read_status

  _decrement_read_status:

  movl  sp@(0x4),a0             /* move semaphore address into a0 */
  moveq   #1,d0                 /* set d0 to indicate failure on return */
  tas   a0@                     /* test and set the indirect address in a0 */
  bmi   dec_read_done           /* if negative, someone else is modifying
                                   semaphore */
  movw  a0@,d1                  /* otherwise, semaphore is owned, and ready
                                   for operation */
  bclr  #15,d1                  /* reset tas bit in register copy of
                                   semaphore */

  cmpw  #0,d1                   /* already zero? */
  beq   dec_read_error          /* if so, error somewhere */

  subqw #1,d1                   /* else decrement reader count (positive) */
  movw  d1,a0@                  /* writeout new semaphore value with tas
                                   bit reset */
  moveq #0,d0                   /* return 0 for success */
  bra   dec_read_done

 dec_read_error:
  movw  d1,a0@                  /* unlock semaphore */
  moveq   #-1,d0                /* signal invalid semaphore */

 dec_read_done:
  rts

/*
  decrement semaphore to signal writer
  function returns: 0 => success, write permission obtained
                    1 => failure, other readers or writer present
  */

  .globl _decrement_write_status

  _decrement_write_status:

  movl  sp@(0x4),a0             /* move semaphore address into a0 */
  moveq   #1,d0                 /* set d0 to indicate failure initially */
  tas   a0@                     /* test and set the indirect address in a0 */
  bmi   dec_write_done          /* if negative, fail, semaphore already
                                   owned */
  movw  a0@,d1                  /* otherwise, semaphore obtained for use */
  bclr  #15,d1                  /* reset tas bit in d1 copy of semaphore */
  cmpw  #0,d1                   /* semaphore zero? */
  bne   dec_write_free          /* if not, someone in the middle of reading */
  subqw #1,d1                   /* else decrement to indicate writer
                                   count (-1) which secures the mutual
                                   exclusion */
  moveq #0,d0                   /* return 0 for success */

 dec_write_free:
  movw  d1,a0@                  /* writeout semaphore value */

 dec_write_done:
  rts

/*
  increment semaphore when writer is done
  function returns: 0 => success, semaphore reset to zero
  1 => fatal error, invalid semaphore
  */

  .globl _increment_write_status

  _increment_write_status:

  movl  sp@(0x4),a0             /* move semaphore address into a0 */
  moveq   #1,d0                 /* set d0 to indicate failure initially */
  tas   a0@                     /* test and set the indirect address in a0 */
  bpl   inc_write_done          /* should never be positive here */
  movw  a0@,d1                  /* otherwise, semaphore should be -1 for
                                   this writer */
  cmpw  #-1,d1                  /* semaphore -1? */
  bne   inc_write_done          /* if not, fatal error!  */

  addqw #1,d1                   /* else increment writer count (to zero) */
  movw  d1,a0@                  /* reset semaphore value and with tas
                                   bit reset */
  moveq #0,d0                   /* return 0 for success */

 inc_write_done:
  rts
