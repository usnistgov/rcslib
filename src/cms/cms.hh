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

/*************************************************************************
* File: cms.hh                                                           *
* Authors: Fred Proctor, Will Shackleford                                *
* Purpose: C++ Header file for the Communication Management System (CMS).*
*          Includes:                                                     *
*                    1. class CMS.                                       *
*                    2. class CMS_UPDATER                                *
*                    2. enum CMS_STATUS.                                 *
*                    3. enum CMSMODE.                                    *
*                    4. enum CMSBUFFERTYPE.                              *
*                    5. enum CMSPROCESSTYPE.                             *
*************************************************************************/

#ifndef CMS_HH
#define CMS_HH

/* Include Files */
#include <stddef.h>		/* size_t */

#include "cms_user.hh"
#include "cms_types.hh"	// enum CMS_STATUS, cms_symbol_lookup_function_t
#include "cms_datetime.hh"	// class CMS_DURATION,class CMS_DATETIME
#include "posemath.h"		// PM_CARTESIAN, etc ...

#ifndef __GNUC__
#ifndef __attribute__
#define __attribute__(x)
#define I_DEFINED_ATTRIBUTE
#endif
#endif

/* Enumerated Data Types */

class  CMS_DIAG_PROC_INFO;
class  CMS_DIAG_HEADER;
class  CMS_DIAGNOSTICS_INFO;

/* This structure will be placed at the beginning of every */
 /* CMS buffer. */
struct CMS_HEADER
{
public:
  CMS_HEADER() :
    was_read(0),write_id(0),in_buffer_size(0) {};

  long was_read;		/* Has the buffer been read since */
  /* the last write? */
  long write_id;		/* Id of last write. */
  long in_buffer_size;		/* How much of the buffer is currently used. */
};

class  CMS_QUEUING_HEADER
{
public:
  CMS_QUEUING_HEADER():
    head(0),tail(0),queue_length(0),end_queue_space(0),write_id(0) {};

  long head;
  long tail;
  long queue_length;
  long end_queue_space;
  long write_id;
};


/* CMS class declaration. */
extern "C" {
  struct cms_enum_info;
}

class  CMS;
class  CMS_UPDATER;
class RCS_LINKED_LIST;
class PHYSMEM_HANDLE;
class CMS_MULTIREADER_PRIORITY_QUEUE;

#ifndef TRANSFER_FROM_FUNCTION_PTR_TYPEDEFED
#define TRANSFER_FROM_FUNCTION_PTR_TYPEDEFED
// Also defined in nml.hh and cms_user.hh
typedef void (*transfer_from_function_ptr) (CMS *, void *from,
					    unsigned long from_size, void *to,
					    unsigned long to_size);
#endif


#ifndef TRANSFER_TO_FUNCTION_PTR_TYPEDEFED
#define TRANSFER_TO_FUNCTION_PTR_TYPEDEFED
// Also defined in nml.hh, and cms_user.hh
typedef void (*transfer_to_function_ptr) (CMS *, void *from, unsigned long from_size,
					  void *to, unsigned long to_size);
#endif

class CMS_TRANSFER_ALIAS
{
public:
  char name[256];
  transfer_from_function_ptr fptr;
  transfer_to_function_ptr tptr;
  void *extra_info;
  void *data;
};


/* CMS class definition. */
class CMS
{
public:
#if 0
  void *operator  new (size_t);
  void operator  delete (void *);
#endif

public:
  /* Constructors and Destructors. */
  CMS (long size);
  CMS (const char *bufline, const char *procline, int set_to_server = 0);
  virtual ~ CMS ();

  /* Simple read/write interface functions. */
  virtual enum CMS_STATUS read ();	/* Read from  buffer. */
  virtual enum CMS_STATUS write (void *user_data);	/* Write to buffer. */

  // Less useful functions.
  virtual enum CMS_STATUS clear ();	/* Clear the memory, subsequent reads return CMS_READ_OLD */
  virtual int check_if_read ();	/* Has the buffer been read recently? */
  virtual int get_msg_count ();	/* How many messages have been written? */
  
  // This function will not be supported if keep_read_count is not set.
  virtual int get_read_count ();/* How many times has the buffer been read? */

  virtual int get_is_clear ();/* is the buffer clear? */

  virtual int check_if_transfers_complete ();	/* Has DMA completed */
  virtual enum CMS_STATUS blocking_read (double _timeout);	/* Read from  buffer, wait for new data. */
  virtual enum CMS_STATUS peek ();	/* Read without setting flag. */
  virtual enum CMS_STATUS write_if_read (void *user_data);	/* Write to buffer. */
  virtual int login (const char *name, const char *passwd);
  virtual int wait_for_anything(double timeout);
  virtual int wait_for_read(double timeout);
  virtual int wait_for_clear(double timeout);
  virtual int wait_for_write(double timeout);
  virtual int wait_for_queue_length_over(int , double timeout);
  virtual int wait_for_queue_length_under(int , double timeout);
  virtual void reconnect ();
  virtual void disconnect ();
  virtual int get_queue_length ();
  virtual int get_space_available ();
  virtual int get_current_mrpq_reader_id();
  virtual int get_new_mrpq_reader_id();
  virtual void set_current_mrpq_reader_id(int);
  virtual void remove_current_mrpq_reader_id();
  virtual CMS_STATUS get_msg_start(void *,size_t);
  virtual long get_msg_type();

  virtual enum CMS_STATUS setup_subscription(double _subscription_period);
  virtual enum CMS_STATUS cancel_subscription();

  /* Protocol Defined Virtual Function Stubs. */
  virtual enum CMS_STATUS main_access (void *_local);

  /* Neutrally Encoded Buffer positioning functions. */
  void rewind ();		/* positions at beginning */
  int get_encoded_msg_size ();	/* Store last position in header.size */

  /* Buffer access control functions. */
  void set_mode (enum CMSMODE im);	/* Determine read/write mode.(check neutral) */

  /* Select a temparary updator -- This is used by the nml msg2string and string2msg functions. */
  void set_temp_updater (CMS_NEUTRAL_ENCODING_METHOD);

  /* Restore the normal update. */
  void restore_normal_updater ();

  int xmlSetStyleProperty (const char *propstring);


  /*******************************************************/
  /* CMS INTERNAL ACCESS FUNCTIONS located in cms_in.cc  */
  /*******************************************************/
  enum CMS_STATUS internal_access (PHYSMEM_HANDLE * _global, void *_local);
  enum CMS_STATUS internal_access (void *_global, long global_size, void *_local);
  enum CMS_STATUS internal_clear ();	/* Zero the global memory.  */
  int check_if_read_raw ();
  int check_if_read_encoded ();
  int get_msg_count_raw ();
  int get_msg_count_encoded ();
  int get_read_count_raw ();
  int get_read_count_encoded ();
  int get_is_clear_raw ();
  int get_is_clear_encoded ();
  enum CMS_STATUS read_raw ();	/* Read from raw buffers. */
  enum CMS_STATUS read_encoded ();	/* Read from neutrally encoded buffers. */
  enum CMS_STATUS peek_raw ();	/* Read  without setting flags. */
  enum CMS_STATUS peek_encoded ();	/* Read  without setting flags. */
  enum CMS_STATUS write_raw (void *user_data);	/* Write to raw buffers. */
  enum CMS_STATUS write_encoded ();	/* Write to neutrally encoded buffers. */
  enum CMS_STATUS write_if_read_raw (void *user_data);	/* Write if read. */
  enum CMS_STATUS write_if_read_encoded ();	/* Write if read. */
  int queue_check_if_read_raw ();
  int queue_check_if_read_encoded ();
  int queue_get_msg_count_raw ();
  int queue_get_msg_count_encoded ();
  int queue_get_read_count_raw ();
  int queue_get_read_count_encoded ();
  int queue_get_is_clear_raw ();
  int queue_get_is_clear_encoded ();
  int queue_get_queue_length_raw ();
  int queue_get_queue_length_encoded ();
  int queue_get_space_available_raw ();
  int queue_get_space_available_encoded ();
  enum CMS_STATUS queue_read_raw ();	/* Read from raw buffers. */
  enum CMS_STATUS queue_read_encoded ();	/* Read from neutral buffers. */
  enum CMS_STATUS queue_peek_raw ();	/* Read  without setting flags. */
  enum CMS_STATUS queue_peek_encoded ();	/* Read  without setting flags. */
  enum CMS_STATUS queue_write_raw (void *user_data);	/* Write to raw bufs */
  enum CMS_STATUS queue_write_encoded ();	/* Write to neutral buffers. */
  enum CMS_STATUS queue_write_if_read_raw (void *user_data);
  enum CMS_STATUS queue_write_if_read_encoded ();	/* Write if read. */
  virtual void clean_buffers ();

  /***********************************************/
  /* CMS UPDATE  FUNCTIONS located in cms_up.cc  */
  /***********************************************/
  /* Access functions for primitive C language data types */
  enum CMS_STATUS update (bool &x);
  enum CMS_STATUS update (char &x);
  enum CMS_STATUS update (unsigned char &x);
  enum CMS_STATUS update (short int &x);
  enum CMS_STATUS update (unsigned short int &x);
  enum CMS_STATUS update (int &x);
  enum CMS_STATUS update (unsigned int &x);
  enum CMS_STATUS update (long int &x);
  enum CMS_STATUS update (unsigned long int &x);
  enum CMS_STATUS update (long long int &x);
  enum CMS_STATUS update (unsigned long long int &x);
  enum CMS_STATUS update (float &x);
  enum CMS_STATUS update (double &x);
  enum CMS_STATUS update (long double &x);
  enum CMS_STATUS update (char *x, unsigned int len);
  enum CMS_STATUS update (unsigned char *x, unsigned int len);
  enum CMS_STATUS update (short *x, unsigned int len);
  enum CMS_STATUS update (unsigned short *x, unsigned int len);
  enum CMS_STATUS update (int *x, unsigned int len);
  enum CMS_STATUS update (unsigned int *x, unsigned int len);
  enum CMS_STATUS update (long *x, unsigned int len);
  enum CMS_STATUS update (unsigned long *x, unsigned int len);
  enum CMS_STATUS update (long long *x, unsigned int len);
  enum CMS_STATUS update (unsigned long long *x, unsigned int len);
  enum CMS_STATUS update (float *x, unsigned int len);
  enum CMS_STATUS update (double *x, unsigned int len);
  enum CMS_STATUS update (long double *x, unsigned int len);

  /***********************************************/
  /* CMS UPDATE  FUNCTIONS located in cms_up.cc  */
  /***********************************************/
  /* Access functions for primitive C language data types  with name arguments. */
  enum CMS_STATUS update_with_name (const char *, bool &x);
  enum CMS_STATUS update_with_name (const char *, char &x);
  enum CMS_STATUS update_with_name (const char *, unsigned char &x);
  enum CMS_STATUS update_with_name (const char *, short int &x);
  enum CMS_STATUS update_with_name (const char *, unsigned short int &x);
  enum CMS_STATUS update_with_name (const char *, int &x);
  enum CMS_STATUS update_with_name (const char *, unsigned int &x);
  enum CMS_STATUS update_with_name (const char *, long int &x);
  enum CMS_STATUS update_with_name (const char *, unsigned long int &x);
  enum CMS_STATUS update_with_name (const char *, long long int &x);
  enum CMS_STATUS update_with_name (const char *, unsigned long long int &x);
  enum CMS_STATUS update_with_name (const char *, float &x);
  enum CMS_STATUS update_with_name (const char *, double &x);
  enum CMS_STATUS update_with_name (const char *, long double &x);
  enum CMS_STATUS update_with_name (const char *, bool *x, unsigned int len);
  enum CMS_STATUS update_with_name (const char *, char *x, unsigned int len);
  enum CMS_STATUS update_with_name (const char *, unsigned char *x,
				    unsigned int len);
  enum CMS_STATUS update_with_name (const char *, short *x, unsigned int len);
  enum CMS_STATUS update_with_name (const char *, unsigned short *x,
				    unsigned int len);
  enum CMS_STATUS update_with_name (const char *, int *x, unsigned int len);
  enum CMS_STATUS update_with_name (const char *, unsigned int *x,
				    unsigned int len);
  enum CMS_STATUS update_with_name (const char *, long *x, unsigned int len);
  enum CMS_STATUS update_with_name (const char *, unsigned long *x,
				    unsigned int len);
  enum CMS_STATUS update_with_name (const char *, long long *x, unsigned int len);
  enum CMS_STATUS update_with_name (const char *, unsigned long long *x,
				    unsigned int len);
  enum CMS_STATUS update_with_name (const char *, float *x, unsigned int len);
  enum CMS_STATUS update_with_name (const char *, double *x, unsigned int len);
  enum CMS_STATUS update_with_name (const char *, long double *x,
				    unsigned int len);

  enum CMS_STATUS update_attribute_with_name (const char *, bool &x);
  enum CMS_STATUS update_attribute_with_name (const char *, char &x);
  enum CMS_STATUS update_attribute_with_name (const char *, unsigned char &x);
  enum CMS_STATUS update_attribute_with_name (const char *, short int &x);
  enum CMS_STATUS update_attribute_with_name (const char *, unsigned short int &x);
  enum CMS_STATUS update_attribute_with_name (const char *, int &x);
  enum CMS_STATUS update_attribute_with_name (const char *, unsigned int &x);
  enum CMS_STATUS update_attribute_with_name (const char *, long int &x);
  enum CMS_STATUS update_attribute_with_name (const char *, unsigned long int &x);
  enum CMS_STATUS update_attribute_with_name (const char *, float &x);
  enum CMS_STATUS update_attribute_with_name (const char *, double &x);
  enum CMS_STATUS update_attribute_with_name (const char *, long double &x);
  enum CMS_STATUS update_attribute_with_name (const char *, char *x,
					      unsigned int len);
  enum CMS_STATUS update_attribute_with_name (const char *, unsigned char *x,
					      unsigned int len);
  enum CMS_STATUS update_attribute_with_name (const char *, short *x,
					      unsigned int len);
  enum CMS_STATUS update_attribute_with_name (const char *, unsigned short *x,
					      unsigned int len);
  enum CMS_STATUS update_attribute_with_name (const char *, int *x,
					      unsigned int len);
  enum CMS_STATUS update_attribute_with_name (const char *, unsigned int *x,
					      unsigned int len);
  enum CMS_STATUS update_attribute_with_name (const char *, long *x,
					      unsigned int len);
  enum CMS_STATUS update_attribute_with_name (const char *, unsigned long *x,
					      unsigned int len);
  enum CMS_STATUS update_attribute_with_name (const char *, float *x,
					      unsigned int len);
  enum CMS_STATUS update_attribute_with_name (const char *, double *x,
					      unsigned int len);
  enum CMS_STATUS update_attribute_with_name (const char *, long double *x,
					      unsigned int len);

  enum CMS_STATUS update_dla_length_with_name (const char *, int &len);

  enum CMS_STATUS update_dla_with_name (const char *, char *x, int &len,
					int maxlen);
  enum CMS_STATUS update_dla_with_name (const char *, unsigned char *x, int &len,
					int maxlen);
  enum CMS_STATUS update_dla_with_name (const char *, short *x, int &len,
					int maxlen);
  enum CMS_STATUS update_dla_with_name (const char *, unsigned short *x, int &len,
					int maxlen);
  enum CMS_STATUS update_dla_with_name (const char *, int *x, int &len,
					int maxlen);
  enum CMS_STATUS update_dla_with_name (const char *, unsigned int *x, int &len,
					int maxlen);
  enum CMS_STATUS update_dla_with_name (const char *, long *x, int &len,
					int maxlen);
  enum CMS_STATUS update_dla_with_name (const char *, unsigned long *x, int &len,
					int maxlen);
  enum CMS_STATUS update_dla_with_name (const char *, float *x, int &len,
					int maxlen);
  enum CMS_STATUS update_dla_with_name (const char *, double *x, int &len,
					int maxlen);
  enum CMS_STATUS update_dla_with_name (const char *, long double *x, int &len,
					int maxlen);

  enum CMS_STATUS update_unbounded_length_with_name (const char *, int &len);

  enum CMS_STATUS update_unbounded_attribute_with_name (const char *, char **x, int &len,
							int &size_allocated);

  enum CMS_STATUS update_unbounded_with_name (const char *, char **x, int &len,
					      int &size_allocated);
  enum CMS_STATUS update_unbounded_with_name (const char *, unsigned char **x,
					      int &len, int &size_allocated);
  enum CMS_STATUS update_unbounded_with_name (const char *, short **x, int &len,
					      int &size_allocated);
  enum CMS_STATUS update_unbounded_with_name (const char *, unsigned short **x,
					      int &len, int &size_allocated);
  enum CMS_STATUS update_unbounded_with_name (const char *, int **x, int &len,
					      int &size_allocated);
  enum CMS_STATUS update_unbounded_with_name (const char *, unsigned int **x,
					      int &len, int &size_allocated);
  enum CMS_STATUS update_unbounded_with_name (const char *, long **x, int &len,
					      int &size_allocated);
  enum CMS_STATUS update_unbounded_with_name (const char *, unsigned long **x,
					      int &len, int &size_allocated);
  enum CMS_STATUS update_unbounded_with_name (const char *, float **x, int &len,
					      int &size_allocated);
  enum CMS_STATUS update_unbounded_with_name (const char *, double **x, int &len,
					      int &size_allocated);
  enum CMS_STATUS update_unbounded_with_name (const char *, long double **x,
					      int &len, int &size_allocated);

  int update_enumeration_with_name (const char *name,
				    int enumin, void *enumaddr,
				    const cms_enum_info * info);

  int update_union_selector_with_name (const char *name,
				       int enumin, void *enumaddr,
				       const cms_enum_info * info);

  enum CMS_STATUS beginEnumerationArray (const char *name,
					 const cms_enum_info * info,
					 unsigned int len);
  enum CMS_STATUS beginEnumerationDLA (const char *name,
				       const cms_enum_info * info, int &len,
				       int maxlen);
  enum CMS_STATUS beginEnumerationUnbounded (const char *name, int **x,
					     const cms_enum_info * info, int &len,
					     int &maxlen, size_t elsize);

  int update_enumeration_array_elem (int enumin, void *enumaddr, int elem);

  enum CMS_STATUS endEnumerationArray (const char *name,
				       const cms_enum_info * info,
				       unsigned int len);
  enum CMS_STATUS endEnumerationDLA (const char *name, const cms_enum_info * info,
				     int &len, int maxlen);
  enum CMS_STATUS endEnumerationUnbounded (const char *name, int **x,
					   const cms_enum_info * info, int &len,
					   int &maxlen, size_t elsize);

  enum CMS_STATUS beginClass (const char *, const char *);
  enum CMS_STATUS endClass (const char *, const char *);
  enum CMS_STATUS beginUnion (const char *);
  enum CMS_STATUS endUnion (const char *);
  enum CMS_STATUS beginBaseClass (const char *);
  enum CMS_STATUS endBaseClass (const char *);
  enum CMS_STATUS beginStructArrayElem (const char *, int);
  enum CMS_STATUS endStructArrayElem (const char *, int);
  enum CMS_STATUS beginStructDynamicArray (const char *, int &len, int maxlen);
  enum CMS_STATUS endStructDynamicArray (const char *, int &len, int maxlen);
  enum CMS_STATUS beginStructUnboundedArray (const char *, void **x, int &len,
					     int &size_allocated, size_t elsize);
  enum CMS_STATUS endStructUnboundedArray (const char *, void **x, int &len,
					   int &size_allocated, size_t elsize);

  enum CMS_STATUS beginClassVar (const char *);
  enum CMS_STATUS endClassVar (const char *);
  enum CMS_STATUS beginUnionVar (const char *);
  enum CMS_STATUS endUnionVar (const char *);
  enum CMS_STATUS next_update_default(const char *);

  virtual void find_max_size(const size_t *sizelist, long list_length);

  long check_type_info (long type, void *buffer, const char *nsname,
			cms_symbol_lookup_function_t symbol_lookup_function,
			const char **namelist,
			const long *idlist,
			const size_t * sizelist,
			long list_length, long max_name_length);

  void set_header_file(const char *);
  const char *get_header_file();
  void set_uses_unbounded(bool);
  bool get_uses_unbounded();

  enum CMS_STATUS update (CMS_DURATION & d);
  enum CMS_STATUS update (CMS_DURATION * d, int len);
  enum CMS_STATUS update (CMS_DATE_TIME & d);
  enum CMS_STATUS update (CMS_DATE_TIME * d, int len);
  enum CMS_STATUS update (CMS_DATE & d);
  enum CMS_STATUS update (CMS_DATE * d, int len);
  enum CMS_STATUS update (CMS_TIME & d);
  enum CMS_STATUS update (CMS_TIME * d, int len);

  enum CMS_STATUS update_with_name (const char *name, CMS_DURATION & d);
  enum CMS_STATUS update_with_name (const char *name, CMS_DURATION * d, int len);
  enum CMS_STATUS update_with_name (const char *name, CMS_DATE_TIME & d);
  enum CMS_STATUS update_with_name (const char *name, CMS_DATE_TIME * d, int len);
  enum CMS_STATUS update_with_name (const char *name, CMS_TIME & d);
  enum CMS_STATUS update_with_name (const char *name, CMS_TIME * d, int len);
  enum CMS_STATUS update_with_name (const char *name, CMS_DATE & d);
  enum CMS_STATUS update_with_name (const char *name, CMS_DATE * d, int len);


  enum CMS_STATUS update_attribute_with_name (const char *name, CMS_DURATION & d);
  enum CMS_STATUS update_attribute_with_name (const char *name, CMS_DURATION * d,
					      int len);
  enum CMS_STATUS update_attribute_with_name (const char *name, CMS_DATE_TIME & d);
  enum CMS_STATUS update_attribute_with_name (const char *name, CMS_DATE_TIME * d,
					      int len);

  enum CMS_STATUS update_attribute_with_name (const char *name, CMS_TIME & d);
  enum CMS_STATUS update_attribute_with_name (const char *name, CMS_TIME * d,
					      int len);

  enum CMS_STATUS update_attribute_with_name (const char *name, CMS_DATE & d);
  enum CMS_STATUS update_attribute_with_name (const char *name, CMS_DATE * d,
					      int len);

  /*************************************************************************
   * CMS UPDATE FUNCTIONS for POSEMATH classes, defined in cms_pm.cc       *
   ************************************************************************/
  // translation types
  enum CMS_STATUS update (PM_CARTESIAN & x);	// Cart
  enum CMS_STATUS update (PM_SPHERICAL & x);	// Sph
  enum CMS_STATUS update (PM_CYLINDRICAL & x);	// Cyl

  // rotation types
  enum CMS_STATUS update (PM_ROTATION_VECTOR & x);	// Rot
  enum CMS_STATUS update (PM_ROTATION_MATRIX & x);	// Mat
  enum CMS_STATUS update (PM_QUATERNION & x);	// Quat
  enum CMS_STATUS update (PM_EULER_ZYZ & x);	// Zyz
  enum CMS_STATUS update (PM_EULER_ZYX & x);	// Zyx
  enum CMS_STATUS update (PM_RPY & x);	// Rpy

  // pose types
  enum CMS_STATUS update (PM_XYA & x);	// Cart
  enum CMS_STATUS update (PM_POSE & x);	// Pose
  enum CMS_STATUS update (PM_HOMOGENEOUS & x);	// Hom

  // CMS UPDATE FUNCTIONS for arrays of POSEMATH types.
  // translation types
  enum CMS_STATUS update (PM_CARTESIAN * x, int n);	// Cart
  enum CMS_STATUS update (PM_SPHERICAL * x, int n);	// Sph
  enum CMS_STATUS update (PM_CYLINDRICAL * x, int n);	// Cyl

  // rotation types
  enum CMS_STATUS update (PM_ROTATION_VECTOR * x, int n);	// Rot
  enum CMS_STATUS update (PM_ROTATION_MATRIX * x, int n);	// Mat
  enum CMS_STATUS update (PM_QUATERNION * x, int n);	// Quat
  enum CMS_STATUS update (PM_EULER_ZYZ * x, int n);	// Zyz
  enum CMS_STATUS update (PM_EULER_ZYX * x, int n);	// Zyx
  enum CMS_STATUS update (PM_RPY * x, int n);	// Rpy

  // pose types
  enum CMS_STATUS update (PM_XYA * x, int n);	// Pose
  enum CMS_STATUS update (PM_POSE * x, int n);	// Pose
  enum CMS_STATUS update (PM_HOMOGENEOUS * x, int n);	// Hom



  // translation types
  enum CMS_STATUS update_with_name (const char *cname, PM_CARTESIAN & x);	// Cart
  enum CMS_STATUS update_with_name (const char *cname, PM_SPHERICAL & x);	// Sph
  enum CMS_STATUS update_with_name (const char *cname, PM_CYLINDRICAL & x);	// Cyl

  // rotation types
  enum CMS_STATUS update_with_name (const char *cname, PM_ROTATION_VECTOR & x);	// Rot
  enum CMS_STATUS update_with_name (const char *cname, PM_ROTATION_MATRIX & x);	// Mat
  enum CMS_STATUS update_with_name (const char *cname, PM_QUATERNION & x);	// Quat
  enum CMS_STATUS update_with_name (const char *cname, PM_EULER_ZYZ & x);	// Zyz
  enum CMS_STATUS update_with_name (const char *cname, PM_EULER_ZYX & x);	// Zyx
  enum CMS_STATUS update_with_name (const char *cname, PM_RPY & x);	// Rpy

  // pose types
  enum CMS_STATUS update_with_name (const char *cname, PM_POSE & x);	// Pose
  enum CMS_STATUS update_with_name (const char *cname, PM_HOMOGENEOUS & x);	// Hom

  // CMS UPDATE FUNCTIONS for arrays of POSEMATH types.
  // translation types
  enum CMS_STATUS update_with_name (const char *cname, PM_CARTESIAN * x, unsigned int len);	// Cart
  enum CMS_STATUS update_with_name (const char *cname, PM_SPHERICAL * x, unsigned int len);	// Sph
  enum CMS_STATUS update_with_name (const char *cname, PM_CYLINDRICAL * x, unsigned int len);	// Cyl

  // rotation types
  enum CMS_STATUS update_with_name (const char *cname, PM_ROTATION_VECTOR * x, unsigned int len);	// Rot
  enum CMS_STATUS update_with_name (const char *cname, PM_ROTATION_MATRIX * x, unsigned int len);	// Mat
  enum CMS_STATUS update_with_name (const char *cname, PM_QUATERNION * x, unsigned int len);	// Quat
  enum CMS_STATUS update_with_name (const char *cname, PM_EULER_ZYZ * x, unsigned int len);	// Zyz
  enum CMS_STATUS update_with_name (const char *cname, PM_EULER_ZYX * x, unsigned int len);	// Zyx
  enum CMS_STATUS update_with_name (const char *cname, PM_RPY * x, unsigned int len);	// Rpy

  // pose types
  enum CMS_STATUS update_with_name (const char *cname, PM_POSE * x, unsigned int len);	// Pose
  enum CMS_STATUS update_with_name (const char *cname, PM_HOMOGENEOUS * x,
				    unsigned int len);

  enum CMS_STATUS update_dla_with_name (const char *cname, PM_CARTESIAN * x, int &len, int maxlen);	// Cart
  enum CMS_STATUS update_dla_with_name (const char *cname, PM_SPHERICAL * x, int &len, int maxlen);	// Sph
  enum CMS_STATUS update_dla_with_name (const char *cname, PM_CYLINDRICAL * x, int &len, int maxlen);	// Cyl

  // rotation types
  enum CMS_STATUS update_dla_with_name (const char *cname, PM_ROTATION_VECTOR * x, int &len, int maxlen);	// Rot
  enum CMS_STATUS update_dla_with_name (const char *cname, PM_ROTATION_MATRIX * x, int &len, int maxlen);	// Mat
  enum CMS_STATUS update_dla_with_name (const char *cname, PM_QUATERNION * x, int &len, int maxlen);	// Quat
  enum CMS_STATUS update_dla_with_name (const char *cname, PM_EULER_ZYZ * x, int &len, int maxlen);	// Zyz
  enum CMS_STATUS update_dla_with_name (const char *cname, PM_EULER_ZYX * x, int &len, int maxlen);	// Zyx
  enum CMS_STATUS update_dla_with_name (const char *cname, PM_RPY * x, int &len, int maxlen);	// Rpy

  // pose types
  enum CMS_STATUS update_dla_with_name (const char *cname, PM_POSE * x, int &len, int maxlen);	// Pose
  enum CMS_STATUS update_dla_with_name (const char *cname, PM_HOMOGENEOUS * x,
					int &len, int maxlen);

  virtual void interrupt_operation(void);
  virtual void clear_interrupt_operation(void);
  virtual void set_leave_resource(bool);


  /* comm protocol parameters shared by all protocols */
  int consecutive_timeouts;
  struct CMS_HEADER header;	/* Information to be stored in CMS buffer. */
  class CMS_QUEUING_HEADER queuing_header; /* information for multi-slot buffers.  */
  enum CMSMODE mode;			/* This process is reading or writing? */
  long size;			/* size of cms */
  long free_space;
  long max_message_size;	/* size of cms buffer available for user */
  /* messages = size - CMS Header space */
  long max_encoded_message_size;	/* Maximum size of message after being encoded. */
  long temp_max_encoded_message_size;	/* Maximum size of message after being encoded. */
  long restore_max_encoded_message_size;	/* Maximum size of message after being encoded. */
  long guaranteed_message_space;	/* Largest size message before being encoded
					   that can be guaranteed to fit after xdr. */

  enum CMS_STATUS status;		/* Status of the last CMS access. */
  void set_cms_status (enum CMS_STATUS);	/* Catch changes in cms status.  */
  /* Buffers for local copies of global buffer. */
  void *encoded_data;		/* pointer to local copy of  encoded data */
  void *temp_encoded_data;
  void *restore_encoded_data;
  int zero_encoded_data_when_set;
  void set_encoded_data (void *, long _encoded_data_size);
  void *data;			/* pointer to local copy of data (raw)  */
  void *subdiv_data;		// pointer to current subdiv;

  /* Intersting Info Saved from the Configuration File. */
  char BufferName[CMS_CONFIG_LINELEN];
  char BufferHost[CMS_CONFIG_LINELEN];
  char ProcessName[CMS_CONFIG_LINELEN];
  char BufferLine[CMS_CONFIG_LINELEN];
  char ProcessLine[CMS_CONFIG_LINELEN];
  char ProcessHost[CMS_CONFIG_LINELEN];
  char buflineupper[CMS_CONFIG_LINELEN];
  char proclineupper[CMS_CONFIG_LINELEN];
  char PermissionString[CMS_CONFIG_LINELEN];
  char WordBuffer[CMS_CONFIG_LINELEN];
  /* so that one area can be read while the */
  /* other is written to? */
  char toggle_bit;
  int first_read_done;
  int first_write_done;
  int write_permission_flag;
  int read_permission_flag;
  unsigned long rpc_program_number;
  int http_port_number;
  int tcp_port_number;
  int stcp_port_number;
  int udp_port_number;
  int gdrs_im_port_number;
  long buffer_number;
  long total_messages_missed;
  long messages_missed_on_last_read;
  char *format_low_ptr;
  char *format_high_ptr;
  long format_size;
  int check_pointer (char * ptr, long bytes);
  int check_pointer_with_name (const char *, char * ptr, long bytes);
  enum CMS_BUFFERTYPE BufferType;
  enum CMS_PROCESSTYPE ProcessType;
  enum CMS_REMOTE_PORT_TYPE remote_port_type;
  int pointer_check_disabled;

  CMSID in_buffer_id;		/* Last id read, used to determine if new. */
  void *encoded_header;		/* pointer to local copy of encoded header */
  void *encoded_queuing_header;	/* pointer to local copy of encoded queue info */
  long encoded_header_size;	/* Dynamically determined size */
  long encoded_queuing_header_size;	/* Dynamically determined size */

  /* Header Neutral Formatting Functions. */
  int encode_header ();		/* header-> ENCODE-> encoded_header */
  int decode_header ();		/* encoded_header -> DECODE -> header */
  int encode_queuing_header ();	/* queuing_header -> encoded_queuing_header */
  int decode_queuing_header ();	/* encoded_queuing_header ->queuing_header */
  /* XDR of ASCII */
  enum CMS_NEUTRAL_ENCODING_METHOD neutral_encoding_method;
  enum CMS_NEUTRAL_ENCODING_METHOD temp_updater_encoding_method;

public:
  /* Type of internal access. */
  enum CMS_INTERNAL_ACCESS_TYPE internal_access_type;
  class PHYSMEM_HANDLE *handle_to_global_data;
  class PHYSMEM_HANDLE *dummy_handle;
  enum CMSMODE read_mode;
  enum CMSMODE write_mode;
  int read_updater_mode;
  int write_updater_mode;
  enum CMSMODE last_im;
  /* data buffer stuff */


  enum CMS_STATUS check_id (CMSID id);	/* Determine if the buffer is new. */
  enum CMS_STATUS setBufferForDiff (void *diffbuf, size_t diffbuffsize);
  friend class CMS_SERVER;
  friend class CMS_SERVER_HANDLER;
  friend class CMS_MULTIREADER_PRIORITY_QUEUE;
public:
  double timeout;
  double orig_timeout;
  double read_timeout;
  double write_timeout;
  double connect_timeout;
  long connection_number;
  long total_connections;
  class CMS_UPDATER *updater;
  class CMS_UPDATER *normal_updater;
  class CMS_UPDATER *temp_updater;
  void recompute_sizes (void);

private:
  unsigned long encode_state;	/* Store position for save, restore. */
  unsigned long decode_state;	/* Store position for save, restore. */
  void open (void);		/* Allocate memory and intialize XDR streams */
  static int number_of_cms_objects;	/* Used to decide when to initialize */
  /* and cleanup PC-NFS Toolkit DLLs */

public:
  long sizeof_message_header;	/* Used by BBD protocol to strip off */
  double blocking_timeout;
  double orig_blocking_timeout;
  int max_repeat_blocking_reads;
  double blocking_read_start;
  double min_compatible_version;
  int confirm_write;
  int disable_final_write_raw_for_dma;
  virtual const char *status_string (int);
  virtual const char *short_status_string (int);

  int total_subdivisions;
  int current_subdivision;
  long subdiv_size;
  int set_subdivision (int _subdiv);
  int get_priority(void);
  int get_default_priority(void);
  void set_priority(int);
  void set_default_priority(int);
  void reset_priority(void);

  enum CMS_BITWISE_OP_TYPE get_bitwise_op(void);
  void set_bitwise_op(CMS_BITWISE_OP_TYPE t);
  void reset_bitwise_op(void);
  CMS_STATUS check_for_dvar(const char *name,double v);


  long encoded_data_size;
  int no_unbounded;
  long temp_encoded_data_size;
  long restore_encoded_data_size;
  long enc_max_size;
  long enable_diagnostics;
  virtual class CMS_DIAG_PROC_INFO *get_diag_proc_info ();
  virtual void set_diag_proc_info (CMS_DIAG_PROC_INFO *);
  virtual void setup_diag_proc_info ();
  virtual void calculate_and_store_diag_info (PHYSMEM_HANDLE * _handle,
					      void *);
  virtual void internal_retrieve_diag_info (PHYSMEM_HANDLE * _handle, void *);
  virtual class CMS_DIAGNOSTICS_INFO *get_diagnostics_info ();
  virtual const char *getRemoteServerBufferList() { return ""; };
  int first_diag_store;
  double pre_op_total_bytes_moved;
  double time_bias;
  int skip_area;
  unsigned long half_offset;
  long half_size;
  long size_without_diagnostics;
  int disable_diag_store;
  long diag_offset;
  int last_id_side0;
  int last_id_side1;
  int use_autokey_for_connection_number;
  int update_cmd_msg_base;
  int update_cmd_msg_base_in_format;
  int update_stat_msg_base;
  int update_stat_msg_base_in_format;
  enum CMS_BITWISE_OP_TYPE bitwise_op;

  int addTransferAlias (const char *name,
			transfer_from_function_ptr fptr,
			transfer_to_function_ptr tptr, void *extra_info);
  int setTransferAlias (const char *name);

  class RCS_LINKED_LIST *transfer_alias_list;
  class CMS_TRANSFER_ALIAS *current_alias;

  enum CMS_STATUS copy_to_copybuff (void **ptr, int len);
  int setCopyBuff (void *ptr, size_t start_size, size_t max_size);

  int copymode;
  void *endofcopybuf;
  void *maxendofcopybuf;
  void *copybuff;
  size_t copybuff_size;
  void *xbase;
  void **copymode_unbounded_struct_array_xbases;
  void **copymode_unbounded_struct_array_copybuffs;
  size_t *copymode_unbounded_struct_array_copybuff_sizes;
  int copymode_unbounded_struct_array_count;
  int max_copymode_unbounded_struct_array_count;
  const char *nmltypename;
  char *nmlcfgsvr;
  class RCS_LINKED_LIST *xml_style_properties;
  int global_xml_style_properties_count;
  int  spawn_server;
  class CMS_MULTIREADER_PRIORITY_QUEUE *mrpq;
  int priority;
  int default_priority;
  int num_readers;
  int queue_length_to_wait_for;
  int read_count;
  int is_clear;
  long starting_wait_for_write_id;
  long starting_wait_for_was_read;
  long starting_wait_for_queue_length;
  long starting_wait_for_queue_head;
  long max_queue_length;
  size_t max_size_from_size_list;
  void *extra_data;
  size_t min_message_size;
  size_t message_size_add;
  size_t message_size_roundup;
  int use_ipv6;

  /* RCS_CMD_MSG, RCS_STAT_MSG stuff */
  class CMS_DIAGNOSTICS_INFO *di;
  class CMS_DIAG_PROC_INFO *dpi;

private:
  CMS (const CMS & cms);		// Don't copy me.
  CMS & operator=(const CMS &_cms);

public:
  // put all  the bools together.
  bool enable_xml_logging;
  bool enable_xml_differencing;
  bool fatal_error_occurred;
  bool add_array_indexes_to_name;
  bool unbounded_used;
  bool write_just_completed;
  bool isserver;			/* Is the process a server. */
  bool is_phantom;		/* Is this a phantom CMS channel? */ 
  bool delete_totally;
  bool is_local_master;
  bool force_raw;
  bool split_buffer;		/* Will the buffer be split into two areas */
  bool using_external_encoded_data;
  bool restore_using_external_encoded_data;
  bool neutral;			/* neutral data format in buffer */
  bool queuing_enabled;		/* queue messages in the buffer  */
  bool fast_mode;
  bool interrupting_operation;
  bool leave_resource;
  bool multireader_priority_queue_enabled;
  bool priority_set;
  void set_preserve_mrpq_reader_id(bool);
  bool preserve_mrpq_reader_id;
  bool waiting_for_queue_length_over;
  bool wait_for_initialized;
  bool blocking_support_enabled;
  bool keep_read_count;
  bool stop_when_connection_refused;
  bool max_size_from_size_list_set;
  bool searching_for_max_size_from_size_list;
  bool wait_for_master;
  bool private_server_object;
  bool fail_on_overflow;
  bool cleaning_flag;
  bool bind_proc_host;
  bool looking_for_dvar;
  bool inside_wrong_struct;
  bool dvar_found;
  bool cloned_buffer;
  bool ignore_cloned_buff;
  bool do_cloning;
  bool no_verify_buf;
  bool ignore_connect_err;
  const char *var_to_look_for;
  const char *varname_only_to_look_for;
  double dvar;
  int var_struct_to_look_for_len;
  bool  get_msg_start_only;
  size_t temp_data_size;
  unsigned char *last_local_p;
  unsigned char *last_global_p;
  unsigned char *last_global_ph_local_a;
  
  char cur_var_struct[256];
  char tbuf[256];
  unsigned char memory_align;
  bool enable_message_memory_map;
  char *message_memory_map_cP;
  char *end_message_memory_map_cP;
  size_t message_memory_map_max_size;
  
  void set_message_memory_map_size(size_t _size_needed);

  enum internal_update_var_type
    {
      cms_update_var_type_none,
      cms_update_var_type_bool,
      cms_update_var_type_uchar,
      cms_update_var_type_char,
      cms_update_var_type_ushort,
      cms_update_var_type_short,
      cms_update_var_type_uint,
      cms_update_var_type_int,
      cms_update_var_type_ulong,
      cms_update_var_type_long,
      cms_update_var_type_float,
      cms_update_var_type_double,
      cms_update_var_type_ldouble,
      cms_update_var_type_enumeration,
      cms_update_var_type_bool_array,
      cms_update_var_type_uchar_array,
      cms_update_var_type_char_array,
      cms_update_var_type_ushort_array,
      cms_update_var_type_short_array,
      cms_update_var_type_uint_array,
      cms_update_var_type_int_array,
      cms_update_var_type_ulong_array,
      cms_update_var_type_long_array,
      cms_update_var_type_float_array,
      cms_update_var_type_double_array,
      cms_update_var_type_ldouble_array,
      cms_update_var_type_enumeration_array,
      cms_update_var_type_ulong_long,
      cms_update_var_type_long_long,
      cms_update_var_type_ulong_long_array,
      cms_update_var_type_long_long_array,
      cms_update_var_type_last
    } var_type;

  bool memory_map_use_raw;
  bool max_message_size_set_on_buffer_line;
  bool skip_print_memory_message_map;
  char *memory_map_pos_offset_from;
  bool set_memory_map_pos_offset_from_to_next_var;
  long memory_map_offset;
  int var_arraylen;
  bool  checking_for_nsname_from_format_check_type_info;
  const char *nsname_from_format_check_type_info;
  const char *name_to_lookup_type_id;
  long type_id_looked_up;
  bool do_not_print_errors;
  bool do_not_print_timeout_errors;
  
#ifndef DISABLE_RCS_PRINT
  int print_error (const char * _fmt, ...) __attribute__((format (printf, 2,3 ))) ;
  int print_timeout_error (const char * _fmt, ...) __attribute__((format (printf, 2,3 ))) ;
#endif

  void find_type_id( const char **namelist,
		     const long *idlist,
		     const long list_length,
		     const long max_name_length);

  void write_memory_map_info(const char *name, 
			     char *ptr,
			     long size);

  void write_memory_map_string(long pos,
			       long size,
			       const char *name,
			       int maxlen,
			       const char *comment);

private:
  long queuing_header_offset;
  void compute_encoded_queue_free_space();
  void compute_raw_queue_free_space();
  const char *header_file_name;
  bool uses_unbounded;

};

class CMS_HOST_ALIAS_ENTRY
{
public:
  char host[64];
  char alias[64];
};

#if defined(DISABLE_RCS_PRINT) || (!defined(CMS_DERIVED_CLASS) && !defined(CMS_UPDATER_CLASS))

#ifdef __GNUC__
static inline int  __attribute__ ((const)) __attribute__ ((format (printf, 1,2))) cms_print_null_func(const char *,...) {return(0);} 
#define cms_print_error if(0) cms_print_null_func
#define cms_print_timeout_error if(0) cms_print_null_func
#else
#define cms_print_error if(0)
#define cms_print_timeout_error if(0)
#endif

#elif defined(CMS_DERIVED_CLASS) && !defined(DISABLE_RCS_PRINT)

#define cms_print_error set_print_rcs_error_info( __FILE__, __LINE__), this->print_error
#define cms_print_timeout_error set_print_rcs_error_info( __FILE__, __LINE__), this->print_timeout_error


#elif defined(CMS_UPDATER_CLASS) && !defined(DISABLE_RCS_PRINT)

#define cms_print_error set_print_rcs_error_info( __FILE__, __LINE__), cms_parent->print_error
#define cms_print_timeout_error set_print_rcs_error_info( __FILE__, __LINE__), cms_parent->print_timeout_error

#endif

extern class RCS_LINKED_LIST *cmsHostAliases;

extern char *cms_check_for_host_alias (char *in);
extern int cms_encoded_data_explosion_factor;
extern int cms_print_queue_free_space;
extern int cms_print_queue_full_messages;
extern int cms_force_http_port;
extern int cms_http_show_files;
extern int last_port_number_set;
extern enum CMS_REMOTE_PORT_TYPE last_port_type_set;


#endif /* !defined(CMS_HH) */
