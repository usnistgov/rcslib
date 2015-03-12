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

/**************************************************************************
* File: cms_up.hh
* This C++ header file defines the abstract CMS_UPDATER class that defines the
* interface used by CMS to convert between local machine-specific data
* representations and network machine-independant represantations such as XDR
*  via the derived classes of CMS_UPDATER.
***************************************************************************/


#ifndef CMS_UP_HH
#define CMS_UP_HH

#include <stddef.h> 		// size_t

#include "cms_types.hh" // enum CMS_STATUS, cms_symbol_lookup_function_t
#include "cms_datetime.hh"    // class CMS_DURATION,class CMS_DATETIME

class CMS;

extern "C" {
  struct cms_enum_info;
}

class RCS_LINKED_LIST;

enum CMS_UPDATER_MODE
{
  CMS_NO_UPDATE = 0,
  CMS_ENCODE_DATA,
  CMS_DECODE_DATA,
  CMS_ENCODE_HEADER,
  CMS_DECODE_HEADER,
  CMS_ENCODE_QUEUING_HEADER,
  CMS_DECODE_QUEUING_HEADER
};

/* Abstract CMS_UPDATER CLASS */
class CMS_UPDATER
{
public:

  /* 
     Access functions for of primitive C language data types
   */
  virtual enum CMS_STATUS update (bool &x);
  virtual enum CMS_STATUS update (char &x) = 0;
  virtual enum CMS_STATUS update (unsigned char &x) = 0;
  virtual enum CMS_STATUS update (short int &x) = 0;
  virtual enum CMS_STATUS update (unsigned short int &x) = 0;
  virtual enum CMS_STATUS update (int &x) = 0;
  virtual enum CMS_STATUS update (unsigned int &x) = 0;
  virtual enum CMS_STATUS update (long int &x) = 0;
  virtual enum CMS_STATUS update (unsigned long int &x) = 0;
  virtual enum CMS_STATUS update (long long int &x);
  virtual enum CMS_STATUS update (unsigned long long int &x);
  virtual enum CMS_STATUS update (float &x) = 0;
  virtual enum CMS_STATUS update (double &x) = 0;
  virtual enum CMS_STATUS update (long double &x) = 0;

  /* 
     Access functions for simple arrays of primitive C language data types
   */
  virtual enum CMS_STATUS update (bool *x, unsigned int len);
  virtual enum CMS_STATUS update (char *x, unsigned int len) = 0;
  virtual enum CMS_STATUS update (unsigned char *x, unsigned int len) = 0;
  virtual enum CMS_STATUS update (short *x, unsigned int len) = 0;
  virtual enum CMS_STATUS update (unsigned short *x, unsigned int len) = 0;
  virtual enum CMS_STATUS update (int *x, unsigned int len) = 0;
  virtual enum CMS_STATUS update (unsigned int *x, unsigned int len) = 0;
  virtual enum CMS_STATUS update (long *x, unsigned int len) = 0;
  virtual enum CMS_STATUS update (unsigned long *x, unsigned int len) = 0;
  virtual enum CMS_STATUS update (long long *x, unsigned int len);
  virtual enum CMS_STATUS update (unsigned long long *x, unsigned int len);
  virtual enum CMS_STATUS update (float *x, unsigned int len) = 0;
  virtual enum CMS_STATUS update (double *x, unsigned int len) = 0;
  virtual enum CMS_STATUS update (long double *x, unsigned int len) = 0;

  /* 
     Access functions for primitive C language data type 
     with name arguments. 
   */
  virtual enum CMS_STATUS update_with_name (const char *, bool &x);
  virtual enum CMS_STATUS update_with_name (const char *, char &x);
  virtual enum CMS_STATUS update_with_name (const char *, unsigned char &x);
  virtual enum CMS_STATUS update_with_name (const char *, short int &x);
  virtual enum CMS_STATUS update_with_name (const char *, unsigned short int &x);
  virtual enum CMS_STATUS update_with_name (const char *, int &x);
  virtual enum CMS_STATUS update_with_name (const char *, unsigned int &x);
  virtual enum CMS_STATUS update_with_name (const char *, long int &x);
  virtual enum CMS_STATUS update_with_name (const char *, unsigned long int &x);
  virtual enum CMS_STATUS update_with_name (const char *, long long int &x);
  virtual enum CMS_STATUS update_with_name (const char *, unsigned long long int &x);
  virtual enum CMS_STATUS update_with_name (const char *, float &x);
  virtual enum CMS_STATUS update_with_name (const char *, double &x);
  virtual enum CMS_STATUS update_with_name (const char *, long double &x);
  virtual enum CMS_STATUS update_with_name (const char *, bool *x,
				       unsigned int len);
  virtual enum CMS_STATUS update_with_name (const char *, char *x,
				       unsigned int len);
  virtual enum CMS_STATUS update_with_name (const char *, unsigned char *x,
				       unsigned int len);
  virtual enum CMS_STATUS update_with_name (const char *, short *x,
				       unsigned int len);
  virtual enum CMS_STATUS update_with_name (const char *, unsigned short *x,
				       unsigned int len);
  virtual enum CMS_STATUS update_with_name (const char *, int *x,
				       unsigned int len);
  virtual enum CMS_STATUS update_with_name (const char *, unsigned int *x,
				       unsigned int len);
  virtual enum CMS_STATUS update_with_name (const char *, long *x,
				       unsigned int len);
  virtual enum CMS_STATUS update_with_name (const char *, unsigned long *x,
				       unsigned int len);
  virtual enum CMS_STATUS update_with_name (const char *, long long *x,
				       unsigned int len);
  virtual enum CMS_STATUS update_with_name (const char *, unsigned long long *x,
				       unsigned int len);
  virtual enum CMS_STATUS update_with_name (const char *, float *x,
				       unsigned int len);
  virtual enum CMS_STATUS update_with_name (const char *, double *x,
				       unsigned int len);
  virtual enum CMS_STATUS update_with_name (const char *, long double *x,
				       unsigned int len);


  virtual enum CMS_STATUS update_dla_with_name (const char *, char *x, int &len,
					   int maxlen);
  virtual enum CMS_STATUS update_dla_with_name (const char *, unsigned char *x,
					   int &len, int maxlen);
  virtual enum CMS_STATUS update_dla_with_name (const char *, short *x, int &len,
					   int maxlen);
  virtual enum CMS_STATUS update_dla_with_name (const char *, unsigned short *x,
					   int &len, int maxlen);
  virtual enum CMS_STATUS update_dla_with_name (const char *, int *x, int &len,
					   int maxlen);
  virtual enum CMS_STATUS update_dla_with_name (const char *, unsigned int *x,
					   int &len, int maxlen);
  virtual enum CMS_STATUS update_dla_with_name (const char *, long *x, int &len,
					   int maxlen);
  virtual enum CMS_STATUS update_dla_with_name (const char *, unsigned long *x,
					   int &len, int maxlen);
  virtual enum CMS_STATUS update_dla_with_name (const char *, float *x, int &len,
					   int maxlen);
  virtual enum CMS_STATUS update_dla_with_name (const char *, double *x, int &len,
					   int maxlen);
  virtual enum CMS_STATUS update_dla_with_name (const char *, long double *x,
					   int &len, int maxlen);

  virtual enum CMS_STATUS update_dla_length_with_name (const char *, int &len);


  virtual enum CMS_STATUS update_unbounded_attribute_with_name (const char *, char **x,
						 int &len,
						 int &size_allocated);

  virtual enum CMS_STATUS update_unbounded_with_name (const char *, char **x,
						 int &len,
						 int &size_allocated);
  virtual enum CMS_STATUS update_unbounded_with_name (const char *,
						 unsigned char **x, int &len,
						 int &size_allocated);
  virtual enum CMS_STATUS update_unbounded_with_name (const char *, short **x,
						 int &len,
						 int &size_allocated);
  virtual enum CMS_STATUS update_unbounded_with_name (const char *,
						 unsigned short **x, int &len,
						 int &size_allocated);
  virtual enum CMS_STATUS update_unbounded_with_name (const char *, int **x,
						 int &len,
						 int &size_allocated);
  virtual enum CMS_STATUS update_unbounded_with_name (const char *,
						 unsigned int **x, int &len,
						 int &size_allocated);
  virtual enum CMS_STATUS update_unbounded_with_name (const char *, long **x,
						 int &len,
						 int &size_allocated);
  virtual enum CMS_STATUS update_unbounded_with_name (const char *,
						 unsigned long **x, int &len,
						 int &size_allocated);
  virtual enum CMS_STATUS update_unbounded_with_name (const char *, float **x,
						 int &len,
						 int &size_allocated);
  virtual enum CMS_STATUS update_unbounded_with_name (const char *, double **x,
						 int &len,
						 int &size_allocated);
  virtual enum CMS_STATUS update_unbounded_with_name (const char *,
						 long double **x, int &len,
						 int &size_allocated);

  virtual enum CMS_STATUS beginStructUnboundedArray (const char *, void **x,
						int &len, int &size_allocated,
						size_t elsize);
  virtual enum CMS_STATUS endStructUnboundedArray (const char *, void **x,
					      int &len, int &size_allocated,
					      size_t elsize);

  virtual enum CMS_STATUS beginEnumerationUnbounded (const char *name, int **x,
						const struct cms_enum_info * info,
						int &len, int &maxlen,
						size_t elsize);
  virtual enum CMS_STATUS endEnumerationUnbounded (const char *name, int **x,
					      const struct cms_enum_info * info,
					      int &len, int &maxlen,
					      size_t elsize);

  virtual void decode_len (const char *name, int &len);

  virtual enum CMS_STATUS update_attribute_with_name (const char *, bool &x);
  virtual enum CMS_STATUS update_attribute_with_name (const char *, char &x);
  virtual enum CMS_STATUS update_attribute_with_name (const char *,
						 unsigned char &x);
  virtual enum CMS_STATUS update_attribute_with_name (const char *, short int &x);
  virtual enum CMS_STATUS update_attribute_with_name (const char *,
						 unsigned short int &x);
  virtual enum CMS_STATUS update_attribute_with_name (const char *, int &x);
  virtual enum CMS_STATUS update_attribute_with_name (const char *,
						 unsigned int &x);
  virtual enum CMS_STATUS update_attribute_with_name (const char *, long int &x);
  virtual enum CMS_STATUS update_attribute_with_name (const char *,
						 unsigned long int &x);
  virtual enum CMS_STATUS update_attribute_with_name (const char *, float &x);
  virtual enum CMS_STATUS update_attribute_with_name (const char *, double &x);
  virtual enum CMS_STATUS update_attribute_with_name (const char *,
						 long double &x);
  virtual enum CMS_STATUS update_attribute_with_name (const char *, char *x,
						 unsigned int len);
  virtual enum CMS_STATUS update_attribute_with_name (const char *,
						 unsigned char *x,
						 unsigned int len);
  virtual enum CMS_STATUS update_attribute_with_name (const char *, short *x,
						 unsigned int len);
  virtual enum CMS_STATUS update_attribute_with_name (const char *,
						 unsigned short *x,
						 unsigned int len);
  virtual enum CMS_STATUS update_attribute_with_name (const char *, int *x,
						 unsigned int len);
  virtual enum CMS_STATUS update_attribute_with_name (const char *,
						 unsigned int *x,
						 unsigned int len);
  virtual enum CMS_STATUS update_attribute_with_name (const char *, long *x,
						 unsigned int len);
  virtual enum CMS_STATUS update_attribute_with_name (const char *,
						 unsigned long *x,
						 unsigned int len);
  virtual enum CMS_STATUS update_attribute_with_name (const char *, float *x,
						 unsigned int len);
  virtual enum CMS_STATUS update_attribute_with_name (const char *, double *x,
						 unsigned int len);
  virtual enum CMS_STATUS update_attribute_with_name (const char *, long double *x,
						 unsigned int len);

  virtual enum CMS_STATUS update_attribute_with_name (const char *name,
						 CMS_DURATION & d);
  virtual enum CMS_STATUS update_attribute_with_name (const char *name,
						 CMS_DURATION * d, int len);
  virtual enum CMS_STATUS update_attribute_with_name (const char *name,
						 CMS_DATE_TIME & d);
  virtual enum CMS_STATUS update_attribute_with_name (const char *name,
						 CMS_DATE_TIME * d, int len);

  virtual enum CMS_STATUS update_attribute_with_name (const char *name,
						 CMS_TIME & d);
  virtual enum CMS_STATUS update_attribute_with_name (const char *name,
						 CMS_TIME * d, int len);
  virtual enum CMS_STATUS update_attribute_with_name (const char *name,
						 CMS_DATE & d);
  virtual enum CMS_STATUS update_attribute_with_name (const char *name,
						 CMS_DATE * d, int len);


  virtual enum CMS_STATUS update (CMS_DURATION & d);
  virtual enum CMS_STATUS update (CMS_DURATION * d, int len);

  virtual enum CMS_STATUS update_with_name (const char *name, CMS_DURATION & d);
  virtual enum CMS_STATUS update_with_name (const char *name, CMS_DURATION * d,
				       int len);

  virtual enum CMS_STATUS update (CMS_DATE_TIME & d);
  virtual enum CMS_STATUS update (CMS_DATE_TIME * d, int len);

  virtual enum CMS_STATUS update_with_name (const char *name, CMS_DATE_TIME & d);
  virtual enum CMS_STATUS update_with_name (const char *name, CMS_DATE_TIME * d,
				       int len);


 virtual enum CMS_STATUS update (CMS_TIME & d);
  virtual enum CMS_STATUS update (CMS_TIME * d, int len);

  virtual enum CMS_STATUS update_with_name (const char *name, CMS_TIME & d);
  virtual enum CMS_STATUS update_with_name (const char *name, CMS_TIME * d,
				       int len);

 virtual enum CMS_STATUS update (CMS_DATE & d);
  virtual enum CMS_STATUS update (CMS_DATE * d, int len);

  virtual enum CMS_STATUS update_with_name (const char *name, CMS_DATE & d);
  virtual enum CMS_STATUS update_with_name (const char *name, CMS_DATE * d,
				       int len);

  virtual int update_enumeration_with_name (const char *name,
					    int enumin, void *enumaddr,
					    const struct cms_enum_info * info);


  virtual int update_union_selector_with_name (const char *name,
					       int enumin, void *enumaddr,
					       const struct cms_enum_info * info);

  virtual enum CMS_STATUS beginEnumerationArray (const char *name,
					    const struct cms_enum_info * info,
					    unsigned int len);
  virtual enum CMS_STATUS beginEnumerationDLA (const char *name,
					  const struct cms_enum_info * info,
					  int &len, int maxlen);

  virtual int update_enumeration_array_elem (int enumin, void *enumaddr,
					     int elem);

  virtual enum CMS_STATUS endEnumerationArray (const char *name,
					  const struct cms_enum_info * info,
					  unsigned int len);
  virtual enum CMS_STATUS endEnumerationDLA (const char *name,
					const struct cms_enum_info * info, int &len,
					int maxlen);

  virtual enum CMS_STATUS beginClass (const char *, const char *);
  virtual enum CMS_STATUS endClass (const char *, const char *);
  virtual enum CMS_STATUS beginBaseClass (const char *);
  virtual enum CMS_STATUS endBaseClass (const char *);

  virtual enum CMS_STATUS beginClassVar (const char *);
  virtual enum CMS_STATUS endClassVar (const char *);


  virtual enum CMS_STATUS beginUnion (const char *);
  virtual enum CMS_STATUS endUnion (const char *);
  virtual enum CMS_STATUS beginUnionVar (const char *);
  virtual enum CMS_STATUS endUnionVar (const char *);

  virtual enum CMS_STATUS beginStructArrayElem (const char *, int);
  virtual enum CMS_STATUS endStructArrayElem (const char *, int);
  virtual enum CMS_STATUS beginStructDynamicArray (const char *, int &len,
					      int maxlen);
  virtual enum CMS_STATUS endStructDynamicArray (const char *, int &len,
					    int maxlen);

  virtual enum CMS_STATUS setBufferForDiff (void *diffbuf, size_t diffbuffsize);

  virtual enum CMS_STATUS next_update_default(const char *);

  virtual long check_type_info (long type, void *buffer, const char *nsname,
				cms_symbol_lookup_function_t
				symbol_lookup_function, const char **namelist,
				const long *idlist, const size_t * sizelist,
				long list_length, long max_name_length);

  virtual int xmlSetStyleProperty (const char *propstring);

  /* Neutrally Encoded Buffer positioning functions. */
  virtual void rewind ();	/* positions at beginning */
  virtual int get_encoded_msg_size () = 0;	/* Store last position in header.size */
  virtual int set_mode (enum CMS_UPDATER_MODE);
  virtual enum CMS_UPDATER_MODE get_mode ();
  virtual void set_encoded_data (void *, long _encoded_data_size);
  virtual int get_pos();
  virtual const char *get_full_name(const char *);

protected:

  virtual void recheck_properties(void);
  virtual int check_pointer (char * ptr, long bytes);
  virtual int check_pointer_with_name (const char *name, char * ptr,long bytes);
  /**********************************************
  * Aliases to variables in the CMS parent
  * using aliases lets CMS and its CMS_UPDATER share this information
  * more conveniently and allowed the CMS_UPDATER functions to be pulled out
  * of CMS with fewer changes.
  *********************************************/
  class CMS *cms_parent;
  void *(&encoded_data);	/* pointer to local copy of encoded data */
  void *(&encoded_header);	/* pointer to local copy of encoded header */
  void *(&encoded_queuing_header);	/* pointer to local copy of encoded queue info */
  enum CMS_STATUS & status;
  long &size;
  long &encoded_header_size;	/* Dynamically determined size */
  long &encoded_queuing_header_size;	/* Dynamically determined size */
  bool &using_external_encoded_data;
  int &pointer_check_disabled;
  long &encoded_data_size;
  int &no_unbounded;
  bool &unbounded_used;
  bool &add_array_indexes_to_name;
  class RCS_LINKED_LIST *(&xml_style_properties);
  int &global_xml_style_properties_count;

  bool check_type_changed;
  long last_check_type_info_type;
  void *last_check_type_info_buffer;
  const char *last_check_type_info_nsname;
  cms_symbol_lookup_function_t last_check_type_info_symbol_lookup_function;
  const char **last_check_type_info_namelist;
  const long *last_check_type_info_idlist;
  const size_t *last_check_type_info_sizelist;
  long last_check_type_info_list_length;
  long last_check_type_info_max_name_length;
  void ***uba_list;
  long uba_list_length;
  long uba_list_size_allocated;

  void add_uba(void **);
  void remove_uba(void **);
  void free_uba_list();

public:
  bool check_type_info_called;
  const bool fail_on_overflow;

protected:
  int my_xml_style_properties_count;
  long neutral_size_factor;
  int encoding;

  enum CMS_UPDATER_MODE mode;
  const char *next_default;



  static int updater_count;
  // Constructors/destructors

  CMS_UPDATER (class CMS *, 
	       int create_encoded_data = 1,
	       long _neutral_size_factor = 4);
  virtual ~ CMS_UPDATER ();


  /* Friends */
  friend class CMS;
private:
  CMS_UPDATER(const CMS_UPDATER &);
  CMS_UPDATER &operator=(const CMS_UPDATER &);
};

// CMS_UP_HH
#endif 


