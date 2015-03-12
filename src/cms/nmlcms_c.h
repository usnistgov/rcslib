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

#ifndef NMLCMS_C_h
#define NMLCMS_C_h

#include <stddef.h>
#include "cms_enum_info.h" 	/* struct cms_enum_info; */

#ifdef __cplusplus
extern "C" {
#endif

  struct nml_c_struct;
  struct cms_c_struct;

  typedef struct nml_c_struct *nml_c_t;

  
  typedef int (*nml_c_format_func_type)(long t, void *, struct cms_c_struct *);


  extern nml_c_t nml_new(nml_c_format_func_type _f_ptr,
				      const char *_buffer_name,
				      const char *_process_name,
				      const char *_configuration_file);

  typedef long nmltype_c_t;

  extern void nml_set_to_server_c(void);
  extern void nml_set_msg_base_offset(nml_c_t,int);


  extern int nml_valid(nml_c_t);

  extern int nml_write(nml_c_t, void  *, nmltype_c_t _t, size_t _s );
  extern int nml_write_if_read(nml_c_t, void  *, nmltype_c_t _t, size_t _s );
  extern int nml_check_if_read(nml_c_t);
  extern int nml_get_msg_count(nml_c_t);
  extern int nml_get_space_available(nml_c_t);
  extern int nml_get_queue_length(nml_c_t);
  extern int nml_reset(nml_c_t);

  extern int nml_xml_schema_save_as(nml_c_t, const char *filename);
  extern int nml_xml_msg_save_as(nml_c_t, void  *, nmltype_c_t _t, size_t _s, const char *filename);
  extern nmltype_c_t  nml_xml_msg_read_from_file(nml_c_t, const char *filename);

  extern nmltype_c_t nml_blocking_read(nml_c_t, double _timeout);
  extern nmltype_c_t nml_read(nml_c_t);
  extern nmltype_c_t nml_peek(nml_c_t);
  extern void *nml_get_address(nml_c_t);
  extern void nml_free(nml_c_t);

  extern int nml_get_last_error_type(nml_c_t);
  extern void nml_set_print_destination_c(int);
  extern void nml_set_print_file_c(const char *);
  extern void nml_debug_on_c(void);
  extern void nml_debug_off_c(void);

  extern void nml_cms_pointer_check_disable(nml_c_t);

  extern void cms_update_unsigned_char(struct cms_c_struct *,
				       const char *name,
				       unsigned char *);

  extern void cms_update_char(struct cms_c_struct *,
			      const char *name,
			      char *);

  extern void cms_update_unsigned_char_array(struct cms_c_struct *,
					     const char *name,
					     unsigned char *,
					     unsigned int len);

  extern void cms_update_char_array(struct cms_c_struct *,
				    const char *name,
				    char *,
				    unsigned int len);

  extern void cms_update_unsigned_char_dla(struct cms_c_struct *,
					   const char *name,
					   unsigned char *,
					   int *lenptr,
					   int len);

  extern void cms_update_char_dla(struct cms_c_struct *,
				  const char *name,
				  char *,
				  int *lenptr,
				  int maxlen);


  extern void cms_update_unsigned_short(struct cms_c_struct *,
				       const char *name,
				       unsigned short *);

  extern void cms_update_short(struct cms_c_struct *,
			      const char *name,
			      short *);

  extern void cms_update_unsigned_short_array(struct cms_c_struct *,
					     const char *name,
					     unsigned short *,
					     unsigned int len);

  extern void cms_update_short_array(struct cms_c_struct *,
				    const char *name,
				    short *,
				    unsigned int len);


  extern void cms_update_unsigned_short_dla(struct cms_c_struct *,
					    const char *name,
					    unsigned short *,
					    int *lenptr,
					    int maxlen);

  extern void cms_update_short_dla(struct cms_c_struct *,
				     const char *name,
				     short *,
				   int *lenptr,
				     int maxlen);


  extern void cms_update_unsigned_int(struct cms_c_struct *,
				       const char *name,
				       unsigned int *);

  extern void cms_update_int(struct cms_c_struct *,
			      const char *name,
			      int *);

  extern void cms_update_unsigned_int_array(struct cms_c_struct *,
					     const char *name,
					     unsigned int *,
					     unsigned int len);

  extern void cms_update_int_array(struct cms_c_struct *,
				    const char *name,
				    int *,
				    unsigned int len);


  extern void cms_update_unsigned_int_dla(struct cms_c_struct *,
					    const char *name,
					    unsigned int *,
					    int *lenptr,
					    int maxlen);

  extern void cms_update_int_dla(struct cms_c_struct *,
				 const char *name,
				 int *,
				 int *lenptr,
				 int maxlen);


  extern void cms_update_unsigned_long(struct cms_c_struct *,
				       const char *name,
				       unsigned long *);

  extern void cms_update_long(struct cms_c_struct *,
			      const char *name,
			      long *);


  extern void cms_update_unsigned_long_long(struct cms_c_struct *,
				       const char *name,
				       unsigned long long *);

  extern void cms_update_long_long(struct cms_c_struct *,
			      const char *name,
			      long long *);

  extern void cms_update_unsigned_long_array(struct cms_c_struct *,
					     const char *name,
					     unsigned long *,
					     unsigned int len);

  extern void cms_update_unsigned_long_long_array(struct cms_c_struct *,
					     const char *name,
					     unsigned long long *,
					     unsigned int len);

  extern void cms_update_long_array(struct cms_c_struct *,
				    const char *name,
				    long *,
				    unsigned int len);

  extern void cms_update_long_long_array(struct cms_c_struct *,
				    const char *name,
				    long long *,
				    unsigned int len);


  extern void cms_update_unsigned_long_dla(struct cms_c_struct *,
					    const char *name,
					    unsigned long *,
					    int *lenptr,
					    int maxlen);

  extern void cms_update_long_dla(struct cms_c_struct *,
				  const char *name,
				  long *,
				  int *lenptr,
				  int maxlen);

  typedef char nml_c_bool_t;

  extern void cms_update_bool(struct cms_c_struct *,
			      const char *name,
			      nml_c_bool_t *);

  extern void cms_update_bool_array(struct cms_c_struct *,
				    const char *name,
				    nml_c_bool_t *,
				    unsigned int len);

#if 0
  /* I skipped this in the C++ interface also. I do not know why
     but I am too lazy to fix it at the moment.
  extern void cms_update_bool_dla(struct cms_c_struct *,
  const char *name,
  nml_c_bool_t *,
  int *lenptr,
  int maxlen);
  */
#endif

  extern void cms_update_float(struct cms_c_struct *,
			      const char *name,
			      float *);

  extern void cms_update_float_array(struct cms_c_struct *,
				    const char *name,
				    float *,
				    unsigned int len);


  extern void cms_update_float_dla(struct cms_c_struct *,
				   const char *name,
				   float *,
				   int *lenptr,
				   int maxlen);

  extern void cms_update_double(struct cms_c_struct *,
				const char *name,
				double *);

  extern void cms_update_double_array(struct cms_c_struct *,
				      const char *name,
				      double *,
				      unsigned int len);
  

  extern void cms_update_double_dla(struct cms_c_struct *,
				    const char *name,
				    double *,
				    int *lenptr,
				    int maxlen);

  extern void cms_update_dla_length(struct cms_c_struct *,
				    const char *name,
				    int *lenptr);

  extern void cms_begin_class(struct cms_c_struct *, const char *name, const char *parent_class);
  extern void cms_end_class(struct cms_c_struct *, const char *name, const char *parent_class);
  
  extern void cms_begin_base_class(struct cms_c_struct *, const char *name);
  extern void cms_end_base_class(struct cms_c_struct *, const char *name);

  extern void cms_begin_class_var(struct cms_c_struct *, const char *name);
  extern void cms_end_class_var(struct cms_c_struct *, const char *name);

  extern void cms_begin_struct_array_elem (struct cms_c_struct *cms,const char *, int);
  extern void cms_end_struct_array_elem (struct cms_c_struct *cms,const char *, int);
  extern void cms_begin_struct_dynamic_array (struct cms_c_struct *cms,
					      const char *, int *len, int maxlen);
  extern void cms_end_struct_dynamic_array (struct cms_c_struct *cms,
					      const char *, int *len, int maxlen);

  extern void cms_begin_struct_unbounded_array (struct cms_c_struct *cms,
						const char *, void **x, int *len,
						int *size_allocated, size_t elsize);
  extern void cms_end_struct_unbounded_array (struct cms_c_struct *cms,
					      const char *, void **x, 
					      int *len,
					      int *size_allocated, 
					      size_t elsize);

  extern void cms_begin_update_cmd_msg_base(struct cms_c_struct *cms, void* x);
  extern void cms_end_update_cmd_msg_base(struct cms_c_struct *cms, void* x);

  extern void cms_begin_update_stat_msg_base(struct cms_c_struct *cms, void* x);
  extern void cms_end_update_stat_msg_base(struct cms_c_struct *cms, void* x);


  extern void cms_next_update_default(struct cms_c_struct *cms,const char *def);

  extern long cms_check_type_info (struct cms_c_struct *cms,
				   long type, void *buffer, const char *nsname,
				   cms_symbol_lookup_function_t symbol_lookup_function,
				   const char **namelist,
				   const long *idlist,
				   const size_t * sizelist,
				   long list_length, long max_name_length);

  extern void cms_set_add_array_indexes_to_name(struct cms_c_struct *cms,
						nml_c_bool_t _val);

  extern int cms_update_enumeration(struct cms_c_struct *cms,
				    const char *name,
				    int enumin, void *enumaddr,
				    const struct cms_enum_info * info);

  extern void cms_begin_enumeration_array (struct cms_c_struct *cms,
					   const char *name,
					   const struct cms_enum_info * info,
					   unsigned int len);

  extern void cms_begin_enumeration_dla(struct cms_c_struct *cms,
					 const char *name,
					 const struct cms_enum_info * info, 
					 int *len,
					 int maxlen);

  extern void cms_begin_enumeration_unbounded (struct cms_c_struct *cms,
					       const char *name, int **x,
					       const struct cms_enum_info * info, 
					       int *len,
					       int *maxlen, 
					       size_t elsize);

  extern int cms_update_enumeration_array_elem (struct cms_c_struct *cms,
						int enumin, 
						void *enumaddr, 
						int elem);

  extern void cms_end_enumeration_array (struct cms_c_struct *cms,
					   const char *name,
					   const struct cms_enum_info * info,
					   unsigned int len);

  extern void cms_end_enumeration_dla (struct cms_c_struct *cms,
				       const char *name,
				       const struct cms_enum_info * info, 
				       int *len,
				       int maxlen);

  extern void cms_end_enumeration_unbounded (struct cms_c_struct *cms,
					     const char *name, int **x,
					     const struct cms_enum_info * info, 
					     int *len,
					     int *maxlen, 
					     size_t elsize);

  extern const struct cms_enum_info *cms_create_cms_enum_info(const char *nameptr,
							      const char *namelistptr,
							      const int *int_list_ptr,
							      int max_name_len,
							      int list_len,
							      cms_symbol_lookup_function_t sl);

  extern void cms_free_cms_enum_info(struct cms_enum_info *ptr);

  extern void run_nml_servers_c(void);
  
#ifdef __cplusplus
}
#endif

#include "nmlmacros.h"
#if !defined(__cplusplus)
#include "posemath_c_n.h"
#ifndef VXWORKS
#include "timetracker_c_n.h"
#endif
#endif

/* endif NMLCMS_C_h */
#endif
