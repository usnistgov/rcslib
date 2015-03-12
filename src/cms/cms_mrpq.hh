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

#ifndef CMS_MRPQ_HH
#define CMS_MRPQ_HH

#include <stddef.h>		// size_t
#include "cms_types.hh"		// enum CMS_STATUS

class PHYSMEM_HANDLE;
class CMS;
class CMS_MRPQ_BLOCK_LIST_NODE;


class CMS_MULTIREADER_PRIORITY_QUEUE
{
public:
  CMS_MULTIREADER_PRIORITY_QUEUE(class CMS *_cms,
				 bool _initialize);

  ~CMS_MULTIREADER_PRIORITY_QUEUE();
  
  enum CMS_STATUS internal_access(class PHYSMEM_HANDLE *,
				  void *,
				  enum CMS_INTERNAL_ACCESS_TYPE,
				  int priority);

  int get_current_id(void);
  void set_pending_id(int);
  void set_preserve_mrpq_reader_id(bool);

protected:

  int get_new_id(void);
  void set_current_id(int);
  void remove_id(int);
  void remove_current_id(void);
  
  int check_if_read_raw (void);
  int check_if_read_encoded (void);
  int get_msg_count_raw (void);
  int get_msg_count_encoded (void);
  int get_space_available_raw(void);
  int clear(void);

  enum CMS_STATUS read_raw (void);	/* Read from raw buffers. */
  enum CMS_STATUS read_encoded (void);	/* Read from neutrally encoded buffers. */
  enum CMS_STATUS peek_raw (void);	/* Read  without setting flags. */
  enum CMS_STATUS peek_encoded (void);	/* Read  without setting flags. */
  enum CMS_STATUS write_raw (void *user_data);	/* Write to raw buffers. */
  enum CMS_STATUS write_encoded (void);	/* Write to neutrally encoded buffers. */
  enum CMS_STATUS write_if_read_raw (void *user_data);	/* Write if read. */
  enum CMS_STATUS write_if_read_encoded (void);	/* Write if read. */
  enum CMS_STATUS status;

  void print_node(void);
  void update_block_usage(void);
  void mark_area(long _offset, size_t _sz);
  long allocate_area(size_t sz);
  void mark_block_for_node(void);
  void remove_node(void);

  class CMS_MRPQ_BLOCK_LIST_NODE *node;
  class CMS_MRPQ_BLOCK_LIST_NODE *last_node;
  class CMS_MRPQ_BLOCK_LIST_NODE *next_node;
  long my_reader_byte_offset;
  unsigned char my_reader_byte_mask;
  size_t size;
  int num_readers;
  bool i_am_a_reader;
  bool neutral;
  class CMS *cms;
  int bytes_in_readers_mask;
  unsigned char *readers_mask;
  unsigned char *base_readers_mask;
  int bytes_in_block_usage;
  unsigned char *block_usage;
  PHYSMEM_HANDLE *ph;
  int priority;
  int reader_id;
  int pending_reader_id;
  long block_usage_start;
  long block_usage_end;
  bool preserve_node;
  bool no_break_when_new;
  bool preserve_mrpq_reader_id;

private:
  CMS_MULTIREADER_PRIORITY_QUEUE(const CMS_MULTIREADER_PRIORITY_QUEUE &);
  CMS_MULTIREADER_PRIORITY_QUEUE &operator=(const CMS_MULTIREADER_PRIORITY_QUEUE &);
};


#endif



