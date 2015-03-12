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


#ifndef LINKED_LIST_HH
#define LINKED_LIST_HH

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stddef.h>		// size_t

#ifdef DEBUG_MEMORY
#include "dbg_mem.h"
#endif

#ifdef EXTERN_C_STD_HEADERS
}
#endif


enum IS_EMPTY
{
  LIST_EMPTY,
  LIST_NOT_EMPTY
};

enum LIST_SIZING_MODE
{
  DELETE_FROM_HEAD,
  DELETE_FROM_TAIL,
  STOP_AT_MAX,
  NO_MAXIMUM_SIZE
};

#define ENUM_LIST_SIZING_MODE_DEFINED 1

class RCS_LINKED_LIST_NODE;

class RCS_LINKED_LIST
{
protected:
  RCS_LINKED_LIST_NODE * head;
  RCS_LINKED_LIST_NODE *tail;
  RCS_LINKED_LIST_NODE *current_node;
  RCS_LINKED_LIST_NODE *extra_node;
  int next_node_id;
public:
  void *operator      new (size_t);
  void operator      delete (void *);
  int get_current_id ();
  int list_size;
  int max_list_size;
  enum LIST_SIZING_MODE sizing_mode;
  void set_list_sizing_mode (int, LIST_SIZING_MODE);
  void set_max_list_size (int);
  size_t last_size_retrieved;
  int delete_data_not_copied;
  void *last_data_retrieved;
  int last_copied_retrieved;
  void *retrieve_head ();
  void *retrieve_tail ();
  size_t last_size_stored;
  void *last_data_stored;
  int store_at_head (void *_data, size_t _size, int _copy);
  int store_at_tail (void *_data, size_t _size, int _copy);
  int store_after_current_node (void *_data, size_t _size, int _copy);
  int store_before_current_node (void *_data, size_t _size, int _copy);
  int get_newest_id ()
  {
    return (next_node_id - 1);
  }
  void *get_head ();  
  void *get_current ();
  void *get_tail ();
  void *get_next ();
  void *get_last ();

  void *get_head_with_external_current (RCS_LINKED_LIST_NODE **ext_cur);  
  void *get_current_with_external_current (RCS_LINKED_LIST_NODE **ext_cur);  
  void *get_tail_with_external_current (RCS_LINKED_LIST_NODE **ext_cur);  
  void *get_next_with_external_current (RCS_LINKED_LIST_NODE **ext_cur);  
  void *get_last_with_external_current (RCS_LINKED_LIST_NODE **ext_cur);  

  void *find_node (int _node_number);
  void delete_node (int _id);
  void delete_current_node ();
  void *get_by_id (int _id);
  void *get_first_newer (int _id);
  void *get_last_newer (int _id);
  IS_EMPTY is_empty ();
  void flush_list ();
  void delete_members ();
  RCS_LINKED_LIST ();
  ~RCS_LINKED_LIST ();

private:
  RCS_LINKED_LIST (const RCS_LINKED_LIST &);	// Don't copy me.
  RCS_LINKED_LIST &operator=(const RCS_LINKED_LIST &);	// Don't copy me.
};



#ifdef LINUXCNC_LIBNML_COMPAT 

class LinkedList : public RCS_LINKED_LIST {
public:
  LinkedList();
  ~LinkedList();
  int store_at_tail (void *_data, size_t _size, int _copy);
  void delete_members ();
  void *retrieve_head ();
  void *get_head ();  
  void *get_current ();
  void *get_tail ();
  void *get_next ();
  void *get_last ();

};

#endif  // LINUXCNC_LIBNML_COMPAT

#endif /* LINKED_LIST_HH */
