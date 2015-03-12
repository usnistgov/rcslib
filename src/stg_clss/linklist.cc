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


#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "linklist_no_config.h"
#endif

#include "linklist.hh"		/* class RCS_LINKED_LIST */
				/* class RCS_LINKED_LIST_NODE */

#include "dbg_mem.h"		// DEBUG_MALLOC,DEBUG_FREE

class RCS_LINKED_LIST_NODE
{
public:
  void *data;
  size_t size;
  int id;
  int copied;
  RCS_LINKED_LIST_NODE *next;
  RCS_LINKED_LIST_NODE *last;
  friend class LINKED_LIST;
  RCS_LINKED_LIST_NODE (void *_data, size_t _size);
  ~RCS_LINKED_LIST_NODE ();

private:
  RCS_LINKED_LIST_NODE(const RCS_LINKED_LIST_NODE &);
  RCS_LINKED_LIST_NODE &operator=(const RCS_LINKED_LIST_NODE &); 
};

void *
  RCS_LINKED_LIST::operator
new (size_t size)
{
  if (size < sizeof (RCS_LINKED_LIST))
    {
      size = sizeof (RCS_LINKED_LIST);
    }
  void *RCS_LINKED_LIST_space = 0;
  RCS_LINKED_LIST_space = DEBUG_MALLOC (size + sizeof (int) * 2);
  if (0 != RCS_LINKED_LIST_space)
    {
      memset (RCS_LINKED_LIST_space, 0, size);
    }
  return RCS_LINKED_LIST_space;
}

void
  RCS_LINKED_LIST::operator
delete (void *RCS_LINKED_LIST_space)
{
  if (0 == RCS_LINKED_LIST_space)
    {
      return;
    }
  DEBUG_FREE (RCS_LINKED_LIST_space);
}


RCS_LINKED_LIST_NODE::RCS_LINKED_LIST_NODE (void *_data, size_t _size):
    data(_data),size(_size),id(0),copied(0),next(0),last(0)
{
  data = _data;
  size = _size;
  next = (RCS_LINKED_LIST_NODE *) NULL;
  last = (RCS_LINKED_LIST_NODE *) NULL;
}

RCS_LINKED_LIST_NODE::~RCS_LINKED_LIST_NODE ()
{
}

RCS_LINKED_LIST::RCS_LINKED_LIST ():
  head(0),tail(0),current_node(0),extra_node(0),next_node_id(0),
  list_size(0),max_list_size(0),sizing_mode(NO_MAXIMUM_SIZE),
  last_size_retrieved(0),delete_data_not_copied(0),last_data_retrieved(0),
  last_copied_retrieved(0),last_size_stored(0),last_data_stored(0)
{
  head = (RCS_LINKED_LIST_NODE *) NULL;
  tail = (RCS_LINKED_LIST_NODE *) NULL;
  current_node = (RCS_LINKED_LIST_NODE *) NULL;
  extra_node = (RCS_LINKED_LIST_NODE *) NULL;
  last_data_retrieved = NULL;
  last_size_retrieved = 0;
  last_copied_retrieved = 0;
  list_size = 0;
  next_node_id = 1;
  delete_data_not_copied = 0;
  extra_node = new RCS_LINKED_LIST_NODE (NULL, 0);
  max_list_size = 0;
  sizing_mode = NO_MAXIMUM_SIZE;
}

RCS_LINKED_LIST::~RCS_LINKED_LIST ()
{
  flush_list ();
  if (NULL != extra_node)
    {
      volatile RCS_LINKED_LIST_NODE *en = extra_node;
      extra_node = (RCS_LINKED_LIST_NODE *) NULL;
      delete en;
    }
}

void
RCS_LINKED_LIST::set_list_sizing_mode (int _new_max_size,
				       LIST_SIZING_MODE _new_sizing_mode)
{
  max_list_size = _new_max_size;
  sizing_mode = _new_sizing_mode;
}

void
RCS_LINKED_LIST::flush_list ()
{
  RCS_LINKED_LIST_NODE *next_node;
  current_node = head;
  while (NULL != current_node)
    {
      next_node = current_node->next;
      if ((current_node->copied || delete_data_not_copied)
	  && (NULL != current_node->data))
	{
	   void *d= current_node->data;
	  current_node->data=0;
	  DEBUG_FREE (d);
	}
       RCS_LINKED_LIST_NODE *cn = current_node;
      current_node = next_node;
      delete cn;
    }
  if (last_copied_retrieved)
    {
      if (last_data_retrieved != NULL)
	{
	   void *d = last_data_retrieved;
	  last_data_retrieved = NULL;
	  last_size_retrieved = 0;
	  DEBUG_FREE (d);
	}
    }
  head = (RCS_LINKED_LIST_NODE *) NULL;
  tail = (RCS_LINKED_LIST_NODE *) NULL;
  list_size = 0;
  last_data_stored = NULL;
  last_size_stored = 0;
}

void
RCS_LINKED_LIST::delete_members ()
{
  int old_delete_data_not_copied = delete_data_not_copied;
  delete_data_not_copied = 1;
  flush_list ();
  delete_data_not_copied = old_delete_data_not_copied;
}

void *
RCS_LINKED_LIST::retrieve_head ()
{
  RCS_LINKED_LIST_NODE *next_node;

  if (NULL != head)
    {
      if (last_copied_retrieved)
	{
	  if (NULL != last_data_retrieved)
	    {
	       void *d = last_data_retrieved;
	      last_data_retrieved = NULL;
	      last_size_retrieved = 0;
	      DEBUG_FREE (d);
	    }
	}
      last_data_retrieved = head->data;
      last_size_retrieved = head->size;
      last_copied_retrieved = head->copied;
       RCS_LINKED_LIST_NODE *hn = head;
      next_node = head->next;
      head = next_node;
      if (NULL != head)
	{
	  head->last = (RCS_LINKED_LIST_NODE *) NULL;
	}
      else
	{
	  tail = (RCS_LINKED_LIST_NODE *) NULL;
	}
      list_size--;
      delete hn;
      return (last_data_retrieved);
    }
  return (NULL);
}

void *
RCS_LINKED_LIST::retrieve_tail ()
{
  RCS_LINKED_LIST_NODE *last_node;

  if (NULL != tail)
    {
      if (last_copied_retrieved)
	{
	  if (NULL != last_data_retrieved)
	    {
	       void *d = last_data_retrieved;
	      last_data_retrieved = NULL;
	      last_size_retrieved = 0;
	      DEBUG_FREE (d);
	    }
	}
      last_data_retrieved = tail->data;
      last_size_retrieved = tail->size;
      last_copied_retrieved = tail->copied;
      last_node = tail->last;
       RCS_LINKED_LIST_NODE *tl = tail;
      tail = last_node;
      if (NULL != tail)
	{
	  tail->next = (RCS_LINKED_LIST_NODE *) NULL;
	}
      else
	{
	  head = (RCS_LINKED_LIST_NODE *) NULL;
	}
      list_size--;
      delete tl;
      return (last_data_retrieved);
    }
  return (NULL);
}

int
RCS_LINKED_LIST::store_at_head (void *_data, size_t _size, int _copy)
{
  RCS_LINKED_LIST_NODE *new_head;
  RCS_LINKED_LIST_NODE *old_tail = tail;

  if (list_size >= max_list_size)
    {
      switch (sizing_mode)
	{
	case DELETE_FROM_TAIL:
	  if (NULL != tail)
	    {
	      tail = tail->last;
	      if (NULL != tail)
		{
		  tail->next = (RCS_LINKED_LIST_NODE *) NULL;
		}
	      else
		{
		  head = (RCS_LINKED_LIST_NODE *) NULL;
		  delete old_tail;
		  list_size = 0;
		  break;
		}
	      delete old_tail;
	      list_size--;
	    }
	  break;

	case NO_MAXIMUM_SIZE:
	  break;

	case DELETE_FROM_HEAD:
	case STOP_AT_MAX:
	default:
	  return (-1);
	}
    }

  if (_copy)
    {
      last_data_stored = DEBUG_MALLOC (_size);
      memcpy (last_data_stored, _data, _size);
      last_size_stored = _size;
      new_head = new RCS_LINKED_LIST_NODE (last_data_stored, _size);
    }
  else
    {
      last_data_stored = _data;
      last_size_stored = _size;
      new_head = new RCS_LINKED_LIST_NODE (_data, _size);
    }
  if (NULL != new_head)
    {
      new_head->copied = _copy;
      new_head->id = next_node_id++;
      if (NULL == head)
	{
	  head = new_head;
	  if (NULL != tail)
	    {
	      return (-1);
	    }
	  tail = new_head;
	}
      else
	{
	  head->last = new_head;
	  new_head->last = (RCS_LINKED_LIST_NODE *) NULL;
	  new_head->next = head;
	  head = new_head;
	}
      list_size++;
      return (head->id);
    }
  else
    {
      return (-1);
    }
}

int
RCS_LINKED_LIST::store_at_tail (void *_data, size_t _size, int _copy)
{
  RCS_LINKED_LIST_NODE *new_tail;
  RCS_LINKED_LIST_NODE *old_head = head;

  if (list_size >= max_list_size)
    {
      switch (sizing_mode)
	{
	case DELETE_FROM_HEAD:
	  if (NULL != head)
	    {
	      head = head->next;
	      if (NULL != head)
		{
		  head->last = (RCS_LINKED_LIST_NODE *) NULL;
		}
	      else
		{
		  head = (RCS_LINKED_LIST_NODE *) NULL;
		  delete old_head;
		  list_size = 0;
		  break;
		}
	      delete old_head;
	      list_size--;
	    }
	  break;

	case NO_MAXIMUM_SIZE:
	  break;

	case DELETE_FROM_TAIL:
	case STOP_AT_MAX:
	default:
#ifndef NO_STDIO
	  fprintf(stderr,"RCS_LINKED_LIST : bad sizing mode %d\n",sizing_mode);
#endif
	  return (-1);
	}
    }

  if (_copy)
    {
      last_data_stored = DEBUG_MALLOC (_size);
      memcpy (last_data_stored, _data, _size);
      last_size_stored = _size;
      new_tail = new RCS_LINKED_LIST_NODE (last_data_stored, _size);
    }
  else
    {
      last_data_stored = _data;
      last_size_stored = _size;
      new_tail = new RCS_LINKED_LIST_NODE (last_data_stored, _size);
    }
  if (NULL != new_tail)
    {
      new_tail->copied = _copy;
      new_tail->id = next_node_id++;
      if (NULL == tail)
	{
	  tail = new_tail;
	  if (NULL != head)
	    {
#ifndef NO_STDIO
	      fprintf (stderr,
		       "RCS_LINKED_LIST: Tail is NULL but head is not.\n");
#endif
	      return (-1);
	    }
	  head = new_tail;
	}
      else
	{
	  tail->next = new_tail;
	  new_tail->last = tail;
	  new_tail->next = (RCS_LINKED_LIST_NODE *) NULL;
	  tail = new_tail;
	}
      list_size++;
      return (tail->id);
    }
  else
    {
#ifndef NO_STDIO
      fprintf (stderr,
	       "RCS_LINKED_LIST: Couldn't create new node to store_at_tail.\n");
#endif
      return (-1);
    }
}

int
RCS_LINKED_LIST::store_after_current_node (void *_data, size_t _size,
					   int _copy)
{
  RCS_LINKED_LIST_NODE *new_node;
  RCS_LINKED_LIST_NODE *old_tail = tail;
  RCS_LINKED_LIST_NODE *old_head = head;

  if (list_size >= max_list_size)
    {
      switch (sizing_mode)
	{
	case DELETE_FROM_TAIL:
	  if (NULL != tail)
	    {
	      tail = tail->last;
	      if (NULL != tail)
		{
		  tail->next = (RCS_LINKED_LIST_NODE *) NULL;
		}
	      else
		{
		  head = (RCS_LINKED_LIST_NODE *) NULL;
		  delete old_tail;
		  list_size = 0;
		  break;
		}
	      delete old_tail;
	      list_size--;
	    }
	  break;

	case NO_MAXIMUM_SIZE:
	  break;

	case DELETE_FROM_HEAD:
	  if (NULL != head)
	    {
	      head = head->next;
	      if (NULL != head)
		{
		  head->last = (RCS_LINKED_LIST_NODE *) NULL;
		}
	      else
		{
		  head = (RCS_LINKED_LIST_NODE *) NULL;
		  delete old_head;
		  list_size = 0;
		  break;
		}
	      delete old_head;
	      list_size--;
	    }
	  break;
	case STOP_AT_MAX:
	default:
#ifndef NO_STDIO
	  fprintf (stderr, "RCS_LINKED_LIST: Invalid list_sizing_mode.\n");
#endif
	  return (-1);
	}
    }

  if (_copy)
    {
      last_data_stored = DEBUG_MALLOC (_size);
      memcpy (last_data_stored, _data, _size);
      last_size_stored = _size;
      new_node = new RCS_LINKED_LIST_NODE (last_data_stored, _size);
    }
  else
    {
      last_data_stored = _data;
      last_size_stored = _size;
      new_node = new RCS_LINKED_LIST_NODE (last_data_stored, _size);
    }
  if (NULL != new_node)
    {
      new_node->copied = _copy;
      new_node->id = next_node_id++;
      if (NULL == current_node)
	{
	  if (tail == NULL)
	    {
	      tail = new_node;
	      if (NULL != head)
		{
#ifndef NO_STDIO
		  fprintf (stderr,
			   "RCS_LINKED_LIST: Tail is NULL but the head is not.\n");
#endif
		  return (-1);
		}
	      head = new_node;
	    }
	  current_node = tail;
	}
      else
	{
	  new_node->next = current_node->next;
	  if (current_node == extra_node)
	    {
	      new_node->last = current_node->last;
	      if (NULL != current_node->last)
		{
		  current_node->last->next = new_node;
		}
	      else
		{
		  head = new_node;
		}
	    }
	  else
	    {
	      new_node->last = current_node;
	    }
	  current_node->next = new_node;
	  if (NULL != new_node->next)
	    {
	      new_node->next->last = new_node;
	    }
	  else
	    {
	      tail = new_node;
	    }
	}
      list_size++;
      return (new_node->id);
    }
  else
    {
#ifndef NO_STDIO
      fprintf (stderr,
	       "RCS_LINKED_LIST: Couldn't create new node to store_after_current.\n");
#endif
      return (-1);
    }
}


int
RCS_LINKED_LIST::store_before_current_node (void *_data, size_t _size,
					    int _copy)
{
  RCS_LINKED_LIST_NODE *new_node;
  RCS_LINKED_LIST_NODE *old_tail = tail;
  RCS_LINKED_LIST_NODE *old_head = head;

  if (list_size >= max_list_size)
    {
      switch (sizing_mode)
	{
	case DELETE_FROM_TAIL:
	  if (NULL != tail)
	    {
	      tail = tail->last;
	      if (NULL != tail)
		{
		  tail->next = (RCS_LINKED_LIST_NODE *) NULL;
		}
	      else
		{
		  head = (RCS_LINKED_LIST_NODE *) NULL;
		  delete old_tail;
		  list_size = 0;
		  break;
		}
	      delete old_tail;
	      list_size--;
	    }
	  break;

	case NO_MAXIMUM_SIZE:
	  break;

	case DELETE_FROM_HEAD:
	  if (NULL != head)
	    {
	      head = head->next;
	      if (NULL != head)
		{
		  head->last = (RCS_LINKED_LIST_NODE *) NULL;
		}
	      else
		{
		  head = (RCS_LINKED_LIST_NODE *) NULL;
		  delete old_head;
		  list_size = 0;
		  break;
		}
	      delete old_head;
	      list_size--;
	    }
	  break;

	case STOP_AT_MAX:
	default:
#ifndef NO_STDIO
	  fprintf (stderr, "RCS_LINKED_LIST: Invalid list_sizing_mode.\n");
#endif
	  return (-1);
	}
    }

  if (_copy)
    {
      last_data_stored = DEBUG_MALLOC (_size);
      memcpy (last_data_stored, _data, _size);
      last_size_stored = _size;
      new_node = new RCS_LINKED_LIST_NODE (last_data_stored, _size);
    }
  else
    {
      last_data_stored = _data;
      last_size_stored = _size;
      new_node = new RCS_LINKED_LIST_NODE (last_data_stored, _size);
    }
  if (NULL != new_node)
    {
      new_node->copied = _copy;
      new_node->id = next_node_id++;
      if (NULL == current_node)
	{
	  if (tail == NULL)
	    {
	      tail = new_node;
	      if (NULL != head)
		{
#ifndef NO_STDIO
		  fprintf (stderr,
			   "RCS_LINKED_LIST: Tail is NULL but head is not.\n");
#endif
		  return (-1);
		}
	      head = new_node;
	    }
	  current_node = tail;
	}
      else
	{
	  new_node->last = current_node->last;
	  if (current_node == extra_node)
	    {
	      new_node->next = current_node->next;
	      if (NULL != current_node->next)
		{
		  current_node->next->last = new_node;
		}
	      else
		{
		  tail = new_node;
		}
	    }
	  else
	    {
	      new_node->next = current_node;
	    }
	  current_node->last = new_node;
	  if (NULL != new_node->last)
	    {
	      new_node->last->next = new_node;
	    }
	  else
	    {
	      head = new_node;
	    }
	}
      list_size++;
      return (new_node->id);
    }
  else
    {
#ifndef NO_STDIO
      fprintf (stderr,
	       "RCS_LINKED_LIST: Couldn't create new node to store_before_current.\n");
#endif
      return (-1);
    }
}

void *
RCS_LINKED_LIST::get_head ()
{
  current_node = head;
  if (NULL != current_node)
    {
      return (current_node->data);
    }
  else
    {
      return (NULL);
    }
}

void *
RCS_LINKED_LIST::get_tail ()
{
  current_node = tail;
  if (NULL != current_node)
    {
      return (current_node->data);
    }
  else
    {
      return (NULL);
    }
}

void *
RCS_LINKED_LIST::get_next ()
{
  if (NULL != current_node)
    {
      current_node = current_node->next;
    }
  if (NULL != current_node)
    {
      return (current_node->data);
    }
  else
    {
      return (NULL);
    }
}

void *
RCS_LINKED_LIST::get_last ()
{
  if (NULL != current_node)
    {
      current_node = current_node->last;
    }
  if (NULL != current_node)
    {
      return (current_node->data);
    }
  else
    {
      return (NULL);
    }
}

void *
RCS_LINKED_LIST::get_current ()
{
  if (NULL != current_node)
    {
      return (current_node->data);
    }
  else
    {
      return (NULL);
    }
}


void *
RCS_LINKED_LIST::get_head_with_external_current (RCS_LINKED_LIST_NODE **ext_cur)
{
  if(!ext_cur)
    {
      // rcs_print_error("RCS_LINKED_LIST::get_head_with_external_current ext_cur is NULL\n");
      return 0;
    }
  (*ext_cur) = head;
  if (NULL != (*ext_cur))
    {
      return ((*ext_cur)->data);
    }
  else
    {
      return (NULL);
    }
}

void *
RCS_LINKED_LIST::get_tail_with_external_current (RCS_LINKED_LIST_NODE **ext_cur)
{
  if(!ext_cur)
    {
      // rcs_print_error("RCS_LINKED_LIST::get_head_with_external_current ext_cur is NULL\n");
      return 0;
    }
  (*ext_cur) = tail;
  if (NULL != (*ext_cur))
    {
      return ((*ext_cur)->data);
    }
  else
    {
      return (NULL);
    }
}

void *
RCS_LINKED_LIST::get_next_with_external_current (RCS_LINKED_LIST_NODE **ext_cur)
{
  if(!ext_cur)
    {
      // rcs_print_error("RCS_LINKED_LIST::get_head_with_external_current ext_cur is NULL\n");
      return 0;
    }
  if (NULL != (*ext_cur))
    {
      (*ext_cur) = (*ext_cur)->next;
    }
  if (NULL != (*ext_cur))
    {
      return ((*ext_cur)->data);
    }
  else
    {
      return (NULL);
    }
}

void *
RCS_LINKED_LIST::get_last_with_external_current (RCS_LINKED_LIST_NODE **ext_cur)
{
  if(!ext_cur)
    {
      // rcs_print_error("RCS_LINKED_LIST::get_head_with_external_current ext_cur is NULL\n");
      return 0;
    }
  if (NULL != (*ext_cur))
    {
      (*ext_cur) = (*ext_cur)->last;
    }
  if (NULL != (*ext_cur))
    {
      return ((*ext_cur)->data);
    }
  else
    {
      return (NULL);
    }
}

void *
RCS_LINKED_LIST::get_current_with_external_current (RCS_LINKED_LIST_NODE **ext_cur)
{
  if(!ext_cur)
    {
      // rcs_print_error("RCS_LINKED_LIST::get_head_with_external_current ext_cur is NULL\n");
      return 0;
    }
  if (NULL != (*ext_cur))
    {
      return ((*ext_cur)->data);
    }
  else
    {
      return (NULL);
    }
}

IS_EMPTY RCS_LINKED_LIST::is_empty ()
{
  if ((NULL == head) || (NULL == tail) || (list_size == 0))
    {
      return (LIST_EMPTY);
    }
  else
    {
      return (LIST_NOT_EMPTY);
    }
}

void *
RCS_LINKED_LIST::get_by_id (int _id)
{
  RCS_LINKED_LIST_NODE *temp;

  temp = head;
  while (NULL != temp)
    {
      if (temp->id == _id)
	{
	  return (temp->data);
	}
      temp = temp->next;
    }
  return (NULL);
}

void *
RCS_LINKED_LIST::get_first_newer (int _id)
{
  current_node = head;
  while (NULL != current_node)
    {
      if (current_node->id > _id)
	{
	  return (current_node->data);
	}
      current_node = current_node->next;
    }
  return (NULL);
}

void *
RCS_LINKED_LIST::get_last_newer (int _id)
{
  current_node = tail;
  while (NULL != current_node)
    {
      if (current_node->id > _id)
	{
	  return (current_node->data);
	}
      current_node = current_node->last;
    }
  return (NULL);
}

void
RCS_LINKED_LIST::delete_node (int _id)
{
  RCS_LINKED_LIST_NODE *temp;

  temp = head;
  while (NULL != temp)
    {
      if (temp->id == _id)
	{
	  list_size--;
	  if (temp == current_node)
	    {
	      if (NULL != extra_node)
		{
		  extra_node->next = current_node->next;
		  extra_node->last = current_node->last;
		  current_node = extra_node;
		}
	    }
	  if (NULL != temp->next)
	    {
	      temp->next->last = temp->last;
	    }
	  else
	    {
	      tail = temp->last;
	    }
	  if (NULL != temp->last)
	    {
	      temp->last->next = temp->next;
	    }
	  else
	    {
	      head = temp->next;
	    }
	  if ((temp->copied || delete_data_not_copied)
	      && (NULL != temp->data))
	    {
	      void *d = temp->data;
	      temp->data = 0;
	      DEBUG_FREE (d);
	    }
	  delete temp;
	  break;
	}
      temp = temp->next;
    }
}

void
RCS_LINKED_LIST::delete_current_node ()
{
  if (NULL != current_node && (current_node != extra_node))
    {
      RCS_LINKED_LIST_NODE *temp;
      temp = current_node;
      if (NULL != extra_node)
	{
	  extra_node->next = current_node->next;
	  extra_node->last = current_node->last;
	  current_node = extra_node;
	}
      if (NULL != temp->next)
	{
	  temp->next->last = temp->last;
	}
      else
	{
	  tail = temp->last;
	}
      if (NULL != temp->last)
	{
	  temp->last->next = temp->next;
	}
      else
	{
	  head = temp->next;
	}
      if ((temp->copied || delete_data_not_copied) && (NULL != temp->data))
	{
	  void *d = temp->data;
	  temp->data = 0;
	  DEBUG_FREE (d);
	}
      delete temp;
      list_size--;
    }
}


int
RCS_LINKED_LIST::get_current_id ()
{
  if (current_node == NULL)
    {
      return (-1);
    }
  return (current_node->id);
}


#ifdef LINUXCNC_LIBNML_COMPAT 

LinkedList::LinkedList() : RCS_LINKED_LIST() {
}

LinkedList::~LinkedList() {
}

int LinkedList::store_at_tail (void *_data, size_t _size, int _copy) {
  return RCS_LINKED_LIST::store_at_tail(_data,_size,_copy);
}

void LinkedList::delete_members () {
  RCS_LINKED_LIST::delete_members();
}

void *LinkedList::retrieve_head () {
  return RCS_LINKED_LIST::retrieve_head();
}

void *LinkedList::get_head () {
  return RCS_LINKED_LIST::get_head();
}

void *LinkedList::get_current () {
  return RCS_LINKED_LIST::get_current();
}

void *LinkedList::get_tail () {
  return RCS_LINKED_LIST::get_tail();
}

void *LinkedList::get_next () {
  return RCS_LINKED_LIST::get_next();
}

void *LinkedList::get_last () {
  return RCS_LINKED_LIST::get_last();
}

#endif  // LINUXCNC_LIBNML_COMPAT
