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
#endif

#if defined(ENABLE_RCS_CMS_MRPQ)


#ifdef HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#endif

#include "cms_mrpq.hh"
#include "physmem.hh"
#include "cms.hh"
#include "rcs_prnt.hh"		// rcs_print_error()

#if defined(ENABLE_RCS_PRINT) || defined(ENABLE_RCS_PRNT)
#define PH_CHECKED_READ_MACRO(r,x,y,z) if((x)->read((y),(z)) < 0) { \
set_print_rcs_error_info( __FILE__, __LINE__); print_rcs_error_new("read failed.\n"); \
status=CMS_INTERNAL_ACCESS_ERROR; \
return r; \
}

#define PH_CHECKED_WRITE_MACRO(r,x,y,z) if((x)->write((y),(z)) < 0) { \
set_print_rcs_error_info( __FILE__, __LINE__); print_rcs_error_new("write failed.\n"); \
status=CMS_INTERNAL_ACCESS_ERROR; \
return r; \
}

#else

#define PH_CHECKED_READ_MACRO(r,x,y,z) if((x)->read((y),(z)) < 0) { \
status=CMS_INTERNAL_ACCESS_ERROR; \
return r; \
} if(false)

#define PH_CHECKED_WRITE_MACRO(r,x,y,z) if((x)->write((y),(z)) < 0) { \
status=CMS_INTERNAL_ACCESS_ERROR; \
return r; \
} if(false)

#endif

class CMS_MRPQ_BLOCK_LIST_NODE
{
public:
  CMS_MRPQ_BLOCK_LIST_NODE():
    priority(0),data_size(0),write_id(0),data_offset(0),read_mask_offset(0),
    next_block(0),last_block(0),my_offset(0)
  {
    priority=0;
    data_size=0;
    data_offset=0;
    read_mask_offset=0;
    next_block=0;
    last_block=0;
    my_offset=0;
  }
  int priority;
  size_t data_size;
  long write_id;
  long data_offset;
  long read_mask_offset;
  long next_block;
  long last_block;
  long my_offset;
};

CMS_MULTIREADER_PRIORITY_QUEUE::CMS_MULTIREADER_PRIORITY_QUEUE(CMS *_cms,
							       bool _initialize):
  status(CMS_STATUS_NOT_SET),
  node(0),
  last_node(0),
  next_node(0),
  my_reader_byte_offset(0),
  my_reader_byte_mask(0),
  size(0),
  num_readers(0),
  i_am_a_reader(false),
  neutral(false),
  cms(0),
  bytes_in_readers_mask(0),
  readers_mask(0),
  base_readers_mask(0),
  bytes_in_block_usage(0),
  block_usage(0),
  ph(0),
  priority(0),
  reader_id(0),
  pending_reader_id(0),
  block_usage_start(0),
  block_usage_end(0),
  preserve_node(false),
  no_break_when_new(false),
  preserve_mrpq_reader_id(false)
{
  preserve_mrpq_reader_id=false;
  cms = _cms;
  class PHYSMEM_HANDLE *_ph  = cms->handle_to_global_data;
  long orig_ph_offset = _ph->get_offset();
  _ph->increment_offset(cms->skip_area);
  neutral = cms->neutral;
  size = cms->subdiv_size;
  num_readers = cms->num_readers;
  i_am_a_reader = false;
  reader_id=-1;
  cms=_cms;
  preserve_node=false;

  if(num_readers > 1 )
    {
      if(num_readers < 31)
	{
	  num_readers=31;
	}
      bytes_in_readers_mask= num_readers/8+1;
      bytes_in_readers_mask+= (4 - bytes_in_readers_mask%4);
      readers_mask = (unsigned char *) malloc(bytes_in_readers_mask);
      base_readers_mask = (unsigned char *) malloc(bytes_in_readers_mask);
      if(_initialize)
	{
	  _ph->memsetf(_ph->get_offset(),0,bytes_in_readers_mask);
	}
    }
  else
    {
      bytes_in_readers_mask=0;
      readers_mask=0;
    }
  bytes_in_block_usage = (size/64)+1;
  block_usage = (unsigned char *) malloc(bytes_in_block_usage);
  node = new CMS_MRPQ_BLOCK_LIST_NODE();
  last_node = new CMS_MRPQ_BLOCK_LIST_NODE();
  next_node = new CMS_MRPQ_BLOCK_LIST_NODE();
  if(_initialize)
    {
        long head_offset = _ph->get_offset() + bytes_in_readers_mask;
	_ph->memsetf(head_offset,0,sizeof(long)*3);
     }
  no_break_when_new=false;
  _ph->set_offset( orig_ph_offset);
}

CMS_MULTIREADER_PRIORITY_QUEUE::~CMS_MULTIREADER_PRIORITY_QUEUE()
{
  if(i_am_a_reader && 0 != ph && !preserve_mrpq_reader_id)
    {
      remove_current_id();
    }
  if(last_node && last_node != node)
    {
      delete last_node;
      last_node=0;
    }
  if(next_node && next_node != node)
    {
      delete next_node;
      next_node=0;
    }
  if(node)
    {
      delete node;
      node=0;
    }
}

void CMS_MULTIREADER_PRIORITY_QUEUE::set_pending_id(int _id)
{
  pending_reader_id = _id;
}

int CMS_MULTIREADER_PRIORITY_QUEUE::get_new_id(void)
{  
  bool found_empty_slot=false;
  PHYSMEM_HANDLE *_ph=ph;

  if(num_readers < 1)
    {
      return 0;
    }
  PH_CHECKED_READ_MACRO(-1,_ph,(void*)base_readers_mask,bytes_in_readers_mask);
  my_reader_byte_mask = 0x1;
  my_reader_byte_offset =0;
  reader_id=0;
  for(int i =0; i < bytes_in_readers_mask && !found_empty_slot ; i++)
    {
      if(base_readers_mask[i] == 0xFF )
	{
	  reader_id += 8;
	  my_reader_byte_offset++;
	  continue;
	}
      for(int j = 0 ; j < 8 ; j++)
	{
	  unsigned char b = base_readers_mask[i];
	  unsigned char m = my_reader_byte_mask;
	  unsigned char b_m = b & m;
	  if(b_m != 0)
	    {
	      my_reader_byte_mask = (my_reader_byte_mask<<1);
	      reader_id++;
	    }
	  else
	    {
	      found_empty_slot=true;
	      my_reader_byte_offset=i;
	      base_readers_mask[i] = base_readers_mask[i] | my_reader_byte_mask;
	      long orig_offset = _ph->get_offset();
	      _ph->increment_offset( my_reader_byte_offset);
	      PH_CHECKED_WRITE_MACRO(-1,_ph, &(base_readers_mask[i]),1);
	      _ph->set_offset( orig_offset);
	      break;
	    }
	}
    }
  i_am_a_reader=found_empty_slot;
  if(!found_empty_slot)
  {
    reader_id= -1;
    return -1;
  }
  rcs_print_debug(PRINT_MISC,"CMS_MULTIREADER_PRIORITY_QUEUE::get_new_id() returning %d\n",reader_id);
  return reader_id;
}

int CMS_MULTIREADER_PRIORITY_QUEUE::get_current_id()
{
  if(!i_am_a_reader)
    {
      return -1;
    }
  return reader_id;
}

void CMS_MULTIREADER_PRIORITY_QUEUE::set_current_id(int _id)
{
  PHYSMEM_HANDLE *_ph=ph;
  if(_id >= 0 && num_readers > 1 && _id != reader_id)
    {
      unsigned char b;
      my_reader_byte_offset = _id/8;
      my_reader_byte_mask = (1 <<(_id%8));
      long orig_offset = _ph->get_offset();
      _ph->increment_offset( my_reader_byte_offset);
      PH_CHECKED_READ_MACRO(,_ph, &b,1);
      b = b | my_reader_byte_mask;
      PH_CHECKED_WRITE_MACRO(,_ph, &b,1);
      _ph->set_offset( orig_offset);
      reader_id = _id;
      i_am_a_reader=true;
      rcs_print_debug(PRINT_MISC,"CMS_MULTIREADER_PRIORITY_QUEUE::set_current_id(%d)\n",reader_id);
    }
}

void CMS_MULTIREADER_PRIORITY_QUEUE::remove_id(int _id)
{
  PHYSMEM_HANDLE *_ph=ph;
  if(_id >= 0 && num_readers > 1)
    {
      unsigned char b;
      long temp_offset = _id/8;
      unsigned char temp_byte_mask = (1 <<(_id%8));
      long orig_offset = _ph->get_offset();
      _ph->increment_offset( temp_offset);
      PH_CHECKED_READ_MACRO(,_ph, &b,1);
      b = b & ~temp_byte_mask;
      PH_CHECKED_WRITE_MACRO(,_ph, &b,1);
      _ph->set_offset( orig_offset);
      rcs_print_debug(PRINT_MISC,"CMS_MULTIREADER_PRIORITY_QUEUE::remove_id(%d)\n",_id);
    }
  if(_id == reader_id)
    {
      i_am_a_reader =false;
      reader_id=-1;
    }
}

void CMS_MULTIREADER_PRIORITY_QUEUE::set_preserve_mrpq_reader_id(bool _b)
{
  preserve_mrpq_reader_id=_b;
  rcs_print_debug(PRINT_MISC,"CMS_MULTIREADER_PRIORITY_QUEUE::set_preserve_mrpq_reader_id(%d) : reader_id=%d\n",((int)preserve_mrpq_reader_id),reader_id);
}

void CMS_MULTIREADER_PRIORITY_QUEUE::remove_current_id(void)
{
  PHYSMEM_HANDLE *_ph=ph;
  if(i_am_a_reader 
     && reader_id >= 0 
     && my_reader_byte_offset >= 0 
     && num_readers>1 
     && !preserve_mrpq_reader_id)
    {
      unsigned char b;
      long orig_offset = _ph->get_offset();
      _ph->increment_offset( my_reader_byte_offset);
      PH_CHECKED_READ_MACRO(,_ph, &b,1);
      b = b & ~my_reader_byte_mask;
      PH_CHECKED_WRITE_MACRO(,_ph, &b,1);
      _ph->set_offset( orig_offset);
      no_break_when_new=true;
      read_raw();
      no_break_when_new=false;
      rcs_print_debug(PRINT_MISC,"CMS_MULTIREADER_PRIORITY_QUEUE::remove_current_id() reader_id=%d\n",reader_id);
    }
  reader_id=-1;
  i_am_a_reader=false;  
}

void CMS_MULTIREADER_PRIORITY_QUEUE::update_block_usage(void)
{
  PHYSMEM_HANDLE *_ph = ph;
  long orig_offset = _ph->get_offset();
  long head_offset = _ph->get_offset() + bytes_in_readers_mask;
  long node_offset;
  block_usage_start = head_offset;
  cms->free_space = size - block_usage_start;
  block_usage_end = bytes_in_block_usage-1-((block_usage_start+63)/64);

  _ph->set_offset( head_offset);
  PH_CHECKED_READ_MACRO(,_ph,&node_offset,sizeof(long));
  memset(block_usage,0,bytes_in_block_usage);
  mark_area(head_offset,sizeof(long)*3);
  if(node_offset == 0)
    {
      _ph->set_offset( orig_offset);
      return;
    }
  _ph->set_offset( node_offset);
  PH_CHECKED_READ_MACRO(,_ph,node,sizeof(class CMS_MRPQ_BLOCK_LIST_NODE));
  while(node_offset > 0)
    {
      if(node_offset != node->my_offset)
	{
	  rcs_print_error("node_offset = %ld (0x%X), node->my_offset=%ld(0x%X)\n",
			  node_offset,(unsigned) node_offset,
			  node->my_offset,(unsigned) node->my_offset);
	  status=CMS_INTERNAL_ACCESS_ERROR;
	  break;
	}
      mark_block_for_node();
      node_offset = node->next_block;
      if(node_offset > 0)
	{
	  _ph->set_offset( node_offset);
	  PH_CHECKED_READ_MACRO(,_ph,node,sizeof(class CMS_MRPQ_BLOCK_LIST_NODE));
	}
    }
  _ph->set_offset( orig_offset);
}

void CMS_MULTIREADER_PRIORITY_QUEUE::mark_area(long _offset, size_t _sz)
{
  long relative_offset= _offset - block_usage_start;
  long bu_offset = (relative_offset)/64;
  unsigned char bmask = (1<<(relative_offset/8)%8); 
  unsigned int bits_to_mark = (_sz+7)/8;
  cms->free_space -= ((_sz+7)/8)*8;
  for(unsigned int i = 0; i < bits_to_mark; i++)
    {
      if(block_usage[bu_offset] & bmask)
	{
	  rcs_print_error("Double marked area. (_offset=%ld(0x%lX),sz=%lu(0x%lX),bu_offset=%ld(0x%lX),block_usage[bu_offset]=0x%X,bmask=0x%X,i=%d(0x%X)) \n",
			  _offset,(unsigned long) _offset,
			  (unsigned long) _sz,(unsigned long)_sz,
			  bu_offset,(unsigned long) bu_offset,
			  block_usage[bu_offset],bmask, 
			  i,i);
	}
      block_usage[bu_offset] |= bmask;
      if(bmask & 0x80)
	{
	  bu_offset ++;
	  bmask = 1;
	}
      else
	{
	  bmask=(bmask << 1);
	}
    }
}     



long CMS_MULTIREADER_PRIORITY_QUEUE::allocate_area(size_t sz)
{
  long abs_offset;
  size_t sz8 = sz/8;
  size_t size_avail=0;
  long bu_offset=0;
  long start_offset=0;
  if(((size_t)cms->free_space) < ((sz+7)/8)*8 )
    {
      return 0;
    }
  if(sz%8 != 0)
    {
      sz8++;
    }
  for(bu_offset = 0; bu_offset < block_usage_end ; bu_offset++)
    {
      unsigned char b = block_usage[bu_offset];
      unsigned char m = 1;
      unsigned char b_m = 0;
      if(b  == 0xFF)
	{
	  size_avail=0;
	  continue;
	}
      for(int i = 0; i < 8 ; i++)
	{
	  m = (1<<i);
	  b_m = b & m;
	  if(b_m == 0)
	    {
	      if(size_avail == 0)
		{
		  start_offset = bu_offset*64+i*8;
		}
	      size_avail++;
	    }
	  else
	    {
	      size_avail =0;
	    }
	  if(size_avail >= sz8)
	    {
	      abs_offset = start_offset + block_usage_start;
	      mark_area(abs_offset, sz);
	      return abs_offset;
	    }
	}
    }
  
  return 0;
}

void CMS_MULTIREADER_PRIORITY_QUEUE::mark_block_for_node(void)
{
  if(node->my_offset > 0)
    {
      mark_area(node->my_offset,sizeof(class CMS_MRPQ_BLOCK_LIST_NODE));
    }
  if(node->data_offset > 0 && node->data_size > 0)
    {
      mark_area(node->data_offset,node->data_size);
    }
  if(num_readers > 0 && bytes_in_readers_mask > 0 && node->read_mask_offset > 0)
    {
      mark_area(node->read_mask_offset,bytes_in_readers_mask);
    }
}

void CMS_MULTIREADER_PRIORITY_QUEUE::remove_node(void)
{
  PHYSMEM_HANDLE *_ph=ph;
  long orig_offset = _ph->get_offset();
  long head_offset = _ph->get_offset() + bytes_in_readers_mask;
  long node_offset = node->my_offset;
  long last_node_offset = node->last_block;
  long next_node_offset = node->next_block;
  long first_node_offset;
  rcs_print_debug(PRINT_MISC,"removing_node %ld(0x%X)\n",
		  node_offset,(unsigned) node_offset);
  _ph->set_offset( head_offset);
  PH_CHECKED_READ_MACRO(,_ph,&first_node_offset,sizeof(long));
  if(last_node_offset > 0)
    {
      ph->set_offset( last_node_offset);
      PH_CHECKED_READ_MACRO(,ph,last_node,sizeof(CMS_MRPQ_BLOCK_LIST_NODE));
    }
  if(next_node_offset > 0)
    {
      ph->set_offset( next_node_offset);
      PH_CHECKED_READ_MACRO(,ph,next_node,sizeof(CMS_MRPQ_BLOCK_LIST_NODE));
    }
  if(last_node && last_node_offset > 0)
    {
      last_node->next_block = next_node_offset;
    }
  if(next_node && next_node_offset > 0)
    {
      next_node->last_block = last_node_offset;
    }
  if(first_node_offset == node_offset)
    {
      _ph->set_offset( head_offset);
      PH_CHECKED_WRITE_MACRO(,_ph,&(node->next_block),sizeof(long));
    }
  node->next_block=0;
  node->last_block=0;
  ph->memsetf(node_offset,0,sizeof(CMS_MRPQ_BLOCK_LIST_NODE));
  if(last_node && last_node_offset > 0)
    {
      ph->set_offset( last_node_offset);
      PH_CHECKED_WRITE_MACRO(,ph,last_node,sizeof(CMS_MRPQ_BLOCK_LIST_NODE));
    }
  if(next_node && next_node_offset > 0)
    {
      ph->set_offset( next_node_offset);
      PH_CHECKED_WRITE_MACRO(,ph,next_node,sizeof(CMS_MRPQ_BLOCK_LIST_NODE));
    }
  ph->set_offset( orig_offset);
}

void  CMS_MULTIREADER_PRIORITY_QUEUE::print_node()
{
  rcs_print_debug(PRINT_MISC,"priority=%d, data_size=%lu, write_id=%ld,data_offset=%ld(0x%lX),read_mask_offset=%ld(0x%lX),next_block=%ld(0x%lX),last_block=%ld(0x%lX),my_offset=%ld(0x%lX)\n",
	 node->priority,
	 (unsigned long) node->data_size,
	 node->write_id,
	 node->data_offset,
	 node->data_offset,
	 node->read_mask_offset,
	 node->read_mask_offset,
	 node->next_block,
	 node->next_block,
	 node->last_block,
	 node->last_block,
	 node->my_offset,
	 (unsigned long) node->my_offset);
  if(node->read_mask_offset)
    {
      rcs_print_debug(PRINT_MISC,"readers_mask: %X%X%X%X, \tbase_readers_mask: %X%X%X%X, \tmy_reader_byte_mask=%X\n",readers_mask[0],readers_mask[1],readers_mask[2],readers_mask[3],base_readers_mask[0],base_readers_mask[1],base_readers_mask[2],base_readers_mask[3],my_reader_byte_mask);
    }
}

CMS_STATUS CMS_MULTIREADER_PRIORITY_QUEUE::read_raw ()
{
  if(!i_am_a_reader && num_readers > 1)
    {
      get_new_id();
    }
  if(!i_am_a_reader && num_readers > 1)
    {
      rcs_print_error("Too many readers\n");
      return (status = CMS_INTERNAL_ACCESS_ERROR);
    }

  PHYSMEM_HANDLE *_ph=ph;
  long orig_offset = _ph->get_offset();
  long head_offset = _ph->get_offset() + bytes_in_readers_mask;
  long node_offset;
  long first_node_offset;
  long read_mask_offset;
  bool all_read =false;
  bool new_to_me=false;
  long queue_length_offset = head_offset + 2*sizeof(long);
  long node_count=0;
  long orig_queue_length = 0;

  _ph->set_offset( queue_length_offset);
  PH_CHECKED_READ_MACRO(status,_ph,&(cms->queuing_header.queue_length),sizeof(cms->queuing_header.queue_length));

  orig_queue_length = cms->queuing_header.queue_length;
  status = CMS_READ_OLD;
  if(num_readers>1)
    {
      _ph->set_offset( orig_offset);
      PH_CHECKED_READ_MACRO(status,ph,base_readers_mask,bytes_in_readers_mask);
      if(0 == (base_readers_mask[my_reader_byte_offset] & my_reader_byte_mask))
	{
	  rcs_print_error("My bit not set in base readers_mask. my_reader_byte_mask=0x%X, my_reader_byte_offset=%ld, base_readers_mask[my_reader_byte_offset]=%X\n", 
			  my_reader_byte_mask,
			  my_reader_byte_offset, 
			  base_readers_mask[my_reader_byte_offset]);
	  i_am_a_reader=false;
	  return (status = CMS_INTERNAL_ACCESS_ERROR);
	}
    }
  _ph->set_offset( head_offset);
  PH_CHECKED_READ_MACRO(status,_ph,&first_node_offset,sizeof(long));
  node_offset = first_node_offset;
  if(node_offset == 0)
    {
      cms->status = CMS_READ_OLD;
      return status = CMS_READ_OLD;
    }
  _ph->set_offset( node_offset);
 
  PH_CHECKED_READ_MACRO(status,_ph,node,sizeof(class CMS_MRPQ_BLOCK_LIST_NODE));  while(node_offset > 0 && !new_to_me)
    {
      if(node_count > orig_queue_length)
	{
	  rcs_print_error("BAD queue data\n");
	  return(status = CMS_INTERNAL_ACCESS_ERROR);
	}
      node_count++;
      node_offset = node->next_block;
      unsigned char r = 0;
      unsigned char m = 0;
      unsigned char r_m =0;
      if(num_readers > 1)
	{
	  read_mask_offset = node->read_mask_offset;
	  _ph->set_offset( read_mask_offset);
	  PH_CHECKED_READ_MACRO(status,_ph,readers_mask,bytes_in_readers_mask);
	  r = readers_mask[my_reader_byte_offset];
	  m = my_reader_byte_mask;
	  r_m = r & m;
	  if( r_m ==0)
	    {
	      readers_mask[my_reader_byte_offset] |= my_reader_byte_mask;
	      _ph->set_offset( read_mask_offset + my_reader_byte_offset);
	      PH_CHECKED_WRITE_MACRO(status,_ph,&(readers_mask[my_reader_byte_offset]),1);
	      new_to_me=true;
	    }
	  all_read = true;
	  for(int i = 0; i < bytes_in_readers_mask; i++)
	    {
	      r = ~readers_mask[i];
	      m = base_readers_mask[i];
	      r_m = r & m;
	      if( r_m != 0)
		{
		  all_read=false;
		  break;
		}
	    }
	}
      else
	{
	  new_to_me=true;
	  all_read=true;
	}
      print_node();
      if(new_to_me)
	{
	  ph->set_offset(node->data_offset);
	  cms->header.write_id = node->write_id;
	  cms->header.in_buffer_size = node->data_size;
	  cms->header.was_read=1;
	  if(cms->check_id(cms->header.write_id) == CMS_READ_OK)
	    {
	      PH_CHECKED_READ_MACRO(status,ph,cms->data,node->data_size);
	      status = CMS_READ_OK;
	    }
	}
      if(all_read && !preserve_node)
	{
	  ph->set_offset( orig_offset);
	  remove_node();
	  cms->queuing_header.queue_length--;
	  _ph->set_offset( queue_length_offset);
	  PH_CHECKED_WRITE_MACRO(status,_ph,&(cms->queuing_header.queue_length),sizeof(cms->queuing_header.queue_length));
	}
      if(new_to_me && !no_break_when_new)
	{
	  _ph->set_offset( orig_offset);
	  return(status = CMS_READ_OK);
	}
      if(node_offset > 0)
	{
	  _ph->set_offset( node_offset);
	  PH_CHECKED_READ_MACRO(status,_ph,node,sizeof(class CMS_MRPQ_BLOCK_LIST_NODE));
	}
    }
  if(!new_to_me && ((int)status) >= 0)
    {
      status = CMS_READ_OLD;
    }
  _ph->set_offset( orig_offset);
  return(status);
}
      
CMS_STATUS CMS_MULTIREADER_PRIORITY_QUEUE::write_raw (void *user_data)
{
  status = CMS_STATUS_NOT_SET;
  update_block_usage();
  if(status < 0)
    {
      return status;
    }

  PHYSMEM_HANDLE *_ph = ph;
  long orig_offset = _ph->get_offset();
  long head_offset = _ph->get_offset() + bytes_in_readers_mask;
  long node_offset;
  long new_node_offset;
  long first_node_offset;
  long last_node_offset;
  long write_id = 0;
  bool wrote_data = false;
  long queue_length_offset = head_offset + 2*sizeof(long);
  long node_count=0;
  
  new_node_offset=0;
  _ph->set_offset( head_offset);
  PH_CHECKED_READ_MACRO(status,_ph,&first_node_offset,sizeof(long));
  _ph->increment_offset( sizeof(long));
  PH_CHECKED_READ_MACRO(status,_ph,&write_id,sizeof(long));
  write_id++;
  if(write_id < 1)
    {
      write_id = 1;
    }
  cms->header.write_id = write_id;
  PH_CHECKED_WRITE_MACRO(status,_ph,&write_id,sizeof(long)); 
  _ph->set_offset( queue_length_offset);
  PH_CHECKED_READ_MACRO(status,_ph,&(cms->queuing_header.queue_length),sizeof(cms->queuing_header.queue_length));
  node_offset = first_node_offset;
  if(first_node_offset > 0)
    {
      _ph->set_offset( node_offset);
      PH_CHECKED_READ_MACRO(status,_ph,node,sizeof(class CMS_MRPQ_BLOCK_LIST_NODE));
    }
  last_node_offset=0;
  while(node_offset > 0 && !wrote_data)
    {
      if(node_count > cms->queuing_header.queue_length)
	{
	  rcs_print_error("BAD queue data\n");
	  return(status = CMS_INTERNAL_ACCESS_ERROR);
	}
      if(last_node_offset != node->last_block)
	{
	  rcs_print_error("last_node_offset=%ld,node->last_block=%ld\n",
			  last_node_offset,node->last_block);
	  return (status = CMS_INTERNAL_ACCESS_ERROR);
	}
      print_node();
      node_count++;
      last_node_offset=node_offset;
      node_offset = node->next_block;
      if(node->priority < priority)
	{
	  new_node_offset = allocate_area(sizeof(class CMS_MRPQ_BLOCK_LIST_NODE));
	  last_node_offset = node->last_block;
	  long next_node_offset = node->my_offset;
	  if(0 == new_node_offset)
	    {
	      return (status = CMS_QUEUE_FULL);
	    }
	  if(last_node_offset > 0)
	    {
	      ph->set_offset( last_node_offset);
	      PH_CHECKED_READ_MACRO(status,ph,last_node,sizeof(CMS_MRPQ_BLOCK_LIST_NODE));
	      last_node->next_block=new_node_offset;
	    }
	  if(next_node_offset > 0)
	    {
	      ph->set_offset( next_node_offset);
	      PH_CHECKED_READ_MACRO(status,ph,next_node,sizeof(CMS_MRPQ_BLOCK_LIST_NODE));
	      next_node->last_block=new_node_offset;
	    }
	  node->next_block = next_node_offset;
	  node->last_block = last_node_offset;
	  if(num_readers > 1)
	    {
	      node->read_mask_offset = allocate_area(bytes_in_readers_mask);
	      if(0 == node->read_mask_offset)
		{
		  return (status = CMS_QUEUE_FULL);
		}
	      _ph->memsetf(node->read_mask_offset,0,bytes_in_readers_mask);
	    }
	  node->data_size = cms->header.in_buffer_size;
	  node->data_offset=allocate_area(node->data_size);
	  if(0 == node->data_offset)
	    {
	      return (status = CMS_QUEUE_FULL);
	    }	  
	  ph->set_offset( new_node_offset);
	  node->write_id=write_id;
	  node->my_offset = new_node_offset;
	  node->priority=priority;
	  PH_CHECKED_WRITE_MACRO(status,ph,node,sizeof(CMS_MRPQ_BLOCK_LIST_NODE));
	  if(first_node_offset == next_node_offset)
	    {
	      ph->set_offset( head_offset);
	      PH_CHECKED_WRITE_MACRO(status,ph,&new_node_offset,sizeof(long));
	    }
	  else if(last_node_offset > 0)
	    {
	      ph->set_offset( last_node_offset);
	      PH_CHECKED_WRITE_MACRO(status,ph,last_node,sizeof(CMS_MRPQ_BLOCK_LIST_NODE));
	    }
	  if(next_node_offset > 0)
	    {
	      ph->set_offset( next_node_offset);
	      PH_CHECKED_WRITE_MACRO(status,ph,next_node,sizeof(CMS_MRPQ_BLOCK_LIST_NODE));
	    }
	  ph->set_offset(node->data_offset);
	  PH_CHECKED_WRITE_MACRO(status,ph,user_data,node->data_size);
	  cms->queuing_header.queue_length++;
	  _ph->set_offset( queue_length_offset);
	  PH_CHECKED_WRITE_MACRO(status,_ph,&(cms->queuing_header.queue_length),sizeof(cms->queuing_header.queue_length));
	  wrote_data=true;
	  status=CMS_WRITE_OK;
	  break;
	}
      if(node_offset > 0)
	{
	  _ph->set_offset( node_offset);
	  PH_CHECKED_READ_MACRO(status,_ph,node,sizeof(class CMS_MRPQ_BLOCK_LIST_NODE));
	}
    }
  if(!wrote_data)
    {
      //	  mark_area(head_offset,sizeof(long));
      new_node_offset = node_offset = allocate_area(sizeof(class CMS_MRPQ_BLOCK_LIST_NODE));
      if(node_offset == 0)
	{
	  return (status = CMS_QUEUE_FULL);
	}
      memset(node,0,sizeof(class CMS_MRPQ_BLOCK_LIST_NODE));
      if(num_readers > 1)
	{
	  node->read_mask_offset = allocate_area(bytes_in_readers_mask);
	  if(0 == node->read_mask_offset)
	    {
	      return (status = CMS_QUEUE_FULL);
	    }
	  ph->memsetf(node->read_mask_offset,0,bytes_in_readers_mask);
	}
      node->data_size = sizeof(CMS_HEADER) + cms->header.in_buffer_size;
      node->data_offset=allocate_area(node->data_size);
      if(0 == node->data_offset)
	{
	  return (status = CMS_QUEUE_FULL);
	}	  
      ph->set_offset( new_node_offset);
      node->write_id = write_id;
      node->my_offset = new_node_offset;
      node->last_block = last_node_offset;
      node->priority=priority;
      PH_CHECKED_WRITE_MACRO(status,ph,node,sizeof(CMS_MRPQ_BLOCK_LIST_NODE));
      if(first_node_offset ==0)
	{
	  first_node_offset = new_node_offset;
	  _ph->set_offset( head_offset);
	  PH_CHECKED_WRITE_MACRO(status,_ph,&first_node_offset,sizeof(long));
	}
      else if(last_node_offset > 0)
	{
	  _ph->set_offset( last_node_offset);
	  PH_CHECKED_READ_MACRO(status,_ph,last_node,sizeof(class CMS_MRPQ_BLOCK_LIST_NODE));
	  last_node->next_block=node_offset;
	  _ph->set_offset(last_node_offset);
	  PH_CHECKED_WRITE_MACRO(status,ph,last_node,sizeof(CMS_MRPQ_BLOCK_LIST_NODE));
	}
      ph->set_offset(node->data_offset);
      PH_CHECKED_WRITE_MACRO(status,ph,user_data, node->data_size);
      cms->queuing_header.queue_length++;
      _ph->set_offset( queue_length_offset);
      PH_CHECKED_WRITE_MACRO(status,_ph,&(cms->queuing_header.queue_length),sizeof(cms->queuing_header.queue_length));
      wrote_data=true;
      status=CMS_WRITE_OK;
    }
  _ph->set_offset( orig_offset);
  return(status);
}

int CMS_MULTIREADER_PRIORITY_QUEUE::check_if_read_raw (void)
{
  PHYSMEM_HANDLE *_ph = ph;
  long orig_offset = _ph->get_offset();
  long head_offset = _ph->get_offset() + bytes_in_readers_mask;
  long write_id_offset = head_offset + sizeof(long);
  long queue_length_offset = head_offset + 2*sizeof(long);
  _ph->set_offset( write_id_offset);
  PH_CHECKED_READ_MACRO(status,_ph,&(cms->header.write_id),sizeof(long));
  _ph->set_offset( queue_length_offset);
  PH_CHECKED_READ_MACRO(status,_ph,&(cms->queuing_header.queue_length),sizeof(long));
  _ph->set_offset( orig_offset);
  return (cms->queuing_header.queue_length == 0);
}

int CMS_MULTIREADER_PRIORITY_QUEUE::clear(void)
{
  PHYSMEM_HANDLE *_ph = ph;
  long orig_offset = _ph->get_offset();
  long head_offset = _ph->get_offset() + bytes_in_readers_mask;
  long first_node_offset;
  long write_id_offset = head_offset + sizeof(long);
  long queue_length_offset = head_offset + 2*sizeof(long);

  first_node_offset=0;
  _ph->set_offset( head_offset);
  PH_CHECKED_WRITE_MACRO(status,_ph,&first_node_offset,sizeof(long));  
  _ph->set_offset( write_id_offset);
  PH_CHECKED_READ_MACRO(status,_ph,&(cms->header.write_id),sizeof(long));
  _ph->set_offset( queue_length_offset);
  cms->queuing_header.queue_length=0;
  PH_CHECKED_WRITE_MACRO(status,_ph,&(cms->queuing_header.queue_length),sizeof(long));
  
  _ph->set_offset( orig_offset);
  return (0);
}

int CMS_MULTIREADER_PRIORITY_QUEUE::check_if_read_encoded (void)
{
  status=CMS_NO_IMPLEMENTATION_ERROR;
  return 0;
}

int CMS_MULTIREADER_PRIORITY_QUEUE::get_space_available_raw (void)
{
  update_block_usage();
  return cms->free_space;
}

int CMS_MULTIREADER_PRIORITY_QUEUE::get_msg_count_raw (void)
{
  PHYSMEM_HANDLE *_ph = ph;
  long orig_offset = _ph->get_offset();
  long head_offset = _ph->get_offset() + bytes_in_readers_mask;
  long write_id_offset = head_offset + sizeof(long);
  long queue_length_offset = head_offset + 2*sizeof(long);
  _ph->increment_offset( write_id_offset);
  PH_CHECKED_READ_MACRO(status,_ph,&(cms->header.write_id),sizeof(long));
  _ph->increment_offset( queue_length_offset);
  PH_CHECKED_READ_MACRO(status,_ph,&(cms->queuing_header.queue_length),sizeof(long));
  _ph->set_offset( orig_offset);
  return cms->header.write_id;
}

int CMS_MULTIREADER_PRIORITY_QUEUE::get_msg_count_encoded (void)
{
  status=CMS_NO_IMPLEMENTATION_ERROR;
  return -1;
}

enum CMS_STATUS
CMS_MULTIREADER_PRIORITY_QUEUE::read_encoded (void)
{
  return(status=CMS_NO_IMPLEMENTATION_ERROR);
}
	/* Read from neutrally encoded buffers. */
enum CMS_STATUS
CMS_MULTIREADER_PRIORITY_QUEUE::peek_raw (void)
{
  preserve_node=true;
  read_raw();
  preserve_node=false;
  return status;
}
	/* Read  without setting flags. */
enum CMS_STATUS
CMS_MULTIREADER_PRIORITY_QUEUE::peek_encoded (void)
{
  return(status=CMS_NO_IMPLEMENTATION_ERROR);
}
	/* Read  without setting flags. */
enum CMS_STATUS
CMS_MULTIREADER_PRIORITY_QUEUE::write_encoded (void)
{
  return(status=CMS_NO_IMPLEMENTATION_ERROR);
}
	/* Write to neutrally encoded buffers. */
enum CMS_STATUS
CMS_MULTIREADER_PRIORITY_QUEUE::write_if_read_raw (void *user_data)
{
  if(check_if_read_raw())
    {
      return write_raw(user_data);
    }
  return (status = CMS_WRITE_WAS_BLOCKED);
}
	/* Write if read. */
enum CMS_STATUS
CMS_MULTIREADER_PRIORITY_QUEUE::write_if_read_encoded (void)
{
  return(status=CMS_NO_IMPLEMENTATION_ERROR);
}

enum CMS_STATUS
CMS_MULTIREADER_PRIORITY_QUEUE::internal_access(class PHYSMEM_HANDLE *_ph,
						void *_local,
						enum CMS_INTERNAL_ACCESS_TYPE internal_access_type,
						int _priority)
{
  ph = _ph;
  priority=_priority;
  status = CMS_STATUS_NOT_SET;

  if(!neutral)
    {
      switch(internal_access_type)
	{
	case CMS_CHECK_IF_READ_ACCESS:
	  check_if_read_raw ();
	  break;
	case CMS_READ_ACCESS:
	  read_raw ();
	  break;
	case CMS_PEEK_ACCESS:
	  peek_raw ();
	  break;
	case CMS_WRITE_ACCESS:
	  write_raw (_local);
	  break;
	case CMS_WRITE_IF_READ_ACCESS:
	  write_if_read_raw (_local);
	  break;
	case CMS_GET_MSG_COUNT_ACCESS:
	  get_msg_count_raw ();
	  break;
	case CMS_GET_SPACE_AVAILABLE_ACCESS:
	  get_space_available_raw ();
	  break;
	case CMS_GET_NEW_READER_ID_ACCESS:
	  get_new_id();
	  break;
	case CMS_CLEAR_ACCESS:
	  get_new_id();
	  break;
	case CMS_SET_READER_ID_ACCESS:
	  set_current_id(pending_reader_id);
	  break;
	case CMS_REMOVE_READER_ID_ACCESS:
	  if(!preserve_mrpq_reader_id)
	    {
	      remove_current_id();
	    }
	  break;

	default:
	  return (status = CMS_INTERNAL_ACCESS_ERROR);      
	}
    }
  else
    {
      switch(internal_access_type)
	{
	case CMS_CHECK_IF_READ_ACCESS:
	  check_if_read_encoded ();
	  break;
	case CMS_READ_ACCESS:
	  read_encoded ();
	  break;
	case CMS_PEEK_ACCESS:
	  peek_encoded ();
	  break;
	case CMS_WRITE_ACCESS:
	  write_encoded ();
	  break;
	case CMS_WRITE_IF_READ_ACCESS:
	  write_if_read_encoded ();
	  break;
	case CMS_GET_MSG_COUNT_ACCESS:
	  get_msg_count_encoded ();
	  break;
	default:
	  return (status = CMS_INTERNAL_ACCESS_ERROR);      
	}
    }
  if(cms)
    {
      cms->status = status;
    }
  rcs_print_debug(PRINT_MISC,"internal_access_type=%d, status=%d\n",
		  internal_access_type,status);
  return status;
}

CMS_MULTIREADER_PRIORITY_QUEUE::CMS_MULTIREADER_PRIORITY_QUEUE(
							       __unused_parameter__ const CMS_MULTIREADER_PRIORITY_QUEUE &_cmrpq):
  status(CMS_STATUS_NOT_SET),
  node(0),
  last_node(0),
  next_node(0),
  my_reader_byte_offset(0),
  my_reader_byte_mask(0),
  size(0),
  num_readers(0),
  i_am_a_reader(false),
  neutral(false),
  cms(0),
  bytes_in_readers_mask(0),
  readers_mask(0),
  base_readers_mask(0),
  bytes_in_block_usage(0),
  block_usage(0),
  ph(0),
  priority(0),
  reader_id(0),
  pending_reader_id(0),
  block_usage_start(0),
  block_usage_end(0),
  preserve_node(false),
  no_break_when_new(false),
  preserve_mrpq_reader_id(false)
{
  rcs_print_error("CMS_MULTIREADER_PRIORITY_QUEUE copy constructor should never be called.\n");
}

CMS_MULTIREADER_PRIORITY_QUEUE &
CMS_MULTIREADER_PRIORITY_QUEUE::operator=(
					  __unused_parameter__ const CMS_MULTIREADER_PRIORITY_QUEUE &_cmrpq)
{
  rcs_print_error("CMS_MULTIREADER_PRIORITY_QUEUE::operator= should never be called.\n");
  return(*this);
}


//  defined(ENABLE_RCS_CMS_MRPQ)

#else
#include "rcs_empty_source"
#endif
