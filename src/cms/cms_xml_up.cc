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

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if !defined(ENABLE_RCS_XML)
#error Do not compile this file without ENABLE_RCS_XML defined.
#endif


#if HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>		// isgraph()
#include <errno.h>
#endif

#if !defined(XML_PARSE_TEST) && !defined(XML_SCHEMA_TO_HEADER)
#ifndef HAVE_CONFIG_H
#include "rcs_defs.hh"
#endif
#include "cms_xml_up.hh"
#include "cms_enum_info.h" 	/* struct cms_enum_info; */

#include "cms.hh"
#include "nmlmsg.hh"
#include "rcs_prnt.hh"
#include "dbg_mem.h"

#else
#define DEBUG_FREE(x) free(x)
#define DEBUG_MALLOC(x) malloc(x)
#define DEBUG_REALLOC(x,y) realloc(x,y)

#endif

#ifdef USE_LIBXML2
#include <libxml/tree.h>
#else
#include "rcs_prnt.hh"
#include "linklist.hh"

#ifdef DEBUG_MEMORY
#define xmlFree(X)  debug_free(__FILE__,__LINE__,X)
#else
#define xmlFree free
#endif


#define DEBUG_ME(x) 

struct XML_NODE;
typedef struct XML_NODE *xmlNodePtr;
typedef char xmlChar;


struct XML_NS
{
public:
  XML_NS():
    prefix(0),href(0),node(0),qualify_elements(false),qualify_attributes(false)
  {
  };

#ifdef DEBUG_MEMORY
  void *operator      new (size_t s)
  {
    return DEBUG_MALLOC (s);
  }
  void operator      delete (void *ptr)
  {
    if (ptr)
      {
	DEBUG_FREE (ptr);
      }
  }
#endif
  char *prefix;
  char *href;
  xmlNodePtr node;
  bool qualify_elements;
  bool qualify_attributes;

private:
  XML_NS(const XML_NS &);
  XML_NS &operator=(const XML_NS &);
};

typedef struct XML_NS *xmlNsPtr;

struct XML_ATTRIBUTE
{
//public:
//  XML_ATTRIBUTE(): 
//   XML_ATTRIBUTE name(0),value(0),free_name(false),free_value(false) 
//  {
//    name = 0;
//    value= 0;
//    free_name=false;
//    free_value=false;
//  }

  const xmlChar * name;
  const xmlChar *value;
  bool free_name;
  bool free_value;
#ifdef DEBUG_MEMORY
  void *operator      new (size_t s)
  {
    return DEBUG_MALLOC (s);
  }
  void operator      delete (void *ptr)
  {
    if (ptr)
      {
	DEBUG_FREE (ptr);
      }
  }
#endif

private:
  XML_ATTRIBUTE(const XML_ATTRIBUTE &);
  XML_ATTRIBUTE &operator=(const XML_ATTRIBUTE &);
};

typedef struct XML_ATTRIBUTE *xmlAttrPtr;

struct XML_NODE
{
public:
  XML_NODE():
    name(0),content(0),children(0),next(0),prev(0),last(0),parent(0),
    start_add(0),fullname(0),attributes_list(0),beginparseptr(0),
    endparseptr(0),content_should_be_freed(0),name_should_be_freed(0),
    ns(0)
  {
  };

#ifdef DEBUG_MEMORY
  void *operator      new (size_t s)
  {
    return DEBUG_MALLOC (s);
  }
  void operator      delete (void *ptr)
  {
    if (ptr)
      {
	DEBUG_FREE (ptr);
      }
  }
#endif
  const xmlChar *name;
  const xmlChar *content;
  xmlNodePtr children;
  xmlNodePtr next;
  xmlNodePtr prev;
  xmlNodePtr last;
  xmlNodePtr parent;
  const char *start_add;
  const char *fullname;
  class RCS_LINKED_LIST *attributes_list;
  const char *beginparseptr;
  const char *endparseptr;
  int content_should_be_freed;
  int name_should_be_freed;
  xmlNsPtr ns;

private:
  XML_NODE(const XML_NODE &);
  XML_NODE &operator=(const XML_NODE &);

};

class xmlDumpingString;

struct XML_DOC
{
public:
  XML_DOC():
    rootNode(0),header(0),xstr(0)
  {
    rootNode=0;
    header=0;
  }
#ifdef DEBUG_MEMORY
  void *operator      new (size_t s)
  {
    return DEBUG_MALLOC (s);
  }
  void operator      delete (void *ptr)
  {
    if (ptr)
      {
	DEBUG_FREE (ptr);
      }
  }
#endif
  xmlNodePtr rootNode;
  const char *header;
  class xmlDumpingString *xstr;

private:
  XML_DOC(const XML_DOC &);
  XML_DOC &operator=(const XML_DOC &);

};

typedef struct XML_DOC *xmlDocPtr;
static void xmlUnlinkNode (XML_NODE * n);


static void
xmlFreeNode (XML_NODE * n)
{
  if (!n)
    {
      return;
    }
  if(n->ns && n->ns->node == n)
  {
      n->ns->node=0;
  }
  if (n->attributes_list)
    {
      xmlAttrPtr xap = (xmlAttrPtr) n->attributes_list->get_head();
      while(xap)
	{
	  if(xap->free_name && 0 != xap->name)
	    {
	      free((void*)xap->name);
	      xap->name = 0;
	    }
	  if(xap->free_value && 0 != xap->value)
	    {
	      free((void*)xap->value);
	      xap->value = 0;
	    }
	  xap = (xmlAttrPtr) n->attributes_list->get_next();
	}
      n->attributes_list->delete_members ();
      delete n->attributes_list;
      n->attributes_list = 0;
    }
  if (n->children)
    {
      xmlNodePtr child = n->children;
      while (child)
	{
	  xmlNodePtr nextchild = child->next;
	  xmlUnlinkNode (child);
	  xmlFreeNode (child);
	  child = nextchild;
	}
      n->children = 0;
    }
  if (n->content && n->content_should_be_freed)
    {
      DEBUG_FREE ((void*)n->content);
    }
  if (n->name && n->name_should_be_freed)
    {
      DEBUG_FREE ((void*)n->name);
    }
  if (n->attributes_list)
    {
      n->attributes_list->delete_members ();
      delete n->attributes_list;
      n->attributes_list = 0;
    }
  n->content = 0;
  n->name = 0;
  delete n;
}

static void
xmlUnlinkNode (XML_NODE * n)
{
  if (!n)
    {
      return;
    }
  if (n->prev)
    {
      if (n->prev->next == n)
	{
	  n->prev->next = n->next;
	}
    }
  if (n->next)
    {
      if (n->next->prev == n)
	{
	  n->next->prev = n->prev;
	}
    }
  if (n->parent)
    {
      if (n->parent->children == n)
	{
	  n->parent->children = n->next;
	}
      if (n->parent->last == n)
	{
	  n->parent->last = n->prev;
	}
    }
  n->next = 0;
  n->prev = 0;
  n->last = 0;
  n->parent = 0;
  n->ns = 0;
}

static void
init_xml_node (xmlNodePtr n)
{
  if (!n)
    {
      return;
    }
  n->fullname = 0;
  n->children = 0;
  n->parent = 0;
  n->next = 0;
  n->prev = 0;
  n->last = 0;
  n->content = 0;
  n->name = 0;
  n->attributes_list = 0;
  n->beginparseptr = 0;
  n->endparseptr = 0;
  n->ns = 0;
  n->content_should_be_freed = 0;
  n->name_should_be_freed = 0;
  n->start_add=0;
}

xmlAttrPtr
xmlSetProp (xmlNodePtr node, const xmlChar * name, const xmlChar * value)
{
  xmlAttrPtr newattr = 0;
  int attrfound = 0;
  if (0 != node->attributes_list)
    {
      newattr = (xmlAttrPtr) node->attributes_list->get_head ();
      while (newattr)
	{
	  if (newattr->name)
	    {
	      if (!strcmp (newattr->name, name))
		{
		  attrfound = 1;
		  break;
		}
	    }
	  newattr = (xmlAttrPtr) node->attributes_list->get_next ();
	}
    }

  if (!newattr)
    {
      newattr = (struct XML_ATTRIBUTE *) malloc(sizeof(struct XML_ATTRIBUTE));
      memset(newattr,0,sizeof(struct XML_ATTRIBUTE));
      if (name)
	{
	  newattr->name = name;
	}
    }
  if (value)
    {
      newattr->value = value;
    }
  if (node && !attrfound)
    {
      if (0 == node->attributes_list)
	{
	  node->attributes_list = new RCS_LINKED_LIST ();
	}
      node->attributes_list->store_at_tail (newattr, sizeof (newattr), 0);
    }
  return newattr;
}



static int parse_attributes = 1;

static xmlNodePtr
xmlParseStringForNode (const char *parseptr, const char *endbufptr)
{
  const char *nameptr = 0;
  XML_NODE *newnode = 0;
  int namelen = 0;
  const char *contentptr = 0;
  const char *endtagname = 0;
  long endtagnamelen = 0;
  long contentlen = 0;

  if (endbufptr <= parseptr || parseptr == 0)
    {
      rcs_print_error
	("xmlParseStringForNode(%p,%p) bad ptrs to parse area.\n", parseptr,
	 endbufptr);
      return 0;
    }
  newnode = new XML_NODE;
  if (0 == newnode)
    {
      fprintf (stderr, "OUT of memory\n");
      exit (-1);
    }
  init_xml_node (newnode);
  newnode->beginparseptr = parseptr;
  newnode->endparseptr = endbufptr;
  while ((*parseptr != '<' || !isalpha (*(parseptr + 1)))
	 && parseptr < endbufptr)
    {
      if (parseptr < endbufptr - 10 && !strncmp (parseptr, "<!--", 4))
	{
	  while (strncmp (parseptr, "-->", 3) && parseptr < endbufptr - 3)
	    {
	      parseptr++;
	    }
	}
      if (parseptr < endbufptr - 3 && !strncmp (parseptr, "<![CDATA[", 9))
	{
	  while (strncmp (parseptr, "]]>", 3) && parseptr < endbufptr - 3)
	    {
	      parseptr++;
	    }
	}
      parseptr++;
    }
  parseptr++;
  nameptr = parseptr;
  while ((isalnum (*parseptr) || *parseptr == ':' || *parseptr == '-'
	  || *parseptr == '_') && parseptr < endbufptr)
    {
      parseptr++;
    }
  namelen = (int) (parseptr - nameptr);
  if (namelen < 1)
    {
      rcs_print_error ("xmlParse ... found node with no name newnode->beginparseptr=%p, parseptr=%p,*parseptr=%c\n",newnode->beginparseptr,parseptr,(*parseptr));
      xmlUnlinkNode (newnode);
      xmlFreeNode (newnode);
      return 0;
    }
  xmlChar *new_name = (xmlChar *) DEBUG_MALLOC (namelen + 1); 
  if (0 == new_name)
    {
      fprintf (stderr, "OUT of memory\n");
      exit (-1);
    }
  memcpy (new_name, nameptr, namelen);
  new_name[namelen] = 0;
  newnode->name = new_name;
  newnode->name_should_be_freed = 1;

  const char *attrnamestart = 0;
  const char *attrnameend = 0;
  const char *attrvalstart = 0;
  const char *attrvalend = 0;
  const char *attreqptr = 0;
  while (*parseptr != '>' && parseptr < endbufptr)
    {
      if (parse_attributes)
	{
	  if (attrnamestart == 0)
	    {
	      if (isalpha (*parseptr))
		{
		  attrnamestart = parseptr;
		}
	    }
	  else if (attrnameend == 0)
	    {
	      if (!isalnum (*parseptr) &&
		  *parseptr != '-' && *parseptr != '_' && *parseptr != ':')
		{
		  attrnameend = parseptr;
		}
	      if (*parseptr == '=')
		{
		  attreqptr = parseptr;
		}
	    }
	  else if (attreqptr == 0)
	    {
	      if (*parseptr == '=')
		{
		  attreqptr = parseptr;
		}
	    }
	  else if (attrvalstart == 0)
	    {
	      if (*parseptr == '\"' || *parseptr == '\'')
		{
		  attrvalstart = parseptr;
		}
	    }
	  else if (attrvalend == 0)
	    {

	      if (*parseptr == *attrvalstart)
		{
		  attrvalend = parseptr;
		}
	      if (attrvalend > attrvalstart + 1
		  && attrnameend > attrnamestart)
		{
		  long atnamelen = (long) (attrnameend-attrnamestart);
		  char *atname =
		    (char *) malloc (atnamelen + 1);
		  strncpy (atname, attrnamestart,atnamelen);
		  atname[atnamelen]=0;
		  long atvallen = (long) (attrvalend - attrvalstart-1);
		  char *atval =
		    (char *) malloc (atvallen + 1);
		  strncpy (atval, attrvalstart + 1,atvallen);
		  atval[atvallen]=0;
		  xmlAttrPtr xap = xmlSetProp (newnode, atname, atval);
		  xap->free_name=true;
		  xap->free_value=true;
		  attrvalend = 0;
		  attrvalstart = 0;
		  attrnameend = 0;
		  attrnamestart = 0;
		  attreqptr = 0;
		}
	    }
	}
      parseptr++;
    }
  if (*(parseptr - 1) == '/')
    {
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlParse .. found empty node %s\n", newnode->name);
#endif
      newnode->endparseptr = parseptr;
      return newnode;
    }
  contentptr = parseptr + 1;
  while (*parseptr && parseptr < endbufptr)
    {
      parseptr++;
      if (*parseptr == '<')
	{
	  if (*(parseptr + 1) == '/')
	    {
	      parseptr++;
	      parseptr++;
	      endtagname = parseptr;
	      while (isalnum (*parseptr) && parseptr < endbufptr)
		{
		  parseptr++;
		}
	      endtagnamelen = (long) (parseptr - endtagname);
	      if (endtagnamelen <= 0)
		{
		  rcs_print_error ("bad end tag for %s\n", newnode->name);
		  xmlUnlinkNode (newnode);
		  xmlFreeNode (newnode);
		  newnode = 0;
		  return 0;
		}
	      if (strncmp (endtagname, newnode->name, endtagnamelen))
		{
		  char *endtagcopy =
		    (char *) DEBUG_MALLOC (endtagnamelen + 1);
		  strncpy (endtagcopy, endtagname, endtagnamelen);
		  endtagcopy[endtagnamelen] = 0;
		  rcs_print_error ("Mismatched xml tags : started with <%s> and ending with </%s>\n",
				   newnode->name, endtagcopy);
		  DEBUG_FREE (endtagcopy);
		  xmlUnlinkNode (newnode);
		  xmlFreeNode (newnode);
		  newnode = 0;
		  return 0;
		}
	      if (newnode->children == 0)
		{
		  contentlen = (long) (endtagname - contentptr - 2);
		  if (contentlen > 0)
		    {
		      xmlChar *new_content=
			(xmlChar *) DEBUG_MALLOC (contentlen + 1);
		      if (0 == new_content)
			{
			  fprintf (stderr, "OUT of memory\n");
			  exit (-1);
			}
		      memcpy (new_content, contentptr, contentlen);
		      new_content[contentlen] = 0;
		      newnode->content = new_content;
		      newnode->content_should_be_freed = 1;
		    }
		}
	      while (*parseptr != '>' && parseptr < endbufptr)
		{
		  parseptr++;
		}
	      if (newnode->content)
		{
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlParse found node %s, content=%s\n",
				   newnode->name, newnode->content);
#endif
		}
	      else
		{
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlParse found node %s\n", newnode->name);
#endif
		}
	      if (newnode->children)
		{
		  if (newnode->children->name)
		    {
		      
#ifdef DEBUG_THIS_FILE
		      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				       "xmlParse the first child of %s is %s\n",
				       newnode->name,
				       newnode->children->name);
#endif
		    }
		}
	      newnode->endparseptr = parseptr;
	      return newnode;
	    }
	  else if (parseptr < endbufptr - 3 && !strncmp (parseptr, "<!--", 4))
	    {
	      while (strncmp (parseptr, "-->", 3) && parseptr < endbufptr - 3)
		{
		  parseptr++;
		}
	    }
	  else if (parseptr < endbufptr - 10 &&
		   !strncmp (parseptr, "<![CDATA[", 9))
	    {
	      while (strncmp (parseptr, "]]>", 3)
		     && parseptr < endbufptr - 10)
		{
		  parseptr++;
		}
	    }
	  else
	    {
	      xmlNodePtr newchildnode =
		xmlParseStringForNode (parseptr, endbufptr);
	      if (0 == newchildnode)
		{
		  rcs_print_error ("Bad child node parsing %s\n",
				   newnode->name);
		  xmlUnlinkNode (newnode);
		  xmlFreeNode (newnode);
		  newnode = 0;
		  return 0;
		}
	      parseptr = newchildnode->endparseptr;
	      newchildnode->parent = newnode;
	      newchildnode->prev = newnode->last;
	      if (newnode->children == 0)
		{
		  newnode->children = newchildnode;
		}
	      if (newnode->last)
		{
		  newnode->last->next = newchildnode;
		}
	      newnode->last = newchildnode;
	    }
	}
    }
  rcs_print_error ("Never found end tag for node %s\n", newnode->name);
  xmlUnlinkNode (newnode);
  xmlFreeNode (newnode);
  return 0;
}

#define  XSTRING_ALLOC_SIZE 0x4000

class xmlDumpingString
{
public:
  xmlDumpingString():
    buffer(0),endofbuf(0),ptr(0),size(0),reallocatable_buffer(true),overflow(false)
  {
    size = XSTRING_ALLOC_SIZE;
    buffer = (char *) DEBUG_MALLOC (size);
    if (0 == buffer)
      {
	fprintf (stderr, "OUT of memory\n");
	exit (-1);
      }
    ptr = buffer;
    endofbuf = buffer + size;
  };

  xmlDumpingString (char *_buffer, size_t _size):
    buffer(0),endofbuf(0),ptr(0),size(0),reallocatable_buffer(false),overflow(false)
  {
    buffer = _buffer;
    size = _size;
    ptr = buffer;
    endofbuf = buffer + size;
  }
  
  void
  setBufferAndSize(char *_buffer, size_t _size)
  {
      if(reallocatable_buffer && buffer && size > 0 && buffer != _buffer)
      {
          DEBUG_FREE(buffer);
      }
    buffer = _buffer;
    size = _size;
    ptr = buffer;
    endofbuf = buffer + size;
    reallocatable_buffer=false;
    overflow=false;
  }
  char *buffer;

private:
  const char *endofbuf;
public:
  char *ptr;
  size_t size;
  bool reallocatable_buffer;
  bool overflow;
  
public:
  inline void cat (const char *str)
  {
    char *newptr = ptr + strlen (str);
    if (newptr >= endofbuf)
      {
        
            long len = (long) (newptr - endofbuf);
            size += XSTRING_ALLOC_SIZE + len - (len % XSTRING_ALLOC_SIZE);
            size_t diff = ptr - buffer;
            if(reallocatable_buffer)
            {
                buffer = (char *) DEBUG_REALLOC (buffer, size);
            }
            else
            {
                char *old_buffer = buffer;
                buffer = (char *) DEBUG_MALLOC(size);
                memcpy(buffer,old_buffer,diff);
                reallocatable_buffer=true;
            }
            if (0 == buffer)
              {
                fprintf (stderr, "OUT of memory\n");
                exit (-1);
              }
            endofbuf = buffer + size;
            ptr = buffer + diff;
            newptr = ptr + strlen (str);
      }
    strcpy (ptr, str);
    ptr = newptr;
    
#if 0
    fprintf (stderr,
	     "-------------- xmlDumpingString::cat ----------------\n");
    fprintf (stderr, "str=%s\n", ptr, ptr);
    fprintf (stderr, "buffer=%p : ptr=%p\n", buffer, ptr);
    fprintf (stderr, "size=%d(0x%X)\n", size, size);
    fprintf (stderr, "(ptr-buffer) = %d(0x%X)\n", ((int) (ptr - buffer)),
	     ((int) (ptr - buffer)));
#endif

  };

private:
  xmlDumpingString(const xmlDumpingString &);
  xmlDumpingString &operator=(const xmlDumpingString &);
};


static void
xmlNodeDumpMemory (class xmlDumpingString * xstr, xmlNodePtr n)
{
  long namelen = 0;

  if (!n || !xstr)
    {
      return;
    }
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "xmlNodeDumpMemory(%p,%p) called.\n", xstr, n);
#endif
  if (n->name)
    {
      if (*(n->name))
	{
	  namelen = (long) strlen (n->name);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeDumpMemory(): namelen=%d\n", namelen);
#endif
	  if (namelen <= 0 || namelen > 100)
	    {
	      rcs_print_error ("xmlNodeDumpMemory: node has bad name\n");
	      return;
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlNodeDumpMemory(%s)\n",
			   n->name);
#endif

	  xstr->cat ("<");
	  if (n->ns)
	    {
	      if (n->ns->prefix && n->ns->qualify_elements)
		{
		  xstr->cat (n->ns->prefix);
		  xstr->cat (":");
		}
	    }
	  xstr->cat (n->name);
	  if (n->attributes_list)
	    {
	      xmlAttrPtr attr = (xmlAttrPtr) n->attributes_list->get_head ();
	      while (attr)
		{
		  if (attr->name && attr->value)
		    {
		      xstr->cat (" ");
		      if (n->ns)
			{
			  if (n->ns->prefix && n->ns->qualify_attributes)
			    {
			      xstr->cat (n->ns->prefix);
			      xstr->cat (":");
			    }
			}
		      xstr->cat (attr->name);
		      xstr->cat ("=\"");
		      xstr->cat (attr->value);
		      xstr->cat ("\"");
		    }
		  attr = (xmlAttrPtr) n->attributes_list->get_next ();
		}
	    }
	  if (0 == n->parent && 0 != n->ns)
	    {
	      if (n->ns->href)
		{
		  xstr->cat (" xmlns");
		  if(n->ns->prefix  && n->ns->qualify_elements)
		    {
		      xstr->cat(":");
		      xstr->cat (n->ns->prefix);
		    }
		  xstr->cat ("=\"");
		  xstr->cat (n->ns->href);
		  xstr->cat ("\"");
		}
	    }
	  if(n->start_add)
	    {
	      xstr->cat(n->start_add);
	    }
	  if (n->children == 0 && n->content == 0)
	    {
	      xstr->cat ("/>");
	      return;
	    }
	  else
	    {
	      xstr->cat (">");
	    }
	}
    }
  if (n->content)
    {
      xstr->cat (n->content);
    }
  if (n->children)
    {
      xmlNodePtr child = n->children;
      while (child != 0)
	{
	  xmlNodePtr nextchild = child->next;
	  xmlNodeDumpMemory (xstr, child);
	  child = nextchild;
	}
    }
  if (n->name)
    {
      if (*(n->name))
	{
	  xstr->cat ("</");
	  if (n->ns)
	    {
	      if (n->ns->prefix && n->ns->qualify_elements)
		{
		  xstr->cat (n->ns->prefix);
		  xstr->cat (":");
		}
	    }
	  xstr->cat (n->name);
	  xstr->cat (">");
	}
    }
}

#if 0
static void
xmlFlushNodeContent (xmlNodePtr n)
{
  if (!n)
    {
      return;
    }
  if (!n->children)
    {
      return;
    }
  xmlDumpingString *xstr = new xmlDumpingString ();
  if (n->content)
    {
      xstr->cat (n->content);
      DEBUG_FREE (n->content);
      n->content = 0;
    }
  xmlNodeDumpMemory (xstr, n);
  n->content = xstr->buffer;
  xmlNodePtr childnode = n->children;
  while (childnode)
    {
      xmlNodePtr nextchild = childnode->next;
      xmlUnlinkNode (childnode);
      xmlFreeNode (childnode);
      childnode = nextchild;
    }
  n->children = 0;
  n->last = 0;
  if (n->name && n->name_should_be_freed)
    {
      DEBUG_FREE (n->name);
    }
  n->name = 0;
  if (n->attributes_list)
    {
      n->attributes_list->delete_members ();
      delete n->attributes_list;
      n->attributes_list = 0;
    }
  xstr->buffer = 0;
  delete xstr;
}
#endif

#ifndef XML_SCHEMA_TO_HEADER

static void
xmlDocDumpMemory (const xmlDocPtr doc, xmlChar ** mem, int *size)
{
  xmlDumpingString *xstr = 0;
  if (0 == doc)
    {
      return;
    }
  if (0 == doc->rootNode)
    {
      return;
    }
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "xmlDocDumpMemory(%p,%p,%p (%d))\n", doc, mem, size,
		   (*size));
#endif

  
  if (xstr == 0)
    {
#ifndef USE_LIBXML2
      if(doc->xstr == 0)
      {
          doc->xstr = new xmlDumpingString();
      }
      xstr = doc->xstr;
#else
      xstr = new xmlDumpingString ();
#endif
    }
  xstr->setBufferAndSize(0,0);
  if (mem != 0 && size != 0)
    {
      if (*mem != 0 && *size != 0)
	{
	  xstr->setBufferAndSize((char *) (*mem), *size);
	}
    }
  if (0 == xstr)
    {
      fprintf (stderr, "OUT of memory\n");
      exit (-1);
    }
  xstr->cat ("<?xml version=\"1.0\"?>\n");
  if(doc->header)
    {
      xstr->cat(doc->header);
    }
  xmlNodeDumpMemory (xstr, doc->rootNode);
  if (0 != mem)
    {
      *mem = xstr->buffer;
    }
  if (0 != size)
    {
      *size = (int) (xstr->ptr - xstr->buffer + 1);
    }
  xstr->buffer = 0;
#ifdef USE_LIBXML2
  delete xstr;
#endif  
  return;
}

#endif 
// ifndef XML_SCHEMA_TO_HEADER

static xmlDocPtr
xmlParseMemory (const char *buffer, int size)
{
  XML_DOC *newdoc = 0;
  const char *parseptr = buffer;
  const char *endbufptr = buffer + size;
  if (size < 1)
    {
      return 0;
    }
  newdoc = new XML_DOC ();
  if (0 == newdoc)
    {
      fprintf (stderr, "OUT of memory\n");
      exit (-1);
    }
  while (*parseptr != '<' && parseptr < endbufptr)
    {
      parseptr++;
    }
  parseptr++;
  if (*parseptr == '?')
    {
      parseptr++;
      while ((*parseptr != '?' || *(parseptr + 1) != '>')
	     && parseptr < endbufptr)
	{
	  parseptr++;
	  if(*parseptr == '<' && (parseptr -buffer ) > 3 )
	    {
	      parseptr -=3;
	      rcs_print_error("Bad xml version tag.");
	      break;
	    }
	}
      if((*parseptr == '>' || *parseptr == '?' || *parseptr == '\r' || *parseptr == '\n' ) && parseptr < endbufptr)
	{
	  parseptr++;
	  if((*parseptr == '>' || *parseptr == '\r' || *parseptr == '\n') && parseptr < endbufptr)
	    {
	      parseptr++;
	    }
	}
    }
  else
    {
      parseptr--;
    }
  newdoc->rootNode = xmlParseStringForNode (parseptr, endbufptr);
  return newdoc;
}

#ifndef XML_SCHEMA_TO_HEADER

static void
xmlFreeDoc (xmlDocPtr doc)
{
  if (!doc)
    {
      return;
    }
  if (doc->rootNode)
    {
      xmlUnlinkNode (doc->rootNode);
      xmlFreeNode (doc->rootNode);
      doc->rootNode = 0;
    }
#ifndef USE_LIBXML2
  if(doc->xstr)
    {
      delete doc->xstr;
      doc->xstr=0;
    }
#endif
  delete doc;
}

static xmlNodePtr
xmlDocGetRootElement (xmlDocPtr doc)
{
  if (!doc)
    {
      return 0;
    }
  return doc->rootNode;
}


static void
xmlDocSetRootElement (xmlDocPtr doc, xmlNodePtr rootNode)
{
  if (!doc)
    {
      return;
    }
  doc->rootNode = rootNode;
}

#endif
// ifndef XML_SCHEMA_TO_HEADER


static const xmlChar *
xmlNodeListGetString (xmlDocPtr, xmlNodePtr list,int)
{
  if (!list)
    {
      return 0;
    }
  return list->content;
}

#ifndef XML_SCHEMA_TO_HEADER

static void
xmlAddChild (xmlNodePtr cur, xmlNodePtr newnode)
{
  if (!cur || !newnode)
    {
      return;
    }
  if (0 == cur->children)
    {
      cur->children = newnode;
    }
  if (cur->last)
    {
      cur->last->next = newnode;
    }
  newnode->prev = cur->last;
  cur->last = newnode;
  newnode->parent = cur;
}

#ifndef XML_SCHEMA_TO_HEADER

static void
xmlNodeAddContent (xmlNodePtr cur, xmlChar * content_to_add)
{
  xmlChar *new_content=0;
  char *orig_content=0;

  if (!cur || !content_to_add)
    {
      return;
    }
  if (*content_to_add == 0)
    {
      return;
    }
  if (cur->children == 0)
    {
      if (0 == cur->content)
	{
	  cur->content = content_to_add;
	}
      else
	{
	  orig_content = (char *)cur->content;
	  if (cur->content_should_be_freed)
	    {
	      new_content =
		(xmlChar *) DEBUG_REALLOC (orig_content,
					   strlen (cur->content) +
					   strlen (content_to_add) + 1);
	      strcat ((char*)new_content, content_to_add);
	    }
	  else
	    {
	      new_content=
		(xmlChar *) DEBUG_MALLOC (strlen (cur->content) +
					  strlen (content_to_add) + 1);
	      strcpy ((char*)new_content, orig_content);
	      strcat ((char*)new_content, content_to_add);
	      cur->content = new_content;
	      cur->content_should_be_freed = 1;
	    }
	}
    }
  else
    {
      xmlNodePtr newnode = new XML_NODE ();
      init_xml_node (newnode);
      newnode->content = content_to_add;
      xmlAddChild (cur, newnode);
    }
}

#endif
// ifndef XML_SCHEMA_TO_HEADER

static void
xmlNodeSetContent (xmlNodePtr cur, const xmlChar * content_to_set)
{
  xmlNodePtr childNode;
  xmlNodePtr nextChildNode;
  xmlChar *new_content;
  xmlChar *orig_content;

  childNode=0;
  nextChildNode=0;
  new_content=0;
  orig_content=0;

  if (!cur)
    {
      return;
    }
  if(cur->children)
    {
      childNode = cur->children;
      while(childNode)
	{
	  nextChildNode = childNode->next;
	  xmlUnlinkNode(childNode);
	  xmlFreeNode(childNode);
	  childNode = nextChildNode;
	}
      cur->children=0;
    }
  if (!content_to_set || *content_to_set == 0)
    {
      if (cur->content_should_be_freed && cur->content)
	{
	  DEBUG_FREE((void*)cur->content);
	}
      cur->content=0;
      return;
    }
  if (0 == cur->content)
    {
      cur->content = content_to_set;
    }
  else
    {
      if (cur->content_should_be_freed)
	{
	  orig_content= (xmlChar*) cur->content;
	  new_content=
	    (xmlChar *) DEBUG_REALLOC ((void*)orig_content,
				       strlen (content_to_set) + 1);
	}
      else
	{
	  new_content =
	    (xmlChar *) DEBUG_MALLOC ( strlen (content_to_set) + 1);
	  cur->content_should_be_freed = 1;
	}
      if(new_content)
	{
	  strcpy ((char*)new_content, content_to_set);
	  cur->content = new_content;
	}
    }
}

static xmlNodePtr
xmlNewTextChild (xmlNodePtr parent,
		 xmlNsPtr ns, const xmlChar * name, const xmlChar * content)
{
  xmlNodePtr newnode = new XML_NODE ();
  init_xml_node (newnode);
  if (name)
    {
      if (*name)
	{
	  newnode->name = name;
	}
    }
  if (content)
    {
      if (*content)
	{
	  newnode->content = content;
	}
    }
  newnode->ns = ns;
  xmlAddChild (parent, newnode);
  return newnode;
}


static xmlNodePtr
xmlNewComment (xmlChar * content)
{
  if (!content)
    {
      return 0;
    }
  xmlChar *new_content;
  new_content=0;
  XML_NODE *newnode = new XML_NODE ();
  init_xml_node (newnode);
  new_content = (xmlChar *) DEBUG_MALLOC (strlen (content) + 8);
  newnode->content_should_be_freed = 1;
  strcpy ((char*)new_content, "<!--");
  strcat ((char*)new_content, content);
  strcat ((char*)new_content, "-->");
  newnode->content = new_content;
  return newnode;
}

#endif
// ifndef XML_SCHEMA_TO_HEADER

#ifndef OE_DM

static const xmlChar *
xmlGetProp (xmlNodePtr node, const xmlChar * name)
{
  if (!node || !name)
    {
      return 0;
    }
  if (0 == node->attributes_list)
    {
      return 0;
    }
  xmlAttrPtr attr = (xmlAttrPtr) node->attributes_list->get_head ();
  while (attr)
    {
      if (!strcmp (attr->name, name))
	{
	  return attr->value;
	}
      attr = (xmlAttrPtr) node->attributes_list->get_next ();
    }
  return 0;
}

// ! OE_DM
#endif


#ifndef XML_SCHEMA_TO_HEADER

static xmlDocPtr
xmlNewDoc (const xmlChar *)
{
  XML_DOC *doc = new XML_DOC ();
  doc->rootNode = 0;
  doc->header=0;
  return doc;
}

static xmlNodePtr
xmlNewDocRawNode (xmlDocPtr doc,
		  xmlNsPtr ns, xmlChar * name, xmlChar * content)
{
  if (!doc)
    {
      return 0;
    }
  doc->rootNode = new XML_NODE ();
  init_xml_node (doc->rootNode);
  if (name)
    {
      doc->rootNode->name = name;
    }
  if (content)
    {
      doc->rootNode->content = content;
    }
  doc->rootNode->ns = ns;
  return doc->rootNode;
}

static xmlNsPtr
xmlNewNs (xmlNodePtr node, xmlChar * href, xmlChar * prefix)
{
  XML_NS *ns = new XML_NS ();
  ns->node = node;
  ns->href = href;
  ns->prefix = prefix;
  ns->qualify_elements=false;
  ns->qualify_attributes=false;
  return ns;
}

static void
xmlSetNs (xmlNodePtr node, xmlNsPtr ns)
{
  if (node)
    {
      node->ns = ns;
    }
  if (ns)
    {
      if (0 == ns->node)
	{
	  ns->node = node;
	}
    }
}

static void
xmlFreeNs (xmlNsPtr cur)
{
  if (cur)
    {
      if (cur->node)
	{
	  if (cur->node->ns == cur)
	    {
	      cur->node = 0;
	    }
	}
      delete cur;
    }
}

#endif
// ifndef XML_SCHEMA_TO_HEADER

#endif

#ifdef XML_SCHEMA_TO_HEADER

static int
sumname (const char *name)
{
  int sum = 0;
  int i = 0;
  while (name[i])
    {
      sum += name[i] * (i % 2 ? 100 : 1);
      i++;
    }
  return sum;
}

#ifndef OE_DM

static RCS_LINKED_LIST *definedClasses = 0;
static RCS_LINKED_LIST *definedEnums = 0;
static RCS_LINKED_LIST *definedNMLmsgs = 0;
static RCS_LINKED_LIST *definedBasicTypes = 0;
static RCS_LINKED_LIST *definedUnions = 0;

static int
isClass (const char *name)
{
  if (!name || !definedClasses)
    {
      return 0;
    }
  char *nameforcmp = (char *) definedClasses->get_head ();
  while (nameforcmp)
    {
      if (!strcmp (name, nameforcmp))
	{
	  return 1;
	}
      nameforcmp = (char *) definedClasses->get_next ();
    }
  return 0;
}

static void
setDefinedClass (const char *name)
{
  rcs_print_debug(1,"setDefinedClass(%s)\n", name);
  if(0 == name)
    {
      return;
    }
  if (0 == definedClasses)
    {
      definedClasses = new RCS_LINKED_LIST ();
    }
  definedClasses->store_at_tail ((void *) name, strlen (name) + 1, 1);
}

static int
isEnum (const char *name)
{
  if (!name || !definedEnums)
    {
      return 0;
    }
  char *nameforcmp = (char *) definedEnums->get_head ();
  while (nameforcmp)
    {
      if (!strcmp (name, nameforcmp))
	{
	  return 1;
	}
      nameforcmp = (char *) definedEnums->get_next ();
    }
  return 0;
}

static void
setDefinedEnum (const char *name)
{
  rcs_print_debug(1,"setDefinedEnum(%s)\n", name);
  if(0 == name)
    {
      return;
    }
  if (0 == definedEnums)
    {
      definedEnums = new RCS_LINKED_LIST ();
    }
  definedEnums->store_at_tail ((void *) name, strlen (name) + 1, 1);
}


static int
isUnion (const char *name)
{
  if (!name || !definedUnions)
    {
      return 0;
    }
  char *nameforcmp = (char *) definedUnions->get_head ();
  while (nameforcmp)
    {
      if (!strcmp (name, nameforcmp))
	{
	  return 1;
	}
      nameforcmp = (char *) definedUnions->get_next ();
    }
  return 0;
}

static void
setDefinedUnion (const char *name)
{
  rcs_print_debug(1,"setDefinedUnion(%s)\n", name);
  if(0 == name)
    {
      return;
    }
  if (0 == definedUnions)
    {
      definedUnions = new RCS_LINKED_LIST ();
    }
  definedUnions->store_at_tail ((void *) name, strlen (name) + 1, 1);
}


static int
isNMLmsg (const char *name)
{
  if (!name || !definedNMLmsgs)
    {
      return 0;
    }
  char *nameforcmp = (char *) definedNMLmsgs->get_head ();
  while (nameforcmp)
    {
      if (!strcmp (name, nameforcmp))
	{
	  return 1;
	}
      nameforcmp = (char *) definedNMLmsgs->get_next ();
    }
  return 0;
}

static void
setDefinedNMLmsg (const char *name)
{
  rcs_print_debug(1,"setDefinedNMLmsg(%s)\n", name);
  if (0 == definedNMLmsgs)
    {
      definedNMLmsgs = new RCS_LINKED_LIST ();
    }
  definedNMLmsgs->store_at_tail ((void *) name, strlen (name) + 1, 1);
}

static int
isBasicType (const char *name)
{
  if (!name || !definedBasicTypes)
    {
      return 0;
    }
  char *nameforcmp = (char *) definedBasicTypes->get_head ();
  while (nameforcmp)
    {
      if (!strcmp (name, nameforcmp))
	{
	  return 1;
	}
      nameforcmp = (char *) definedBasicTypes->get_next ();
    }
  return 0;
}

static void
setDefinedBasicType (const char *name)
{
  rcs_print_debug(1,"setDefinedBasicType(%s)\n", name);
  if (0 == definedBasicTypes)
    {
      definedBasicTypes = new RCS_LINKED_LIST ();
    }
  definedBasicTypes->store_at_tail ((void *) name, strlen (name) + 1, 1);
}

static int
alreadyDefined (const char *name)
{
  if (isClass (name))
    {
      return 1;
    }
  if (isUnion (name))
    {
      return 1;
    }
  if (isEnum (name))
    {
      return 1;
    }
  return 0;
}

static int use_unbounded_arrays = 1;

// OE_DM
#endif




static char *shortfname;
static const char *xsd_ns="xsd";
xmlDocPtr currentDoc =0;


int xmlNameCompare(const char *fullname, const char *ns, const char *name)
{
  int c1;
  const char *cptr;
  int nslen;

  c1 =0;
  cptr=fullname;
  nslen=0;

  if(ns)
    {
      nslen = strlen(ns);
      c1 = strncmp(fullname,ns,nslen);
      if(c1)
	{
	  return c1;
	}
      cptr += nslen;
      if(*cptr != ':')
	{
	  return (*cptr - ':');
	}
      cptr++;
    }
  return strcmp(cptr,name);
}

#ifndef OE_DM
      
xmlNodePtr lastRefNode =0;

xmlNodePtr lookUpRefNode(const char *refprop)
{
  xmlNodePtr n2;
  const char *nameprop2;

  nameprop2=0;
  n2 = 0;
  
  if(lastRefNode)
    {
      nameprop2 = (const char *) xmlGetProp(lastRefNode,"name");
      if(!strcmp(nameprop2,refprop))
	{
	  return lastRefNode;
	}
    }
  lastRefNode=0;
  if(refprop && currentDoc &&  
     ( isBasicType(refprop) || isNMLmsg(refprop)))
    {
      n2  = currentDoc->rootNode->children;
      while (n2)
	{
	  if (n2->name)
	    {
	      rcs_print_debug(1,"n2->name=%s\n", n2->name);
	      if (!xmlNameCompare(n2->name,xsd_ns,"element"))
		{
		  nameprop2 = (const char *) xmlGetProp (n2, "name");
		  rcs_print_debug(1,"nameprop2=%s\n", nameprop2);
		  if (nameprop2)
		    {
		      if(!strcmp(nameprop2, refprop))
			{
			  lastRefNode = n2;
			  return n2;
			}
		    }
		}
	    }
	  n2 = n2->next;
	}
    }
  return 0;
}

static const char *
declareVarString (const char *type, const char *name, xmlNodePtr n)
{
  char tempbuf[256];
  char temptype[256];
  char temptype2[256];
  int string_type;
  const char *baseprop;
  const char *simple_type_name;
  xmlNodePtr nc1;
  int type_length_val;
  int type_max_length_val;
  const char *maxoccursprop;
  const char *minoccursprop;
  int maxoccurs;
  int minoccurs;
  const char *refprop;
  const char *orig_type;
  const char *typeprop2;
  const char *valueprop;
  xmlNodePtr refNode;

  if (!n || !name)
    {
      return 0;
    }
  
  simple_type_name=0;
  type_length_val = -1;
  type_max_length_val = -1;
  string_type=0;
  baseprop=0;
  maxoccursprop=0;
  minoccursprop=0;
  maxoccurs=1;
  minoccurs=1;
  refNode = 0;
  refprop =0;
  typeprop2=0;
  valueprop=0;
  orig_type = type;

  if (!n || !name)
    {
      return 0;
    }

  refprop = (const char *) xmlGetProp(n,"ref");
  if(refprop)
    {
      refNode = lookUpRefNode(refprop);
      if(refNode)
	{
	  typeprop2 = (const char *) xmlGetProp(refNode,"type");
	  if(typeprop2)
	    {
	      type=typeprop2;
	    }
	}
    }

  nc1 = n->children;
  while (nc1)
    {
      if (nc1->name)
	{
	  if (!xmlNameCompare(nc1->name,xsd_ns,"simpleType"))
	    {
	      xmlNodePtr nc2 = nc1->children;
	      while (nc2)
		{
		  if (nc2->name)
		    {
		      if (!xmlNameCompare(nc2->name, xsd_ns,"extension")
			  || !xmlNameCompare(nc2->name, xsd_ns,"restriction"))
			{
			  baseprop = (const char *) xmlGetProp (nc2, "base");
			  if (baseprop && !type)
			    {
			      type = baseprop;
			    }
			  xmlNodePtr nc3 = nc2->children;
			  while (nc3)
			    {
			      if (nc3->name)
				{
				  if (!xmlNameCompare(nc3->name,xsd_ns,"maxLength"))
				    {
				      valueprop  = (const char *)
					xmlGetProp (nc3, "value");
				      if (valueprop)
					{
					  type_max_length_val =
					    strtol (valueprop, 0, 10);
					}
				    }
				  if (!xmlNameCompare(nc3->name,xsd_ns,"length"))
				    {
				      valueprop = (const char *)
					xmlGetProp (nc3, "value");
				      if (valueprop)
					{
					  type_length_val =
					    strtol (valueprop, 0, 10);
					}
				    }
				}
			      nc3 = nc3->next;
			    }
			  break;
			}
		    }
		  nc2 = nc2->next;
		}
	    }
	}
      nc1 = nc1->next;
    }


  if(refNode)
    {
      nc1 = refNode->children;
      while (nc1)
	{
	  if (nc1->name)
	    {
	      if (!strcmp (nc1->name, "xsd:simpleType")
		  || !strcmp (nc1->name, "simpleType"))
		{
		  xmlNodePtr nc2 = nc1->children;
		  while (nc2)
		    {
		      if (nc2->name)
			{
			  if (!xmlNameCompare(nc2->name, xsd_ns,"extension")
			      || !xmlNameCompare(nc2->name, xsd_ns,"restriction"))
			    {
			      baseprop = xmlGetProp (nc2, "base");
			      if (baseprop)
				{
				  type = baseprop;
				}
			      xmlNodePtr nc3 = nc2->children;
			      while (nc3)
				{
				  if (nc3->name)
				    {
				      if (!xmlNameCompare(nc3->name,xsd_ns,"maxLength"))
					{
					  valueprop = (const char *)
					    xmlGetProp (nc3, "value");
					  if (valueprop)
					    {
					      type_max_length_val =
						strtol (valueprop, 0, 10);
					    }
					}
				      if (!xmlNameCompare(nc3->name,xsd_ns,"length"))
					{
					  valueprop = (const char *)
					    xmlGetProp (nc3, "value");
					  if (valueprop)
					    {
					      type_length_val =
						strtol (valueprop, 0, 10);
					    }
					}
				    }
				  nc3 = nc3->next;
				}
			      break;
			    }
			}
		      nc2 = nc2->next;
		    }
		}
	    }
	  nc1 = nc1->next;
	}
    }

  if(n->children && n->children->name 
     && n->children->children && n->children->children->name)
    {
      if(!xmlNameCompare(n->children->name,xsd_ns,"simpleType"))
	{
	  if(!xmlNameCompare(n->children->children->name,xsd_ns,"extension")
	     || !xmlNameCompare(n->children->children->name,xsd_ns,"restriction"))
	    {
	      baseprop = xmlGetProp(n->children->children,"base");
	      if(baseprop)
		{
		  type=baseprop;
		}
	    }
	}
    }

  if(refNode && refNode->children && refNode->children->name 
     && refNode->children->children && refNode->children->children->name)
    {
      if(!xmlNameCompare(refNode->children->name,xsd_ns,"simpleType"))
	{
	  if(!xmlNameCompare(refNode->children->children->name,xsd_ns,"extension")
	     || !xmlNameCompare(refNode->children->children->name,xsd_ns,"restriction"))
	    {
	      baseprop = xmlGetProp(refNode->children->children,"base");
	      if(baseprop)
		{
		  type=baseprop;
		}
	    }
	}
    }
	  
  if(n->children && !n->children->children && !n->children->next)
    {
      if(n->children->name && 
	 !xmlNameCompare(n->children->name,xsd_ns,"complexType") &&
	 ! n->children->attributes_list)
	{
	  strcpy(temptype,xsd_ns);
	  strcat(temptype,":string");
	  type = (char *) temptype;
	}
    }

  if(!type)
    {
      return 0;
    }

  if(isEnum(type))
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(temptype2,sizeof(temptype2)),
		      "enum %sEnum",type);
      simple_type_name = temptype2;
    }
  else if(isEnum(orig_type))
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(temptype2,sizeof(temptype2)),
		      "enum %sEnum",orig_type);
      simple_type_name = temptype2;
    }
  else if (!xmlNameCompare(type,xsd_ns,"dateTime"))
    {
      simple_type_name = ("CMS_DATE_TIME");
    }
  else if (!xmlNameCompare(type,xsd_ns,"time"))
    {
      simple_type_name = ("CMS_TIME");
    }
  else if (!xmlNameCompare(type,xsd_ns,"date"))
    {
      simple_type_name = ("CMS_DATE");
    }
  else if (!xmlNameCompare(type,xsd_ns,"anyURI"))
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(tempbuf,sizeof(tempbuf)),
		      "char %s[%s_URI_STRING_LENGTH]; /*default=http://www.nist.gov*/",
		      name, shortfname);
      return strdup (tempbuf);
    }
  else if (!xmlNameCompare(type,xsd_ns,"duration"))
    {
      simple_type_name = ("CMS_DURATION");
    }
  else if (!xmlNameCompare(type,xsd_ns,"anySimpleType"))
    {
      simple_type_name = ("char");
      string_type = 1;
    }
  else if (!xmlNameCompare(type,xsd_ns,"string"))
    {
      simple_type_name = ("char");
      string_type = 1;
    }
  else if (!xmlNameCompare(type,xsd_ns,"normalizedString"))
    {
      simple_type_name = ("char");
      string_type = 1;
    }
  else if (!xmlNameCompare(type,xsd_ns,"hexBinary"))
    {
      simple_type_name = ("unsigned char");
      string_type = 1;
    }
  else if (!xmlNameCompare(type,xsd_ns,"unsignedByte"))
    {
      simple_type_name = ("unsigned char");

    }
  else if (!xmlNameCompare(type,xsd_ns,"byte") )
    {
      simple_type_name = ("char");

    }
  else if (!xmlNameCompare(type,xsd_ns,"boolean") )
    {
      simple_type_name = ("bool");

    }
  else if (!xmlNameCompare(type,xsd_ns,"integer") )
    {
      simple_type_name = ("int");

    }
  else if (!xmlNameCompare(type,xsd_ns,"positiveInteger"))
    {
      simple_type_name = ("unsigned int");

    }
  else if (!xmlNameCompare(type,xsd_ns,"nonNegativeInteger"))
    {
      simple_type_name = ("unsigned int");

    }
  else if (!xmlNameCompare(type,xsd_ns,"nonPositiveInteger"))
    {
      simple_type_name = ("int");

    }
  else if (!xmlNameCompare(type,xsd_ns,"short") )
    {
      simple_type_name = ("short");

    }
  else if (!xmlNameCompare(type,xsd_ns,"unsignedShort"))
    {
      simple_type_name = ("unsigned short");

    }
  else if (!xmlNameCompare(type,xsd_ns,"int") )
    {
      simple_type_name = ("int");

    }
  else if (!xmlNameCompare(type,xsd_ns,"unsignedInt"))
    {
      simple_type_name = ("unsigned int");

    }
  else if (!xmlNameCompare(type,xsd_ns,"long") )
    {
      simple_type_name = ("long");

    }
  else if (!xmlNameCompare(type,xsd_ns,"unsignedLong"))
    {
      simple_type_name = ("unsigned long");

    }
  else if (!xmlNameCompare(type,xsd_ns,"decimal") )
    {
      simple_type_name = ("double");

    }
  else if (!xmlNameCompare(type,xsd_ns,"double") )
    {
      simple_type_name = ("double");

    }
  else if (!xmlNameCompare(type,xsd_ns,"float") )
    {
      simple_type_name = ("float");
    }
  else
    {
      return 0;
    }

  if (!simple_type_name)
    {
      return 0;
    }

  maxoccursprop = (const char *) xmlGetProp (n, "maxOccurs");
  minoccursprop = (const char *) xmlGetProp (n, "minOccurs");
  maxoccurs = 1;
  minoccurs = 1;
  if (maxoccursprop)
    {
      if (!strcmp (maxoccursprop, "unbounded"))
	{
	  maxoccurs = -1;
	}
      else
	{
	  maxoccurs = strtol (maxoccursprop, 0, 10);
	}
    }
  if (minoccursprop)
    {
      minoccurs = strtol (minoccursprop, 0, 10);
    }
  if (string_type)
    {
      if (type_max_length_val < 1 && type_length_val < 1)
	{
	  if ((!xmlNameCompare(n->name,xsd_ns,"element")
	       || !xmlNameCompare(n->name,xsd_ns,"attribute"))
	      && use_unbounded_arrays)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(tempbuf,sizeof(tempbuf)),
			      "DECLARE_NML_UNBOUNDED_ARRAY(%s,%s);",
		       simple_type_name, name);
	    }
	  else
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(tempbuf,sizeof(tempbuf)),
			      "%s %s[%s_STRING_LENGTH];", simple_type_name,
		       name, shortfname);
	    }
	  return strdup(tempbuf);
	}
      else if (type_length_val > type_max_length_val)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(tempbuf,sizeof(tempbuf)),
			  "%s %s[%d];", simple_type_name, name,
			  type_length_val);
	  return strdup(tempbuf);
	}
      else
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(tempbuf,sizeof(tempbuf)),
		   "%s %s[%d];", simple_type_name, name,
		   type_max_length_val);
	  return strdup(tempbuf);
	}
    }
  if (maxoccurs == 1)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(tempbuf,sizeof(tempbuf)),
	       "%s %s;", simple_type_name, name);
      return strdup(tempbuf);
    }
  else if (maxoccurs == -1)
    {
      if (use_unbounded_arrays)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(tempbuf,sizeof(tempbuf)),
			  "DECLARE_NML_UNBOUNDED_ARRAY(%s,%s);",
			  simple_type_name, name);
	}
      else
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(tempbuf,sizeof(tempbuf)),
		   "DECLARE_NML_DYNAMIC_LENGTH_ARRAY(%s,%s,%s_UNBOUNDED_LENGTH);",
		   simple_type_name, name, shortfname);
	}
      return strdup(tempbuf);
    }
  else if (maxoccurs > 1)
    {
      if (minoccurs == maxoccurs)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(tempbuf,sizeof(tempbuf)),
			  "%s %s[%d];", simple_type_name, name, maxoccurs);
	  return strdup(tempbuf);
	}
      else
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(tempbuf,sizeof(tempbuf)),
			  "DECLARE_NML_DYNAMIC_LENGTH_ARRAY(%s,%s,%d);",
			  simple_type_name, name, maxoccurs);
	  return strdup(tempbuf);
	}
    }
  return 0;
}

char *
fixname (const char *name,  const char *type, int *changed)
{
  if (!name)
    {
      return 0;
    }
  int was_changed = 0;
  char *newname = strdup (name);
  char *nextnewname;
  char *colonptr = strrchr (newname, ':');
  if (colonptr)
    {
      was_changed = 1;
      newname = colonptr + 1;
    }
  char *dashptr = strchr (newname, '-');
  while (dashptr)
    {
      was_changed = 1;
      *dashptr = '_';
      dashptr = strchr (dashptr + 1, '-');
    }
  if (!strcmp (newname, "operator"))
    {
      free(newname);
      newname = strdup("operator_var");
      was_changed = 1;
    }
  else if (!strcmp (newname, "default"))
    {
      free(newname);
      newname = strdup("default_var");
      was_changed = 1;
    }
  else if(! type )
    {
      nextnewname = (char *) malloc(strlen(newname)+ 20);
      strcpy(nextnewname, newname);
      strcat(nextnewname,"_var");
      was_changed=1;
      newname=nextnewname;
    }
  else if(!strcmp(type,newname))
    {
      nextnewname = (char *) malloc(strlen(newname)+ 20);
      strcpy(nextnewname, newname);
      strcat(nextnewname,"_var");
      was_changed=1;
      newname=nextnewname;
    }
  if (changed)
    {
      *changed = was_changed;
    }
  return newname;
}


char *
fixenumvaluename (const char *name, int *changed)
{
  if (!name)
    {
      return 0;
    }
  int was_changed = 0;
  char *newname = strdup (name);
  char *nextnewname;
  char *colonptr = strrchr (newname, ':');
  if (colonptr)
    {
      was_changed = 1;
      newname = colonptr + 1;
    }
  char *dashptr = strchr (newname, '-');
  while (dashptr)
    {
      was_changed = 1;
      *dashptr = '_';
      dashptr = strchr (dashptr + 1, '-');
    }
  char *slashptr = strchr (newname, '/');
  while (slashptr)
    {
      was_changed = 1;
      *slashptr = '_';
      slashptr = strchr (slashptr + 1, '/');
    }
  char *periodptr = strchr (newname, '.');
  while (periodptr)
    {
      was_changed = 1;
      *periodptr = '_';
      periodptr = strchr (periodptr + 1, '.');
    }
  if(*newname >= '0' && *newname <= '9')
    {
      nextnewname = (char *) malloc(strlen(newname)+10);
      strcpy(nextnewname,"enum_val_");
      strcat(nextnewname,newname);
      newname=nextnewname;
      was_changed=1;
    }
  if (!strcmp (newname, "operator"))
    {
      free(newname);
      newname = strdup("operator_enum_val");
      was_changed = 1;
    }
  else if (!strcmp (newname, "default"))
    {
      free(newname);
      newname = strdup("default_enum_val");
      was_changed = 1;
    }
  if (changed)
    {
      *changed = was_changed;
    }
  return newname;
}



void
writeCppCommentForXmlNode (xmlDocPtr d, FILE * f, xmlNodePtr n, const char *tabstring)
{
  xmlNodePtr child;
  xmlNodePtr grandchild;
  char *documentation;
  char *orig_documentation;
  char *enddocumentation;
  char *line;
  const char *nameprop;
  if(!n || !f)
    {
      return;
    }
  documentation=0;
  enddocumentation=0;
  line=0;
  grandchild=0;
  orig_documentation=0;
  nameprop=0;

  child=n->children;
  while(child)
    {
	  if(child->name && !xmlNameCompare(child->name,xsd_ns,"annotation"))
	    {
	      grandchild = child->children;
	      while(grandchild)
		{
		  if(grandchild->name && !xmlNameCompare(grandchild->name,xsd_ns,"documentation"))
		    {
		      documentation = orig_documentation = strdup((char *) xmlNodeListGetString(d,grandchild,0));
		      if(documentation)
			{
			  while(*documentation && isspace(*documentation))
			    {
			      documentation++;
			    }
			  enddocumentation = documentation + strlen(documentation) -1;
			  while(isspace(*enddocumentation) && enddocumentation > documentation)
			    {
			      *enddocumentation =0;
			      enddocumentation--;
			    }
			  nameprop = (const char *) xmlGetProp(n,"name");
			  line = strtok(documentation,"\r\n*");
			  if(nameprop || line)
			    {
			      fprintf(f,"\n");
			    }
			  if(nameprop)
			    {
			      if(tabstring)
				{
				  fprintf(f,"%s",tabstring);
				}
			      fprintf(f,"// %s:\n",nameprop);
			    }
			  while(line)
			    {
			      if(tabstring)
				{
				  fprintf(f,"%s",tabstring);
				}
			      fprintf(f,"// %s\n",line);
			      line = strtok(0,"\r\n*");
			    }
			  if(orig_documentation)
			    {
			      free(orig_documentation);
			    }
			    documentation=0;
			    enddocumentation=0;
			    line=0;
			    orig_documentation=0;
			}
		      break;
		    }
		  grandchild=grandchild->next;
		}
	      break;
	    }
	  child=child->next;
    }
}

// ! OE_DM
#endif


#ifdef OE_DM
void
writeCppDataItem(xmlDocPtr d, FILE *f, xmlNodePtr n, 
		 const char *componentName,
		 const char *componentType,
		 long component_dim,
		 bool component_has_record)
{
  if(!d || !f || !n )
    {
      return;
    }

  if(!componentName)
    {
      rcs_print_error("component has no name.\n");
      return;
    }
  rcs_print_debug(1,"writeCppDataItem( . . . , n->name=%s, componentName=%s, . . .)\n",n->name,componentName);

  if(component_has_record)
    {
      fprintf(f,"\tstruct %sStruct %s",componentName,componentName);
    }
  else if(componentType == 0)
    {
      rcs_print_error("Component has no type %s\n",componentName);
    }
  else if (!strcmp(componentType,"Boolean"))
    {
      fprintf(f,"\tbool %s",componentName);
    }
  else if (!strcmp(componentType,"Character"))
    {
      fprintf(f,"\tchar %s",componentName);
    }
  else if (!strcmp(componentType,"Unsigned Character"))
    {
      fprintf(f,"\tunsigned char %s",componentName);
    }
  else if (!strcmp(componentType,"Short Integer"))
    {
      fprintf(f,"\tshort int %s",componentName);
    }
  else if (!strcmp(componentType,"Unsigned Short Integer"))
    {
      fprintf(f,"\tunsigned short int %s",componentName);
    }
  else if (!strcmp(componentType,"Integer"))
    {
      fprintf(f,"\tint %s",componentName);
    }
  else if (!strcmp(componentType,"Unsigned Integer"))
    {
      fprintf(f,"\tunsigned int %s",componentName);
    }
  else if (!strcmp(componentType,"Long Integer"))
    {
      fprintf(f,"\tlong int %s",componentName);
    }
  else if (!strcmp(componentType,"Unsigned Long Integer"))
    {
      fprintf(f,"\tunsigned long int %s",componentName);
    }
  else if (!strcmp(componentType,"Float"))
    {
      fprintf(f,"\tfloat %s",componentName);
    }
  else if (!strcmp(componentType,"Long Float"))
    {
      fprintf(f,"\tdouble %s",componentName);
    }
  else
    {
      rcs_print_error("Bad component type %s\n",componentType);
    }

  if(component_dim > 0)
    {
      fprintf(f,"[%ld]",component_dim);
    }
  fprintf(f,";\n");
}

void
writeCppStructMembersForXMLNode(xmlDocPtr d,FILE *f,xmlNodePtr n)
{
  if(!d || ! f || !n)
    {
      return;
    }
  xmlNodePtr msgdefNode = 0;
  xmlNodePtr recordNode = 0;
  xmlNodePtr msgdefChild = 0;
  xmlNodePtr componentNode=0;
  const char *componentName=0;
  const char *componentType=0;
  const char *componentDimStr=0;
  xmlNodePtr componentChildNode = 0;
  long component_dim=0;

  if(!xmlNameCompare(n->name,xsd_ns,"msg_def") ||
     !xmlNameCompare(n->name,xsd_ns,"component"))
    {
      msgdefNode = n;
    }
  if(msgdefNode)
    {
      msgdefChild = msgdefNode->children;
      while(msgdefChild)
	{
	  if(!xmlNameCompare(msgdefChild->name,xsd_ns,"record"))
	    {
	      recordNode = msgdefChild;
	      break;
	    }
	  msgdefChild = msgdefChild->next;
	}
    }
  else if(!xmlNameCompare(n->name,xsd_ns,"record"))
    {
      recordNode = n;
    }
  if(!recordNode)
    {
      rcs_print_error("writeCppStructMembersForXMLNode: bad node no record member.\n");
      return;
    }
  componentNode = recordNode->children;
  bool component_has_record=false;
  while(componentNode)
    {
      componentName=0;
      componentType=0;
      componentDimStr=0;
      component_dim=0;
      component_has_record=false;
      componentChildNode = componentNode->children;
      while(componentChildNode)
	{
	  if(!xmlNameCompare(componentChildNode->name,xsd_ns,"name"))
	    {
	      componentName = xmlNodeListGetString(d,componentChildNode,0);
	    }
	  else if(!xmlNameCompare(componentChildNode->name,xsd_ns,"type"))
	    {
	      componentType = xmlNodeListGetString(d,componentChildNode,0);
	    }
	  else if(!xmlNameCompare(componentChildNode->name,xsd_ns,"dimension"))
	    {
	      componentDimStr = xmlNodeListGetString(d,componentChildNode,0);
	      component_dim = strtol(componentDimStr,0,0);
	    }
	  else if(!xmlNameCompare(componentChildNode->name,xsd_ns,"record"))
	    {
	      component_has_record=true;
	    }
	  componentChildNode = componentChildNode->next;
	}
      writeCppDataItem(d,f,n, componentName,componentType,component_dim,
		       component_has_record);
      componentNode = componentNode->next;
    }
}


void
writeCppStructForXMLNode (xmlDocPtr d, FILE * f, xmlNodePtr n)
{
  if(!d || ! f || !n)
    {
      return;
    }
  bool is_msg_def=false;
  xmlNodePtr child=0;

  if(!xmlNameCompare(n->name,xsd_ns,"msg_def_list"))
    {
      if(d->rootNode == n && n->parent == 0)
	{
	  child = n->children;
	  while(child)
	    {
	      writeCppStructForXMLNode(d,f,child);
	      child = child->next;
	    }
	  return;
	}
      else
	{
	  rcs_print_error("msg_def_list should only be the root node.\n");
	}
    }
  else if(d->rootNode == n || n->parent == 0)
    {
      rcs_print_error("root node is %s  not msg_def_list.\n", n->name);
      return;
    }
  else if(!xmlNameCompare(n->name,xsd_ns,"msg_def") && n->children != 0)
    {
      is_msg_def=true;
    }
  else if(xmlNameCompare(n->name,xsd_ns,"component") && n->children != 0)
    {
      rcs_print_error("writeCppStructMembersForXMLNode: bad node: n->name=%s\n",
		      n->name);
    }
  child = n->children;
  const char *name=0;
  while(child)
    {
      if(!xmlNameCompare(child->name,xsd_ns,"name"))
	{
	  name = xmlNodeListGetString(d,child,0);
	  break;
	}
      child = child->next;
    }
  if(!name)
    {
      rcs_print_error("No name for msg_def\n");
      return;
    }

  rcs_print_debug(1,"writeCppStructForXMLNode : name=%s\n",name);

  child=n->children;
  xmlNodePtr grandchild=0;
  while(child)
    {
      if(child->name)
	{
	  rcs_print_debug(1,"child->name=%s\n",child->name);
	}
      grandchild=child->children;
      while(grandchild)
	{
	  if(grandchild->name)
	    {
	      rcs_print_debug(1,"grandchild->name=%s\n",grandchild->name);
	    }
	  xmlNodePtr ggcNode=0;
	  if(!xmlNameCompare(grandchild->name,xsd_ns,"component"))
	    {
	      ggcNode = grandchild->children;
	      const char *gcName=0;
	      bool gc_node_has_record=false;
	      while(ggcNode)
		{
		  if(ggcNode->name)
		    {
		      rcs_print_debug(1,"ggcNode->name=%s\n",ggcNode->name);
		    }
		  if(!xmlNameCompare(ggcNode->name,xsd_ns,"record"))
		    {
		      gc_node_has_record=true;
		    }
		  if(!xmlNameCompare(ggcNode->name,xsd_ns,"name"))
		    {
		      gcName = xmlNodeListGetString(d,ggcNode,0);
		    }
		  ggcNode= ggcNode->next;
		}
	      if(gc_node_has_record)
		{
		  rcs_print_debug(1,"write out extra struct definition for %s\n",gcName);
		  if(!gcName)
		    {
		      rcs_print_error("record has no name\n");
		    }
		  else
		    {
		      writeCppStructForXMLNode(d,f,grandchild);
		    }
		}
	    }
	  grandchild=grandchild->next;
	}
      child=child->next;
    }

  if(is_msg_def)
    {
      fprintf(f,"class OE_Data_Message_%s: public nml_oe::OE_Data_Message\n",name);
      fprintf(f,"{\n");
      fprintf(f,"public:\n");
      fprintf(f,"\tOE_Data_Message_%s(void): nml_oe::OE_Data_Message(%s_TYPE,sizeof(OE_Data_Message_%s),%s_format) {};\n",name,name,name,shortfname);
      fprintf(f,"\tvoid update(CMS *);\n");
      fprintf(f,"\n");
      fprintf(f,"\t// Data members.\n");
    }
  else
    {
      fprintf(f,"struct %sStruct\n",name);
      fprintf(f,"{\n");
    }
  writeCppStructMembersForXMLNode(d,f,n);
  fprintf(f,"\n");
  fprintf(f,"};\n");
  fprintf(f,"\n");  
}

// OE_DM
#else
  

void
writeCppStructForXMLNode (xmlDocPtr d, FILE * f, xmlNodePtr n)
{
  const char *simpletypename;
  char namebuf[512];
  char namebuf2[512];
  xmlNodePtr ctn;
  xmlNodePtr parent;
  const char *pnameprop;
  const char *ptypeprop;
  const char *nnameprop;
  const char *ntypeprop;
  const char *dcnameprop;
  xmlNodePtr dc;
  const char *ctnnameprop;
  const char *nameprop;
  xmlNodePtr seqn;
  xmlNodePtr cxcn;
  const char *derivedfrom ;
  xmlNodePtr exn;
  xmlNodePtr origSeqn;
  xmlNodePtr eln;
  int name_changed;
  const char *eltypeprop;
  const char *elnameprop;
  const char *elrefprop;
  const char *fixedelnameprop;
  const char *vardeclare;
  const char *atttypeprop;
  const char *attnameprop;
  const char *fixedattnameprop;
  const char *nameprop3;
  xmlNodePtr n3;
  int levels;
  const char *maxoccursprop;
  const char *minoccursprop;
  int maxoccurs;
  int minoccurs;
  const char *valueprop;
  const char *fixedvalueprop;
  const char *baseprop;
  char *namepropbuf;

  int is_enum;
  xmlNodePtr refNode;
  xmlNodePtr nc1;
  xmlNodePtr nc0;

  if (!n || !d || !f)
    {
      return;
    }
  if (!n->name)
    {
      return;
    }
  
  is_enum =0;

  namepropbuf=0;
  simpletypename=0;
  pnameprop=0;
  ptypeprop=0;
  nnameprop=0;
  ntypeprop=0;
  dcnameprop=0;
  ctn = 0;
  seqn=0;
  parent = n;
  dc=0;
  ctnnameprop=0;
  nameprop=0;
  cxcn = 0;
  derivedfrom = 0;
  exn = 0;
  origSeqn=0;
  eln = 0;
  eltypeprop=0;
  elnameprop=0;
  elrefprop=0;
  vardeclare=0;
  name_changed = 0;
  fixedelnameprop =0;
  atttypeprop =0;
  attnameprop = 0;
  fixedattnameprop =0;
  nameprop3=0;
  n3=0;
  levels=0;
  maxoccursprop=0;
  minoccursprop=0;
  maxoccurs=1;
  minoccurs=1;
  refNode=0;
  nc1=0;
  baseprop=0;

  memset (namebuf, 0, sizeof (namebuf));
  memset (namebuf, 0, sizeof (namebuf2));

  while (parent)
    {
      strncpy (namebuf2, namebuf, sizeof (namebuf2));
      pnameprop = (const char *) xmlGetProp (parent, "name");
      ptypeprop = (const char *) xmlGetProp (parent, "type");
      strncpy (namebuf, parent->name, sizeof (namebuf) - 10);
      if (pnameprop || ptypeprop)
	{
	  strcat (namebuf, "(");
	}
      if (pnameprop)
	{
	  strcat (namebuf, "name=");
	  strncat (namebuf, pnameprop,
		   sizeof (namebuf) - strlen (namebuf) - 10);
	}
      if (pnameprop && ptypeprop)
	{
	  strcat (namebuf, ",");
	}
      if (ptypeprop)
	{
	  strcat (namebuf, "type=");
	  strncat (namebuf, ptypeprop,
		   sizeof (namebuf) - strlen (namebuf) - 10);
	}
      if (pnameprop || ptypeprop)
	{
	  strcat (namebuf, ")");
	}
      strcat (namebuf, ".");
      strncat (namebuf2, namebuf, sizeof (namebuf2) - strlen (namebuf2) - 10);
      strncpy (namebuf, namebuf2, sizeof (namebuf));
      parent = parent->parent;
    }
  rcs_print_debug(1,"writeCppStructForXMLNode(...,%s)\n", namebuf);
  nnameprop = (const char *) xmlGetProp (n, "name");
  rcs_print_debug(1,"nnameprop=%s\n", nnameprop);
  ntypeprop = (const char *) xmlGetProp (n, "type");
  rcs_print_debug(1,"ntypeprop=%s\n", ntypeprop);

  if(nnameprop && alreadyDefined(nnameprop))
    {
      rcs_print_debug(1,"%s already defined.", nnameprop);
      return;
    }

  bool restricted_simple_type=false;
  rcs_print_debug(1,"n->children=%p\n", (void*)n->children);
  rcs_print_debug(1,"n->name=%s\n", n->name);
  if(n->children)
    {
      rcs_print_debug(1,"n->children->name=%s\n", n->children->name);
      rcs_print_debug(1,"n->children->children=%p\n", (void*)n->children->children);
      if(n->children->children)
	{
	  rcs_print_debug(1,"n->children->children->name=%s\n", n->children->children->name);
	}
    }

  if(nnameprop && n->children)
    {
      nc0 = n->children;
      while(nc0)
	{
	  rcs_print_debug(1,"nc0->name=%s\n",nc0->name);
	  if(nc0->name && !xmlNameCompare(nc0->name,xsd_ns,"simpleType"))
	    {
	      simpletypename = (const char *) xmlGetProp(nc0,"name");
	      if(!simpletypename)
		{
		  simpletypename =nnameprop;
		}
	      rcs_print_debug(1,"simpletypename=%s\n",simpletypename);
	      break;
	    }
	  nc0 = nc0->next;
	}
      if(nc0)
	{
	  nc1 = nc0->children;
	  while(nc1)
	    {
	      rcs_print_debug(1,"nc1->name=%s\n",nc1->name);
	      if(nc1->name && nc1->children && !xmlNameCompare(nc1->name,xsd_ns,"restriction"))
		{
		  nc1 = nc1->children;
		  restricted_simple_type=true;
		  break;
		}
	      nc1 = nc1->next;
	    }
	}
    }
  if(nnameprop && n->name && !xmlNameCompare(n->name,xsd_ns,"simpleType"))
    {
      simpletypename = (const char *) xmlGetProp(n,"name");
      rcs_print_debug(1,"simpletypename=%s\n",simpletypename);
      nc1 = n->children;
      while(nc1)
	{
	  rcs_print_debug(1,"nc1->name=%s\n",nc1->name);
      	  if(nc1->name && nc1->children &&  !xmlNameCompare(nc1->name,xsd_ns,"restriction"))
	    {
	      nc1 = nc1->children;
	      restricted_simple_type=true;
	      break;
	    }
	  nc1 = nc1->next;
	}
    }


  if (!xmlNameCompare(n->name,xsd_ns,"element"))
    {
      nc0 = n->children;
      while(nc0)
	{
	  if(nc0->name && !xmlNameCompare(nc0->name,xsd_ns,"complexType"))
	    {
	      ctn = nc0;
	      break;
	    }
	  nc0 = nc0->next;
	}
    }
  if (!xmlNameCompare(n->name,xsd_ns,"complexType"))
    {
      ctn = n;
    }
  if (!ctn && ntypeprop)
    {
      dc = d->rootNode->children;
      while (dc)
	{
	  if (dc->name)
	    {
	      rcs_print_debug(1,"dc->name=%s\n", dc->name);
	      if (!xmlNameCompare(dc->name,xsd_ns,"simpleType"))
		{
		  dcnameprop = (const char *) xmlGetProp (dc, "name");
		  rcs_print_debug(1,"dc->nameprop=%s\n", dcnameprop);
		  if (!dcnameprop)
		    {
		      dcnameprop = (const char *) xmlGetProp (dc, "xsd:name");
		    }
		  if (!strcmp (ntypeprop, dcnameprop))
		    {
		      simpletypename = dcnameprop;
		      rcs_print_debug(1,"simpletypename=%s\n",simpletypename);
		      nc1 = dc->children;
		      while(nc1)
			{
			  rcs_print_debug(1,"nc1->name=%s\n",nc1->name);
			  if(nc1->name && nc1->children &&  !xmlNameCompare(nc1->name,xsd_ns,"restriction"))
			    {
			      nc1 = nc1->children;
			      restricted_simple_type=true;
			      break;
			    }
			  nc1 = nc1->next;
			}
		      break;
		    }
		}
	      if (!xmlNameCompare(dc->name,xsd_ns,"complexType"))
		{
		  dcnameprop = (const char *) xmlGetProp (dc, "name");
		  rcs_print_debug(1,"dc->nameprop=%s\n", dcnameprop);
		  if (!dcnameprop)
		    {
		      dcnameprop = (const char *) xmlGetProp (dc, "xsd:name");
		    }
		  if (!strcmp (ntypeprop, dcnameprop))
		    {
		      ctn = dc;
		      break;
		    }
		}
	    }
	  dc = dc->next;
	}
    }
  else if (!ctn && nnameprop)
    {
      dc = d->rootNode->children;
      while (dc)
	{
	  if (dc->name)
	    {
	      rcs_print_debug(1,"dc->name=%s\n", dc->name);
	      if (!xmlNameCompare(dc->name,xsd_ns,"complexType"))
		{
		  dcnameprop = (const char *) xmlGetProp (dc, "name");
		  rcs_print_debug(1,"dc->nameprop=%s\n", dcnameprop);
		  if (!dcnameprop)
		    {
		      dcnameprop = (const char *) xmlGetProp (dc, "xsd:name");
		    }
		  if (!strcmp (nnameprop, dcnameprop))
		    {
		      ctn = dc;
		      break;
		    }
		}
	    }
	  dc = dc->next;
	}
    }

  if(restricted_simple_type)
    {
      rcs_print_debug(1,"restricted_simple_type=true\n");
      while(nc1)
	{
	  rcs_print_debug(1,"nc1->name=%s\n",nc1->name);
	  if(nc1->name && !xmlNameCompare(nc1->name,xsd_ns,"enumeration"))
	    {
	      if(!is_enum)
		{
		  is_enum=1;
		  setDefinedEnum(simpletypename);
		  if(nc1->parent)
		    {
		      writeCppCommentForXmlNode(d,f,nc1->parent,0);
		    }
		  fprintf(f,"enum %sEnum {\n",simpletypename);
		}
	      valueprop= (const char *) xmlGetProp(nc1,"value");
	      rcs_print_debug(1,"valueprop=%s\n",valueprop);
	      if(valueprop)
		{
		  writeCppCommentForXmlNode(d,f,nc1->parent,"\t");
		  fixedvalueprop=fixenumvaluename(valueprop, &name_changed);
		  if(nc1->next != 0)
		    {
		      fprintf(f,"\t%s,",fixedvalueprop);
		    }
		  else
		    {
		      fprintf(f,"\t%s",fixedvalueprop);
		    }
		  if(name_changed)
		    {
		      fprintf(f,"\t//name=%s",valueprop);
		    }
		  fprintf(f,"\n");
		}
	    }
	  nc1 = nc1->next;
	}
      if(is_enum)
	{
	  fprintf(f,"};\n\n");
	  setDefinedEnum(nnameprop);
	  return;
	}
    }



  if (!ctn)
    {
      return;
    }
  ctnnameprop = (const char *) xmlGetProp (ctn, "name");
  rcs_print_debug(1,"ctnnameprop=%s\n", ctnnameprop);
  nameprop = ctnnameprop;
  if (!nameprop)
    {
      namepropbuf = (char *) malloc (strlen (nnameprop) + 30);
      strcpy (namepropbuf, nnameprop);
      nameprop = namepropbuf;
    }
  rcs_print_debug(1,"nameprop=%s\n", nameprop);
  if (!nameprop)
    {
      return;
    }

  if (alreadyDefined (nameprop))
    {
      return;
    }
  seqn = ctn;
  if (!xmlNameCompare(ctn->name,xsd_ns,"complexType"))
    {
      seqn = ctn->children;
    }
  cxcn = 0;
  derivedfrom = 0;
  exn = 0;
  if (!seqn)
    {
      writeCppCommentForXmlNode(d,f,ctn,0);
      if (derivedfrom)
	{
	  if(isNMLmsg(derivedfrom))
	  {
	    fprintf (f,
		     "class %s : public %s\n{\npublic:\n\t%sStruct();\n\tvoid update(CMS *);\n};// empty struct \n",
		     nameprop, derivedfrom,nameprop);
	  }
	  else
	  {
	    fprintf (f,
		     "struct  %sStruct : public %sStruct\n{\n};// empty struct \n",
		     nameprop, derivedfrom);
	  }
	}
      else
	{
	  if(isNMLmsg(nameprop))
	    {
	      fprintf (f, "class %s : public NMLmsg\n{\npublic:\n\t%s();\n\tvoid update(CMS *);\n};// empty struct \n", nameprop,nameprop);
	    }
	  else
	    {
	      fprintf (f, "struct  %sStruct\n{\n};// empty struct \n", nameprop);
	    }
	}
      setDefinedClass (nameprop);
      return;
    }

  if (!xmlNameCompare(seqn->name,xsd_ns,"complexContent"))
    {
      cxcn = seqn;
      exn = cxcn->children;
      if (!xmlNameCompare(exn->name,xsd_ns,"extension"))
	{
	  derivedfrom = (const char *) xmlGetProp (exn, "base");
	  if (0 != derivedfrom && !alreadyDefined (derivedfrom))
	    {
	      dc = d->rootNode->children;
	      while (dc)
		{
		  if (dc->name)
		    {
		      rcs_print_debug(1,"dc->name=%s\n", dc->name);
		      if (!xmlNameCompare(dc->name,xsd_ns,"complexType"))
			{
			  dcnameprop = xmlGetProp (dc, "name");
			  rcs_print_debug(1,"dc->nameprop=%s\n", dcnameprop);
			  if (!dcnameprop)
			    {
			      dcnameprop = xmlGetProp (dc, "xsd:name");
			    }
			  if (!strcmp (derivedfrom, dcnameprop))
			    {
			      writeCppStructForXMLNode (d, f, dc);
			    }
			}
		    }
		  dc = dc->next;
		}
	    }
	  seqn = exn->children;
	  if (!seqn)
	    {
	      writeCppCommentForXmlNode(d,f,ctn,0);
	      if(isNMLmsg(derivedfrom))
		{
		  fprintf (f,
			   "class  %s : public %s {\npublic:\n\t%s();\n\tvoid update(CMS *);\n};// empty struct \n",
			   nameprop, derivedfrom, nameprop);
		}
	      else
		{
		  fprintf (f,
			   "struct  %sStruct : public %sStruct {};// empty struct \n",
			   nameprop, derivedfrom);
		}
	      setDefinedClass (nameprop);
	      return;
	    }
	}
      else
	{
	  seqn = exn;
	  exn = 0;
	}
    }

  if (!seqn)
    {
      writeCppCommentForXmlNode(d,f,ctn,0);
      if (derivedfrom)
	{
	  fprintf (f,
		   "struct  %sStruct : public %sStruct\n{\n};// empty struct \n",
		   nameprop, derivedfrom);
	}
      else
	{
	  fprintf (f, "struct  %sStruct\n{\n};// empty struct \n", nameprop);
	}
      setDefinedClass (nameprop);
      return;
    }

  origSeqn = seqn;
  while (seqn)
    {
      if (seqn->name)
	{
	  if (!xmlNameCompare(seqn->name,xsd_ns,"sequence")
	      || !xmlNameCompare(seqn->name,xsd_ns,"all"))
	    {
	      break;
	    }
	}
      seqn = seqn->next;
    }
  if (!seqn)
    {
      seqn = origSeqn;
    }

  if ((!xmlNameCompare(seqn->name,xsd_ns,"sequence") 
       || !xmlNameCompare(seqn->name,xsd_ns,"all")
       || !xmlNameCompare(seqn->name,xsd_ns,"choice"))
      && (seqn->children == 0 || seqn->children->next == 0))
    {
      eln = seqn->children;
      if (!eln)
	{
	  return;
	}
      eltypeprop=0;
      elnameprop=0;
      elrefprop=0;
      vardeclare=0;
      while (eln)
	{
	  if (eln->name)
	    {
	      if (!xmlNameCompare(eln->name,xsd_ns,"choice")
		  || !xmlNameCompare(eln->name,xsd_ns,"complexContent")
		  || !xmlNameCompare(eln->name,xsd_ns,"complexType")
		  || !xmlNameCompare(eln->name,xsd_ns,"sequence"))
		{
		  if (eln->children)
		    {
		      writeCppStructForXMLNode (d, f, eln->children);
		    }
		}
	      if (!xmlNameCompare(eln->name,xsd_ns,"element"))
		{
		  eltypeprop = (const char *) xmlGetProp (eln, "type");
		  elnameprop = (const char *) xmlGetProp (eln, "name");
		  vardeclare =
		    declareVarString (eltypeprop, elnameprop, eln);
		  if (!vardeclare && !alreadyDefined (eltypeprop))
		    {
		      writeCppStructForXMLNode (d, f, eln);
		    }
		  if(!eltypeprop)
		    {
		      elrefprop = xmlGetProp (eln, "ref");
		      if(elrefprop)
			{
			  if(!alreadyDefined(elrefprop))
			    {
			      n3  = d->rootNode->children;
			      while (n3)
				{
				  if (n3->name)
				    {
				      rcs_print_debug(1,"n->name=%s\n", n->name);
				      if (!xmlNameCompare(n3->name,xsd_ns,"element"))
					{
					  nameprop3 = xmlGetProp (n3, "name");
					  rcs_print_debug(1,"nameprop=%s\n", nameprop);
					  if (nameprop3)
					    {
					      if(!strcmp(nameprop3,elrefprop))
						{
						  writeCppStructForXMLNode(d,f,n3);
						  break;
						}
					    }
					}
				    }
				  n3 = n3->next;
				}
			    }
			  
			}
		    }
		}
	    }
	  eln = eln->next;
	}
      eln = seqn->children;
      if (!eln)
	{
	  return;
	}
      fputs ("\n", f);
      writeCppCommentForXmlNode(d,f,ctn,0);
      if (!derivedfrom)
	{
	  if (isNMLmsg (nameprop))
	    {
	      fprintf (f, "class %s : public NMLmsg\n{\n", nameprop);
	      fprintf (f, "public:\n");
	      fprintf (f, "\t%s();\n", nameprop);
	      fprintf (f, "\tvoid update(CMS *);\n\n");
	    }
	  else
	    {
	      fprintf (f, "struct %sStruct\n{\n", nameprop);
	    }
	}
      else
	{
	  if (isNMLmsg (nameprop))
	    {
	      fprintf (f, "class %s : public %s\n{\n", nameprop, derivedfrom);
	      fprintf (f, "public:\n");
	      fprintf (f, "\t%s();\n", nameprop);
	      fprintf (f, "\tvoid update(CMS *);\n\n");
	    }
	  else
	    {
	      fprintf (f, "struct %sStruct : public %sStruct\n{\n", nameprop,
		       derivedfrom);
	    }
	}
      levels=0;
      while (eln)
	{
	  if (eln->name)
	    {
	      if (!xmlNameCompare(eln->name,xsd_ns,"choice")
		  || !xmlNameCompare(eln->name,xsd_ns,"complexContent")
		  || !xmlNameCompare(eln->name,xsd_ns,"complexType")
		  || !xmlNameCompare(eln->name,xsd_ns,"sequence"))
		{
		  levels++;
		  eln = eln->children;
		}
	      if (!xmlNameCompare(eln->name,xsd_ns,"element"))
		{
		  writeCppCommentForXmlNode(d,f,eln,"\t");
		  eltypeprop = xmlGetProp (eln, "type");
		  elnameprop = xmlGetProp (eln, "name");
		  elrefprop = xmlGetProp (eln, "ref");
		  refNode=eln;
		  name_changed = 0;
		  if(!elnameprop && elrefprop)
		    {
		      elnameprop = strdup(elrefprop);
		    }
		  if(elrefprop)
		    {
		      refNode = lookUpRefNode(elrefprop);
		      if(refNode)
			{
			  eltypeprop = xmlGetProp(refNode,"type");
			}
		      if(!eltypeprop)
			{
			  eltypeprop = strdup(elrefprop);
			}
		    }
		  fixedelnameprop =
		    fixname (elnameprop,eltypeprop, &name_changed);
		  vardeclare =
		    declareVarString (eltypeprop, fixedelnameprop, refNode);
		  if (vardeclare)
		    {
		      fprintf (f, "\t%s", vardeclare);
		    }
		  else
		    {
		      maxoccursprop = xmlGetProp (eln, "maxOccurs");
		      minoccursprop = xmlGetProp (eln, "minOccurs");
		      maxoccurs = 1;
		      minoccurs = 1;
		      if (maxoccursprop)
			{
			  if (!strcmp (maxoccursprop, "unbounded"))
			    {
			      maxoccurs = -1;
			    }
			  else
			    {
			      maxoccurs = strtol (maxoccursprop, 0, 10);
			    }
			}
		      if (minoccursprop)
			{
			  minoccurs = strtol (minoccursprop, 0, 10);
			}

		      if (eltypeprop)
			{
			  if (isUnion (eltypeprop))
			    {
			      if (maxoccurs == 1)
				{
				  fprintf (f, "\t%sUnionStruct %s;",
					   eltypeprop, fixedelnameprop);
				}
			      else if (maxoccurs < 1)
				{
				  if (use_unbounded_arrays)
				    {
				      fprintf (f,
					       "\tDECLARE_NML_UNBOUNDED_ARRAY(%sUnionStruct,%s);",
					       eltypeprop, fixedelnameprop);
				    }
				  else
				    {
				      fprintf (f,
					       "\tDECLARE_NML_DYNAMIC_LENGTH_ARRAY(%sUnionStruct,%s,%s_UNBOUNDED_LENGTH);",
					       eltypeprop, fixedelnameprop,
					       shortfname);
				    }
				}
			      else if (maxoccurs != minoccurs)
				{
				  fprintf (f,
					   "\tDECLARE_NML_DYNAMIC_LENGTH_ARRAY(%sUnionStruct,%s,%d);",
					   eltypeprop, fixedelnameprop,
					   maxoccurs);
				}
			      else
				{
				  fprintf (f, "\t%sUnionStruct %s[%d];",
					   eltypeprop, fixedelnameprop,
					   maxoccurs);
				}
			    }
			  else
			    {
			      if (isNMLmsg (eltypeprop))
				{
				  if (maxoccurs == 1)
				    {
				      fprintf (f, "\t%s %s;", eltypeprop,
					       fixedelnameprop);
				    }
				  else if (maxoccurs < 1)
				    {
				      if (use_unbounded_arrays)
					{
					  fprintf (f,
						   "\tDECLARE_NML_UNBOUNDED_ARRAY(%s,%s);",
						   eltypeprop,
						   fixedelnameprop);
					}
				      else
					{
					  fprintf (f,
						   "\tDECLARE_NML_DYNAMIC_LENGTH_ARRAY(%s,%s.%s_UNBOUNDED_LENGTH);",
						   eltypeprop,
						   fixedelnameprop,
						   shortfname);
					}
				    }
				  else if (maxoccurs != minoccurs)
				    {
				      fprintf (f,
					       "\tDECLARE_NML_DYNAMIC_LENGTH_ARRAY(%s,%s,%d);",
					       eltypeprop, fixedelnameprop,
					       maxoccurs);
				    }
				  else
				    {
				      fprintf (f, "\t%s %s[%d];", eltypeprop,
					       fixedelnameprop, maxoccurs);
				    }
				}
			      else
				{
				  if (maxoccurs == 1)
				    {
				      fprintf (f, "\t%sStruct %s;",
					       eltypeprop, fixedelnameprop);
				    }
				  else if (maxoccurs < 1)
				    {
				      if (use_unbounded_arrays)
					{
					  fprintf (f,
						   "\tDECLARE_NML_UNBOUNDED_ARRAY(%sStruct,%s);",
						   eltypeprop,
						   fixedelnameprop);
					}
				      else
					{
					  fprintf (f,
						   "\tDECLARE_NML_DYNAMIC_LENGTH_ARRAY(%sStruct,%s,%s_UNBOUNDED_LENGTH);",
						   eltypeprop,
						   fixedelnameprop,
						   shortfname);
					}
				    }
				  else if (maxoccurs != minoccurs)
				    {
				      fprintf (f,
					       "\tDECLARE_NML_DYNAMIC_LENGTH_ARRAY(%sStruct,%s,%d);",
					       eltypeprop, fixedelnameprop,
					       maxoccurs);
				    }
				  else
				    {
				      fprintf (f, "\t%sStruct %s[%d];",
					       eltypeprop, fixedelnameprop,
					       maxoccurs);
				    }
				}
			    }
			}
		      else if (fixedelnameprop)
			{
			  if (isUnion (fixedelnameprop))
			    {
			      if (maxoccurs == 1)
				{
				  fprintf (f, "\t%sUnionStruct %s;",
					   fixedelnameprop,
					   fixedelnameprop);
				}
			      else if (maxoccurs < 1)
				{
				  if (use_unbounded_arrays)
				    {
				      fprintf (f,
					       "\tDECLARE_NML_UNBOUNDED_ARRAY(%sUnionStruct,%s);",
					       fixedelnameprop,
					       fixedelnameprop);
				    }
				  else
				    {
				      fprintf (f,
					       "\tDECLARE_NML_DYNAMIC_LENGTH_ARRAY(%sUnionStruct,%s,%s_UNBOUNDED_LENGTH);",
					       fixedelnameprop,
					       fixedelnameprop, shortfname);
				    }
				}
			      else if (maxoccurs != minoccurs)
				{
				  fprintf (f,
					   "\tDECLARE_NML_DYNAMIC_LENGTH_ARRAY(%sUnionStruct,%s,%d);",
					   fixedelnameprop, fixedelnameprop,
					   maxoccurs);
				}
			      else
				{
				  fprintf (f, "\t%sUnionStruct %s[%d];",
					   fixedelnameprop, fixedelnameprop,
					   maxoccurs);
				}
			    }
			  else
			    {
			      if (isNMLmsg (fixedelnameprop))
				{
				  if (maxoccurs == 1)
				    {
				      fprintf (f, "\t%s %smsg;",
					       fixedelnameprop,
					       fixedelnameprop);
				    }
				  else if (maxoccurs < 1)
				    {
				      fprintf (f,
					       "\tDECLARE_NML_DYNAMIC_LENGTH_ARRAY(%s,%smsg,%s_UNBOUNDED_LENGTH);",
					       fixedelnameprop,
					       fixedelnameprop, shortfname);
				    }
				  else if (maxoccurs != minoccurs)
				    {
				      fprintf (f,
					       "\tDECLARE_NML_DYNAMIC_LENGTH_ARRAY(%s,%smsg,%d);",
					       fixedelnameprop,
					       fixedelnameprop, maxoccurs);
				    }
				  else
				    {
				      fprintf (f, "\t%s %smsg[%d];",
					       fixedelnameprop,
					       fixedelnameprop, maxoccurs);
				    }
				}
			      else
				{
				  if (maxoccurs == 1)
				    {
				      fprintf (f, "\t%sStruct %s;",
					       fixedelnameprop,
					       fixedelnameprop);
				    }
				  else if (maxoccurs < 1)
				    {
				      fprintf (f,
					       "\tDECLARE_NML_DYNAMIC_LENGTH_ARRAY(%sStruct,%s,%s_UNBOUNDED_LENGTH);",
					       fixedelnameprop,
					       fixedelnameprop, shortfname);
				    }
				  else if (maxoccurs != minoccurs)
				    {
				      fprintf (f,
					       "\tDECLARE_NML_DYNAMIC_LENGTH_ARRAY(%sStruct,%s,%d);",
					       fixedelnameprop,
					       fixedelnameprop, maxoccurs);
				    }
				  else
				    {
				      fprintf (f, "\t%sStruct %s[%d];",
					       fixedelnameprop,
					       fixedelnameprop, maxoccurs);
				    }
				}
			    }
			}
		    }
		  if (name_changed)
		    {
		      fprintf (f, "// name=%s", elnameprop);
		    }
		  fprintf (f, "\n");
		}

	      if (!xmlNameCompare(eln->name,xsd_ns,"attribute"))
		{
		  writeCppCommentForXmlNode(d,f,eln,"\t");
		  eltypeprop = xmlGetProp (eln, "type");
		  elnameprop = xmlGetProp (eln, "name");
		  name_changed = 0;
		  fixedelnameprop =
		    fixname (elnameprop,eltypeprop, &name_changed);
		  vardeclare =
		    declareVarString (eltypeprop, fixedelnameprop, eln);
		  if (vardeclare)
		    {
		      fprintf (f, "\t%s //attribute", vardeclare);
		    }
		  if (name_changed)
		    {
		      fprintf (f, ", name=%s", elnameprop);
		    }
		  fprintf (f, "\n");
		}
	    }
	  if (!eln->next && levels > 0)
	    {
	      levels--;
	      eln = eln->parent;
	    }
	  eln = eln->next;

	}

      xmlNodePtr atn = ctn->children;
      while (atn)
	{
	  if (atn->name)
	    {
	      if (!xmlNameCompare(atn->name,xsd_ns,"attribute"))
		{
		  writeCppCommentForXmlNode(d,f,atn,"\t");
		  atttypeprop = xmlGetProp (atn, "type");
		  attnameprop = xmlGetProp (atn, "name");
		  name_changed = 0;
		  fixedattnameprop =
		    fixname (attnameprop,atttypeprop, &name_changed);
		  vardeclare =
		    declareVarString (atttypeprop, fixedattnameprop, atn);
		  if (vardeclare)
		    {
		      fprintf (f, "\t%s //attribute", vardeclare);
		    }
		  if (name_changed)
		    {
		      fprintf (f, ", name=%s", attnameprop);
		    }
		  fprintf (f, "\n");
		}
	    }
	  if(atn->children && xmlNameCompare(atn->name,xsd_ns,"element"))
	    {
	      atn = atn->children;
	    }
	  else if(!atn->next && atn->parent && atn->parent != ctn)
	    {
	      atn = atn->parent->next;
	    }
	  else
	    {
	      atn = atn->next;
	    }
	}
      if(ctn->children && ctn->children->children &&
	 !xmlNameCompare(ctn->children->name,xsd_ns,"extension"))
	{
	  baseprop = xmlGetProp(ctn->children,"base");
	  if(baseprop && !xmlNameCompare(baseprop,xsd_ns,"string"))
	    {
	      if(use_unbounded_arrays)
		{
		  fprintf(f,"\tDECLARE_NML_UNBOUNDED_ARRAY(char,unnamed_content); //name=\n");
		}
	      else
		{
		  fprintf(f,"\tchar unnamed_content[%s_STRING_LENGTH]; //name=\n",shortfname);
		}
	    }
	}
      else if (ctn->children && ctn->children->children && ctn->children->children->name

	       && xmlNameCompare(ctn->children->name,xsd_ns,"element") 
	       && !xmlNameCompare(ctn->children->children->name,xsd_ns,"extension") )
	{
	  baseprop = xmlGetProp(ctn->children->children,"base");
	  if(baseprop && !xmlNameCompare(baseprop,xsd_ns,"string"))
	    {
	      if(use_unbounded_arrays)
		{
		  fprintf(f,"\tDECLARE_NML_UNBOUNDED_ARRAY(char,unnamed_content); //name=\n");
		}
	      else
		{
		  fprintf(f,"\tchar unnamed_content[%s_STRING_LENGTH]; //name=\n",shortfname);
		}
	    }
	}
      else if (ctn->children && ctn->children->children && ctn->children->children->children && ctn->children->children->children->name
	       && xmlNameCompare(ctn->children->name,xsd_ns,"element") 
	       && xmlNameCompare(ctn->children->children->name,xsd_ns,"element") 
	       && !xmlNameCompare(ctn->children->children->children->name,xsd_ns,"extension"))
	{
	  baseprop = xmlGetProp(ctn->children->children->children,"base");
	  if(baseprop && !xmlNameCompare(baseprop,xsd_ns,"string"))
	    {
	      if(use_unbounded_arrays)
		{
		  fprintf(f,"\tDECLARE_NML_UNBOUNDED_ARRAY(char,unnamed_content); //name=\n");
		}
	      else
		{
		  fprintf(f,"\tchar unnamed_content[%s_STRING_LENGTH]; //name=\n",shortfname);
		}
	    }
	}
      fprintf (f, "};\n");
      setDefinedClass (nameprop);
    }
  else if (!xmlNameCompare(seqn->name,xsd_ns,"choice"))
    {
      eln = seqn->children;
      if (!eln)
	{
	  return;
	}
      while (eln)
	{
	  if (eln->name)
	    {
	      if (!xmlNameCompare(eln->name,xsd_ns,"element"))
		{
		  eltypeprop = xmlGetProp (eln, "type");
		  elnameprop = xmlGetProp (eln, "name");
		  vardeclare =
		    declareVarString (eltypeprop, elnameprop, eln);
		  if (!vardeclare && !alreadyDefined (eltypeprop))
		    {
		      writeCppStructForXMLNode (d, f, eln);
		    }
		}
	    }
	  eln = eln->next;
	}
      eln = seqn->children;
      if (!eln)
	{
	  return;
	}
      fputs ("\n", f);
      fprintf (f, "enum %s_UNION_ENUM\n{\n", nameprop);
      while (eln)
	{
	  if (eln->name)
	    {
	      if (!xmlNameCompare(eln->name,xsd_ns, "element"))
		{
		  writeCppCommentForXmlNode(d,f,eln,"\t");
		  elnameprop = xmlGetProp (eln, "name");
		  name_changed = 0;
		  fixedelnameprop =
		    fixname (elnameprop,eltypeprop, &name_changed);
		  fprintf (f, "\tUNION_%s_SELECTED_%s,\n", nameprop,
			   fixedelnameprop);
		}
	    }
	  eln = eln->next;
	}
      fprintf (f, "\tUNION_%s_SELECTED_INVALID\n", nameprop);
      fputs ("};\n", f);
      fputs ("\n", f);
      fprintf (f, "union %sUnion{\n", nameprop);
      eln = seqn->children;
      while (eln)
	{
	  if (eln->name)
	    {
	      if (!xmlNameCompare(eln->name,xsd_ns,"element"))
		{
		  writeCppCommentForXmlNode(d,f,eln,"\t");
		  eltypeprop = xmlGetProp (eln, "type");
		  elnameprop = xmlGetProp (eln, "name");
		  name_changed = 0;
		  fixedelnameprop =
		    fixname (elnameprop,eltypeprop, &name_changed);
		  vardeclare =
		    declareVarString (eltypeprop, fixedelnameprop, eln);
		  if (vardeclare)
		    {
		      fprintf (f, "\t\t%s", vardeclare);
		    }
		  else
		    {
		      if (eltypeprop)
			{
			  if (isUnion (eltypeprop))
			    {
			      fprintf (f, "\t\t%sUnionStruct %s;",
				       eltypeprop, fixedelnameprop);
			    }
			  else
			    {
			      fprintf (f, "\t\t%sStruct %s;", eltypeprop,
				       fixedelnameprop);
			    }
			}
		      else if (fixedelnameprop)
			{
			  if (isUnion (eltypeprop))
			    {
			      fprintf (f, "\t\t%sUnionStruct %s;",
				       fixedelnameprop, fixedelnameprop);
			    }
			  else
			    {
			      fprintf (f, "\t\t%sStruct %s;",
				       fixedelnameprop, fixedelnameprop);
			    }
			}
		    }
		  if (name_changed)
		    {
		      fprintf (f, "// name=%s", elnameprop);
		    }
		  fprintf (f, "\n");
		}
	    }
	  eln = eln->next;
	}
      fprintf (f, "};\n");
      fprintf (f, "struct %sUnionStruct\n{\n", nameprop);
      fprintf (f, "\tenum %s_UNION_ENUM union_selector_%s;\n", nameprop,
	       nameprop);
      fprintf (f, "\tunion %sUnion %sUnionVar;\n", nameprop, nameprop);
      fprintf (f, "};\n");
      setDefinedUnion (nameprop);
    }
  else
    {
      if (derivedfrom)
	{
	  if(isNMLmsg(derivedfrom))
	    {
	      fprintf (f, "class %s : public %s\n{\npublic:\n\t%s();\n\tvoid update(CMS *);\n\n", 
		       nameprop, derivedfrom, nameprop);
	    }
	  else
	    {
	      fprintf (f, "struct  %sStruct : public %sStruct\n{\n", nameprop,
		       derivedfrom);
	    }
	  xmlNodePtr atn = ctn->children;
	  while (atn)
	    {
	      if (atn->name)
		{
		  if (!xmlNameCompare(atn->name,xsd_ns,"attribute"))
		    {
		      writeCppCommentForXmlNode(d,f,atn,"\t");
		      const char *eltypeprop3 = xmlGetProp (atn, "type");
		      const char *elnameprop3 = xmlGetProp (atn, "name");
		      int name_changed3 = 0;
		      char *fixedelnameprop3 =
			fixname (elnameprop3,eltypeprop3, &name_changed3);
		      const char *vardeclare3 =
			declareVarString (eltypeprop3, fixedelnameprop3, atn);
		      if (vardeclare3)
			{
			  fprintf (f, "\t%s //attribute", vardeclare);
			  if (name_changed3)
			    {
			      fprintf (f, ", name=%s", elnameprop3);
			    }
			}
		      fprintf (f, "\n");
		    }
		}
	      if(atn->children && xmlNameCompare(atn->name,xsd_ns,"element"))
		{
		  atn = atn->children;
		}
	      else if(!atn->next && atn->parent && atn->parent != ctn)
		{
		  atn = atn->parent->next;
		}
	      else
		{
		  atn = atn->next;
		}
	    }
	  if(ctn->children && ctn->children->children &&
	     !xmlNameCompare(ctn->children->name,xsd_ns,"extension"))
	    {
	      baseprop = xmlGetProp(ctn->children,"base");
	      if(baseprop && !xmlNameCompare(baseprop,xsd_ns,"string"))
		{
		  if(use_unbounded_arrays)
		    {
		      fprintf(f,"\tDECLARE_NML_UNBOUNDED_ARRAY(char,unnamed_content); //name=\n");
		    }
		  else
		    {
		      fprintf(f,"\tchar unnamed_content[%s_STRING_LENGTH]; //name=\n",shortfname);
		    }
		}
	    }
	  else if (ctn->children && ctn->children->children && ctn->children->children->name

		   && xmlNameCompare(ctn->children->name,xsd_ns,"element") 
		   && !xmlNameCompare(ctn->children->children->name,xsd_ns,"extension") )
	    {
	      baseprop = xmlGetProp(ctn->children->children,"base");
	      if(baseprop && !xmlNameCompare(baseprop,xsd_ns,"string"))
		{
		  if(use_unbounded_arrays)
		    {
		      fprintf(f,"\tDECLARE_NML_UNBOUNDED_ARRAY(char,unnamed_content); //name=\n");
		    }
		  else
		    {
		      fprintf(f,"\tchar unnamed_content[%s_STRING_LENGTH]; //name=\n",shortfname);
		    }
		}
	    }
	  else if (ctn->children && ctn->children->children && ctn->children->children->children && ctn->children->children->children->name
		   && xmlNameCompare(ctn->children->name,xsd_ns,"element") 
		   && xmlNameCompare(ctn->children->children->name,xsd_ns,"element") 
		   && !xmlNameCompare(ctn->children->children->children->name,xsd_ns,"extension"))
	    {
	      baseprop = xmlGetProp(ctn->children->children->children,"base");
	      if(baseprop && !xmlNameCompare(baseprop,xsd_ns,"string"))
		{
		  if(use_unbounded_arrays)
		    {
		      fprintf(f,"\tDECLARE_NML_UNBOUNDED_ARRAY(char,unnamed_content); //name=\n");
		    }
		  else
		    {
		      fprintf(f,"\tchar unnamed_content[%s_STRING_LENGTH]; //name=\n",shortfname);
		    }
		}
	    }
	  fprintf (f, "};// empty struct \n");
	}
      else
	{
	  if(isNMLmsg(nameprop))
	    {
	      fprintf (f, "class  %s : public NMLmsg\n{\npublic:\n\t%s();\n\tvoid update(CMS *);\n\n", nameprop,nameprop);
	    }
	  else
	    {
	      fprintf (f, "struct  %sStruct\n{\n", nameprop);	    
	    }
	  xmlNodePtr atn = ctn->children;
	  while (atn)
	    {
	      if (atn->name)
		{
		  if (!xmlNameCompare(atn->name,xsd_ns,"attribute"))
		    {
		      writeCppCommentForXmlNode(d,f,eln,"\t");
		      atttypeprop = xmlGetProp (atn, "type");
		      attnameprop = xmlGetProp (atn, "name");
		      name_changed = 0;
		      fixedattnameprop =
			fixname (attnameprop, atttypeprop,&name_changed);
		      vardeclare =
			declareVarString (atttypeprop, fixedattnameprop, atn);
		      if (vardeclare)
			{
			  fprintf (f, "\t%s //attribute", vardeclare);
			}
		      if (name_changed)
			{
			  fprintf (f, ", name=%s", attnameprop);
			}
		      fprintf (f, "\n");
		    }
		}
	      if(atn->children && xmlNameCompare(atn->name,xsd_ns,"element"))
		{
		  atn = atn->children;
		}
	      else if(!atn->next && atn->parent != ctn)
		{
		  atn = atn->parent->next;
		}
	      else
		{
		  atn = atn->next;
		}
	    }
	  if(ctn->children && ctn->children->children &&
	     !xmlNameCompare(ctn->children->name,xsd_ns,"extension"))
	    {
	      baseprop = xmlGetProp(ctn->children,"base");
	      if(baseprop && !xmlNameCompare(baseprop,xsd_ns,"string"))
		{
		  if(use_unbounded_arrays)
		    {
		      fprintf(f,"\tDECLARE_NML_UNBOUNDED_ARRAY(char,unnamed_content); //name=\n");
		    }
		  else
		    {
		      fprintf(f,"\tchar unnamed_content[%s_STRING_LENGTH]; //name=\n",shortfname);
		    }
		}
	    }
	  else if (ctn->children && ctn->children->children && ctn->children->children->name

		   && xmlNameCompare(ctn->children->name,xsd_ns,"element") 
		   && !xmlNameCompare(ctn->children->children->name,xsd_ns,"extension") )
	    {
	      baseprop = xmlGetProp(ctn->children->children,"base");
	      if(baseprop && !xmlNameCompare(baseprop,xsd_ns,"string"))
		{
		  if(use_unbounded_arrays)
		    {
		      fprintf(f,"\tDECLARE_NML_UNBOUNDED_ARRAY(char,unnamed_content); //name=\n");
		    }
		  else
		    {
		      fprintf(f,"\tchar unnamed_content[%s_STRING_LENGTH]; //name=\n",shortfname);
		    }
		}
	    }
	  else if (ctn->children && ctn->children->children && ctn->children->children->children && ctn->children->children->children->name
		   && xmlNameCompare(ctn->children->name,xsd_ns,"element") 
		   && xmlNameCompare(ctn->children->children->name,xsd_ns,"element") 
		   && !xmlNameCompare(ctn->children->children->children->name,xsd_ns,"extension"))
	    {
	      baseprop = xmlGetProp(ctn->children->children->children,"base");
	      if(baseprop && !xmlNameCompare(baseprop,xsd_ns,"string"))
		{
		  if(use_unbounded_arrays)
		    {
		      fprintf(f,"\tDECLARE_NML_UNBOUNDED_ARRAY(char,unnamed_content); //name=\n");
		    }
		  else
		    {
		      fprintf(f,"\tchar unnamed_content[%s_STRING_LENGTH]; //name=\n",shortfname);
		    }
		}
	    }
	  fprintf (f, "};// empty struct \n");
	}
      setDefinedClass (nameprop);
      fprintf(f,"\n");
    }
}

#endif

xmlDocPtr
xmlReadDoc (const char *xmlfile)
{
#ifdef ENABLE_RCS_INET_FILES
  INET_FILE *fp;
  fp = inet_file_open ((char *) xmlfile, "r");
#else
  FILE *fp;
  fp = fopen (xmlfile, "r");
#endif

  if (fp == 0)
    {
      rcs_print_error ("readMsgFromXmlFile(%s) : open failed %d:%s\n",
		       xmlfile, errno, strerror (errno));
      exit (1);
    }

  int bytes_read_this_time = 0;
  int xml_size = 0x100000;
  char *xmldata = (char *) DEBUG_MALLOC (xml_size);
  char *ptr = xmldata;
  int bytes_read = 0;


#ifdef ENABLE_RCS_INET_FILES
  while (!inet_file_eof (fp))
#else
  while (!feof (fp))
#endif
    {
      if (bytes_read > xml_size)
	{
	  rcs_print_error ("xml message file is too large.\n");
	  DEBUG_FREE (xmldata);
#ifdef ENABLE_RCS_INET_FILES
	  inet_file_close (fp);
#else
	  fclose (fp);
#endif
	  exit (1);
	}
#ifdef ENABLE_RCS_INET_FILES
      if ((inet_file_gets (ptr, xml_size - bytes_read, fp)) == NULL)
	{
	  break;
	}
#else
      if ((fgets (ptr, xml_size - bytes_read, fp)) == NULL)
	{
	  break;
	}
#endif
      bytes_read_this_time = strlen (ptr);
      if (bytes_read_this_time > 0)
	{
	  ptr += bytes_read_this_time;
	  bytes_read += bytes_read_this_time;
	}
    }
#ifdef ENABLE_RCS_INET_FILES
  inet_file_close (fp);
#else
  fclose (fp);
#endif
  xmlDocPtr doc = xmlParseMemory (xmldata, bytes_read);
  return doc;
}

#include "rcs_prnt.hh"
#include <time.h>


const char *xml_schema_file=0;

int
xmlSchemaDocToNMLHeader (xmlDocPtr d, const char *filename)
{
  const char *nameprop;
  const char *typeprop;

  typeprop=0;
  nameprop=0;

  FILE *f = fopen (filename, "w");
  const char *shortfname_const = (const char *) strrchr (filename, '/');
  if (shortfname_const == 0)
    {
      shortfname_const = (char *) filename;
    }
  else
    {
      shortfname_const++;
    }
  shortfname = (char *) strdup (shortfname_const);
  char *p = (char *) strrchr (shortfname, '.');
  if (p)
    {
      *p = 0;
    }
  xmlNodePtr root = d->rootNode;
  if(!root)
    {
      return -1;
    }
  if(root->name)
    {
      rcs_print_debug(1,"root->name=%s\n",root->name);
    }
#ifdef OE_DM
  xsd_ns=0;
#else
  if(root->attributes_list)
    {
      XML_ATTRIBUTE *at = (XML_ATTRIBUTE *) root->attributes_list->get_head();
      while(at)
	{
	  if(at->name)
	    {
	      rcs_print_debug(1,"at->name=%s\n", at->name);
	      if(!strncmp(at->name,"xmlns:",6))
		{
		  xsd_ns = strdup(at->name+6);
		  break;
		}
	    }
	  at = (XML_ATTRIBUTE *) root->attributes_list->get_next();
	}
    }
#endif
  if(xml_schema_file)
    {
      time_t now = time(0);
      fprintf(f,"// This file automatically generated\n// from %s\n",
	      xml_schema_file);
      fprintf(f,"// on %s\n",ctime(&now));
#ifndef OE_DM
      fprintf(f,"// by xsd2nmlh\n");
#else
      fprintf(f,"// by oedm_xml2nmlh\n");
#endif
    }
  fprintf(f,"\n");
#ifndef OE_DM
  writeCppCommentForXmlNode(d,f,root,0);
  fprintf(f,"\n");
#endif
  xmlNodePtr n = root->children;

  fprintf (f, "#ifndef %s_header_included\n", shortfname);
  fprintf (f, "#define %s_header_included\n", shortfname);

  fprintf (f, "\n");
  fprintf (f, "#include \"rcs.hh\"\n");
#ifdef OE_DM
  fprintf (f, "#include \"oe_dm_nml.hh\"\n");
  fprintf (f, "\n");
#endif  
  fprintf (f,"extern int %s_format(NMLTYPE type, void *buffer, CMS *cms);\n",
	   shortfname);
  fprintf (f, "\n");
  fprintf (f, "#define DO_NOT_ADD_INDEXES_TO_ARRAY_NAMES\n");
  fprintf (f, "#define %s_URI_STRING_LENGTH 64\n", shortfname);
  fprintf (f, "#define %s_STRING_LENGTH 64\n", shortfname);
  fprintf (f, "#define %s_UNBOUNDED_LENGTH 10\n", shortfname);
  fprintf (f, "\n");
  long type_id = ((sumname (filename) % 10000) * 100) + 1;

  while (n)
    {
      if (n->name)
	{
#ifdef OE_DM
	  rcs_print_debug(1,"n->name=%s\n", n->name);
	  if (!xmlNameCompare(n->name,xsd_ns,"msg_def"))
	    {
	      const char *ncname=0;
	      xmlNodePtr nc = n->children;
	      while(nc)
		{
		  if(!xmlNameCompare(nc->name,xsd_ns,"name"))
		    {
		      ncname=xmlNodeListGetString(d,nc,0);
		      rcs_print_debug(1,"ncname=%s\n", ncname);
		    }
		  nc = nc->next;
		}
	      if  (ncname)
		{
		  fprintf (f, "#define %s_TYPE \t%ld\n", ncname, type_id);
		  type_id++;
		}
	    }

	  // OE_DM
#else
	  rcs_print_debug(1,"n->name=%s\n", n->name);
	  if (!xmlNameCompare(n->name,xsd_ns,"element"))
	    {
	      nameprop = xmlGetProp (n, "name");
	      rcs_print_debug(1,"nameprop=%s\n", nameprop);
	      if (nameprop)
		{
		  typeprop = xmlGetProp (n, "type");
		  if(typeprop && !xmlNameCompare(typeprop,xsd_ns,"string"))
		    {
		      setDefinedBasicType(nameprop);
		    }
		  else
		    {
		      fprintf (f, "#define %s_TYPE \t%ld\n", nameprop, type_id);
		      setDefinedNMLmsg (nameprop);
		      type_id++;
		    }
		}
	    }

	  // !OE_DM
#endif
	}
      n = n->next;
    }
  fprintf (f, "\n");
  currentDoc = d;
  n = root->children;
  while (n)
    {
      writeCppStructForXMLNode (d, f, n);
      n = n->next;
    }
  fprintf (f, "\n");
  fprintf (f, "#endif\n \t// %s_header_included\n\n", shortfname);
  return(0);
}

int
main (int argc, const char **argv)
{
  const char *RCS_DEBUG_FLAG=0;
  const char *MY_LOG_FILE=0;
#if HAVE_GETENV
  RCS_DEBUG_FLAG = getenv("RCS_DEBUG_FLAG");
  if(RCS_DEBUG_FLAG != 0)
    { 
      set_rcs_print_flag(strtol(RCS_DEBUG_FLAG,0,0));
    }
  else
    {
      set_rcs_print_flag(PRINT_EVERYTHING);
    }
  MY_LOG_FILE = getenv("XSD2NMLH_LOG_FILE");
  if(!MY_LOG_FILE)
    {
#ifndef OE_DM
      MY_LOG_FILE="xsd2nmlh.log";
#else
      MY_LOG_FILE="oedm_xml2nmlh.log";
#endif
    }
#endif

  if(MY_LOG_FILE)
    {
      set_rcs_print_file(MY_LOG_FILE);
      set_rcs_print_destination(RCS_PRINT_TO_FILE);
    }
  rcs_print_debug(1,"xsd2nmlh compiled on " __DATE__  " at " __TIME__ "\n");
  if (argc < 3)
    {
      fprintf (stderr, "usage: xmlschema nmlheader\n");
      exit (255);
    }
  const char *xmlfile = argv[1];
  const char *nmlhfile = argv[2];
  xml_schema_file=xmlfile;
  set_rcs_print_flag (PRINT_EVERYTHING);
  parse_attributes = 1;
  xmlDocPtr d = xmlReadDoc (xmlfile);
  if (d)
    {
      xmlSchemaDocToNMLHeader (d, nmlhfile);
    }
}


#endif

#ifdef XML_PARSE_TEST

int
main (int argc, const char **argv)
{
  const char *filename = "test.xml";
  if (argc > 1)
    {
      filename = argv[1];
    }

#ifdef ENABLE_RCS_INET_FILES
  INET_FILE *fp;
  fp = inet_file_open ((char *) filename, "r");
#else
  FILE *fp;
  fp = fopen (filename, "r");
#endif

  if (fp == 0)
    {
      rcs_print_error ("readMsgFromXmlFile(%s) : open failed %d:%s\n",
		       filename, errno, strerror (errno));
      exit (1);
    }

  int bytes_read_this_time = 0;
  int xml_size = 0x100000;
  char *xmldata = (char *) DEBUG_MALLOC (xml_size);
  char *ptr = xmldata;
  int bytes_read = 0;


#ifdef ENABLE_RCS_INET_FILES
  while (!inet_file_eof (fp))
#else
  while (!feof (fp))
#endif
    {
      if (bytes_read > xml_size)
	{
	  rcs_print_error ("xml message file is too large.\n");
	  DEBUG_FREE (xmldata);
#ifdef ENABLE_RCS_INET_FILES
	  inet_file_close (fp);
#else
	  fclose (fp);
#endif
	  exit (1);
	}
#ifdef ENABLE_RCS_INET_FILES
      if ((inet_file_gets (ptr, xml_size - bytes_read, fp)) == NULL)
	{
	  break;
	}
#else
      if ((fgets (ptr, xml_size - bytes_read, fp)) == NULL)
	{
	  break;
	}
#endif
      bytes_read_this_time = strlen (ptr);
      if (bytes_read_this_time > 0)
	{
	  ptr += bytes_read_this_time;
	  bytes_read += bytes_read_this_time;
	}
    }
#ifdef ENABLE_RCS_INET_FILES
  inet_file_close (fp);
#else
  fclose (fp);
#endif
  xmlDocPtr doc = xmlParseMemory (xmldata, bytes_read);
  char *buf = 0;
  int size = 0;
  xmlDocDumpMemory (doc, &buf, &size);
  rcs_print_debug(1,"%s\n", buf);
}

#else

#ifndef XML_SCHEMA_TO_HEADER

CMS_STATUS
CMS_XML_UPDATER::startSchemaGen (void)
{
  schema_type_index = 0;
  schema_gen_mode = 1;
  make_xml_pretty = 1;
  last_check_type_info_list_length = -1;
  if (0 != doc)
    {
      xmlFreeDoc ((xmlDocPtr) doc);
      schemaNode = 0;
      mainElementNode = 0;
      currentNode = 0;
      currentElement = 0;
      doc = 0;
    }
  return CMS_STATUS_NOT_SET;
}


const char *
CMS_XML_UPDATER::removeRefs (const char *str)
{
  const char *ptr = str;
  int refsneeded = 0;
  char *ret = 0;
  char *newptr = 0;
  char tempbuf[8];

  while (*ptr)
    {
      if (*ptr == '&')
	{
	  refsneeded = 1;
	  break;
	}
      if (*ptr == '<')
	{
	  if (!strncmp (ptr, "<![CDATA[", 9))
	    {
	      refsneeded = 1;
	    }
	}
      ptr++;
    }
  if (!refsneeded)
    {
      return str;
    }
  long stringlength = (long) (strlen (str) + 1);
  if ((stringspaceused < (size_t) (stringspaceend - stringspacebegin)) ||
      (stringspaceend < stringspacebegin))
    {
      rcs_print_error
	("Inconsistant values for strinspacebegin=%p, stringspaceend=%p, ((stringspaceend - stringspacebegin)=%lu, stringspaceused=%lu\n",
	 stringspacebegin, stringspaceend,
	 (unsigned long) (stringspaceend - stringspacebegin), 
	 (unsigned long) stringspaceused);
      stringspaceused = (stringspaceend - stringspacebegin);
    }
  if (stringlength + stringspaceused + 1 > stringspacesize)
    {
      stringspacesize =
	(stringlength + stringspaceused) + 256 + (512 -
						  (stringlength +
						   stringspaceused +
						   256) % 512);
      stringspacebegin =
	(char *) DEBUG_MALLOC (stringspacesize - stringspaceused);
      numstringstofree++;
      if (numstringstofree >= maxstringstofree)
	{
	  maxstringstofree += 16;
	  char **oldstringstofree = stringstofree;
	  stringstofree =
	    (char **) DEBUG_MALLOC (maxstringstofree * sizeof (char *));
	  memset (stringstofree, 0, maxstringstofree * sizeof (char *));
	  if (maxstringstofree > 16 && 0 != oldstringstofree)
	    {
	      memcpy (stringstofree, oldstringstofree,
		      (maxstringstofree - 16) * sizeof (char *));
	      DEBUG_FREE (oldstringstofree);
	    }
	}
      stringstofree[numstringstofree] = stringspacebegin;
      stringspaceend = stringspacebegin;
    }
  ret = (char *) stringspaceend;
  stringspaceend += stringlength + 1;
  stringspaceused += stringlength + 1;
  if ((stringspaceused < (size_t) (stringspaceend - stringspacebegin)) ||
      (stringspaceend < stringspacebegin))
    {
      rcs_print_error
	("Inconsistant values for strinspacebegin=%p, stringspaceend=%p, ((stringspaceend - stringspacebegin)=%lu, stringspaceused=%lu\n",
	 stringspacebegin, stringspaceend,
	 (unsigned long)(stringspaceend - stringspacebegin), 
	 (unsigned long) stringspaceused);
      stringspaceused = (stringspaceend - stringspacebegin);
    }
  newptr = ret;
  ptr = str;
  int inside_cdata = 0;
  while (*ptr)
    {
      if (*ptr == '<' && !inside_cdata)
	{
	  if (!strncmp (ptr, "<![CDATA[", 9))
	    {
	      inside_cdata = 1;
	      ptr += 9;
	      continue;
	    }
	}
      if (*ptr == ']' && inside_cdata)
	{
	  if (!strncmp (ptr, "]]>", 3))
	    {
	      inside_cdata = 0;
	      ptr += 3;
	      continue;
	    }
	}
      if (*ptr == '&' && !inside_cdata)
	{
	  ptr++;
	  if (!strncmp (ptr, "amp;", 4))
	    {
	      *newptr = '&';
	      ptr += 4;
	    }
	  else if (!strncmp (ptr, "gt;", 3))
	    {
	      *newptr = '>';
	      ptr += 3;
	    }
	  else if (!strncmp (ptr, "lt;", 3))
	    {
	      *newptr = '<';
	      ptr += 3;
	    }
	  else if (!strncmp (ptr, "apos;", 5))
	    {
	      *newptr = '\'';
	      ptr += 5;
	    }
	  else if (!strncmp (ptr, "quot;", 5))
	    {
	      *newptr = '\"';
	      ptr += 5;
	    }
	  else if (!strncmp (ptr, "#x", 2))
	    {
	      ptr += 2;
	      tempbuf[0] = *ptr;
	      ptr++;
	      if (*ptr != ';')
		{
		  tempbuf[1] = *ptr;
		  ptr++;
		}
	      else
		{
		  tempbuf[1] = 0;
		}
	      tempbuf[2] = 0;
	      unsigned long ul = strtoul (tempbuf, 0, 16);
	      ul = ul & 0xFF;
	      char c = (char) ul;
	      *newptr = c;
	      ptr++;
	    }
	  else if (*ptr == '#')
	    {
	      tempbuf[0] = *ptr;
	      ptr++;
	      if (*ptr != ';')
		{
		  tempbuf[1] = *ptr;
		  ptr++;
		}
	      else
		{
		  tempbuf[1] = 0;
		}
	      if (*ptr != ';')
		{
		  tempbuf[2] = *ptr;
		  ptr++;
		}
	      else
		{
		  tempbuf[2] = 0;
		}
	      tempbuf[3] = 0;
	      unsigned long ul = strtoul (tempbuf, 0, 10);
	      ul = ul & 0xFF;
	      char c = (char) ul;
	      *newptr = c;
	    }
	  else
	    {
	      ptr--;
	      strncpy (tempbuf, ptr, 8);
	      tempbuf[7] = 0;
	      rcs_print_error ("CMS_XML_UPDATER::removeRefs bad ref %s\n",
			       tempbuf);
	      *newptr = '&';
	    }
	}
      else
	{
	  *newptr = *ptr;
	  ptr++;
	}
      newptr++;
    }

  return ret;
}



const char *
CMS_XML_UPDATER::insertRefs (const char *str, int len)
{
  const char *ptr;
  ptr = str;
  int refsneeded = 0;
  int addedstringlength = 0;
  const char *endstr = str + len;
  while (ptr < endstr && *ptr)
    {
      if (*ptr == '<' || *ptr == '>')
	{
	  addedstringlength += 3;
	  refsneeded++;
	}
      else if (*ptr == '&')
	{
	  addedstringlength += 4;
	  refsneeded++;
	}
      else if (*ptr == '\'' || *ptr == '\"' || *ptr == '\n' || *ptr == '\r')
	{
	  addedstringlength += 5;
	  refsneeded++;
	}
      else if (!isprint (*ptr))
	{
	  addedstringlength += 5;
	  refsneeded++;
	}
      ptr++;
    }
  int orig_string_length = (int) (ptr - str);
  if (!refsneeded && ptr < endstr)
    {
      return str;
    }
  if (orig_string_length > len)
    {
      orig_string_length = len;
    }
  int stringlength = orig_string_length + addedstringlength + 1;
  if ((stringspaceused < (size_t) (stringspaceend - stringspacebegin)) ||
      (stringspaceend < stringspacebegin))
    {
      rcs_print_error
	("Inconsistant values for strinspacebegin=%p, stringspaceend=%p, ((stringspaceend - stringspacebegin)=%lu, stringspaceused=%lu\n",
	 stringspacebegin, stringspaceend,
	 (unsigned long)(stringspaceend - stringspacebegin), 
	 (unsigned long) stringspaceused);
      stringspaceused = (stringspaceend - stringspacebegin);
    }
  if (stringlength + stringspaceused + 1 > stringspacesize)
    {
      stringspacesize =
	(stringlength + stringspaceused) + 256 + (512 -
						  (stringlength +
						   stringspaceused +
						   256) % 512);
      stringspacebegin =
	(char *) DEBUG_MALLOC (stringspacesize - stringspaceused);
      numstringstofree++;
      if (numstringstofree >= maxstringstofree)
	{
	  maxstringstofree += 16;
	  char **oldstringstofree = stringstofree;
	  stringstofree =
	    (char **) DEBUG_MALLOC (maxstringstofree * sizeof (char *));
	  memset (stringstofree, 0, maxstringstofree * sizeof (char *));
	  if (maxstringstofree > 16 && 0 != oldstringstofree)
	    {
	      memcpy (stringstofree, oldstringstofree,
		      (maxstringstofree - 16) * sizeof (char *));
	      DEBUG_FREE (oldstringstofree);
	    }
	}
      stringstofree[numstringstofree] = stringspacebegin;
      stringspaceend = stringspacebegin;
    }
  char *ret = (char *) stringspaceend;
  stringspaceend += stringlength + 1;
  stringspaceused += stringlength + 1;
  if ((stringspaceused < (size_t) (stringspaceend - stringspacebegin)) ||
      (stringspaceend < stringspacebegin))
    {
      rcs_print_error
	("Inconsistant values for strinspacebegin=%p, stringspaceend=%p, ((stringspaceend - stringspacebegin)=%lu, stringspaceused=%lu\n",
	 stringspacebegin, stringspaceend,
	 (unsigned long) (stringspaceend - stringspacebegin), 
	 (unsigned long) stringspaceused);
      stringspaceused = (stringspaceend - stringspacebegin);
    }
  char *newptr = ret;
  ptr = str;
  while (ptr < endstr && *ptr)
    {
      switch (*ptr)
	{
	case '\n':
	  strcpy (newptr, "&#x0A;");
	  newptr += 6;
	  break;

	case '\r':
	  strcpy (newptr, "&#x0D;");
	  newptr += 6;
	  break;

	case '<':
	  strcpy (newptr, "&lt;");
	  newptr += 4;
	  break;

	case '>':
	  strcpy (newptr, "&gt;");
	  newptr += 4;
	  break;

	case '&':
	  strcpy (newptr, "&amp;");
	  newptr += 5;
	  break;

	case '\'':
	  strcpy (newptr, "&apos;");
	  newptr += 6;
	  break;

	case '\"':
	  strcpy (newptr, "&quot;");
	  newptr += 6;
	  break;

	default:
	  if (isprint (*ptr))
	    {
	      *newptr = *ptr;
	      newptr++;
	    }
	  else
	    {
	      unsigned char uc = (unsigned char) (*ptr);
	      unsigned int ui = (unsigned int) uc;
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(newptr,(stringspacesize-stringspaceused)), 
					    "&#x%2.2X;", ui);
	      newptr += 6;
	    }
	  break;
	}
      ptr++;
    }
  *newptr = 0;
  return ret;
}

const char *
CMS_XML_UPDATER::hexToBinaryConvert (const char *str, int len)
{
  char bytes[3];
  bytes[2] = 0;
  int stringlength = (len + 1) / 2;
  if (((int) strlen (str)) > len)
    {
      rcs_print_error ("hexToBinaryConvert(%s,%d) string length too long.\n",
		       str, len);
      return 0;
    }
  if ((stringspaceused < (size_t) (stringspaceend - stringspacebegin)) ||
      (stringspaceend < stringspacebegin))
    {
      rcs_print_error
	("Inconsistant values for strinspacebegin=%p, stringspaceend=%p, ((stringspaceend - stringspacebegin)=%lu, stringspaceused=%lu\n",
	 stringspacebegin, stringspaceend,
	 (unsigned long) (stringspaceend - stringspacebegin), 
	 (unsigned long) stringspaceused);
      stringspaceused = (stringspaceend - stringspacebegin);
    }
  if (stringlength + stringspaceused + 1 > stringspacesize)
    {
      stringspacesize =
	(stringlength + stringspaceused) + 256 + (512 -
						  (stringlength +
						   stringspaceused +
						   256) % 512);
      stringspacebegin =
	(char *) DEBUG_MALLOC (stringspacesize - stringspaceused);
      numstringstofree++;
      if (numstringstofree >= maxstringstofree)
	{
	  maxstringstofree += 16;
	  char **oldstringstofree = stringstofree;
	  stringstofree =
	    (char **) DEBUG_MALLOC (maxstringstofree * sizeof (char *));
	  memset (stringstofree, 0, maxstringstofree * sizeof (char *));
	  if (maxstringstofree > 16 && 0 != oldstringstofree)
	    {
	      memcpy (stringstofree, oldstringstofree,
		      (maxstringstofree - 16) * sizeof (char *));
	      DEBUG_FREE (oldstringstofree);
	    }
	}
      stringstofree[numstringstofree] = stringspacebegin;
      stringspaceend = stringspacebegin;
    }
  char *ret = (char *) stringspaceend;
  stringspaceend += stringlength + 1;
  stringspaceused += stringlength + 1;
  if ((stringspaceused < (size_t) (stringspaceend - stringspacebegin)) ||
      (stringspaceend < stringspacebegin))
    {
      rcs_print_error
	("Inconsistant values for strinspacebegin=%p, stringspaceend=%p, ((stringspaceend - stringspacebegin)=%lu, stringspaceused=%lu\n",
	 stringspacebegin, stringspaceend,
	 (unsigned long) (stringspaceend - stringspacebegin), 
	 (unsigned long) stringspaceused);
      stringspaceused = (stringspaceend - stringspacebegin);
    }
  for (int i = 0; i < stringlength; i++)
    {
      bytes[0] = str[i * 2];
      bytes[1] = str[i * 2 + 1];
      int lasterrno = errno;
      long strtolret = strtol (bytes, 0, 16);
      if (strtolret > 256 || strtolret < 0
	  || ((errno == EINVAL || errno == ERANGE) && lasterrno != errno))
	{
	  rcs_print_error
	    ("Can't convert hex data to binary for string %s at position %d\n",
	     str, (i * 2));
	  return 0;
	}
      ret[i] = (char) strtolret;
    }
  ret[stringlength] = 0;
  return ret;
}

const char *
CMS_XML_UPDATER::binaryToHexConvert (const char *str, int len)
{
  char *ret = 0;
  unsigned char ucval = 0;
  unsigned long ulval = 0;
  int i = 0;
  int stringlength = len * 2 + 1;

  if ((stringspaceused < (size_t) (stringspaceend - stringspacebegin)) ||
      (stringspaceend < stringspacebegin))
    {
      rcs_print_error
	("Inconsistant values for strinspacebegin=%p, stringspaceend=%p, ((stringspaceend - stringspacebegin)=%lu, stringspaceused=%lu\n",
	 stringspacebegin, stringspaceend,
	 (unsigned long) (stringspaceend - stringspacebegin), 
	 (unsigned long) stringspaceused);
      stringspaceused = (stringspaceend - stringspacebegin);
    }
  if (stringlength + stringspaceused + 1 > stringspacesize)
    {
      stringspacesize =
	(stringlength + stringspaceused) + 256 + (512 -
						  (stringlength +
						   stringspaceused +
						   256) % 512);
      stringspacebegin =
	(char *) DEBUG_MALLOC (stringspacesize - stringspaceused);
      numstringstofree++;
      if (numstringstofree >= maxstringstofree)
	{
	  maxstringstofree += 16;
	  char **oldstringstofree = stringstofree;
	  stringstofree =
	    (char **) DEBUG_MALLOC (maxstringstofree * sizeof (char *));
	  memset (stringstofree, 0, maxstringstofree * sizeof (char *));
	  if (maxstringstofree > 16 && 0 != oldstringstofree)
	    {
	      memcpy (stringstofree, oldstringstofree,
		      (maxstringstofree - 16) * sizeof (char *));
	      DEBUG_FREE (oldstringstofree);
	    }
	}
      stringstofree[numstringstofree] = stringspacebegin;
      stringspaceend = stringspacebegin;
    }
  ret = (char *) stringspaceend;
  stringspaceend += stringlength + 1;
  stringspaceused += stringlength + 1;
  if ((stringspaceused < (size_t) (stringspaceend - stringspacebegin)) ||
      (stringspaceend < stringspacebegin))
    {
      rcs_print_error
	("Inconsistant values for strinspacebegin=%p, stringspaceend=%p, ((stringspaceend - stringspacebegin)=%lu, stringspaceused=%lu\n",
	 stringspacebegin, stringspaceend,
	 (unsigned long) (stringspaceend - stringspacebegin), 
	 (unsigned long) stringspaceused);
      stringspaceused = (stringspaceend - stringspacebegin);
    }
  for (i = 0; i < len; i++)
    {
      ucval = (unsigned char) str[i];
      ulval = (unsigned long) ucval;
      if (ulval > 255)
	{
	  rcs_print_error
	    ("something is very wrong and weird here. str=%s, str[i]=%d, ulval=%lu\n",
	     str, str[i], ulval);
	}
      SNPRINTF_FUNC ( SNPRINTF_ARGS((ret+(i*2)),3),
		      "%2.2lX", ulval);
    }
  ret[stringlength] = 0;
  return ret;
}


CMS_STATUS
CMS_XML_UPDATER::cancelSchemaGen (void)
{
  schema_gen_mode = 0;
  make_xml_pretty = 0;
  return CMS_STATUS_NOT_SET;
}

int
CMS_XML_UPDATER::is_schema_ready ()
{
  return ((schema_type_index > (last_check_type_info_list_length - 2)) &&
	  (last_check_type_info_list_length > 0));
}

CMS_STATUS
CMS_XML_UPDATER::update (bool &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);
  autovarnum++;
  return update_with_name (autovarname, x);
}


CMS_STATUS
CMS_XML_UPDATER::update (char &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);

  autovarnum++;
  return update_with_name (autovarname, x);
}

CMS_STATUS
CMS_XML_UPDATER::update (unsigned char &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x);
}


CMS_STATUS
CMS_XML_UPDATER::update (short int &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x);
}


CMS_STATUS
CMS_XML_UPDATER::update (unsigned short int &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x);
}


CMS_STATUS
CMS_XML_UPDATER::update (int &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x);
}


CMS_STATUS
CMS_XML_UPDATER::update (unsigned int &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x);
}


CMS_STATUS
CMS_XML_UPDATER::update (long int &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x);
}


CMS_STATUS
CMS_XML_UPDATER::update (unsigned long int &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x);
}


CMS_STATUS
CMS_XML_UPDATER::update (float &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x);
}


CMS_STATUS
CMS_XML_UPDATER::update (double &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x);
}


CMS_STATUS
CMS_XML_UPDATER::update (long double &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x);
}

CMS_STATUS
CMS_XML_UPDATER::update (bool *x, unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (len < 1)
    {
      return (status = CMS_UPDATE_ERROR);
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);
  autovarnum++;
  return update_with_name (autovarname, x, len);
}


CMS_STATUS
CMS_XML_UPDATER::update (char *x, unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (len < 1)


    {
      return (status = CMS_UPDATE_ERROR);
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);
  autovarnum++;
  return update_with_name (autovarname, x, len);
}

CMS_STATUS
CMS_XML_UPDATER::update (unsigned char *x, unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (len < 1)


    {
      return (status = CMS_UPDATE_ERROR);
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);
  autovarnum++;
  return update_with_name (autovarname, x, len);
}


CMS_STATUS
CMS_XML_UPDATER::update (short *x, unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x, len);
}


CMS_STATUS
CMS_XML_UPDATER::update (unsigned short *x, unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x, len);
}


CMS_STATUS
CMS_XML_UPDATER::update (int *x, unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x, len);
}


CMS_STATUS
CMS_XML_UPDATER::update (unsigned int *x, unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x, len);
}


CMS_STATUS
CMS_XML_UPDATER::update (long *x, unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x, len);
}


CMS_STATUS
CMS_XML_UPDATER::update (unsigned long *x, unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x, len);
}


CMS_STATUS
CMS_XML_UPDATER::update (float *x, unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x, len);
}


CMS_STATUS
CMS_XML_UPDATER::update (double *x, unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);


  autovarnum++;
  return update_with_name (autovarname, x, len);
}


CMS_STATUS
CMS_XML_UPDATER::update (long double *x, unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(autovarname,sizeof(autovarname)),
		  "autovar--%d", autovarnum);
  autovarnum++;
  return update_with_name (autovarname, x, len);
}



const char *
CMS_XML_UPDATER::findNodeString (const char *name)
{
  const char *ret;
  if (!schema_gen_mode)
    {
      if (0 != ((xmlNodePtr) currentElement))
	{
	  if (0 != ((xmlNodePtr) currentElement)->next)
	    {
#ifdef USE_LIBXML2
	      if ((make_xml_pretty && !content_set_for_this_class))
		{
		  if (0 != ((xmlNodePtr) currentElement)->next->next)
		    {
		      currentElement =
			((xmlNodePtr) currentElement)->next->next;
		    }
		}
	      else
		{
		  currentElement = ((xmlNodePtr) currentElement)->next;
		}
#else
	      currentElement = ((xmlNodePtr) currentElement)->next;
#endif
	    }
	}
    }
  while (((xmlNodePtr) currentElement) != NULL)
    {
      if (0 != ((xmlNodePtr) currentElement)->name)
	{
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "CMS_XML_UPDATER::findNodeString comparing %s with %s , currentElement=%s\n",
			   name, ((xmlNodePtr) currentElement)->name,
			   fullName (((xmlNodePtr) currentElement)));
#endif
	  if (!strcmp (name, (char *) ((xmlNodePtr) currentElement)->name))
	    {
	      if (0 != ((xmlNodePtr) currentElement)->children)
		{
		  ret = (char *) xmlNodeListGetString ((xmlDocPtr) doc,
						       ((xmlNodePtr)
							currentElement)->
						       children, 1);
		}
	      else
		{
		  ret = (char *) xmlNodeListGetString ((xmlDocPtr) doc,
						       ((xmlNodePtr)
							currentElement), 1);
		}
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "CMS_XML_UPDATER::findNodeString(%s) (currentElement = %s) returning %s\n",
			       name, fullName (((xmlNodePtr) currentElement)),
			       ret);
#endif
	      next_default=0;
	      return ret;
	    }
	}
      currentElement = ((xmlNodePtr) currentElement)->next;
    }
  if (0 != ((xmlNodePtr) currentNode))
    {
      currentElement = ((xmlNodePtr) currentNode)->children;
      while (((xmlNodePtr) currentElement) != NULL)
	{
	  if (0 != ((xmlNodePtr) currentElement)->name)
	    {
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "CMS_XML_UPDATER::findNodeString comparing %s with %s , currentElement=%s\n",
			       name, ((xmlNodePtr) currentElement)->name,
			       fullName (((xmlNodePtr) currentElement)));
#endif
	      if (!strcmp
		  (name, (char *) ((xmlNodePtr) currentElement)->name))
		{
		  if (0 != ((xmlNodePtr) currentElement)->children)
		    {
		      ret =
			(char *) xmlNodeListGetString ((xmlDocPtr) doc,
						       ((xmlNodePtr)
							currentElement)->
						       children, 1);
		    }
		  else
		    {
		      ret =
			(char *) xmlNodeListGetString ((xmlDocPtr) doc,
						       ((xmlNodePtr)
							currentElement), 1);
		    }
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "CMS_XML_UPDATER::findNodeString(%s) returning %s\n",
				   name, ret);
#endif
		  next_default=0;
		  return ret;
		}
	    }
	  currentElement = ((xmlNodePtr) currentElement)->next;
	}
    }
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::findNodeString could not find element %s in xml node %s\n",
		   name, ((char *) fullName (((xmlNodePtr) currentNode))));
#endif
  if(next_default != 0)
    {
      ret = next_default;
      next_default = 0;
      return ret;
    }
  return 0;
}


#define XSDTYPE(X) (xmlXsdType(xsdNs,"xsd:" X))

static inline const xmlChar *xmlXsdType(const xmlNsPtr ns, const char *tname)
{
    if(ns && !ns->qualify_elements)
    {
        return (xmlChar *) ((tname+4));
    }
    return (xmlChar *) tname;
}


CMS_STATUS
CMS_XML_UPDATER::update_attribute_with_name (const char *name, bool &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;
  if (encoding)
    {
      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr
	    newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:boolean");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("boolean"));
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  if (inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"unbounded\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  (xmlChar *) "unbounded");
	    }
	  else if (!add_array_indexes_to_name && max_occurs > 1)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(max_occurs_string,sizeof(max_occurs_string)), "%d", max_occurs);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"%s\"))\n",
			       fullName (newNode), max_occurs_string);
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  fakeStrdup (max_occurs_string));
	    }
	  return CMS_STATUS_NOT_SET;
	}


      if (0 == compareForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      if(x)
	{
	  xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
			   "true");
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,%s,%s);\n",
			   ((char *) fullName (((xmlNodePtr) currentNode))), name,
			   "true");
#endif
	}
      else
	{
	  xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
			   "false");
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,%s,%s);\n",
			   ((char *) fullName (((xmlNodePtr) currentNode))), name,
			   "false");
#endif
	}

    }
  else
    {
      const char *decodevaluestring = xmlGetProp(((xmlNodePtr)currentNode),(xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;
      if (0 != decodevaluestring)
	{
	  if(!strcmp(decodevaluestring,"true") 
	     || !strcmp(decodevaluestring,"1"))
	    {
	      x = true;
	    }
	  else if(!strcmp(decodevaluestring,"false") 
		  || !strcmp(decodevaluestring,"0"))
	    {
	      x = false;
	    }
	  else 
	    {
	      rcs_print_error("bad bool string %s\n",decodevaluestring);
	      return (status = CMS_UPDATE_ERROR);
	    }
	}
    }
  return CMS_STATUS_NOT_SET;
}

size_t cms_xml_strnlen(const char *s, size_t maxlen)
{
  if(!s)
    {
      return 0;
    }
  const char *sP = s;
  size_t len=0;
  while(len < maxlen && *sP)
    {
      len++;
      sP++;
    }
  return len;
}


CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name, char *x,
					       unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, x, sizeof (*x) * len) < 0)
    return status;
  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 45; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  whitespace[jj] = '\t';
	  jj++;
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute",0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",\"xsd:string\"))\n",
			   fullName (newNode));
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("string"));
	  return CMS_STATUS_NOT_SET;
	}

      size_t xstrlen = cms_xml_strnlen(x,len);
      if (xstrlen > len)
	{
	  xstrlen = len;
	}
      if (0 == compareAttributeForDiff (name, x, xstrlen))
	{
	  return CMS_STATUS_NOT_SET;
	}
      if (diffbuff != 0)
	{
	  if (*x == 0)
	    {
	      long offset = (long) ( x - ((char *) base));
	      if (0 == *((char *) diffbuff + offset))
		{
		  return CMS_STATUS_NOT_SET;
		}
	    }
	}
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (insertRefs (x, len)));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  strncpy (x, removeRefs (decodevaluestring), len);
	  x[len-1]=0;
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}



CMS_STATUS
CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					     short int *ixP,
					     unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  
  if (check_pointer_with_name (name, (char *)ixP, sizeof (*ixP) * len) < 0)
    return status;

  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 45; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  whitespace[jj] = '\t';
	  jj++;
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute",0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",\"xsd:string\"))\n",
			   fullName (newNode));
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("string"));
	  return CMS_STATUS_NOT_SET;
	}

      size_t space = len*10+1;
      char *x = (char *) stringSpaceAlloc((long)space);
      char *xendP = x;
      for(int i = 0; i < ((int) len) && xendP; i++)
	{
	  if(i > 0)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(xendP,space),",%d",ixP[i]);
	    }
	  else
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(xendP,space),"%d",ixP[i]);
	    }
	  size_t xilen = strlen(xendP);
	  space -= xilen;
	  xendP += xilen;
	}
	  
      size_t xstrlen = cms_xml_strnlen(x,len);
      if (xstrlen > len)
	{
	  xstrlen = len;
	}
      if (0 == compareAttributeForDiff (name, x, xstrlen))
	{
	  return CMS_STATUS_NOT_SET;
	}
      if (diffbuff != 0)
	{
	  if (*x == 0)
	    {
	      long offset = (long) ( x - ((char *) base));
	      if (0 == *((char *) diffbuff + offset))
		{
		  return CMS_STATUS_NOT_SET;
		}
	    }
	}
      xmlSetProp (((xmlNodePtr) currentNode), 
		  fakeStrdup (name),
		  (xmlChar *) x);      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  const char *xendP = decodevaluestring;
	  for(int i =0; i < ((int) len) && *xendP; i++)
	    {
	      char *newXendP =  (char *) xendP;
	      ixP[i] = (int) strtol(xendP,&newXendP,0);
	      xendP = newXendP;
	      if(*xendP == ' ' || *xendP == ',')
		{
		  xendP++;
		}
	    }
	  //strncpy (x, removeRefs (decodevaluestring), len);
	  //x[len-1]=0;
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					     unsigned short int *ixP,
					     unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  
  if (check_pointer_with_name (name, (char *)ixP, sizeof (*ixP) * len) < 0)
    return status;

  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 45; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  whitespace[jj] = '\t';
	  jj++;
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute",0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",\"xsd:string\"))\n",
			   fullName (newNode));
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("string"));
	  return CMS_STATUS_NOT_SET;
	}

      size_t space = len*10+1;
      char *x = (char *) stringSpaceAlloc((long) space);
      char *xendP = x;
      for(int i = 0; i < ((int) len) && xendP; i++)
	{
	  if(i > 0)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(xendP,space),",%d",ixP[i]);
	    }
	  else
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(xendP,space),"%d",ixP[i]);
	    }
	  size_t xilen = strlen(xendP);
	  space -= xilen;
	  xendP += xilen;
	}
	  
      size_t xstrlen = cms_xml_strnlen(x,len);
      if (xstrlen > len)
	{
	  xstrlen = len;
	}
      if (0 == compareAttributeForDiff (name, x, xstrlen))
	{
	  return CMS_STATUS_NOT_SET;
	}
      if (diffbuff != 0)
	{
	  if (*x == 0)
	    {
	      long offset = (long) ( x - ((char *) base));
	      if (0 == *((char *) diffbuff + offset))
		{
		  return CMS_STATUS_NOT_SET;
		}
	    }
	}
      xmlSetProp (((xmlNodePtr) currentNode), 
		  fakeStrdup (name),
		  (xmlChar *) x);      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  const char *xendP = decodevaluestring;
	  for(int i =0; i < ((int) len) && *xendP; i++)
	    {
	      char *newXendP =  (char *) xendP;
	      ixP[i] = (int) strtol(xendP,&newXendP,0);
	      xendP = newXendP;
	      if(*xendP == ' ' || *xendP == ',')
		{
		  xendP++;
		}
	    }
	  //strncpy (x, removeRefs (decodevaluestring), len);
	  //x[len-1]=0;
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					     int *ixP,
					     unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  
  if (check_pointer_with_name (name, (char *)ixP, sizeof (*ixP) * len) < 0)
    return status;

  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 45; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  whitespace[jj] = '\t';
	  jj++;
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute",0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",\"xsd:string\"))\n",
			   fullName (newNode));
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("string"));
	  return CMS_STATUS_NOT_SET;
	}

      size_t space = len*10+1;
      char *x = (char *) stringSpaceAlloc((long)space);
      char *xendP = x;
      for(int i = 0; i < ((int) len) && xendP; i++)
	{
	  if(i > 0)
	    {
	      SNPRINTF_FUNC( SNPRINTF_ARGS(xendP,space),",%d",ixP[i]);
	    }
	  else
	    {
	      SNPRINTF_FUNC( SNPRINTF_ARGS(xendP,space),"%d",ixP[i]);
	    }
	  size_t xilen = strlen(xendP);
	  space -= xilen;
	  xendP += xilen;
	}
	  
      size_t xstrlen = cms_xml_strnlen(x,len);
      if (xstrlen > len)
	{
	  xstrlen = len;
	}
      if (0 == compareAttributeForDiff (name, x, xstrlen))
	{
	  return CMS_STATUS_NOT_SET;
	}
      if (diffbuff != 0)
	{
	  if (*x == 0)
	    {
	      long offset = (long) ( x - ((char *) base));
	      if (0 == *((char *) diffbuff + offset))
		{
		  return CMS_STATUS_NOT_SET;
		}
	    }
	}
      xmlSetProp (((xmlNodePtr) currentNode), 
		  fakeStrdup (name),
		  (xmlChar *) x);      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  const char *xendP = decodevaluestring;
	  for(int i =0; i < ((int) len) && *xendP; i++)
	    {
	      char *newXendP =  (char *) xendP;
	      ixP[i] = (int) strtol(xendP,&newXendP,0);
	      xendP = newXendP;
	      if(*xendP == ' ' || *xendP == ',')
		{
		  xendP++;
		}
	    }
	  //strncpy (x, removeRefs (decodevaluestring), len);
	  //x[len-1]=0;
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					     unsigned int *ixP,
					     unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  
  if (check_pointer_with_name (name, (char *)ixP, sizeof (*ixP) * len) < 0)
    return status;

  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 45; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  whitespace[jj] = '\t';
	  jj++;
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute",0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",\"xsd:string\"))\n",
			   fullName (newNode));
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("string"));
	  return CMS_STATUS_NOT_SET;
	}

      size_t space = len*10+1;
      char *x = (char *) stringSpaceAlloc((long)space);
      char *xendP = x;
      for(int i = 0; i < ((int) len) && xendP; i++)
	{
	  if(i > 0)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(xendP,space),",%d",ixP[i]);
	    }
	  else
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(xendP,space),"%d",ixP[i]);
	    }
	  size_t xilen = strlen(xendP);
	  space -= xilen;
	  xendP += xilen;
	}
	  
      size_t xstrlen = cms_xml_strnlen(x,len);
      if (xstrlen > len)
	{
	  xstrlen = len;
	}
      if (0 == compareAttributeForDiff (name, x, xstrlen))
	{
	  return CMS_STATUS_NOT_SET;
	}
      if (diffbuff != 0)
	{
	  if (*x == 0)
	    {
	      long offset = (long) ( x - ((char *) base));
	      if (0 == *((char *) diffbuff + offset))
		{
		  return CMS_STATUS_NOT_SET;
		}
	    }
	}
      xmlSetProp (((xmlNodePtr) currentNode), 
		  fakeStrdup (name),
		  (xmlChar *) x);      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  const char *xendP = decodevaluestring;
	  for(int i =0; i < ((int) len) && *xendP; i++)
	    {
	      char *newXendP =  (char *) xendP;
	      ixP[i] = (int) strtoul(xendP,&newXendP,0);
	      xendP = newXendP;
	      if(*xendP == ' ' || *xendP == ',')
		{
		  xendP++;
		}
	    }
	  //strncpy (x, removeRefs (decodevaluestring), len);
	  //x[len-1]=0;
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					     long int *ixP,
					     unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  
  if (check_pointer_with_name (name, (char *)ixP, sizeof (*ixP) * len) < 0)
    return status;

  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 45; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  whitespace[jj] = '\t';
	  jj++;
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute",0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",\"xsd:string\"))\n",
			   fullName (newNode));
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("string"));
	  return CMS_STATUS_NOT_SET;
	}

      size_t space = len*10+1;
      char *x = (char *) stringSpaceAlloc((long)space);
      char *xendP = x;
      for(int i = 0; i < ((int) len) && xendP; i++)
	{
	  if(i > 0)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(xendP,space),",%ld",ixP[i]);
	    }
	  else
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(xendP,space),"%ld",ixP[i]);
	    }
	  size_t xilen = strlen(xendP);
	  space -= xilen;
	  xendP += xilen;
	}
	  
      size_t xstrlen = cms_xml_strnlen(x,len);
      if (xstrlen > len)
	{
	  xstrlen = len;
	}
      if (0 == compareAttributeForDiff (name, x, xstrlen))
	{
	  return CMS_STATUS_NOT_SET;
	}
      if (diffbuff != 0)
	{
	  if (*x == 0)
	    {
	      long offset = (long) ( x - ((char *) base));
	      if (0 == *((char *) diffbuff + offset))
		{
		  return CMS_STATUS_NOT_SET;
		}
	    }
	}
      xmlSetProp (((xmlNodePtr) currentNode), 
		  fakeStrdup (name),
		  (xmlChar *) x);      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  const char *xendP = decodevaluestring;
	  for(int i =0; i < ((int) len) && *xendP; i++)
	    {
	      char *newXendP =  (char *) xendP;
	      ixP[i] = (int) strtol(xendP,&newXendP,0);
	      xendP = newXendP;
	      if(*xendP == ' ' || *xendP == ',')
		{
		  xendP++;
		}
	    }
	  //strncpy (x, removeRefs (decodevaluestring), len);
	  //x[len-1]=0;
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					     unsigned long int *ixP,
					     unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  
  if (check_pointer_with_name (name, (char *)ixP, sizeof (*ixP) * len) < 0)
    return status;

  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 45; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  whitespace[jj] = '\t';
	  jj++;
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute",0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",\"xsd:string\"))\n",
			   fullName (newNode));
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("string"));
	  return CMS_STATUS_NOT_SET;
	}

      size_t space = len*10+1;
      char *x = (char *) stringSpaceAlloc((long)space);
      char *xendP = x;
      for(int i = 0; i < ((int) len) && xendP; i++)
	{
	  if(i > 0)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(xendP,space),",%lu",ixP[i]);
	    }
	  else
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(xendP,space),"%lu",ixP[i]);
	    }
	  size_t xilen = strlen(xendP);
	  space -= xilen;
	  xendP += xilen;
	}
	  
      size_t xstrlen = cms_xml_strnlen(x,len);
      if (xstrlen > len)
	{
	  xstrlen = len;
	}
      if (0 == compareAttributeForDiff (name, x, xstrlen))
	{
	  return CMS_STATUS_NOT_SET;
	}
      if (diffbuff != 0)
	{
	  if (*x == 0)
	    {
	      long offset = (long) ( x - ((char *) base));
	      if (0 == *((char *) diffbuff + offset))
		{
		  return CMS_STATUS_NOT_SET;
		}
	    }
	}
      xmlSetProp (((xmlNodePtr) currentNode), 
		  fakeStrdup (name),
		  (xmlChar *) x);      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  const char *xendP = decodevaluestring;
	  for(int i =0; i < ((int) len) && *xendP; i++)
	    {
	      char *newXendP =  (char *) xendP;
	      ixP[i] = (int) strtoul(xendP,&newXendP,0);
	      xendP = newXendP;
	      if(*xendP == ' ' || *xendP == ',')
		{
		  xendP++;
		}
	    }
	  //strncpy (x, removeRefs (decodevaluestring), len);
	  //x[len-1]=0;
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					     float *ixP,
					     unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  
  if (check_pointer_with_name (name, (char *)ixP, sizeof (*ixP) * len) < 0)
    return status;

  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 45; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  whitespace[jj] = '\t';
	  jj++;
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute",0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",\"xsd:string\"))\n",
			   fullName (newNode));
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("string"));
	  return CMS_STATUS_NOT_SET;
	}

      size_t space = len*10+1;
      char *x = (char *) stringSpaceAlloc((long)space);
      char *xendP = x;
      for(int i = 0; i < ((int) len) && xendP; i++)
	{
	  if(i > 0)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(xendP,space),",%f",ixP[i]);
	    }
	  else
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(xendP,space),"%f",ixP[i]);
	    }
	  size_t xilen = strlen(xendP);
	  space -= xilen;
	  xendP += xilen;
	}
	  
      size_t xstrlen = cms_xml_strnlen(x,len);
      if (xstrlen > len)
	{
	  xstrlen = len;
	}
      if (0 == compareAttributeForDiff (name, x, xstrlen))
	{
	  return CMS_STATUS_NOT_SET;
	}
      if (diffbuff != 0)
	{
	  if (*x == 0)
	    {
	      long offset = (long) ( x - ((char *) base));
	      if (0 == *((char *) diffbuff + offset))
		{
		  return CMS_STATUS_NOT_SET;
		}
	    }
	}
      xmlSetProp (((xmlNodePtr) currentNode), 
		  fakeStrdup (name),
		  (xmlChar *) x);      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  const char *xendP = decodevaluestring;
	  for(int i =0; i < ((int) len) && *xendP; i++)
	    {
	      char *newXendP =  (char *) xendP;
	      ixP[i] = (float) strtod(xendP,&newXendP);
	      xendP = newXendP;
	      if(*xendP == ' ' || *xendP == ',')
		{
		  xendP++;
		}
	    }
	  //strncpy (x, removeRefs (decodevaluestring), len);
	  //x[len-1]=0;
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					     double *ixP,
					     unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  
  if (check_pointer_with_name (name, (char *)ixP, sizeof (*ixP) * len) < 0)
    return status;

  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 45; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  whitespace[jj] = '\t';
	  jj++;
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute",0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",\"xsd:string\"))\n",
			   fullName (newNode));
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("string"));
	  return CMS_STATUS_NOT_SET;
	}

      size_t space = len*10+1;
      char *x = (char *) stringSpaceAlloc((long)space);
      char *xendP = x;
      for(int i = 0; i < ((int) len) && xendP; i++)
	{
	  if(i > 0)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(xendP,space),",%f",ixP[i]);
	    }
	  else
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(xendP,space),"%f",ixP[i]);
	    }
	  size_t xilen = strlen(xendP);
	  space -= xilen;
	  xendP += xilen;
	}
	  
      size_t xstrlen = cms_xml_strnlen(x,len);
      if (xstrlen > len)
	{
	  xstrlen = len;
	}
      if (0 == compareAttributeForDiff (name, x, xstrlen))
	{
	  return CMS_STATUS_NOT_SET;
	}
      if (diffbuff != 0)
	{
	  if (*x == 0)
	    {
	      long offset = (long) ( x - ((char *) base));
	      if (0 == *((char *) diffbuff + offset))
		{
		  return CMS_STATUS_NOT_SET;
		}
	    }
	}
      xmlSetProp (((xmlNodePtr) currentNode), 
		  fakeStrdup (name),
		  (xmlChar *) x);      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  const char *xendP = decodevaluestring;
	  for(int i =0; i < ((int) len) && *xendP; i++)
	    {
	      char *newXendP =  (char *) xendP;
	      ixP[i] = strtod(xendP,&newXendP);
	      xendP = newXendP;
	      if(*xendP == ' ' || *xendP == ',')
		{
		  xendP++;
		}
	    }
	  //strncpy (x, removeRefs (decodevaluestring), len);
	  //x[len-1]=0;
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name, char &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;
  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:byte");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("byte"));
	  {
	    return CMS_STATUS_NOT_SET;
	  }
	}


      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%d", x);
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif
    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  x = (char) strtol (decodevaluestring, 0, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					       unsigned char &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;
  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:unsignedByte");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type",
		      (xmlChar *) "xsd:unsignedByte");
	  return CMS_STATUS_NOT_SET;
	}


      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%d", x);
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif
    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  x = (char) strtoul (decodevaluestring, 0, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name, short &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;
  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:short");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("short"));
	  return CMS_STATUS_NOT_SET;
	}


      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%d", x);
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif
    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  x = (short) strtol (decodevaluestring, 0, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					       unsigned short &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;
  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:unsignedShort");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type",
		      (xmlChar *) "xsd:unsignedShort");
	  return CMS_STATUS_NOT_SET;
	}


      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%d", x);
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif
    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  x = (unsigned short) strtoul (decodevaluestring, 0, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name, int &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;
  if (encoding)
    {
      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:int");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("int"));
	  return CMS_STATUS_NOT_SET;
	}


      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%d", x);
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif
    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  x = (int) strtol (decodevaluestring, 0, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}



CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					       unsigned int &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;
  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:unsignedInt");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type",
		      (xmlChar *) "xsd:unsignedInt");
	  return CMS_STATUS_NOT_SET;
	}


      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%d", x);
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif
    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  x = (unsigned int) strtoul (decodevaluestring, 0, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name, long &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;
  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:long");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("long"));
	  return CMS_STATUS_NOT_SET;
	}


      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%ld", x);
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif
    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  x = (long) strtol (decodevaluestring, 0, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					       unsigned long &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;
  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:unsignedLong");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type",
		      (xmlChar *) "xsd:unsignedLong");
	  return CMS_STATUS_NOT_SET;
	}


      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%ld", x);
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif
    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  x = (unsigned long) strtoul (decodevaluestring, 0, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name, float &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;
  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:float");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("float"));
	  return CMS_STATUS_NOT_SET;
	}


      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%f", x);
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif
    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  x = (float) strtod (decodevaluestring, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name, double &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;
  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:double");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("double"));
	  return CMS_STATUS_NOT_SET;
	}


      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%f", x);
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif
    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  x = (double) strtod (decodevaluestring, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					       long double &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;
  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count]-1 : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:double");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("double"));
	  return CMS_STATUS_NOT_SET;
	}


      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

#ifdef MS_WINDOWS_API
      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%f", (double)x);
#else
      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%Lf", x);
#endif
      
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif
    }
  else
    {
      const char *decodevaluestring =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if(0 == decodevaluestring && next_default != 0)
	{
	  decodevaluestring=next_default;
	}
      next_default=0;

      if (0 != decodevaluestring)
	{
	  x = (double) strtod (decodevaluestring, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_XML_UPDATER::update_with_name (const char *name, bool &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;

  last_var_was_struct=0;
  if (encoding)
    {
      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr
	    newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:boolean");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("boolean"));
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  if (inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"unbounded\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  (xmlChar *) "unbounded");
	    }
	  else if (!add_array_indexes_to_name && max_occurs > 1)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(max_occurs_string,sizeof(max_occurs_string)), "%d", max_occurs);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"%s\"))\n",
			       fullName (newNode), max_occurs_string);
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  fakeStrdup (max_occurs_string));
	    }
	  return CMS_STATUS_NOT_SET;
	}


      if (0 == compareForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      if(x)
	{
	  xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
			   "true");
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,0,%s,%s);\n",
			   ((char *) fullName (((xmlNodePtr) currentNode))), name,
			   "true");
#endif
	}
      else
	{
	  xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
			   "false");
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,0,%s,%s);\n",
			   ((char *) fullName (((xmlNodePtr) currentNode))), name,
			   "false");
#endif
	}

    }
  else
    {
      const char *decodevaluestring = 0;
      if(*name == 0)
	{
	  decodevaluestring = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	  if(0 == decodevaluestring && next_default != 0)
	    {
	      decodevaluestring=next_default;
	    }
	  next_default=0;
	}
      else
	{
	  decodevaluestring = findNodeString (name);
	}
      if (0 != decodevaluestring)
	{
	  if(!strcmp(decodevaluestring,"true") || !strcmp(decodevaluestring,"1"))
	    {
	      x = true;
	    }
	  else if(!strcmp(decodevaluestring,"false") || !strcmp(decodevaluestring,"0"))
	    {
	      x = false;
	    }
	  else 
	    {
	      rcs_print_error("bad bool string %s\n",decodevaluestring);
	      return (status = CMS_UPDATE_ERROR);
	    }
	}
    }
  return CMS_STATUS_NOT_SET;
}



CMS_STATUS
CMS_XML_UPDATER::update_with_name (const char *name, char &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;

  last_var_was_struct=0;

  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr
	    newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:byte");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("byte"));
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  if (inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"unbounded\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  (xmlChar *) "unbounded");
	    }
	  else if (!add_array_indexes_to_name && max_occurs > 1)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(max_occurs_string,sizeof(max_occurs_string)), "%d", max_occurs);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"%s\"))\n",
			       fullName (newNode), max_occurs_string);
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  fakeStrdup (max_occurs_string));
	    }
	  return CMS_STATUS_NOT_SET;
	}


      if (0 == compareForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%d", x);
      if(*name == 0)
	{
	  xmlNodeSetContent(((xmlNodePtr)currentNode),
			    fakeStrdup(valuestring));
	  content_set_for_this_class=1;
	}
      else
	{

	  xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
			   fakeStrdup (valuestring));
	}
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif

    }
  else
    {
      const char *decodevaluestring = 0;
      if(*name == 0)
	{
	  decodevaluestring = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	  if(0 == decodevaluestring && next_default != 0)
	    {
	      decodevaluestring=next_default;
	    }
	  next_default=0;
	}
      else
	{
	  decodevaluestring = findNodeString (name);
	}
      if (0 != decodevaluestring)
	{
	  x = (char) strtol (decodevaluestring, 0, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}



CMS_STATUS
CMS_XML_UPDATER::update_with_name (const char *name, unsigned char &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;

  last_var_was_struct=0;

  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:unsignedByte");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type",
		      (xmlChar *) "xsd:unsignedByte");
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  if (inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"unbounded\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  (xmlChar *) "unbounded");
	    }
	  else if (!add_array_indexes_to_name && max_occurs > 1)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(max_occurs_string,sizeof(max_occurs_string)), "%d", max_occurs);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"%s\"))\n",
			       fullName (newNode), max_occurs_string);
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  fakeStrdup (max_occurs_string));
	    }
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}
      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%d", x);
      if(*name == 0)
	{
	  xmlNodeSetContent(((xmlNodePtr)currentNode),
			    fakeStrdup(valuestring));
	  content_set_for_this_class=1;
	}
      else
	{

	  xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
			   fakeStrdup (valuestring));
	}

      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring = 0;
      if(*name == 0)
	{
	  decodevaluestring = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	  if(0 == decodevaluestring && next_default != 0)
	    {
	      decodevaluestring=next_default;
	    }
	  next_default=0;
	}
      else
	{
	  decodevaluestring = findNodeString (name);
	}
      if (0 != decodevaluestring)
	{
	  x = (unsigned char) strtoul (decodevaluestring, 0, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
CMS_XML_UPDATER::update_with_name (const char *name, short int &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    return CMS_STATUS_NOT_SET;

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;

  last_var_was_struct=0;

  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class && *name != 0))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr
	    newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:short");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("short"));
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  if (inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"unbounded\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  (xmlChar *) "unbounded");
	    }
	  else if (!add_array_indexes_to_name && max_occurs > 1)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(max_occurs_string,sizeof(max_occurs_string)), "%d", max_occurs);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"%s\"))\n",
			       fullName (newNode), max_occurs_string);
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  fakeStrdup (max_occurs_string));
	    }
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%d", x);
      if(*name == 0)
	{
	  xmlNodeSetContent(((xmlNodePtr)currentNode),
			    fakeStrdup(valuestring));
	  content_set_for_this_class=1;
	}
      else
	{

	  xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
			   fakeStrdup (valuestring));
	}
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring = 0;
      if(*name == 0)
	{
	  decodevaluestring = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	  if(0 == decodevaluestring && next_default != 0)
	    {
	      decodevaluestring=next_default;
	    }
	  next_default=0;
	}
      else
	{
	  decodevaluestring = findNodeString (name);
	}
      if (0 != decodevaluestring)
	{
	  x = atoi (decodevaluestring);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
CMS_XML_UPDATER::update_with_name (const char *name, unsigned short int &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;

  last_var_was_struct=0;

  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:unsignedShort");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type",
		      (xmlChar *) "xsd:unsignedShort");
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  if (inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"unbounded\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  (xmlChar *) "unbounded");
	    }
	  else if (!add_array_indexes_to_name && max_occurs > 1)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(max_occurs_string,sizeof(max_occurs_string)), "%d", max_occurs);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"%s\"))\n",
			       fullName (newNode), max_occurs_string);
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  fakeStrdup (max_occurs_string));
	    }
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%d", x);
      xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
		       fakeStrdup (valuestring));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring = 0;
      if(*name == 0)
	{
	  decodevaluestring = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	  if(0 == decodevaluestring && next_default != 0)
	    {
	      decodevaluestring=next_default;
	    }
	  next_default=0;
	}
      else
	{
	  decodevaluestring = findNodeString (name);
	}
      if (0 != decodevaluestring)
	{
	  x = (unsigned short) strtoul (decodevaluestring, 0, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_XML_UPDATER::update_with_name (const char *name, int &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;

  last_var_was_struct=0;

  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class && *name != 0))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr
	    newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:int");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("int"));
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  if (inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"unbounded\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  (xmlChar *) "unbounded");
	    }
	  else if (!add_array_indexes_to_name && max_occurs > 1)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(max_occurs_string,sizeof(max_occurs_string)), "%d", max_occurs);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"%s\"))\n",
			       fullName (newNode), max_occurs_string);
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  fakeStrdup (max_occurs_string));
	    }
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%d", x);
      if(*name == 0)
	{
	  xmlNodeSetContent(((xmlNodePtr)currentNode),
			    fakeStrdup(valuestring));
	  content_set_for_this_class=1;
	}
      else
	{

	  xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
			   fakeStrdup (valuestring));
	}
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring = 0;
      if(*name == 0)
	{
	  decodevaluestring = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	  if(0 == decodevaluestring && next_default != 0)
	    {
	      decodevaluestring=next_default;
	    }
	  next_default=0;
	}
      else
	{
	  decodevaluestring = findNodeString (name);
	}
      if (0 != decodevaluestring)
	{
	  x = atoi (decodevaluestring);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_XML_UPDATER::update_with_name (const char *name, unsigned int &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;

  last_var_was_struct=0;

  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:unsignedInt");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type",
		      (xmlChar *) "xsd:unsignedInt");
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  if (inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"unbounded\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  (xmlChar *) "unbounded");
	    }
	  else if (!add_array_indexes_to_name && max_occurs > 1)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(max_occurs_string,sizeof(max_occurs_string)), "%d", max_occurs);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"%s\"))\n",
			       fullName (newNode), max_occurs_string);
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  fakeStrdup (max_occurs_string));
	    }
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%u", x);
      if(*name == 0)
	{
	  xmlNodeSetContent(((xmlNodePtr)currentNode),
			    fakeStrdup(valuestring));
	  content_set_for_this_class=1;
	}
      else
	{

	  xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
			   fakeStrdup (valuestring));
	}
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring = 0;
      if(*name == 0)
	{
	  decodevaluestring = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	  if(0 == decodevaluestring && next_default != 0)
	    {
	      decodevaluestring=next_default;
	    }
	  next_default=0;
	}
      else
	{
	  decodevaluestring = findNodeString (name);
	}
      if (0 != decodevaluestring)
	{
	  x = strtoul (decodevaluestring, 0, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_XML_UPDATER::update_with_name (const char *name, long int &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;

  last_var_was_struct=0;

  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr
	    newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:int");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("int"));
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  if (inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"unbounded\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  (xmlChar *) "unbounded");
	    }
	  else if (!add_array_indexes_to_name && max_occurs > 1)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(max_occurs_string,sizeof(max_occurs_string)), "%d", max_occurs);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"%s\"))\n",
			       fullName (newNode), max_occurs_string);
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  fakeStrdup (max_occurs_string));
	    }
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%ld", x);
      if(*name == 0)
	{
	  xmlNodeSetContent(((xmlNodePtr)currentNode),
			    fakeStrdup(valuestring));
	  content_set_for_this_class=1;
	}
      else
	{

	  xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
			   fakeStrdup (valuestring));
	}

      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring = 0;
      if(*name == 0)
	{
	  decodevaluestring = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	  if(0 == decodevaluestring && next_default != 0)
	    {
	      decodevaluestring=next_default;
	    }
	  next_default=0;
	}
      else
	{
	  decodevaluestring = findNodeString (name);
	}
      if (0 != decodevaluestring)
	{
	  x = strtol (decodevaluestring, 0, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_XML_UPDATER::update_with_name (const char *name, unsigned long int &x)
{

  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;

  last_var_was_struct=0;

  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:unsignedInt");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type",
		      (xmlChar *) "xsd:unsignedInt");
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  if (inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"unbounded\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  (xmlChar *) "unbounded");
	    }
	  else if (!add_array_indexes_to_name && max_occurs > 1)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(max_occurs_string,sizeof(max_occurs_string)), "%d", max_occurs);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"%s\"))\n",
			       fullName (newNode), max_occurs_string);
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  fakeStrdup (max_occurs_string));
	    }
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%lu", x);
      if(*name == 0)
	{
	  xmlNodeSetContent(((xmlNodePtr)currentNode),
			    fakeStrdup(valuestring));
	  content_set_for_this_class=1;
	}
      else
	{

	  xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
			   fakeStrdup (valuestring));
	}
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring = 0;
      if(*name == 0)
	{
	  decodevaluestring = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	  if(0 == decodevaluestring && next_default != 0)
	    {
	      decodevaluestring=next_default;
	    }
	  next_default=0;
	}
      else
	{
	  decodevaluestring = findNodeString (name);
	}
      if (0 != decodevaluestring)
	{
	  x = strtoul (decodevaluestring, 0, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_XML_UPDATER::update_with_name (const char *name, float &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;

  last_var_was_struct=0;

  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr
	    newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:float");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("float"));
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\")\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  if (inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"unbounded\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  (xmlChar *) "unbounded");
	    }
	  else if (!add_array_indexes_to_name && max_occurs > 1)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(max_occurs_string,sizeof(max_occurs_string)), "%d", max_occurs);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"%s\"))\n",
			       fullName (newNode), max_occurs_string);
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  fakeStrdup (max_occurs_string));
	    }
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%f", x);
      
      char *valptr = valuestring;
      while (*valptr == '0' && *(valptr + 1) != '.')
	{
	  valptr++;
	}
      char *endvalptr = valptr + strlen (valptr) - 1;
      if (!strchr (valptr, 'e') && !strchr (valptr, 'E')
	  && strchr (valptr, '.'))
	{
	  while (*endvalptr == '0' && *(endvalptr - 1) != '.')
	    {
	      *endvalptr = 0;
	      endvalptr--;
	    }
	}
      if(*name == 0)
	{
	  xmlNodeSetContent(((xmlNodePtr)currentNode),
			    fakeStrdup(valuestring));
	  content_set_for_this_class=1;
	}
      else
	{

	  xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
			   fakeStrdup (valuestring));
	}
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring = 0;
      if(*name == 0)
	{
	  decodevaluestring = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	  if(0 == decodevaluestring && next_default != 0)
	    {
	      decodevaluestring=next_default;
	    }
	  next_default=0;
	}
      else
	{
	  decodevaluestring = findNodeString (name);
	}
      if (0 != decodevaluestring)
	{
#ifdef HAVE_STRTOF
	  x = strtof (decodevaluestring, 0);
#else
	  x = (float) strtod (decodevaluestring, 0);
#endif

#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_XML_UPDATER::update_with_name (const char *name, double &x)
{
  const char *compare_addr;
  unsigned long offset;
  double dforcompare;
  double ddiff;
  char *valptr;
  char *endvalptr;
  int jj;


  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    {
      return status;
    }

  last_var_was_struct=0;

  if (encoding)
    {
       if (0 == compareForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      if (diffbuff != 0 && base != 0 && !schema_gen_mode)
	{
	  compare_addr = 0;
	  offset = 0;
	  dforcompare = 0;
	  ddiff = 0;
	  offset = (unsigned long) (((char *) &x) - ((char *) base));
	  if(offset%sizeof(double) == 0 )
	    {
	      if (offset > 0 && (offset + sizeof (double)) < diffbuff_size)
		{
		  const char *ccdiffbuff = (const char *) diffbuff;
		  compare_addr = ccdiffbuff + offset;
		  if(((unsigned long)compare_addr)%sizeof(double) == 0)
		    {
		      //memcpy (&dforcompare, compare_addr, sizeof (double));
		      dforcompare = *((double *) compare_addr);	      
		      ddiff = (dforcompare - x);
		      if (ddiff < 1E-12 && ddiff > -1E-12)
			{
			  return CMS_STATUS_NOT_SET;
			}
		    }
		}
	    }
	}

      jj = 0;

      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr
	    newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:double");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("double"));
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  if (inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"unbounded\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  (xmlChar *) "unbounded");
	    }
	  else if (!add_array_indexes_to_name && max_occurs > 1)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(max_occurs_string,sizeof(max_occurs_string)), "%d", max_occurs);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"%s\"))\n",
			       fullName (newNode), max_occurs_string);
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  fakeStrdup (max_occurs_string));
	    }
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%12.12f", x);
      valptr = valuestring;
      while (*valptr == '0' && *(valptr + 1) != '.')
	{
	  valptr++;
	}
      endvalptr = valptr + strlen (valptr) - 1;
      if (!strchr (valptr, 'e') && !strchr (valptr, 'E')
	  && strchr (valptr, '.'))
	{
	  while (*endvalptr == '0' && *(endvalptr - 1) != '.')
	    {
	      *endvalptr = 0;
	      endvalptr--;
	    }
	}

      if(*name == 0)
	{
	  xmlNodeSetContent(((xmlNodePtr)currentNode),
			    fakeStrdup(valuestring));
	  content_set_for_this_class=1;
	}
      else
	{

	  xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
			   fakeStrdup (valuestring));
	}

      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring = 0;
      if(*name == 0)
	{
	  decodevaluestring = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	  if(0 == decodevaluestring && next_default != 0)
	    {
	      decodevaluestring=next_default;
	    }
	  next_default=0;
	}
      else
	{
	  decodevaluestring = findNodeString (name);
	}
      if (0 != decodevaluestring)
	{
	  x = strtod (decodevaluestring, 0);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, long double &x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;

  last_var_was_struct=0;

  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:double");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("double"));
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  if (inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"unbounded\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  (xmlChar *) "unbounded");
	    }
	  else if (!add_array_indexes_to_name && max_occurs > 1)
	    {
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(max_occurs_string,sizeof(max_occurs_string)), "%d", max_occurs);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"maxOccurs\",\"%s\"))\n",
			       fullName (newNode), max_occurs_string);
#endif
	      xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			  fakeStrdup (max_occurs_string));
	    }
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

#if MS_WINDOWS_API
      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%f",(double) x);
#else
      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%Lf", x);
#endif

      char *valptr = valuestring;
      while (*valptr == '0' && *(valptr + 1) != '.')
	{
	  valptr++;
	}
      char *endvalptr = valptr + strlen (valptr) - 1;
      if (!strchr (valptr, 'e') && !strchr (valptr, 'E')
	  && strchr (valptr, '.'))
	{
	  while (*endvalptr == '0' && *(endvalptr - 1) != '.')
	    {
	      *endvalptr = 0;
	      endvalptr--;
	    }
	}
      if(*name == 0)
	{
	  xmlNodeSetContent(((xmlNodePtr)currentNode),
			    fakeStrdup(valuestring));
	  content_set_for_this_class=1;
	}
      else
	{

	  xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
			   fakeStrdup (valuestring));
	}
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valptr);
#endif


    }
  else
    {
      const char *decodevaluestring = 0;
      if(*name == 0)
	{
	  decodevaluestring = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	  if(0 == decodevaluestring && next_default != 0)
	    {
	      decodevaluestring=next_default;
	    }
	  next_default=0;
	}
      else
	{
	  decodevaluestring = findNodeString (name);
	}
      if (0 != decodevaluestring)
	{
#ifdef HAVE_STRTOLD
	  x = strtold (decodevaluestring, 0);
#else
	  x = strtod (decodevaluestring, 0);
#endif
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, bool *x,
				     unsigned int len)
{
  const char *orig_next_default = next_default;
  unsigned int i;

  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) x, sizeof (double) * len) < 0)
    {
      return status;
    }

  if (encoding && !schema_gen_mode
      && 0 == compareForDiff (name, x, len * sizeof (*x)))
    {
      return CMS_STATUS_NOT_SET;
    }

  max_occurs = len;
  if (add_array_indexes_to_name)
    {
      size_t namelen = strlen (name);
      if (namelen > sizeof (arrayvarname) - 20)
	{
	  rcs_print_error ("name %s is too long\n", name);
	  return (status = CMS_UPDATE_ERROR);
	}
      strcpy (arrayvarname, name);
      for (i = 0; i < len; i++)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(arrayvarname+namelen,sizeof(arrayvarname)-namelen),
			  "-%d", i);
	  next_default = orig_next_default;
	  update_with_name (arrayvarname, x[i]);
	}
    }
  else
    {
      for (i = 0; i < len; i++)
	{
	  next_default = orig_next_default;
	  update_with_name (name, x[i]);
	  if (schema_gen_mode)
	    {
	      break;
	    }
	}
    }
  max_occurs = 1;
  next_default=0;

  return CMS_STATUS_NOT_SET;
}



CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, char *x,
				     unsigned int len)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, x, sizeof (*x) * len) < 0)
    return status;
  if (encoding)
    {
      if(*name == 0)
	{
	  if(schema_gen_mode)
	    {
	      xmlNodePtr parentNode = ((xmlNodePtr)currentNode)->parent;
	      if(parentNode)
		{
		  xmlNodePtr origChildren = parentNode->children;
		  xmlNodePtr origLast = parentNode->last;
		  const xmlChar *orig_content = parentNode->content;
		  parentNode->children = 0;
		  parentNode->last = 0;
		  parentNode->content = 0;
		  xmlNodePtr newNode = xmlNewTextChild(parentNode,(xmlNsPtr)xsdNs,"simpleContent",0);
		  xmlNodePtr newNodeChild = xmlNewTextChild(newNode,(xmlNsPtr)xsdNs,"extension",0);
		  xmlSetProp(newNodeChild,"base","xsd:string");
		  xmlNodeSetContent(newNodeChild,orig_content);
		  newNodeChild->children = origChildren;
		  newNodeChild->last = origLast;
		  currentNode = newNodeChild;
		  xmlNodePtr n = newNodeChild->children;
		  while(n)
		    {
		      if(n->name)
			{
			  if(!strcmp(n->name,"sequence") || !strcmp(n->name,"xsd:sequence"))
			   {
			      xmlNodePtr nn = n->next;
			      if(nn)
				{
				  nn->parent = newNodeChild;
				}
			      n->parent=newNodeChild;
			      xmlUnlinkNode(n);
			      xmlFreeNode(n);
			      n = nn;
			      continue;
			    }
			}
		      n->parent = newNodeChild;
		      n = n->next;
		    }
		}
	      return CMS_STATUS_NOT_SET;
	    }
	  else
	    {
	      xmlNodeSetContent(((xmlNodePtr)currentNode),(xmlChar *)insertRefs(x,len));
	      content_set_for_this_class=1;
	    }
	  return CMS_STATUS_NOT_SET;
	}
      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 45; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}

      if (schema_gen_mode)
	{
	  whitespace[jj] = '\t';
	  jj++;
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element",0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  whitespace[jj] = '\t';
	  jj++;
	  if (inside_unbounded[class_count])
	    {
	      xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("string"));
	      xmlSetProp (newNode, (xmlChar *) "minOccurs",
			  (xmlChar *) "0");
	    }
	  else
	    {
	      xmlNodePtr simpleNode =
		xmlNewTextChild (newNode, (xmlNsPtr) xsdNs,
				 (xmlChar *) "simpleType",
				 fakeStrdup (whitespace));
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNewTextChild(%s,\"xsd\",\"simpleType\",0);\
n",
			       ((char *) fullName (newNode)));
#endif

	      whitespace[jj] = '\t';
	      jj++;
	      xmlNodePtr restrictionNode =
		xmlNewTextChild (simpleNode, (xmlNsPtr) xsdNs,
				 (xmlChar *) "restriction",
				 fakeStrdup (whitespace));
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNewTextChild(%s,\"xsd\",\"restriction\",0);\n",
			       ((char *) fullName (simpleNode)));
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"base\",%s))\n",
			       fullName (restrictionNode), "xsd:string");
#endif
	      xmlSetProp (restrictionNode, (xmlChar *) "base", XSDTYPE("string"));
	      xmlNodePtr maxlengthNode =
		xmlNewTextChild (restrictionNode, (xmlNsPtr) xsdNs,
				 (xmlChar *) "maxLength", 0);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNewTextChild(%s,\"xsd\",\"maxLength\",0);\
n",
			       ((char *) fullName (restrictionNode)));
#endif

	      char maxlengthstring[40];
	      SNPRINTF_FUNC ( SNPRINTF_ARGS(maxlengthstring,sizeof(maxlengthstring)),
			      "%d", (len - 1));
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"value\",%s))\n",
			       fullName (restrictionNode), maxlengthstring);
#endif
	      xmlSetProp (maxlengthNode, (xmlChar *) "value",
			  fakeStrdup (maxlengthstring));
	      jj--;
	      whitespace[jj] = 0;
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			       fullName (restrictionNode), whitespace);
#endif
	      xmlNodeAddContent (restrictionNode, fakeStrdup (whitespace));
	      jj--;
	      whitespace[jj] = 0;
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			       fullName (simpleNode), whitespace);
#endif
	      xmlNodeAddContent (simpleNode, fakeStrdup (whitespace));
	      jj--;
	      whitespace[jj] = 0;
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			       fullName (newNode), whitespace);
#endif
	      xmlNodeAddContent (newNode, fakeStrdup (whitespace));
	    }
	  return CMS_STATUS_NOT_SET;
	}

      size_t xstrlen = cms_xml_strnlen(x,len);
      if (xstrlen > len)
	{
	  xstrlen = len;
	}
      if (0 == compareForDiff (name, x, xstrlen))
	{
	  return CMS_STATUS_NOT_SET;
	}
      if (diffbuff != 0)
	{
	  if (*x == 0)
	    {
	      long offset = (long) (x - ((char *) base));
	      if (offset > 0 && offset <= (int) diffbuff_size
		  && 0 == *((char *) diffbuff + offset))
		{
		  return CMS_STATUS_NOT_SET;
		}
	    }
	}
      xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
		       fakeStrdup (insertRefs (x, len)));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0, %s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif
    }
  else
    {
      const char *decodevaluestring = 0;
      if(*name == 0)
	{
	  decodevaluestring = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	  if(0 == decodevaluestring && next_default != 0)
	    {
	      decodevaluestring=next_default;
	    }
	  next_default=0;
	}
      else
	{
	  decodevaluestring = findNodeString (name);
	}
      if(*name == 0)
	{
	  decodevaluestring = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	  if(0 == decodevaluestring && next_default != 0)
	    {
	      decodevaluestring=next_default;
	    }
	  next_default=0;
	}
      else
	{
	  decodevaluestring = findNodeString (name);
	}
      if (0 != decodevaluestring)
	{
	  strncpy (x, removeRefs (decodevaluestring), len);
	  x[len-1]=0;
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}



CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, unsigned char *x,
				     unsigned int len)
{


  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) x, sizeof (*x) * len) < 0)
    return status;
  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 45; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  whitespace[jj] = '\t';
	  jj++;
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", fakeStrdup (whitespace));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));

	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  whitespace[jj] = '\t';
	  jj++;
	  if (inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"type\",\"xsd:hexBinary\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "type",
			  (xmlChar *) "xsd:hexBinary");
	    }
	  else
	    {
	      xmlNodePtr simpleNode =
		xmlNewTextChild (newNode, (xmlNsPtr) xsdNs,
				 (xmlChar *) "simpleType",
				 fakeStrdup (whitespace));
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNewTextChild(%s,\"xsd\",\"simpleType\",0);\
n",
			       ((char *) fullName (newNode)));
#endif

	      whitespace[jj] = '\t';
	      jj++;
	      xmlNodePtr restrictionNode =
		xmlNewTextChild (simpleNode, (xmlNsPtr) xsdNs,
				 (xmlChar *) "restriction",
				 fakeStrdup (whitespace));
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNewTextChild(%s,\"xsd\",\"restriction\",0);\
n",
			       ((char *) fullName (simpleNode)));
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"base\",%s))\n",
			       fullName (restrictionNode), "xsd:hexBinary");
#endif
	      xmlSetProp (restrictionNode, (xmlChar *) "base", XSDTYPE("hexBinary"));
	      char maxlengthstring[40];
	      SNPRINTF_FUNC (SNPRINTF_ARGS(maxlengthstring,sizeof(maxlengthstring)),
			     "%d", (len));
	      xmlNodePtr lengthNode =
		xmlNewTextChild (restrictionNode, (xmlNsPtr) xsdNs,
				 (xmlChar *) "length", 0);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNewTextChild(%s,\"xsd\",\"length\",0);\
n",
			       ((char *) fullName (restrictionNode)));

	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"value\",%s))\n",
			       fullName (restrictionNode), maxlengthstring);
#endif
	      xmlSetProp (lengthNode, (xmlChar *) "value",
			  fakeStrdup (maxlengthstring));
	      jj--;
	      whitespace[jj] = 0;
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			       fullName (restrictionNode), whitespace);
#endif
	      xmlNodeAddContent (restrictionNode, fakeStrdup (whitespace));
	      jj--;
	      whitespace[jj] = 0;
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			       fullName (simpleNode), whitespace);
#endif
	      xmlNodeAddContent (simpleNode, fakeStrdup (whitespace));
	      jj--;
	      whitespace[jj] = 0;
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			       fullName (newNode), whitespace);
#endif
	      xmlNodeAddContent (newNode, fakeStrdup (whitespace));
	    }
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareForDiff (name, x, len))
	{
	  return CMS_STATUS_NOT_SET;
	}

      const char *hexdata = binaryToHexConvert ((char *) x, len);
      xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
		       (xmlChar *) hexdata);
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       hexdata);
#endif


    }
  else
    {
      const char *decodevaluestring = findNodeString (name);
      if (0 != decodevaluestring)
	{
	  const char *bindata =
	    hexToBinaryConvert (decodevaluestring, len * 2);
	  if (0 != bindata)
	    {
	      memcpy (x, bindata, len);
	    }
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, short *x,
				     unsigned int len)
{
  const char *orig_next_default = next_default;
  unsigned int i;

  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) x, sizeof (*x) * len) < 0)
    {
      return status;
    }


  if (encoding && !schema_gen_mode
      && 0 == compareForDiff (name, x, len * sizeof (*x)))
    {
      return CMS_STATUS_NOT_SET;
    }

  max_occurs = len;
  if (add_array_indexes_to_name)
    {
      size_t namelen = strlen (name);
      if (namelen > sizeof (arrayvarname) - 20)
	{
	  rcs_print_error ("name %s is too long\n", name);
	  return (status = CMS_UPDATE_ERROR);
	}
      strcpy (arrayvarname, name);
      for (i = 0; i < len; i++)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(arrayvarname+namelen,sizeof(arrayvarname)-namelen), "-%d", i);
	  next_default = orig_next_default;
	  update_with_name (arrayvarname, x[i]);
	}
    }
  else
    {
      for (i = 0; i < len; i++)
	{
	  next_default = orig_next_default;
	  update_with_name (name, x[i]);
	  if (schema_gen_mode)
	    {
	      break;
	    }
	}
    }
  max_occurs = 1;
  next_default=0;
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, unsigned short *x,
				     unsigned int len)
{
  const char *orig_next_default = next_default;
  unsigned int i;

  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) x, sizeof (*x) * len) < 0)
    {
      return status;
    }

  if (encoding && !schema_gen_mode
      && 0 == compareForDiff (name, x, len * sizeof (*x)))
    {
      return CMS_STATUS_NOT_SET;
    }


  max_occurs = len;
  if (add_array_indexes_to_name)
    {
      size_t namelen = strlen (name);
      if (namelen > sizeof (arrayvarname) - 20)
	{
	  rcs_print_error ("name %s is too long\n", name);
	  return (status = CMS_UPDATE_ERROR);
	}
      strcpy (arrayvarname, name);
      for (i = 0; i < len; i++)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(arrayvarname+namelen,sizeof(arrayvarname)-namelen), "-%d", i);
	  next_default = orig_next_default;
	  update_with_name (arrayvarname, x[i]);
	}
    }
  else
    {
      for (i = 0; i < len; i++)
	{
	  next_default=orig_next_default;
	  update_with_name (name, x[i]);
	  if (schema_gen_mode)
	    {
	      break;
	    }
	}
    }
  max_occurs = 1;
  next_default=0;
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, int *x,
				     unsigned int len)
{
  const char *orig_next_default = next_default;
  unsigned int i;


  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) x, sizeof (*x) * len) < 0)
    return status;

  if (encoding && !schema_gen_mode
      && 0 == compareForDiff (name, x, len * sizeof (*x)))
    {
      return CMS_STATUS_NOT_SET;
    }

  max_occurs = len;
  if (add_array_indexes_to_name)
    {
      size_t namelen = strlen (name);
      if (namelen > sizeof (arrayvarname) - 20)
	{
	  rcs_print_error ("name %s is too long\n", name);
	  return (status = CMS_UPDATE_ERROR);
	}
      strcpy (arrayvarname, name);
      for (i = 0; i < len; i++)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(arrayvarname+namelen,sizeof(arrayvarname)-namelen), "-%d", i);
	  next_default=orig_next_default;
	  update_with_name (arrayvarname, x[i]);
	}
    }
  else
    {
      for (i = 0; i < len; i++)
	{
	  next_default=orig_next_default;
	  update_with_name (name, x[i]);
	  if (schema_gen_mode)
	    {
	      break;
	    }
	}
    }
  next_default=0;
  max_occurs = 1;
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, unsigned int *x,
				     unsigned int len)
{
  const char *orig_next_default = next_default;
  unsigned int i;

  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) x, sizeof (*x) * len) < 0)
    {
      return status;
    }


  if (encoding && !schema_gen_mode
      && 0 == compareForDiff (name, x, len * sizeof (*x)))
    {
      return CMS_STATUS_NOT_SET;
    }

  max_occurs = len;
  if (add_array_indexes_to_name)
    {
      size_t namelen = strlen (name);
      if (namelen > sizeof (arrayvarname) - 20)
	{
	  rcs_print_error ("name %s is too long\n", name);
	  return (status = CMS_UPDATE_ERROR);
	}
      strcpy (arrayvarname, name);
      for (i = 0; i < len; i++)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(arrayvarname+namelen,sizeof(arrayvarname)-namelen), "-%d", i);
	  next_default=orig_next_default;
	  update_with_name (arrayvarname, x[i]);
	}
    }
  else
    {
      for (i = 0; i < len; i++)
	{
	  next_default=orig_next_default;
	  update_with_name (name, x[i]);
	  if (schema_gen_mode)
	    {
	      break;
	    }
	}
    }
  max_occurs = 1;
  next_default=0;
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, long *x,
				     unsigned int len)
{
  const char *orig_next_default = next_default;
  unsigned int i;

  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) x, sizeof (*x) * len) < 0)
    {
      return status;
    }

  if (encoding && !schema_gen_mode
      && 0 == compareForDiff (name, x, len * sizeof (*x)))
    {
      return CMS_STATUS_NOT_SET;
    }

  max_occurs = len;
  if (add_array_indexes_to_name)
    {
      size_t namelen = strlen (name);
      if (namelen > sizeof (arrayvarname) - 20)
	{
	  rcs_print_error ("name %s is too long\n", name);
	  return (status = CMS_UPDATE_ERROR);
	}
      strcpy (arrayvarname, name);
      for (i = 0; i < len; i++)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(arrayvarname+namelen,sizeof(arrayvarname)-namelen), "-%d", i);
	  next_default=orig_next_default;
	  update_with_name (arrayvarname, x[i]);
	}
    }
  else
    {
      for (i = 0; i < len; i++)
	{
	  next_default=orig_next_default;
	  update_with_name (name, x[i]);
	  if (schema_gen_mode)
	    {
	      break;
	    }
	}
    }
  next_default=0;
  max_occurs = 1;
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, unsigned long *x,
				     unsigned int len)
{
  const char *orig_next_default = next_default;
  unsigned int i;

  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) x, sizeof (*x) * len) < 0)
    {
      return status;
    }

  if (encoding && !schema_gen_mode
      && 0 == compareForDiff (name, x, len * sizeof (*x)))
    {
      return CMS_STATUS_NOT_SET;
    }

  max_occurs = len;
  if (add_array_indexes_to_name)
    {
      size_t namelen = strlen (name);
      if (namelen > sizeof (arrayvarname) - 20)
	{
	  rcs_print_error ("name %s is too long\n", name);
	  return (status = CMS_UPDATE_ERROR);
	}
      strcpy (arrayvarname, name);
      for (i = 0; i < len; i++)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(arrayvarname+namelen,sizeof(arrayvarname)-namelen), "-%d", i);
	  next_default=orig_next_default;
	  update_with_name (arrayvarname, x[i]);
	}
    }
  else
    {
      for (i = 0; i < len; i++)
	{
	  next_default=orig_next_default;
	  update_with_name (name, x[i]);
	  if (schema_gen_mode)
	    {
	      break;
	    }
	}
    }
  next_default=0;
  max_occurs = 1;
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, float *x,
				     unsigned int len)
{
  const char *orig_next_default = next_default;
  unsigned int i;

  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) x, sizeof (float) * len) < 0)
    {
      return status;
    }

  if (encoding && !schema_gen_mode
      && 0 == compareForDiff (name, x, len * sizeof (*x)))
    {
      return CMS_STATUS_NOT_SET;
    }

  max_occurs = len;
  if (add_array_indexes_to_name)
    {
      size_t namelen = strlen (name);
      if (namelen > sizeof (arrayvarname) - 20)
	{
	  rcs_print_error ("name %s is too long\n", name);
	  return (status = CMS_UPDATE_ERROR);
	}
      strcpy (arrayvarname, name);
      for (i = 0; i < len; i++)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(arrayvarname+namelen,sizeof(arrayvarname)-namelen), "-%d", i);
	  next_default=orig_next_default;
	  update_with_name (arrayvarname, x[i]);
	}
    }
  else
    {
      for (i = 0; i < len; i++)
	{
	  next_default=orig_next_default;
	  update_with_name (name, x[i]);
	  if (schema_gen_mode)
	    {
	      break;
	    }
	}
    }
  max_occurs = 1;
  next_default=0;
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, double *x,
				     unsigned int len)
{
  const char *orig_next_default = next_default;
  unsigned int i;

  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) x, sizeof (double) * len) < 0)
    {
      return status;
    }

  if (encoding && !schema_gen_mode
      && 0 == compareForDiff (name, x, len * sizeof (*x)))
    {
      return CMS_STATUS_NOT_SET;
    }

  max_occurs = len;
  if (add_array_indexes_to_name)
    {
      size_t namelen = strlen (name);
      if (namelen > sizeof (arrayvarname) - 20)
	{
	  rcs_print_error ("name %s is too long\n", name);
	  return (status = CMS_UPDATE_ERROR);
	}
      strcpy (arrayvarname, name);
      for (i = 0; i < len; i++)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(arrayvarname+namelen,sizeof(arrayvarname)-namelen), "-%d", i);
	  next_default=orig_next_default;
	  update_with_name (arrayvarname, x[i]);
	}
    }
  else
    {
      for (i = 0; i < len; i++)
	{
	  next_default=orig_next_default;
	  update_with_name (name, x[i]);
	  if (schema_gen_mode)
	    {
	      break;
	    }
	}
    }
  max_occurs = 1;
  next_default=0;
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, long double *x,
				     unsigned int len)
{
  unsigned int i;

  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }
  if (check_pointer_with_name (name, (char *) x, sizeof (long double) * len) <
      0)
    {
      return status;
    }

  if (encoding && !schema_gen_mode
      && 0 == compareForDiff (name, x, len * sizeof (*x)))
    {
      return CMS_STATUS_NOT_SET;
    }

  max_occurs = len;
  if (add_array_indexes_to_name)
    {
      size_t namelen = strlen (name);
      if (namelen > sizeof (arrayvarname) - 20)
	{
	  rcs_print_error ("name %s is too long\n", name);
	  return (status = CMS_UPDATE_ERROR);
	}
      strcpy (arrayvarname, name);
      for (i = 0; i < len; i++)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(arrayvarname+namelen,sizeof(arrayvarname)-namelen), "-%d", i);
	  update_with_name (arrayvarname, x[i]);
	}
    }
  else
    {
      for (i = 0; i < len; i++)
	{
	  update_with_name (name, x[i]);
	  if (schema_gen_mode)
	    {
	      break;
	    }
	}
    }
  max_occurs = 1;
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_XML_UPDATER::update_dla_with_name (const char *name, char *x, int &len,
					 int maxlen)
{
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (schema_gen_mode)
    len = maxlen;
  if (!encoding && !schema_gen_mode && !add_array_indexes_to_name)
    {
      len = countNodes (name);
      if (len > maxlen)
	{
	  rcs_print_error ("Too many (%d>%d) %s nodes in %s\n", len, maxlen,
			   name, fullName ((xmlNodePtr) currentNode));
	}
    }
  if (!encoding && !schema_gen_mode && !add_array_indexes_to_name)
    {
      len = countNodes (name);
      if (len > maxlen)
	{
	  rcs_print_error ("Too many (%d>%d) %s nodes in %s\n", len, maxlen,
			   name, fullName ((xmlNodePtr) currentNode));
	}
    }
  inside_dla[class_count] = 1;
  retval =
    update_with_name (name, x, (unsigned int) (len < maxlen ? len : maxlen));
  inside_dla[class_count] = 0;
  return retval;
}

CMS_STATUS
  CMS_XML_UPDATER::update_dla_with_name (const char *name, unsigned char *x,
					 int &len, int maxlen)
{
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (schema_gen_mode)
    len = maxlen;
  if (!encoding && !schema_gen_mode && !add_array_indexes_to_name)
    {
      len = countNodes (name);
      if (len > maxlen)
	{
	  rcs_print_error ("Too many (%d>%d) %s nodes in %s\n", len, maxlen,
			   name, fullName ((xmlNodePtr) currentNode));
	}
    }
  inside_dla[class_count] = 1;
  retval =
    update_with_name (name, x, (unsigned int) (len < maxlen ? len : maxlen));
  inside_dla[class_count] = 0;
  return retval;
}

CMS_STATUS
  CMS_XML_UPDATER::update_dla_with_name (const char *name, short *x, int &len,
					 int maxlen)
{
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (schema_gen_mode)
    len = maxlen;
  if (!encoding && !schema_gen_mode && !add_array_indexes_to_name)
    {
      len = countNodes (name);
      if (len > maxlen)
	{
	  rcs_print_error ("Too many (%d>%d) %s nodes in %s\n", len, maxlen,
			   name, fullName ((xmlNodePtr) currentNode));
	}
    }
  inside_dla[class_count] = 1;
  retval =
    update_with_name (name, x, (unsigned int) (len < maxlen ? len : maxlen));
  inside_dla[class_count] = 0;
  return retval;
}

CMS_STATUS
  CMS_XML_UPDATER::update_dla_with_name (const char *name, unsigned short *x,
					 int &len, int maxlen)
{
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (schema_gen_mode)
    len = maxlen;
  if (!encoding && !schema_gen_mode && !add_array_indexes_to_name)
    {
      len = countNodes (name);
      if (len > maxlen)
	{
	  rcs_print_error ("Too many (%d>%d) %s nodes in %s\n", len, maxlen,
			   name, fullName ((xmlNodePtr) currentNode));
	}
    }
  inside_dla[class_count] = 1;
  retval =
    update_with_name (name, x, (unsigned int) (len < maxlen ? len : maxlen));
  inside_dla[class_count] = 0;
  return retval;
}

CMS_STATUS
  CMS_XML_UPDATER::update_dla_with_name (const char *name, int *x, int &len,
					 int maxlen)
{
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (schema_gen_mode)
    len = maxlen;
  if (!encoding && !schema_gen_mode && !add_array_indexes_to_name)
    {
      len = countNodes (name);
      if (len > maxlen)
	{
	  rcs_print_error ("Too many (%d>%d) %s nodes in %s\n", len, maxlen,
			   name, fullName ((xmlNodePtr) currentNode));
	}
    }
  inside_dla[class_count] = 1;
  retval =
    update_with_name (name, x, (unsigned int) (len < maxlen ? len : maxlen));
  inside_dla[class_count] = 0;
  return retval;
}

CMS_STATUS
  CMS_XML_UPDATER::update_dla_with_name (const char *name, unsigned int *x,
					 int &len, int maxlen)
{
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (schema_gen_mode)
    len = maxlen;
  if (!encoding && !schema_gen_mode && !add_array_indexes_to_name)
    {
      len = countNodes (name);
      if (len > maxlen)
	{
	  rcs_print_error ("Too many (%d>%d) %s nodes in %s\n", len, maxlen,
			   name, fullName ((xmlNodePtr) currentNode));
	}
    }
  inside_dla[class_count] = 1;
  retval =
    update_with_name (name, x, (unsigned int) (len < maxlen ? len : maxlen));
  inside_dla[class_count] = 0;
  return retval;
}

CMS_STATUS
  CMS_XML_UPDATER::update_dla_with_name (const char *name, long *x, int &len,
					 int maxlen)
{
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (schema_gen_mode)
    len = maxlen;
  if (!encoding && !schema_gen_mode && !add_array_indexes_to_name)
    {
      len = countNodes (name);
      if (len > maxlen)
	{
	  rcs_print_error ("Too many (%d>%d) %s nodes in %s\n", len, maxlen,
			   name, fullName ((xmlNodePtr) currentNode));
	}
    }
  inside_dla[class_count] = 1;
  retval =
    update_with_name (name, x, (unsigned int) (len < maxlen ? len : maxlen));
  inside_dla[class_count] = 0;
  return retval;
}

CMS_STATUS
  CMS_XML_UPDATER::update_dla_with_name (const char *name, unsigned long *x,
					 int &len, int maxlen)
{
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (schema_gen_mode)
    len = maxlen;
  if (!encoding && !schema_gen_mode && !add_array_indexes_to_name)
    {
      len = countNodes (name);
      if (len > maxlen)
	{
	  rcs_print_error ("Too many (%d>%d) %s nodes in %s\n", len, maxlen,
			   name, fullName ((xmlNodePtr) currentNode));
	}
    }
  inside_dla[class_count] = 1;
  retval =
    update_with_name (name, x, (unsigned int) (len < maxlen ? len : maxlen));
  inside_dla[class_count] = 0;
  return retval;
}

CMS_STATUS
  CMS_XML_UPDATER::update_dla_with_name (const char *name, float *x, int &len,
					 int maxlen)
{
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (schema_gen_mode)
    len = maxlen;
  if (!encoding && !schema_gen_mode && !add_array_indexes_to_name)
    {
      len = countNodes (name);
      if (len > maxlen)
	{
	  rcs_print_error ("Too many (%d>%d) %s nodes in %s\n", len, maxlen,
			   name, fullName ((xmlNodePtr) currentNode));
	}
    }
  inside_dla[class_count] = 1;
  retval =
    update_with_name (name, x, (unsigned int) (len < maxlen ? len : maxlen));
  inside_dla[class_count] = 0;
  return retval;
}

CMS_STATUS
  CMS_XML_UPDATER::update_dla_with_name (const char *name, double *x,
					 int &len, int maxlen)
{
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (schema_gen_mode)
    len = maxlen;
  if (!encoding && !schema_gen_mode && !add_array_indexes_to_name)
    {
      len = countNodes (name);
      if (len > maxlen)
	{
	  rcs_print_error ("Too many (%d>%d) %s nodes in %s\n", len, maxlen,
			   name, fullName ((xmlNodePtr) currentNode));
	}
    }
  inside_dla[class_count] = 1;
  retval =
    update_with_name (name, x, (unsigned int) (len < maxlen ? len : maxlen));
  inside_dla[class_count] = 0;
  return retval;
}

CMS_STATUS
  CMS_XML_UPDATER::update_dla_with_name (const char *name, long double *x,
					 int &len, int maxlen)
{
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (schema_gen_mode)
    len = maxlen;
  if (!encoding && !schema_gen_mode && !add_array_indexes_to_name)
    {
      len = countNodes (name);
      if (len > maxlen)
	{
	  rcs_print_error ("Too many (%d>%d) %s nodes in %s\n", len, maxlen,
			   name, fullName ((xmlNodePtr) currentNode));
	}
    }
  inside_dla[class_count] = 1;
  retval =
    update_with_name (name, x, (unsigned int) (len < maxlen ? len : maxlen));
  inside_dla[class_count] = 0;
  return retval;
}

struct XML_NODE *
CMS_XML_UPDATER::findNode (const char *name)
{
  if (0 == ((xmlNodePtr) currentNode))
    return 0;
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::findNode(%s) called.\n", name);
#endif
  xmlNodePtr originalElement = ((xmlNodePtr) currentElement);
  if(currentElement && ((xmlNodePtr)currentElement)->next)
    {
      currentElement = ((xmlNodePtr)currentElement)->next;
    }
  while (((xmlNodePtr) currentElement) != NULL)
    {
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "CMS_XML_UPDATER::findNode comparing %s with %s , currentElement=%s\n",
		       name, ((xmlNodePtr) currentElement)->name,
		       fullName (((xmlNodePtr) currentElement)));
#endif
      if (!strcmp (name, (char *) ((xmlNodePtr) currentElement)->name))
	{
	  return ((xmlNodePtr) currentElement);
	}
      currentElement = ((xmlNodePtr) currentElement)->next;
    }
  if (0 != ((xmlNodePtr) currentNode))
    {
      currentElement = ((xmlNodePtr) currentNode)->children;
      while (((xmlNodePtr) currentElement) != NULL)
	{
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "CMS_XML_UPDATER::findNode comparing %s with %s , currentElement=%s\n",
			   name, ((xmlNodePtr) currentElement)->name,
			   fullName (((xmlNodePtr) currentElement)));
#endif
	  if (!strcmp (name, (char *) ((xmlNodePtr) currentElement)->name))
	    {
	      return ((xmlNodePtr) currentElement);
	    }
	  currentElement = ((xmlNodePtr) currentElement)->next;
	}
    }
  currentElement = originalElement;
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::findNodeString could not find sub structure element %s in xml node %s\n",
		   name, ((char *) fullName (((xmlNodePtr) currentNode))));
#endif
  return 0;
}





int
CMS_XML_UPDATER::countNodes (const char *name)
{
  if (0 == ((xmlNodePtr) currentNode))
    return 0;
  int count = 0;
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::countNodes(%s) called.\n", name);
#endif
  xmlNodePtr originalElement = ((xmlNodePtr) currentElement);
#if 0
  while (((xmlNodePtr) currentElement) != NULL)
    {
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "CMS_XML_UPDATER::countNodes comparing %s with %s , currentElement=%s\n",
		       name, ((xmlNodePtr) currentElement)->name,
		       fullName (((xmlNodePtr) currentElement)));
#endif
      if (!strcmp (name, (char *) ((xmlNodePtr) currentElement)->name))
	{
	  count++;
	}
      currentElement = ((xmlNodePtr) currentElement)->next;
    }
#endif
  if (0 != ((xmlNodePtr) currentNode))
    {
      currentElement = ((xmlNodePtr) currentNode)->children;
      while (((xmlNodePtr) currentElement) != NULL)
	{
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "CMS_XML_UPDATER::countNodes comparing %s with %s , currentElement=%s\n",
			   name, ((xmlNodePtr) currentElement)->name,
			   fullName (((xmlNodePtr) currentElement)));
#endif
	  if (!strcmp (name, (char *) ((xmlNodePtr) currentElement)->name))
	    {
	      count++;
	    }
	  currentElement = ((xmlNodePtr) currentElement)->next;
	}
    }
  currentElement = originalElement;
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::countNodes found %d nodes with name  %s in xml node %s\n",
		   count, name,
		   ((char *) fullName (((xmlNodePtr) currentNode))));
#endif
  return count;
}


struct XML_NODE *
CMS_XML_UPDATER::findNodeWithProperty (const char *nodename,
				       const char *propertyname,
				       const char *propertyValue)
{
  if (0 == ((xmlNodePtr) currentNode))
    return 0;
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::findNodeWithProperty(%s,%s,%s) called.\n",
		   nodename, propertyname, propertyValue);
#endif
  xmlNodePtr originalElement = ((xmlNodePtr) currentElement);
  while (((xmlNodePtr) currentElement) != NULL)
    {
      if (0 != ((xmlNodePtr) currentElement)->name)
	{
	  if (!strcmp
	      (nodename, (char *) ((xmlNodePtr) currentElement)->name))
	    {
	      const char *pvaltocheck =
		(const char *) xmlGetProp (((xmlNodePtr) currentElement),
					   (xmlChar *) propertyname);
	      if (0 != pvaltocheck)
		{
		  if (!strcmp (pvaltocheck, propertyValue))
		    {
		      return ((xmlNodePtr) currentElement);
		    }
		}
	    }
	}
      currentElement = ((xmlNodePtr) currentElement)->next;
    }
  if (0 != ((xmlNodePtr) currentNode))
    {
      currentElement = ((xmlNodePtr) currentNode)->children;
      while (((xmlNodePtr) currentElement) != NULL
	     && ((xmlNodePtr) currentElement) != originalElement)
	{
	  if (((xmlNodePtr) currentElement)->name != 0)
	    {
	      if (0 != ((xmlNodePtr) currentElement)->name)
		{
		  if (!strcmp
		      (nodename,
		       (char *) ((xmlNodePtr) currentElement)->name))
		    {
		      const char *pvaltocheck =
			(const char *)
			xmlGetProp (((xmlNodePtr) currentElement),
				    (xmlChar *) propertyname);
		      if (0 != pvaltocheck)
			{
			  if (!strcmp (pvaltocheck, propertyValue))
			    {
			      return ((xmlNodePtr) currentElement);
			    }
			}
		    }
		}
	    }
	  currentElement = ((xmlNodePtr) currentElement)->next;
	}
    }
  currentElement = originalElement;
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "could not find sub structure element %s with property %s equal to %s in xml node %s\n",
		   nodename, propertyname, propertyValue,
		   ((char *) fullName (((xmlNodePtr) currentNode))));
#endif
  return 0;
}


xmlChar *
CMS_XML_UPDATER::stringSpaceAlloc (long stringlength)
{
  if ((stringspaceused < (size_t) (stringspaceend - stringspacebegin)) ||
      (stringspaceend < stringspacebegin))
    {
      rcs_print_error
	("Inconsistant values for strinspacebegin=%p, stringspaceend=%p, ((stringspaceend - stringspacebegin)=%lu, stringspaceused=%lu\n",
	 stringspacebegin, stringspaceend,
	 (unsigned long) (stringspaceend - stringspacebegin), 
	 (unsigned long) stringspaceused);
      stringspaceused = (stringspaceend - stringspacebegin);
    }
  if (stringlength + stringspaceused + 1 > stringspacesize)
    {
      stringspacesize =
	(stringlength + stringspaceused) + 256 + (512 -
						  (stringlength +
						   stringspaceused +
						   256) % 512);

      stringspacebegin =
	(char *) DEBUG_MALLOC (stringspacesize - stringspaceused);
      stringstofree[numstringstofree] = stringspacebegin;
      numstringstofree++;
      if (numstringstofree >= maxstringstofree)
	{
	  maxstringstofree += 16;
	  char **oldstringstofree = stringstofree;
	  stringstofree =
	    (char **) DEBUG_MALLOC (maxstringstofree * sizeof (char *));
	  memset (stringstofree, 0, maxstringstofree * sizeof (char *));
	  if (maxstringstofree > 16 && 0 != oldstringstofree)
	    {
	      memcpy (stringstofree, oldstringstofree,
		      (maxstringstofree - 16) * sizeof (char *));
	      DEBUG_FREE (oldstringstofree);
	    }
	}
      stringspaceend = stringspacebegin;
    }
  xmlChar *ret = (xmlChar *) stringspaceend;
  stringspaceend += stringlength + 1;
  stringspaceused += stringlength + 1;
  if ((stringspaceused < (size_t) (stringspaceend - stringspacebegin)) ||
      (stringspaceend < stringspacebegin))
    {
      rcs_print_error
	("Inconsistant values for strinspacebegin=%p, stringspaceend=%p, ((stringspaceend - stringspacebegin)=%lu, stringspaceused=%lu\n",
	 stringspacebegin, stringspaceend,
	 (unsigned long) (stringspaceend - stringspacebegin), 
	 (unsigned long) stringspaceused);
      stringspaceused = (stringspaceend - stringspacebegin);
    }
  return ret;
}


xmlChar *
CMS_XML_UPDATER::fakeStrdup (const char *str)
{
  long stringlength = (long) strlen (str);
  xmlChar *ret = (xmlChar *) stringSpaceAlloc(stringlength);
  strcpy (ret, str);
  return ret;
}

#ifdef USE_LIBXML2
const char *
CMS_XML_UPDATER::fullName (xmlNodePtr node)
#else
const char *
CMS_XML_UPDATER::fullName (void *node)
#endif
{
  if (0 == ((xmlNodePtr) node))
    {
      return "node";
    }
#ifndef USE_LIBXML2
  if (((xmlNodePtr) node)->fullname)
    {
      return ((xmlNodePtr) node)->fullname;
    }
#endif
  char buf1[256];
  char buf2[256];
  memset (buf1, 0, sizeof (buf1));
  memset (buf2, 0, sizeof (buf2));
  xmlNodePtr parent = ((xmlNodePtr) node);
  char *endbuf1 = buf1;
  while (parent != 0
	 && (endbuf1 < (buf1 + sizeof (buf1)))
	 && strlen (buf2) < sizeof (buf2) - 10)
    {
      strncpy (buf2, buf1, sizeof (buf2));
      endbuf1 = buf1;
      if (parent->name != 0)
	{
	  strncpy (endbuf1, (char *) parent->name, sizeof (buf1));
	  endbuf1 += strlen (endbuf1);
	  char *nameProp = (char *) xmlGetProp (parent, (xmlChar *) "name");
	  if (0 != nameProp)
	    {
	      strncat (endbuf1, "(name=", (buf1 + sizeof (buf1)) - endbuf1);
	      endbuf1 += strlen (endbuf1);
	      strncat (endbuf1, nameProp, (buf1 + sizeof (buf1)) - endbuf1);
	      endbuf1 += strlen (endbuf1);
	      strncat (endbuf1, ")", (buf1 + sizeof (buf1)) - endbuf1);
	      endbuf1 += strlen (endbuf1);
	    }
	  char *typeProp = (char *) xmlGetProp (parent, (xmlChar *) "type");
	  if (0 != typeProp)
	    {
	      strncat (endbuf1, "(type=", (buf1 + sizeof (buf1)) - endbuf1);
	      endbuf1 += strlen (endbuf1);
	      strncat (endbuf1, typeProp, (buf1 + sizeof (buf1)) - endbuf1);
	      endbuf1 += strlen (endbuf1);
	      strncat (endbuf1, ")", (buf1 + sizeof (buf1)) - endbuf1);
	      endbuf1 += strlen (endbuf1);
	    }
	  if (buf2[0])
	    {
	      strncat (endbuf1, "/", (buf1 + sizeof (buf1)) - endbuf1);
	      endbuf1 += strlen (endbuf1);
	    }
	  strncat (endbuf1, buf2, (buf1 + sizeof (buf1)) - endbuf1);
	  endbuf1 += strlen (endbuf1);
	}
      parent = parent->parent;
    }
  const char *retval = (const char *) fakeStrdup (buf1);
#ifndef USE_LIBXML2
  ((xmlNodePtr) node)->fullname = retval;
#endif
  return retval;
}

CMS_STATUS 
CMS_XML_UPDATER::beginBaseClass (const char *)
{
  base_class_count++;
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
CMS_XML_UPDATER::endBaseClass (const char *)
{
  base_class_count--;
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
CMS_XML_UPDATER::beginClass (const char *name, const char *basename)
{
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::beginClass(%s) called, class_count=%d, inside_classvar_not_found=%d, currentNode=%s,currentElement=%s\n",
		   name, class_count, inside_classvar_not_found,
		   fullName (((xmlNodePtr) currentNode)),
		   fullName (((xmlNodePtr) currentElement)));
#endif

  int jj = 0;
  content_set_for_this_class=0;

  if (encoding)
    {
      if ((0 == mainElementNode || 0 == currentNode) && !schema_gen_mode)
	{
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewDocRawNode((xmlDocPtr)doc,0,%s, . . .) called.\n",
			   name);
#endif
	  if ((make_xml_pretty && !content_set_for_this_class))
	    {
	      mainElementNode =
		xmlNewDocRawNode ((xmlDocPtr) doc, 0, fakeStrdup (name),
				  (xmlChar *) "\n\t");
	    }
	  else
	    {
	      mainElementNode =
		xmlNewDocRawNode ((xmlDocPtr) doc, 0, fakeStrdup (name),0);
	    }
	  xmlDocSetRootElement ((xmlDocPtr) doc,
				(xmlNodePtr) mainElementNode);
	  if(inside_root_start_addition)
	    {
	      ((xmlNodePtr)mainElementNode)->start_add = inside_root_start_addition;
	    }
	  currentNode = mainElementNode;
	}
      if (class_count + 2 > maxclasscount)
	{
	  maxclasscount += 32;
	  structisempty =
	    (char *) DEBUG_REALLOC (structisempty, maxclasscount);
	  elementtablevel =
	    (char *) DEBUG_REALLOC (elementtablevel, maxclasscount);
	  inside_dla = (char *) DEBUG_REALLOC (inside_dla, maxclasscount);
	  inside_unbounded =
	    (char *) DEBUG_REALLOC (inside_unbounded, maxclasscount);
	  memset (elementtablevel + class_count + 1, 4,
		  (maxclasscount - (class_count + 1)));
	  memset (structisempty + class_count + 1, 0,
		  (maxclasscount - (class_count + 1)));
	  memset (inside_dla + class_count + 1, 0,
		  (maxclasscount - (class_count + 1)));
	  memset (inside_unbounded + class_count + 1, 0,
		  (maxclasscount - (class_count + 1)));

	}
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  if (class_count + 2 > maxleftoffnodes)
	    {
	      maxleftoffnodes += 10;
#ifdef USE_LIBXML2
	      leftOffNodes =
		(xmlNode **) DEBUG_REALLOC (leftOffNodes,
					    sizeof (xmlNodePtr) *
					    maxleftoffnodes);
	      leftOffElements =
		(xmlNode **) DEBUG_REALLOC (leftOffElements,
					    sizeof (xmlNodePtr) *
					    maxleftoffnodes);
#else
	      leftOffNodes =
		(void **) DEBUG_REALLOC (leftOffNodes,
					 sizeof (xmlNodePtr) *
					 maxleftoffnodes);
	      leftOffElements =
		(void **) DEBUG_REALLOC (leftOffElements,
					 sizeof (xmlNodePtr) *
					 maxleftoffnodes);
#endif
	    }
	  if (inside_classvar_not_found)
	    {
	      inside_classvar_not_found++;
	    }
	  else
	    {
	      xmlNodePtr origCurrentNode = ((xmlNodePtr) currentNode);
	      xmlNodePtr origCurrentElement = ((xmlNodePtr) currentElement);
	      if (((xmlNodePtr) currentElement) != 0)
		{
		  if (((xmlNodePtr) currentElement)->name != 0)
		    {
		      if (!strcmp
			  ((char *) ((xmlNodePtr) currentElement)->name,
			   "element"))
			{
			  
#ifdef DEBUG_THIS_FILE
			  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
					   "xmlSetProp(%s,\"type\",%s))\n",
					   fullName (((xmlNodePtr)
						      currentElement)), name);
#endif
			  xmlSetProp (((xmlNodePtr) currentElement),
				      (xmlChar *) "type", fakeStrdup (name));
			}
		      else
			{
			  rcs_print_error
			    ("bad ((xmlNodePtr)currentNode) for beginClass %s\n",
			     fullName (((xmlNodePtr) currentNode)));
			  rcs_print_error
			    ("or bad ((xmlNodePtr)currentElement) for beginClass %s\n",
			     fullName (((xmlNodePtr) currentElement)));
			  return (status = CMS_UPDATE_ERROR);
			}
		    }
		}

	      currentNode = schemaNode;
	      currentElement = 0;
	      if (0 == findNodeWithProperty ("complexType", "name", name))
		{
		  leftOffNodes[class_count + 1] = origCurrentNode;
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "CMS_XML_UPDATER::beginClass(%s): leftOffNodes[%d] = %s\n",
				   name, (class_count + 1),
				   fullName (leftOffNodes[class_count + 1]));
#endif
		  ((xmlNodePtr *) leftOffElements)[class_count + 1] =
		    origCurrentElement;
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlNodeAddContent(%s,  . . .);\n",
				   fullName (schemaNode));
#endif
		  xmlNodeAddContent (((xmlNodePtr) schemaNode),
				     (xmlChar *) "\n\t");
		  xmlNodePtr classNode =
		    xmlNewTextChild (((xmlNodePtr) schemaNode),
				     (xmlNsPtr) xsdNs,
				     (xmlChar *) "complexType",
				     (xmlChar *) "\n\t\t");
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlNewTextChild(%s,(xmlNsPtr)xsdNs,\"complexType\",..)\n",
				   (char *) ((xmlNodePtr) schemaNode)->name);
#endif
		  xmlSetProp (classNode, (xmlChar *) "name",
			      fakeStrdup (name));
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlSetProp(%s,\"name\",%s)\n",
				   (char *) fullName (classNode),
				   (char *) name);
#endif
		  elementtablevel[class_count + 1] = 4;
		  if (basename != 0)
		    {
		      if (strcmp (basename, "NMLmsg"))
			{
			  xmlNodePtr complexContentNode =
			    xmlNewTextChild (classNode, (xmlNsPtr) xsdNs,
					     (xmlChar *) "complexContent",
					     (xmlChar *) "\n\t\t\t");
			  
#ifdef DEBUG_THIS_FILE
			  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
					   "xmlNewTextChild(%s,(xmlNsPtr)xsdNs,\"complexContent\",..)\n",
					   (char *) fullName (classNode));
#endif
			  xmlNodePtr extensionNode =
			    xmlNewTextChild (complexContentNode,
					     (xmlNsPtr) xsdNs,
					     (xmlChar *) "extension",
					     (xmlChar *) "\n\t\t\t\t");
			  
#ifdef DEBUG_THIS_FILE
			  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
					   "xmlNewTextChild(%s,(xmlNsPtr)xsdNs,\"extension\",..)\n",
					   (char *)
					   fullName (complexContentNode));
#endif
			  xmlSetProp (extensionNode, (xmlChar *) "base",
				      fakeStrdup (basename));
			  
#ifdef DEBUG_THIS_FILE
			  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
					   "xmlSetProp(%s,\"base\",%s)\n",
					   (char *) fullName (extensionNode),
					   (char *) basename);
#endif
			  classNode = extensionNode;
			  elementtablevel[class_count + 1] = 7;
			}
		    }
		  xmlNodePtr sequenceNode =
		    xmlNewTextChild (classNode, (xmlNsPtr) xsdNs,
				     (xmlChar *) "sequence", 0);
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlNewTextChild(%s,(xmlNsPtr)xsdNs,\"sequence\")\n",
				   (char *) fullName (classNode));
#endif
		  currentNode = sequenceNode;
		  currentElement = 0;
		}
	      else
		{
		  currentElement = origCurrentElement;
		  currentNode = origCurrentNode;
		  inside_classvar_not_found++;
		}
	    }
	}
      else if ((make_xml_pretty && !content_set_for_this_class))
	{
	  if (base_class_count < 1)
	    {
	      char comment[256];
	      strncpy (comment, name, 255);
	      strncat (comment, " ", 255 - strlen (name));
	      strncat (comment, fullName (((xmlNodePtr) currentNode)),
		       255 - strlen (name));
	      comment[255] = 0;
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlAddChild(%s,xmlNewComment(%s)))\n",
			       fullName (((xmlNodePtr) currentNode)), name);
#endif
	      xmlAddChild (((xmlNodePtr) currentNode),
			   xmlNewComment ((xmlChar *) comment));
	      last_var_was_struct = 0;
	    }
	}
    }
  class_count++;
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
CMS_XML_UPDATER::endClass (const char *name, const char *basename)
{
  if (0 == name || 0 == ((xmlNodePtr) currentNode))
    {
      return CMS_STATUS_NOT_SET;
    }

  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::endClass(%s) called, class_count=%d, inside_classvar_not_found=%d, currentNode=%s,currentElement=%s\n",
		   name, class_count, inside_classvar_not_found,
		   fullName (((xmlNodePtr) currentNode)),
		   fullName (((xmlNodePtr) currentElement)));
#endif
  class_count--;
  int allchildrennameless = 1;

  if(encoding && !content_set_for_this_class && currentNode)
    {
      xmlNodePtr curChild = ((xmlNodePtr) currentNode)->children;
      if(curChild)
	{
	  while(curChild)
	    {
	      if(curChild->name)
		{
		  allchildrennameless=0;
		  break;
		}
	      curChild=curChild->next;
	    }
	  if(allchildrennameless)
	    {
	      curChild = ((xmlNodePtr) currentNode)->children;
	      while(curChild)
		{
		  xmlNodePtr nextChild = curChild->next;
		  xmlUnlinkNode(curChild);
		  xmlFreeNode(curChild);
		  curChild = nextChild;
		}
	      ((xmlNodePtr) currentNode)->children=0;
	      ((xmlNodePtr) currentNode)->content=0;
	    }
	}
    }
 
  if (encoding && schema_gen_mode)
    {
      if (inside_classvar_not_found > 0)
	{
	  inside_classvar_not_found--;
	}
      else
	{
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "CMS_XML_UPDATER::endClass setting ((xmlNodePtr)currentNode) to %s from %s\n",
			   fullName (((xmlNodePtr *) leftOffNodes)[class_count
								   + 1]),
			   fullName (((xmlNodePtr) currentNode)));
#endif
	  if (0 != ((xmlNodePtr) currentNode))
	    {
	      if(!allchildrennameless)
		{
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlNodeAddContent(%s,  . . .);\n",
				   fullName (((xmlNodePtr) currentNode)));
#endif
		  xmlNodeAddContent (((xmlNodePtr) currentNode),
				     (xmlChar *) "\n\t\t");
		}
	      if (0 != ((xmlNodePtr) currentNode)->parent)
		{
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlNodeAddContent(%s,  . . .);\n",
				   fullName (((xmlNodePtr) currentNode)->
					     parent));
#endif
		  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
				     (xmlChar *) "\n\t");
		}
	      if (basename != 0)
		{
		  if (strcmp (basename, "NMLmsg"))
		    {
		      if(!allchildrennameless)
			{
			  
#ifdef DEBUG_THIS_FILE
			  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
					   "xmlNodeAddContent(%s,  . . .);\n",
					   fullName (((xmlNodePtr) currentNode)));
#endif
			  xmlNodeAddContent (((xmlNodePtr) currentNode),
					     (xmlChar *) "\t\t");
			}
		      if (0 != ((xmlNodePtr) currentNode)->parent)
			{
			  
#ifdef DEBUG_THIS_FILE
			  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
					   "xmlNodeAddContent(%s,  . . .);\n",
					   fullName (((xmlNodePtr)
						      currentNode)->parent));
#endif
			  xmlNodeAddContent (((xmlNodePtr) currentNode)->
					     parent, (xmlChar *) "\t\t");
			  if (0 != ((xmlNodePtr) currentNode)->parent->parent)
			    {
			      
#ifdef DEBUG_THIS_FILE
			      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
					       "xmlNodeAddContent(%s,  . . .);\n",
					       fullName (((xmlNodePtr)
							  currentNode)->
							 parent->parent));
#endif
			      xmlNodeAddContent (((xmlNodePtr) currentNode)->
						 parent->parent,
						 (xmlChar *) "\n\t\t");
			      if (0 !=
				  ((xmlNodePtr) currentNode)->parent->parent->
				  parent)
				{
				  
#ifdef DEBUG_THIS_FILE
				  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
						   "xmlNodeAddContent(%s,  . . .);\n",
						   fullName (((xmlNodePtr)
							      currentNode)->
							     parent->parent->
							     parent));
#endif
				  xmlNodeAddContent (((xmlNodePtr)
						      currentNode)->parent->
						     parent->parent,
						     (xmlChar *) "\n\t");
				}
			    }
			}
		    }
		}
#ifndef USE_LIBXML2
	      // xmlFlushNodeContent((xmlNodePtr)currentNode);
#endif
	    }
	  currentNode = ((xmlNodePtr *) leftOffNodes)[class_count + 1];
	  currentElement = ((xmlNodePtr *) leftOffElements)[class_count + 1];
	}
    }
  if (encoding && 0 == class_count && (schema_gen_mode || (make_xml_pretty && !content_set_for_this_class)))
    {
      xmlNodePtr rootNode = xmlDocGetRootElement ((xmlDocPtr) doc);
      if (rootNode != 0)
	{
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,  . . .);\n",
			   fullName (rootNode));
#endif
	  xmlNodeAddContent (rootNode, (xmlChar *) "\n\n");
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_XML_UPDATER::beginClassVar (const char *name)
{

  int jj = 0;
  xmlNodePtr newNode = 0;
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::beginClassVar(%s) called, class_count=%d, inside_classvar_not_found=%d, currentNode=%s,currentElement=%s\n",
		   name, class_count, inside_classvar_not_found,
		   fullName (((xmlNodePtr) currentNode)),
		   fullName (((xmlNodePtr) currentElement)));
#endif

  if (0 == name || 0 == ((xmlNodePtr) currentNode))
    return CMS_STATUS_NOT_SET;
  
  content_set_for_this_class=0;

  if (schema_gen_mode && inside_classvar_not_found > 0)
    {
      inside_classvar_not_found++;
      return CMS_STATUS_NOT_SET;
    }

  if (encoding)
    {
      if (class_count + 2 > maxclasscount)
	{
	  maxclasscount += 32;
	  structisempty =
	    (char *) DEBUG_REALLOC (structisempty, maxclasscount);
	  elementtablevel =
	    (char *) DEBUG_REALLOC (elementtablevel, maxclasscount);
	  inside_dla = (char *) DEBUG_REALLOC (inside_dla, maxclasscount);
	  inside_unbounded =
	    (char *) DEBUG_REALLOC (inside_unbounded, maxclasscount);

	  memset (elementtablevel + class_count + 1, 4,
		  (maxclasscount - (class_count + 1)));
	  memset (structisempty + class_count + 1, 0,
		  (maxclasscount - (class_count + 1)));
	  memset (inside_dla + class_count + 1, 0,
		  (maxclasscount - (class_count + 1)));
	  memset (inside_unbounded + class_count + 1, 0,
		  (maxclasscount - (class_count + 1)));
	}
      structisempty[class_count + 1] = 1;
      if (make_xml_pretty || schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }

	  if (true) //!last_var_was_struct || schema_gen_mode)
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			       fullName (((xmlNodePtr) currentNode)),
			       whitespace);
#endif
	      xmlNodeAddContent (((xmlNodePtr) currentNode),
				 fakeStrdup (whitespace));
	    }
	  whitespace[jj] = '\t';
	  if (!schema_gen_mode)
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNewTextChild(%s,0,%s,%s)\n",
			       fullName (((xmlNodePtr) currentNode)), name,
			       whitespace);
#endif
	      newNode =
		xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs),
				 fakeStrdup (name), fakeStrdup (whitespace));
	    }
	  else
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNewTextChild(%s,0,%s,0)\n",
			       fullName (((xmlNodePtr) currentNode)),
			       "element");
#endif
	      newNode =
		xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
				 (xmlChar *) "element", 0);
	      xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"name\",%s)\n",
			       fullName (newNode), (char *) name);
#endif
	      if (inside_dla[class_count] || inside_unbounded[class_count])
		{
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
				   fullName (newNode));
#endif
		  xmlSetProp (newNode, (xmlChar *) "minOccurs",
			      (xmlChar *) "0");
		}
	      if (inside_unbounded[class_count])
		{
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlSetProp(%s,\"minOccurs\",\"unbounded\"))\n",
				   fullName (newNode));
#endif
		  xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			      (xmlChar *) "unbounded");
		}
	      else if (!add_array_indexes_to_name && max_occurs > 1)
		{
		  SNPRINTF_FUNC ( SNPRINTF_ARGS(max_occurs_string,sizeof(max_occurs_string)),
				  "%d", max_occurs);
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlSetProp(%s,\"maxOccurs\",\"%s\"))\n",
				   fullName (newNode), max_occurs_string);
#endif
		  xmlSetProp (newNode, (xmlChar *) "maxOccurs",
			      fakeStrdup (max_occurs_string));
		  max_occurs = 0;
		}
	      currentElement = newNode;
	    }
	}
      else
	{
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,0,%s,0);\n",
			   fullName (((xmlNodePtr) currentNode)), name);
#endif
	  newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
			     0);
	}
      if (!schema_gen_mode)
	{
	  if (0 == ((xmlNodePtr) currentNode)->children)
	    {
	      ((xmlNodePtr) currentNode)->children = newNode;
	    }
	  if (0 == newNode->parent)
	    {
	      newNode->parent = ((xmlNodePtr) currentNode);
	    }
	  currentNode = newNode;
	}
    }
  else
    {
      if (inside_classvar_not_found > 0)
	{
	  inside_classvar_not_found++;
	}
      else
	{
	  newNode = (xmlNodePtr) findNode (name);
	  if (0 != newNode)
	    {
	      currentNode = newNode;
	    }
	  else
	    {
	      inside_classvar_not_found = 1;
	    }
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_XML_UPDATER::endClassVar (const char *name)
{
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::endClassVar(%s) called, class_count=%d, inside_classvar_not_found=%d,currentNode=%s,currentElement=%s\n",
		   name, class_count, inside_classvar_not_found,
		   fullName (((xmlNodePtr) currentNode)),
		   fullName (((xmlNodePtr) currentNode)));
#endif
  if (0 == name || 0 == ((xmlNodePtr) currentNode))
    return CMS_STATUS_NOT_SET;

  int wi = 0;
  int allchildrennameless = 1;
  int orig_last_var_was_struct= last_var_was_struct;
  last_var_was_struct = 1;
  if (inside_classvar_not_found > 0)
    {
      inside_classvar_not_found--;
      return CMS_STATUS_NOT_SET;
    }
  if (0 != ((xmlNodePtr) currentNode))
    {
      if (encoding && !schema_gen_mode && (make_xml_pretty && !content_set_for_this_class))
	{
	  if(!content_set_for_this_class)
	    {
	      xmlNodePtr curChild = ((xmlNodePtr) currentNode)->children;
	      if(curChild)
		{
		  while(curChild)
		    {
		      if(curChild->name)
			{
			  allchildrennameless=0;
			  break;
			}
		      curChild=curChild->next;
		    }
		  if(allchildrennameless)
		    {
		      curChild = ((xmlNodePtr) currentNode)->children;
		      while(curChild)
			{
			  xmlNodePtr nextChild = curChild->next;
			  xmlUnlinkNode(curChild);
			  xmlFreeNode(curChild);
			  curChild = nextChild;
			}
		      ((xmlNodePtr) currentNode)->children=0;
		      ((xmlNodePtr) currentNode)->content=0;
		    }
		}
	    }
	  if(!allchildrennameless)
	    {
	      memset (whitespace, 0, 50);
	      wi = 0;
	      whitespace[0] = '\n';
	      for (wi = 1;
		   wi <
		     (schema_gen_mode ? elementtablevel[class_count] : class_count +
		      1) && wi < 49; wi++)
		{
		  whitespace[wi] = '\t';
		}
	      //strncpy(whitespace+wi,name,50-wi);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			       fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	      xmlNodeAddContent (((xmlNodePtr) currentNode),
				 fakeStrdup (whitespace));
	    }
	}
      else
	{
	  last_var_was_struct = orig_last_var_was_struct;
	}
      if (0 == ((xmlNodePtr) currentNode)->parent)
	{
	  rcs_print_error
	    ("CMS_XML_UPDATER::endClassVar -- node has no parent, "
	     " possibly mismatched beginClassVar endClassVar calls for %s\n",
	     name);
	}
      else
	{
	  xmlNodePtr parentNode = ((xmlNodePtr) currentNode)->parent;
	  if (structisempty[class_count + 1] && !schema_gen_mode)
	    {
	      if (diffbuff != 0)
		{
		  xmlNodePtr prevNode = ((xmlNodePtr) currentNode)->prev;
		  while (prevNode)
		    {
		      if (0 == prevNode->name)
			{
			  
#ifdef DEBUG_THIS_FILE
			  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
					   "xmlUnlinkNode(%s);\n",
					   fullName (((xmlNodePtr)
						      prevNode)));
#endif
			  xmlUnlinkNode (((xmlNodePtr) prevNode));
			  xmlFreeNode (((xmlNodePtr) prevNode));
			}
		      else
			{
			  break;
			}
		      prevNode = ((xmlNodePtr) prevNode)->prev;
		    }
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlUnlinkNode(%s);\n",
				   fullName (((xmlNodePtr) currentNode)));
#endif
		  xmlUnlinkNode (((xmlNodePtr) currentNode));
		  xmlFreeNode (((xmlNodePtr) currentNode));
		}
	      else
		{
		  int reallyempty = 1;
		  xmlNodePtr child = ((xmlNodePtr) currentNode)->children;
		  while (child)
		    {
		      xmlNodePtr nextchild = child->next;
		      if (0 != child->name)
			{
			  reallyempty = 0;
			  break;
			}
		      child = nextchild;
		    }
		  if (reallyempty)
		    {
		      child = ((xmlNodePtr) currentNode)->children;
		      while (child)
			{
			  xmlNodePtr nextchild = child->next;
			  if (0 == child->name)
			    {
			      xmlUnlinkNode (child);
			      xmlFreeNode (child);
			    }
			  child = nextchild;
			}
		      if (0 == ((xmlNodePtr) currentNode)->children)
			{
			  ((xmlNodePtr) currentNode)->content = 0;
			}
		    }
		}
	      currentNode = 0;
	      currentElement = 0;
	      last_var_was_struct = orig_last_var_was_struct;
	    }
	  else
	    {
	      structisempty[class_count] = 0;
	    }
	  if (!schema_gen_mode)
	    {
	      currentElement = ((xmlNodePtr) currentNode);
	      currentNode = parentNode;
	      if (encoding && currentElement != 0)
		{
		  if (((xmlNodePtr) currentElement)->children == 0 &&
		      ((xmlNodePtr) currentElement)->name != 0 &&
		      ((xmlNodePtr) currentElement)->parent != 0 &&
		      ((xmlNodePtr) currentElement)->parent == currentNode &&
		      diffbuff != 0)
		    {
		      xmlUnlinkNode ((xmlNodePtr) currentElement);
		      xmlFreeNode ((xmlNodePtr) currentElement);
		      currentElement = 0;
		    }
		}
#ifndef USE_LIBXML2
	      //xmlFlushNodeContent((xmlNodePtr)currentElement);
#endif
	    }
	}
#if 0
      if (encoding && !schema_gen_mode && (make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  wi = 0;
	  whitespace[0] = '\n';
	  for (wi = 1;
	       wi <
		 (schema_gen_mode ? elementtablevel[class_count] : class_count +
		  1) && wi < 49; wi++)
	    {
	      whitespace[wi] = '\t';
	    }
	  //strncpy(whitespace+wi,name,50-wi);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
#endif
    }
  else if(content_set_for_this_class)
    {
      last_var_was_struct = 0; 
    }
  content_set_for_this_class=0;
  return CMS_STATUS_NOT_SET;

}

CMS_STATUS
  CMS_XML_UPDATER::beginStructArrayElem (const char *name, int elemnum)
{
  if (add_array_indexes_to_name && !inside_unbounded[class_count])
    {
      strncpy (arrayvarname, name, 198);
      size_t namelen = strlen (name);
      if (namelen > 198)
	{
	  namelen = 198;
	}
      SNPRINTF_FUNC ( SNPRINTF_ARGS(arrayvarname+namelen,sizeof(arrayvarname)-namelen),
		      "-%d", elemnum);

      return beginClassVar (arrayvarname);
    }
  else
    {
      return beginClassVar (name);
    }
}


CMS_STATUS
CMS_XML_UPDATER::endStructArrayElem (const char *name, int elemnum)
{
  if (add_array_indexes_to_name && !inside_unbounded[class_count])
    {
      strncpy (arrayvarname, name, 198);
      size_t namelen = strlen (name);
      if (namelen > 198)
	{
	  namelen = 198;
	}
      SNPRINTF_FUNC ( SNPRINTF_ARGS(arrayvarname+namelen,sizeof(arrayvarname)-namelen),
		      "-%d", elemnum);

      return endClassVar (arrayvarname);
    }
  else
    {
      return endClassVar (name);
    }
}



CMS_STATUS
  CMS_XML_UPDATER::beginStructDynamicArray (const char *name, int &len,
					    int maxlen)
{
  if (schema_gen_mode)
    len = maxlen;
  if (!encoding && !schema_gen_mode && !add_array_indexes_to_name)
    {
      len = countNodes (name);
      if (len > maxlen)
	{
	  rcs_print_error ("Too many (%d>%d) %s nodes in %s\n", len, maxlen,
			   name, fullName ((xmlNodePtr) currentNode));
	}
    }
  inside_dla[class_count] = 1;
  return (CMS_STATUS_NOT_SET);
}


CMS_STATUS
CMS_XML_UPDATER::endStructDynamicArray (const char *,int &,int)
{
  inside_dla[class_count] = 0;
  return (CMS_STATUS_NOT_SET);
}


CMS_XML_UPDATER::CMS_XML_UPDATER(const CMS_XML_UPDATER &):
  CMS_UPDATER(0),
  make_xml_pretty(0),max_occurs(0),
  stringstofree(0),maxstringstofree(0),numstringstofree(0),
  stringspacebegin(0),stringspaceend(0),stringspacesize(0),stringspaceused(0),
  schema_gen_mode(0),base(0),diffbuff(0),diffbuff_size(0),
  autovarnum(0),class_count(0),base_class_count(0),
  inside_classvar_not_found(0),last_var_was_struct(0),
  structisempty(0),inside_dla(0),inside_unbounded(0),elementtablevel(0),
  maxclasscount(0),schema_type_index(0),maxleftoffnodes(0),
  lastenuminfo(0),lastenumname(0),enumarraylen(0),inside_unbounded_array(0),
  currentNode(0),currentElement(0),schemaNode(0),xsdNs(0),normalNs(0),
  leftOffNodes(0),leftOffElements(0),mainElementNode(0),doc(0),
  unbounded_struct_array_bases(0),
  unbounded_struct_array_diffbuffs(0),
  unbounded_struct_array_diffbuff_sizes(0),
  unbounded_struct_array_count(0),
  max_unbounded_struct_array_count(0),
  content_set_for_this_class(0),
  inside_xml_declaration_addition(0),
  after_xml_declaration_addition(0),
  inside_root_start_addition(0),
  ns_href(0),
  ns_prefix(0),
  ns_changed(false)
{
  rcs_print_error("CMS_XML_UPDATER copy constructor should never be called.\n");
}
  
CMS_XML_UPDATER &
CMS_XML_UPDATER::operator=(const CMS_XML_UPDATER &)
{
  rcs_print_error("CMS_XML_UPDATER::operator= should never be called.\n");
  return(*this);
}


CMS_XML_UPDATER::CMS_XML_UPDATER (CMS * _cms_parent):
  CMS_UPDATER (_cms_parent, 1,16),
  make_xml_pretty(0),max_occurs(0),
  stringstofree(0),maxstringstofree(0),numstringstofree(0),
  stringspacebegin(0),stringspaceend(0),stringspacesize(0),stringspaceused(0),
  schema_gen_mode(0),base(0),diffbuff(0),diffbuff_size(0),
  autovarnum(0),class_count(0),base_class_count(0),
  inside_classvar_not_found(0),last_var_was_struct(0),
  structisempty(0),inside_dla(0),inside_unbounded(0),elementtablevel(0),
  maxclasscount(0),schema_type_index(0),maxleftoffnodes(0),
  lastenuminfo(0),lastenumname(0),enumarraylen(0),inside_unbounded_array(0),
  currentNode(0),currentElement(0),schemaNode(0),xsdNs(0),normalNs(0),
  leftOffNodes(0),leftOffElements(0),mainElementNode(0),doc(0),
  unbounded_struct_array_bases(0),
  unbounded_struct_array_diffbuffs(0),
  unbounded_struct_array_diffbuff_sizes(0),
  unbounded_struct_array_count(0),
  max_unbounded_struct_array_count(0),
  content_set_for_this_class(0),
  inside_xml_declaration_addition(0),
  after_xml_declaration_addition(0),
  inside_root_start_addition(0),
  ns_href(0),
  ns_prefix(0),
  ns_changed(false)
{
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
		   "constructing CMS_XML_UPDATER (compiled on " __DATE__ " "
		   __TIME__ ")\n");
#endif
  memset(valuestring,0,sizeof(valuestring));
  doc = xmlNewDoc ((xmlChar *) "1.0");
  class_count = 0;
  if ((size_t)encoded_header_size < sizeof (CMS_HEADER))
    {
      encoded_header_size = 512;
    }
  if (!encoded_header)
    {
      encoded_header = DEBUG_MALLOC (encoded_header_size);
    }

  if ((size_t) encoded_queuing_header_size < sizeof (CMS_QUEUING_HEADER))
    {
      encoded_queuing_header_size = 512;
    }
  if (!encoded_queuing_header)
    {
      encoded_queuing_header = DEBUG_MALLOC (encoded_queuing_header_size);
    }

  xsdNs = 0;
  base_class_count = 0;
  add_array_indexes_to_name = true;
  inside_classvar_not_found = 0;
  stringspacesize = encoded_data_size * 2;
  stringspacebegin = (char *) DEBUG_MALLOC (stringspacesize);
  memset (stringspacebegin, 0, 256);
  stringspaceend = stringspacebegin;
  stringspaceused = 0;
  stringstofree = (char **) DEBUG_MALLOC (sizeof (char *) * 16);

  inside_xml_declaration_addition=0;
  after_xml_declaration_addition=0;
  inside_root_start_addition=0;
  ns_prefix=0;
  ns_href=0;
  normalNs=0;

  for (int i1 = 0; i1 < 16; i1++)
    {
      stringstofree[i1] = 0;
    }
  maxstringstofree = 16;
  numstringstofree = 0;
  diffbuff = 0;
  diffbuff_size = 0;
  maxclasscount = 64;
  structisempty = (char *) DEBUG_MALLOC (maxclasscount);
  elementtablevel = (char *) DEBUG_MALLOC (maxclasscount);
  inside_dla = (char *) DEBUG_MALLOC (maxclasscount);
  inside_unbounded = (char *) DEBUG_MALLOC (maxclasscount);
  for (int i = 0; i < maxclasscount; i++)
    {
      elementtablevel[i] = 4;
      structisempty[i] = 0;
      inside_dla[i] = 0;
      inside_unbounded[i] = 0;
    }
#ifdef USE_LIBXML2
  leftOffNodes = (xmlNode **) DEBUG_MALLOC (maxclasscount * sizeof (xmlNodePtr));
  leftOffElements = (xmlNode **) DEBUG_MALLOC (maxclasscount * sizeof (xmlNodePtr));
#else
  leftOffNodes = (void **) DEBUG_MALLOC (maxclasscount * sizeof (xmlNodePtr));
  leftOffElements = (void **) DEBUG_MALLOC (maxclasscount * sizeof (xmlNodePtr));
#endif
  maxleftoffnodes = 0;
  for (int jj = 0; jj < maxleftoffnodes; jj++)
    {
      ((xmlNodePtr *) leftOffNodes)[jj] = 0;
      ((xmlNodePtr *) leftOffElements)[jj] = 0;
    }
  memset (autovarname, 0, sizeof (autovarname));
  currentNode = 0;
  currentElement = 0;
  mainElementNode = 0;
  schemaNode = 0;
  schema_gen_mode = 0;
  lastenumname = 0;
  autovarnum = 0;
  enumarraylen = 0;
  make_xml_pretty = 0;
  last_var_was_struct = 0;
  schema_type_index = 0;
  content_set_for_this_class=0;
  
  my_xml_style_properties_count=0;
  recheck_properties();
  if(after_xml_declaration_addition)
    {
      ((xmlDocPtr)doc)->header = after_xml_declaration_addition;
    }


  unbounded_struct_array_bases = 0;
  unbounded_struct_array_diffbuffs = 0;
  unbounded_struct_array_diffbuff_sizes = 0;
  unbounded_struct_array_count = 0;
  max_unbounded_struct_array_count = 0;

  check_type_info_called=false;

#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
		   "finished constructing CMS_XML_UPDATER\n");
#endif
}

CMS_XML_UPDATER::~CMS_XML_UPDATER ()
{
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_CMS_DESTRUCTORS, "deleting CMS_XML_UPDATER\n");
#endif
  if(0 != normalNs)
    {
      xmlFreeNs((xmlNsPtr)normalNs);
      normalNs=0;
    }
  if (0 != doc)
    {
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_CMS_DESTRUCTORS,
		       "~CMS_XML_UPDATER deleting doc\n");
#endif
      xmlFreeDoc ((xmlDocPtr) doc);
      doc = 0;
    }
  if (0 != xsdNs)
    {
      xmlFreeNs ((xmlNsPtr) xsdNs);
      xsdNs = 0;
    }
  
  if (elementtablevel != 0)
    {
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_CMS_DESTRUCTORS,
		       "~CMS_XML_UPDATER deleting elementtablevel\n");
#endif
      DEBUG_FREE (elementtablevel);
      elementtablevel = 0;
    }
  if (structisempty != 0)
    {
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_CMS_DESTRUCTORS,
		       "~CMS_XML_UPDATER deleting structisempty\n");
#endif
      DEBUG_FREE (structisempty);
      structisempty = 0;
    }
  if (inside_dla != 0)
    {
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_CMS_DESTRUCTORS,
		       "~CMS_XML_UPDATER deleting inside_dla\n");
#endif
      DEBUG_FREE (inside_dla);
      inside_dla = 0;
    }
  if (inside_unbounded != 0)
    {
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_CMS_DESTRUCTORS,
		       "~CMS_XML_UPDATER deleting inside_unbounded\n");
#endif
      DEBUG_FREE (inside_unbounded);
      inside_unbounded = 0;
    }
  if (0 != stringstofree)
    {
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_CMS_DESTRUCTORS,
		       "~CMS_XML_UPDATER deleting stringstofree\n");
#endif

      int fi = 0;
      for (fi = 0; fi < numstringstofree; fi++)
	{
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_CMS_DESTRUCTORS, "~CMS_XML_UPDATER fi=%d\n",
			   fi);
#endif

	  if (0 != stringstofree[fi])
	    {
	      if (stringspacebegin == stringstofree[fi])
		{
		  stringspacebegin = 0;
		}
	      DEBUG_FREE (stringstofree[fi]);
	      stringstofree[fi] = 0;
	    }
	}
      numstringstofree = 0;
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_CMS_DESTRUCTORS,
		       "~CMS_XML_UPDATER stringstofree=%p\n", stringstofree);
#endif
      DEBUG_FREE (stringstofree);
      stringstofree = 0;
      maxstringstofree = 0;
    }
  if (stringspacebegin != 0)
    {
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_CMS_DESTRUCTORS,
		       "~CMS_XML_UPDATER deleting stringspacebegin\n");
#endif
      DEBUG_FREE (stringspacebegin);
      stringspacebegin = 0;
      stringspaceend = 0;
      stringspacesize = 0;
      stringspaceused = 0;
    }
  if (0 != leftOffNodes)
    {
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_CMS_DESTRUCTORS,
		       "~CMS_XML_UPDATER deleting leftOffNodes\n");
#endif
      DEBUG_FREE (leftOffNodes);
      leftOffNodes = 0;
    }
  if (0 != leftOffElements)
    {
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_CMS_DESTRUCTORS,
		       "~CMS_XML_UPDATER deleting leftOffElements\n");
#endif
      DEBUG_FREE (leftOffElements);
      leftOffElements = 0;
    }
  if (NULL != encoded_header)
    {
      DEBUG_FREE (encoded_header);
      encoded_header = NULL;
    }
  if (NULL != encoded_queuing_header)
    {
      DEBUG_FREE (encoded_queuing_header);
      encoded_queuing_header = NULL;
    }
}

void
CMS_XML_UPDATER::rewind ()
{
  check_type_info_called=false;
  recheck_properties();

  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::rewind () -- encoding=%d, schema_gen_mode=%d\n",
		   encoding, schema_gen_mode);
#endif
  if (schema_gen_mode)
    {
      mode = CMS_ENCODE_DATA;
      encoding = 1;
    }
  if (!schema_gen_mode)
    {
      if (0 != doc)
	{
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlFreeDoc() called.\n");
#endif

	  xmlFreeDoc ((xmlDocPtr) doc);
	  doc = 0;
	  schemaNode = 0;
	  currentNode = 0;
	  mainElementNode = 0;
	  currentElement = 0;
	}
    }
  if (encoding)
    {
      if (doc == 0)
	{
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlNewDoc() called.\n");
#endif
	  doc = xmlNewDoc ((xmlChar *) "1.0");
	  if(after_xml_declaration_addition)
	    {
	      ((xmlDocPtr)doc)->header = after_xml_declaration_addition;
	    }
	  switch (mode)
	    {
	    case CMS_ENCODE_DATA:
	      //memset (encoded_data, 0, encoded_data_size);
	      break;


	    case CMS_ENCODE_HEADER:
	      memset (encoded_header, 0, encoded_header_size);
	      break;


	    case CMS_ENCODE_QUEUING_HEADER:
	      memset (encoded_queuing_header, 0, encoded_queuing_header_size);
	      break;

	    case CMS_NO_UPDATE:
	      break;

	    case CMS_DECODE_DATA:
	    case CMS_DECODE_HEADER:
	    case CMS_DECODE_QUEUING_HEADER:
	    default:
	      rcs_print_error("CMS_XML_UPDATER: bad mode =%d\n",mode);
	      break;
	      
	    }
	}
    }
  else
    {
      switch (mode)
	{
	case CMS_DECODE_DATA:
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlParseMemory (%s,%ld) called\n", 
			   (char *)encoded_data,
			   encoded_data_size);
#endif
	  doc =
	    xmlParseMemory ((const char *) encoded_data, encoded_data_size);
	  break;


	case CMS_DECODE_HEADER:
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlParseMemory (%s,%ld) called\n", 
			   (char *) encoded_header,
			   encoded_header_size);
#endif
	  doc =
	    xmlParseMemory ((const char *) encoded_header,
			    encoded_header_size);
	  break;


	case CMS_DECODE_QUEUING_HEADER:
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlParseMemory (%s,%ld) called\n",
			   (char *)encoded_queuing_header,
			   encoded_queuing_header_size);
#endif
	  doc =
	    xmlParseMemory ((const char *) encoded_queuing_header,
			    encoded_queuing_header_size);
	  break;


	case CMS_NO_UPDATE:
	  break;
	  
	case CMS_ENCODE_DATA:
	case CMS_ENCODE_HEADER:
	case CMS_ENCODE_QUEUING_HEADER:
	default:
	  rcs_print_error("CMS_XML_UPDATER: bad mode =%d\n",mode);
	  break;
	}
      if (0 == doc)
	{
	  rcs_print_error ("xmlParseMemory failed. size=%ld,memory=%s\n",
			   encoded_data_size, (char *)encoded_data);
	  status = CMS_UPDATE_ERROR;
	}
      else
	{
	  currentNode = xmlDocGetRootElement ((xmlDocPtr) doc);

	  if (0 == currentNode)
	    {
	      rcs_print_error
		("parsed document has no root size=%ld memory=%s\n",
		 encoded_data_size, (char *)encoded_data);
	      status = CMS_UPDATE_ERROR;
	    }
	}
    }
  next_default=0;
  class_count = 0;
  base_class_count = 0;
  base = 0;
  inside_classvar_not_found = 0;
  content_set_for_this_class=0;
  memset(inside_dla,0,maxclasscount);
  memset(inside_unbounded,0,maxclasscount);
  memset(structisempty,0,maxclasscount);
  if(!schema_gen_mode && make_xml_pretty)
    {
      memset(elementtablevel,4,maxclasscount);
    }


  if (!schema_gen_mode || schema_type_index < 1)
    {

      int fi = 0;
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "CMS_XML_UPDATER::rewind() numstringstofree=%d\n",
		       numstringstofree);
#endif
      if (numstringstofree > 1)
	{
	  for (fi = 0; fi < numstringstofree; fi++)
	    {
	      if (0 != stringstofree[fi])
		{
		  DEBUG_FREE (stringstofree[fi]);
		  stringstofree[fi] = 0;
		}
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "CMS_XML_UPDATER::rewind() stringspacesize=%d\n",
			   stringspacesize);
#endif
	  stringspacebegin = (char *) DEBUG_MALLOC (stringspacesize);
	  if (stringspacebegin == 0)
	    {
	      rcs_print_error ("Out of memory\n");
	    }
	  stringstofree[0] = stringspacebegin;
	}
      stringspaceused = 0;
      stringspaceend = stringspacebegin;
      numstringstofree = 1;
      currentElement = 0;
    }
  for (int jj = 0; jj < maxleftoffnodes; jj++)
    {
      ((xmlNodePtr *) leftOffNodes)[jj] = 0;
      ((xmlNodePtr *) leftOffElements)[jj] = 0;
    }
  currentNode = 0;
  autovarnum = 0;
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::rewind () finished\n");
#endif
}


int
CMS_XML_UPDATER::get_encoded_msg_size ()
{
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::get_encoded_msg_size () -- encoding=%d\n",
		   encoding);
#endif
  xmlChar * newencodeddataarea;
  int newencodeddatasize;

  if (encoding)
    {
      switch (mode)
	{
	case CMS_ENCODE_DATA:
	  newencodeddataarea = (xmlChar *) encoded_data;
	  newencodeddatasize = encoded_data_size;
	  xmlDocDumpMemory ((xmlDocPtr) doc, &newencodeddataarea,
			    &newencodeddatasize);
	  if (newencodeddataarea == 0)
	    {
	      rcs_print_error
		("CMS_XML_UPDATER::get_encoded_msg_size -- xmlDocDumpMemory  failed.\n");
	      status = CMS_UPDATE_ERROR;
	      return 0;
	    }
	  if (newencodeddataarea != encoded_data)
	    {
	      if (!using_external_encoded_data)
		{
		  if (newencodeddatasize >= encoded_data_size)
		    {
		      if (cms_parent)
			{
			  int orig_zero_encoded_data_when_set =
			    cms_parent->zero_encoded_data_when_set;
			  cms_parent->zero_encoded_data_when_set = 0;
			  cms_parent->set_encoded_data (newencodeddataarea,
							newencodeddatasize);
			  cms_parent->zero_encoded_data_when_set =
			    orig_zero_encoded_data_when_set = 0;
			}
		      else
			{
			  set_encoded_data (newencodeddataarea,
					    newencodeddatasize);
			}
		      using_external_encoded_data = false;
		    }
		  else
		    {
		      memcpy (encoded_data, newencodeddataarea,
			      newencodeddatasize);
		      ((char *) encoded_data)[newencodeddatasize - 1] = 0;
		      xmlFree (newencodeddataarea);
		    }
		}
	      else
		{
		  if (newencodeddatasize > encoded_data_size)
		    {
		      rcs_print_error
			("CMS_XML_UPDATER::get_encoded_msg_size() encoded message of size %d is too big. (>%ld)\n",
			 newencodeddatasize, encoded_data_size);
		      memcpy (encoded_data, newencodeddataarea,
			      encoded_data_size);
		      ((char *) encoded_data)[encoded_data_size - 1] = 0;
		      status = CMS_UPDATE_ERROR;
		    }
		  else
		    {
		      memcpy (encoded_data, newencodeddataarea,
			      newencodeddatasize);
		      ((char *) encoded_data)[newencodeddatasize - 1] = 0;
		    }
		  xmlFree (newencodeddataarea);
		}
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlDocDump returned (size=%d,%s)\n",
			   newencodeddatasize, (char *) encoded_data);
#endif
	  return newencodeddatasize;
	  break;


	case CMS_ENCODE_HEADER:
	  xmlChar * newencodedheaderarea;
	  int newencodedheadersize;
	  newencodedheaderarea = (xmlChar *) encoded_header;
	  newencodedheadersize = encoded_header_size;
	  xmlDocDumpMemory ((xmlDocPtr) doc, &newencodedheaderarea,
			    &newencodedheadersize);
	  if (newencodedheaderarea == 0)
	    {
	      rcs_print_error
		("CMS_XML_UPDATER::get_encoded_msg_size -- xmlDocDumpMemory  failed.\n");
	      status = CMS_UPDATE_ERROR;
	      return 0;
	    }
	  if (newencodedheaderarea != encoded_header)
	    {
	      memcpy (encoded_header, newencodedheaderarea,
		      newencodedheadersize);
	      ((char *) encoded_header)[newencodedheadersize - 1] = 0;
	      xmlFree (newencodedheaderarea);
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlDocDump returned (size=%d,%s)\n",
			   newencodedheadersize, (char *) encoded_header);
#endif
	  return newencodedheadersize;
	  break;


	case CMS_ENCODE_QUEUING_HEADER:
	  xmlChar * newencodedqueuing_headerarea;
	  int newencodedqueuing_headersize;
	  newencodedqueuing_headerarea = (xmlChar *) encoded_queuing_header;
	  newencodedqueuing_headersize = encoded_queuing_header_size;
	  xmlDocDumpMemory ((xmlDocPtr) doc, &newencodedqueuing_headerarea,
			    &newencodedqueuing_headersize);
	  if (newencodedqueuing_headerarea == 0)
	    {
	      rcs_print_error
		("CMS_XML_UPDATER::get_encoded_msg_size -- xmlDocDumpMemory  failed.\n");
	      status = CMS_UPDATE_ERROR;
	      return 0;
	    }
	  if (newencodedqueuing_headerarea != encoded_queuing_header)
	    {
	      memcpy (encoded_queuing_header, newencodedqueuing_headerarea,
		      newencodedqueuing_headersize);
	      ((char *) encoded_queuing_header)[newencodedqueuing_headersize -
						1] = 0;
	      xmlFree (newencodedqueuing_headerarea);
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlDocDump returned (size=%d,%s)\n",
			   newencodedqueuing_headersize,
			   (char *) encoded_queuing_header);
#endif
	  return newencodedqueuing_headersize;
	  break;

	case CMS_NO_UPDATE:
	  break;
	  
	case CMS_DECODE_DATA:
	case CMS_DECODE_HEADER:
	case CMS_DECODE_QUEUING_HEADER:
	default:
	  break;
	}
    }
  return (int) strlen ((const char *) encoded_data);
}

long
CMS_XML_UPDATER::check_type_info (long type, void *buffer, const char *nsname,
				  cms_symbol_lookup_function_t
				  symbol_lookup_function,
				  const char **namelist, const long *idlist,
				  const size_t * sizelist, long list_length,
				  long max_name_length)
{
  check_type_info_called=true;

#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::check_type_info(%ld . . .) called. encoding=%d\n",
		   type, encoding);
#endif
  CMS_UPDATER::check_type_info (type, buffer, nsname, symbol_lookup_function,
				namelist, idlist, sizelist, list_length,
				max_name_length);

  base = buffer;
  xmlNodePtr rootNode;
  if (schema_gen_mode)
    {
      mode = CMS_ENCODE_DATA;
      encoding = 1;
    }
  if (list_length < 2)
    {
      rcs_print_error
	("CMS_XML_UPDATER::check_type_info bad list_length %ld\n",
	 list_length);
      status = CMS_UPDATE_ERROR;
      return -1;
    }
  if (max_name_length < 2)
    {
      rcs_print_error
	("CMS_XML_UPDATER::check_type_info bad max_name_length %ld\n",
	 max_name_length);
      rcs_print_error("Check that \"generate_symbol_lookups=true\" was passed to CodeGenerator.\n");
      status = CMS_UPDATE_ERROR;
      return -1;
    }
  if (encoding)
    {
      if (!schema_gen_mode)
	{
	  cms_parent->nmltypename = symbol_lookup_function (type);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewDocRawNode((xmlDocPtr)doc=%p,0,%s, . . .) called.\n",
			   doc,cms_parent->nmltypename);
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "make_xml_pretty=%d,content_set_for_this_class=%d\n",
			   make_xml_pretty,content_set_for_this_class);
#endif
	  if ((make_xml_pretty && !content_set_for_this_class))
	    {
	      mainElementNode =
		xmlNewDocRawNode ((xmlDocPtr) doc, 0, (xmlChar *) cms_parent->nmltypename,
				  (xmlChar *) "\n\t");
	    }
	  else
	    {
	      mainElementNode =
		xmlNewDocRawNode ((xmlDocPtr) doc, 0, (xmlChar *) cms_parent->nmltypename,0);
	    }
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "mainElementNode=%p\n",mainElementNode);
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "normalNs=%p\n",normalNs);
#endif

	  if(0 != normalNs)
	    {
	      xmlFreeNs((xmlNsPtr)normalNs);
	      normalNs=0;
	    }
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "ns_prefix=%s,ns_href_%s\n",
			   ns_prefix,ns_href);
#endif
	  if(ns_prefix != 0 || ns_href != 0)
	    {
	      normalNs = xmlNewNs(((xmlNodePtr)mainElementNode),(xmlChar*)ns_href,(xmlChar*)ns_prefix);
	      xmlSetNs(((xmlNodePtr)mainElementNode),((xmlNsPtr)normalNs));
	    }
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "normalNs=%p\n",normalNs);
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "inside_root_start_addition=%p\n",
			   inside_root_start_addition);
#endif
	  if(inside_root_start_addition)
	    {
	      ((xmlNodePtr)mainElementNode)->start_add = inside_root_start_addition;
	    }
	  xmlDocSetRootElement ((xmlDocPtr) doc,
				(xmlNodePtr) mainElementNode);
	  currentNode = mainElementNode;
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "currentNode=%p\n",
			   currentNode);
#endif

	}
      else
	{

	  if (0 == schemaNode)
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNewDocRawNode((xmlDocPtr)doc,0,\"schema\", . . .) called.\n");
#endif
	      schemaNode =
		xmlNewDocRawNode ((xmlDocPtr) doc, 0, (xmlChar *) "schema",
				  0);
	      if (0 == xsdNs)
		{
		  xsdNs =
		    xmlNewNs (((xmlNodePtr) schemaNode),
			      (xmlChar *) "http://www.w3.org/2001/XMLSchema",
			      (xmlChar *) "xsd");
                  xsdNs->qualify_attributes=false;
                  xsdNs->qualify_elements=true;
		}
	      xmlSetNs (((xmlNodePtr) schemaNode), (xmlNsPtr) xsdNs);
	      for (int i = 0; i < list_length - 1; i++)
		{
		  type = idlist[i];
		  const char *nmltypename = symbol_lookup_function (type);
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlNodeAddContent(%s, ...);\n",
				   fullName (schemaNode));
#endif
		  xmlNodeAddContent (((xmlNodePtr) schemaNode),
				     (xmlChar *) "\n\t");
		  mainElementNode =
		    xmlNewTextChild (((xmlNodePtr) schemaNode),
				     (xmlNsPtr) xsdNs, (xmlChar *) "element",
				     (xmlChar *) 0);
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlNewTextChild(%s,(xmlNsPtr)xsdNs,\"element\",...)\n",
				   (char *) ((xmlNodePtr) schemaNode)->name);
#endif
		  xmlSetProp ((xmlNodePtr) mainElementNode,
			      (xmlChar *) "name", (xmlChar *) nmltypename);
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlSetProp(%s,\"name\",%s)\n",
				   (char *) ((xmlNodePtr) mainElementNode)->
				   name, (char *) nmltypename);
#endif
		  xmlSetProp ((xmlNodePtr) mainElementNode,
			      (xmlChar *) "type", (xmlChar *) nmltypename);
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlSetProp(%s,\"type\",%s)\n",
				   (char *) ((xmlNodePtr) mainElementNode)->
				   name, (char *) nmltypename);
#endif
		}
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNodeAddContent(%s, ...);\n",
			       fullName (schemaNode));
#endif
	      xmlNodeAddContent (((xmlNodePtr) schemaNode),
				 (xmlChar *) "\n\n");
	      xmlDocSetRootElement ((xmlDocPtr) doc, (xmlNodePtr) schemaNode);
	    }
	  if (schema_type_index < list_length - 1)
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "schema_type_index = %d, list_length=%ld\n",
			       schema_type_index, list_length);
#endif
	      type = idlist[schema_type_index];
	      /* This is a hack that requires CMS depend on an NML 
	         definition. Not the way it was supposed to be designed. */
	      ((NMLmsg *) buffer)->type = idlist[schema_type_index];
	      ((NMLmsg *) buffer)->size = (long) sizelist[schema_type_index];
	      cms_parent->format_low_ptr = (char *) buffer;
	      cms_parent->format_high_ptr =
		((char *) buffer) + sizelist[schema_type_index];
	      if (sizelist[schema_type_index] > (size_t) cms_parent->size)
		{
		  rcs_print_error
		    ("CMS: size of %lu for message %s is too large for this buffer of size %lu\n",
		     (unsigned long) sizelist[schema_type_index],
		     (char  *) (namelist + (max_name_length * schema_type_index)),
		     (unsigned long) cms_parent->size);
		  status = CMS_UPDATE_ERROR;
		  return -1;
		}
	      schema_type_index++;
	    }
	  currentNode = schemaNode;
	  currentElement = mainElementNode;
	}

      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "CMS_XML_UPDATER::check_type_info returning %ld\n",
		       type);
#endif
      return type;
    }
  int section_size = (list_length) / 2;
  int last_section_size = list_length;
  int second_to_last_section_size = list_length * 2 + 1;
  int entry = section_size;
  int strcmpret = 0;
  char *nameptr = 0;
  if (namelist == 0 || idlist == 0 || sizelist == 0 || max_name_length < 2 ||
      list_length < 2)
    {
      rcs_print_error
	("CMS_XML_UPDATER::(%ld,%p,%s,%p,%p,%p,%p,%ld,%ld) bad parameter.\n",
	 type, (void*)buffer, nsname, (void*)symbol_lookup_function, 
	 (void*)namelist, (void*)idlist,
	 (void*)sizelist, list_length, max_name_length);
      status = CMS_UPDATE_ERROR;
      return -1;
    }
  if (doc == 0)
    {
      rcs_print_error ("CMS_XML_UPDATER:: doc is null.\n");
      status = CMS_UPDATE_ERROR;
      return -1;
    }
  rootNode = xmlDocGetRootElement ((xmlDocPtr) doc);
  if (rootNode == 0)
    {
      rcs_print_error
	("CMS_XML_UPDATER::check_type_info rootNode is null.\n");
      status = CMS_UPDATE_ERROR;
      return -1;
    }
  if (rootNode->name == 0)
    {
      rcs_print_error
	("CMS_XML_UPDATER::check_type_info rootNode->name is null.\n");
      status = CMS_UPDATE_ERROR;
      return -1;
    }
  if (currentNode == 0)
    {
      currentNode = rootNode;
    }
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::check_type_info(%ld . . .) called. rootNode->name=%s\n",
		   type, rootNode->name);
#endif

  while (section_size > 0 && second_to_last_section_size > 1)
    {
      if (entry > list_length - 2)
	{
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "entry=%d has to be clipped to %ld\n", entry,
			   list_length - 2);
#endif
	  entry = list_length - 2;
	}
      if (entry < 0)
	{
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "entry=%d has to be clipped to 0\n", entry);
#endif
	  entry = 0;
	}
      nameptr = ((char *) namelist) + (entry * max_name_length);
      if (0 == *nameptr)
	{
	  if (entry > 0)
	    {
	      entry--;
	      nameptr = ((char *) namelist) + (entry * max_name_length);
	    }
	  else
	    {
	      break;
	    }
	}
      strcmpret = strcmp ((char *) rootNode->name, nameptr);
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "entry=%d, nameptr=%s,section_size=%d,strcmpret=%d\n",
		       entry, nameptr, section_size, strcmpret);
#endif
      second_to_last_section_size = last_section_size;
      last_section_size = section_size;
      section_size = (section_size + 1) / 2;
      if (0 == strcmpret)
	{
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "CMS_XML_UPDATER::check_type_info(%ld . . .) called. entry = %d,idlist[entry]=%ld,sizeList[entry]=%d\n",
			   type, entry, idlist[entry], sizelist[entry]);
#endif

	  /* This is a hack that requires CMS depend on an NML 
	     definition. Not the way it was supposed to be designed. */
	  if (mode == CMS_DECODE_DATA)
	    {
	      ((NMLmsg *) buffer)->type = idlist[entry];
	      ((NMLmsg *) buffer)->size = (long) sizelist[entry];
	      if(cms_parent->message_size_add >0)
		{
		  ((NMLmsg *) buffer)->size += (long) cms_parent->message_size_add;
		}
	      cms_parent->format_low_ptr = (char *) buffer;
	      cms_parent->format_high_ptr =
		((char *) buffer) + ((NMLmsg*)buffer)->size;
	      if ( (size_t) (((NMLmsg*)buffer)->size) > (size_t) cms_parent->size)
		{
		  rcs_print_error
		    ("CMS: size of %lu for message %s is too large for this buffer of size %lu\n",
		     (unsigned long) ((NMLmsg*)buffer)->size, rootNode->name, 
		     (unsigned long) cms_parent->size);
		  status = CMS_UPDATE_ERROR;
		  return -1;
		}
	    }
	  cms_parent->nmltypename = nameptr;
	  return idlist[entry];
	}
      else if (strcmpret > 0)
	{
	  entry += section_size;
	}
      else if (strcmpret < 0)
	{
	  entry -= section_size;
	}
    }
  if(mode != CMS_DECODE_QUEUING_HEADER && mode != CMS_DECODE_HEADER)
    {
      rcs_print_error
	("Can not find NML type corresponding to XML root node name %s.\n",
	 rootNode->name);
      status = CMS_UPDATE_ERROR;
    }
  if (type > 0)
    {
      return type;
    }
  return -1;
}


/* This function is only useable with encoding methods such as 
   xml that allow partial messages to be sent. */
CMS_STATUS
  CMS_XML_UPDATER::setBufferForDiff (void *_diffbuf, size_t _diffbuff_size)
{
  diffbuff = _diffbuf;
  diffbuff_size = _diffbuff_size;
  return CMS_STATUS_NOT_SET;
}

int
CMS_XML_UPDATER::compareAttributeForDiff (const char *name, void *x,
					  size_t xsize)
{
  int retval;
  int orig_struct_is_empty = structisempty[class_count];
  if (0 == base || 0 == diffbuff || xsize < 1 || 0 == diffbuff_size
      || mode != CMS_ENCODE_DATA)
    {
      structisempty[class_count] = 0;
      last_var_was_struct = 0;
      return -1;
    }

  retval = compareForDiff (name, x, xsize);
  structisempty[class_count] = orig_struct_is_empty;
  return retval;
}

int
CMS_XML_UPDATER::compareForDiff (const char *name, void *x, size_t xsize)
{
  const char *compare_addr = 0;
  long offset = 0;
  int ret = 0;

  if (0 == base
      || 0 == diffbuff
      || xsize < 1 || 0 == diffbuff_size || mode != CMS_ENCODE_DATA)
    {
      structisempty[class_count] = 0;
      return -1;
    }
  offset = (long) (((char *) x) - ((char *) base));
  if (offset < 0 || (offset + xsize) > diffbuff_size)
    {
      rcs_print_error
	("CMS_XML_UPDATER::compareForDiff(name=%s,x=%p,xsize=%lu) (offset=%ld) < 0  || ((offset + xsize)=%lu) > (diffbuff_size=%lu)\n",
	 name, x, (unsigned long) xsize, offset, 
	 (unsigned long) (offset + xsize), (unsigned long) diffbuff_size);
      structisempty[class_count] = 0;
      last_var_was_struct = 0;
      return -1;
    }
  compare_addr = ((char *) diffbuff + offset);
  ret = memcmp (x, compare_addr, xsize);
  if (ret)
    {
      structisempty[class_count] = 0;
      last_var_was_struct = 0;
      return ret;
    }
  else if (make_xml_pretty && !content_set_for_this_class && currentNode)
     {
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "compareForDiff(name=%s,x=%p,xsize=%d) offset=%ld, compare_addr=%p,ret=%d\n",
		   name, x, xsize, offset, compare_addr, ret);
#endif
      xmlNodePtr prevNode = ((xmlNodePtr) currentNode)->last;
      while (prevNode)
	{
	  if (0 != prevNode->name)
	    {
	      break;
	    }
	  if (0 != prevNode->content)
	    {
	      if (!isspace (prevNode->content[0]))
		{
		  break;
		}
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlUnlinkNode(%s);\n",
			   fullName (((xmlNodePtr) prevNode)));
#endif
	  if (currentElement == prevNode)
	    {
	      currentElement = 0;
	    }
	  xmlUnlinkNode (((xmlNodePtr) prevNode));
	  xmlFreeNode (((xmlNodePtr) prevNode));
	  prevNode = ((xmlNodePtr) prevNode)->prev;
	}
    }
  return ret;
}


int
CMS_XML_UPDATER::update_enumeration_with_name (const char *name,
					       int enumin, void *enumaddr,
					       const cms_enum_info * info)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    return enumin;

  if (check_pointer_with_name (name, (char *) enumaddr, sizeof (int)) < 0)
    {
      return enumin;
    }

  last_var_was_struct=0;

#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "update_enumeration_with_name(%s,%d,%p,%p) called.\n",
		   name, enumin, enumaddr, info);
#endif

  if (encoding)
    {
      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), info->name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", (xmlChar *) info->name);
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\"))\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  xmlNodePtr origCurrentNode = ((xmlNodePtr) currentNode);
	  xmlNodePtr origCurrentElement = ((xmlNodePtr) currentElement);
	  currentNode = schemaNode;
	  xmlNodePtr enumNode =
	    (xmlNodePtr) findNodeWithProperty ("simpleType", "name",
					       info->name);
	  if (enumNode == 0)
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNodeAddContent(%s, ...);\n",
			       fullName (schemaNode));
#endif
	      xmlNodeAddContent (((xmlNodePtr) schemaNode),
				 (xmlChar *) "\n\t");
	      enumNode =
		xmlNewTextChild (((xmlNodePtr) schemaNode), (xmlNsPtr) xsdNs,
				 (xmlChar *) "simpleType",
				 (xmlChar *) "\n\t\t");
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNewTextChild(%s,\"xsd\",\"simpleType\",...);\
n",
			       ((char *) fullName (schemaNode)));
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"name\",%s))\n",
			       fullName (enumNode), info->name);
#endif
	      xmlSetProp (enumNode, (xmlChar *) "name",
			  (xmlChar *) info->name);
	      xmlNodePtr restrictionNode =
		xmlNewTextChild (enumNode, (xmlNsPtr) xsdNs,
				 (xmlChar *) "restriction", 0);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNewTextChild(%s,\"xsd\",\"restriction\",0);\
n",
			       ((char *) fullName (enumNode)));
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"base\",%s))\n",
			       fullName (restrictionNode), "xsd:string");
#endif
	      xmlSetProp (restrictionNode, (xmlChar *) "base", XSDTYPE("string"));
	      const char *val = (const char *) info->string_list;
	      for (int i = 0; i < (info->list_len - 1);
		   i++, val += info->max_string_len)
		{
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlNodeAddContent(%s, ...);\n",
				   fullName (restrictionNode));
#endif
		  xmlNodeAddContent (restrictionNode, (xmlChar *) "\n\t\t\t");
		  xmlNodePtr enumerationNode =
		    xmlNewTextChild (restrictionNode, (xmlNsPtr) xsdNs,
				     (xmlChar *) "enumeration", 0);
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlNewTextChild(%s,\"xsd\",\"enumeration\",0);\
n",
				   ((char *) fullName (restrictionNode)));
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "xmlSetProp(%s,\"value\",%s))\n",
				   fullName (enumerationNode), val);
#endif
		  xmlSetProp (enumerationNode, (xmlChar *) "value",
			      (xmlChar *) val);
		}
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNodeAddContent(%s, ...);\n",
			       fullName (restrictionNode));
#endif
	      xmlNodeAddContent (restrictionNode, (xmlChar *) "\n\t\t");
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlNodeAddContent(%s, ...);\n",
			       fullName (enumNode));
#endif
	      xmlNodeAddContent (enumNode, (xmlChar *) "\n\t");
	    }
	  currentNode = origCurrentNode;
	  currentElement = origCurrentElement;
	  return enumin;
	}

      if (0 == compareForDiff (name, enumaddr, sizeof (int)))
	return enumin;
      const char *sym = info->lookupfunc (enumin);
      if (0 == sym)
	{
	  rcs_print_error
	    ("CMS_XML_UPDATER::update_enumeration_with_name : Invalid value %d for enumeration of type %s named %s\n",
	     enumin, info->name, name);
	  status = CMS_UPDATE_ERROR;
	  return (enumin);
	}
      xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
		       (xmlChar *) sym);
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       (xmlChar *) sym);
#endif
    }
  else
    {
      const char *decodevaluestring = findNodeString (name);
      if (0 != decodevaluestring)
	{
	  int section_size = info->list_len / 2;
	  int last_section_size = info->list_len;
	  int second_to_last_section_size = info->list_len * 2 + 1;
	  int entry = section_size;
	  int strcmpret = 0;
	  char *nameptr = 0;
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "CMS_XML_UPDATER::update_enumeration_with_name(%s, ...) : decodevaluestring=%s\n",
			   name, decodevaluestring);
#endif
	  while(last_section_size > 0 && second_to_last_section_size > 1)
	    {
	      if (entry > info->list_len - 2)
		{
		  entry = info->list_len - 2;
		}
	      if (entry < 0)
		{
		  entry = 0;
		}
	      nameptr =
		((char *) info->string_list) + (entry * info->max_string_len);
	      if (0 == *nameptr)
		{
		  if (entry > 0)
		    {
		      entry--;
		      nameptr =
			((char *) info->string_list) +
			(entry * info->max_string_len);
		    }
		  else
		    {
		      break;
		    }
		}
	      
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "entry=%d, nameptr=%s,section_size=%d,last_section_size=%d,second_to_last_section_size=%d\n", entry, nameptr,
		   section_size,
		   last_section_size,
		   second_to_last_section_size);
#endif

	      strcmpret = strcmp ((char *) decodevaluestring, nameptr);
	      second_to_last_section_size = last_section_size;
	      last_section_size = section_size;
	      section_size = (section_size + 1) / 2;
	      if (0 == strcmpret)
		{
		  
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
				   "CMS_XML_UPDATER::update_enumeration_with_name(%s,...) called. entry = %d,int_list[entry]=%d\n",
				   name, entry, info->int_list[entry]);
#endif

#ifdef USE_LIBXML2
		  free (decodevaluestring);
#endif
		  enumin = info->int_list[entry];
		  return enumin;
		}
	      else if (strcmpret > 0)
		{
		  entry += section_size;
		  if(section_size == 0)
		    {
		      entry++;
		    }
		}
	      else if (strcmpret < 0)
		{
		  entry -= section_size;
		  if(section_size == 0)
		    {
		      entry--;
		    }
		}
	    }
	  rcs_print_error
	    ("Can not find value corresponding to %s for enum type %s of variable %s. info=%p,info->list_len=%d,info->max_string_len=%d,info->string_list=%p\n",
	     decodevaluestring, info->name, name,
	     (void *)info,
	     info->list_len,
	     info->max_string_len,
	     (void *)info->string_list);
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "update_enumeration_with_name(%s, ...) returning %d=%s\n",
		   name, enumin, info->lookupfunc (enumin));
#endif
  return enumin;
}

CMS_STATUS
  CMS_XML_UPDATER::beginEnumerationArray (const char *name,
					  const cms_enum_info * info,
					  unsigned int len)
{
  lastenuminfo = info;
  enumarraylen = len;
  lastenumname = name;
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::beginEnumerationDLA (const char *name,
					const cms_enum_info * info, int &len,
					int maxlen)
{
  lastenuminfo = info;
  enumarraylen = len;
  lastenumname = name;
  inside_dla[class_count] = 1;
  if (schema_gen_mode)
    {
      len = maxlen;
    }
  return CMS_STATUS_NOT_SET;
}

int
CMS_XML_UPDATER::update_enumeration_array_elem (int enumin, void *enumaddr,
						int elem)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    return enumin;

  if (check_pointer_with_name (lastenumname, (char *) enumaddr, sizeof (int))
      < 0)
    {
      return enumin;
    }

  last_var_was_struct=0;

  int enumout = enumin;

  if(add_array_indexes_to_name)
    {
      size_t namelen = strlen (lastenumname);
      if (namelen > sizeof (arrayvarname) - 20)
	{
	  rcs_print_error ("name %s is too long\n", lastenumname);
	  status = CMS_UPDATE_ERROR;
	  return enumin;
	}
      strcpy (arrayvarname, lastenumname);
      SNPRINTF_FUNC ( SNPRINTF_ARGS(arrayvarname+namelen,sizeof(arrayvarname)-namelen), "-%d", elem);
      enumout =
	update_enumeration_with_name (arrayvarname, enumin, enumaddr,
				      lastenuminfo);
    }
  else
    {
      enumout =
	update_enumeration_with_name (lastenumname, enumin, enumaddr,
				      lastenuminfo);
    }
  return enumout;
}


CMS_STATUS
CMS_XML_UPDATER::endEnumerationArray (const char *,
				      const cms_enum_info *,
				      unsigned int)
{
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
CMS_XML_UPDATER::endEnumerationDLA (const char *,
				    const cms_enum_info *,
				    int &,
				    int)
{
  inside_dla[class_count] = 0;
  return CMS_STATUS_NOT_SET;
}


int
CMS_XML_UPDATER::update_union_selector_with_name (
						  const char *,
						  int enumin, 
						  void *,
						  const cms_enum_info * info)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    return enumin;

  last_var_was_struct=0;

  if (!encoding)
    {
      for (int i = 0; i < info->list_len; i++)
	{
	  if (findNode (info->string_list[i]))
	    {
	      enumin = info->int_list[i];
	      break;
	    }
	}
    }

  return (enumin);
}



CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, CMS_DURATION & x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    return CMS_STATUS_NOT_SET;
  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;

  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:duration");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("duration"));
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\")\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareForDiff (name, &x, sizeof (x)))
	return CMS_STATUS_NOT_SET;

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "P%ldY%ldM%ldDT%ldH%ldM%fS",
	       x.years, x.months, x.days, x.hours, x.minutes, x.seconds);
      xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
		       fakeStrdup (valuestring));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *const_decodevaluestring = findNodeString (name);
      char *decodevaluestring = (char *) fakeStrdup(const_decodevaluestring);
      if (0 != decodevaluestring)
	{
	  char *sloc = (char *) strchr (decodevaluestring, 'S');
	  if (sloc)
	    {
	      *sloc = 0;
	      sloc--;
	      char c = *sloc;
	      while (((c >= '0' && c <= '9') || c == '.' || c == '-'
		      || c == 'E' || c == '+') && sloc > decodevaluestring)
		{
		  sloc--;
		  c = *sloc;
		}
	      sloc++;
#ifdef HAVE_STRTOF
	      x.seconds = strtof (sloc, 0);
#else
	      x.seconds = strtod (sloc, 0);
#endif
	    }
	  char *tloc = (char *) strchr (decodevaluestring, 'T');
	  if (tloc)
	    {
	      char *mloc = (char *) strchr (tloc + 1, 'M');
	      if (mloc && mloc > tloc)
		{
		  *mloc = 0;
		  mloc--;
		  char c = *mloc;
		  while (((c >= '0' && c <= '9') || c == '-' || c == '+')
			 && mloc > decodevaluestring)
		    {
		      mloc--;
		      c = *mloc;
		    }
		  mloc++;
		  x.minutes = strtol (mloc, 0, 0);
		}
	    }
	  char *hloc = (char *) strchr (decodevaluestring, 'H');
	  if (hloc)
	    {
	      *hloc = 0;
	      hloc--;
	      char c = *hloc;
	      while (((c >= '0' && c <= '9') || c == '-' || c == '+')
		     && hloc > decodevaluestring)
		{
		  hloc--;
		  c = *hloc;
		}
	      hloc++;
	      x.hours = strtol (hloc, 0, 0);
	    }
	  char *dloc = (char *) strchr (decodevaluestring, 'D');
	  if (dloc)
	    {
	      *dloc = 0;
	      dloc--;
	      char c = *dloc;
	      while (((c >= '0' && c <= '9') || c == '-' || c == '+')
		     && dloc > decodevaluestring)
		{
		  dloc--;
		  c = *dloc;
		}
	      dloc++;
	      x.days = strtol (dloc, 0, 0);
	    }

	  char *MMloc = (char *) strchr (decodevaluestring, 'Y');
	  if (MMloc && (!tloc || tloc > MMloc))
	    {
	      *MMloc = 0;
	      MMloc--;
	      char c = *MMloc;
	      while (((c >= '0' && c <= '9') || c == '-' || c == '+')
		     && MMloc > decodevaluestring)
		{
		  MMloc--;
		  c = *MMloc;
		}
	      MMloc++;
	      x.months = strtol (MMloc, 0, 0);
	    }
	  char *yloc = (char *) strchr (decodevaluestring, 'M');
	  if (yloc && (!tloc || tloc > yloc))
	    {
	      *yloc = 0;
	      yloc--;
	      char c = *yloc;
	      while (((c >= '0' && c <= '9') || c == '-' || c == '+')
		     && yloc > decodevaluestring)
		{
		  yloc--;
		  c = *yloc;
		}
	      yloc++;
	      x.years = strtol (yloc, 0, 0);
	    }
#ifdef USE_LIBXML2
	  free (decodevaluestring);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}




CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, CMS_DATE_TIME & x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    return CMS_STATUS_NOT_SET;
  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    return status;
  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:dateTime");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("dateTime"));
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\")\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  return CMS_STATUS_NOT_SET;
	}

      int orig_struct_is_empty = structisempty[class_count];
      if (0 == compareForDiff (name, &x, sizeof (x)))
	return CMS_STATUS_NOT_SET;

      // This is an ugly HACK, w3c says year 0000 is not allowed but
      // since I am lazy and don't initialize this it occurs alot.
      if (x.years == 0)
	{
	  x.years = 1971;
	  x.months = 1;
	  x.days = 16;
	  x.hours = 1;
	  structisempty[class_count] = orig_struct_is_empty;
	  if (0 != base && 0 != diffbuff && 0 != diffbuff_size
	      && mode == CMS_ENCODE_DATA)
	    {
	      return CMS_STATUS_NOT_SET;
	    }
	}
      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), 
	       "%04ld-%02ld-%02ldT%02ld:%02ld:",
	       x.years, x.months, x.days, x.hours, x.minutes);
      if (x.seconds < 10)
	{
	  strcat (valuestring, "0");
	}
      size_t valstring_len = strlen(valuestring);
      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring+valstring_len,sizeof(valuestring)-valstring_len),
				    "%f", x.seconds);
      char *endvaluestringptr = valuestring + strlen (valuestring) - 1;
      if (strchr (valuestring, '.'))
	{
	  while (*endvaluestringptr == '0' && endvaluestringptr > valuestring)
	    {
	      *endvaluestringptr = 0;
	      endvaluestringptr--;
	    }
	  if (*endvaluestringptr == '.' && endvaluestringptr > valuestring)
	    {
	      *endvaluestringptr = 0;
	    }
	}
#if 0
      // Ignoring time zone
      if (0 == x.timezoneoffsethours)
	{
	  strcat (valuestring, "Z");
	}
      else if (x.timezoneoffsethours > 0)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(endvaluestringptr,valuestring+sizeof(valuestring)-endvaluestringptr),
			  "+%02d:00", x.timezoneoffsethours);
	}
      else if (x.timezoneoffsethours < 0)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(endvaluestringptr,valuestring+sizeof(valuestring)-endvaluestringptr),
		   "-%02d:00",
		   (-1 * x.timezoneoffsethours));
	}
#endif

      xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
		       fakeStrdup (valuestring));

      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring_const = findNodeString (name);
      char * decodevaluestring = fakeStrdup(decodevaluestring_const);
      if (0 != decodevaluestring)
	{
	  char *tloc = (char *) strchr (decodevaluestring, 'T');
	  if (tloc)
	    {
	      sscanf (tloc + 1, "%ld:%ld:%lf", &(x.hours), &(x.minutes),
		      &(x.seconds));
	      *tloc = 0;
	      x.timezoneoffsethours = 0;
	      char *minusloc = strchr (tloc + 1, '-');
	      if (minusloc > tloc + 1)
		{
		  if (*(minusloc - 1) != ':')
		    {
		      x.timezoneoffsethours = strtol (minusloc, 0, 10);
		    }
		}
	      char *plusloc = (char *) strchr (tloc + 1, '+');
	      if (plusloc > tloc + 1)
		{
		  if (*(plusloc - 1) != ':')
		    {
		      x.timezoneoffsethours = strtol (plusloc, 0, 10);
		    }
		}
	    }
	  sscanf (decodevaluestring, "%ld-%ld-%ld", &(x.years), &(x.months),
		  &(x.days));
#ifdef USE_LIBXML2
	  free (decodevaluestring_const);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, CMS_TIME & x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    {
      return status;
    }


  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:time");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("time"));
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\")\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%02d:%02d:",
	        x.hours, x.minutes);
      if (x.seconds < 10)
	{
	  strcat (valuestring, "0");
	}
      size_t valstring_len=strlen(valuestring);
      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring+valstring_len,sizeof(valuestring)-valstring_len),
		      "%f", x.seconds);
      char *endvaluestringptr = valuestring + strlen (valuestring) - 1;
      if (strchr (valuestring, '.'))
	{
	  while (*endvaluestringptr == '0' && endvaluestringptr > valuestring)
	    {
	      *endvaluestringptr = 0;
	      endvaluestringptr--;
	    }
	  if (*endvaluestringptr == '.' && endvaluestringptr > valuestring)
	    {
	      *endvaluestringptr = 0;
	    }
	}
#if 0
      // Ignoring time zone
      if (0 == x.timezoneoffsethours)
	{
	  strcat (valuestring, "Z");
	}
      else if (x.timezoneoffsethours > 0)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(endvaluestringptr,valuestring+sizeof(valuestring)-endvaluestringptr),
			  "+%02d:00", x.timezoneoffsethours);
	}
      else if (x.timezoneoffsethours < 0)
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(endvaluestringptr,valuestring+sizeof(valuestring)-endvaluestringptr), 
		   "-%02d:00",
		   (-1 * x.timezoneoffsethours));
	}
#endif

      xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
		       fakeStrdup (valuestring));

      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring_const = findNodeString (name);
      char *decodevaluestring = fakeStrdup(decodevaluestring_const);
      if (0 != decodevaluestring)
	{
	  sscanf (decodevaluestring, "%d:%d:%lf", &(x.hours), &(x.minutes),
		  &(x.seconds));
	  x.timezoneoffsethours = 0;
	  char *minusloc = (char *) strchr (decodevaluestring, '-');
	  if (minusloc > decodevaluestring+1)
	    {
	      if (*(minusloc - 1) != ':')
		{
		  x.timezoneoffsethours = strtol (minusloc, 0, 10);
		}
	    }
	  char *plusloc = (char *) strchr (decodevaluestring+1, '+');
	  if (plusloc > decodevaluestring + 1)
	    {
	      if (*(plusloc - 1) != ':')
		{
		  x.timezoneoffsethours = strtol (plusloc, 0, 10);
		}
	    }
#ifdef USE_LIBXML2
	  free (decodevaluestring_const);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_XML_UPDATER::update_with_name (const char *name, CMS_DATE & x)
{
  int tempint;

  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    {
      return status;
    }

  if (encoding)
    {
      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr) xsdNs,
			     (xmlChar *) "element", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"element\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode))));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:date");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("date"));
	  if (inside_dla[class_count] || inside_unbounded[class_count])
	    {
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			       "xmlSetProp(%s,\"minOccurs\",\"0\")\n",
			       fullName (newNode));
#endif
	      xmlSetProp (newNode, (xmlChar *) "minOccurs", (xmlChar *) "0");
	    }
	  return CMS_STATUS_NOT_SET;
	}

      int orig_struct_is_empty = structisempty[class_count];
      if (0 == compareForDiff (name, &x, sizeof (x)))
	return CMS_STATUS_NOT_SET;

      // This is an ugly HACK, w3c says year 0000 is not allowed but
      // since I am lazy and don't initialize this it occurs alot.
      if (x.years == 0)
	{
	  x.years = 1971;
	  x.months = 1;
	  x.days = 16;
	  structisempty[class_count] = orig_struct_is_empty;
	  if (0 != base && 0 != diffbuff && 0 != diffbuff_size
	      && mode == CMS_ENCODE_DATA)
	    {
	      return CMS_STATUS_NOT_SET;
	    }
	}
      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%04ld-%02ld-%02ld",
	       x.years, x.months, x.days);
      xmlNewTextChild (((xmlNodePtr) currentNode),((xmlNsPtr)normalNs), fakeStrdup (name),
		       fakeStrdup (valuestring));

      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlNewTextChild(%s,0,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring_const = findNodeString (name);
      char *decodevaluestring = fakeStrdup(decodevaluestring_const);
      if (0 != decodevaluestring)
	{
	  sscanf (decodevaluestring, "%ld-%ld-%ld", &(x.years), &(x.months),
		  &(x.days));
	  	  if(x.years < 100 || x.years > 10000 || x.months < 0 || x.months > 12 || x.days < 0 ||
	     x.days > 31)
	    {
	      rcs_print_error("Suspicious date %s: expected format is YYYY-MM-DD\n", decodevaluestring);
	    }
	  if(x.years >= 0 && x.years < 50)
	    {
	      if(x.days > 31)
		{
		  tempint = x.years;
		  x.years = x.days;
		  x.days = tempint;
		}
	      else
		{
		  x.years += 2000;
		}
	    }
	  if(x.years > 50 && x.years < 100)
	    {
	      if(x.days > 31)
		{
		  tempint = x.years;
		  x.years = x.days;
		  x.days = x.months;
		  x.months = tempint;
		}
	      else
		{
		  x.years += 1900;
		}
	    }
#ifdef USE_LIBXML2
	  free (decodevaluestring_const);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					       CMS_DURATION & x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    {
      return status;
    }

  if (encoding)
    {

      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	}
      if (schema_gen_mode)
	{
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:duration");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("duration"));
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	return CMS_STATUS_NOT_SET;

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "P%ldY%ldM%ldDT%ldH%ldM%fS",
	       x.years, x.months, x.days, x.hours, x.minutes, x.seconds);
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));
      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY, "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring_const =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      char *decodevaluestring = fakeStrdup(decodevaluestring_const);
      if (0 != decodevaluestring)
	{
	  char *sloc = (char *) strchr (decodevaluestring, 'S');
	  if (sloc)
	    {
	      *sloc = 0;
	      sloc--;
	      char c = *sloc;
	      while (((c >= '0' && c <= '9') || c == '.' || c == '-'
		      || c == 'E' || c == '+') && sloc > decodevaluestring)
		{
		  sloc--;
		  c = *sloc;
		}
	      sloc++;
#ifdef HAVE_STRTOF
	      x.seconds = strtof (sloc, 0);
#else
	      x.seconds = strtod (sloc, 0);
#endif
	    }
	  char *tloc = (char *) strchr (decodevaluestring, 'T');
	  if (tloc)
	    {
	      char *mloc = (char *) strchr (tloc + 1, 'M');
	      if (mloc && mloc > tloc)
		{
		  *mloc = 0;
		  mloc--;
		  char c = *mloc;
		  while (((c >= '0' && c <= '9') || c == '-' || c == '+')
			 && mloc > decodevaluestring)
		    {
		      mloc--;
		      c = *mloc;
		    }
		  mloc++;
		  x.minutes = strtol (mloc, 0, 0);
		}
	    }
	  char *hloc = (char *) strchr (decodevaluestring, 'H');
	  if (hloc)
	    {
	      *hloc = 0;
	      hloc--;
	      char c = *hloc;
	      while (((c >= '0' && c <= '9') || c == '-' || c == '+')
		     && hloc > decodevaluestring)
		{
		  hloc--;
		  c = *hloc;
		}
	      hloc++;
	      x.hours = strtol (hloc, 0, 0);
	    }
	  char *dloc = (char *) strchr (decodevaluestring, 'D');
	  if (dloc)
	    {
	      *dloc = 0;
	      dloc--;
	      char c = *dloc;
	      while (((c >= '0' && c <= '9') || c == '-' || c == '+')
		     && dloc > decodevaluestring)
		{
		  dloc--;
		  c = *dloc;
		}
	      dloc++;
	      x.days = strtol (dloc, 0, 0);
	    }

	  char *MMloc = (char *) strchr (decodevaluestring, 'Y');
	  if (MMloc && (!tloc || tloc > MMloc))
	    {
	      *MMloc = 0;
	      MMloc--;
	      char c = *MMloc;
	      while (((c >= '0' && c <= '9') || c == '-' || c == '+')
		     && MMloc > decodevaluestring)
		{
		  MMloc--;
		  c = *MMloc;
		}
	      MMloc++;
	      x.months = strtol (MMloc, 0, 0);
	    }
	  char *yloc = (char *) strchr (decodevaluestring, 'M');
	  if (yloc && (!tloc || tloc > yloc))
	    {
	      *yloc = 0;
	      yloc--;
	      char c = *yloc;
	      while (((c >= '0' && c <= '9') || c == '-' || c == '+')
		     && yloc > decodevaluestring)
		{
		  yloc--;
		  c = *yloc;
		}
	      yloc++;
	      x.years = strtol (yloc, 0, 0);
	    }


#ifdef USE_LIBXML2
	  free (decodevaluestring_const);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}





CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					       CMS_TIME & x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    {
      return status;
    }

  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:time");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("time"));
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	{
	  return CMS_STATUS_NOT_SET;
	}

      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%02d:%02d:",
	       x.hours, x.minutes);
      if (x.seconds < 10)
	{
	  strcat (valuestring, "0");
	}
      size_t valstring_len = strlen(valuestring);
      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring+valstring_len,sizeof(valuestring)-valstring_len),
	       "%f", x.seconds);
      if (strchr (valuestring, '.'))
	{
	  char *endvaluestringptr = valuestring + strlen (valuestring) - 1;
	  while (*endvaluestringptr == '0' && endvaluestringptr > valuestring)
	    {
	      *endvaluestringptr = 0;
	      endvaluestringptr--;
	    }
	  if (*endvaluestringptr == '.' && endvaluestringptr > valuestring)
	    {
	      *endvaluestringptr = 0;
	    }
	}
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));

      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring_const =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if (0 != decodevaluestring_const)
	{

	  sscanf (decodevaluestring_const, "%d:%d:%lf", &(x.hours), &(x.minutes),
		  &(x.seconds));

#ifdef USE_LIBXML2
	  free (decodevaluestring_const);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					       CMS_DATE & x)
{
  int tempint;

  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    {
      return status;
    }

  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:date");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("date"));
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	return CMS_STATUS_NOT_SET;

      // This is an ugly HACK, w3c says year 0000 is not allowed but
      // since I am lazy and don't initialize properly this occurs alot.
      if (x.years == 0)
	{
	  // This date is my birthday (WPS) 
	  x.years = 1971;
	  x.months = 1;
	  x.days = 16;
	}
      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%04ld-%02ld-%02ld",
	       x.years, x.months, x.days);
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));

      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif


    }
  else
    {
      const char *decodevaluestring_const =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      if (0 != decodevaluestring_const)
	{
	  sscanf (decodevaluestring_const, "%ld-%ld-%ld", &(x.years), &(x.months),
		  &(x.days));
	  if(x.years < 100 || x.years > 10000 || x.months < 0 || x.months > 12 || x.days < 0 ||
	     x.days > 31)
	    {
	      rcs_print_error("Suspicious date %s: expected format is YYYY-MM-DD\n", decodevaluestring_const);
	    }
	  if(x.years >= 0 && x.years < 50)
	    {
	      if(x.days > 31)
		{
		  tempint = x.years;
		  x.years = x.days;
		  x.days = tempint;
		}
	      else
		{
		  x.years += 2000;
		}
	    }
	  if(x.years > 50 && x.years < 100)
	    {
	      if(x.days > 31)
		{
		  tempint = x.years;
		  x.years = x.days;
		  x.days = x.months;
		  x.months = tempint;
		}
	      else
		{
		  x.years += 1900;
		}
	    }
#ifdef USE_LIBXML2
	  free (decodevaluestring_const);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_XML_UPDATER::update_dla_length_with_name (const char *name, int &len)
{
  if (!add_array_indexes_to_name)
    {
      return CMS_STATUS_NOT_SET;
    }
  return update_with_name (name, len);
}


CMS_STATUS
CMS_XML_UPDATER::update_attribute_with_name (const char *name,
					       CMS_DATE_TIME & x)
{
  if (0 == ((xmlNodePtr) currentNode) || inside_classvar_not_found > 0)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (check_pointer_with_name (name, (char *) &x, sizeof (x)) < 0)
    {
      return status;
    }


  if (encoding)
    {

      int jj = 0;
      if (schema_gen_mode)
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode)->parent,
			     fakeStrdup (whitespace));
	  xmlNodePtr newNode =
	    xmlNewTextChild (((xmlNodePtr) currentNode)->parent, (xmlNsPtr) xsdNs,
			     (xmlChar *) "attribute", 0);
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNewTextChild(%s,\"xsd\",\"attribute\",0);\
n",
			   ((char *) fullName (((xmlNodePtr) currentNode)->parent)));
	  
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"name\",%s))\n",
			   fullName (newNode), name);
#endif
	  xmlSetProp (newNode, (xmlChar *) "name", fakeStrdup (name));
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlSetProp(%s,\"type\",%s))\n",
			   fullName (newNode), "xsd:dateTime");
#endif
	  xmlSetProp (newNode, (xmlChar *) "type", XSDTYPE("dateTime"));
	  return CMS_STATUS_NOT_SET;
	}

      if (0 == compareAttributeForDiff (name, &x, sizeof (x)))
	return CMS_STATUS_NOT_SET;

      // This is an ugly HACK, w3c says year 0000 is not allowed but
      // since I am lazy and don't initialize properly this occurs alot.
      if (x.years == 0)
	{
	  // This date is my birthday (WPS) 
	  x.years = 1971;
	  x.months = 1;
	  x.days = 16;
	  x.hours = 1;
	}
      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring,sizeof(valuestring)), "%04ld-%02ld-%02ldT%02ld:%02ld:",
	       x.years, x.months, x.days, x.hours, x.minutes);
      if (x.seconds < 10)
	{
	  strcat (valuestring, "0");
	}
      size_t valstring_len = strlen(valuestring);
      SNPRINTF_FUNC ( SNPRINTF_ARGS(valuestring+valstring_len,sizeof(valuestring)-valstring_len),
		      "%f", x.seconds);
      if (strchr (valuestring, '.'))
	{
	  char *endvaluestringptr = valuestring + strlen (valuestring) - 1;
	  while (*endvaluestringptr == '0' && endvaluestringptr > valuestring)
	    {
	      *endvaluestringptr = 0;
	      endvaluestringptr--;
	    }
	  if (*endvaluestringptr == '.' && endvaluestringptr > valuestring)
	    {
	      *endvaluestringptr = 0;
	    }
	}
      xmlSetProp (((xmlNodePtr) currentNode), fakeStrdup (name),
		  fakeStrdup (valuestring));

      
#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "xmlSetProp(%s,%s,%s);\n",
		       ((char *) fullName (((xmlNodePtr) currentNode))), name,
		       valuestring);
#endif
    }
  else
    {
      const char *decodevaluestring_const  =
	xmlGetProp (((xmlNodePtr) currentNode), (xmlChar *) name);
      char *decodevaluestring = fakeStrdup(decodevaluestring_const);
      if (0 != decodevaluestring)
	{
	  char *tloc = (char *) strchr (decodevaluestring, 'T');
	  if (tloc)
	    {
	      sscanf (tloc + 1, "%ld:%ld:%lf", &(x.hours), &(x.minutes),
		      &(x.seconds));
	      *tloc = 0;
	    }
	  sscanf (decodevaluestring, "%ld-%ld-%ld", &(x.years), &(x.months),
		  &(x.days));
#ifdef USE_LIBXML2
	  free (decodevaluestring_const);
#endif
	}
    }
  return CMS_STATUS_NOT_SET;
}


void
CMS_XML_UPDATER::decode_len (const char *name, int &len)
{
  if (no_unbounded || status == CMS_UPDATE_ERROR)
    return;

  len = countNodes (name);
  
#ifdef DEBUG_THIS_FILE
  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		   "CMS_XML_UPDATER::decode_len(%s,%d)\n", name, len);
#endif
}

CMS_STATUS
  CMS_XML_UPDATER::update_unbounded_attribute_with_name (const char *name, char **x,
					       int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR || x == 0
      || (len <= 0 && encoding && !schema_gen_mode))
    {
      return CMS_STATUS_NOT_SET;
    }

  int orig_pointer_check_disabled = pointer_check_disabled;
  void *orig_base = base;
  void *orig_diffbuff = diffbuff;
  size_t orig_diffbuff_size = diffbuff_size;
  pointer_check_disabled = 1;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (encoding && !schema_gen_mode)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      if (!schema_gen_mode)
	{
	  const xmlChar *decodestr = xmlGetProp ((xmlNodePtr)currentNode,(xmlChar*)name);
	  if (decodestr)
	    {
	      len = (int) strlen ((const char *)decodestr) + 1;
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "CMS_XML_UPDATER::update_unbounded_with_name name=%s, decodstr=%s, len=%d\n",
			   name, decodestr, len);
#endif
	}
      else
	{
	  len = 1;
	}
      if(len > 0)
	{
	  if (*x == 0 || check_type_changed || schema_gen_mode || size_allocated == 0)
	    {
	      *x = (char *) malloc (len * sizeof (**x));
	      memset((char*)(*x),0,(len*sizeof(**x)));
	      size_allocated = len;
	      add_uba((void **)x);
	    }
	  if (*x != 0)
	    {
	      if (len > size_allocated)
		{
		  remove_uba((void **)x);
		  *x = (char *) realloc (*x, len * sizeof (**x));
		  memset(((char*)(*x))+size_allocated*sizeof(**x),
			 0,(len-size_allocated)*sizeof(**x));
		  size_allocated = len;
		  add_uba((void **)x);
		}
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      bool orig_add_array_indexes_to_name = add_array_indexes_to_name;
      add_array_indexes_to_name = false;
      inside_unbounded[class_count] = 1;
      long offset = (long) (((char *) x) - ((char *) base));
      if (diffbuff != 0 && base != 0 && offset < (int) diffbuff_size)
	{
	  char *newdiffbuffptraddr = (char *) diffbuff;
	  newdiffbuffptraddr += offset;
	  void **vnewdiffbuffptraddr = (void **) newdiffbuffptraddr;
	  diffbuff = (void *) (*vnewdiffbuffptraddr);
	  base = *x;
	  diffbuff_size = sizeof (*x) * size_allocated;
	}
      retval = update_attribute_with_name (name, *x, len);
      inside_unbounded[class_count] = 0;
      base = orig_base;
      diffbuff = orig_diffbuff;
      diffbuff_size = orig_diffbuff_size;
      add_array_indexes_to_name = orig_add_array_indexes_to_name;
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}


CMS_STATUS
CMS_XML_UPDATER::update_unbounded_with_name (const char *name, char **x,
					     int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR)
    {
      return CMS_STATUS_NOT_SET;
    }
  last_var_was_struct=0;

  if(len == 0 && !schema_gen_mode && encoding && currentNode && name && *name
     && (0 == diffbuff || 0 == base))
    {
      int jj = 0;
      if ((make_xml_pretty && !content_set_for_this_class))
	{
	  memset (whitespace, 0, 50);
	  whitespace[0] = '\n';
	  for (jj = 1;
	       jj <
	       (schema_gen_mode ? elementtablevel[class_count] : class_count +
		1) && jj < 49; jj++)
	    {
	      whitespace[jj] = '\t';
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "xmlNodeAddContent(%s,(xmlChar*)%s);\n",
			   fullName (((xmlNodePtr) currentNode)), whitespace);
#endif
	  xmlNodeAddContent (((xmlNodePtr) currentNode),
			     fakeStrdup (whitespace));
	}
       xmlNewTextChild (((xmlNodePtr) currentNode), (xmlNsPtr)0,
			     (xmlChar *) name, 0);
       
#ifdef DEBUG_THIS_FILE
       rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			"xmlNewTextChild(%s,0,%s,0);\n",
			((char *) fullName (((xmlNodePtr) currentNode))),
			name);
#endif
       return CMS_STATUS_NOT_SET;
    }

  
  int orig_pointer_check_disabled = pointer_check_disabled;
  void *orig_base = base;
  void *orig_diffbuff = diffbuff;
  size_t orig_diffbuff_size = diffbuff_size;
  pointer_check_disabled = 1;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (encoding && !schema_gen_mode)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      if (!schema_gen_mode)
	{
	  const char *decodestr;
	  decodestr=0;
	  if(*name == 0)
	    {
	      decodestr = xmlNodeListGetString((xmlDocPtr)doc,
						   (xmlNodePtr)currentNode,1);
	    }
	  else
	    {
	      decodestr = findNodeString (name);
	    }
	  if (decodestr)
	    {
	      len = (int) strlen (decodestr) + 1;
	    }
	  
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "CMS_XML_UPDATER::update_unbounded_with_name name=%s, decodstr=%s, len=%d\n",
			   name, decodestr, len);
#endif
	}
      else
	{
	  len = 1;
	}
      if(len > 0)
	{
	  if (*x == 0 || check_type_changed || schema_gen_mode || size_allocated == 0 )
	    {
	      *x = (char *) malloc (len * sizeof (**x));
	      memset(*x,0,len*sizeof(**x));
	      size_allocated = len;
	      add_uba((void **)x);
	    }
	  if (*x != 0)
	    {
	      if (len > size_allocated)
		{
		  remove_uba((void **)x);
		  *x = (char *) realloc (*x, len * sizeof (**x));
		  memset(((char*)(*x))+size_allocated*sizeof(**x),
			 0,(len-size_allocated)*sizeof(**x));
		  size_allocated = len;
		  add_uba((void **)x);
		}
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      bool orig_add_array_indexes_to_name = add_array_indexes_to_name;
      add_array_indexes_to_name = false;
      inside_unbounded[class_count] = 1;
      long offset = (long)(((char *) x) - ((char *) base));
      if (diffbuff != 0 && base != 0 && offset < (int) diffbuff_size)
	{
	  char *newdiffbuffptraddr = (char *) diffbuff;
	  newdiffbuffptraddr += offset;
	  void **vnewdiffbuffptraddr = (void **) newdiffbuffptraddr;
	  diffbuff = (void *) (*vnewdiffbuffptraddr);
	  base = *x;
	  diffbuff_size = sizeof (*x) * size_allocated;
	}
      retval = update_with_name (name, *x, len);
      inside_unbounded[class_count] = 0;
      base = orig_base;
      diffbuff = orig_diffbuff;
      diffbuff_size = orig_diffbuff_size;
      add_array_indexes_to_name = orig_add_array_indexes_to_name;
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
CMS_XML_UPDATER::update_unbounded_with_name (const char *name,
					     unsigned char **x, int &len,
					     int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR || x == 0
      || (len <= 0 && encoding && !schema_gen_mode))
    {
      return CMS_STATUS_NOT_SET;
    }

  last_var_was_struct=0;

  int orig_pointer_check_disabled = pointer_check_disabled;
  void *orig_base = base;
  void *orig_diffbuff = diffbuff;
  size_t orig_diffbuff_size = diffbuff_size;
  pointer_check_disabled = 1;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (encoding && !schema_gen_mode)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      if (!schema_gen_mode)
	{
	  const char *decodestr = findNodeString (name);
	  if (decodestr)
	    {
	      len = (int) (strlen (decodestr)/2);
	    }
	  
#ifdef DEBUG_THIS_FILE     
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "CMS_XML_UPDATER::update_unbounded_with_name name=%s, decodstr=%s, len=%d\n",
			   name, decodestr, len);
#endif
	}
      else
	{
	  len = 1;
	}
      if(len > 0)
	{
	  if (*x == 0 || check_type_changed || schema_gen_mode || size_allocated == 0)
	    {
	      *x = (unsigned char *) malloc (len * sizeof (**x)); 
	      memset(*x,0,len*sizeof(**x));
	      size_allocated = len;
	      add_uba((void **)x);
	    }
	  if (*x != 0)
	    {
	      if (len > size_allocated)
		{
		  remove_uba((void **)x);
		  *x = (unsigned char *) realloc (*x, len * sizeof (**x));
		  memset(((char*)(*x))+size_allocated*sizeof(**x),
			 0,(len-size_allocated)*sizeof(**x));
		  size_allocated = len;
		  add_uba((void **)x);
		}
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      bool orig_add_array_indexes_to_name = add_array_indexes_to_name;
      add_array_indexes_to_name = false;
      inside_unbounded[class_count] = 1;
      long offset = (long) (((char *) x) - ((char *) base));
      if (diffbuff != 0 && base != 0 && offset < (int) diffbuff_size)
	{
	  char *newdiffbuffptraddr = (char *) diffbuff;
	  newdiffbuffptraddr += offset;
	  void **vnewdiffbuffptraddr = (void **) newdiffbuffptraddr;
	  diffbuff = (void *) (*vnewdiffbuffptraddr);
	  base = *x;
	  diffbuff_size = sizeof (*x) * size_allocated;
	}
      retval = update_with_name (name, *x, len);
      inside_unbounded[class_count] = 0;
      base = orig_base;
      diffbuff = orig_diffbuff;
      diffbuff_size = orig_diffbuff_size;
      add_array_indexes_to_name = orig_add_array_indexes_to_name;
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
CMS_XML_UPDATER::update_unbounded_with_name (const char *name, short **x,
					     int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR || x == 0
      || (len <= 0 && encoding && !schema_gen_mode))
    return CMS_STATUS_NOT_SET;

  int orig_pointer_check_disabled = pointer_check_disabled;
  void *orig_base = base;
  void *orig_diffbuff = diffbuff;
  size_t orig_diffbuff_size = diffbuff_size;

  last_var_was_struct=0;
  pointer_check_disabled = 1;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (encoding && !schema_gen_mode)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      if (!schema_gen_mode)
	{
	  decode_len (name, len);
	}
      else
	{
	  len = 1;
	}
      if(len > 0)
	{
	  if (*x == 0 || check_type_changed || schema_gen_mode || size_allocated == 0)
	    {
	      *x = (short *) malloc (len * sizeof (**x));
	      memset(*x,0,len*sizeof(**x));
	      size_allocated = len;
	      add_uba((void **)x);
	    }
	  if (*x != 0)
	    {
	      if (len > size_allocated)
		{
		  remove_uba((void **)x);
		  *x = (short *) realloc (*x, len * sizeof (**x));
		  memset(((char*)(*x))+size_allocated*sizeof(**x),
			 0,(len-size_allocated)*sizeof(**x));
		  size_allocated = len;
		  add_uba((void **)x);
		}
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      bool orig_add_array_indexes_to_name = add_array_indexes_to_name;
      add_array_indexes_to_name = false;
      inside_unbounded[class_count] = 1;
      long offset = (long) (((char *) x) - ((char *) base));
      if (diffbuff != 0 && base != 0 && offset < (int) diffbuff_size)
	{
	  char *newdiffbuffptraddr = (char *) diffbuff;
	  newdiffbuffptraddr += offset;
	  void **vnewdiffbuffptraddr = (void **) newdiffbuffptraddr;
	  diffbuff = (void *) (*vnewdiffbuffptraddr);
	  base = *x;
	  diffbuff_size = sizeof (*x) * size_allocated;
	}
      retval = update_with_name (name, *x, len);
      inside_unbounded[class_count] = 0;
      base = orig_base;
      diffbuff = orig_diffbuff;
      diffbuff_size = orig_diffbuff_size;
      add_array_indexes_to_name = orig_add_array_indexes_to_name;
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
CMS_XML_UPDATER::update_unbounded_with_name (const char *name,
					     unsigned short **x, int &len,
					     int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR || x == 0
      || (len <= 0 && encoding && !schema_gen_mode))
    return CMS_STATUS_NOT_SET;

  int orig_pointer_check_disabled = pointer_check_disabled;
  void *orig_base = base;
  void *orig_diffbuff = diffbuff;
  size_t orig_diffbuff_size = diffbuff_size;

  last_var_was_struct=0;
  pointer_check_disabled = 1;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (encoding && !schema_gen_mode)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      if (!schema_gen_mode)
	{
	  decode_len (name, len);
	}
      else
	{
	  len = 1;
	}
      if(len > 0)
	{
	  if (*x == 0 || check_type_changed || schema_gen_mode || size_allocated == 0)
	    {
	      *x = (unsigned short *) malloc (len * sizeof (**x));
	      memset((char*)(*x),0,len*sizeof(**x)); 
	      size_allocated = len;
	      add_uba((void **)x);
	    }
	  if (*x != 0)
	    {
	      if (len > size_allocated)
		{
		  remove_uba((void **)x);
		  *x = (unsigned short *) realloc (*x, len * sizeof (**x));
		  memset(((char*)(*x))+size_allocated*sizeof(**x),
		     0,(len-size_allocated)*sizeof(**x));
		  size_allocated = len;
		  add_uba((void **)x);
		}
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      bool orig_add_array_indexes_to_name = add_array_indexes_to_name;
      add_array_indexes_to_name = false;
      inside_unbounded[class_count] = 1;
      long offset = (long) (((char *) x) - ((char *) base));
      if (diffbuff != 0 && base != 0 && offset < (int) diffbuff_size)
	{
	  char *newdiffbuffptraddr = (char *) diffbuff;
	  newdiffbuffptraddr += offset;
	  void **vnewdiffbuffptraddr = (void **) newdiffbuffptraddr;
	  diffbuff = (void *) (*vnewdiffbuffptraddr);
	  base = *x;
	  diffbuff_size = sizeof (*x) * size_allocated;
	}
      retval = update_with_name (name, *x, len);
      inside_unbounded[class_count] = 0;
      base = orig_base;
      diffbuff = orig_diffbuff;
      diffbuff_size = orig_diffbuff_size;
      add_array_indexes_to_name = orig_add_array_indexes_to_name;
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
CMS_XML_UPDATER::update_unbounded_with_name (const char *name, int **x,
					     int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR || x == 0
      || (len <= 0 && encoding && !schema_gen_mode))
    return CMS_STATUS_NOT_SET;

  int orig_pointer_check_disabled = pointer_check_disabled;
  void *orig_base = base;
  void *orig_diffbuff = diffbuff;
  size_t orig_diffbuff_size = diffbuff_size;
  pointer_check_disabled = 1;
  last_var_was_struct=0;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (encoding && !schema_gen_mode)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      if (!schema_gen_mode)
	{
	  decode_len (name, len);
	}
      else
	{
	  len = 1;
	}
      if(len > 0)
	{
	  if (*x == 0 || check_type_changed || schema_gen_mode || size_allocated == 0)
	    {
	      *x = (int *) malloc (len * sizeof (**x));
	      memset(*x,0,len * sizeof(**x));
	      size_allocated = len;
	      add_uba((void **)x);
	    }
	  if (*x != 0)
	    {
	      if (len > size_allocated)
		{
		  remove_uba((void **)x);
		  *x = (int *) realloc (*x, len * sizeof (**x));
		  memset(((char*)(*x))+size_allocated*sizeof(**x),
			 0,(len-size_allocated)*sizeof(**x));	      
		  size_allocated = len;
		  add_uba((void **)x);
		}
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      bool orig_add_array_indexes_to_name = add_array_indexes_to_name;
      add_array_indexes_to_name = false;
      inside_unbounded[class_count] = 1;
      long offset = (long) (((char *) x) - ((char *) base));
      if (diffbuff != 0 && base != 0 && offset < (int) diffbuff_size)
	{
	  char *newdiffbuffptraddr = (char *) diffbuff;
	  newdiffbuffptraddr += offset;
	  void **vnewdiffbuffptraddr = (void **) newdiffbuffptraddr;
	  diffbuff = (void *) (*vnewdiffbuffptraddr);
	  base = *x;
	  diffbuff_size = sizeof (*x) * size_allocated;
	}
      retval = update_with_name (name, *x, len);
      inside_unbounded[class_count] = 0;
      base = orig_base;
      diffbuff = orig_diffbuff;
      diffbuff_size = orig_diffbuff_size;
      add_array_indexes_to_name = orig_add_array_indexes_to_name;
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
CMS_XML_UPDATER::update_unbounded_with_name (const char *name,
					     unsigned int **x, int &len,
					     int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR || x == 0
      || (len <= 0 && encoding && !schema_gen_mode))
    return CMS_STATUS_NOT_SET;

  int orig_pointer_check_disabled = pointer_check_disabled;
  void *orig_base = base;
  void *orig_diffbuff = diffbuff;
  size_t orig_diffbuff_size = diffbuff_size;

  last_var_was_struct=0;
  pointer_check_disabled = 1;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;

  if (encoding && !schema_gen_mode)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      if (!schema_gen_mode)
	{
	  decode_len (name, len);
	}
      else
	{
	  len = 1;
	}
      if(len > 0)
	{
	  if (*x == 0 || check_type_changed || schema_gen_mode || size_allocated == 0)
	    {
	      *x = (unsigned int *) malloc (len * sizeof (**x));
	      memset(*x,0,len*sizeof(**x));
	      size_allocated = len;
	      add_uba((void **)x);
	    }
	  if (*x != 0)
	    {
	      if (len > size_allocated)
		{
		  remove_uba((void **)x);
		  *x = (unsigned int *) realloc (*x, len * sizeof (**x));
		  memset(((char*)(*x))+size_allocated*sizeof(**x),
			 0,(len-size_allocated)*sizeof(**x));
		  size_allocated = len;
		  add_uba((void **)x);
		}
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      bool orig_add_array_indexes_to_name = add_array_indexes_to_name;
      add_array_indexes_to_name = false;
      inside_unbounded[class_count] = 1;
      long offset = (long) (((char *) x) - ((char *) base));
      if (diffbuff != 0 && base != 0 && offset < (int) diffbuff_size)
	{
	  char *newdiffbuffptraddr = (char *) diffbuff;
	  newdiffbuffptraddr += offset;
	  void **vnewdiffbuffptraddr = (void **) newdiffbuffptraddr;
	  diffbuff = (void *) (*vnewdiffbuffptraddr);
	  base = *x;
	  diffbuff_size = sizeof (*x) * size_allocated;
	}
      retval = update_with_name (name, *x, len);
      inside_unbounded[class_count] = 0;
      base = orig_base;
      diffbuff = orig_diffbuff;
      diffbuff_size = orig_diffbuff_size;
      add_array_indexes_to_name = orig_add_array_indexes_to_name;
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
CMS_XML_UPDATER::update_unbounded_with_name (const char *name, long **x,
					     int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR || x == 0
      || (len <= 0 && encoding && !schema_gen_mode))
    return CMS_STATUS_NOT_SET;

  int orig_pointer_check_disabled = pointer_check_disabled;
  void *orig_base = base;
  void *orig_diffbuff = diffbuff;
  size_t orig_diffbuff_size = diffbuff_size;

  last_var_was_struct=0;
  pointer_check_disabled = 1;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (encoding && !schema_gen_mode)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      if (!schema_gen_mode)
	{
	  decode_len (name, len);
	}
      else
	{
	  len = 1;
	}
      if(len > 0)
	{
	  if (*x == 0 || check_type_changed || schema_gen_mode || size_allocated == 0)
	    {
	      *x = (long *) malloc (len * sizeof (**x));
	      memset(*x,0,len*sizeof(**x));
	      size_allocated = len;
	      add_uba((void **)x);
	    }
	  if (*x != 0)
	    {
	      if (len > size_allocated)
		{
		  remove_uba((void **)x);
		  *x = (long *) realloc (*x, len * sizeof (**x));
		  memset(((char*)(*x))+size_allocated*sizeof(**x),
			 0,(len-size_allocated)*sizeof(**x));
		  size_allocated = len;
		  add_uba((void **)x);
		}
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      bool orig_add_array_indexes_to_name = add_array_indexes_to_name;
      add_array_indexes_to_name = false;
      inside_unbounded[class_count] = 1;
      long offset = (long) (((char *) x) - ((char *) base));
      if (diffbuff != 0 && base != 0 && offset < (int) diffbuff_size)
	{
	  char *newdiffbuffptraddr = (char *) diffbuff;
	  newdiffbuffptraddr += offset;
	  void **vnewdiffbuffptraddr = (void **) newdiffbuffptraddr;
	  diffbuff = (void *) (*vnewdiffbuffptraddr);
	  base = *x;
	  diffbuff_size = sizeof (*x) * size_allocated;
	}
      retval = update_with_name (name, *x, len);
      inside_unbounded[class_count] = 0;
      base = orig_base;
      diffbuff = orig_diffbuff;
      diffbuff_size = orig_diffbuff_size;
      add_array_indexes_to_name = orig_add_array_indexes_to_name;
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
CMS_XML_UPDATER::update_unbounded_with_name (const char *name,
					     unsigned long **x, int &len,
					     int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR || x == 0
      || (len <= 0 && encoding && !schema_gen_mode))
    return CMS_STATUS_NOT_SET;

  int orig_pointer_check_disabled = pointer_check_disabled;
  void *orig_base = base;
  void *orig_diffbuff = diffbuff;
  size_t orig_diffbuff_size = diffbuff_size;
  last_var_was_struct=0;
  pointer_check_disabled = 1;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (encoding && !schema_gen_mode)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      if (!schema_gen_mode)
	{
	  decode_len (name, len);
	}
      else
	{
	  len = 1;
	}
      if(len > 0)
	{
	  if (*x == 0 || check_type_changed || schema_gen_mode || size_allocated == 0)
	    {
	      *x = (unsigned long *) malloc (len * sizeof (**x));
	      memset((char*)(*x),0,len*sizeof(**x));
	      size_allocated = len;
	      add_uba((void **)x);
	    }
	  if (*x != 0)
	    {
	      if (len > size_allocated)
		{
		  remove_uba((void **)x);
		  *x = (unsigned long *) realloc (*x, len * sizeof (**x));
		  memset(((char*)(*x))+size_allocated*sizeof(**x),
			 0,(len-size_allocated)*sizeof(**x));
		  size_allocated = len;
		  add_uba((void **)x);
		}
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      bool orig_add_array_indexes_to_name = add_array_indexes_to_name;
      add_array_indexes_to_name = false;
      inside_unbounded[class_count] = 1;
      long offset = (long)(((char *) x) - ((char *) base));
      if (diffbuff != 0 && base != 0 && offset < (int) diffbuff_size)
	{
	  char *newdiffbuffptraddr = (char *) diffbuff;
	  newdiffbuffptraddr += offset;
	  void **vnewdiffbuffptraddr = (void **) newdiffbuffptraddr;
	  diffbuff = (void *) (*vnewdiffbuffptraddr);
	  base = *x;
	  diffbuff_size = sizeof (*x) * size_allocated;
	}
      retval = update_with_name (name, *x, len);
      inside_unbounded[class_count] = 0;
      base = orig_base;
      diffbuff = orig_diffbuff;
      diffbuff_size = orig_diffbuff_size;
      add_array_indexes_to_name = orig_add_array_indexes_to_name;
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
CMS_XML_UPDATER::update_unbounded_with_name (const char *name, float **x,
					     int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR || x == 0
      || (len <= 0 && encoding && !schema_gen_mode))
    return CMS_STATUS_NOT_SET;

  int orig_pointer_check_disabled = pointer_check_disabled;
  void *orig_base = base;
  void *orig_diffbuff = diffbuff;
  size_t orig_diffbuff_size = diffbuff_size;

  pointer_check_disabled = 1;
  last_var_was_struct=0;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (encoding && !schema_gen_mode)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      if (!schema_gen_mode)
	{
	  decode_len (name, len);
	}
      else
	{
	  len = 1;
	}
      if(len > 0)
	{
	  if (*x == 0 || check_type_changed || schema_gen_mode || size_allocated == 0)
	    {
	      *x = (float *) malloc (len * sizeof (**x));
	      memset((char*)(*x),0,len*sizeof(**x));
	      size_allocated = len;
	      add_uba((void **)x);
	    }
	  if (*x != 0)
	    {
	      if (len > size_allocated)
		{
		  remove_uba((void **)x);
		  *x = (float *) realloc (*x, len * sizeof (**x));
		  memset(((char*)(*x))+size_allocated*sizeof(**x),
			 0,(len-size_allocated)*sizeof(**x));
		  size_allocated = len;
		  add_uba((void **)x);
		}
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      bool orig_add_array_indexes_to_name = add_array_indexes_to_name;
      add_array_indexes_to_name = false;
      inside_unbounded[class_count] = 1;
      long offset = (long) (((char *) x) - ((char *) base));
      if (diffbuff != 0 && base != 0 && offset < (int) diffbuff_size)
	{
	  char *newdiffbuffptraddr = (char *) diffbuff;
	  newdiffbuffptraddr += offset;
	  void **vnewdiffbuffptraddr = (void **) newdiffbuffptraddr;
	  diffbuff = (void *) (*vnewdiffbuffptraddr);
	  base = *x;
	  diffbuff_size = sizeof (*x) * size_allocated;
	}
      retval = update_with_name (name, *x, len);
      inside_unbounded[class_count] = 0;
      base = orig_base;
      diffbuff = orig_diffbuff;
      diffbuff_size = orig_diffbuff_size;
      add_array_indexes_to_name = orig_add_array_indexes_to_name;
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
CMS_XML_UPDATER::update_unbounded_with_name (const char *name, double **x,
					     int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR || x == 0
      || (len <= 0 && encoding && !schema_gen_mode))
    return CMS_STATUS_NOT_SET;

  int orig_pointer_check_disabled = pointer_check_disabled;
  void *orig_base = base;
  void *orig_diffbuff = diffbuff;
  size_t orig_diffbuff_size = diffbuff_size;

  last_var_was_struct=0;
  pointer_check_disabled = 1;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (encoding && !schema_gen_mode)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      if (!schema_gen_mode)
	{
	  decode_len (name, len);
	}
      else
	{
	  len = 1;
	}
      if(len > 0)
	{
	  if (*x == 0 || check_type_changed || schema_gen_mode || size_allocated == 0)
	    {
	      *x = (double *) malloc (len * sizeof (**x));
	      memset(*x,0,len*sizeof(**x));
	      size_allocated = len;
	      add_uba((void **)x);
	    }
	  if (*x != 0)
	    {
	      if (len > size_allocated)
		{
		  remove_uba((void **)x);
		  *x = (double *) realloc (*x, len * sizeof (**x));
		  memset(((char*)(*x))+size_allocated*sizeof(**x),
			 0,(len-size_allocated)*sizeof(**x));
		  size_allocated = len;
		  add_uba((void **)x);
		}
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      bool orig_add_array_indexes_to_name = add_array_indexes_to_name;
      add_array_indexes_to_name = false;
      inside_unbounded[class_count] = 1;
      long offset = (long) (((char *) x) - ((char *) base));
      if (diffbuff != 0 && base != 0 && offset < (int) diffbuff_size)
	{
	  char *newdiffbuffptraddr = (char *) diffbuff;
	  newdiffbuffptraddr += offset;
	  void **vnewdiffbuffptraddr = (void **) newdiffbuffptraddr;
	  diffbuff = (void *) (*vnewdiffbuffptraddr);
	  base = *x;
	  diffbuff_size = sizeof (*x) * size_allocated;
	}
      retval = update_with_name (name, *x, len);
      inside_unbounded[class_count] = 0;
      base = orig_base;
      diffbuff = orig_diffbuff;
      diffbuff_size = orig_diffbuff_size;
      add_array_indexes_to_name = orig_add_array_indexes_to_name;
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
CMS_XML_UPDATER::update_unbounded_with_name (const char *name,
					     long double **x, int &len,
					     int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR || x == 0
      || (len <= 0 && encoding && !schema_gen_mode))
    return CMS_STATUS_NOT_SET;

  int orig_pointer_check_disabled = pointer_check_disabled;
  void *orig_base = base;
  void *orig_diffbuff = diffbuff;
  size_t orig_diffbuff_size = diffbuff_size;

  last_var_was_struct=0;
  pointer_check_disabled = 1;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  if (encoding && !schema_gen_mode)
    {
      if (*x == 0 || check_type_changed || schema_gen_mode)
	{
	  size_allocated = 0;
	}
      if (len > size_allocated)
	{
	  len = size_allocated;
	}
    }
  else
    {
      if (!schema_gen_mode)
	{
	  decode_len (name, len);
	}
      else
	{
	  len = 1;
	}
      if(len > 0)
	{
	  if (*x == 0 || check_type_changed || schema_gen_mode || size_allocated == 0)
	    {
	      *x = (long double *) malloc (len * sizeof (**x));
	      memset(*x,0,len*sizeof(**x));
	      size_allocated = len;
	      add_uba((void **)x);
	    }
	  if (*x != 0)
	    {
	      if (len > size_allocated)
		{
		  remove_uba((void **)x);
		  *x = (long double *) realloc (*x, len * sizeof (**x));
		  memset(((char*)(*x))+size_allocated*sizeof(**x),
			 0,(len-size_allocated)*sizeof(**x));
		  size_allocated = len;
		  add_uba((void **)x);
		}
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      bool orig_add_array_indexes_to_name = add_array_indexes_to_name;
      add_array_indexes_to_name = false;
      inside_unbounded[class_count] = 1;
      long offset = (long) (((char *) x) - ((char *) base));
      if (diffbuff != 0 && base != 0 && offset < (int) diffbuff_size)
	{
	  char *newdiffbuffptraddr = (char *) diffbuff;
	  newdiffbuffptraddr += offset;
	  void **vnewdiffbuffptraddr = (void **) newdiffbuffptraddr;
	  diffbuff = (void *) (*vnewdiffbuffptraddr);
	  base = *x;
	  diffbuff_size = sizeof (*x) * size_allocated;
	}
      retval = update_with_name (name, *x, len);
      inside_unbounded[class_count] = 0;
      base = orig_base;
      diffbuff = orig_diffbuff;
      diffbuff_size = orig_diffbuff_size;
      add_array_indexes_to_name = orig_add_array_indexes_to_name;
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}


CMS_STATUS
CMS_XML_UPDATER::beginStructUnboundedArray (const char *name, void **x,
					    int &len, int &size_allocated,
					    size_t elsize)
{
  unbounded_used = true;
  if (pointer_check_disabled < 0)
    {
      pointer_check_disabled = 1;
    }
  else
    {
      pointer_check_disabled++;
    }
  if (base != 0 && diffbuff != 0)
    {
      if (unbounded_struct_array_bases == 0 ||
	  unbounded_struct_array_diffbuffs == 0 ||
	  unbounded_struct_array_diffbuff_sizes == 0)
	{
	  max_unbounded_struct_array_count = 64;
	  unbounded_struct_array_bases = (void **)
	    DEBUG_MALLOC (max_unbounded_struct_array_count * sizeof (void *));
	  unbounded_struct_array_diffbuffs = (void **)
	    DEBUG_MALLOC (max_unbounded_struct_array_count * sizeof (void *));
	  unbounded_struct_array_diffbuff_sizes = (size_t *)
	    DEBUG_MALLOC (max_unbounded_struct_array_count * sizeof (size_t));
	}
      if (unbounded_struct_array_count + 1 >=
	  max_unbounded_struct_array_count)
	{
	  max_unbounded_struct_array_count += 32;
	  unbounded_struct_array_bases = (void **)
	    DEBUG_REALLOC ((void *) unbounded_struct_array_bases,
			   max_unbounded_struct_array_count *
			   sizeof (void *));
	  unbounded_struct_array_diffbuffs =
	    (void **) DEBUG_REALLOC ((void *)
				     unbounded_struct_array_diffbuffs,
				     max_unbounded_struct_array_count *
				     sizeof (void *));
	  unbounded_struct_array_diffbuff_sizes =
	    (size_t *)DEBUG_REALLOC ((void *)
				      unbounded_struct_array_diffbuff_sizes,
				      max_unbounded_struct_array_count *
				      sizeof (size_t));
	}

      unbounded_struct_array_bases[unbounded_struct_array_count] = base;
      unbounded_struct_array_diffbuffs[unbounded_struct_array_count] =
	diffbuff;
      unbounded_struct_array_diffbuff_sizes[unbounded_struct_array_count] =
	diffbuff_size;
      unbounded_struct_array_count++;
    }

  if ((len <= 0 && encoding && !schema_gen_mode) || no_unbounded || x == 0
      || status == CMS_UPDATE_ERROR || elsize < 1)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (encoding && !schema_gen_mode)
    {
      if (*x == 0 || check_type_changed || schema_gen_mode)
	{
	  size_allocated = 0;
	}
      if (len > size_allocated)
	{
	  len = size_allocated;
	}
    }
  else
    {
      if (!schema_gen_mode)
	{
	  decode_len (name, len);
	}
      else
	{
	  len = 1;
	}
      if(len > 0)
	{
	  if (*x == 0 || check_type_changed || schema_gen_mode || size_allocated == 0 )
	    {
	      *x = malloc (elsize * len);
	      memset(*x,0,elsize*len);
	      size_allocated = len;
	    }
	  if (*x != 0)
	    {
	      if (len > size_allocated)
		{
		  *x = realloc (*x, elsize * len);
		  memset(((char*)(*x))+elsize*size_allocated,
			 0,(len-size_allocated)*elsize);	      
		  size_allocated = len;
		}
	    }
	}
    }
  long offset = (long) (((char *) x) - ((char *) base));
  if (diffbuff != 0 && base != 0 && offset < (int) diffbuff_size)
    {
      char *newdiffbuffptraddr = (char *) diffbuff;
      newdiffbuffptraddr += offset;
      void **vnewdiffbuffptraddr = (void **) newdiffbuffptraddr;
      diffbuff = (void *) (*vnewdiffbuffptraddr);
      base = *x;
      diffbuff_size = elsize * size_allocated;
    }
  inside_unbounded[class_count] = 1;
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_XML_UPDATER::endStructUnboundedArray (const char *, 
					  void **,
					  int &,
					  int &,
					  size_t)
{
  unbounded_used = true;
  inside_unbounded[class_count] = 0;
  if (pointer_check_disabled > 0)
    {
      pointer_check_disabled--;
    }
  if (unbounded_struct_array_count > 0)
    {
      unbounded_struct_array_count--;
      base = unbounded_struct_array_bases[unbounded_struct_array_count];
      diffbuff =
	unbounded_struct_array_diffbuffs[unbounded_struct_array_count];
      diffbuff_size =
	unbounded_struct_array_diffbuff_sizes[unbounded_struct_array_count];
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_XML_UPDATER::beginEnumerationUnbounded (const char *name, int **x,
					      const cms_enum_info * info,
					      int &len, int &size_allocated,
					      size_t elsize)
{
  unbounded_used = true;
  if (pointer_check_disabled < 0)
    {
      pointer_check_disabled = 1;
    }
  else
    {
      pointer_check_disabled++;
    }
  if ((len <= 0 && encoding && !schema_gen_mode) || no_unbounded || x == 0
      || status == CMS_UPDATE_ERROR || elsize < 1)
    return CMS_STATUS_NOT_SET;
  if (encoding && !schema_gen_mode)
    {
      if (*x == 0 || check_type_changed || schema_gen_mode)
	{
	  size_allocated = 0;
	}
      if (len > size_allocated)
	{
	  len = size_allocated;
	}
    }
  else
    {
      if (!schema_gen_mode)
	{
	  decode_len (name, len);
	}
      else
	{
	  len = 1;
	}
      if (elsize < sizeof (int))
	{
	  elsize = sizeof (int);
	}
      if (*x == 0 || check_type_changed)
	{
	  *x = (int *) malloc (elsize * len);
	  size_allocated = len;
	  add_uba((void **)x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **)x);
	      *x = (int *) realloc (*x, elsize * len);
	      size_allocated = len;
	      add_uba((void **)x);
	    }
	}
    }
  return beginEnumerationArray (name, info, len);
}


CMS_STATUS
  CMS_XML_UPDATER::endEnumerationUnbounded (const char *name, 
					    int **,
					    const cms_enum_info * info,
					    int &len, 
					    int &,
					    size_t)
{
  unbounded_used = true;
  if (pointer_check_disabled > 0)
    {
      pointer_check_disabled--;
    }
  return endEnumerationArray (name, info, len);
}

void CMS_XML_UPDATER::recheck_properties(void)
{
  if(my_xml_style_properties_count != global_xml_style_properties_count)
    {
      my_xml_style_properties_count = global_xml_style_properties_count;

#ifdef DEBUG_THIS_FILE
      rcs_print_debug (PRINT_UPDATER_ACTIVITY,
		       "CMS_XML_UPDATER: xml_style_properties=%p\n",
		       xml_style_properties);
#endif
      if (0 != xml_style_properties)
	{
      
#ifdef DEBUG_THIS_FILE
	  rcs_print_debug (PRINT_UPDATER_ACTIVITY,
			   "CMS_XML_UPDATER: xml_style_properties->list_size=%d\n",
			   xml_style_properties->list_size);
#endif
	  const char *
	    propstr = (char *)
	    xml_style_properties->
	    get_head ();
	  while (propstr)
	    {
	      const char *vstring = strchr(propstr,'=');
	      if(vstring <= propstr ||  0 ==  vstring)
		{
		  rcs_print_error("bad XML style property string %s\n",
				  propstr);
		  propstr = (char *) xml_style_properties->get_next ();
		  continue;
		}
	      vstring++;
	      long namelen = (long) (vstring - propstr);
	      
#ifdef DEBUG_THIS_FILE
	      rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
			       "CMS_XML_UPDATER: from xml_style_properties propstr=%s,strlen(propstr)=%d\n",
			       propstr, strlen (propstr));
#endif
	      if (!strncmp (propstr, "ADD_ARRAY_INDEXES_TO_NAME=", namelen))
		{
		  if(!strcmp(vstring,"true") || !strcmp(vstring,"TRUE") ||
		     !strcmp(vstring,"1"))
		    {
		      add_array_indexes_to_name = true;
		    }
		  else if(!strcmp(vstring,"false") || !strcmp(vstring,"FALSE") ||
			  !strcmp(vstring,"0"))
		    {
		      add_array_indexes_to_name = false;
		    }
		  else
		    {
		      rcs_print_error("bad XML style property string %s\n",
				      propstr);
		    }
#ifdef DEBUG_THIS_FILE
		  rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
				   "CMS_XML_UPDATER: from xml_style_properties add_array_indexes_to_name=%d\n",
				   add_array_indexes_to_name);
#endif
		}
	      else if (!strncmp (propstr, "INSIDE_XML_DECLARATION=", namelen))
		{
		  if(inside_xml_declaration_addition == 0)
		    {
		      inside_xml_declaration_addition = strdup(vstring);
#ifdef DEBUG_THIS_FILE
		      rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
				       "CMS_XML_UPDATER: inside_xml_declaration_addition=%s\n",
				       inside_xml_declaration_addition );
#endif

		    }
		  else if(!strcmp(inside_xml_declaration_addition,vstring))
		    {
		      inside_xml_declaration_addition = strdup(vstring);
#ifdef DEBUG_THIS_FILE
		      rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
				       "CMS_XML_UPDATER: inside_xml_declaration_addition=%s\n",
				       inside_xml_declaration_addition );
#endif
		    }	
		}
	      else if (!strncmp (propstr, "AFTER_XML_DECLARATION=", namelen))
		{
		  if(after_xml_declaration_addition == 0)
		    {
		      after_xml_declaration_addition = strdup(vstring);
#ifdef DEBUG_THIS_FILE
		      rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
				       "CMS_XML_UPDATER: after_xml_declaration_addition=%s\n",
				       after_xml_declaration_addition );
#endif

		    }
		  else if(!strcmp(after_xml_declaration_addition,vstring))
		    {
		      after_xml_declaration_addition = strdup(vstring);
#ifdef DEBUG_THIS_FILE
		      rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
				       "CMS_XML_UPDATER: after_xml_declaration_addition=%s\n",
				       after_xml_declaration_addition );
#endif
		    }
		}
	      else if (!strncmp (propstr, "XML_ROOT_START=", namelen))
		{
		  if(inside_root_start_addition == 0)
		    {
		      inside_root_start_addition = strdup(vstring);
#ifdef DEBUG_THIS_FILE
		      rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
				       "CMS_XML_UPDATER: inside_xml_inside_root_start_addition=%s\n",
				       inside_root_start_addition );
#endif

		    }
		  else if(!strcmp(inside_root_start_addition,vstring))
		    {
		      inside_root_start_addition = strdup(vstring);
#ifdef DEBUG_THIS_FILE
		      rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
				       "CMS_XML_UPDATER: inside_xml_inside_root_start_addition=%s\n",
				       inside_root_start_addition );
#endif
		    }
		}
	      else if (!strncmp (propstr, "NS_PREFIX=", namelen))
		{
		  if(ns_prefix == 0)
		    {
		      ns_prefix = strdup(vstring);
		      ns_changed=true;
#ifdef DEBUG_THIS_FILE
		      rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
				       "CMS_XML_UPDATER: ns_prefix=%s\n",
				       ns_prefix );
#endif

		    }
		  else if(!strcmp(ns_prefix,vstring))
		    {
		      ns_prefix = strdup(vstring);
		      ns_changed = true;
#ifdef DEBUG_THIS_FILE
		      rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
				       "CMS_XML_UPDATER: ns_prefix=%s\n",
				       ns_prefix );
#endif
		    }
		}
	      else if (!strncmp (propstr, "NS_HREF=", namelen))
		{
		  if(ns_href == 0)
		    {
		      ns_href = strdup(vstring);
		      ns_changed=true;
#ifdef DEBUG_THIS_FILE
		      rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
				       "CMS_XML_UPDATER: ns_href=%s\n",
				       ns_href );
#endif

		    }
		  else if(!strcmp(ns_href,vstring))
		    {
		      ns_href = strdup(vstring);
		      ns_changed=true;
#ifdef DEBUG_THIS_FILE
		      rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
				       "CMS_XML_UPDATER: ns_href=%s\n",
				       ns_href );
#endif
		    }
		}
	      else
		{
		  rcs_print_error("bad XML style property string %s\n",
				  propstr);
		}
	      propstr = (char *) xml_style_properties->get_next ();
	    }
	}
    }
}

#endif // ! XML_SCHEMA_TO_HEADER
#endif // ! XML_PARSE_TEST


