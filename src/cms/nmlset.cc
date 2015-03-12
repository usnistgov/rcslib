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
#endif

#include "cms.hh"
#include "nml.hh"
#include "nmlset.hh"
#include "linklist.hh"
#include "nmlcfgsvr_clntcalls.hh"
#include "rcs_prnt.hh"

#ifndef __unused_parameter__
#ifdef __GNUC__
#if (__GNUC__ >= 3 ) && !defined(MS_WINDOWS_API)
#define __unused_parameter__ __attribute__ ((unused))
#else
#define __unused_parameter__
#endif
#else
#define __unused_parameter__
#endif
#endif

class NMLDOMAINSET_MEMBER_PRIVATE_DATA
{
public:
  NMLDOMAINSET *parent_domain_set;
  int id;

  NMLDOMAINSET_MEMBER_PRIVATE_DATA():
    parent_domain_set(0),id(0)
  {
  };
  ~NMLDOMAINSET_MEMBER_PRIVATE_DATA()
  {
    if(parent_domain_set)
      {
	parent_domain_set->set_member(0,id);
	parent_domain_set=0;
      }
  }
private:
  NMLDOMAINSET_MEMBER_PRIVATE_DATA(const NMLDOMAINSET_MEMBER_PRIVATE_DATA &);
  NMLDOMAINSET_MEMBER_PRIVATE_DATA &operator=(const NMLDOMAINSET_MEMBER_PRIVATE_DATA &);

};
  

NMLDOMAINSET_MEMBER::~NMLDOMAINSET_MEMBER()
{
  if(pd)
    {
      delete pd;
      pd=0;
    }
}


NMLDOMAINSET_MEMBER::NMLDOMAINSET_MEMBER(const char *bufline, 
					 const char *procline):
    NML(bufline,procline),pd(0)
{
  pd = new NMLDOMAINSET_MEMBER_PRIVATE_DATA();
}

void
NMLDOMAINSET_MEMBER::set_parent_id(NMLDOMAINSET *_parent, int _id)
{
  if(pd)
    {
      pd->parent_domain_set = _parent;
      pd->id = _id;
    }
}

class NMLDOMAINSET_PRIVATE_DATA
{
public:
  NMLSET *parent_set;
  int id;
  NMLDOMAINSET_MEMBER **nmldomainset_member_vector;
  int sizeof_nmldomainset_member_vector;
  char *domain_name;
  char *cfgsvr_name;
  int cfgsvr_id;

  NMLDOMAINSET_PRIVATE_DATA(int _sizeof_nmldomainset_member_vector,
			    const char *_domain_name,
			    const char *_cfgsvr_name,
			    int _cfgsvr_id):
    parent_set(0),id(0),
    nmldomainset_member_vector(0),
    sizeof_nmldomainset_member_vector(_sizeof_nmldomainset_member_vector),
    domain_name(0),cfgsvr_name(0),cfgsvr_id(_cfgsvr_id)
  {
    if(_domain_name)
      {
	domain_name = strdup(_domain_name);
      }
    if(_cfgsvr_name)
      {
	cfgsvr_name = strdup(_cfgsvr_name);
      }
    if(sizeof_nmldomainset_member_vector > 0)
      {
	nmldomainset_member_vector = (NMLDOMAINSET_MEMBER **) 
	  malloc((sizeof(NMLDOMAINSET_MEMBER *))*sizeof_nmldomainset_member_vector);
      }
    for(int i =0; i< sizeof_nmldomainset_member_vector; i++)
      {
	nmldomainset_member_vector[i]=0;
      }
  }
  ~NMLDOMAINSET_PRIVATE_DATA()
  {
    if(nmldomainset_member_vector)
      {
	for(int i =0; i< sizeof_nmldomainset_member_vector; i++)
	  {
	    if(nmldomainset_member_vector[i])
	      {
		delete nmldomainset_member_vector[i];
		nmldomainset_member_vector[i]=0;
	      }
	  }
	free(nmldomainset_member_vector);
	nmldomainset_member_vector=0;
      }
    if(parent_set && id >= 0)
      {
	parent_set->remove_domainset(id);
	id=-1;
	parent_set=0;
      }
    if(domain_name)
      {
	free(domain_name);
	domain_name=0;
      }
    if(cfgsvr_name)
      {
	free(cfgsvr_name);
	cfgsvr_name=0;
      }
  }

    
private:
  NMLDOMAINSET_PRIVATE_DATA(const NMLDOMAINSET_PRIVATE_DATA &);
  NMLDOMAINSET_PRIVATE_DATA &operator=(const NMLDOMAINSET_PRIVATE_DATA &);
};

NMLDOMAINSET::~NMLDOMAINSET()
{
  if(pd)
    {
      delete pd;
      pd=0;
    }
}

class NMLDOMAINSET_MEMBER  *
NMLDOMAINSET::get_member(int id)
{
  if(!pd)
    {
      return 0;
    }
  if(pd->sizeof_nmldomainset_member_vector <= id || id < 0)
    {
      return 0;
    }
  if(!pd->nmldomainset_member_vector)
    {
      return 0;
    }
  return pd->nmldomainset_member_vector[id];
}

const char *
NMLDOMAINSET::getdomainname() const
{
  if(!pd)
    {
      return 0;
    }
  return pd->domain_name;
}

const char *
NMLDOMAINSET::getcfgsvrname() const
{
  if(!pd)
    {
      return 0;
    }
  return pd->cfgsvr_name;
}
  

int 
NMLDOMAINSET::get_cfgsvr_id()
{
  if(!pd)
    {
      return -1;
    }
  return pd->cfgsvr_id;
}


int 
NMLDOMAINSET::get_max_member_id() const
{
  if(pd)
    {
      return pd->sizeof_nmldomainset_member_vector-1;
    }
  return(-1);
}

NMLDOMAINSET::NMLDOMAINSET(int _sizeof_nmldomainset_member_vector,
			   const char *_domain_name,
			   const char *_cfgsvr_name,
			   int _cfgsvr_id):
  pd(0)
{
  pd = new NMLDOMAINSET_PRIVATE_DATA(_sizeof_nmldomainset_member_vector, 
				     _domain_name, _cfgsvr_name, _cfgsvr_id);
}

void 
NMLDOMAINSET::set_parent_id(NMLSET *_parent_set, int _id)
{
  if(pd)
    {
      pd->parent_set = _parent_set;
      pd->id = _id;
    }
}

void
NMLDOMAINSET::set_member(NMLDOMAINSET_MEMBER *_member_to_set, int _id)
{
  if(!pd)
    {
      return;
    }
  if(pd->sizeof_nmldomainset_member_vector <= _id || _id < 0)
    {
      return;
    }
  if(!pd->nmldomainset_member_vector)
    {
      return;
    }
  if(pd->nmldomainset_member_vector[_id])
    {
      if(pd->parent_set && 0 == _member_to_set)
	{
	  pd->parent_set->remove_member(pd->id, _id);
	}
    }
  pd->nmldomainset_member_vector[_id] = _member_to_set;
  if(pd->nmldomainset_member_vector[_id])
    {
      pd->nmldomainset_member_vector[_id]->set_parent_id(this,_id);
    }
}


class NMLSET_PRIVATE_DATA
{
public:
  NML_FORMAT_PTR f_ptr;
  const char **buflist;

  int buflistsize;
  char *procname; 
  const char **cfglist;
  int cfglistsize;
  bool check_for_new_domains;
  RCS_LINKED_LIST *domains_linked_list;
  char **domainquerystrings;
  char temp_domainname[CMS_CONFIG_LINELEN];
  char temp_bufline[CMS_CONFIG_LINELEN];
  char temp_procline[CMS_CONFIG_LINELEN];
  char recvstring[CMS_CONFIG_LINELEN*2];
  char sendstring[CMS_CONFIG_LINELEN*2];

private:
  char *buflistbuf;
  char *cfglistbuf;

public:

  NMLSET_PRIVATE_DATA (NML_FORMAT_PTR _f_ptr, 
		       const char *_buflist, 
		       const char *_procname, 
		       const char *_cfglist):
    f_ptr(_f_ptr),buflist(0),buflistsize(0),procname(0),
    cfglist(0),cfglistsize(0),
    check_for_new_domains(false),
    domains_linked_list(0),
    domainquerystrings(0),
    buflistbuf(0),
    cfglistbuf(0)
  {
    char *cptr=0;

    if(_buflist)
      {
	buflistbuf = strdup(_buflist);
	cptr = buflistbuf;
	buflistsize=1;
	while(*cptr)
	  {
	    if(*cptr == ';')
	      {
		buflistsize++;
	      }
	    cptr ++;
	  }
	int max_buflistsize=buflistsize+1;
	buflist = (const char **) malloc(sizeof(const char *)*(max_buflistsize));
	cptr = buflistbuf;
	buflistsize=1;
	buflist[0] = buflistbuf;
	rcs_print_debug(PRINT_CMS_CONFIG_INFO,"buflistbuf=%s\n",buflistbuf);
	while(*cptr)
	  {
	    if(*cptr == ';')
	      {
		if(*(cptr+1))
		  {
		    buflist[buflistsize]= (cptr+1);
		    buflistsize++;
		    if(buflistsize >= max_buflistsize)
		      {
			break;
		      }
		  }
		*cptr=0;
	      }
	    cptr ++;
	  } 
	buflist[buflistsize]=0;
	for(int j=0; j < buflistsize; j++)
	  {
	    rcs_print_debug(PRINT_CMS_CONFIG_INFO,"buflist[%d]=%s\n",j,buflist[j]);
	  }
      }
    if(_procname)
      {
	procname = strdup(_procname);
      }
    if(_cfglist)
      {
	cfglistbuf = strdup(_cfglist);
	cptr = cfglistbuf;
	cfglistsize=1;
	while(*cptr)
	  {
	    if(*cptr == ';')
	      {
		cfglistsize++;
	      }
	    cptr ++;
	  }
	int max_cfglistsize = cfglistsize+1;
	cfglist = (const char **) malloc(sizeof(const char *)*(max_cfglistsize));
	cptr = cfglistbuf;
	rcs_print_debug(PRINT_CMS_CONFIG_INFO,"cfglistbuf=%s\n",cfglistbuf);
	cfglistsize=1;
	cfglist[0] = cfglistbuf;
	while(*cptr)
	  {
	    if(*cptr == ';')
	      {
		if(*(cptr+1))
		  {
		    cfglist[cfglistsize]= (cptr+1);
		    cfglistsize++;
		    if(cfglistsize >= max_cfglistsize)
		      {
			break;
		      }
		  }
		*cptr=0;
	      }
	    cptr ++;
	  } 
	for(int i = 0 ; i < cfglistsize; i++)
	  {
	    if(!cfglist[i])
	      {
		break;
	      }
	    if(!strncmp(cfglist[i],"nmlcfgsvr:",10) || !strncmp(cfglist[i],"nmlcfgsvr/",10))
	      {
		cfglist[i] += 10;
	      }
	    rcs_print_debug(PRINT_CMS_CONFIG_INFO,"cfglist[%d]=%s\n",i,cfglist[i]);
	  }
	cfglist[cfglistsize]=0;
      }
    size_t domainquerystrings_len = sizeof(char *)*(cfglistsize+1)*(buflistsize+1);
    domainquerystrings = (char **) malloc(domainquerystrings_len);
    memset(domainquerystrings,0,domainquerystrings_len);    
    domains_linked_list = new RCS_LINKED_LIST();
    memset(temp_domainname,0,sizeof(temp_domainname));
    memset(temp_bufline,0,sizeof(temp_bufline));
    memset(temp_procline,0,sizeof(temp_procline));
  }

  ~NMLSET_PRIVATE_DATA()
  {
    if(domainquerystrings)
      {
	for(int i = 0; i < (cfglistsize>0?cfglistsize:1)*(buflistsize>0?buflistsize:1) ; i++)
	  {
	    if(domainquerystrings[i])
	      {
		free(domainquerystrings[i]);
		domainquerystrings[i]=0;
	      }
	  }
	free(domainquerystrings);
	domainquerystrings=0;
      }	      
    if(buflist)
      {
	free(buflist);
	buflist=0;
      }
    if(procname)
      {
	free(procname);
	procname=0;
      }
    if(cfglist)
      {
	free(cfglist);
	cfglist=0;
      }
    if(domains_linked_list)
      {
	RCS_LINKED_LIST *ll = domains_linked_list;
	domains_linked_list=0;
	NMLDOMAINSET *nds = (NMLDOMAINSET *) ll->get_head();
	while(nds)
	  {
	    delete nds;
	    ll->delete_current_node();
	    nds = (NMLDOMAINSET *) ll->get_next();
	  }
	delete ll;
      }	
    if(buflistbuf)
      {
	free(buflistbuf);
	buflistbuf=0;
      }
    if(cfglistbuf)
      {
	free(cfglistbuf);
	cfglistbuf=0;
      }
	
  }
private:
  NMLSET_PRIVATE_DATA(const NMLSET_PRIVATE_DATA &);
  NMLSET_PRIVATE_DATA &operator=(const NMLSET_PRIVATE_DATA &);
};

NMLSET::NMLSET(NML_FORMAT_PTR _f_ptr, 
	 const char *_buflist, 
	 const char *_procname, 
	 const char *_cfglist):
  pd(0)
{
  pd = new NMLSET_PRIVATE_DATA(_f_ptr,_buflist,_procname,_cfglist);
}

NMLSET::~NMLSET()
{
  if(pd)
    {
      delete pd;
      pd=0;
    }
}

class NMLDOMAINSET  *
NMLSET::get_first_domainset()
{
  if(pd && pd->domains_linked_list)
    {
      return (class NMLDOMAINSET  *) pd->domains_linked_list->get_head();
    }
  return 0;
}
  
class NMLDOMAINSET  *
NMLSET::get_next_domainset()
{
  if(pd && pd->domains_linked_list)
    {
      return (class NMLDOMAINSET  *) pd->domains_linked_list->get_next();
    }
  return 0;
}  

void
NMLSET::update_set(double _timeout)
{
  //rcs_print_debug(PRINT_CMS_CONFIG_INFO,"NMLSET::update_set,pd=%p\n",(void *)pd);
  int i=0;
  int j=0;
  ///set_rcs_print_flag(PRINT_EVERYTHING);

  // enum NMLCFGSVR_STATUS ncs;
  if(!pd)
    {
      return;
    }
  // rcs_print_debug(PRINT_CMS_CONFIG_INFO,"NMLSET::update_set,pd->buflist=%p\n",(void *)(pd->buflist));
  if(!pd->buflist)
    {
      return;
    }

  for(j=0; j < pd->cfglistsize; j++)
    {
      //rcs_print_debug(PRINT_CMS_CONFIG_INFO,"NMLSET::update_set,i=%d\n",i);
      //rcs_print_debug(PRINT_CMS_CONFIG_INFO,"NMLSET::update_set,pd->buflist[i]=%s\n",pd->buflist[i]);
      if(!pd->cfglist[j])
	{
	  break;
	}
      for(i = 0; i < pd->buflistsize; i++)
	{
	  if(!pd->buflist[i])
	    {
	      break;
	    }
	  int dqs_index = (i*(pd->cfglistsize))+j;
	  const char *dqs = pd->domainquerystrings[dqs_index];
	  rcs_print_debug(PRINT_CMS_CONFIG_INFO,"i=%d,j=%d,pd->cfglist[j]=%s,pd->buflist[i]=%s,dqs_index=%d,dqs=%s\n",i,j,pd->cfglist[j],pd->buflist[i],dqs_index,dqs);
	  if(NMLCFGSVR_CREATE_OK == 
	     nmlcfgsvr_create(pd->cfglist[j],
			      pd->buflist[i],
			      pd->procname,
			      pd->temp_bufline,
			      pd->temp_procline,
			      pd->recvstring,sizeof(pd->recvstring),
			      pd->sendstring,sizeof(pd->sendstring),
			      "create=get",
			      _timeout,
			      (dqs?dqs:"*"),
			      false)
	     )		 
	    {
	      const char *domain_str = strstr(pd->temp_bufline,"domain=");
	      rcs_print_debug(PRINT_CMS_CONFIG_INFO,"domain_str=%s\n",domain_str);
	      rcs_print_debug(PRINT_CMS_CONFIG_INFO,"pd->domainquerystrings[%d]=%s\n",(i*(pd->cfglistsize))+j,pd->domainquerystrings[dqs_index]);
	      size_t size_domainquerystring_needed = 2;
	      size_t domain_str_len =0;
	      if(domain_str)
		{
		  size_domainquerystring_needed += strlen(domain_str)+1;
		  domain_str += 7;
		  const char *cptr = domain_str;
		  while(*cptr && *cptr != ' ' && *cptr != '\r' && *cptr != '\n' && *cptr != '\t')
		    {
		      cptr++;
		      domain_str_len++;
		    }
		}
	      else
		{
		  domain_str= "";
		  size_domainquerystring_needed += 1;
		}
	      NMLDOMAINSET_MEMBER *nml = new NMLDOMAINSET_MEMBER(pd->temp_bufline,pd->temp_procline);
	      if(nml)
		{
		  if(pd->f_ptr)
		    {
		      nml->prefix_format_chain(pd->f_ptr);
		    }
		  else
		    {
		      nml->ignore_format_chain=1;
		    }
		  if(nml->valid())
		    {
		      nml->ignore_format_chain=0;
		      NMLDOMAINSET  *curset = (NMLDOMAINSET  *)
			pd->domains_linked_list->get_head();
		      bool domains_match=false;
		      while(curset)
			{
			  domains_match=false;
			  if(j != curset->get_cfgsvr_id())
			    {
			      domains_match=false;
			      curset = (NMLDOMAINSET  *)
				pd->domains_linked_list->get_next();
			      continue;
			    }
			  const char *cursetdomain = curset->getdomainname();
			  if((!cursetdomain || !cursetdomain[0])
			     && (!domain_str || !domain_str[0] || domain_str_len < 1))
			    {
			      domains_match=true;
			    }
			  else if((!cursetdomain || !cursetdomain[0])
				  || (!domain_str || !domain_str[0] || domain_str_len < 1))
			    {
			      domains_match=false;
			      curset = (NMLDOMAINSET  *)
				pd->domains_linked_list->get_next();
			      continue;
			    }
			  else if(!strncmp(cursetdomain,domain_str,domain_str_len) && cursetdomain[domain_str_len] == 0)
			    {
			      domains_match=true;
			    }
			  if(domains_match)
			    {
			      break;
			    }			      
			  curset = (NMLDOMAINSET  *)
			    pd->domains_linked_list->get_next();
			}
		      if(!curset)
			{
			  memset(pd->temp_domainname,0,sizeof(pd->temp_domainname));
			  strncpy(pd->temp_domainname,domain_str,domain_str_len);
			  curset = new NMLDOMAINSET(pd->buflistsize,
						    pd->temp_domainname,
						    pd->cfglist[j], j);
			  pd->domains_linked_list->store_at_tail(curset,sizeof(NMLDOMAINSET),0);
			  curset->set_parent_id(this,pd->domains_linked_list->get_newest_id());
			}
		      curset->set_member(nml,i);
		      if(pd->domainquerystrings[dqs_index])
			{
			  size_domainquerystring_needed += 
			    strlen(pd->domainquerystrings[dqs_index])+1;
			  pd->domainquerystrings[dqs_index] = (char *)
			    realloc(pd->domainquerystrings[dqs_index],size_domainquerystring_needed);
			}
		      else
			{
			  pd->domainquerystrings[dqs_index] = (char *)
			    malloc(size_domainquerystring_needed);
			  memset(pd->domainquerystrings[dqs_index],0,size_domainquerystring_needed);
			  strcpy(pd->domainquerystrings[dqs_index],"!");
			}		    
		      strncat(pd->domainquerystrings[(i*(pd->cfglistsize))+j],domain_str,domain_str_len);
		      strcat(pd->domainquerystrings[dqs_index],",");
		      rcs_print_debug(PRINT_CMS_CONFIG_INFO,"pd->domainquerystrings[%d]=%s\n",dqs_index,pd->domainquerystrings[dqs_index]);

		    }
		  else
		    {
		      delete nml;
		      nml=0;
		    }
		}
	    }
	}
    }
  // clear_rcs_print_flag(PRINT_EVERYTHING);
}


void
NMLSET::remove_member(int _domain_set_id, int _member_id)
{
  rcs_print_debug(PRINT_CMS_CONFIG_INFO,"NMLSET::remove_member(_domain_set_id=%d, _member_i=%d) this=%p\n",
	 _domain_set_id,_member_id,((void*)this));
  if(pd && pd->domains_linked_list)
    {      
      NMLDOMAINSET *nds = (NMLDOMAINSET *) pd->domains_linked_list->get_by_id(_domain_set_id);
      rcs_print_debug(PRINT_CMS_CONFIG_INFO,"nds=%p\n",((void*)nds));
      if(!nds)
	{
	  return;
	}
      NMLDOMAINSET_MEMBER *nm = nds->get_member(_member_id);
      rcs_print_debug(PRINT_CMS_CONFIG_INFO,"nm=%p\n",((void*)nm));
      if(!nm)
	{
	  return;
	}
      int bufid = 0;
      while(bufid < pd->buflistsize)
	{
	  rcs_print_debug(PRINT_CMS_CONFIG_INFO,"pd->buflist[%d]=%s, \tnm->cms->BufferName=%s\n",
		 bufid,pd->buflist[bufid],nm->cms->BufferName);
	  if(!strcmp(pd->buflist[bufid],nm->cms->BufferName))
	    {
	      break;
	    }
	  bufid++;
	}
      if(bufid >= pd->buflistsize)
	{
	  return;
	}
      int cfgid = 0;
      while(cfgid < pd->cfglistsize)
	{
	  rcs_print_debug(PRINT_CMS_CONFIG_INFO,"pd->cfglist[%d]=%s, \tnds->getcfgsvrname()=%s\n",
		 cfgid,pd->cfglist[cfgid],nds->getcfgsvrname());
	  if(!strcmp(pd->cfglist[cfgid],nds->getcfgsvrname()))
	    {
	      break;
	    }
	  cfgid++;
	}
      if(cfgid >= pd->cfglistsize)
	{
	  return;
	}
      if(pd->domainquerystrings[(bufid*(pd->cfglistsize))+cfgid])
	{
	  rcs_print_debug(PRINT_CMS_CONFIG_INFO,"pd->domainquerystrings[%d]=%s\n",
		 (bufid*(pd->cfglistsize))+cfgid, 
		 pd->domainquerystrings[(bufid*(pd->cfglistsize))+cfgid]);
	  memset(pd->temp_domainname,0,sizeof(pd->temp_domainname));
	  strcpy(pd->temp_domainname,(pd->domainquerystrings[(bufid*(pd->cfglistsize))+cfgid]));
	  char *begindomain = pd->temp_domainname;
	  char *enddomain= pd->temp_domainname;
	  const char *domaintolookfor = nds->getdomainname();
	  rcs_print_debug(PRINT_CMS_CONFIG_INFO,"domaintolookfor=%s\n",domaintolookfor);
	  if(*begindomain == '!')
	  {
	    begindomain++;
	    enddomain++;
	  }
	  while(*begindomain)
	    {
	      rcs_print_debug(PRINT_CMS_CONFIG_INFO,"begindomain=%s\n",begindomain);
	      while(*enddomain && *enddomain != ',')
		{
		  enddomain++;
		}
	      char c = *enddomain;
	      if(*domaintolookfor == 0)
		{
		  if(enddomain == begindomain)
		    {
		      enddomain++;
		      strcpy(begindomain,enddomain);
		      break;
		    }
		  else
		    {
		      enddomain++;
		      begindomain = enddomain;
		      continue;
		    }
		}
	      *enddomain = 0;
	      if(!strcmp(begindomain,domaintolookfor))
		{
		  enddomain++;
		  strcpy(begindomain,enddomain);
		  break;
		}
	      *enddomain = c;
	      enddomain++;
	      begindomain = enddomain;
	    }
         strcpy((pd->domainquerystrings[(bufid*(pd->cfglistsize))+cfgid]),
		pd->temp_domainname);
	 if(!strcmp(pd->domainquerystrings[(bufid*(pd->cfglistsize))+cfgid],"!"))
	   {
	     free(pd->domainquerystrings[(bufid*(pd->cfglistsize))+cfgid]);
	     pd->domainquerystrings[(bufid*(pd->cfglistsize))+cfgid]=0;
	   }
	 memset(pd->temp_domainname,0,sizeof(pd->temp_domainname));
	 if(pd->domainquerystrings[(bufid*(pd->cfglistsize))+cfgid])
	   {
	     rcs_print_debug(PRINT_CMS_CONFIG_INFO,"pd->domainquerystrings[%d]=%s\n",
		    (bufid*(pd->cfglistsize))+cfgid, 
		    pd->domainquerystrings[(bufid*(pd->cfglistsize))+cfgid]);
	   }
	}
    }
}

void
NMLSET::remove_domainset(int id)
{
  if(pd && pd->domains_linked_list)
    {
      pd->domains_linked_list->delete_node(id);
    }
}

