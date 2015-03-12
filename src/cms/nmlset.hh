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

#ifndef NMLSET_HH
#define NMLSET_HH

#include "nml.hh"

class NMLSET;
class NMLDOMAINSET;
class NMLDOMAINSET_MEMBER_PRIVATE_DATA;

class NMLDOMAINSET_MEMBER : public NML
{
public:
  ~NMLDOMAINSET_MEMBER();

private:
  class NMLDOMAINSET_MEMBER_PRIVATE_DATA *pd;
  void set_parent_id(NMLDOMAINSET *_parent,int _id);

  NMLDOMAINSET_MEMBER(const char *bufline, 
		      const char *procline);
  NMLDOMAINSET_MEMBER(const NMLDOMAINSET_MEMBER &);
  NMLDOMAINSET_MEMBER &operator=(const NMLDOMAINSET_MEMBER &);
  friend class NMLSET;
  friend class NMLSET_PRIVATE_DATA;
  friend class NMLDOMAINSET;
  friend class NMLDOMAINSET_PRIVATE_DATA;
  friend class NMLDOMAINSET_MEMBER_PRIVATE_DATA;
};

class NMLDOMAINSET
{
public:
  ~NMLDOMAINSET();

  class NMLDOMAINSET_MEMBER  *get_member(int id);
  const char *getdomainname() const;
  const char *getcfgsvrname() const;
  int get_max_member_id() const;

private:
  NMLDOMAINSET( int _sizeof_nmldomainset_member_vector,
		const char *_domain_name, 
		const char *_cfgsvr_name,
		int cfgsvr_id);

  int get_cfgsvr_id();
  void set_parent_id(NMLSET *_parent_set, int id);
  void set_member(NMLDOMAINSET_MEMBER *,int id);
  class NMLDOMAINSET_PRIVATE_DATA *pd;
  NMLDOMAINSET(const NMLDOMAINSET &);
  NMLDOMAINSET &operator=(const NMLDOMAINSET &);

  friend class NMLDOMAINSET_MEMBER;
  friend class NMLDOMAINSET_MEMBER_PRIVATE_DATA;
  friend class NMLSET;
  friend class NMLSET_PRIVATE_DATA;
  friend class NMLDOMAINSET_PRIVATE_DATA;
};

class NMLSET
{
public:
  NMLSET(NML_FORMAT_PTR f_ptr, 
	 const char *buflist, 
	 const char *procname, 
	 const char *cfglist);
  ~NMLSET();

  class NMLDOMAINSET  *get_first_domainset();
  class NMLDOMAINSET  *get_next_domainset();
  void update_set(double _timeout);

private:
  class NMLSET_PRIVATE_DATA *pd;
  void remove_domainset(int id);
  void remove_member(int domain_set_id, int member_id);

  
  NMLSET(const NMLSET &);
  NMLSET &operator=(const NMLSET &);
  
  friend class NMLSET_PRIVATE_DATA;
  friend class NMLDOMAINSET;
  friend class NMLDOMAINSET_PRIVATE_DATA;
};


#endif
