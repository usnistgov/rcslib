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

#ifndef CMS_XML_UP_HH
#define CMS_XML_UP_HH

#include "cms_up.hh"		/* class CMS_UPDATER */

struct XML_DOC;
struct XML_NODE;
struct XML_ATTRIBUTE;
struct XML_NS;

class CMS_XML_UPDATER:public CMS_UPDATER
{
public:
  CMS_STATUS update (bool &x);
  CMS_STATUS update (char &x);
  CMS_STATUS update (unsigned char &x);
  CMS_STATUS update (short int &x);
  CMS_STATUS update (unsigned short int &x);
  CMS_STATUS update (int &x);
  CMS_STATUS update (unsigned int &x);
  CMS_STATUS update (long int &x);
  CMS_STATUS update (unsigned long int &x);
  CMS_STATUS update (float &x);
  CMS_STATUS update (double &x);
  CMS_STATUS update (long double &x);
  CMS_STATUS update (bool *x, unsigned int len);
  CMS_STATUS update (char *x, unsigned int len);
  CMS_STATUS update (unsigned char *x, unsigned int len);
  CMS_STATUS update (short *x, unsigned int len);
  CMS_STATUS update (unsigned short *x, unsigned int len);
  CMS_STATUS update (int *x, unsigned int len);
  CMS_STATUS update (unsigned int *x, unsigned int len);
  CMS_STATUS update (long *x, unsigned int len);
  CMS_STATUS update (unsigned long *x, unsigned int len);
  CMS_STATUS update (float *x, unsigned int len);
  CMS_STATUS update (double *x, unsigned int len);
  CMS_STATUS update (long double *x, unsigned int len);

  CMS_STATUS update_with_name (const char *name, bool &x);
  CMS_STATUS update_with_name (const char *name, char &x);
  CMS_STATUS update_with_name (const char *name, unsigned char &x);
  CMS_STATUS update_with_name (const char *name, short int &x);
  CMS_STATUS update_with_name (const char *name, unsigned short int &x);
  CMS_STATUS update_with_name (const char *name, int &x);
  CMS_STATUS update_with_name (const char *name, unsigned int &x);
  CMS_STATUS update_with_name (const char *name, long int &x);
  CMS_STATUS update_with_name (const char *name, unsigned long int &x);
  CMS_STATUS update_with_name (const char *name, float &x);
  CMS_STATUS update_with_name (const char *name, double &x);
  CMS_STATUS update_with_name (const char *name, long double &x);
  CMS_STATUS update_with_name (const char *name, bool *x, unsigned int len);
  CMS_STATUS update_with_name (const char *name, char *x, unsigned int len);
  CMS_STATUS update_with_name (const char *name, unsigned char *x,
			       unsigned int len);
  CMS_STATUS update_with_name (const char *name, short *x, unsigned int len);
  CMS_STATUS update_with_name (const char *name, unsigned short *x,
			       unsigned int len);
  CMS_STATUS update_with_name (const char *name, int *x, unsigned int len);
  CMS_STATUS update_with_name (const char *name, unsigned int *x,
			       unsigned int len);
  CMS_STATUS update_with_name (const char *name, long *x, unsigned int len);
  CMS_STATUS update_with_name (const char *name, unsigned long *x,
			       unsigned int len);
  CMS_STATUS update_with_name (const char *name, float *x, unsigned int len);
  CMS_STATUS update_with_name (const char *name, double *x, unsigned int len);
  CMS_STATUS update_with_name (const char *name, long double *x,
			       unsigned int len);


  CMS_STATUS update_attribute_with_name (const char *, bool &x);
  CMS_STATUS update_attribute_with_name (const char *, char &x);
  CMS_STATUS update_attribute_with_name (const char *, unsigned char &x);
  CMS_STATUS update_attribute_with_name (const char *, short int &x);
  CMS_STATUS update_attribute_with_name (const char *, unsigned short int &x);
  CMS_STATUS update_attribute_with_name (const char *, int &x);
  CMS_STATUS update_attribute_with_name (const char *, unsigned int &x);
  CMS_STATUS update_attribute_with_name (const char *, long int &x);
  CMS_STATUS update_attribute_with_name (const char *, unsigned long int &x);
  CMS_STATUS update_attribute_with_name (const char *, float &x);
  CMS_STATUS update_attribute_with_name (const char *, double &x);
  CMS_STATUS update_attribute_with_name (const char *, long double &x);
  CMS_STATUS update_attribute_with_name (const char *, char *x,
					 unsigned int len);


  CMS_STATUS update_attribute_with_name (const char *name,
					 int *ixP,
					 unsigned int len);

  CMS_STATUS update_attribute_with_name (const char *name, bool *x,
					 unsigned int len)
  { return update_with_name(name,x,len);};


  CMS_STATUS update_attribute_with_name (const char *name, unsigned char *x,
					 unsigned int len)
  { return update_with_name(name,x,len);};

  CMS_STATUS update_attribute_with_name (const char *name, short *x,
					 unsigned int len);

  CMS_STATUS update_attribute_with_name (const char *name, unsigned short *x,
					 unsigned int len);

  CMS_STATUS update_attribute_with_name (const char *name, unsigned int *x,
					 unsigned int len);

  CMS_STATUS update_attribute_with_name (const char *name, long *x,
					 unsigned int len);

  CMS_STATUS update_attribute_with_name (const char *name, unsigned long *x,
					 unsigned int len);

  CMS_STATUS update_attribute_with_name (const char *name, float *x,
					 unsigned int len);

  CMS_STATUS update_attribute_with_name (const char *name,  double  *x,
					 unsigned int len);

  CMS_STATUS update_attribute_with_name (const char *name, long double *x,
					 unsigned int len)
  { return update_with_name(name,x,len);};

  CMS_STATUS update_attribute_with_name (const char *name, CMS_DURATION & d);
  CMS_STATUS update_attribute_with_name (const char *name, CMS_DATE_TIME & d);
  CMS_STATUS update_attribute_with_name (const char *name, CMS_TIME & d);
  CMS_STATUS update_attribute_with_name (const char *name, CMS_DATE & d);


  CMS_STATUS update_attribute_with_name (const char *name, CMS_DURATION *d, int len)
  { return CMS_UPDATER::update_attribute_with_name(name,d,len); }

  CMS_STATUS update_attribute_with_name (const char *name, CMS_DATE_TIME *d, int len)
  { return CMS_UPDATER::update_attribute_with_name(name,d,len); }

  CMS_STATUS update_attribute_with_name (const char *name, CMS_TIME *d, int len)
  { return CMS_UPDATER::update_attribute_with_name(name,d,len); }

  CMS_STATUS update_attribute_with_name (const char *name, CMS_DATE *d, int len)
  { return CMS_UPDATER::update_attribute_with_name(name,d,len); }

  CMS_STATUS update_dla_with_name (const char *, char *x, int &len,
				   int maxlen);
  CMS_STATUS update_dla_with_name (const char *, unsigned char *x, int &len,
				   int maxlen);
  CMS_STATUS update_dla_with_name (const char *, short *x, int &len,
				   int maxlen);
  CMS_STATUS update_dla_with_name (const char *, unsigned short *x, int &len,
				   int maxlen);
  CMS_STATUS update_dla_with_name (const char *, int *x, int &len,
				   int maxlen);
  CMS_STATUS update_dla_with_name (const char *, unsigned int *x, int &len,
				   int maxlen);
  CMS_STATUS update_dla_with_name (const char *, long *x, int &len,
				   int maxlen);
  CMS_STATUS update_dla_with_name (const char *, unsigned long *x, int &len,
				   int maxlen);
  CMS_STATUS update_dla_with_name (const char *, float *x, int &len,
				   int maxlen);
  CMS_STATUS update_dla_with_name (const char *, double *x, int &len,
				   int maxlen);
  CMS_STATUS update_dla_with_name (const char *, long double *x, int &len,
				   int maxlen);

  CMS_STATUS update_unbounded_attribute_with_name (const char *, char **x, int &len,
					 int &size_allocated);
  CMS_STATUS update_unbounded_with_name (const char *, char **x, int &len,
					 int &size_allocated);
  CMS_STATUS update_unbounded_with_name (const char *, unsigned char **x,
					 int &len, int &size_allocated);
  CMS_STATUS update_unbounded_with_name (const char *, short **x, int &len,
					 int &size_allocated);
  CMS_STATUS update_unbounded_with_name (const char *, unsigned short **x,
					 int &len, int &size_allocated);
  CMS_STATUS update_unbounded_with_name (const char *, int **x, int &len,
					 int &size_allocated);
  CMS_STATUS update_unbounded_with_name (const char *, unsigned int **x,
					 int &len, int &size_allocated);
  CMS_STATUS update_unbounded_with_name (const char *, long **x, int &len,
					 int &size_allocated);
  CMS_STATUS update_unbounded_with_name (const char *, unsigned long **x,
					 int &len, int &size_allocated);
  CMS_STATUS update_unbounded_with_name (const char *, float **x, int &len,
					 int &size_allocated);
  CMS_STATUS update_unbounded_with_name (const char *, double **x, int &len,
					 int &size_allocated);
  CMS_STATUS update_unbounded_with_name (const char *, long double **x,
					 int &len, int &size_allocated);

  CMS_STATUS beginStructUnboundedArray (const char *, void **x, int &len,
					int &size_allocated, size_t elsize);
  CMS_STATUS endStructUnboundedArray (const char *, void **x, int &len,
				      int &size_allocated, size_t elsize);

  CMS_STATUS beginClass (const char *, const char *);
  CMS_STATUS endClass (const char *, const char *);
  CMS_STATUS beginBaseClass (const char *);
  CMS_STATUS endBaseClass (const char *);

  CMS_STATUS update_with_name (const char *name, CMS_DURATION & d);
  CMS_STATUS update_with_name (const char *name, CMS_DATE_TIME & d);
  CMS_STATUS update_with_name (const char *name, CMS_TIME & d);
  CMS_STATUS update_with_name (const char *name, CMS_DATE & d);

  enum CMS_STATUS update (CMS_DURATION & d)
  {return CMS_UPDATER::update(d);};

  enum CMS_STATUS update (CMS_DURATION * d, int len)
  {return CMS_UPDATER::update(d,len);};
  
  enum CMS_STATUS update_with_name (const char *name, CMS_DURATION * d,
				    int len) 
  {return CMS_UPDATER::update_with_name(name,d,len);};
  
  enum CMS_STATUS update (CMS_DATE_TIME & d)
  {return CMS_UPDATER::update(d);};
  enum CMS_STATUS update (CMS_DATE_TIME * d, int len)
  {return CMS_UPDATER::update(d,len);};

  enum CMS_STATUS update_with_name (const char *name, CMS_DATE_TIME * d,
				       int len)
      {return CMS_UPDATER::update_with_name(name,d,len);};

  enum CMS_STATUS update (CMS_TIME & d)
  {return CMS_UPDATER::update(d);};
  enum CMS_STATUS update (CMS_TIME * d, int len)
  {return CMS_UPDATER::update(d,len);};
  
  enum CMS_STATUS update_with_name (const char *name, CMS_TIME * d,
				       int len)
  {return CMS_UPDATER::update_with_name(name,d,len);};

  enum CMS_STATUS update (CMS_DATE & d)
  {return CMS_UPDATER::update(d);};
  enum CMS_STATUS update (CMS_DATE * d, int len)
  {return CMS_UPDATER::update(d,len);};
  
  enum CMS_STATUS update_with_name (const char *name, CMS_DATE * d,
				    int len)
  {return CMS_UPDATER::update_with_name(name,d,len);};


  CMS_STATUS beginClassVar (const char *name);
  CMS_STATUS endClassVar (const char *name);
  CMS_STATUS beginStructArrayElem (const char *, int);
  CMS_STATUS endStructArrayElem (const char *, int);
  CMS_STATUS beginStructDynamicArray (const char *, int &len, int maxlen);
  CMS_STATUS endStructDynamicArray (const char *, int &len, int maxlen);

  int update_union_selector_with_name (const char *name,
				       int enumin, void *enumaddr,
				       const struct cms_enum_info * info);


  int update_enumeration_with_name (const char *name,
				    int enumin, void *enumaddr,
				    const struct cms_enum_info * info);

  CMS_STATUS beginEnumerationArray (const char *name,
				    const struct cms_enum_info * info,
				    unsigned int len);
  CMS_STATUS beginEnumerationDLA (const char *name,
				  const struct cms_enum_info * info, int &len,
				  int maxlen);

  CMS_STATUS beginEnumerationUnbounded (const char *name, int **x,
					const struct cms_enum_info * info, int &len,
					int &maxlen, size_t elsize);
  CMS_STATUS endEnumerationUnbounded (const char *name, int **x,
				      const struct cms_enum_info * info, int &len,
				      int &maxlen, size_t elsize);

  int update_enumeration_array_elem (int enumin, void *enumaddr, int elem);

  CMS_STATUS endEnumerationArray (const char *name,
				  const struct cms_enum_info * info,
				  unsigned int len);
  CMS_STATUS endEnumerationDLA (const char *name, const struct cms_enum_info * info,
				int &len, int maxlen);

  CMS_STATUS update_dla_length_with_name (const char *, int &len);

  CMS_STATUS setBufferForDiff (void *diffbuf, size_t diffbuffsize);

  long check_type_info (long type, void *buffer, const char *nsname,
			cms_symbol_lookup_function_t symbol_lookup_function,
			const char **namelist,
			const long *idlist,
			const size_t * sizelist,
			long list_length, long max_name_length);

  void rewind ();
  int get_encoded_msg_size ();
  CMS_STATUS startSchemaGen (void);
  CMS_STATUS cancelSchemaGen (void);
  int is_schema_ready (void);
  int make_xml_pretty;
  void recheck_properties(void);

protected:
    friend class CMS;

    CMS_XML_UPDATER (CMS *);
    virtual ~ CMS_XML_UPDATER ();
  const char *findNodeString (const char *);
  int compareAttributeForDiff (const char *, void *x, size_t xsize);
  int compareForDiff (const char *, void *x, size_t xsize);
  const char *binaryToHexConvert (const char *, int len);
  const char *hexToBinaryConvert (const char *, int len);
  const char *insertRefs (const char *, int len);
  const char *removeRefs (const char *);

  //int decideToHexOrNotToHex(const char *name, int len);
  int max_occurs;
  char max_occurs_string[40];
  void decode_len (const char *name, int &len);



  char **stringstofree;
  int maxstringstofree;
  int numstringstofree;
  char *stringspacebegin;
  char *stringspaceend;
  size_t stringspacesize;
  size_t stringspaceused;
  int schema_gen_mode;
  void *base;
  void *diffbuff;
  size_t diffbuff_size;


  char autovarname[20];
  int autovarnum;
  char valuestring[512];
  char whitespace[50];

  int class_count;
  int base_class_count;
  int inside_classvar_not_found;
  int last_var_was_struct;
  char *structisempty;
  char *inside_dla;
  char *inside_unbounded;
  char *elementtablevel;
  int maxclasscount;
  int schema_type_index;

  int maxleftoffnodes;
  const struct cms_enum_info *lastenuminfo;
  const char *lastenumname;
  int enumarraylen;
  char arrayvarname[256];

  int countNodes (const char *name);

  int inside_unbounded_array;

  struct XML_NODE *findNodeWithProperty (const char *name,
			      const char *propertyname,
			      const char *propertyValue);
  struct XML_NODE *findNode (const char *);
  const char *fullName (void *node);
  char *fakeStrdup (const char *);
  char *stringSpaceAlloc (long);
  
  struct XML_NODE *currentNode;
  struct XML_NODE *currentElement;
  struct XML_NODE *schemaNode;
  struct XML_NS *xsdNs;
  struct XML_NS *normalNs;
  void **leftOffNodes;
  void **leftOffElements;
  struct XML_NODE *mainElementNode;
  struct XML_DOC *doc;

  void **unbounded_struct_array_bases;
  void **unbounded_struct_array_diffbuffs;
  size_t *unbounded_struct_array_diffbuff_sizes;
  int unbounded_struct_array_count;
  int max_unbounded_struct_array_count;
  int content_set_for_this_class;
  const char *inside_xml_declaration_addition;
  const char *after_xml_declaration_addition;
  const char *inside_root_start_addition;
  const char *ns_href;
  const char *ns_prefix;
  bool ns_changed;

private:
  CMS_XML_UPDATER(const CMS_XML_UPDATER &);
  CMS_XML_UPDATER &operator=(const CMS_XML_UPDATER &);

};




#endif
// !defined(CMS_XML_UP_HH)
