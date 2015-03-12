#ifndef STAT_MSG_V2_HH
#define STAT_MSG_V2_HH


#include "stat_msg.hh"
#include "timetracker.hh"

// This is a special hot-comment interpreted by the CodeGenerator 
// to genereate enum symbol lookups ie
// extern const char *RCS_STAT_MSG_V2_enum_RCS_STATUS_symbol_lookup(long v);
// even if it is not included directly used in this file.
//
//generate_all_enum_symbol_lookups=true


enum RCS_ADMIN_STATE {
  RCS_ADMIN_ZERO = 0,
  ADMIN_UNINITIALIZED = 1,
  ADMIN_INITIALIZED  =  2,
  ADMIN_SHUT_DOWN  =    3
};

class RCS_STAT_MSG_V2: public RCS_STAT_MSG
{
private:
  RCS_STAT_MSG_V2() : RCS_STAT_MSG(0,0){};
public:
#ifndef JAVA_DIAG_APPLET
  RCS_STAT_MSG_V2(NMLTYPE t,size_t s) :RCS_STAT_MSG(t,s) {};
#endif
  
  void update(CMS *);

  RCS_ADMIN_STATE admin_state; //default=ADMIN_UNINITIALIZED
  struct time_tracker tt;
  //   int error;   the code for properly updating this was never added,
  // and it is more trouble than I want to mess with especially when 
  // it would make more sense to add an enum with actual meaningful 
  // values than this unknown generic integer.

  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char,message,80)
};

/* 
 * Before June 5, 2009 the symbol lookups were in the library but
 * not prototyped in the header.
 * Use this to ifdef compatibility.
 */
#define RCS_HAVE_STAT_MSG_V2_SYMBOL_LOOKUP 1 

extern const char *RCS_STAT_MSG_V2_enum_RCS_ADMIN_STATE_symbol_lookup(long v);
extern const char *RCS_STAT_MSG_V2_enum_RCS_STATUS_symbol_lookup(long v);

#endif
