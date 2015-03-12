/* 
This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST RCS intelligent mobility software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.
*/ 

// Author Tsai Hong

// Prevent Multiple Inclusion
#ifndef LOG_PLAYBACK_STS_HH
#define LOG_PLAYBACK_STS_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nmlOffsets.hh" //LOG_PLAYBACK_BASE
#define Log_Playback_Sts_name  "Log_Playback_Sts"

// Define the integer type ids.
typedef enum {
  LOG_PLAYBACK_STATUS_TYPE = LOG_PLAYBACK_STS_BASE,
} Log_Playback_StsIdEnum;

// Define the NML Message Classes

struct playback_dir_info
{
  int num_entries;
  int cur_entry;
  double timeStamp;
  double time_since_start;
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char, name,80);
};

struct plaback_dir_list
{
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(struct playback_dir_info, pdlist, 10);
};

// Status Class
class LOG_PLAYBACK_STATUS : public RCS_STAT_MSG
{
public:
  // Normal Constructor
  LOG_PLAYBACK_STATUS();
  
  // CMS Update Function
  void update(CMS *);
  
  /* determine whether the LOG result is working or not.*/
  time_tracker tt;
  double delay;
  bool paused;
  double log_duration;
  double playback_time;
  double time_since_start;
  double time_until_end;
  double log_start_time;
  double log_end_time;
  int last_buffer_num;
  int steps_before_pause;
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char, playback_time_string,256);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char,fname,256);
  struct plaback_dir_list playback_dirs;
};

//! Standard NML format routine
int logPlaybackSts_format(NMLTYPE type, void *buf, CMS *cms);
//! standard NML type lookup routine
const char *logPlaybackSts_symbol_lookup(long type);

#endif 	// MYNAM_HH
