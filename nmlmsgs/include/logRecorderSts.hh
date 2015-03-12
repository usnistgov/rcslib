/* 
This software wags developed at the National Institute of Standards and
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

// generate_symbol_lookups=true
// generate_enum_symbol_lookups=true

// Author Tsai Hong

// Prevent Multiple Inclusion
#ifndef LOG_RECORDER_STS_HH
#define LOG_RECORDER_STS_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nmlOffsets.hh" //LOG_RECORDER_BASE
#define logRecorderSts_name  "logRecorderSts"

// Define the integer type ids.
typedef enum {
  LOG_RECORDER_STATUS_TYPE = LOG_RECORDER_STS_BASE,
} logRecorderStsIdEnum;

// Define the NML Message Classes

struct BufferInfo
{
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char, name,256);
  time_tracker tt;
  double time_diff;
  double max_time_diff;
  double last_time_stamp;
  long last_type;
  long last_size;
  long last_packed_size;
  long total_size;
  long queue_length;
  long max_queue_length;
};

struct BufferInfoList
{
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(struct BufferInfo, bi,20);
};

struct header_item
{
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char, name,256);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char, ID,256);
};

struct header_list
{
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(struct header_item, header,256);
};


// Status Class
class LOG_RECORDER_STATUS : public RCS_STAT_MSG
{
public:
  // Normal Constructor
  LOG_RECORDER_STATUS();
  
  // CMS Update Function
  void update(CMS *);
  
  /* determine whether the LOG result is working or not.*/
  time_tracker tt;
  time_tracker log_tt;
  bool logging;
  bool single_file_mode;
  int frame_number;
  struct BufferInfoList buffer_list;
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char, fname,256);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char, label,64);
  double record_data_start_time;
  double max_record_data_time;
  double min_record_data_time;
  double current_record_data_time; 
  struct header_list headers;
};

//! Standard NML format routine
int logRecorderSts_format(NMLTYPE type, void *buf, CMS *cms);
//! standard NML type lookup routine
const char *logRecorderSts_symbol_lookup(long type);

#define LOG_RECORDER_STS_ID "$Id: logRecorderSts.hh 871 2010-03-03 19:27:36Z shackle $"

#endif 	// MYNAM_HH
