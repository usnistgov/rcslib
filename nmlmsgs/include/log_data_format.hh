// generate_symbol_lookups=true

#ifndef LOG_DATA_FORMAT
#define LOG_DATA_FORMAT

#define HAVE_MOAST 1

#include "LadarData.hh"
#include "sicksafetyData.hh"
#include "sickData.hh"
#include "sonarData.hh"
#include "playerPosData.hh"
#include "fwCameraLog_v2.hh"
#include "fwCameraData_v2.hh"
#include "SkyTrax_TCPMsg.hh"
//#include "muChameleonData.hh"
#include "labjackData.hh"

#ifdef HAVE_MOAST
#include "moastTypes.hh"
#include "moastLogRecordSuperCmd.hh"
#include "moastLogRecordSuperSts.hh"
#include "amMobJA.hh"
#include "primMobJA.hh"
#include "servoMobJA.hh"
#include "primDebug.hh"
#include "primSP.hh"
#include "sensorData.hh"
#include "navDataExt.hh"
#include "atrvTune.hh"
#include "nav200Dbg.hh"
#endif

#define LOG_DATA_FORMAT_ID "$Id: log_data_format.hh 875 2010-03-30 18:09:40Z shackle $"
#define LOD_DATA_FORMAT_REV "$Rev: 875 $"

// Tell CodeGen & Diag tool to ignore this function.
#ifndef JAVA_DIAG_APPLET

static inline const char *
log_data_get_compiled_revision(NMLTYPE nml_type)
{
  switch(nml_type)
    {
    case LADAR_DATA_TYPE:
      return (const char *) LADAR_DATA_REV;

      //    case MUCHAMELEON_DATA_TYPE:
      // return (const char *) MUCHAMELEON_DATA_REV;

    case LABJACK_DATA_TYPE:
      return (const char *) LABJACK_DATA_REV;

    case SICK_DATA_TYPE:
      return (const char *) SICK_DATA_REV;

    case SAFETY_SICK_S3000_DATA_TYPE:
      return (const char *) SAFETY_SICK_S3000_DATA_REV;

    case SONAR_DATA_TYPE:
      return (const char *) SONAR_DATA_REV;

    case PLAYERPOS_DATA_TYPE:
      return (const char *) PLAYERPOS_DATA_REV;

    case FW_CAMERA_DATA_TYPE:
      return (const char *) FWCAMERA_DATA_REV;

    case FW_CAMERA_LOG_TYPE:
      return (const char *) FWCAMERA_LOG_REV;

    case TCPMsg_TYPE:
      return (const char *) SKYTRAX_DATA_REV;

    case SIMPLE_POS_TYPE:
      return (const char *) SKYTRAX_DATA_REV;

    default:
      break;
    }
  return(0);
}

static inline const char *
log_data_get_compiled_header(NMLTYPE nml_type)
{
  switch(nml_type)
    {
    case LADAR_DATA_TYPE:
      return (const char *) LADAR_DATA_HEADER_FILE;

    // case MUCHAMELEON_DATA_TYPE:
    //   return (const char *) MUCHAMELEON_DATA_HEADER_FILE;

    case LABJACK_DATA_TYPE:
      return (const char *) LABJACK_DATA_HEADER_FILE;

    case SICK_DATA_TYPE:
      return (const char *) SICK_DATA_HEADER_FILE;

    case SAFETY_SICK_S3000_DATA_TYPE:
      return (const char *) SAFETY_SICK_S3000_DATA_HEADER_FILE;

    case SONAR_DATA_TYPE:
      return (const char *) SONAR_DATA_HEADER_FILE;

    case PLAYERPOS_DATA_TYPE:
      return (const char *) PLAYERPOS_DATA_HEADER_FILE;

    case FW_CAMERA_DATA_TYPE:
      return (const char *) FWCAMERA_DATA_HEADER_FILE;

    case FW_CAMERA_LOG_TYPE:
      return (const char *) FWCAMERA_LOG_HEADER_FILE;

    case TCPMsg_TYPE:
      return (const char *) SKYTRAX_DATA_HEADER_FILE;

    case SIMPLE_POS_TYPE:
      return (const char *) SKYTRAX_DATA_HEADER_FILE;

    default:
      break;
    }
  return(0);
}


static inline double
log_data_get_time_stamp(NMLmsg *msgP)
{
  switch(msgP->type)
    {
    case LADAR_DATA_TYPE:
      return ((LADAR_DATA *)msgP)->timeStamp;

    // case MUCHAMELEON_DATA_TYPE:
    //   return ((MUCHAMELEON_DATA *)msgP)->tt.now;
      
    case LABJACK_DATA_TYPE:
      return ((LABJACK_DATA *)msgP)->tt.now;

    case SICK_DATA_TYPE:
      return ((SICK_DATA *)msgP)->timeStamp;

    case SAFETY_SICK_S3000_DATA_TYPE:
      return ((SAFETY_SICK_S3000_DATA *)msgP)->time;

    case SONAR_DATA_TYPE:
      return ((SONAR_DATA *)msgP)->timeStamp;

    case PLAYERPOS_DATA_TYPE:
      return ((PLAYERPOS_DATA *)msgP)->timeStamp;
      
    case FW_CAMERA_LOG_TYPE:
      return ((FW_CAMERA_LOG *)msgP)->time;

    case SIMPLE_POS_TYPE:
      return ((SIMPLE_POS *)msgP)->timeStamp;

#ifdef HAVE_MOAST
    case SENSOR_DATA_1D_TYPE:
      return ((SensorData1D *)msgP)->collectTimeStart;

    case SENSOR_DATA_ARRAY_TYPE:
      return ((SensorDataArray *)msgP)->arrayElem[0].collectionTime;

    case NAV_DATA_EXT_TYPE:
      return ((NavDataExt*)msgP)->time;
#endif

    }

  // unrecognized msg type
  return (-1.0);
}


typedef enum
{
  DC1394_COLOR_FILTER_BGGR,
  DC1394_COLOR_FILTER_GRBG,
  DC1394_COLOR_FILTER_RGGB,
  DC1394_COLOR_FILTER_GBRG
} bayer_pattern_t;

typedef unsigned char uint8_t;


static inline void
dc1394_bayer_NearestNeighbor (const unsigned char *bayer, 
                              unsigned char       *rgb, 
                              int                  sx, 
                              int                  sy, 
                              int                  tile)
{
  const int bayerStep = sx;
  const int rgbStep = 3 * sx;
  int width = sx;
  int height = sy;
  int blue = tile == DC1394_COLOR_FILTER_BGGR
    || tile == DC1394_COLOR_FILTER_GBRG ? -1 : 1;
  int start_with_green = tile == DC1394_COLOR_FILTER_GBRG
    || tile == DC1394_COLOR_FILTER_GRBG;
  int i, imax, iinc;
  
  /* add black border */
  imax = sx * sy * 3;
  for (i = sx * (sy - 1) * 3; i < imax; i++) {
    rgb[i] = 0;
  }
  iinc = (sx - 1) * 3;
  for (i = (sx - 1) * 3; i < imax; i += iinc) {
    rgb[i++] = 0;
    rgb[i++] = 0;
    rgb[i++] = 0;
  }
  
  rgb += 1;
  width -= 1;
  height -= 1;
  
  for (; height--; bayer += bayerStep, rgb += rgbStep) {
    //int t0, t1;
    const unsigned char *bayerEnd = bayer + width;
    
    if (start_with_green) {
      rgb[-blue] = bayer[1];
      rgb[0] = bayer[bayerStep + 1];
      rgb[blue] = bayer[bayerStep];
      bayer++;
      rgb += 3;
    }
    
    if (blue > 0) {
      for (; bayer <= bayerEnd - 2; bayer += 2, rgb += 6) {
        rgb[-1] = bayer[0];
        rgb[0] = bayer[1];
        rgb[1] = bayer[bayerStep + 1];
        
        rgb[2] = bayer[2];
        rgb[3] = bayer[bayerStep + 2];
        rgb[4] = bayer[bayerStep + 1];
      }
    } else {
      for (; bayer <= bayerEnd - 2; bayer += 2, rgb += 6) {
        rgb[1] = bayer[0];
        rgb[0] = bayer[1];
        rgb[-1] = bayer[bayerStep + 1];
        
        rgb[4] = bayer[2];
        rgb[3] = bayer[bayerStep + 2];
        rgb[2] = bayer[bayerStep + 1];
      }
    }
    
    if (bayer < bayerEnd) {
      rgb[-blue] = bayer[0];
      rgb[0] = bayer[1];
      rgb[blue] = bayer[bayerStep + 1];
      bayer++;
      rgb += 3;
    }
    
    bayer -= width;
    rgb -= width * 3;
    
    blue = -blue;
    start_with_green = !start_with_green;
  }
}

static inline NMLmsg *
log_data_convert_to_playback_msg(NMLmsg *msgP)
{
  switch(msgP->type)
    {
    case FW_CAMERA_LOG_TYPE:
      {
	static FW_CAMERA_DATA dataMsg;
	FW_CAMERA_LOG *logMsgP = (FW_CAMERA_LOG *) msgP;
	dataMsg.nRows = logMsgP->nRows;
	dataMsg.nCols = logMsgP->nCols;
	dc1394_bayer_NearestNeighbor (logMsgP->rawData,// yuv422_buffP,
				      dataMsg.rgbData, //out_rgbImg,
				      logMsgP->nCols, // width
				      logMsgP->nRows, // height
				      DC1394_COLOR_FILTER_GRBG);
	dataMsg.rgbData_length = 3*logMsgP->nRows * logMsgP->nCols;
	return &dataMsg;
      }
      break;

    default:
      break;
    }
  return msgP;
};

#endif 

#define CODEGEN_SELECT_FROM_ALL_FILES 1
#define CODEGEN_GENERATE_FORMAT_ONLY 1

extern int log_data_format(NMLTYPE type, void *buf, CMS *cms);

//! standard NML type lookup routine
extern const char *log_data_symbol_lookup(long type);

#endif
