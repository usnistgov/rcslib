/*****************************************************************************
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*****************************************************************************/
/*!
  \file   sensorData.hh
  \brief  A low-level sensor output channel

  This channel outputs various classes of sensor data. These classes include
  a list class that contains a list of non-adjancent outputs,
  a 1D map class that outputs a 1-D array of values and a 2D may
  class that outputs a 2-D array of values;
  World or local coordinates may be specified. The format of 
  PM_CARTESIAN entries is x-north, y-east, z-down.
*/

/*
  The update() and format() commands will be automatically generated
  by the NIST auto-code-writer routine (JAVA-based).  These will be
  found in the file *.cc
*/

#ifndef SENSOR_DATA_HH
#define SENSOR_DATA_HH

#include  <rcs.hh>
#include <moastTypes.hh>
#include  "moastNmlOffsets.hh"
//! The base name of the nml channel for this buffer
/*! This base name will have the vehicle ID appended to it to form the entire name.
  For example, vehicle ID 1 will have a channel nammed "sensorData1" (where
  WM_SENSOROUT_NAME is defined to be "sensorData").
*/

#define SERVO_SP_LINESCAN_BUF_NAME "servoSPLinescan"
#define SERVO_SP_VICTIM_BUF_NAME "servoSPVict"
#define SERVO_SP_RFID_BUF_NAME "servoSPRfid"
#define SERVO_SP_SONAR_BUF_NAME "servoSPSonarArr"
#define SERVO_SP_NAV_BUF_NAME "servoSPNav"


#define PRIM_SP_LINESCAN_NAME "primSPLinescan"
#define AM_SP_MAP_NAME "amSPMap"

//! the version number of this channel
#define SENSOR_DATA_VERSION 1.0
//! maximum number of cells for list message
#define MAX_SENSOR_DATA_LIST 2000
//! maximum number of values in data tuple
#define MAX_SENSOR_DATA_ARRAY_TUPLE 6
//! maximum length of strings in text array elements
#define MAX_SENSOR_DATA_ARRAY_STRLEN 100
//! maximum number of elements in data array
#define MAX_SENSOR_DATA_ARRAY 20
//! maximum number of elements in text array
#define MAX_SENSOR_DATA_TEXT_ARRAY 10
//! maximum size of data array for 1D array message
#define MAX_SENSOR_DATA_1D 762
//! maximum size of data array for 2D array  message (x*y)
#define MAX_SENSOR_DATA_2D 45000
//! The maximum number of 3D sensors that can be expected 
#define MAX_SENSOR_DATA_3D 32

//! maximum size of char for text buffer
#define SENSOR_DATA_TXT_BUFFER_MAX 1024
//! maximum size of string pointer for text buffer
#define SENSOR_DATA_TXT_STR_PTR_MAX 50
//! maximum number of user defined doubles in sensor text message
#define SENSOR_DATA_TXT_VALUE_MAX 100

//! maximum number of sensor data sources on a robot
#define MAX_SENSOR_DATA_BUFFERS 8
//! maximum number of sensor data values for a particular sensor
#define SENSOR_DATA_MAX 256

// channel commands
// DO NOT USE O+ IT DOES NOT WORK!
//! Status contains a list of x,y,f triplets (location, feature)
#define SENSOR_DATA_LIST_TYPE   (SENSOR_DATA_BASE + 1)
//! Status contains an array of double values
#define SENSOR_DATA_ARRAY_TYPE   (SENSOR_DATA_BASE + 2)
//! Status contains an array of string
#define SENSOR_DATA_TEXT_ARRAY_TYPE   (SENSOR_DATA_BASE + 3)
//! Status contains a 1D continuous array of data
#define SENSOR_DATA_1D_TYPE     (SENSOR_DATA_BASE + 4)
//! Status contains a 2D continuous array of data
#define SENSOR_DATA_2D_TYPE     (SENSOR_DATA_BASE + 5)
//! Status contains a text buffer of NULL delimited strings and double values
#define SENSOR_DATA_TXT_TYPE    (SENSOR_DATA_BASE + 6)
//! Status contains a 1D continuous array of range scanner data
#define RANGE_SCANNER_DATA_TYPE  (SENSOR_DATA_BASE + 7)
//! Status contains an array of sensor range values and an array of sensor poses
#define SENSOR_DATA_3D_TYPE (SENSOR_DATA_BASE + 8)
//! Status contains an array of sensor values 
#define SENSOR_DATA_TYPE (SENSOR_DATA_BASE + 9)

typedef enum {
  SERVO_SP_LINESCANNER_SENSOR,
  SERVO_SP_SONAR_SENSOR,
  SERVO_SP_TOUCH_SENSOR,
  SERVO_SP_RFID_SENSOR,
  SERVO_SP_VICTIM_SENSOR,
  SERVO_SP_INU_SENSOR,
  SERVO_SP_INS_SENSOR,
  SERVO_SP_GPS_SENSOR,
  SERVO_SP_ODOMETER_SENSOR,
  SERVO_SP_TACHOMETER_SENSOR,
  SERVO_SP_ENCODER_SENSOR,
  SERVO_SP_GRD_TRUTH_SENSOR,
  SERVO_SP_CAMERA_SENSOR,
  SERVO_SP_ACOUSTIC_SENSOR
} ServoSensorType;

class SensorData : public NMLmsg {
public:
  SensorData() : NMLmsg(SENSOR_DATA_TYPE, sizeof(SensorData)) {};
  void update(CMS *);

  double time;		 //!< timestamp for the data
  ServoSensorType type;	 //!< one of SERVO_SP_ODOMETER,TACHOMETER, ...
  
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, data, SENSOR_DATA_MAX);
};

typedef enum BODY_PART_TYPE
{
  BODY_PART_HEAD_TYPE,
  BODY_PART_CHEST_TYPE,
  BODY_PART_PELVIS_TYPE,
  BODY_PART_LEG_TYPE,
  BODY_PART_FOOT_TYPE, 
  BODY_PART_ARM_TYPE,
  BODY_PART_HAND_TYPE,
  BODY_PART_UNKNOWN_TYPE,
} BodyPartType;


class DataArrayElement
{
public:
  int type;
  int id;
  double collectionTime;
  double dataTuple[MAX_SENSOR_DATA_ARRAY_TUPLE];
};


class SensorDataArrayElement
{
public: 
  int sensorType;
  int sensorID;
  double collectionTime;
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, dataTuple,MAX_SENSOR_DATA_ARRAY_TUPLE);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char,   dataStr,  MAX_SENSOR_DATA_ARRAY_STRLEN);
};


class SensorDataArray:public NMLmsg {
public:
  SensorDataArray():NMLmsg(SENSOR_DATA_ARRAY_TYPE, sizeof(SensorDataArray)) {};
  //! standard NML update function
  void update(CMS *);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(DataArrayElement, arrayElem,
				   MAX_SENSOR_DATA_ARRAY);
};


class SensorDataTextArray:public NMLmsg {
public:
  SensorDataTextArray():NMLmsg(SENSOR_DATA_TEXT_ARRAY_TYPE, sizeof(SensorDataTextArray)) {};
  //! standard NML update function
  void update(CMS *);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(SensorDataArrayElement, dataElem,
				   MAX_SENSOR_DATA_TEXT_ARRAY);
};

// ================================================================

class SensorDataList:public NMLmsg {
public:
  SensorDataList():NMLmsg(SENSOR_DATA_LIST_TYPE, sizeof(SensorDataList)) {
  };
  //! standard NML update function
  void update(CMS *);
  //! are the cells in local or global coordinates?
  bool isGlobal;
  /*! what is the increment between array members. Example, meters between
      cells or angle between returns. Since SensorPoints identify their
      location, however, some applications may not need to use the
      value of increment. 
  */
  double increment;

  /*! maximum range of sensor. SensorPoints whose distance between
      sensorLoc and dataLoc exceeds this should not be used.
      The value should be positive.
  */
  double maxRange;

  /*! minimum range of sensor. SensorPoints whose distance between
      sensorLoc and dataLoc is less than this should not be used.
      The value may be zero but should not be negative.
  */
  double minRange;

  /*!
     Variable length array of sensor points to add. The name of the array is
     "featList", whose maximum length is defined by MAX_WM_SENSOR_LIST.
     NML creates a variable named "featList_length" that contains the number
     of elements in the current message.
   */
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(SensorPoint, featList,
				   MAX_SENSOR_DATA_LIST);
  /*! time that data was collected, in seconds */
  double collectTime;
};

// ================================================================

/*! Class for representing range scanner return data

The scanner is assumed to have indexable pitch. For one data set, the
pitch is constant. The scanner repeatedly moves to another pitch and
scans, making a new data set at each pitch.

*/

class RangeScannerData:public NMLmsg {
public:
  RangeScannerData():NMLmsg(RANGE_SCANNER_DATA_TYPE, sizeof(RangeScannerData)){
  };
  //! standard NML update function
  void update(CMS *);
  /*! pitch (tilt) of range scanner in scanner coordinate system in radians.
      Tilt axis is assumed parallel to Y axis of scanner. Scanner coordinate
      system is assumed to be like vehicle coordinate system (X straight
      ahead, Y right, Z down). Pitch is 0 is when the beam is parallel to
      the X axis. Tilting towards the -Z axis (up) is positive. */
  double pitch;
  //! angle to first data element in radians
  double startAngle;
  //! angle increment between array members in radians
  double increment;
  /*! maximum range of range scanner. If distances[n] is larger than this,
      the beam is assumed not to have hit anything. */
  double maxRange;
  /*! minimum range of the range scanner. If distances[n] is smaller than
      this, the data is assumed to be useless. */
  double minRange;
  /*! Variable length array of hit point distances. The maximum length of
      the distances array is MAX_WM_SENSOR_1D.
      NML creates a variable named "distances_length" that contains the number
      of elements in the current array. Each double represents the
      distance in meters being reported. */
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, distances, MAX_SENSOR_DATA_1D);
  //! time that the data started to be collected, in seconds
  double collectTimeStart;
  //! duration of the data collection (start time to end time), in seconds. 
  double collectTimeDuration;
};

// ================================================================

/*! Class for representing 1D sensor return
 */
class SensorData1D:public NMLmsg {
public:
  SensorData1D():NMLmsg(SENSOR_DATA_1D_TYPE, sizeof(SensorData1D)) {
  };
  //! standard NML update function
  void update(CMS *);
  //! are the cells in local or global coordinates?
  bool isGlobal;
  //! location of start of array, e.g., angle to first data element or location of first grid point
  double start;
  //! what is the increment between array members. Example, meters between cells or angle between returns
  double increment;
  
  /* FIX ME */
  /* MOAST discussion point. Where does this information belong??*/
  //! Max range reported by sensor
  double maxRange;
  //! Min range reported by sensor
  double minRange;
 
  /*!
     Variable length array of feature values to add. The name of the array is
     "featArray", whose maximum length is defined by MAX_WM_SENSOR_1D.
     NML creates a variable named "featArray_length" that contains the number
     of elements in the current message. The contents of each cell is the
     value of the feature being reported
   */
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, featArray, MAX_SENSOR_DATA_1D);

  /*! time that data was collected, in seconds */
  double collectTimeStart;
  double collectTimeDuration;
};

// ================================================================

/*! Class for representing 2D sensor return
  array is packed by rows where a row runs from west to east
  each subsequent row is north of the previous row.
  In location, x is north and y is east
 */
class SensorData2D:public NMLmsg {
public:
  SensorData2D():NMLmsg(SENSOR_DATA_2D_TYPE, sizeof(SensorData2D)) {
  };
  //! standard NML update function
  void update(CMS *);
  //! are the cells in local or global coordinates?
  bool isGlobal;
  //! location of lowerleft of array
  PM_CARTESIAN lowerLeft;
  //! what is the increment between array members. Example, meters between cells or angle between retuns
  double incrementX;
  //! what is the increment between array members. Example, meters between cells or angle between retuns
  double incrementY;
  //! dimensions of map
  int x;
  //! dimensions of map
  int y;
  /*!
     Variable length array of feature values to add. The name of the array is
     "featArray", whose maximum lnegth is defined by MAX_WM_SENSOR_2D.
     NML creates a variable named "featArray_length" that contains the number
     of elements in the current message. The contents of each cell is the
     value of the feature being reported
   */
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, featArray, MAX_SENSOR_DATA_2D);

  /*! time that data was collected, in seconds */
  double collectTime;
};

/*!
  Class for representing an array of sensor return data, where
  each sensor in the array has an independent location. Typically
  used for sets of sonar sensors.
*/

typedef struct {
  double time;			//!< The time of the sensor sample
  double range;			//!< The output of a single range sensor 
  PM_POSE pose;			//!< The mounting pose of a sensor
} SensorData3DElement;

class SensorData3D : public NMLmsg {
public:
  SensorData3D() : NMLmsg(SENSOR_DATA_3D_TYPE, sizeof(SensorData3D)) {};

  void update(CMS *);

  /*!
    The variable length array for the sensor return values, with \a element_length holding how many there actually are.
  */
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(SensorData3DElement, element, MAX_SENSOR_DATA_3D);
};

// ================================================================

/*! Generic data message for representing txt data
    Variable length char array contains strPtr_length strings. 
    The strPtr provides index into txtBuffer where the first character
    of a string is located.  The dVal array provides a variable length array 
    of doubles that provide user defined values.  
 */
class SensorDataText:public NMLmsg {
public:
  SensorDataText():NMLmsg(SENSOR_DATA_TXT_TYPE,
			      sizeof(SensorDataText)) {
  };
  void update(CMS *);
  //! text buffer that contains series of NULL delimited strings
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char, txtBuffer,
				   SENSOR_DATA_TXT_BUFFER_MAX);
  //! Pointers that index into text buffer the first character of each string
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(int, strPtr,
				   SENSOR_DATA_TXT_STR_PTR_MAX);
  //! Variable length array of user defined doubles
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, dVal,
				   SENSOR_DATA_TXT_VALUE_MAX);

};



//! The base name of the nml channel for this buffer
/*! This base name will have the vehicle ID appended to it to form the entire name.
  For example, vehicle ID 1 will have a channel nammed "symbolicData1" (where
  WM_SYMBOLICOUT_NAME is defined to be "symbolicData").
*/
#define SYMBOLIC_DATA_NAME    "symbolicData"
//! the version number of this channel
#define SYMBOLIC_DATA_VERSION 1.0
//! maximum number of entries for list message
#define SYMBOLIC_DATA_LIST_MAX 25
//! maximum number of verticies for each entry in list message
#define SYMBOLIC_DATA_VERT_MAX 100
// channel commands
// DO NOT USE O+ IT DOES NOT WORK!
//! Status contains a list of x,y,f tripplets (location, feature)f
#define SYMBOLIC_DATA_FEATURE_TYPE   (SYMBOLIC_DATA_BASE + 1)
//! Status contains a 1D continuous array of data
#define SYMBOLIC_DATA_ENTITY_TYPE    (SYMBOLIC_DATA_BASE + 2)
//! Status contains a 2D continuous array of data
#define SYMBOLIC_DATA_HAZARD_TYPE    (SYMBOLIC_DATA_BASE + 3)

// ================================================================
// structures and enums
//! Specifies the class of feature
enum FeatClass {
  ROAD,
  STREAM,
  RIVER,
  TREE,
  BUILDING
};

//! specifies if feature is point, line, or polygon
enum FeatStructure {
  MOAST_POINT,
  MOAST_LINE,
  MOAST_POLYGON
};

/*! We'd like to use a union of actual named parameter structures
  but NML doesn't work with unions. So we define an array of doubles,
  and mnemonic index values. The index to use is determined by the 
  FeatClass enum from above
*/
#define SYMBOLIC_DATA_PARAMETERS_MAX 8

//! params for road feature
#define SYMBOLIC_DATA_ROAD_ID  0

//! params for stream feature
#define SYMBOLIC_DATA_STREAM_ID 0

//! params for river feature
#define SYMBOLIC_DATA_RIVER_ID 0

//! params for tree feature
#define SYMBOLIC_DATA_TREE_ID     0
#define SYMBOLIC_DATA_TREE_HEIGHT 1

//! params for building feature
#define SYMBOLIC_DATA_BUILDING_ID     0
#define SYMBOLIC_DATA_BUILDING_HEIGHT 1

//! structure to define individual feature
typedef struct {
  /*!
     vertices contains the locations of the vertices of the feature. The variable
     vertices_length is an autogenerated variable that should be set the number
     of vertices
   */
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(Location2D, vertices, SYMBOLIC_DATA_VERT_MAX);
  /*!
     'parameters' is opaque array, indexed using e.g. SYMBOLIC_DATA_TREE_HEIGHT.
     'parameters_length' is an auto-generated variable that must be filled in
     with the appropriate value.
   */
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, parameters, SYMBOLIC_DATA_PARAMETERS_MAX);
  FeatStructure structure;
  FeatClass type;
  //! should this feature be added or removed
  /*! true for adding feature
     false for deleting feature
     for feature delete, only the feature ID may be sent
   */
  bool addFeat;
} FeatInfo;

enum HazardClass {
  IED
};

/*! We'd like to use a union of actual named parameter structures
  but NML doesn't work with unions. So we define an array of doubles,
  and mnemonic index values. The index to use is determined by the 
  HazardClass enum from above. The max number of features is set by
  SYMBOLIC_DATA_PARAMETERS_MAX.
*/
//! params for ied hazard
//! id of ied
#define SYMBOLIC_DATA_IED_ID  0
//! 0-vehicle_ied, 1-package_ied
#define SYMBOLIC_DATA_IED_CLASS 1
//! 0-no, 1-yes
#define SYMBOLIC_DATA_IED_IS_DETONATED
//! 0-undetected, 1-detected, 2-recognized, 3-disarmed
#define SYMBOLIC_DATA_IED_STATUS

//!  Provides detailed information about individual Hazzards
typedef struct {
  int IEDClass;			// 0-vehicle_ied, 1-vehicle, 2-package_ied, 3-package
  int isDetonated;		// 0- no, 1- yes
  int status;			// 0-undetected, 1-detected, 2-recognized, 3-disarmed
  PM_CARTESIAN location;	// in utm
  int zone;
} IEDInfo;

//! structure to define individual hazard
typedef struct {
  //! location of hazard
  Location2D loc;
  /*!
     'parameters' is opaque array, indexed using e.g. SYMBOLIC_DATA_TREE_HEIGHT.
     'parameters_length' is an auto-generated variable that must be filled in
     with the appropriate value.
   */
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, parameters, SYMBOLIC_DATA_PARAMETERS_MAX);
  HazardClass type;
  //! should this feature be added or removed
  /*! true for adding feature
     false for deleting feature
     for feature delete, only the feature ID may be sent
   */
  bool addHazard;
} HazardInfo;


///////////////////////////////////////////////////////////////////////////
// Classes
/*! Class for sending feature data
 */
class SymbolicDataFeature:public NMLmsg {
public:
  SymbolicDataFeature():NMLmsg(SYMBOLIC_DATA_FEATURE_TYPE,
			       sizeof(SymbolicDataFeature)) {
  };
  //! standard NML update function
  void update(CMS *);
  /*!
     Variable length array of features to add or delete. The name of the array is
     "featList", whose maximum lnegth is defined by SYMBOLIC_DATA_LIST_MAX.
     NML creates a variable named "featList_length" that contains the number
     of elements in the current message. 
   */
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(FeatInfo, featList,
				   SYMBOLIC_DATA_LIST_MAX);
};

/*! Class for representing detected entity data
 */
class SymbolicDataEntity:public NMLmsg {
public:
  SymbolicDataEntity():NMLmsg(SYMBOLIC_DATA_ENTITY_TYPE,
			      sizeof(SymbolicDataEntity)) {
  };
  //! standard NML update function
  void update(CMS *);
  /*!
     Variable length array of feature values to add. The name of the array is
     "featArray", whose maximum lnegth is defined by MAX_WM_SYMBOLIC_1D.
     NML creates a variable named "featArray_length" that contains the number
     of elements in the current message. The contents of each cell is the
     value of the feature being reported
   */
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, entityArray,
				   SYMBOLIC_DATA_LIST_MAX);
};

/*! Class for representing hazards
 */
class SymbolicDataHazard:public NMLmsg {
public:
  SymbolicDataHazard():NMLmsg(SYMBOLIC_DATA_HAZARD_TYPE,
			      sizeof(SymbolicDataHazard)) {
  };
  //! standard NML update function
  void update(CMS *);
  /*!
     Variable length array of feature values to add. The name of the array is
     "featArray", whose maximum lnegth is defined by MAX_WM_SYMBOLIC_2D.
     NML creates a variable named "featArray_length" that contains the number
     of elements in the current message. The contents of each cell is the
     value of the feature being reported
   */
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(HazardInfo, hazardArray,
				   SYMBOLIC_DATA_LIST_MAX);
};



//! standard NML format function
extern int sensorData_format(NMLTYPE type, void *buf, CMS * cms);

//! standard NML command name lookup
extern const char *sensorData_symbol_lookup(long type);

#endif // SENSOR_DATA_HH
