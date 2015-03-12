/*
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*/

/*!
  \file servoMobJA_v2.hh

  \brief NML declarations for vehicle servo level command, status, settings.

  Synopsis:
  #include <rcs.hh>
  #include "moastNmlOffsets.hh"
*/

#ifndef SERVO_MOB_JA_HH
#define SERVO_MOB_JA_HH

//#include <rcs.hh>		// RCS_CMD_MSG, CMS, PM_CARTESIAN
#include "moastTypes.hh"	// MOAST_WORLD_NAME_LEN
#include "moastNmlOffsets.hh"

#define SERVO_MOB_JA_CMD_NAME "servoMobJACmd"
#define SERVO_MOB_JA_STAT_NAME "servoMobJAStat"
#define SERVO_MOB_JA_CFG_NAME "servoMobJACfg"
#define SERVO_MOB_JA_SET_NAME "servoMobJASet"

#define SERVO_MOB_JA_CMD_BASE             (SERVO_MOB_JA_BASE)
#define SERVO_MOB_JA_STAT_BASE            (SERVO_MOB_JA_BASE + 100)
#define SERVO_MOB_JA_CFG_BASE             (SERVO_MOB_JA_BASE + 200)
#define SERVO_MOB_JA_SET_BASE             (SERVO_MOB_JA_BASE + 300)

#define SERVO_MOB_JA_CMD_INIT_TYPE        (SERVO_MOB_JA_CMD_BASE + 1)
#define SERVO_MOB_JA_CMD_ABORT_TYPE       (SERVO_MOB_JA_CMD_BASE + 2)
#define SERVO_MOB_JA_CMD_HALT_TYPE        (SERVO_MOB_JA_CMD_BASE + 3)
#define SERVO_MOB_JA_CMD_SHUTDOWN_TYPE    (SERVO_MOB_JA_CMD_BASE + 4)
#define SERVO_MOB_JA_CMD_ACKERMAN_TYPE    (SERVO_MOB_JA_CMD_BASE + 5)
#define SERVO_MOB_JA_CMD_SKID_TYPE        (SERVO_MOB_JA_CMD_BASE + 6)
#define SERVO_MOB_JA_CMD_FLIP_SKID_TYPE   (SERVO_MOB_JA_CMD_BASE + 7)
#define SERVO_MOB_JA_CMD_OMNI_TYPE        (SERVO_MOB_JA_CMD_BASE + 8)
#define SERVO_MOB_JA_CMD_SUBMARINE_TYPE   (SERVO_MOB_JA_CMD_BASE + 9)
#define SERVO_MOB_JA_CMD_ROTARY_WING_TYPE (SERVO_MOB_JA_CMD_BASE + 10)

#define SERVO_MOB_JA_STAT_TYPE            (SERVO_MOB_JA_STAT_BASE + 1)
#define SERVO_MOB_JA_STAT_GRD_VEH_TYPE    (SERVO_MOB_JA_STAT_BASE + 2)
#define SERVO_MOB_JA_STAT_AIR_BOT_TYPE    (SERVO_MOB_JA_STAT_BASE + 3)

#define SERVO_MOB_JA_CFG_CYCLE_TIME_TYPE  (SERVO_MOB_JA_CFG_BASE + 1)
#define SERVO_MOB_JA_CFG_DEBUG_TYPE       (SERVO_MOB_JA_CFG_BASE + 2)
#define SERVO_MOB_JA_CFG_TELEPORT_TYPE    (SERVO_MOB_JA_CFG_BASE + 3)

#define SERVO_MOB_JA_SET_TYPE             (SERVO_MOB_JA_SET_BASE + 1)
#define SERVO_MOB_JA_SET_GRD_VEH_TYPE     (SERVO_MOB_JA_SET_BASE + 2)
#define SERVO_MOB_JA_SET_AIR_BOT_TYPE     (SERVO_MOB_JA_SET_BASE + 3)

//// Flippers messages
#define SERVO_MOB_JA_FLIPPER_TYPE_MAX 4

typedef struct SERVO_MOB_JA_FLIPPER_SET
{
  moastFlipperType fType;
  double minAngle;
  double maxAngle;
  double length;
  double width;
  PM_CARTESIAN location;
  PM_RPY orientation;
}ServoMobJAFlipperSet;

typedef struct SERVO_MOB_JA_FLIPPER_MSG
{
  moastFlipperType fType;
  double flipperAngle;
}ServoMobJAFlipperMsg;


// SERVO_MOB_JA_CMD_XXX
class ServoMobJACmdInit:public RCS_CMD_MSG {
public:
  ServoMobJACmdInit():RCS_CMD_MSG(SERVO_MOB_JA_CMD_INIT_TYPE,
				   sizeof(ServoMobJACmdInit)) {};
  void update(CMS *);
};

class ServoMobJACmdAbort:public RCS_CMD_MSG {
public:
  ServoMobJACmdAbort():RCS_CMD_MSG(SERVO_MOB_JA_CMD_ABORT_TYPE,
				    sizeof(ServoMobJACmdAbort)) {};
  void update(CMS *);
};

class ServoMobJACmdHalt:public RCS_CMD_MSG {
public:
  ServoMobJACmdHalt():RCS_CMD_MSG(SERVO_MOB_JA_CMD_HALT_TYPE,
				   sizeof(ServoMobJACmdHalt)) {};
  void update(CMS *);
};

class ServoMobJACmdShutdown:public RCS_CMD_MSG {
public:
  ServoMobJACmdShutdown():RCS_CMD_MSG(SERVO_MOB_JA_CMD_SHUTDOWN_TYPE,
				   sizeof(ServoMobJACmdShutdown)) {};
  void update(CMS *);
};


class ServoMobJACmdSkid:public RCS_CMD_MSG {
public:
  ServoMobJACmdSkid():RCS_CMD_MSG(SERVO_MOB_JA_CMD_SKID_TYPE,
				   sizeof(ServoMobJACmdSkid)) {};
  void update(CMS *);
  double wLeft;			// left wheel speed, [rad/s]
  double wRight;		// right wheel speed, [rad/s]
};

class ServoMobJACmdFlipSkid:public RCS_CMD_MSG {
public:
  ServoMobJACmdFlipSkid():RCS_CMD_MSG(SERVO_MOB_JA_CMD_FLIP_SKID_TYPE,
				   sizeof(ServoMobJACmdFlipSkid)) {};
  void update(CMS *); 
  double wLeft;			// left wheel speed, [rad/s]
  double wRight;		// right wheel speed, [rad/s]
  //! Cmds to control the angle of flippers on platform
  //! fCmd_length == the number of flipper messages in array
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(ServoMobJAFlipperMsg, fCmd, SERVO_MOB_JA_FLIPPER_TYPE_MAX);
};

class ServoMobJACmdSubmarine:public RCS_CMD_MSG {
public:
  ServoMobJACmdSubmarine():RCS_CMD_MSG(SERVO_MOB_JA_CMD_SUBMARINE_TYPE,
				   sizeof(ServoMobJACmdSubmarine)) {};
  void update(CMS *);
  double propeller;
  double rudder;
  double sternPlane;
};


class ServoMobJACmdAckerman:public RCS_CMD_MSG {
public:
  ServoMobJACmdAckerman():RCS_CMD_MSG(SERVO_MOB_JA_CMD_ACKERMAN_TYPE,
				   sizeof(ServoMobJACmdAckerman)) {};
  void update(CMS *);
  double velocity;	// linear velocity  [rads/s]
  double steerAngle;    // steering angle of front tire [rads]
  double crabAngle;     // steering angle of rear tire [rads]
};

class ServoMobJACmdOmni:public RCS_CMD_MSG {
public:
  ServoMobJACmdOmni():RCS_CMD_MSG(SERVO_MOB_JA_CMD_OMNI_TYPE,
				   sizeof(ServoMobJACmdOmni)) {};
  void update(CMS *);
  double vLinear;     //linear velocity [rads/s]
  double vLateral;    //lateral velocity [rads/s]
  double rot;           //rotational velocity [rads/s]
};

class ServoMobJACmdRotaryWing:public RCS_CMD_MSG {
public:
  ServoMobJACmdRotaryWing():RCS_CMD_MSG(SERVO_MOB_JA_CMD_ROTARY_WING_TYPE,
				   sizeof(ServoMobJACmdRotaryWing)) {};
  void update(CMS *);
  double vLinear;     //linear velocity
  double vLateral;    //lateral velocity 
  double vVertical;   //Vertical Velocity
  double vRotation;   //Rotational Velocity
};

// SERVO_MOB_JA_STAT
class ServoMobJAStat:public RCS_STAT_MSG_V2 {
public:
  ServoMobJAStat():RCS_STAT_MSG_V2(SERVO_MOB_JA_STAT_TYPE, sizeof(ServoMobJAStat)) {};
  ServoMobJAStat(int type, int size):RCS_STAT_MSG_V2(type, size) {};
  
  void update(CMS *);
  double time;
  int error;			// was in stat_msg_v2
  int heartbeat;		//!< incremented once each cycle
  //! vehicle battery voltage
  float batteryVolt;
  //! fuel level, range [0,1] 0 - empty, 1 - full
  float fuelLevel;
  //! headlight 0 - off,  1 - on 
  int headLights;
}; 

// SERVO_MOB_JA_STAT
class ServoMobJAStatGrdVeh:public ServoMobJAStat {
public:
  ServoMobJAStatGrdVeh():ServoMobJAStat(SERVO_MOB_JA_STAT_GRD_VEH_TYPE, sizeof(ServoMobJAStatGrdVeh)) {};
  
  void update(CMS *);
  double steerAngle;
  double crabAngle;
  //! Reports the angle of flippers on platform
  //! fCmd_length == the number of flipper messages in array
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(ServoMobJAFlipperMsg, fStat, SERVO_MOB_JA_FLIPPER_TYPE_MAX);
}; 


// SERVO_MOB_JA_STAT
class ServoMobJAStatAirBot:public ServoMobJAStat {
public:
  ServoMobJAStatAirBot():ServoMobJAStat(SERVO_MOB_JA_STAT_AIR_BOT_TYPE, sizeof(ServoMobJAStatAirBot)) {};
  
  void update(CMS *);
  double vRotation;  //rotational velocity
  double vLateral;   //lateral velocity
  double vLinear;    //linear velocity
  double vVertical;  //vertical velocity
}; 



/*! 
  ServoMobJASet is the class that holds the quasi-static vehicle settings,
  those that change infrequently and in response to vehicle configuration
  messages.
 */
class ServoMobJASet : public RCS_STAT_MSG {
public:

  ServoMobJASet():RCS_STAT_MSG(SERVO_MOB_JA_SET_TYPE, sizeof(ServoMobJASet)) {};
  ServoMobJASet(int type, int size):RCS_STAT_MSG(type, size) {};
  
  void update(CMS *);
  double cycleTime;		//!< cycle time, in seconds, of mobility servo
  int debug;			//!< debug level  
  /*! Name of world that vehicle is operating, yf real world then worldName = "live"
    else name of the simulated world  */
  // char worldName[MOAST_WORLD_NAME_LEN];

  //! Name of the platform 
  char platformName[MOAST_PLATFORM_NAME_LEN]; 
  /*! length, width and height are the bounding box around the vehicle,
    origin is at center. Units are [m]. */
  double length;
  double width;
  double height;
  //! mass in [g]
  double mass;
  //! center of gravity wrt origin, in [m]
  PM_CARTESIAN cg;
  moastSteerType steerType;
  moastRobotType botType;

  //! Number of effecters mounted on platform
  // If effCount==0, servo echelon will not open the effecter nml channels
  int effCount;
  //! Number of sensors mounted on platform
  int senCount;
  //! Number of mission packages mounted on platform
  // Indicates which mission package nml channels are valid
  int misCount;
};


class ServoMobJASetGrdVeh : public  ServoMobJASet {
public:
  ServoMobJASetGrdVeh():ServoMobJASet(SERVO_MOB_JA_SET_GRD_VEH_TYPE, sizeof(ServoMobJASetGrdVeh)) {};
  
  void update(CMS *);
  //! steering mechanism of platform
  double  maxWheelRot; 
  double  maxTorque;
  double  wheelSeparation;
  double  wheelRadius;
  double  wheelBase;
  double  maxSteerAngle;
  double  maxCrabAngle;
  double  minTurningRadius;
  //! Reports the presence of configuration of flippers on platform
  //! fSet_length == the number of flipper messages in array
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(ServoMobJAFlipperSet, fSet, SERVO_MOB_JA_FLIPPER_TYPE_MAX);
};


class ServoMobJASetAirBot : public  ServoMobJASet {
public:
  ServoMobJASetAirBot():ServoMobJASet(SERVO_MOB_JA_SET_AIR_BOT_TYPE, sizeof(ServoMobJASetAirBot)) {};
  
  void update(CMS *);
  //! steering mechanism of platform
  double  maxLatVel;  // maximum linear velocity
  double  maxLinVel;  // maximum lateral velocity
  double  maxRotVel;  // maximum rotational velocity
  double  maxVertVel; // maximum vertical velocity
};

/*!
  This settings message sets the cycle time for the servo mobility controller.
*/
class ServoMobJACfgCycleTime : public RCS_CMD_MSG {
public:
  ServoMobJACfgCycleTime():RCS_CMD_MSG
  (SERVO_MOB_JA_CFG_CYCLE_TIME_TYPE,
   sizeof(ServoMobJACfgCycleTime)) {};
  
  void update(CMS *);
  double cycleTime;
};

#define SERVO_MOB_JA_DEBUG_ALL 0xFFFFFFFF

class ServoMobJACfgDebug:public RCS_CMD_MSG {
public:
  ServoMobJACfgDebug():RCS_CMD_MSG
  (SERVO_MOB_JA_CFG_DEBUG_TYPE,
   sizeof(ServoMobJACfgDebug)) {};
  
  void update(CMS *);
  int debug;			// debug mask
};

/*!
  This command will cause the vehicle to jump is global coordinates
  to the given value 
*/
class ServoMobJACfgTeleport : public RCS_CMD_MSG {
public:
  ServoMobJACfgTeleport():RCS_CMD_MSG
  (SERVO_MOB_JA_CFG_TELEPORT_TYPE,
   sizeof(ServoMobJACfgTeleport)) {};
  
  void update(CMS *);
  //! Point in global coordinates to teleport to
  PM_POSE goal;
};

extern int servoMobJA_format(NMLTYPE type, void *buf, CMS * cms);
extern const char * servoMobJA_symbol_lookup(long type);
extern const char * servoMobJA_enum_moastSteerType_symbol_lookup(long v);
extern const char * servoMobJA_enum_moastRobotType_symbol_lookup(long v);
extern const char *servoMobJA_enum_moastFlipperType_symbol_lookup(long v);

#endif // SERVO_MOB_JA_HH

