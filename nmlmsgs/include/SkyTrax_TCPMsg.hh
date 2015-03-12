

#ifndef SKYTRAX_TCPMSG_HH
#define SKYTRAX_TCPMSG_HH

/*
 * This header defines two messages a very complicated one (TCPMsg) 
 * that contains all of the information that was in the original skytrax
 * server message.
 * You can get this info directly by using telnet or nc to port 8888 of
 * HiCass or wherever the SkyTrax server is running..
 *
 ie.

 nc 192.168.1.10 8888 

 * will just repeatedly dump the xml data

<?xml version="1.0" encoding="utf-8"?>
<TCPMsg>
  <MsgType>PosUpdate</MsgType>
  <MsgID />
  <MsgBody d2p1:type="ServerPositionUpdateMsgBody" xmlns:d2p1="http://www.w3.org/2001/XMLSchema-instance">
    <EventCollection>
      <anyType d2p1:type="VehicleStatusEvent">
        <TimeStamp>2009-05-01T15:16:57.765625-04:00</TimeStamp>
        <EventClass>Observed</EventClass>
        <VehicleID>1</VehicleID>
        <X>15.47</X>
        <Y>9.5900000000000034</Y>
        <Orientation>339</Orientation>
        <Heading>90</Heading>
        <LoadHeight>0</LoadHeight>
        <LoadPresent>false</LoadPresent>
        <Speed>0</Speed>
        <Status>1</Status>
        <Q>
          <Value>0</Value>
          <OPMsRead>0</OPMsRead>
          <OPMsUsable>0</OPMsUsable>
          <StdDeviation>0</StdDeviation>
        </Q>
        <ErrorCount>0</ErrorCount>
        <Description>Robot</Description>
        <Length>3</Length>
        <Width>3</Width>
        <OperatorID>1</OperatorID>
        <EventStatusCode>0</EventStatusCode>
      </anyType>
      <anyType d2p1:type="VehicleStatusEvent">
        <TimeStamp>2009-05-01T15:16:57.765625-04:00</TimeStamp>
        <EventClass>Observed</EventClass>
        <VehicleID>2</VehicleID>
        <X>0</X>
        <Y>0</Y>
        <Orientation>0</Orientation>
        <Heading>0</Heading>
        <LoadHeight>0</LoadHeight>
        <LoadPresent>false</LoadPresent>
        <Speed>0</Speed>
        <Status>0</Status>
        <Q>
          <Value>0</Value>
          <OPMsRead>0</OPMsRead>
          <OPMsUsable>0</OPMsUsable>
          <StdDeviation>0</StdDeviation>
        </Q>
        <ErrorCount>0</ErrorCount>
        <Description>ATRV</Description>
        <Length>3</Length>
        <Width>3</Width>
        <OperatorID>1</OperatorID>
        <EventStatusCode>0</EventStatusCode>
      </anyType>
    </EventCollection>
  </MsgBody>
</TCPMsg

*/



// This comment changes the behavior of CodeGen.
// generate_symbol_lookups=true

#include "rcs.hh"
#include "nmlOffsets.hh"

enum SkyTrax_NmlMsg_Type_Enum 
  {
    TCPMsg_TYPE = SKYTRAX_DATA_BASE,
    SIMPLE_POS_TYPE
  };


struct OpmInfo {
  int Value;
  int OPMsRead;
  int OPMsUseable;
  double StdDeviation;
};
  
struct VehicleStatusEvent
{
  char TimeStamp[80];
  char EventClass[80];
  int VehicleID;
  double X;
  double Y;
  double Orientation;
  double Heading;
  double LoadHeight;
  bool LoadPresent;
  double Speed;
  int Status;
  struct OpmInfo Q;
  int ErrorCount;
  char Description[80];
  double Length;
  double Width;
  int OperatorID;
  int EventStatusCode;
};

struct EventCollectionType {
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(struct VehicleStatusEvent,anyType,10);
};

struct MsgBodyType
{
  struct EventCollectionType EventCollection;
};

class TCPMsg : public NMLmsg 
{
public:
  TCPMsg();

  char MsgType[80];
  char MsgID[80];
  struct MsgBodyType MsgBody;

  void update(CMS *);
};

class SIMPLE_POS : public NMLmsg
{
public:
  SIMPLE_POS();

  void update(CMS *);
  double timeStamp;
  double x,y,yaw;
};


#define SKYTRAX_DATA_REV "$Rev: 623 $"


#ifndef UNUSED
#ifdef __GNUC__
#define UNUSED __attribute__ ((unused ))
#else
#define UNUSED 
#endif
#endif

static const char UNUSED *SKYTRAX_DATA_HEADER_FILE=__FILE__;
  
extern int SkyTrax_TCPMsg_format(NMLTYPE type, void *buffer, CMS *cms);

#endif
// end of #ifndef SKYTRAX_TCPMSG_HH
