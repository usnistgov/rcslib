From szabo@cme.nist.gov Mon Aug  8 08:27:59 1994
Return-Path: <szabo@cme.nist.gov>
Received: from cme.nist.gov (quickmail.cme.nist.gov) by stella.cme.nist.gov (4.1/SMI-3.2-del.7)
	id AA13260; Mon, 8 Aug 94 08:27:55 EDT
Message-Id: <9408081227.AA13260@stella.cme.nist.gov>
Date: 8 Aug 1994 08:27:32 -0500
From: "Sandor Szabo" <szabo@cme.nist.gov>
Subject: Re: recent controller node i
To: huang@cme.nist.gov
Cc: legowik@cme.nist.gov, "John Michaloski" <michalos@stella>,
        proctor@cme.nist.gov, "Will Shackleford" <shackle@stella>
Content-Length: 11456
X-Lines: 250
Status: RO

        Reply to:   RE>recent controller node issues
I believe issue 1 is somewhat more involved.  What we had discussed is whether
a single OIfunction within a Node can handle all OI requirements for any type
of planner.  A problem in defining the OIfunction is that OI requirements vary
and can be quite complex.  Here is a partial list of OI interactions:

1)  noncritical data display - data is displayed to the operator in a
continuous fashion at some specific location on a screen.  The data may be
sampled at any time and the currently displayed data may be overwritten without
concern.  The operator may select what data is displayed.  An example is the
current joint angles or tool frame of a robot.  

2)  critical data display - data is displayed at a screen but it is important
that it not be lost and that the operator see it.  This might be state
information that is changing relatively fast.  The operator does not have the
ability to select whether to display or not (this might cause the operator to
miss important info).  The applications programmer is responsible for ensuring
the data is displayed appropriately.  An example is a lost message (maybe due
to communications failure).

3)  operator interaction - the operator needs to interact with the control
system in a tightly coupled fashion.  Specific prompts are displayed to the
operator (lights, messages, warning buzzers, etc.) and the response dictates
how the controller will proceed.  An example might be designing and
constructing a bridge from a CAD model.  An ingenious company has developed an
off-line system that allows designer to design a bridge and develop plans for
how the bridge is constructed.  During construction a planner helps the
construction crew build the bridge based upon the plans created by the bridge
designers.  Real world conditions may require the plan be modified.  The
planner interacts with the site manager and allows various changes in the
field.  Some changes, that don't exceed kinematic or dynamic constraints, are
allowed and the manager is told to continue erecting the bridge.  Other changes
require that the planner halt construction until the bridge designers are
consulted.

What is the model for implementing these interactions?  Where does the code go?
 What interfaces and libraries are used to simplify coding?  Who is responsible
for developing the code?  How are the impacts of changes to the system
minimized?

One of the goals of the consolidation project (and the Consolidated Robot
Controller, CRC) was to build a programming environment that allowed an
application programmer to select planners (or plans) from a library based upon
the application.  Each planner might have it's own OI requirements (i.e., a
device and planner specific code to interact with the device) which are
isolated in plannerOi routines.  The CRC generic node template has the
responsibility to call the planner when the appropriate command was received. 
An example is the joystick planner for the Consolidate Robot Controller.  The
call to the joystickPlanner consists of:

void
  joyPlanner()
{
  /* see if planner needs to initialized */
  if( !joyInitialized )
  {
    joyInit();
    joyInitialized = TRUE;
  }

  if( primNewCommand )
    joyNewCmd();

  joyInterpCmd();               /* interpret key commands */
  joyDecelerate();              /* decelerate inactive axes */
  joyCalcNewJoints();           /* calculate new position */
  joyUpdatePanel(joyUpdatePtr,
                 joyUpdateCnt,
                 &joyUpdateIndex); /* update the current control panel */
}

Code that is specific to the OI is in joyInterpCmd() (handles requests such as
selecting coordinate frame, modify speed, etc. and puts it into the format
required by the planner) and joyUpdatePanel() (determines how the data is
displayed on the terminal).  The routines joyDecelerate() and
joyCalcNewJoints() are essentially the plans that shape the robot trajectory
based on inputs (called key commands) from the operator terminal device.  The
joyDecelerate routine is interesting since it reflects how the plan must be
modified based on the characteristics of the OI, in this case, if no command is
received within some period, it is assumed that the operator has released the
joystick (stopped sending keys) and that the robot should slow down along the
current specified trajectory (different devices may handle how a joystick is
released in different manners).

Different people may implement this functionality in different ways.  Our
approach reflects two design considerations:
1)  We want robot behavior to be concentrated in the planning and execution
modules.
2)  Where behavior is dependent on OI interactions, those interactions appear
close to the planning and execution modules.

Within the plannerOI routines (i.e., joyInterpCmd and joyUpdatePanel) are the
interactions with a specific device, in this case a hand held terminal.  All
interactions with a hand held terminal (HT) take place through a defined
interface and are serviced by the htServer (see Handheld Terminal Library
Guide).  Our goals were to allow several nodes to interact with the HT and to
allow for several types of hand held or dumb terminal devices.  A terminal is
defined as a keyboard and screen (a touch screen is mapped in to additional
keys).  We developed an interface which allows any program to make requests to
display data on the screen (for fixed periods of time), to display screen
prompts that allow the operator to enter string messages, to change panels
(Panels consist of screens which provide fixed buttons, graphics and messages)
and to receive key strokes (which are often interpreted as commands).

How do you program the HT.  Well our assumption was that there would be
programmers responsible for developing the plans for each node in the
controller.  Each programmer would design the necessary operator interfaces and
make the appropriate OI calls.  If there were critical interactions between the
operator and the planner, they were handled within the plan.  One problem with
this distributed approach is conflicts in the use of the screen.  We decided
that the conflicts would be resolved by negotiation between the node
programmers.

Getting back to your item 1 regarding OIfunctions, and whether a single
OIfunction within a Node can handle all OI requirements for any type of
planner, I don't believe that options (a) or (b) are necessarily the only
implementations.  I would propose option (c), coupled (not scattered) with
planning and execution.  I think the discussion at Fridays Tool meeting with
ATR brought out this type of OI, when the operator was coupled to a node in the
control system.  I'm not convinced the Node OIfunction can contain all the OI
interactions without making things more complex.  There may be some generic OI
functionality that can be handled by an OIfunction (possibly OI interaction 1
above, where a request for data from a node is handled).  I am for isolating
planning and OI code.  Maybe an alternative approach might be to create planner
and executor classes that contains the OI functions (there may be a way to pass
the functions to a OIfunction but I'm not sure it's worth it).  Another
suggestion I have is to simplify the Node class definition and run time
behavior.  For those who want to add certain types of structure and utilities,
they can build other classes that inherit the more basic Node class
functionality.  I would like to discuss what we did with the CRC as an example
of a more basic node.
Sandor.

--------------------------------------
Date: 8/4/94 5:49 PM
To: Sandor Szabo
From: Hui-Min Huang
Hi:

Does the following writeup pretty much capture what we (some of us) have
discussed about the node template recently?  I thought something like this
helps clarify issues.  It also helps both us and other people understanding how
the template would work.  I'll put it in
/home/manta/rcs/plat.generic/src/rcs/doc/node_issue_log where the
node_sers_guide and CMS_users_guide are.  I volunteer to update it, or, you may
just add new issues in.

Hui

=========================================================
CONTROLLER NODE ISSUE LOG

1.	Implementation of the Operator Interface (OI) function

Options:
(a)	As a module in the node that handles all the I/O between the node and the
devices.
(b)	As a process running concurrently with the node.
(c)	Scattered wherever I/O is needed (e.g., with a planner as Sandor
implemented)

Resolution:
Leave optional between (a) and (b) depending on the execution time and
communication load (see below).

Discussion:  
Option (a) reduces communication bottleneck if the node and the devices
communicate frequently.  Option (b) reduces the cycling time of a controller
node, since the the OI may deal with the following issues and which may make it
a complex function:

*	parsing through command scripts;
*	parsing strings;
*	operator cmd execution timing--execute right away, after the current cmd
done, delay for certain time, delay until another condition met;
*	handshaking between human input and the controller:
	--	operator hitting a key repeatedly, to ignore, to queue, or to barf?
	--	some devices require a status back every cycle.
*	multiple OI devices to a node--arbitration;
*	one device to multiple nodes;

2.	OI to enter at the same level as the corresponding controller or one level
higher.

Options:
(a)	Same level
(b)	On level higher

Resolution:
Use (a)

Discussion:
(a) is more intuitive from a human understanding perspective.  It also
eliminates the need to have to implement an upper level node just for the sake
of handling OI.

3.	Timing for operator to take over control 

Options:
(a)	Allowing operator to take over at any time as long as he switches the mode
to tele-operation.
(b)	Operator to take over only under the preprogrammed and controlled
conditions ("computer determines whether OI is allowed")

Resolution:
TBD

Discussion:
Ideally human should be in charge.  Practically the worry is that, say, if we
allow an operator to take over low level control, the high levels may lose
track of world state and won't be able to resume once the mode switched back to
auto.  The counter argument is that separate status buffers are still used to
send up status during tele mode, along with the updated world model.  I.e., the
upper nodes continue computing everything else other than sending commands
down.

4.	Whether and how to allow human and computer combined execution of plans.

Options:
(a)	The parent decomposes the plan to a series of plans at every point where
human input is expected.  The parent switches mode for the son controller when
sending the plans.
(b)	The parent does not decompose.  Include in the plan the mode switching
statements and allow human to interact closely with the controller, execution
can stop to wait for human input if necessary.
(c)	Templates do not dictate this level of detail.

Resolution:
TBD

Discussion:
Might be application specific.

5.	At what state should the system be brought up?

Options:
(a)	Include in the template a built in Init command that automatically ripples
through all the nodes and gets executed when the system starts up.  The system
will be ready to execute the real missions.
(b)	Requires that the user enter an initialization command before allowing any
other commands.

Resolution:  
(a)

Discussion:
Unless applications have their specific start up procedures.




