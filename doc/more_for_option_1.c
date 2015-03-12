From szabo@cme.nist.gov Mon Aug  8 14:50:45 1994
Return-Path: <szabo@cme.nist.gov>
Received: from cme.nist.gov (durer.cme.nist.gov) by stella.cme.nist.gov (4.1/SMI-3.2-del.7)
	id AA14122; Mon, 8 Aug 94 14:50:44 EDT
Received: from stella.cme.nist.gov by cme.nist.gov (4.1/SMI-3.2-del.8)
	id AA01004; Mon, 8 Aug 94 14:50:31 EDT
Received: from cme.nist.gov (quickmail.cme.nist.gov) by stella.cme.nist.gov (4.1/SMI-3.2-del.7)
	id AA14117; Mon, 8 Aug 94 14:50:41 EDT
Message-Id: <9408081850.AA14117@stella.cme.nist.gov>
Date: 8 Aug 1994 14:50:09 -0500
From: "Sandor Szabo" <szabo@cme.nist.gov>
Subject: Re: recent controller node 
To: "Will Shackleford" <shackle@cme.nist.gov>
Cc: huang@cme.nist.gov, legowik@cme.nist.gov,
        "John Michaloski" <michalos@stella>, proctor@cme.nist.gov
Content-Length: 7855
X-Lines: 191
Status: RO

        Reply to:   RE>>recent controller node i
I just love written debates.  {For those who don't give a rat's derriere, skip
the rest of this message.}
>>> My comments <<< are listed below.

--------------------------------------
Date: 8/8/94 11:06 AM
To: Sandor Szabo
From: Will Shackleford
----------
X-Sun-Data-Type: text
X-Sun-Data-Description: text
X-Sun-Data-Name: text
X-Sun-Content-Lines: 1


----------
X-Sun-Data-Type: default
X-Sun-Data-Name: OI_arguments
X-Sun-Content-Lines: 87

 

I would like to summarize my arguments against option C of issue 1.

>>> In summary, options a, b, and c are all valid.  Option c addresses the case
where a planner decides when an operator interaction is required, not a generic
OIfunction.  If there are functions that are common to all OI's , then they
should go in a generic OIfunction.  I believe you have a good list of some of
those functions, other functionality may be premature (see comments below). 
Why don't you propose a first set of functionality and we can all agree on
them.<<<

I will illustrate using the following example for each point.

EXAMPLE:
The designer is first told to develop a controller that will allow a person
with one of those old Atari joysticks to control a toy train. The output of
the joystick is to floats corresponding to the deflection in the x and y
directions. (called Jx and Jy)

If she uses method a or b she would develop a node with a command like
move_train at velocity Vx(a command parameter), and an operator interface
module that converts deflections on the joystick to move_train command. The
control
code will see a move_train command from the operator the same as a move_train
command from above.

the OI would include something like

Vx = Jx * Joystick_scale_factor;
 

and the control code would produce a voltage for a digital to analog converter
that(Vdac) drives the train that would look something like

Vdac = Vx* Velocity_to_Voltage;

If she uses method c she would develop a single joystick planner in a node that
get a follow_joystick command with something like

Vdac = Jx * Joystick_to_Voltage;



1. Separate the control of OI devices from the control of Robot Control
Devices.

There should be a large variety of OI devices that can allow the operator to
see or enter the same type of information. (Ex: Most modern programs let me
choose whether to use the mouse to choose commands from a menu or to enter
control keys )

If in our example we decided to use the arrow keys on a keyboard to replace the
joystick and we used either method a or b.

Then we only need to replace  the OI module to produce something like

switch(key)
{
case UP_ARROW;
 Vx = +VELOCITY;
case DOWN_ARROW;
 Vx = -VELOCITY;
}

We don't have to know anything about the joystick OI that we are replacing and
we only have to know the published command interface for the control section of
the node.

However if we used method c we would have to search through the control code
for references to joystick variables and to know how our new code should
operate it is likely that we would have to understand the joystick we were
replacing.

>>> Yes I agree.  In the example code I presented there is a clear separation
between the planner and the OI software.  OI devices can be easily swapped as
long as the interfaces are consistent.   The question was whether the OI
specific software developed for a planner should be separated again and placed
into a generic OIfunction that resides in the Node. <<<

2. Allow superior control modules to reuse control code developed for OI
modules.

Both operators and superior modules are clients of the the node, they request
that the node perform certain tasks, sometimes these tasks may be different but
much more often they will overlap. In fact it is the amount of overlap that can
be achieved that should dictate with which node an operator interacts.

In this example if method a or b was used nothing has to be done to the node at
all, but if method c was used a new planner intended to be used by a superior
module must now be developed.

>>> From our previous discussions, I believe you are proposing that every
operator input message be treated as a new command, i.e., the interface to the
superior is identical to the operator interface.  This is a good idea sometimes
(for example at the top of the hierarchy where the only superior is the
operator).  At lower levels there are many pitfalls to beware of.  The superior
may become out of sync with the subordinate.  Also sending a new command when a
planner  requires operator input will cause the current planner to be
interrupted, its state saved, and then restarted (by a new command from the
operator?).  This may be more difficult to program. <<< 


3. Allow OI modules to reuse control code developed for superior control
modules.

The reuse can also go the other way, if the control node were given a command
like goto_point_on_track using either a or b the OI could be modified
immediately to take advantage of this. Using method c we would need to write an
entirely new planner that gets the point form the operator.

>>>Hopefully you wouldn't have to write an entirely new planner.  Again, I
propose an interface between the planner and the OI software.  If everyone met
the syntax and behavior of the interface, there would only be code to switch
the source of the command.  This is what I did at the Emove level.<<<

4. Allow multiple OI modules for either multiple operators, multiple locations 
for the operator or multiple types of OI devices to be used simultaneously.

A planner developed with a single OI in mind is hard to change to handle if
method c is chosen, however if either method a or b is used the control planner
sees only a single command message at any time regardless of whether that
message
comes from the same source every time or from a different source every cycle.

>>> In any approach, as long as the interface meets the specification, you can
have multiple anythings.  Again, approach C only states that "all" OI code
dosn't go into a generic OIfunction. <<<

5. Allow the systems team to provide more generic utilities. 

If all of the OI is done using modules with a consistent interface there is a
great deal more that the systems team can do. (Utilities for controlling when
OI requests are accepted, linked lists of multiple oi interfaces, opening and
closing communications channels, spawning CMS servers) However if OI is
scattered throughout the control planners there is less that can be done
"generically".

>>> OI isn't scattered thoughout, it exists in clearly defined places in
conjunction with the specifics of a given planner.  The main advantage in C is
that the planner decides when the OI interactions takes place, not the generic
OIfunction.  <<< 

6. Seperate OI and control modules should make each smaller and simpler.
>>><<<

Conclusion:

I believe that all three types of OI from Sandor's message can be handled with
either a or b. So I would suggest the following OI policy.


1. We will not do anything that specifically prevents an applications
programmer from using method c, but if they do they will not have the same
level of support in the node class as if they had used a or b.

2. We should investigate trying to use methods a and b in as many different
types of OI interactions as possible to see if we can actually identify a type
of OI where method c really has significant advantages. (In other words, try a
or b first.)    

>>> a, b or c are all viable options.  Most of the utilities you described look
good, and I hope we can use them in any of the options.  I am concerned about
treating all operator messages as commands and how that will affect the
complexity of planners.   You should give some thought before you spend alot of
time on it.  Any comments from other implementors?<<<





