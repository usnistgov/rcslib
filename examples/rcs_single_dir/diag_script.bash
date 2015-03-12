#!/bin/bash

# This is a bash shell script that demonstrates using the 
# diagtool to script sending simple commands to the application.

# Possible commands include
# connect
#  -- wait until connected
# module <modname>
# -- select module
# <cmd> <param>=<value> <param>=<value>
#  -- Send command to module with given pameter values.
#  -- ie goto_point point.x=4
#  -- which commands are available depend on the app and which module is selected.
# status <statusvar>
#  -- Get the given status variable, if no variable is given all status varaibles are printed.


set -x

rm f1 f2
touch f1 f2


java -jar ~/rcslib/plat/java/lib/diagapplet.jar rcs_single_dir.cfg no_banner=true <f1 >f2 &

exec 3<>f1
exec 4<>f2

#sleep 30


echo "connect" >&3
read response <&4
while test "x${response}" = "x" ; do sleep 1; read response <&4 ; done

echo response = $response

echo "module servo" >&3
read response <&4
while test "x${response}" = "x" ; do sleep 1; read response <&4 ; done

echo response = $response

echo "goto_point point.x=4" >&3
read response <&4
while test "x${response}" = "x" ; do sleep 1; read response <&4 ; done

echo response = $response

sleep 3 

echo "status status" >&3
read response <&4
while test "x${response}" = "x" ; do sleep 1; read response <&4 ; done

echo response = $response


echo "status echo_serial_number" >&3
read response <&4
while test "x${response}" = "x" ; do sleep 1; read response <&4 ; done

echo response = $response

sleep 3

echo "halt" >&3
read response <&4
while test "x${response}" = "x" ; do sleep 1; read response <&4 ; done

echo response = $response

echo "status status" >&3
read response <&4
while test "x${response}" = "x" ; do sleep 1; read response <&4 ; done

echo response = $response

sleep 3 

echo "status status" >&3
read response <&4
while test "x${response}" = "x" ; do sleep 1; read response <&4 ; done

echo response = $response

kill %%

exec 3>&-
exec 4>&-

