#! /bin/sh

# CD to the User Directory, just in-case we're not already there.
cd /home/shackle/rcslib/examples/rcs_single_dir

if test "x${HOST}" = "x" ;  then
	echo Setting HOST to `hostname`
	HOST=`hostname`;
	export HOST;
fi

if test "x${xterm_cmd}" = "x" ; then
	xterm_cmd="xterm -sb -sl 1000";
fi


pwd

if test "x${RCSLIB_MAIN_DIR}" = "x" ;  then
	RCSLIB_MAIN_DIR=../..
	export RCSLIB_MAIN_DIR;
fi

echo RCSLIB_MAIN_DIR=${RCSLIB_MAIN_DIR}

LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${RCSLIB_MAIN_DIR}/lib;
export LD_LIBRARY_PATH

LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${RCSLIB_MAIN_DIR}:${RCSLIB_MAIN_DIR}/plat/${PLAT}/lib;
export LD_LIBRARY_PATH

echo LD_LIBRARY_PATH=${LD_LIBRARY_PATH}


# Store current host name in NML configuration file
if test "x${HOST}" != "x" ;  then
	if test -f rcs_single_dir.nml.local ; then
		\rm -f rcs_single_dir.nml
		cat rcs_single_dir.nml.local | sed s/localhost/${HOST}/ >rcs_single_dir.nml
	fi
fi


# robot_supersvr
\rm -f robot_supersvr.log
	echo Starting robot_supersvr . . .
	if test ! -x ./robot_supersvr ; then
		echo Can not execute ./robot_supersvr
	fi
	if test "x${USE_XTERM}" = "x1" ; then
		${xterm_cmd} -lf robot_supersvr.log -title "ROBOT_SUPERSVR" -iconic -e ./robot_supersvr &
	else
		./robot_supersvr &
	fi


# rcs_single_dirsvr
\rm -f rcs_single_dirsvr.log
	echo Starting rcs_single_dirsvr . . .
	if test ! -x ./rcs_single_dirsvr ; then
		echo Can not execute ./rcs_single_dirsvr
	fi
	if test "x${USE_XTERM}" = "x1" ; then
		${xterm_cmd} -lf rcs_single_dirsvr.log -title "RCS_SINGLE_DIRSVR" -iconic -e ./rcs_single_dirsvr &
	else
		./rcs_single_dirsvr &
	fi


# wmsvr
\rm -f wmsvr.log
	echo Starting wmsvr . . .
	if test ! -x ./wmsvr ; then
		echo Can not execute ./wmsvr
	fi
	if test "x${USE_XTERM}" = "x1" ; then
		${xterm_cmd} -lf wmsvr.log -title "WMSVR" -iconic -e ./wmsvr &
	else
		./wmsvr &
	fi


# spsvr
\rm -f spsvr.log
	echo Starting spsvr . . .
	if test ! -x ./spsvr ; then
		echo Can not execute ./spsvr
	fi
	if test "x${USE_XTERM}" = "x1" ; then
		${xterm_cmd} -lf spsvr.log -title "SPSVR" -iconic -e ./spsvr &
	else
		./spsvr &
	fi


# bgprimservosvr
\rm -f bgprimservosvr.log
	echo Starting bgprimservosvr . . .
	if test ! -x ./bgprimservosvr ; then
		echo Can not execute ./bgprimservosvr
	fi
	if test "x${USE_XTERM}" = "x1" ; then
		${xterm_cmd} -lf bgprimservosvr.log -title "BGPRIMSERVOSVR" -iconic -e ./bgprimservosvr &
	else
		./bgprimservosvr &
	fi

	sleep 2

# bgmain
\rm -f bgmain.log
	echo Starting bgmain  . . .
	if test ! -x ./bgmain ; then
		echo Can not execute ./bgmain
	fi
	if test "x${USE_XTERM}" = "x1" ; then
		${xterm_cmd}  -lf bgmain.log -title "BGMAIN" -iconic -e ./bgmain &
	else
		 ./bgmain &
	fi

# primmain
\rm -f primmain.log
	echo Starting primmain  . . .
	if test ! -x ./primmain ; then
		echo Can not execute ./primmain
	fi
	if test "x${USE_XTERM}" = "x1" ; then
		${xterm_cmd}  -lf primmain.log -title "PRIMMAIN" -iconic -e ./primmain &
	else
		 ./primmain &
	fi

# robot_supermain
\rm -f robot_supermain.log
	echo Starting robot_supermain  . . .
	if test ! -x ./robot_supermain ; then
		echo Can not execute ./robot_supermain
	fi
	if test "x${USE_XTERM}" = "x1" ; then
		${xterm_cmd}  -lf robot_supermain.log -title "ROBOT_SUPERMAIN" -iconic -e ./robot_supermain &
	else
		 ./robot_supermain &
	fi

# servomain
\rm -f servomain.log
	echo Starting servomain  . . .
	if test ! -x ./servomain ; then
		echo Can not execute ./servomain
	fi
	if test "x${USE_XTERM}" = "x1" ; then
		${xterm_cmd}  -lf servomain.log -title "SERVOMAIN" -iconic -e ./servomain &
	else
		 ./servomain &
	fi

# spmain
\rm -f spmain.log
	echo Starting spmain  . . .
	if test ! -x ./spmain ; then
		echo Can not execute ./spmain
	fi
	if test "x${USE_XTERM}" = "x1" ; then
		${xterm_cmd}  -lf spmain.log -title "SPMAIN" -iconic -e ./spmain &
	else
		 ./spmain &
	fi

# wmmain
\rm -f wmmain.log
	echo Starting wmmain  . . .
	if test ! -x ./wmmain ; then
		echo Can not execute ./wmmain
	fi
	if test "x${USE_XTERM}" = "x1" ; then
		${xterm_cmd}  -lf wmmain.log -title "WMMAIN" -iconic -e ./wmmain &
	else
		 ./wmmain &
	fi
	sleep 2

echo "Run Diagnostics Tool? '(y/n)'"
read diag_confirm

if test "x${diag_confirm}" = "xy" ;  then

	if test "x${diag_cmd}" = "x" ; then
		diag_cmd="java  -jar ../../plat/java/lib/diag_NB.jar";
	fi

	echo Starting Diagnostics Tool . . .
	if test "x${USE_XTERM}" = "x1" ; then
		if test "x${ec}" = "x" -a -x ./.ec ; then
			ec=./.ec;
		fi
		echo ${xterm_cmd} -lf diagapplet.log -title Diagnostics_IO_Term -iconic -e ${ec} ${diag_cmd} rcs_single_dir.cfg
		${xterm_cmd}   -lf diagapplet.log -title Diagnostics_IO_Term -iconic -e ${ec} ${diag_cmd} rcs_single_dir.cfg &
	else
		echo ${diag_cmd} rcs_single_dir.cfg
		${diag_cmd}  rcs_single_dir.cfg &
	fi
	sleep 2

fi

echo "Shutdown?"
echo "Enter "\"y\"" when you are ready to shutdown the controllers and servers,"
echo "or "\"n\"" to exit the script with everything running."
read shutdown_confirm
while test "x${shutdown_confirm}" != "xn" -a "x${shutdown_confirm}" != "xy" ; do 
	echo "Shutdown? '(y/n)'"
	read shutdown_confirm
done

if test "x${shutdown_confirm}" = "xy" ; then


	bgmain_pid=`ps -ea | grep bgmain | grep -v run. | awk '{print $1}' `
	echo Killing bgmain, pid = $bgmain_pid
	kill -INT $bgmain_pid

	primmain_pid=`ps -ea | grep primmain | grep -v run. | awk '{print $1}' `
	echo Killing primmain, pid = $primmain_pid
	kill -INT $primmain_pid

	rcs_single_dirmain_pid=`ps -ea | grep rcs_sing | grep -v run. | awk '{print $1}' `
	echo Killing rcs_single_dirmain, pid = $rcs_single_dirmain_pid
	kill -INT $rcs_single_dirmain_pid

	robot_supermain_pid=`ps -ea | grep robot_su | grep -v run. | awk '{print $1}' `
	echo Killing robot_supermain, pid = $robot_supermain_pid
	kill -INT $robot_supermain_pid

	servomain_pid=`ps -ea | grep servomai | grep -v run. | awk '{print $1}' `
	echo Killing servomain, pid = $servomain_pid
	kill -INT $servomain_pid

	spmain_pid=`ps -ea | grep spmain | grep -v run. | awk '{print $1}' `
	echo Killing spmain, pid = $spmain_pid
	kill -INT $spmain_pid

	wmmain_pid=`ps -ea | grep wmmain | grep -v run. | awk '{print $1}' `
	echo Killing wmmain, pid = $wmmain_pid
	kill -INT $wmmain_pid
	sleep 2
	robot_supersvr_pid=`ps -ea | grep robot_su | grep -v run. | awk '{print $1}' `
	echo Killing robot_supersvr, pid = $robot_supersvr_pid
	kill -INT $robot_supersvr_pid
	rcs_single_dirsvr_pid=`ps -ea | grep rcs_sing | grep -v run. | awk '{print $1}' `
	echo Killing rcs_single_dirsvr, pid = $rcs_single_dirsvr_pid
	kill -INT $rcs_single_dirsvr_pid
	wmsvr_pid=`ps -ea | grep wmsvr | grep -v run. | awk '{print $1}' `
	echo Killing wmsvr, pid = $wmsvr_pid
	kill -INT $wmsvr_pid
	spsvr_pid=`ps -ea | grep spsvr | grep -v run. | awk '{print $1}' `
	echo Killing spsvr, pid = $spsvr_pid
	kill -INT $spsvr_pid
	bgprimservosvr_pid=`ps -ea | grep bgprimse | grep -v run. | awk '{print $1}' `
	echo Killing bgprimservosvr, pid = $bgprimservosvr_pid
	kill -INT $bgprimservosvr_pid

	sleep 2


	bgmain_pid=`ps -ea | grep bgmain | grep -v run. | awk '{print $1}' `
	echo Killing bgmain, pid = $bgmain_pid
	kill -KILL $bgmain_pid

	primmain_pid=`ps -ea | grep primmain | grep -v run. | awk '{print $1}' `
	echo Killing primmain, pid = $primmain_pid
	kill -KILL $primmain_pid

	rcs_single_dirmain_pid=`ps -ea | grep rcs_sing | grep -v run. | awk '{print $1}' `
	echo Killing rcs_single_dirmain, pid = $rcs_single_dirmain_pid
	kill -KILL $rcs_single_dirmain_pid

	robot_supermain_pid=`ps -ea | grep robot_su | grep -v run. | awk '{print $1}' `
	echo Killing robot_supermain, pid = $robot_supermain_pid
	kill -KILL $robot_supermain_pid

	servomain_pid=`ps -ea | grep servomai | grep -v run. | awk '{print $1}' `
	echo Killing servomain, pid = $servomain_pid
	kill -KILL $servomain_pid

	spmain_pid=`ps -ea | grep spmain | grep -v run. | awk '{print $1}' `
	echo Killing spmain, pid = $spmain_pid
	kill -KILL $spmain_pid

	wmmain_pid=`ps -ea | grep wmmain | grep -v run. | awk '{print $1}' `
	echo Killing wmmain, pid = $wmmain_pid
	kill -KILL $wmmain_pid
	sleep 2
	robot_supersvr_pid=`ps -ea | grep robot_su | grep -v run. | awk '{print $1}' `
	echo Killing robot_supersvr, pid = $robot_supersvr_pid
	kill -KILL $robot_supersvr_pid
	rcs_single_dirsvr_pid=`ps -ea | grep rcs_sing | grep -v run. | awk '{print $1}' `
	echo Killing rcs_single_dirsvr, pid = $rcs_single_dirsvr_pid
	kill -KILL $rcs_single_dirsvr_pid
	wmsvr_pid=`ps -ea | grep wmsvr | grep -v run. | awk '{print $1}' `
	echo Killing wmsvr, pid = $wmsvr_pid
	kill -KILL $wmsvr_pid
	spsvr_pid=`ps -ea | grep spsvr | grep -v run. | awk '{print $1}' `
	echo Killing spsvr, pid = $spsvr_pid
	kill -KILL $spsvr_pid
	bgprimservosvr_pid=`ps -ea | grep bgprimse | grep -v run. | awk '{print $1}' `
	echo Killing bgprimservosvr, pid = $bgprimservosvr_pid
	kill -KILL $bgprimservosvr_pid

fi

