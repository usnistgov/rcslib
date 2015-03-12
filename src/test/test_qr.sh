#! /bin/sh

set -x
export ABORT_ON_RCS_ERROR=1;

find . -type f -perm -0111 -exec killall  -KILL '{}' \;

echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_9.log "
( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_9.log

rm -f qr*.log
rm -f qr*.ok
rm -f qr*.fail
rm -f core core.[0-9]*


# PAUSE_ON_RCS_ERROR=1
# export PAUSE_ON_RCS_ERROR

ABORT_ON_RCS_ERROR=1;
export ABORT_ON_RCS_ERROR;

( ipcs || true )
( netstat -na || true)
( netstat -napee || true)
ps -ae 
ps
sleep 2

date

\rm -f nml_test_qr_nml_svr.running ;
${PROGRAM_LAUNCHER} ./nml_test_qr_nml_svr${EXEEXT} echo echo_nml_svr qrtest.nml qr_nml_svr.log &
while test ! -f ./nml_test_qr_nml_svr.running ; do
    sleep 1
done;

\rm -f nml_test_qr_server.running ;
${PROGRAM_LAUNCHER} ./nml_test_qr_server${EXEEXT} echo echosvr qrtest.nml qrsvr.log &
while test ! -f ./nml_test_qr_server.running ; do
    sleep 1
done;

( ipcs || true )
( netstat -na || true)
( netstat -napee || true)
ipcs
ps -ae 
ps

sleep 5

ipcs
ps -ae 
ps
date

((${PROGRAM_LAUNCHER} ./nml_test_qr_client${EXEEXT} echo remote_echoclnt qrtest.nml qrclnt0.log 3.0 <number.txt || ( sts=$?; echo "QR test failed sts=${sts}"; cat -v qrclnt0.log; touch qr0.fail ; exit ${sts})) && touch qr0.ok) &

((${PROGRAM_LAUNCHER} ./nml_test_qr_client${EXEEXT} echo local_echoclnt qrtest.nml qrclnt1.log 3.0 <number.txt || ( sts=$?; echo "QR test failed sts=${sts}"; cat -v qrclnt1.log; touch qr1.fail ; exit ${sts})) && touch qr1.ok) &

((${PROGRAM_LAUNCHER} ./nml_test_qr_client${EXEEXT} echo remote_echoclnt qrtest.nml qrclnt2.log 3.0 <number.txt || ( sts=$?; echo "QR test failed sts=${sts}"; cat -v qrclnt2.log; touch qr2.fail ; exit ${sts})) && touch qr2.ok) &

((${PROGRAM_LAUNCHER} ./nml_test_qr_client${EXEEXT} echo local_echoclnt qrtest.nml qrclnt3.log 3.0 <number.txt || ( sts=$?; echo "QR test failed sts=${sts}"; cat -v qrclnt3.log; touch qr3.fail ; exit ${sts})) && touch qr3.ok) &

((${PROGRAM_LAUNCHER} ./nml_test_qr_client${EXEEXT} echo remote_echoclnt qrtest.nml qrclnt4.log 3.0 <number.txt || ( sts=$?; echo "QR test failed sts=${sts}"; cat -v qrclnt4.log; touch qr4.fail ; exit ${sts})) && touch qr4.ok) &

((${PROGRAM_LAUNCHER} ./nml_test_qr_client${EXEEXT} echo local_echoclnt qrtest.nml qrclnt5.log 3.0 <number.txt || ( sts=$?; echo "QR test failed sts=${sts}"; cat -v qrclnt5.log; touch qr5.fail ; exit ${sts})) && touch qr5.ok) &


while test '!' -f qr0.ok -a '!' -f qr0.fail -a '!' -f qr1.fail -a '!' -f qr2.fail -a '!' -f qr3.fail -a '!' -f qr4.fail -a '!' -f qr5.fail -a "x" = x`ls core* 2>/dev/null` ; do
  pwd
  ls -l qr?.ok qr?.fail qrclnt?.log
  ps -ae | grep nml_test
  for qrlog in qrclnt?.log ; do
      if test "x${qrlog}" != "x" -a "x${qrlog}" != "xqrclnt?.log" ; then
	  echo $qrlog;
	  ls -l "${qrlog}";
	  tail -n 3 "${qrlog}";
	  ls -l qrsvr.log;
	  tail -n 3 qrsvr.log;
      fi
  done
  sleep 3
done

while test '!' -f qr1.ok -a '!' -f qr0.fail -a '!' -f qr1.fail -a '!' -f qr2.fail -a '!' -f qr3.fail -a '!' -f qr4.fail -a '!' -f qr5.fail -a "x" = x`ls core* 2>/dev/null`; do
  pwd
  ls -l qr?.ok qr?.fail qrclnt?.log
  ps -ae | grep nml_test
  for qrlog in qrclnt?.log ; do
      if test "x${qrlog}" != "x" -a "x${qrlog}" != "xqrclnt?.log" ; then
	  echo $qrlog;
	  ls -l "${qrlog}";
	  tail -n 3 "${qrlog}";
	  ls -l qrsvr.log;
	  tail -n 3 qrsvr.log;
      fi
  done
  sleep 3
done

while test '!' -f qr2.ok -a '!' -f qr0.fail -a '!' -f qr1.fail -a '!' -f qr2.fail -a '!' -f qr3.fail -a '!' -f qr4.fail -a '!' -f qr5.fail -a "x" = x`ls core* 2>/dev/null`; do
  pwd
  ls -l qr?.ok qr?.fail qrclnt?.log
  ps -ae | grep nml_test
  for qrlog in qrclnt?.log ; do
      if test "x${qrlog}" != "x" -a "x${qrlog}" != "xqrclnt?.log" ; then
	  echo $qrlog;
	  ls -l "${qrlog}";
	  tail -n 3 "${qrlog}";
	  ls -l qrsvr.log;
	  tail -n 3 qrsvr.log;
      fi
  done
  sleep 3
done

while test '!' -f qr3.ok -a '!' -f qr0.fail -a '!' -f qr1.fail -a '!' -f qr2.fail -a '!' -f qr3.fail -a '!' -f qr4.fail -a '!' -f qr5.fail -a  "x" = x`ls core* 2>/dev/null` ; do
  pwd
  ls -l qr?.ok qr?.fail qrclnt?.log
  ps -ae | grep nml_test
  for qrlog in qrclnt?.log ; do
      if test "x${qrlog}" != "x" -a "x${qrlog}" != "xqrclnt?.log" ; then
	  echo $qrlog;
	  ls -l "${qrlog}";
	  tail -n 3 "${qrlog}";
	  ls -l qrsvr.log;
	  tail -n 3 qrsvr.log;
      fi
  done
  sleep 3
done

while test '!' -f qr4.ok -a '!' -f qr0.fail -a '!' -f qr1.fail -a '!' -f qr2.fail -a '!' -f qr3.fail -a '!' -f qr4.fail -a '!' -f qr5.fail -a "x" = x`ls core* 2>/dev/null` ; do
  pwd
  ls -l qr?.ok qr?.fail qrclnt?.log
  ps -ae | grep nml_test
  for qrlog in qrclnt?.log ; do
      if test "x${qrlog}" != "x" -a "x${qrlog}" != "xqrclnt?.log" ; then
	  echo $qrlog;
	  ls -l "${qrlog}";
	  tail -n 3 "${qrlog}";
	  ls -l qrsvr.log;
	  tail -n 3 qrsvr.log;
      fi
  done
  sleep 3
done

while test '!' -f qr5.ok -a '!' -f qr0.fail -a '!' -f qr1.fail -a '!' -f qr2.fail -a '!' -f qr3.fail -a '!' -f qr4.fail -a '!' -f qr5.fail  -a "x" = x`ls core* 2>/dev/null` ; do
  pwd
  ls -l qr?.ok qr?.fail qrclnt?.log
  ps -ae | grep nml_test
  for qrlog in qrclnt?.log ; do
      if test "x${qrlog}" != "x" -a "x${qrlog}" != "xqrclnt?.log" ; then
	  echo $qrlog;
	  ls -l "${qrlog}";
	  tail -n 3 "${qrlog}";
	  ls -l qrsvr.log;
	  tail -n 3 qrsvr.log;
      fi
  done
  sleep 3
done

ls -l qr*
date

if test '!' -f qr0.ok -o \
    '!' -f qr1.ok -o \
    '!' -f qr2.ok -o \
    '!' -f qr3.ok -o \
    '!' -f qr4.ok -o \
    '!' -f qr5.ok -o \
    "x" != x`ls core* 2>/dev/null` ; then
    echo "Query Reply tests failed."
    if test "${TEST_QR_DEBUG_ERROR}" != "x" ; then
	echo "Press any key. (Programs still running for testing.) \n"
	read foo;
    fi
    echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_9.log "
    ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_9.log
    sleep 3
    ls -l qr*.ok qr*.fail core*
    echo "Query Reply tests failed."
    exit 99
fi


date

echo " ( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_10.log "
( ${tmpdir}/extra_tests_killnt.sh 2>&1 ) >${tmpdir}/extra_tests_killnt_10.log

echo "${0} (test_qr.sh) ($$) sts=${sts}"

exit ${sts};
