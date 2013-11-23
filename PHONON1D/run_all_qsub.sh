#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_CSMOON_001_noscat_OnlyPS_noatt.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_CSMOON_001_noscat_OnlyPS.sh
sleep 4
