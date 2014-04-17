#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_PSVPREM_062_40Hz_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSVPREM_062_40Hz_0050km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSVPREM_062_40Hz_1000km.sh
sleep 1
