#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_PVPREM_001_40Hz_noscat.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSMOON_012a_noscat.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSMOON_011a_noscat.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSMOON_001_noscat.sh
sleep 1
