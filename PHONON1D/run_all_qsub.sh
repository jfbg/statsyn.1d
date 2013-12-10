#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_PSMOON_001_0000km_noscat.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSMOON_001_0030km_noscat.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSMOON_001_1000km_noscat.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSMOON_012a_0000km_noscat.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSMOON_012a_0030km_noscat.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSMOON_012a_1000km_noscat.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PVPREM_001_40Hz_0000km_noscat.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PVPREM_001_40Hz_0030km_noscat.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PVPREM_001_40Hz_1000km_noscat.sh
sleep 1
