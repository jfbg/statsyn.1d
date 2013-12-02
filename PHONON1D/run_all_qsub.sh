#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_RSMOON_001_0000km_2.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RSMOON_002a_0000km_2.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RSMOON_004a_0000km_2.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RSMOON_008a_0000km_2.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RSMOON_010b_0000km_2.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RSMOON_011a_0000km_2.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RSMOON_012a_0000km_2.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RSMOON_012c_0000km_2.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RVPREM_001_07Hz_0000km_2.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RVPREM_001_40Hz_0000km_2.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RVPREM_002_07Hz_0000km_2.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RVPREM_002_40Hz_0000km_2.sh
sleep 4
