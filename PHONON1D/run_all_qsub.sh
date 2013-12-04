#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_RSMOON_001_0000km_noscat.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RSMOON_001_noscat.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RSMOON_012a_0000km_noscat.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RSMOON_012a_noscat.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RVPREM_001_07Hz_0000km_noscat.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RVPREM_001_07Hz_noscat.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RVPREM_001_40Hz_0000km_noscat.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_RVPREM_001_40Hz_noscat.sh
sleep 4
