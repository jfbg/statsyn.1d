#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_FSMOON_001_0000km_noscat.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_FSMOON_001_noscat.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_FSMOON_012a_0000km_noscat.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_FSMOON_012a_noscat.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_FVPREM_001_07Hz_0000km_noscat.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_FVPREM_001_07Hz_noscat.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_FVPREM_001_40Hz_0000km_noscat.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_FVPREM_001_40Hz_noscat.sh
sleep 4
