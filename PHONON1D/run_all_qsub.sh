#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_VPREM_081_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_081_40Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_082_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_082_40Hz.sh
sleep 4
