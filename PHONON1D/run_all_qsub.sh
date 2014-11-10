#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_B_001x_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_B_001x_1000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_B_021x_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_B_021x_1000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_B_022x_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_B_022x_1000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSVPREM_001x_1000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSVPREM_001x__0000km.sh
sleep 1
