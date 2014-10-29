#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_B_021_rd025_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_B_021_rd025_1000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_B_021_rd050_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_B_021_rd050_1000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_B_021_rd100_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_B_021_rd100_1000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_B_021_rd125_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_B_021_rd125_1000km.sh
sleep 1
