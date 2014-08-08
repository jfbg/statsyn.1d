#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_cosscat_001_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_cosscat_001_0050km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSPACE_cosscat_001_1000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSVPREM_cosscat_001_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSVPREM_cosscat_001_0050km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PSVPREM_cosscat_001_1000km.sh
sleep 1
