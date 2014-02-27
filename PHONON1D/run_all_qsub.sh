#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_BMC_EARTHPREM_SPIKE_100km_EXP_noATT_AllPs.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_BMC_MOON2LAYERS_700km_noATT_AllPs.sh
sleep 1
