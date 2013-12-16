#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_BM_onlySPIKE_02.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_BM_onlySPIKE_07.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_BM_onlySPIKE_20.sh
sleep 1
