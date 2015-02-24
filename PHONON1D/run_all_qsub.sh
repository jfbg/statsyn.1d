#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_SL_sm_0005.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_SL_sm_0010.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_SL_sm_0015.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_SL_sm_0030.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_SL_sm_0060.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_SL_vp_0005.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_SL_vp_0010.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_SL_vp_0015.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_SL_vp_0030.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_SL_vp_0060.sh
sleep 1
