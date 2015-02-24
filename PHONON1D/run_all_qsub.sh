#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_TB_sm_0001.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_TB_sm_0025.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_TB_sm_0050.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_TB_sm_0100.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_TB_sm_0200.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_TB_sm_0500.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_TB_vp_0001.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_TB_vp_0025.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_TB_vp_0050.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_TB_vp_0100.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_TB_vp_0200.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_NBASIN_TB_vp_0500.sh
sleep 1
