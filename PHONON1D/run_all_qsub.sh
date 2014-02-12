#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_004_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_004.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_005_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_005.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_006_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_006.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_004_0000km_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_004_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_005_0000km_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_005_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_006_0000km_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_006_5Hz.sh
sleep 1
