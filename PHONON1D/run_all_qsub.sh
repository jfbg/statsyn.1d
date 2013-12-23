#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_001_0000km_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_001_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_002_0000km_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_002_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_003_0000km_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_003_5Hz.sh
sleep 1
