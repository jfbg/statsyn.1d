#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_051_0000km_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_051_1000km_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_052_0000km_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_052_1000km_5Hz.sh
sleep 1
