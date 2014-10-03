#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_CS_2330B_1000km_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_CS_2331B_1000km_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_CS_2332B_1000km_5Hz.sh
sleep 1
