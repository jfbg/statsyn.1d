#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_001n_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_001n_0000km_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_003n_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_003n_0000km_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_004n_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_004n_0000km_5Hz.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_006n_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_006n_0000km_5Hz.sh
sleep 1
