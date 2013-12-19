#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_002_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_002.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_003_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_003.sh
sleep 1
