#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_001_0000km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_PBASIN_001.sh
sleep 1
