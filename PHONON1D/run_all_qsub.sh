#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_BM_EARTH_0100_S3_07_E2.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_BM_EARTH_0100_S3_07_E3.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_BM_EARTH_0100_S3_07_E4.sh
sleep 1
