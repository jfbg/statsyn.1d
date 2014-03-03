#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_BM_EARTH_0100_S3_10_E.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_BM_EARTH_0100_S3_20_E.sh
sleep 1
