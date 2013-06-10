#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/ qsub_VPREM_008_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/ qsub_VPREM_008_40Hz.sh
sleep 4
