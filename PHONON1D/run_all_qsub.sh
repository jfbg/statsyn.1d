#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_CSIMPLEMOON_Q2000.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_CSIMPLEMOON_Q6500.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_CSIMPLEMOON_basic.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_CSIMPLEMOON_crust.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_CSIMPLEMOON_lcore300.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_CSIMPLEMOON_score300.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_CSIMPLEMOON_vlvl.sh
sleep 1
