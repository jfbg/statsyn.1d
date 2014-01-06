#!/bin/tcsh
qsub ./QSUB_SCRIPTS/qsub_CSIMPLEMOON_Q2000_average.sh
qsub ./QSUB_SCRIPTS/qsub_CSIMPLEMOON_Q6500_average.sh
qsub ./QSUB_SCRIPTS/qsub_CSIMPLEMOON_basic_average.sh
qsub ./QSUB_SCRIPTS/qsub_CSIMPLEMOON_crust_average.sh
qsub ./QSUB_SCRIPTS/qsub_CSIMPLEMOON_lcore300_average.sh
qsub ./QSUB_SCRIPTS/qsub_CSIMPLEMOON_score300_average.sh
qsub ./QSUB_SCRIPTS/qsub_CSIMPLEMOON_vlvl_average.sh
