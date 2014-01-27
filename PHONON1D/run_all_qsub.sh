#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_BM_MOON2LAYERS_SPIKE_noATT_700km.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_CSIMPLEMOON_lcore300_FirstS.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_CSIMPLEMOON_score300_FirstS.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_CSIMPLEMOON_vlvl_FirstS.sh
sleep 1
