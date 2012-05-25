#
#This script plots the 2D data
#

#
#Set saturation level
#
set SAT = "-40.0 40.0"
set FIG = ./FIGS
set SMOOTH = 0

./bin/pstack2D << EOF
./trace.out.prob_00.LHZ
$SAT
$FIG/SYNTH_prob_00.LHZ.ps
0
0
0
$SMOOTH
EOF
gv ./FIGS/SYNTH_prob_00.LHZ.ps &

./bin/pstack2D << EOF
./trace.out.prob_20.LHZ
$SAT
$FIG/SYNTH_prob_20.LHZ.ps
0
0
0
$SMOOTH
EOF
gv ./FIGS/SYNTH_prob_20.LHZ.ps &

./bin/pstack2D << EOF
./trace.out.prob_40.LHZ
$SAT
$FIG/SYNTH_prob_40.LHZ.ps
0
0
0
$SMOOTH
EOF
gv ./FIGS/SYNTH_prob_40.LHZ.ps &
