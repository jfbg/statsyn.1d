#
#This script plots the 2D data
#

#
#Set saturation level
#
set SAT = "-0.0001 0.0001"
set FIG = ./FIGS
set SMOOTH = 0

./bin/pstack2D << EOF
OUTPUT/beyn_lyr_0.1km_sp_0.6.0020.1.1.40.NS.lpz
$SAT
$FIG/SYNTH_LHZ.ps
0
0
0
$SMOOTH
EOF
gv ./FIGS/SYNTH_LHZ.ps &
