#
#This script plots the 2D data
#

cd SRC
make pstack2D.x
cd ..

#
#Set saturation level
#
set SAT = "-10.0 10.0"
set FIG = ./FIGS
set SMOOTH = 0
set dir  = "./OUTPUT/"
set file = "trace.out.prob.1100.LHZ"
set file = "trace.out.prob.0020.LHZ"
./bin/pstack2D << EOF
$dir$file
$SAT
$FIG$file.ps
0
0
0
$SMOOTH
EOF
gv $FIG$file.ps &
