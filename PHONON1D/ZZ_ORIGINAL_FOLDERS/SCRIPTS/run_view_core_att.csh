#!/bin/csh


#
# Convert model to 4km thick layers (scale-lenght of scatterers)
#

cd SRC
make interp_mod.o
cd ..

#
# Set synthetic parameters
#

set ray_par    = "0.0 0.1668 0.2931"
set t_window   = "0 900 3601"
set d_range    = "0 180 361"
set rand_kern  = "105"
set q_depth    = "0."
set model      = "2"
set p_or_s     = "1"
set mx_scat_dp = "10"
set n_phonon   = "4000000"
set prob_00    = 0.00000
set prob_20    = 0.20000
set prob_40    = 0.40000
set prob_60    = 0.60000
set prob_80    = 0.80000
set file_out   = "trace.out.prob"

#
# Set plot parameters
#
set SAT = "-15.0 15.0"
set FIG = ./FIGS_CORE
set out_dir    = "./OUTPUT_CORE"
set SMOOTH = 1

#
# Compile statistical phonon code
#

cd SRC
make statsyn_1D.x
cd ..







# 
# Start plotting
# 
./bin/pstack2D << EOF
OUTPUT_ATT/trace.out.prob.000.core.300_400.LHZ
$SAT
FIGS_ATT/trace.out.prob.000.core.300_400.LHZ.ps
0
0
0
$SMOOTH
EOF
gv FIGS_ATT/trace.out.prob.000.core.300_400.LHZ.ps &
