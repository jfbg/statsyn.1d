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
set n_phonon   = "1000000"
set prob_00    = 0.00000
set prob_20    = 0.20000
set prob_40    = 0.40000
set prob_60    = 0.60000
set prob_80    = 0.80000
set file_out   = "trace.out.prob"

#
# Set plot parameters
#
set SAT = "-10.0 10.0"
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
# Set loop for 0-8 (0.0 to 0.8)
#
@ is =  -2
@ di =  4
@ i  =  $is
@ ie =  $di * 1




set iii = 000
set ii = 0.0

echo $i $ii $iii

@ js = -1
@ dj =  1
@ j  =  $js
@ je =  $dj * 4

while ($j < $je)
@ j = $j + $dj
@ jjj = $j * 100
if ($jjj == 0) set jjj = 000
set mod_name = "_core_$jjj"


# 
# Start plotting
# 
./bin/pstack2D << EOF
$out_dir/$file_out.$iii.core.$jjj.LHZ
$SAT
$FIG/$file_out.$iii.core.$jjj.LHZ.ps
0
0
0
$SMOOTH
EOF
gv $FIG/$file_out.$iii.core.$jjj.LHZ.ps &

end
