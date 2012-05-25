#!/bin/csh


#
# Convert model to 4km thick layers (scale-lenght of scatterers)
#

cd SRC
make interp_mod.o
cd ..

set mod_name = ""

echo 'RUNNING INTERPOLATION (SCALE LENGTH):'
./bin/interp_mod << EOF
./MODELS/luna_nakamura$mod_name
./MODELS/luna_nakamura_int$mod_name
1.                      !SCALE LENGTH OF SCATTERING
EOF

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
set SAT = "-1.0 1.0"
set FIG = ./FIGS
set out_dir    = "./OUTPUT"
set SMOOTH = 0

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
@ di =  2
@ i  =  $is
@ ie =  $di * 1



while ($i < $ie)
@ i = $i + $di
@ iii = $i * 10
if ($iii == 0) set iii = 00
set ii = 0.$iii

echo $i $ii $iii

# 
# Start plotting
# 
./bin/pstack2D << EOF
$out_dir/$file_out.$iii.LHZ
$SAT
$FIG/$file_out.$iii.LHZ.ps
0
0
0
$SMOOTH
EOF
gv $FIG/$file_out.$iii.LHZ.ps &

end
