#!/bin/csh

# running on GITAYLOR


#
# Set synthetic parameters
#

set ray_par    = "0.0 0.1668 0.2931"
@ t_start      = 0
@ t_max        = 5400			# 90 minutes
set d_range    = "0 180 91"
@ rand_kern    = 155
set q_depth    = "0."
set model      = "2"
set p_or_s     = "1"
set mx_scat_dp = "10"
set n_phonon   = "500000"
set prob_scat    = 0.60000
#set prob_00    = 0.00000
#set prob_20    = 0.20000
#set prob_40    = 0.40000
#set prob_60    = 0.40000
#set prob_80    = 0.80000
set file_out   = "WEB10km_250m"
set model_name = "WEB10km_250m"

@ n_depth = 3     ## Number of depths to use
@ n_freq  = 2     ## Number of frequency bands (40s and 6.66666s)
@ n_kern  = 1     ## Number of kernels to use per iteration (simultaneous run)
@ n_iter  = 5    ## Number of iterations

#
# Set plot parameters
#
# set SAT = "-.10 .10"
# set FIG = ./FIGS
set out_dir    = "./OUTPUT"
# set SMOOTH = 0

#
# Compile statistical phonon code
#

cd SRC
make statsyn_1D.x
cd ..

##
# Loop over iterations
##
@ k  = 0
while ($k < $n_iter)
@ k = $k + 1

##
# Loop over frequency
##

@ l = 0
while ($l < $n_freq)
@ l = $l + 1

if ($l == 1) then
 set dt = "0.025"
 set period = "40"
else
 set dt = "0.150"
 set period = "07"
endif


##
# Loop over depth
##
@ i  =  0
while ($i < $n_depth)
@ i = $i + 1
if ($i == 1) then
 set q_depth = 0020 
else if ($i == 2) then
 set q_depth = 1100
else
 set q_depth = 0.01
endif

echo "Depth=:" $q_depth


##
# Loop over randomizing kernels
##
@ j  = 0
while ($j < $n_kern)
@ j = $j + 1
@ rand_kern = $rand_kern + 1

sleep 3

## 
# Start phonon synthetics
##

set file_whole = $file_out.$q_depth.$j.$k.$period
set file_csh   = SCRIPTS_RUN/$file_out.$q_depth.$j.$k.$period.csh

echo "./bin/statsyn_1D << EOF"                              >  $file_csh
echo "./MODELS/$model_name"																>> $file_csh
echo "1"                                                    >> $file_csh
echo "$ray_par          \!LIMIT THE RAY PARAMETER"          >> $file_csh
echo "$t_start $t_max $dt \!LIMIT THE TIME WINDOW (SECONDS) " >> $file_csh
echo "$d_range          \!LIMIT THE DISTANCE (DEGREES)    " >> $file_csh
echo "$n_phonon         \!NUMBER OF PHONONS TO FIRE       " >> $file_csh
echo "$rand_kern        \!RANDOMIZING KERNEL TO USE       " >> $file_csh
echo "$q_depth          \!DEPTH OF SOURCE                 " >> $file_csh
echo "$model            \!1=EARTH, 2=MOON                 " >> $file_csh
echo "$p_or_s           \!1=P, 2=SH WAVES                 " >> $file_csh
echo "$mx_scat_dp       \!MAX SCATTERING DEPTH            " >> $file_csh
echo "$prob_scat          \!SCATTERING PROB (SIMILAR TO RMS)" >> $file_csh
echo "$out_dir/$file_whole"                                 >> $file_csh
echo "EOF"                                                  >> $file_csh

echo " " >> $file_csh

# 
# Start plotting
# 
# echo "./bin/pstack2D << EOF2"                        >> $file_csh
# echo "$out_dir/$file_whole.lpz"         >> $file_csh
# echo "$SAT"                                          >> $file_csh
# echo "$FIG/$file_whole.lpz.ps"          >> $file_csh
# echo "0                                            " >> $file_csh
# echo "0                                            " >> $file_csh
# echo "0                                            " >> $file_csh
# echo "$SMOOTH                                      " >> $file_csh
# echo "EOF2"                                          >> $file_csh
#echo "gv $FIG/$file_whole.lpz.ps &"     >> $file_csh


if ($j == $n_kern)  then
csh $file_csh
endif
if ($j < $n_kern) then
csh $file_csh &
endif

end
echo end inner loop for many kernels

end 
echo end depth loop.

end 
echo end loop over frequencies

end
echo end outer loop for many iterations.

