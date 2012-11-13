#!/bin/csh

# J010

# Runs on 6 cores on MOHO
# 1e7p x 6 cores x 3 iter = 1.8e8p
# Only 2 depths (20 & 1100)
# 2 freqs
# With TRACKING
# NO SCATTERING
# Constant Qi = 7000

# !!!!!!
# No near-surface very low velocity zone (regolith)
# Constant velocities in crust, 2sharp contrast at mantle
# Is the same model as J009.
# !!!!!!

# Started on March 4th, 2012

#
# Set synthetic parameters
#

set ray_par    = "0.0 0.1668 0.2931"
@ t_start      = 0
@ t_max        = 4500			# 75 minutes
set d_range    = "0 180 91"
set model      = "2"	#2 for Moon
set mx_scat_dp = "0"
set n_phonon   = "10000000"
set prob_scat    = 0.0000
set file_out   = "J010_VP_gc05_nvlvl_10km_500m_M"
set model_name = "VPREMOON_Q7000_gc05_nvlvl_10km_500m"

@ n_depth = 2     ## Number of depths to use
@ n_freq  = 2     ## Number of frequency bands (40s and 6.66666s)
@ n_kern  = 6     ## Number of kernels to use per iteration (simultaneous run)
@ n_iter  = 3    ## Number of iterations

# Output folder
set out_dir    = "./OUTPUT"
set outTRACK_dir    = "./TRACKING"


# Compile statistical phonon code
cd SRC
make track.x
cd ..

##
# Loop over ITERATIONS
##
@ k  = 0
while ($k < $n_iter)
@ k = $k + 1

##
# Loop over FREQUENCY
##
@ l = 0
while ($l < $n_freq)
@ l = $l + 1

if ($l == 2) then
 set dt = "0.025"
 set period = "40"
else
 set dt = "0.150"
 set period = "07"
endif


##
# Loop over DEPTH
##
@ i  =  0
while ($i < $n_depth)
@ i = $i + 1
if ($i == 2) then
 set q_depth = 0020 
else if ($i == 1) then
 set q_depth = 1100
else
 set q_depth = 0.01
endif

echo "Depth=:" $q_depth


##
# Loop over KERNELS
##
@ j  = 0
while ($j < $n_kern)
@ j = $j + 1


sleep 1

## 
# Start phonon synthetics
##

set file_whole = $file_out.$q_depth.$j.$k.$period
set file_track = $file_out.$q_depth.$j.$k.$period.TRACK
set file_csh   = SCRIPTS_RUN/$file_out.$q_depth.$j.$k.$period.csh

echo "./bin/statsyn_TRACK << EOF"                              >  $file_csh
echo "./MODELS/$model_name"				>> $file_csh
#echo "1"                                                    >> $file_csh
echo "$ray_par          \!LIMIT THE RAY PARAMETER"          >> $file_csh
echo "$t_start $t_max $dt \!LIMIT THE TIME WINDOW (SECONDS) " >> $file_csh
echo "$d_range          \!LIMIT THE DISTANCE (DEGREES)    " >> $file_csh
echo "$n_phonon         \!NUMBER OF PHONONS TO FIRE       " >> $file_csh
# echo "$rand_kern        \!RANDOMIZING KERNEL TO USE       " >> $file_csh
echo "$q_depth          \!DEPTH OF SOURCE                 " >> $file_csh
echo "$model            \!1=EARTH, 2=MOON                 " >> $file_csh
# echo "$p_or_s           \!1=P, 2=SH WAVES                 " >> $file_csh
echo "$mx_scat_dp       \!MAX SCATTERING DEPTH            " >> $file_csh
echo "$prob_scat          \!SCATTERING PROB (SIMILAR TO RMS)" >> $file_csh
echo "$outTRACK_dir/$file_track"                            >> $file_csh
echo "$out_dir/$file_whole"                                 >> $file_csh
echo "EOF"                                                  >> $file_csh

echo " " >> $file_csh


if ($j == $n_kern)  then
csh $file_csh
endif
if ($j < $n_kern) then
csh $file_csh &
endif

\rm OUTPUT/*.lpt

end
echo end inner loop for many kernels

end 
echo end depth loop.

end 
echo end loop over frequencies

end
echo end outer loop for many iterations.

