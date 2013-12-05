#!/bin/csh

#
# Set synthetic parameters
#

@ t_start      = 0
@ t_max        = 4500			# 90 minutes
set d_range    = "0 180 91"
set n_phonon   = "10000000"

# Source attenuation and type
set dQdfstyle  = 1
set sourcetype = 3    # delta (1), sine (2), custom (9)
set customsourcefile = 'LP_0_01t0_5Hz_dt1s.source'
set rPrSVrSH   = "1 10 10"  # Energy partioning at source
set samtype    = 1   # Sampling over takeoff angles (1), or slownesses (2),or BM (3)

# Code Parameters
set cons_EorA = 2  # Conserve Amplitude (1) or Energy (2) at interfaces (Benchmark works with 2)
set Watt      = 1  # With attenuation (1) or without (0)
set track     = 0  # Yes (1). Produce tracking files (follows phonon throughout)
                   # This is actually not activated in the code yet.

# SCATTERING
set mx_scat_dp = 30   # Depth of scattering layer
set bg_scat    = 0.0   # Global scattering probability (keep low....!)
set prob_scat  = 0.9    # Scattering Layer scattering probability
set dsmin      = 0.05   # Min scaterrer length scale
set dsmax      = 10     # Max scaterrer length scale
set npow       = -0.5   # Power law factor for scatterer lengthscale
set velperturb = 0.6

set file_out   = "RSMOON_001_2"
set model_name = "CSIMPLEMOON_basic"
set pfac       = -2     # Density factor for flattening  (factor = pfac -2)

@ n_depth = 1     ## Number of depths to use
@ n_freq  = 1     ## Number of frequency bands (40s and 6.66666s)
@ n_kern  = 16    ## Number of kernels to use per iteration (simultaneous run)
@ n_iter  = 5     ## Number of iterations

# Output folder
set out_dir    = "./OUTPUT"
set outTRACK_dir    = "./TRACKING"
set log_dir    = "./LOG"


# Compile statistical phonon code
#cd SRC
#make statsynr_intel.x
#cd ..


##
# Loop over FREQUENCY
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
# Loop over DEPTH
##
@ i  =  0
while ($i < $n_depth)
@ i = $i + 1
if ($i == 3) then
 set q_depth = 0.01
else if ($i == 2) then
 set q_depth = 30
else
 set q_depth = 1000
endif

echo "Depth=:" $q_depth

##
# Loop over ITERATIONS
##
@ k  = 0
while ($k < $n_iter)
@ k = $k + 1


##
# Loop over KERNELS
##
@ j  = 0
while ($j < $n_kern)
@ j = $j + 1

@ kernelnum = $j

sleep 25

## 
# Start phonon synthetics
##

set file_whole = $file_out.$q_depth.$j.$k.$period
set file_log = log.$file_out.$q_depth.$j.$k.$period
set file_track = $file_out.$q_depth.$j.$k.$period.TRACK
set file_csh   = SCRIPTS_RUN/$file_out.$q_depth.$j.$k.$period.csh

echo "./bin/statsynr_intel << EOF"                         >  $file_csh
echo "./MODELS/$model_name"				                      >> $file_csh
echo "$pfac"				                                  >> $file_csh
echo "$t_start $t_max $dt \!LIMIT THE TIME WINDOW (SECONDS) " >> $file_csh
echo "$d_range          \!LIMIT THE DISTANCE (DEGREES)    "   >> $file_csh
echo "$n_phonon         \!NUMBER OF PHONONS TO FIRE       "   >> $file_csh
echo "$q_depth          \!DEPTH OF SOURCE                 "   >> $file_csh
echo "$sourcetype        \!Source Type            "           >> $file_csh

if ($sourcetype == 9) then
echo "SOURCES/$customsourcefile"        >> $file_csh
endif

echo "$rPrSVrSH        \!Energy partitioning      "           >> $file_csh
echo "$samtype        \!sampling                  "           >> $file_csh
echo "$mx_scat_dp       \!MAX SCATTERING DEPTH            "   >> $file_csh
echo "$prob_scat          \!SCATTERING PROB (SIMILAR TO RMS)" >> $file_csh
echo "$bg_scat          \!BACKGROUND SCATTERING PROB (SIMILAR TO RMS)" >> $file_csh
echo "$dsmin $dsmax $npow \!SCATERER SCALE-LENGTHS"           >> $file_csh
echo "$velperturb         \!VELOCITY PERTURBATION"            >> $file_csh
echo "$Watt             \!With or withour attenuation"        >> $file_csh
echo "$dQdfstyle        \!dQ/df Style            "            >> $file_csh
echo "$cons_EorA        \!Conserve Energy or Amplitde"        >> $file_csh
echo "$outTRACK_dir/$file_track"                              >> $file_csh
echo "$out_dir/$file_whole"                                   >> $file_csh
echo "$log_dir/$file_log"                                     >> $file_csh
echo "$kernelnum        \!Kernel number"                      >> $file_csh
echo "EOF"                                                   >> $file_csh

echo " " >> $file_csh

#
if ($j == $n_kern)  then
csh $file_csh
endif
if ($j < $n_kern) then
csh $file_csh &
endif

#\rm OUTPUT/*.lpt

end
echo end inner loop for many kernels

end
echo end outer loop for many iterations.

end 
echo end depth loop.

end 
echo end loop over frequencies

