./bin/statsyn_TRACKtest << EOF
./MODELS/VPREMOON_Q7000_gc03_nvlvl_10km_500m
0.0 0.1668 0.2931          !LIMIT THE RAY PARAMETER
0 4500 0.150 !LIMIT THE TIME WINDOW (SECONDS) 
0 180 91          !LIMIT THE DISTANCE (DEGREES)    
5000         !NUMBER OF PHONONS TO FIRE       
1100          !DEPTH OF SOURCE                 
2            !1=EARTH, 2=MOON                 
10       !MAX SCATTERING DEPTH            
0.6000          !SCATTERING PROB (SIMILAR TO RMS)
./TRACKING/J001_tracktest.1100.2.2.07.TRACK
./OUTPUT/J001_tracktest.1100.2.2.07
EOF
 
