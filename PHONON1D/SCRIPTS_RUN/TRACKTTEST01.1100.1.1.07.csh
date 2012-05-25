./bin/statsyn_TRACK << EOF
./MODELS/WEB10km_50m
0.0 0.1668 0.2931          !LIMIT THE RAY PARAMETER
0 4500 0.150 !LIMIT THE TIME WINDOW (SECONDS) 
0 180 91          !LIMIT THE DISTANCE (DEGREES)    
1         !NUMBER OF PHONONS TO FIRE       
1100          !DEPTH OF SOURCE                 
2            !1=EARTH, 2=MOON                 
10       !MAX SCATTERING DEPTH            
0.60000          !SCATTERING PROB (SIMILAR TO RMS)
./TRACKING/TRACKTTEST01.1100.1.1.07.TRACK
./OUTPUT/TRACKTTEST01.1100.1.1.07
EOF
 
