./bin/statsyn_1D << EOF
./MODELS/WEB01km_500m
1
0.0 0.1668 0.2931          !LIMIT THE RAY PARAMETER
0 5400 0.025 !LIMIT THE TIME WINDOW (SECONDS) 
0 180 91          !LIMIT THE DISTANCE (DEGREES)    
500000         !NUMBER OF PHONONS TO FIRE       
156        !RANDOMIZING KERNEL TO USE       
0020          !DEPTH OF SOURCE                 
2            !1=EARTH, 2=MOON                 
1           !1=P, 2=SH WAVES                 
1       !MAX SCATTERING DEPTH            
0.60000          !SCATTERING PROB (SIMILAR TO RMS)
./OUTPUT/WEB01km_500m.0020.1.1.40
EOF
 
