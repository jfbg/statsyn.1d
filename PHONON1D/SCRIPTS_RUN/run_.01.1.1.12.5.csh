./bin/statsyn_1D_track_time << EOF
./MODELS/luna_weber_crustgrad_FINAL
1
0.0 0.1668 0.2931          !LIMIT THE RAY PARAMETER
0 3600 0.08 !LIMIT THE TIME WINDOW (SECONDS) 
0 180 91          !LIMIT THE DISTANCE (DEGREES)    
5000         !NUMBER OF PHONONS TO FIRE       
157        !RANDOMIZING KERNEL TO USE       
.01          !DEPTH OF SOURCE                 
2            !1=EARTH, 2=MOON                 
1           !1=P, 2=SH WAVES                 
10       !MAX SCATTERING DEPTH            
0.00000          !SCATTERING PROB (SIMILAR TO RMS)
./TRACKING/trackcounttime..01.1.1.12.5
./OUTPUT/tracktest..01.1.1.12.5
EOF
 
