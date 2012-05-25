#!/bin/csh

cd SRC
make statsyn_1D.x
cd ..

time ./bin/statsyn_1D << EOF
./MODELS/luna_nakamura_int
2
0.0 0.293000 20000      !LIMIT THE RAY PARAMETER
0 1800 3601             !LIMIT THE TIME WINDOW (SECONDS)
0 180 181               !LIMIT THE DISTANCE (DEGREES)
100000                  !NUMBER OF PHONONS TO FIRE
105                     !RANDOMIZING KERNEL TO USE
0.                      !DEPTH OF SOURCE
2                       !1=EARTH, 2=MOON
2                       !1=P, 2=SH WAVES
trace.out
EOF

#UNCOMMENT THE NEXT LINE IF  YOU KEEP GETTING "nan" FOR EDITING AND DEBUGGING
#edit ./trace.out

csh SCRIPTS/view.csh &
