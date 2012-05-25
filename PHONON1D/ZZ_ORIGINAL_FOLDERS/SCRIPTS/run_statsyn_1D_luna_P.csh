#!/bin/csh

cd SRC
make interp_mod.o
cd ..

echo 'RUNNING INTERPOLATION (SCALE LENGTH):'
./bin/interp_mod << EOF
./MODELS/luna_nakamura
./MODELS/luna_nakamura_int
4.                      !SCALE LENGTH OF SCATTERING
EOF




cd SRC
make statsyn_1D.x
cd ..

time ./bin/statsyn_1D << EOF
./MODELS/luna_nakamura_int
1
0.0 0.293    20000      !LIMIT THE RAY PARAMETER
0 900 3601              !LIMIT THE TIME WINDOW (SECONDS)
0 180 181               !LIMIT THE DISTANCE (DEGREES)
4000000                 !NUMBER OF PHONONS TO FIRE
105                     !RANDOMIZING KERNEL TO USE
0.                      !DEPTH OF SOURCE
2                       !1=EARTH, 2=MOON
1                       !1=P, 2=SH WAVES
10.                     !MAX SCATTERING DEPTH
0.0000                  !SCATTERING PROB (SIMILAR TO RMS)
trace.out
EOF

#UNCOMMENT THE NEXT LINE IF  YOU KEEP GETTING "nan" FOR EDITING AND DEBUGGING
#edit ./trace.out

csh SCRIPTS/view.csh &
