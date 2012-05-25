cd SRC
make convolve.o
cd ..

./bin/convolve << EOF
./trace.out.prob_00.LHZ
./convolve.out
EOF

./bin/pstack2D << EOF
./convolve.out
-1 1
./FIGS/CONVOLVE.ps
0
0
0
1
EOF
gv ./FIGS/CONVOLVE.ps &

cd SRC
make xyz2sac.o
cd ..

rm SAC/*.sac
./bin/xyz2sac << EOF
convolve.out
EOF

echo 'hi'

cd SAC
sac << EOF
r D_025.0*sac D_055.0*sac D_085.0*sac
color on inc on list black blue red
qdp off
fileid off
begg sgf
p1
endg sgf
EOF
sgftops f001.sgf convolve.ps
gv convolve.ps &
cd ..

echo 'hi2:'

#./bin/xyz2sac << EOF
#trace.out
#EOF
