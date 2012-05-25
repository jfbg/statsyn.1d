cd SRC
make decon.o
cd ..

./bin/decon << EOF
./trace.out
./decon.out
EOF

./bin/pstack2D << EOF
./decon.out
-.1 .1
./FIGS/DECON.ps
0
0
0
1
EOF
gv ./FIGS/DECON.ps &
