#!/bin/csh
make statsyn_1D.o
time ./statsyn_1D << EOF
iasp91.nw
2
0.0 0.22240 20000
0 3600 3601
0 180 361
100000
105
0.
trace.out
EOF

csh view.csh &
