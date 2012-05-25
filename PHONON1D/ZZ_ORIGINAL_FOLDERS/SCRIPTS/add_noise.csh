cd SRC
make pstack2D.x
make add_noise.x
make xyz2sac.x
cd ..
set base_dir = "./OUTPUT/"
set figs_dir = "./FIGS/"
cd $base_dir

set file = "trace.out.prob.110*.LHZ"
set file = "trace.out.prob.0020.LHZ"

foreach f ($file)
cd ../

#./bin/pstack2D << EOF
#$base_dir$f
#-10 10
#$f.ps
#0
#0
#0
#0
#EOF

#gv ./FIGS/$f.ps &



./bin/add_noise << EOF
$base_dir$f
$base_dir$f.dec
20
10
EOF


./bin/pstack2D << EOF
$base_dir$f.dec
-0.001 0.001
$figs_dir$f.dec.ps
0
0
0
0
EOF
gv $figs_dir$f.dec.ps &



#rm SAC/*.sac
#./bin/xyz2sac << EOF
#./OUTPUT/$base_file2
#EOF

#cd SAC
#sac << EOF
#r D_025.0.sac D_055.0.sac D_085.0.sac
#color on inc on list black blue red
#qdp off
#fileid off
#begg sgf
#p1
#endg sgf
#EOF
#sgftops f001.sgf ../FIGS/$base_file2.ps
#cd ..
#gv ./FIGS/$base_file2.ps &


cd $base_dir
end

