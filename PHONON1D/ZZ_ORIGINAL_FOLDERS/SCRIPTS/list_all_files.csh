cd SRC
gfortran average_output.f90 -o ../bin/average_output
cd ..


ls ./OUTPUT/beyn_lyr_0.1km_sp_0.6.0020.?.lpr > list.0020.lpr

./bin/average_output << EOF
./list.0020.lpr
./OUTPUT/beyn_lyr_0.1km_sp_0.6.0020.all.lpr
EOF

./bin/pstack2D << EOF
./OUTPUT/beyn_lyr_0.1km_sp_0.6.0020.all.lpr
-0.001 0.001                               
./FIGS/beyn_lyr_0.1km_sp_0.6.0020.all.lpr.ps  
0                                  
0                                  
0                                  
0                            
EOF
/sw/bin/gv ./FIGS/beyn_lyr_0.1km_sp_0.6.0020.all.lpr.ps &
