set file_base1 = "beyn_lyr_0.1km_sp_0.6."
set file_base2 = "OUTPUT/$file_base1"
set depth     = "0020"
set cmp       = "lpz"
set period    = "07"

ls $file_base2$depth.*.*.$period.$cmp > list_$depth.$period.$cmp

./bin/average_output << EOF
list_$depth.$period.$cmp
$file_base2$depth.$period.all.$cmp
EOF

./bin/pstack2D << EOF2
$file_base2$depth.$period.all.$cmp
-0.01 0.01                               
./FIGS/$file_base1$depth.$period.all.$cmp.ps  
0                                  
0                                  
0                                  
0                            
EOF2
/sw/bin/gv ./FIGS/$file_base1$depth.$period.all.$cmp.ps &
