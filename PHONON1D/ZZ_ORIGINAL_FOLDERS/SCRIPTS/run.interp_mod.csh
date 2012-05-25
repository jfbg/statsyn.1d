#cd SRC
#make interp_mod.o
#cd ..

#./bin/interp_mod << EOF
#./MODELS/luna_nakamura
#./MODELS/luna_nakamura_int
#EOF

./bin/interp_mod << EOF
./MODELS/luna_nakamura_2
./MODELS/luna_nakamura_int_2
4
EOF

