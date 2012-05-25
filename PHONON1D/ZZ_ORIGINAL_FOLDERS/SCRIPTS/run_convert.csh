cd SRC
gfortran convert_model.f90 -o ../bin/convert_model
cd ../



cd MODELS
../bin/convert_model << EOF
vzf.nak
luna_nakamura_2
EOF

edit luna_nakamura_2 &
