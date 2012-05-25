cd SRC
gfortran inst_resp.f90 -o ../bin/inst_resp
cd ..

./bin/inst_resp

psxy fort.7 -R0/40/0/10000000000 -JX4i -B10/1000000000 -M -P > temp.ps
gv temp.ps &
