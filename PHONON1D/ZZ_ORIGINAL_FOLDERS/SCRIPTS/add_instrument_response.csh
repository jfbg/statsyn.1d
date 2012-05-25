cd SRC
make xyz2sac.x
make sac2xyz.x
cd ..


set file_start = "beyn_lyr_0.1km_sp_0.6"
set depth = "0020"
set chn   = "lpz"
set chn2  = "sp"
set Hz    = "07"



@ i = 0
while ($i <= 2)
@ i = $i + 1

if ($i == 1) then
set depth = "0020"
endif
if ($i == 2) then
set depth = "0.01"
endif
if ($i == 3) then
set depth = "1100"
endif

echo "HI"

@ j = 0
while ($j <= 1) 
@ j = $j + 1

if ($j == 1) then
set Hz = "07"
set BP = "bp n 8 co 0.2 2"
endif
if ($j == 2) then
set Hz = "40"
set BP = "bp n 8 co 1 11"
endif


rm SAC/*.sac

set file1 = $file_start.$depth.$Hz.$chn
set file2 = OUTPUT/$file1

./bin/xyz2sac << EOF
$file2
EOF

cd SAC

sactosac -f *.sac

sac << EOF
r D*.sac
$BP
write over
q
EOF

ls D_*.sac > list.sac

../bin/sac2xyz << EOF
list.sac
../$file2.filt
EOF

cd ..

set SAT = "-0.001 0.001"

./bin/pstack2D << EOF
$file2.filt
$SAT
./FIGS/$file1.filt.ps
0
0
0
0
EOF
gv ./FIGS/$file1.filt.ps &



end

end
