set file_start = "beyn_lyr_0.1km_sp_0.6"
set depth = "0020"
set chn   = "lpr"
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
endif
if ($j == 2) then
set Hz = "40"
endif


set SAT = "-0.0001 0.0001"

./bin/xyz2sac << EOF
./OUTPUT/$file_start.$depth.$Hz.$chn
$SAT
./FIGS/$file_start.$depth.$Hz.$chn.ps
0
0
0
0
EOF
gv ./FIGS/$file_start.$depth.$Hz.$chn.ps &

end

end
