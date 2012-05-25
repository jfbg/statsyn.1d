setenv FCOMP /sw/bin/fort77
setenv CCOMP /usr/bin/gcc
setenv OPENWINHOME /usr/X11R6

cd SRC/X/LEOLIB
rm *.a
make

cd ..

rm *.o
rm *.a

make -f Makefile_subs
make

cd ..

make
