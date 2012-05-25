.SUFFIXES: .r
.r.f:
	ratfor -C $< > $@
SUBS   = leolib.o

all: leolib.o leolib.a clean

leolib.o:
	$(CCOMP) -c leolib.c

#pscreendump.o:
#	$(FCOMP) -c psscreendup.f

leolib.a: $(SUBS)
	ar cr leolib.a leolib.o $(SUBS)
	ranlib leolib.a

clean:
	rm *.o

ezxplot.o:   
	$(FCOMP) -c ezxplot.f
