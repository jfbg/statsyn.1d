OBJS=    ezxplotlib.a LEOLIB/leolib.a 
# G4 & G5:
#OPENWINHOME = /usr/X11R6
# SUN:
#OPENWINHOME = /usr/openwin

testezxplot: Makefile testezxplot.f $(OBJS)
	$(FCOMP)  testezxplot.f $(OBJS) -I$(OPENWINHOME)/include -L$(OPENWINHOME)/lib -lX11 -o testezxplot
