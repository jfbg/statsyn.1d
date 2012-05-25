OBJS=    ./leolib.a 
X11 =    -I/usr/X11R6/include -L/usr/X11R6/lib -lX11

all: czxplot_test.o

czxplot_test.o: Makefile czxplot_test.c $(OBJS)
	cc  czxplot_test.c $(OBJS) $(X11) -o ../../bin/czxplot_test
