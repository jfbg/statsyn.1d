GFORT = gfortran
GSAC  = /Users/jf/src/sac/lib/sacio.a 


trackiso.x:
	$(GFORT) -m32 get_spec.f PHO_VARS.f90 statsyn_TRACK_iso.f90 $(GSAC) -o ../bin/statsyn_iso

tracktime.x:	
	$(GFORT) -m32 get_spec.f statsyn_1D_TIMETRACK.f90 $(GSAC) -o ../bin/statsyn_1D_track_time

track.x:	
	$(GFORT) -m32 get_spec.f statsyn_TRACK.f90 $(GSAC) -o ../bin/statsyn_TRACK

tracktest.x:
	$(GFORT) -m32 get_spec.f statsyn_TRACKtest.f90 $(GSAC) -o ../bin/statsyn_TRACKtest

statsyn_1D.x:
	$(GFORT) -m32 get_spec.f statsyn_1D.f90 $(GSAC) -o ../bin/statsyn_1D

statsyn_1D_att.x:
	$(GFORT) get_spec.f statsyn_1D_att.f90 $(GSAC) -o ../bin/statsyn_1D_att

trace.nw.o:
	$(FORT) trace.nw.f -o ../bin/trace.nw.1

get_spec.o:
	$(FORT) -c get_spec.f

pstack2D.x:
	$(GFORT) pstack2D.f90  psplot.f90 -o ../bin/pstack2D

interp_mod.o:
	$(GFORT) interp_mod.f -o ../bin/interp_mod

decon.o:
	$(GFORT) decon.f transfer.f get_spec.f -o ../bin/decon

add_noise.x:
	$(GFORT) add_noise.f90 transfer.f get_spec.f bit_limit.o -o add_noise

convolve.o:
	$(GFORT) convolve.f transfer.f get_spec.f bit_limit.o -o ../bin/convolve

trace.luna.o:
	fort77 trace.luna.f trace_subs.f psplot.f $(XLIB) -o ../bin/trace.luna

xyz2sac.x:
	$(GFORT) xyz2sac.f90 $(GSAC) -o ../bin/xyz2sac

sac2xyz.x:
	$(GFORT) sac2xyz.f90 $(GSAC) -o ../bin/sac2xyz

convert_prem_moon.x:
	$(GFORT) convert_prem_moon.f90 -o ../bin/convert_prem_moon


#
#Subroutines
#
bit_limit.x:
	$(GFORT) -c bit_limit.f90

get_spec.x:
	$(GFORT) -c get_spec.f

psplot.x:
	$(GFORT) -c psplot.f90

trace_subs.x:
	$(GFORT) -c trace_subs.f
	
transfer.x:
	$(GFORT) -c transfer.x

