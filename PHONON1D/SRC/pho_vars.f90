MODULE PHO_VARS			! Make variables global
				
				IMPLICIT NONE
							
				INTEGER, PARAMETER :: nlay0=1000, nt0=144000, nx0=91
				REAL          z(nlay0),vf(nlay0,2),rh(nlay0)
				REAL          z_s(nlay0),r_s(nlay0),vs(nlay0,2)
				REAL          t,x,xo,a,w(nt0)
				REAL          dx1,dt1
				INTEGER       irtr1
				INTEGER     :: iz,iz1,itt
				REAL  			:: maxcount
				INTEGER     :: IT,JT,I,J,ic,jj,k,kk,ll,mm
				REAL          p,ang1
				REAL          Q(nlay0)              !QUALITY FACTOR 
				REAL          dtstr1                !ATTENUATION PER LAYER
				REAL          pi,P0
				INTEGER       n180,idelt1,idelt2
				REAL       :: angst                 !! Starting angle for trace
				INTEGER		 :: iztrack,ixtrack
				REAL       :: deg2km
				

				
				! SCATTERING
        REAL          ds_scat, dsmin, dsmax, npow  !Distance between scatterers and power law factor.
				REAL					dz										!Distance between actual depth and base of layer
				REAL					ds_SL									!Distance between phonon and next velocity layer
				REAL          dh										!Vertical Distance between phonon and next vel layer.
				REAL					iz2										!Temp var to find location of phonon
				INTEGER				izfac									!0 if traveling above iz, 1 if below
				REAL					z_act									!Depth when in between two vel layers
				REAL          Q0										!Background Qi for frequency dependent Qi
				REAL          dQdf									!Q gradient with f
				INTEGER       iz_scat								!Vel layer in which phonon is while it's scattered
				
				
				INTEGER       ncaust,icaust         !NUMBER OF CAUSTICS IN A RAY TRACE
				INTEGER       ud
				
				REAL          frac
				REAL          erad
				
				REAL          arp,ars,atp,ats,ar,at !P- & S-WAVE REFL & TRANS COEFS
				REAL          rt_sum,rt_min,rt_max  !MAX & MIN REFL PROBABILITIES
				
				INTEGER       ip,ip0                !1=P, 2=SH, 3=SV
				REAL          x_sign
				
				REAL          scat_depth,scat_prob
				REAL          scat_thet,scat_phi
				REAL          az
				REAL          dp
				REAL        :: d
				REAL        :: delta
				REAL        :: dxi
				REAL        :: h     !! Layer thickness
				INTEGER     :: idum
				INTEGER     :: imth  !! Interpolation method (1 or 2)
				INTEGER     :: iwave !! (P(2) or S(2))
				INTEGER     :: ix,nx ,ixtemp,ixdeg   !! Index & number of distances
				INTEGER     :: nfil  !! Number of filter values for hilbert transform
				INTEGER     :: ntr   !! Number of traces
				INTEGER     :: nts,nts1   !! Number of time series points for source
				INTEGER     :: nitr  !! Number of ith trace (last)
				INTEGER     :: nt    !! Number of time in output file
				INTEGER     :: nlay  !! Number of layers in model
				REAL        :: r0,r1    !! random number 0-1
				REAL        :: pow2,pow1 !! Normalization factor for hilber transform
				REAL        :: s,s1,s2     !! Attenuation & bounds on attenuation for distance
				REAL        :: scr1,scr2,scr3,scr4 !! Flat earth approximation variables
				REAL        :: t0,t1,t2,dti,t_last  !! Time variables (bounds & interval)
				REAL        :: ubot, utop !! Bottom & Top slowness
				REAL        :: x1, x2     !! Distance bounds
				
				REAL           c_mult(3)
				CHARACTER*3    cmp(3)
				REAL           p1,p2(2)              !Ray parameters
				REAL           qdep
				
				INTEGER        status                !I/O ERROR (0=no READ error)
				INTEGER        n_iter_last,it_last,ix_last
				INTEGER     :: nseed
				INTEGER     :: seed
				INTEGER (kind=8)     :: nclock,nclock1

			END MODULE PHO_VARS