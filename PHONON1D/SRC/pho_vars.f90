MODULE PHO_VARS      ! Make variables global
        
        IMPLICIT NONE
        
        
        
        INTEGER, PARAMETER :: nlay0=1000
                
        ! ENERGY TRACKING
        CHARACTER*100 :: tfile
        DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:) :: trackcount !Phonon Tracking array
        REAL(8)       ::  attn, minattn
        INTEGER       ::  nttrack,ixtrack    !track time points
        INTEGER       ::  iztrack,ixtrackm
        INTEGER       ::  nttrack_dt            !time interval for saving phonon position
        REAL(8)       ::  normfactor            !Normalization factor for cell size
        REAL(8)       ::  dt_track
        REAL(8)           d2r,re,rm,circum
        INTEGER           dotrack,ixt,itt
        
        INTEGER       EorM                  !1=EARTH, 2=MOON
        
        ! VELOCITY MODEL CHECKS
        INTEGER       check_scat, check_core, check_scat2, check_source
        
        REAL(8)       t,x,xo,a,x_index
        REAL(8)       z(nlay0),vf(nlay0,2),rh(nlay0)
        REAL(8)       z_st(nlay0),r_st(nlay0),vst(nlay0,2),rht(nlay0)
        REAL(8)       z_s(nlay0),r_s(nlay0),vs(nlay0,2),rhs(nlay0)
        REAL(8)       dx1,dt1
        INTEGER       irtr1
        INTEGER     :: iz,iz1,iz2
        REAL(8)     :: maxcount
        INTEGER     :: IT,JT,I,J,ic,jj,k,kk,ll,mm
        REAL(8)       p,ang1
        REAL(8)       Q(nlay0,2),Qt(nlay0)              !QUALITY FACTOR 
        REAL(8)       dtstr1                !ATTENUATION PER LAYER
        REAL(8)       pi,P0
        INTEGER       n180,idelt1,idelt2
        REAL(8)     :: angst                 !! Starting angle for trace
        REAL(8)     :: deg2km
        REAL(8)       corelayer
        
        CHARACTER*100 IFile,ofile,ofile2,logfile
        
        ! Measure elapsed time
        REAL        elapsed(2)
        REAL        totaltime,ttime1,ttime2,ttime3,ttime4,ttimestart
        REAL        tt1,tt2,tt3,tt4,tt5,tt6,tt7,tt8,tt9

        
        ! SURFACE HIT
        REAL(8)          dtsurf   !Time difference for phonon hitting some distance away from receiver 
        REAL(8)          dreceiver !radius around receiver in which the phonons will be recorded (deg)
        
        ! SCATTERING
        REAL(8)          dsmin, dsmax, npow    !power law factor for scatterer length-scales
        REAL(8)          ds_scat_nf,ds_scat    !scatterer length0scale(non-flattened + flattened)
        REAL(8)          dz                    !Distance between actual depth and base of layer
        REAL(8)          ds_SL                  !Distance between phonon and next velocity layer
        REAL(8)          dh,dh2                    !Vertical Distance between phonon and next vel layer.
        INTEGER        izfac                  !0 if traveling above iz, 1 if below
        REAL(8)          z_act                  !Depth when in between two vel layers
        REAL(8)          Q0                    !Background Qi for frequency dependent Qi
        REAL(8)          dQdf                  !Q gradient with f
        INTEGER       iz_scat,iz_from                !Vel layer in which phonon is while it's scattered
        REAL(8)          scat_depth,scat_prob,BG_prob,SL_prob
        REAL(8)          scat_thet,scat_phi
        REAL(8)          z_mid                !Mid depth of travel between two scatterers.
        INTEGER       scat_FLAG
        REAL(8) ::    vel_perturb
        INTEGER       ud_pre
        INTEGER       conv_count(6)
        
        ! ATTENUATION
        INTEGER       dQdfSTYLE        !Let user choose dQdf behaviour based on list.   
        INTEGER       Watt             !(1) With Attenuation, (0) No attenuation 
        
        INTEGER       ncaust,icaust         !NUMBER OF CAUSTICS IN A RAY TRACE
        INTEGER       ud
        
        REAL(8)          frac
        REAL(8)          erad
        
        REAL(8)          arp,ars,atp,ats,ar,at !P- & S-WAVE REFL & TRANS COEFS
        REAL(8)          rt_sum,rt_min,rt_max  !MAX & MIN REFL PROBABILITIES
        
        INTEGER       ip,ip0                !1=P, 2=SH, 3=SV
        REAL(8)          x_sign
        
        REAL(8)          az
        REAL(8)          dp
        REAL(8)        :: totald
        REAL(8)        :: delta
        REAL(8)        :: dxi
        REAL(8)        :: h     !! Layer thickness
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
        REAL(8)        :: r0,r1    !! random number 0-1
        REAL           r2s
        REAL(8)        :: pow2,pow1 !! Normalization factor for hilber transform
        REAL(8)        :: s,s1,s2     !! Attenuation & bounds on attenuation for distance
        REAL(8)        :: scr1,scr2,scr3,scr4 !! Flat earth approximation variables
        REAL(8)        :: t0,t1,t2,dti,t_last  !! Time variables (bounds & interval)
        INTEGER        :: t_last_count,tstuck
        REAL(8)        :: ubot, utop !! Bottom & Top slowness
        REAL(8)        :: x1, x2     !! Distance bounds
        
        REAL(8)           c_mult(3)
        CHARACTER*3    cmp(3)
        REAL(8)           p1,p2(2)              !Ray parameters
        REAL(8)           qdep
        INTEGER        cons_EorA
        
        !INTERFACE
        INTEGER     INCI,init_ud
        REAL(4)     c,rhof,a1,b1,rhos
        REAL(4)     TdPP, TdSP, RdPP, TuPP, TuPS, RuPP
        REAL(4)     RuSP, RuPS, RuSS
        REAL(4)     SumCOEFF

!       !IF SprCrtcl > 1, then drop energy
!					INTEGER       SprCrtcl, SprCrtcl_count
!					REAL(8)       SprCrtcl_time
        
        ! SYSTEM + DEBUG
        INTEGER        status                !I/O ERROR (0=no READ error)
        INTEGER        n_iter_last,it_last,ix_last
        INTEGER     :: nseed
        INTEGER     :: seed2
        INTEGER (kind=8)     :: nclock,nclock1
        INTEGER           ntime(8)
        INTEGER     last_RT
        INTEGER              kernelnum
        
END MODULE PHO_VARS
