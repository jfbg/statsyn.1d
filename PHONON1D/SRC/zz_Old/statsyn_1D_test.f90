program statsyn_1d_test
!
! Last edited by JFBG on		2011/05/25, randomize initial phonon state (P,SH, SV)
!
!
      implicit none
      integer, parameter :: nlay0=1000, nt0=144000, nx0=91
      real          z(nlay0),vf(nlay0,2),rh(nlay0)
      real          z_s(nlay0),r_s(nlay0),vs(nlay0,2)
      real          t,x,xo,a,w(nt0)
      character*100 ifile,ofile,ofile2
      real          dx1,dt1
      integer       irtr1
      integer     :: iz,iz1
      integer     :: IT,JT,I,J,ic,jj,k
      real          p,ang1
      double precision wf(nx0,nt0,3)        !STACKED DATA
      real          Q(nlay0)              !QUALITY FACTOR 
      real          dtstr1                !ATTENUATION PER LAYER
      real          mt(nt0)               !SOURCE-TIME FUNCTION 
      complex       ms(nt0),ss(nx0,nt0)   !SOURCE & STACKED SPECTRA
      real          nn(nx0,nt0)
      real          pi,P0
      integer       n180,idelt1,idelt2
      real       :: angst                 !! Starting angle for trace
      
      
      real          mts(101,4,nt0)        !ATTENUATED SOURCE
      real          b(nt0), e(nt0)        !HILBERT TRANSFORM & ENVELOPE
      real          mtsc(nt0),datt,dtst1  !SCRATCH SPACE
      integer       ims                   
      
      integer       ncaust,icaust         !NUMBER OF CAUSTICS IN A RAY TRACE
      integer       ud
      
      real          d2r,re,rm,deg2km
      integer       EorM                  !1=EARTH, 2=MOON
      
      real          frac
      real          erad
      
      real          arp,ars,atp,ats,ar,at !P- & S-WAVE REFL & TRANS COEFS
      real          rt_sum,rt_min,rt_max  !MAX & MIN REFL PROBABILITIES
      
      integer       ip,ip0                !1=P, 2=SH, 3=SV
      real          x_sign
      
      real          scat_depth,scat_prob
      real          scat_thet,scat_phi
      real          az
      real          dp
      real        :: d
      real        :: delta
      real        :: dold
      real        :: dxi
      real        :: f0
      real        :: h     !! Layer thickness
      integer     :: idum
      integer     :: imth  !! Interpolation method (1 or 2)
      integer     :: iwave !! (P(2) or S(2))
      integer     :: ix,nx,ixtemp    !! Index & number of distances
      integer     :: nfil  !! Number of filter values for hilber transform
      integer     :: ntr   !! Number of traces
      integer     :: nts,nts1   !! Number of time series points for source
      integer     :: nitr  !! Number of ith trace (last)
      integer     :: nt    !! Number of time in output file
      integer     :: nlay  !! Number of layers in model
      real        :: r0,r1    !! random number 0-1
      real        :: pow2,pow1 !! Normalization factor for hilber transform
      real        :: s,s1,s2     !! Attenuation & bounds on attenuation for distance
      real        :: scr1,scr2,scr3,scr4 !! Flat earth approximation variables
      real        :: t0,t1,t2,dti  !! Time variables (bounds & interval)
      real        :: ubot, utop !! Bottom & Top slowness
      real        :: x1, x2     !! Distance bounds
      
      real          c_mult(3)
      character*3   cmp(3)
      real          p1,p2(2)              !Ray parameters
      real          qdep
      
      integer       status                !I/O ERROR (0=no read error)
      integer       n_iter_last,it_last,ix_last
      integer :: nseed

      INTEGER:: seed
      integer (kind=8)     :: nclock,nclock1
!      CALL RANDOM_SEED(size = nseed)
!      ALLOCATE(      seed(nseed))
!      write(6,*) nseed
!      CALL SYSTEM_CLOCK(COUNT=nclock1)
!      CALL Sleep(3)

      cmp(1) = 'lpz'
      cmp(2) = 'lpt'
      cmp(3) = 'lpr'
      
      pi = atan(1.)*4.
      re = 6371.
      rm = 1737.
      d2r = pi/180.
			
			OPEN(54,FILE='ixdebut.txt',STATUS='UNKNOWN') !DEBUG
      
      
      write(6,*) 'ENTER SEISMIC VELOCITY MODEL FILE NAME'
      read (*,'(A)') ifile
      OPEN(1,FILE=ifile,STATUS='OLD')    !OPEN SEISMIC VELOCITY MODEL
      
      write(6,'(A)') 'ENTER:  (1) P  or  (2) S'
      read (5,    *)  iwave

25    write(6,'(A)') 'ENTER RAY PARAMETER RANGE (p1, p2(P), p2(S)):'
      read (5,    *)  p1, p2(1), p2(2)

50    write(6,'(A)') 'ENTER TIME WINDOW & # POINTS (t1, t2, nt):'
      read (5, *) t1,t2,dti                    !TIME WINDOW & # OF TIME STEPS
      nt = int((t2-t1)/dti) + 1
      
60    write(6,'(A)') 'ENTER DISTANCE RANGE (x1, x2, nx IN DEGREES):'
      read (5,    *)  x1, x2, nx              !DISTANCE RANGE & # DISTANCE STEP
      dxi = (x2-x1)/float(nx-1)               !DISTANCE SAMPLING INTERVAL
      write(6,'(A)') 'ENTER NUMBER OF RANDOM TRACES TO LAUNCH:'
      read (5,    *)  ntr
      write(6,'(A)') 'ENTER RANDOMIZING KERNEL:'
      read (5,    *)  IDUM
      write(6,*) 'IDUM:',IDUM
!      call srand(idum)

      call init_random_seed()
      write(6,'(A)') 'ENTER EARTHQUAKE DEPTH:'
      read (5,    *)  qdep
      write(6,*) 'QDEP:',qdep
      write(6,'(A)') 'ENTER 1) EARTH, or 2) MOON:'
      read (5,    *)  EorM
      write(6,*) 'EorM',EorM
      write(6,'(A)') 'ENTER 1=P, or 2=SH, 3=SV:'
      read (5,    *)  ip0
      write(6,*) 'HI1:',ip0
      write(6,'(A)') 'ENTER MAX SCATTERING DEPTH:'
      read (5,    *)  scat_depth
      write(6,*) 'HI2:',scat_depth
      write(6,'(A)') 'ENTER SCATTERING PROBABILITY:'
      read (5,    *)  scat_prob
      
      if (EorM  ==  1) then
       deg2km = re*d2r
       erad   = re
      else
       deg2km = rm*d2r
       erad   = rm
      end if

      do I = 1, nlay0                     !read IN VELOCITY MODEL
       read(1,*,IOSTAT=status) z_s(i),r_s(i),vs(i,1),vs(i,2),rh(i)
       if (status /= 0) exit
       call FLATTEN_NEW(z_s(i),vs(i,1),z(i),vf(i,1),erad)!FLATTEN VELOCITIES
       call FLATTEN_NEW(z_s(i),vs(i,2),z(i),vf(i,2),erad)         
       nlay = I                           !NUMBER OF LAYERS
111   end do
      CLOSE (1)
      write(6,*) 'NLAY:',nlay
      
      write(6,*) ' '      
      write(6,'(A)')'************************* Table of Model Interfaces' &
      ,'**********************'
      write(6,'(A)')' Depth  Top Velocities  Bot Velocities    -----Flat' & 
      ,'Earth Slownesses-----'
      write(6,'(A)')'             vp1  vs1        vp2  vs2       p1     ' &  
      ,' p2      s1      s2'
      
      do I = 2, nlay 
       if (z(i) == z(i-1)) then                !ZERO LAYER THICK=DISCONTINUITY
        scr1=1./vf(i-1,1)                      !P-VELOCITY UPPER
        scr2=1./vf(i,1)                        !P-VELOCITY LOWER 
        scr3=999.                              !FLAG if S VELOCITY = ZERO
        if (vf(i-1,2) /= 0.) scr3=1./vf(i-1,2) !S-VELOCITY UPPER
        scr4=999.                              !FLAG if S VELOCITY = ZERO
        if (vf(i  ,2) /= 0.) scr4=1./vf(i  ,2) !S-VELOCITY LOWER
         write(6,FMT=22) z_s(i),i-1,vs(i-1,1),vs(i-1,2), &
                        i,vs(i,1),vs(i,2),scr1,scr2,scr3,scr4
       end if
      end do
22    FORMAT (f6.1,2(i5,f6.2,f5.2),2x,2f9.6,2x,2f9.6)
      write(6,*) ' '      


      f0 = 0.
      
      do I = 1, nlay+5                      !BUILD Q(z) MODEL
       Q(I) = 10000.
!       if ((z_s(I) < 15.).AND.(z_s(i-1) /= 15.) ) then
!        Q(I) = 3000.
!       else if ((z_s(I) < 80.).AND.(z_s(i-1) /= 80.) ) then
!        Q(I) = 3000.
!       else if ((z_s(I) < 220.).AND.(z_s(i-1) /= 220.) ) then
!        Q(I) = 3000.
!       else if ((z_s(I) < 670.).AND.(z_s(i-1) /= 670.) ) then
!        Q(I) = 3000.
!       else if ((z_s(I) < 5149.5).AND.(z_s(i-1) /= 5149.5) ) then
!        Q(I) = 3000.
!       else
!        Q(I) = 3000.
!       end if
       if (EorM  ==  2) Q(I) = 7000       !FOR MOON Q IS HIGH UNTIL ??CORE??
      end do

      iz1 = 1
      do while (qdep > z_s(iz1))               !FIND WHICH LAYER QUAKE STARTS IN
       iz1 = iz1 + 1
      end do
      write(6,*) 'DEPTH:',iz1,z_s(iz1)
      
      write(6,*) 'ZEROING STACKS:'            !ZERO STACKS
      do I = 1, nx
       do J = 1, nt
        do k = 1, 3
	 wf(I,J,k) = 0.
        end do
       end do
      end do

      write(6,'(A)') 'ENTER OUTPUT FILE NAME:'!REQUEST OUTPUT FILE NAME
      read (5,'(A)')  ofile                   !
      
      write(6,*) 'CALCULATING SOURCE:'        !CALCULATING SOURCE
      pi = atan(1.)*4.                        !
      P0 = dti*4.                             !DOMINANT PERIOD
      nts = nint(P0*4./dti)+1                 !# OF POINTS IN SOURCE SERIES
      if (nts < 31) nts = 31
      nts1 = 1000
      do I = 1, nts1
       mt(I) = 0.
      end do
      do I = 1, nts                           !SOURCE-TIME FUNCTION
       t0 = dti*float(I-1)-P0
       mt(I) = -4.*pi**2.*P0**(-2.)*(t0-P0/2.) &
               *exp(-2.*pi**2.*(t0/P0-0.5)**2.)
       write(6,*) t0,mt(i)
      end do

      datt = 0.02
      do I = 1, 101                           !SOURCES * ATTENUATION
       dtst1 = float(I-1)*datt                !ATTENUATION
       call attenuate(mt,mtsc,nts1,dti,dtst1) !
       pow1 = 0.
       do J = 1, nts1                         !
        mts(I,1,J) =  mtsc(J)                 !
        mts(I,3,J) = -mtsc(J)                 !
        pow1 = pow1 + mtsc(J)**2
       end do                                 !
       nfil = 5
       call TILBERT(mtsc,dti,nts1,nfil,b,e)   !HILBER TRANSFORM (pi/2PHASESHFT)
       pow2 = 0.                              !ZERO POWER OF SERIES
       do K = 1, nts1                         !COPY HILBERT SERIES TO CORRECT
        mts(I,2,K) = -b(K)                    !
        mts(I,4,K) =  b(K)
        pow2 = pow2 + b(k)**2                 !CUMULATIVE POWER
       end do
       do K = 1, nts1                         !NORMALIZE HILBERTS
        mts(I,2,K) = mts(I,2,K)*pow1/pow2     !
        mts(I,4,K) = mts(I,4,K)*pow1/pow2     !
       end do
      end do                                  !
      
      
      open(23,FILE='source.out')              !OUTPUT SOURCE
      write(23,*) nts,101                     !
      write(23,FMT=888) 999.99,(datt*float(J-1),J=1,101)
      do I = 1, nts
       write(23,FMT=888) float(I-1)*dti,(mts(J,2,I)*1.,J=1,101)
      end do
      close(23)
      

      n180 = nint(180/dxi)
      write(6,*) 'BEGINNING RANDOM TRACE:',nts
      do I = 1, ntr                           !FOR EACH TRACE

	   ! Pick P- ,SH- or SV- initial phonon state randomly.
       CALL SYSTEM_CLOCK(COUNT=nclock)
       seed = (nclock)! + 11 * (/ (k - 1, k = 1, nseed) /)
       CALL srand(seed)
       iz = iz1
	   r0 = rand()							  !First rand output not random
											  ! It is seed (clock) dependent
       
       ip = int(5.*r0)+1 
       if (iz == 1) ip = 1                    ! Surface impact=P-wave only
											  !iz is layer in which source is
	   if (ip /= 1) then
        r0 = rand()
	    if (r0 < 1/3) then
		    ip = 1 !P
		else if ((r0 >= 1/3).and.(r0 < 2/3)) then
			ip = 2 !SH
		else 
		    ip = 3 !SV
		end if
	   end if
	   
	  iwave = ip							  !1 = P, 2 = SH, 3 = SV
 
	  if (iwave == 3) iwave = 2			    ! ASSUMING ISOTROPY SO v_SH == v_SV
       
      if (iz == 1) then                      !IF QUAKE STARTS AT SURF GO DOWN
        angst = pi/2.                         !0 - 90 (0 = DOWN)
	  else                                   !IF QUAKE AT DEPTH then UP OR DOWN
        angst = pi                            !0 - 180 (0 = DOWN)
      end if                                 !
       
	   t = 0.                                 !SET START TIME = ZERO
       x = 0.                                 !START LOCATION = ZERO
       s = 0.                                 !SET START ATTENUATION = ZERO
       a = 0.0001                             !START AMPLITUDE = 1.
       dold = 0.
       d = 0.                                 !START AT ZERO KM TRAVELED
       x_sign = 1.                            !DISTANCE DIRECTION
       
       r0 = rand()                        !SELECT RANDOM RAY PARAMETER 
       ang1 = angst*r0                        !Randomly select angle of

       p    = abs(sin(ang1))/vf(iz,iwave)
       az   = 0.
       a    = cos(ang1*2.-pi/4.)              !SOURCE AMPLITUDE
       ncaust = 0                             !# OF CAUSTICS STARS AT 0.

       a = 1.                                 !
       if (ang1 < pi/2.) then
        ud = 1	
       else
        ud = -1
       end if
       NITR = 0

       n_iter_last = -999
       ix_last = -999
       it_last = -999

       
       do while ((t < t2).AND.(NITR < 100*nlay))!TRACE UNTIL TIME PASSES TIME WINDOW
        NITR = NITR + 1
        r0 = rand()                       !RANDOM NUMBER FROM 0 TO 1
       
		if (z_s(iz) < scat_depth) then
          r0 = rand()
          if (r0 < scat_prob) then

	        r0 = rand()
	        if (r0 < 0.5) x_sign=-x_sign
			
			r0 = rand()
            if (r0 < scat_prob) ud = -ud
	  
            r0 = rand()
	        r0 = ( r0 - 0.5 )
            p = p1 + r0*(1./vf(iz,iwave)-p1)!*scat_prob
            
			do while ((p < p1).OR.(p >= 1./vf(iz,iwave)) ) !p2(iwave)))
              r0 = rand()                       !SELECT RANDOM RAY PARAMETER 
              ang1 = angst*r0
              p = abs(sin(ang1))/vf(iz,iwave)
            end do
          
		    r0 = rand()                        !
	        r1 = rand()                        !
            if (r1 < 0.5) az = az - pi
	        az = az + asin(r0**2)                  !
	        if (az < -pi) az = az + 2.*pi
            if (az >  pi) az = az - 2.*pi
			
          end if 
        end if


	    if (iz /= 1) then
	      if (abs(vf(iz-1,iwave)) > 0.) then
            utop = 1./vf(iz-1,iwave)              !SLOWNESS AT TOP OF LAYER
		  else
            utop = 0.
		  end if 
		
		if (abs(vf(iz,iwave)) > 0.) then
          ubot = 1./vf(iz,iwave)                !SLOWNESS AT BOTTOM OF LAYER
		else
          ubot = 0.
		end if
         
		h    = z(iz)-z(iz-1)                  !THICKNESS OF LAYER
		imth = 2                              !INTERPOLATION METHOD
		
		call LAYERTRACE(p,h,utop,ubot,imth,dx1,dt1,irtr1)
		dtstr1 = dt1/Q(iz)                    !t* = TIME/QUALITY FACTOR
	  else
	    irtr1  = -1
        dx1    = 0.
        dt1    = 0.
        dtstr1 = 0.
	   end if
        
        if (irtr1 == 0) then
         ud = -ud
        else if (irtr1 >= 1) then
         d = d + ((z_s(iz)-z_s(iz-1))**2+dx1**2)**0.5!
         
         t = t + dt1                    !TRAVEL TIME
         x = x + dx1*x_sign*cos(az)     !EPICENTRAL DISTANCE TRAVELED-km
         s = s + dtstr1                 !CUMULATIVE t*
        end if
        
	IF ( (iz > 1).AND.(abs(irtr1) == 1).AND. &
            (iz < nlay) ) then
	 if ( (iz > 1).AND.(iz <= nlay) ) h = z_s(iz)-z_s(iz-1)

          if (ip  ==  2) then
          
           if ( (ud == 1) ) then               !IF DOWNGOING SH WAVE
            call REFTRAN_SH(p,vf(iz-1,2),vf(iz,2),rh(iz-1),rh(iz), &
                           ar,at)
           else if ((ud == -1) ) then          !IF UPGOING SH WAVE
            call REFTRAN_SH(p,vf(iz,2),vf(iz-1,2),rh(iz),rh(iz-1), &
                           ar,at)
           end if
          else

           if ( (ud == 1) ) then               !IF DOWNGOING P-SV WAVE
            call RTCOEF2(p,vf(iz-1,1),vf(iz-1,2),rh(iz-1), &
                          vf(iz  ,1),vf(iz  ,2),rh(iz), &
                          ip,arp,ars,atp,ats)
           else if ((ud == -1) ) then          !IF UPGOING P-SV WAVE
            call RTCOEF2(p,vf(iz  ,1),vf(iz  ,2),rh(iz  ), &
                          vf(iz-1,1),vf(iz-1,2),rh(iz-1), &
                          ip,arp,ars,atp,ats)           
!           write(6,*) 'HI'
           end if
          end if
          
          r0 = rand()                       !RANDOM NUMBER FROM 0 TO 1

          if (ip  ==  2) then                   !IF SH-WAVE

           if (h > 0.) then                    !IF GRADIENT, then
            if (r0 < (abs(ar)/(abs(ar)+abs(at)))/P0**2) then!CHECK FOR REFLECTN
             if (ar < 0) a = -a                !OPPOSITE POLARITY
             ud = -ud                           !DOWNGOING/UPGOING
            end if                              !
           else                                 !IF INTERFACE then
            if (r0 < (abs(ar)/(abs(ar)+abs(at)))) then!CHECK FOR REFLECTION
             if (ar < 0) a = -a                !OPPOSITE POLARITY
             ud = -ud                           !DOWNGOING/UPGOING
            end if                              !
           end if                               !

          else                                  !IF P- OR SV-WAVE 
           if (h <= 0.) then
            rt_sum = abs(arp)+abs(atp)+abs(ars)+abs(ats)    !SUM OF REFL/TRAN COEFS

            rt_min = 0.                          !RANGE PROBABILITIES FOR P REFL
	    rt_max = abs(arp)/rt_sum             !
            if ( (r0 >= rt_min).AND.(r0 < rt_max) ) then!CHECK if REFLECTED P
             if (arp < 0) a = -a                 !REVERSE POLARITY
             ud = -ud                            !UPGOING <-> DOWNGOING
 	     ip = 1                              !P WAVE
            end if                               !
           
!             if (z_s(iz) == 137.) write(6,*) arp,atp,r0,ud,ip,a

            rt_min = rt_max                      !RANGE PROBABILITIES 4 SV REFL
	    rt_max = rt_max+abs(ars)/rt_sum      !
            if ( (r0 >= rt_min).AND.(r0 < rt_max) ) then!CHECK if REFLECTED SV
             if (ars < 0) a = -a                 !REVERSE POLARITY
             ud = -ud                            !UPGOING <-> DOWNGOING
             ip = 3                              !SV WAVE
            end if                               !

            rt_min = rt_max                      !RANGE PROBABILITIES 4 P TRANS
	    rt_max = rt_max+abs(atp)/rt_sum      !
            if ( (r0 >= rt_min).AND.(r0 < rt_max) ) then!CHECK if TRAMSITTED P
             ip = 1                              !P WAVE
            end if                               !

            rt_min = rt_max                      !RANGE PROBABILITIES 4 SV TRANS
            rt_max = rt_max+abs(ats)/rt_sum      !
            if ( (r0 >= rt_min).AND.(r0 <= rt_max) ) then!CHECK if TRANSMITTED SV
             ip = 3                              !SV WAVE
            end if                               !
           end if      
          end if                                !end if: SH, OR P-SV
         
        else if (iz == nlay) then               !ONCE HIT OTHER SIDE OF CORE
	 ud = -ud
	 x = x + 180*deg2km
	end if
	
        
!   |FIX NEXT if FOR DIFFRACTED WAVES: 
	IF (irtr1 == 2) then             !RAY TURNS IN LAYER FOLLOW 1 LEN
	 ud = -ud
         ncaust = ncaust + 1                   !# OF CAUSTICS
        end if
        
	r0 = rand()
        
        if (iz == 1) then                      !IF RAY HITS SUFACE then RECORD
         ud = 1                                !RAY NOW MUST TRAVEL DOWN
         ix = nint((abs(x)/deg2km-x1)/dxi) + 1      !EVENT TO SURFACE HIT DISTANCE 
				 ixtemp = ix


         xo = x1 + float(ix-1)*dxi
!	 write(6,*) xo,abs(x)/deg2km,ix
	 if ( abs(xo-abs(x)/deg2km) > 0.1) cycle


         if (ix > n180) ix = n180 - (ix-n180)
         if (ix < 1) ix = -ix + 1

		write(54,*) ix,ixtemp,n180

         if (abs(x/deg2km)-abs(x1+dxi*float(ix-1)) > 0.2) cycle
!         if (x>0.001)write(6,*) x/deg2km,x1+dxi*float(ix-1),ix,dxi
         IT = nint((t       -t1)/dti) + 1 

         ims = int(s/datt)+1
	 if (ims > 100) ims = 100
         if (ims <=   1) ims =   2
!         ims = 2
         s1 = float(ims-1)*datt
         s2 = float(ims  )*datt
	 frac = (s-s1)/(s2-s1)
         if (ncaust <= 1) then
	  icaust = 1
	 else
	  icaust = ncaust
          do while (icaust > 4)
           icaust = icaust - 4
          end do
         end if

         if ( (IT > 1-nts).and.(IT <= nt0+nts) ) then
          if ( (ip == 1).or.(ip==3) ) then
           c_mult(1) = cos(ang1)*cos(az)
           c_mult(2) = sin(ang1)  *sin(az)*0.
           c_mult(3) = sin(ang1)  *cos(az)*.1
          else if (ip == 2) then
           c_mult(1) = 0.!cos(asin(p*vf(iz,iwave)))*sin(az)
	   c_mult(2) = cos(ang1)*cos(az)
	   c_mult(3) = cos(ang1)*sin(az)
          else if (ip == 3) then
	   c_mult(3) = cos(ang1)*cos(az)
	   c_mult(2) = cos(ang1)*sin(az)
           c_mult(1) = p*vf(iz,iwave)!*cos(az)
          end if
           p    = abs(sin(ang1))/vf(iz,2)
!          if (it>1)write(6,*) ip,iwave,ix,it,a,ang1*180/pi,c_mult(1),c_mult(2),c_mult(3)

          if((n_iter_last == nitr).and.(ix_last==ix) &
	                           .and.(abs(it_last-it)<5.)) cycle

          n_iter_last = nitr
	  ix_last = ix
	  it_last = it
	  do ic = 1, 3
	   do JJ = 1, nts
            JT = IT + JJ - 1
            if ( (JT > 0).AND.(JT <= nt0).AND.(a /= 0.) ) then
             wf(ix,JT,ic) = wf(ix,JT,ic) + a * c_mult(ic) &
                      * (   (1.-frac)*mts(ims-1,icaust,JJ) &
                          + (   frac)*mts(ims  ,icaust,JJ) )!ATTENUATION
            end if
           end do
          end do
         end if
        end if
        iz = iz + ud                           !GO TO NEXT DEPTH
        if (iz < 1) t = 999999.
       end do
       if (mod(float(I),float(ntr)/20.) == 0) then !STATUS REPORT
        write(6,*) nint(float(I)/float(ntr)*100),'% COMPLETE'
       end if
      end do
      wf(1,1,1) = 1.
      wf(1,1,2) = 1.
      wf(1,1,3) = 1.
      
      do ic = 1, 3
       ofile2 = trim(ofile)//'.'//cmp(ic)
       OPEN(22,FILE=trim(ofile2),STATUS='UNKNOWN')    !OPEN OUTPUT FILE
       
       write(22,*) nt,nx
       write(22,FMT=888) 999.99,(x1+dxi*float(J-1),J=1,nx)
      
       do I = 1, nt
        do J = 1, nx
         if (abs(wf(J,I,ic)) > 999.9999) wf(J,I,ic) = 999.9999*wf(J,I,ic)/abs(wf(J,I,ic))
        end do
        write(22,FMT=888) t1+float(I-1)*dti,(wf(J,I,ic)*0.1,J=1,nx)
       end do
       CLOSE(22)
      end do
      
!      do I = 1, nx
!       delta = x1+dxi*float(I-1)
!       idelt1 = int(delta)
!       idelt2 = int(10.*(delta-float(idelt1)))
!       do ic = 1, 3
!        write(ofile,FMT=898) idelt1,idelt2,cmp(ic)
!        write(6,*) delta,idelt1,idelt2,ofile
!        do J = 1, nt
!         w(J) = wf(I,J,ic)*0.1
!        end do
!	ofile2 = trim(ofile)
!!        call rsac_syn(trim(ofile2),w,nt,0.,dti,delta)
!       end do
!      end do

close(54)
      
      
888   FORMAT(F10.2,1X,361(F10.6,1X))
!898   FORMAT('./SAC/D_',I3.3,'.',I1.1,'.',A3,'.sac')

!   deallocate(seed)
   stop
end program statsyn_1d_test



subroutine init_random_seed()
    INTEGER :: i, n, nclock
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed
    n=100000
    CALL RANDOM_SEED(size = n)
    ALLOCATE(seed(n))
         
    CALL SYSTEM_CLOCK(COUNT=nclock)
    seed = nclock + 37 * (/ (i - 1, i = 1, n) /)
    CALL RANDOM_SEED(PUT = seed)
          
    deallocate(seed)
end subroutine init_random_seed
      
      
subroutine ATTENUATE(sin,sout,ndat,dt,tstar)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   |THIS SUBROUTINE ATTENUATES AN INPUT SIGNAL (sin) BY A VALUE (tstar) !   !
!   |                                                                    !   !
!   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   !   !
!   |     CONTACT: jflawrence@stanfrd.edu                                    !   !
!   |                                                                    !   !
!   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      !   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   |DECLARE VARIABLES AND SET PARAMETERS:                               !   !
      real           sin(*),sout(*),tstar
      integer        ndat,nfreq              !# OF POINTS IN TIME & FREQ DOMAIN
      integer        MAXPTS                  !MAX # OF POINTS & ITERATIONS
      PARAMETER(     MAXPTS = 16384)         !
      real           xs(16384)               !SCRATCH SPACE
      complex        xf(16384),yf(16384)     !SCRATCH SPACE
      real           dt,df                   !TIME & FREQ SAMPLING INTERVAL
      real           pi                      !SET PI = 3.14....
      real           w,dw                    !FREQUCNEY VARIABLES
      real           damp
      
      call np2(ndat,npts)                    !FIND POWER OF TWO, npts >= ndat
      if (npts > MAXPTS) then               !CHECK THAT ARRAY IS NOT TOO BIG
       write(6,*) 'WARNING: SERIES TRUNCATED TO:',MAXPTS
       npts = MAXPTS
      end if                                 !
      call PADR(sin,ndat+1,npts)             !PAD SERIES WITH ZEROS
      call COPYR(sin,xs,npts)                 !COPY INITIAL DENOMINATOR
      
      call GET_SPEC(xs,npts,dt,xf,nfreq,df) !GET SPECTRUM OF x
      pi = atan(1.)*4.                       !SET PI = 3.14....
      dw = 2.*pi*df                          !ANGULAR FREQUENCY SAMPLING INTERVAL
      dadw = -tstar*dw                       !DERIVATIVE dA(w)di = -dt**dw
      
      do I = 1, nfreq                        !APPLY ATTENUATION FILTER
       damp = exp(float(I-1)*dadw)
       w     = dw* float(I-1)                !ANGULAR FREQUENCY
       if (damp < 0.) damp = 0.
       yf(I) = xf(I)*cmplx(damp,0.)
       yf(I) = yf(I)*exp( cmplx(0.,w*tstar))
      end do

      call GET_TS(yf,nfreq,df,0,sout,npts,dt) !GET TIME SERIES OF ATTENUATED SPEC

      return
end subroutine attenuate                      !END ATTENUATE
      

subroutine np2(npts,np)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   |THIS SUBROUTINE FINES THE POWER OF TWO THAT IS GREATER THAN OR EQUAL!   !
!   |     TO npts.                                                       !   !
!   |                                                                    !   !
!   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   !   !
!   |     CONTACT: jflawrence@stanford.edu                               !   !
!   |                                                                    !   !
!   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      !   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      integer     npts,np                      !INPUT & 2^n=np NUMBER OF POINTS
      np = 2                                   !ASSUME STARTING AT 2
      do while (npts > np)                    !
       np = np * 2                             !KEEP INCREASING SIZE*2 UNTIL BIG
      end do
      return
      end subroutine np2                       !END np2


subroutine COPYR(f1,f2,npts)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   |THIS SUBROUTINE COPIES ONE real VECTOR TO ANOTHER                   !   !
!   |                                                                    !   !
!   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   !   !
!   |     CONTACT: jflawrence@stanford.edu                               !   !
!   |                                                                    !   !
!   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      !   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      integer     I,npts                       !STEP & NUMBER OF POINTS
      real        f1(*),f2(*)                  !INPUT AND OUTPUT SERIES
      do I = 1, npts                           !
       f2(I) = f1(I)                           !COPY POINTS
      end do
      
      return
end subroutine copyr                           !END COPYR

      
subroutine PADR(f1,n1,n2)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   |THIS SUBROUTINE COPIES ONE real VECTOR TO ANOTHER                   !   !
!   |                                                                    !   !
!   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   !   !
!   |     CONTACT: jflawrence@stanford.edu                               !   !
!   |                                                                    !   !
!   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      !   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      integer     I,n1,n2                      !STEP, START & end POINTS
      real        f1(*)                        !SERIES TO ADD ZEROS TO
      do I = n1, n2                            !
       f1(I) = 0.                              !MAKE ZERO
      end do
      return
end subroutine padr                            !END PADR
      
      
      
      

subroutine REFTRAN_SH(p,b1,b2,rh1,rh2,ar,at)
      real       p,ar,at
      real       pi,j1,j2,b1,rh1,rh2
      pi   = atan(1.)*4.
      r2d = 180./pi
      if (p*b1 <= 1.) then
       j1   = asin(p*b1)
      else
       j1 = pi/2.
      end if
      if (p*b2 <= 1.) then
       j2   = asin(p*b2)
      else
       j2   = pi/2. 
      end if
      
      DD   = rh1*b1*cos(j1)+rh2*b2*cos(j2)

      ar   = (rh1*b1*cos(j1)-rh2*b2*cos(j2))/DD
      at   = 2.*rh1*b1*cos(j1)/DD
      
      return
end subroutine REFTRAN_SH



! subroutine RTCOEF calculates reflection/transmission coefficients
! for interface between two solid layers, based on the equations on 
! p. 150 of Aki and Richards.
!
!  Inputs:    vp1     =  P-wave velocity of layer 1 (top layer)
!  (real)     vs1     =  S-wave velocity of layer 1
!             den1    =  density of layer 1
!             vp2     =  P-wave velocity of layer 2 (bottom layer)
!             vs2     =  S-wave velocity of layer 2
!             den2    =  density of layer 2
!             pin     =  horizontal slowness (ray parameter)
!             PorS    =  1=P-WAVE, 3=SV-WAVE
!  returns:   arp     =  down P to P up     (refl)
!  (complex)  ars     =  down P to S up     (refl)
!             atp     =  down P to P down   (tran)
!             ats     =  down P to S down   (tran)
!   OR:
!             arp     =  down S to P up     (refl)
!             ars     =  down S to S up     (refl)
!             atp     =  down S to P down   (tran)
!             ats     =  down S to S down   (tran)
!
! NOTE:  All input variables are real.  
!        All output variables are complex!
!        Coefficients are not energy normalized.
!
subroutine RTCOEF2(pin,vp1,vs1,den1,vp2,vs2,den2,pors, &
                         rrp,rrs,rtp,rts)
      IMPLICIT     NONE
      real         vp1,vs1,den1,vp2,vs2,den2     !VELOCITY & DENSITY
      integer      pors                          !P (1) OR S (2)                          
      complex      a,b,c,d,e,f,g,H               !TEMPORARY VARIABLES
      complex      cone,ctwo                     !complex  = 1 OR = 2
      complex      va1,vb1,rho1,va2,vb2,rho2     !VELOCITY & DENSITY (complex)
      real         pin                           !INPUT SLOWNESS
      complex      p                             !INPUT SLOWNESS (P OR S)
      complex      si1,si2,sj1,sj2               !SIN OF ANGLE
      complex      ci1,ci2,cj1,cj2               !complex SCRATCH
      complex      term1,term2                   !complex SCRATCH
      complex      DEN                           !DENOMINATOR
      complex      trm1,trm2                     !complex SCRATCH
      complex      arp,ars,atp,ats               !REFLECTION & TRANSMISSION COEFS
      real         rrp,rrs,rtp,rts               !REFLECTION & TRANSMISSION COEFS
      
      va1    = cmplx(vp1,  0.)                   !MAKE VEL & DENSITY complex
      vb1    = cmplx(vs1,  0.)
      rho1   = cmplx(den1, 0.)
      va2    = cmplx(vp2,  0.)
      vb2    = cmplx(vs2,  0.)
      rho2   = cmplx(den2, 0.)

      p      = cmplx(pin,  0.)                   !MAKE RAY PARAMETER COMPEX      
      
      cone   = cmplx(1.,0.)                      !complex 1 & 2
      ctwo   = cmplx(2.,0.)
      
      si1    = va1 * p                           !SIN OF ANGLE
      si2    = va2 * p          
      sj1    = vb1 * p
      sj2    = vb2 * p       
!
      ci1    = csqrt(cone-si1**2)                !
      ci2    = csqrt(cone-si2**2)
      cj1    = csqrt(cone-sj1**2)
      cj2    = csqrt(cone-sj2**2)         
!
      term1  = (cone-ctwo*vb2*vb2*p*p)
      term2  = (cone-ctwo*vb1*vb1*p*p)
      
      a      = rho2*term1-rho1*term2
      b      = rho2*term1+ctwo*rho1*vb1*vb1*p*p
      c      = rho1*term2+ctwo*rho2*vb2*vb2*p*p
      d      = ctwo*(rho2*vb2*vb2-rho1*vb1*vb1)
      E      = b*ci1/va1+c*ci2/va2
      F      = b*cj1/vb1+c*cj2/vb2
      G      = a-d*ci1*cj2/(va1*vb2)
      H      = a-d*ci2*cj1/(va2*vb1)
      DEN    = E*F+G*H*p*p
!
      if (PorS  ==  1) then
       trm1   = b*ci1/va1-c*ci2/va2          
       trm2   = a+d*ci1*cj2/(va1*vb2)
       arp    = (trm1*F-trm2*H*p*p)/DEN           !refl down P to P up
       trm1   = a*b+c*d*ci2*cj2/(va2*vb2)       
       ars    = (-ctwo*ci1*trm1*p)/(vb1*DEN)      !refl down P to S up
       atp    = ctwo*rho1*ci1*F/(va2*DEN)         !trans down P to P down
       ats    = ctwo*rho1*ci1*H*p/(vb2*DEN)       !trans down P to S down
      else
       trm1   = a*b+c*d*ci2*cj2/(va2*vb2)       
       arp    = (-ctwo*cj1*trm1*p)/(va1*DEN)      !refl down S to P up
       trm1   = b*cj1/vb1-c*cj2/vb2               
       trm2   = a+d*ci2*cj1/(va2*vb1)
       ars    = -(trm1*E-trm2*G*p*p)/DEN          !refl down S to S up
       atp    = -ctwo*rho1*cj1*G*p/(va2*DEN)      !trans down S to P down 
       ats    = ctwo*rho1*cj1*E/(vb2*DEN)         !trans down S to S down
      end if
      
      rrp = real(arp)!**2+imag(arp)**2)**0.5
      rrs = real(ars)!**2+imag(ars)**2)**0.5
      rtp = real(atp)!**2+imag(atp)**2)**0.5
      rts = real(ats)!**2+imag(ats)**2)**0.5
      
!      write(6,*) 'HI1:',a,b,c,d,E,F,G,H,DEN
      
      return
end subroutine rtcoef2




subroutine FLATTEN(z_s,vs,z_f,vf_f)
!   !FLATTEN calculates flat earth tranformation.
      erad=6371.
      r=erad-z_s
      z_f=-erad*alog(r/erad)
      vf_f=vs*(erad/r)
      return
end subroutine flatten

subroutine FLATTEN_NEW(z_s,vs,z_f,vf_f,erad)
      real     z_s,z_f,vf_f,vs,erad,r
      r=erad-z_s
      z_f=-erad*alog(r/erad)
      vf_f=vs*(erad/r)
      return
end subroutine flatten_new

subroutine LAYERTRACE(p,h,utop,ubot,imth,dx,dt,irtr)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   ! LAYERTRACE calculates the travel time and range offset
!   ! for ray tracing through a single layer.
!   !
!   ! Input:    p     =  horizontal slowness
!   !           h     =  layer thickness
!   !           utop  =  slowness at top of layer
!   !           ubot  =  slowness at bottom of layer
!   !           imth  =  interpolation method
!   !                    imth = 1,  v(z) = 1/sqrt(a - 2*b*z)
!   !                         = 2,  v(z) = a - b*z
!   !                         = 3,  v(z) = a*exp(-b*z)
!   !
!   ! returns:  dx    =  range offset
!   !           dt    =  travel time
!   !           irtr  =  return code
!   !                 = -1,  zero thickness layer
!   !                 =  0,  ray turned above layer
!   !                 =  1,  ray passed through layer
!   !                 =  2,  ray turned within layer, 1 segment counted in dx,dt
!   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      if (h == 0.) then      !check for zero thickness layer
         dx=0.
         dt=0.
         irtr=-1
         return         
      end if
!   !
      u=utop
      y=u-p
      if (y <= 0.) then   !complex vertical slowness
         dx=0.            !ray turned above layer
         dt=0.
         irtr=0
         return
      end if
!
      q=y*(u+p)
      qs=sqrt(q)
!   ! special function needed for integral at top of layer
      if (imth == 2) then
         y=u+qs
         if (p /= 0.) y=y/p
         qr=alog(y)
      else if (imth == 3) then
         qr=atan2(qs,p)
      end if      
!
      if (imth == 1) then
          b=-(utop**2-ubot**2)/(2.*h)
      else if (imth == 2) then
          vtop=1./utop
          vbot=1./ubot
          b=-(vtop-vbot)/h
      else
          b=-alog(ubot/utop)/h
      end if  
!
      if (b == 0.) then     !constant vfocity layer
         b=-1./h
         etau=qs
         ex=p/qs
         go to 160
      end if
!   !integral at upper limit, 1/b factor omitted until end
      if (imth == 1) then
         etau=-q*qs/3.
         ex=-qs*p
      else if (imth == 2) then
         ex=qs/u
         etau=qr-ex
         if (p /= 0.) ex=ex/p
      else
         etau=qs-p*qr
         ex=qr
      end if
!   ! check lower limit to see if we have turning point
      u=ubot
      if (u <= p) then   !if turning point,
         irtr=2          !then no contribution
         go to 160       !from bottom point
      end if 
      irtr=1
      q=(u-p)*(u+p)
      qs=sqrt(q)
!
      if (imth == 1) then
         etau=etau+q*qs/3.
         ex=ex+qs*p
      else if (imth == 2) then
         y=u+qs
         z=qs/u
         etau=etau+z
         if (p /= 0.) then
            y=y/p
            z=z/p
         end if
         qr=alog(y)
         etau=etau-qr
         ex=ex-z
      else
         qr=atan2(qs,p)
         etau=etau-qs+p*qr
         ex=ex-qr
      end if      
!
160   dx=ex/b
      dtau=etau/b
      dt=dtau+p*dx     !convert tau to t
!
      return
end subroutine layertrace





subroutine NPOW2(npts,np)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   |THIS SERIES DETERMINES THE POWER OF TWO THAT IS EQUAL OR JUST       !   !
!   |     GREATER THAN THE NUMBER OF POINTS SUPPLIED:                    !   !
      integer npts,np
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      np = 0
      do while (2**np < npts)
       np = np + 1
      end do

      return
end subroutine npow2 



subroutine TILBERT(a,dt,npts,nfil,b,e)
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
! of time series in the time domain.
!     Inputs:   a    =  time series
!               dt   =  time spacing
!               npts =  number of points in a
!               nfil =  half-number of points in filter
!     returns:  b    =  Hilbert transform of time series
!               e    =  envelope time function
!
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
      real       a(*),b(*),e(*),h(2001)
      real       dt
      integer    npts,nfil,nfiltot,I,J,II,JJ,i1,i2
      SAVE       dt0,npts0,h,nfiltot             !STORE VARIABLES TO SAVE TIME
      pi = atan(1.)*4.                           !SET PI = 3.14.....

      if ( (dt /= dt0).and.(npts /= npts0)) then !SET UP THE FILTER
       nfiltot = 2*nfil+1                        !# OF POINTS IN FILTER
       do 10 i = 1, nfiltot                      !FOR EACH FILTER POINT
        t=float(i-nfil-1)*dt                     !TIME OF ITH POINT
        if (i /= nfil+1) then                    !CALCULATE FILTER
         h(i) = -1./(pi*t)
        else                                     !AVOID SINGULARITY
         h(i) = 0.
        end if
10     end do
       call TAPERR(h,nfiltot,0.5,0.5)
       dt0     = dt                              !STORE SAMPLING INTERVAL
       npts0   = npts                            !STORE NUMBER OF POINTS
      end if

      call ZEROR(e,npts)                         !ZERO ENVELOPE
      call ZEROR(b,npts)                         !ZERO HILBERT TRANSFORM

      i1 = 1 + nfil
      i2 = npts - nfil
      do 50 i=i1,i2
       do 40 j = 1, nfiltot
        ii  = i - (j-nfil-1)
        b(i)= b(i)+a(ii)*h(j)
40     end do
       b(i)=b(i)*dt
50    end do
      do 70 i=i1,i2
       e(i) = ( (a(i)**2+b(i)**2) )**0.5
70    end do
      return
end subroutine tilbert







subroutine TAPERR(S1,ndat,tap1,tap2)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   | THIS SUBROUTINE TAPERS ANY SIGNAL (S1), OF LENGTH npts, FROM 1 TO  !   ! 
!   |      tap1*npts, AND FROM tap2*npts TO npts.                        !   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      real       S1(*),tap1,tap2,PI,cs
      integer    tap1n,tap2n
      integer    I,ndat
      PI = atan(1.0)*4.
      tap1n = nint(tap1*float(ndat))                 !integer TAPER POINT 1
      tap2n = nint(tap2*float(ndat))                 !integer TAPER POINT 2

      do I = 1, tap1n                                !TAPER FROM 1 TO POINT 1
         cs     = sin(float(I-1)*PI/float(tap1n-1)/2.)
         S1(I)  = S1(I) * cs**2
      end do
      do I = 1, tap2n                                !TAPER FROM POINT 2 TO end
         cs     = sin(float(I-1)*PI/float(tap2n-1)/2.)
         S1(ndat-I+1)  = S1(ndat-I+1) * cs**2
      end do

      return
end subroutine taperr


subroutine ZEROR(series,npts)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   |THIS SUBROUTINE ZEROES ANY 1D SERIES OF LENGTH TO POINT, npts:      !   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      real series(*)
      integer npts

      do I = 1, npts
       series(I) = 0.
      end do

      return
end subroutine zeror

subroutine usph2car(lon,lat,x1,y1,z1)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   !THIS SUBROUTINE CONVERTS SPHERICAL COORDINATES (LON,LAT) (RADIAN) TO!   !
!   !     CARTESIAN COORDINATES (X,Y,Z), WITH RADIUS = 1.                !   !
!   !                                                                    !   !
!   !THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   !   !
!   !    CONTACT: jflawrence@stanford.edu                                !   ! 
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   ! DECLARE VARIABLES:                                                 !   !
      real    ::  lon,lat                  !LOCATION (SPHERICAL)
      real    ::  x1,y1,z1                 !LOCATION (CARTESIAN)
      
      x1 = cos(lat) * cos(lon)             !CARTESIAN POSITION
      y1 = cos(lat) * sin(lon)             !
      z1 = sin(lat)                        !
      
      return                               !
end subroutine usph2car                    !END LON_LAT_X_Y_Z
      
      
      
      
      
subroutine ucar2sphr(x1,x2,x3,lon,lat)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   !THIS SUBROUTINE CONVERSTS TO LATITUDE AND LONGITUDE.  THE SUB   !   !
!   !     ASSUMES THAT THE COORDINATE IS AT THE SURFACE.             !   !
!   !     OUTPUTS ARE IN RADIANS:                                    !   !
!   !                                                                    !   !
!   !THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   !   !
!   !    CONTACT: jflawrence@stanford.edu                                !   ! 
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   ! DECLARE VARIABLES:                                                 !   !
      real      lon,lat,x1,x2,x3,pi
      pi = atan(1.)*4.                          !SET PI = 3.14....
      lat = arsin(x3)                           !
      if (x1 == 0.) then
       if (x2 > 0.) lon = pi / 2.
       if (x2 < 0.) lon = 3. * pi / 2.
      else
       lon = atan(x2 / x1)
       if (x1 < 0.) lon = lon + pi
       if ( (x1 > 0.).AND.(x2 < 0.) )	lon = lon + 2. * pi
      end if
      
      return                                    !
end subroutine ucar2sphr                                      !END UCAR2SPHD
      

subroutine dist_two_angles(lon1,lat1,lon2,lat2,angdist)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   !THIS SUBROUTINE USES angdis TO DETERMINE THE DISTANCE TWO POINTS    !   !
!   !     GIVEN LONGITUDE, LATITUDE FOR EACH POINT ALL IN DEGREES.       !   !
!   !     THIS SUBROUTINE DOES ALL THE RADIAN TO DEGREE CONVERSIONS.     !   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
      real    ::  lat1,lon1,lat2,lon2,angdist
      real    ::  x1,x2,x3,y1,y2,y3
      real    ::  pi,arcos

      if (lat1 /= lat2) then
       if  (abs(lon1-lon2)/abs(lat1-lat2) < 0.02) then
        angdist = abs(lat1-lat2)
        return
       end if
      end if

      call USPH2CAR(lon1,lat1,x1,x2,x3)
      call USPH2CAR(lon2,lat2,y1,y2,y3)
      angdist = abs(arcos(x1*y1 + x2*y2 + x3*y3))

      return
end subroutine dist_two_angles

      
   
real function arcos(a)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   !THIS FUCTION DETERMINES THE ARC COSINE OF AN ANGLE (a):             !   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
      implicit     none
      real      :: a, aa, pi, pi2,artan2
      aa = 1D0-a*a
      pi = atan(1.)*4D0
      pi2 = pi/2D0
      
      if (aa > 0) then
       arcos = artan2(sqrt(aa),a)
      else
       arcos = pi2-sign(pi2,a)
      end if
      return
end function arcos



real function arsin(a)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   !THIS FUCTION DETERMINES THE ARC SINE OF AN ANGLE (a):               !   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
      implicit      none
      real      ::  a, aa, pi, pi2
      aa = 1.-a*a
      pi = atan(1.)*4D0
      pi2 = pi/2D0
      if (aa > 0) then
       arsin = atan(a/sqrt(aa))
      else
       arsin = sign(pi2,a)
      end if
      return
end function arsin
      
      
      
real function artan2(y,x)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   !THIS FUCTION DETERMINES THE ARC TANGENT OF AN ANGLE (X AND Y):     !   !
!   !     THIS VERSION OF ARCTAN DOES NOT CHOKE AT (0,0):               !   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
      implicit        none
      real        ::  y,x,sign, pi, pi2
      pi = atan(1.)*4D0
      pi2 = pi/2D0
      if (x == 0) then
       artan2 = sign(pi2,y)
      else
       artan2 = atan(y/x)
       if (x < 0) then
        artan2 = artan2+sign(pi,y)
       end if
      end if
      return
end function artan2

