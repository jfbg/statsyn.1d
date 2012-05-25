      PROGRAM trace_new
!
      PARAMETER(    nlay0=10000,nt0=10001,nx0=361)
      REAL          z(nlay0),vf(nlay0,2),rh(nlay0)
      REAL          z_s(nlay0),r_s(nlay0),vs(nlay0,2)
      REAL          t,x,a,w(nt0)
      CHARACTER*100 ifile,ofile
      REAL          dx1,dt1
      INTEGER       irtr1
      INTEGER       iz,IT,JT,I,J,ic
      REAL          p,ang1
      DOUBLE PRECISION wf(nx0,nt0,3)        !STACKED DATA
      REAL          Q(nlay0)              !QUALITY FACTOR 
      REAL          dtstr1                !ATTENUATION PER LAYER
      REAL          mt(nt0)               !SOURCE-TIME FUNCTION 
      COMPLEX       ms(nt0),ss(nx0,nt0)   !SOURCE & STACKED SPECTRA
      REAL          nn(nx0,nt0)
      REAL          pi,P0
      INTEGER       n180,idelt1,idelt2
      
      
      REAL          mts(101,4,nt0)        !ATTENUATED SOURCE
      REAL          b(nt0), e(nt0)        !HILBERT TRANSFORM & ENVELOPE
      REAL          mtsc(nt0),datt,dtst1  !SCRATCH SPACE
      INTEGER       ims                   
      
      INTEGER       ncaust,icaust         !NUMBER OF CAUSTICS IN A RAY TRACE
      INTEGER       ud
      
      REAL          d2r,re,rm,deg2km
      INTEGER       EorM                  !1=EARTH, 2=MOON
      
      REAL          frac
      REAL          erad
      
      REAL          arp,ars,atp,ats       !P- & S-WAVE REFL & TRANS COEFS
      REAL          rt_sum,rt_min,rt_max  !MAX & MIN REFL PROBABILITIES
      
      INTEGER       ip,ip0                !1=P, 2=SH, 3=SV
      REAL          x_sign
      
      REAL          scat_depth,scat_prob
      
      REAL          dp
      
      REAL          c_mult(3)
      CHARACTER*3   cmp(3)
      
      INTEGER       status                !I/O ERROR (0=no read error)
      
      cmp(1) = 'LHZ'
      cmp(2) = 'LHT'
      cmp(3) = 'LHR'
      
      pi = atan(1.)*4.
      re = 6371.
      rm = 1737.
      d2r = pi/180.
      
      
      WRITE(6,*) 'ENTER SEISMIC VELOCITY MODEL FILE NAME'
      READ (*,'(A)') ifile
      OPEN(1,FILE=ifile,STATUS='OLD')    !OPEN SEISMIC VELOCITY MODEL
      
      WRITE(6,'(A)') 'ENTER:  (1) P  or  (2) S'
      READ (5,    *)  iwave

25    WRITE(6,'(A)') 'ENTER RAY PARAMETER RANGE (p1, p2):'
      READ (5,    *)  p1, p2

50    WRITE(6,'(A)') 'ENTER TIME WINDOW & # POINTS (t1, t2, nt):'
      READ (5, *) t1,t2,nt                    !TIME WINDOW & # OF TIME STEPS
      nto = nt
      
      dti = (t2-t1)/float(nt-1)                !TIME SAMPLING INTERVAL
      IF (nt.GT.nt0) THEN
       WRITE(6,'(A)') 'nt MUST BE < ',nt0
       GOTO 50
      END IF

60    WRITE(6,'(A)') 'ENTER DISTANCE RANGE (x1, x2, nx IN DEGREES):'
      READ (5,    *)  x1, x2, nx              !DISTANCE RANGE & # DISTANCE STEP
      dxi = (x2-x1)/float(nx-1)               !DISTANCE SAMPLING INTERVAL
      WRITE(6,'(A)') 'ENTER NUMBER OF RANDOM TRACES TO LAUNCH:'
      READ (5,    *)  ntr
      WRITE(6,'(A)') 'ENTER RANDOMIZING KERNEL:'
      READ (5,    *)  IDUM
      WRITE(6,'(A)') 'ENTER EARTHQUAKE DEPTH:'
      READ (5,    *)  qdep
      WRITE(6,'(A)') 'ENTER 1) EARTH, or 2) MOON:'
      READ (5,    *)  EorM
      WRITE(6,'(A)') 'ENTER 1=P, or 2=SH, 3=SV:'
      READ (5,    *)  ip0
      WRITE(6,'(A)') 'ENTER MAX SCATTERING DEPTH:'
      READ (5,    *)  scat_depth
      WRITE(6,'(A)') 'ENTER SCATTERING PROBABILITY:'
      READ (5,    *)  scat_prob
      
      IF (EorM .EQ. 1) THEN
       deg2km = re*d2r
       erad   = re
      ELSE
       deg2km = rm*d2r
       erad   = rm
      END IF

      DO I = 1, nlay0                     !READ IN VELOCITY MODEL
       READ(1,*,IOSTAT=status) z_s(i),r_s(i),vs(i,1),vs(i,2),rh(i)
       if (status /= 0) exit
       CALL FLATTEN_NEW(z_s(i),vs(i,1),z(i),vf(i,1),erad)!FLATTEN VELOCITIES
       CALL FLATTEN_NEW(z_s(i),vs(i,2),z(i),vf(i,2),erad)         
       nlay = I                           !NUMBER OF LAYERS
111   END DO
      CLOSE (1)
      WRITE(6,*) 'NLAY:',nlay
      
      WRITE(6,*) ' '      
      WRITE(6,'(A)')'************************* Table of Model Interfaces' &
      '**********************'
      WRITE(6,'(A)')' Depth  Top Velocities  Bot Velocities    -----Flat' & 
      'Earth Slownesses-----'
      WRITE(6,'(A)')'             vp1  vs1        vp2  vs2       p1     ' &  
      ' p2      s1      s2'
      
      DO I = 2, nlay 
       IF (z(i).EQ.z(i-1)) THEN                !ZERO LAYER THICK=DISCONTINUITY
        scr1=1./vf(i-1,1)                      !P-VELOCITY UPPER
        scr2=1./vf(i,1)                        !P-VELOCITY LOWER 
        scr3=999.                              !FLAG IF S VELOCITY = ZERO
        IF (vf(i-1,2).NE.0.) scr3=1./vf(i-1,2) !S-VELOCITY UPPER
        scr4=999.                              !FLAG IF S VELOCITY = ZERO
        IF (vf(i  ,2).NE.0.) scr4=1./vf(i  ,2) !S-VELOCITY LOWER
         WRITE(6,FMT=22) z_s(i),i-1,vs(i-1,1),vs(i-1,2), &
                        i,vs(i,1),vs(i,2),scr1,scr2,scr3,scr4
       END IF
      END DO
22    FORMAT (f6.1,2(i5,f6.2,f5.2),2x,2f9.6,2x,2f9.6)
      WRITE(6,*) ' '      


      f0 = 0.
      
      DO I = 1, nlay+5                      !BUILD Q(z) MODEL
       Q(I) = 10000.
!       IF ((z_s(I).LT.15.).AND.(z_s(i-1).NE.15.) ) THEN
!        Q(I) = 3000.
!       ELSE IF ((z_s(I).LT.80.).AND.(z_s(i-1).NE.80.) ) THEN
!        Q(I) = 3000.
!       ELSE IF ((z_s(I).LT.220.).AND.(z_s(i-1).NE.220.) ) THEN
!        Q(I) = 3000.
!       ELSE IF ((z_s(I).LT.670.).AND.(z_s(i-1).NE.670.) ) THEN
!        Q(I) = 3000.
!       ELSE IF ((z_s(I).LT.5149.5).AND.(z_s(i-1).NE.5149.5) ) THEN
!        Q(I) = 3000.
!       ELSE
!        Q(I) = 3000.
!       END IF
       IF (EorM .EQ. 2) Q(I) = 12000       !FOR MOON Q IS HIGH UNTIL ??CORE??
      END DO

      iz1 = 1
      DO WHILE (qdep.GT.z_s(iz1))               !FIND WHICH LAYER QUAKE STARTS IN
       iz1 = iz1 + 1
      END DO
      WRITE(6,*) 'DEPTH:',iz1,z_s(iz1)
      
      WRITE(6,*) 'ZEROING STACKS:'            !ZERO STACKS
      DO I = 1, nx
       DO J = 1, nt
        do k = 1, 3
	 wf(I,J,k) = 0.
        end do
       END DO
      END DO

      WRITE(6,'(A)') 'ENTER OUTPUT FILE NAME:'!REQUEST OUTPUT FILE NAME
      READ (5,'(A)')  ofile                   !
      OPEN(22,FILE=ofile,STATUS='UNKNOWN')    !OPEN OUTPUT FILE
      
      WRITE(6,*) 'CALCULATING SOURCE:'        !CALCULATING SOURCE
      pi = atan(1.)*4.                        !
      P0 = 1.0                                !DOMINANT PERIOD
      nts = nint(P0*4./dti)+1                 !# OF POINTS IN SOURCE SERIES
      IF (nts.LT.31) nts = 31
      nts1 = 1000
      DO I = 1, nts1
       mt(I) = 0.
      END DO
      DO I = 1, nts                           !SOURCE-TIME FUNCTION
       t0 = dti*float(I-1)-P0-4.
       mt(I) = -4.*pi**2.*P0**(-2.)*(t0-P0/2.) &
               *exp(-2.*pi**2.*(t0/P0-0.5)**2.)
      END DO

      datt = 0.05
      DO I = 1, 101                           !SOURCES * ATTENUATION
       dtst1 = float(I-1)*datt                !ATTENUATION
       CALL attenuate(mt,mtsc,nts1,dti,dtst1) !
       pow1 = 0.
       DO J = 1, nts1                         !
        mts(I,1,J) =  mtsc(J)                 !
        mts(I,3,J) = -mtsc(J)                 !
        pow1 = pow1 + mtsc(J)**2
       END DO                                 !
       nfil = 5
       CALL TILBERT(mtsc,dti,nts1,nfil,b,e)   !HILBER TRANSFORM (pi/2PHASESHFT)
       pow2 = 0.                              !ZERO POWER OF SERIES
       DO K = 1, nts1                         !COPY HILBERT SERIES TO CORRECT
        mts(I,2,K) = -b(K)                    !
        mts(I,4,K) =  b(K)
        pow2 = pow2 + b(k)**2                 !CUMULATIVE POWER
       END DO
       DO K = 1, nts1                         !NORMALIZE HILBERTS
        mts(I,2,K) = mts(I,2,K)*pow1/pow2     !
        mts(I,4,K) = mts(I,4,K)*pow1/pow2     !
       END DO
      END DO                                  !
      
      
      OPEN(23,FILE='source.out')              !OUTPUT SOURCE
      WRITE(23,*) nts,101                     !
      WRITE(23,FMT=888) 999.99,(datt*float(J-1),J=1,101)
      DO I = 1, nts
       WRITE(23,FMT=888) float(I-1)*dti,(mts(J,2,I)*1.,J=1,101)
      END DO
      CLOSE(23)
      

      n180 = nint(180/dxi)
      WRITE(6,*) 'BEGINNING RANDOM TRACE:',nts
      DO I = 1, ntr                           !FOR EACH TRACE
       iz = iz1
       r0 = ran0(IDUM)
       ip = int(3.*r0)+1 
!       ip = ip0
       IF (iz.EQ.1) THEN                      !IF QUAKE STARTS AT SURF GO DOWN
        angst = pi/2.                         !0 - 90 (0 = DOWN)
       ELSE                                   !IF QUAKE AT DEPTH THEN UP OR DOWN
        angst = pi                            !0 - 180 (0 = DOWN)
       END IF                                 !
       t = 0.                                 !SET START TIME = ZERO
       x = 0.                                 !START LOCATION = ZERO
       s = 0.                                 !SET START ATTENUATION = ZERO
       a = 0.0001                             !START AMPLITUDE = 1.
       dold = 0.
       d = 0.                                 !START AT ZERO KM TRAVELED
       x_sign = 1.                            !DISTANCE DIRECTION
       
       r0 = ran0(IDUM)                        !SELECT RANDOM RAY PARAMETER 
       ang1 = angst*r0
       p    = abs(sin(ang1))/vf(iz,2)
       DO WHILE ((p.LT.p1).OR.(p.GT.p2))
        r0 = ran0(IDUM)                       !SELECT RANDOM RAY PARAMETER 
        ang1 = angst*r0
        p    = abs(sin(ang1))/vf(iz,ip)
       END DO
       a    = cos(ang1*2.-pi/4.)              !SOURCE AMPLITUDE
       ncaust = 0                              !# OF CAUSTICS
       a = 1.                                 !
       IF (ang1.LT.pi/2.)THEN
        ud = 1
       ELSE
        ud = -1
       END IF
       NITR = 0
       DO WHILE ((t.LT.t2).AND.(NITR.LT.10*nlay))!TRACE UNTIL TIME PASSES TIME WIND
        NITR = NITR + 1
        r0 = ran0(IDUM)                       !RANDOM NUMBER FROM 0 TO 1
        IF (z_s(iz) < scat_depth) THEN
         r0 = ran0(IDUM)
         IF (r0 < scat_prob) THEN
          r0 = ran0(IDUM)
          IF (r0 < 0.5) x_sign=-x_sign
          r0 = ran0(IDUM)
          IF (r0 < scat_prob) ud = -ud
          r0 = ran0(IDUM)
          r0 = ( r0 - 0.5 )
          p    = p + r0*(p2-p1)*scat_prob
          DO WHILE ((p.LT.p1).OR.(p.GT.p2))
           r0 = ran0(IDUM)                       !SELECT RANDOM RAY PARAMETER 
           ang1 = angst*r0
           p    = abs(sin(ang1))/vf(iz,2)
          END DO
          END IF 
        END IF

	iwave = ip
	IF (iwave .EQ. 3) iwave = 2
	IF (iz.NE.1) THEN
         IF (abs(vf(iz-1,iwave)).GT.0.) THEN
          utop = 1./vf(iz-1,iwave)              !SLOWNESS AT TOP OF LAYER
         ELSE
          utop = 0.
         END IF 
         IF (abs(vf(iz,iwave)).GT.0.) THEN
          ubot = 1./vf(iz,iwave)                !SLOWNESS AT BOTTOM OF LAYER
         ELSE
          ubot = 0.
         END IF
         h    = z(iz)-z(iz-1)                  !THICKNESS OF LAYER
         imth = 2                              !INTERPOLATION METHOD
         CALL LAYERTRACE(p,h,utop,ubot,imth,dx1,dt1,irtr1)
         dtstr1 = dt1/Q(iz)                    !t* = TIME/QUALITY FACTOR
        ELSE
         irtr1  = -1
         dx1    = 0.
         dt1    = 0.
         dtstr1 = 0.
        END IF
        
        IF (irtr1.EQ.0) THEN
         ud = -ud
        ELSE IF (irtr1.GE.1) THEN
         d = d + ((z_s(iz)-z_s(iz-1))**2+dx1**2)**0.5!
         t = t + dt1                    !TRAVEL TIME
         x = x + dx1*x_sign             !EPICENTRAL DISTANCE TRAVELED-km
         s = s + dtstr1                 !CUMULATIVE t*
        END IF
        
	IF ( (iz.GT.1).AND.(abs(irtr1).EQ.1).AND. &
            (iz.LT.nlay) ) THEN
	 IF ( (iz.GT.1).AND.(iz.LE.nlay) ) h = z_s(iz)-z_s(iz-1)

          IF (ip .EQ. 2) THEN
          
           IF ( (ud.EQ.1) ) THEN               !IF DOWNGOING SH WAVE
            CALL REFTRAN_SH(p,vf(iz-1,2),vf(iz,2),rh(iz-1),rh(iz), &
                           ar,at)
           ELSE IF ((ud.EQ.-1) ) THEN          !IF UPGOING SH WAVE
            CALL REFTRAN_SH(p,vf(iz,2),vf(iz-1,2),rh(iz),rh(iz-1), &
                           ar,at)
           END IF
          ELSE

           IF ( (ud.EQ.1) ) THEN               !IF DOWNGOING P-SV WAVE
            CALL RTCOEF2(p,vf(iz-1,1),vf(iz-1,2),rh(iz-1), &
                          vf(iz  ,1),vf(iz  ,2),rh(iz), &
                          ip,arp,ars,atp,ats)
           ELSE IF ((ud.EQ.-1) ) THEN          !IF UPGOING P-SV WAVE
            CALL RTCOEF2(p,vf(iz  ,1),vf(iz  ,2),rh(iz  ), &
                          vf(iz-1,1),vf(iz-1,2),rh(iz-1), &
                          ip,arp,ars,atp,ats)           
!           WRITE(6,*) 'HI'
           END IF
          END IF
          
!          WRITE(6,*) ip,ud,arp,ars,atp,ats,vf(iz,1),vf(iz-1,1)
          r0 = ran0(IDUM)                       !RANDOM NUMBER FROM 0 TO 1

          IF (ip .EQ. 2) THEN                   !IF SH-WAVE

           IF (h.GT.0.) THEN                    !IF GRADIENT, THEN
            IF (r0.LT.(abs(ar)/(abs(ar)+abs(at)))/P0**2) THEN!CHECK FOR REFLECTN
             IF (ar.LT.0) a = -a                !OPPOSITE POLARITY
             ud = -ud                           !DOWNGOING/UPGOING
            END IF                              !
           ELSE                                 !IF INTERFACE THEN
            IF (r0.LT.(abs(ar)/(abs(ar)+abs(at)))) THEN!CHECK FOR REFLECTION
             IF (ar.LT.0) a = -a                !OPPOSITE POLARITY
             ud = -ud                           !DOWNGOING/UPGOING
            END IF                              !
           END IF                               !

          ELSE                                  !IF P- OR SV-WAVE 
           IF (h.LE.0.) THEN
            rt_sum = (arp)**2+(atp)**2+(ars)**2+(ats)**2!SUM OF REFL/TRAN COEFS

            rt_min = 0.                          !RANGE PROBABILITIES FOR P REFL
	    rt_max = (arp)**2/rt_sum             !
            IF ( (r0.GE.rt_min).AND.(r0.LT.rt_max) ) THEN!CHECK IF REFLECTED P
             IF (arp.LT.0) a = -a                 !REVERSE POLARITY
             ud = -ud                            !UPGOING <-> DOWNGOING
 	     ip = 1                              !P WAVE
            END IF                               !
           
!             IF (z_s(iz).EQ.137.) WRITE(6,*) arp,atp,r0,ud,ip,a

            rt_min = rt_max                      !RANGE PROBABILITIES 4 SV REFL
	    rt_max = rt_max+(ars)**2/rt_sum      !
            IF ( (r0.GE.rt_min).AND.(r0.LT.rt_max) ) THEN!CHECK IF REFLECTED SV
             IF (ars.LT.0) a = -a                 !REVERSE POLARITY
             ud = -ud                            !UPGOING <-> DOWNGOING
             ip = 3                              !SV WAVE
            END IF                               !

            rt_min = rt_max                      !RANGE PROBABILITIES 4 P TRANS
	    rt_max = rt_max+(atp)**2/rt_sum      !
            IF ( (r0.GE.rt_min).AND.(r0.LT.rt_max) ) THEN!CHECK IF TRAMSITTED P
             ip = 1                              !P WAVE
            END IF                               !

            rt_min = rt_max                      !RANGE PROBABILITIES 4 SV TRANS
            rt_max = rt_max+(ats)**2/rt_sum      !
            IF ( (r0.GE.rt_min).AND.(r0.LE.rt_max) ) THEN!CHECK IF TRANSMITTED SV
             ip = 3                              !SV WAVE
            END IF                               !
           END IF      
          END IF                                !END IF: SH, OR P-SV
         
        ELSE IF (iz.EQ.nlay) THEN               !ONCE HIT OTHER SIDE OF CORE
	 ud = -ud
	 x = x + 180*deg2km
	END IF
	
        
!   |FIX NEXT IF FOR DIFFRACTED WAVES: 
	IF (irtr1.EQ.2) THEN             !RAY TURNS IN LAYER FOLLOW 1 LEN
	 ud = -ud
         ncaust = ncaust + 1                   !# OF CAUSTICS
        END IF
        
	r0 = ran0(IDUM)
        
        IF (iz.EQ.1) THEN                      !IF RAY HITS SUFACE THEN RECORD
         ud = 1                                !RAY NOW MUST TRAVEL DOWN
         ix = nint((x/deg2km-x1)/dxi) + 1      !EVENT TO SURFACE HIT DISTANCE 
         IF (ix.GT.n180) ix = n180 - (ix-n180)
         IF (ix.LT.1) ix = -ix + 1
         IT = nint((t       -t1 -P0)/dti) + 1 

         ims = int(s/datt)+1
         IF (ims.GT.100) ims = 100
         IF (ims.LE.  1) ims =   2
         s1 = float(ims-1)*datt
         s2 = float(ims  )*datt
	 frac = (s-s1)/(s2-s1)
         IF (ncaust.LE.1) THEN
	  icaust = 1
	 ELSE
	  icaust = ncaust
          DO WHILE (icaust.GT.4)
           icaust = icaust - 4
          END DO
         END IF
!         WRITE(6,*) ix,JT,frac,s,ims,icaust,JJ,a
!         icaust = 1

         IF ( (IT.GT.1-nts).and.(IT.LE.nt0+nts) ) THEN
          if ( (ip == 1).or.(ip==3) ) then
           c_mult(1) = p*vf(iz,ip)
           c_mult(2) = 0.1*cos(asin(c_mult(1)))
           c_mult(3) = cos(asin(c_mult(1)))
          else if (ip == 2) then
           c_mult(1) = 0.1
	   c_mult(2) = 1.0
	   c_mult(3) = 0.1
          end if
           p    = abs(sin(ang1))/vf(iz,2)
          do ic = 1, 3
	  DO JJ = 1, nts
           JT = IT + JJ - 1
           IF ( (JT.GT.0).AND.(JT.LE.nt0).AND.(a.NE.0.) ) THEN
            wf(ix,JT,ic) = wf(ix,JT,ic) + a &
                     * (   (   frac)*mts(ims  ,icaust,JJ) &
                         + (1.-frac)*mts(ims+1,icaust,JJ) )!ATTENUATION
           END IF
          END DO
          end do
         END IF
        END IF
        iz = iz + ud                           !GO TO NEXT DEPTH
        IF (iz.LT.1) t = 999999.
       END DO
       IF (mod(float(I),float(ntr)/20.).EQ.0)THEN !STATUS REPORT
        WRITE(6,*) nint(float(I)/float(ntr)*100),'% COMPLETE'
       END IF
      END DO
      wf(1,1,1) = 1.
      wf(1,1,2) = 1.
      wf(1,1,3) = 1.
      WRITE(22,*) nt,nx
      WRITE(22,FMT=888) 999.99,(x1+dxi*float(J-1),J=1,nx)

      DO I = 1, nt
       do ic = 1, 3
        DO J = 1, nx
	 IF (abs(wf(J,I,ic)).GT.999.9999) wf(J,I,ic) = 999.9999*wf(J,I,ic)/abs(wf(J,I,ic))
        END DO
        WRITE(22,FMT=888) t1+float(I-1)*dti,(wf(J,I,ic)*0.1,J=1,nx)
       end do
      END DO
      CLOSE(22)
      
      DO I = 1, nx
       delta = x1+dxi*float(I-1)
       idelt1 = int(delta)
       idelt2 = int(10.*(delta-float(idelt1)))
       do ic = 1, 3
        WRITE(ofile,FMT=898) idelt1,idelt2,cmp(ic)
        WRITE(6,*) delta,idelt1,idelt2,ofile
        DO J = 1, nt
         w(J) = wf(I,J,ic)*0.1
        END DO
        CALL rsac_syn(ofile,w,nt,0.,dti,delta)
       end do
      END DO
      
      
888   FORMAT(F10.2,1X,361(F10.6,1X))
898   FORMAT('./SAC/D_',I3.3,'.',I1.1,'.',A3,'.sac')

      STOP
      END
      
      
      SUBROUTINE ATTENUATE(sin,sout,ndat,dt,tstar)
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
!   |THIS SUBROUTINE ATTENUATES AN INPUT SIGNAL (sin) BY A VALUE (tstar) |   C
!   |                                                                    |   C
!   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
!   |     CONTACT: jflawrence@stanfrd.edu                                    |   C
!   |                                                                    |   C
!   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
!   |DECLARE VARIABLES AND SET PARAMETERS:                               |   C
      REAL           sin(*),sout(*),tstar
      INTEGER        ndat,nfreq              !# OF POINTS IN TIME & FREQ DOMAIN
      INTEGER        MAXPTS                  !MAX # OF POINTS & ITERATIONS
      PARAMETER(     MAXPTS = 16384)         !
      REAL           xs(16384)               !SCRATCH SPACE
      COMPLEX        xf(16384),yf(16384)     !SCRATCH SPACE
      REAL           dt,df                   !TIME & FREQ SAMPLING INTERVAL
      REAL           pi                      !SET PI = 3.14....
      REAL           w,dw                    !FREQUCNEY VARIABLES
      REAL           damp
      
      CALL np2(ndat,npts)                    !FIND POWER OF TWO, npts >= ndat
      IF (npts.GT.MAXPTS) THEN               !CHECK THAT ARRAY IS NOT TOO BIG
       WRITE(6,*) 'WARNING: SERIES TRUNCATED TO:',MAXPTS
       npts = MAXPTS
      END IF                                 !
      CALL PADR(sin,ndat+1,npts)             !PAD SERIES WITH ZEROS
      CALL COPYR(sin,xs,npts)                 !COPY INITIAL DENOMINATOR
      
      CALL GET_SPEC(xs,npts,dt,xf,nfreq,df) !GET SPECTRUM OF x
      pi = atan(1.)*4.                       !SET PI = 3.14....
      dw = 2.*pi*df                          !ANGULAR FREQUENCY SAMPLING INTERVAL
      dadw = -tstar*dw                       !DERIVATIVE dA(w)di = -dt**dw
      
      DO I = 1, nfreq                        !APPLY ATTENUATION FILTER
       damp = exp(float(I-1)*dadw)
       w     = dw* float(I-1)                !ANGULAR FREQUENCY
       IF (damp.LT.0.) damp = 0.
       yf(I) = xf(I)*cmplx(damp,0.)
       yf(I) = yf(I)*exp( cmplx(0.,w*tstar))
      END DO

      CALL GET_TS(yf,nfreq,df,0,sout,npts,dt) !GET TIME SERIES OF ATTENUATED SPEC

      RETURN
      END                                    !END ATTENUATE
      

      SUBROUTINE np2(npts,np)
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
!   |THIS SUBROUTINE FINES THE POWER OF TWO THAT IS GREATER THAN OR EQUAL|   C
!   |     TO npts.                                                       |   C
!   |                                                                    |   C
!   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
!   |     CONTACT: jflawrence@stanford.edu                               |   C
!   |                                                                    |   C
!   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
      INTEGER     npts,np                      !INPUT & 2^n=np NUMBER OF POINTS
      np = 2                                   !ASSUME STARTING AT 2
      DO WHILE (npts.GT.np)                    !
       np = np * 2                             !KEEP INCREASING SIZE*2 UNTIL BIG
      END DO
      RETURN
      END                                      !END np2


      SUBROUTINE COPYR(f1,f2,npts)
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
!   |THIS SUBROUTINE COPIES ONE REAL VECTOR TO ANOTHER                   |   C
!   |                                                                    |   C
!   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
!   |     CONTACT: jflawrence@stanford.edu                               |   C
!   |                                                                    |   C
!   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
      INTEGER     I,npts                       !STEP & NUMBER OF POINTS
      REAL        f1(*),f2(*)                  !INPUT AND OUTPUT SERIES
      DO I = 1, npts                           !
       f2(I) = f1(I)                           !COPY POINTS
      END DO
      
      RETURN
      END                                      !END COPYR

      
      SUBROUTINE PADR(f1,n1,n2)
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
!   |THIS SUBROUTINE COPIES ONE REAL VECTOR TO ANOTHER                   |   C
!   |                                                                    |   C
!   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
!   |     CONTACT: jflawrence@stanford.edu                               |   C
!   |                                                                    |   C
!   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
      INTEGER     I,n1,n2                      !STEP, START & END POINTS
      REAL        f1(*)                        !SERIES TO ADD ZEROS TO
      DO I = n1, n2                            !
       f1(I) = 0.                              !MAKE ZERO
      END DO
      RETURN
      END                                      !END PADR
      
      
      
      
      SUBROUTINE rsac_syn(sfile,y,npts,tbeg,dt,delta)
      REAL           y(*),yd(1000000),tbeg,dt,delta
      INTEGER        npts
      REAL           qlon,qlat,qdep,slon,slat,sel,saz,qaz
      REAL           tend,dist
      INTEGER        qyr,qdy,qhr,qmn,qsc,qms
      CHARACTER*4    sname,ntwk
      CHARACTER*(*)  sfile
       CALL NEWHDR()                          !CREATE A NEW HEADER
       CALL SETNHV('npts'  ,npts   ,nerr)     !SET HEADER NUMBER OF POINTS
       CALL SETLHV('leven' ,.TRUE. ,nerr)     !SET HEADER EVEN SPACING
       CALL SETFHV('B'     ,tbeg   ,nerr)     !SET START FROM EARTHQUAKE
       CALL SETFHV('delta' ,dt     ,nerr)     !SET SAMPLING INTERVAL
       tend = tbeg+float(npts)*dt
       CALL SETFHV('E'     ,tend   ,nerr)     !SET END TIME FROM BEGINNING
       CALL SETIHV('iftype','ITIME',nerr)     !SET HEADER TO TIME SERIES
       CALL SETIHV('idep  ','IDISP',nerr)     !SET HEADER TO TIME SERIES
       CALL SETIHV('iztype','IO'   ,nerr)     !SET HEADER TIME TYPE: EQ
       qlat = 90.
       qlon = 0
       qdep = 0.
       slat = qlat-delta
       slon = 0.
       sel  = 0.
       qaz  = 180.
       saz  = 0.
       CALL SETFHV('stla'  ,slat   ,nerr)     !SET HEADER STATION LATITUDE
       CALL SETFHV('stlo'  ,slon   ,nerr)     !SET HEADER STATION LONGITUDE
       CALL SETFHV('stel'  ,sel    ,nerr)     !SET HEADER STATION ELEVATION
       CALL SETFHV('evla'  ,qlat   ,nerr)     !SET HEADER EQ LATITUDE
       CALL SETFHV('evlo'  ,qlon   ,nerr)     !SET HEADER EQ LONGITUDE
       CALL SETFHV('evdp'  ,qdep   ,nerr)     !SET HEADER EQ DEPTH
       
       qyr = 2007
       qdy = 1
       qhr = 1
       qmn = 1
       qsc = 1
       qms = 1
       
       
       CALL SETNHV('nzyear',qyr    ,nerr)     !SET HEADER START YEAR
       CALL SETNHV('nzjday',qdy    ,nerr)     !SET HEADER START JULIAN DAY
       CALL SETNHV('nzhour',qhr    ,nerr)     !SET HEADER START HOUR
       CALL SETNHV('nzmin' ,qmn    ,nerr)     !SET HEADER START MINUTE
       CALL SETNHV('nzsec' ,qsc    ,nerr)     !SET HEADER START SECOND
       CALL SETNHV('nzmsec',qms    ,nerr)     !SET HEADER START MILISECOND
       CALL SETFHV('gcarc' ,delta  ,nerr)     !SET HEADER EQ-STATN DIST (DEGREES)
       dist = delta/111.19
       CALL SETFHV('dist ' ,dist   ,nerr)     !SET HEADER EQ-STATN DIST (KM)
       CALL SETFHV('baz'   ,saz    ,nerr)     !SET HEADER BACKAZIMUTH STATN-EQ
       CALL SETFHV('az'    ,qaz    ,nerr)     !SET HEADER AZIMUTH     EQ-STATN
       sname = 'synt'
       ntwk  = 'NA'
       CALL SETKHV('kstnm' ,sname  ,nerr)     !SET HEADER STATION NAME
       CALL SETKHV('knetwk',ntwk   ,nerr)     !SET HEADER STATION NETWORK
       CALL SETKHV('kcmpnm','SYT'  ,nerr)     !SET HEADER COMPONENT NAME
       DO K = 1, npts                        !MAKE SERIES OF TIME STEPS
        yd(K) = tbeg+float(K-1)*dt
       END DO
       CALL WSAC0(sfile,yd,y,nerr)            !OUTPUT DATA FILE
      
      RETURN
      END


      SUBROUTINE REFTRAN_SH(p,b1,b2,rh1,rh2,ar,at)
      REAL       p,ar,at
      REAL       pi,j1,j2,b1,rh1,rh2
      pi   = atan(1.)*4.
      r2d = 180./pi
      IF (p*b1.LE.1.) THEN
       j1   = asin(p*b1)
      ELSE
       j1 = pi/2.
      END IF
      IF (p*b2.LE.1.) THEN
       j2   = asin(p*b2)
      ELSE
       j2   = pi/2. 
      END IF
      
      DD   = rh1*b1*cos(j1)+rh2*b2*cos(j2)

      ar   = (rh1*b1*cos(j1)-rh2*b2*cos(j2))/DD
      at   = 2.*rh1*b1*cos(j1)/DD
      
      RETURN
      END



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
!  Returns:   arp     =  down P to P up     (refl)
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
      SUBROUTINE RTCOEF2(pin,vp1,vs1,den1,vp2,vs2,den2,pors, &
                         rrp,rrs,rtp,rts)
      IMPLICIT     NONE
      REAL         vp1,vs1,den1,vp2,vs2,den2     !VELOCITY & DENSITY
      INTEGER      pors                          !P (1) OR S (2)                          
      COMPLEX      a,b,c,d,e,f,g,H               !TEMPORARY VARIABLES
      COMPLEX      cone,ctwo                     !COMPLEX  = 1 OR = 2
      COMPLEX      va1,vb1,rho1,va2,vb2,rho2     !VELOCITY & DENSITY (COMPLEX)
      REAL         pin                           !INPUT SLOWNESS
      COMPLEX      p                             !INPUT SLOWNESS (P OR S)
      COMPLEX      si1,si2,sj1,sj2               !SIN OF ANGLE
      COMPLEX      ci1,ci2,cj1,cj2               !COMPLEX SCRATCH
      COMPLEX      term1,term2                   !COMPLEX SCRATCH
      COMPLEX      DEN                           !DENOMINATOR
      COMPLEX      trm1,trm2                     !COMPLEX SCRATCH
      COMPLEX      arp,ars,atp,ats               !REFLECTION & TRANSMISSION COEFS
      REAL         rrp,rrs,rtp,rts               !REFLECTION & TRANSMISSION COEFS
      
      va1    = cmplx(vp1,  0.)                   !MAKE VEL & DENSITY COMPLEX
      vb1    = cmplx(vs1,  0.)
      rho1   = cmplx(den1, 0.)
      va2    = cmplx(vp2,  0.)
      vb2    = cmplx(vs2,  0.)
      rho2   = cmplx(den2, 0.)

      p      = cmplx(pin,  0.)                   !MAKE RAY PARAMETER COMPEX      
      
      cone   = cmplx(1.,0.)                      !COMPLEX 1 & 2
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
      IF (PorS .EQ. 1) THEN
       trm1   = b*ci1/va1-c*ci2/va2          
       trm2   = a+d*ci1*cj2/(va1*vb2)
       arp    = (trm1*F-trm2*H*p*p)/DEN           !refl down P to P up
       trm1   = a*b+c*d*ci2*cj2/(va2*vb2)       
       ars    = (-ctwo*ci1*trm1*p)/(vb1*DEN)      !refl down P to S up
       atp    = ctwo*rho1*ci1*F/(va2*DEN)         !trans down P to P down
       ats    = ctwo*rho1*ci1*H*p/(vb2*DEN)       !trans down P to S down
      ELSE
       trm1   = a*b+c*d*ci2*cj2/(va2*vb2)       
       arp    = (-ctwo*cj1*trm1*p)/(va1*DEN)      !refl down S to P up
       trm1   = b*cj1/vb1-c*cj2/vb2               
       trm2   = a+d*ci2*cj1/(va2*vb1)
       ars    = -(trm1*E-trm2*G*p*p)/DEN          !refl down S to S up
       atp    = -ctwo*rho1*cj1*G*p/(va2*DEN)      !trans down S to P down 
       ats    = ctwo*rho1*cj1*E/(vb2*DEN)         !trans down S to S down
      END IF
      
      rrp = real(arp)!**2+imag(arp)**2)**0.5
      rrs = real(ars)!**2+imag(ars)**2)**0.5
      rtp = real(atp)!**2+imag(atp)**2)**0.5
      rts = real(ats)!**2+imag(ats)**2)**0.5
      
!      WRITE(6,*) 'HI1:',a,b,c,d,E,F,G,H,DEN
      
      RETURN
      END




      subroutine FLATTEN(z_s,vs,z_f,vf_f)
!   !FLATTEN calculates flat earth tranformation.
      erad=6371.
      r=erad-z_s
      z_f=-erad*alog(r/erad)
      vf_f=vs*(erad/r)
      return
      end

      subroutine FLATTEN_NEW(z_s,vs,z_f,vf_f,erad)
      REAL     z_s,z_f,vf_f,vs,erad,r
      r=erad-z_s
      z_f=-erad*alog(r/erad)
      vf_f=vs*(erad/r)
      return
      end

      subroutine LAYERTRACE(p,h,utop,ubot,imth,dx,dt,irtr)
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
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
!   ! Returns:  dx    =  range offset
!   !           dt    =  travel time
!   !           irtr  =  return code
!   !                 = -1,  zero thickness layer
!   !                 =  0,  ray turned above layer
!   !                 =  1,  ray passed through layer
!   !                 =  2,  ray turned within layer, 1 segment counted in dx,dt
!   !
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
      if (h.eq.0.) then      !check for zero thickness layer
         dx=0.
         dt=0.
         irtr=-1
         return         
      end if
!   !
      u=utop
      y=u-p
      if (y.le.0.) then   !complex vertical slowness
         dx=0.            !ray turned above layer
         dt=0.
         irtr=0
         return
      end if
!
      q=y*(u+p)
      qs=sqrt(q)
!   ! special function needed for integral at top of layer
      if (imth.eq.2) then
         y=u+qs
         if (p.ne.0.) y=y/p
         qr=alog(y)
      else if (imth.eq.3) then
         qr=atan2(qs,p)
      end if      
!
      if (imth.eq.1) then
          b=-(utop**2-ubot**2)/(2.*h)
      else if (imth.eq.2) then
          vtop=1./utop
          vbot=1./ubot
          b=-(vtop-vbot)/h
      else
          b=-alog(ubot/utop)/h
      end if  
!
      if (b.eq.0.) then     !constant vfocity layer
         b=-1./h
         etau=qs
         ex=p/qs
         go to 160
      end if
!   !integral at upper limit, 1/b factor omitted until end
      if (imth.eq.1) then
         etau=-q*qs/3.
         ex=-qs*p
      else if (imth.eq.2) then
         ex=qs/u
         etau=qr-ex
         if (p.ne.0.) ex=ex/p
      else
         etau=qs-p*qr
         ex=qr
      end if
!   ! check lower limit to see if we have turning point
      u=ubot
      if (u.le.p) then   !if turning point,
         irtr=2          !then no contribution
         go to 160       !from bottom point
      end if 
      irtr=1
      q=(u-p)*(u+p)
      qs=sqrt(q)
!
      if (imth.eq.1) then
         etau=etau+q*qs/3.
         ex=ex+qs*p
      else if (imth.eq.2) then
         y=u+qs
         z=qs/u
         etau=etau+z
         if (p.ne.0.) then
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
      end




      FUNCTION ran0(idum)
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
      INTEGER idum,IA,IM,IQ,IR,MASK
      REAL ran0,AM
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,
     *MASK=123459876)
      INTEGER k
      idum=ieor(idum,MASK)
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      ran0=AM*idum
      idum=ieor(idum,MASK)
      return
      END



      SUBROUTINE NPOW2(npts,np)
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
!   |THIS SERIES DETERMINES THE POWER OF TWO THAT IS EQUAL OR JUST       |   C
!   |     GREATER THAN THE NUMBER OF POINTS SUPPLIED:                    |   C
      INTEGER npts,np
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
      np = 0
      DO WHILE (2**np.LT.npts)
       np = np + 1
      END DO

      RETURN
      END 



      SUBROUTINE TILBERT(a,dt,npts,nfil,b,e)
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
! of time series in the time domain.
!     Inputs:   a    =  time series
!               dt   =  time spacing
!               npts =  number of points in a
!               nfil =  half-number of points in filter
!     Returns:  b    =  Hilbert transform of time series
!               e    =  envelope time function
!
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
      REAL       a(*),b(*),e(*),h(2001)
      REAL       dt
      INTEGER    npts,nfil,nfiltot,I,J,II,JJ,i1,i2
      SAVE       dt0,npts0,h,nfiltot             !STORE VARIABLES TO SAVE TIME
      pi = atan(1.)*4.                           !SET PI = 3.14.....

      IF ( (dt.NE.dt0).and.(npts.NE.npts0)) THEN !SET UP THE FILTER
       nfiltot = 2*nfil+1                        !# OF POINTS IN FILTER
       DO 10 i = 1, nfiltot                      !FOR EACH FILTER POINT
        t=float(i-nfil-1)*dt                     !TIME OF ITH POINT
        IF (i.NE.nfil+1) THEN                    !CALCULATE FILTER
         h(i) = -1./(pi*t)
        ELSE                                     !AVOID SINGULARITY
         h(i) = 0.
        END IF
10     END DO
       CALL TAPERR(h,nfiltot,0.5,0.5)
       dt0     = dt                              !STORE SAMPLING INTERVAL
       npts0   = npts                            !STORE NUMBER OF POINTS
      END IF

      CALL ZEROR(e,npts)                         !ZERO ENVELOPE
      CALL ZEROR(b,npts)                         !ZERO HILBERT TRANSFORM

      i1 = 1 + nfil
      i2 = npts - nfil
      DO 50 i=i1,i2
       DO 40 j = 1, nfiltot
        ii  = i - (j-nfil-1)
        b(i)= b(i)+a(ii)*h(j)
40     END DO
       b(i)=b(i)*dt
50    END DO
      DO 70 i=i1,i2
       e(i) = ( (a(i)**2+b(i)**2) )**0.5
70    END DO
      RETURN
      END







      SUBROUTINE TAPERR(S1,ndat,tap1,tap2)
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
!   | THIS SUBROUTINE TAPERS ANY SIGNAL (S1), OF LENGTH npts, FROM 1 TO  |   C 
!   |      tap1*npts, AND FROM tap2*npts TO npts.                        |   C
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
      REAL       S1(*),tap1,tap2,PI,cs
      INTEGER    tap1n,tap2n
      INTEGER    I,ndat
      PI = atan(1.0)*4.
      tap1n = nint(tap1*float(ndat))                 !INTEGER TAPER POINT 1
      tap2n = nint(tap2*float(ndat))                 !INTEGER TAPER POINT 2

      DO I = 1, tap1n                                !TAPER FROM 1 TO POINT 1
         cs     = sin(float(I-1)*PI/float(tap1n-1)/2.)
         S1(I)  = S1(I) * cs**2
      END DO
      DO I = 1, tap2n                                !TAPER FROM POINT 2 TO END
         cs     = sin(float(I-1)*PI/float(tap2n-1)/2.)
         S1(ndat-I+1)  = S1(ndat-I+1) * cs**2
      END DO

      RETURN
      END


      SUBROUTINE ZEROR(series,npts)
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
!   |THIS SUBROUTINE ZEROES ANY 1D SERIES OF LENGTH TO POINT, npts:      |   C
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
      REAL series(*)
      INTEGER npts

      DO I = 1, npts
       series(I) = 0.
      END DO

      RETURN
      END
