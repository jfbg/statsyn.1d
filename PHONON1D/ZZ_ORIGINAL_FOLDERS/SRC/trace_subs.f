      SUBROUTINE FINDCHAR(string1,string2,yn)
C   |THIS SUBROUTINE DETERMINES IF THE SECOND STRING IS CONTAINED IN THE |   C
C   |     FIRST STRING AND RETURNS yn = 1 IF FOUND:                      |   C
      CHARACTER*(*)       string1
      CHARACTER*(*)       string2
      INTEGER             yn,iblank1,iblank2
      
      
      
      CALL ialen(string1,iblank1)
      CALL ialen(string2,iblank2)
            
      IF (iblank2.LE.iblank1) THEN
      
       DO I = 1, iblank1-iblank2+1
        IF (string1(I:I+iblank2-1).EQ.string2(1:iblank2)) THEN
         yn = 1
	 WRITE(6,*) 'DIFFRACTED WAVE:',string1(I:I+iblank2)
         RETURN
        END IF
       END DO
       
      END IF

      RETURN
      END





      SUBROUTINE trph(p,va,vb,z,r,d,t,vt,evdp,dec,dr,phase,ie,ddel,imod)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      REAL         va(*),vb(*)                  !VELOCITY MODELS
      REAL         z(*),r(*),d(*),t(*),vt(*)        !RAY TRACE INFO
      CHARACTER*80 phase
      INTEGER      ie,imod                          !PHASE AND # OF DATA IN TRAC
      REAL         p,dec,dr                         !RAY PARAMETER, MODEL DEC
      REAL         evdp                             !EVENT DEPTH (KM)
      
      re   =      1737.                             !SET LAYER DEPTHS
      rcmb = re - 1380.
      ricb = re - 1637.
      r660 = re -  660.
      r410 = re -  410.
      ie   = 1                                      !START TRACE ON POINT #1
      rbeg = re - evdp
      IF (phase(1:9).EQ.'P        ') THEN           !TRACE P WAVE:
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       CALL tracenw(p,va,z,r,d,t,vt,rend,re  , dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'P660s    ') THEN      !TRACE P660s WAVE:
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = r660
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       rend = r410
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
       rbeg = re
       CALL tracenw(p,vb,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'P410s    ') THEN      !TRACE P410s WAVE:
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = r410
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       rend = re
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'Ppdp     ') THEN      !TRACE P660s WAVE:
       rend = rcmb
       rbeg = re
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = re
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       rend = re - evdp
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = re
       CALL tracenw(p,vb,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'PP       ') THEN      !TRACE PP WAVE:
       DO I = 1, 2
        rend = rcmb
        CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
        rbeg = re
        CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       END DO
      ELSE IF (phase(1:9).EQ.'P3       ') THEN      !TRACE PP WAVE:
       DO I = 1, 3
        rend = rcmb
        CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
        rbeg = re
        CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       END DO
      ELSE IF (phase(1:9).EQ.'P4       ') THEN      !TRACE PP WAVE:
       DO I = 1, 4
        rend = rcmb
        CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
        rbeg = re
        CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       END DO
      ELSE IF (phase(1:9).EQ.'P5       ') THEN      !TRACE PP WAVE:
       DO I = 1, 5
        rend = rcmb
        CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
        rbeg = re
        CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       END DO
      ELSE IF (phase(1:9).EQ.'PcP      ') THEN      !TRACE PcP WAVE:
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       CALL tracenw(p,va,z,r,d,t,vt,rend,re  , dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'PcP2     ') THEN      !TRACE PcP WAVE:
       DO I = 1, 2
        rend = rcmb
        CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
        rbeg = re
        CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       END DO
      ELSE IF (phase(1:9).EQ.'S        ') THEN      !TRACE S WAVE:
       rend = rcmb
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       CALL tracenw(p,vb,z,r,d,t,vt,rend,re  , dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'S410p    ') THEN      !TRACE S WAVE:
       rend = rcmb
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = r410
       CALL tracenw(p,vb,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       rend = re
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'S660p    ') THEN      !TRACE S WAVE:
       rend = rcmb
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = r660
       CALL tracenw(p,vb,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       rend = re
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'SS       ') THEN      !TRACE SS WAVE:
       DO I = 1, 2
        rend = rcmb
        CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
        rbeg = re
        CALL tracenw(p,vb,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       END DO
      ELSE IF (phase(1:9).EQ.'S3       ') THEN      !TRACE PP WAVE:
       DO I = 1, 3
        rend = rcmb
        CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
        rbeg = re
        CALL tracenw(p,vb,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       END DO
      ELSE IF (phase(1:9).EQ.'S4       ') THEN      !TRACE PP WAVE:
       DO I = 1, 4
        rend = rcmb
        CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
        rbeg = re
        CALL tracenw(p,vb,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       END DO
      ELSE IF (phase(1:9).EQ.'S5       ') THEN      !TRACE PP WAVE:
       DO I = 1, 5
        rend = rcmb
        CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
        rbeg = re
        CALL tracenw(p,vb,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       END DO
       ELSE IF (phase(1:9).EQ.'S410S    ') THEN      !TRACE S410S WAVE:
       rend = rcmb
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = r410
       CALL tracenw(p,vb,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       rend = rcmb
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = re
       CALL tracenw(p,vb,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'S660S    ') THEN      !TRACE S660S WAVE:
       rend = rcmb
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = r660
       CALL tracenw(p,vb,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       rend = rcmb
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = re
       CALL tracenw(p,vb,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'P410P    ') THEN      !TRACE S410S WAVE:
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = r410
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = re
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'P660P    ') THEN      !TRACE S660S WAVE:
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = r660
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = re
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'P410sP   ') THEN      !TRACE S660S WAVE:
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = r410
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       rend = re
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
       rbeg = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg,-dec,dec,ie)
       rbeg = re
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'P660sP   ') THEN      !TRACE S660S WAVE:
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = r660
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       rend = re
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
       rbeg = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg,-dec,dec,ie)
       rbeg = re
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'ScS      ') THEN      !TRACE ScS WAVE:
       rend = rcmb
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       CALL tracenw(p,vb,z,r,d,t,vt,rend,re  , dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'SKS      ') THEN      !TRACE SKS WAVE:
       rend = rcmb-1.            !FUDGE
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = ricb
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg,-dec,dec,ie)
       rend = rcmb-2.            !FUDGE
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
       CALL tracenw(p,vb,z,r,d,t,vt,rend,re  , dec,dec,ie)
      ELSE IF ( (phase(1:9).EQ.'PKP      ').OR.
     &          (phase(1:9).EQ.'PKPBC    ') )THEN      !TRACE PKP WAVE:
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = ricb
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg,-dec,dec,ie)
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
       CALL tracenw(p,va,z,r,d,t,vt,rend,re  , dec,dec,ie)

      ELSE IF (phase(1:9).EQ.'PKKP     ') THEN      !TRACE PKP WAVE:
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       
       rbeg = ricb
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg,-dec,dec,ie)
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
       
       rbeg = ricb
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg,-dec,dec,ie)
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
       
       CALL tracenw(p,va,z,r,d,t,vt,rend,re  , dec,dec,ie)

      ELSE IF ((phase(1:9).EQ.'PKPDF    ').OR.
     &         (phase(1:9).EQ.'PKIKP    ') ) THEN      !TRACE PKIKP WAVE:
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = ricb
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg,-dec,dec,ie)
       rend = 0.
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
       CALL tracenw(p,va,z,r,d,t,vt,rend,re  , dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'PKiKP    ') THEN      !TRACE PKIKP WAVE:
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = ricb
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg,-dec,dec,ie)
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,re  , dec,dec,ie)

      ELSE IF (phase(1:9).EQ.'PKJKP    ') THEN      !TRACE PKJKP WAVE:
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = ricb
       CALL tracenw(p,va,z,r,d,t,vt,rend,rbeg,-dec,dec,ie)
       rend = 0.
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       CALL tracenw(p,vb,z,r,d,t,vt,rend,rbeg, dec,dec,ie)
       rend = rcmb
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend, dec,dec,ie)
       CALL tracenw(p,va,z,r,d,t,vt,rend,re  , dec,dec,ie)
      ELSE IF (phase(1:9).EQ.'Pdiff    ') THEN      !TRACE Pdiff WAVE:
       rend = rcmb
       zi = nint((re-rend)/dec+1)
       p = (rcmb-dec)/va(zi)
       CALL tracenw(p,va,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = rcmb - dec
C       WRITE(6,*) ddel
       CALL trdiff(va,z,r,d,t,vt,rbeg,dec,ddel,ie)
       CALL tracenw(p,va,z,r,d,t,vt,rend,re  , dec,dec,ie)

      ELSE IF (phase(1:9).EQ.'Sdiff    ') THEN      !TRACE Pdiff WAVE:
       rend = rcmb
       zi = nint((re-rend)/dec+1)
       p = (rcmb-dec)/vb(zi)
       CALL tracenw(p,vb,z,r,d,t,vt,rbeg,rend,-dec,dec,ie)
       rbeg = rcmb - dec
C       WRITE(6,*) ddel
       CALL trdiff(vb,z,r,d,t,vt,rbeg,dec,ddel,ie)
       CALL tracenw(p,vb,z,r,d,t,vt,rend,re  , dec,dec,ie)

      ELSE
       WRITE(6,*) 'PHASE NOT FOUND FOR:',phase(1:9)
       ie = -1
      END IF

      RETURN
      END



      SUBROUTINE GET_MODS(imodel,period,dec,z,vela,velb,NIMX)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE READS IN THE CORRECT RADIAL MODEL FOR RAY TRACING:  |   C
C   |                                                                    |   C
C   |INPUTS:   imodel: INTEGER NUMBER CORRESPONDING TO THE MODEL         |   C
C   |               1 = PREM                                             |   C
C   |               2 = PREM WITH OCEAN CRUST                            |   C
C   |               3 = IASP91                                           |   C
C   |               4 = SP6                                              |   C
C   |          period: PERIOD OF THE WAVE TO TRACE (ONLY FOR PREM)       |   C
C   |                                                                    |   C
C   |OUTPUTS:  z     : FLATTENED DEPTHS                                  |   C
C   |          alpha : FLATTENED P-VELOCITY (KM/S)                       |   C
C   |          beta  : FLATTENED S-VELOCITY (KM/S)                       |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |SET PARAMETERS AND DECLARE VARIABLES:                               |   C
      PARAMETER   (NLMX = 1000, N2MX = 1737)
      REAL         zs(NLMX),alphas(NLMX),betas(NLMX) !SPHERICAL MODEL
      REAL         z(N2MX),vela(*),velb(*)           !SPHERICAL MODEL
      CHARACTER*64 vmodel                            !MODEL NAME
      INTEGER      nlyr
      REAL         el,qal,pi,qu,qk
      pi = atan(1.)*4.

C   |DETERMINE WHICH MODEL TO USE:                                       |   C
      IF (imodel.LE.2) THEN                        !PREM MODEL
       IF (imodel.EQ.1) vmodel = './MODELS/luna_nakamura_int'
       IF (imodel.EQ.2) vmodel = './MODELS/luna_nakamura_int'
       depcmb = 1380.
       depicb = 1637.
      ELSE IF (imodel.EQ.3) THEN                   !IASP91 MODEL
       vmodel = './MODELS/luna_nakamura_int'
       depcmb = 1380.
       depicb = 1637.
       period = 1
      ELSE IF (imodel.EQ.5) THEN                   !USER DEFINED MODEL
       WRITE(6,*) 'ENTER MODEL FILE NAME:'
       READ (5,*)  vmodel
       depcmb = 1380.
       depicb = 1637.
       WRITE(6,*) 'ENTER PERIOD OF MODEL:'
       READ (5,*)  period
       depcmb = 1380.
       depicb = 1637.
      ELSE                                         !SP6 MODEL
       vmodel = './MODELS/luna_nakamura_int'
       depcmb = 1380.
       depicb = 1637.
       period = 1
      END IF

      erad = 1737.
      OPEN(7,FILE=vmodel,STATUS='OLD')             !INPUT MODEL
      DO 10 I = 1, NLMX
       READ(7,FMT=20,END=30) zs(i),alphas(i),betas(i)
       nlyr = I
       IF (zs(i).EQ.erad) GOTO 30
       IF ( (imodel.EQ.1).AND.(period.NE.1.) )THEN!ADJUST PERIOD FOR PREM
        qu  = 1200.
        qk  = 57822.
        el        = (4./3.)*(betas(i)/alphas(i))**2
        qal       = 1./(el/qu+(1.-el)/qk)
        betas(i) =  betas(i)*(1.-log(period)/(pi*qu))
        alphas(i)= alphas(i)*(1.-log(period)/(pi*qal))
       END IF
10    END DO
20    FORMAT (F12.5,12X,2F12.5)
30    CLOSE(7)
      CALL DEC_MOD(zs,alphas,dec,z,vela,nlyr,NIMX)
      CALL DEC_MOD(zs, betas,dec,z,velb,nlyr,NIMX)

      RETURN
      END








      SUBROUTINE DEC_MOD(zs,veli,dec,z,velo,nl,NIMX)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      REAL       zs(*),veli(*)
      REAL       z(*),velo(*)
      INTEGER    nl,nl2,L,I
      
      erad   = 1737.                            !EARTH'S RADIUS
      ierad  = nint(1737./dec)                  !MODEL MAX NUMBERS.
      nlt = 0
      z(1) = 0.
      velo(1) = veli(1)
      L = 1
      DO I = 2, nl
       nl2 = int((zs(I)-zs(I-1))/dec)+1         !NUMBER OF STEPS PER LAYER
       IF (nl2.NE.0) THEN                       !SKIP DISCONTINUITIES
        DO J = 1, nl2-1
         L    = L + 1                           !LAYER NUMBER
         z(L) = float(L)*dec                    !DEPTH
         CALL INTERP(zs(I-1),zs(I),z(L),veli(I-1),veli(I),velo(L))
        END DO
       END IF
      END DO
      NIMX = L
      RETURN
      END


      SUBROUTINE trdiff(vel,z,r,d,t,vt,rend,dec,del,ith)
C      (p,vel,z,r,d,t,vt,rbeg,rend,dr,dec,ith)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      REAL           z(*),vel(*)                  !VELOCITY MODEL
      REAL           r(*),d(*),t(*),vt(*)         !PATH INFO
      REAL           rend,dr,dec,dt
      INTEGER        ith,zi
      REAL           pi,r2d
      pi  = atan(1.)*4.
      r2d = 180./pi
      p = rend/vel(nint( (1737-rend)*dec )+1)
      ndel = nint(del*100.)
      ddel = del/float(ndel)/r2d
      IF (del.GT.0) THEN

       DO I = 1, ndel
        ith    = ith + 1
        r(ith) = rend
        zi     = nint( (1737-r(ith))*dec )+1
        vt(ith)= vel(zi)
        s      = ddel*rend
        t(ith) = t(ith-1)+s/vt(ith)
        d(ith) = d(ith-1)+ddel
       END DO
      END IF      
      RETURN
      END





      SUBROUTINE tracenw(p,vel,z,r,d,t,vt,rbeg,rend,dr,dec,ith)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      REAL           z(*),vel(*)                  !VELOCITY MODEL
      REAL           r(*),d(*),t(*),vt(*)         !PATH INFO
      REAL           rbeg,rend,dr,dec,dt
      INTEGER        ith,zi,dri
      REAL           pi,r2d
      INTEGER        idec,yi
      idec = dec
      pi  = atan(1.)*4.
      r2d = 180./pi
      dri =nint( dr)
      IF (ith.EQ.1) THEN
       r(ith) = rbeg
       zi     = nint( (1737-r(ith))*dec )+1 
       vt(1)  = vel(zi)
       t(1)   = 0.
       d(1)   = 0.
      END IF
      var = 9999.
      WRITE(6,*) 'DIDI:',z(ith),r(ith),d(ith),t(ith),-dr*(r(ith)-rend)
      DO WHILE ( (-dr*(r(ith)-rend).GT.0.).AND.(var.GT.p) )
       ith    = ith + 1
       r(ith) = r(ith-1) + dr
       zi     = nint( (1737-r(ith))*dec )+1
       vt(ith)= vel(zi)
       IF (zi.EQ.1) zi = zi + 1
       etta   = r(ith-1)/vel(zi-1)
       t(ith) = etta**2*abs(dr)/r(ith-1)/(etta**2-p**2)**0.5
       d(ith) = d(ith-1)+p      *abs(dr)/r(ith-1)/(etta**2-p**2)**0.5 !DISTANCE
       yi = nint( 1737.-(r(ith)+dr))!*idec
       var    = (r(ith)+dr)/vel(yi) !CANT REFRACT
       WRITE(6,*)'DADA:',z(ith),r(ith),zi,etta,var,p,-dr*(r(ith)-rend)
      END DO
      WRITE(6,*) 'DODO:',z(ith),r(ith),d(ith),t(ith),ith
      
      rend = r(ith)
      
      IF (dr.LT.0.) THEN
       CALL trbot(p,vel,z,r,d,t,vt,rbeg,rend,dr,dec,ith)
      END IF
      
      rend = r(ith)
      
      RETURN
      END


      SUBROUTINE trbot(p,vel,z,r,d,t,vt,rbeg,rend,dr,dec,ith)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      REAL           z(*),vel(*)                  !VELOCITY MODEL
      REAL           r(*),d(*),t(*),vt(*)         !PATH INFO
      REAL           rbeg,rend,dr,dec,dt
      INTEGER        ith,zi,z1,z2
      REAL           p1,p2
      REAL           pi,r2d
      REAL            ddd
      pi  = atan(1.)*4.
      r2d = 180./pi

      z1 = nint((1737.-r(ith))*abs(dec))+1
      p1 = r(ith)/vel(z1)
      
      z2 = nint((1737.-r(ith))*abs(dec)+dr)+1
      IF (vel(z2).LT.0.01) RETURN
      p2 = (r(ith)+dec)/vel(z1)
      
      IF ((p.GT.p1).AND.(p.LT.p2)) THEN
       ith     = ith + 1
       ddr     = dr/(p2-p1)*(p-p1)
       r(ith)  = r(ith-1)
       ddd     = acos( (r(ith)+ddr)/r(ith-1) )
       dist111 = r(ith)*tan(ddd)*2
       t(ith)  = t(ith-1) + dist111/vel(z2)
       d(ith)  = d(ith-1) + ddd*2
      
C       WRITE(6,*) 'HI1:',p,p1,p2,r(ith)+ddr,r(ith-1)
C       WRITE(6,*) 'HI2:',d(ith),d(ith-1),t(ith)
C       WRITE(6,*) 'HI3:',ddd,dist111,dr/(p2-p1)*(p-p1)
      END IF
      RETURN
      END


      SUBROUTINE GET_TTJ(phase,ip,del,qdep,tt,dtdh,dtddel,iflag)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE OBTAINS THE TRAVEL TIME, HORIZONTAL, AND VERTICAL   |   C
C   |     FOR A SPECIFIED PHASE FOR ANY GIVEN DISTANCE AND DEPTH.  THE   |   C
C   |     TRAVEL TIMES AND SLOWNESS VALUES ARE INTERPOLATED FROM PRE-    |   C
C   |     CONSTRUCTED TRAVEL TIME TABLES.                                |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE WAS CHANGED FROM TTM BY CHANGING UNITS TO SECONDS   |   C
C   |     RATHER THAN MINUTES.                                           |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE WAS RIPPED OFF OF PETER SHEARER'S GET_TTS SUBROUTINE|   C
C   |     WHICH WAS ALTERED FROM GET_TT. THE PROGRAM DIFFERS FROM GET_TT |   C
C   |     BY SAVING DATA WITHOUT HAVING TO RE-READ TT TABLES (IO IS SLOW)|   C
C   |     THIS SUBROUTINE ALSO DIFFERS FROM GET_TTS BY CALCULATING THE   |   C
C   |     SLOWNESS INSTEAD OF JUST THE TRAVEL TIMES.  THIS MEANS THAT YOU|   C
C   |     DON'T NEED TO CONSTRUCT SEPARATE SLOWNESS TABLES.  THIS SUB    |   C
C   |     REMOVES THE EXTRAPOLATION PORTION FROM GET_TT AND GET_TTS.     |   C
C   |                                                                    |   C
C   |INPUTS:    phase  =  NAME OF FILE CONTAINING TRAVEL TIME TABLES     |   C
c   |           ip     =  INDEX NUMBER OF FILE (NOT IO FOR FORTRAN).     |   C
C   |           del    =  DISTANCE RANGE (IN DEGREES).                   |   C
C   |           qdep   =  EARTHQUAKE DEPTH                               |   C
C   |                                                                    |   C
C   |OUTPUTS:   tt     =  TRAVEL TIME IN (SECONDS)                       |   C
C   |           dtdh   =  VERTICAL SLOWNESS IN MINUTES/KM                |   C
C   |           dtddel =  HORIZONTAL SLOWNESS IN MINUTES/DEGREE          |   C
C   |           iflag  = -1 IF OUTSIDE DEPTH RANGE OF TT TABLE           |   C
C   |                  =  0 FOR INTERPOLATION                            |   C
C   |                                                                    |   C
C   |WARNING:  THIS SUBROUTINE MAY HAVE CONFLICTS WITH DATA INPUT FROM   |   C
C   |      FILE I/O NUMBER 3.                                            |   C
C   |                                                                    |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |SET PARAMETERS AND VARIABLES:                                       |   C
      PARAMETER   (nx0=361,nd0=100,np0=20)         !MAX DIMENSIONS OF TT TABLES
      CHARACTER*80 phase,phaseold(np0),linebuf    !PHASE FILE
      INTEGER      nx(np0),nd(np0)                !NUMBER OF ROWS AND COLUMNS
      REAL         t(nx0,nd0,np0),x(nx0,np0),d(nd0,np0) !TTIME,DISTANCE,EQ DEPTH
      SAVE         t,x,d,phaseold,nx,nd           !DATA SAVED REDUCING RE-READS

      IF (ip.LT.1.OR.ip.GT.np0) THEN
         WRITE(6,*) '*** ERROR: in TTM, ip OUT OF RANGE: ',ip
         STOP
      END IF
c
c READ file IF new phase file is specIFied
      IF (phase.NE.phaseold(ip)) THEN             !READ IF NOT ALREADY READ IN
         OPEN (3,file=phase,status='old',err=990)
         READ (3,'(a40)') linebuf   !ignore first line  !BLANK LINE
         READ (3,*) nx(ip),nd(ip)                 !DIMENSION OF THE TT TABLE
         IF (nx(ip).GT.nx0) THEN                  !TRUNCATE IF TABLE TOO LARGE
          WRITE(6,*) '***WARNING: GET_TTS nx TRUNCATED ',nx(ip),nx0
          nx(ip)=nx0
         END IF
         IF (nd(ip).GT.nd0) THEN                  !TRUNCATE IF TABLE TOO LARGE
          WRITE(6,*) '***WARNING: GET_TTS nd TRUNCATED ',nd(id),nd0
             nd(ip)=nd0
         END IF
         READ (3,*) (d(id,ip),id=1,nd(ip))        !READ IN THE DEPTHS OF TABLE
         DO 20 ix=1,nx(ip)                        !READ DISTANCES AND TIMES
          READ (3,*) x(ix,ip),(t(ix,id,ip),id=1,nd(ip))
20       END DO
         CLOSE (3)
      END IF
      phaseold(ip)=phase                          !STORE PHASE NAME FOR FUTURE

C   |CHECK TO SEE IF EQ DEPTH IS OUTSIDE TABLE DEPTH RANGE               |   C
      IF ( (qdep.LT.d(1,ip)) .OR. (qdep.GT.d(nd(ip),ip)) ) THEN
       iflag  = -1
       tt     = 999
       dtdh   = 999
       dtddel = 999
       RETURN
      END IF

C   |CHECK IF DISTANCE AND DEPTH IS IN THE RANGE OF THE THE TT TABLES:   |   C
      DO 30 id=2,nd(ip)                              !SCROLL THROUGH DEPTHS
       IF (d(id,ip).LT.qdep) GOTO 30
       id1  =id-1
       id2  =id
       GOTO 32
30    END DO
      id1     = nd(ip)-1
      id2     = nd(ip)
32    DO 35 ix=2,nx(ip)                              !SCROLL THROUGH DISTANCES
         IF (x(ix,ip).LT.del) GOTO 35
         ix1  = ix-1
         ix2  = ix
         GOTO 37
35    END DO
      ix1=nx(ip)-1
      ix2=nx(ip)

37    IF (t(ix1,id1,ip).EQ.0.) GOTO 50              !CHECK FOR EXTRAPOLATION
      IF (t(ix1,id2,ip).EQ.0.) GOTO 50
      IF (t(ix2,id1,ip).EQ.0.) GOTO 50
      IF (t(ix2,id2,ip).EQ.0.) GOTO 50
      IF (x(ix2,ip).LT.del)    GOTO 50
      
      iflag=0
      xfrac=(del-x(ix1,ip))/(x(ix2,ip)-x(ix1,ip))   !INTERPOLATION DIST FRACTION
      dfrac=(qdep-d(id1,ip))/(d(id2,ip)-d(id1,ip))  !INTERPOLATION DEPTH FRACT
      
      t1     =t(ix1,id1,ip)+xfrac*(t(ix2,id1,ip)-t(ix1,id1,ip)) !dtdx1
      t2     =t(ix1,id2,ip)+xfrac*(t(ix2,id2,ip)-t(ix1,id2,ip)) !dtdx2
      t3     =t(ix1,id1,ip)+dfrac*(t(ix1,id2,ip)-t(ix1,id1,ip)) !dtdh1
      t4     =t(ix2,id1,ip)+dfrac*(t(ix2,id2,ip)-t(ix2,id1,ip)) !dtdh2
            
      tt     = (t1+dfrac*(t2-t1))*60.              !METHOD 1 TRAVEL TIME
      tt22   = (t3+xfrac*(t4-t3))*60.              !METHOD 2 TRAVEL TIME
      dtdh   = (t2-t1)/(d(id2,ip)-d(id1,ip))*60.   !VERTICAL SLOWNESS
      dtddel = (t4-t3)*180./atan(1.)/4.*60.        !HORIZONTAL SLOWNESS
      dtddel = dtddel/(x(ix2,ip)-x(ix1,ip))!/6371./1737.
      RETURN

50    iflag  = -2
      tt     = 999
      dtdh   = 999
      dtddel = 999
      RETURN

990   WRITE(6,*)  '*** ERROR: PHASE NOT FOUND: ',phase
      STOP
      
999   RETURN
      END


      SUBROUTINE ialen(string,I)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE DETERMINS THE LENGT OF A CHARACTER STRING, string:  |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      CHARACTER*(*) string
      INTEGER       I
      I = 0
      DO WHILE (string(I+1:I+1).NE.' ')
       I = I + 1
      END DO
      RETURN
      END 


      SUBROUTINE INTERP(x1,x2,x3,y1,y2,y3)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE FINDS THE Y3 VALUE BETWEEN Y1 AND Y2 USING THE X3   |   C
C   |     POSITION RELATIVE TO X1 AND X2:                                |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      fract = (x3-x1)/(x2-x1)
      y3    = y1+fract*(y2-y1)
      RETURN
      END
