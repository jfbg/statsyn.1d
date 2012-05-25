      PROGRAM PSSTACK
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
      integer,PARAMETER  :: NMAX = 1000,SMAX = 8192,MMAX = NMAX*SMAX*3
      CHARACTER*80  ifile,outfile,pfile(100),pnm,atemp
      INTEGER       ndat,ndmax,icol,CNT,CNT2
      REAL          temp,del(NMAX),time(SMAX)
      REAL          val(MMAX)
      INTEGER*2     ibuf(MMAX)
      REAL          amin,amax
      REAL          tmin,tmax,dmin,dmax
      REAL          ttic1,ttic2,dtic1,dtic2
      INTEGER       plotyn,iplot,idmin,idmax,smoothyn
      REAL          qdep,delt
      REAL          val2(MMAX),val2D(SMAX,NMAX)

      WRITE(6,*) 'ENTER THE FILE NAME OF INPUT STACK FILE:'
      READ (5,'(A)')     ifile
      WRITE(6,*) 'ENTER SATURATION AMPLITUDES (MIN,MAX):'
      READ (5,*)     amin,amax
      WRITE(6,*) 'ENTER THE OUTPUT FILE NAME:'
      READ (5,'(A)')     outfile
      WRITE(6,*) 'PLOT TRAVEL TIMES (0)NO (1)YES?:'
      READ (5,*)     plotyn
      WRITE(6,*) 'REF PHASE: 0)NONE 1=P 2=S 3=PP 4=SS: 32=PKPDF'
      READ (5,*)     IP
      WRITE(6,*) 'ENTER THE EARTHQUAKE DEPTH (IN KM):'
      READ (5,*)     qdep
      WRITE(6,*) 'SMOOTH? (0)NO  (1)YES:'
      READ (5,*)     smoothyn

      WRITE(6,*) 'READING HEADER:'
      OPEN(1,FILE=ifile,STATUS='OLD')                  !OPEN INTPUT FILE
      READ (1,*)     ndat,ndmax
      WRITE(6,*)     ndat,ndmax
      READ (1,FMT=888) temp,(del(I),I=1,ndmax)
      CNT = 1
      WRITE(6,*) 'READING DATA:'
      IF (mod(ndmax,2).NE.0) ndmax = ndmax - 1        !REQUIRE EVEN ARRAY SIZE
      IF (mod(ndat ,2).NE.0) ndat  = ndat  - 1 
      ndmax = ndmax -4
      DO I = 1, ndat
       READ(1,FMT=888)time(I),(val2D(I,J),J=1,ndmax)
       DO J = 1, ndmax
        val2D(I,J) = -val2D(I,J)
       END DO
       CNT = CNT + ndmax
      END DO
      CLOSE(1)

      WRITE(6,*) 'DATA READ IN.',ndat,ndmax
      
      CALL minr1(time, ndat,tmin)                     !GET MIN & MAX TIMES
      CALL maxr1(time, ndat,tmax)
      CALL minr1( del,ndmax,dmin)                     !GET MIN & MAX DISTANCES
      CALL maxr1( del,ndmax,dmax)
      dt   = time(2) - time(1)
      ddel =  del(2) -  del(1)
      CALL PSFILE(outfile)                            !START POSTSCRIPT PLOT
      CALL PSFONT('t12')
      CALL PSCOL(1)
      WXMN = 1.0
      WXMX = 3.5
      WYMN = 1.0
      WYMX = 3.5
      CALL PSWIND(1.0,7.5,1.0,7.5,dmin,dmax,tmin,tmax) !SET PLOT SIZE
      WRITE(6,*) 'MAIN PLOT CREATED.',dmin,dmax,tmin,tmax
      CALL PSNICE(tmin,tmax,ttic1,ttic2)              !SET TIME TICK MARKS
      CALL PSNICE(dmin,dmax,dtic1,dtic2)              !SET DISTANCE TICK MARS
      ttic1 = ttic1/4.
      ttic2 = ttic2/4.
      WRITE(6,*) 'TICKS CREATED:',dtic1,dtic2,dtic1,dtic2
      CALL PSAXES(dtic1,dtic2,.05,'i3',ttic1,ttic2,.005,'f8.0') !PLOT AXES
      CALL PSLAX('Range (degrees)',.3,'Time (sec)',.4) !LABEL THE AXES
      WRITE(6,*) 'AXES CREATED:',dmin,dmax,tmin,tmax
      CALL PSLORG(8)                                  !POSITION OF CURSOR
      dave = (dmax+dmin)/2.                           !AVERAGE DELTA VALUE
      CALL PSMOVE(dave,tmax)                          !MOVE CURSOR TO TOP 
      CALL PSTIC(.1,0.,1)                             !MOVE CURSOR UP A LITTLE
      CALL PSLAB('Amplitude')                         !LABEL THE PLOT
      icol = 1
      
      
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
!     SMOOTH THE PLOT:                                                   |   !
    
      IF (smoothyn.GE.1) THEN
       CNT =   0 
       DO I = 1, ndat
        DO J = 1, ndmax
         CNT = CNT + 1
         val2(CNT) = val2D(I,J)*2.
         CNT2      = 2.
	IF (smoothyn.GE.2) THEN
         IF (I.NE.1) THEN
          val2(CNT) = val2(CNT) + val2D(I-1,J  )*1.
          CNT2      = CNT2 + 1
         END IF
         IF (I.NE.ndat) THEN
          val2(CNT) = val2(CNT) + val2D(I+1,J  )*1.
          CNT2      = CNT2 + 1
         END IF
        END IF
        IF (J.NE.1) THEN
         val2(CNT) = val2(CNT) + val2D(I  ,J-1)*2.
         CNT2      = CNT2 + 2
        END IF
	IF (J.NE.ndmax ) THEN
         val2(CNT) = val2(CNT) + val2D(I  ,J+1)*2.
         CNT2      = CNT2 + 2
        END IF
        IF( (I.NE.1    ).AND.(J.NE.1    ) ) THEN
         val2(CNT) = val2(CNT) + val2D(I-1,J-1)
         CNT2      = CNT2 + 1
        END IF
        IF( (I.NE.ndat ).AND.(J.NE.1    ) ) THEN
         val2(CNT) = val2(CNT) + val2D(I+1,J-1)
         CNT2      = CNT2 + 1
        END IF
        IF ( (I.NE.1   ).AND.(J.NE.ndmax) ) THEN
         val2(CNT) = val2(CNT) + val2D(I-1,J+1)
         CNT2      = CNT2 + 1
        END IF
        IF ( (I.NE.ndat).AND.(J.NE.ndmax) ) THEN
         val2(CNT) = val2(CNT) + val2D(I+1,J+1)
         CNT2      = CNT2 + 1
        END IF
        IF (CNT2.GT.1) val2(CNT) = val2(CNT)/float(CNT2)
       END DO
      END DO
      ELSE
       CNT = 0
       DO I = 1, ndat
        DO J = 1, ndmax
         CNT = CNT + 1
         val2(CNT) = val2D(I,J)
        END DO
       END DO
      END IF

      
      CNT  = -2
      CNT2 =  0
      DO I = 1, ndat
       DO J = 1, ndmax
         CNT  =  CNT + 3
         CNT2 = CNT2 + 1
         anorm=(val2(CNT2)-amin)/(amax-amin)
         IF (anorm.GT. 1.) anorm =  1.
         IF (anorm.LT. 0.) anorm =  0.
         CALL GETRBG(anorm,red,blue,green)
         ibuf(CNT  ) = nint(blue*225.)
         ibuf(CNT+1) = nint(green*225.)
         ibuf(CNT+2) = nint(red*225.)
!        WRITE(6,*) CNT,ibuf(CNT),ibuf(CNT+1),ibuf(CNT+2)
       END DO
      END DO
      
      
      CALL PSIMAG(ibuf,ndmax,ndat,icol)
      
      pfile(01)='/Volumes/eyes/TT/tt91.S'   !  SET TRAVEL TIME TABLE NAMES
      pfile(02)='/Volumes/eyes/TT/tt91.ScS'    
      pfile(03)='/Volumes/eyes/TT/tt91.SS'
      pfile(04)='/Volumes/eyes/TT/tt91.S3'
      pfile(05)='/Volumes/eyes/TT/tt91.S4'
      pfile(06)='/Volumes/eyes/TT/tt91.S660S'
      pfile(07)='/Volumes/eyes/TT/tt91.S410S'
      pfile(08)='/Volumes/eyes/TT/tt91.ScS2'
      pfile(09)='/Volumes/eyes/TT/tt91.ScS3'

      CALL PSPS('stroke')
      atemp = 'NONE                       '
      IF (plotyn.NE.0) THEN
!      IP = 1
      DO I = 1, 9
       IF (IP.NE.0) THEN
        pnm = pfile(I)
        pnm = pnm(30:38)
       CALL ttplot(pfile(IP),pfile(I),pnm,dmin,dmax,ddel,tmin,tmax,qdep)
       ELSE
        pnm = pfile(I)
        pnm(1:5) = pnm(30:34)
        CALL ttplot(atemp   ,pfile(I),pnm,dmin,dmax,ddel,tmin,tmax,qdep)
       END IF
      END DO
      END IF
            
      CALL PSEND

      
888   FORMAT(F10.6,1X,500(F12.6,1X))

      STOP
      END program psstack




      SUBROUTINE ttplot(pfile1,pfile2,pnm,dmin,dmax,ddel,tmin,tmax,qdep)
      CHARACTER*80      pfile1,pfile2,pnm
      CHARACTER*9       pnm2
      REAL              dmin,dmax,ddel,tmin,tmax        !DIMENSIONS OF PLOT
      INTEGER           idmin,idmax
      REAL              tt1,tt2,dtime,del,qdep,dtdh,dtddel
      INTEGER           iflag,CNT
      SAVE              DD
      pnm2 = pnm(1:9)
      idmin = 0
      idmax = nint(dmax/ddel) - nint(dmin/ddel)          !SET HORIZONTAL DIMENSION OF PLOT
      tt1   = 0.                                        !SET VARAIBLES TO ZERO
      CNT  = 0
      xp = -999.
      yp = -999.
      DD = DD + 20
      IF (DD.GT.idmax) DD = DD - idmax
      IF (DD.LT.dmin) DD = 10

      CALL PSMOVE(dmin,tmin)
      DO I = idmin,idmax                                !FOR EACH DISTANCE IN PLOT
       del = float(I)*ddel+dmin
       tt1  = 0.
       IF (pfile1(1:4).NE.'NONE') THEN 
        CALL GET_TTJ(pfile1,1,del,qdep,tt1,dtdh,dtddel,iflag)!CALCULATE REFERENCE TRAVEL TIME
       END IF
       IF (iflag.LT.0) tt1 = 0. 
       CALL GET_TTJ(pfile2,2,del,qdep,tt2,dtdh,dtddel,iflag)!CALCULATE PHASE TRAVEL TIME
       IF (iflag.GE.0) THEN
        dtime = tt2 - tt1                                !ADJUST FOR REFERENCE FRAME TIME
!        IF (pnm2(1:9).EQ.'PKIKP    ') WRITE(6,*) pnm2(1:9),del,dtime
        IF ((dtime.LE.tmax).AND.(dtime.GE.tmin))THEN
         IF (CNT.EQ.0)  CALL PSMOVE(del,dtime)
         IF (CNT.GT.0)  CALL PSDRAW(del,dtime)          !DRAW THE POINT IF DATA IN PLOT RANGE
         IF (CNT.EQ.DD+5) THEN
         xpn = del
         ypn = dtime
         END IF
         IF (CNT.EQ.DD) THEN
          xp = del
          yp = dtime
         END IF
         CNT = CNT + 1
        END IF
       END IF
999   END DO      
      CALL PSPS('stroke')
      
      yrng = tmax-tmin
      xrng = dmax-tmin
      r2d  = 180./atan(1.)/4.
      IF (xp.NE.-999)THEN
       CALL PSLORG(2)
       CALL PSMOVE(xp,yp+5)
       ang = atan ( ((ypn-yp)/yrng)/ ((xpn-xp)/xrng) )*r2d
       WRITE(6,*) ang
       CALL PSANG(ang)
       CALL PSLAB(pnm2)
      END IF
      
      
      RETURN
      END





      SUBROUTINE GETRBG(a,red,blue,green)
      IF (a.LE.0.5) THEN         
         red=1.
         wht=2*a
         blue=wht
         green=wht
      ELSE
         blue=1.
         wht=2*(1.-a)
         red=wht
         green=wht
      END IF
      RETURN
      END



      SUBROUTINE GET_TTJ(phase,ip,del,qdep,tt,dtdh,dtddel,iflag)
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
!   |THIS SUBROUTINE OBTAINS THE TRAVEL TIME, HORIZONTAL, AND VERTICAL   |   !
!   |     FOR A SPECIFIED PHASE FOR ANY GIVEN DISTANCE AND DEPTH.  THE   |   !
!   |     TRAVEL TIMES AND SLOWNESS VALUES ARE INTERPOLATED FROM PRE-    |   !
!   |     CONSTRUCTED TRAVEL TIME TABLES.                                |   !
!   |                                                                    |   !
!   |THIS SUBROUTINE WAS CHANGED FROM TTM BY CHANGING UNITS TO SECONDS   |   !
!   |     RATHER THAN MINUTES.                                           |   !
!   |                                                                    |   !
!   |THIS SUBROUTINE WAS RIPPED OFF OF PETER SHEARER'S GET_TTS SUBROUTINE|   !
!   |     WHICH WAS ALTERED FROM GET_TT. THE PROGRAM DIFFERS FROM GET_TT |   !
!   |     BY SAVING DATA WITHOUT HAVING TO RE-READ TT TABLES (IO IS SLOW)|   !
!   |     THIS SUBROUTINE ALSO DIFFERS FROM GET_TTS BY CALCULATING THE   |   !
!   |     SLOWNESS INSTEAD OF JUST THE TRAVEL TIMES.  THIS MEANS THAT YOU|   !
!   |     DON'T NEED TO CONSTRUCT SEPARATE SLOWNESS TABLES.  THIS SUB    |   !
!   |     REMOVES THE EXTRAPOLATION PORTION FROM GET_TT AND GET_TTS.     |   !
!   |                                                                    |   !
!   |INPUTS:    phase  =  NAME OF FILE CONTAINING TRAVEL TIME TABLES     |   !
!   |           ip     =  INDEX NUMBER OF FILE (NOT IO FOR FORTRAN).     |   !
!   |           del    =  DISTANCE RANGE (IN DEGREES).                   |   !
!   |           qdep   =  EARTHQUAKE DEPTH                               |   !
!   |                                                                    |   !
!   |OUTPUTS:   tt     =  TRAVEL TIME IN (SECONDS)                       |   !
!   |           dtdh   =  VERTICAL SLOWNESS IN MINUTES/KM                |   !
!   |           dtddel =  HORIZONTAL SLOWNESS IN MINUTES/DEGREE          |   !
!   |           iflag  = -1 IF OUTSIDE DEPTH RANGE OF TT TABLE           |   !
!   |                  =  0 FOR INTERPOLATION                            |   !
!   |                                                                    |   !
!   |WARNING:  THIS SUBROUTINE MAY HAVE CONFLICTS WITH DATA INPUT FROM   |   !
!   |      FILE I/O NUMBER 3.                                            |   !
!   |                                                                    |   !
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
!   |SET PARAMETERS AND VARIABLES:                                       |   !
      PARAMETER   (nx0=361,nd0=100,np0=20)         !MAX DIMENSIONS OF TT TABLES
      CHARACTER*80 phase,phaseold(np0),linebuf    !PHASE FILE
      INTEGER      nx(np0),nd(np0)                !NUMBER OF ROWS AND COLUMNS
      REAL         t(nx0,nd0,np0),x(nx0,np0),d(nd0,np0) !TTIME,DISTANCE,EQ DEPTH
      SAVE         t,x,d,phaseold,nx,nd           !DATA SAVED REDUCING RE-READS

      IF (ip.LT.1.OR.ip.GT.np0) THEN
         WRITE(6,*) '*** ERROR: in TTM, ip OUT OF RANGE: ',ip
         STOP
      END IF
!
! READ file IF new phase file is specIFied
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

!   |CHECK TO SEE IF EQ DEPTH IS OUTSIDE TABLE DEPTH RANGE               |   !
      IF ( (qdep.LT.d(1,ip)) .OR. (qdep.GT.d(nd(ip),ip)) ) THEN
       iflag  = -1
       tt     = 999
       dtdh   = 999
       dtddel = 999
       RETURN
      END IF

!   |CHECK IF DISTANCE AND DEPTH IS IN THE RANGE OF THE THE TT TABLES:   |   !
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



      SUBROUTINE maxr1(series,ndat,max)
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
!   |THIS SUBROUTINE RECORDS THE MAXIMUM, max, OF A SERIES, series, WITH |   !
!   |     npts POINTS, AT SPACE HOLDER imax.                             |   !
!   |                                                                    |   !
!   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   !
!   |      CONTACT:       jflawrence@stanford.edu                        |   !
!   |                                                                    |   !
!   |AS WITH ALL CODES: "BEWARE OF THE BUG!"                             |   !
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
!   |SET PARAMETERS AND DECLARE VARIABLES:                               |   !
      REAL             series(*),max
      INTEGER          I,ndat

      max = 0.
      DO I = 1, ndat
       IF (max.LT.series(I)) max = series(I)
      END DO

      RETURN
      END

      SUBROUTINE minr1(series,ndat,min)
!   |THIS SUBROUTINE RECORDS THE minIMUM, min, OF A SERIES, series, WITH |   !
!   |     npts POINTS, AT SPACE HOLDER imin.                             |   !
      REAL             series(*),min
      INTEGER          I,ndat

      min = 99999.
      DO I = 1, ndat
       IF (min.GT.series(I)) min = series(I)
      END DO

      RETURN
      END

