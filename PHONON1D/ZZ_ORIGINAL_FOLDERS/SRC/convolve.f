      PROGRAM convolve_prog
      IMPLICIT     NONE
      INTEGER      SMX,DMX                             !MAX SIG & DNST LENGTH
      PARAMETER(   SMX= 8192,DMX=1000)                 !MAX SIG & DNST LENGTH
      REAL         tt(SMX),d(DMX)                       !TIME & DNSTANCE
      REAL         s_in(DMX,SMX),s_out(DMX,SMX)        !SIGNAL IN & OUT
      CHARACTER*80 ifile,ofile                         !INPUT & OUTPUT FILES
      INTEGER      status                              !I/O ERROR 0=OK
      INTEGER      nt,nd                               !# OF TIME & DNST STEPS
      REAL         s1(SMX),s2(SMX)                     !SCRATCH
      REAL         sta,lta                             !SHORT- & LONG-TERM AVERAGES
      INTEGER      NLT,NST                             !LENTH OF SHORT- & LONG-TRM AV
      REAL         sta_lta_min                         !MINIMUM STA/LTA FOR TRIGGER
      INTEGER      i_trigger                           !POINT OF STA/LTA TRIGGER
      REAL         win_length                          !TRIGGER WINDOW LENGTH (SEC)
      REAL         dt                                  !TIME INTERVAL
      INTEGER      n_win                               !WINDOW LENGTH (IN POINTS)
      REAL         pi,wt                               !PI=3.14..., & COSINE WEIGHT
      REAL         tdelay                              !SHIFT FOR DECONVOLVED SERIES
      REAL         t1
      REAL         tf21(SMX)                           !TRANSFER FUNCTION
      INTEGER      npts,dt2                            !NUMBER OF DATA POINTS
      REAL         blnk                                !DUMMY VARIABLE
      INTEGER      I,J                                 !INDEX VARIABLES
      REAL         ran0
      INTEGER      IDUM,IJ

      IDUM = 112
      
      WRITE(6,'(A)') 'ENTER INPUT FILE NAME (E.G. trace.out):'
      READ (5,'(A)')  ifile
      OPEN (1,FILE=ifile,STATUS='OLD')                 !OPEN INPUT FILE
      WRITE(6,'(A)') 'ENTER OUTPUT FILE NAME (E.G. decon.out):'
      READ (5,'(A)')  ofile
      OPEN (2,FILE=ofile,STATUS='UNKNOWN')             !OPEN OUTPUT FILE
      
      READ (1,*) nt,nd                                 !READ IN # TIME & DNSTS
      WRITE(2,*) nt,nd                                 !COPY TO OUTPUT
      READ (1,FMT=888) blnk,(d(J),J=1,nd)              !READ IN DISTANCES
      WRITE(2,FMT=888) blnk,(d(J),J=1,nd)              !READ IN DISTANCES
      
      DO I = 1, nt                                     !FOR EACH TIME & DNSTANCE
       READ(1,FMT=888,IOSTAT=status)tt(I),(s_in(J,I),J=1,nd)!READ IN EACH ROW (TIME)
       IF (status /= 0) EXIT                           !IF READ ERROR, THEN STOP
       write(6,*) I,tt(I)
      END DO                                           !
      
      NLT = 100                                        !# STEPS IN LONG-TERM AVERAGE
      NST = 3                                          !# STEPS IN SHORT-TERM AVERAGE
      sta_lta_min = 3.                                 !MIN STA/LTA FOR A TRIGGER
      win_length = 5.                                !WINDOW LENGTH AROUND TRIGGER
      dt = tt(2)-tt(1)                                   !TIME INTERVAL
      n_win      = int(win_length/dt)+1                !TRIGGER WINDOW LENGTH (POINTS)
      pi = atan(1.)*4.                                 !SET pi = 3.14...
      
      DO I = 1, nd
       s1(1:nt) = s_in(I,1:nt)                         !COPY 2D DATA TO 1D VECTOR 
       s2(1:nt) = s_in(I,1:nt)                         !COPY 2D DATA TO 1D VECTOR 

       lta = s1(1)                                     !START LONG-TERM AVERAGE
       DO J = 2, NLT                                   !EACH POINT IN LTA
        lta = lta + s1(J)                              !SUM INITIAL LONG-TERM AVE
       END DO                                          !
       lta = lta / float(NLT)                          !NORMALIZE LTA
       
       WRITE(6,*) 'HI1:',I
       
       sta = s1(NLT-NST)                               !START SHORT-TERM AVERAGE
       DO J = NLT-NST+1, NLT                           !EACH POINT IN STA
        sta = sta + s1(J)                              !SUM INITIAL SHORT-TERM AVE
       END DO                                          !
       sta = sta / float(NST)                          !NORMALIZE LTA
       
       DO J = NLT+1, nt                                !RUN STA/LTA FILTER
        sta = sta + (s1(J)-s1(J-NST))/ float(NST)      !CALC STA
        lta = lta + (s1(J)-s1(J-NLT))/ float(NLT)      !CALC LTA
        IF (abs(sta) > 0.) THEN                        !CHECK THAT SOME SIGNAL EXISTS
         IF (sta > sta_lta_min*lta) THEN               !CHECK IF STA/LTA > MIN
          i_trigger = J                                !POINT OF TRIGGER
          EXIT                                         !STOP STA/LTA FILTER
         END IF
        END IF
       END DO
       WRITE(6,*) 'HI3:',I,'ITRIG:',tt(i_trigger)
       
       DO J = 1, nt
        IJ =J + int(400*(ran0(IDUM)-0.5))
        if (J > nt-300) s1(j) = s1(j) / (301-(nt-j))**0.125
	if ( (IJ <1).OR.(IJ > nt) ) cycle
        s1(J) = s1(J) + s1(IJ)/1.75
	! + s1(J-50)/5.0 + s1(J-150)/5.
       END DO
       
       
       DO J = 1, nt                                    !FOR EACH POINT NEAR TRIGGER
         s2(J) = ran0(IDUM)                             !WEIGHT THE SERIES
         s2(J) = s2(J)*0.1
       END DO
       s2(1)   = 2.00
       s2(2)   = 1.00
       s2(5)   = 0.50
       s2(10)  = 2.00
       s2(20)  = 0.5
       s2(50)  = -1.0
       s2(100) = 2.0
       s2(101) = 0.50
       s2(102) = 2.00
       s2(105) = 0.50
       s2(110) = 2.00
       s2(120) = 0.5
       s2(150) = -1.
       s2(200) = -0.5

       WRITE(6,*) 'HI4:',I,dt

       npts = nt                                       !COPY # POINTS JUST IN CASE
       dt2  = dt                                       !COPY TIME INBERVAL J.I.C.
       tdelay = tt(i_trigger)                           !TIME TO SHIFT DECON SERIES
C       WRITE(6,*) npts,dt
       CALL convolve(npts,dt,tdelay,s2,s1,tf21)    !DECONVOLVE GET TRANSFER FUNCT
       call bit_limit(tf21(1:nt),nt,10.)
       s_out(I,1:nt) = tf21(1:nt)                      !COPY 1D DECON TO OUTPUT
       WRITE(6,*) 'HI5:',I
       
      END DO
      
      DO I = 1, nt
       WRITE(2,FMT=888) tt(I),(s_out(J,I)/100.,J=1,nd)       !WRITE OUT EACH ROW (TIME)
      END DO
      
      CLOSE(1)
      CLOSE(2)
      
888   FORMAT(F10.6,1X,500(F10.6,1X))                       !FORMATTING STATEMENT
      STOP
      END PROGRAM convolve_prog


      SUBROUTINE NEXT_POWER_2(npts,np)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SERIES DETERMINES THE POWER OF TWO THAT IS EQUAL OR JUST       |   C
C   |     GREATER THAN THE NUMBER OF POINTS SUPPLIED:                    |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
C   |     CONTACT: jlawrence@ucsd.edu                                    |   C
C   |                                                                    |   C
C   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      INTEGER npts,np                  !NUMBER OF POINTS, & 2^np >= npts

      np = 0                           !ASSUME SMALL AND BUILD UP FROM THERE
      DO WHILE (2**np.LT.npts)         !
       np = np + 1
      END DO
      np = 2**np                       !
      
      RETURN
      END SUBROUTINE NEXT_POWER_2      !END NPOW2



      SUBROUTINE NORM21R(series,ndat)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE NORMALIZES ANY SERIES, series, TO SOME VALUE, normf.|   C
C   |                                                                    |   C
C   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
C   |     CONTACT: jlawrence@ucsd.edu                                    |   C
C   |                                                                    |   C
C   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   | SET PARAMETERS AND VARIABLES:                                      |   C
      REAL             series(*),maxf         !INPUT & OUTPUT SERIES
      INTEGER          ndat                   !NUMBER OF POINTS
      maxf = series(1)                        !MAX IS FIRST POINT
      DO I = 2, ndat                          !
       IF (maxf.LT.series(I)) maxf = series(I)!REPLACE MAX IF BETTER
      END DO                                  !
      IF (maxf.EQ.0.) THEN                    !IF MAX IS ZERO, THEN SKIP
       WRITE(6,*) 'SKIPPING NORMALIZATION, MAX = 0.',maxf
       RETURN                                 !
      END IF                                  !
      DO I = 1, ndat                          !DIVIDE SERIES BY MAXIMUM
       series(I) = series(I)/maxf             !
      END DO                                  !
      RETURN                                  !
      END SUBROUTINE NORM21R                  !END NORM21R
      
      FUNCTION ran0(idum)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
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
      END FUNCTION ran0
