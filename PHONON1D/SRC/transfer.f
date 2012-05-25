      SUBROUTINE TRANSFER_JL(npts,dt,toff,w1,w2,nf,df,
     &                       s1,s2,cohe,tf12,tf21)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE CALCULATES THE TRANSFER FUNCTIONS (FORWARD & BACK)  |   C
C   |     BETWEEN TWO REAL SERIES.  THE INPUT IS TIME DOMAIN. THE OUTPUT |   C
C   |     IS IN THE FREQUENCY DOMAIN.  IT ALSO RETURNS EACH SERIES IN THE|   C
C   |     FREQUENCY DOMAIN.                                              |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE USES THE FOLLOWING SUBROUTINES:                     |   C
C   |     NEXT_POWER_2    -   FINDS THE NEXT POWER OF TWO >= VALUE GIVEN |   C
C   |     GET_SPEC        -   DOES THE FAST FOURIER TRANSFORM            |   C
C   |     WHITEN_JL       -   WHITENS THE SPECTRUM BY NORMALIZING W/ LTA |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
C   |     CONTACT: jflawrence@stanford.edu                               |   C
C   |                                                                    |   C
C   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      IMPLICIT   NONE                            !ALLOW NO IMPLICIT VARIABLES
      REAL       w1(*),w2(*)                     !INPUT DATA SERIES
      COMPLEX    s1(*),s2(*)                     !SPECTRA OF EACH SERIES
      COMPLEX    cohe(*)                         !COHERENCE
      COMPLEX    tf12(*),tf21(*)                 !TRANSFER FUNCTIONS
      INTEGER    npts,np2,nf                     !NUMBER OF DATA POINTS
      REAL       dt,df                           !TIME & FREQ SAMPLING INTERVALS
      INTEGER    I,J,K                           !STEP
      REAL       wl1,wl2,wl                      !WATER LEVELS
      REAL       toff                            !TIME DIFF BETWEEN FILES
      REAL       pi                              !pi = 3.14....
      COMPLEX    p_shift12,p_shift21             !PHASE SHIFTS
      
      
      pi = atan(1.)*4.                           !SET PI = 3.14....
      CALL NEXT_POWER_2(npts,np2)                !FIND THE NEXT POWER OF 2
      
      IF (np2 < npts) THEN                       !PAD SERIES WITH ZEROS 
       w1(npts+1:np2) = 0.                       !
       w2(npts+1:np2) = 0.                       !
      END IF                                     !
      
      CALL GET_SPEC(w1,np2,dt,s1,nf,df)          !GET SPECTRUM OF DATA SERIES 1
      CALL GET_SPEC(w2,np2,dt,s2,nf,df)          !GET SPECTRUM OF DATA SERIES 2
      
      
      CALL WHITEN_JL(nf,s1,20)                   !WHITEN SPECTRUM
      CALL WHITEN_JL(nf,s2,20)                   !WHITEN SPECTRUM
      
      wl = 1D-6                                  !WATER LEVEL
      
      DO I = 1, nf                               !CALCULATE TRANSFER & COHERENC
       wl1 = s1(I)*conjg(s1(I))                  !CALCULATE DENOMINATORS
       wl2 = s2(I)*conjg(s2(I))                  !

       cohe(I) = s1(I)*conjg(s2(I))/abs(wl1*wl2)**0.5!*p_shift21
     &         + s2(I)*conjg(s1(I))/abs(wl1*wl2)**0.5!*p_shift12  !
   
       IF (wl1 < wl*wl2) wl1 = wl*wl2            !USE WATER LEVEL FOR TRANSFER FUNCS
       IF (wl2 < wl*wl1) wl2 = wl*wl1            !USE WATER LEVEL FOR TRANSFER FUNCS
       
       tf12(I) = s1(I)*conjg(s2(I))/wl2          !CALCULATE TRANSFER FUNCTIONS
       tf21(I) = s2(I)*conjg(s1(I))/wl1          !CALCULATE TRANSFER FUNCTIONS
       
      END DO                                     !
      
      RETURN                                     !
      END SUBROUTINE TRANSFER_JL                 !
      
      
      
      SUBROUTINE TRANSFER_JL2(npts,dt,toff,w1,w2,r1)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE CALCULATES THE TRANSFER FUNCTIONS (FORWARD & BACK)  |   C
C   |     BETWEEN TWO REAL SERIES.  THE INPUT IS TIME DOMAIN. THE OUTPUT |   C
C   |     IS IN THE FREQUENCY DOMAIN.  IT ALSO RETURNS EACH SERIES IN THE|   C
C   |     FREQUENCY DOMAIN.                                              |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE USES THE FOLLOWING SUBROUTINES:                     |   C
C   |     NEXT_POWER_2    -   FINDS THE NEXT POWER OF TWO >= VALUE GIVEN |   C
C   |     GET_SPEC        -   DOES THE FAST FOURIER TRANSFORM            |   C
C   |     WHITEN_JL       -   WHITENS THE SPECTRUM BY NORMALIZING W/ LTA |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
C   |     CONTACT: jflawrence@stanford.edu                               |   C
C   |                                                                    |   C
C   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      IMPLICIT   NONE                            !ALLOW NO IMPLICIT VARIABLES
      REAL       w1(*),w2(*)                     !INPUT DATA SERIES
      COMPLEX    s1(16384),s2(16384)             !SPECTRA OF EACH SERIES
      COMPLEX    cohe(16384)                     !COHERENCE
      COMPLEX    tf12(16384),tf21(16384)         !TRANSFER FUNCTIONS
      INTEGER    npts,np2,nf                     !NUMBER OF DATA POINTS
      REAL       dt,df                           !TIME & FREQ SAMPLING INTERVALS
      INTEGER    I,J,K                           !STEP
      REAL       wl1,wl2,wl                      !WATER LEVELS
      REAL       toff                            !TIME DIFF BETWEEN FILES
      REAL       pi                              !pi = 3.14....
      COMPLEX    p_shift12,p_shift21             !PHASE SHIFTS
      REAL       r1(*)                           !
      INTEGER    ndat                            !
      REAL       dt2, w                          !
      REAL       gwidth,four_gw_sq,damp

      pi = atan(1.)*4.                           !SET PI = 3.14....
      CALL NEXT_POWER_2(npts,np2)                !FIND THE NEXT POWER OF 2
      
      IF (np2 > npts) THEN                       !PAD SERIES WITH ZEROS 
       w1(npts+1:np2) = 0.                       !
       w2(npts+1:np2) = 0.                       !
      END IF                                     !
      
      WRITE(6,*) 'dtA:',dt                       !
      CALL GET_SPEC(w1,np2,dt,s1,nf,df)          !GET SPECTRUM OF DATA SERIES 1
      CALL GET_SPEC(w2,np2,dt,s2,nf,df)          !GET SPECTRUM OF DATA SERIES 2
      WRITE(6,*) 'dtB:',dt                       !
      
!      CALL WHITEN_JL(nf,s1,20)                   !WHITEN SPECTRUM
!      CALL WHITEN_JL(nf,s2,20)                   !WHITEN SPECTRUM
      
      wl = 1D-3                                  !WATER LEVEL
      gwidth = 0.1                                !GAUSSIAN WIDTH
      four_gw_sq = 4.*gwidth**2                  !COMPUTE FACTOR AHEAD OF TIME
      
      DO I = 1, nf                               !CALCULATE TRANSFER & COHERENC
       wl1 = s1(I)*conjg(s1(I))                  !CALCULATE DENOMINATORS
       wl2 = s2(I)*conjg(s2(I))                  !
       w     = 2.   * pi   * float(I-1)*df       !ANGULAR FREQUENCY
       damp  = -(w)**2/four_gw_sq   !CALC THE DAMPING FACTOR

       IF (wl1 < wl*wl2) wl1 = wl*wl2            !USE WATER LEVEL FOR TRANSFER FUNCS
       tf21(I) = s2(I)*conjg(s1(I))/wl1
     &         * exp( cmplx( damp,  w*toff))     !CALCULATE TRANSFER FUNCTIONS
C       WRITE(6,*) 'DO:',w1(I),w2(I),s1(I),s2(I),dt,nf,df
      END DO                                     !
      
      CALL GET_TS(tf21,nf,df,0,r1,ndat,dt2)       !GET SPECTRUM OF y
C      WRITE(6,*) 'DING:',nf,ndat,tf21(1),tf21(2),r1(1),r1(2)
      RETURN                                     !
      END SUBROUTINE TRANSFER_JL2                !
      
      SUBROUTINE CONVOLVE(npts,dt,toff,w1,w2,r1)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE CALCULATES THE TRANSFER FUNCTIONS (FORWARD & BACK)  |   C
C   |     BETWEEN TWO REAL SERIES.  THE INPUT IS TIME DOMAIN. THE OUTPUT |   C
C   |     IS IN THE FREQUENCY DOMAIN.  IT ALSO RETURNS EACH SERIES IN THE|   C
C   |     FREQUENCY DOMAIN.                                              |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE USES THE FOLLOWING SUBROUTINES:                     |   C
C   |     NEXT_POWER_2    -   FINDS THE NEXT POWER OF TWO >= VALUE GIVEN |   C
C   |     GET_SPEC        -   DOES THE FAST FOURIER TRANSFORM            |   C
C   |     WHITEN_JL       -   WHITENS THE SPECTRUM BY NORMALIZING W/ LTA |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
C   |     CONTACT: jflawrence@stanford.edu                               |   C
C   |                                                                    |   C
C   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      IMPLICIT   NONE                            !ALLOW NO IMPLICIT VARIABLES
      REAL       w1(*),w2(*)                     !INPUT DATA SERIES
      COMPLEX    s1(16384),s2(16384)             !SPECTRA OF EACH SERIES
      COMPLEX    cohe(16384)                     !COHERENCE
      COMPLEX    tf12(16384),tf21(16384)         !TRANSFER FUNCTIONS
      INTEGER    npts,np2,nf                     !NUMBER OF DATA POINTS
      REAL       dt,df                           !TIME & FREQ SAMPLING INTERVALS
      INTEGER    I,J,K                           !STEP
      REAL       wl1,wl2,wl                      !WATER LEVELS
      REAL       toff                            !TIME DIFF BETWEEN FILES
      REAL       pi                              !pi = 3.14....
      COMPLEX    p_shift12,p_shift21             !PHASE SHIFTS
      REAL       r1(*)                           !
      INTEGER    ndat                            !
      REAL       dt2, w                          !
      REAL       gwidth,four_gw_sq,damp

      pi = atan(1.)*4.                           !SET PI = 3.14....
      CALL NEXT_POWER_2(npts,np2)                !FIND THE NEXT POWER OF 2
      
      IF (np2 > npts) THEN                       !PAD SERIES WITH ZEROS 
       w1(npts+1:np2) = 0.                       !
       w2(npts+1:np2) = 0.                       !
      END IF                                     !
      
      CALL GET_SPEC(w1,np2,dt,s1,nf,df)          !GET SPECTRUM OF DATA SERIES 1
      CALL GET_SPEC(w2,np2,dt,s2,nf,df)          !GET SPECTRUM OF DATA SERIES 2
      df=1./(dt*float((nf-1)*2))

      DO I = 1, nf                               !CALCULATE TRANSFER & COHERENC
       w     = 2.   * pi   * float(I-1)*df       !ANGULAR FREQUENCY
!       write(6,*) 'w',i,w,toff
       tf21(I) = s2(I)*(s1(I))                   !CALCULATE TRANSFER FUNCTIONS
     &         * exp( cmplx( 0., -w*toff))     !CALCULATE TRANSFER FUNCTIONS
      END DO                                     !
      
      CALL GET_TS(tf21,nf,df,0,r1,ndat,dt2)       !GET SPECTRUM OF y
C      WRITE(6,*) 'DING:',nf,ndat,tf21(1),tf21(2),r1(1),r1(2)
      RETURN                                     !
      END SUBROUTINE CONVOLVE                !
      
      
      SUBROUTINE WHITEN_JL(nf,s1,n_ave)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE SHIFTS THE POINTS BY TOFF AND INTERPOLATES BETWEEN  |   C
C   |     PREVIOUS POINTS.                                               |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
C   |     CONTACT: jflawrence@stanford.edu                               |   C
C   |                                                                    |   C
C   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      IMPLICIT             NONE
      INTEGER              nf,n_ave              !# OF POINTS & SMOOTHING POINTS
      INTEGER              n_half                !HALF OF n_ave
      COMPLEX              s1(*)                 !SPECTRUM
      REAL                 ave,cnt               !AVERAGE OVER n_ave POINTS
      COMPLEX(4), DIMENSION(:), ALLOCATABLE   :: s2 
      INTEGER              I,J,K                 !COUNTER VARIABLES
      
      ALLOCATE(s2(nf))                           !MAKE s2 nf POINTS LONG
      
      n_half = int(float(n_ave)/2.)              !HALF OF n_ave POINTS TO SMOOTH
      
      DO I = 1, nf                               !SPECTRAL WHITEN I'TH POINT
       ave = 0.                                  !ZERO AVERAGE VALUE
       cnt = 0.                                  !ZERO AVERAGE COUNT
       DO J = -n_half,n_half                     !FOR EACH POINT TO AVERAGE OVER
        K = I + J                                !
	IF ((K >=1).AND.(K<=nf)) THEN            !ONLY IF POINT IS OKAY
	 ave = ave + abs(s1(I)*conjg(s1(I)))     !SUM ABSOLUTE VALUES
         cnt = cnt + 1.                          !ADD CNT
        END IF
       END DO
       IF (cnt > 0.) THEN                        !ONLY NORMALIZE IF ENOUGH POINTS
        s2(I) = s1(I)/(ave/cnt)                  !NORMALIZE BY AVERAGE
       ELSE                                      !
        s2(I) = 0.                               !OTHERWIZE ZERO
       END IF                                    !
      END DO                                     !
      
      s1(1:nf) = s2(1:nf)                        !COPY BACK TO ORIGINAL ARRAY
      
      
      RETURN
      END SUBROUTINE WHITEN_JL
      
      
      
      
      
      
      


      SUBROUTINE GET_SPEC_JL(npts,dt,w1,nf,df,s1)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE SHIFTS THE POINTS BY TOFF AND INTERPOLATES BETWEEN  |   C
C   |     PREVIOUS POINTS.                                               |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE USES THE FOLLOWING SUBROUTINES:                     |   C
C   |     NEXT_POWER_2    -   FINDS THE NEXT POWER OF TWO >= VALUE GIVEN |   C
C   |     GET_SPEC        -   DOES THE FAST FOURIER TRANSFORM            |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
C   |     CONTACT: jflawrence@stanford.edu                               |   C
C   |                                                                    |   C
C   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      IMPLICIT   NONE                            !ALLOW NO IMPLICIT VARIABLES
      REAL       w1(*)                           !INPUT DATA SERIES
      COMPLEX    s1(*)                           !SPECTRA OF SERIES
      INTEGER    npts,np2,nf                     !NUMBER OF DATA POINTS
      REAL       dt,df                           !TIME & FREQ SAMPLING INTERVALS
      REAL       pi                              !SET PI = 3.14....
      
      pi = atan(1.)*4.                           !SET PI = 3.14....
      CALL NEXT_POWER_2(npts,np2)                !FIND THE NEXT POWER OF 2
      
      IF (np2 < npts) THEN                       !PAD SERIES WITH ZEROS 
       w1(npts+1:np2) = 0.
      END IF
      CALL GET_SPEC(w1,np2,dt,s1,nf,df)          !GET SPECTRUM OF DATA SERIES 1
      
      RETURN
      END SUBROUTINE GET_SPEC_JL
      
      
      
      
      SUBROUTINE GFILTER_BP_E(nfreq,df,xs,gwidth,gfreq,xf,ndat,dt)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE APPLIES A GUASSIAN FILTER OF WIDTH, gwidth, TO A    |   C
C   |     SERIES, xs, OF LENGTH, nfreq, AND FREQUENCY INTERVAL df, AND   |   C
C   |     YIELDS A FILTERED SERIES xf. THE INPUT IS IN FREQUENCY DOMAIN  |   C
C   |     THE OUTPUT IS IN THE TIME DOMAIN. THIS VERSION OF THE SUBROUTIN|   C
C   |     RETURNS THE ENVELOPE OF THE FILTERED SERIES.                   |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE USES SEVERAL SUBROUTINES:                           |   C
C   |     GET_TS          -   DOES THE INVERSE FAST FOURIER TRANSFORM    |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
C   |     CONTACT: jflawrence@stanford.edu                               |   C
C   |                                                                    |   C
C   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      IMPLICIT     NONE                    !ALLOW NO IMPLICIT VARIABLES
      INTEGER      TSMX,TSMX2              !MAX # OF POINTS IN TIME SERIES
      PARAMETER(   TSMX=32768,TSMX2=16385) !SET MAX AS PARAMETER
      REAL         xf(*)                   !INPUT SERIES, FILTERED OUTPUT SERIES
      REAL         gwidth                  !GAUSSIAN FILTER WIDTH
      INTEGER      I,J,ndat,nfreq          !STEP, NUMBER OF POINTS IN TS & SPECT
      COMPLEX      xs(*),fs(TSMX2)         !COMPLEX SPECTRA OF x & y
      REAL         dt,df                   !TIME & FREQUENCY SAMPLING INTERVALS
      REAL         pi,dw,four_gw_sq,w      !FREQUENCY CONTENT & GAUSSIAN FACTORS
      REAL         gfreq,gfreqw,damp
      INTEGER      ndat1
      COMPLEX      fh(TSMX2)               !HILBERT TRANSFORM FREQUENCY
      REAL         xh(TSMX)                !HILBERT TRANSFORM TIME
      
      pi = atan(1.)*4.                     !SET PI = 3.14....
      dw = 2.*pi*df                        !ANGULAR FREQUENCY SAMPLING INTERVAL
      four_gw_sq = 4.*gwidth**2            !COMPUTE FACTOR AHEAD OF TIME
      gfreqw = gfreq*2.*pi                 !GAUSSIAN FILTER WIDTH
      
      DO I = 1, nfreq                      !FOR EACH FREQUENCY
       w     = float(I)*dw                 !CALC ANGULAR FREQUENCY
       damp  = -(w-gfreqw)**2/four_gw_sq   !CALC THE DAMPING FACTOR
       fs(I)=xs(I)*cmplx(exp(damp),0.)     !APPLY GAUSSIAN FILTER
C       IF (I < int(float(nfreq)/2.)) THEN  !CALCULATE THE HILBERT TRANSFORM
C        fh(I) = cmplx( -imag(fs(I)) ,  real(fs(I)) )
        fh(I) = fs(I)*exp(cmplx(0.,pi/2D0))
C       ELSE
C        fh(I) = cmplx(  imag(fs(I)) , -real(fs(I)) )
C       END IF
      
      END DO                               !

      CALL GET_TS(fs,nfreq,df,0,xf,ndat,dt)!GET SPECTRUM OF y
      CALL GET_TS(fh,nfreq,df,0,xh,ndat,dt)!GET SPECTRUM OF y
      
      DO I = 1, ndat                       !ENVELOPE IS MAGNITUDE OF NORM & HILBERT
       xf(I) = (xf(I)**2+xh(I)**2)**0.5
      END DO
      
      CALL NORM21R(xf,ndat)
      
      RETURN
      END SUBROUTINE GFILTER_BP_E          !END GFILTER_BP
      
      
      SUBROUTINE GFILTER_BP(nfreq,df,xs,gwidth,gfreq,xf,ndat,dt)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE APPLIES A GUASSIAN FILTER OF WIDTH, gwidth, TO A    |   C
C   |     SERIES, xs, OF LENGTH, nfreq, AND FREQUENCY INTERVAL df, AND   |   C
C   |     YIELDS A FILTERED SERIES xf. THE INPUT IS IN FREQUENCY DOMAIN  |   C
C   |     THE OUTPUT IS IN THE TIME DOMAIN. THIS VERSION OF THE SUBROUTIN|   C
C   |     RETURNS THE ENVELOPE OF THE FILTERED SERIES.                   |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE USES SEVERAL SUBROUTINES:                           |   C
C   |     GET_TS          -   DOES THE INVERSE FAST FOURIER TRANSFORM    |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
C   |     CONTACT: jflawrence@stanford.edu                               |   C
C   |                                                                    |   C
C   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      IMPLICIT     NONE                    !ALLOW NO IMPLICIT VARIABLES
      INTEGER      TSMX,TSMX2              !MAX # OF POINTS IN TIME SERIES
      PARAMETER(   TSMX=32768,TSMX2=16385) !SET MAX AS PARAMETER
      REAL         xf(*)                   !INPUT SERIES, FILTERED OUTPUT SERIES
      REAL         gwidth                  !GAUSSIAN FILTER WIDTH
      INTEGER      I,J,ndat,nfreq          !STEP, NUMBER OF POINTS IN TS & SPECT
      COMPLEX      xs(*),fs(TSMX2)         !COMPLEX SPECTRA OF x & y
      REAL         dt,df                   !TIME & FREQUENCY SAMPLING INTERVALS
      REAL         pi,dw,four_gw_sq,w      !FREQUENCY CONTENT & GAUSSIAN FACTORS
      REAL         gfreq,gfreqw,damp
      INTEGER      ndat1
      
      pi = atan(1.)*4.                     !SET PI = 3.14....
      dw = 2.*pi*df                        !ANGULAR FREQUENCY SAMPLING INTERVAL
      four_gw_sq = 4.*gwidth**2            !COMPUTE FACTOR AHEAD OF TIME
      gfreqw = gfreq*2.*pi
      
      DO I = 1, nfreq                      !FOR EACH FREQUENCY STEP
       w     = float(I)*dw                 !CALC ANGULAR FREQUENCY
       damp  = -(w-gfreqw)**2/four_gw_sq   !CALC THE DAMPING FACTOR
       fs(I)=xs(I)*cmplx(exp(damp),0.)     !APPLY GAUSSIAN FILTER
       fs(I)=fs(I)*exp(cmplx(0.,-w/(gfreq*4.)))  !APPLY GAUSSIAN FILTER
      END DO

      CALL GET_TS(fs,nfreq,df,0,xf,ndat,dt)!GET SPECTRUM OF y
      
      DO I = 1, ndat
       xf(I) = -xf(I)
      END DO
      CALL NORM21R(xf,ndat)
      
      RETURN
      END SUBROUTINE GFILTER_BP            !END GFILTER_BP
