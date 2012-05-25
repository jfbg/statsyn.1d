      PROGRAM interp_mod
      IMPLICIT      NONE
      INTEGER       I,J,nl,nlyr
      INTEGER       NMX
      PARAMETER(    NMX = 10000)
      REAL          z(NMX),r(NMX),vp(NMX),vs(NMX),rh(NMX)
      CHARACTER*80  ifile
      INTEGER       status
      REAL          zz,rr,pp,ss,hh
      REAL          scale_length
      
      WRITE(6,'(A)') 'ENTER INPUT VELOCITY MODEL NAME:'
      READ (5,'(A)')  ifile
      
      
      
      OPEN(1,FILE=ifile,STATUS='OLD')   !OPEN INPUT FILE
      DO I = 1, NMX                     !READ IN VELOCITY MODEL
       READ(1,*,IOSTAT=status) z(I),r(I),vp(I),vs(I),rh(I)
       IF (status /= 0) EXIT            !STOP IF YOU REACH THE END
       nlyr = I
999   END DO
			WRITE(6,*) 'nlyr = ' nlyr
      CLOSE(1)                          !CLOSE INPUT VELOCIT MODEL FILE
      
      
      
      WRITE(6,'(A)') 'ENTER OUTPUT VELOCITY MODEL NAME:'
      READ (5,'(A)')  ifile
      WRITE(6,'(A)') 'ENTER SCAL LENGTH:'
      READ (5,   *)   scale_length
      
      OPEN (2,FILE=ifile,STATUS='UNKNOWN')!OPEN OUTPUT FILE
      
      DO I = 1, nlyr-1                 !FOR EACH LAYER:
       IF (z(I).LT.z(I+1)) THEN        !CHECK VELOCITY LAYER (NOT INTERFACE)
        nl = int( (z(I+1)-z(I))/scale_length)    !NUMBER OF SUB-LAYERS TO INTERPOLATE
        WRITE(2,FMT=1010) z(I),r(I),vp(I),vs(I),rh(I)!OUTPUT TOP VELOCITY
        DO J = 1, nl-1                 !INTERPOLATE AT EACH STEP
         zz = z(I) + float(J)*(z(I+1)-z(I))/float(nl)
         rr = r(I) + float(J)*(r(I+1)-r(I))/float(nl)
         pp = vp(I) + float(J)*(vp(I+1)-vp(I))/float(nl)
         ss = vs(I) + float(J)*(vs(I+1)-vs(I))/float(nl)
         hh = rh(I) + float(J)*(rh(I+1)-rh(I))/float(nl)
         WRITE(2,FMT=1010) zz,rr,pp,ss,hh !OUTPUT INTERPOLATION
        END DO                         !
       ELSE                            !IF INTERFACE JUST OUTPUT THE INPUT
        WRITE(2,FMT=1010) z(I),r(I),vp(I),vs(I),rh(I)
       END IF
      END DO
      I = nlyr
      WRITE(2,FMT=1010) z(I),r(I),vp(I),vs(I),rh(I)
      
1010  FORMAT(5(2X,F10.5))             !OUTPUT FORMAT
      
      CLOSE(2)
      
      
      STOP
      END PROGRAM interp_mod
