      PROGRAM temp1
      INTEGER     NMX
      PARAMETER(  NMX=10000)
      REAL        d(NMX),z(NMX),vp(NMX),vs(NMX),rho(NMX)
      REAL        qp(NMX),qs(NMX)
      INTEGER     I,ndat
      
      OPEN(1,FILE='PREM.txt1')
      OPEN(2,FILE='prem.new')
      
      DO I = 1, 100000
       READ(1,*,END=111) d(I),z(I),vp(I),vs(I),rho(I),qp(I),qs(I)
       ndat = I
111   END DO
      
      WRITE(6,*) ndat
      
      DO I = ndat, 1, -1
       WRITE(2,FMT=222) z(I),d(I),vp(I),vs(I),rho(I),qp(I),qs(I)
      END DO
      
222   FORMAT(5(2X,F10.5),2(2X,F10.3))
      CLOSE(1)
      CLOSE(2)
      
      
      STOP
      END
