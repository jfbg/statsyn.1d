      PROGRAM source
      REAL    pi,P0,mt(10001),t0,dti
      REAL    ang1,a(361)
      INTEGER I,J,K,ntr,nts1
      REAL    pdf(361),cdf(361)
      
      OPEN(65,FILE='Source_Amp_time.xy')
      OPEN(66,FILE='Source_Amp_xy.xy')
      OPEN(67,FILE='Source_Amp_angle.xy')
      OPEN(68,FILE='Source_PDF_angle.xy')
      OPEN(69,FILE='Source_CDF_angle.xy')
      
      
      WRITE(6,*) 'CALCULATING SOURCE:'        !CALCULATING SOURCE
      pi = atan(1.)*4.                        !
      P0 = 5.                                !DOMINANT PERIOD
      nts = nint(P0*4./dti)+1                 !# OF POINTS IN SOURCE SERIES
      IF (nts.LT.31) nts = 31
      nts1 = 1000
      DO I = 1, nts1
       mt(I) = 0.
      END DO
      DO I = 1, nts                           !SOURCE-TIME FUNCTION
       t0 = dti*float(I-1)-P0
       mt(I) = -4.*pi**2.*P0**(-2.)*(t0-P0/2.)
     &          *exp(-2.*pi**2.*(t0/P0-0.5)**2.)
       WRITE(65,*) t0,mt(I)
      END DO
      
      angst = 2*pi
      ntr = 361
      pow = 0.
      DO I = 1, ntr
       ang1 = 2.*pi*float(I-1)/float(ntr-1)
       a(I)    = sin(ang1*2.)             !SOURCE AMPLITUDE
       pow = pow + abs(a(I))
      END DO
      
      tot = 0.
      DO I = 1, ntr
       pdf(I) = abs(a(I))/pow
       tot = tot + pdf(I)
       cdf(I) = tot
       ang1 = 2.*pi*float(I-1)/float(ntr-1)
       x = cos(ang1)*a(I)
       y = sin(ang1)*a(I)
       WRITE(66,*) x,y
       ang1 = ang1*180./pi
       WRITE(67,*) ang1,a(I)
       WRITE(68,*) ang1,pdf(I)
       WRITE(69,*) ang1,cdf(I)
      END DO
      
      
      STOP
      END
