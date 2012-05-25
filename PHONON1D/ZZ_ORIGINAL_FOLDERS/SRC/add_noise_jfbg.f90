program add_noise_jfbg

      implicit     NONE
      integer, parameter :: SMX= 8192,DMX=1000         !MAX SIG & DNST LENGTH
      real         tt(SMX),d(DMX)                      !TIME & DNSTANCE
      real         s_in(DMX,SMX),s_out(DMX,SMX)        !SIGNAL IN & OUT
      character*80 ifile,ofile                         !INPUT & OUTPUT FILES
      integer      status                              !I/O ERROR 0=OK
      integer      nt,nd                               !# OF TIME & DNST STEPS
      real         s1(SMX),s2(SMX)                     !SCRATCH
      real         dt                                  !TIME INTERVAL
      real         pi,wt                               !PI=3.14..., & COSINE WEIGHT
      real         tdelay                              !SHIFT FOR DECONVOLVED SERIES
      real         t1
      real         tf21(SMX)                           !TRANSFER FUNCTION
      integer      npts,dt2                            !NUMBER OF DATA POINTS
      real         blnk                                !DUMMY VARIABLE
      integer      I,J                                 !INDEX VARIABLES
      real         ran0,r0                             !Randomizing function & Number
      integer      IDUM                                !Randomizing Kernel          
      real         snr,bit_lim                         !Sig/noise ratio & bit limit
      real         maxf
      real         dx
      integer      id
      real         cnt
      IDUM = 112                                       !Randomizing Kernel
      
      write(6,'(A)') 'ENTER INPUT FILE NAME (E.G. trace.out):'
      read (5,'(A)')  ifile
      open (1,FILE=ifile,STATUS='OLD')                 !open INPUT FILE
      write(6,'(A)') 'ENTER OUTPUT FILE NAME (E.G. decon.out):'
      read (5,'(A)')  ofile
      open (2,FILE=ofile,STATUS='UNKNOWN')             !open OUTPUT FILE
      
      write(6,'(a)') 'Enter signal-to-noise ratio:'
      read (5,    *)  snr
      
      write(6,'(a)') 'Enter bit-limit (e.g., 10):'
      read (5,    *)  bit_lim
      
      read (1,*) nt,nd                                 !read IN # TIME & DNSTS
      write(2,*) nt,nd                                 !COPY TO OUTPUT
      read (1,FMT=887) blnk,(d(J),J=1,nd)              !read IN DISTANCES
      write(2,FMT=888) blnk,(d(J),J=1,nd)              !read IN DISTANCES
      dx = (d(nd)-d(1))/(nd-1)
      do I = 1, nt                                     !FOR EACH TIME & DNSTANCE
       read(1,FMT=888,IOSTAT=status)tt(I),(s_in(J,I),J=1,nd)!read IN EACH ROW (TIME)
       if (status /= 0) EXIT                           !IF read ERROR, THEN STOP
!       write(6,*) I,tt(I)
      end do                                           !
      
      dt = tt(2)-tt(1)                                 !TIME INTERVAL
      pi = atan(1.)*4.                                 !SET pi = 3.14...
      tdelay = 0.!200.                                   !
      id = int(11./dx)+1
      maxf = -99999.
      do i = 1, nd
       do j = 1, nt
        if (abs(s_in(i,J)) > 99.) s_in(i,j)=s_in(i,j)/100.
       end do
      end do
      
      do J = 1, nt                                    !FOR EACH POINT NEAR TRIGGER
        if (maxf < abs(s_in(id,j))) maxf = abs(s_in(id,j))
!	write(6,*) 'DING:',id,j,s_in(id,j),maxf
      end do
      
      bit_lim = maxf/bit_lim
      do I = 1, nd
       s1(1:nt) = s_in(I,1:nt)                         !COPY 2D DATA TO 1D VECTOR 
       cnt = 1.
       if (i > 1) then
        s1(1:nt) = s1(1:nt) + s_in(i-1,1:nt)
        cnt = cnt + 1.
       end if
!       if (i > 2) then
!        s1(1:nt) = s1(1:nt) + s_in(i-2,1:nt)
!        cnt = cnt + 1.
!       end if
       if (i < nd) then
        s1(1:nt) = s1(1:nt) + s_in(i+1,1:nt)
	cnt = cnt + 1.
       end if
!       if (i < nd-1) then
!        s1(1:nt) = s1(1:nt) + s_in(i+2,1:nt)
!	cnt = cnt + 1.
!       end if
       s1(1:nt) = s1(1:nt) / cnt
!       call norm21r(s1(1:nt),nt)
!       maxf = 0.
       do j = 1, nt                                    !FOR EACH POINT NEAR TRIGGER
        s1(j) = s1(j) / maxf
!	write(6,*) 'HI:MAXF:',maxf,id,j,s_in(id,j)
       end do
       maxf = 1.
       do J = 1, nt                                    !FOR EACH POINT NEAR TRIGGER
         r0    = ran0(IDUM)                            !WEIGHT THE SERIES
         s2(J) = (r0-0.5)/snr*1.1!*maxf
       end do
       s2(1)   = 1.00
       s2(2)   = 0.50
       s2(5)   = 0.25
       s2(10)  = 0.90
       s2(20)  = 0.45
       s2(30)  = -.40
       s2(40)  = 0.50
       s2(50)  = -0.6
       s2(60)  = 0.35
       s2(70)  = -.30
       s2(80)  = 0.25
       s2(90)  = 0.35
       s2(100) = 0.30
       s2(101) = 0.50
       s2(102) = 0.80
       s2(105) = 0.50
       s2(110) = 0.40
       s2(120) = 0.5
       s2(150) = -0.2
       s2(200) = -0.4
       npts = nt                                       !COPY # POINTS JUST IN CASE
       dt2  = dt                                       !COPY TIME INBERVAL J.I.C.
       call convolve(npts,dt,tdelay,s2,s1,tf21)        !DECONVOLVE GET TRANSFER FUNCT
!       tf21(1:nt) = s1(1:nt) + s2(1:nt)
       write(6,*) bit_lim,tdelay
       call bit_limit(tf21(1:nt),nt,bit_lim)
       s_out(I,1:nt) = tf21(1:nt)/100.                      !COPY 1D DECON TO OUTPUT
      end do
      
      do I = 1, nt
       write(2,FMT=888) tt(I),(s_out(J,I),J=1,nd)       !write OUT EACH ROW (TIME)
      end do
      
      close(1)
      close(2)
      
887   format(F10.6,1X,500(F10.6,1X))                       !FORMATTING STATEMENT
888   format(F10.4,1X,500(F10.6,1X))                       !FORMATTING STATEMENT
      stop
end program add_noise


subroutine NEXT_POWER_2(npts,np)
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
!   |THIS SERIES DETERMINES THE POWER OF TWO THAT IS EQUAL OR JUST       |   C
!   |     GREATER THAN THE NUMBER OF POINTS SUPPLIED:                    |   C
!   |                                                                    |   C
!   |THIS subroutine WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
!   |     CONTACT: jlawrence@ucsd.edu                                    |   C
!   |                                                                    |   C
!   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
      implicit      none
      integer    :: npts,np            !NUMBER OF POINTS, & 2^np >= npts

      np = 0                           !ASSUME SMALL AND BUILD UP FROM THERE
      do while (2**np < npts)         !
       np = np + 1
      end do
      np = 2**np                       !
      
      return
end subroutine NEXT_POWER_2            !END NEXT_POWER_2



      subroutine NORM21R(series,ndat)
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
!   |THIS subroutine NORMALIZES ANY SERIES, series, TO SOME VALUE, normf.|   C
!   |                                                                    |   C
!   |THIS subroutine WAS WRITTEN BY JESSE F. LAWRENCE.                   |   C
!   |     CONTACT: jlawrence@ucsd.edu                                    |   C
!   |                                                                    |   C
!   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      |   C
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
!   | SET PARAMETERS AND VARIABLES:                                      |   C
      implicit         none
      real             series(*),maxf         !INPUT & OUTPUT SERIES
      integer          ndat,I                 !NUMBER OF POINTS
      maxf = series(1)                        !MAX IS FIRST POINT
      do I = 2, ndat                          !
       if (maxf < series(I)) maxf = series(I)!REPLACE MAX if BETTER
      end do                                  !
      if (maxf == 0.) THEN                    !IF MAX IS ZERO, THEN SKIP
       write(6,*) 'SKIPPING NORMALIZATION, MAX = 0.',maxf
       return                                 !
      end if                                  !
      do I = 1, ndat                          !DIVIDE SERIES BY MAXIMUM
       series(I) = series(I)/maxf             !
      end do                                  !
      return                                  !
      end subroutine NORM21R                  !END NORM21R
      
real function ran0(idum)
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
!   | --- --------- --------- --------- --------- --------- --------- -- |   C
      integer            :: idum
      real               :: ran0
      integer, parameter :: IA=16807,IM=2147483647,IQ=127773,IR=2836
      real               :: AM
      integer, parameter :: MASK=123459876
      integer            :: k
      idum=ieor(idum,MASK)
      AM = 1./float(IM)
      k       = idum/IQ
      idum    =IA*(idum-k*IQ)-IR*k
      if (idum < 0) idum=idum+IM
      ran0    = AM*idum
      idum    = ieor(idum,MASK)
      return
end function ran0
