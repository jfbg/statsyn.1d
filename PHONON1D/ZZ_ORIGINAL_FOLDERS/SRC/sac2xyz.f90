      PROGRAM xyz2sac
      implicit     none
      integer      XMX,TMX
      parameter(   XMX = 361,TMX = 144001)!MAX # OF DISTANCE & TIME STEPS 
      real         wf(XMX,TMX)          !2D WAVEFIELD
      real         x(XMX),t(TMX),dt     !DISTANCES & TIMES
      integer      nt,nx                !NUMBER OF TIME & DISTANCE STEPS
      character*80 ifile,ofile          !INPUT & OUTPUT FILE NAMES
      real         delta                !DISTANCE
      integer      idelt1,idelt2        !integer DISTANCE & DECIMAL
      real         w(TMX)               !1D WAVEFORM
      logical      lex                  !TRUE if SAC DIRECTORY EXISTS
      integer      I,J
      integer     :: status
      character (len=80) :: files(XMX)
      integer     :: nerr
      real        :: b
      INTEGER     :: TMX2
      
      integer     :: seed,nclock,k
      
      TMX2 = TMX
      write(6,*) 'Enter list of input files'
      read (5,'(a)')  ifile
      
      open(1,file=ifile)
      
      do i = 1, 1000
       read (1,*,IOSTAT=status) files(i)
       if (status/=0) exit
       write(6,*) files(i)
       nx = i
      end do
      close(1)
      
      do i = 1, nx
       call rsac1(files(i),w,nt,b,dt,TMX2,nerr)
       wf(i,1:nt) = w(1:nt)
       write(6,*) nx,nt,files(i)
!       call getfhv("slat",x(i),nerr)
!       x(i) = 90.-x(i)
        x(i) = 2.*(i-1)
      end do
      do i = 1, nt
       t(i) = dt*(i-1)
      end do
      
      do i = 1, nx
       CALL SYSTEM_CLOCK(COUNT=nclock)
       seed = (nclock)
       CALL srand(seed)
       do j = 1, nt
        wf(i,j) = (float(nint(wf(i,j)*5000+(rand()-0.5)*1.5)))/5000.
       end do
      end do
      
      
      write(6,'(A)') 'ENTER OUTPUT FILE NAME'
      read (5,'(A)')  ifile
      open(22,file=ifile)
      write(22,*) nt,nx
      write(22,FMT=887) 999.99,(x(J),J=1,nx)
      do I = 1, nt
       write(22,FMT=888) t(I),(wf(J,I),J=1,nx)
      end do
      close(22)
      
887   format(F10.2,1X,401(F10.6,1X))
888   format(F10.2,1X,401(F10.6,1X))
898   format('SAC/D_',I3.3,'.',I1.1,'.sac')
      stop
end program xyz2sac
      
