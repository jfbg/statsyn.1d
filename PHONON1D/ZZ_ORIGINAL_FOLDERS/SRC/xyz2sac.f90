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
      
      write(6,'(A)') 'ENTER INPUT FILE NAME'
      read (5,'(A)')  ifile
      open(22,file=ifile,status='OLD')
      read(22,*) nt,nx
      read(22,FMT=887) delta,(x(J),J=1,nx)
      do I = 1, nt
       read(22,FMT=888) t(I),(wf(J,I),J=1,nx)
      end do
      close(22)
      
      inquire (file='SAC',EXIST=lex)
      if (.NOT.(LEX)) then
       call system('mkdir SAC')
      end IF
      
      do I = 1, nx
       delta = x(I)
       idelt1 = int(delta)
       idelt2 = int(10.*(delta-float(idelt1)))
       write(ofile,FMT=898) idelt1,idelt2
       dt = (t(2)-t(1))!/float(nt-1)
       do J = 1, nt
        w(J) = wf(I,J)
       end do
       call rsac_syn(ofile,w,nt,0.,dt,delta)
      end do
      
887   format(F10.2,1X,401(F10.6,1X))
888   format(F10.2,1X,401(F10.6,1X))
898   format('SAC/D_',I3.3,'.',I1.1,'.sac')
      stop
end program xyz2sac
      
      
subroutine rsac_syn(sfile,y,npts,tbeg,dt,delta)
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
      implicit       none
      real           y(*),yd(1000000),tbeg,dt,delta
      integer        npts
      real           qlon,qlat,qdep,slon,slat,sel,saz,qaz
      real           tend,dist
      integer        qyr,qdy,qhr,qmn,qsc,qms
      character*4    sname,ntwk
      character*(*)  sfile
      logical        lex
      character*128  sys_cmd
      integer        K,nerr
      
       call NEWHDR()                          !CREATE A NEW HEADER
       call SETNHV('npts'  ,npts   ,nerr)     !SET HEADER NUMBER OF POINTS
       call SETLHV('leven' ,.TRUE. ,nerr)     !SET HEADER EVEN SPACING
       call SETFHV('B'     ,tbeg   ,nerr)     !SET START FROM EARTHQUAKE
       call SETFHV('delta' ,dt     ,nerr)     !SET SAMPLING INTERVAL
       tend = tbeg+float(npts)*dt
       call SETFHV('E'     ,tend   ,nerr)     !SET end TIME FROM BEGINNING
       call SETIHV('iftype','ITIME',nerr)     !SET HEADER TO TIME SERIES
       call SETIHV('idep  ','IDISP',nerr)     !SET HEADER TO TIME SERIES
       call SETIHV('iztype','IO'   ,nerr)     !SET HEADER TIME TYPE: EQ
       qlat = 90.
       qlon = 0
       qdep = 0.
       slat = qlat-delta
       slon = 0.
       sel  = 0.
       qaz  = 180.
       saz  = 0.
       call SETFHV('stla'  ,slat   ,nerr)     !SET HEADER STATION LATITUDE
       call SETFHV('stlo'  ,slon   ,nerr)     !SET HEADER STATION LONGITUDE
       call SETFHV('stel'  ,sel    ,nerr)     !SET HEADER STATION ELEVATION
       call SETFHV('evla'  ,qlat   ,nerr)     !SET HEADER EQ LATITUDE
       call SETFHV('evlo'  ,qlon   ,nerr)     !SET HEADER EQ LONGITUDE
       call SETFHV('evdp'  ,qdep   ,nerr)     !SET HEADER EQ DEPTH
       
       qyr = 2007
       qdy = 1
       qhr = 1
       qmn = 1
       qsc = 1
       qms = 1
       
       
       call SETNHV('nzyear',qyr    ,nerr)     !SET HEADER START YEAR
       call SETNHV('nzjday',qdy    ,nerr)     !SET HEADER START JULIAN DAY
       call SETNHV('nzhour',qhr    ,nerr)     !SET HEADER START HOUR
       call SETNHV('nzmin' ,qmn    ,nerr)     !SET HEADER START MINUTE
       call SETNHV('nzsec' ,qsc    ,nerr)     !SET HEADER START SECOND
       call SETNHV('nzmsec',qms    ,nerr)     !SET HEADER START MILISECOND
       call SETFHV('gcarc' ,delta  ,nerr)     !SET HEADER EQ-STATN DIST (DEGREES)
       dist = delta/111.19
       call SETFHV('dist ' ,dist   ,nerr)     !SET HEADER EQ-STATN DIST (KM)
       call SETFHV('baz'   ,saz    ,nerr)     !SET HEADER BACKAZIMUTH STATN-EQ
       call SETFHV('az'    ,qaz    ,nerr)     !SET HEADER AZIMUTH     EQ-STATN
       sname = 'synt'
       ntwk  = 'NA'
       call SETKHV('kstnm' ,sname  ,nerr)     !SET HEADER STATION NAME
       call SETKHV('knetwk',ntwk   ,nerr)     !SET HEADER STATION NETWORK
       call SETKHV('kcmpnm','SYT'  ,nerr)     !SET HEADER COMPONENT NAME
       write(6,*) 'hi21:'
       do K = 1, npts                        !MAKE SERIES OF TIME STEPS
        yd(K) = tbeg+float(K-1)*dt
       end do
       
       write(6,*) 'hi22:'
       inquire(file=sfile,EXIST=lex)          !CHECK if THE FILE EXISTS
       if (lex) then                          !IF FILE EXISTS, DELETE IT
        sys_cmd = 'rm '//sfile
	call system(sys_cmd)
       end if
       write(6,*) 'hi40:',npts
       call WSAC0(sfile,yd,y,nerr)            !OUTPUT DATA FILE
       write(6,*) 'hi50:'
      return
end subroutine rsac_syn
