      PROGRAM xyz2sac
      implicit     none
      INTEGER      XMX,TMX
      PARAMETER(   XMX = 361,TMX = 16384)!MAX # OF DISTANCE & TIME STEPS 
      REAL         wf(XMX,TMX)          !2D WAVEFIELD
      REAL         x(XMX),t(TMX),dt     !DISTANCES & TIMES
      INTEGER      nt,nx                !NUMBER OF TIME & DISTANCE STEPS
      CHARACTER*80 ifile,ofile          !INPUT & OUTPUT FILE NAMES
      REAL         delta                !DISTANCE
      INTEGER      idelt1,idelt2        !INTEGER DISTANCE & DECIMAL
      REAL         w(TMX)               !1D WAVEFORM
      LOGICAL      lex                  !TRUE IF SAC DIRECTORY EXISTS
      INTEGER      I,J,status
      
      
      WRITE(6,'(A)') 'ENTER INPUT FILE NAME'
      READ (5,'(A)')  ifile
      OPEN(22,FILE=ifile,STATUS='OLD')
      READ(22,*) nt,nx
      READ(22,FMT=888) delta,(x(J),J=1,nx)
      DO I = 1, nt-2
       READ(22,FMT=888,IOSTAT=status) t(I),(wf(J,I),J=1,nx)
       if (status /= 0) exit
      END DO
      CLOSE(22)
      nt = nt-2
      
      
      INQUIRE (FILE='SAC',EXIST=lex)
      IF (.NOT.(LEX)) THEN
       CALL SYSTEM('mkdir SAC')
      END IF
      
      write(6,*) 'hi'
      
      DO I = 1, nx
       delta = x(I)
       idelt1 = int(delta)
       idelt2 = int(10.*(delta-float(idelt1)))
       WRITE(ofile,FMT=898) idelt1,idelt2
       write(6,*) I,x(i),idelt1,idelt2,ofile
       dt = (t(nt)-t(1))/float(nt-1)
       write(6,*) dt,nt
       DO J = 1, nt
        w(J) = wf(I,J)
       END DO
       write(6,*) 'hi1:',delta
       CALL rsac_syn(ofile,nt,w(1:nt),t(1:nt),I,0.,dt,delta)
       write(6,*) 'hi2:'
      END DO
      
888   FORMAT(F10.2,1X,401(F10.6,1X))
898   FORMAT('./SAC/D_',I3.3,'.',I1.1,'.sac')
      STOP
      END PROGRAM xyz2sac
     
      
      SUBROUTINE rsac_syn(sfile,npts,y,yd,is,tbeg,dt,delta)
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
      implicit       none
      integer        npts
      REAL           y(*),yd(*),tbeg,dt,delta
      INTEGER        nerr
      REAL           qlon,qlat,qdep,slon,slat,sel,saz,qaz
      REAL           tend,dist
      INTEGER        qyr,qdy,qhr,qmn,qsc,qms
      CHARACTER*4    sname,ntwk
      CHARACTER*(*)  sfile
      INTEGER        K,is
      
      
      WRITE(sname,FMT=1010) is
      write(6,*) 'HIR1:',npts,is,sname,tbeg,dt,delta
1010  FORMAT('S',I3.3)
      
      CALL NEWHDR()                          !CREATE A NEW HEADER
      CALL SETNHV('npts'  ,npts   ,nerr)     !SET HEADER NUMBER OF POINTS
      CALL SETLHV('leven' ,.TRUE. ,nerr)     !SET HEADER EVEN SPACING
      CALL SETFHV('B'     ,tbeg   ,nerr)     !SET START FROM EARTHQUAKE
      CALL SETFHV('delta' ,dt     ,nerr)     !SET SAMPLING INTERVAL
      tend = tbeg+float(npts)*dt
      CALL SETFHV('E'     ,tend   ,nerr)     !SET END TIME FROM BEGINNING
      CALL SETIHV('iftype','ITIME',nerr)     !SET HEADER TO TIME SERIES
      CALL SETIHV('idep  ','IDISP',nerr)     !SET HEADER TO TIME SERIES
      CALL SETIHV('iztype','IO'   ,nerr)     !SET HEADER TIME TYPE: EQ
      
      qlat = 90.
      qlon = 0
      qdep = 0.
      slat = qlat-delta
      slon = 0.
      sel  = 0.
      qaz  = 180.
      saz  = 0.
       
      CALL SETFHV('stla'  ,slat   ,nerr)     !SET HEADER STATION LATITUDE
      CALL SETFHV('stlo'  ,slon   ,nerr)     !SET HEADER STATION LONGITUDE
      CALL SETFHV('stel'  ,sel    ,nerr)     !SET HEADER STATION ELEVATION
      CALL SETFHV('evla'  ,qlat   ,nerr)     !SET HEADER EQ LATITUDE
      CALL SETFHV('evlo'  ,qlon   ,nerr)     !SET HEADER EQ LONGITUDE
      CALL SETFHV('evdp'  ,qdep   ,nerr)     !SET HEADER EQ DEPTH
       
      qyr = 2008
      qdy = 1
      qhr = 1
      qmn = 1
      qsc = 1
      qms = 1
       
       
      CALL SETNHV('nzyear',qyr    ,nerr)     !SET HEADER START YEAR
      CALL SETNHV('nzjday',qdy    ,nerr)     !SET HEADER START JULIAN DAY
      CALL SETNHV('nzhour',qhr    ,nerr)     !SET HEADER START HOUR
      CALL SETNHV('nzmin' ,qmn    ,nerr)     !SET HEADER START MINUTE
      CALL SETNHV('nzsec' ,qsc    ,nerr)     !SET HEADER START SECOND
      CALL SETNHV('nzmsec',qms    ,nerr)     !SET HEADER START MILISECOND
      CALL SETFHV('gcarc' ,delta  ,nerr)     !SET HEADER EQ-STATN DIST (DEGREES)
      dist = delta/111.19
      CALL SETFHV('dist ' ,dist   ,nerr)     !SET HEADER EQ-STATN DIST (KM)
      CALL SETFHV('baz'   ,saz    ,nerr)     !SET HEADER BACKAZIMUTH STATN-EQ
      CALL SETFHV('az'    ,qaz    ,nerr)     !SET HEADER AZIMUTH     EQ-STATN
      ntwk  = 'NA'
      CALL SETKHV('kstnm' ,sname  ,nerr)     !SET HEADER STATION NAME
      CALL SETKHV('knetwk',ntwk   ,nerr)     !SET HEADER STATION NETWORK
      CALL SETKHV('kcmpnm','SYT'  ,nerr)     !SET HEADER COMPONENT NAME
      write(6,*) 'HIR20:',nerr,trim(sfile),npts
      do k = 1, npts
       write(6,*) k,y(k),npts
      end do
      CALL WSAC0(trim(sfile),yd(1:npts),y(1:npts),nerr)!OUTPUT DATA FILE
      write(6,*) 'HIR21:',nerr
      RETURN
      END SUBROUTINE rsac_syn
