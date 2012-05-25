      PROGRAM xyz2sac
      integer      XMX,TMX
      PARAMETER(   XMX = 361,TMX = 6001)!MAX # OF DISTANCE & TIME STEPS 
      REAL         wf(XMX,TMX)          !2D WAVEFIELD
      REAL         x(XMX),t(TMX),dt     !DISTANCES & TIMES
      INTEGER      nt,nx                !NUMBER OF TIME & DISTANCE STEPS
      CHARACTER*80 ifile,ofile          !INPUT & OUTPUT FILE NAMES
      REAL         delta                !DISTANCE
      INTEGER      idelt1,idelt2        !INTEGER DISTANCE & DECIMAL
      REAL         w(TMX)               !1D WAVEFORM
      LOGICAL      lex                  !TRUE IF SAC DIRECTORY EXISTS
      
      WRITE(6,'(A)') 'ENTER INPUT FILE NAME'
      READ (5,'(A)')  ifile
      OPEN(22,FILE=ifile,STATUS='OLD')
      READ(22,*) nt,nx
      READ(22,FMT=888) delta,(x(J),J=1,nx)
      DO I = 1, nt-1
       READ(22,FMT=888) t(I),(wf(J,I),J=1,nx)
      END DO
      CLOSE(22)
      
      INQUIRE (FILE='SAC',EXIST=lex)
      IF (.NOT.(LEX)) THEN
       CALL SYSTEM('mkdir SAC')
      END IF
      
      DO I = 1, nx
       delta = x(I)
       idelt1 = int(delta)
       idelt2 = int(10.*(delta-float(idelt1)))
       WRITE(ofile,FMT=898) idelt1,idelt2
       dt = (t(nx)-t(1))/float(nt-1)
       DO J = 1, nt
        w(J) = wf(I,J)
       END DO
       CALL rsac_syn(ofile,w,nt,0.,dt,delta)
      END DO
      
888   FORMAT(F10.2,1X,401(F10.6,1X))
898   FORMAT('SAC/D_',I3.3,'.',I1.1,'.sac')
      STOP
      END
      
      
      SUBROUTINE rsac_syn(sfile,y,npts,tbeg,dt,delta)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      REAL           y(*),yd(1000000),tbeg,dt,delta
      INTEGER        npts
      REAL           qlon,qlat,qdep,slon,slat,sel,saz,qaz
      REAL           tend,dist
      INTEGER        qyr,qdy,qhr,qmn,qsc,qms
      CHARACTER*4    sname,ntwk
      CHARACTER*(*)  sfile
      logical        lex
      character*128  sys_cmd
      
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
       
       qyr = 2007
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
       sname = 'synt'
       ntwk  = 'NA'
       CALL SETKHV('kstnm' ,sname  ,nerr)     !SET HEADER STATION NAME
       CALL SETKHV('knetwk',ntwk   ,nerr)     !SET HEADER STATION NETWORK
       CALL SETKHV('kcmpnm','SYT'  ,nerr)     !SET HEADER COMPONENT NAME
       write(6,*) 'hi21:'
       DO K = 1, npts                        !MAKE SERIES OF TIME STEPS
        yd(K) = tbeg+float(K-1)*dt
       END DO
       write(6,*) 'hi25:',sfile,yd(1),y(1),npts,nerr
       
       inquire(file=trim(sfile),EXIST=lex)          !CHECK IF THE FILE EXISTS
       if (lex) then                          !IF FILE EXISTS, DELETE IT
        sys_cmd = 'rm '//trim(sfile)
	call system(sys_cmd)
       end if
       CALL WSAC0(sfile,yd,y,nerr)            !OUTPUT DATA FILE
       write(6,*) 'hi27:'
      RETURN
      END
