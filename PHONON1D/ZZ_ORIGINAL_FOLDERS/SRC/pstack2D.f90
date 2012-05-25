program PSSTACK
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
      integer,PARAMETER  :: NMAX = 361,SMAX = 144001,MMAX = NMAX*SMAX*3
      character*80  ifile,outfile,pfile(100),pnm,atemp
      integer       ndat,ndmax,icol,CNT,CNT2
      real          temp,del(NMAX),time(SMAX)
      integer*2     ibuf(MMAX)
      real          amin,amax
      real          tmin,tmax,dmin,dmax
      real          ttic1,ttic2,dtic1,dtic2
      integer       plotyn,iplot,idmin,idmax,smoothyn
      real          qdep,delt
      real          val2(MMAX),val2D(SMAX,NMAX)
      real          del2
      integer       idel

      write(6,*) 'ENTER THE FILE NAME OF INPUT STACK FILE:'
      read (5,'(A)')     ifile
      write(6,*) 'ENTER SATURATION AMPLITUDES (MIN,MAX):'
      read (5,*)     amin,amax
      write(6,*) 'ENTER THE OUTPUT FILE NAME:'
      read (5,'(A)')     outfile
      write(6,*) 'PLOT TRAVEL TIMES (0)NO (1)YES?:'
      read (5,*)     plotyn
      write(6,*) 'REF PHASE: 0)NONE 1=P 2=S 3=PP 4=SS: 32=PKPDF'
      read (5,*)     IP
      write(6,*) 'ENTER THE EARTHQUAKE DEPTH (IN KM):'
      read (5,*)     qdep
      write(6,*) 'SMOOTH? (0)NO  (1)YES:'
      read (5,*)     smoothyn

      write(6,*) 'readING HEADER:'
      open(1,FILE=ifile,STATUS='OLD')                  !open INTPUT FILE
      read (1,*)     ndat,ndmax
      write(6,*)     ndat,ndmax
      read (1,FMT=888) temp,(del(I),I=1,ndmax)
      CNT = 1
      write(6,*) 'readING DATA:'
      if (mod(ndmax,2) /= 0) ndmax = ndmax - 1        !REQUIRE EVEN ARRAY SIZE
      if (mod(ndat ,2) /= 0) ndat  = ndat  - 1 
      ndmax = ndmax -4
      do I = 1, ndat
       read(1,FMT=888)time(I),(val2D(I,J),J=1,ndmax)
       do J = 1, ndmax
        val2D(I,J) = -val2D(I,J)
        if (abs(val2d(I,J)).GT.99.) val2d(I,J) = val2d(I,J)/10. 
       end do
       CNT = CNT + ndmax
      end do
      CLOSE(1)

      write(6,*) 'DATA read IN.',ndat,ndmax
      
      call minr1(time, ndat,tmin)                     !GET MIN & MAX TIMES
      call maxr1(time, ndat,tmax)
      call minr1( del,ndmax,dmin)                     !GET MIN & MAX DISTANCES
      call maxr1( del,ndmax,dmax)
      dt   = time(2) - time(1)
      ddel =  del(2) -  del(1)
      call PSFILE(outfile)                            !START POSTSCRIPT PLOT
      call PSFONT('t12')
      call PSCOL(1)
      WXMN = 1.0
      WXMX = 3.5
      WYMN = 1.0
      WYMX = 3.5
      call PSWIND(1.0,7.5,1.0,7.5,dmin,dmax,tmin,tmax) !SET PLOT SIZE
      write(6,*) 'MAIN PLOT CREATED.',dmin,dmax,tmin,tmax
      call PSNICE(tmin,tmax,ttic1,ttic2)              !SET TIME TICK MARKS
      call PSNICE(dmin,dmax,dtic1,dtic2)              !SET DISTANCE TICK MARS
      ttic1 = ttic1/4.
      ttic2 = ttic2/4.
      write(6,*) 'TICKS CREATED:',dtic1,dtic2,dtic1,dtic2
      call PSAXES(dtic1,dtic2,.05,'i3',ttic1,ttic2,.005,'f8.0') !PLOT AXES
      call PSLAX('Range (degrees)',.3,'Time (sec)',.4) !LABEL THE AXES
      write(6,*) 'AXES CREATED:',dmin,dmax,tmin,tmax
      call PSLORG(8)                                  !POSITION OF CURSOR
      dave = (dmax+dmin)/2.                           !AVERAGE DELTA VALUE
      call PSMOVE(dave,tmax)                          !MOVE CURSOR TO TOP 
      call PSTIC(.1,0.,1)                             !MOVE CURSOR UP A LITTLE
      call PSLAB('Amplitude')                         !LABEL THE PLOT
      icol = 1
      
      
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
!     SMOOTH THE PLOT:                                                   |   !
    
      if (smoothyn >= 1) THEN
       CNT =   0 
       do I = 1, ndat
        do J = 1, ndmax
         CNT = CNT + 1
         val2(CNT) = val2D(I,J)*2.
         CNT2      = 2.
	IF (smoothyn >= 2) THEN
         if (I /= 1) THEN
          val2(CNT) = val2(CNT) + val2D(I-1,J  )*1.
          CNT2      = CNT2 + 1
         end IF
         if (I /= ndat) THEN
          val2(CNT) = val2(CNT) + val2D(I+1,J  )*1.
          CNT2      = CNT2 + 1
         end IF
        end IF
        if (J /= 1) THEN
         val2(CNT) = val2(CNT) + val2D(I  ,J-1)*2.
         CNT2      = CNT2 + 2
        end IF
	IF (J /= ndmax ) THEN
         val2(CNT) = val2(CNT) + val2D(I  ,J+1)*2.
         CNT2      = CNT2 + 2
        end IF
        IF( (I /= 1    ).AND.(J /= 1    ) ) THEN
         val2(CNT) = val2(CNT) + val2D(I-1,J-1)
         CNT2      = CNT2 + 1
        end IF
        IF( (I /= ndat ).AND.(J /= 1    ) ) THEN
         val2(CNT) = val2(CNT) + val2D(I+1,J-1)
         CNT2      = CNT2 + 1
        end IF
        if ( (I /= 1   ).AND.(J /= ndmax) ) THEN
         val2(CNT) = val2(CNT) + val2D(I-1,J+1)
         CNT2      = CNT2 + 1
        end IF
        if ( (I /= ndat).AND.(J /= ndmax) ) THEN
         val2(CNT) = val2(CNT) + val2D(I+1,J+1)
         CNT2      = CNT2 + 1
        end IF
        if (CNT2 > 1) val2(CNT) = val2(CNT)/float(CNT2)
       end do
      end do
      ELSE
       CNT = 0
       do I = 1, ndat
        do J = 1, ndmax
         CNT = CNT + 1
         val2(CNT) = val2D(I,J)
        end do
       end do
      end IF

      
      CNT  = -2
      CNT2 =  0
      do I = 1, ndat
       do J = 1, ndmax
         CNT  =  CNT + 3
         CNT2 = CNT2 + 1
         anorm=(val2(CNT2)-amin)/(amax-amin)
         if (anorm >  1.) anorm =  1.
         if (anorm <  0.) anorm =  0.
         call GETRBG(anorm,red,blue,green)
         ibuf(CNT  ) = nint(blue*225.)
         ibuf(CNT+1) = nint(green*225.)
         ibuf(CNT+2) = nint(red*225.)
!        write(6,*) CNT,ibuf(CNT),ibuf(CNT+1),ibuf(CNT+2)
       end do
      end do
      
      
      call PSIMAG(ibuf,ndmax,ndat,icol)
      
      pfile(01)='/Volumes/eyes/TT/tt91.S'   !  SET TRAVEL TIME TABLE NAMES
      pfile(02)='/Volumes/eyes/TT/tt91.ScS'    
      pfile(03)='/Volumes/eyes/TT/tt91.SS'
      pfile(04)='/Volumes/eyes/TT/tt91.S3'
      pfile(05)='/Volumes/eyes/TT/tt91.S4'
      pfile(06)='/Volumes/eyes/TT/tt91.S660S'
      pfile(07)='/Volumes/eyes/TT/tt91.S410S'
      pfile(08)='/Volumes/eyes/TT/tt91.ScS2'
      pfile(09)='/Volumes/eyes/TT/tt91.ScS3'

      call PSPS('stroke')
      atemp = 'NONE                       '
      if (plotyn /= 0) THEN
!      IP = 1
      do I = 1, 9
       if (IP /= 0) THEN
        pnm = pfile(I)
        pnm = pnm(30:38)
        call ttplot(pfile(IP),pfile(I),pnm,dmin,dmax,ddel,tmin,tmax,qdep)
       else
        pnm = pfile(I)
        pnm(1:5) = pnm(30:34)
        call ttplot(atemp   ,pfile(I),pnm,dmin,dmax,ddel,tmin,tmax,qdep)
       end if
      end do
      end if
      
      do i = 1, 8
       del2 = I*20
       idel = (I*20)/ddel
       call psmove(del,0.)
       do j = 1, ndat
        call psdraw(del2+val2d(j,idel)*1000.,float(j-1)*dt)
       end do
      end do
      
      call PSend

      
888   FORMAT(F10.2,1X,500(F10.6,1X))

      stop
end program psstack




subroutine ttplot(pfile1,pfile2,pnm,dmin,dmax,ddel,tmin,tmax,qdep)
      character*80      pfile1,pfile2,pnm
      character*9       pnm2
      real              dmin,dmax,ddel,tmin,tmax        !DIMENSIONS OF PLOT
      integer           idmin,idmax
      real              tt1,tt2,dtime,del,qdep,dtdh,dtddel
      integer           iflag,CNT
      SAVE              DD
      pnm2 = pnm(1:9)
      idmin = 0
      idmax = nint(dmax/ddel) - nint(dmin/ddel)          !SET HORIZONTAL DIMENSION OF PLOT
      tt1   = 0.                                        !SET VARAIBLES TO ZERO
      CNT  = 0
      xp = -999.
      yp = -999.
      DD = DD + 20
      if (DD > idmax) DD = DD - idmax
      if (DD < dmin) DD = 10

      call PSMOVE(dmin,tmin)
      do I = idmin,idmax                                !FOR EACH DISTANCE IN PLOT
       del = float(I)*ddel+dmin
       tt1  = 0.
       if (pfile1(1:4) /= 'NONE') THEN 
        call GET_TTJ(pfile1,1,del,qdep,tt1,dtdh,dtddel,iflag)!CALCULATE REFERENCE TRAVEL TIME
       end IF
       if (iflag < 0) tt1 = 0. 
       call GET_TTJ(pfile2,2,del,qdep,tt2,dtdh,dtddel,iflag)!CALCULATE PHASE TRAVEL TIME
       if (iflag >= 0) THEN
        dtime = tt2 - tt1                                !ADJUST FOR REFERENCE FRAME TIME
!        if (pnm2(1:9).EQ.'PKIKP    ') write(6,*) pnm2(1:9),del,dtime
        if ((dtime <= tmax).AND.(dtime >= tmin))THEN
         if (CNT.EQ.0)  call PSMOVE(del,dtime)
         if (CNT > 0)  call PSDRAW(del,dtime)          !DRAW THE POINT if DATA IN PLOT RANGE
         if (CNT.EQ.DD+5) THEN
         xpn = del
         ypn = dtime
         end IF
         if (CNT.EQ.DD) THEN
          xp = del
          yp = dtime
         end IF
         CNT = CNT + 1
        end IF
       end IF
999   end do      
      call PSPS('stroke')
      
      yrng = tmax-tmin
      xrng = dmax-tmin
      r2d  = 180./atan(1.)/4.
      if (xp /= -999)THEN
       call PSLORG(2)
       call PSMOVE(xp,yp+5)
       ang = atan ( ((ypn-yp)/yrng)/ ((xpn-xp)/xrng) )*r2d
       write(6,*) ang
       call PSANG(ang)
       call PSLAB(pnm2)
      end IF
      
      
      return
end subroutine ttplot





subroutine GETRBG(a,red,blue,green)
      if (a <= 0.5) THEN         
         red=1.
         wht=2*a
         blue=wht
         green=wht
      ELSE
         blue=1.
         wht=2*(1.-a)
         red=wht
         green=wht
      end IF
      return
end subroutine getrbg



subroutine GET_TTJ(phase,ip,del,qdep,tt,dtdh,dtddel,iflag)
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
!   |THIS SUBROUTINE OBTAINS THE TRAVEL TIME, HORIZONTAL, AND VERTICAL   |   !
!   |     FOR A SPECIFIED PHASE FOR ANY GIVEN DISTANCE AND DEPTH.  THE   |   !
!   |     TRAVEL TIMES AND SLOWNESS VALUES ARE INTERPOLATED FROM PRE-    |   !
!   |     CONSTRUCTED TRAVEL TIME TABLES.                                |   !
!   |                                                                    |   !
!   |THIS SUBROUTINE WAS CHANGED FROM TTM BY CHANGING UNITS TO SECONDS   |   !
!   |     RATHER THAN MINUTES.                                           |   !
!   |                                                                    |   !
!   |THIS SUBROUTINE WAS RIPPED OFF OF PETER SHEARER'S GET_TTS SUBROUTINE|   !
!   |     WHICH WAS ALTERED FROM GET_TT. THE PROGRAM DIFFERS FROM GET_TT |   !
!   |     BY SAVING DATA WITHOUT HAVING TO RE-read TT TABLES (IO IS SLOW)|   !
!   |     THIS SUBROUTINE ALSO DIFFERS FROM GET_TTS BY CALCULATING THE   |   !
!   |     SLOWNESS INSTEAD OF JUST THE TRAVEL TIMES.  THIS MEANS THAT YOU|   !
!   |     doN'T NEED TO CONSTRUCT SEPARATE SLOWNESS TABLES.  THIS SUB    |   !
!   |     REMOVES THE EXTRAPOLATION PORTION FROM GET_TT AND GET_TTS.     |   !
!   |                                                                    |   !
!   |INPUTS:    phase  =  NAME OF FILE CONTAINING TRAVEL TIME TABLES     |   !
!   |           ip     =  INDEX NUMBER OF FILE (NOT IO FOR FORTRAN).     |   !
!   |           del    =  DISTANCE RANGE (IN DEGREES).                   |   !
!   |           qdep   =  EARTHQUAKE DEPTH                               |   !
!   |                                                                    |   !
!   |OUTPUTS:   tt     =  TRAVEL TIME IN (SECONDS)                       |   !
!   |           dtdh   =  VERTICAL SLOWNESS IN MINUTES/KM                |   !
!   |           dtddel =  HORIZONTAL SLOWNESS IN MINUTES/DEGREE          |   !
!   |           iflag  = -1 if OUTSIDE DEPTH RANGE OF TT TABLE           |   !
!   |                  =  0 FOR INTERPOLATION                            |   !
!   |                                                                    |   !
!   |WARNING:  THIS SUBROUTINE MAY HAVE CONFLICTS WITH DATA INPUT FROM   |   !
!   |      FILE I/O NUMBER 3.                                            |   !
!   |                                                                    |   !
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
!   |SET PARAMETERS AND VARIABLES:                                       |   !
      PARAMETER   (nx0=361,nd0=100,np0=20)         !MAX DIMENSIONS OF TT TABLES
      character*80 phase,phaseold(np0),linebuf    !PHASE FILE
      integer      nx(np0),nd(np0)                !NUMBER OF ROWS AND COLUMNS
      real         t(nx0,nd0,np0),x(nx0,np0),d(nd0,np0) !TTIME,DISTANCE,EQ DEPTH
      SAVE         t,x,d,phaseold,nx,nd           !DATA SAVED REDUCING RE-readS

      if (ip < 1.OR.ip > np0) THEN
         write(6,*) '*** ERROR: in TTM, ip OUT OF RANGE: ',ip
         stop
      end IF
!
! read file if new phase file is specified
      if (phase /= phaseold(ip)) THEN             !read if NOT ALreadY read IN
         open (3,file=phase,status='old')
         read (3,'(a40)') linebuf   !ignore first line  !BLANK LINE
         read (3,*) nx(ip),nd(ip)                 !DIMENSION OF THE TT TABLE
         if (nx(ip) > nx0) THEN                  !TRUNCATE if TABLE TOO LARGE
          write(6,*) '***WARNING: GET_TTS nx TRUNCATED ',nx(ip),nx0
          nx(ip)=nx0
         end IF
         if (nd(ip) > nd0) THEN                  !TRUNCATE if TABLE TOO LARGE
          write(6,*) '***WARNING: GET_TTS nd TRUNCATED ',nd(id),nd0
             nd(ip)=nd0
         end IF
         read (3,*) (d(id,ip),id=1,nd(ip))        !read IN THE DEPTHS OF TABLE
         do 20 ix=1,nx(ip)                        !read DISTANCES AND TIMES
          read (3,*) x(ix,ip),(t(ix,id,ip),id=1,nd(ip))
20       end do
         CLOSE (3)
      end IF
      phaseold(ip)=phase                          !STORE PHASE NAME FOR FUTURE

!   |CHECK TO SEE if EQ DEPTH IS OUTSIDE TABLE DEPTH RANGE               |   !
      if ( (qdep < d(1,ip)) .OR. (qdep > d(nd(ip),ip)) ) THEN
       iflag  = -1
       tt     = 999
       dtdh   = 999
       dtddel = 999
       return
      end IF

      id1     = nd(ip)-1
      id2     = nd(ip)
!   |CHECK if DISTANCE AND DEPTH IS IN THE RANGE OF THE THE TT TABLES:   |   !
      do 30 id=2,nd(ip)                              !SCROLL THROUGH DEPTHS
       if (d(id,ip) < qdep) cycle
       id1  =id-1
       id2  =id
       exit
30    end do
      ix1=nx(ip)-1
      ix2=nx(ip)
32    do 35 ix=2,nx(ip)                              !SCROLL THROUGH DISTANCES
         if (x(ix,ip) < del) cycle
         ix1  = ix-1
         ix2  = ix
         exit
35    end do

37    if  ( (t(ix1,id1,ip).EQ.0.) .or. &              !CHECK FOR EXTRAPOLATION
            (t(ix1,id2,ip).EQ.0.) .or. &
            (t(ix2,id1,ip).EQ.0.) .or. &
            (t(ix2,id2,ip).EQ.0.) .or. &
            (x(ix2,ip) < del) ) then
        iflag  = -2
        tt     = 999
        dtdh   = 999
        dtddel = 999
        return
	    
      end if
      
      iflag=0
      xfrac=(del-x(ix1,ip))/(x(ix2,ip)-x(ix1,ip))   !INTERPOLATION DIST FRACTION
      dfrac=(qdep-d(id1,ip))/(d(id2,ip)-d(id1,ip))  !INTERPOLATION DEPTH FRACT
      
      t1     =t(ix1,id1,ip)+xfrac*(t(ix2,id1,ip)-t(ix1,id1,ip)) !dtdx1
      t2     =t(ix1,id2,ip)+xfrac*(t(ix2,id2,ip)-t(ix1,id2,ip)) !dtdx2
      t3     =t(ix1,id1,ip)+dfrac*(t(ix1,id2,ip)-t(ix1,id1,ip)) !dtdh1
      t4     =t(ix2,id1,ip)+dfrac*(t(ix2,id2,ip)-t(ix2,id1,ip)) !dtdh2
            
      tt     = (t1+dfrac*(t2-t1))*60.              !METHOD 1 TRAVEL TIME
      tt22   = (t3+xfrac*(t4-t3))*60.              !METHOD 2 TRAVEL TIME
      dtdh   = (t2-t1)/(d(id2,ip)-d(id1,ip))*60.   !VERTICAL SLOWNESS
      dtddel = (t4-t3)*180./atan(1.)/4.*60.        !HORIZONTAL SLOWNESS
      
      return

      
999   return
end subroutine GET_TTJ



subroutine maxr1(series,ndat,max)
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
!   |THIS SUBROUTINE RECORDS THE MAXIMUM, max, OF A SERIES, series, WITH |   !
!   |     npts POINTS, AT SPACE HOLDER imax.                             |   !
!   |                                                                    |   !
!   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   |   !
!   |      CONTACT:       jflawrence@stanford.edu                        |   !
!   |                                                                    |   !
!   |AS WITH ALL CODES: "BEWARE OF THE BUG!"                             |   !
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
!   |SET PARAMETERS AND DECLARE VARIABLES:                               |   !
      real             series(*),max
      integer          I,ndat

      max = 0.
      do I = 1, ndat
       if (max < series(I)) max = series(I)
      end do

      return
end subroutine maxr1

subroutine minr1(series,ndat,min)
!   |THIS SUBROUTINE RECORDS THE minIMUM, min, OF A SERIES, series, WITH |   !
!   |     npts POINTS, AT SPACE HOLDER imin.                             |   !
      real             series(*),min
      integer          I,ndat

      min = 99999.
      do I = 1, ndat
       if (min > series(I)) min = series(I)
      end do

      return
end subroutine minr1

