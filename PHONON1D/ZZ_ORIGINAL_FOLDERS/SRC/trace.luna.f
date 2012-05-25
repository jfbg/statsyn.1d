      PROGRAM trprog
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      PARAMETER     (NLMX = 12742,NDAT=NLMX*40)        !MAX NUMBER OF MODEL LAYERS
      REAL           z(NLMX),va(NLMX),vb(NLMX)
      REAL           r(NDAT),d(NDAT),t(NDAT),vt(NDAT)
      INTEGER        imodel,plotyn,quad,ie
      REAL           del(100),qdep,eqdp(100),rmin,p
      CHARACTER*80   phs(100),DIR,cols(100),menu(100)
      INTEGER        pmx,iphs(100),icol(100),nphs,cmx,mmx
      INTEGER        plotnum
      CHARACTER*80   trfile

C   |SET SOME DEFAULTS THAT THE USER CAN CHANGE LATER:                   |   C
      imodel =   3    !(1) = PREM; (2) = PREM.OCEAN; (3) = IASP91; (4) = SP5
C      del    =  90.   !DISTANCE (IN DEGREES)
      qdep   =   0.   !EARTHQUAKE DEPTH (IN KM)
      plotyn =   1    !PLOT TO SCRENE (1) PLOT TO POSTSCRIPT (2)
      quad   =   4    !PLOT (1) PIE (2) 1/2 CIRCLE (4) CIRCLE
      CALL EZXINIT(1000,900)                 !INITIATE NEW WINDOW
      CALL EZXLOADFONT(I,'9X15')             !LOAD PLOT FONTS
  777 CALL EZXCLR                            !CLEAR THE SCREEN
      CALL EZXCOL(1)                         !SET PLOT COLOR TO BLACK
      CALL vload(quad)                       !SET UP EARTH LAYERS

      CALL menload(menu,mmx)                 !LOAD THE OPTION MENU UPTIONS
      CALL ttload2(DIR,phs,pmx)              !LOAD THE WAVE TYPE MENUE OPTIONS
      CALL coload(cols,cmx)                  !LOAD THE COLOR MENUE OPTIONS
      nphs    = 1                            !START WITH ONE PHASE (SKIP FIRST)
      idig    = 0                            !DONT ASSUME WHICH BUTTON IS USED 
      plotnum = 0                            !NUMBER OF PLOTS OUTPUT
      DO WHILE (idig.NE.3)
 1111  CALL EZXCOL(1)                        !SET THE PLOT COLOR TO BLACK
       CALL EZXLIST(1908.,2726.,-1090.,2181.,mmx,menu,imen,1,3,0)!MAIN MENUE
       IF (imen.EQ.mmx  ) GOTO 8888          !QUIT OPTION
       IF (imen.EQ.mmx-1) plotyn = 2         !OUTPUT TO trace.ps FILE
       IF (imen.EQ.mmx-2) GOTO 777           !CLEAR THE SCRENE & START OVER
       IF (imen.EQ.1) THEN                   !SELECT PREM OR IASP91
        CALL EZXBUT2(954., 545.,2,35,' PREM  ',1,3)  !PREM   BUTTON
        CALL EZXBUT2(954., 545.,2,35,'IASP91 ',1,3)  !IASP91 BUTTON
        CALL EZXDIG(x,y,ibut)                           !GET BUTTON
	IF ((abs(x-2590).LE.272).AND.(abs(y-1908).LE.272)) THEN
         imodel = 1                                     !PREM MODEL
	 period = 20.
        ELSE IF ((abs(x-3135).LE.272).AND.(abs(y-1908).LE.272)) THEN
         imodel = 3                                     !IASP91 MODEL
        END IF
       END IF
       IF ((imen.GT.nphs+1).AND.(imen.LT.mmx-2)) imen = nphs + 1!DONT SKIP WAVES
       IF ((imen.GT.nphs  ).AND.(imen.LT.mmx-2)) nphs = nphs + 1!ADD A PHSE
       IF ( (imen.GT.1).AND.(imen.LT.mmx-2) ) THEN
        CALL EZXMESS("CHOOSE A PHASE TO CHANGE OR ADD")
        CALL EZXLIST(2862.,4089.,-1908.,2181.
     &              ,pmx,phs ,iphs(imen),1,3,1)    !CHOOSE WHICH PHASE FROM MENU
        IF (iphs(imen).EQ.0) GOTO 1111             !NO PHASE SELECTED. START OVER
        CALL EZXMESS("CHOOSE A COLOR FOR THE PHASE")
        CALL EZXLIST(2862.,4089.,-1908.,2181.
     &             ,cmx,cols,icol(imen),1,3,1)     !CHOOSE WHICH COLOR FROM MENU
        IF (icol(imen).LE.0) GOTO 1111             !NO COLOR SELECTED. START OVER
        CALL EZXMESS("ENTER THE EQ-ST DISTANCE:")  !HELPING MESSAGE
        CALL EZXCALC(2862.,4089.,272.,2181.,del(imen)) !ENTER EQ-ST DISTANCE
        IF (del(imen).LE.0.) GOTO 1111             !NO DISTANCE SELECTED. START O
        CALL EZXMESS("ENTER THE EQ DEPTH:")        !HELPING MESSAGE
        CALL EZXCALC(2862.,4089.,272.,2181.,eqdp(imen)) !ENTER EQ DEPTH
        menu(imen) = phs(iphs(imen))               !CHANGE PHASE 2 NEW PHASE NAME
       END IF
       CALL EZXCLR                                 !CLEAR SCREEN PLOT
       CALL vload(quad)                            !SET UP EARTH LAYERS 4 SCREEN
       IF (plotyn.EQ.2) THEN                       !IF PRINT SELECTED THEN
        plotnum = plotnum + 1                      !INCREASE PLOT NUMBER
	WRITE(trfile,FMT=9035)'trace.',plotnum,'.ps' !CHANGE NAME TO NEW PLOT #
        CALL PSFILE(trfile)                        !START HEADER OF PS FILE
        CALL PSFONT('t12')                         !SET FONT FOR PS FILE
        CALL PSLOAD(quad)                          !LOAD EARTH: PIE=1 ... CIRC=4
        CALL PSPS("stroke")                        !THIS FINISHES LINE SEGMENT
       END IF
       DO I = 2, nphs                              !FOR EACH SELECTED WAVE
        CALL trmaster3(imodel,del(I),eqdp(I),phs(iphs(I)),plotyn,quad
     &                     ,z,va,vb,p,r,d,t,vt,ie) !TRACE EACH WAVE
        CALL vtrace2(r,d,ie,icol(I))               !PLOT THE TRACE TO SCRENE
        IF (plotyn.EQ.2) CALL pstrace2(r,d,ie,icol(I)) !PLOT THE TRACE TO SCRENE
        IF (plotyn.EQ.2) CALL PSPS("stroke")       !THIS FINISHES LINE SEGMENT
       END DO
       IF (plotyn.EQ.2) THEN       
        CALL PSEND                                 !CLOSE THE POSTSCRIPT
	plotyn = 1                                 !TURN OFF PRINTING OPTION
       END IF
      END DO
9035  FORMAT(A6,I2.2,A3)                           !PS FILE OUTPUT NAME FORMAT
8888  CALL EZXQUIT                                 !QUIT THE WINDOW
      
9999  STOP
      END




      SUBROUTINE EZXCALC(xmin,xmax,ymin,ymax,val)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE ADDS A CALCULATOR TO THE SCRENE.  SO FAR THIS ONLY  |   C
C   |     ENTERS THE INPUTS NUMBERS.  NO MATH DONE.                      |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      REAL         xmin,xmax,ymin,ymax           !SIZE OF CALCULATOR
      REAL         val                           !ENTERED NUMBER (OUTPUT)
      REAL         x,y,dx,dy                     !USER SELECTION/BUTON LOCATIONS
      CHARACTER*8  label                         !BUTTON LABEL
      INTEGER      idig                          !MOUSE BUTTON SELECTED
      CHARACTER*20 chval                         !CHARACTER REPRESENTATION OF #
      INTEGER      dec
      
      dy = (ymax-ymin)/4.                        !SET BUTTON SPACING VERTICAL
      dx = (xmax-xmin)/3.                        !SET BUTTON SPACING HORIZONTAL
      DO I = 1, 12                               !SET 12 MAIN BUTTONS
       y = float(int(float(I-1)/3.))             !DETERMINE LOCATION
       x = float(I)-y*3.
       y = y *dy+ymin
       x = x *dx+xmin
       IF (I.GT.3) THEN                          !LABEL BUTTON NAMES (NUMBERS)
        WRITE(label,1010) I-3
       ELSE IF (I.EQ.1) THEN
        WRITE(label,1010) 0
       ELSE IF (I.EQ.2) THEN                     !LABEL BUTTON (DECIMAL)
        WRITE(label,1020) '   .    '
       ELSE IF (I.EQ.3) THEN                     !LABEL BUTTON (DONE)
        WRITE(label,1020)  '  DONE  '
       END IF
       CALL EZXBUT2(x,y,2,30,label,3,1)          !PLOT BUTTON TO SCRENE
      END DO
      
      CALL EZXBUT2(xmax      ,ymax,2,30,' CANCEL ',3,1) !PLOT CANCEL BUTTON
      CALL EZXBUT2(xmin+dx   ,ymax,2,30,' BACK   ',3,1) !PLOT BACK SPACE BUTTON
      CALL EZXBUT2(xmin+2*dx ,ymax,2,30,' CLEAR  ',3,1) !PLOT CLEAR BUTTON
      
      CALL EZXFRAM(xmin+dx/2.,xmax+dx/2.,ymin-dy,ymin-dy/2.)!PLOT DISPLAY WINDOW
      
      chval = '                    '             !CLEAR # STORED AS CHARACTER
      val  = 0.                                  !CLEAR THE NUMBER
      dec  = 0                                   !NUMBER OF DIGITS W/ DEC
      dec2 = 0                                   !ONLY ALLOW ONE DECIMAL POINT
      DO WHILE (idig.NE.3)
       CALL EZXDIG(xx,yy,idig)                   !DETERMINE WHICH BUTTON SELECT
       IF (idig.EQ.3) GOTO 9999                  !IF MOUSE 3RD BUTTON: QUIT
       ival = 0                                  !ASSUME NO CALC BUTTON SELECT 
       DO I = 1, 12                              !DETERMINE WHICH BUTTON SELECT
        y = float(int(float(I-1)/3.))
        x = float(I)-y*3.
        y = y *dy+ymin
        x = x *dx+xmin
        IF ( (abs(xx-x).LT.245.).AND.(abs(yy-y).LT.245.) ) THEN
         ival = I
        END IF
       END DO
       x = xmax                                  !CANCEL BUTTON SELECT
       y = ymax
       IF ( (abs(xx-x).LT.245.).AND.(abs(yy-y).LT.245.) ) THEN
        val  = 0.
	GOTO 9999
       END IF
       x = xmin+dx                               !BACKSPACE BUTTON SELECT
       y = ymax
       IF ( (abs(xx-x).LT.245.).AND.(abs(yy-y).LT.245.) ) THEN
	WRITE(chval(dec:dec),'(A1)') ' '
        dec = dec - 1 
       END IF
       x = xmin+dx*2                             !CLEAR BUTTON SELECT
       y = ymax
       IF ( (abs(xx-x).LT.245.).AND.(abs(yy-y).LT.245.) ) THEN
	WRITE(chval(1:20),'(A20)') '                    '
        dec = 0 
       END IF

       IF ( (ival.GT.3) ) THEN                  !EXECUTE NUMBER COMMAND
        dec = dec + 1
        WRITE(chval(dec:dec),'(I1)')(ival)-3
       ELSE IF ( (ival.EQ.2).AND.(dec2.EQ.0) ) THEN !EXECUTE DECIMAL COMMAND
        dec  = dec + 1
	dec2 = 1
        WRITE(chval(dec:dec),'(A1)') '.'
       ELSE IF (ival.EQ.3) THEN                 !EXECUTE DONE/ENTER COMMAND
        GOTO 8888
       ELSE IF ( (ival.EQ.1) ) THEN             !EXECUTE ZERO COMMAND
        dec = dec + 1
        WRITE(chval(dec:dec),'(A1)') '0'
       END IF
       CALL EZXCLRB(xmin+dx/2.,xmax+dx/2.,ymin-dy,ymin-dy/2.)!CLEAR # FROM SCREE
       CALL EZXFRAM(xmin+dx/2.,xmax+dx/2.,ymin-dy,ymin-dy/2.)!REPLOT DISPLAY WIN
       CALL EZXMOVE(xmin+2*dx,ymin-dy*2./3.)    !GOTO DISPLAY WINDOW LOCATION
       CALL EZXLAB(chval)                       !PLOT NUMBER IN WINDOW
      END DO
 8888 READ (chval(1:dec),*) val                 !CONVERT CHAR TO READL NUMBER
 9999 CALL EZXCLRB(xmin-50.,xmax+dx,ymin-dy-50.,ymax+dy+50.) !CLEAR CALCULATOR

 1010 FORMAT(4X,I1.1,3X)
 1020 FORMAT(A8)
      RETURN
      END











      SUBROUTINE MENLOAD(menu,mmx)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE ENTERS THE MAIN MENUE OPTIONS FOR TRACE.VIS PROGRAM. |   C
C   |     MOST MENU OPTIONS ARE TO ADD/CHANGE WAVE/PHASE TYPE.  OTHERS    |   C
C   |     INCLUDE MODEL (PREM,OR IASP91), CLEAR SCREEN, PLOT TO A         |   C
C   |     POSTSCRIPT FILE, AND QUIT THE PROGRAM.                          |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      CHARACTER*80       menu(*)
      INTEGER            mmx
      mmx = 24                                  !SET # OF MENU OPTIONS < 100
      menu(1) = 'MODEL'                         !SET MENU LABELS
      DO I = 2, mmx-2
       WRITE(menu(I),1010)'PHASE ',I-1 
      END DO
      menu(mmx-2) = 'CLEAR'
      menu(mmx-1) = 'PS FILE'
      menu(mmx)   = 'QUIT'
 1010 FORMAT(A6,I2.2)
      RETURN
      END



      SUBROUTINE coload(cols,ncol)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |SET THE NAMES OF THE COLOR OPTIONS FOR PSPLOT AND EZXPLOT.           |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      CHARACTER*80 cols(*)
      INTEGER      ncol
      cols( 1) = 'black'
      cols( 2) = 'red'
      cols( 3) = 'blue'
      cols( 4) = 'yellow'
      cols( 5) = 'green'
      cols( 6) = 'orange'
      cols( 7) = 'purple'
      cols( 8) = 'teal'
      cols( 9) = 'pink'
      cols(10) = 'grey'
      ncol     = 10
      
      RETURN
      END


      SUBROUTINE EZXLIST(xmin,xmax,ymin,ymax,nlist,list,ival,bcol,tcol
     &                  ,cl)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE TAKES A LIST, list, OF, nlist, MENU OPTIONS, AND     |   C 
C   |     PLOTS THEM TO THE SCREEN BETWEEN xmin,ymin AND xmax,ymax.  THEN |   C
C   |     THE USER SELECTS WHICH OPTION WITH THE MOUSE AND THE SUBROUTINE |   C
C   |     RETURNS WHICH NUMBER OPTION WAS SELECTED (1 - nlist).           |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      REAL         xmin,xmax,ymin,ymax          !BOUNDS OF MENU PLOT
      INTEGER      nlist,ival                   !NUMBER OF OPTIONS/SELECTED OPT
      INTEGER      bcol,tcol                    !COLORS OF BOARDER & TEXT
      INTEGER      cl                           !CLEAR MENUE AFTER (1) OR NOT(0)
      CHARACTER*80 list(*)                      !MENU LABELS
      dy = (ymax-ymin)/float(nlist)             !VERTICAL SPACING OF MENU

      CALL EZXLORG(1)                           !SET TEXT POSITION CENTER
      x2 = (xmin*2+xmax)/3                      !TEXT HORIZONTAL START POSITION

      DO I = 1, nlist                           !PLOT EACH OPTION TO SCREEN
       y1 = ymax - float(I-1)*dy                !TOP OF BUTTON
       y2 = ymax - float(I  )*dy                !BOTTOM OF BUTTON
       y3 = (y2)                                !TEXT VERTICAL START POSITION
       CALL EZXCOL(bcol)                        !CHANGE COLOR TO BOARDER COLOR
       CALL EZXFRAM(xmin,xmax,y1,y2)            !PLOT BLOCK ARROUND MENU OPTION
       CALL EZXCOL(tcol)                        !CHANGE COLOR TO TEXT COLOR
       CALL EZXMOVE(x2,y3)                      !MOVE TO TEXT POSITION
       CALL EZXLAB(list(I))                     !PLOT MENU LABEL TO SCREEN
      END DO
      idig = 0                                  !DONT ASSUME MOUSE BUTTON
      ival = 0                                  !ASSUME NO BUTTON SELECTED

      DO WHILE(idig.NE.3)                       !DETERMINE WHICH OPTION SELECTED
       CALL EZXDIG(x,y,idig)                    !GET USER INPUT POSITION
       IF (idig.EQ.3) GOTO 8888                 !IF MOUSE 3RD BOTTON: QUIT MENU
       IF ((x.GE.xmin).AND.(x.LE.xmax)) THEN    !ONLY RECORD OPTIONS INSIDE MENU
        DO I = 1, nlist                         !DETERMINE WHICH POSITION
         y1 = ymax - float(I-1)*dy
         y2 = ymax - float(I  )*dy
         IF ((y.LE.y1).AND.(y.GE.y2)) THEN      !ONCE DETERMINED RETURN 2 PROG
          ival = I
	  GOTO 8888
         END IF
        END DO
       ELSE
        CALL EZXMESS('SELECT FROM THE LIST:')   !PLOT MESSAGE 2 BOTTOM OF SCREEN
       END IF
      END DO
8888  IF (cl.EQ.1) CALL EZXCLRB(xmin-5.,xmax+50.,ymin-50.,ymax+50.)!CLEAR MENU
      IF (idig.EQ.3) ival = 0


9999  RETURN
      END






      SUBROUTINE EZXBUT2(x,y,shape,size,text,tcol,bcol)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE PLACES A BUTTON AT POINT (x,y) WITH THE RIGHT       |    C
C   |     SHAPE, TEXT, AND COLOR:                                        |    C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      REAL         x,y
      INTEGER      shape,tcol,bcol,size
      CHARACTER*8  text
      
      CALL EZXMOVE(x,y)                         !GO TO STARTING POSIION
      CALL EZXCOL(bcol)                         !SET SHAPE COLOR
      CALL EZXSYMB(shape,size)                  !CREATE SHAPE
      CALL EZXLORG(5)                           !PLACE TEXT AT CENTER OF BTN
      CALL EZXCOL(tcol)                         !SET TEXT COLOR
      CALL EZXLAB(text)                         !WRITE TEXT AT CENTER OF BTN
      CALL EZXCOL(1)                            !RETURN WITH COLOR SET BLACK

      RETURN
      END
      

      SUBROUTINE vload(quad)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE PLOTS THE LAYERS OF THE EARTH TO SCREEN FOR (1) A   |   C
C   |     PIE SHAPED EARTH, 2 A HALF SHAPED EARTH, 3 A ALL BUT PIE SHAPED|   C
C   |     EARTH, AND 4 A CIRCULAR EARTH.                                 |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      PARAMETER (NMAX=100000)
      INTEGER    quad
      REAL       re,r660,r410,rcmb,ricb
      REAL       pi,r2d
      REAL       ymin,ymax,xmin,xmax,dmax
      REAL       rx,ry,xx,yy

      re   = 1737.                               !SET RADIUS OF EARTH
      
      r660 = re - 57.                            !SET 660-KM RADIUS
      r410 = re - 270.                           !SET 410-KM RADIUS
      rcmb = re - 1680.                          !SET CMB RADIUS 
      ricb = re - 1000.0                         !SET ICB RADIUS 
      pi   = atan(1.)*4.                         !SET PI
      r2d  = 180./pi                             !CONV RADIANS TO DEGREES

      ymin = 0.                                  !SET BOUNDS OF PLOT
      ymax = re 
      xmin = 0.
      xmax = re
      dmax = 90.*float(quad)                     !BOUNDS SET BY # QUADRANTS
      IF (quad.GT.1) ymin = -re
      IF (quad.GT.2) xmin = -re

      ry = (ymax-ymin)/re*250.                   !ADJUST PLOT RANGE
      rx = (xmax-xmin)/re*250.
      PXMN = 600-rx                              !SET PLOT SIZE
      PXMX = PXMN+rx
      PYMN = 700-ry
      PYMX = PYMN+ry
      
      WRITE(6,*) 'HI:',PXMN,PXMX,PYMN,PYMX

      CALL EZXWIND(PXMN,PXMX,PYMN,PYMX,xmin,xmax,ymin,ymax)!SET UNITS OF WINDOW
      
      CALL EZXMOVE(0.,re)                        !DRAW SURFACE OF EARTH
      DO I = 1, dmax
       dd = float(I)/r2d
       xx = re*sin(dd)
       yy = re*cos(dd)
       CALL EZXDRAW(xx,yy)
      END DO

      CALL EZXMOVE(0.,rcmb)                      !DRAW CORE MANTLE BOUNDARY
      DO I = 1, dmax
       dd = float(I)/r2d
       xx = rcmb*sin(dd)
       yy = rcmb*cos(dd)
       CALL EZXDRAW(xx,yy)
      END DO

      CALL EZXMOVE(0.,ricb)                      !DRAW INNER-CORE BOUNDARY
      DO I = 1, dmax
       dd = float(I)/r2d
       xx = ricb*sin(dd)
       yy = ricb*cos(dd)
       CALL EZXDRAW(xx,yy)
      END DO

      IF (ymin.EQ.0.) THEN                       !DRAW LINE FROM CENTER
       CALL EZXMOVE(xmin,0.)
       CALL EZXDRAW(xmax,0.)
      END IF
      IF (xmin.EQ.0.) THEN                       !DRAW LINE FROM CENTER
       CALL EZXMOVE(0.,ymin)
       CALL EZXDRAW(0.,ymax)
      END IF
      
      WRITE(6,*) 'HI THERE:'
      
      RETURN
      END

      SUBROUTINE vtrace2(r,d,ndat,col)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE TAKES THE A TRACE IN RADIUS AND DISTANCE AND PLOTS  |   C
C   |     ALL, ndat, POINTS IN THE COLOR, col, SELECTED, USING EZXPLOT.  |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE ASSUMES THAT THE WINDOW IS ALREADY SET UP AND THAT  |   C
C   |     THE SPACING/SCALING IS ALREADY SET.  PLUS THAT YOU CLOSE THE   |   C
C   |     WINDOW AFTER THE SUBROUTINE.                                   |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      PARAMETER (NMAX=100000)
      REAL       r(*),d(*),x(NMAX),y(NMAX)              !CILINDRICAL/CARTESIAN
      INTEGER    ndat,np,quad,col

      IF (ndat.GT.NMAX) THEN                            !CHECK ARRAY DIMENSIONS
       WRITE(6,*)'PLOT OF TRACE TRUNCATED AT:',NMXI
       np = NMAX
      ELSE
       np = ndat
      END IF
      
      CALL EZXCOL(col)
      
      re   = 1737.                                      !SET RADIUS OF EARTH
      r660 = re - 660.                                  !SET 660-KM RADIUS
      r410 = re - 410.                                  !SET 410-KM RADIUS
      rcmb = re - 1380.                                 !SET CMB RADIUS 
      ricb = re - 1637.                                 !SET ICB RADIUS 
      pi   = atan(1.)*4.                                !SET PI
      r2d  = 180./pi                                    !CONV RADIANS TO DEGREES
      DO I = 1, np
       x(I) = r(I)*sin(d(I))                            !CONV POLAR TO CARTESIAN
       y(I) = r(I)*cos(d(I))
      END DO

      CALL EZXMOVE(x(1),y(1))                         !DRAW THE RAY PATH
      DO I = 2, np
       CALL EZXDRAW(x(I),y(I))
      END DO
      
      RETURN
      END

      SUBROUTINE pstrace2(r,d,ndat,col)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE PLOTS THE LAYERS OF THE EARTH TO A PS FILE FOR (1) A|   C
C   |     PIE SHAPED EARTH, 2 A HALF SHAPED EARTH, 3 A ALL BUT PIE SHAPED|   C
C   |     EARTH, AND 4 A CIRCULAR EARTH.                                 |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE ASSUMES THAT THE WINDOW IS ALREADY SET UP AND THAT  |   C
C   |     THE SPACING/SCALING IS ALREADY SET.  PLUS THAT YOU CLOSE THE   |   C
C   |     WINDOW AFTER THE SUBROUTINE.                                   |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      PARAMETER (NMAX=100000)
      REAL       r(*),d(*),x(NMAX),y(NMAX)
      INTEGER    ndat,np,quad,col

      IF (ndat.GT.NMAX) THEN                            !CHECK ARRAY DIMENSIONS
       WRITE(6,*)'PLOT OF TRACE TRUNCATED AT:',NMXI
       np = NMAX
      ELSE
       np = ndat
      END IF
      
      CALL PSCOL(col)
      
      re   = 1737.                                      !SET RADIUS OF EARTH
      r660 = re - 50.                                  !SET 660-KM RADIUS
      r410 = re - 410.                                  !SET 410-KM RADIUS
      rcmb = re - 1380.                                 !SET CMB RADIUS 
      ricb = re - 1637.                                 !SET ICB RADIUS 
      pi   = atan(1.)*4.                                !SET PI
      r2d  = 180./pi                                    !CONV RADIANS TO DEGREES
      DO I = 1, np
       x(I) = r(I)*sin(d(I))                            !CONV POLAR TO CARTESIAN
       y(I) = r(I)*cos(d(I))
      END DO

      CALL PSMOVE(x(1),y(1))                         !DRAW THE RAY PATH
      DO I = 2, np
       CALL PSDRAW(x(I),y(I))
      END DO
      
      RETURN
      END




      SUBROUTINE trmaster3(imodel,del,qdep,phs,plotyn,quad
     &                   ,z,va,vb,p,r,d,t,vt,ie)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE RUNS THE A SET OF SUBROUTINES THAT CALCULATE THE RAY|   C
C   |     PATH FOR ANY NUMBER OF PHASES:                                 |   C
C   |                                                                    |   C
C   |THIS SUBROUTINE USES A SET OF SUBROUTINES THAT CAN BE ACCESSED W/O  |   C
C   |     THE PROGRAM.  THAT WAY OTHER PROGRAMS CAN USE ASPECTS AND      |   C
C   |     ALL UPDATES ARE DONE IN ONE PLACE.  I UNDERSTAND THAT THIS ADDS|   C
C   |     SEVERAL LEVELS OF COMPLEXITY, BUT IT MAKES LIFE EASIER IN THE  |   C
C   |     END.                                                           |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      PARAMETER     (NLMX = 12742,NDAT=NLMX*40)        !MAX NUMBER OF MODEL LAYERS
      REAL           z(*),va(*),vb(*)
      INTEGER        nlyr,ie,phmx,plotyn,quad,nodiff
      CHARACTER*80   DIR,phase(100),phs,phn,ph,pdiff2,phas
      REAL           r(*),d(*),t(*),vt(*)
      REAL           tt,dtdh,dtddel,p,dec,pi,r2d,del,qdep
      pi = atan(1.)*4.
      r2d = 180./pi


      IPHS = 0                              !WORK WITH PHASE INFO
      CALL ttload2(DIR,phase,phmx)          !LOAD POSSIBLE PHASES & DIRECTORY
      DO I = 1, phmx                        !DETERMINE WHICH MATCHES INPUT
       phn = phase(I)
       IF (phs(1:9).EQ.phn(1:9)) IPHS = I
      END DO
      IF (IPHS.EQ.0) THEN                   !RETURN IF NO PHASE CHOSEN
       WRITE(6,*) 'NO PHASE MATCHES SELECTION',phs(1:9)
       err = -1
       RETURN
      END IF                                !IF NONE MATCH REQUIRE RE-ENTRY
      CALL ialen(DIR,iblank)                !GET NON-SPACE LENGTH OF DIRECTORY 
      phn = DIR(1:iblank)//phase(IPHS)      !TOTAL PHASE FILE NAME WITH DIRECT      
 
      period = 1
      IF (imodel.EQ.1) THEN                 !FOR PREM ADJUST FOR PERIOD
       WRITE(6,*) 'ENTER THE PERIOD ACCOUNTING FOR Q IN PREM:'
       READ (5,*) period
      END IF
      dec = 1.0                               !VELOCITY DECIMAITON = 1 KM

      CALL GET_MODS(imodel,period,dec,z,va,vb,nlyr)!INPUT VELOCITY MODEL
      CALL GET_TTJ(phn,1,del,qdep,tt,dtdh,dtddel,iflag)!GET TRAVEL TIME/RAY PAR

      pdiff2 = 'diff   '
      CALL FINDCHAR2(phase(IPHS),pdiff2,nodiff) !FIND CHARACTERS 'diff'
      p = dtddel-0.1                        !SET LOW END OF RAY PARATER SEARCH 
      fac  = 0.02                           !SET SEARCH DECIMATION
      erro = 999.                           !SET ERRORS HIGH TO START
      err  = 99.
      ddel = del-98.
      IF (ddel.LT.0) ddel = 0.
      fac2  =0.01
      CNT = 0
      sign = 1.


      DO WHILE (err.GT.0.01)                !ITTERATE OVER RAY PARAMETER
       CNT = CNT + 1
       IF (CNT.GT.200) GOTO 989

       IF (nodiff.EQ.0) THEN                !NON-DIFFRACTED WAVES:
        ddel = 0.
        IF (err.LE.erro) THEN               !IF ITTERATION BETTER DISTANCE ->
         po   = p                           !    CONTINUE
         p    = p + fac
 	 erro = err
        ELSE                                !IF ITTERATION WORSE -> BACK UP &
         fac  =-fac/2.                      !    USE SMALLER STEP SIZES
         IF (abs(fac).LT.0.0001) fac = fac*1.25
	 p    = po + fac
         erro = err
        END IF

       ELSE                                 !DIFFRACTED WAVES:
        p = 9.
	ddelo = ddel
        IF (CNT.GT.1) ddel  = ddel+ (del- d(ie)*r2d)
        erro  = err
	WRITE(6,*) del,d(ie)
       END IF
       IF (ddel.LT.0) ddel = 0.
       IF (p.GE.0.) THEN                   !DISALLOW NEGATIVE RAY PARAMETER
        ie = 0
        CALL trph(p,va,vb,z,r,d,t,vt,qdep,dec,dr,phase(IPHS)
     &           ,ie,ddel,imodel)
        WRITE(77,*) d(ie)*r2d,t(ie)
       ELSE 
        ie    = 0.
	d(ie) = 9999.
	WRITE(6,*) 'WARNING:  NEGATIVE RAY PARAMETER!!!'
       END IF
       IF (ie.LE.0) RETURN
       err = abs(d(ie)*r2d-del)            !DETERMINE THE ERROR
       WRITE(6,*) 'HI:',d(ie)*r2d,del,err,CNT,fac,p
      END DO
      WRITE(6,*) p,CNT,err,del,d(ie)
989   RETURN
      END




      SUBROUTINE FINDCHAR2(string1,string2,yn)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE DETERMINES IF THE SECOND STRING IS CONTAINED IN THE |   C
C   |     FIRST STRING AND RETURNS yn = 1 IF FOUND:                      |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      CHARACTER*(*)       string1
      CHARACTER*(*)       string2
      INTEGER             yn,iblank1,iblank2
      
      yn = 0
      
      CALL ialen(string1,iblank1)           !GET CHARACTER STRING LENGTH: NO " "
      CALL ialen(string2,iblank2)           !GET CHARACTER STRING LENGTH: NO " "
      IF (iblank2.LE.iblank1) THEN
       DO I = 1, iblank1-iblank2+1          !COMPARE VARIABLES AT EACH SPACING
        IF (string1(I:I+iblank2-1).EQ.string2(1:iblank2)) THEN
         yn = 1                             !STOP SEARCH IF STRING FOUND
         RETURN
        END IF
       END DO
       
      END IF

      RETURN
      END

      SUBROUTINE psload(quad)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE PLOTS THE LAYERS OF THE EARTH TO A PSFILE FOR (1) A |   C
C   |     PIE SHAPED EARTH, 2 A HALF SHAPED EARTH, 3 A ALL BUT PIE SHAPED|   C
C   |     EARTH, AND 4 A CIRCULAR EARTH.                                 |   C
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      PARAMETER (NMAX=100000)
      INTEGER    ndat,np,quad,col

      IF (ndat.GT.NMAX) THEN                          !CHECK ARRAY DIMENSIONS
       WRITE(6,*)'PLOT OF TRACE TRUNCATED AT:',NMXI
       np = NMAX
      ELSE
       np = ndat
      END IF
      re   = 1737.                                    !SET RADIUS OF EARTH
      r660 = re - 660.                                !SET 660-KM RADIUS
      r410 = re - 410.                                !SET 410-KM RADIUS
      rcmb = re - 1380.                               !SET CMB RADIUS 
      ricb = re - 1637.                               !SET ICB RADIUS 
      pi   = atan(1.)*4.                              !SET PI
      r2d  = 180./pi                                  !CONV RADIANS TO DEGREES
      CALL PSCOL(1)
      ymin = 0.                                       !SET BOUNDS OF PLOT
      ymax = re 
      xmin = 0.
      xmax = re
      dmax = 90.*float(quad)                          !BOUNDS SET BY # QUADRANTS
      IF (quad.GT.1) ymin = -re
      IF (quad.GT.2) xmin = -re

      ry = (ymax-ymin)/re*2.50                        !ADJUST PLOT RANGE
      rx = (xmax-xmin)/re*2.50
      PXMN = 6.00-rx                                  !SET PLOT SIZE
      PXMX = PXMN+rx
      PYMN = 7.00-ry
      PYMX = PYMN+ry

      CALL PSWIND(PXMN,PXMX,PYMN,PYMX,xmin,xmax,ymin,ymax)!SET UNITS OF WINDOW
      CALL PSCOL(1)
      
      CALL PSMOVE(0.,re)                             !DRAW SURFACE OF EARTH
      DO I = 1, dmax
       dd = float(I)/r2d
       xx = re*sin(dd)
       yy = re*cos(dd)
       CALL PSDRAW(xx,yy)
      END DO
      CALL PSPS('stroke')

C      CALL PSMOVE(0.,r660)                           !DRAW 660-KM DISCONTINUITY
C      DO I = 1, dmax
C       dd = float(I)/r2d
C       xx = r660*sin(dd)
C       yy = r660*cos(dd)
C       CALL PSDRAW(xx,yy)
C      END DO
C      CALL PSPS('stroke')

C      CALL PSMOVE(0.,r410)                           !DRAW 410-KM DISCONTINUITY
C      DO I = 1, dmax
C       dd = float(I)/r2d
C       xx = r410*sin(dd)
C       yy = r410*cos(dd)
C       CALL PSDRAW(xx,yy)
C      END DO
C      CALL PSPS('stroke')

C      CALL PSMOVE(0.,ricb)                           !DRAW INNER-CORE BOUNDARY
C      DO I = 1, dmax
C       dd = float(I)/r2d
C       xx = ricb*sin(dd)
C       yy = ricb*cos(dd)
C       CALL PSDRAW(xx,yy)
C      END DO
C      CALL PSPS('stroke')

      CALL PSMOVE(0.,rcmb)                           !DRAW CORE MANTLE BOUNDARY
      DO I = 1, dmax
       dd = float(I)/r2d
       xx = rcmb*sin(dd)
       yy = rcmb*cos(dd)
       CALL PSDRAW(xx,yy)
      END DO
      CALL PSPS('stroke')

      IF (ymin.EQ.0.) THEN                            !DRAW LINE FROM CENTER
       CALL PSMOVE(xmin,0.)
       CALL PSDRAW(xmax,0.)
      END IF
      IF (xmin.EQ.0.) THEN                            !DRAW LINE FROM CENTER
       CALL PSMOVE(0.,ymin)
       CALL PSDRAW(0.,ymax)
      END IF
      CALL PSPS('stroke')
      
      RETURN
      END









      SUBROUTINE ttload2(DIR,pfile,pmx)
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
C   |THIS SUBROUTINE SETS UP THE DIRECTORY & FILE NAMES OF PRE-BUILT     |   C
C   |     TRAVEL TIME TABLES.  THESE TRAVEL TIME TABLES ARE BUILT USING  |   C
C   |     PETER SHEARER'S autotable PROGRAM.
C   | --- --------- --------- --------- --------- --------- --------- -- |   C
      CHARACTER*80 DIR,pfile(100)
      INTEGER      pmx
      DIR      ='./TT/ttnm.'
      pfile(01)='P'   !  SET TRAVEL TIME TABLE NAMES
      pfile(02)='PP'
      pfile(03)='P3'
      pfile(04)='P4'
      pfile(05)='P5'
      pfile(06)='P410P'
      pfile(07)='P660P'
      pfile(08)='P410s'
      pfile(09)='P660s'
      pfile(10)='PcP'
      pfile(11)='PcP2'
      pfile(12)='Pdiff'
      pfile(13)='PKP'
      pfile(14)='PKiKP'
      pfile(15)='PKKP'
      pfile(16)='PKPDF'
      pfile(17)='PKJKP'
      pfile(18)='PS'
      pfile(19)='S'    
      pfile(20)='SS'
      pfile(21)='S3'
      pfile(22)='S4'
      pfile(23)='SS'
      pfile(24)='S410S'
      pfile(25)='S660S'
      pfile(26)='S410p'
      pfile(27)='S660p'
      pfile(28)='ScS'
      pfile(29)='ScP'
      pfile(30)='Sdiff'
      pfile(31)='SKiKP'
      pfile(32)='SKS'
      pfile(33)='Ppdp'
      pmx = 33
      RETURN
      END


