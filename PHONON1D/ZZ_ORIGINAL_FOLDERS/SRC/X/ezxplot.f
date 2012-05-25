c EZXPLOT routines in progress July, 1993

c EZXINIT creates an X window for plotting
c   Inputs:  nx = number of x pixels
c            ny = number of y pixels
      SUBROUTINE EZXINIT(nx,ny)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      CALL linitializewindow(nx,ny)
      CALL lsetcolor(1)          !set to black
      CALL llinestyle(0,0,0,0)   !set to fastest line width 
      nxpix=nx
      nypix=ny     
      RETURN
      END SUBROUTINE EZXINIT

c EZXQUIT exits from X window after user hits mouse key
      SUBROUTINE EZXQUIT
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      print *,'Hit mouse key to exit X window'
      CALL EZXMESS('Hit mouse key to exit X window')
      CALL ldoit
      CALL lwaitforbuttondown(ibut,ix,iy,mod)
      CALL ldie
      RETURN
      END SUBROUTINE EZXQUIT

c EZXWIND defines local coordinates for plotting
c   Inputs:  x1,x2,y1,y2  =  reference rectangle in pixels
c            x3,x4,y3,y4  =  local values at boundaries of rectangle
      SUBROUTINE EZXWIND(x1,x2,yy1,yy2,x3,x4,y3,y4)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      IF (x3.eq.x4.or.y3.eq.y4) then
         print *,'***Error in EZXWIND.  Singular coordinates'
         print *,'x3,x4,y3,y4 = ',x3,x4,y3,y4
      END IF
      y1=nypix-yy1
      y2=nypix-yy2
      axplot=(x2-x1)/(x4-x3)
      ayplot=(y2-y1)/(y4-y3)
      bxplot=x1-x3*axplot
      byplot=y1-y3*ayplot
      xfrm1=x3
      xfrm2=x4
      yfrm1=y3
      yfrm2=y4
      RETURN
      END SUBROUTINE EZXWIND

c EZXMOVE moves current pen position without drawing line
c   Inputs:  x,y  =  coordinates of pen      
      SUBROUTINE EZXMOVE(x,y)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      xpen=axplot*x+bxplot
      ypen=ayplot*y+byplot
      CALL lmoveto(nint(xpen),nint(ypen))
      RETURN
      END SUBROUTINE EZXMOVE

c EZXDRAW draws line from current pen position to new pen position
c   Inputs:  x,y  =  new pen position
      SUBROUTINE EZXDRAW(x,y)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      xpen=axplot*x+bxplot
      ypen=ayplot*y+byplot
      CALL llineto(nint(xpen),nint(ypen))
      RETURN
      END SUBROUTINE EZXDRAW

c EZXPOLY draws polygon 
      SUBROUTINE EZXPOLY(x,y,n)
      parameter (nbuf=5000)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      REAL x(*),y(*)
      integer*4 ix(nbuf),iy(nbuf)
      j=0
      DO 10 i=1,n
         j=j+1
         ix(j)=nint(axplot*x(i)+bxplot)
         iy(j)=nint(ayplot*y(i)+byplot)
         IF (j.eq.nbuf) then
            CALL llines(ix,iy,nbuf)
            ix(1)=ix(nbuf)
            iy(1)=iy(nbuf)
            j=1
         END IF
10    continue
      IF (j.ne.1) CALL llines(ix,iy,j)
      xpen=axplot*x(n)+bxplot
      ypen=ayplot*y(n)+byplot
      RETURN
      END SUBROUTINE EZXPOLY


c EZXPOLYFILL draws filled polygon 
      SUBROUTINE EZXPOLYFILL(x,y,n)
      parameter (nbuf=5000)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      REAL x(*),y(*)
      integer*4 ix(nbuf),iy(nbuf)
      j=0
      DO 10 i=1,n
         j=j+1
         ix(j)=nint(axplot*x(i)+bxplot)
         iy(j)=nint(ayplot*y(i)+byplot)
10    continue
      IF (j.ne.1) CALL lfillpolygon(ix,iy,j,1,0)
      xpen=axplot*x(n)+bxplot
      ypen=ayplot*y(n)+byplot
      RETURN
      END SUBROUTINE EZXPOLYFILL


c EZXCIR draws circle of specified color
c  x,y = user coordinates of center
c  irad = radius in pixels
c  icol = color, 0=wht,1=blk,2=red,3=blu,4=yel,5=grn,6=brw,7=pur,8=cyn,9=mag
c  ifill = 0 for no fill, =1 for fill
      SUBROUTINE EZXCIR(x,y,irad,icol,ifill)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      xpen=axplot*x+bxplot
      ypen=ayplot*y+byplot
      CALL lsetcolor(icol)      
      IF (ifill.eq.0) then
         CALL ldrawhollowcircle(nint(xpen),nint(ypen),irad)
      ELSE
         CALL ldrawfilledcircle(nint(xpen),nint(ypen),irad)
      END IF
      RETURN
      END SUBROUTINE EZXCIR


c EZXLINE defines line width and dash length
c  iwid  = line width in pixels
c        = 0 for thinnest, fastest line
c  idash = dash length in pixels
c        = 0 for solid line
      SUBROUTINE EZXLINE(iwid,idash)
      CALL llinestyle(iwid,idash,0,0)
      RETURN
      END SUBROUTINE EZXLINE

c EZXCOL defines plotting color
c  icol = color, 0=wht,1=blk,2=red,3=blu,4=yel,5=grn,6=brw,7=pur,8=cyn,9=mag
      SUBROUTINE EZXCOL(icol)
      CALL lsetcolor(icol)
      RETURN
      END SUBROUTINE EZXCOL

c EZXFRAM draws rectangle on screen
c   Inputs:  x1,x2,y1,y2  =  coordinates of rectangle
      SUBROUTINE EZXFRAM(x1,x2,y1,y2)
      CALL EZXMOVE(x1,y1)
      CALL EZXDRAW(x2,y1)
      CALL EZXDRAW(x2,y2)
      CALL EZXDRAW(x1,y2)
      CALL EZXDRAW(x1,y1)
      RETURN
      END SUBROUTINE EZXFRAM

c EZXCLR erases the screen
      SUBROUTINE EZXCLR
      CALL lclearwindow
      RETURN
      END SUBROUTINE EZXCLR

c EZXCLRB erases the screen within a rectangle
c   Inputs:  x1,x2,y1,y2 = boundary of rectangle
      SUBROUTINE EZXCLRB(x1,x2,y1,y2)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      xx1=axplot*x1+bxplot
      xx2=axplot*x2+bxplot
      yy1=ayplot*y1+byplot
      yy2=ayplot*y2+byplot
      dx=xx2-xx1
      dy=yy1-yy2
      CALL lclearpartwindow(nint(xx1),nint(yy2),nint(dx),nint(dy))
      RETURN
      END SUBROUTINE EZXCLRB

c EZXBOX draws a filled box in current color
c   Inputs:  x1,x2,y1,y2 = boundary of rectangle
      SUBROUTINE EZXBOX(x1,x2,y1,y2)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      xx1=axplot*x1+bxplot
      xx2=axplot*x2+bxplot
      yy1=ayplot*y1+byplot
      yy2=ayplot*y2+byplot
      dx=xx2-xx1
      dy=yy1-yy2
      CALL ldrawfilledbox(nint(xx1),nint(yy2),nint(dx),nint(dy))
      RETURN
      END SUBROUTINE EZXBOX

c EZXLOADFONT loads desired font name
c   Input:  name  =  string defining font name
c   RETURNs:   i  =  index for font to be used by EZXSETFONT
      SUBROUTINE EZXLOADFONT(i,name)
      character*(*) name
      CALL lloadfont(i,name)
      RETURN
      END SUBROUTINE EZXLOADFONT
      
c EZXSETFONT sets font
c   Input:  i  =  font index (defined by EZXLOADFONT)
      SUBROUTINE EZXSETFONT(i)
      CALL lsetfont(i)
      RETURN
      END SUBROUTINE EZXSETFONT

c EZXLAB writes text at current pen position
c   Input:  text = desired text
      SUBROUTINE EZXLAB(text)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      character*(*) text
      character text2*200
      m=min(len(text),200)
      CALL DEWEIRD(text,m,text2)
      DO 10 i=m,1,-1
         IF (text2(i:i).ne.' ') go to 20
10    continue
20    mm=i             !length of string without blanks
      CALL lprint(text2(1:mm))
      RETURN
      END SUBROUTINE EZXLAB

      SUBROUTINE DEWEIRD(text1,n,text2)
      character*1 text1(n),text2(n)
      character abc*52,num*40
      save abc,num
      data abc/'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'/
! I removed \ from this line because it gives unknown escape sequence warning
!      data num/'0123456789!@#$%^&*()\-=|_+[]{};:"~,./<>?'/
      data num/'0123456789!@#$%^&*()--=|_+[]{};:"~,./<>?'/
      DO 50 i=1,n
         text2(i)=' '
         DO 30 j=1,52
            IF (text1(i).eq.abc(j:j)) go to 45
30       continue
         DO 40 j=1,40
            IF (text1(i).eq.num(j:j)) go to 45
40       continue
         go to 50
45       text2(i)=text1(i)
50    continue
      RETURN
      END SUBROUTINE DEWEIRD

c EZXNUM write number at current pen position
c   Inputs:  fp  =  number (real)
c            fmt =  desired format (e.g., 'f5.2' or 'i3')
      SUBROUTINE EZXNUM(fp,fmt)
      character*(*) fmt
      character*20 fmt1,format,text,textnb
      IF (fmt(1:1).eq.' ') go to 900
      fmt1='                    '
      fmt1=fmt
      IF (fmt1(1:1).eq.' ') go to 900
      DO 10 i=1,20
         IF (fmt1(i:i).eq.' ') go to 11
10    continue
      go to 900
11    nc=i-1                  !last nonblank character
      format(1:1)='('
      format(2:nc+1)=fmt(1:nc)
      format(nc+2:nc+2)=')'
      IF (fmt(1:1).eq.'I'.or.fmt(1:1).eq.'i') then
         ifp=nint(fp)
         write (text,format) ifp
      ELSE
         write (text,format) fp
      END IF
      nb=0
      nc=0
      DO 20 i=1,20
         IF (text(i:i).eq.' ') go to 20
         IF (nb.eq.0) nb=i
         nc=i
20    continue
      ncnb=nc-nb+1
      IF (ncnb.le.0) go to 900
      textnb(1:ncnb)=text(nb:nc)
      CALL EZXLAB(textnb(1:ncnb))
900   RETURN
      END SUBROUTINE EZXNUM

c EZXANG sets angle to draw text
c   Input:  tang  =  angle of text (degrees c.w. from horizontal)
c                 =   0. for normal
c                 =  90. for up
c                 = 270. for down
      SUBROUTINE EZXANG(tang)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      CALL ltextangle(tang)
      RETURN
      END SUBROUTINE EZXANG

c EZXLORG allows the user to specify how text or numbers will be
c centered around the current pen position.  This will affect
c EZXLAB and EZXNUM.
c                          
c                 7xxxxxx8xxxxxx9
c     nlorg=      4      5      6     centers block as shown
c                 1xxxxxx2xxxxxx3
c
      SUBROUTINE EZXLORG(nlorg)
      CALL laligntext(nlorg)
      RETURN
      END SUBROUTINE EZXLORG

c EZXTIC moves pen position nx pixels to the right and ny pixels up
c   Input:  nx,ny = number of pixels to move
c           nup   = 0 for move (no draw)
c           nup   = 1 for drawing line
      SUBROUTINE EZXTIC(nx,ny,nup)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      xoff=float(nx)
      yoff=float(ny)
      xpen2=xpen+xoff
      ypen2=ypen-yoff
      IF (nup.eq.1) then
         CALL llineto(nint(xpen2),nint(ypen2))
      ELSE
         CALL lmoveto(nint(xpen2),nint(ypen2))
      END IF
      xpen=xpen2
      ypen=ypen2
      RETURN
      END SUBROUTINE EZXTIC

c EZXAXES makes a frame with tics and tic labels as specified.
c xtic   =  small tic increment
c xlab   =  big tic increment (length is 2*nxticl)
c nxticl =  small tic length in pixels
c xfrmt  =  format for x axis numbers (next to big tics)
c etc.
      SUBROUTINE EZXAXES(xtic,xlab,nxticl,xfrmt,
     &                   ytic,ylab,nyticl,yfrmt)
      REAL          y,ystart,y2,ylab
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      character*(*) xfrmt,yfrmt
      character*20 xfrmt1,yfrmt1
      INTEGER      I,NX
      xfrmt1=xfrmt
      yfrmt1=yfrmt
      x1=xfrm1
      x2=xfrm2
      y1=yfrm1
      y2=yfrm2
      IF (x1.gt.x2) then             !switch IF reverse order
         scr=x1
         x1=x2
         x2=scr
      END IF
      IF (y1.gt.y2) then             !switch IF reverse order
         scr=y1
         y1=y2
         y2=scr
      END IF
      CALL EZXFRAM(x1,x2,y1,y2)
      scrx1=x1-1./axplot         
      scrx2=x2+1./axplot
      scry1=y1-1./ayplot
      scry2=y2+1./ayplot
      CALL EZXFRAM(scrx1,scrx2,scry1,scry2)       !offset frame for bold axes
c
      xstart=float(int(abs(x1)/xtic+0.999999))*xtic
      IF (x1 < 0.) xstart=-xstart+xtic
      y0=yfrm1
      nx = int((x2-xstart)/xtic)+1
      DO I = 1, nx
         x=xstart+xtic*float(I-1)
         CALL EZXMOVE(x,y0)
         CALL EZXTIC(0,nxticl,1)
      END DO
      
      xstart=float(int(abs(x1)/xlab+0.999999))*xlab
      IF (x1.lt.0.) xstart=-xstart+xlab
      CALL EZXLORG(8)
      nx = int((x2-xstart)/xlab)+1
      DO I = 1, nx
         x=xstart+xtic*float(I-1)
         CALL EZXMOVE(x,y0)
         CALL EZXTIC(0,2*nxticl,1)
         CALL EZXMOVE(x,y0)
         CALL EZXTIC(0,-3,0)
         IF (xfrmt1(1:1).ne.' ') CALL EZXNUM(x,xfrmt1)
      END DO

      ystart=float(int(abs(y1)/ytic+0.999999))*ytic
      IF (y1.lt.0.) ystart=-ystart+ytic
      x0=xfrm1
      ny = int((y2-ystart)/ytic)+1
      DO I = 1, nx
         y = ystart + ytic*float(I-1)
         CALL EZXMOVE(x0,y)
         CALL EZXTIC(nyticl,0,1)
      END DO
      ystart=float(int(abs(y1)/ylab+0.999999))*ylab
      IF (y1.lt.0.) ystart=-ystart+ylab
      CALL EZXLORG(6)
      ny = int((y2-ystart)/ylab)+1
      DO I = 1, nx
         y = ystart + ylab*float(I-1)
         CALL EZXMOVE(x0,y)
         CALL EZXTIC(2*nyticl,0,1)
         CALL EZXMOVE(x0,y)
         CALL EZXTIC(-5,0,0)
         IF (yfrmt1(1:1).ne.' ') CALL EZXNUM(y,yfrmt1)
      END DO
c
      CALL EZXLORG(1)
      CALL ldoit
      RETURN
      END SUBROUTINE EZXAXES

c EZXLAX puts x and y axes labels on axes
c   Inputs:  xlabel = x-axis label
c            nxoff  = number of pixels to offset label
c            etc.
      SUBROUTINE EZXLAX(xlabel,nxoff,ylabel,nyoff)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      character*(*) xlabel,ylabel
      integer*4 nxoff,nyoff
      x1=xfrm1
      x2=xfrm2
      y1=yfrm1
      y2=yfrm2
      CALL EZXLORG(8)
      xavg=(x1+x2)/2.
      CALL EZXMOVE(xavg,y1)
      CALL EZXTIC(0,-nxoff,0)
      CALL EZXLAB(xlabel)
      CALL EZXLORG(2)
      CALL EZXANG(90.)
      yavg=(y1+y2)/2.
      CALL EZXMOVE(x1,yavg)
      CALL EZXTIC(-nyoff,0,0)
      CALL EZXLAB(ylabel)
      CALL EZXANG(0.)
      CALL EZXLORG(1)
      RETURN
      END SUBROUTINE EZXLAX

c EZXTIT puts title above axes
c   Inputs:  title = title
c            nyoff  = number of pixels to offset label
      SUBROUTINE EZXTIT(title,nyoff)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      character*(*) title
      integer*4 nyoff
      x1=xfrm1
      x2=xfrm2
      y1=yfrm1
      y2=yfrm2
      CALL EZXLORG(2)
      xavg=(x1+x2)/2.
      CALL EZXMOVE(xavg,y2)
      CALL EZXTIC(0,nyoff,0)
      CALL EZXLAB(title)
      CALL EZXLORG(1)
      RETURN
      END SUBROUTINE EZXTIT



c EZXSYMB plots symbols at current pen position
c n=1  --  +
c n=2  --  square
c n=3  --  triangle
c n=4  --  inverted triangle
c n=5  --  X
c n=6  --  diamond
c npix = symbol half-height in pixels
      SUBROUTINE EZXSYMB(n,npix)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      IF (n.eq.1) then
         CALL EZXTIC(-npix,0,0)
         CALL EZXTIC(2*npix,0,1)
         CALL EZXTIC(-npix,-npix,0)
         CALL EZXTIC(0,2*npix,1)
         CALL EZXTIC(0,-npix,0)
      ELSE IF (n.eq.2) then
         CALL EZXTIC(-npix,-npix,0)
         CALL EZXTIC(2*npix,0,1)
         CALL EZXTIC(0,2*npix,1)
         CALL EZXTIC(-2*npix,0,1)
         CALL EZXTIC(0,-2*npix,1)
         CALL EZXTIC(npix,npix,0)
      ELSE IF (n.eq.3) then
         CALL EZXTIC(-npix,-npix,0)
         CALL EZXTIC(2*npix,0,1)
         CALL EZXTIC(-npix,2*npix,1)
         CALL EZXTIC(-npix,-2*npix,1)
         CALL EZXTIC(npix,npix,0)
      ELSE IF (n.eq.4) then
         CALL EZXTIC(npix,npix,0)
         CALL EZXTIC(-2*npix,0,1)
         CALL EZXTIC(npix,-2*npix,1)
         CALL EZXTIC(npix,2*npix,1)
         CALL EZXTIC(-npix,-npix,0)
      ELSE IF (n.eq.5) then
         CALL EZXTIC(-npix,-npix,0)
         CALL EZXTIC(2*npix,2*npix,1)
         CALL EZXTIC(-2*npix,0,0)
         CALL EZXTIC(2*npix,-2*npix,1)
         CALL EZXTIC(-npix,npix,0)
      ELSE
         CALL EZXTIC(0,-npix,0)
         CALL EZXTIC(npix,npix,1)
         CALL EZXTIC(-npix,npix,1)
         CALL EZXTIC(-npix,-npix,1)
         CALL EZXTIC(npix,-npix,1)
         CALL EZXTIC(0,npix,0)
      END IF
      RETURN
      END SUBROUTINE EZXSYMB

c EZXDIG RETURNs the mouse position when a mouse button is hit
c   RETURNs:  x,y =  local coordinates of mouse 
c             ibut = 1,2,3 for three mouse buttons
c                  = 4,5,6 for mouse buttons with shift key depressed
      SUBROUTINE EZXDIG(x,y,ibut)
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg
      CALL ldoit 
      CALL lwaitforbuttondown(ibut,ix,iy,imod)
      IF (imod.ne.0) ibut=ibut+3
      xpen=float(ix)
      ypen=float(iy)
      x=(xpen-bxplot)/axplot
      y=(ypen-byplot)/ayplot  
      RETURN
      END SUBROUTINE EZXDIG

c EZXMESS displays message at bottom of window
      SUBROUTINE EZXMESS(text)
      character*(*) text
      REAL          xpen,ypen,axplot,bxplot,ayplot,byplot
      REAL          xfrm1,xfrm2,yfrm1,yfrm2
      INTEGER       nxpix,nypix
      REAL          cang
      INTEGER       nctall,nopenseg
      COMMON/ezxcom/xpen,ypen,axplot,bxplot,ayplot,byplot,
     &             xfrm1,xfrm2,yfrm1,yfrm2,nxpix,nypix,
     &             cang,nctall,nopenseg 
      CALL ldoit
      CALL lclearpartwindow(0,nypix-20,nxpix,nypix)
      CALL EZXLORG(1)
      CALL EZXANG(0.)
      CALL lmoveto(1,nypix)
      CALL lprint(text)      
      RETURN
      END SUBROUTINE EZXMESS
      

c EZXNICE determines reasonable spacing for large
c and small tics for a given interval.
      SUBROUTINE EZXNICE(xmin,xmax,tic1,tic2)
      dx=abs(xmax-xmin)
      scale=10.**int(log10(dx))
      IF (dx.lt.1.) scale=scale/10.
      dxnorm=dx/scale               !should be between 1 and 10
      IF (dxnorm.le.1.1) then
         tic1=0.1
         tic2=0.2
      ELSE IF (dxnorm.le.2.1) then
         tic1=.1
         tic2=.5
      ELSE IF (dxnorm.le.4.1) then
         tic1=0.5
         tic2=1.0 
      ELSE
         tic1=1.0
         tic2=2.0
      END IF
      tic1=tic1*scale
      tic2=tic2*scale
      RETURN
      END SUBROUTINE EZXNICE
