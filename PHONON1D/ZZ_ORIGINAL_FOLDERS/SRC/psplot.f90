! PSPLOT subroutines make PostScript plotting easy.  They are
! similar, but not identical, to the EZPLOT routines.
!
! Note:  Output is to unit 17.  Don't use this unit in your program!
!
!                                    Peter Shearer
!                                    IGPP 0225, UCSD
!                                    La Jolla, CA 92093
!                                    pshearer@ucsd.edu
!
!
! PSFILE opens a postscript file with name filename 
      subroutine PSFILE(filename)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                 xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                 linecount,textangle
      character*(*) filename
      print *,'Creating psplot postscript file : ',filename
      open (17,file=filename)
      write (17,'(a18)') '%! PostScript file'
      write (17,*) '%------- definitions --------'
      write (17,*) '/m {moveto} def'
      write (17,*) '/d {lineto} def'
      write (17,*) '/ssfont {scalefont setfont} def' 
      write (17,*) '0.072 0.072 scale    % Coords are 1/1000 th inch'
      write (17,*) 
      write (17,*) '/h8 {/Helvetica findfont 111.11 ssfont} def'
      write (17,*) '/h9 {/Helvetica findfont 125 ssfont} def'
      write (17,*) '/h10 {/Helvetica findfont 138.9 ssfont} def'
      write (17,*) '/h11 {/Helvetica findfont 152.78 ssfont} def'
      write (17,*) '/h12 {/Helvetica findfont 166.67 ssfont} def'
      write (17,*) '/h14 {/Helvetica findfont 194.44 ssfont} def'
      write (17,*) '/h16 {/Helvetica findfont 222.22 ssfont} def'
      write (17,*) '/h18 {/Helvetica findfont 250 ssfont} def'
      write (17,*) '/h24 {/Helvetica findfont 333.33 ssfont} def'
      write (17,*) '/h36 {/Helvetica findfont 500 ssfont} def'
      write (17,*) '/h48 {/Helvetica findfont 666.67 ssfont} def'
      write (17,*) '/h72 {/Helvetica findfont 1000 ssfont} def'
      write (17,*) '/hb8 {/Helvetica-Bold findfont 111.11 ssfont} def'
      write (17,*) '/hb9 {/Helvetica-Bold findfont 125 ssfont} def'
      write (17,*) '/hb10 {/Helvetica-Bold findfont 138.9 ssfont} def'
      write (17,*) '/hb11 {/Helvetica-Bold findfont 152.78 ssfont} def'
      write (17,*) '/hb12 {/Helvetica-Bold findfont 166.67 ssfont} def'
      write (17,*) '/hb14 {/Helvetica-Bold findfont 194.44 ssfont} def'
      write (17,*) '/hb18 {/Helvetica-Bold findfont 250 ssfont} def'
      write (17,*) '/hb24 {/Helvetica-Bold findfont 333.33 ssfont} def'
      write (17,*) '/hb36 {/Helvetica-Bold findfont 500 ssfont} def'
      write (17,*) '/hb48 {/Helvetica-Bold findfont 666.67 ssfont} def'
      write (17,*) '/hb72 {/Helvetica-Bold findfont 1000 ssfont} def'     
      
      write (17,*) '/t8 {/Times-Roman findfont 111.11 ssfont} def'
      write (17,*) '/t9 {/Times-Roman findfont 125 ssfont} def'
      write (17,*) '/t10 {/Times-Roman findfont 138.9 ssfont} def'
      write (17,*) '/t11 {/Times-Roman findfont 152.78 ssfont} def'
      write (17,*) '/t12 {/Times-Roman findfont 166.67 ssfont} def'
      write (17,*) '/t14 {/Times-Roman findfont 194.44 ssfont} def'
      write (17,*) '/t18 {/Times-Roman findfont 250 ssfont} def'
      write (17,*) '/t24 {/Times-Roman findfont 333.33 ssfont} def'
      write (17,*) '/t36 {/Times-Roman findfont 500 ssfont} def'
      write (17,*) '/t48 {/Times-Roman findfont 666.67 ssfont} def'
      write (17,*) '/t72 {/Times-Roman findfont 1000 ssfont} def'
      write (17,*) '/tb8 {/Times-Bold findfont 111.11 ssfont} def'
      write (17,*) '/tb9 {/Times-Bold findfont 125 ssfont} def'
      write (17,*) '/tb10 {/Times-Bold findfont 138.9 ssfont} def'
      write (17,*) '/tb11 {/Times-Bold findfont 152.78 ssfont} def'
      write (17,*) '/tb12 {/Times-Bold findfont 166.67 ssfont} def'
      write (17,*) '/tb14 {/Times-Bold findfont 194.44 ssfont} def'
      write (17,*) '/tb18 {/Times-Bold findfont 250 ssfont} def'
      write (17,*) '/tb24 {/Times-Bold findfont 333.33 ssfont} def'
      write (17,*) '/tb36 {/Times-Bold findfont 500 ssfont} def'
      write (17,*) '/tb48 {/Times-Bold findfont 666.67 ssfont} def'
      write (17,*) '/tb72 {/Times-Bold findfont 1000 ssfont} def'
      write (17,*) '/ti8 {/Times-Italic findfont 111.11 ssfont} def'
      write (17,*) '/ti9 {/Times-Italic findfont 125 ssfont} def'
      write (17,*) '/ti10 {/Times-Italic findfont 138.9 ssfont} def'
      write (17,*) '/ti11 {/Times-Italic findfont 152.78 ssfont} def'
      write (17,*) '/ti12 {/Times-Italic findfont 166.67 ssfont} def'
      write (17,*) '/ti14 {/Times-Italic findfont 194.44 ssfont} def'
      write (17,*) '/ti18 {/Times-Italic findfont 250 ssfont} def'
      write (17,*) '/ti24 {/Times-Italic findfont 333.33 ssfont} def'
      write (17,*) '/ti36 {/Times-Italic findfont 500 ssfont} def'
      write (17,*) '/ti48 {/Times-Italic findfont 666.67 ssfont} def'
      write (17,*) '/ti72 {/Times-Italic findfont 1000 ssfont} def'
      write (17,*)            
      write (17,*) '/stringheight   %stack:string    ret:stringheight'
      write (17,*) '   {gsave currentpoint exch pop  %currnt y position'
      write (17,*) '    exch true charpath pathbbox exch pop'
      write (17,*) '    exch pop exch pop    %y pos of top of string'
      write (17,*) '    exch sub newpath grestore} def'
      write (17,*) 
      write (17,*) '/show1   %stack:string,angle'
      write (17,*) '   {gsave currentpoint translate rotate'
      write (17,*) '    show grestore} def'
      write (17,*) '/show2   %stack:string,angle'
      write (17,*) '   {gsave currentpoint translate rotate'
      write (17,*) '    dup stringwidth pop 2 div -1 mul 0 rmoveto'
      write (17,*) '    show grestore} def'
      write (17,*) '/show3   %stack:string,angle'
      write (17,*) '   {gsave currentpoint translate rotate'
      write (17,*) '    dup stringwidth pop -1 mul 0 rmoveto'
      write (17,*) '    show grestore} def'
      write (17,*) '/show4   %stack:string,angle'
      write (17,*) '   {gsave currentpoint translate rotate'
      write (17,*) '    dup stringheight -1 mul 2 div 0 exch rmoveto'
      write (17,*) '    show grestore} def'
      write (17,*) '/show5   %stack:string,angle'
      write (17,*) '   {gsave currentpoint translate rotate'
      write (17,*) '    dup dup stringheight exch stringwidth pop'
      write (17,*) '    2 div -1 mul exch -1 mul 2 div rmoveto'
      write (17,*) '    show grestore} def'
      write (17,*) '/show6   %stack:string,angle'
      write (17,*) '   {gsave currentpoint translate rotate'
      write (17,*) '    dup dup stringheight exch stringwidth pop'
      write (17,*) '    -1 mul exch -1 mul 2 div rmoveto'
      write (17,*) '    show grestore} def'
      write (17,*) '/show7   %stack:string,angle'
      write (17,*) '   {gsave currentpoint translate rotate'
      write (17,*) '    dup stringheight -1 mul 0 exch rmoveto'
      write (17,*) '    show grestore} def'
      write (17,*) '/show8   %stack:string,angle'
      write (17,*) '   {gsave currentpoint translate rotate'
      write (17,*) '    dup dup stringheight exch stringwidth pop'
      write (17,*) '    2 div -1 mul exch -1 mul rmoveto'
      write (17,*) '    show grestore} def'
      write (17,*) '/show9   %stack:string,angle'
      write (17,*) '   {gsave currentpoint translate rotate'
      write (17,*) '    dup dup stringheight exch stringwidth pop'
      write (17,*) '    -1 mul exch -1 mul rmoveto'
      write (17,*) '    show grestore} def'
      write (17,*) 
      write (17,*) '/cir %stack:r,hue'
      write (17,*) '  {gsave 1 1 sethsbcolor currentpoint'
      write (17,*) '  3 2 roll 0 360 arc'
      write (17,*) '  closepath fill grestore} def'
      write (17,*)
      write (17,*) '/cir3 %stack:r,hue,sat,bri'
      write (17,*) ' {gsave sethsbcolor currentpoint '
      write (17,*) '3 2 roll 0 360 arc'
      write (17,*) '    closepath fill grestore} def'
      write (17,*)
      write (17,*) '%------- initializations ---------'
      write (17,*) '0 setlinecap'
      write (17,*) '0 setlinejoin'
      call PSFONT('h14')
      call PSLINE(10.)
      call PSWIND(0.,1.,0.,1.,0.,1.,0.,1.)
      call PSLORG(1)      
      write (17,*) '%------------------------------- end PSPLOT header'
      write (17,*)

      linecount=0
      return
      end subroutine psfile


! PSFILE_SHORT opens a postscript file with name filename
!   This version uses a shorter header 
      subroutine PSFILE_SHORT(filename)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                 xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                 linecount,textangle
      character*(*) filename
      print *,'Creating psplot postscript file : ',filename
      open (17,file=filename)
      write (17,*) '%! PostScript file'
      write (17,*) '%------- definitions --------'
      write (17,*) '/m {moveto} def'
      write (17,*) '/d {lineto} def'
      write (17,*) '/ssfont {scalefont setfont} def' 
      write (17,*) '0.072 0.072 scale    % Coords are 1/1000 th inch'
      write (17,*)
      write (17,*) '/Helvetica findfont 194.44 scalefont setfont'
      write (17,*) 
      write (17,*) '/cir %stack:r,hue'
      write (17,*) '  {gsave 1 1 sethsbcolor currentpoint'
      write (17,*) '  3 2 roll 0 360 arc'
      write (17,*) '  closepath fill grestore} def'
      write (17,*)
      write (17,*) '/cir3 %stack:r,hue,sat,bri'
      write (17,*) ' {gsave sethsbcolor currentpoint '
      write (17,*) '3 2 roll 0 360 arc'
      write (17,*) '    closepath fill grestore} def'
      write (17,*)
      write (17,*) '%------- initializations ---------'
      write (17,*) '0 setlinecap'
      write (17,*) '0 setlinejoin'
      write (17,*) '%------------------------------- end PSPLOT header'      
      return
      end subroutine psfile_short
      

! PSLAND switches orientation of plot to horizontal (landscape) mode.
! This should be called immediately after PSFILE
! Note:  This is hardwired for standard size paper! 
      subroutine PSLAND
      write (17,*) '0 11000 translate -90 rotate'
      return
      end subroutine psland


! PSFONT specifies the current font.
!   text = 'h10' for 10-point Helvetica
!        = 'h14' for 14-point Helvetica
!        = 'hb12' for 12-point Helvetica-Bold
!        =  etc.
!   Note:  Only Helvetica (h8,h9,h10,h11,h12,h14,h18,h24) and
!          Helvetica-Bold (hb8,hb9,hb10,hb11,hb12,hb14,hb18,hb24)
!          are available with the PSFONT routine.  Other fonts
!          can be set directly (but note scaling to 1/000 inch scale)
!          with the PSPS subroutine or by adding to the header part of
!          the file defined with the PSFILE subroutine.  
      subroutine PSFONT(text)
      character*(*) text
      write (17,*) text
      return
      end subroutine psfont


! PSLINE sets current line width to flinewidth
      subroutine PSLINE(flinewidth)
      write (17,10) flinewidth,'setlinewidth'
10    format (f5.1,1x,a15)
      return
      end subroutine psline
!
!
      subroutine PSWIND(xx1,xx2,yy1,yy2,x3,x4,y3,y4)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                 xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                 linecount,textangle
      x1=xx1*1000.     !convert inches to 1/1000 th inches
      x2=xx2*1000.
      y1=yy1*1000.
      y2=yy2*1000.
      axplot=(x2-x1)/(x4-x3)
      ayplot=(y2-y1)/(y4-y3)
      bxplot=x1-x3*axplot
      byplot=y1-y3*ayplot
      xfrm1=x3
      xfrm2=x4
      yfrm1=y3
      yfrm2=y4
      return
      end subroutine pswind


      subroutine PSGRAY(graylevel)
      write (17,*) graylevel,' setgray'
      return
      end subroutine psgray


      subroutine PSHSB(hue,sat,bri)
      write (17,*) hue,sat,bri,' sethsbcolor'
      return
      end subroutine pshsb


      subroutine PSPS(text)
      character*(*) text
      write (17,*) text
      return
      end subroutine  psps


      subroutine PSPOS(x,y)   
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                   xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                   linecount,textangle
      xpen=axplot*x+bxplot
      ypen=ayplot*y+byplot
      return
      end  subroutine pspos


      subroutine PSMOVE(x,y)   
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                   xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                   linecount,textangle
      xpen=axplot*x+bxplot
      ypen=ayplot*y+byplot
      linecount=linecount+1
      if (linecount.gt.5000) then
         write (17,*) 'stroke'
         linecount=0
      end if
      write (17,*) nint(xpen),nint(ypen),' m'
      return
      end subroutine  psmove


      subroutine PSDRAW(x,y)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, & 
                   xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, & 
                   linecount,textangle
      xpen=axplot*x+bxplot
      ypen=ayplot*y+byplot
      write (17,*) nint(xpen),nint(ypen),' d'
      linecount=linecount+1
      if (linecount.gt.5000) then
         write (17,*) 'stroke'
         linecount=0
         write (17,*) nint(xpen),nint(ypen),' m'
         linecount=0
      end if
      return
      end  subroutine psdraw


      subroutine PSTROK
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                   xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                   linecount,textangle
      write (17,*) 'stroke'
      linecount=0
      return
      end  subroutine pstrok


      subroutine PSEND
      write (17,*) 'stroke'
      linecount=0
      write (17,'(a8)') 'showpage'
      write (17,'(a23)') '%!PS-Adobe-2.0 EPSF-1.2'
      close (17)
      return
      end subroutine psend


      subroutine PSEND_BB(x1,x2,y1,y2)
      write (17,*) 'stroke'
      linecount=0
      write (17,'(a8)') 'showpage'
      xx1=x1*72.
      xx2=x2*72.
      yy1=y1*72.
      yy2=y2*72.
      write (17,5) xx1,yy1,xx2,yy2
5     format ('%%BoundingBox: ',4f8.1)
      close (17)
      return
      end subroutine psend_bb


      subroutine PSFRAM(x1,x2,y1,y2)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, & 
                   xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, & 
                   linecount,textangle
      call PSMOVE(x1,y1)
      call PSDRAW(x1,y2)
      call PSDRAW(x2,y2)
      call PSDRAW(x2,y1)
      call PSDRAW(x1,y1)
      write (17,*) 'closepath stroke'
      linecount=0
      return
      end subroutine psfram


      subroutine PSTIC(x,y,nup)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, & 
                   xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, & 
                   linecount,textangle
      xpen=xpen+x*1000.
      ypen=ypen+y*1000.
      x1=(xpen-bxplot)/axplot
      y1=(ypen-byplot)/ayplot
      if (nup.eq.0) then
         call PSMOVE(x1,y1)
      else
         call PSDRAW(x1,y1)
      end if
      return
      end subroutine pstic
!
!
      subroutine PSLORG(n)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                 xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                 linecount,textangle
      nlorg=n
      return
      end subroutine pslorg


      subroutine PSANG(ang)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                 xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                 linecount,textangle
      textangle=ang
      return
      end subroutine psang


      subroutine PSLAB(text)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                   xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                   linecount,textangle
      character*(*) text
      character*100 text1
!      call PSTROK
      m=len(text)
      do 10 i=m,1,-1
         if (text(i:i).ne.' ') go to 20
10    continue
20    m=i             !length of string without blanks
      text1(1:1)='('
      text1(2:m+1)=text(1:m)
      text1(m+2:m+2)=')'
!      write (17,*) 'gsave 13.889 13.889 scale'
      itang=nint(textangle)
      if (nlorg.eq.1) then
         write (17,*) text1(1:m+2),' ',itang,' show1'
      else if (nlorg.eq.2) then
         write (17,*) text1(1:m+2),' ',itang,' show2'
      else if (nlorg.eq.3) then
         write (17,*) text1(1:m+2),' ',itang,' show3'         
      else if (nlorg.eq.4) then
         write (17,*) text1(1:m+2),' ',itang,' show4'         
      else if (nlorg.eq.5) then
         write (17,*) text1(1:m+2),' ',itang,' show5'         
      else if (nlorg.eq.6) then
         write (17,*) text1(1:m+2),' ',itang,' show6'
      else if (nlorg.eq.7) then
         write (17,*) text1(1:m+2),' ',itang,' show7'
      else if (nlorg.eq.8) then
         write (17,*) text1(1:m+2),' ',itang,' show8'
      else
         write (17,*) text1(1:m+2),' ',itang,' show9'
      end if
!      write (17,*) 'grestore'
30    format (a50,1x,f8.2,a6)
      call PSTROK        
      return
      end subroutine pslab


! PSNUMB write number at current pen position
!   Inputs:  fp  =  number (real)
!            fmt =  desired format (e.g., 'f5.2')
      subroutine PSNUMB(fp,fmt)
      character*(*) fmt
      character*20 fmt1,format,text,textnb
      if (fmt(1:1).eq.' ') go to 900
      if (fmt(1:1).eq.'(') then
         print *,'*** Do not use ( in format'
         return
      end if
      fmt1='                    '
      fmt1=fmt
      if (fmt1(1:1).eq.' ') go to 900
      do 10 i=1,20
         if (fmt1(i:i).eq.' ') go to 11
10    continue
      go to 900
11    nc=i-1                  !last nonblank character
      format(1:1)='('
      format(2:nc+1)=fmt(1:nc)
      format(nc+2:nc+2)=')'
      if (fmt(1:1).eq.'I'.or.fmt(1:1).eq.'i') then
         ifp=nint(fp)
         write (text,format) ifp
      else
         write (text,format) fp
      end if
      nb=0
      nc=0
      do 20 i=1,20
         if (text(i:i).eq.' ') go to 20
         if (nb.eq.0) nb=i
         nc=i
20    continue
      ncnb=nc-nb+1
      if (ncnb.le.0) go to 900
      textnb(1:ncnb)=text(nb:nc)
      call PSLAB(textnb(1:ncnb))
900   return
      end subroutine psnumb
     
! **** alternative name included to match EZXPLOT routine ***      
! PSNUM writes number at current pen position
!   Inputs:  fp  =  number (real)
!            fmt =  desired format (e.g., 'f5.2')
      subroutine PSNUM(fp,fmt)
      character*(*) fmt
      character*20 fmt1,format,text,textnb
      if (fmt(1:1).eq.' ') go to 900
      if (fmt(1:1).eq.'(') then
         print *,'*** Do not use ( in format'
         return
      end if
      fmt1='                    '
      fmt1=fmt
      if (fmt1(1:1).eq.' ') go to 900
      do 10 i=1,20
         if (fmt1(i:i).eq.' ') go to 11
10    continue
      go to 900
11    nc=i-1                  !last nonblank character
      format(1:1)='('
      format(2:nc+1)=fmt(1:nc)
      format(nc+2:nc+2)=')'
      if (fmt(1:1).eq.'I'.or.fmt(1:1).eq.'i') then
         ifp=nint(fp)
         write (text,format) ifp
      else
         write (text,format) fp
      end if
      nb=0
      nc=0
      do 20 i=1,20
         if (text(i:i).eq.' ') go to 20
         if (nb.eq.0) nb=i
         nc=i
20    continue
      ncnb=nc-nb+1
      if (ncnb.le.0) go to 900
      textnb(1:ncnb)=text(nb:nc)
      call PSLAB(textnb(1:ncnb))
900   return
      end subroutine psnum


      subroutine PSAXES(xtic,xlab,xticl0,xfrmt,ytic,ylab,yticl0,yfrmt)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                   xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                   linecount,textangle
      character*(*) xfrmt,yfrmt
      real          x,x1,x2,y1,y2
      
      xticl=abs(xticl0)
      yticl=abs(yticl0)
      x1=xfrm1
      x2=xfrm2
      y1=yfrm1
      y2=yfrm2
      if (x1.gt.x2) then    !switch if reverse order
         scr=x1
         x1=x2
         x2=scr
      end if
      if (y1.gt.y2) then    !switch if reverse order
         scr=y1
         y1=y2
         y2=scr
      end if
      call PSTROK
      call PSFRAM(x1,x2,y1,y2)
!
      xstart=float(int(abs(x1)/xtic+0.999999))*xtic
      if (x1.lt.0.) xstart=-xstart+xtic
      nx1 = int((x2-xstart)/xtic)+1
      do i=1, nx1
         x=xstart+float(i-1)*xtic
         call PSMOVE(x,yfrm1)
         call PSTIC(0.,xticl,1)
         if (xticl0.lt.0) then
            call PSMOVE(x,yfrm2)
            call PSTIC(0.,-xticl,1)
         end if
10    end do
      xstart=float(int(abs(x1)/xlab+0.999999))*xlab
      if (x1.lt.0.) xstart=-xstart
      iscr=8
      call PSLORG(iscr)
      nx1 = int((x2-xstart)/xlab)+1
      do i=1, nx1
         x=xstart+float(i-1)*xlab
         if (x.lt.x1) cycle
         call PSMOVE(x,yfrm1)
         fscr=1.6*xticl
         call PSTIC(0.,fscr,1)
         call PSPS('stroke')
         linecount=0
         if (xfrmt.ne.' ') then
            call PSPOS(x,yfrm1)
            call PSTIC(0.,-.05,0)
            call PSNUMB(x,xfrmt)
         end if
         if (xticl0.lt.0) then
            call PSMOVE(x,yfrm2)
            call PSTIC(0.,-fscr,1)
         end if     
20    end do
!
      ystart=float(int(abs(y1)/ytic+0.999999))*ytic
      if (y1.lt.0.) ystart=-ystart+ytic
      ny1 = int((y2-ystart)/ytic)+1
      do i=1, ny1
         y=ystart+float(i-1)*ytic
         call PSMOVE(xfrm1,y)
         call PSTIC(yticl,0.,1)
         if (yticl0.lt.0) then
            call PSMOVE(xfrm2,y)
            call PSTIC(-yticl,0.,1)            
         end if
30    end do
      ystart=float(int(abs(y1)/ylab+0.999999))*ylab
      if (y1.lt.0.) ystart=-ystart
      iscr=6
      call PSLORG(iscr)
      ny1 = int((y2-ystart)/ylab)+1
      do i=1, ny1
         y=ystart+float(i-1)*ylab
         if (y.lt.y1) cycle
         call PSMOVE(xfrm1,y)
         fscr=1.6*yticl
         call PSTIC(fscr,0.,1)
         call PSPS('stroke')
         linecount=0
         if (yfrmt.ne.' ') then
            call PSPOS(xfrm1,y)
            call PSTIC(-.05,0.,0)
            call PSNUMB(y,yfrmt)
         end if
         if (yticl0.lt.0) then
            call PSMOVE(xfrm2,y)
            call PSTIC(-fscr,0.,1)            
         end if
40    end do
!
      call PSLORG(1)
      call PSTROK
      return
      end subroutine psaxes


      subroutine PSNICE(xmin,xmax,tic1,tic2)
      dx=abs(xmax-xmin)
      scale=10.**int(log10(dx))
      if (dx.lt.1.) scale=scale/10.
      dxnorm=dx/scale               !should be between 1 and 10
      if (dxnorm.le.1.1) then
         tic1=0.1
         tic2=0.2
      else if (dxnorm.le.2.1) then
         tic1=.1
         tic2=.5
      else if (dxnorm.le.4.1) then
         tic1=0.5
         tic2=1.0 
      else
         tic1=1.0
         tic2=2.0
      end if
      tic1=tic1*scale
      tic2=tic2*scale
      return
      end subroutine psnice


! PSLAX puts x and y axes labels on axes
!   Inputs:  xlabel = x-axis label
!            xoff   = inches to offset label
!            etc.
      subroutine PSLAX(xlabel,xoff,ylabel,yoff)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, & 
                   xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, & 
                   linecount,textangle
      character*(*) xlabel,ylabel
      x1=xfrm1
      x2=xfrm2
      y1=yfrm1
      y2=yfrm2
!
      call PSTROK
      call PSLORG(8)
      xavg=(x1+x2)/2.
      call PSPOS(xavg,y1)
      call PSTIC(0.,-xoff,0)
      call PSLAB(xlabel)
!
      call PSLORG(2)
      call PSANG(90.)
      yavg=(y1+y2)/2.
      call PSPOS(x1,yavg)
      call PSTIC(-yoff,0.,0)
      call PSLAB(ylabel)
      call PSANG(0.)
      call PSLORG(1)
      call PSTROK
!
      return
      end subroutine pslax


! PSTIT puts title above axes
!   Inputs:  title = title
!            yoff   = inches to offset title from top
      subroutine PSTIT(title,yoff)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                   xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                   linecount,textangle
      character*(*) title
      x1=xfrm1
      x2=xfrm2
      y1=yfrm1
      y2=yfrm2
      call PSLORG(2)
      xavg=(x1+x2)/2.
      call PSPOS(xavg,y2)
      call PSTIC(0.,yoff,0)
      call PSLAB(title)
      call PSLORG(1)
      return
      end subroutine pstit


! PSNOTE writes text in lower left corner of plot
      subroutine PSNOTE(text)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                   xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                   linecount,textangle
      character*(*) text
      write (17,*) '300 300 m'
      call PSLORG(1)
      call PSLAB(text)
      return
      end subroutine psnote


! PSSYMB plots symbols at current pen position
!   n   = 1  --  +
!       = 2  --  square
!       = 3  --  triangle
!       = 4  --  inverted triangle
!       = 5  --  X
!       = 6  --  diamond
!       = 7  --  hexagon
!       = 8  --  circle
! size2 = symbol height in inches
! NOTE:  Negative symbol numbers make filled symbols
      subroutine PSSYMB(nn,size2)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                   xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                   linecount,textangle
      x1=(xpen-bxplot)/axplot
      y1=(ypen-byplot)/ayplot
      n=abs(nn)
      size=abs(size2/2.)
      if (n.eq.1) then
         call PSTIC(-size,0.0,0)
         call PSTIC(2*size,0.0,1)
         call PSTIC(-size,-size,0)
         call PSTIC(0.0,2*size,1)
         call PSTIC(0.0,-size,0)
      else if (n.eq.2) then
         call PSTIC(-size,-size,0)
         call PSTIC(2*size,0.0,1)
         call PSTIC(0.0,2*size,1)
         call PSTIC(-2*size,0.0,1)
         call PSTIC(0.0,-2*size,1)
         call PSTIC(size,size,0)
      else if (n.eq.3) then
         call PSTIC(-size,-size,0)
         call PSTIC(2*size,0.0,1)
         call PSTIC(-size,2*size,1)
         call PSTIC(-size,-2*size,1)
         call PSTIC(size,size,0)
      else if (n.eq.4) then
         call PSTIC(size,size,0)
         call PSTIC(-2*size,0.0,1)
         call PSTIC(size,-2*size,1)
         call PSTIC(size,2*size,1)
         call PSTIC(-size,-size,0)
      else if (n.eq.5) then
         call PSTIC(-size,-size,0)
         call PSTIC(2*size,2*size,1)
         call PSTIC(-2*size,0.0,0)
         call PSTIC(2*size,-2*size,1)
         call PSTIC(-size,size,0)
      else if (n.eq.6) then
         call PSTIC(0.0,-size,0)
         call PSTIC(size,size,1)
         call PSTIC(-size,size,1)
         call PSTIC(-size,-size,1)
         call PSTIC(size,-size,1)
         call PSTIC(size,size,1)    !extra line avoids notch for thick lines
         call PSTIC(-size,0.0,0)
      else if (n.eq.7) then
         s1=.707*size
         s2=size-s1
         call PSTIC(0.0,size,0)
         call PSTIC(s1,-s2,1)
         call PSTIC(s2,-s1,1)
         call PSTIC(-s2,-s1,1)
         call PSTIC(-s1,-s2,1)
         call PSTIC(-s1,s2,1)
         call PSTIC(-s2,s1,1)
         call PSTIC(s2,s1,1)
         call PSTIC(s1,s2,1)
         call PSTIC(0.0,-size,0)
      else
         is1=nint(xpen)
         is2=nint(ypen)
         is3=nint(size*1000.)
         is4=0
         is5=360
         write (17,*) 'newpath ',is1,is2,is3,is4,is5,' arc'
      end if
      if (nn.gt.0) then
         write (17,*) 'stroke'
         linecount=0
      else
         if (size2.gt.0.) then
            write (17,*) 'closepath fill'
            linecount=0
         else
            write (17,*) 'closepath'
         end if
      end if
      call PSMOVE(x1,y1)
      return
      end subroutine pssymb


! PSBOX draws filled in grayshade box (gray=0 for black, =1 for white)
      subroutine PSBOX(x1,x2,y1,y2,gray)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, & 
                  xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, & 
                  linecount,textangle
      write (17,*) 'stroke'
      linecount=0
      write (17,*) 'gsave ',gray,' setgray'
      call PSMOVE(x1,y1)
      call PSDRAW(x1,y2)
      call PSDRAW(x2,y2)
      call PSDRAW(x2,y1)
      call PSDRAW(x1,y1)
      write (17,*) 'closepath fill grestore'
      return
      end subroutine psbox


! PSCBOX draws filled in color box
      subroutine PSCBOX(x1,x2,y1,y2,hue,sat,bri)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                   xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                   linecount,textangle
      write (17,*) 'stroke'
      linecount=0
      write (17,*) 'gsave ',hue,sat,bri,' sethsbcolor'
      call PSMOVE(x1,y1)
      call PSDRAW(x1,y2)
      call PSDRAW(x2,y2)
      call PSDRAW(x2,y1)
      call PSDRAW(x1,y1)
      write (17,*) 'closepath fill grestore'
      return
      end subroutine pscbox


! PSIMAG displays a bitmap 
!     ibuf  =  integer*2 array containing bitmap
!              1 number per pixel for grayscale
!              3 numbers per pixel for color (RBG multiplexed)
!              values must be between 0 and 255
!              1st pixel is at lower left, last at upper right
!              image is formed by rows from bottom to top
!     nx    =  number of columns of ibuf (pixels)
!     ny    =  number of rows of ibuf (pixels)
!     icol  =  0 for grayscale
!           =  1 for color 
      subroutine PSIMAG(ibuf,nx,ny,icol)
      integer*2 ibuf(*)
      common/pscom/xpen,ypen,axplot,bxplot,ayplot,byplot, &
                  xpen0,ypen0,nlorg,xfrm1,xfrm2,yfrm1,yfrm2, &
                  linecount,textangle
      if (mod(ny,2).ne.0) then
         print *,'***PSIMAG warning.  ny is odd'
         print *,'   this has caused BAD results...'
         print *,'   BEWARE!!!'
      end if
      write (17,*) 'gsave'
      x1=bxplot+xfrm1*axplot
      x2=bxplot+xfrm2*axplot
      y1=byplot+yfrm1*ayplot
      y2=byplot+yfrm2*ayplot
      write (17,*) nint(x1),nint(y1),' translate'
      s1=x2-x1
      s2=y2-y1
      write (17,*) s1,s2,' scale'
      if (icol.ne.1) then
         nschar=nx*2
      else
         nschar=nx*6
      end if
      write (17,*) '/linebuf ',nschar,' string def'
      write (17,*) nx,ny,' 8 [ ',nx,' 0 0 ',ny,' 0 0]'
      write (17,*) '{currentfile linebuf readhexstring pop}'
      if (icol.ne.1) then
         write (17,*) 'image'
      else
         write (17,*) 'false 3 colorimage'
      end if
      npts=nx*ny
      if (icol.eq.1) npts=npts*3
      write (17,21) (ibuf(i),i=1,npts)
21    format (40z2.2)
      write (17,*) 'grestore'
      return
      end subroutine psimag


      subroutine TORGB(hue,sat,bri,red,green,blue)
      if (hue.lt.0..or.hue.gt.1..or.sat.lt.0..or.sat.gt.1. &
         .or.bri.lt.0..or.bri.gt.1.) then
         print *,'Error in TORGB: hue,sat,bri = ',hue,sta,bri
         stop
      end if
      sixth=1./6.
      wht=1.-sat
      if (hue.le.sixth) then
         frac=(hue-0.)/sixth
         red=1.
         green=frac+wht*(1.-frac)
         blue=0.+wht
      else if (hue.le.(1./3.)) then
         frac=(hue-sixth)/sixth
         green=1.
         red=1.-frac+wht*frac
         blue=0.+wht
      else if (hue.le.0.5) then
         frac=(hue-1./3.)/sixth
         green=1.
         blue=frac+wht*(1.-frac)
         red=0.+wht
      else if (hue.lt.2./3.) then
         frac=(hue-0.5)/sixth
         blue=1.
         green=1.-frac+wht*frac
         red=0.+wht
      else if (hue.lt.(5./6.)) then
         frac=(hue-2./3.)/sixth
         blue=1.
         red=frac+wht*(1.-frac)
         green=0.+wht
      else
         frac=(hue-5./6.)/sixth
         red=1.
         blue=1.-frac+wht*frac
         green=0.+wht
      end if
      red=red*bri
      green=green*bri
      blue=blue*bri
      if (red.lt.-0.1.or.red.gt.1.1.or.green.lt.-0.1.or.green.gt.1.1 &
         .or.blue.lt.-0.1.or.blue.gt.1.1) then
          print *,'Error in TORGB:',hue,sat,bri,red,green,blue
          stop
      end if
      if (red.lt.0.) red=0.
      if (red.gt.1.) red=1.
      if (blue.lt.0.) blue=0.
      if (blue.gt.1.) blue=1.
      if (green.lt.0.) green=0.
      if (green.gt.1.) green=1.
      return
      end subroutine TORGB



      subroutine PSCOL(icol)
      parameter (ncol=15)
      real hue(0:ncol),sat(0:ncol),bri(0:ncol)
      if (icol.lt.0.or.icol.gt.ncol) then
         print *,'***WARNING in PSCOL, icol = ',icol
         return
      end if

      hue(0)=0.           ! 0 = white
      sat(0)=0.
      bri(0)=1.

      hue(1)=0.           ! 1 = black
      sat(1)=0.
      bri(1)=0.

      hue(2)=0.           ! 2 = red
      sat(2)=1.
      bri(2)=1.

      hue(3)=0.67         ! 3 = dark blue
      sat(3)=1.
      bri(3)=1.

      hue(4)=0.167        ! 4 = yellow
      sat(4)=1.
      bri(4)=1.

      hue(5)=0.4          ! 5 = green
      sat(5)=1.
      bri(5)=1.

      hue(6)=0.           ! 6 = brown
      sat(6)=1.
      bri(6)=0.5

      hue(7)=0.75         ! 7 = purple
      sat(7)=1.
      bri(7)=1.

      hue(8)=0.555        ! 8 = cyan
      sat(8)=0.5
      bri(8)=1.

      hue(9)=0.833        ! 9 = magenta
      sat(9)=1.
      bri(9)=1.

      hue(10)=0.111       ! 10 = orange
      sat(10)=1.
      bri(10)=1.

      hue(11)=0.333       ! 11 = light green
      sat(11)=0.4
      bri(11)=1.

      hue(12)=0.          ! 12 = pink
      sat(12)=0.3
      bri(12)=1.

      hue(13)=0.5         ! 13 = blue-green
      sat(13)=1.
      bri(13)=1.

      hue(14)=0.2         ! 14 = yellow-green
      sat(14)=1.
      bri(14)=1.

      hue(15)=0.         ! 15 = gray
      sat(15)=0.
      bri(15)=0.5

      call PSHSB(hue(icol),sat(icol),bri(icol) )

      return
      end subroutine pscol




