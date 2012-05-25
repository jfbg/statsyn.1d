      real xa(100000),ya(100000)
      character xlabel*80,font1*5
      character text5*5
      call EZXINIT(900,700)
            
      call EZXLOADFONT(i,'9x15')
      
      call EZXWIND(100.,800.,100.,600.,0.,10.,0.,10.)
      
      call EZXMOVE(2.,2.)
      call EZXDRAW(4.,2.1)
      
      xlabel='X-axis label '
      call EZXLAB('test message')
!      call EZXLAB(xlabel)
      
      call EZXAXES(1.,2.,5,'i3',1.,5.,5,'i3')
      
      xlabel='X-axis label'
      call EZXLAX(xlabel,30,'Y-axis label',50)
c      do 10 i=1,100000
c         xa(i)=10.*float(i)/100000.
c         call RAN(ya(i))
c         ya(i)=6.+sin(float(i)/5000.)+ya(i)
c10    continue
c      call ldoit
c      print *,'(1) move and draw   or  (2) poly'
c      read *,menu
c      if (menu.eq.2) then
c         call EZXPOLY(xa,ya,100000)
c      else
c         call EZXMOVE(0.,5.)
c         do 20 i=1,100000
c            call EZXDRAW(xa(i),ya(i))
c20       continue
c      end if
c      call EZXCLRB(1.,5.,1.,5.)
c      call EZXMOVE(3.,3.)
c      call EZXLAB('test text')
      call EZXANG(45.)
      do 25 i=0,9
         call EZXANG(float(i)*10.)
         x=float(i)
         y=5.
         call EZXMOVE(x,y)
         isymb=min(i,6)
         call EZXCOL(i)
         call EZXSYMB(isymb,5)
         call EZXLORG(i)
         call EZXLAB('text')
         call EZXCIR(x,y+1.,(i+10)*2,i,1)
         call EZXCIR(x,y-1.,20,i,0)
         call EZXCOL(1)
25    continue

      do i=0,9
         xoff=real(i)
         xa(1)=xoff+1.
         ya(1)=1.
         xa(2)=xoff+2.
         ya(2)=1.
         xa(3)=xoff+1.5
         ya(3)=0.5
         xa(4)=xa(1)
         ya(4)=ya(1)
	 call EZXCOL(i)
         call EZXPOLYFILL(xa,ya,4)
      enddo
      call EZXCOL(1) 
      

30    call EZXMESS('Hit mouse key to digitize')
      call EZXDIG(x,y,ibut)
      print *,'x,y,ibut = ',x,y,ibut
C      if (ibut.eq.6) call EZXDUMP('mypost')
      if (ibut.eq.2) call EZXCLR
      if (ibut.ne.3) go to 30

900   call EZXQUIT
      stop
      end

c RAN gets random number between 0 and 1
c from Numerical Recipies, p. 195-198
      subroutine RAN(fran)
      save jran,ifirst
      im=120050                          !overflow at 2**28
      ia=2311
      ic=25367
      if (ifirst.ne.12345) then
         jran=314159
         ifirst=12345
      end if
      jran=mod(jran*ia+ic,im)
      fran=float(jran)/float(im)
      return
      end

