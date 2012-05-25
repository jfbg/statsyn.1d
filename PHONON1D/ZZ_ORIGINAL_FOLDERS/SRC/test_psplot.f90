program test_psplot
!   !Program to test psplot subroutines
   real a(10,20)
   integer ibuf(200)
   integer i,j,k

   call PSFILE('mypost.ps')

   call PSWIND(1.5,7.5,3.,9.,0.,10.,0.,20.)

   call PSAXES(1.,2.,.1,'i3',1.,5.,.1,'i3') !Plot axes
   call PSLAX('X label',.5,'Y label',.5)    !
   call PSGRAY(0.9)                         !
   call PSTIT('Title',.5)                   !Plot Title
   call PSGRAY(0.)                          !

   do i = 1, 20                             !Plot lines
      s1   =float(i)*2
      call PSLINE(s1)
      s2   =float(i)
      call PSMOVE(2.,s2)
      call PSDRAW(5.,s2)
      call PSTROK
   end do


   do i=1,9                                 !Plot symbols
      call PSLORG(i)
      x    =7.
      y    =float(i)*2
      call PSMOVE(x,y)                      !Move to x,y position
      call PSTIC(0.1,0.,1)
      call PSLAB('test')
      call PSMOVE(x,y)
      call PSSYMB(i,1.)
   end do
   call PSTROK                              !Forces finish of plot


   call PSLORG(1)
   call PSTROK

   do icol=0,15
      x=2.
      y=float(icol)+2.
      call PSMOVE(x,y)
      call PSCOL(icol)
      call PSSYMB(-3,0.15)
      call PSMOVE(x,y)
      call PSTIC(0.2,0.,0)
      call PSNUMB(float(icol),'i3')
   end do
     


   call PSEND
   stop
end program test_psplot


subroutine GETGRAY(a,gray)
!   !This subroutine returns the value of grey
   real     :: a,gray
   gray=1.-a
   return
end subroutine getgray






