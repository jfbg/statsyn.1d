program convert_model
!   This program converts velocity models from z,Vp,Vs to Z, R, Vp, Vs, Rho
!   
!   To compile: gfortran convert_model.f90 -o ../bin/convert_model


   implicit      none                                      !allow no implicit
   integer, parameter :: vmx = 6371                        !max number layers
   real               :: z(vmx),r(vmx),p(vmx),s(vmx),rh(vmx)!model
   
   integer            :: i,j                               !index variables
   
   character*(80)     :: ifile, ofile                      !input & output files
   integer            :: stat                              !I/O status (0=okay)
   
   write(6,'(A)') 'Enter input model name:'   !Input File
   read (5,'(A)')  ifile
   open(1,file=ifile,status='old')
   
   write(6,'(A)') 'Enter output model name:'  !Output file
   read (5,'(A)')  ofile
   open(2,file=ofile,status='unknown')
   
   do i = 1, vmx
      read(1,*,iostat=stat) z(i),p(i),s(i)                !read in depth Vp and Vs
      if (stat /= 0) cycle
      r(i) = 1737.-z(i)                       !radius from depth
      rh(i) = s(i)*.77                        !density (approximate for mantle)
      write(2,fmt=1010) z(i),r(i),p(i),s(i),rh(i)
   end do
   
   close(1)
   close(2)

1010 format(5f12.5)


   stop
end program convert_model
