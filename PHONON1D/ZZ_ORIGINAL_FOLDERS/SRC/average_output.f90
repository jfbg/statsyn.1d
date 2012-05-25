program average_output

   integer, parameter :: nmx=1000, smx=144001, dmx = 361
   
   real(4)            :: d(dmx)                  !! Distances
   real(4)            :: a1(dmx)                 !! Value at each distance for each time
   real(4)            :: aa(dmx)                 !! Average value at each time
   real(4)            :: temp                    !! Temporary variable
   real(4)            :: time
   
   integer            :: nd,nt                   !! Number of distances and times
   
   integer            :: nfiles                  !! Number of files
   
   character (len=128):: ifile,files(nmx)        !! Input file names
   character (len=128):: ofile                   !! Output file name
   integer            :: status                  !! I/O read error
   
   character (len=2)  :: ch2                     !! 2 characters to skip line
   real(4)            :: aaa


   write(6,'(a)') 'Enter input list file name:'
   read (5,'(a)')  ifile
   
   write(6,'(a)') 'Enter output stack file name:'
   read (5,'(a)')  ofile


!! Read in list of files 
   open(1,file=ifile)
   do i = 11, 10+nmx
    read(1,'(a)',IOSTAT=status) files(i) 
    if (status/=0) exit
    nfiles=i-10
   end do
   close(1)
   if (nfiles> 80) nfiles=80
   
!! For each file, open and read in first line (number of times and distances):
   do i = 11, 10+nfiles
    open(i,file=files(i))
    read(i,*) nt,nd
    read(i,fmt=888) temp,(d(j),j=1,nd)
    write(6,*) trim(files(i))
   end do
   


   open(2,file=ofile)
   write(2,*) nt, nd
   write(2,fmt=888) temp,(d(j),j=1,nd)
!! For each time, read in all distances for all files. Average, and output.
   do i = 1, nt
    aa(1:nd)=0.
    do k = 11, 10+nfiles
    
     read (k,fmt=888) time,(a1(j),j=1,nd) 
     aa(1:nd)=aa(1:nd) + a1(1:nd)
    
    end do
    aa(1:nd) = aa(1:nd) / float(nfiles) / 10.
    
    write(2,fmt=888) time,(aa(j),j=1,nd)
    aaa = 0.
    do k = 1, nfiles
     aaa = aaa + abs(aa(k))/float(nfiles)
    end do
!    write(6,*) time,aaa
   end do
   close(2)
   
   do i = 1, nfiles
    close(10+i)
   end do
   
   
888   format(f10.2,1X,500(f10.6,1x))



   stop
end program average_output
