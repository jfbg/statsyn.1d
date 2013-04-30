program test_ray_trace

   implicit none
   real(8)  :: n1(3),t1(3),v1,v2
   integer  :: itr
   real(8)  :: t2(3)
   
   write(6,'(a)') 'Enter trace trajectory: t1(1,2,3):'
   read (5,    *)  t1(1),t1(2),t1(3)
   
   write(6,'(a)') 'Enter reflector normal vector: n1(1,2,3):'
   read (5,    *)  n1(1),n1(2),n1(3)
   
   write(6,'(a)') 'Enter velocities: (v1,v2):'
   read (5,    *)  v1,v2
   
   write(6,'(a)') 'Enter 1(transmitted) or -1(reflected):'
   read (5,    *)  itr
   
   
   call ref_tran_ray(n1,t1,v1,v2,itr,t2)


   write(6,*) 't=(',t2(1),', ',t2(2),', ',t2(3),')'

   stop
end program test_ray_trace
