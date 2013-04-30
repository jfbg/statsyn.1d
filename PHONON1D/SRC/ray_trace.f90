

double precision function dot_product_3(a,b)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   ! This function calculates the dot product of two 3-component vectors!   !
!   !                                                                    !   !
!   ! This function   was written by Jesse F. Lawrence                   !   !
!   !      Contact:   jflawrence@stanford.edu                            !   !
!   !                                                                    !   !
!   ! Beware of the bug!! (I don't know which one, but it must be there!)!   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
    implicit none                                 !! Allow no implicit variables
    real(8) :: a(3),b(3)                          !! Input vectors
        
    dot_product_3 = a(1)*b(1)+a(2)*b(2)+a(3)*b(3) !! Calculate the dot product
     
    return   
end function dot_product_3



double precision function get_ang(x1,x2)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   ! This function gets the angle between two vectors.                  !   !
!   !                                                                    !   !
!   ! This subroutine was written by Jesse F. Lawrence                   !   !
!   !      Contact:   jflawrence@stanford.edu                            !   !
!   !                                                                    !   !
!   ! Beware of the bug!! (I don't know which one, but it must be there!)!   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
   implicit             none
   real(8)           :: x1(3),x2(3)               !! two vectors
   real (8)          :: arcos                     !! Inverse cosine
   real (8)          :: dot_product_3             !! The dot product
   
   get_ang = abs(arcos(dot_product_3(x1,x2)))     !! Calculate the angle
   
   return                                         !! Return
end function get_ang






subroutine unit_vector_3(n)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   ! This subroutine makes a unit vector out of input vector, n.        !   !
!   !                                                                    !   !
!   ! This subroutine was written by Jesse F. Lawrence                   !   !
!   !      Contact:   jflawrence@stanford.edu                            !   !
!   !                                                                    !   !
!   ! Beware of the bug!! (I don't know which one, but it must be there!)!   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
   implicit           none
   real(8)         :: n(3)
   real(8)         :: n_mag
   real(8)         :: dot_product_3
   
   n_mag = (dot_product_3(n,n))**0.5
   n(1:3) = n(1:3)/n_mag
   return
end subroutine unit_vector_3




subroutine ref_tran_ray(n1,t1,v1,v2,itr,t2)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   ! This subourine calculates the vector of the new trajectory after   !   !
!   !      transmission or reflection (itr).                             !   !
!   !                                                                    !   !
!   ! This subroutine was written by Jesse F. Lawrence                   !   !
!   !      Contact:   jflawrence@stanford.edu                            !   !
!   !                                                                    !   !
!   ! Beware of the bug!! (I don't know which one, but it must be there!)!   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
   implicit none
   real(8) :: n1(3),t1(3),t2(3)
   real(8) :: a1(3),b1(3),a2(3),b2(3)
   integer :: i
   real(8) :: dp_tn
   real(8) :: v1,v2 !! Velocities of two media
   real(8) :: dp,dot_product_3,b2_mag
   integer :: itr
!
!                          b1  ^
!                       \------\
!                              \
!                       |-     \     ^
!                        \     \     \
!                         \    \     \
!                          \   \     \  a1
!                       t1  \  \     \
!                            \ \     \
!                             \\     \
!          --------------------\-------------------->
!                    \         \\.
!                    \         \  \.
!                    \         \    \.    t2
!                a2  \         \      \.
!                    \         \        \.
!                    \         \          _|
!                    V         \
!                              \ ---------->
!                              \     b2
!
!
!        a1 = - (t1.n1)*n1
!        b1 = a1 - t1 = n1 (1+n1.t1)
!        b2 = v2/v1 * b1 = v2/v1 * n1 (1-n1.t1)
!        a2 = - sqrt[1-b2^2]*n1 

   call unit_vector_3(n1)
   call unit_vector_3(t1)
   
   
   dp = dot_product_3(t1,n1)
   
   write(6,*) "dp:",dp
      
   if (dp < 1) then
      t1(1:3) = -t1(1:3)
      dp = -dp
   end if

   do i = 1, 3
    a1(i) = dp * n1(i)
    b1(i) = a1(i) - t1(i)
    b2(i) = v2*v2/v1/v1 * b1(i)
   end do
   
   b2_mag = (1.-dot_product_3(b2,b2))**0.5
   
   do i = 1, 3
    write(6,*) 'DING',b2_mag
    a2(i) = - b2_mag*n1(i)*float(itr)
    t2(i) = b2(i) + a2(i)
   end do
   
   write(6,*) 'A1:',a1(1:3)
   write(6,*) 'B1:',b1(1:3)
   write(6,*) 'A2:',a2(1:3)
   write(6,*) 'B2:',b2(1:3)
   
   write(6,*) '|t2|:',(dot_product(t2,t2))**0.5
   
   return
end subroutine ref_tran_ray



 
