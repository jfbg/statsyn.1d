subroutine bit_limit(y,ndat,y_max)
   real     :: y(*)
   integer  :: ndat
   real     :: y_max,y_min,ymax
   integer  :: i
   
   ymax = 0.
   do i = 1, ndat
    if ( abs(y(i)) > ymax) ymax = abs(y(i))
   end do
   
   do i = 1, ndat
    y(i) = real ( int(y(i)*y_max/ymax) )*ymax
   end do
   return
end subroutine bit_limit
