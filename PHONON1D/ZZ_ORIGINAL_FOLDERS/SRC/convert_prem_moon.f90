program convert_prem_moon
   implicit         none
   integer, parameter :: dmx=6371
   real (8)           :: z(dmx),r(dmx),vp(dmx),vs(dmx),rho(dmx)
   integer            :: status, ndat                  !I/O error flag & # point
   real (8)           :: z2(dmx),r2(dmx)
   real (8)           :: g(dmx)                       !Gravity 
   real (8)           :: g_c                          !Gravitational constant
   real (8)           :: pi                           !pi = 3.14...
   integer            :: i,j,k
   real (8)           :: g_moon,factor,d_factor       !grav-moon, grav m/e,
   real (8)           :: dh                           !layer thickness
   real (8)           :: factor_0
   real (8)           :: r_moon
   open(1,file='prem.new')
   open(2,file='prem_moon')
   
   g_c = 6.673d-11                                    !m^3 kg^-1 s^-2
   pi  = atan(1.)*4.
   g_moon = 1.6366                                    !Lunar gravity (m s^-2)
   r_moon = 1737.
   do i = 1, dmx
    read(1,*,IOSTAT=status) z(i),r(i),vp(i),vs(i),rho(i)
    if (status /= 0) exit
    ndat = i 
   
   end do

   factor   = 0.167
!   factor    = 1.
   d_factor = 1.
   do j = 1, 100
    factor = factor * d_factor
    do i = 1, ndat
     z2(i) = z(i)*factor
     r2(i) = r_moon-z2(i)
    end do
    do i = 1, ndat
     g(i) = 0.
    end do
    do i = ndat-1, 1, -1
     if (r2(i) > 0.) then
      if (r2(i+1) <= 0.) then
       dh = r2(i)
      else
       dh = (r2(i)-r2(i+1))*1000
      end if
      g(i) = g(i+1) + rho(i)*1000.*(  4/3.*pi*(r2(i  )*1000.)**3 &
                                    - 4/3.*pi*(r2(i+1)*1000.)**3 )
     else
      g(i) = 0.
     end if
    end do
    do i = 1, ndat
     g(i) = g(i) * g_c/(r2(i)*1000.)**2
    end do
    d_factor = g(1)/g_moon
    write(6,*) factor,d_factor,g(1),g_moon
   end do
   do i = 1, ndat-1
    z2(i) = z(i)*factor
    r2(i) = r_moon-z2(i)
    if (r2(i) > 0.) then
     write(2,fmt=1010) z2(i),r2(i),vp(i),vs(i),rho(i)
    end if
    if ( (r2(i) < 0.).and.(r2(i-1) > 0.) ) then
     write(2,fmt=1010) r_moon,0.,vp(i),vs(i),rho(i)
    end if 
   end do   
 1010  FORMAT(5(2X,F10.5))             !OUTPUT FORMAT
  
   stop
end program convert_prem_moon
