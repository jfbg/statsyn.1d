
SUBROUTINE REF_TRAN_PROB (vel_perturb)
      USE pho_vars
      implicit none
      real :: vel_perturb
      real ::  rt(10)
      real :: art(10),ref_tran_sum
      real :: vp2,vs2,rh2,v2              !! P & S  & density of layer 2 (generic vel=v2)
      real :: azr                         !! azimuth in radians
      real(8) :: ta(3),n1(3),tb(3)        !! Phonon trajectory, reflector normal & new traject
      real(8) :: ca,sa,ci,si              !! Cosine & sine of azimuth & inclination
      real(8) :: fact                     !! Impedence contrast factor
      real :: inc
      integer :: ip2,irt
      integer :: iwave2
      real(8) :: theta,phi,theta2,phi2
      real(8) :: pp,ps                    !! Ray parameters for P & S waves
      real(8) :: get_ang
      
      pi = atan(1.)*4.
      
      phi = asin(p/vf(iz,iwave))          !!
      theta   = az*pi/180.                !!
      ta(1) = sin(phi)*cos(theta)          !! East
      ta(2) = sin(phi)*sin(theta)          !! North
      ta(3) = cos(phi)                     !! Vertical
      call unit_vector_3(ta)
      
!! Create inclination & declination of reflector plane
      r0 = rand()
      theta2 = 2.*pi*(r0-0.5)
      r0 = rand()
      phi2   =    pi*(r0-0.5)
      
      n1(1) = sin(phi2)*cos(theta)
      n1(2) = sin(phi2)*sin(theta)
      n1(3) = cos(phi2)
      call unit_vector_3(n1)
      
!! Sin & Cos of inc and az      
      sa = sin(phi-phi2)
      si = sin(theta-theta2)
      ca = cos(phi-phi2)
      ci = cos(theta-theta2)
      
      ang1 = abs(get_ang(ta,n1))           !! Angle between phonon trajectory & norm
      
      pp = sin(ang1)/vf(iz,1)              !! P-wave ray parameter
      ps = sin(ang1)/vf(iz,2)              !! S-wave ray parameter

!! Create random velocity ontrast between 0.05 and -0.05
      fact = 1.0 + vel_perturb*(rand()-0.5)!! Factor of impedence contrast
      vp2 = vf(iz,1)*fact                  !! New P & S wave velocities for other side
      vs2 = vf(iz,2)*fact                  !!
      rh2 = rh(iz)*fact                    !! 
      iwave = ip                           !! (1)=P wave, 2=S wave velocity
      if (ip==3) iwave = 2
      
      if (ip==1) then                      !! P Incident
       CALL RTCOEF2(pp,vf(iz,1),vf(iz,2),rh(iz),vp2,vs2,rh2,1,rt(1),rt(2),rt(3),rt(4)) !! P-SV reflected & transmitted (Pr=1,Sr=2,Pt=3,St=4)
       rt(1)=rt(1)                         !! P  refl
       rt(5)=rt(2)*sa                      !! SH refl
       rt(2)=rt(2)*ca                      !! SV refl
       rt(3)=rt(3)                         !! P  trans
       rt(6)=rt(4)*sa                      !! SH trans
       rt(4)=rt(4)*ca                      !! SV trans
      else
       CALL REFTRAN_SH(ps,vf(iz,2),vs2,rh(iz),rh2,rt(9),rt(10))   !! SH reflected & transmitted amplitudes
       CALL RTCOEF2(ps,vf(iz,1),vf(iz,2),rh(iz),vp2,vs2,rh2,2,rt(5),rt(6),rt(7),rt(8)) !! SV-P reflected & transmitted (Pr=5,Sr=6,Pt=7,St=8)
       do i = 1, 10                        !! If errors, zero them
        if (isnan(rt(i))) rt(i) = 0.
       end do
      
       if (ip==3) then                     !! SV Incidence
       
        rt(1)=rt(5)*ca                     !! P  refl
        rt(2)=rt(6)*ca+rt(9)*sa            !! SV refl
        rt(3)=rt(7)*ca                     !! P  trans
        rt(4)=rt(8)*ca+rt(10)*sa           !! SV trans
	rt(5)=rt(6)*sa+rt(9)*ca            !! SH refl
	rt(6)=rt(8)*sa+rt(10)*ca           !! SH trans
	
       else                                !! SH Incidence
        rt(1)=rt(5)*sa                     !! P  refl
        rt(2)=rt(6)*sa+rt(9)*ca            !! SV refl
        rt(3)=rt(7)*sa                     !! P  trans
        rt(4)=rt(8)*sa+rt(10)*ca           !! SV trans
	rt(5)=rt(6)*ca+rt(9)*sa            !! SH trans
	rt(6)=rt(8)*ca+rt(10)*sa           !! SH trans
       end if
      end if
            
      ref_tran_sum = 0.                                    !! Zero total prob normalization
      
      art(1)= abs(rt(1))                                   !! prob=abs(ref or trans coeff)
      do i = 2, 6
       if (.not.(isnan(rt(i)))) art(i) = art(i-1) + abs(rt(i))!! Sum the absolute value of reflection & transmittion coefficients
      end do
      
      ref_tran_sum = rt(6)
      
      if (isnan(ref_tran_sum).or.ref_tran_sum==0.) return  !! ERROR SO SKIP
      
      r0 = rand()
      
      if (r0 < art(1)) then                               !! Determine output phase & set vars
       ip2 = 1                                            !! P reflected
       irt = -1
      else if (r0 < art(2)) then                          !! SV reflected
       ip2 = 3
       irt = -1
      else if (r0 < art(3)) then                          !! P transmitted
       ip2 = 1
       irt = 1
      else if (r0 < art(4)) then                          !! SV transmitted
       ip2 = 3
       irt = 1
      else if (r0 < art(5)) then                          !! SH reflected
       ip2 = 2
       irt = -1
      else if (r0 < art(6)) then                          !! SH tranmsitted
       ip2 = 2
       irt = 1
      end if         
      
      iwave2 = ip2                                        !! Set output wave velocity P=1,S=2
      if (ip2 == 3) iwave2 = 2
      
      
      if (irt==1) then                                    !! Velocity in ref/trans layer
       if (ip2/=1) then
        v2 = vs2
       else
        v2 = vp2
       endif
      else
       v2  = vf(iz,iwave2)
      endif
      
      call ref_tran_ray(n1,ta,vf(iz,iwave),v2,irt,tb)     !! Get the new trajectory
      if (tb(1)+tb(2)+tb(3) < 0) ud = -ud                 !! set up-down variable
      call ucar2sphr(ta(1),ta(2),ta(3),az,inc)                 !! get azimuth & inclination
      iwave = iwave2
      ip = ip2
      p = sin(inc)/vf(iz,iwave2)
        
      return
END SUBROUTINE REF_TRAN_PROB

