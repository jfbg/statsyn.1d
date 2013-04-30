      SUBROUTINE RAYTRACE_SCAT
      
      USE pho_vars

! ============ >>
				! RAY TRACING IN LAYER			
				IF (iz /= 1) THEN
				  IF (abs(vf(iz-1,iwave)) > 0.) THEN
				    utop = 1./vf(iz-1,iwave)              !SLOWNESS AT TOP OF LAYER
				  ELSE
				    utop = 0.
				  END IF 
		
					IF (abs(vf(iz,iwave)) > 0.) THEN
						ubot = 1./vf(iz,iwave)                !SLOWNESS AT BOTTOM OF LAYER
					ELSE
						ubot = 0.
					END IF
         
					h = dh                  !THICKNESS OF LAYER
					imth = 2                              !INTERPOLATION METHOD
		
					CALL LAYERTRACE(p,h,utop,ubot,imth,dx1,dt1,irtr1)
					dtstr1 = dt1/Q(iz)                    !t* = TIME/QUALITY FACTOR
				ELSE
					irtr1  = -1
					dx1    = 0.
					dt1    = 0.
					dtstr1 = 0.
				END IF
        
        IF (irtr1 == 0) THEN
         ud = -ud
        ELSE IF (irtr1 >= 1) THEN
         d = d + ((z_s(iz)-z_s(iz-1))**2+dx1**2)**0.5 !DISTANCE TRAVELED IN LAYER
         
         t = t + dt1                    !TRAVEL TIME
         x = x + dx1*x_sign*cos(az)     !EPICENTRAL DISTANCE TRAVELED-km
         s = s + dtstr1                 !CUMULATIVE t*
        END IF
        
      END SUBROUTINE RAYTRACE_SCAT
