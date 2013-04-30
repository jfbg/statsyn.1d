      SUBROUTINE INTERFACE_SCATTER
      
      USE pho_vars
      
      IMPLICIT NONE
    					
      ! ----- Initialize Randomization -----			
      ! CALL SYSTEM_CLOCK(COUNT=nclock)
      ! seed = (nclock)! + 11 * (/ (k - 1, k = 1, nseed) /)
      ! CALL srand(seed)
      ! 
	    ! r0 = rand()    !First rand output not random
      !                  ! It is seed (clock) dependent
      ! ============ <<
      
      !Check if scatter first
      r0 = rand()
      IF (r0 < scat_prob) THEN      
				 r0 = rand()
				 IF (r0 < 0.5) x_sign=-x_sign		
				 r0 = rand()
				 IF (r0 < scat_prob) ud = -ud
		 
				 r0 = rand()
				 r0 = ( r0 - 0.5 )
				 p = p1 + r0*(1./vf(iz,iwave)-p1)!*scat_prob
		 
				 DO WHILE ((p < p1).OR.(p >= 1./vf(iz,iwave)) ) !p2(iwave)))
				 r0 = rand()                       !SELECT RANDOM RAY PARAMETER 
				 ang1 = angst*r0
				 p = abs(sin(ang1))/vf(iz,iwave)
				 END DO
		 
				 r0 = rand()                        !
				 r1 = rand()                        !
				 IF (r1 < 0.5) az = az - pi
				 az = az + asin(r0**2)                  !
				 IF (az < -pi) az = az + 2.*pi
				 IF (az >  pi) az = az - 2.*pi
      END IF		 
					
      END SUBROUTINE INTERFACE_SCATTER