      SUBROUTINE RAYTRACE
      
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
         
					h = z(iz)-z(iz-1)                  !THICKNESS OF LAYER
					imth = 2                              !INTERPOLATION METHOD
		
					CALL LAYERTRACE(p,h,utop,ubot,imth,dx1,dt1,irtr1)
					dtstr1 = dt1/Q(iz)                    !t* = TIME/QUALITY FACTOR
				ELSE
					irtr1  = -1
					dx1    = 0.
					dt1    = 0.
					dtstr1 = 0.
				END IF
        
        IF (irtr1 == 0) THEN			!JFBG_QUESTION_2
         ud = -ud
        ELSE IF (irtr1 >= 1) THEN
         d = d + ((z_s(iz)-z_s(iz-1))**2+dx1**2)**0.5 !DISTANCE TRAVELED IN LAYER
         
         t = t + dt1                    !TRAVEL TIME
         x = x + dx1*x_sign*cos(az)     !EPICENTRAL DISTANCE TRAVELED-km
         s = s + dtstr1                 !CUMULATIVE t*
        END IF
        
				IF ( (iz > 1).AND.(abs(irtr1) == 1).AND. &
							(iz < nlay) ) THEN
					IF ( (iz > 1).AND.(iz <= nlay) ) h = z_s(iz)-z_s(iz-1)

          IF (ip  ==  2) THEN
						IF ( (ud == 1) ) THEN               !IF downGOING SH WAVE
							CALL REFTRAN_SH(p,vf(iz-1,2),vf(iz,2),rh(iz-1),rh(iz), &
                           ar,at)
						ELSE IF ((ud == -1) ) THEN          !IF UPGOING SH WAVE
							CALL REFTRAN_SH(p,vf(iz,2),vf(iz-1,2),rh(iz),rh(iz-1), &
                           ar,at)
						END IF
          ELSE
						IF ( (ud == 1) ) THEN               !IF downGOING P-SV WAVE
							CALL RTCOEF2(p,vf(iz-1,1),vf(iz-1,2),rh(iz-1), &
                          vf(iz  ,1),vf(iz  ,2),rh(iz), &
                          ip,arp,ars,atp,ats)
						ELSE IF ((ud == -1) ) THEN          !IF UPGOING P-SV WAVE
							CALL RTCOEF2(p,vf(iz  ,1),vf(iz  ,2),rh(iz  ), &
                          vf(iz-1,1),vf(iz-1,2),rh(iz-1), &
                          ip,arp,ars,atp,ats)           
!           WRITE(6,*) 'HI'
						END IF
          END IF
          
          r0 = rand()                       !RANDOM NUMBER FROM 0 TO 1

          IF (ip  ==  2) THEN                   !IF SH-WAVE

						IF (h > 0.) THEN                    !IF GRADIENT, THEN
							IF (r0 < (abs(ar)/(abs(ar)+abs(at)))/P0**2) THEN!CHECK FOR REFLECTN
								IF (ar < 0) a = -a                !OPPOSITE POLARITY
								ud = -ud                           !downGOING/UPGOING
							END IF                              !
						ELSE                                 !IF INTERFACE THEN
							IF (r0 < (abs(ar)/(abs(ar)+abs(at)))) THEN!CHECK FOR REFLECTION
								IF (ar < 0) a = -a                !OPPOSITE POLARITY
								ud = -ud                           !downGOING/UPGOING
							END IF                              !
						END IF                               !

          ELSE                                  !IF P- OR SV-WAVE 
          	IF (h <= 0.) THEN
							rt_sum = abs(arp)+abs(atp)+abs(ars)+abs(ats)    !SUM OF REFL/TRAN COEFS

							rt_min = 0.                          !RANGE PROBABILITIES FOR P REFL
							rt_max = abs(arp)/rt_sum             !
							IF ( (r0 >= rt_min).AND.(r0 < rt_max) ) THEN!CHECK IF REFLECTED P
								IF (arp < 0) a = -a                 !REVERSE POLARITY
								ud = -ud                            !UPGOING <-> downGOING
								ip = 1                              !P WAVE
							END IF                               !
           
!             IF (z_s(iz) == 137.) WRITE(6,*) arp,atp,r0,ud,ip,a

							rt_min = rt_max                      !RANGE PROBABILITIES 4 SV REFL
							rt_max = rt_max+abs(ars)/rt_sum      !
							IF ( (r0 >= rt_min).AND.(r0 < rt_max) ) THEN!CHECK IF REFLECTED SV
								IF (ars < 0) a = -a                 !REVERSE POLARITY
								ud = -ud                            !UPGOING <-> downGOING
								ip = 3                              !SV WAVE
							END IF                                !

							rt_min = rt_max                      !RANGE PROBABILITIES 4 P TRANS
							rt_max = rt_max+abs(atp)/rt_sum      !
							IF ( (r0 >= rt_min).AND.(r0 < rt_max) ) THEN!CHECK IF TRAMSITTED P
								ip = 1                              !P WAVE
							END IF                               !

							rt_min = rt_max                      !RANGE PROBABILITIES 4 SV TRANS
							rt_max = rt_max+abs(ats)/rt_sum      !
							IF ( (r0 >= rt_min).AND.(r0 <= rt_max) ) THEN!CHECK IF TRANSMITTED SV
								ip = 3                              !SV WAVE
							END IF                               !
						END IF      
          END IF                                !END IF: SH, OR P-SV
         
        ELSE IF (iz == nlay) THEN               !ONCE HIT OTHER SIDE OF CORE
					ud = -ud
					x = x + 180*deg2km
				END IF
	
        
				!FIX NEXT IF FOR DIFFRACTED WAVES: 
				IF (irtr1 == 2) THEN             !RAY TURNS IN LAYER FOLLOW 1 LEN
				ud = -ud
				ncaust = ncaust + 1                   !# OF CAUSTICS
				END IF
				! RAY TRACING IN LAYER	
  			! ============ <<
  			
  			END SUBROUTINE RAYTRACE
