      SUBROUTINE VEL_MODEL_CHECKS
      
      USE pho_vars
      
      IMPLICIT NONE
      
      REAL qdepdiff
      
      !MODEL CHECKS
			check_scat = 1
			check_scat2 = 0
			check_core = 1
			check_source = 1
      
      OPEN(1,FILE=ifile,STATUS='OLD')    !OPEN SEISMIC VELOCITY MODEL
      
      DO I = 1, nlay0                     !READ IN VELOCITY MODEL
       READ(1,*,IOSTAT=status) z_st(I),r_st(I),vst(I,1),vst(I,2),rht(I),Qt(I)
       IF (z_st(I) == scat_depth) THEN
          check_scat = 0		!Velocity layer depth is same as scattering layer.   
       END IF
!       IF (z_st(I) == qdep) THEN
!          check_source = 0		!Velocity layer depth is same as source depth.   
!       END IF
       IF (status /= 0) EXIT
      END DO
      
      I = I-1 ! Number of layers in input model.
      
      CLOSE (1)
      
      WRITE(6,*) 'Velocity model checks:'
      WRITE(6,*) '   --> Total pre-checks layers: ',I
      
      iz1 = 1
			DO WHILE (scat_depth >= z_st(iz1+1))      !FIND WHICH LAYER THE SCAT LAYER IS ABOVE
			 iz1 = iz1 +1														 !NEW VEL LAYER WILL BE AT (iz1 + 1)
			END DO
			WRITE(6,*) '   --> Scattering layer at or below initial layer:', iz1
			
			
    
      IF (z_st(I) - z_st(I-1) < corelayer) check_core = 0 !Last layer is small enough to ignore.

      nlay = I + check_scat + check_core ! NUMBER OF LAYERS
      																	 ! Layers are added if checks failed.
      
			IF (nlay > I) THEN																	 
			! BUILD MODEL 
				DO K = 1,nlay
					IF (K <= iz1) THEN
					 z_s(K) = z_st(K)
					 r_s(K) = r_st(K)
					 vs(K,1) = vst(K,1)
					 vs(K,2) = vst(K,2)
					 rh(K) = rht(K)
					 Q(K) = Qt(K)
					END IF
				 
					IF ((K == iz1 +1).AND.(check_scat == 1)) THEN
					 WRITE(6,*) '   --> Adding velocity layer at the base of the scattering layer'
							z_s(K) = scat_depth
							r_s(K) = erad-scat_depth
							vs(K,1) = (vst(K,1)-vst(K-1,1))/(z_st(K) - z_st(K-1)) * (scat_depth - z_st(K-1)) + vst(K-1,1)
							vs(K,2) = (vst(K,2)-vst(K-1,2))/(z_st(K) - z_st(K-1)) * (scat_depth - z_st(K-1)) + vst(K-1,2)
							rh(K) = (rht(K)-rht(K-1))/(z_st(K) - z_st(K-1)) * (scat_depth - z_st(K)) + rht(K)
							Q(K) = (Qt(K)-Qt(K-1))/(z_st(K) - z_st(K-1)) * (scat_depth - z_st(K)) + Qt(K)
					ELSEIF ((K == iz1 +1).AND.(check_scat == 0)) THEN
							z_s(K) = z_st(K)
							r_s(K) = r_st(K)
							vs(K,1) = vst(K,1)
							vs(K,2) = vst(K,2)
							rh(K) = rht(K)
							Q(K) = Qt(K)
					END IF
						
					IF ((K > iz1 + 1).AND.(K < nlay)) THEN
							z_s(K) = z_st(K-check_scat)
							r_s(K) = r_st(K-check_scat)
							vs(K,1) = vst(K-check_scat,1)
							vs(K,2) = vst(K-check_scat,2)
							rh(K) = rht(K-check_scat)
							Q(K) = Qt(K-check_scat)
					END IF
									
				END DO
					
				IF (check_core == 0) THEN
						z_s(nlay) = z_st(nlay-1)
						r_s(nlay) = r_st(nlay-1)
						vs(nlay,1) = vst(nlay-1,1)
						vs(nlay,2) = vst(nlay-1,2)
						rh(nlay) = rht(nlay-1)
						Q(nlay) = Qt(nlay-1)    		  
			 ELSE
				 WRITE(6,*) '   --> Adding thin layer near core'
						z_s(nlay-1) = z_st(I) - corelayer
						z_s(nlay)   = z_st(I)
						r_s(nlay-1) = r_st(I) + corelayer
						r_s(nlay)   = r_st(I)
						vs(nlay-1,1)  = (vst(I,1)-vst(I-1,1))/(z_st(I) - z_st(I-1)) * (z_st(I) - corelayer - z_st(I-1)) + vst(I-1,1)
						vs(nlay-1,2)  = (vst(I,2)-vst(I-1,2))/(z_st(I) - z_st(I-1)) * (z_st(I) - corelayer - z_st(I-1)) + vst(I-1,2)
						vs(nlay,1)  = vst(I,1)
						vs(nlay,2)  = vst(I,2)
						rh(nlay-1)  = (rht(I)-rht(I-1))/(z_st(I) - z_st(I-1)) * (z_st(I) - corelayer - z_st(I-1)) + rht(I-1)
						rh(nlay)  = rht(I)
						Q(nlay-1)  = (Qt(I)-Qt(I-1))/(z_st(I) - z_st(I-1)) * (z_st(I) - corelayer - z_st(I-1)) + Qt(I-1)
						Q(nlay)  = Qt(I)
			 END IF
						
				 
			ELSE
				z_s = z_st
				r_s = r_st
				vs = vst
				rh = rht
				Q = Qt
			END IF
			
			
			iz2 = 1
			DO WHILE (qdep >= z_s(iz2+1))      !FIND WHICH LAYER THE SCAT LAYER IS ABOVE
			 iz2 = iz2 +1														 !NEW VEL LAYER WILL BE AT (iz1 + 1)
			END DO
			WRITE(6,*) '   --> Source is in or on initial layer:', iz2
			WRITE(6,*) '          Distance between source and layer is (km):',qdep-z_s(iz2)			
			IF (qdep - z_s(iz2) < 2)  check_source = 0
			
			qdepdiff = qdep-.1
			
			! Adding a layer where the source is located
			IF (check_source == 1) THEN
				z_st = z_s
				r_st = r_s
				vst = vs
				rht = rh
				Qt = Q
				
				nlay = nlay+1
				
				
				DO K = 1,nlay !one new layer for the source
					IF (K <= iz2) THEN
					 z_s(K) = z_st(K)
					 r_s(K) = r_st(K)
					 vs(K,1) = vst(K,1)
					 vs(K,2) = vst(K,2)
					 rh(K) = rht(K)
					 Q(K) = Qt(K)
					END IF
					
					IF (K == iz2 +1) THEN
					 WRITE(6,*) '   --> Adding velocity layer at source depth'
							z_s(K) = qdepdiff
							r_s(K) = erad-qdepdiff
							vs(K,1) = (vst(K,1)-vst(K-1,1))/(z_st(K) - z_st(K-1)) * (qdepdiff - z_st(K-1)) + vst(K-1,1)
							vs(K,2) = (vst(K,2)-vst(K-1,2))/(z_st(K) - z_st(K-1)) * (qdepdiff - z_st(K-1)) + vst(K-1,2)
							rh(K) = (rht(K)-rht(K-1))/(z_st(K) - z_st(K-1)) * (qdepdiff - z_st(K)) + rht(K)
							Q(K) = (Qt(K)-Qt(K-1))/(z_st(K) - z_st(K-1)) * (qdepdiff - z_st(K)) + Qt(K)
					END IF
					
					IF (K > iz2 + 1) THEN
							z_s(K) = z_st(K-1)
							r_s(K) = r_st(K-1)
							vs(K,1) = vst(K-1,1)
							vs(K,2) = vst(K-1,2)
							rh(K) = rht(K-1)
							Q(K) = Qt(K-1)
					END IF
					
				END DO
			END IF
			
      OPEN(45,FILE='model_modified.txt',STATUS='UNKNOWN')    !OPEN SEISMIC VELOCITY MODEL
      
      DO I = 1,nlay
      	WRITE(45,*) z_s(I),vs(I,1),vs(I,2),rh(I),Q(i)
      END DO
      
      CLOSE(45)
     	


      RETURN

      END SUBROUTINE VEL_MODEL_CHECKS
