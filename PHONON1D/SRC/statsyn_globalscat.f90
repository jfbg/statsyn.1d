PROGRAM STATSYN_GLOBALSCAT
!
! ModIFied version from statsyn_TRACK_iso:
!
! Scattering is isotropic
! Scattering length-scale follows power law (declared in running shell)
! Qi is frequency dependent (Need to change form of dQ/df accordingly
!														(search for rdQdf to modify it)
!
! Scattering is possible in the surface scattering layer and as a backgroung scattering,
! which occurs everywhere outside of the scattering layer and has a low scat probability.
! Both probabilities have to be declared in the running shell script.
!
! The distance between scatterers can be calculated differently based on the location within
! or without the scattering layer. Global background scattering scale-length is constant 
! (at this point) and set to 10km. This can be changed in the GET_DS_SCAT SUBROUTINE.
!
! Flattening of density is implemented. Change exponent in FLATTEN SUBROUTINE.
!
! 
!
! $Revision$
! $Date$
! $Author$
!
!
!
!     ======================================================
!			----- DECLARATIONS -----
				
				! All declarations in pho_vars except debugging.
				USE pho_vars
				
				IMPLICIT NONE
				
				INTEGER, PARAMETER :: nt0=180000, nx0=91
				DOUBLE PRECISION wf(nx0,nt0,3)        !STACKED DATA
				REAL(8)             w(nt0)
			
				! SOURCE
				! Leaving source declarations here (and not in pho_vars) speeds up the compilation
				! time by a lot (nt0 is large).
				REAL(8)          mts(101,4,nt0)        !ATTENUATED SOURCE
				REAL             mts4(101,4,nt0)        !ATTENUATED SOURCE
				REAL          b(nt0), e(nt0)        !HILBERT TRANSFORM & ENVELOPE
				REAL          mtsc(nt0),datt,dtst1  !SCRATCH SPACE
				INTEGER       ims
				REAL          mt(nt0)               !SOURCE-TIME FUNCTION 
				COMPLEX       ms(nt0),ss(nx0,nt0)   !SOURCE & STACKED SPECTRA
				REAL          nn(nx0,nt0)
				REAL          dt4
							
				
				
				! DEBUGGING
				CHARACTER*100 :: debugfile,logEND
				REAL(8)						 dhsmall
				INTEGER (kind=8) ::  surfcount
			  INTEGER (kind=8) ::  surCYC1,exTIME,exNLAY
			  INTEGER 				 ::  logperc
			  INTEGER (kind=8) ::  scat_time
 
						
!			^^^^^ DECLARATIONS ^^^^^

			!DEBUG
			surCYC1 = 0
			exTIME = 0
			exNLAY = 0

      write(*,*) 'ISOTROPIC Scattering'
      write(*,*) 'Last Edited on $Date$'
      write(*,*) 'Last Edited by $Author$'
      write(*,*) '$Revision$'
      


!     ======================================================
!			----- GET INPUTS FROM USER-----
      
      WRITE(6,*) 'ENTER SEISMIC VELOCITY MODEL FILE NAME'
      READ (*,'(A)') ifile

25    WRITE(6,'(A)') 'ENTER RAY PARAMETER RANGE (p1, p2(P), p2(S)):'
      READ (5,    *)  p1, p2(1), p2(2)

50    WRITE(6,'(A)') 'ENTER TIME WINDOW & SAMPLING INTERVAL (t1, t2, dt):'
      READ (5, *) t1,t2,dti                    !TIME WINDOW & # OF TIME STEPS
      nt = int((t2-t1)/dti) + 1
      
60    WRITE(6,'(A)') 'ENTER DISTANCE RANGE (x1, x2, nx IN DEGREES):'
      READ (5,    *)  x1, x2, nx              !DISTANCE RANGE & # DISTANCE STEP
      dxi = (x2-x1)/float(nx-1)               !DISTANCE SAMPLING INTERVAL

      WRITE(6,'(A)') 'ENTER NUMBER OF RANDOM TRACES TO LAUNCH:'
      READ (5,    *)  ntr
      WRITE(6,*) 'NUM_TRACES:',ntr 
      
      WRITE(6,'(A)') 'ENTER SOURCE DEPTH:'
      READ (5,    *)  qdep
      WRITE(6,*) 'QDEP:',qdep

      WRITE(6,'(A)') 'ENTER 1) EARTH, or 2) MOON:'
      READ (5,    *)  EorM
      WRITE(6,*) 'EorM',EorM

      WRITE(6,'(A)') 'ENTER MAX SCATTERING DEPTH:'
      READ (5,    *)  scat_depth
      WRITE(6,*) 'Base of Scattering Layer (km):',scat_depth

      WRITE(6,'(A)') 'ENTER SCATTERING PROBABILITY IN SCATTERING LAYER:'
      READ (5,    *)  SL_prob
			WRITE(6,*) 'SProb:',SL_prob
			
      WRITE(6,'(A)') 'ENTER BACKGROUND SCATTERING PROBABILITY:'
      READ (5,    *)  BG_prob
			WRITE(6,*) 'BG Prob:',BG_prob
			
			WRITE(6,'(A)') 'ENTER SCATTERER LENGTH-SCALES (km) (MIN, MAX, NPOW):'
      READ (5,    *)  dsmin, dsmax, npow
			WRITE(6,*) 'dsmin/dsmax/npow:',dsmin, dsmax, npow
			
		  WRITE(6,'(A)') 'ENTER VELOCITY PERTURBATION:'
      READ (5,    *)  vel_perturb
			WRITE(6,*) 'vel_perturb:',vel_perturb
			
 			WRITE(6,'(A)') 'ENTER dQdf STYLE (SEE dQdfSTYLES.txt)'
      READ (5,    *)  dQdfSTYLE
			WRITE(6,*) 'dQdf STYLE:',dQdfSTYLE	

      WRITE(6,'(A)') 'ENTER TRACK OUTPUT FILE:'
      READ (5,'(A)')  tfile 
			WRITE(6,*) 'Tfile:',tfile
			
			WRITE(6,'(A)') 'ENTER OUTPUT FILE NAME:'!REQUEST OUTPUT FILE NAME
      READ (5,'(A)')  ofile                   !
      
  		WRITE(6,'(A)') 'ENTER LOG FILE NAME:'!REQUEST LOG FILE NAME
      READ (5,'(A)')  logfile                   !
!			^^^^^ GET INPUTS ^^^^^


!     ======================================================
!			----- INITIALIZE PARAMETERS -----
			
      cmp(1) = 'lpz'
      cmp(2) = 'lpt'
      cmp(3) = 'lpr'
      
      pi = atan(1.)*4.
      re = 6371.
      rm = 1737.
      
      d2r = pi/180.
      
      nttrack = 5								!Set number of time intervals to track
      nttrack_dt = t2/nttrack
			
      n180 = nint(180/dxi)			!Number of intervals in 180deg
      
      dreceiver = 0.05  !radius around receiver in which the phonons will be recorded (deg)
     
      !Initialize random seed for sequence of random numbers used to multiply to clock-based seeds.	  
      CALL INIT_RANDOM_SEED()
      CALL RANDOM_NUMBER(r2s)
      
      OPEN(79,FILE=logfile,STATUS='UNKNOWN')    !OPEN LOG FILE

!			^^^^^ INITIALIZE PARAMETERS ^^^^^



!     ======================================================
!			----- PICK MODEL (Earth & Moon) -----						   
      IF (EorM  ==  1) THEN
       deg2km = re*d2r
       erad   = re
      ELSE
       deg2km = rm*d2r
       erad   = rm
      END IF
      circum = 2*pi*erad
!			^^^^^ PICK MODEL (Earth & Moon) ^^^^^			


!     ======================================================
!			----- CHECKS AND UPDATES ON VELOCITY MODEL -----						   
!				--> One layer is directly at base of scattering layer (need this for scattering to work)
!				--> Thin 100m layer at core to deal with singularity caused by flattening

      corelayer = 0.1        ! Thickness of thin layer near core to deal with singularity
  
			CALL VEL_MODEL_CHECKS
!			^^^^^ CHECKS ON VELOCITY MODEL ^^^^^				
			

!     ======================================================
!			----- APPLY FLATTENING TRANSFORMATION -----

!      OPEN(1,FILE=ifile,STATUS='OLD')    !OPEN SEISMIC VELOCITY MODEL
	  
      DO I = 1, nlay                    
       CALL FLATTEN(z_s(i),vs(i,1),rhs(i),z(i),vf(i,1),rh(i),erad)!FLATTEN P-WAVE VELOCITIES
       CALL FLATTEN(z_s(i),vs(i,2),rhs(i),z(i),vf(i,2),rh(i),erad)!FLATTEN S-WAVE VELOCITIES
      END DO



      WRITE(6,*) 'NLAY:',nlay
      
      WRITE(6,*) ' '      
      WRITE(6,'(A)')'************************* Table of Model Interfaces **********************'
      WRITE(6,'(A)')' Depth  Top Velocities  Bot Velocities    -----FlatEarth Slownesses-----'
      WRITE(6,'(A)')'             vp1  vs1        vp2  vs2       p1     p2      s1      s2'
      
      DO i = 2, nlay 
       IF (z(i) == z(i-1)) THEN                !ZERO LAYER THICK=DISCONTINUITY
        scr1=1./vf(i-1,1)                      !P-VELOCITY UPPER
        scr2=1./vf(i,1)                        !P-VELOCITY LOWER 
        scr3=.999                              !FLAG IF S VELOCITY = ZERO
        IF (vf(i-1,2) /= 0.) scr3=1./vf(i-1,2) !S-VELOCITY UPPER
        scr4=.999                              !FLAG IF S VELOCITY = ZERO
        IF (vf(i  ,2) /= 0.) scr4=1./vf(i  ,2) !S-VELOCITY LOWER
         WRITE(6,FMT=22) z_s(i),i-1,vs(i-1,1),vs(i-1,2), &
                        i,vs(i,1),vs(i,2),scr1,scr2,scr3,scr4
           
        
       END IF
       
      END DO
22    FORMAT (f6.1,2(i5,f6.2,f5.2),2x,2f9.6,2x,2f9.6)

      OPEN(15,FILE='model_flat.txt',STATUS='UNKNOWN')    !OPEN SEISMIC VELOCITY MODEL
      DO I = 1,nlay
      	WRITE(15,*) z_s(I),z(I),vf(I,1),vf(I,2),rhs(I),rh(I),Q(I,1),Q(I,2)
      END DO
      
      CLOSE(15)
      

!			----- Convert depths to flat depth -----
			scat_depth = -erad*dlog((erad-scat_depth)/erad)  !FLATTEN scat_depth
			WRITE(6,*) 'Flattened scattering depth =',scat_depth



!			^^^^^^ APPLY FLATTENING TRANSFORMATION ^^^^^^
			
		
!     ======================================================
!			----- INITIALIZE TRACKING PARAMETERS -----		
!			
!			WRITE(6,*) '!!!!!!!!!!!!!!!!!!!!!'	!DEBUG
!			WRITE(6,*) ' ',nx,nlay,nttrack			!DEBUG
!
!			!Allocate memory for tracking number of phonons in each area
!     ALLOCATE(trackcount(nx,nlay,nttrack))              
!			
!			DO kk = 1,nx
!				DO ll = 1,nlay
!					DO mm = 1,nttrack
!						trackcount(kk,ll,mm) = 0.
!					END DO
!				END DO
!			END DO	
!			^^^^^ INITIALIZE TRACKING PARAMETERS ^^^^^

	

!     ======================================================
!			----- Initialize stacks variable -----			
!      WRITE(6,*) 'ZEROING STACKS:'            !ZERO STACKS
      DO I = 1, nx
       DO J = 1, nt
        DO k = 1, 3
					wf(I,J,k) = 0.
        END DO
       END DO
      END DO
      
      conv_count(1:6) = 0
!			^^^^^ Initialize stacks variable ^^^^^	



!     ======================================================
!			----- Find Source Layer -----
      iz1 = 1
      DO WHILE (qdep >= z_s(iz1+1))               !FIND WHICH LAYER QUAKE STARTS IN
       iz1 = iz1 +1															 !FIRST LAYER IS ASSUMED TO BE AT 0km.
      END DO
		  WRITE(6,*) 'SOURCE DEPTH AND LAYER:',iz1,z_s(iz1)
		  
		  !iz1 will be iz1+1 if downgoing phonon, but this is corrected once the launch
		  ! angle is defined.
!			^^^^^ Find Source Layer ^^^^^
      

		

!     ======================================================
!			----- Generate Source Function -----		           
!      WRITE(6,*) 'CALCULATING SOURCE:'        !CALCULATING SOURCE
      dt4 = REAL(dti,4)
      pi = atan(1.)*4.                        !
!      write(6,*) 'pi =',pi
      P0 = dti*4.                             !DOMINANT PERIOD
!      write(6,*) 'p0 =',P0
      nts = nint(P0*4./dti)+1                 !# OF POINTS IN SOURCE SERIES
!      write(6,*) 'nts =',nts
      IF (nts < 31) nts = 31
      nts1 = 1000
      DO I = 1, nts1
       mt(I) = 0.
      END DO
      DO I = 1, nts                           !SOURCE-TIME FUNCTION
       t0 = dti*float(I-1)-P0
       mt(I) = -4.*pi**2.*P0**(-2.)*(t0-P0/2.) &
               *dexp(-2.*pi**2.*(t0/P0-0.5)**2.)
!       WRITE(6,*) '>>>>',t0,mt(i)
      END DO
			
			
      !Calculate maximum source power (i.e. no attenuation) to normalize attn
      minattn = 0.
      DO JJ = 1,nts
        minattn = minattn + mt(JJ)**2
      END DO

!     ^^^^^ Generate Source Function ^^^^^		           


!     ======================================================
!			----- Attenuation + Attenuated source -----
      datt = .02		! Arbitrary datt, but tstar shouldn't get.lt.2 in Moon.
      							! This is datt, not max att. max att will be datt*(101-1) = 2.
     DO I = 1, 101                           !SOURCES * ATTENUATION
       dtst1 = float(I-1)*datt                !ATTENUATION
       CALL ATTENUATE(mt,mtsc,nts1,dt4,dtst1,dQdfSTYLE) !
       pow1 = 0.
       DO J = 1, nts1                         !
        mts4(I,1,J) =  mtsc(J)                 !
        mts4(I,3,J) = -mtsc(J)                 !
        pow1 = pow1 + mtsc(J)**2
       END DO                                 !
       nfil = 5
       CALL TILBERT(mtsc,dt4,nts1,nfil,b,e)   !HILBER TRANSFORM (pi/2PHASESHFT)
       pow2 = 0.                              !ZERO POWER OF SERIES
       DO K = 1, nts1                         !COPY HILBERT SERIES TO CORRECT
        mts4(I,2,K) = -b(K)                    !
        mts4(I,4,K) =  b(K)
        pow2 = pow2 + b(k)**2                 !CUMULATIVE POWER
       END DO
       DO K = 1, nts1                         !NORMALIZE HILBERTS
        mts4(I,2,K) = mts4(I,2,K)*pow1/pow2     !
        mts4(I,4,K) = mts4(I,4,K)*pow1/pow2     !
       END DO
      END DO
      
      mts = REAL(mts4,8)
			
      OPEN(23,FILE='source.out')              !OUTPUT SOURCE
      WRITE(23,*) nts,101                     !
      WRITE(23,FMT=888) 999.99,(datt*float(J-1),J=1,101)
      DO I = 1, nts
        WRITE(23,FMT=888) float(I-1)*dti,(mts(J,3,I)*1.,J=1,101)
      END DO
      CLOSE(23)
 
      OPEN(24,file='mts.out',status='unknown')
        DO I = 1,101
          DO mm = 1,4
            DO K = 1,nts1
              WRITE(24,*) mts(I,mm,K)
		    END DO
          END DO
		END DO
      CLOSE(24)
!			^^^^^ Attenuation ^^^^^		
!     ======================================================
!   	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   	!!!!! START OF MAIN RAY TRACING LOOP
!   	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Debug vv
      OPEN(77,FILE='Debug_x.txt',STATUS='UNKNOWN')    !OPEN OUTPUT FILE
      OPEN(78,FILE='Track_x.txt',STATUS='UNKNOWN')    !OPEN OUTPUT FILE
      OPEN(76,FILE='Track_p.txt',STATUS='UNKNOWN')    !OPEN OUTPUT FILE

			!Debug
			surfcount = 0.
      CALL etime(elapsed,ttimestart)
			
			
			
      DO I = 1, ntr   !FOR EACH TRACE -- DOLOOP_001
      
      
      CALL etime(elapsed,tt1)
      
      
	  ! ============ >>
      ! ----- Initialize Randomization -----			
       CALL DATE_AND_TIME(values=ntime)
!       nclock = ntime(1)*ntime(2)*ntime(3)*ntime(5)*ntime(6)*ntime(7)*ntime(8)
       DO jj = 5,8
         IF (ntime(jj) == 0) ntime(jj) = 1
       END DO
       
       nclock = ntime(5)*ntime(6)*ntime(7)*ntime(8)
       
       CALL RANDOM_NUMBER(r2s)
       seed = abs((nclock*r2s))! + 11 * (/ (k - 1, k = 1, nseed) /)      
       CALL srand(seed)    
	     r0 = rand()    !First rand output not random
                        ! It is seed (clock) depENDent
!        write(6,*) 'seed:',seed,I,r2s,ntime(8)
!        write(6,*) 'ntime:',ntime(5:8)
      ! ============ <<
       
				 
				! ============ >>
				! Pick P- ,SH- or SV- initial phonon state randomly.
				! Ratios based on Hardebeck:2002
				!
				IF (iz1 == 1)   ip = 1         ! Surface impact = P-wave only
				
				IF (iz1 /= 1) THEN
					r0 = rand()
!					IF (r0 < 1./3.) THEN
!					 ip = 1
!					ELSE IF ((r0 >= 1./3.).AND.(r0 < 2./3.)) THEN
!					 ip = 2
!					ELSE
!					 ip = 3
!					END IF 
					IF (r0 < 1./21.) THEN
						ip = 1 !P
					ELSE IF ((r0 >= 1./21.).and.(r0 < 11./21.)) THEN
						ip = 2 !SH
					ELSE 
						ip = 3 !SV
					END IF
				END IF
        
!        ip = 2
        iwave = ip
				IF (iwave == 3) iwave = 2			          ! ASSUMING ISOTROPY SO v_SH == v_SV
				
				! ============ <<

	   
				! ============ >>
				! Pick take-off angle					 			 
				
				angst = pi/2.                         
        r0 = rand()                            !SELECT RANDOM RAY PARAMETER 
        ang1 = angst*r0                        !Randomly select angle

        ! ============ <<
				
				
        ! ============ >>
        ! Initialize parameters
        t = 0.                                 !SET START TIME = ZERO
        x = 0.                                 !START LOCATION = ZERO
        s = 0.                                 !SET START ATTENUATION = ZERO
        a = 1.                                 !START AMPLITUDE = 1.
        ! a    = cos(ang1*2.-pi/4.)              !SOURCE AMPLITUDE
        totald = 0.                                 !START AT ZERO KM TRAVELED
        x_sign = 1.                            !DISTANCE DIRECTION
        az   = 0.
        ncaust = 0                             !# OF CAUSTICS STARS AT 0.
        
        !Set initial depth index (iz)
   			iz = iz1		!iz1 is layer in which the source is.
				
				!Set up/down direction
        r0 = rand()
        IF ((r0 < 0.5).OR.(iz == 1)) THEN			 !IF QUAKE STARTS AT SURF GO DOWN
         ud = 1	
         iz = iz + 1 !Need this to make sure that source always stars at the same depth.
        ELSE
         ud = -1
        END IF


       !Set ray parameter
        p    = abs(sin(ang1))/vf(iz,iwave)
        
        NITR = 0
        
        n_iter_last = -999
        ix_last = -999
        it_last = -999
        t_last = 0
        ! ============ <<
        
        ! ============ >>
        ! Set scatterer scale-length
        ! ds_scat = 1   !DEBUG
        scat_time = 0
        ! ============ <<			 


			 
			 ! ====================== >>
			 ! Start single ray tracing while loop
			 ! ====================== >>
			 
			 
       DO WHILE ((t < t2).AND.(NITR < 200*nlay)) !TRACE UNTIL TIME PASSES TIME WINDOW - DOLOOP_002
       
       CALL etime(elapsed,tt2)
       !WRITE(6,*) '       Params:',tt2-tt1,I
      
       NITR = NITR + 1
       scat_FLAG = 0
      
       
	     ! Calculate actual phonon depth
       izfac = 0
			 IF (ud == 1) izfac = -1 
			 z_act = z(iz+izfac)    !Depth of phonon before ray tracing  FLAT

			 !DEBUG
!       WRITE(78,*) I,NITR,z_act,x,t,az,p,ip,ds_scat,ds_SL,iz,ud,scat_prob,1
      
			
				! ============ >>
				! Track phonon's position
				!
				! CAN ADD TRACKING FROM statsyn_TRACK.f90 HERE
				! 
				! Track phonon's position
				! ============ <<
				

				! ============ >>
				! SCATTERING LAYER
				

	
						! THE PHONON CAN BE SCATTERED AT ALL DEPTHS. THE ONLY THING THAT CHANGES
						! IS THE SCATTERING PROBABILITY, WHICH DEPENDS ON THE DEPTH.
						
							  iz_scat = iz -1  !Layer in which phonon would scatter
						
								!Set depth-dependent scattering probability
								scat_prob = BG_prob			!Assume background probability at first.
								IF ((z_act <= scat_depth).AND.(SL_prob > 0.)) scat_prob = SL_prob		
								   !Scattering layer probability
								!IF ((z_act == scat_depth).AND.(ud == 1)) scat_prob = BG_prob
								   !Background probability IF leaving scat layer from at base
								IF (iz >= nlay-2) scat_prob = 0. !no scattering near center of Moon
								IF (vf(iz_scat,2) == 0.) scat_prob = 0. !No scattering in liquid layers
  					!Check if scatter at interface
  					r0 = rand()
					  IF ((scat_prob > 0.).AND.(iz > 1).AND.(r0 < scat_prob)) THEN   !CALL INTERFACE_SCATTER
								ud_pre = ud  !save current ud before scattering it.
								CALL REF_TRAN_PROB(p,az,iz_scat,x_sign,ud,iwave,ip,vel_perturb,vf,conv_count,rh)  !Scatter	
								!Fix iz if direction has changed
								IF ((ud_pre == 1).AND.(ud == -1))  iz = iz-1
								IF ((ud_pre == -1).AND.(ud == 1))  iz = iz+1
            END IF

						IF ((z_act == scat_depth).AND.(ud == 1)) scat_prob = BG_prob
						
       CALL etime(elapsed,tt3)
       !WRITE(6,*) '       Prescat :',tt3-tt2,I
!       WRITE(76,*) tt3-tt2

					  IF ((scat_prob > 0.).AND.(iz > 1)) THEN
		
					  !Get iz_scat (layer in which phonon is scattered, if scattered)
					  iz_scat = iz-1
					  
						dh = z(iz) - z(iz-1) !First dh is thickness of layer (FLAT)
						
					 
							 ! Calculate linear distance to next velocity layer (ds_SL)
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
								 
								 imth = 2	!Interpolation method in Layer trace (2 is linear)
								 CALL LAYERTRACE(p,dh,utop,ubot,imth,dx1,dt1,irtr1)
								 ds_SL = (dh**2+dx1**2)**0.5
								 
							IF (irtr1 == 1) THEN !Only scatter if ray would have gone through layer
							
							        scat_FLAG = 1

											!ds_SL is linear distance to next velocity layer
											!If ds_SL > ds_scat, THEN the phonon reach next scatterer 
											!                          before reaching the next layer
											!Calculate first ds_scat (distance to next scatterer)
											CALL GET_DS_SCAT  !change ds_scat  FLAT
										  
										DO WHILE ((ds_scat < ds_SL).AND.(irtr1 == 1))
										
												!Calculate what would dh be if phonon only travelled ds_scat km
												dh = ds_scat*abs(cos(asin(p*vf(iz_scat,iwave))))   !FLAT

												!Calculate vertical to next layer
												IF (ud == 1) dh2 = abs(z(iz_scat+1) - z_act)
									      IF (ud == -1) dh2 = abs(z_act - z(iz_scat))
												
												IF (dh < dh2) THEN
															!Make phonon travel to  next scatterer
															CALL RAYTRACE_SCAT
															
			!												IF (irtr1 /= 0)  z_act = z_act + dh*ud !Calculate new depth FLAT
														  z_act = z_act + dh*ud !Calculate new depth FLAT
															
															!Is phonon scattered at scatterer or does it go through?
															r0 = rand()
															IF (r0 < scat_prob) THEN
																 CALL REF_TRAN_PROB(p,az,iz_scat,x_sign,ud,iwave,ip,vel_perturb,vf,conv_count,rh)   !Scatter
															END IF														
															
															! Calculate new ds_SL based on new ud and p (if it got scattered)
															CALL GET_DS_SL
															
															!New ds_scat
															CALL GET_DS_SCAT
															
															
												ELSE
												
                              ds_scat = 9999
                              
                        END IF												
								      


										 !DEBUG
!										 WRITE(78,*) I,NITR,z_act,x,t,az,p,ip,ds_scat,ds_SL,iz,ud,scat_prob,2
																						
			
										END DO

											 
								 !Leaves WHILE loop when ds_SL < distance to next vel layer
								 !Need to travel to next vel layer
!  								 dh = ds_SL*abs(cos(asin(p*vf(iz_scat,iwave))))   !FLAT
									 IF (ud == 1) dh = abs(z(iz_scat+1) - z_act)
									 IF (ud == -1) dh = abs(z_act - z(iz_scat))
									 !write(77,*) dh,z_act,z(iz_scat),z(iz_scat+1),ud,z_act + dh*ud
									 CALL RAYTRACE_SCAT
									 
									 
									 !Set iz to what it would have been without scattering, based on direction
									 iz = iz_scat+1
									 CALL INTERFACE_NORMAL
									 
									 
       CALL etime(elapsed,tt4)
       !WRITE(6,*) '       Allscat:',tt4-tt3,I
					    	   
			   

							ELSE
  									 ! ============ >>
										 ! RAY TRACING IN LAYER
										 CALL RAYTRACE
										 CALL INTERFACE_NORMAL
										 ! RAY TRACING IN LAYER	
										 ! ============ <<
										 
       CALL etime(elapsed,tt5)
       !WRITE(6,*) '   NoscatInscat:',tt5-tt3,I
										 
							 END IF
							 


        
															
    		ELSE IF (iz > 1) THEN !No scattering in layer (make it faster if scat_prob == 0.)
				
					! ============ >>
					! RAY TRACING IN LAYER
					CALL RAYTRACE
					CALL INTERFACE_NORMAL			
					! RAY TRACING IN LAYER	
					! ============ <<
					
			 CALL etime(elapsed,tt6)
       !WRITE(6,*) '       Noscat  :',tt6-tt3,I
					
				END IF !SCATTERING LAYER IF    					
				! SCATTERING LAYER
				! ============ <<
				
       CALL etime(elapsed,tt7)
       !WRITE(6,*) '       ScatLoop:',tt7-tt3,I				
				
				
				
				! ============ >>
				! RECORD IF PHONON IS AT SURFACE
				IF (iz <= 1) THEN                      !IF RAY HITS SUFACE THEN RECORD
!			    iz = 1
					ud = 1                                !RAY NOW MUST TRAVEL down
					
					!Find index for distance
					x_index = abs(x)
					DO WHILE (x_index >= circum)
					  x_index = x_index - circum
					END DO
					
					IF (x_index >= circum/2) x_index = x_index - 2*(x_index-circum/2)
					ix = nint((x_index/deg2km-x1)/dxi) + 1      !EVENT TO SURFACE HIT DISTANCE 

					xo = x1 + float(ix-1)*dxi					!Distance_index in degrees
					
					IF ( abs(xo-x_index/deg2km) <= dreceiver) THEN
						! phonon is closer THEN 0.05 (dreceiver) deg to a recorder, RECORD AT SURFACE		

										!Time correction if phonon doesn't hit the surface
										! right on the receiver. Max time is when ang1 is 90.					
										dtsurf = (xo-x_index/deg2km)*deg2km*p 
					
										IT = nint((t +dtsurf      -t1)/dti) + 1 
										
								
										ims = int(s/datt)+1
										IF (ims > 100) ims = 100
										IF (ims <=   1) ims =   2
										s1 = float(ims-1)*datt
										s2 = float(ims  )*datt
										frac = (s-s1)/(s2-s1)
										IF (ncaust <= 1) THEN
											icaust = 1
										ELSE
											icaust = ncaust
											DO WHILE (icaust > 4)
												icaust = icaust - 4
											END DO
										END IF
										
										!JFL
										! Calculate angle of incidence. This was not done before so the angle used
										! was the last one from scattering or the initial take-off angle.
										ang1 = asin(p*vf(iz,iwave))
!										WRITE(6,*) ang1/pi*180
					
										IF ( (IT > 1-nts).and.(IT <= nt0+nts) ) THEN
											IF ( (ip == 1) ) THEN
												c_mult(1) = cos(ang1) * cos(az)  !! Vertical Amp from P wave
												c_mult(2) = sin(ang1) * sin(az)  !! Tangential Amp from P wave
												c_mult(3) = sin(ang1) * cos(az)  !! Radial Amp for P wave
											ELSE IF (ip == 2) THEN
												c_mult(1) = sin(ang1)*cos(az)    !! Vertical amp for SV
												c_mult(2) = cos(ang1)*sin(az)    !! Tangential amp for SV
												c_mult(3) = cos(ang1)*cos(az)    !! Radial amp for SV
											ELSE IF (ip == 3) THEN
												c_mult(1) = 0.!cos(ang1)*sin(az) !! Vertical Amp for SH
												c_mult(2) = cos(az)              !! Tangential Amp for SH
												c_mult(3) = sin(az)              !! Radial Amp for SH
											END IF
											
										!JFL	Not sure why p was redefined here, for an s-wave iwave = 2 (??).
										! p is set so it's ang1 that needs to be calculated before rotating
										! the waveform.
!										p    = abs(sin(ang1))/vf(1,2)	!vf(iz,2) but iz == 1 at surface
					
											n_iter_last = nitr
											ix_last = ix
											it_last = it
											
!										WRITE(77,*) t,dtsurf,dti,IT,nts,nt0
											
							
											DO ic = 1, 3
												DO JJ = 1, nts
													JT = IT + JJ - 1
													IF ( (JT > 0).AND.(JT <= nt0).AND.(a /= 0.) ) THEN
														wf(ix,JT,ic) = wf(ix,JT,ic) + a * c_mult(ic) &
																* (   (1.-frac)*mts(ims-1,icaust,JJ) &
																		+ (   frac)*mts(ims  ,icaust,JJ) )!ATTENUATION																		
!													 write(6,*) '----->',wf(ix,JT,ic)
													END IF
												END DO
											END DO
										!Debug
										surfcount = surfcount +1
											
										END IF
										

					ELSE!CYCLE 1
					! If the REAL phonon distance (x) is more than 0.05 (dreceiver) deg from the seismogram at xo,
					! do not record this surface hit (cycle).
					      surCYC1 = surCYC1 +1
					END IF
					        
        END IF

				! RECORD IF PHONON IS AT SURFACE
				! ============ <<

				!GO TO NEXT LAYER
        iz = iz + ud  
       
			 END DO		!CLOSE SINGLE RAY TRACING LOOP - DOLOOP_002
			 ! ====================== <<
			 ! Close single ray tracing while loop
			 ! ====================== <<	

       CALL etime(elapsed,tt8)
       !WRITE(6,*) '        Surface:',tt8-tt7,I							 
			 			 
			 IF (mod(float(I),float(ntr)/20.) == 0) THEN !STATUS REPORT
        WRITE(6,*) nint(float(I)/float(ntr)*100),'% COMPLETE'
			 END IF
      
!				maxcount = 0.
!				DO kk = 1,nx
!					DO ll = 1,nlay
!						IF (trackcount(kk,ll) > maxcount) THEN
!							maxcount = trackcount(kk,ll)
!						END IF
!					END DO
!				END DO

!				trackcount = trackcount / 100.

       CALL etime(elapsed,tt9)
       !WRITE(6,*) '           Rest:',tt9-tt8,I				
       
       CALL etime(elapsed,ttime4)      
      !WRITE(6,*) '---->',I,ttime4-ttime3,'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'

			END DO	!CLOSE MAIN RAY TRACING LOOP - DOLOOP_001
!   	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   	!!!!! CLOSE MAIN RAY TRACING LOOP
!   	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!			======================================================

      CALL etime(elapsed,totaltime)
      WRITE(6,FMT = 871) totaltime-ttimestart,I-1,(totaltime-ttimestart)/(I-1)
871   FORMAT ('Total time = ',f9.2,'s for ',i8,' phonons (',f7.5,'s/p)')	
      WRITE(6,*) 'Total Surface records = ', surfcount
      WRITE(6,*) 'Too far from receiver = ', surCYC1      
      WRITE(6,*) 'Scattered:',  conv_count(1:6)		

			
!			======================================================
!			----- Output Synthetics -----	
			wf(1,1,1) = 1.
      wf(1,1,2) = 1.
      wf(1,1,3) = 1.
      
      DO ic = 1, 3
				ofile2 = trim(ofile)//'.'//cmp(ic)
	
				OPEN(22,FILE=trim(ofile2),STATUS='UNKNOWN')    !OPEN OUTPUT FILE
				 
				 WRITE(22,*) nt,nx
				 WRITE(22,FMT=888) 999.99,(x1+dxi*float(J-1),J=1,nx)
				
					DO I = 1, nt
						DO J = 1, nx
							IF (abs(wf(J,I,ic)) > 999.9999) wf(J,I,ic) = 999.9999*wf(J,I,ic)/abs(wf(J,I,ic))
						END DO
						WRITE(22,FMT=888) t1+float(I-1)*dti,(wf(J,I,ic)*0.1,J=1,nx)
					END DO				
			  
			  CLOSE(22)
							
      END DO
      WRITE(6,*) 'Synthetic outputs done'
      

!			^^^^^ Output Synthetics ^^^^^
			
      
      CLOSE(77)
      CLOSE(78)
 			CLOSE(79)
 			CLOSE(76)
      !Debug ^^
			
			
			
!			======================================================
!			----- Output Energy Tracking -----
			! ADD OUTPUT ENERGY TRACKING HERE FROM EARLIER VERSION 
!			^^^^^ Output Energy Tracking ^^^^^


			

!			======================================================
!			----- Formats -----
878   FORMAT(2(f10.2,1X),f15.5) 
879   FORMAT(2(f10.2,1X),i6,1x,f15.5)      
888   FORMAT(F10.2,1X,361(F10.6,1X))
!			^^^^^ Formats ^^^^^


			STOP
			END PROGRAM STATSYN_GLOBALSCAT
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^








!			======================================================
!   	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   	!!!!! SUBROUTINES
!   	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE INIT_RANDOM_SEED()
    INTEGER :: i, n, nclock
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed
    n=100000
    CALL RANDOM_SEED(size = n)
    ALLOCATE(seed(n))
         
    CALL SYSTEM_CLOCK(COUNT=nclock)
    seed = nclock + 37 * (/ (i - 1, i = 1, n) /)
    CALL RANDOM_SEED(PUT = seed)
          
    DEALLOCATE(seed)
END SUBROUTINE INIT_RANDOM_SEED
      
      
SUBROUTINE ATTENUATE(sin,sout,ndat,dt,tstar,dQdfSTYLE)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   |THIS SUBROUTINE ATTENUATES AN INPUT SIGNAL (sin) BY A VALUE (tstar) !   !
!   |                                                                    !   !
!   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   !   !
!   |     CONTACT: jflawrence@stanford.edu                               !   !
!   |                                                                    !   !
!   |MODIFIED BY JFBG to include frequency depENDence of Qi              !   !
!   |                                                                    !   !
!   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      !   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   |DECLARE VARIABLES AND SET PARAMETERS:                               !   !
      IMPLICIT       NONE
      REAL           sin(*),sout(*),tstar
      INTEGER        ndat,nfreq,npts              !# OF POINTS IN TIME & FREQ DOMAIN
      INTEGER        MAXPTS                  !MAX # OF POINTS & ITERATIONS
      PARAMETER(     MAXPTS = 16384)         !
      REAL           xs(16384)               !SCRATCH SPACE
      COMPLEX        xf(16384),yf(16384)     !SCRATCH SPACE
      REAL           dt,df                   !TIME & FREQ SAMPLING INTERVAL
      REAL           pi                      !SET PI = 3.14....
      REAL           w,dw,dadw                    !FREQUENCY VARIABLES
      REAL           damp
      REAL           rdQdf(16384)
      INTEGER				 dQdfSTYLE,I

      
      CALL NP2(ndat,npts)                    !FIND POWER OF TWO, npts >= ndat
      IF (npts > MAXPTS) THEN               !CHECK THAT ARRAY IS NOT TOO BIG
       WRITE(6,*) 'WARNING: SERIES TRUNCATED TO:',MAXPTS
       npts = MAXPTS
      END IF  

      CALL PADR(sin,ndat+1,npts)             !PAD SERIES WITH ZEROS
      CALL COPYR(sin,xs,npts)                 !COPY INITIAL DENOMINATOR
      
      CALL GET_SPEC(xs,npts,dt,xf,nfreq,df) !GET SPECTRUM OF x
      
      
      pi = atan(1.)*4.                       !SET PI = 3.14....
      dw = 2.*pi*df                          !ANGULAR FREQUENCY SAMPLING INTERVAL
!      write(6,*) 'dw:',dw,pi,df,dt,ndat,npts

      DO I = 1, nfreq
      	!Can give rdQdf any form. 
      	IF (dQdfSTYLE == 1) THEN
      	     rdQdf(I) = 1.      !Q constant at all frequencies
!      	                       WRITE(6,*) 'Atte6'
      	ELSE IF (dQdfSTYLE == 2) THEN
      	     rdQdf(I) = 1. + ((df*float(I-1)-1)*.3)
      	ELSE
      	     rdQdf(I) = 1.
      	END IF
      END DO
      
      dadw = -tstar*dw                       !DERIVATIVE dA(w)di = -dt*dw
      
      DO I = 1, nfreq                        !APPLY ATTENUATION FILTER
       damp =  exp(float(I-1)*dadw/rdQdf(I))
       w     = dw* float(I-1)                !ANGULAR FREQUENCY
       IF (damp < 0.) damp = 0.
       yf(I) = xf(I)*cmplx(damp,0.)
       yf(I) = yf(I)*exp( cmplx(0.,w*tstar/rdQdf(I)))
!       yf(I) = yf(I)*exp( cmplx(0.,w*tstar))
      END DO


      CALL GET_TS(yf,nfreq,df,0,sout,npts,dt) !GET TIME SERIES OF ATTENUATED SPEC
    
      RETURN
END SUBROUTINE ATTENUATE                      !END ATTENUATE
      

SUBROUTINE NP2(npts,np)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   |THIS SUBROUTINE FINES THE POWER OF TWO THAT IS GREATER THAN OR EQUAL!   !
!   |     TO npts.                                                       !   !
!   |                                                                    !   !
!   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   !   !
!   |     CONTACT: jflawrence@stanford.edu                               !   !
!   |                                                                    !   !
!   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      !   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      INTEGER     npts,np                      !INPUT & 2^n=np NUMBER OF POINTS
      np = 2                                   !ASSUME STARTING AT 2
      DO WHILE (npts > np)                    !
       np = np * 2                             !KEEP INCREASING SIZE*2 UNTIL BIG
      END DO
      RETURN
      END SUBROUTINE NP2                       !END NP2


SUBROUTINE COPYR(f1,f2,npts)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   |THIS SUBROUTINE COPIES ONE REAL VECTOR TO ANOTHER                   !   !
!   |                                                                    !   !
!   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   !   !
!   |     CONTACT: jflawrence@stanford.edu                               !   !
!   |                                                                    !   !
!   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      !   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      INTEGER     I,npts                       !STEP & NUMBER OF POINTS
      REAL        f1(*),f2(*)                  !INPUT AND OUTPUT SERIES
      DO I = 1, npts                           !
       f2(I) = f1(I)                           !COPY POINTS
      END DO
      
      RETURN
END SUBROUTINE copyr                           !END COPYR

      
SUBROUTINE PADR(f1,n1,n2)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   |THIS SUBROUTINE COPIES ONE REAL VECTOR TO ANOTHER                   !   !
!   |                                                                    !   !
!   |THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   !   !
!   |     CONTACT: jflawrence@stanford.edu                               !   !
!   |                                                                    !   !
!   |AS WITH ALL MY CODES, BEWARE OF THE BUG. NO GUARANTEES! SORRY!      !   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      INTEGER     I,n1,n2                      !STEP, START & END POINTS
      REAL        f1(*)                        !SERIES TO ADD ZEROS TO
      DO I = n1, n2                            !
       f1(I) = 0.                              !MAKE ZERO
      END DO
      RETURN
END SUBROUTINE padr                            !END PADR
      
      

SUBROUTINE REFTRAN_SH(p,b1,b2,rh1,rh2,ar,at)
! p.139 of Aki and Richards 2nd edition
      REAL(8)       p,ar,at
      REAL(8)       pi,j1,j2,b1,b2,rh1,rh2
      COMPLEX       vb1,rho1,vb2,rho2,psub
      COMPLEX       cone,ctwo,sj1,sj2,cj1,cj2
      COMPLEX       DD,car,cat
				REAL        elapsed(2)
				REAL        totaltimem,ttime1,ttime2
!      pi   = atan(1.)*4.
!      r2d = 180./pi
!      IF (p*b1 <= 1.) THEN
!       j1   = abs(asin(p*b1))
!      ELSE
!       j1 = pi/2.
!      END IF
!      IF (p*b2 <= 1.) THEN
!       j2   = abs(asin(p*b2))
!      ELSE
!       j2   = pi/2. 
!      END IF


      
      vb1    = cmplx(b1,  0.)
      rho1   = cmplx(rh1, 0.)
      vb2    = cmplx(b2,  0.)
      rho2   = cmplx(rh2, 0.)
      psub   = cmplx(p,  0.)                   !MAKE RAY PARAMETER COMPEX      
      
      cone = cmplx(1,0)
      ctwo = cmplx(2,0)
      
      sj1    = vb1 * psub      
      sj2    = vb2 * psub       
      cj1    = csqrt(cone-sj1**2)
      cj2    = csqrt(cone-sj2**2)  
      
      DD   = rho1*vb1*cj1+rho2*vb2*cj2
      car   = (rho1*vb1*cj1-rho2*vb2*cj2)/DD
      cat   = ctwo*rho1*vb1*cj1/DD
      
      ar = REAL(car)
      at = REAL(cat)
      
      !Check for total internal reflection !fix
      IF (b2*p > 1) at = 0

      
      RETURN
END SUBROUTINE REFTRAN_SH



! SUBROUTINE RTCOEF calculates reflection/transmission coefficients
! for interface between two solid layers, based on the equations on 
! p. 144 of Aki and Richards, 2nd edition.
!
!  Inputs:    vp1     =  P-wave velocity of layer 1 (top layer)
!  (REAL)     vs1     =  S-wave velocity of layer 1
!             den1    =  density of layer 1
!             vp2     =  P-wave velocity of layer 2 (bottom layer)
!             vs2     =  S-wave velocity of layer 2
!             den2    =  density of layer 2
!             pin     =  horizontal slowness (ray PARAMETER)
!             PorS    =  1=P-WAVE, 3=SV-WAVE
!  Returns:   rrp     =  down P to P up     (refl)
!  (REAL)     rrs     =  down P to S up     (refl)
!             rtp     =  down P to P down   (tran)
!             rts     =  down P to S down   (tran)
!   OR:
!             arp     =  down S to P up     (refl)
!             ars     =  down S to S up     (refl)
!             atp     =  down S to P down   (tran)
!             ats     =  down S to S down   (tran)
!
! NOTE:  All input variables are REAL.  
!        All output variables are REAL.
!        Coefficients are not energy normalized.
!
SUBROUTINE RTCOEF2(pin,vp1,vs1,den1,vp2,vs2,den2,pors, &
                         rrp,rrs,rtp,rts)
      IMPLICIT     NONE
      REAL(8)         vp1,vs1,den1,vp2,vs2,den2     !VELOCITY & DENSITY
      INTEGER      pors                          !P (1) OR S (2)                          
      COMPLEX      a,b,c,d,e,f,g,H               !TEMPORARY VARIABLES
      COMPLEX      cone,ctwo                     !COMPLEX  = 1 OR = 2
      COMPLEX      va1,vb1,rho1,va2,vb2,rho2     !VELOCITY & DENSITY (COMPLEX)
      REAL(8)         pin                           !INPUT SLOWNESS
      COMPLEX      psub                          !INPUT SLOWNESS (P OR S)
      COMPLEX      si1,si2,sj1,sj2               !SIN OF ANGLE
      COMPLEX      ci1,ci2,cj1,cj2               !COMPLEX SCRATCH
      COMPLEX      term1,term2                   !COMPLEX SCRATCH
      COMPLEX      DEN                           !DENOMINATOR
      COMPLEX      trm1,trm2                     !COMPLEX SCRATCH
      COMPLEX      arp,ars,atp,ats               !REFLECTION & TRANSMISSION COEFS
      REAL(8)         rrp,rrs,rtp,rts               !REFLECTION & TRANSMISSION COEFS
 				REAL        elapsed(2)
				REAL        totaltimem,ttime1,ttime2

      
      va1    = cmplx(vp1,  0.)                   !MAKE VEL & DENSITY COMPLEX
      vb1    = cmplx(vs1,  0.)
      rho1   = cmplx(den1, 0.)
      va2    = cmplx(vp2,  0.)
      vb2    = cmplx(vs2,  0.)
      rho2   = cmplx(den2, 0.)

      psub   = cmplx(pin,  0.)                   !MAKE RAY PARAMETER COMPEX      
      
      cone   = cmplx(1.,0.)                      !COMPLEX 1 & 2
      ctwo   = cmplx(2.,0.)
      
      si1    = va1 * psub                           !SIN OF ANGLE
      si2    = va2 * psub          
      sj1    = vb1 * psub
      sj2    = vb2 * psub       
!
      ci1    = csqrt(cone-si1**2)                ! cosine of angle
      ci2    = csqrt(cone-si2**2)
      cj1    = csqrt(cone-sj1**2)
      cj2    = csqrt(cone-sj2**2)         
!
      term1  = (cone-ctwo*vb2*vb2*psub*psub)
      term2  = (cone-ctwo*vb1*vb1*psub*psub)
      
      a      = rho2*term1-rho1*term2
      b      = rho2*term1+ctwo*rho1*vb1*vb1*psub*psub
      c      = rho1*term2+ctwo*rho2*vb2*vb2*psub*psub
      d      = ctwo*(rho2*vb2*vb2-rho1*vb1*vb1)
      E      = b*ci1/va1+c*ci2/va2
      F      = b*cj1/vb1+c*cj2/vb2
      G      = a-d*ci1*cj2/(va1*vb2)
      H      = a-d*ci2*cj1/(va2*vb1)
      DEN    = E*F+G*H*psub*psub
!
      IF (PorS  ==  1) THEN
       trm1   = b*ci1/va1-c*ci2/va2          
       trm2   = a+d*ci1*cj2/(va1*vb2)
       arp    = (trm1*F-trm2*H*psub*psub)/DEN           !refl down P to P up
       trm1   = a*b+c*d*ci2*cj2/(va2*vb2)       
       ars    = (-ctwo*ci1*trm1*psub)/(vb1*DEN)      !refl down P to S up
       atp    = ctwo*rho1*ci1*F/(va2*DEN)         !trans down P to P down
       ats    = ctwo*rho1*ci1*H*psub/(vb2*DEN)       !trans down P to S down
      ELSE
       trm1   = a*b+c*d*ci2*cj2/(va2*vb2)       
       arp    = (-ctwo*cj1*trm1*psub)/(va1*DEN)      !refl down S to P up
       trm1   = b*cj1/vb1-c*cj2/vb2               
       trm2   = a+d*ci2*cj1/(va2*vb1)
       ars    = -(trm1*E-trm2*G*psub*psub)/DEN          !refl down S to S up
       atp    = -ctwo*rho1*cj1*G*psub/(va2*DEN)      !trans down S to P down 
       ats    = ctwo*rho1*cj1*E/(vb2*DEN)         !trans down S to S down
      END IF
      
      rrp = REAL(arp)!**2+imag(arp)**2)**0.5
      rrs = REAL(ars)!**2+imag(ars)**2)**0.5
      rtp = REAL(atp)!**2+imag(atp)**2)**0.5
      rts = REAL(ats)!**2+imag(ats)**2)**0.5
      
      ! Check for total internal reflection     !fix
      IF (vp2*pin > 1) rtp = 0
      IF (vs2*pin > 1) rts = 0
      IF (vp1*pin > 1) rrp = 0
      IF (vs1*pin > 1) rrs = 0
      

      
      RETURN
END SUBROUTINE RTCOEF2


SUBROUTINE FLATTEN(z_s,vs,rhs,z_f,vf_f,rh_f,erad)
      REAL(8)     z_s,z_f,vf_f,vs,erad,r,rhs,rh_f,pfac
      
      pfac = 2
      
      r=erad-z_s
      z_f=-erad*dlog(r/erad)
      vf_f=vs*(erad/r)
      rh_f = ((erad/r)**(pfac-2))*rhs
      
      RETURN
END SUBROUTINE FLATTEN

SUBROUTINE LAYERTRACE(p,h,utop,ubot,imth,dx,dt,irtr)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   ! LAYERTRACE calculates the travel time and range offset
!   ! for ray tracing through a single layer.
!   !
!   ! Input:    p     =  horizontal slowness
!   !           h     =  layer thickness
!   !           utop  =  slowness at top of layer
!   !           ubot  =  slowness at bottom of layer
!   !           imth  =  interpolation method
!   !                    imth = 1,  v(z) = 1/sqrt(a - 2*b*z)
!   !                         = 2,  v(z) = a - b*z
!   !                         = 3,  v(z) = a*exp(-b*z)
!   !
!   ! RETURNs:  dx    =  range offset
!   !           dt    =  travel time
!   !           irtr  =  RETURN code
!   !                 = -1,  zero thickness layer
!   !                 =  0,  ray turned above layer
!   !                 =  1,  ray passed through layer
!   !                 =  2,  ray turned within layer, 1 segment counted in dx,dt
!   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      IMPLICIT NONE
      REAL(8)   p,h,utop,ubot,dx,dt
      INTEGER   imth,irtr
      REAL(8)   u,y,q,qs,qr,b,vtop,vbot,etau,ex,z,dtau

      IF (h == 0.) THEN      !check for zero thickness layer
         dx=0.
         dt=0.
         irtr=-1
         RETURN         
      END IF
!   !
      u=utop
      y=u-p
      IF (y <= 0.) THEN   !COMPLEX vertical slowness
         dx=0.            !ray turned above layer
         dt=0.
         irtr=0
         !DEBUG
         !IF (I < 11) WRITE(78,*) 'RAY TURNED',p,utop
         RETURN
      END IF
!
      q=y*(u+p)
      qs=sqrt(q)
!   ! special FUNCTION needed for integral at top of layer
      IF (imth == 2) THEN
         y=u+qs
         IF (p /= 0.) y=y/p
         qr=dlog(y)
      ELSE IF (imth == 3) THEN
         qr=atan2(qs,p)
      END IF      
!
      IF (imth == 1) THEN
          b=-(utop**2-ubot**2)/(2.*h)
      ELSE IF (imth == 2) THEN
          vtop=1./utop
          vbot=1./ubot
          b=-(vtop-vbot)/h
      ELSE
          b=-dlog(ubot/utop)/h
      END IF  

      IF (b == 0.) THEN     !constant velocity layer
         b=1./h						! CORRECTED USING SHEARER'S BOOK. WAS -1./h !JFL
         etau=qs
         ex=p/qs
         go to 160
      END IF


!   !integral at upper limit, 1/b factor omitted until END
      IF (imth == 1) THEN
         etau=-q*qs/3.
         ex=-qs*p
      ELSE IF (imth == 2) THEN
         ex=qs/u
         etau=qr-ex
         IF (p /= 0.) ex=ex/p
      ELSE
         etau=qs-p*qr
         ex=qr
      END IF
!   ! check lower limit to see if we have turning point
      u=ubot
      IF (u <= p) THEN   !if turning point,
         irtr=2          !THEN no contribution
         go to 160       !from bottom point
      END IF 
      irtr=1
      q=(u-p)*(u+p)
      qs=sqrt(q)
!
      IF (imth == 1) THEN
         etau=etau+q*qs/3.
         ex=ex+qs*p
      ELSE IF (imth == 2) THEN
         y=u+qs
         z=qs/u
         etau=etau+z
         IF (p /= 0.) THEN
            y=y/p
            z=z/p
         END IF
         qr=dlog(y)
         etau=etau-qr
         ex=ex-z
      ELSE
         qr=atan2(qs,p)
         etau=etau-qs+p*qr
         ex=ex-qr
      END IF      
!
160   dx=ex/b
      dtau=etau/b
      dt=dtau+p*dx     !convert tau to t
!
      RETURN
END SUBROUTINE LAYERTRACE





SUBROUTINE NPOW2(npts,np)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   |THIS SERIES DETERMINES THE POWER OF TWO THAT IS EQUAL OR JUST       !   !
!   |     GREATER THAN THE NUMBER OF POINTS SUPPLIED:                    !   !
      INTEGER npts,np
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      np = 0
      DO WHILE (2**np < npts)
       np = np + 1
      END DO

      RETURN
END SUBROUTINE npow2 



SUBROUTINE TILBERT(a,dt,npts,nfil,b,e)
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
! of time series in the time domain.
!     Inputs:   a    =  time series
!               dt   =  time spacing
!               npts =  number of points in a
!               nfil =  half-number of points in filter
!     RETURNs:  b    =  Hilbert transform of time series
!               e    =  envelope time FUNCTION
!
!   | --- --------- --------- --------- --------- --------- --------- -- |   !
      REAL       a(*),b(*),e(*),h(2001)
      REAL       dt
      INTEGER    npts,nfil,nfiltot,I,J,II,JJ,i1,i2
      SAVE       dt0,npts0,h,nfiltot             !STORE VARIABLES TO SAVE TIME
      pi = atan(1.)*4.                           !SET PI = 3.14.....

      IF ( (dt /= dt0).and.(npts /= npts0)) THEN !SET UP THE FILTER
       nfiltot = 2*nfil+1                        !# OF POINTS IN FILTER
       DO 10 i = 1, nfiltot                      !FOR EACH FILTER POINT
        t=float(i-nfil-1)*dt                     !TIME OF ITH POINT
        IF (i /= nfil+1) THEN                    !CALCULATE FILTER
         h(i) = -1./(pi*t)
        ELSE                                     !AVOID SINGULARITY
         h(i) = 0.
        END IF
10     END DO
       CALL TAPERR(h,nfiltot,0.5,0.5)
       dt0     = dt                              !STORE SAMPLING INTERVAL
       npts0   = npts                            !STORE NUMBER OF POINTS
      END IF

      CALL ZEROR(e,npts)                         !ZERO ENVELOPE
      CALL ZEROR(b,npts)                         !ZERO HILBERT TRANSFORM

      i1 = 1 + nfil
      i2 = npts - nfil
      DO 50 i=i1,i2
       DO 40 j = 1, nfiltot
        ii  = i - (j-nfil-1)
        b(i)= b(i)+a(ii)*h(j)
40     END DO
       b(i)=b(i)*dt
50    END DO
      DO 70 i=i1,i2
       e(i) = ( (a(i)**2+b(i)**2) )**0.5
70    END DO
      RETURN
END SUBROUTINE TILBERT







SUBROUTINE TAPERR(S1,ndat,tap1,tap2)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   | THIS SUBROUTINE TAPERS ANY SIGNAL (S1), OF LENGTH npts, FROM 1 TO  !   ! 
!   |      tap1*npts, AND FROM tap2*npts TO npts.                        !   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      REAL       S1(*),tap1,tap2,PI,cs
      INTEGER    tap1n,tap2n
      INTEGER    I,ndat
      PI = atan(1.0)*4.
      tap1n = nint(tap1*float(ndat))                 !INTEGER TAPER POINT 1
      tap2n = nint(tap2*float(ndat))                 !INTEGER TAPER POINT 2

      DO I = 1, tap1n                                !TAPER FROM 1 TO POINT 1
         cs     = sin(float(I-1)*PI/float(tap1n-1)/2.)
         S1(I)  = S1(I) * cs**2
      END DO
      DO I = 1, tap2n                                !TAPER FROM POINT 2 TO END
         cs     = sin(float(I-1)*PI/float(tap2n-1)/2.)
         S1(ndat-I+1)  = S1(ndat-I+1) * cs**2
      END DO

      RETURN
END SUBROUTINE taperr


SUBROUTINE ZEROR(series,npts)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   |THIS SUBROUTINE ZEROES ANY 1D SERIES OF LENGTH TO POINT, npts:      !   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      REAL series(*)
      INTEGER npts

      DO I = 1, npts
       series(I) = 0.
      END DO

      RETURN
END SUBROUTINE zeror

SUBROUTINE USPH2CAR(lon,lat,x1,y1,z1)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   !THIS SUBROUTINE CONVERTS SPHERICAL COORDINATES (LON,LAT) (RADIAN) TO!   !
!   !     CARTESIAN COORDINATES (X,Y,Z), WITH RADIUS = 1.                !   !
!   !                                                                    !   !
!   !THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   !   !
!   !    CONTACT: jflawrence@stanford.edu                                !   ! 
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   ! DECLARE VARIABLES:                                                 !   !
      REAL(8)    ::  lon,lat                  !LOCATION (SPHERICAL)
      REAL(8)    ::  x1,y1,z1                 !LOCATION (CARTESIAN)
      
      x1 = cos(lat) * cos(lon)             !CARTESIAN POSITION
      y1 = cos(lat) * sin(lon)             !
      z1 = sin(lat)                        !
      
      RETURN                               !
END SUBROUTINE USPH2CAR                    !END LON_LAT_X_Y_Z
      
      
      
      
      
SUBROUTINE UCAR2SPHR(x1,x2,x3,lon,lat)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   !THIS SUBROUTINE CONVERSTS TO LATITUDE AND LONGITUDE.  THE SUB   !   !
!   !     ASSUMES THAT THE COORDINATE IS AT THE SURFACE.             !   !
!   !     OUTPUTS ARE IN RADIANS:                                    !   !
!   !                                                                    !   !
!   !THIS SUBROUTINE WAS WRITTEN BY JESSE F. LAWRENCE.                   !   !
!   !    CONTACT: jflawrence@stanford.edu                                !   ! 
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   ! DECLARE VARIABLES:                                                 !   !
      REAL(8)      lon,lat,x1,x2,x3,pi
      pi = atan(1.)*4.                          !SET PI = 3.14....
      lat = ARSIN(x3)                           !
      IF (x1 == 0.) THEN
       IF (x2 > 0.) lon = pi / 2.
       IF (x2 < 0.) lon = 3. * pi / 2.
      ELSE
       lon = atan(x2 / x1)
       IF (x1 < 0.) lon = lon + pi
       IF ( (x1 > 0.).AND.(x2 < 0.) )	lon = lon + 2. * pi
      END IF
      
      RETURN                                    !
END SUBROUTINE UCAR2SPHR                                      !END UCAR2SPHD
      

SUBROUTINE DIST_TWO_ANGLES(lon1,lat1,lon2,lat2,angdist)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   !THIS SUBROUTINE USES angdis TO DETERMINE THE DISTANCE TWO POINTS    !   !
!   !     GIVEN LONGITUDE, LATITUDE FOR EACH POINT ALL IN DEGREES.       !   !
!   !     THIS SUBROUTINE DOES ALL THE RADIAN TO DEGREE CONVERSIONS.     !   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
      REAL(8)    ::  lat1,lon1,lat2,lon2,angdist
      REAL(8)    ::  x1,x2,x3,y1,y2,y3
      REAL(8)    ::  pi
      REAL       ::  ARCOS

      IF (lat1 /= lat2) THEN
       IF  (abs(lon1-lon2)/abs(lat1-lat2) < 0.02) THEN
        angdist = abs(lat1-lat2)
        RETURN
       END IF
      END IF

      CALL USPH2CAR(lon1,lat1,x1,x2,x3)
      CALL USPH2CAR(lon2,lat2,y1,y2,y3)
      angdist = abs(ARCOS(x1*y1 + x2*y2 + x3*y3))

      RETURN
END SUBROUTINE DIST_TWO_ANGLES

      
   
REAL FUNCTION ARCOS(a)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   !THIS FUCTION DETERMINES THE ARC COSINE OF AN ANGLE (a):             !   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
      IMPLICIT     NONE
      REAL(8)      :: a, aa, pi, pi2
      REAL         :: ARTAN2
      aa = 1D0-a*a
      pi = atan(1.)*4D0
      pi2 = pi/2D0
      
      IF (aa > 0) THEN
       ARCOS = ARTAN2(sqrt(aa),a)
      ELSE
       ARCOS = pi2-sign(pi2,a)
      END IF
      RETURN
END FUNCTION ARCOS



REAL FUNCTION ARSIN(a)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   !THIS FUCTION DETERMINES THE ARC SINE OF AN ANGLE (a):               !   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
      IMPLICIT      NONE
      REAL(8)      ::  a, aa, pi, pi2
      aa = 1.-a*a
      pi = atan(1.)*4D0
      pi2 = pi/2D0
      IF (aa > 0) THEN
       ARSIN = atan(a/sqrt(aa))
      ELSE
       ARSIN = sign(pi2,a)
      END IF
      RETURN
END FUNCTION ARSIN
      
      
      
REAL FUNCTION ARTAN2(y,x)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   !THIS FUCTION DETERMINES THE ARC TANGENT OF AN ANGLE (X AND Y):     !   !
!   !     THIS VERSION OF ARCTAN DOES NOT CHOKE AT (0,0):               !   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
      IMPLICIT        NONE
      REAL(8)        ::  y,x,sign, pi, pi2
      pi = atan(1.)*4D0
      pi2 = pi/2D0
      IF (x == 0) THEN
       ARTAN2 = sign(pi2,y)
      ELSE
       ARTAN2 = atan(y/x)
       IF (x < 0) THEN
        ARTAN2 = ARTAN2+sign(pi,y)
       END IF
      END IF
      RETURN
END FUNCTION ARTAN2

!=======================================
SUBROUTINE INTERFACE_NORMAL
      
      USE pho_vars
      
      IMPLICIT NONE
      
            
				IF ( (iz > 1).AND.(abs(irtr1) == 1).AND. &												!IF1
							(iz < nlay-1) ) THEN
							
					IF ((iz == 2).AND.(ud == -1)) THEN
					! Skip INTERFACE_NORMAL BECAUSE PHONON IS AT SURFACE
					ELSE
							
					h = z(iz)-z(iz-1)		

          IF (ip  ==  2) THEN																							!IF2a
						IF ( (ud == 1) ) THEN               !IF downGOING SH WAVE			!IF3a
							CALL REFTRAN_SH(p,vf(iz-1,2),vf(iz,2),rh(iz-1),rh(iz), &
                           ar,at)
						ELSE IF ((ud == -1) ) THEN          !IF UPGOING SH WAVE				!IF3a
							CALL REFTRAN_SH(p,vf(iz,2),vf(iz-1,2),rh(iz),rh(iz-1), &
                           ar,at)
!							CALL REFTRAN_SH(p,vf(iz-1,2),vf(iz-2,2),rh(iz-1),rh(iz-2), &
!                           ar,at)
						END IF																												!IF3a
          ELSE																														!IF2a
						IF ( (ud == 1) ) THEN               !IF downGOING P-SV WAVE		!IF3b
							CALL RTCOEF2(p,vf(iz-1,1),vf(iz-1,2),rh(iz-1), &
                             vf(iz  ,1),vf(iz  ,2),rh(iz), &
                          ip,arp,ars,atp,ats)
						ELSE IF ((ud == -1) ) THEN          !IF UPGOING P-SV WAVE			!IF3b
							CALL RTCOEF2(p,vf(iz  ,1),vf(iz  ,2),rh(iz  ), &
                             vf(iz-1,1),vf(iz-1,2),rh(iz-1), &
                          ip,arp,ars,atp,ats)     
!							CALL RTCOEF2(p,vf(iz-1,1),vf(iz-1,2),rh(iz-1), &
!                             vf(iz-2,1),vf(iz-2,2),rh(iz-2), &
!                          ip,arp,ars,atp,ats)            
						END IF																												!IF3b
          END IF																													!IF2a
          
          r0 = rand()                       !RANDOM NUMBER FROM 0 TO 1



          
          IF (ip  ==  2) THEN                   !IF SH-WAVE								!IF2b

						IF (h > 0.) THEN                    !IF GRADIENT, THEN				!IF3c
!							IF (r0 < (abs(ar)/(abs(ar)+abs(at)))/P0**2) THEN!CHECK FOR REFLECTN !IF4a
! 								IF (ar < 0) a = -a                !OPPOSITE POLARITY
!								ud = -ud                           !downGOING/UPGOING
!							END IF                              !												!IF4a
						ELSE                                 !IF INTERFACE THEN				!IF3c
							IF (r0 < (abs(ar)/(abs(ar)+abs(at)))) THEN!CHECK FOR REFLECTION	!IF4b
								IF (ar < 0) a = -a                !OPPOSITE POLARITY
								ud = -ud                           !downGOING/UPGOING
							END IF                              !														!IF4b
						END IF                               !												!IF3c

          ELSE                                  !IF P- OR SV-WAVE 				!IF2b
          	IF (h <= 0.) THEN																							!IF3d
          						        
							rt_sum = abs(arp)+abs(atp)+abs(ars)+abs(ats)    !SUM OF REFL/TRAN COEFS
							
  						rt_min = 0.                          !RANGE PROBABILITIES FOR P REFL
							rt_max = abs(arp)/rt_sum             !
							IF ( (r0 >= rt_min).AND.(r0 < rt_max) ) THEN!CHECK IF REFLECTED P !IF4c
								IF (arp < 0) a = -a                 !REVERSE POLARITY
								ud = -ud                            !UPGOING <-> downGOING
								ip = 1                              !P WAVE			
							END IF                               !														!IF4c
           

							rt_min = rt_max                      !RANGE PROBABILITIES 4 SV REFL
							rt_max = rt_max+abs(ars)/rt_sum      !
							IF ( (r0 >= rt_min).AND.(r0 < rt_max) ) THEN!CHECK IF REFLECTED SV  !IF4d
								IF (ars < 0) a = -a                 !REVERSE POLARITY
								ud = -ud                            !UPGOING <-> downGOING
								ip = 3                              !SV WAVE
							END IF                               !															!IF4d

							rt_min = rt_max                      !RANGE PROBABILITIES 4 P TRANS
							rt_max = rt_max+abs(atp)/rt_sum      !
							IF ( (r0 >= rt_min).AND.(r0 < rt_max) ) THEN!CHECK IF TRAMSITTED P	!IF4e
								ip = 1                              !P WAVE
							END IF                               !															!IF4e

							rt_min = rt_max                      !RANGE PROBABILITIES 4 SV TRANS
							rt_max = rt_max+abs(ats)/rt_sum      !
							IF ( (r0 >= rt_min).AND.(r0 <= rt_max) ) THEN!CHECK IF TRANSMITTED SV !IF4f
								ip = 3                              !SV WAVE
							END IF                               !																!IF4f
						END IF      																										!IF3d
          END IF                                !END IF: SH, OR P-SV				!IF2b
         
         END IF
        ELSE IF (iz == nlay-1) THEN               !ONCE HIT OTHER SIDE OF CORE  !IF1
					ud = -ud
					dt1 = (2*corelayer)/vf(iz,iwave)
					x = x + 180*deg2km
					t = t + dt1
					totald = totald + 2*corelayer
					s = s + dt1/Q(nlay,iwave)
					
				
				!debug
!				WRITE(*,*) '--->',r0
!  						IF (I < 11) WRITE(78,*) 'COEFFICIENTS:',irtr1,r0, &
!  											abs(arp)/rt_sum,abs(atp)/rt_sum,abs(ars)/rt_sum,abs(ats)/rt_sum,p,'WAVE',ip																															!IF1
	
        END IF				!IF1
        
				!FIX NEXT IF FOR DIFFRACTED WAVES: 
				IF (irtr1 == 2) THEN             !RAY TURNS IN LAYER FOLLOW 1 LEN
				ud = -ud
				ncaust = ncaust + 1                   !# OF CAUSTICS
				END IF
				
!				ip = 2
				iwave = ip
				IF (iwave == 3) iwave = 2			          ! ASSUMING ISOTROPY SO v_SH == v_SV
			
			RETURN	
END SUBROUTINE INTERFACE_NORMAL



!SUBROUTINE INLAYER_SCATTER
!      
!      USE pho_vars
!      IMPLICIT NONE
!      
!     ! WRITE(6,*) 'Made it:', I,NITR
!!      INTEGER     ud_pre  !ud before scattering
!!
!!      !Check if scatter first
!!      r0 = rand()
!!      IF (r0 < scat_prob) THEN 
!!      
!!     	     
!!				 r0 = rand()
!!				 IF (r0 < 0.5) x_sign=-x_sign		
!!				 r0 = rand()
!!				 IF (r0 < scat_prob) ud = -ud
!!		 
!!				 r0 = rand()                       !SELECT RANDOM RAY PARAMETER 
!!				 ang1 = angst*r0
!!				 p = abs(sin(ang1))/vf(iz_scat,iwave)
!!				 
!!
!!				 DO WHILE ((p < p1).OR.(p >= 1./vf(iz_scat,iwave)) ) !p2(iwave)))
!!				   r0 = rand()                       !SELECT RANDOM RAY PARAMETER 
!!				   ang1 = angst*r0
!!				   p = abs(sin(ang1))/vf(iz_scat,iwave)
!!				 END DO
!!				 
!!		 
!!				 r0 = rand()                        
!!				 r1 = rand()                        
!!				 IF (r1 < 0.5) az = az - pi
!!				 az = az + asin(r0**2)              
!!				 IF (az < -pi) az = az + 2.*pi
!!				 IF (az >  pi) az = az - 2.*pi
!!
!!        END IF
!
!      r0 = rand()
!      IF (r0 < scat_prob) THEN
!         CALL REF_TRAN_PROB
!      END IF
!
!
!      RETURN
!END SUBROUTINE INLAYER_SCATTER


!SUBROUTINE INTERFACE_SCATTER
!      
!      USE pho_vars
!      IMPLICIT NONE
!      INTEGER     ud_pre2  !ud before scattering
!      REAL    time10,time11
!      
!      
!      CALL etime(elapsed,time10)
!     
!
!      !Check if scatter first
!      r0 = rand()
!      IF (r0 < scat_prob) THEN 
!      
!      ud_pre2 = ud  !save current ud before scattering it.
!      
!			 CALL REF_TRAN_PROB
!
!		  
!		 !Fix iz if direction has changed
!		 IF ((ud_pre2 == 1).AND.(ud == -1))  iz = iz-1
!     IF ((ud_pre2 == -1).AND.(ud == 1))  iz = iz+1		  
!      
!				  
!				!r0 = rand()
!!!				 IF (r0 < 0.5) x_sign=-x_sign		
!!!				 r0 = rand()
!!!				 IF (r0 < 0.5) ud = -ud
!!!				 
!!!  			 IF (z_act <= 0.) ud = 1 !make sure you're going down if leaving surface
!!!				
!!!				 !Fix iz if direction has changed
!!!				 IF ((ud_pre == 1).AND.(ud == -1))  iz = iz-1
!!!         IF ((ud_pre == -1).AND.(ud == 1))  iz = iz+1	
!!!		 
!!!				 r0 = rand()
!!!				 r0 = ( r0 - 0.5 )
!!!				 p = p1 + r0*(1./vf(iz,iwave)-p1)!*scat_prob
!!!
!!!				 DO WHILE ((p < p1).OR.(p >= 1./vf(iz-1,iwave)) ) !p2(iwave)))
!!!				 r0 = rand()                       !SELECT RANDOM RAY PARAMETER 
!!!				 ang1 = angst*r0
!!!				 p = abs(sin(ang1))/vf(iz,iwave)
!!!				 END DO
!!!				 
!!!
!!!		 
!!!				 r0 = rand()                        !
!!!				 r1 = rand()                        !
!!!				 IF (r1 < 0.5) az = az - pi
!!!				 az = az + asin(r0**2)                  !
!!!				 IF (az < -pi) az = az + 2.*pi
!!!				 IF (az >  pi) az = az - 2.*pi
!				
!      END IF	
!      
!      CALL etime(elapsed,time11)
!      
!      !WRITE(6,*) '--------->',time11-time10
! 
!			
!			RETURN	
!END SUBROUTINE INTERFACE_SCATTER
      
      
SUBROUTINE RAYTRACE
      
      USE pho_vars
	
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
					dtstr1 = dt1/Q(iz,iwave)                    !t* = TIME/QUALITY FACTOR
				ELSE
					irtr1  = -1
					dx1    = 0.
					dt1    = 0.
					dtstr1 = 0.
				END IF
        
        IF (irtr1 == 0) THEN			
         ud = -ud
        ELSE IF (irtr1 >= 1) THEN
         totald = totald + ((z_s(iz)-z_s(iz-1))**2+dx1**2)**0.5 !DISTANCE TRAVELED IN LAYER
         !JFL --> Should this be z(iz) (FLAT z), because dx1 was calculated in the flat model
         ! totald isn't used anywhere though.
         t = t + dt1                    !TRAVEL TIME
         x = x + dx1*x_sign*cos(az)     !EPICENTRAL DISTANCE TRAVELED-km
         s = s + dtstr1                 !CUMULATIVE t*
         
        END IF
        
				RETURN 			
END SUBROUTINE RAYTRACE

SUBROUTINE RAYTRACE_SCAT
      
      USE pho_vars
			IMPLICIT NONE
			REAL(8)   ztop,zbot,vtop,vbot,z_pos,vgrad

			
			!First need to interp velocities at z_act and at z_act+dh
			! based on velocities at top and at the bottom of the layer
 
      vgrad = (vf(iz_scat+1,iwave)-vf(iz_scat,iwave))/(z(iz_scat+1)-z(iz_scat))  !FLAT GRADIENT
      z_pos = z_act + dh*ud  !Final z position							 !FLAT
      
      IF (ud == 1) THEN
          ztop = z_act
          zbot = z_pos
      ELSE IF (ud == -1) THEN
      		ztop = z_pos
      		zbot = z_act
      END IF 
      
	
				  IF (abs(vf(iz_scat,iwave)) > 0.) THEN !USELESS WHEN set no scattering in liquid
				    vtop = vf(iz_scat,iwave) + (ztop-z(iz_scat)) * vgrad
				    utop = 1./vtop              !SLOWNESS AT 1st SCATTERER
				  ELSE
				    utop = 0.
				  END IF 
		
					IF (abs(vf(iz_scat,iwave)) > 0.) THEN !USELESS WHEN set no scattering in liquid
		        vbot = vf(iz_scat,iwave) + (zbot-z(iz_scat)) * vgrad
						ubot = 1./vbot                !SLOWNESS AT 2nd SCATTERER
					ELSE
						ubot = 0.
					END IF
         
					imth = 2                              !INTERPOLATION METHOD

					CALL LAYERTRACE(p,dh,utop,ubot,imth,dx1,dt1,irtr1)
					
					dtstr1 = dt1/Q(iz_scat,iwave)                    !t* = TIME/QUALITY FACTOR
        
        IF (irtr1 == 0) THEN
         ud = -ud
        ELSE IF (irtr1 >= 1) THEN
         totald = totald + ds_scat !DISTANCE TRAVELED IN LAYER
         
         t = t + dt1                    !TRAVEL TIME
         x = x + dx1*x_sign*abs(cos(az))     !EPICENTRAL DISTANCE TRAVELED-km
         s = s + dtstr1                 !CUMULATIVE t*
        END IF
      
      RETURN  
END SUBROUTINE RAYTRACE_SCAT
      
      
SUBROUTINE GET_DS_SCAT
      !Subroutine is used to calculate ds_scat (distance between scatterers). The method
      ! depENDs on the depth of scattering.
      !
      !The output dscat has been
      
      USE pho_vars      
      IMPLICIT NONE
      REAL(8)     rt,z_ft,fac,bg_sl,z_nf
      
      bg_sl = 10
      

      ds_scat_nf = bg_sl   !background scatterer scale-length ~10km

      r0 = rand()
         
      IF ((z_act <= scat_depth).AND.(SL_prob > 0)) THEN
            !Change uniform distribution into power-law distribution
            ds_scat_nf = ((dsmax**(npow+1) - dsmin**(npow+1))*r0 & 
																				+ dsmin**(npow+1))**(1/(npow+1))
			END IF
			
			IF ((z_act == scat_depth).AND.(ud == 1)) THEN
						ds_scat_nf = bg_sl   !background scatterer scale-length ~10km
			END IF
			
			!Find approximate flattening factor, based on actual depth z_act
			! z_act is FLAT, find what z_act would be if NONFLAT, ratio gives
			! scale_length factor
      z_nf = erad - erad*dexp(z_act/(-1*erad)) ;
		
			IF ((z_nf == 0).OR.(z_act == 0)) THEN
			   fac = 1
			ELSE
			   fac = z_act/z_nf
			END IF

			  ds_scat = fac*ds_scat_nf  !Flatten ds_scat_nf (approximation based on mid depth)
			  
			  IF (ds_scat == 0) WRITE(*,*) 'ds_scat is 0!!'

      
      !DEBUG
!      WRITE(78,*) 'NEXT ds_scat =',ds_scat
!      WRITE(78,*) ds_scat_nf,z_act,z_mid,r,z_f
		
	
			RETURN			 
END SUBROUTINE GET_DS_SCAT
      
SUBROUTINE GET_DS_SL
      
      USE pho_vars
      IMPLICIT NONE
      
      REAL(8) dh_temp,vscat,vgrad
			 
			 IF (ud == -1) THEN
			     dh_temp = abs(z_act - z(iz_scat)) ! Distance to vel layer above			     
			 ELSE IF (ud == 1) THEN
			     dh_temp = abs(z_act - z(iz_scat+1))  ! Distance to vel layer below
			 END IF
			 
			vgrad = (vf(iz_scat+1,iwave)-vf(iz_scat,iwave))/(z(iz_scat+1)-z(iz_scat))  !FLAT GRADIENT
			vscat = vf(iz_scat,iwave) + abs((z_act-z(iz_scat))) * vgrad

      
      IF (ud == -1) THEN
          utop = 1./vf(iz_scat,iwave)
          ubot = 1./vscat
      ELSE IF (ud == 1) THEN
      		utop = 1./vscat
      		ubot = 1./vf(iz_scat+1,iwave)
      END IF 
      
			 
			 CALL LAYERTRACE(p,dh_temp,utop,ubot,imth,dx1,dt1,irtr1)
			 ds_SL = (dh_temp**2+dx1**2)**0.5
      
      RETURN
END SUBROUTINE GET_DS_SL

SUBROUTINE REF_TRAN_PROB(p,az,iz_scat,x_sign,ud,iwave,ip,vel_perturb,vf,conv_count,rh)
!      USE pho_vars
      IMPLICIT NONE
      
      INTEGER, PARAMETER :: nlay0=1000

      REAL(8) ::  rt(10)
      REAL(8) :: art(10),ref_tran_sum
      REAL(8) :: vp2,vs2,rh2,v2              !! P & S  & density of layer 2 (generic vel=v2)
      REAL(8) :: azr                         !! azimuth in radians
      REAL(8) :: ta(3),n1(3),tb(3),n2(3)        !! Phonon trajectory, reflector normal & new traject
      REAL(8) :: ca,sa,ci,si              !! Cosine & sine of azimuth & inclination
      REAL(8) :: fact                     !! Impedence contrast factor
      REAL(8) :: inc
      INTEGER :: ip2,irt
      INTEGER :: iwave2,rin,iwave_in
      REAL(8) :: theta,phi,theta2,phi2
      REAL(8) :: pp,ps                 !! Ray parameters for P & S waves
      REAL(8) :: GET_ANG,ang2,p_in
      REAL       rrr1,rrr2
      INTEGER    jj
      REAL(8)    pi
      
      REAL(8)    p,az,x_sign,r0
			INTEGER    iwave,ip,ud,iz_scat
      REAL(8) :: vel_perturb 
			REAL(8)    vf(nlay0,2),rh(nlay0)
      INTEGER       conv_count(6)
      
      !For REF_TRAN_RAY
     REAL(8) :: vc !! Velocities of two media
     REAL(8) :: dp,DOT_PRODUCT_3,dp2,sg
      
            
      !CALL etime(elapsed,rrr1)      

      iwave_in = iwave
      p_in = p

      pi = atan(1.)*4D0
      
      IF (isnan(asin(p*vf(iz_scat,iwave)))) RETURN
      
      !Zero coefficient vectors
      rt(1:10) = 0
      art(1:10) = 0
      
     
      !Incoming ray      
      phi = asin(p*vf(iz_scat,iwave))          !!  
      theta   = az   !fix, az is already in radians   *pi/180.                !!
      ta(1) = sin(phi)*cos(theta)*x_sign   !! East  !fix added x_sign
      ta(2) = sin(phi)*sin(theta)          !! North
      ta(3) = cos(phi)*ud                  !! Vertical  !fix added ud

      CALL UNIT_VECTOR_3(ta)
      
      ! Create inclination & declination of reflector plane (normal to plane)
      r0 = rand()
      theta2 = 2.*pi*(r0-0.5) 
      r0 = rand()
      phi2   =    pi*(r0-0.5) 
      
      n1(1) = sin(phi2)*cos(theta2)  
      n1(2) = sin(phi2)*sin(theta2)
      n1(3) = cos(phi2)

      CALL UNIT_VECTOR_3(n1)
     
      ! Sin & Cos of inc and az      
      sa = sin(phi-phi2)
      si = sin(theta-theta2)
      ca = cos(phi-phi2)
      ci = cos(theta-theta2)
      
      ang2 = abs(GET_ANG(ta,n1))           !! Angle between phonon trajectory & norm
                                           !! do not need abs because it is abs in GET_ANG
                                           !! ang2 should be <= 90deg (pi/2)
      IF (ang2 > pi/2) THEN    !fix
        n1 = -1.*n1
        ang2 = abs(GET_ANG(ta,n1))
      END IF
      
      pp = sin(ang2)/vf(iz_scat,1)              !! P-wave ray parameter
      ps = sin(ang2)/vf(iz_scat,2)              !! S-wave ray parameter


      ! Create random velocity contrast between 0.05 and -0.05 (or value of vel_perturb)
      fact = 1.0 + vel_perturb*((rand()-0.5)*2)!! Factor of impedence contrast 
      vp2 = vf(iz_scat,1)*fact                  !! New P & S wave velocities for other side
      vs2 = vf(iz_scat,2)*fact                  !!
      rh2 = rh(iz_scat)*fact                    !! 
      
      
      IF (ip == 1) THEN                      !! P Incident
       CALL RTCOEF2(pp,vf(iz_scat,1),vf(iz_scat,2),rh(iz_scat),vp2,vs2,rh2,1,rt(1),rt(2),rt(3),rt(4)) !! P-SV reflected & transmitted (Pr=1,Sr=2,Pt=3,St=4)
       rt(1)=rt(1)                         !! P  refl
       rt(5)=rt(2)*sa                      !! SH refl
       rt(2)=rt(2)*ca                      !! SV refl
       rt(3)=rt(3)                         !! P  trans
       rt(6)=rt(4)*sa                      !! SH trans
       rt(4)=rt(4)*ca                      !! SV trans
       
       DO jj = 1, 10                        !! If errors, zero them 
        IF (isnan(rt(jj))) rt(jj) = 0.
       END DO
       
      ELSE
       CALL REFTRAN_SH(ps,vf(iz_scat,2),vs2,rh(iz_scat),rh2,rt(9),rt(10))   !! SH reflected & transmitted amplitudes
       CALL RTCOEF2(ps,vf(iz_scat,1),vf(iz_scat,2),rh(iz_scat),vp2,vs2,rh2,2,rt(5),rt(6),rt(7),rt(8)) !! SV-P reflected & transmitted (Pr=5,Sr=6,Pt=7,St=8)
       DO jj = 1, 10                        !! If errors, zero them
        IF (isnan(rt(jj))) rt(jj) = 0.
       END DO    
  
       IF (ip==2) THEN                     !! SV Incidence   !fix, SV is ip == 2
       
        rt(1)=rt(5)*ca                     !! P  refl
        rt(2)=abs(rt(6))*ca+abs(rt(9))*sa  !! SV refl  !fix
        rt(3)=rt(7)*ca                     !! P  trans
        rt(4)=abs(rt(8))*ca+abs(rt(10))*sa !! SV trans !fix
	      rt(5)=abs(rt(6))*sa+abs(rt(9))*ca  !! SH refl  !fix
	      rt(6)=abs(rt(8))*sa+abs(rt(10))*ca !! SH trans !fix
	
       ELSE                                !! SH Incidence
        rt(1)=rt(5)*sa                     !! P  refl
        rt(2)=abs(rt(6))*sa+abs(rt(9))*ca  !! SV refl  !fix
        rt(3)=rt(7)*sa                     !! P  trans
        rt(4)=abs(rt(8))*sa+abs(rt(10))*ca !! SV trans !fix
      	rt(5)=abs(rt(6))*ca+abs(rt(9))*sa  !! SH refl  !fix
	      rt(6)=abs(rt(8))*ca+abs(rt(10))*sa !! SH trans !fix
       END IF
      END IF
      
           
      !Sumcum coefficients
      ref_tran_sum = 0.                                    !! Zero total prob normalization
      art(1)= abs(rt(1))                                   !! prob=abs(ref or trans coeff)
      DO jj = 2, 6
        art(jj) = art(jj-1) + abs(rt(jj))!! Sum the absolute value of reflection & transmittion coefficients
      END DO 

      !Normalize cumulative coefficients
      ref_tran_sum = art(6)   
      art = art/ref_tran_sum  
            
      IF (isnan(ref_tran_sum).or.ref_tran_sum==0.) RETURN  !! ERROR SO SKIP
    
     
      r0 = rand()
      
      IF (r0 < art(1)) THEN                               !! Determine output phase & set vars
       ip2 = 1                                            !! P reflected
       irt = -1
       rin = 1
       conv_count(1) = conv_count(1) + 1
      ELSE IF (r0 < art(2)) THEN                          !! SV reflected
       ip2 = 2 !fix was 3
       irt = -1
       rin = 2
       conv_count(2) = conv_count(2) + 1
      ELSE IF (r0 < art(3)) THEN                          !! P transmitted
       ip2 = 1
       irt = 1
       rin = 3
       conv_count(3) = conv_count(3) + 1
      ELSE IF (r0 < art(4)) THEN                          !! SV transmitted
       ip2 = 2 !fix was 3
       irt = 1
       rin = 4
       conv_count(4) = conv_count(4) + 1
      ELSE IF (r0 < art(5)) THEN                          !! SH reflected
       ip2 = 3 !fix was 2
       irt = -1
       rin = 5
       conv_count(5) = conv_count(5) + 1
      ELSE IF (r0 < art(6)) THEN                          !! SH tranmsitted
       ip2 = 3 !fix was 2
       irt = 1
       rin = 6
       conv_count(6) = conv_count(6) + 1
      END IF   
      
      iwave2 = ip2                                        !! Set output wave velocity P=1,S=2
      IF (ip2 == 3) iwave2 = 2

      IF (irt==1) THEN                                    !! Velocity in ref/trans layer
       IF (ip2/=1) THEN
        v2 = vs2
       ELSE
        v2 = vp2
       ENDIF
      ELSE
       v2  = vf(iz_scat,iwave2)
      ENDIF
      
      ! Get the new trajectory
			 ! ~========================================================
      ! REF_TRAN_RAY used to be a subroutine but it slowed down TITUS a lot, so
      ! included in this subroutine
!      CALL ref_tran_ray(n1,ta,vf(iz_scat,iwave),v2,irt,tb)     
      
      CALL UNIT_VECTOR_3(n1) 
      CALL UNIT_VECTOR_3(ta)
      
      vc = v2/vf(iz_scat,iwave)   !u1/u2
      dp = DOT_PRODUCT_3(n1,ta)
      sg = abs(dp)/dp
      
      ! Find transmitted vector
      tb = (sg*(1.-vc**2+dp**2*vc**2)**.5-dp*vc)*n1+vc*ta
      
      !For reflection, reflect vector on plane (normal is n1)
			 IF (irt == -1) THEN
				 dp2 = DOT_PRODUCT_3(tb,n1)
				 DO jj = 1,3
					 tb(jj) = tb(jj) - 2*dp2*n1(jj)
				 END DO
			 END IF 
			 
			 ! ~========================================================

      ! set up-down variable   
      !IF (tb(1)+tb(2)+tb(3) < 0) ud = -ud                 !fix
      IF (tb(3) < 0) THEN                           !fix IF LOOP
        ud = -1
      ELSE  
        ud = 1 
      END IF
      
      ! set east-west variable         
      IF (tb(1) < 0) THEN                           !fix IF LOOP
        x_sign = -1.
      ELSE  
        x_sign = 1.
      END IF
      
      ! Get azimuth & inclination
      n2(1:3) = 0
      n2(3) = 1 !Make vertical vector, normal to horizontal plane
			inc = abs(GET_ANG(tb,n2))
      IF (inc > pi/2) THEN    !fix
        n2 = -1.*n2
        inc = abs(GET_ANG(tb,n2))
      END IF
      
      n2(1:3) = 0
      n2(3) = 1 !Make horizontal vector, pointing East (x), azimuth is form x so that cos(az == 0) = 1
			az = abs(GET_ANG(tb,n2))
      IF (az > pi/2) THEN    !fix
        n2 = -1.*n2
        az = abs(GET_ANG(tb,n2))
      END IF

      iwave = iwave2
      ip = ip2
      p = sin(inc)/vf(iz_scat,iwave2)
      
      !CALL etime(elapsed,rrr2)
      

      RETURN
END SUBROUTINE REF_TRAN_PROB



DOUBLE PRECISION FUNCTION DOT_PRODUCT_3(a,b)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   ! This function calculates the dot product of two 3-component vectors!   !
!   !                                                                    !   !
!   ! This function   was written by Jesse F. Lawrence                   !   !
!   !      Contact:   jflawrence@stanford.edu                            !   !
!   !                                                                    !   !
!   ! Beware of the bug!! (I don't know which one, but it must be there!)!   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
    IMPLICIT NONE                                 !! Allow no implicit variables
    REAL(8) :: a(3),b(3)                          !! Input vectors
        
    DOT_PRODUCT_3 = a(1)*b(1)+a(2)*b(2)+a(3)*b(3) !! Calculate the dot product
     
    RETURN   
END FUNCTION DOT_PRODUCT_3



DOUBLE PRECISION FUNCTION GET_ANG(x1,x2)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   ! This function gets the angle between two vectors.                  !   !
!   !                                                                    !   !
!   ! This SUBROUTINE was written by Jesse F. Lawrence                   !   !
!   !      Contact:   jflawrence@stanford.edu                            !   !
!   !                                                                    !   !
!   ! Beware of the bug!! (I don't know which one, but it must be there!)!   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
   implicit             none
   REAL(8)           :: x1(3),x2(3)               !! two vectors
   REAL              :: ARCOS                     !! Inverse cosine
   REAL(8)           :: DOT_PRODUCT_3             !! The dot product
   REAL(8)           :: x1mag,x2mag
   
   x1mag = (dot_product(x1,x1))**.5
   x2mag = (dot_product(x2,x2))**.5
   
!   GET_ANG = abs(ARCOS(DOT_PRODUCT_3(x1,x2)))     !! Calculate the angle
   GET_ANG = abs(ARCOS(DOT_PRODUCT_3(x1,x2)/(x1mag*x2mag)))     !! fix
   
   
   ! FIX EXPLANATION
   !x1 . x2 = |x1||x2|cos(ang)
   !(x1.x2) / (|x1||x1|) = cos(ang)
   !ang = acos((x1.x2) / (|x1||x1|))
   ! Shouldn't have been a problem as we are using unit vectors, but probably better
   ! to generalize the function
   
   RETURN                                         !! Return
END FUNCTION GET_ANG






SUBROUTINE UNIT_VECTOR_3(n)
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   ! This SUBROUTINE makes a unit vector out of input vector, n.        !   !
!   !                                                                    !   !
!   ! This SUBROUTINE was written by Jesse F. Lawrence                   !   !
!   !      Contact:   jflawrence@stanford.edu                            !   !
!   !                                                                    !   !
!   ! Beware of the bug!! (I don't know which one, but it must be there!)!   !
!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
   IMPLICIT           NONE
   REAL(8)         :: n(3)
   REAL(8)         :: n_mag
   REAL(8)         :: DOT_PRODUCT_3
   
   n_mag = (DOT_PRODUCT_3(n,n))**0.5
   n(1:3) = n(1:3)/n_mag
   RETURN
END SUBROUTINE UNIT_VECTOR_3




!SUBROUTINE REF_TRAN_RAY(n1,ta1,v1,v2,itr,ta2)
!!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!!   ! This subourine calculates the vector of the new trajectory after   !   !
!!   !      transmission or reflection (itr).                             !   !
!!   !                                                                    !   !
!!   ! This SUBROUTINE was written by Jesse F. Lawrence                   !   !
!!   !      Contact:   jflawrence@stanford.edu                            !   !
!!   !                                                                    !   !
!!   ! Beware of the bug!! (I don't know which one, but it must be there!)!   !
!!   ! --- --------- --------- --------- --------- --------- --------- -- !   !
!   IMPLICIT NONE
!   REAL(8) :: n1(3),ta1(3),ta2(3)
!   REAL(8) :: a1(3),b1(3),a2(3),b2(3)
!   INTEGER :: i
!   REAL(8) :: dp_tn
!   REAL(8) :: v1,v2,vc !! Velocities of two media
!   REAL(8) :: dp,DOT_PRODUCT_3,b2_mag,dp2,sg
!   INTEGER :: itr,jj
!!
!!                          b1  ^
!!                       \------\
!!                              \
!!                       |-     \     ^
!!                        \     \     \
!!                         \    \     \
!!                          \   \     \  a1
!!                      ta1  \  \     \
!!                            \ \     \
!!                             \\     \
!!          --------------------\-------------------->
!!                    \         \\.
!!                    \         \  \.
!!                    \         \    \.    ta2
!!                a2  \         \      \.
!!                    \         \        \.
!!                    \         \          _|
!!                    V         \
!!                              \ ---------->
!!                              \     b2
!!
!!
!!        a1 = - (ta1.n1)*n1
!!        b1 = a1 - ta1 = n1 (1+n1.ta1)
!!        b2 = v2/v1 * b1 = v2/v1 * n1 (1-n1.ta1)
!!        a2 = - sqrt[1-b2_mag^2]*n1 
!
!
!!   nu = v2^-1/v1^-1;
!!   ta2 = (sign(dot(ta1,n1))*(1-nu^2+dot(n1,ta1)^2*nu^2)^.5-dot(n1,ta1)*nu)*n1+nu*ta1
!       
!   
!
!      CALL UNIT_VECTOR_3(n1) 
!      CALL UNIT_VECTOR_3(ta1)
!      
!      vc = v2/v1   !u1/u2
!      dp = DOT_PRODUCT_3(n1,ta1)
!      sg = abs(dp)/dp
!      
!      ! Find transmitted vector
!      ta2 = (sg*(1.-vc**2+dp**2*vc**2)**.5-dp*vc)*n1+vc*ta1
!      
!      !For reflection, reflect vector on plane (normal is n1)
!			 IF (itr == -1) THEN
!				 dp2 = DOT_PRODUCT_3(ta2,n1)
!				 DO jj = 1,3
!					 ta2(jj) = ta2(jj) - 2*dp2*n1(jj)
!				 END DO
!			 END IF 
!
!!   
!!   
!!   dp = DOT_PRODUCT_3(ta1,n1)
!!   
!!     
!!!   if (dp < 1) then
!!   if (dp < 0) then     !fix
!!      ta1(1:3) = -ta1(1:3)
!!      dp = -dp
!!   end if
!!   
!!   do jj = 1, 3
!!    a1(jj) = dp * n1(jj)             !ok
!!!    b1(jj) = a1(jj) - ta1(jj)
!!    b1(jj) = ta1(jj) - a1(jj)  !fix
!!!    b2(jj) = v2*v2/v1/v1 * b1(jj)
!!    b2(jj) = v2/v1 * b1(jj)
!!   end do
!!
!!   b2_mag = (1.-DOT_PRODUCT_3(b2,b2))**0.5
!!   
!!   do jj = 1, 3
!!!    write(6,*) 'DING',b2_mag
!!!    a2(jj) = - b2_mag*n1(jj)*float(itr)
!!    a2(jj) =  b2_mag*n1(jj)   !*float(itr) reflection is not -1*(vector), depends on n1
!!    ta2(jj) = b2(jj) + a2(jj)
!!   end do
!!   
!!   IF (itr == -1) THEN
!!     dp2 = DOT_PRODUCT_3(ta2,n1)
!!     DO jj = 1,3
!!       ta2(jj) = ta2(jj) - 2*dp2*n1(jj)
!!     END DO
!!   END IF  
!!   
!!!   write(6,*) 'A1:',a1(1:3)
!!!   write(6,*) 'B1:',b1(1:3)
!!!   write(6,*) 'A2:',a2(1:3)
!!!   write(6,*) 'B2:',b2(1:3)
!!!   
!!!   write(6,*) '|ta2|:',(dot_product(ta2,ta2))**0.5
!   
!   RETURN
!END SUBROUTINE REF_TRAN_RAY



