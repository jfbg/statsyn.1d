PROGRAM testRT

! Test 1  P upward incident

      REAL     b,a,c,rhos,rhof,pi,inc,p
      REAL     TdPP, TdSP, RdPP, TuPP, TuPS, RuPP, RuSP, RuPS, RuSS
      
      b = 7.26
			a = 13.71
			c = 8.06
			rhos = 5.56
			rhof = 9.9

			pi = atan(1.)*4.
			inc = 21
			p = sin(inc/180*pi)/a
			
			CALL RTFLUID(p,c,rhof,a,b,rhos,TdPP, TdSP, RdPP, TuPP, TuPS, RuPP, RuSP, RuPS, RuSS)
			WRITE(6,*) 'p = ',p 
			WRITE(6,*) 'TdPP, TdSP, RdPP, TuPP, TuPS, RuPP, RuSP, RuPS, RuSS'
			WRITE(6,FMT = 404) TdPP, TdSP, RdPP, TuPP, TuPS, RuPP, RuSP, RuPS, RuSS

404   FORMAT (9(f6.4))  
			
END PROGRAM
			
			


SUBROUTINE RTFLUID(p,c,rhof,a,b,rhos,TdPP, TdSP, RdPP, TuPP, TuPS, RuPP, RuSP, RuPS, RuSS)

      IMPLICIT NONE
  
      REAL(4)     p,c,rhof,a,b,rhos
      REAL(4)     TdPP, TdSP, RdPP, TuPP, TuPS, RuPP
      REAL(4)     RuSP, RuPS, RuSS
      
      REAL(4)     p2,mu,lm,qf,Ff(2,2),Fs(4,4),invFf(2,2),L(3,4),Ltemp(2,4)
      REAL(4)     dum(3,3),tdum(3,3)
      REAL(4)     q(6),N(6,6)

! FUNCTION [TdPP, TdSP, RdPP, TuPP, TuPS, RuPP, RuSP, RuPS, RuSS] = RTFLUID(p,c,rhof,a,b,rhos)
! FUNCTION RTFLUID computes reflection and transmission coefficients for a fluid-solid boundary
! (e.g. ocean bottom) where P is slowness, C is fluid velocity, RHOF is fluid density, A,B 
! are P- and S-velocity of solid and RHOS is solid density.
!
! CONVERTED FROM MICHAEL BOSTOCK MATLAB VERSION by JF BLANCHETTE-GUERTIN (2012/11/09)
		 
		 tdum(:,:) = 0
		 
		 p2=p*p
		 mu=rhos*b*b
		 lm=rhos*a*a-2*mu
		 
		 !Fluid properties.
		 qf=sqrt(1/c**2-p2)
		 Ff(1,1) = qf*c 
		 Ff(1,2) = -qf*c
		 Ff(2,1) = rhof*c
		 Ff(2,2) = rhof*c
		 
		 ! Find inverse of Ff
!		 invFf(1,:) = (1/(Ff(1,1)*Ff(2,2) - Ff(1,2)*Ff(2,1)))* [Ff(2,2),-Ff(1,2)]
!		 invFf(2,:) = (1/(Ff(1,1)*Ff(2,2) - Ff(1,2)*Ff(2,1)))* [F-f(2,1),Ff(1,1)]		 

     invFf = Ff
     CALL GAUSS(invFf,2)
		 
!!!!!!!
		 ! Solid properties (call isotroc for consistency with 
		 ! Rmatrix on conventions of signs etc.)
		 
		 CALL ISOTROC(q,N,a,b,rhos,p,0.);
		 
!		 WRITE(6,*) N(1,:)
!		 WRITE(6,*) N(2,:)
!		 WRITE(6,*) N(3,:)
!		 WRITE(6,*) N(4,:)
!		 WRITE(6,*) N(5,:)
!		 WRITE(6,*) N(6,:)
		 
		 
		 Fs(1,1:4) = [N(1,1),N(1,2),N(1,4),N(1,5)]
		 Fs(2,1:4) = [N(3,1),N(3,2),N(3,4),N(3,5)]
		 Fs(3,1:4) = [N(4,1),N(4,2),N(4,4),N(4,5)]
     Fs(4,1:4) = [N(6,1),N(6,2),N(6,4),N(6,5)]
		 
		 ! Define matrix L that encapsulates continuity conditions (continuity of
		 ! vertical displacement and vertical traction) as well as vanishing horizontal
		 ! traction at fluid solid interface.
		
		 Ltemp(1,:) = Fs(2,:)
		 Ltemp(2,:) = Fs(4,:)
		 
		 L(1:2,:) = matmul(invFf,Ltemp)
		 L(3,:) = Fs(3,:)
		 
		 ! P-incidence from above (from liquid)
		 dum(1,:) = [L(1,1),L(1,2),0.]
		 dum(2,:) = [L(2,1),L(2,2),-1.]
		 dum(3,:) = [L(3,1),L(3,2),0.]
		 
		 CALL GAUSS(dum,3)
		 tdum(:,1) = [1,0,0]
		 		 
		 dum = matmul(dum,tdum)

		 TdPP=dum(1,1);
		 TdSP=dum(2,1);
		 RdPP=dum(3,1);
		 
		 ! P-incidence from below.
		 dum(1,:) = [0.,L(1,1),L(1,2)]
		 dum(2,:) = [-1.,L(2,1),L(2,2)]
		 dum(3,:) = [0.,L(3,1),L(3,2)]
 		 CALL GAUSS(dum,3)			
     tdum(:,1) =  [-L(1,3),-L(2,3),-L(3,3)]
		 dum = matmul(dum,tdum)
		      
		 TuPP=dum(1,1);
		 RuPP=dum(2,1);
		 RuSP=dum(3,1);
		 
		 ! S-incidence from below.
		 dum(1,:) = [0.,L(1,1),L(1,2)]
		 dum(2,:) = [-1.,L(2,1),L(2,2)]
		 dum(3,:) = [0.,L(3,1),L(3,2)]
		 		 
 		 CALL GAUSS(dum,3)			
     tdum(:,1) =  [-L(1,4),-L(2,4),-L(3,4)]
		 dum = matmul(dum,tdum)

		 TuPS=dum(1,1);
		 RuPS=dum(2,1);
		 RuSS=dum(3,1);
		 
		 RETURN
END SUBROUTINE RTFLUID

SUBROUTINE ISOTROC(q,rN,rvp,rvs,rrho,rp1,rp2)

      IMPLICIT NONE
      
      REAL(4)    q(6),rN(6,6),rvp,rvs,rrho,rp1,rp2
      COMPLEX(4) vp,vs,rho,p1,p2
      COMPLEX(4) mu,pp,qdp,qds,qup,qus,norm,tnorm1(3),tnorm2
      COMPLEX(4) N(6,6)
      COMPLEX(4) qN(6)
      INTEGER    II
            
      qN(:) = 0.
      N(:,:) = 0.
      
      vp = CMPLX(rvp,0)
      vs = CMPLX(rvs,0)
      rho = CMPLX(rrho,0)
      p1 = CMPLX(rp1,0)
      p2 = CMPLX(rp2,0)

! FUNCTION [q,N] = ISOTROC(vp,vs,rho,p1,p2);
! Analytic construction of the fundamental matrix for isotropic
! media from Fryer and Frazer (1987, eq (4.16)) but modified for
! a different Fourier transform sign convention.
!
! CONVERTED FROM MICHAEL BOSTOCK MATLAB VERSION by JF BLANCHETTE-GUERTIN (2012/11/09)

			mu=  rho*vs*vs;
			WRITE(6,*) 'mu:',mu
			pp=  (p1*p1+p2*p2);
			WRITE(6,*) 'pp:',pp
			qdp= csqrt(1/(vp*vp)-pp);
			WRITE(6,*) 'qdp:',qdp
			qds= csqrt(1/(vs*vs)-pp);
			WRITE(6,*) 'qds',qds
			qup= -qdp;
			qus= -qds;
		
			WRITE(6,*) 'qup:',qup
  		WRITE(6,*) 'qus:',qus
		
		! Sort eigenvalues and eigenvectors (don't forget normal vs
		! conjugate transpose).
			qN(1:6) = [qdp,qds,qds,qup,qus,qus]  !.';
			N(1:6,1)= [p1,p2,qdp,2*mu*p1*qdp,2*mu*p2*qdp,(rho-2*mu*pp)] !';
			N(1:6,2)= [p1,p2,-pp/qds,p1*N(6,1)/qds,p2*N(6,1)/qds,-2*mu*pp] !';
			N(1:6,3)= [-p2,p1,CMPLX(0,0),-p2*qds*mu,p1*qds*mu,CMPLX(0,0)] !';
			N(1:6,4)= [p1,p2,qup,2*mu*p1*qup,2*mu*p2*qup,N(6,1)] !';
			N(1:6,5)= [p1,p2,-pp/qus,p1*N(6,1)/qus,p2*N(6,1)/qus,-2*mu*pp] !';
			N(1:6,6)= [-p2,p1,CMPLX(0,0),-p2*qus*mu,p1*qus*mu,CMPLX(0,0)] !';
		
		 WRITE(6,*) qN	
		 WRITE(6,*) N(1,:)
		 WRITE(6,*) N(2,:)
		 WRITE(6,*) N(3,:)
		 WRITE(6,*) N(4,:)
		 WRITE(6,*) N(5,:)
		 WRITE(6,*) N(6,:)
		
		! Normalize vectors wrt displacement magnitude.
			DO II = 1,6
			  tnorm1 = N(1:3,II)*CONJG(N(1:3,II))
			  norm = csqrt(tnorm1(1) + tnorm1(2) + tnorm1(3))
!				norm= csqrt(N(1:3,II)*conj(N(1:3,j)));
				N(:,II)=N(:,II)/norm;
      END DO
      
      rN = REAL(N,8)

      RETURN
END SUBROUTINE ISOTROC



! --------------------------------------------------------------------
SUBROUTINE GAUSS (a,n)       ! Invert matrix by Gauss method
! --------------------------------------------------------------------
			IMPLICIT NONE
			
			INTEGER :: n
			REAL(4) :: a(n,n)
			
			! - - - Local Variables - - -
			REAL(4) :: b(n,n), c, d, temp(n)
			INTEGER :: i, j, k, m, imax(1), ipvt(n)
			! - - - - - - - - - - - - - -
			
			b = a
			ipvt = (/ (i, i = 1, n) /)
			
			DO k = 1,n
				 imax = MAXLOC(ABS(b(k:n,k)))
				 m = k-1+imax(1)
			
				 IF (m /= k) THEN
						ipvt( (/m,k/) ) = ipvt( (/k,m/) )
						b((/m,k/),:) = b((/k,m/),:)
				 END IF
				 d = 1/b(k,k)
			
				 temp = b(:,k)
				 DO j = 1, n
						c = b(k,j)*d
						b(:,j) = b(:,j)-temp*c
						b(k,j) = c
				 END DO
				 b(:,k) = temp*(-d)
				 b(k,k) = d
			END DO
			
			a(:,ipvt) = b

      RETURN
END SUBROUTINE GAUSS