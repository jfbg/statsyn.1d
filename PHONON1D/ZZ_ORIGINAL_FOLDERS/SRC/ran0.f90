real function ran0(idum)
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
!   | --- --------- --------- --------- --------- --------- --------- -- !   !
      integer idum,IA,IM,IQ,IR,MASK
      real ran0,AM
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836, &
      MASK=123459876)
      integer k
      idum=ieor(idum,MASK)
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum < 0) idum=idum+IM
      ran0=AM*idum
      idum=ieor(idum,MASK)
      return
end function ran0
