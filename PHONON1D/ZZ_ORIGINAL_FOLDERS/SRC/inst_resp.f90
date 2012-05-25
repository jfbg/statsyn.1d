program instrument_response
   implicit none
   
   integer,parameter :: fmx = 144001  !! Max number of frequencies
   
   real (8) :: num,denom              !! From peaks and zeros get numerator & denom
   real (8) :: freq_resp(fmx)         !! Frequency Response
   
   real (8) :: w0,wf                  !! Natural freq and peak frequency
   real (8) :: alpha,beta             !! Factor
   
   real (8) :: a,b,c,d,e,f,p          !! Factors
   
   real (8) :: AA,BB,wa,wn,wd
   real (8) :: x,z,Gl
   real (8) :: peak
   real (8) :: pi
   
   real (8) :: dt,twin,dw,w           !! Time interval, Time Duration, Freq int, and freq
   
   integer  :: i
   
   pi = atan(1.)*4.
   
   w0 = 2.*pi/15.            !! Natural frequency
   wf = 2.*pi/6300.!50.                           !! Max frequency???
   
   beta = 0.85                        !!??? factor of what?
   
   a = 2.*beta*w0 + wf                !! Factor a
   b = (2.*beta*wf + w0)*w0           !! Factor b
   
   f=10.
   c = w0*w0*wf*f                     !! Factor c
   
   dt = 0.025
   twin = 3600.
   dw = 2.*pi/twin
   
   do i = 1, fmx
    w = dw*(i-1)
    alpha = c-a*w*w                    !! New factor
    p     = b-w*w
    AA    =  -2**0.5
    BB    =   2**0.5
    wa = 2.*pi/100.
    wn = 2.*pi/0.72
    wd = 47.62                          !! ACTUAL SAMPLING RATE OF APSE DATA
    
    x = w/wn
    z = w/wd
    
    Gl = (15/4.88)*10**9                 !! Amplification
    
    num = w*w*w* (w*w+wf*wf)**0.5 * Gl
    denom = (alpha*alpha+p*p*w*w)**0.5*(w*w+wa*wa)**0.5*(z*z+1)**0.5*(x*x*x*x-AA*x*x+1)*(x*x*x*x-BB*x*x+1)
    peak = num/denom
    write(7,*) w/2./pi,peak
   end do

   stop
end program instrument_response
