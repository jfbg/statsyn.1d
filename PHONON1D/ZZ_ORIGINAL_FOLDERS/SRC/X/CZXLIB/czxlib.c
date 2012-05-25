#define maximum_number_fonts 30
#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/IOKitLib.h>


#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#include <string.h>
#include <stdio.h>

#include "rotated.h"  /* Alan Richardson's text rotation
		 	 library. */

unsigned int  window_width, window_height;    /* WINDOW DIMENSIONS (PIXELS)    */
unsigned long xpen,ypen;
float         axp,ayp,bxp,byp;                /* FRAME DIMENSIONS IN PIXELS    */
float         fr_x1,fr_x2,fr_y1,fr_y2;        /* FRAME SIZE IN PLOTTING UNITS  */
//long CPx=0, CPy=0;  /* The current position of the pointer. */

void czxcol_(icol)
long *icol;
{
   lsetcolor_(icol);
}

void czxinit_(fwidth, fheight)
/* Calls linitfancy window, with some default settings. */
long *fwidth, *fheight;
{
   long origx, origy;
   char *w = "QCN",
        *i = "LeoLib";
   int  ii = strlen(i), 
        wi = strlen(w);
   int  icol =0, l1=1, l2=0, l3=0, l4=0;
   char *cname = "9X15";
   long wfont = strlen(cname); long nfont = 1;
   origx=0;
   origy=0;
   linitfancywindow_(fwidth,fheight,&origx,&origy,w,i,wi,ii);
   lsetcolor_(&icol);                        /*  set to black              */
   llinestyle_(&l1,&l2,&l3,&l4);             /*  set to fastest line width  */
   lloadfont_(&nfont,cname,wfont);

}                                            /* linitializewindow_ */


void czxmove_(x1,y1)
float *x1,*y1;
{
   long  xpen = axp**x1+bxp;
   long  ypen = ayp**y1+byp;
   lmoveto_(&xpen,&ypen);
}

void czxdraw_(x1,y1)
/* czxdraw_ draws a line from the previous pen position to the new pen position */
float *x1,*y1;
{
   xpen = axp**x1+bxp;
   ypen = ayp**y1+byp;
   llineto_(&xpen,&ypen);
}

void czxdrawf_(x1,y1)
/*  czxdrawf is faster than czxdraw because it doesn't update the window afterwards */
float *x1,*y1;
{
   xpen = (long) (axp**x1+bxp);
   ypen = (long) (ayp**y1+byp);
   llineto_(&xpen,&ypen);
}

void czxlines_(x,y,npts)
/* czxlines plots a whole series of lines faster than doing czxdraw over and over */
float *x, *y, npts;
{
   int i;
   czxmove_(x[0],y[0]);       /* moves current pen position to first x, y */
   for (i = 1; i<npts; i++) {
      czxdrawf_(x[i],y[i]);   /* czxdraw_ draws a line from previous pen position to new */
   };
     update_window();           /* plots updates to window */
}  /* End czxlines_     */

void czxfram_(x1,x2,y1,y2)
/* Creates a line frame from corner (x1,y1) to corner (x2,y2):      */
float *x1,*x2,*y1,*y2;
{
   czxmove_(x1,y1);
   czxdrawf_(x2,y1);
   czxdrawf_(x2,y2);
   czxdrawf_(x1,y2);
   czxdrawf_(x1,y1);
}

void czxlab_(alab)
/* THIS SUBROUTINE PRINTS A LABEL, alab, TO THE SCREEN AT THE CURRENT PEN POSITION */
char *alab;
{
   lprint_(alab,strlen(alab));      
   update_window();
}

void czxlabxy_(x1,y1,alab)
float *x1,*y1;
char  *alab;
{
   czxmove_(x1,y1);
   lprint_(alab,strlen(alab));      
   update_window();
}


void czxmess_(amess)
/* THIS SUBROUTINE PRINTS A amess1 ON THE SCREEN OF THE WINDOW.      */
char *amess;
{
   long  z0=0,z1=window_height,nlorg=1;
   long  x1=1,y1=window_height-20;
   float nang=0.;
   long  cindx=0;
   
   lclearpartwindow_(&z0,&z1,&window_width,&window_height);
   lmoveto_(&x1,&z1);
   laligntext_(&nlorg);
   ltextangle_(&nang);
   lsetcolor_(&cindx);
   lprint_(amess,strlen(amess));      
     update_window();
}  /* End czxmess */

void czxmessc_clr_()
/* THIS SUBROUTINE PRINTS A amess1 ON THE SCREEN OF THE WINDOW.      */
{
   long  z0=0.,z1=window_height-20,z2=window_width,nlorg=2;
   long  x1=0,z3=20;
   float nang=0.;
   long  cindx=0,cindx_old=1;
   czxcol_(&cindx_old);
   ldrawfilledbox_(&z0,&z1,&z2,&z3);
}  /* End czxmess */


void czxmessc_(amess)
/* THIS SUBROUTINE PRINTS A amess1 ON THE SCREEN OF THE WINDOW.      */
char *amess;
{
   long  z0=0.,z1=window_height-20,z2=window_width,nlorg=2;
   long  x1=0,z3=20;
   float nang=0.;
   long  cindx=0,cindx_old=1;
   czxcol_(&cindx_old);
   ldrawfilledbox_(&z0,&z1,&z2,&z3);
   x1=window_width/2.;
   z1 = window_height;
   lmoveto_(&x1,&z1);
   laligntext_(&nlorg);
   ltextangle_(&nang);
   cindx=0;
   lsetcolor_(&cindx);
   lprint_(amess,strlen(amess));      
   update_window();
}  /* End czxmess */


void czxbox_(x1,y1,x2,y2)
float *x1,*y1,*x2,*y2;
{
   long xpen = axp* *x1+bxp ;
   long ypen = ayp* *y1+byp ;
   long dx   = axp*(*x2-*x1);
   long dy   = ayp*(*y1-*y2);
   ldrawfilledbox_(&xpen,&ypen,&dx,&dy);
}



void czxclr_()
{
   lclearwindow_();
}


void czxclrb_(fx1, fy1, fw, fh)
float  *fx1,*fy1,*fw,*fh;
{
   int ix1  = (axp* *fx1 + bxp );
   int iy1  = (ayp* *fy1 + byp );
   int iw   = (axp* *fw )+1;
   int ih   = (ayp* *fh );
   lclearpartwindow_(&ix1, &iy1, &iw, &ih);
}





void czxcol_line_(x,y,z,dx1,dy1,zmn,zmx)
float *zmn,*zmx,*x,*y,*z,*dx1,*dy1;
{
   double r,g,b;
   int  ix,iy;
   long x2,y2;
   float z2=*z;
   long dx = axp*(*dx1)+1, dy = ayp*(*dy1)+1;
   if (dy<0) {dy = -dy;};
   x2 = axp* *x+bxp;
   y2 = ayp* *y+byp-dy;
   lgetcol_rb_(&z2,&r,&b,&g,zmn,zmx);
   printf("HI: %f %f %f %f %f %f \n", z2, r, b, g, *zmn, *zmx);
   ldrawfilledbox_(&x2,&y2,&dx,&dy);
}




void czxcol_cont_(nx,ny,zmn,zmx,XYMX,x,y,z)
long *nx,*ny,*XYMX;
float *zmn,*zmx,*x,*y,z[1000][1000];
{
   double r,g,b;
   int  ix,iy;
   long x2,y2;
   long dx = axp*(x[1]-x[0])+1, dy = ayp*(y[0]-y[1])+1;
   if (dy<0) {dy = -dy;};
   for (ix = 0; ix < *nx; ix++) {
      x2 = axp* x[ix]+bxp;
      for (iy = 0; iy < *ny; iy++) {
         y2 = ayp* y[iy]+byp-dy;
         lgetcol_rb_(&z[ix][iy],&r,&b,&g,zmn,zmx);
	 ldrawfilledbox_(&x2,&y2,&dx,&dy);
      }
   }
     update_window();
}

void czxwind_(px1,px2,py1,py2,x1,x2,y1,y2)
long *px1,*px2,*py1,*py2;
float *x1,*x2,*y1,*y2;
{
   long icol = 1;
   printf("czxwind:1: %f %f %f %f \n",*x1,*y1,*x2,*y2);
   axp = (float) (*px2-*px1)/(*x2-*x1);
   ayp = (float) (*py1-*py2)/(*y2-*y1);
   bxp = (float) *px1 - *x1 * axp;
   byp = (float) (window_height-*py1) - *y1 * ayp;
   fr_x1 = *x1;
   fr_x2 = *x2;
   fr_y1 = *y1;
   fr_y2 = *y2;
}

void czxquit_()
/* Calls linitfancy window, with some default settings. */
{
   long ibut,ix,iy,mod;
   char *amess1="PRESS ANY BUTTON TO CLOSE WINDOW: ";
   czxmess_(amess1);            /*  clear part of the window  */
     update_window();
   lwaitforbuttondown_(&ibut,&ix,&iy,&mod);
   ldie_();

}  /* End czxquit_ */


void czxshft_(fx1,fy1,fw,fh,dx2)
float   *fx1,*fy1,*fw,*fh,*dx2;
{
   long ix1  = (int) (axp* *fx1+bxp );
   long iy1  = (int) (ayp* *fy1+byp );
   long ix2  = (int) (axp* (*fx1+*dx2) + bxp );
   long iy2  = (int) (ayp* *fy1+byp );
   long iw   = (int) (axp* *fw );
   long ih   = (int) (ayp* *fh );
   lbitblt_(&ix1,&iy1,&iw,&ih,&ix2,&iy2);   
}

void OnePointLogo_()
{
    char alab[20];
    int ix1=.4*window_width;int ix2=.5*window_width;
    int iy1=.03*window_height;int iy2=0.1*window_height;
    int idx=.1*window_width; int idy=0.04*window_height;
    int icol = 0;   czxcol_(&icol);
    
    ldrawfilledbox_(&ix1,&iy1,&idx,&idy);
    icol = 2;       czxcol_(&icol);
    long nlorg = 6; laligntext_(&nlorg); sprintf(alab,"One"); 
    ix1 = 0.5*window_width-4; iy1 = 0.05*window_height; lmoveto_(&ix1,&iy1);
    czxlab_(alab);
    ix1 = ix1 + 1; iy1 = iy1 + 1; lmoveto_(&ix1,&iy1);  czxlab_(alab);
    icol = 2; czxcol_(&icol); ix1 = 0.5*window_width; iy1 = 0.03*window_height;
    ldrawfilledbox_(&ix1,&iy1,&idx,&idy);
    icol = 0;       czxcol_(&icol);
    nlorg = 4; laligntext_(&nlorg); sprintf(alab,"Point"); 
    ix1 = 0.5*window_width+1; iy1 = 0.05*window_height; lmoveto_(&ix1,&iy1);
    czxlab_(alab);
    ix1 = ix1 + 1; iy1 = iy1 + 1; lmoveto_(&ix1,&iy1);  czxlab_(alab);
}
