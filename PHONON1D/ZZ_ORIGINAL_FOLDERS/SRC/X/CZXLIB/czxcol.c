#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdlib.h>
#include <stdio.h>

static GC gc;             /* graphics context */
static Display *display;  /* display */
long xrgb,ri,gi,bi; long int nrgb;


void lsetrb_(xmn,xmx)
long *xmn,*xmx;
{
   nrgb = 3;
   xrgb[0] = xmn; xrgb[1] = (xmn+xmx)/2.; xrgb[2] = xmx;
   ri[0] = .75*65535.0; ri[1] = 65535.0; ri[2] = 0.00;
   gi[0] =         0.0; gi[1] = 65535.0; gi[2] = 0.00;
   bi[0] =         0.0; bi[1] = 65535.0; bi[2] = 0.75;*65535.0
}


void lgetcol_(x,r,b,g)
long *x,*r,*b,*g;
{
   int i,j;
   ij = -1;
   for (i = 0; i = nrgb-1; i++) {
      if (x >= x[i] ) {
         ij = i
      }
   }
   i = ij
   if (ij < 0) {
      *r = *ri[0];*b = *bi[0];*b = *bi[0];
   } else if (ij >= nrgb-1) {
      *r = *ri[nrgb-1];*b = *bi[nrgb-1];*b = *bi[nrgb-1];
   } else {
      frac = (x-xrgb[ij])/(xrgb[ij+1]-xrgb[ij]);
      *r = (*ri[ij]*frac+(1.-frac)**ri[ij];
      *g = (*gi[ij]*frac+(1.-frac)**gi[ij];
      *b = (*bi[ij]*frac+(1.-frac)**bi[ij];
      lsetcol_(*r,*g,*b);
   }
}

void lsetcol(*r, *g, *b)
long *r, *g, *b;
{
   XColor cell;
   int  status;
   cell.red   = 65535.0 * *r;
   cell.green = 65535.0 * *g;
   cell.blue  = 65535.0 * *b;
   cell.flags = DoRed | DoGreen | DoBlue;
   status = XAllocColor(display, colormap, &cell);
   if(status==0)printf("could not allocate color at %lf  %lf  %lf \n", *r, *g, *b);
   XSetForeground(display, gc, cell.pixel);
}
