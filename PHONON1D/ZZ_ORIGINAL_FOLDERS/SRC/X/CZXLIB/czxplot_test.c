#include <string.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include <math.h>
int main()
{

   long  fwidth  = 1100, fheight = 800;
   long  ibut,xb,yb,mod;
   int   i,j,k;
   long  px1 = 100, px2=900, py1=100, py2 = 700;
   float x1  = 0. ,  x2= 10,  y1=0. , y2  = 12;
   long  icol = 1;
   char  *atemp="hi";
   czxinit_(&fwidth,&fheight);
   czxwind_(&px1,&px2,&py1,&py2,&x1,&x2,&y1,&y2);
   czxfram_(&x1,&x2,&y1,&y2);
   printf("HI:POST-czxfram: \n");
   
   long  nx = 100, ny = 100, XYMX=300;
   float dx = (float)(x2 - x1) / (float) nx;
   float dy = (float)(y2 - y1) / (float) ny;
   float x[XYMX],y[XYMX],z[1000][1000];
   for (i = 0; i <= nx; i++) {
      x[i] = x1 + dx*i;
      for (j = 0; j <= ny; j++) {
         y[j] = y1 + dy*j;
         z[i][j] = x[i]-y[j];
      }
   }
   printf("HI:POST-BUILD: %f \n",x[2]);
   float zmn = -1., zmx = 1.;
   czxcol_cont_(&nx,&ny,&zmn,&zmx,&XYMX,&x,&y,&z);
   lwaitforbuttondown_(&ibut,&xb,&yb,&mod);
   printf("HI:POST-lwaitforbuttondown: %lo %lo %lo %lo \n",ibut,x,y,mod);
   
   czxquit_();
}



