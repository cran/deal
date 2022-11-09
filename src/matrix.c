/*                               -*- Mode: C -*-
 * matrix.c --- Simple matrix functions for use with postc.c
 * Author          : Claus Dethlefsen
 * Created On      : Thu Mar 14 06:48:02 2002
 * Last Modified By: Claus Dethlefsen
 * Last Modified On: Wed Jun 04 11:56:23 2003
 * Update Count    : 36
 * Status          : Ready
 */

/*
  ##
##    Copyright (C) 2002  Susanne Gammelgaard Bottcher, Claus Dethlefsen
##
##    This program is free software; you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation; either version 2 of the License, or
##    (at your option) any later version.
##
##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.
##
##    You should have received a copy of the GNU General Public License
##    along with this program; if not, write to the Free Software
##    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
######################################################################
*/


#include "matrix.h"
#include <R.h>
#include <Rmath.h>


int *ivector(int nl, int nh)
{
   int *v;

   v=(int *) R_alloc((unsigned) (nh-nl+1)*sizeof(int),sizeof(int));
   if ( v == NULL ){
      error("memory allocation failure in ivector()"); return(NULL);
   }
   return v-nl;
}

void free_ivector(int *v, int nl, int nh) { free((char*) (v+nl)); }

double **dmatrix(int nrl, int nrh, int ncl, int nch)
{
   int i;
   double **m;

   m=(double **) R_alloc((unsigned) (nrh-nrl+1)*sizeof(double*),sizeof(double*));
   if ( m == NULL ){
      error("memory allocation failure 1 in dmatrix()"); return(NULL);
   }
   m -= nrl;

   for(i=nrl;i<=nrh;i++) {
	   m[i]=(double *) R_alloc((unsigned) (nch-ncl+1)*sizeof(double),sizeof(double));
      if ( m[i] == NULL ){
         error("memory allocation failure 2 in dmatrix()"); return(NULL);
      }
      m[i] -= ncl;
   }
   return m;
}

void printmat(double **mat, int nr, int nc) {
	int i,j;
	for (i=1; i<=nr; i++) {
		for (j=1; j<=nc; j++)
			Rprintf("%f\t",mat[i][j]);
		Rprintf("\n");
	}
}

void asmatrix(double *vek, double **mat, int nr, int nc) {
	int i,j;
	for (i=1; i<=nr; i++) {
		for (j=1; j<=nc; j++) {
			mat[i][j] = vek[j-1+(i-1)*nc];
		}
	}

}

double** matcopy(double **mat, int nr, int nc) {
	/* copy mat[i][j] into nat[i][j] */
	int i,j;
	double **nat;
	nat = dmatrix(1,nr,1,nc);
/*	Rprintf("(nr=%d,nc=%d)\n",nr,nc);
	Rprintf("(mat=%d)\n",mat);
	Rprintf("(mat[1][1]=%f)\n",mat[1][1]);
*/

	for (i=1; i<=nr; i++) {
		for (j=1; j<=nc; j++) {
			 nat[i][j] = mat[i][j];
		}
	}
	return(nat);
}

double** matmult(double **a, double **b, int nra, int nca, int ncb) {
	double **c;
	int i,j,k;
	c = dmatrix(1,nra,1,ncb);
	for (i=1; i<=nra; i++)
		for (j=1; j<=ncb; j++)
			c[i][j] = 0.0;

	for (i=1; i<=nra; i++)
		for (k=1; k<=ncb; k++)
			for (j=1; j<=nca; j++)
				c[i][k] += a[i][j]*b[j][k];
	return(c);
}

double** matsum(double **a, double **b, int nr, int nc) {
	double **c;
	int i,j;
	c = dmatrix(1,nr,1,nc);

	for (i=1; i<=nr; i++)
		for (j=1; j<=nc; j++)
			c[i][j] = a[i][j] + b[i][j];
	return(c);
}

double** matminus(double **a, double **b, int nr, int nc) {
	double **c;
	int i,j;
	c = dmatrix(1,nr,1,nc);

	for (i=1; i<=nr; i++)
		for (j=1; j<=nc; j++)
			c[i][j] = a[i][j] - b[i][j];
	return(c);
}

double** transp (double **a, int n, int m) {
	double **b;
	int i,j;
	b = dmatrix(1,m,1,n);
	for (i=1; i<=n; i++)
		for (j=1; j<=m; j++)
			b[j][i] = a[i][j];
	return(b);
}

int invers(double **a, int n, double **b, int m)
{
   int *indxc,*indxr,*ipiv;
   int i,icol=1,irow=1,j,k,l,ll;
   double big,dum,pivinv;

   if( (indxc = ivector(1,n)) == NULL){ return(-1); }
   if( (indxr = ivector(1,n)) == NULL){ return(-1); }
   if( (ipiv  = ivector(1,n)) == NULL){ return(-1); }
   for (j=1;j<=n;j++) ipiv[j]=0;
   for (i=1;i<=n;i++) {
      big=0.0;
      for (j=1;j<=n;j++)
         if (ipiv[j] != 1)
            for (k=1;k<=n;k++) {
               if (ipiv[k] == 0) {
                  if (fabs(a[j][k]) >= big) {
                     big=fabs(a[j][k]);
                     irow=j;
                     icol=k;
                  }
               } else if (ipiv[k] > 1){
                  error("Invers: Singular Matrix-1");
                  return(-1);
               }
            }
      ++(ipiv[icol]);
      if (irow != icol) {
         for (l=1;l<=n;l++){
            double temp=a[irow][l]; a[irow][l]=a[icol][l]; a[icol][l]=temp;
         }
         for (l=1;l<=m;l++){
            double temp=b[irow][l]; b[irow][l]=b[icol][l]; b[icol][l]=temp;
         }
      }
      indxr[i]=irow;
      indxc[i]=icol;
      if (a[icol][icol] == 0.0){
         error("Invers: Singular Matrix-2");
         return(-1);
      }
      pivinv=1.0/a[icol][icol];
      a[icol][icol]=1.0;
      for (l=1;l<=n;l++) a[icol][l] *= pivinv;
      for (l=1;l<=m;l++) b[icol][l] *= pivinv;
      for (ll=1;ll<=n;ll++)
         if (ll != icol) {
            dum=a[ll][icol];
            a[ll][icol]=0.0;
            for (l=1;l<=n;l++) a[ll][l] -= a[icol][l]*dum;
            for (l=1;l<=m;l++) b[ll][l] -= b[icol][l]*dum;
         }
   }
   for (l=n;l>=1;l--) {
      if (indxr[l] != indxc[l]){
         for (k=1;k<=n;k++){
            double temp     = a[k][indxr[l]];
            a[k][indxr[l]] = a[k][indxc[l]];
            a[k][indxc[l]] = temp;
         }
      }
   }
   return(0);
}


