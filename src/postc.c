/*                               -*- Mode: C -*- 
 * postc.c --- Posterior for continuous node with continuous parents
 * Author          : Claus Dethlefsen
 * Created On      : Tue Mar 12 06:44:35 2002
 * Last Modified By: Claus Dethlefsen
 * Last Modified On: Wed Jun 04 11:56:51 2003
 * Update Count    : 227
 * Status          : Unknown, Use with caution!
 */

/*
  ##
##    Copyright (C) 2002  Susanne Gammelgaard B?ttcher, Claus Dethlefsen
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

#include <R.h>
#include <Rmath.h>
#include "matrix.h"




void postc(double *mu, double *tau, double *rho, double *phi, double
	    *loglik, double *y, double *z, int *n, int *d)
{
	/* 
	   mu:  dx1 matrix
	   tau: dxd matrix
	   rho: real
	   phi: real
	   loglik: real
	   y: nx1 matrix
	   z: nxd matrix
	   n: int
	   d: int
	*/

	int i,j;
	double logscale,logk,mscore;
	double **oldtau=0, **oldmu=0, **mtau, **mmu, **tauinv=0;
	double **zero, **zi, **ziy;

	/* allocate space for matrices */
	mtau   = dmatrix(1,*d,1,*d);
	zi     = dmatrix(1,*d,1,1);
	ziy    = dmatrix(1,*d,1,1);
	mmu    = dmatrix(1,*d,1,1);
	zero   = dmatrix(1,*d,1,1);

	/* copy arguments into the matrices */
	asmatrix(mu,mmu,*d,1);
	asmatrix(tau,mtau,*d,*d);
	
	/* show input */
/*
	Rprintf("Mu=(%d x 1)\n",*d);
	printmat(mmu,*d,1);

	Rprintf("Tau=\n");
	printmat(mtau,*d,*d);
	
	Rprintf("Rho=%f\n",*rho);
	Rprintf("Phi=%f\n",*phi);
	Rprintf("loglik=%f\n",*loglik);
	


	Rprintf("Entering loop\n");
	Rprintf("z=(%d x %d)\n",*n,*d);
	printmat(mz,*n,*d);
*/
	for(i = 1; i <= *n; i++) {

		/*
			Rprintf("y[i]=%f\n",y[i]);
			Rprintf("\n");
			printmat(mtau,*d,*d);
			Rprintf("\n");
		*/

		tauinv = matcopy(mtau,*d,*d);
		invers(tauinv, *d, zero, 1);

/*
  printmat(tauinv,*d,*d);
			Rprintf("\n");
*/
		for (j=1; j<=*d; j++) {
			zi[j][1] = z[j-1+(i-1)*(*d)];
		}
		
		logscale = log(*phi) +
			log1p(
				matmult(
					transp(zi,*d,1),
					matmult(tauinv,zi,*d,*d,1),
					1,*d,1
					)[1][1]
				);
		
		logk = lgammafn( 0.5*(1.0+*rho) ) - lgammafn(*rho*0.5);
		logk -= 0.5*(logscale + log(M_PI));
		
		mscore =  logk - 0.5*(*rho+1)*
			log1p(
				(y[i-1] - matmult(
					transp(zi,*d,1),
					mmu,1,*d,1
					)[1][1]
					)
				*
				(y[i-1] - matmult(
					transp(zi,*d,1),
					mmu,1,*d,1
					)[1][1])
				/exp(logscale)
				);
		
		*loglik += mscore;
		/*
			Rprintf("logscale=%f\n",logscale);
			Rprintf("logk=%f\n",logk);
			Rprintf("mscore=%f\n",mscore);
			Rprintf("her er loglik=%f\n",*loglik);
		*/
		oldtau = matcopy(mtau,*d,*d);
		oldmu  = matcopy(mmu,*d,1);
		
		/*
			Rprintf("mtau=\n");
			printmat(mtau,*d,*d);
			Rprintf("zi=\n");
			printmat(zi,*d,1);
			Rprintf("transp(zi,*d,1)=\n");
			printmat(transp(zi,*d,1),1,*d);
			Rprintf("matmult(zi,transp(zi,*d,1),*d,1,*d)\n");
			printmat(matmult(zi,transp(zi,*d,1),*d,1,*d),*d,*d);
			Rprintf("matsum(mtau, 
			      matmult(zi,transp(zi,*d,1),*d,1,*d)
			      , *d, *d
			)\n");
			printmat(matsum(mtau, 
			      matmult(zi,transp(zi,*d,1),*d,1,*d)
			      , *d, *d
					 ),*d,*d);
		*/

		mtau = matsum(mtau, 
			      matmult(zi,transp(zi,*d,1),*d,1,*d)
			      , *d, *d
			);
		/*
			Rprintf("Tau=\n");
			printmat(mtau,*d,*d);
		*/
		tauinv = matcopy(mtau,*d,*d);
		invers(tauinv, *d, zero, 1);

		for (j=1;j<=*d;j++)
			ziy[j][1] = zi[j][1]*y[i-1];

		mmu = matmult(tauinv,
			      matsum(
				      matmult(oldtau,mmu,*d,*d,1),
				      ziy,
				      *d,1)
			      ,*d,*d,1);

		/*
			Rprintf("Mu=\n");
			printmat(mmu,*d,1);
		*/

		(*rho)++;
		/*
			Rprintf("t(zi)=\n");
			printmat(transp(zi,*d,1),1,*d);
			Rprintf("mmu=\n");
			printmat(mmu,*d,1);
			Rprintf("t(zi)*mmu=\n");
			printmat(matmult(transp(zi,*d,1),mmu,1,*d,1),1,1);
		*/
		(*phi) += (y[i-1]-
			 matmult(
				 transp(zi,*d,1),
				 mmu,1,*d,1)[1][1])*y[i-1]
			+
			matmult(
				transp(
					matminus(oldmu,mmu,*d,1),
					*d,1
					),
				matmult(
					oldtau,
					oldmu,
					*d,*d,1
					),
				1,*d,1
				)[1][1];
		/*
			Rprintf("Phi=%f\n",*phi);
		*/
	} 
	
/* RESULTS */
	/*
		Rprintf("Mu=\n");
		printmat(mmu,*d,1);
		Rprintf("Tau=\n");
		printmat(mtau,*d,*d);
		Rprintf("Rho=%f\n",*rho);
		Rprintf("Phi=%f\n",*phi);
		Rprintf("loglik=%f\n",*loglik);
	*/
	
	
	for (i=1; i<=*d;i++)
		mu[i-1] = mmu[i][1];
	for (i=1; i<=*d; i++)
		for (j=1; j<=*d; j++)
			tau[(*d)*(j-1)+i-1] = mtau[i][j];
	
	/*
		Rprintf("Returned mu=\n");
		for (i=0; i<*d; i++)
			Rprintf("%f\n",mu[i]);
		Rprintf("Returned tau=\n");
		for (i=0; i<(*d)*(*d); i++)
			Rprintf("%f\n",tau[i]);
	*/

	/* destruct the matrices
	double **oldtau, **oldmu, **mtau, **mz, **mmu, **tauinv;
	double **zero, **zi, **ziy;
	*/
/*	free_dmatrix(mtau,1,*d,1,*d);
	free_dmatrix(mz,1,*n,1,*d);
	free_dmatrix(zi,1,*d,1,1);
	free_dmatrix(ziy,1,*d,1,1);
	free_dmatrix(mmu,1,*d,1,1);
	free_dmatrix(zero,1,*d,1,1);

	free_dmatrix(oldtau,1,*d,1,*d);
	free_dmatrix(tauinv,1,*d,1,*d);
	free_dmatrix(oldmu,1,*d,1,1);
*/

} 
