/*                               -*- Mode: C -*-
 * postc0.c --- Posterior for continuous node with 0 parents
 * Author          : Claus Dethlefsen
 * Created On      : Tue Mar 12 06:44:35 2002
 * Last Modified By: Claus Dethlefsen
 * Last Modified On: Wed Jun 04 11:57:10 2003
 * Update Count    : 55
 * Status          : Unknown, Use with caution!
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

#include <R.h>
#include <Rmath.h>


void postc0(double *mu, double *tau, double *rho, double *phi, double
	    *loglik, double *y, int *n)
{
	int i;
	double logscale,logk,mscore;
	double oldtau,oldmu;

/*	Rprintf("her er mu=%f\n",*mu);
	Rprintf("her er tau=%f\n",*tau);
	Rprintf("her er rho=%f\n",*rho);
	Rprintf("her er phi=%f\n",*phi);
	Rprintf("her er loglik=%f\n",*loglik);
*/

	for(i = 0; i < *n; i++) {

		logscale = log(*phi)+log1p(1.0/(*tau));
		logk = lgammafn( 0.5*(1.0+*rho) ) - lgammafn(*rho*0.5);
		logk -= 0.5*(logscale + log(M_PI));
		mscore = logk - 0.5*(*rho+1.0)*log1p( (y[i]-*mu)*(y[i]-*mu)/exp(logscale));
		*loglik += mscore;

		oldtau = *tau;
		oldmu  = *mu;

		(*tau)++;
		(*rho)++;
/*	Rprintf("her er oldmu=%f\n",oldmu);
	Rprintf("her er oldtau=%f\n",oldtau);
	Rprintf("her er mu=%f\n",*mu);
	Rprintf("her er tau=%f\n",*tau);
*/
		*mu = (oldtau*(*mu)+y[i])/(*tau);
		*phi+= (y[i]-(*mu))*y[i] + (oldmu-(*mu))*oldtau*oldmu;
/*		Rprintf("logscale=%f\n",logscale);
		Rprintf("logk=%f\n",logk);
		Rprintf("mscore=%f\n",mscore);
		Rprintf("loglik=%f\n",*loglik);

	Rprintf("her er mu=%f\n",*mu);
	Rprintf("her er tau=%f\n",*tau);
	Rprintf("her er rho=%f\n",*rho);
	Rprintf("her er phi=%f\n",*phi);
	Rprintf("her er loglik=%f\n",*loglik);
*/
	}
}

