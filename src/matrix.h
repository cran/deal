/*                               -*- Mode: C -*- 
 * matrix.h --- 
 * Author          : Claus Dethlefsen
 * Created On      : Thu Mar 14 06:47:52 2002
 * Last Modified By: Claus Dethlefsen
 * Last Modified On: Tue May 07 09:39:46 2002
 * Update Count    : 22
 * Status          : Unknown, Use with caution!
 */

/*
  ##
##    Copyright (C) 2002  Susanne Gammelgaard Bøttcher, Claus Dethlefsen
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

extern double **   dmatrix( int, int, int, int );
extern int *       ivector( int, int );
extern void        free_ivector( int    *, int, int );

extern int         invers(double **a, int n, double **b, int m);
extern void        printmat( double **, int, int);
extern void        asmatrix( double *, double **, int, int);
extern double**    matcopy(double **, int, int);
extern double**    matmult(double **,double **, int, int, int);
extern double**    matsum(double **a, double **b, int nr, int nc);
extern double**    matminus(double **a, double **b, int nr, int nc);
extern double**    transp (double **a, int n, int m); 
