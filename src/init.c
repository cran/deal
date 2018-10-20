/*                               -*- Mode: C -*- 
 * init.c --- Registration of C functions
 * Author          : Claus Dethlefsen
 * Created On      : Fri Oct 16 16:44:35 2018
 * Last Modified By: Claus Dethlefsen
 * Last Modified On: Fri Oct 16 16:44:35 2018
 * Update Count    : 1
 * Status          : Final
 */

/*
  ##
##    Copyright (C) 2018  Susanne Gammelgaard B?ttcher, Claus Dethlefsen
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

#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .C calls */
extern void postc(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void postc0(void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
  {"postc",  (DL_FUNC) &postc,  9},
  {"postc0", (DL_FUNC) &postc0, 7},
  {NULL, NULL, 0}
};

void R_init_deal(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
