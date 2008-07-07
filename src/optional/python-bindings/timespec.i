/*
 * timespec.i -- SWIG interface file for type translation of Timespec types
 *
 * Copyright (C) 2008 ParIT Worker Co-operative <paritinfo@parit.ca>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 * @author Mark Jenkins, ParIT Worker Co-operative <mark@parit.ca>
 */

// A typemap for converting python dates to Timespec in functions that
// require Timespec as an argument
%typemap(in) Timespec {
    PyDateTime_IMPORT;
    $1 = gnc_dmy2timespec(PyDateTime_GET_DAY($input),
                          PyDateTime_GET_MONTH($input),
                          PyDateTime_GET_YEAR($input) );
}

// A typemap for converting python dates to Timespec *, for fuctions that
// requires a Timespec * as an argument. BIG ASSUMPTION, the function
// recieving this pointer is going to make a copy of the data. After the
// function call, the memory for the Timespec used to perform this conversion
// is going to be lost, so make damn sure that the recipiant of this pointer
// is NOT going dereference it sometime after this function call takes place.
//
// As far as I know, the xaccTransSetDate[Posted|Entered|Due]TS functions
// from Transaction.h are the only functions with Timespec * that we re
// actually using. I have personaly verifyed in the source that the pointer
// being produced by this typemap is being deferenced, and the data copied
// in all three functions.
// 
// The memory for the Timespec used for this conversion is allocated on the
// stack. (SWIG will name the variables ts1, ts2, ts3...)
//
// Mark Jenkins <mark@parit.ca>
%typemap(in) Timespec * (Timespec ts) {
    PyDateTime_IMPORT;
    ts = gnc_dmy2timespec(PyDateTime_GET_DAY($input),
                          PyDateTime_GET_MONTH($input),
                          PyDateTime_GET_YEAR($input) );
    $1 = &ts;
}

// A typemap for converting Timespec values returned from functions to
// python dates.
%typemap(out) Timespec {
    int year, month, day;
    gnc_timespec2dmy($1, &day, &month, &year);
    PyDateTime_IMPORT;
    $result = PyDate_FromDate(year, month, day);
}
