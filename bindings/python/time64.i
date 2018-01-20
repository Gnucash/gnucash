/*
 * time64.i -- SWIG interface file for type translation of time64 types
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

/** @file
    @brief SWIG interface file for type translation of time64 types
    @author Mark Jenkins, ParIT Worker Co-operative <mark@parit.ca>
    @author Jeff Green,   ParIT Worker Co-operative <jeff@parit.ca>
    @ingroup python_bindings */

// A typemap for converting python dates to time64 in functions that
// require time64 as an argument
%typemap(in) time64 {
    PyDateTime_IMPORT;
    struct tm time = {PyDateTime_DATE_GET_SECOND($input),
                      PyDateTime_DATE_GET_MINUTE($input),
                      PyDateTime_DATE_GET_HOUR($input),
                      PyDateTime_GET_DAY($input),
                      PyDateTime_GET_MONTH($input) - 1,
                      PyDateTime_GET_YEAR($input) - 1900};
    $1 = gnc_mktime(&time);
}

// A typemap for converting python dates to time64 *, for functions that
// requires a time64 * as an argument. BIG ASSUMPTION, the function
// receiving this pointer is going to make a copy of the data. After the
// function call, the memory for the time64 used to perform this conversion
// is going to be lost, so make damn sure that the recipient of this pointer
// is NOT going dereference it sometime after this function call takes place.
//
// As far as I know, the xaccTransSetDate[Posted|Entered|Due]TS functions
// from Transaction.h are the only functions with time64 * that we re
// actually using. I have personally verified in the source that the pointer
// being produced by this typemap is being dereferenced, and the data copied
// in all three functions.
// 
// The memory for the time64 used for this conversion is allocated on the
// stack. (SWIG will name the variables ts1, ts2, ts3...)
//
// Mark Jenkins <mark@parit.ca>
%typemap(in) time64 * (time64 secs) {
    PyDateTime_IMPORT;
    struct tm time = {PyDateTime_DATE_GET_SECOND($input),
                      PyDateTime_DATE_GET_MINUTE($input),
                      PyDateTime_DATE_GET_HOUR($input),
                      PyDateTime_GET_DAY($input),
                      PyDateTime_GET_MONTH($input) - 1,
                      PyDateTime_GET_YEAR($input) - 1900};
    time64 secs = gnc_mktime(&time);
    $1 = &secs;
}

// A typemap for converting time64 values returned from functions to
// python dates. Note that we can't use Python DateTime's fromtimestamp function because it relies upon libc's localtime. Note also that while we create times with timegm we retrieve it with localtime
%typemap(out) time64 {
    PyDateTime_IMPORT;
    struct tm t;
    gnc_localtime_r(&$1, &t);
    $result = PyDateTime_FromDateAndTime(t.tm_year + 1900, t.tm_mon + 1,
                                         t.tm_mday, t.tm_hour, t.tm_min,
                                         t.tm_sec, 0);
}
