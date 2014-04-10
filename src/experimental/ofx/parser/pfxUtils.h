//
// FILE:
// pfxUtils.h
//
// FUNCTION:
// Assorted utilities
//
// HISTORY:
// Written by Linas Vepstas March 1998


#ifndef __PFX_UTILS_H__
#define __PFX_UTILS_H__

#include <stdio.h>

#include "StringOf.h"

#include "config.h"

#ifdef SP_NAMESPACE
namespace SP_NAMESPACE {
#endif

// ===========================================================
// convert the string type to an ordinary char *

char * pfxCharify (const String<short unsigned int> &s);
char * pfxToLower (char * str);
char * pfxToUpper (char * str);
char * pfxCapLower (char * str);

#define DENT(dent) { for (int bonk=0; bonk<(dent); bonk++) printf ("    "); }


#ifdef SP_NAMESPACE
}
#endif

#endif /* __PFX_UTILS_H__ */
