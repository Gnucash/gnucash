//
// FILE:
// utils.C
//
// FUNCTION:
// Handles OFX base types 
//
// HISTORY:
// Written by Linas Vepstas March 1998

#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "StringOf.h"

#include "config.h"
#include "pfxUtils.h"

#ifdef SP_NAMESPACE
namespace SP_NAMESPACE {
#endif

// ===========================================================
// convert the string type to an ordinary char *

char *
pfxCharify (const String<short unsigned int> &s)
{
   int len = s.size ();

   char * str = new char [len+1];

   for (int i=0; i<len; i++) {
      str[i] = s[i];
   }
   str[len] = 0x0;

   return str;
}

char * pfxToLower (char * str)
{
   int len = strlen (str);

   char * low = new char [len+1];
   
   for (int i=0; i<len; i++) {
      low[i] = tolower (str[i]);
   }
   low[len] = 0x0;

   return low;
}

char * pfxToUpper (char * str)
{
   int len = strlen (str);

   char * hi = new char [len+1];
   
   for (int i=0; i<len; i++) {
      hi[i] = toupper (str[i]);
   }
   hi[len] = 0x0;

   return hi;
}

char * pfxCapLower (char * str)
{
   int len = strlen (str);

   char * low = new char [len+1];
   
   for (int i=0; i<len; i++) {
      low[i] = tolower (str[i]);
   }

   if (1 < len) {
      low[0] = str[0];
   }

   low[len] = 0x0;

   return low;
}


#ifdef SP_NAMESPACE
}
#endif
