//
// FILE:
// pfxBaseTypes.h
//
// FUNCTION:
// Handles OFX base types 
//
// HISTORY:
// Written by Linas Vepstas March 1998

#ifndef __PFX_BASE_TYPES_H__
#define __PFX_BASE_TYPES_H__

#include "config.h"
#include "DtdParser.h"

#include "pfxUtils.h"
#include "pfxBaseTypes.h"
#include "pfxLangOut.h"

// ===========================================================
// class pfxBaseType stores & handles base types

// AddBaseVar adds a new variable with a base type.  
//    returns pointer to variable name if a base type, else returns null.

#define MAXVARS 3000

class pfxBaseType {
   public:
      pfxBaseType (void);

      char * AddBaseVar (const ElementType *);
      char * GetBaseType (char *varname);
      void PrintBaseTypes (int dent);
      void PrintBaseDecl (const ContentToken & token);

      pfxLangOutput *prtout;

   private:
      char * varname[MAXVARS];
      char * vartype[MAXVARS];
      int nvars;

};

#ifdef SP_NAMESPACE
}
#endif

#endif /* __PFX_BASE_TYPES_H__ */
