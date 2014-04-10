//
// FILE:
// pfxCompTypes.h
//
// FUNCTION:
// Parses OFX dtd's
//
// HISTORY:
// Written by Linas Vepstas March 1998

#ifndef __PFX_COMP_TYPES_H__
#define __PFX_COMP_TYPES_H__

#include "config.h"
#include "DtdParser.h"

#include "pfxBaseTypes.h"
#include "pfxUtils.h"


// ===========================================================
// handles compund types
// IsUnion returns true if its pure-or 

class pfxCompoundType : 
   public pfxBaseType
{
    public:
        pfxCompoundType (void);
        char * AddCompoundType (const ElementType *);

        void  PrintClass (const ElementType *);

        void  PrintMember (const ContentToken &);

        int   IsUnion (const ModelGroup *);
        void  PrintUnionDecl (const ContentToken &);
        void  PrintMacroDecl (const ContentToken &);

        char * GetUnnamedClassName (const ContentToken &);
        void  PrintUnnamedClassDecl (const ContentToken &);
        void  PrintUnnamedInstance (const ContentToken &);

        void  PrintGroupMembers (const ModelGroup *);

        char * IsClass (char *);

   private:
      char * classname[MAXVARS];
      int nclasses;
};

#ifdef SP_NAMESPACE
}
#endif

#endif /* __PFX_COMP_TYPES_H__ */
