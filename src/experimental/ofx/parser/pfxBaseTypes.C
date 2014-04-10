//
// FILE:
// basTypes.C
//
// FUNCTION:
// Handles OFX base types 
//
// HISTORY:
// Written by Linas Vepstas March 1998

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "config.h"
#include "DtdParser.h"

#include "pfxUtils.h"
#include "pfxBaseTypes.h"

// ===========================================================
// class pfxBaseType stores & handles base types


pfxBaseType :: pfxBaseType (void)
{
    prtout = 0x0;
    nvars = 0;
    varname[0] = 0x0;
    vartype[0] = 0x0;
}

char * 
pfxBaseType :: AddBaseVar (const ElementType *type)
{

    // its not a base type unless we have a model group.
    if (ElementDefinition::modelGroup != 
        type->definition()->declaredContent()) return 0x0;

    const ModelGroup * mg;
    mg = type->definition()->compiledModelGroup()->modelGroup();

    // to be a base type, it must have exactly one member
    if (1 != mg->nMembers()) return 0x0;

    const ContentToken & token = mg->member (0);

    // anything with a sub-model cannot be a base type
    const ModelGroup * subModel = token.asModelGroup();
    if (subModel)  return 0x0;

    const LeafContentToken * leaf = token.asLeafContentToken();
    const ElementType * elementType = leaf->elementType();

    if (!elementType) {
       char * name = pfxCharify (type->name());

       printf ("Unkonwon, unexpected PCDATA: %s \n", name);
       return 0x0;
    }

    // now look to see if we have already processed this one 
    char *vname = pfxCharify (type->name());

    // if its already in out database, just return
    for (int i = 0; i<nvars; i++) {
        if (!strcmp (vname, varname[i])) {
           delete vname;
           return varname[i];
        }
    }

    // look to see if this is a typ[e that we recognize
    char * vtype = pfxCharify (elementType->name());
    if (strcmp ("XACC-AMOUNT",   vtype) &&
        strcmp ("XACC-BOOL",     vtype) &&
        strcmp ("XACC-DTTM",     vtype) &&
        strcmp ("XACC-ID",       vtype) &&
        strcmp ("XACC-INT",      vtype) &&
        strcmp ("XACC-PRICE",    vtype) &&
        strcmp ("XACC-RATE",     vtype) &&
        strcmp ("XACC-SRVRID",   vtype) &&
        strcmp ("XACC-STR",      vtype) &&
        strcmp ("XACC-URL",      vtype) &&
        strcmp ("XACC-UUID",     vtype)) {

        delete vtype;
        delete vname;
        return 0x0;
    } 

    // we haven't encounterd this before. Add it to our list.
    varname[nvars] = vname;
    vartype[nvars] = vtype;
    nvars ++;

    return vname;
}

void 
pfxBaseType :: PrintBaseTypes (int dent)
{

    DENT (dent); printf ("// Basic Types \n");
    DENT (dent); printf ("typedef double bfxAmount; \n");
    DENT (dent); printf ("typedef int    bfxBool; \n");
    DENT (dent); printf ("typedef long int  bfxDttm; \n");
    DENT (dent); printf ("typedef char * bfxId; \n");
    DENT (dent); printf ("typedef int    bfxInt; \n");
    DENT (dent); printf ("typedef double bfxPrice; \n");
    DENT (dent); printf ("typedef double bfxRate; \n");
    DENT (dent); printf ("typedef char * bfxStr; \n");
    DENT (dent); printf ("typedef char * bfxSrvrid; \n");
    DENT (dent); printf ("typedef char * bfxUrl; \n");
    DENT (dent); printf ("typedef char   bfxUuid [36]; \n");
    printf ("\n\n");
}

void 
pfxBaseType :: PrintBaseDecl (const ContentToken & token)
{
    // print an individual member
    const LeafContentToken * leaf = token.asLeafContentToken();
    const ElementType * elementType = leaf->elementType();
    if (!elementType) return;

    char * varname = pfxCharify (elementType->name());
                
    if (!varname) return;

    char * vtype = GetBaseType (varname);
    if (!vtype) return;

    char * decl;

    if (!strcmp ("XACC-AMOUNT", vtype)) {
       decl = "bfxAmount";
    } else

    if (!strcmp ("XACC-BOOL", vtype)) {
       decl = "bfxBool";
    } else

    if (!strcmp ("XACC-DTTM", vtype)) {
       decl = "bfxDttm";
    } else

    if (!strcmp ("XACC-ID", vtype)) {
       decl = "bfxId";
    } else

    if (!strcmp ("XACC-INT", vtype)) {
       decl = "bfxInt";
    } else

    if (!strcmp ("XACC-PRICE", vtype)) {
       decl = "bfxPrice";
    } else

    if (!strcmp ("XACC-RATE", vtype)) {
       decl = "bfxRate";
    } else

    if (!strcmp ("XACC-STR", vtype)) {
       decl = "bfxStr";
    } else
    
    if (!strcmp ("XACC-SRVRID", vtype)) {
       decl = "bfxSrvrid";
    } else
    
    if (!strcmp ("XACC-URL", vtype)) {
       decl = "bfxUrl";
    } else
    
    if (!strcmp ("XACC-UUID", vtype)) {
       decl = "bfxUuid";
    } else
    
    printf ("---> unknown type >%s<\n", vtype);


    // handle repeated lists
    pfxLangOutput::occ currance;
    switch ( token.occurrenceIndicator()) {

      // element *must* occur
      case ContentToken::none:
        currance = pfxLangOutput::MUST;
        break;

      // element may or may not occur
      case ContentToken::opt:
        currance = pfxLangOutput::OPT;
        break;

      // element must occur at least once, maybe more times.
      case ContentToken::plus:
        currance = pfxLangOutput::PLUS;
        break;

      // element may occur zero or more times
      case ContentToken::rep:
        currance = pfxLangOutput::REP;
        break;
    }

    // printed var names will be lower case
    char * vname = pfxToLower (varname);

    char * saveprefix = prtout->prefix;

    prtout -> prefix = "";
    prtout -> PrintMember (currance, decl, vname);
    prtout -> prefix = saveprefix;

    delete vname;
    delete varname;
}


char *
pfxBaseType :: GetBaseType (char * vname)
{

    for (int i = 0; i<nvars; i++) {
        if (!strcmp (vname, varname[i])) return vartype[i];
    }

    return 0x0;
}


#ifdef SP_NAMESPACE
}
#endif
