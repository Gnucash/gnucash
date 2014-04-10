//
// FILE:
// pfxCompTypes.C
//
// FUNCTION:
// Parses OFX dtd's
//
// HISTORY:
// Written by Linas Vepstas March 1998

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "config.h"
#include "DtdParser.h"

#include "pfxBaseTypes.h"
#include "pfxCompTypes.h"
#include "pfxUtils.h"


// =====================================================

pfxCompoundType :: pfxCompoundType (void)
{
   nclasses = 0;
   classname[0] = 0x0;
}

char *
pfxCompoundType :: IsClass (char * vname)
{
    for (int i = 0; i<nclasses; i++) {
        if (!strcmp (vname, classname[i])) return classname[i];
    }

    return 0x0;
}

char * 
pfxCompoundType :: AddCompoundType (const ElementType *type)
{

    // its not a compound type unless we have a model group.
    if (ElementDefinition::modelGroup != 
        type->definition()->declaredContent()) return 0x0;

    // make sure its not a base type
    char * basename = AddBaseVar (type);
    if (basename) return 0x0;

    const ModelGroup * mg;
    mg = type->definition()->compiledModelGroup()->modelGroup();

    char * cname = pfxCharify (type->name());

    classname [nclasses] = cname;
    nclasses ++;

    char * pname = pfxCapLower (cname);
    printf ("class cfx%s;\n", pname);
    delete pname;

    return cname;
}

void 
pfxCompoundType :: PrintMember (const ContentToken & token)
{
    if (!prtout) return;

    // print an individual member
    const LeafContentToken * leaf = token.asLeafContentToken();
    const ElementType * elementType = leaf->elementType();
    if (!elementType) return;

    char * varname = pfxCharify (elementType->name());
                
    // see if its a base type -- e.g. int, double, etc.
    char * basetype = GetBaseType (varname);
    if (basetype) {
        PrintBaseDecl (token);
    } else {

        // Not base type.  Must be a class
        char * classtype = IsClass (varname);
        if (classtype) {
            char * decl = pfxCapLower (varname);
            char * var  = pfxToLower (varname);

            // look to see if the element occurs
            // once, more than once, etc.
            switch ( token.occurrenceIndicator()) {

              // element *must* occur
              case ContentToken::none:
                prtout -> PrintMember (pfxLangOutput::MUST, decl, var);
                break;

              // element may or may not occur
              case ContentToken::opt:
                prtout -> PrintMember (pfxLangOutput::OPT, decl, var);
                break;

              // element must occur at least once, maybe more times.
              case ContentToken::plus:
                prtout -> PrintMember (pfxLangOutput::PLUS, decl, var);
                break;

              // element may occur zero or more times
              case ContentToken::rep:
                prtout -> PrintMember (pfxLangOutput::REP, decl, var);
                break;
            }

            delete decl;
            delete var;
        } else {
            printf (" this variable has no type -----> %s \n", varname);
        }
    }
    delete varname;
}

int  
pfxCompoundType :: IsUnion (const ModelGroup * mg)
{
    if (ModelGroup::orConnector == mg->connector()) return 1;
    return 0;
}

char * 
pfxCompoundType :: GetUnnamedClassName (const ContentToken & untoken)
{
    const ModelGroup * mg = untoken.asModelGroup();

    if (!mg) {
       printf ("Error: GetUnnamedClassName: unexpected non model \n");
       return 0x0;
    }

    // The connector *must* be a sequence or a logical-AND type in order 
    // for this to be a struct.
    if ((ModelGroup::andConnector != mg->connector()) &&
        (ModelGroup::seqConnector != mg->connector())) return 0x0;


    // create a long compound name
    // first, count the length of that name
    int i = 0;
    int namelen = 0;
    while (i < mg->nMembers()) {
        const ContentToken & token = mg->member( i);
        const ModelGroup * subModel = token.asModelGroup();

        if (!subModel) {
            const LeafContentToken * leaf = token.asLeafContentToken();
            const ElementType * elementType = leaf->elementType();
            namelen += (elementType->name()).size() + 1;
        }
        i++;
    }

    // now, build the compund name
    i = 0;
    char * name = new char [namelen+1];
    name[0] = 0x0;
    while (i < mg->nMembers()) {
        const ContentToken & token = mg->member( i);
        const ModelGroup * subModel = token.asModelGroup();

        if (!subModel) {
            const LeafContentToken * leaf = token.asLeafContentToken();
            const ElementType * elementType = leaf->elementType();

            char * varname = pfxCharify (elementType->name());
            char * capname = pfxCapLower (varname);
            strcat (name, "_");
            strcat (name, capname);
        }
        i++;
    }

    return name;
}

void 
pfxCompoundType :: PrintMacroDecl (const ContentToken & untoken)
{
    const ModelGroup * mg = untoken.asModelGroup();

    if (!mg) {
       printf ("Error: PrintMacroDecl: unexpected non model \n");
       return;
    }

    // If connector is a sequence or a logical-AND type,
    // then treat as a class.
    if ((ModelGroup::andConnector == mg->connector()) ||
        (ModelGroup::seqConnector == mg->connector())) {

        PrintUnnamedClassDecl (untoken);
        PrintUnnamedInstance (untoken);
    } else

    // If the connector is a logical-OR connector, 
    // then treat as a union
    if (ModelGroup::orConnector == mg->connector()) {
        PrintUnionDecl (untoken);
    }
}

void 
pfxCompoundType :: PrintUnnamedClassDecl (const ContentToken & untoken)
{
    const ModelGroup * mg = untoken.asModelGroup();

    if (!mg) {
       printf ("Error: PrintUnnamedClassDecl: unexpected non model \n");
       return;
    }

    // The connector *must* be a sequence or a logical-AND type in order 
    // for this to be a struct.
    if ((ModelGroup::andConnector != mg->connector()) &&
        (ModelGroup::seqConnector != mg->connector())) return;

    char * name = GetUnnamedClassName (untoken);

    // If the returned name of the thing is null, then
    // I think we can safely assume that this thing is 
    // the result of a macro expansion in the DTD.
    // This means that there is no true heirarchy, even 
    // though there is an appearent heirarchy. So,
    // don't let things nest. Just flatten it.
    // We flatten by not printing the prolog or epilogue
    // (since there really isn't a prolog or epilog).

    // now, print the class definition
    if (name[0]) prtout->PrintClassProlog (name);
    int i = 0;
    while (i < mg->nMembers()) {
        const ContentToken & token = mg->member( i);
        const ModelGroup * subModel = token.asModelGroup();
        if (subModel) {
            printf (" // -------> begin flattening \n");
            PrintMacroDecl (token);
            printf (" // -------> end flattening \n");
        } else {
            // put the member elements into the class declaration
            PrintMember(token);
        }
        i++;
    }
    if (name[0]) prtout->PrintClassEpilog (name);
}

void 
pfxCompoundType :: PrintUnnamedInstance (const ContentToken & untoken)
{
    const ModelGroup * mg = untoken.asModelGroup();

    if (!mg) {
       printf ("Error: PrintUnnamedInstance: unexpected non model \n");
       return;
    }

    // The connector *must* be a sequence or a logical-AND type in order 
    // for this to be a struct.
    if ((ModelGroup::andConnector != mg->connector()) &&
        (ModelGroup::seqConnector != mg->connector())) return;


    char * name = GetUnnamedClassName (untoken);

    // now handle repeated lists
    pfxLangOutput::occ currance;
    switch ( untoken.occurrenceIndicator()) {

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

    prtout ->PrintMember (currance, name, name);
}

void 
pfxCompoundType :: PrintUnionDecl (const ContentToken & untoken)
{

    const ModelGroup * mg = untoken.asModelGroup();

    if (!mg) {
       printf ("Error: PrintUnionDecl: unexpected non model \n");
       return;
    }

    // The connector *must* be logical-OR type in order 
    // for this to be a union.
    if (ModelGroup::orConnector != mg->connector()) return;

    // now handle repeated lists
    pfxLangOutput::occ currance;
    switch ( untoken.occurrenceIndicator()) {

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

    // if there are any groups that occur,
    // lets create a name for them, and print 
    // thier declarations
    int i = 0;
    while (i < mg->nMembers()) {
        const ContentToken & token = mg->member( i);
        const ModelGroup * subModel = token.asModelGroup();
        if (subModel) {
            // ack -- its an un-named, compound type
            PrintUnnamedClassDecl (token);
        }
        i++;
    }

    // OK, now print the union
    prtout ->PrintUnionProlog (currance, "abcd");

    i = 0;
    while (i < mg->nMembers()) {
        const ContentToken & token = mg->member( i);
        const ModelGroup * subModel = token.asModelGroup();
        if (subModel) {
            // ack -- its an un-named, compound type
            PrintUnnamedInstance (token);
        } else {
            // put the member elements into the class declaration
            PrintMember (token);
        }
        i++;
    }

    prtout ->PrintUnionEpilog (currance, "abcd");
}

void 
pfxCompoundType :: PrintGroupMembers (const ModelGroup * mg)
{

    int i = 0;
    while (i < mg->nMembers()) {
        const ContentToken & token = mg->member( i);
        const ModelGroup * subModel = token.asModelGroup();

        if ( subModel) {
             if (ModelGroup::orConnector == subModel->connector()) {
                 PrintUnionDecl (token);

             } else {
                 if (ContentToken::none == token.occurrenceIndicator()) {
                    PrintGroupMembers (subModel);
                 } else {
                    // ack -- its an un-named, compound type
                    PrintUnnamedClassDecl (token);
                 }
             }
        } else {

            // put the member elements into the class declaration
            PrintMember (token);
        }

        i++;
    }
}


void 
pfxCompoundType :: PrintClass (const ElementType *type)
{

    // its not a compound type unless we have a model group.
    if (ElementDefinition::modelGroup != 
        type->definition()->declaredContent()) return;

    const ModelGroup * mg;
    mg = type->definition()->compiledModelGroup()->modelGroup();

    // open the class declaration
    char * cname = pfxCharify (type->name());
    char * pname = pfxCapLower (cname);

    prtout->PrintClassProlog (pname);

    PrintGroupMembers (mg);

    prtout->PrintClassEpilog (pname);
}

// ===========================================================

#ifdef SP_NAMESPACE
}
#endif
