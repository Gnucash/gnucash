//
// FILE:
// pfxLangOut.C
//
// FUNCTION:
// Prints the C++ equivalents of the OFX dtd's
// The actual implementation of this stuff is very 
// much of an ugly hack. Sorry.
//
// HISTORY:
// Written by Linas Vepstas April 1998

#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

#include "pfxLangOut.h"
#include "pfxUtils.h"

pfxLangOutput :: pfxLangOutput (void)
{
   dent = 0;
   prefix = 0x0;
}

// =======================================================
pfxOutDecl :: pfxOutDecl (void)
{
   nelts = 0;
   doing_union = 0;
   doing_class = 0;
   nth_union = 0;
   prefix = strdup("cfx");
}

void
pfxOutDecl :: PrintClassProlog (char * decl)
{
    if (0 == doing_class) nth_union = 0;
    doing_class = 1;

    DENT (dent); printf ("\n\n");
    DENT (dent); printf ("class cfx%s \n", decl);
    DENT (dent); printf ("{ \n");
    DENT (dent); printf ("    public:\n");
    DENT (dent); printf ("        cfx%s (void);      // constructor \n", decl);
    DENT (dent); printf ("        virtual ~cfx%s (); // destructor \n", decl);
    DENT (dent); printf ("\n");

    dent +=2;
}

void
pfxOutDecl :: PrintClassEpilog (char * decl)
{
    doing_class = 0;
    dent -=2;
    DENT (dent); printf ("};\n");
}

void
pfxOutDecl :: PrintUnionProlog (occ rance, char * decl)
{
   doing_union = 1;
   nth_union ++;
}

void
pfxOutDecl :: PrintUnionEpilog (occ rance, char * decl)
{
   // not an onion any more, so that PrintMember will revert to normal
   doing_union = 0;

   // print enumerated type for the union members
   DENT (dent); printf ("enum Enum%d{\n", nth_union);
   for (int i=0; i<nelts; i++) {
      DENT (dent+1); printf ("T_%s,\n", pfxToUpper (varname[i]));
   }
   DENT (dent); printf ("};\n\n");

   // print the union declaration
   DENT (dent); printf ("union Union%d{\n", nth_union);
   dent ++;
   for (int i=0; i<nelts; i++) {
      char * savefix = prefix;
      prefix = refix[i];
      PrintMember (currance[i], declaration[i], varname[i]);
      prefix = savefix;
   }
   dent --;
   DENT (dent); printf ("};\n\n");

   // print the type that will identify the union
   DENT (dent); printf ("Enum%d utype%d;\n", nth_union, nth_union);

   // print an instance of the union
   char docl[20];
   char vor[20];
   sprintf (docl, "Union%d", nth_union);
   sprintf (vor,  "onion%d", nth_union);
   char * saveprefix = prefix;
   prefix = "";
   PrintMember (rance, docl, vor);
   prefix = saveprefix;
   DENT (dent); printf ("\n");

   nelts = 0;
}

void
pfxOutDecl :: PrintMember (occ rance, char * decl, char * var)
{
   // for handling a union, just collect the names
   if (doing_union) {
      declaration.insert (declaration.begin()+ nelts, strdup (decl));
      varname.insert (varname.begin()+ nelts, strdup (var));
      refix.insert (refix.begin()+ nelts, strdup (prefix));
      currance.insert (currance.begin()+ nelts, rance);
      nelts ++;
      return;
   }

   // for handling ordinary class members, just print them.
   switch (rance) {
      case MUST:
         if (0 < nth_union) {
            printf ("\n");
            DENT (dent);
            printf ("// %s *must* be present \n", decl);
         }
      
         DENT (dent);
         printf ("%s%s *%s; \n", prefix, decl, var);
         break;

      case OPT:
         if (0 < nth_union) {
            printf ("\n");
            DENT (dent);
            printf ("// %s may be a null pointer \n", decl);
         }
      
         DENT (dent);
         printf ("%s%s *%s; \n", prefix, decl, var);
         break;

      case PLUS:
         if (0 < nth_union) {
            printf ("\n");
            DENT (dent);
            printf ("// %s is a list of one or more items \n", decl);
         }
      
         DENT (dent);
         printf ("%s%s **%s; \n", prefix, decl, var);
         break;

      case REP:
         if (0 < nth_union) {
            printf ("\n");
            DENT (dent);
            printf ("// %s is a list of zero or more items \n", decl);
         }
      
         DENT (dent);
         printf ("%s%s **%s; \n", prefix, decl, var);
   }
}

// =======================================================

pfxOutConstructor :: pfxOutConstructor (void)
{
   prefix = strdup ("cfx");
   doing_union = 0;
   doing_class = 0;
   nth_union = 0;
   done_union = 0;
   nelts = 0;
   nest = 0x0;
   scope [0] = 0x0;
}

void
pfxOutConstructor :: PrintClassProlog (char * decl)
{
    if (0 == doing_class) nth_union = 0;

    // check for subclass nesting 
    if (doing_class) {
       nest = new pfxOutConstructor ();
       strcpy (nest -> scope, scope); 
       nest->dent = dent;
       nest->PrintClassProlog (decl);
    } else {
       strcpy (classname, prefix);
       strcat (classname, decl);
       strcat (scope, classname);
       strcat (scope, " :: "); 
    }
    doing_class = 1;

}

void
pfxOutConstructor :: PrintClassEpilog (char * decl)
{
    // handle subclass nesting
    if (nest) {
       nest -> PrintClassEpilog (decl);
       delete nest;
       nest = 0x0;
    }

    doing_class = 0;
    DENT (dent); printf ("\n\n");
    DENT (dent); printf ("%s %s (void)\n", scope, classname);
    DENT (dent); printf ("{\n");
    dent ++;
    for (int i=0; i<nelts; i++) {
        if (override[i][0]) {
           DENT (dent); printf ("%s", override[i]);
        } else {
           PrintMember (currance[i], declaration[i], varname[i]);
        }
     }
    dent --;
    DENT (dent); printf ("}\n");

    nelts = 0;
    scope[0] = 0x0;
}

void
pfxOutConstructor :: PrintUnionProlog (occ rance, char * decl)
{
    nth_union ++;
    doing_union = 1;
    done_union = 0;
}

void
pfxOutConstructor :: PrintUnionEpilog (occ rance, char * decl)
{
    doing_union = 0;
}

void
pfxOutConstructor :: PrintMember (occ rance, char * decl, char * var)
{
   if (nest) {
      nest -> PrintMember (rance, decl, var);
      return;
   }

   if (doing_class) {
      if (doing_union) {
         if (done_union) return;

         char buff[1000];
         sprintf (buff, 
              "utype%d = T_%s;\n", nth_union, pfxToUpper (decl));
         override.insert (override.begin()+ nelts, strdup (buff));
         nelts ++;

         done_union = 1;
         return;
      }
      declaration.insert (declaration.begin()+ nelts, strdup (decl));
      varname.insert (varname.begin()+ nelts, strdup (var));
      refix.insert (refix.begin()+ nelts, strdup (prefix));
      override.insert (override.begin()+ nelts, strdup (""));
      currance.insert (currance.begin()+ nelts, rance);
      nelts ++;
      return;
   }

   switch (rance) {
      case MUST:
         printf ("\n");
         DENT (dent);
         printf ("// %s *must* be present \n", decl);
      
         DENT (dent);
         printf ("%s = new %s%s; \n", var, prefix, decl);
         break;

      case OPT:
         printf ("\n");
         DENT (dent);
         printf ("// %s may be a null pointer \n", decl);
      
         DENT (dent);
         printf ("%s = 0x0; \n", var);
         break;

      case PLUS:
         printf ("\n");
         DENT (dent);
         printf ("// %s is a list of one or more items \n", decl);
      
         DENT (dent);
         printf ("%s = 0x0; \n", var);
         break;

      case REP:
         printf ("\n");
         DENT (dent);
         printf ("// %s is a list of zero or more items \n", decl);
      
         DENT (dent);
         printf ("%s = 0x0; \n", var);
         break;
   }

}

