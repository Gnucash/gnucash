//
// FILE:
// pfxLangOut.h
//
// FUNCTION:
// print language specific output
// In this case, this generates C++ output
//
// HISTORY:
// Written by Linas Vepstas April 1998

#ifndef __PFX_COMP_OUT_H__
#define __PFX_COMP_OUT_H__

#include "config.h"
#include <vector.h>

// virtual base class for output
// This is the base class that is used by the parser to 
// generate output.
class pfxLangOutput 
{
   public:
     enum occ {
        MUST, OPT, PLUS, REP };
     pfxLangOutput (void);
     virtual void PrintClassProlog (char *hilo) = 0;
     virtual void PrintClassEpilog (char *hilo) = 0;
     virtual void PrintUnionProlog (occ, char *hilo) = 0;
     virtual void PrintUnionEpilog (occ, char *hilo) = 0;

     virtual void PrintMember (occ, char *hilo, char * lo) = 0;

      char * prefix;

   protected:
      int dent;

};

// This class prints the class and union declarations
class pfxOutDecl  :
   public pfxLangOutput
{
   public:
     pfxOutDecl (void);
     virtual void PrintClassProlog (char *hilo);
     virtual void PrintClassEpilog (char *hilo);
     virtual void PrintUnionProlog (occ, char *hilo);
     virtual void PrintUnionEpilog (occ, char *hilo);

     virtual void PrintMember (occ, char *hilo, char * lo);

   private:
      short doing_union;
      short doing_class;
      int nth_union;

      // When a union has been declared, these are used to store 
      // the members of the union.   This is needed because
      // declaring a union requires several repeats of the data,
      // each in slightly different form.
      vector<char *> declaration;
      vector<char *> varname;
      vector<char *> refix;
      vector<occ>   currance;
      int nelts;

};

// This class prints the constructors
class pfxOutConstructor  :
   public pfxLangOutput
{
   public:
     pfxOutConstructor (void);
     virtual void PrintClassProlog (char *hilo);
     virtual void PrintClassEpilog (char *hilo);
     virtual void PrintUnionProlog (occ, char *hilo);
     virtual void PrintUnionEpilog (occ, char *hilo);

     virtual void PrintMember (occ, char *hilo, char * lo);

   private:
      short doing_union;
      short doing_class;
      int nth_union;
      short done_union;

      pfxOutConstructor *nest;
      char scope[450];
      char classname[450];

      // when a class within a class has been dclared, we
      // can't just create a constructor within the constructor.
      // Thus, we need to defer the PrintMember() calls until the
      // call to epilog has shown up.
      int nelts;
      vector<char *> declaration;
      vector<char *> varname;
      vector<char *> refix;
      vector<char *> override;
      vector<occ>   currance;

};

#endif /* __PFX_COMP_OUT_H__ */
