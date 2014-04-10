// $Id$
// Copyright (C) 1997 ISOGEN International Corp. and TechnoTeacher, Inc.
// All Rights Reserved.
//
// <Restrictions>
// This file and its associated materials are copyrighted material of
// ISOGEN International Corp. (ISOGEN) and TechnoTeacher,
// Inc. (TechnoTeacher).  License to copy and use this file and its
// associated materials is granted to everyone, free of charge, with
// the following restrictions: (1) The ISOGEN and TechnoTeacher
// copyright statement must be maintained in any copies. (2) New
// materials derived from these materials must indicate their source,
// including references to the ISOGEN and TechnoTeacher web sites
// (www.isogen.com and www.techno.com). (3) These materials may not be
// sold in any form without the express written permission of ISOGEN
// and TechnoTeacher.  [However, feel free to sell things you create
// from these materials as long as the things you create are truly
// different in function--we want to encourage people to learn from
// these materials and benefit from having learned--we just don't want
// others to sell what we're giving away.]
// </Restrictions>

#ifndef DtdParser_INCLUDED
#define DtdParser_INCLUDED 1

#include "Dtd.h"
#include "Ptr.h"
#include "Vector.h"
#include "CodingSystem.h"
#include "CodingSystemKit.h"
#include "SgmlParser.h"
#include "ParserOptions.h"
#include "ExtendEntityManager.h"

#ifdef SP_NAMESPACE
namespace SP_NAMESPACE {
#endif

class DtdParser {
  public:
    DtdParser( const char * requiredInternalCode = 0);
    void setMessenger( Messenger *);

    ConstPtr< Dtd> parseDtd( const StringC & sysid);

#ifdef SP_WIDE_SYSTEM
    typedef wchar_t AppChar;
#else
    typedef char AppChar;
#endif
    const CodingSystem * codingSystem();
    const CharsetInfo & systemCharset();
    ConstPtr< InputCodingSystemKit> inputCodingSystemKit();
    StringC convertInput( const AppChar * s);

    Boolean makeSystemId( int nFiles, AppChar * const * files, StringC & result);
    Ptr< ExtendEntityManager> & entityManager();

    SgmlParser & parser();

  private:
    Messenger * mgr_;
    NullMessenger nullMgr_;

    // Coding system stuff
    void initCodingSystem( const char * requiredInternalCode);
    const CodingSystem * lookupCodingSystem( const AppChar * codingName);
    Boolean internalCharsetIsDocCharset_;
    Ptr< CodingSystemKit> codingSystemKit_;
    const CodingSystem * codingSystem_;

    // Entity manager stuff
    Vector< const AppChar *> searchDirs_;
    Vector< const AppChar *> catalogSysids_;
    Boolean mapCatalogDocument_;
    Ptr< ExtendEntityManager> entityManager_;

    // SGML Parser stuff
    ParserOptions options_;
    SgmlParser sdParser_;
    SgmlParser parser_;
};

inline
const CodingSystem *
DtdParser::
codingSystem()
{
    return codingSystem_;
}

inline
ConstPtr< InputCodingSystemKit>
DtdParser::
inputCodingSystemKit()
{
    return codingSystemKit_.pointer();
}

inline
const CharsetInfo &
DtdParser::
systemCharset()
{
    return codingSystemKit_->systemCharset();
}

inline
SgmlParser &
DtdParser::
parser()
{
    return parser_;
}

#ifdef SP_NAMESPACE
}
#endif

#endif /* not DtdParser_INCLUDED */
