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

#include "config.h"
#include "DtdParser.h"

#include "macros.h"
#include "sptchar.h"
#include "CodingSystemKit.h"

#include "PosixStorage.h"
#ifdef SP_WININET
#include "WinInetStorage.h"
#else
#include "URLStorage.h"
#endif
#include "LiteralStorage.h"
#include "NotationStorage.h"
#include "ExtendEntityManager.h"
#include "SOEntityCatalog.h"

#include "ErrorCountEventHandler.h"

#ifndef SP_DEFAULT_ENCODING
#ifdef WIN32
#define SP_DEFAULT_ENCODING SP_T("WINDOWS")
#else
#define SP_DEFAULT_ENCODING  SP_T("IS8859-1")
#endif
#endif /* not SP_DEFAULT_ENCODING */

#ifndef SGML_SEARCH_PATH_DEFAULT
#define SGML_SEARCH_PATH_DEFAULT SP_T("")
#endif

#ifndef SGML_CATALOG_FILES_DEFAULT
#define SGML_CATALOG_FILES_DEFAULT SP_T("")
#endif /* not SGML_CATALOG_FILES_DEFAULT */

#ifdef SP_NAMESPACE
namespace SP_NAMESPACE {
#endif

#ifdef SP_MSDOS_FILENAMES
const Char FILE_SEP = ';';
#else
const Char FILE_SEP = ':';
#endif

DtdParser::
DtdParser( const char * requiredInternalCode)
: internalCharsetIsDocCharset_( 1),
  codingSystem_( 0),
  mapCatalogDocument_(0),
  mgr_( &nullMgr_)
{
    initCodingSystem( requiredInternalCode);
}

void
DtdParser::
setMessenger( Messenger * mgr)
{
    mgr_ = mgr ? mgr : &nullMgr_;
}

class DtdParserEventHandler
: public ErrorCountEventHandler
{
  public:
    DtdParserEventHandler( Messenger *);
    void message( MessageEvent *);
  private:
    Messenger * mgr_;
};

DtdParserEventHandler::
DtdParserEventHandler( Messenger * mgr)
: mgr_( mgr)
{
}

void
DtdParserEventHandler::
message( MessageEvent * event)
{
    // ignore ParserMessages::documentEndProlog and ParserMessages::documentElementUndefined
    if ( event->message().type->number() != 45 && event->message().type->number() != 319) {
	mgr_->dispatchMessage( event->message());
    }
    ErrorCountEventHandler::message( event);
}

ConstPtr< Dtd>
DtdParser::
parseDtd( const StringC & sysid)
{
    DtdParserEventHandler handler( mgr_);

    SgmlParser::Params sdParams;
    sdParams.sysid = convertInput( SP_T( "<LITERAL> "));
    sdParams.entityManager = entityManager().pointer();
    sdParams.options = &options_;
    sdParser_.init( sdParams);
    sdParser_.parseAll( handler, handler.cancelPtr());

    SgmlParser::Params params;
    params.sysid = sysid;
    params.entityType = SgmlParser::Params::dtd;
// unnecessary, since params.doctypeName starts out empty anyway.
//    params.doctypeName = convertInput( SP_T( ""));
    params.entityManager = entityManager().pointer();
    params.options = &options_;
    params.parent = &sdParser_;
    parser_.init( params);
    parser_.parseAll( handler, handler.cancelPtr());

    Ptr< Dtd> dtd( parser_.baseDtd());
    delete dtd->removeElementType( params.doctypeName);

    return dtd;
}

Boolean
stringMatches( const SP_TCHAR * s, const char * key)
{
  for (; *key != '\0'; s++, key++) {
    if (*s != tolower(*key) && *s != toupper(*key))
      return 0;
  }
  return *s == '\0';
}

void
DtdParser::
initCodingSystem( const char * requiredInternalCode)
{
  const char *name = requiredInternalCode;
#ifdef SP_MULTI_BYTE
  char buf[256];
  if (!name) {
    const SP_TCHAR *internalCode = tgetenv(SP_T("SP_SYSTEM_CHARSET"));
    if (internalCode) {
      buf[255] = '\0';
      for (size_t i = 0; i < 255; i++) {
	buf[i] = internalCode[i];
	if (buf[i] == '\0')
	  break;
      }
      name = buf;
    }
  }
  if (requiredInternalCode)
    internalCharsetIsDocCharset_ = 0;
  else {
    const SP_TCHAR *useInternal = tgetenv(SP_T("SP_CHARSET_FIXED"));
    if (useInternal
        && (stringMatches(useInternal, "YES")
	    || stringMatches(useInternal, "1")))
      internalCharsetIsDocCharset_ = 0;
  }
#endif /* SP_MULTI_BYTE */
  codingSystemKit_ = CodingSystemKit::make(name);
  const SP_TCHAR *codingName = tgetenv(internalCharsetIsDocCharset_
				       ? SP_T("SP_BCTF")
				       : SP_T("SP_ENCODING"));
  if (codingName)
    codingSystem_ = lookupCodingSystem(codingName);
#ifdef SP_MULTI_BYTE
  if (!codingSystem_ && !internalCharsetIsDocCharset_)
    codingSystem_ = lookupCodingSystem(SP_DEFAULT_ENCODING);
#endif
  if (!codingSystem_
#ifndef SP_WIDE_SYSTEM
      || codingSystem_->fixedBytesPerChar() > 1
#endif
    )
    codingSystem_ = codingSystemKit_->identityCodingSystem();
}

const CodingSystem *
DtdParser::
lookupCodingSystem( const AppChar * codingName)
{
#define MAX_CS_NAME 50
  if (tcslen(codingName) < MAX_CS_NAME) {
    char buf[MAX_CS_NAME];
    int i;
    for (i = 0; codingName[i] != SP_T('\0'); i++) {
      SP_TUCHAR c = codingName[i];
#ifdef SP_WIDE_SYSTEM
      if (c > (unsigned char)-1)
	return 0;
#endif
      buf[i] = char(c);
    }
    buf[i] = '\0';
    return codingSystemKit_->makeCodingSystem(buf, internalCharsetIsDocCharset_);
  }
  return 0;
}

StringC
DtdParser::
convertInput( const SP_TCHAR * s)
{
#ifdef SP_WIDE_SYSTEM
  StringC str(s, wcslen(s));
#else
  StringC str(codingSystem()->convertIn(s));
#endif
  for (size_t i = 0; i < str.size(); i++)
    if (str[i] == '\n')
      str[i] = '\r';
  return str;
}

Boolean
DtdParser::
makeSystemId( int nFiles, AppChar * const * files, StringC & result)
{
  Vector<StringC> filenames(nFiles == 0 ? 1 : nFiles);
  int i;
  for (i = 0; i < nFiles; i++)
    filenames[i] = convertInput(tcscmp(files[i], SP_T("-")) == 0
				? SP_T("<OSFD>0")
				: files[i]);
  if (nFiles == 0)
    filenames[0] = convertInput(SP_T("<OSFD>0"));
  return entityManager()->mergeSystemIds(filenames,
					 mapCatalogDocument_,
					 systemCharset(),
					 *mgr_,
					 result);
}

Ptr< ExtendEntityManager> &
DtdParser::
entityManager()
{
  if (!entityManager_.isNull())
    return entityManager_;
  PosixStorageManager *sm
    = new PosixStorageManager("OSFILE",
			      &systemCharset(),
#ifndef SP_WIDE_SYSTEM
			      codingSystem(),
#endif
			      5);
  size_t i;
  for (i = 0; i < searchDirs_.size(); i++)
    sm->addSearchDir(convertInput(searchDirs_[i]));
  {
    const AppChar *e = tgetenv(SP_T("SGML_SEARCH_PATH"));
    if (!e)
      e = SGML_SEARCH_PATH_DEFAULT;
    if (*e) {
      StringC str(convertInput(e));
      size_t i = 0;
      size_t start = 0;
      for (;;) {
	if (i == str.size() || str[i] == FILE_SEP) {
	  sm->addSearchDir(StringC(str.data() + start,
				   i - start));
	  if (i == str.size())
	    break;
	  start = ++i;
	}
	else
	  i++;
      }
    }
  }

  entityManager_ = ExtendEntityManager::make(sm,
					     codingSystem(),
					     inputCodingSystemKit(),
					     internalCharsetIsDocCharset_);
#ifdef SP_WININET
  entityManager_->registerStorageManager(new WinInetStorageManager("URL"));
#else
  entityManager_->registerStorageManager(new URLStorageManager("URL"));
#endif
  entityManager_->registerStorageManager(new LiteralStorageManager("LITERAL"));
  entityManager_->registerStorageManager(new NotationStorageManager("CLSID"));
  entityManager_->registerStorageManager(new NotationStorageManager("MIMETYPE"));
  Vector<StringC> v;
  for (i = 0; i < catalogSysids_.size(); i++)
    // filenames specified on command-line must exist
    v.push_back(convertInput(catalogSysids_[i]));
  {
    const AppChar *e = tgetenv(SP_T("SGML_CATALOG_FILES"));
    if (!e)
      e = SGML_CATALOG_FILES_DEFAULT;
    if (*e) {
      StringC str(convertInput(e));
      size_t i = 0;
      size_t start = 0;
      for (;;) {
	if (i == str.size() || str[i] == FILE_SEP) {
	  v.push_back(StringC(str.data() + start,
			      i - start));
	  if (i == str.size())
	    break;
	  start = ++i;
	}
	else
	  i++;
      }
    }
  }
  entityManager_->setCatalogManager(SOCatalogManager::make(v,
							   catalogSysids_.size(),
							   &systemCharset(),
							   &systemCharset(),
				 			   0));
  return entityManager_;
}

#ifdef SP_NAMESPACE
}
#endif
