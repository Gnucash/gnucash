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
#include "OutputCharStream.h"

#define OUTPUT_MESSAGES

#ifdef OUTPUT_MESSAGES
#include "sptchar.h"
#include "MessageReporter.h"
#include "MessageTable.h"
#endif

#ifdef SP_NAMESPACE
namespace SP_NAMESPACE {
#endif


#ifdef SP_MANUAL_INST

#define SP_DEFINE_TEMPLATES
#include "Owner.h"
#undef SP_DEFINE_TEMPLATES

#include "Message.h"

#ifdef SP_ANSI_CLASS_INST
template class Owner< Messenger>;
#else
typedef Owner< Messenger> Dummy_0;
#endif

#endif /* SP_MANUAL_INST */


static FileOutputByteStream standardOutput(1, 0);
static FileOutputByteStream standardError(2, 0);

#ifdef OUTPUT_MESSAGES

class MyMessageReporter
: public MessageReporter
{
  public:
    MyMessageReporter( const InputCodingSystem *, OutputCharStream *);
  private:
    Boolean getMessageText( const MessageFragment &, StringC &);
    const InputCodingSystem * codingSystem_;
};

MyMessageReporter::
MyMessageReporter( const InputCodingSystem * codesys,
		   OutputCharStream * errorStream)
: MessageReporter( errorStream),
  codingSystem_( codesys)
{
}

Boolean
MyMessageReporter::
getMessageText( const MessageFragment & frag, StringC & text)
{
  String<SP_TCHAR> str;
  if (!MessageTable::instance()->getText(frag, str))
    return 0;
#ifdef SP_WIDE_SYSTEM
  text.assign((const Char *)str.data(), str.size());
#else
  str += 0;
  text = codingSystem_->convertIn(str.data());
#endif
  return 1;
}

#endif /* OUTPUT_MESSAGES */

void
outputModelGroup( OutputCharStream & os, const ModelGroup * modelGroup, unsigned level)
{
    os << "( ";
    level++;
    const char * connector = 0x0;
    switch ( modelGroup->connector()) {
      case ModelGroup::andConnector:
	connector = " &";
	break;
      case ModelGroup::orConnector:
	connector = " |";
	break;
      case ModelGroup::seqConnector:
	connector = ",";
	break;
    }
    unsigned i = 0;
    while ( i < modelGroup->nMembers()) {
	const ContentToken & token = modelGroup->member( i);
	const ModelGroup * subModel = token.asModelGroup();
	Boolean tokenIsPcdata = false;
	if ( subModel)
	  outputModelGroup( os, subModel, level);
	else {
	    const LeafContentToken * leaf = token.asLeafContentToken();
	    const ElementType * elementType = leaf->elementType();
	    if ( elementType)
	      os << elementType->name();
	    else {
	      tokenIsPcdata = true;
	      os << "#PCDATA";
	    }
	}
	if ( !tokenIsPcdata) {
	    switch ( token.occurrenceIndicator()) {
	      case ContentToken::none:
		break;
	      case ContentToken::opt:
		os << "?";
		break;
	      case ContentToken::plus:
		os << "+";
		break;
	      case ContentToken::rep:
		os << "*";
		break;
	    }
	}
	if ( ++i >= modelGroup->nMembers())
	  break;
	os << connector << "\n";
	unsigned l = level;
	while ( l--)
	  os << "  ";
	continue;
    }
    os << " )";
}

extern "C"
int
main( int argc, char ** argv)
{
    if ( argc < 2)
      return 1;

    DtdParser parser;

#ifdef OUTPUT_MESSAGES
    Owner< Messenger> mgr;
    if ( argv[ 1][ 0] != '-' || argv[ 1][ 1] != 's' || argv[ 1][ 2] != '\0') {
	mgr = new MyMessageReporter( parser.codingSystem(),
				     new EncodeOutputCharStream( &standardError,
								 parser.codingSystem()));
	parser.setMessenger( mgr.pointer());
    }
#endif

    StringC sysid;
    parser.makeSystemId( argc - 1, argv + 1, sysid);
    ConstPtr< Dtd> dtd = parser.parseDtd( sysid);

    if ( dtd.isNull())
      return 1;

    EncodeOutputCharStream os( &standardOutput, parser.codingSystem());
    Dtd::ConstElementTypeIter e( dtd->elementTypeIter());
    const ElementType * type = e.next();
    while ( type) {
	os << type->name() << " =\n  ";
	switch ( type->definition()->declaredContent()) {
	  case ElementDefinition::modelGroup:
	    outputModelGroup( os, type->definition()->compiledModelGroup()->modelGroup(), 1);
	    break;
	  case ElementDefinition::any: {
	      Dtd::ConstElementTypeIter i( dtd->elementTypeIter());
	      os << "( #PCDATA";
	      const ElementType * type;
	      while ( ( type = i.next()))
		os << " |\n    " << type->name();
	      os << " )*";
	      break;
	  }
	  case ElementDefinition::cdata:
	    os << "CDATA";
	    break;
	  case ElementDefinition::rcdata:
	    os << "RCDATA";
	    break;
	  case ElementDefinition::empty:
	    os << "EMPTY";
	    break;
	}
	os << "\n";
	const AttributeDefinitionList * attlist = type->attributeDefTemp();
	if ( attlist) {
	    os << "[\n";
	    for ( size_t a = 0; a < attlist->size(); a++) {
		const AttributeDefinition * def = attlist->def( a);
		os << "  " << def->name() << " : ";
		size_t indent = def->name().size() + 5;
		AttributeDefinitionDesc desc;
		def->getDesc( desc);
		switch ( desc.declaredValue) {
		  case AttributeDefinitionDesc::cdata:
		    os << "CDATA";
		    break;
		  case AttributeDefinitionDesc::name:
		    os << "NAME";
		    break;
		  case AttributeDefinitionDesc::number:
		    os << "NUMBER";
		    break;
		  case AttributeDefinitionDesc::nmtoken:
		    os << "NMTOKEN";
		    break;
		  case AttributeDefinitionDesc::nutoken:
		    os << "NUTOKEN";
		    break;
		  case AttributeDefinitionDesc::entity:
		    os << "ENTITY";
		    break;
		  case AttributeDefinitionDesc::idref:
		    os << "IDREF";
		    break;
		  case AttributeDefinitionDesc::names:
		    os << "NAMES";
		    break;
		  case AttributeDefinitionDesc::numbers:
		    os << "NUMBERS";
		    break;
		  case AttributeDefinitionDesc::nmtokens:
		    os << "NMTOKENS";
		    break;
		  case AttributeDefinitionDesc::nutokens:
		    os << "NUTOKENS";
		    break;
		  case AttributeDefinitionDesc::entities:
		    os << "ENTITIES";
		    break;
		  case AttributeDefinitionDesc::idrefs:
		    os << "IDREFS";
		    break;
		  case AttributeDefinitionDesc::id:
		    os << "ID";
		    break;
		  case AttributeDefinitionDesc::notation:
		    os << "NOTATION";
		    indent += 8;
		  case AttributeDefinitionDesc::nameTokenGroup: {
		      os << "( ";
		      indent += 2;
		      size_t i;
		      size_t v = 0;
		      while ( v < desc.allowedValues.size()) {
			  os << desc.allowedValues[ v];
			  if ( ++v >= desc.allowedValues.size())
			    break;
			  os << " |\n";
			  i = indent;
			  while ( i--)
			    os << " ";
			  continue;
		      }
		      os << " )";
		      break;
		  }
		}
		os << "\n";
	    }
	    os << "]\n";
	}
	if ( !( type = e.next()))
	  break;
	os << "\n";
	continue;
    }
    return 0;
}

#ifdef SP_NAMESPACE
}
#endif
