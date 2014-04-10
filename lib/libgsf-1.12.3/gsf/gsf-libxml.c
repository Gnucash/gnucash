/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-libxml.c :
 *
 * Copyright (C) 2002-2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2.1 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <gsf-config.h>
#include <gsf/gsf-libxml.h>
#include <gsf/gsf-input.h>
#include <gsf/gsf-output.h>
#include <gsf/gsf-input-gzip.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-utils.h>

#include <math.h>
#include <string.h>

static GObjectClass *parent_class;

/* Note: libxml erroneously declares the length argument as int.  */
static int
gsf_libxml_read (void *context, char *buffer, int len)
{
	gsf_off_t remaining = gsf_input_remaining ((GsfInput *)context);
	guint8* res;

	if (len > remaining)
		len = remaining;
	res = (guint8 *) gsf_input_read ((GsfInput *)context,
					 (size_t)len, buffer);
	if (res == NULL && len > 0) /* Not an error if len == 0 */
		return -1;
	return len;
}

static int
gsf_libxml_write (void *context, char const *buffer, int len)
{
	if (!gsf_output_write ((GsfOutput *)context, (size_t)len, buffer))
		return -1;
	return len;
}

static int
gsf_libxml_close (void *context)
{
	g_object_unref (G_OBJECT (context));
	return TRUE;
}

static xmlParserCtxtPtr
gsf_xml_parser_context_full (GsfInput *input, xmlSAXHandlerPtr sax, gpointer user)
{
	GsfInput *gzip;

	g_return_val_if_fail (GSF_IS_INPUT (input), NULL);

	gzip = gsf_input_gzip_new (input, NULL);
	if (gzip != NULL)
		input = gzip;
	else
		g_object_ref (G_OBJECT (input));

	return xmlCreateIOParserCtxt (
		sax, user,
		(xmlInputReadCallback) gsf_libxml_read, 
		(xmlInputCloseCallback) gsf_libxml_close,
		input, XML_CHAR_ENCODING_NONE);
}

/**
 * gsf_xml_parser_context :
 * @input :
 *
 * Create a libxml2 pull style parser context wrapper around a gsf input.
 * This signature will probably change to supply a SAX structure.
 *
 * NOTE : adds a reference to @input
 * NOTE : a simple wrapper around a cleaner implementation that will fold in
 *	 when we add other api changes.  Its not worth bumping just for this
 *
 * Returns : A parser context or NULL
 **/
xmlParserCtxtPtr
gsf_xml_parser_context (GsfInput *input)
{
	return gsf_xml_parser_context_full (input, NULL, NULL);
}

/**
 * gsf_xml_output_buffer_new :
 * @output :
 * @encoding : optionally NULL.
 *
 * NOTE : adds a reference to @output
 * NOTE : This is _not_ releated to GsfXMLOut
 */
static xmlOutputBufferPtr
gsf_xml_output_buffer_new (GsfOutput *output,
			   xmlCharEncodingHandlerPtr handler)
{
	xmlOutputBufferPtr res = xmlAllocOutputBuffer (handler);
	if (res != NULL) {
		g_object_ref (G_OBJECT (output));
		res->context = (void *)output;
		res->writecallback = gsf_libxml_write;
		res->closecallback = gsf_libxml_close;
	}

	return res;
}

int
gsf_xmlDocFormatDump (GsfOutput *output, xmlDocPtr cur, char const *encoding,
		      gboolean format)
{
	xmlOutputBufferPtr buf;
	xmlCharEncodingHandlerPtr handler = NULL;

	if (cur == NULL) {
#ifdef DEBUG_TREE
		xmlGenericError(xmlGenericErrorContext,
				"xmlDocDump : document == NULL\n");
#endif
		return(-1);
	}

	if (encoding != NULL) {
		xmlCharEncoding enc;

		enc = xmlParseCharEncoding(encoding);

		if (cur->charset != XML_CHAR_ENCODING_UTF8) {
			xmlGenericError(xmlGenericErrorContext,
					"xmlDocDump: document not in UTF8\n");
			return(-1);
		}
		if (enc != XML_CHAR_ENCODING_UTF8) {
			handler = xmlFindCharEncodingHandler(encoding);
			if (handler == NULL) {
				xmlFree((char *) cur->encoding);
				cur->encoding = NULL;
			}
		}
	}
	buf = gsf_xml_output_buffer_new (output, handler);
	return xmlSaveFormatFileTo (buf, cur, encoding, format);
}

/***************************************************************************/

struct _GsfXMLInDoc {
	GsfXMLInNode	*root;
	GsfXMLInNS	*ns;
	GPtrArray	*ns_by_id;
	GsfXMLInUnknownFunc	unknown_handler;
};

typedef struct {
	char    *tag;
	unsigned taglen;
	unsigned ref_count;
} GsfXMLInNSInstance;

typedef struct {
	GsfXMLInNS const *ns;
	GSList *elem;
} GsfXMLInNodeGroup;

static char const *
node_name (GsfXMLInNode const *node)
{
	return (node->name != NULL) ? node->name : "{catch all)}";
}

static void
gsf_xml_in_start_element (GsfXMLIn *state, xmlChar const *name, xmlChar const **attrs)
{
	GSList *ptr, *elem;
	GsfXMLInNSInstance *inst;
	GsfXMLInNodeGroup  *group;
	GsfXMLInNode	   *node;
	GsfXMLInNS const   *ns, *default_ns = state->default_ns;
	xmlChar const **ns_ptr;
	char const *tmp;
	int i;
	gboolean check_unknown_handler = TRUE;

	/* Scan for namespace declarations.  Yes it is ugly to have the api
	 * flag that its children can declare namespaces. However, given that a
	 * we need to know which namespace we are in before we can recognize
	 * the current node there is no choice.
	 * eg <gnm:Workbook xmlns:gnm="www.gnumeric.org"/> we can not know
	 * that we are in node 'Workbook' without recognizing ns=gnm, which we
	 * would not do unless we checked for a namespace */
	ns = state->doc->ns;
	if (ns != NULL && state->node->check_children_for_ns) {
		for (ns_ptr = attrs; ns_ptr != NULL && ns_ptr[0] && ns_ptr[1] ; ns_ptr += 2) {
			if (strncmp (*ns_ptr, "xmlns", 5))
				continue;
			if (ns_ptr[0][5] != '\0' && ns_ptr[0][5] != ':')
				continue;
			for (i = 0; (tmp = ns[i].uri) != NULL ; i++) {
				if (strcmp (tmp, ns_ptr[1]))
					continue;

				if (ns_ptr[0][5] == '\0') {
					default_ns = ns + i;
					break;
				}

				inst = g_hash_table_lookup (state->ns_prefixes, ns_ptr[0] + 6);
				if (inst == NULL) {
					inst = g_new0 (GsfXMLInNSInstance, 1);
					inst->tag    = g_strconcat (ns_ptr[0] + 6, ":", NULL);
					inst->taglen = strlen (inst->tag);
					inst->ref_count = 1;
					g_hash_table_insert (state->ns_prefixes, inst->tag, inst);

					if (ns[i].ns_id >= state->ns_by_id->len)
						g_ptr_array_set_size  (state->ns_by_id, ns[i].ns_id+1);
					if (g_ptr_array_index (state->ns_by_id, ns[i].ns_id)) {
						g_warning ("Damn.  Someone just declared the same namespace '%s' with a different prefix '%s'",
							   ns[i].uri, inst->tag);
					} else
						g_ptr_array_index (state->ns_by_id, ns[i].ns_id) = inst;
				} else
					inst->ref_count++;
				break;
			}
		}
	}

lookup_child :
	for (ptr = state->node->groups ; ptr != NULL ; ptr = ptr->next) {
		group = ptr->data;
		/* does the namespace match */
		if (group->ns != NULL && group->ns != default_ns) {
			g_return_if_fail (state->ns_by_id->len > group->ns->ns_id);
			inst = g_ptr_array_index (state->ns_by_id, group->ns->ns_id);
			if (inst == NULL || 0 != strncmp (name, inst->tag, inst->taglen))
				continue;
			tmp = name + inst->taglen;
		} else {
#if 0
			g_return_if_fail (state->ns_by_id->len > group->ns->ns_id);
			inst = g_ptr_array_index (state->ns_by_id, group->ns->ns_id);
			g_warning ("accepted ns = '%s' looking for '%s'", inst->tag, name);
#endif
			tmp = name;
		}
		for (elem = group->elem ; elem != NULL ; elem = elem->next) {
			node = elem->data;
			if (node->name == NULL || !strcmp (tmp, node->name)) {
				if (node->has_content == GSF_XML_CONTENT &&
				    state->content->len > 0) {
					g_warning ("too lazy to support nested unshared content for now.  We'll add it for 2.0");
				}
				state->state_stack = g_slist_prepend (state->state_stack,
								      (gpointer)state->node);
				state->ns_stack = g_slist_prepend (state->ns_stack,
								   (gpointer)state->default_ns);
				state->node = node;
				state->default_ns = default_ns;
				if (node->start != NULL)
					node->start (state, attrs);
				return;
			}
		}
	}

	if (check_unknown_handler) {
		check_unknown_handler = FALSE; /* only loop once */
		if (state->doc->unknown_handler != NULL &&
		    (state->doc->unknown_handler) (state, name, attrs))
			goto lookup_child;
	}
	if (state->unknown_depth++)
		return;
	g_warning ("Unexpected element '%s' in state %s.", name, node_name (state->node));
	{
		GSList *ptr;
		GsfXMLInNode *node;
		ptr = state->state_stack = g_slist_reverse (state->state_stack);
		for (;ptr != NULL && ptr->next != NULL; ptr = ptr->next) {
			node = ptr->data;
			if (node != NULL) {
/* FIXME FIXME FIXME if we really want this do we also want namespaces ? */
				g_print ("%s", node_name (node));
				if (ptr->next != NULL && ptr->next->data != NULL)
					g_print (" -> ");
			}
		}
		state->state_stack = g_slist_reverse (state->state_stack);
	}
}

static void
gsf_xml_in_end_element (GsfXMLIn *state,
			G_GNUC_UNUSED xmlChar const *name)
{
	if (state->unknown_depth > 0) {
		state->unknown_depth--;
		return;
	}

	g_return_if_fail (state->state_stack != NULL);
	g_return_if_fail (state->ns_stack != NULL);

	if (state->node->end)
		state->node->end (state, NULL);
	if (state->node->has_content == GSF_XML_CONTENT)
		g_string_truncate (state->content, 0);

	/* pop the state stack */
	state->node	   = state->state_stack->data;
	state->state_stack = g_slist_remove (state->state_stack, state->node);
	state->default_ns = state->ns_stack->data;
	state->ns_stack   = g_slist_remove (state->ns_stack, state->default_ns);
}

static void
gsf_xml_in_characters (GsfXMLIn *state, const xmlChar *chars, int len)
{
	if (state->node->has_content != GSF_XML_NO_CONTENT)
		g_string_append_len (state->content, chars, len);
}

static xmlEntityPtr
gsf_xml_in_get_entity (G_GNUC_UNUSED GsfXMLIn *state, const xmlChar *name)
{
	return xmlGetPredefinedEntity (name);
}

static void
gsf_xml_in_start_document (GsfXMLIn *state)
{
	state->node = state->doc->root;
	state->unknown_depth = 0;
	state->state_stack = NULL;
	state->ns_stack    = NULL;
	state->default_ns  = NULL;
	state->ns_by_id    = g_ptr_array_new ();
	state->ns_prefixes = g_hash_table_new_full (g_str_hash, g_str_equal,
		NULL, g_free);
}

static void
gsf_xml_in_end_document (GsfXMLIn *state)
{
	g_string_free (state->content, TRUE);
	state->content = NULL;

	g_return_if_fail (state->node == state->doc->root);
	g_return_if_fail (state->unknown_depth == 0);

	g_ptr_array_free (state->ns_by_id, TRUE);
	state->ns_by_id = NULL;

	g_hash_table_destroy (state->ns_prefixes);
	state->ns_prefixes = NULL;
}

static void
gsf_xml_in_warning (G_GNUC_UNUSED GsfXMLIn *state, char const *msg, ...)
{
	va_list args;

	va_start (args, msg);
	g_logv ("XML", G_LOG_LEVEL_WARNING, msg, args);
	va_end (args);
}

static void
gsf_xml_in_error (G_GNUC_UNUSED GsfXMLIn *state, char const *msg, ...)
{
	va_list args;

	va_start (args, msg);
	g_logv ("XML", G_LOG_LEVEL_CRITICAL, msg, args);
	va_end (args);
}

static void
gsf_xml_in_fatal_error (G_GNUC_UNUSED GsfXMLIn *state, char const *msg, ...)
{
	va_list args;

	va_start (args, msg);
	g_logv ("XML", G_LOG_LEVEL_ERROR, msg, args);
	va_end (args);
}

static xmlSAXHandler gsfXMLInParser = {
	NULL, /* internalSubset */
	NULL, /* isStandalone */
	NULL, /* hasInternalSubset */
	NULL, /* hasExternalSubset */
	NULL, /* resolveEntity */
	(getEntitySAXFunc)gsf_xml_in_get_entity, /* getEntity */
	NULL, /* entityDecl */
	NULL, /* notationDecl */
	NULL, /* attributeDecl */
	NULL, /* elementDecl */
	NULL, /* unparsedEntityDecl */
	NULL, /* setDocumentLocator */
	(startDocumentSAXFunc)gsf_xml_in_start_document, /* startDocument */
	(endDocumentSAXFunc)gsf_xml_in_end_document, /* endDocument */
	(startElementSAXFunc)gsf_xml_in_start_element, /* startElement */
	(endElementSAXFunc)gsf_xml_in_end_element, /* endElement */
	NULL, /* reference */
	(charactersSAXFunc)gsf_xml_in_characters, /* characters */
	NULL, /* ignorableWhitespace */
	NULL, /* processingInstruction */
	NULL, /* comment */
	(warningSAXFunc)gsf_xml_in_warning, /* warning */
	(errorSAXFunc)gsf_xml_in_error, /* error */
	(fatalErrorSAXFunc)gsf_xml_in_fatal_error, /* fatalError */
	NULL, /* getParameterEntity */
	NULL, /* cdataBlock */
	NULL, /* externalSubset */
	0
#if LIBXML_VERSION >= 20600
	,
	NULL, NULL, NULL
#if LIBXML_VERSION >= 20602
	,
	NULL /* serror */
#endif
#endif
};

/**
 * gsf_xml_in_doc_new :
 * @root : an array of node descriptors
 * @ns : an array of namespace identifiers
 *
 * Put the nodes in the NULL terminated array starting at @root and the name
 * spaces in the NULL terminated array starting at @ns together.  Link them up
 * and prepare the static data structures necessary to validate a doument based
 * on that description.
 *
 * Returns NULL on error
 **/
GsfXMLInDoc *
gsf_xml_in_doc_new (GsfXMLInNode *root, GsfXMLInNS *ns)
{
	GsfXMLInDoc  *doc;
	unsigned i;

	if (root->parent_initialized)
		return NULL;

	doc = g_new0 (GsfXMLInDoc, 1);
	doc->root     = root;
	doc->ns       = ns;
	doc->ns_by_id = g_ptr_array_new ();

	/* Add namespaces to an idex */
	if (ns != NULL) {
		for (i = 0; ns[i].uri != NULL ; i++) {
			if (ns[i].ns_id >= doc->ns_by_id->len)
				g_ptr_array_set_size  (doc->ns_by_id, ns[i].ns_id+1);
			g_ptr_array_index (doc->ns_by_id, ns[i].ns_id) = ns+i;
		}
	}
	gsf_xml_in_doc_extend (doc, root);
	return doc;
}

void
gsf_xml_in_doc_extend (GsfXMLInDoc  *doc,
		       GsfXMLInNode *extension_nodes)
{
	GsfXMLInNode *tmp, *real_node, *node;
	GHashTable   *symbols = g_hash_table_new (g_str_hash, g_str_equal);

	g_return_if_fail (doc != NULL);
	g_return_if_fail (extension_nodes != NULL);

	if (extension_nodes->parent_initialized)
		return;
	for (node = extension_nodes; node->id != NULL ; node++ ) {
		g_return_if_fail (!node->parent_initialized);

		tmp = g_hash_table_lookup (symbols, node->id);
		if (tmp != NULL) {
			/* if its empty then this is just a recusion */
			if (node->start != NULL || node->end != NULL ||
			    node->has_content != GSF_XML_NO_CONTENT || node->user_data.v_int != 0) {
				g_warning ("ID '%s' has already been registered.\n"
					   "The additional decls should not specify start,end,content,data", node->id);
				return;
			}
			real_node = tmp;
		} else {
			/* be anal, the macro probably initialized this, but do it just in case */
			node->groups = NULL;
			g_hash_table_insert (symbols, (gpointer)node->id, node);
			real_node = node;
		}

		tmp = g_hash_table_lookup (symbols, node->parent_id);
		if (tmp != NULL) {
			GSList *ptr;
			GsfXMLInNodeGroup *group = NULL;
			GsfXMLInNS const *ns = NULL;
			
			ns = (real_node->ns_id < 0) ? NULL
				: g_ptr_array_index (doc->ns_by_id,
						     real_node->ns_id);
			for (ptr = tmp->groups; ptr != NULL ; ptr = ptr->next) {
				group = ptr->data;
				if (group->ns == ns)
					break;
			}
			if (ptr == NULL) {
				group = g_new0 (GsfXMLInNodeGroup, 1);
				group->ns = ns;
				tmp->groups = g_slist_prepend (tmp->groups, group);
			}
			group->elem = g_slist_prepend (group->elem, real_node);
		} else if (strcmp (node->id, node->parent_id)) {
			g_warning ("Parent ID '%s' unknown", node->parent_id);
			return;
		}

		/* WARNING VILE HACK :
		 * The api in 1.8.2 passed has_content as a boolean.  It's too
		 * late to change it but we need more contol.  We edit the bool
		 * here to be GSF_CONTENT_NONE, GSF_CONTENT_ROOT and add a
		 * mechanism to edit the flag later */
		if (node->has_content != 0 &&
		    node->has_content != GSF_XML_SHARED_CONTENT)
			node->has_content = GSF_XML_CONTENT;

		node->parent_initialized = TRUE;
	}

	g_hash_table_destroy (symbols);
}

/**
 * gsf_xml_in_doc_set_unknown_handler:
 * @doc : #GsfXMLInDoc
 * @handler : The function to call
 *
 * Call the function @handler when an unexpected child node is found
 **/
void
gsf_xml_in_doc_set_unknown_handler (GsfXMLInDoc *doc,
				    GsfXMLInUnknownFunc handler)
{
	g_return_if_fail (doc != NULL);
	doc->unknown_handler = handler;
}

/**
 * gsf_xml_in_doc_free :
 * @doc :
 *
 * Free up resources
 **/
void
gsf_xml_in_doc_free (GsfXMLInDoc *doc)
{
	GSList *ptr;
	GsfXMLInNode *node;
	GsfXMLInNodeGroup *group;

	g_return_if_fail (doc != NULL);
	g_return_if_fail (doc->root != NULL);
	g_return_if_fail (doc->ns_by_id != NULL);

	for (node = doc->root; node->id != NULL ; node++ ) {
		for (ptr = node->groups; ptr != NULL ; ptr = ptr->next) {
			group = ptr->data;
			g_slist_free (group->elem);
			g_free (group);
		}
		g_slist_free (node->groups);
		node->groups = NULL;
	}

	g_ptr_array_free (doc->ns_by_id, TRUE);
	g_free (doc);
}

/**
 * gsf_xml_in_parse :
 * @state :
 * @input :
 *
 * Read an xml document from @input and parse based on the the descriptor in
 * @state::doc
 *
 * returns FALSE on error
 **/
gboolean
gsf_xml_in_parse (GsfXMLIn *state, GsfInput *input)
{
	xmlParserCtxt *ctxt;
	gboolean res;

	g_return_val_if_fail (state != NULL, FALSE);
	g_return_val_if_fail (state->doc != NULL, FALSE);
	g_return_val_if_fail (GSF_IS_INPUT (input), FALSE);

	ctxt = gsf_xml_parser_context_full (input, &gsfXMLInParser, state);

	g_return_val_if_fail (ctxt != NULL, FALSE);

	state->content = g_string_sized_new (128);
	xmlParseDocument (ctxt);
	res = ctxt->wellFormed;
	xmlFreeParserCtxt (ctxt);

	return res;
}

/**
 * gsf_xml_in_check_ns :
 * @state :
 * @str :
 * @ns_id :
 * 
 * According to @state is @str in the namespace @ns_id ?
 *
 * Returns a pointer to @str after the namespace if successful,
 * otherwise NULL.
 **/
char const *
gsf_xml_in_check_ns (GsfXMLIn const *state, char const *str, unsigned int ns_id)
{
	GsfXMLInNSInstance *inst;
	if (state->ns_by_id->len <= ns_id)
		return NULL;
	if (NULL == (inst = g_ptr_array_index (state->ns_by_id, ns_id)))
		return NULL;
	if (strncmp (str, inst->tag, inst->taglen))
		return NULL;
	return str + inst->taglen;
}

/**
 * gsf_xml_in_namecmp :
 * @state : The #GsfXMLIn we are reading from.
 * @str   : The potentially namespace qualified node name.
 * @ns_id : The name space id to check
 * @name  : The target node name
 *
 * Returns TRUE if @str == @ns_id:@name according to @state.
 **/
gboolean
gsf_xml_in_namecmp (GsfXMLIn const *state, char const *str,
		    unsigned int ns_id, char const *name)
{
	GsfXMLInNSInstance *inst;
	if (state->ns_by_id->len <= ns_id)
		return FALSE;
	if (NULL == (inst = g_ptr_array_index (state->ns_by_id, ns_id)))
		return FALSE;
	if (strncmp (str, inst->tag, inst->taglen))
		return FALSE;
	return 0 == strcmp (name, str + inst->taglen);
}

/****************************************************************************/

typedef enum {
	GSF_XML_OUT_NOCONTENT,
	GSF_XML_OUT_CHILD,
	GSF_XML_OUT_CONTENT
} GsfXMLOutState;

struct _GsfXMLOut {
	GObject	   base;

	GsfOutput	 *output;
	char		 *doc_type;
	GSList		 *stack;
	GsfXMLOutState state;
	unsigned   	  indent;
	gboolean	  needs_header;
};

typedef struct {
	GObjectClass  base;
} GsfXMLOutClass;

static void
gsf_xml_out_finalize (GObject *obj)
{
	GsfXMLOut *xml = GSF_XML_OUT (obj);

	g_free (xml->doc_type);

	parent_class->finalize (obj);
}

static void
gsf_xml_out_init (GObject *obj)
{
	GsfXMLOut *xml = GSF_XML_OUT (obj);
	xml->output = NULL;
	xml->stack  = NULL;
	xml->state  = GSF_XML_OUT_CHILD;
	xml->indent = 0;
	xml->needs_header = TRUE;
	xml->doc_type = NULL;
}

static void
gsf_xml_out_class_init (GObjectClass *gobject_class)
{
	gobject_class->finalize = gsf_xml_out_finalize;
	parent_class = g_type_class_peek_parent (gobject_class);
}

GSF_CLASS (GsfXMLOut, gsf_xml_out,
	   gsf_xml_out_class_init, gsf_xml_out_init,
	   G_TYPE_OBJECT)

GsfXMLOut *
gsf_xml_out_new (GsfOutput *output)
{
	GsfXMLOut *xml = g_object_new (GSF_XML_OUT_TYPE, NULL);
	if (!gsf_output_wrap (G_OBJECT (xml), output))
		return NULL;
	xml->output = output;
	return xml;
}

/**
 * gsf_xml_out_set_doc_type :
 * @xml : #GsfXMLOut
 * @type :
 * 
 * Store some optional some &lt;!DOCTYPE .. &gt; content
 **/
void
gsf_xml_out_set_doc_type (GsfXMLOut *xml, char const *type)
{
	g_free (xml->doc_type);
	xml->doc_type = g_strdup (type);
}

static inline void
gsf_xml_out_indent (GsfXMLOut *xml)
{
	static char const spaces [] =
		"                                        "
		"                                        "
		"                                        "
		"                                        "
		"                                        "
		"                                        ";
	unsigned i;
	for (i = xml->indent ; i > (sizeof (spaces)/2) ; i -= sizeof (spaces)/2)
		gsf_output_write (xml->output, sizeof (spaces) - 1, spaces);
	gsf_output_write (xml->output, i*2, spaces);
}

/**
 * gsf_xml_out_start_elem :
 * @xml :
 * @id  :
 */
void
gsf_xml_out_start_element (GsfXMLOut *xml, char const *id)
{
	g_return_if_fail (id != NULL);
	g_return_if_fail (xml != NULL);
	g_return_if_fail (xml->state != GSF_XML_OUT_CONTENT);

	if (xml->needs_header) {
		static char const header0[] =
			"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
		gsf_output_write (xml->output, sizeof (header0) - 1, header0);
		if (xml->doc_type != NULL)
			gsf_output_puts (xml->output, xml->doc_type);
		xml->needs_header = FALSE;
	}
	if (xml->state == GSF_XML_OUT_NOCONTENT)
		gsf_output_write (xml->output, 2, ">\n");

	gsf_xml_out_indent (xml);
	gsf_output_printf (xml->output, "<%s", id);

	xml->stack = g_slist_prepend (xml->stack, (gpointer)id);
	xml->indent++;
	xml->state = GSF_XML_OUT_NOCONTENT;
}

/**
 * gsf_xml_out_end_elem :
 * @xml :
 */
char const *
gsf_xml_out_end_element (GsfXMLOut *xml)
{
	char const *id;

	g_return_val_if_fail (xml != NULL, NULL);
	g_return_val_if_fail (xml->stack != NULL, NULL);

	id = xml->stack->data;
	xml->stack = g_slist_remove (xml->stack, id);
	xml->indent--;
	switch (xml->state) {
	case GSF_XML_OUT_NOCONTENT :
		gsf_output_write (xml->output, 3, "/>\n");
		break;

	case GSF_XML_OUT_CHILD :
		gsf_xml_out_indent (xml);
	/* fall through */
	case GSF_XML_OUT_CONTENT :
		gsf_output_printf (xml->output, "</%s>\n", id);
	}
	xml->state = GSF_XML_OUT_CHILD;
	return id;
}

/**
 * gsf_xml_out_simple_element :
 * @xml :
 * @id  :
 * @content :
 *
 * A convenience routine
 **/
void
gsf_xml_out_simple_element (GsfXMLOut *xml, char const *id,
			    char const *content)
{
	gsf_xml_out_start_element (xml, id);
	if (content != NULL)
		gsf_xml_out_add_cstr (xml, NULL, content);
	gsf_xml_out_end_element (xml);
}

/**
 * gsf_xml_out_simple_int_element :
 * @xml :
 * @id  :
 * @val :
 *
 * A convenience routine
 **/
void
gsf_xml_out_simple_int_element (GsfXMLOut *xml, char const *id, int val)
{
	gsf_xml_out_start_element (xml, id);
	gsf_xml_out_add_int (xml, NULL, val);
	gsf_xml_out_end_element (xml);
}

/**
 * gsf_xml_out_simple_float_element :
 * @xml :
 * @id  :
 * @val :
 * @precision :
 *
 * A convenience routine
 **/
void
gsf_xml_out_simple_float_element (GsfXMLOut *xml, char const *id,
				  double val, int precision)
{
	gsf_xml_out_start_element (xml, id);
	gsf_xml_out_add_float (xml, NULL, val, precision);
	gsf_xml_out_end_element (xml);
}

static void
close_tag_if_neccessary (GsfXMLOut* xml)
{
	if (xml->state != GSF_XML_OUT_CONTENT) {
		xml->state = GSF_XML_OUT_CONTENT;
		gsf_output_write (xml->output, 1, ">");
	}
}

/**
 * gsf_xml_out_add_cstr_unchecked :
 * @xml :
 * @id : optionally NULL for content
 * @val_utf8 : a utf8 encoded string to export
 *
 * dump @val_utf8 to an attribute named @id without checking to see if the
 * content needs escaping.  A useful performance enhancement when the
 * application knows that structure of the content well.  If @val_utf8 is NULL
 * do nothing (no warning, no output)
 **/
void
gsf_xml_out_add_cstr_unchecked (GsfXMLOut *xml, char const *id,
				char const *val_utf8)
{
	g_return_if_fail (xml != NULL);

	if (val_utf8 == NULL)
		return;

	if (id == NULL) {
		close_tag_if_neccessary (xml);
		gsf_output_write (xml->output, strlen (val_utf8), val_utf8);
	} else
		gsf_output_printf (xml->output, " %s=\"%s\"", id, val_utf8);
}

/**
 * gsf_xml_out_add_cstr :
 * @xml :
 * @id : optionally NULL for content
 * @val_utf8 : a utf8 encoded string
 *
 * dump @val_utf8 to an attribute named @id or as the nodes content escaping
 * characters as necessary.  If @val_utf8 is NULL do nothing (no warning, no
 * output)
 **/
void
gsf_xml_out_add_cstr (GsfXMLOut *xml, char const *id,
		      char const *val_utf8)
{
	guint8 const *cur   = val_utf8;
	guint8 const *start = val_utf8;

	g_return_if_fail (xml != NULL);

	if (val_utf8 == NULL)
		return;

	if (id == NULL) {
		close_tag_if_neccessary (xml);
	} else
		gsf_output_printf (xml->output, " %s=\"", id);
	while (*cur != '\0') {
		if (*cur == '<') {
			if (cur != start)
				gsf_output_write (xml->output, cur-start, start);
			start = ++cur;
			gsf_output_write (xml->output, 4, "&lt;");
		} else if (*cur == '>') {
			if (cur != start)
				gsf_output_write (xml->output, cur-start, start);
			start = ++cur;
			gsf_output_write (xml->output, 4, "&gt;");
		} else if (*cur == '&') {
			if (cur != start)
				gsf_output_write (xml->output, cur-start, start);
			start = ++cur;
			gsf_output_write (xml->output, 5, "&amp;");
		} else if (*cur == '"') {
			if (cur != start)
				gsf_output_write (xml->output, cur-start, start);
			start = ++cur;
			gsf_output_write (xml->output, 6, "&quot;");
		} else if (*cur < 0x20 && id != NULL) {
			guint8 buf[8];
			sprintf (buf, "&#%d;", *cur);

			if (cur != start)
				gsf_output_write (xml->output, cur-start, start);
			start = ++cur;

			gsf_output_write (xml->output, strlen (buf), buf);
		} else if (((*cur >= 0x20) && (*cur != 0x7f)) ||
			   (*cur == '\n') || (*cur == '\r') || (*cur == '\t')) {
			cur++;
		} else {
			g_warning ("Unknown char 0x%hhx in string", *cur);
			cur++;
		}
	}
	if (cur != start)
		gsf_output_write (xml->output, cur-start, start);
	if (id != NULL)
		gsf_output_write (xml->output, 1, "\"");
}

/**
 * gsf_xml_out_add_bool :
 * @xml :
 * @id  : optionally NULL for content
 * @val : a boolean
 *
 * dump boolean value @val to an attribute named @id or as the nodes content
 * Use '1' or '0' to simplify import
 **/
void
gsf_xml_out_add_bool (GsfXMLOut *xml, char const *id,
		      gboolean val)
{
	gsf_xml_out_add_cstr_unchecked (xml, id,
					val ? "1" : "0");
}

/**
 * gsf_xml_out_add_int :
 * @xml :
 * @id  : optionally NULL for content
 * @val : the value
 *
 * dump integer value @val to an attribute named @id or as the nodes content
 **/
void
gsf_xml_out_add_int (GsfXMLOut *xml, char const *id,
		     int val)
{
	char buf [4 * sizeof (int)];
	sprintf (buf, "%d", val);
	gsf_xml_out_add_cstr_unchecked (xml, id, buf);
}

/**
 * gsf_xml_out_add_uint :
 * @xml :
 * @id  : optionally NULL for content
 * @val : the value
 *
 * dump unsigned integer value @val to an attribute named @id or as the nodes
 * content
 **/
void
gsf_xml_out_add_uint (GsfXMLOut *xml, char const *id,
		      unsigned int val)
{
	char buf [4 * sizeof (unsigned int)];
	sprintf (buf, "%u", val);
	gsf_xml_out_add_cstr_unchecked (xml, id, buf);
}

/**
 * gsf_xml_out_add_float :
 * @xml :
 * @id  : optionally NULL for content
 * @val : the value
 * @precision : the number of significant digits to use, -1 meaning "enough".
 *
 * dump float value @val to an attribute named @id or as the nodes
 * content with precision @precision.  The number will be formattted
 * according to the "C" locale.
 **/
void
gsf_xml_out_add_float (GsfXMLOut *xml, char const *id,
		       double val, int precision)
{
	char format_str[4 * sizeof (int) + 10];
	char buf[G_ASCII_DTOSTR_BUF_SIZE + DBL_DIG];

	if (precision < 0 || precision > DBL_DIG)
		precision = DBL_DIG;

	sprintf (format_str, "%%.%dg", precision);
	g_ascii_formatd (buf, sizeof (buf), format_str, val);
	gsf_xml_out_add_cstr_unchecked (xml, id, buf);
}

/**
 * gsf_xml_out_add_color :
 * @xml :
 * @id  : optionally NULL for content
 * @r :
 * @g :
 * @b :
 *
 * dump Color @r.@g.@b to an attribute named @id or as the nodes content
 **/
void
gsf_xml_out_add_color (GsfXMLOut *xml, char const *id,
		       unsigned int r, unsigned int g, unsigned int b)
{
	char buf [3 * 4 * sizeof (unsigned int) + 1];
	sprintf (buf, "%X:%X:%X", r, g, b);
	gsf_xml_out_add_cstr_unchecked (xml, id, buf);
}

void
gsf_xml_out_add_enum (GsfXMLOut *xml, char const *id, GType etype, gint val)
{
	GEnumClass *eclass = G_ENUM_CLASS (g_type_class_peek (etype));
	GEnumValue *ev = g_enum_get_value (eclass, val);

	if (ev)
		gsf_xml_out_add_cstr_unchecked (xml, id, ev->value_name);
	else
		g_warning ("Invalid value %d for type %s",
			   val, g_type_name (etype));
}

/**
 * gsf_xml_out_add_base64 :
 * @xml :
 * @id  : optionally NULL for content
 * @data :
 * @len :
 *
 * dump @len bytes in @data into the content of node @id using base64
 **/
void
gsf_xml_out_add_base64 (GsfXMLOut *xml, char const *id,
			guint8 const *data, unsigned int len)
{
	/* We could optimize and stream right to the output,
	 * or even just keep the buffer around
	 * for now we start simple */
	guint8 *tmp = gsf_base64_encode_simple (data, len);
	if (tmp == NULL)
		return;
	if (id != NULL)
		g_warning ("Stream a binary blob into an attribute ??");
	gsf_xml_out_add_cstr_unchecked (xml, id, tmp);
	g_free (tmp);
}
