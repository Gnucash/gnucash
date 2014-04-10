#ifndef GNUMERIC_XML_IO_H
#define GNUMERIC_XML_IO_H

#include <gdk/gdktypes.h>
#include "gnumeric.h"
#include "xml-io-version.h"
//#include "file.h"
#include <gsf/gsf-libxml.h>
#include <libxml/tree.h>
#include <libxml/xmlmemory.h>
#include <goffice/utils/goffice-utils.h>

struct _XmlParseContext {
	xmlDocPtr doc;		/* Xml document */
	xmlNsPtr  ns;		/* Main name space */

	Sheet	     *sheet;	/* the associated sheet */
	Workbook     *wb;	/* the associated workbook */
	WorkbookView *wb_view;
	IOContext    *io_context;

	GHashTable *style_table;/* old style styles compatibility */
	GHashTable *expr_map;	/*
				 * Emitted expressions with ref count > 1
				 * When writing this is map from expr pointer -> index
				 */
	GPtrArray *shared_exprs;/*
				 * When reading this is a map from index -> expr pointer
				 */
	GnumericXMLVersion    version;

	GnmExprConventions *exprconv;
};

XmlParseContext *xml_parse_ctx_new     (xmlDoc		*doc,
				        xmlNs		*ns,
				        WorkbookView	*wb_view);
void		 xml_parse_ctx_destroy (XmlParseContext *ctxt);


xmlNodePtr   xml_write_style    (XmlParseContext *ctxt, GnmStyle *style);

xmlChar	   *xml_cellregion_write (WorkbookControl *context,
				  GnmCellRegion *cr, int *size);
GnmCellRegion *xml_cellregion_read  (WorkbookControl *context, Sheet *sheet,
				  guchar *buffer, int length);

/* Some utility routines for setting attributes or content */
xmlChar   *xml_node_get_cstr	(xmlNodePtr node, char const *name);
void	   xml_node_set_cstr	(xmlNodePtr node, char const *name, char const *val);
gboolean   xml_node_get_bool	(xmlNodePtr node, char const *name, gboolean *result);
void       xml_node_set_bool	(xmlNodePtr node, char const *name, gboolean val);
gboolean   xml_node_get_int	(xmlNodePtr node, char const *name, int *result);
void       xml_node_set_int	(xmlNodePtr node, char const *name, int  val);
gboolean   xml_node_get_double	(xmlNodePtr node, char const *name, double *result);
void       xml_node_set_double	(xmlNodePtr node, char const *name, double  val, int precision);
gboolean   xml_node_get_gocolor (xmlNodePtr node, char const *name, GOColor *result);
void	   xml_node_set_gocolor (xmlNodePtr node, char const *name, GOColor  val);
GnmColor  *xml_node_get_color	(xmlNodePtr node, char const *name);
void       xml_node_set_color	(xmlNodePtr node, char const *name, GnmColor const *color);

xmlNodePtr   xml_write_style    (XmlParseContext *ctxt, GnmStyle *style);
GnmStyle      *xml_read_style     (XmlParseContext *ctxt, xmlNodePtr tree);

void      xml_init (void);

xmlNode *e_xml_get_child_by_name	 (xmlNode const *tree, char const *name);
xmlNode *e_xml_get_child_by_name_no_lang (xmlNode const *tree, char const *name);
xmlNode *e_xml_get_child_by_name_by_lang (xmlNode const *tree, char const *name);

/* Gnumeric specific SAX utilities */
void gnm_xml_out_add_color   (GsfXMLOut *o, char const *id, GnmColor const *c);
void gnm_xml_out_add_gocolor (GsfXMLOut *o, char const *id, GOColor c);
void gnm_xml_out_add_cellpos (GsfXMLOut *o, char const *id, GnmCellPos const *p);

#endif /* GNUMERIC_XML_IO_H */
