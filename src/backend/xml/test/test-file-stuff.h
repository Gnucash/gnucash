/* This file declares testing functions for xml files.
 *
 */
/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/


#ifndef TEST_FILE_STUFF_H
#define TEST_FILE_STUFF_H
extern "C"
{
#include <glib.h>

#include <gnc-commodity.h>
#include <gnc-engine.h>
}
#include <gnc-xml-helper.h>
#include <io-gncxml-gen.h>
#include <sixtp.h>

#ifndef __KVP_FRAME
typedef struct KvpFrameImpl KvpFrame;
#define __KVP_FRAME
#endif

void write_dom_node_to_file (xmlNodePtr node, int fd);

int files_compare (const gchar* f1, const gchar* f2);

gboolean print_dom_tree (gpointer data_for_children,
                         GSList* data_from_children,
                         GSList* sibling_data, gpointer parent_data,
                         gpointer global_data, gpointer* result,
                         const gchar* tag);

/**/
gboolean check_dom_tree_version (xmlNodePtr node,  const char* verstr);
gboolean equals_node_val_vs_string (xmlNodePtr node, const gchar* str);
gboolean equals_node_val_vs_guid (xmlNodePtr node, const GncGUID* id);
gboolean equals_node_val_vs_commodity (xmlNodePtr node,
                                       const gnc_commodity* com, QofBook*);
gboolean equals_node_val_vs_kvp_frame (xmlNodePtr node, const KvpFrame* frm);
gboolean equals_node_val_vs_date (xmlNodePtr node, const Timespec tm);
gboolean equals_node_val_vs_int (xmlNodePtr node, gint64 val);
gboolean equals_node_val_vs_boolean (xmlNodePtr node, gboolean val);

void
test_files_in_dir (int argc, char** argv, gxpf_callback cb,
                   sixtp* parser, const char* parser_tag,
                   QofBook* book);
#endif
