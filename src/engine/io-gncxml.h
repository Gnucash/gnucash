/*
 * io-gncxml.h -- api for new XML-based file format
 */
#ifndef __IO_GNCXML_H__
#define __IO_GNCXML_H__

#include <glib.h>

#include "Group.h"

gboolean gncxml_read(const gchar *name, AccountGroup **result_group);
gboolean gncxml_write(AccountGroup *group, const gchar *name);
gboolean is_gncxml_file(const gchar *name);

#endif
