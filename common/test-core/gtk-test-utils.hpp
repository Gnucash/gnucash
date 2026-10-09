/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#ifndef GNC_GTK_TEST_UTILS_HPP
#define GNC_GTK_TEST_UTILS_HPP

#include <gtk/gtk.h>

namespace gnc::test
{
/** Find a widget by its GtkBuilder name below root.
 *
 * The returned widget is borrowed from the widget tree. The caller must keep
 * the tree alive while using it.
 */
inline GtkWidget *
find_widget_by_buildable_name (GtkWidget *root, const char *name)
{
    if (GTK_IS_BUILDABLE (root) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (root)), name) == 0)
        return root;
    if (!GTK_IS_CONTAINER (root))
        return nullptr;

    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_widget_by_buildable_name (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return found;
}
}
#endif
