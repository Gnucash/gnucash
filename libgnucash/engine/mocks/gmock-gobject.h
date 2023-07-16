#ifndef GMOCK_GOBJECT_H
#define GMOCK_GOBJECT_H

#include <glib.h>
#include <glib-object.h>

static gpointer
mock_g_object_new (GType object_type, const gchar *first_property_name, size_t size)
{
    GTypeQuery query;

    g_type_query(object_type, &query);
    g_assert_true (size == query.instance_size);
    return g_object_new (object_type, first_property_name);
}

static void
mock_g_object_unref (gpointer object, size_t size)
{
    GType      object_type = G_OBJECT_TYPE(object);
    GTypeQuery query;

    g_type_query(object_type, &query);
    g_assert_true (size == query.instance_size);
    g_object_unref(object);
}

#endif
