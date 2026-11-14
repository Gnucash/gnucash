/* Tests for object reference display-name snapshots.
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include "qofinstance.h"
#include "dialog-object-references.h"

namespace
{
struct NamedInstance
{
    QofInstance parent;
    const char *name;
};
struct NamedInstanceClass { QofInstanceClass parent; };
G_DEFINE_TYPE (NamedInstance, named_instance, QOF_TYPE_INSTANCE)

static gchar *
named_instance_display_name (const QofInstance *instance)
{
    return g_strdup (reinterpret_cast<const NamedInstance*> (instance)->name);
}

static void
named_instance_class_init (NamedInstanceClass *klass)
{
    QOF_INSTANCE_CLASS (klass)->get_display_name = named_instance_display_name;
}

static void
named_instance_init ([[maybe_unused]] NamedInstance *instance)
{
}

class ObjectReferencesTest : public ::testing::Test
{
protected:
    GList *objects = nullptr;
    GStrv names = nullptr;

    void SetUp () override
    {
        for (const auto name : {"Invoice B", "Rechnung Ä", "Invoice B"})
        {
            auto object = static_cast<NamedInstance*> (g_object_new (named_instance_get_type (), nullptr));
            object->name = name;
            objects = g_list_append (objects, object);
        }
    }
    void clear_objects ()
    {
        g_list_free_full (objects, g_object_unref);
        objects = nullptr;
    }
    void TearDown () override
    {
        clear_objects ();
        g_strfreev (names);
    }
};

TEST_F (ObjectReferencesTest, EmptyList)
{
    names = gnc_object_references_get_display_names (nullptr);
    ASSERT_NE (names, nullptr);
    EXPECT_EQ (names[0], nullptr);
}

TEST_F (ObjectReferencesTest, OrderedNames)
{
    names = gnc_object_references_get_display_names (objects);
    ASSERT_NE (names, nullptr);
    ASSERT_EQ (g_strv_length (names), 3u);
    EXPECT_STREQ (names[0], "Invoice B");
    EXPECT_STREQ (names[1], "Rechnung Ä");
    EXPECT_STREQ (names[2], "Invoice B");
}

TEST_F (ObjectReferencesTest, IndependentSnapshot)
{
    names = gnc_object_references_get_display_names (objects);
    clear_objects ();
    ASSERT_NE (names, nullptr);
    ASSERT_EQ (g_strv_length (names), 3u);
    EXPECT_STREQ (names[0], "Invoice B");
    EXPECT_STREQ (names[1], "Rechnung Ä");
    EXPECT_STREQ (names[2], "Invoice B");
}
}
