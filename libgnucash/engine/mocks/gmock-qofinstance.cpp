#include <glib.h>

#include <gmock/gmock.h>

#include <qofinstance.h>
#include <qofinstance-p.h>


G_DEFINE_TYPE(QofInstance, qof_instance, G_TYPE_OBJECT)

static void
qof_instance_init (QofInstance *inst)
{
    // function is unused, initialization is done in the constructor of the derived mock class
}

static void
qof_instance_class_init(QofInstanceClass *klass)
{
    // function is unused, class functions are defined in C++ code
}

// This is a reimplementation of the function from qofinstance.cpp
void
qof_instance_get (const QofInstance *inst, const gchar *first_prop, ...)
{
    va_list ap;
    ASSERT_TRUE (QOF_IS_INSTANCE (inst));

    va_start (ap, first_prop);
    g_object_get_valist (G_OBJECT (inst), first_prop, ap);
    va_end (ap);
}

// This is a reimplementation of the function from qofinstance.cpp
// without calling qof_instance_set_dirty()
void
qof_instance_set (QofInstance *inst, const gchar *first_prop, ...)
{
    va_list ap;
    ASSERT_TRUE (QOF_IS_INSTANCE (inst));

    va_start (ap, first_prop);
    g_object_set_valist (G_OBJECT (inst), first_prop, ap);
    va_end (ap);
}

