#ifndef __IO_EXAMPLE_ACCOUNT_H__
#define __IO_EXAMPLE_ACCOUNT_H__

#include <glib.h>

#include "sixtp.h"
#include "Group.h"

struct GncExampleAccount_struct
{
    gchar *title;
    gchar *filename;
    AccountGroup *group;
    gchar *short_description;
    gchar *long_description;
};
typedef struct GncExampleAccount_struct GncExampleAccount;

GncExampleAccount* gnc_create_example_account(
    AccountGroup *grp, const char *title, const char *filename,
    const char *short_descrip, const char *long_descrip);

void gnc_destroy_example_account(GncExampleAccount *gea);

gboolean gnc_write_example_account(GncExampleAccount *gea,
                                   const gchar *filename);
GncExampleAccount *gnc_read_example_account(const gchar *filename);


gboolean gnc_is_xml_data_file_v2(const gchar *filename);

void gnc_free_example_account_list(GSList *list);
GSList* gnc_load_example_account_list(const char *dirname);

gboolean gnc_is_example_account_xml(const gchar *name);

#endif /* __IO_EXAMPLE_ACCOUNT_H__ */
