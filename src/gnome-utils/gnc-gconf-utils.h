#include <gconf/gconf-client.h>

char * gnc_gconf_section_name (const char *name);

gboolean
gnc_gconf_client_get_bool (GConfClient *client,
			   gchar *key,
			   const gchar *section,
			   const gchar *name,
			   gboolean *value);

void gnc_gconf_client_set_bool (GConfClient *client,
				gchar *key,
				const gchar *section,
				const gchar *name,
				const gboolean value);

gboolean
gnc_gconf_client_get_int (GConfClient *client,
			  gchar *key,
			  const gchar *section,
			  const gchar *name,
			  gint *value);

void gnc_gconf_client_set_int (GConfClient *client,
			       gchar *key,
			       const gchar *section,
			       const gchar *name,
			       const gint value);

gboolean gnc_gconf_client_get_string (GConfClient *client,
				      gchar *key,
				      const gchar *section,
				      const gchar *name,
				      gchar **value);
void gnc_gconf_client_set_string (GConfClient *client,
				  gchar *key,
				  const gchar *section,
				  const gchar *name,
				  const gchar *value);
