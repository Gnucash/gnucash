/*
 *  Copyright (C) 2002 Derek Atkins
 *
 *  Authors: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <sys/types.h>
#include <regex.h>
#include <gnome.h>

#include "search-string.h"
#include "QueryCore.h"

#define d(x)

static void editable_enters (GNCSearchCoreType *fe, GnomeDialog *dialog);
static void grab_focus (GNCSearchCoreType *fe);
static GNCSearchCoreType *clone(GNCSearchCoreType *fe);
static gboolean validate (GNCSearchCoreType *fe);
static GtkWidget *get_widget(GNCSearchCoreType *fe);
static QueryPredData_t get_predicate (GNCSearchCoreType *fe);

static void gnc_search_string_class_init	(GNCSearchStringClass *class);
static void gnc_search_string_init	(GNCSearchString *gspaper);
static void gnc_search_string_finalise	(GtkObject *obj);

#define _PRIVATE(x) (((GNCSearchString *)(x))->priv)

struct _GNCSearchStringPrivate {
  GtkWidget *entry;
};

static GNCSearchCoreTypeClass *parent_class;

enum {
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

guint
gnc_search_string_get_type (void)
{
  static guint type = 0;
	
  if (!type) {
    GtkTypeInfo type_info = {
      "GNCSearchString",
      sizeof(GNCSearchString),
      sizeof(GNCSearchStringClass),
      (GtkClassInitFunc)gnc_search_string_class_init,
      (GtkObjectInitFunc)gnc_search_string_init,
      (GtkArgSetFunc)NULL,
      (GtkArgGetFunc)NULL
    };
		
    type = gtk_type_unique(gnc_search_core_type_get_type (), &type_info);
  }
	
  return type;
}

static void
gnc_search_string_class_init (GNCSearchStringClass *class)
{
  GtkObjectClass *object_class;
  GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)class;

  object_class = (GtkObjectClass *)class;
  parent_class = gtk_type_class(gnc_search_core_type_get_type ());

  object_class->finalize = gnc_search_string_finalise;

  /* override methods */
  gnc_search_core_type->editable_enters = editable_enters;
  gnc_search_core_type->grab_focus = grab_focus;
  gnc_search_core_type->validate = validate;
  gnc_search_core_type->get_widget = get_widget;
  gnc_search_core_type->get_predicate = get_predicate;
  gnc_search_core_type->clone = clone;

  /* signals */

  gtk_object_class_add_signals(object_class, signals, LAST_SIGNAL);
}

static void
gnc_search_string_init (GNCSearchString *o)
{
  o->priv = g_malloc0 (sizeof (*o->priv));
  o->value = NULL;
  o->how = SEARCH_STRING_CONTAINS;
  o->ign_case = TRUE;
}

static void
gnc_search_string_finalise (GtkObject *obj)
{
  GNCSearchString *o = (GNCSearchString *)obj;
  g_assert (IS_GNCSEARCH_STRING (o));

  g_free (o->value);
  g_free(o->priv);
	
  ((GtkObjectClass *)(parent_class))->finalize(obj);
}

/**
 * gnc_search_string_new:
 *
 * Create a new GNCSearchString object.
 * 
 * Return value: A new #GNCSearchString object.
 **/
GNCSearchString *
gnc_search_string_new (void)
{
  GNCSearchString *o = (GNCSearchString *)gtk_type_new(gnc_search_string_get_type ());
  return o;
}

void
gnc_search_string_set_value (GNCSearchString *fi, const char *value)
{
  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_STRING (fi));
	
  if (fi->value)
    g_free (fi->value);

  fi->value = g_strdup (value);
}

void
gnc_search_string_set_how (GNCSearchString *fi, GNCSearchString_Type how)
{
  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_STRING (fi));
  fi->how = how;
}

void
gnc_search_string_set_case (GNCSearchString *fi, gboolean ignore_case)
{
  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_STRING (fi));
  fi->ign_case = ignore_case;
}

static gboolean
validate (GNCSearchCoreType *fe)
{
  GNCSearchString *fi = (GNCSearchString *)fe;
  gboolean valid = TRUE;

  g_return_val_if_fail (fi, FALSE);
  g_return_val_if_fail (IS_GNCSEARCH_STRING (fi), FALSE);
	
  if (!fi->value || *(fi->value) == '\0') {
    GtkWidget *dialog;
    dialog = gnome_ok_dialog (_("You need to enter a string value"));
    gnome_dialog_run_and_close (GNOME_DIALOG (dialog));
    return FALSE;
  }

  if (fi->how == SEARCH_STRING_MATCHES_REGEX ||
      fi->how == SEARCH_STRING_NOT_MATCHES_REGEX) {
    regex_t regexpat;        /* regex patern */
    gint regerr;
    int flags = REG_EXTENDED;
		
    if (fi->ign_case)
      flags |= REG_ICASE;

    regerr = regcomp (&regexpat, fi->value, flags);
    if (regerr) {
      GtkWidget *dialog;
      gchar *regmsg, *errmsg;
      size_t reglen;
			
      /* regerror gets called twice to get the full error string 
	 length to do proper posix error reporting */
      reglen = regerror (regerr, &regexpat, 0, 0);
      regmsg = g_malloc0 (reglen + 1);
      regerror (regerr, &regexpat, regmsg, reglen);
			
      errmsg = g_strdup_printf (_("Error in regular expression '%s':\n%s"),
				fi->value, regmsg);
      g_free (regmsg);
			
      dialog = gnome_ok_dialog (errmsg);
			
      gnome_dialog_run_and_close (GNOME_DIALOG (dialog));
			
      g_free (errmsg);
      valid = FALSE;
    }
		
    regfree (&regexpat);
  }
	
  return valid;
}

static void
toggle_changed (GtkToggleButton *button, GNCSearchString *fe)
{
  fe->ign_case = gtk_toggle_button_get_active (button);
}

static void
option_changed (GtkWidget *widget, GNCSearchString *fe)
{
  fe->how = (GNCSearchString_Type)
    gtk_object_get_data (GTK_OBJECT (widget), "option");
}

static void
entry_changed (GtkEntry *entry, GNCSearchString *fe)
{
  char *new;
	
  new = gtk_entry_get_text(entry);
  gnc_search_string_set_value (fe, new);
}

static GtkWidget *
add_menu_item (GtkWidget *menu, gpointer user_data, char *label,
	       GNCSearchString_Type option)
{
  GtkWidget *item = gtk_menu_item_new_with_label (label);
  gtk_object_set_data (GTK_OBJECT (item), "option", (gpointer) option);
  gtk_signal_connect (GTK_OBJECT (item), "activate", option_changed, user_data);
  gtk_menu_append (GTK_MENU (menu), item);
  gtk_widget_show (item);
  return item;
}

static GtkWidget *
make_menu (GNCSearchCoreType *fe)
{
  GNCSearchString *fi = (GNCSearchString *)fe;
  GtkWidget *menu, *item, *first, *opmenu;
  int current = 0;

  menu = gtk_menu_new ();

  item = add_menu_item (menu, fe, _("contains"), SEARCH_STRING_CONTAINS);
  first = item;

  item = add_menu_item (menu, fe, _("does not contain"),
			SEARCH_STRING_NOT_CONTAINS);
  if (fi->how == SEARCH_STRING_NOT_CONTAINS) { current = 1; first = item; }

  item = add_menu_item (menu, fe, _("matches regex"),
			SEARCH_STRING_MATCHES_REGEX);
  if (fi->how == SEARCH_STRING_MATCHES_REGEX) { current = 2; first = item; }

  item = add_menu_item (menu, fe, _("does not match regex"),
			SEARCH_STRING_NOT_MATCHES_REGEX);
  if (fi->how == SEARCH_STRING_NOT_MATCHES_REGEX)
    { current = 3; first = item; }

  opmenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu (GTK_OPTION_MENU (opmenu), menu);

  gtk_signal_emit_by_name (GTK_OBJECT (first), "activate", fe);
  gtk_option_menu_set_history (GTK_OPTION_MENU (opmenu), current);

  return opmenu;
}

static void
grab_focus (GNCSearchCoreType *fe)
{
  GNCSearchString *fi = (GNCSearchString *)fe;

  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_STRING (fi));

  if (fi->priv->entry)
    gtk_widget_grab_focus (fi->priv->entry);
}

static void
editable_enters (GNCSearchCoreType *fe, GnomeDialog *dialog)
{
  GNCSearchString *fi = (GNCSearchString *)fe;

  g_return_if_fail (fi);
  g_return_if_fail (IS_GNCSEARCH_STRING (fi));
  g_return_if_fail (dialog);

  if (fi->priv->entry)
    gnome_dialog_editable_enters (dialog, GTK_EDITABLE (fi->priv->entry));
}

static GtkWidget *
get_widget (GNCSearchCoreType *fe)
{
  GtkWidget *entry, *toggle, *menu, *box;
  GNCSearchString *fi = (GNCSearchString *)fe;
	
  g_return_val_if_fail (fi, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_STRING (fi), NULL);

  box = gtk_hbox_new (FALSE, 3);

  /* Build and connect the option menu */
  menu = make_menu (fe);
  gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

  /* Build and connect the entry window */
  entry = gtk_entry_new ();
  if (fi->value)
    gtk_entry_set_text (GTK_ENTRY (entry), fi->value);
  gtk_signal_connect (GTK_OBJECT (entry), "changed", entry_changed, fe);
  gtk_box_pack_start (GTK_BOX (box), entry, FALSE, FALSE, 3);
  fi->priv->entry = entry;

  /* Build and connect the toggle button */
  toggle = gtk_toggle_button_new_with_label (_("Case Insensitive?"));
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (toggle), fi->ign_case);
  gtk_signal_connect (GTK_OBJECT(toggle), "toggled", toggle_changed, fe);
  gtk_box_pack_start (GTK_BOX (box), toggle, FALSE, FALSE, 3);

  /* And return the box */
  return box;
}

static QueryPredData_t get_predicate (GNCSearchCoreType *fe)
{
  GNCSearchString *ss = (GNCSearchString *)fe;
  query_compare_t how;
  string_match_t options = STRING_MATCH_NORMAL;
  gboolean is_regex = FALSE;

  g_return_val_if_fail (ss, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_STRING (ss), NULL);

  switch (ss->how) {
  case SEARCH_STRING_MATCHES_REGEX:
    is_regex = TRUE;
    /* FALLTHROUGH */
  case SEARCH_STRING_CONTAINS:
    how = COMPARE_EQUAL;
    break;
  case SEARCH_STRING_NOT_MATCHES_REGEX:
    is_regex = TRUE;
    /* FALLTHROUGH */
  case SEARCH_STRING_NOT_CONTAINS:
    how = COMPARE_NEQ;
    break;
  default:
    g_warning ("invalid string choice: %d", ss->how);
    return NULL;
  }

  if (ss->ign_case)
    options = STRING_MATCH_CASEINSENSITIVE;

  return gncQueryStringPredicate (how, ss->value, options, is_regex);
}

static GNCSearchCoreType *clone(GNCSearchCoreType *fe)
{
  GNCSearchString *se, *fse = (GNCSearchString *)fe;

  g_return_val_if_fail (fse, NULL);
  g_return_val_if_fail (IS_GNCSEARCH_STRING (fse), NULL);

  se = gnc_search_string_new ();
  gnc_search_string_set_value (se, fse->value);
  gnc_search_string_set_how (se, fse->how);
  gnc_search_string_set_case (se, fse->ign_case);

  return (GNCSearchCoreType *)se;
}
