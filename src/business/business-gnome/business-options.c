/*
 * business-options.c -- Initialize Business Options
 *
 * Written By: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2002 Derek Atkins
 */

#include "config.h"

#include <gnome.h>
#include <g-wrap-wct.h>
#include <libguile.h>

#include "gnc-ui-util.h"
#include "gnc-engine-util.h"
#include "option-util.h"

#include "dialog-options.h"
#include "business-options.h"
#include "business-utils.h"

static int
owner_changed_cb (GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;

  gnc_option_set_changed (option, TRUE);
  gnc_option_call_option_widget_changed_proc (option);
  gnc_options_dialog_changed_internal (widget);

  return FALSE;
}

static GtkWidget *
create_owner_widget (GNCOption *option, GncOwnerType type, GtkWidget *hbox)
{
  GtkWidget *widget;
  GncOwner owner;

  switch (type) {
  case GNC_OWNER_CUSTOMER:
    gncOwnerInitCustomer (&owner, NULL);
    break;
  case GNC_OWNER_VENDOR:
    gncOwnerInitVendor (&owner, NULL);
    break;
  case GNC_OWNER_JOB:
    gncOwnerInitJob (&owner, NULL);
    break;
  default:
    return NULL;
  }

  widget = gnc_owner_select_create (NULL, hbox,
				    gnc_get_current_book (), &owner);
  gnc_option_set_widget (option, widget);

  gtk_signal_connect (GTK_OBJECT (widget), "changed", 
		      GTK_SIGNAL_FUNC (owner_changed_cb), option);

  return widget;
}

static GtkWidget *
make_name_label (char *name)
{
  GtkWidget *label;
  gchar *colon_name;

  colon_name = g_strconcat (name, ":", NULL);
  label = gtk_label_new (colon_name);
  gtk_misc_set_alignment (GTK_MISC (label), 1.0, 0.5);
  g_free (colon_name);

  return label;
}

/********************************************************************/
/* "Owner" Option functions */


static GncOwnerType
get_owner_type_from_option (GNCOption *option)
{
  SCM odata = gnc_option_get_option_data (option);
  SCM conv_func;

  conv_func = gh_eval_str ("gw:enum-<gnc:GncOwnerType>-val->int");
  odata = gh_call1 (conv_func, odata);

  return gh_scm2long (odata);
}


/* Function to set the UI widget based upon the option */
static GtkWidget *
owner_set_widget (GNCOption *option, GtkBox *page_box,
		  GtkTooltips *tooltips,
		  char *name, char *documentation,
		  /* Return values */
		  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  GtkWidget *label;

  *enclosing = gtk_hbox_new (FALSE, 5);
  label = make_name_label (name);
  gtk_box_pack_start (GTK_BOX (*enclosing), label, FALSE, FALSE, 0);

  value = create_owner_widget (option, get_owner_type_from_option (option),
			       *enclosing);

  gnc_option_set_ui_value (option, FALSE);

  gtk_widget_show_all (*enclosing);
  return value;
}

/* Function to set the UI Value for a particular option */
static gboolean
owner_set_value (GNCOption *option, gboolean use_default,
		 GtkWidget *widget, SCM value)
{
  GncOwner owner_def;
  GncOwner *owner;

  if (!gw_wcp_p (value))
    scm_misc_error("business_options:owner_set_value",
		   "Item is not a gw:wcp.", value);

  owner = gw_wcp_get_ptr (value);

  /* XXX: should we verify that the owner type is correct? */
  if (!owner) {
    owner_def.type = get_owner_type_from_option (option);
    owner_def.owner.undefined = NULL;
    owner = &owner_def;
  }

  widget = gnc_option_get_widget (option);
  gnc_owner_set_owner (widget, owner);
  return FALSE;
}

/* Function to get the UI Value for a particular option */
static SCM
owner_get_value (GNCOption *option, GtkWidget *widget)
{
  static GncOwner owner;	/* XXX: might cause trouble? */
  GncOwnerType type;

  type = get_owner_type_from_option (option);
  owner.type = type;
  gnc_owner_get_owner (widget, &owner);

  return gw_wcp_assimilate_ptr (&owner, gh_eval_str("<gnc:GncOwner*>"));
}


/********************************************************************/
/* "Customer" Option functions */


/* Function to set the UI widget based upon the option */
static GtkWidget *
customer_set_widget (GNCOption *option, GtkBox *page_box,
		     GtkTooltips *tooltips,
		     char *name, char *documentation,
		     /* Return values */
		     GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  GtkWidget *label;

  *enclosing = gtk_hbox_new (FALSE, 5);
  label = make_name_label (name);
  gtk_box_pack_start (GTK_BOX (*enclosing), label, FALSE, FALSE, 0);

  value = create_owner_widget (option, GNC_OWNER_CUSTOMER, *enclosing);

  gnc_option_set_ui_value (option, FALSE);

  gtk_widget_show_all (*enclosing);
  return value;
}

/* Function to set the UI Value for a particular option */
static gboolean
customer_set_value (GNCOption *option, gboolean use_default,
		    GtkWidget *widget, SCM value)
{
  GncOwner owner;
  GncCustomer *customer;

  if (!gw_wcp_p (value))
    scm_misc_error("business_options:customer_set_value",
		   "Item is not a gw:wcp.", value);

  customer = gw_wcp_get_ptr (value);
  gncOwnerInitCustomer (&owner, customer);

  widget = gnc_option_get_widget (option);
  gnc_owner_set_owner (widget, &owner);
  return FALSE;
}

/* Function to get the UI Value for a particular option */
static SCM
customer_get_value (GNCOption *option, GtkWidget *widget)
{
  GncOwner owner;

  gnc_owner_get_owner (widget, &owner);

  return gw_wcp_assimilate_ptr (owner.owner.undefined,
				gh_eval_str("<gnc:GncCustomer*>"));
}


/********************************************************************/
/* "Vendor" Option functions */


/* Function to set the UI widget based upon the option */
static GtkWidget *
vendor_set_widget (GNCOption *option, GtkBox *page_box,
		     GtkTooltips *tooltips,
		     char *name, char *documentation,
		     /* Return values */
		     GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  GtkWidget *label;

  *enclosing = gtk_hbox_new (FALSE, 5);
  label = make_name_label (name);
  gtk_box_pack_start (GTK_BOX (*enclosing), label, FALSE, FALSE, 0);

  value = create_owner_widget (option, GNC_OWNER_VENDOR, *enclosing);

  gnc_option_set_ui_value (option, FALSE);

  gtk_widget_show_all (*enclosing);
  return value;
}

/* Function to set the UI Value for a particular option */
static gboolean
vendor_set_value (GNCOption *option, gboolean use_default,
		    GtkWidget *widget, SCM value)
{
  GncOwner owner;
  GncVendor *vendor;

  if (!gw_wcp_p (value))
    scm_misc_error("business_options:vendor_set_value",
		   "Item is not a gw:wcp.", value);

  vendor = gw_wcp_get_ptr (value);
  gncOwnerInitVendor (&owner, vendor);

  widget = gnc_option_get_widget (option);
  gnc_owner_set_owner (widget, &owner);
  return FALSE;
}

/* Function to get the UI Value for a particular option */
static SCM
vendor_get_value (GNCOption *option, GtkWidget *widget)
{
  GncOwner owner;

  gnc_owner_get_owner (widget, &owner);

  return gw_wcp_assimilate_ptr (owner.owner.undefined,
				gh_eval_str("<gnc:GncVendor*>"));
}




void
gnc_business_options_initialize (void)
{
  int i;
  static GNCOptionDef_t options[] = {
    { "owner", owner_set_widget, owner_set_value, owner_get_value },
    { "customer", customer_set_widget, customer_set_value,
      customer_get_value },
    { "vendor", vendor_set_widget, vendor_set_value, vendor_get_value },
    { NULL }
  };

  for (i = 0; options[i].option_name; i++)
    gnc_options_ui_register_option (&(options[i]));
}
