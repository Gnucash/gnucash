/*
 * business-utils.c -- General GUI Utilities for GNC Business Objects
 *
 * Written By: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2001
 */

#include "config.h"

#include <gnome.h>

#include "gncBusiness.h"
#include "gncCustomer.h"
#include "gncJob.h"
#include "gncVendor.h"
#include "gncOwner.h"

#include "business-utils.h"
#include "dialog-customer.h"
#include "dialog-job.h"
#include "dialog-vendor.h"

static GtkWidget * gnc_owner_new (GtkWidget *label, GtkWidget *hbox,
				  GNCBook *book, GncOwner *owner,
				  GNCGeneralSelectType type)
{
  GtkWidget *edit;
  GNCGeneralSelectNewSelectCB do_select = NULL;
  const GncBusinessObject *bus_obj;
  const char *type_name = NULL;

  switch (owner->type) {
  case GNC_OWNER_NONE:
  case GNC_OWNER_UNDEFINED:
    return NULL;

  case GNC_OWNER_CUSTOMER:
    if (type == GNC_GENERAL_SELECT_TYPE_SELECT)
      do_select = gnc_customer_edit_new_select;
    else
      do_select = gnc_customer_edit_new_edit;
    type_name = GNC_CUSTOMER_MODULE_NAME;
    break;

  case GNC_OWNER_JOB:
    /* XXX: Jobs are funny things... */
    return NULL;

    if (type == GNC_GENERAL_SELECT_TYPE_SELECT)
      ; //      do_select = gnc_job_edit_new_select;
    else
      ; //      do_select = gnc_job_edit_new_edit;
    type_name = GNC_JOB_MODULE_NAME;
    break;

  case GNC_OWNER_VENDOR:
    if (type == GNC_GENERAL_SELECT_TYPE_SELECT)
      do_select = gnc_vendor_edit_new_select;
    else
      do_select = gnc_vendor_edit_new_edit;
    type_name = GNC_VENDOR_MODULE_NAME;
    break;

  default:
    g_warning ("Unknown type");
    return NULL;
  }

  bus_obj = gncBusinessLookup (type_name);
  if (!bus_obj) {
    g_warning ("Cannot find business object for name and printable()\n");
    return NULL;
  }

  edit = gnc_general_select_new (type, bus_obj->printable, do_select, book);
  if (!edit)
    return NULL;

  gnc_general_select_set_selected (GNC_GENERAL_SELECT (edit),
				   owner->owner.undefined);
  gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);
  gtk_label_set_text (GTK_LABEL (label), gncBusinessGetTypeLabel (type_name));

  return edit;
}


GtkWidget * gnc_owner_select_create (GtkWidget *label, GtkWidget *hbox,
				     GNCBook *book, GncOwner *owner)
{
  g_return_val_if_fail (label != NULL, NULL);
  g_return_val_if_fail (hbox != NULL, NULL);
  g_return_val_if_fail (book != NULL, NULL);
  g_return_val_if_fail (owner != NULL, NULL);

  return gnc_owner_new (label, hbox, book, owner,
			GNC_GENERAL_SELECT_TYPE_SELECT);
}

GtkWidget * gnc_owner_edit_create (GtkWidget *label, GtkWidget *hbox,
				   GNCBook *book, GncOwner *owner)
{
  g_return_val_if_fail (label != NULL, NULL);
  g_return_val_if_fail (hbox != NULL, NULL);
  g_return_val_if_fail (book != NULL, NULL);
  g_return_val_if_fail (owner != NULL, NULL);

  return gnc_owner_new (label, hbox, book, owner,
			GNC_GENERAL_SELECT_TYPE_EDIT);
}

void gnc_owner_get_owner (GtkWidget *widget, GncOwner *owner)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (owner != NULL);

  /* We'll assume that the owner has the proper 'type' because we
   * can't change it here.  Hopefully the caller has it set properly
   */
  owner->owner.undefined =
    gnc_general_select_get_selected (GNC_GENERAL_SELECT (widget));
}
