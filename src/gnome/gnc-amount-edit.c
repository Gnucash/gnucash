/*
 * gnc-amount-edit.c -- Amount editor widget
 *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>
 * All rights reserved.
 *
 * Gnucash is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
 */

/*
 * Amount editor widget
 *
 * Authors: Dave Peticolas <dave@krondo.com>
 */

#include "config.h"

#include "gnc-amount-edit.h"
#include "gnc-exp-parser.h"
#include "messages.h"
#include "query-user.h"
#include "gnc-ui-util.h"
#include "gnc-engine-util.h"


/* Signal codes */
enum
{
  AMOUNT_CHANGED,
  LAST_SIGNAL
};


static gint amount_edit_signals [LAST_SIGNAL] = { 0 };


static void gnc_amount_edit_init         (GNCAmountEdit      *gae);
static void gnc_amount_edit_class_init   (GNCAmountEditClass *class);
static void gnc_amount_edit_destroy      (GtkObject          *object);
static void gnc_amount_edit_forall       (GtkContainer       *container,
                                          gboolean	    include_internals,
                                          GtkCallback	    callback,
                                          gpointer	    callbabck_data);


static GtkHBoxClass *parent_class;

/**
 * gnc_amount_edit_get_type:
 *
 * Returns the GtkType for the GNCAmountEdit widget
 */
guint
gnc_amount_edit_get_type (void)
{
  static guint amount_edit_type = 0;

  if (!amount_edit_type){
    GtkTypeInfo amount_edit_info = {
      "GNCAmountEdit",
      sizeof (GNCAmountEdit),
      sizeof (GNCAmountEditClass),
      (GtkClassInitFunc) gnc_amount_edit_class_init,
      (GtkObjectInitFunc) gnc_amount_edit_init,
      NULL,
      NULL,
    };

    amount_edit_type = gtk_type_unique (gtk_hbox_get_type (),
                                        &amount_edit_info);
  }

  return amount_edit_type;
}

static void
gnc_amount_edit_class_init (GNCAmountEditClass *class)
{
  GtkObjectClass *object_class = (GtkObjectClass *) class;
  GtkContainerClass *container_class = (GtkContainerClass *) class;

  object_class = (GtkObjectClass*) class;

  parent_class = gtk_type_class (gtk_hbox_get_type ());

  amount_edit_signals [AMOUNT_CHANGED] =
    gtk_signal_new ("amount_changed",
                    GTK_RUN_FIRST, object_class->type, 
                    GTK_SIGNAL_OFFSET (GNCAmountEditClass,
                                       amount_changed),
                    gtk_signal_default_marshaller,
                    GTK_TYPE_NONE, 0);
	
  gtk_object_class_add_signals (object_class, amount_edit_signals,
                                LAST_SIGNAL);

  container_class->forall = gnc_amount_edit_forall;

  object_class->destroy = gnc_amount_edit_destroy;

  class->amount_changed = NULL;
}

static void
gnc_amount_edit_init (GNCAmountEdit *gae)
{
  gae->need_to_parse = FALSE;
  gae->amount = gnc_numeric_zero ();
  gae->print_info = gnc_default_print_info (FALSE);
  gae->fraction = 0;
  gae->evaluate_on_enter = FALSE;
}

static void
gnc_amount_edit_destroy (GtkObject *object)
{
  GNCAmountEdit *gae;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GNC_IS_AMOUNT_EDIT (object));

  gae = GNC_AMOUNT_EDIT (object);

  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}

static void
gnc_amount_edit_forall (GtkContainer *container, gboolean include_internals,
			GtkCallback callback, gpointer callback_data)
{
  g_return_if_fail (container != NULL);
  g_return_if_fail (GNC_IS_AMOUNT_EDIT (container));
  g_return_if_fail (callback != NULL);

  /* Let GtkBox handle things only if the internal widgets need
   * to be poked. */
  if (include_internals)
    if (GTK_CONTAINER_CLASS (parent_class)->forall)
      (* GTK_CONTAINER_CLASS (parent_class)->forall)
        (container,
         include_internals,
         callback,
         callback_data);
}


static void
amount_entry_changed (GtkEditable *editable, gpointer data)
{
  GNCAmountEdit *gae = data;

  gae->need_to_parse = TRUE;
}

static int
amount_entry_key_press(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
  GNCAmountEdit *gae = data;

  switch (event->keyval)
  {
    case GDK_Return:
      if (gae->evaluate_on_enter)
        break;
      if (event->state & (GDK_CONTROL_MASK | GDK_MOD1_MASK | GDK_SHIFT_MASK))
        break;
      return FALSE;
    case GDK_KP_Enter:
      break;
    default:
      return FALSE;
  }

  gtk_signal_emit_stop_by_name (GTK_OBJECT (widget), "key_press_event");

  gnc_amount_edit_evaluate (gae);

  return TRUE;
}

static void
create_children (GNCAmountEdit *gae)
{
  gae->amount_entry  = gtk_entry_new ();
  gtk_box_pack_start (GTK_BOX (gae), gae->amount_entry, TRUE, TRUE, 0);
  gtk_widget_show (gae->amount_entry);
  gtk_signal_connect (GTK_OBJECT (gae->amount_entry), "changed",
                      GTK_SIGNAL_FUNC(amount_entry_changed), gae);
  gtk_signal_connect (GTK_OBJECT (gae->amount_entry), "key_press_event",
                      GTK_SIGNAL_FUNC(amount_entry_key_press), gae);
}

/**
 * gnc_amount_edit_new:
 *
 * Creates a new GNCAmountEdit widget which can be used to provide
 * an easy to use way for entering amounts, allowing the user to
 * enter and evaluate expressions.
 * 
 * Returns a GNCAmountEdit widget.
 */
GtkWidget *
gnc_amount_edit_new (void)
{
  GNCAmountEdit *gae;

  gae = gtk_type_new (gnc_amount_edit_get_type ());

  create_children (gae);

  return GTK_WIDGET (gae);
}

/**
 * gnc_amount_edit_evaluate
 * @gae: The GNCAmountEdit widget
 *
 * If needed, parse the expression in the amount entry
 * and replace the expression with the result of evaluation.
 * If there is a parsing error, don't change the expression entry,
 * but do put the cursor at the point of the error.
 *
 * Return TRUE if parsing was successful or there was no need to parse.
 */
gboolean
gnc_amount_edit_evaluate (GNCAmountEdit *gae)
{
  const char *string;
  char *error_loc;
  gnc_numeric amount;
  gboolean ok;

  g_return_val_if_fail(gae != NULL, FALSE);
  g_return_val_if_fail(GNC_IS_AMOUNT_EDIT(gae), FALSE);

  if (!gae->need_to_parse)
    return TRUE;

  string = gtk_entry_get_text(GTK_ENTRY(gae->amount_entry));

  error_loc = NULL;

  ok = gnc_exp_parser_parse (string, &amount, &error_loc);

  if (ok)
  {
    gnc_numeric old_amount = gae->amount;

    if (gae->fraction > 0)
      amount = gnc_numeric_convert (amount, gae->fraction, GNC_RND_ROUND);

    gnc_amount_edit_set_amount (gae, amount);

    if (!gnc_numeric_equal (amount, old_amount))
      gtk_signal_emit (GTK_OBJECT (gae), amount_edit_signals [AMOUNT_CHANGED]);

    return TRUE;
  }

  /* Not ok */
  if (error_loc != NULL)
    gtk_editable_set_position (GTK_EDITABLE(gae->amount_entry),
                               error_loc - string);

  return FALSE;
}


/**
 * gnc_amount_edit_get_amount:
 * @gae: The GNCAmountEdit widget
 *
 * Returns the amount entered in the GNCAmountEdit widget,
 * parsing the epxression if necessary. The result of parsing
 * replaces the expression.
 */
gnc_numeric
gnc_amount_edit_get_amount (GNCAmountEdit *gae)
{
  g_return_val_if_fail(gae != NULL, gnc_numeric_zero ());
  g_return_val_if_fail(GNC_IS_AMOUNT_EDIT(gae), gnc_numeric_zero ());

  gnc_amount_edit_evaluate (gae);

  return gae->amount;
}


/**
 * gnc_amount_edit_get_amount:
 * @gae: The GNCAmountEdit widget
 *
 * Returns the amount entered in the GNCAmountEdit widget,
 * parsing the epxression if necessary. The result of parsing
 * replaces the expression.
 */
double
gnc_amount_edit_get_damount (GNCAmountEdit *gae)
{
  g_return_val_if_fail(gae != NULL, 0.0);
  g_return_val_if_fail(GNC_IS_AMOUNT_EDIT(gae), 0.0);

  gnc_amount_edit_evaluate (gae);

  return gnc_numeric_to_double (gae->amount);
}


/**
 * gnc_amount_edit_set_amount:
 * @gae: The GNCAmountEdit widget
 * @amount: The amount to set
 *
 * Returns nothing.
 */
void
gnc_amount_edit_set_amount (GNCAmountEdit *gae, gnc_numeric amount)
{
  const char * amount_string;

  g_return_if_fail(gae != NULL);
  g_return_if_fail(GNC_IS_AMOUNT_EDIT(gae));
  g_return_if_fail(!gnc_numeric_check (amount));

  gae->amount = amount;
  gae->need_to_parse = FALSE;

  amount_string = xaccPrintAmount (amount, gae->print_info);

  gtk_entry_set_text (GTK_ENTRY (gae->amount_entry), amount_string);
}

/**
 * gnc_amount_edit_set_amount:
 * @gae: The GNCAmountEdit widget
 * @amount: The amount to set
 *
 * Returns nothing.
 */
void
gnc_amount_edit_set_damount (GNCAmountEdit *gae, double damount)
{
  gnc_numeric amount;
  int fraction;

  g_return_if_fail(gae != NULL);
  g_return_if_fail(GNC_IS_AMOUNT_EDIT(gae));

  if (gae->fraction > 0)
    fraction = gae->fraction;
  else
    fraction = 100000;

  amount = double_to_gnc_numeric (damount, fraction, GNC_RND_ROUND);

  gnc_amount_edit_set_amount (gae, amount);
}

/**
 * gnc_amount_edit_set_print_flags:
 * @gae: The GNCAmountEdit widget
 * @print_flags: The print flags to set
 *
 * Returns nothing.
 */
void
gnc_amount_edit_set_print_info (GNCAmountEdit *gae,
                                GNCPrintAmountInfo print_info)
{
  g_return_if_fail(gae != NULL);
  g_return_if_fail(GNC_IS_AMOUNT_EDIT(gae));

  gae->print_info = print_info;
  gae->print_info.use_symbol = 0;
}


/**
 * gnc_amount_edit_set_fraction:
 * @gae: The GNCAmountEdit widget
 * @fraction: The fraction to set
 *
 * Returns nothing.
 */
void
gnc_amount_edit_set_fraction (GNCAmountEdit *gae, int fraction)
{
  g_return_if_fail(gae != NULL);
  g_return_if_fail(GNC_IS_AMOUNT_EDIT(gae));

  fraction = MAX (0, fraction);

  gae->fraction = fraction;
}


/**
 * gnc_amount_edit_gtk_entry:
 * @gae: The GNCAmountEdit widget
 *
 * Returns the gtk entry of the widget..
 */
GtkWidget *
gnc_amount_edit_gtk_entry (GNCAmountEdit *gae)
{
  g_return_val_if_fail(gae != NULL, NULL);
  g_return_val_if_fail(GNC_IS_AMOUNT_EDIT(gae), NULL);

  return gae->amount_entry;
}


/**
 * gnc_amount_edit_set_evaluate_on_enter:
 * @gae: The GNCAmountEdit widget
 * @evaluate_on_enter: The flag value to set
 *
 * Returns nothing.
 */
void
gnc_amount_edit_set_evaluate_on_enter (GNCAmountEdit *gae,
                                       gboolean evaluate_on_enter)
{
  g_return_if_fail(gae != NULL);
  g_return_if_fail(GNC_IS_AMOUNT_EDIT(gae));

  gae->evaluate_on_enter = evaluate_on_enter;
}
