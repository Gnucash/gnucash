/* hbci-interaction.c
   Copyright 2002 by Christian Stimming <stimming@tuhh.de> */

/***************************************************************************
 *                                                                         *
 *   This library is free software; you can redistribute it and/or         *
 *   modify it under the terms of the GNU Lesser General Public            *
 *   License as published by the Free Software Foundation; either          *
 *   version 2.1 of the License, or (at your option) any later version.    *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston,                 *
 *   MA  02111-1307  USA                                                   *
 *                                                                         *
 ***************************************************************************/

#include "config.h"

#include <stdio.h>
#include <string.h>
#include <locale.h>
#include "hbci-interaction.h"
#include "hbci-interactionP.h"

#include <openhbci/interactorcb.h>
#include <openhbci/progressmonitorcb.h>
#include "dialog-utils.h"
#include "druid-utils.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "global-options.h"

#include "dialog-pass.h"

#include <openhbci.h>

#define PREF_TAB_ONLINE_BANKING N_("Online Banking & Importing")


/** Adds the interactor and progressmonitor classes to the api. */
GNCInteractor *gnc_hbci_api_interactors (HBCI_API *api, GtkWidget *parent)
{
  GNCInteractor *data;
  
  data = g_new0 (GNCInteractor, 1);
  data->parent = parent;
  data->keepAlive = TRUE;
  data->cache_pin = 
    gnc_lookup_boolean_option(PREF_TAB_ONLINE_BANKING,
			      "HBCI Remember PIN in memory",
                              FALSE);

  /* set HBCI_Interactor */
  HBCI_Hbci_setInteractor(HBCI_API_Hbci(api), 
			  gnc_hbci_new_interactor(data), TRUE);
  /* Set HBCI_Progressmonitor */
  HBCI_API_setMonitor(api, gnc_hbci_new_pmonitor(data), TRUE);
  return data;
}

gboolean GNCInteractor_aborted(const GNCInteractor *i)
{
  g_assert(i);
  return !(i->keepAlive);
}

void GNCInteractor_show_nodelete(GNCInteractor *i)
{
  gboolean cache_pin = 
    gnc_lookup_boolean_option(PREF_TAB_ONLINE_BANKING,
			      "HBCI Remember PIN in memory",
			      FALSE);
  g_assert(i);
  /* Show widgets */
  gtk_widget_show_all (i->dialog);

  /* Make sure the cache_pin option is up to date. */
  if (cache_pin != i->cache_pin) {
    i->cache_pin = cache_pin;
    if (cache_pin == FALSE)
      GNCInteractor_erasePIN (i);
  }
}
void GNCInteractor_show(GNCInteractor *i)
{
  g_assert(i);
  GNCInteractor_show_nodelete(i);
  /* Clear log window. */
  gtk_editable_delete_text (GTK_EDITABLE (i->log_text), 0, -1);
}


void GNCInteractor_hide(GNCInteractor *i)
{
  g_assert(i);
  if (gtk_toggle_button_get_active 
      (GTK_TOGGLE_BUTTON (i->close_checkbutton)))
    gtk_widget_hide_all (i->dialog);
  gnc_set_boolean_option ("__gui", "hbci_close_on_finish",
			  gtk_toggle_button_get_active 
			  (GTK_TOGGLE_BUTTON (i->close_checkbutton)));
}

void GNCInteractor_delete(GNCInteractor *data)
{
  if (data == NULL)
    return;
  if (data->dialog != NULL) {
    gnc_set_boolean_option ("__gui", "hbci_close_on_finish",
			    gtk_toggle_button_get_active 
			    (GTK_TOGGLE_BUTTON (data->close_checkbutton)));
    gtk_object_unref (GTK_OBJECT (data->dialog));
    gtk_widget_destroy (data->dialog);
  }
  
  data->dialog = NULL;
}

void GNCInteractor_erasePIN(GNCInteractor *i)
{
  g_assert(i);
  if (i->pw != NULL)
    g_free (memset (i->pw, 0, strlen (i->pw)));
  i->pw = NULL;
  i->user = NULL;
}
void GNCInteractor_reparent (GNCInteractor *i, GtkWidget *new_parent)
{
  g_assert (i);
  if (new_parent != i->parent)
    {
      i->parent = new_parent;
      /*if (GTK_WIDGET (i->dialog) -> parent != NULL)
	gtk_widget_reparent (GTK_WIDGET (i->dialog), new_parent);
	else
	gtk_widget_set_parent (GTK_WIDGET (i->dialog), new_parent);*/
      gnome_dialog_set_parent (GNOME_DIALOG (i->dialog), 
			       GTK_WINDOW (new_parent));
    }
}

/* Helper functions */
static const char *bank_to_str (const HBCI_Bank *bank)
{
  g_assert (bank);
  return ((strlen(HBCI_Bank_name (bank)) > 0) ?
	  HBCI_Bank_name (bank) :
	  HBCI_Bank_bankCode(bank));
}

/********************************************************
 * Now all the callback functions 
 */
static int msgInputPin(const HBCI_User *user,
		       char **pinbuf,
		       int minsize,
		       int newPin,
		       void *user_data)
{
  GNCInteractor *data = user_data;
  const HBCI_Bank *bank = NULL;
  char *msgstr = NULL, *passwd = NULL;
  int retval = 0;
  g_assert(data);

  while (TRUE) {
    const char *username;
    username =
      (user ?
       (strlen (HBCI_User_userName (user)) > 0 ? HBCI_User_userName (user) :
	(HBCI_User_userId (user) ? HBCI_User_userId (user) :
	 _("Unknown"))) : 
       _("Unknown"));
    g_assert (username);
    
    if (newPin) {
      if (user != NULL) {
	bank = HBCI_User_bank (user);
	if (bank != NULL) {
	  /* Translators: Strings from this file are really only
	   * needed inside Germany (HBCI is not supported anywhere
	   * else). You may safely ignore strings from the
	   * import-export/hbci subdirectory in other countries.
	   *
	   * xgettext:c-format */	    
	  msgstr = g_strdup_printf (_("Please enter and confirm new PIN for \n"
				      "user '%s' at bank '%s',\n"
				      "with at least %d characters."),
				    username, 
				    bank_to_str (bank),
				    minsize);
	}
	else 
	  /* xgettext:c-format */	    
	  msgstr = g_strdup_printf ( _("Please enter and confirm a new PIN for \n"
				       "user '%s',\n"
				       "with at least %d characters."), 
				     username, minsize);
      }
      else 
	/* xgettext:c-format */	    
	msgstr = g_strdup_printf ( _("Please enter and confirm a new PIN\n"
				     "with at least %d characters."), 
				   minsize);

      retval = gnc_hbci_get_initial_password (data->parent,
					      msgstr,
					      &passwd);
      g_free (msgstr);
    }
    else {
      if (user && (user == data->user)) {
	/* Cached user matches, so use cached PIN. */
	/*printf("Got the cached PIN for user %s.\n", HBCI_User_userId (user));*/
	*pinbuf = g_strdup (data->pw);
	return 1;
      }
      else {
	if (user != NULL) {
	  bank = HBCI_User_bank (user);
	  if (bank != NULL) {
	    /* xgettext:c-format */	    
	    msgstr = g_strdup_printf (_("Please enter PIN for \n"
					"user '%s' at bank '%s'."),
				      username, 
				      bank_to_str (bank));
	  }
	  else {
	    /* xgettext:c-format */	    
	    msgstr = g_strdup_printf ( _("Please enter PIN for \n"
					 "user '%s' at unknown bank."),
				       username);
	  }
	}
	else 
	  msgstr = g_strdup ( _("Please enter the PIN for \n"
				"the newly created user."));
	
	retval = gnc_hbci_get_password (data->parent,
					msgstr,
					NULL,
					&passwd);
	g_free (msgstr);
      } /* user == data->user */
    } /* newPin */
    
    if (!retval)
      break;
    
    if (strlen(passwd) < (unsigned int)minsize) {
      gboolean retval;
      char *msg = 
	g_strdup_printf (  _("The PIN needs to be at least %d characters \n"
			     "long. Do you want to try again?"),
			   minsize);
      retval = gnc_verify_dialog (data->parent, TRUE, msg);
      g_free (msg);
      if (!retval)
	break;
    }
    else {
      *pinbuf = g_strdup (passwd);
      if (user && data->cache_pin) {
	/*printf("Cached the PIN for user %s.\n", HBCI_User_userId (user));*/
	data->user = user;
	if (data->pw)
	  g_free (memset (data->pw, 0, strlen (data->pw)));
	data->pw = passwd;
      }
      else 
	g_free (memset (passwd, 0, strlen (passwd)));
      return 1;
    }
  }
  
  /* User wanted to abort. */
  return 0;
}


static int msgInsertMediumOrAbort(const HBCI_User *user, 
				MediumType mtype, 
				void *user_data)
{
  GNCInteractor *data = user_data;
  const HBCI_Bank * b = NULL;
  char *msgstr = NULL;
  GNCVerifyResult retval;
  g_assert(data);

  if (user != NULL) {
    const char *username = 
      (HBCI_User_userName (user) ? HBCI_User_userName (user) :
       (HBCI_User_userId (user) ? HBCI_User_userId (user) :
	_("Unknown")));
    b = HBCI_User_bank (user);
    switch (mtype) 
      {
      case MediumTypeFile:
	if (b != NULL) 
	  /* xgettext:c-format */	    
	  msgstr = g_strdup_printf 
	    ( _("Please make sure the key file for user '%s' at bank '%s' can be \n"
		"accessed. If the key file is on your harddisk, simply press 'Ok'. If \n"
		"the key file is on a floppy disk or other removable media, please make \n"
		"sure the floppy disk or medium is correctly mounted."), 
	      username, bank_to_str (b));
	else 
	  /* xgettext:c-format */	    
	  msgstr = g_strdup_printf 
	    ( _("Please make sure the key file for user '%s' can be \n"
		"accessed. If the key file is on your harddisk, simply press 'Ok'. If \n"
		"the key file is on a floppy disk or other removable media, please make \n"
		"sure the floppy disk or medium is correctly mounted."), 
				     username);
	break;
      case MediumTypeCard:
      default:
	if (b != NULL) 
	  /* xgettext:c-format */	    
	  msgstr = g_strdup_printf ( _("Please insert chip card for \n"
				       "user '%s' at bank '%s'."), 
				     username, bank_to_str (b));
	else 
	  /* xgettext:c-format */	    
	  msgstr = g_strdup_printf ( _("Please insert chip card for \n"
				       "user '%s'."), 
				     username);
    }
  }
  else 
    switch (mtype) 
      {
      case MediumTypeFile:
	msgstr = g_strdup 
	  (_("Please make sure the key file for the newly created user can be \n"
	     "accessed. If you want to create the key file on your harddisk, simply \n"
	     "press 'Ok'. If you want to create the key on a floppy disk or other \n"
	     "removable media, please make sure the floppy disk or medium is \n"
	     "correctly mounted."));
	break;
      case MediumTypeCard:
      default:
	msgstr = g_strdup ( _("Please insert chip card for \n"
			      "the newly created user."));
      }
    
  retval = gnc_ok_cancel_dialog (data->parent, GTK_RESPONSE_OK, "%s", msgstr);
  g_free (msgstr);
  
  return (retval == GTK_RESPONSE_OK);
}


static int msgInsertCorrectMediumOrAbort(const HBCI_User *user, 
				       MediumType mtype, 
				       void *user_data)
{
  GNCInteractor *data = user_data;
  const HBCI_Bank * b = NULL;
  char *msgstr = NULL;
  GNCVerifyResult retval;
  g_assert(data);

  if (user != NULL) {
    const char *username = 
      (HBCI_User_userName (user) ? HBCI_User_userName (user) :
       (HBCI_User_userId (user) ? HBCI_User_userId (user) :
	_("Unknown")));
    b = HBCI_User_bank (user);
    switch (mtype) 
      {
      case MediumTypeFile: 
	if (b != NULL) 
	  /* xgettext:c-format */	    
	  msgstr = g_strdup_printf ( _("The key file does not seem to be the correct \n"
				       "file for user '%s' at bank '%s'. Please make \n"
				       "sure the correct key file can be accessed."), 
				     username, bank_to_str (b));
	else 
	  /* xgettext:c-format */	    
	  msgstr = g_strdup_printf ( _("The key file does not seem to be the correct \n"
				       "file for user '%s'. Please make sure the \n"
				       "correct key file can be accessed."), 
				     username);
	break;
      case MediumTypeCard: 
      default:
	if (b != NULL) 
	  /* xgettext:c-format */	    
	  msgstr = g_strdup_printf ( _("Please insert the correct chip card for \n"
				       "user '%s' at bank '%s'."), 
				     username, bank_to_str (b));
	else 
	  /* xgettext:c-format */	    
	  msgstr = g_strdup_printf ( _("Please insert the correct chip card for \n"
				       "user '%s'."), 
				     username);
      }
  }
  else 
    switch (mtype) 
      {
      case MediumTypeFile: 
	msgstr = g_strdup ( _("The key file does not seem to be the correct \n"
			      "file for the newly created user. Please make \n"
			      "sure the correct key file can be accessed."));
	break;
      case MediumTypeCard: 
      default:
	msgstr = g_strdup ( _("Please insert the correct chip card for \n"
			      "the newly created user."));
      }
  
  retval = gnc_ok_cancel_dialog (data->parent, GTK_RESPONSE_OK, "%s", msgstr);
  g_free (msgstr);
  
  return (retval == GTK_RESPONSE_OK);
}


static void msgStateResponse(const char *msg, void *user_data)
{
  GNCInteractor *data = user_data;
  g_assert(data);

  add_log_text (data, msg);
  /*fprintf(stdout,"hbci-initial-druid-msgStateResponse: %s\n",msg);*/
  /* Let the widgets be redrawn */
  while (g_main_iteration (FALSE));
}

static int keepAlive(void *user_data)
{
  GNCInteractor *data = user_data;
  g_assert(data);
  /*fprintf(stdout, "my-keepAlive: returning 1\n");*/

  /* Let the widgets be redrawn */
  while (g_main_iteration (FALSE));

  return data->keepAlive;
}

static void destr(void *user_data) 
{
  GNCInteractor *data = user_data;
  if (data == NULL)
    return;

  if (data->pw) {
    memset (data->pw, 0, strlen(data->pw));
    g_free (data->pw);
  }
}

static void 
msgEndInputPinViaKeypadCB(const HBCI_User *user, void *user_data)
{
  GNCInteractor *data = user_data;
  g_assert(data);
  if (data->pin_keypad_dialog) {
    gnome_dialog_close (GNOME_DIALOG (data->pin_keypad_dialog));
    gtk_widget_destroy (data->pin_keypad_dialog);
    data->pin_keypad_dialog = NULL;
  }
}

static void 
msgStartInputPinViaKeypadCB(const HBCI_User *user, void *user_data)
{
  const HBCI_Bank *bank;
  char *msgstr;
  GtkWidget *dialog;
  GNCInteractor *data = user_data;
  
  g_assert(data);

  /* Already an existing dialog? Shouldn't happen. Better delete
     existing dialog first. */
  if (data->pin_keypad_dialog != NULL)
    msgEndInputPinViaKeypadCB(user, user_data);

  /* Create message string */
  if (user != NULL) {
    const char *username = 
      (HBCI_User_userName (user) ? HBCI_User_userName (user) :
       (HBCI_User_userId (user) ? HBCI_User_userId (user) :
	_("Unknown")));
    bank = HBCI_User_bank (user);
    if (bank != NULL) {
      /* xgettext:c-format */	    
      msgstr = g_strdup_printf (_("Please enter PIN for \n"
				  "user '%s' at bank '%s'\n"
				  "at the keypad of your chip card reader."),
				username, 
				bank_to_str (bank));
    }
    else {
      /* xgettext:c-format */	    
      msgstr = g_strdup_printf ( _("Please enter PIN for \n"
				   "user '%s'\n"
				   "at the keypad of your chip card reader."),
				 username);
    }
  }
  else 
    msgstr = g_strdup ( _("Please enter PIN for \n"
			  "the newly created user \n"
			  "at the keypad of your chip card reader."));

  /* Create new dialog */
  dialog = gnome_ok_dialog_parented (msgstr, GTK_WINDOW (data->parent));
  gnome_dialog_close_hides (GNOME_DIALOG(dialog), TRUE);
  gtk_widget_show_all (dialog);
  data->pin_keypad_dialog = dialog;

  g_free (msgstr);
}



/********************************************************
 * Constructor 
 */
HBCI_Interactor *
gnc_hbci_new_interactor(GNCInteractor *data)
{
  HBCI_InteractorCB *inter;

  inter = HBCI_InteractorCB_new2(&destr,
				 &msgInputPin,
				 &msgInsertMediumOrAbort,
				 &msgInsertCorrectMediumOrAbort,
				 &msgStateResponse,
				 &keepAlive,
				 &msgStartInputPinViaKeypadCB,
				 &msgEndInputPinViaKeypadCB,
				 data);

  return HBCI_InteractorCB_Interactor(inter);
}
