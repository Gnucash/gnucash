/***************************************************************************
    begin       : Sun May 16 2010
    copyright   : (C) 2010 by Martin Preuss
    email       : martin@libchipcard.de

 ***************************************************************************
 *          Please see toplevel file COPYING for license details           *
 ***************************************************************************/

#ifndef GTK3_GUI_H
#define GTK3_GUI_H


#include <gtk/gtk.h>


#if defined __GNUC__ && (! defined (__sun)) && (__GNUC__ >= 4 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 3))
# ifdef BUILDING_GTK3_GUI
#   define GTK3GUI_API __attribute__ ((visibility("default")))
# else
#   define GTK3GUI_API
# endif
#else
# define GTK3GUI_API
#endif

#include <gwenhywfar/gui.h>



GTK3GUI_API GWEN_GUI *Gtk3_Gui_new(void);



#endif


