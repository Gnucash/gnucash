
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/MainW.h>

#include "register.h"

/* ================================= */

main (int argc, char *argv[]) {
  Widget toplevel, mainwindow, actionform;
  XtAppContext app;
  BasicRegister *reg;


  toplevel = XtVaAppInitialize( &app, "Xacc", NULL, 0,
                                &argc, argv, NULL,
                                NULL );


  mainwindow = XtVaCreateManagedWidget( "mainwindow", 
                                  xmMainWindowWidgetClass, toplevel, 
                                        XmNdeleteResponse, XmDESTROY,
                                        NULL );

  actionform = XtVaCreateWidget( "form", 
                                 xmFormWidgetClass, mainwindow,
                                 NULL );

  reg = xaccMallocBasicRegister ();

  xaccCreateTable (reg->table, actionform, "yodudue");

  XtManageChild (actionform);

  XtRealizeWidget(toplevel);
  XtRealizeWidget (reg->table->reg);

  XtAppMainLoop(app);

  return 0;
  }


