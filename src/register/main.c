
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/MainW.h>

#include "FileIO.h"
#include "register.h"

void
xaccLoadRegister (BasicRegister *reg, Account *acc)
{

printf ("its %s \n", acc->accountName);

}

/* ================================= */
int loglevel = 1;

main (int argc, char *argv[]) {
  Widget toplevel, mainwindow, actionform;
  XtAppContext app;
  BasicRegister *reg;
  AccountGroup * grp;


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

  grp = xaccReadData ("../../data/test4.xac");

  reg = xaccMallocBasicRegister ();

  xaccLoadRegister (reg, grp->account[0]);

  xaccCreateTable (reg->table, actionform, "yodudue");

  XtManageChild (actionform);

  XtRealizeWidget(toplevel);
  XtRealizeWidget (reg->table->reg);

  XtAppMainLoop(app);

  return 0;
  }


