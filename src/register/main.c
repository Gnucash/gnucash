
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/MainW.h>

#include "FileIO.h"
#include "register.h"

#define BUFSIZE 1024

void
xaccLoadRegister (BasicRegister *reg, Split **slist)
{
   int i;
   Split *split;
   Transaction *trans;
   char buff[BUFSIZE];

   i=0;
   split = slist[0]; 
   while (split) {

      trans = (Transaction *) (split->parent);

      xaccMoveCursor (reg->table, i, 0);

      sprintf (buff, "%2d/%2d/%4d", trans->date.day, 
                                    trans->date.month,
                                    trans->date.year);

      xaccSetBasicCellValue (reg->dateCell, buff);

      xaccSetBasicCellValue (reg->numCell, trans->num);
      xaccSetBasicCellValue (reg->descCell, trans->description);
      xaccSetBasicCellValue (reg->memoCell, split->memo);

      buff[0] = split->reconciled;
      buff[1] = 0x0;
      xaccSetBasicCellValue (reg->recnCell, buff);

      xaccCommitEdits (reg->table);
/*
      reg->PriceCell
*/
      i++;
      split = slist[i];
   }
/*
   xaccRefreshTable (reg->table);
*/
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

  xaccLoadRegister (reg, grp->account[0]->splits);

  xaccCreateTable (reg->table, actionform, "yodudue");

  XtManageChild (actionform);

  XtRealizeWidget(toplevel);
  XtRealizeWidget (reg->table->reg);

  XtAppMainLoop(app);

  return 0;
  }


