
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/MainW.h>

#include "price.h"
#include "table.h"

Table *
CreateReg(Widget parent ) {

   Table * table;
   CellBlock *curs, *header;
   SingleCell *cell;

   curs = xaccMallocCellBlock (2, 10);
   header = xaccMallocCellBlock (1, 10);

   cell = xaccMallocPriceCell();
   cell->row = 0;
   cell->col = 3;
   cell->width = 9;
   xaccAddCell (header, cell);
   
   cell = xaccMallocPriceCell();
   cell->row = 0;
   cell->col = 4;
   cell->width = 9;
   xaccAddCell (header, cell);
   

   table =  xaccMallocTable (0);
   table -> cursor = curs;
   table -> header = header;
   xaccInitTable (table, 15);

   xaccCreateTable (table, parent, "yodudue");
   return table;
}

main (int argc, char *argv[]) {
  Widget toplevel, mainwindow, actionform;
  XtAppContext app;
  Table * table;


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

  table = CreateReg (actionform);
  XtManageChild (actionform);

  XtRealizeWidget(toplevel);
  XtRealizeWidget (table->reg);

  XtAppMainLoop(app);

  return 0;
  }


