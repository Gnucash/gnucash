
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/MainW.h>

#include "datecell.h"
#include "price.h"
#include "table.h"
#include "textcell.h"

#define DESC_CELL_C  2
#define DESC_CELL_R  0
#define MEMO_CELL_C  2
#define MEMO_CELL_R  1

Table *
CreateReg(Widget parent ) {

   Table * table;
   CellBlock *curs, *header;
   SingleCell *cell;

   header = xaccMallocCellBlock (1, 10);

   cell = xaccMallocDateCell();
   cell->width = 9;
   xaccAddCell (header, cell, 0, 0);
   
   cell = xaccMallocPriceCell();
   cell->width = 9;
   xaccAddCell (header, cell, 0, 3);
   
   cell = xaccMallocPriceCell();
   cell->width = 9;
   xaccAddCell (header, cell, 0, 4);

   cell = xaccMallocTextCell();
   cell->width = 9;
   xaccAddCell (header, cell, DESC_CELL_R, DESC_CELL_C);
   
   /* --------------------------- */
   curs = xaccMallocCellBlock (2, 10);
   
   cell = xaccMallocDateCell();
   cell->width = 9;
   xaccAddCell (curs, cell, 0, 0);
   
   cell = xaccMallocTextCell();
   cell->width = 9;
   xaccAddCell (curs, cell, DESC_CELL_R, DESC_CELL_C);
   
   cell = xaccMallocTextCell();
   cell->width = 9;
   xaccAddCell (curs, cell, MEMO_CELL_R, MEMO_CELL_C);

   cell = xaccMallocPriceCell();
   cell->width = 9;
   xaccAddCell (curs, cell, 0, 3);
   
   cell = xaccMallocPriceCell();
   cell->width = 9;
   xaccAddCell (curs, cell, 0, 4);
   
   

   table =  xaccMallocTable (0, 0);
   table -> header = header;
   xaccSetCursor (table, curs);
   xaccInitTable (table, 15, 1);

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


