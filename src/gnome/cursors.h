#ifndef CURSORS_H
#define CURSORS_H


#define GNC_CURSOR_NORMAL 0
#define GNC_CURSOR_BUSY       1
#define GNC_CURSOR_LAST       2

typedef struct 
{
	GdkCursor *cursor;
	int type;
} GncCursorDef;

	
extern GncCursorDef gnc_cursors[];

void gnc_ui_init_cursors (void);
void gnc_ui_shutdown_cursors (void);


#endif /* CURSORS_H */
