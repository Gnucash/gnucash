#ifndef EGG_ACCEL_DIALOG_H
#define EGG_ACCEL_DIALOG_H

#include <gtk/gtk.h>

#define EGG_TYPE_ACCEL_DIALOG            (egg_accel_dialog_get_type ())
#define EGG_ACCEL_DIALOG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TYPE_ACCEL_DIALOG, EggAccelDialog))
#define EGG_ACCEL_DIALOG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), EGG_TYPE_ACCEL_DIALOG, EggAccelDialogClass))
#define EGG_IS_ACCEL_DIALOG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TYPE_ACCEL_DIALOG))
#define EGG_IS_ACCEL_DIALOG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), EGG_TYPE_ACCEL_DIALOG))
#define EGG_ACCEL_DIALOG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), EGG_TYPE_ACCEL_DIALOG, EggAccelDialogClass))

typedef struct _EggAccelDialog      EggAccelDialog;
typedef struct _EggAccelDialogClass EggAccelDialogClass;

struct _EggAccelDialog {
  GtkDialog parent;

  GtkListStore *accel_store;

  GtkWidget *accel_view;

  GtkWidget *shift_toggle;
  GtkWidget *ctrl_toggle;
  GtkWidget *alt_toggle;
  GtkWidget *key_entry;

  GtkWidget *set_button;
  GtkWidget *reset_button;

  GtkWidget *ok_button;
};

struct _EggAccelDialogClass {
  GtkDialogClass parent_class;
};

GType      egg_accel_dialog_get_type      (void);
GtkWidget *egg_accel_dialog_new           (void);

void       egg_accel_dialog_rescan_accels (EggAccelDialog *accel_dialog);

#endif
