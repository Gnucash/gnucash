#ifndef EGG_ACTION_H
#define EGG_ACTION_H

#include <gtk/gtk.h>

#define EGG_TYPE_ACTION            (egg_action_get_type ())
#define EGG_ACTION(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TYPE_ACTION, EggAction))
#define EGG_ACTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), EGG_TYPE_ACTION, EggActionClass))
#define EGG_IS_ACTION(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TYPE_ACTION))
#define EGG_IS_ACTION_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), EGG_TYPE_ACTION))
#define EGG_ACTION_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), EGG_TYPE_ACTION, EggActionClass))

typedef struct _EggAction      EggAction;
typedef struct _EggActionClass EggActionClass;

struct _EggAction
{
  GObject object;

  gchar *name;
  gchar *label;
  gchar *short_label;
  gchar *tooltip;
  gchar *stock_id; /* icon */

  guint sensitive : 1;
  guint visible : 1;
  guint label_set : 1;       /* these two used so we can set label */
  guint short_label_set : 1; /* based on stock id */

  /* accelerator */
  GQuark accel_quark;

  /* list of proxy widgets */
  GSList *proxies;
};

struct _EggActionClass
{
  GObjectClass parent_class;

  /* activation signal */
  void       (* activate)           (EggAction    *action);

  GType      menu_item_type;
  GType      toolbar_item_type;

  /* widget creation routines (not signals) */
  GtkWidget *(* create_menu_item)   (EggAction *action);
  GtkWidget *(* create_tool_item)   (EggAction *action);
  void       (* connect_proxy)      (EggAction *action,
				     GtkWidget *proxy);
  void       (* disconnect_proxy)   (EggAction *action,
				     GtkWidget *proxy);
};

GType egg_action_get_type (void);

void       egg_action_activate           (EggAction *action);

GtkWidget *egg_action_create_icon        (EggAction *action,
					  GtkIconSize icon_size);
GtkWidget *egg_action_create_menu_item   (EggAction *action);
GtkWidget *egg_action_create_tool_item   (EggAction *action);
void       egg_action_connect_proxy      (EggAction *action,
					  GtkWidget *proxy);
void       egg_action_disconnect_proxy   (EggAction *action,
					  GtkWidget *proxy);

/* protected ... for use by child actions */
void egg_action_block_activate_from   (EggAction *action,
				       GtkWidget *proxy);
void egg_action_unblock_activate_from (EggAction *action,
				       GtkWidget *proxy);

/* protected ... for use by action groups */
void egg_action_set_accel_path (EggAction *action,
				const gchar *accel_path);


#endif
