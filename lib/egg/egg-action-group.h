#ifndef EGG_ACTION_GROUP_H
#define EGG_ACTION_GROUP_H

#include <gtk/gtk.h>
#include <egg-action.h>

#define EGG_TYPE_ACTION_GROUP              (egg_action_group_get_type ())
#define EGG_ACTION_GROUP(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TYPE_ACTION_GROUP, EggActionGroup))
#define EGG_ACTION_GROUP_CLASS(vtable)     (G_TYPE_CHECK_CLASS_CAST ((vtable), EGG_TYPE_ACTION_GROUP, EggActionGroupClass))
#define EGG_IS_ACTION_GROUP(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TYPE_ACTION_GROUP))
#define EGG_IS_ACTION_GROUP_CLASS(vtable)  (G_TYPE_CHECK_CLASS_TYPE ((vtable), EGG_TYPE_ACTION_GROUP))
#define EGG_ACTION_GROUP_GET_CLASS(inst)   (G_TYPE_INSTANCE_GET_CLASS ((inst), EGG_TYPE_ACTION_GROUP, EggActionGroupClass))

typedef struct _EggActionGroup      EggActionGroup;
typedef struct _EggActionGroupClass EggActionGroupClass;
typedef struct _EggActionEntry      EggActionEntry;
typedef struct _EggToggleActionEntry  EggToggleActionEntry;
typedef struct _EggRadioActionEntry   EggRadioActionEntry;

struct _EggActionGroup
{
  GObject parent;

  gchar *name;
  GHashTable *actions;
};

struct _EggActionGroupClass
{
  GObjectClass parent_class;

  EggAction *(* get_action) (EggActionGroup *action_group,
			     const gchar *action_name);
};

struct _EggActionEntry {
  gchar *name;
  gchar *label;
  gchar *stock_id;
  gchar *accelerator;
  gchar *tooltip;

  GCallback callback;
};

struct _EggToggleActionEntry 
{
  gchar     *name;
  gchar     *stock_id;
  gchar     *label;
  gchar     *accelerator;
  gchar     *tooltip;
  GCallback  callback;
  gboolean   is_active;
};

struct _EggRadioActionEntry 
{
  gchar *name;
  gchar *stock_id;
  gchar *label;
  gchar *accelerator;
  gchar *tooltip;
  gint   value; 
};

GType           egg_action_group_get_type      (void);

EggActionGroup *egg_action_group_new           (const gchar *name);

const gchar    *egg_action_group_get_name      (EggActionGroup *action_group);
EggAction      *egg_action_group_get_action    (EggActionGroup *action_group,
						const gchar *action_name);
GList          *egg_action_group_list_actions  (EggActionGroup *action_group);
void            egg_action_group_add_action    (EggActionGroup *action_group,
						EggAction *action);
void            egg_action_group_remove_action (EggActionGroup *action_group,
						EggAction *action);

void            egg_action_group_add_actions   (EggActionGroup *action_group,
						EggActionEntry *entries,
						guint n_entries,
						gpointer user_data);
void            egg_action_group_add_toggle_actions      (EggActionGroup       *action_group,
							  EggToggleActionEntry *entries,
							  guint                 n_entries,
							  gpointer              user_data);
void            egg_action_group_add_radio_actions       (EggActionGroup       *action_group,
							  EggRadioActionEntry  *entries,
							  guint                 n_entries,
							  gint                  value,
							  GCallback             on_change,
							  gpointer              user_data);
void            egg_action_group_add_actions_full        (EggActionGroup       *action_group,
							  EggActionEntry       *entries,
							  guint                 n_entries,
							  gpointer              user_data,
							  GDestroyNotify        destroy);
void            egg_action_group_add_toggle_actions_full (EggActionGroup       *action_group,
							  EggToggleActionEntry *entries,
							  guint                 n_entries,
							  gpointer              user_data,
							  GDestroyNotify        destroy);
void            egg_action_group_add_radio_actions_full  (EggActionGroup       *action_group,
							  EggRadioActionEntry  *entries,
							  guint                 n_entries,
							  gint                  value,
							  GCallback             on_change,
							  gpointer              user_data,
							  GDestroyNotify        destroy);

#endif
