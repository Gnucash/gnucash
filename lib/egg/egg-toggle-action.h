#ifndef EGG_TOGGLE_ACTION_H
#define EGG_TOGGLE_ACTION_H

#include <gtk/gtk.h>
#include <egg-action.h>

#define EGG_TYPE_TOGGLE_ACTION            (egg_toggle_action_get_type ())
#define EGG_TOGGLE_ACTION(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TYPE_TOGGLE_ACTION, EggToggleAction))
#define EGG_TOGGLE_ACTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), EGG_TYPE_TOGGLE_ACTION, EggToggleActionClass))
#define EGG_IS_TOGGLE_ACTION(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TYPE_TOGGLE_ACTION))
#define EGG_IS_TOGGLE_ACTION_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), EGG_TYPE_TOGGLE_ACTION))
#define EGG_TOGGLE_ACTION_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), EGG_TYPE_TOGGLE_ACTION, EggToggleActionClass))

typedef struct _EggToggleAction      EggToggleAction;
typedef struct _EggToggleActionClass EggToggleActionClass;

struct _EggToggleAction
{
  EggAction parent;

  guint active : 1;
};

struct _EggToggleActionClass
{
  EggActionClass parent_class;

  void (* toggled) (EggToggleAction *action);
};

GType    egg_toggle_action_get_type   (void);

void     egg_toggle_action_toggled    (EggToggleAction *action);
void     egg_toggle_action_set_active (EggToggleAction *action,
				       gboolean is_active);
gboolean egg_toggle_action_get_active (EggToggleAction *action);

#endif
