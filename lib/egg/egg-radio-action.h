#ifndef EGG_RADIO_ACTION_H
#define EGG_RADIO_ACTION_H

#include <gtk/gtk.h>
#include <egg-action.h>
#include <egg-toggle-action.h>

#define EGG_TYPE_RADIO_ACTION            (egg_radio_action_get_type ())
#define EGG_RADIO_ACTION(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TYPE_RADIO_ACTION, EggRadioAction))
#define EGG_RADIO_ACTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), EGG_TYPE_RADIO_ACTION, EggRadioActionClass))
#define EGG_IS_RADIO_ACTION(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TYPE_RADIO_ACTION))
#define EGG_IS_RADIO_ACTION_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), EGG_TYPE_RADIO_ACTION))
#define EGG_RADIO_ACTION_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), EGG_TYPE_RADIO_ACTION, EggRadioActionClass))

typedef struct _EggRadioAction      EggRadioAction;
typedef struct _EggRadioActionClass EggRadioActionClass;

struct _EggRadioAction {
  EggToggleAction parent;

  GSList *group;
};

struct _EggRadioActionClass {
  EggToggleActionClass parent_class;
};

GType    egg_radio_action_get_type  (void);

GSList  *egg_radio_action_get_group (EggRadioAction *action);
void     egg_radio_action_set_group (EggRadioAction *action,
				     GSList *group);

#endif
