#ifndef EGG_MENU_MERGE_H
#define EGG_MENU_MERGE_H

#include <glib.h>
#include <glib-object.h>

#include <egg-action.h>
#include <egg-action-group.h>

#define EGG_TYPE_MENU_MERGE            (egg_menu_merge_get_type ())
#define EGG_MENU_MERGE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TYPE_MENU_MERGE, EggMenuMerge))
#define EGG_MENU_MERGE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), EGG_TYPE_MENU_MERGE, EggMenuMergeClass))
#define EGG_IS_MENU_MERGE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TYPE_MENU_MERGE))
#define EGG_IS_MENU_MERGE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), EGG_TYPE_MENU_MERGE))
#define EGG_MENU_MERGE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), EGG_TYPE_MENU_MERGE, EggMenuMergeClass))

typedef struct _EggMenuMerge      EggMenuMerge;
typedef struct _EggMenuMergeClass EggMenuMergeClass;
typedef struct _EggMenuMergeNode  EggMenuMergeNode;

typedef enum {
  EGG_MENU_MERGE_UNDECIDED,
  EGG_MENU_MERGE_ROOT,
  EGG_MENU_MERGE_MENUBAR,
  EGG_MENU_MERGE_MENU,
  EGG_MENU_MERGE_TOOLBAR,
  EGG_MENU_MERGE_MENU_PLACEHOLDER,
  EGG_MENU_MERGE_TOOLBAR_PLACEHOLDER,
  EGG_MENU_MERGE_POPUPS,
  EGG_MENU_MERGE_MENUITEM,
  EGG_MENU_MERGE_TOOLITEM,
  EGG_MENU_MERGE_SEPARATOR,
} EggMenuMergeNodeType;

struct _EggMenuMerge {
  GObject parent;

  GtkAccelGroup *accel_group;

  GNode *root_node;
  GList *action_groups;

  guint last_merge_id;

  guint update_tag;
};

struct _EggMenuMergeClass {
  GObjectClass parent_class;

  void (* add_widget) (EggMenuMerge *merge, GtkWidget *widget);
  void (* remove_widget) (EggMenuMerge *merge, GtkWidget *widget);
};

struct _EggMenuMergeNode {
  EggMenuMergeNodeType type;

  const gchar *name;

  GQuark action_name;
  EggAction *action;
  GtkWidget *proxy;
  GtkWidget *extra; /*GtkMenu for submenus, second separator for placeholders*/

  GList *uifiles;

  guint dirty : 1;
};

GType         egg_menu_merge_get_type            (void);
EggMenuMerge *egg_menu_merge_new                 (void);

/* these two functions will dirty all merge nodes, as they may need to
 * be connected up to different actions */
void          egg_menu_merge_insert_action_group (EggMenuMerge *self,
						  EggActionGroup *action_group,
						  gint pos);
void          egg_menu_merge_remove_action_group (EggMenuMerge *self,
						  EggActionGroup*action_group);


GtkWidget    *egg_menu_merge_get_widget          (EggMenuMerge *self,
						  const gchar *path);

/* these two functions are for adding UI elements to the merged user
 * interface */
guint         egg_menu_merge_add_ui_from_string  (EggMenuMerge *self,
						  const gchar *buffer,
						  guint length,
						  GError **error);
guint         egg_menu_merge_add_ui_from_file    (EggMenuMerge *self,
						  const gchar *filename,
						  GError **error);
void          egg_menu_merge_remove_ui           (EggMenuMerge *self,
						  guint merge_id);

void          egg_menu_merge_ensure_update       (EggMenuMerge *self);


#endif /* EGG_MENU_MERGE_H */
