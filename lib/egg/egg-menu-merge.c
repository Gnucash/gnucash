#include <string.h>
#include "egg-menu-merge.h"
#include "eggtoolbar.h"
#include "eggseparatortoolitem.h"
#include "eggintl.h"

#define NODE_INFO(node) ((EggMenuMergeNode *)node->data)
//#define NODE_INFO(node) ((Node *)node->data)

typedef struct {
  guint merge_id;
  GQuark action_quark;
} NodeUIReference;

typedef enum 
{
  NODE_TYPE_UNDECIDED,
  NODE_TYPE_ROOT,
  NODE_TYPE_MENUBAR,
  NODE_TYPE_MENU,
  NODE_TYPE_TOOLBAR,
  NODE_TYPE_MENU_PLACEHOLDER,
  NODE_TYPE_TOOLBAR_PLACEHOLDER,
  NODE_TYPE_POPUP,
  NODE_TYPE_MENUITEM,
  NODE_TYPE_TOOLITEM,
  NODE_TYPE_SEPARATOR,
  NODE_TYPE_ACCELERATOR
} NodeType;

typedef struct _Node Node;

struct _Node {
  NodeType type;

  gchar *name;

  GQuark action_name;
  EggAction *action;
  GtkWidget *proxy;
  GtkWidget *extra; /* second separator for placeholders */

  GList *uifiles;

  guint dirty : 1;
};

static void   egg_menu_merge_class_init    (EggMenuMergeClass *class);
static void   egg_menu_merge_init          (EggMenuMerge *merge);

static void   egg_menu_merge_queue_update  (EggMenuMerge *self);
static void   egg_menu_merge_dirty_all     (EggMenuMerge *self);

static GNode *get_child_node               (EggMenuMerge *self, GNode *parent,
					    const gchar *childname,
					    gint childname_length,
					    EggMenuMergeNodeType node_type,
					    gboolean create, gboolean top);
static GNode *egg_menu_merge_get_node      (EggMenuMerge *self,
					    const gchar *path,
					    EggMenuMergeNodeType node_type,
					    gboolean create);
static guint egg_menu_merge_next_merge_id  (EggMenuMerge *self);

static void  egg_menu_merge_node_prepend_ui_reference (EggMenuMergeNode *node,
						       guint merge_id,
						       GQuark action_quark);
static void  egg_menu_merge_node_remove_ui_reference  (EggMenuMergeNode *node,
						       guint merge_id);

enum {
  ADD_WIDGET,
  REMOVE_WIDGET,
  LAST_SIGNAL
};

static guint merge_signals[LAST_SIGNAL] = { 0 };

static GMemChunk *merge_node_chunk = NULL;

GType
egg_menu_merge_get_type (void)
{
  static GtkType type = 0;

  if (!type)
    {
      static const GTypeInfo type_info =
      {
        sizeof (EggMenuMergeClass),
        (GBaseInitFunc) NULL,
        (GBaseFinalizeFunc) NULL,
        (GClassInitFunc) egg_menu_merge_class_init,
        (GClassFinalizeFunc) NULL,
        NULL,
        
        sizeof (EggMenuMerge),
        0, /* n_preallocs */
        (GInstanceInitFunc) egg_menu_merge_init,
      };

      type = g_type_register_static (G_TYPE_OBJECT,
				     "EggMenuMerge",
				     &type_info, 0);
    }
  return type;
}

static void
egg_menu_merge_class_init (EggMenuMergeClass *class)
{
  if (!merge_node_chunk)
    merge_node_chunk = g_mem_chunk_create(EggMenuMergeNode, 64,
					  G_ALLOC_AND_FREE);

  merge_signals[ADD_WIDGET] =
    g_signal_new ("add_widget",
		  G_OBJECT_CLASS_TYPE (class),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_NO_RECURSE,
		  G_STRUCT_OFFSET (EggMenuMergeClass, add_widget), NULL, NULL,
		  g_cclosure_marshal_VOID__OBJECT,
		  G_TYPE_NONE, 1,
		  GTK_TYPE_WIDGET);
  merge_signals[REMOVE_WIDGET] =
    g_signal_new ("remove_widget",
		  G_OBJECT_CLASS_TYPE (class),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_NO_RECURSE,
		  G_STRUCT_OFFSET (EggMenuMergeClass, remove_widget), NULL, NULL,
		  g_cclosure_marshal_VOID__OBJECT,
		  G_TYPE_NONE, 1,
		  GTK_TYPE_WIDGET);  
  
}


static void
egg_menu_merge_init (EggMenuMerge *self)
{
  guint merge_id;
  GNode *node;

  self->accel_group = gtk_accel_group_new();

  self->root_node = NULL;
  self->action_groups = NULL;

  self->last_merge_id = 0;


  merge_id = egg_menu_merge_next_merge_id(self);
  node = get_child_node(self, NULL, "ui", 4,
			EGG_MENU_MERGE_ROOT, TRUE, FALSE);
  egg_menu_merge_node_prepend_ui_reference(NODE_INFO(node), merge_id, 0);
}

EggMenuMerge *
egg_menu_merge_new (void)
{
  return g_object_new(EGG_TYPE_MENU_MERGE, NULL);
}

void
egg_menu_merge_insert_action_group (EggMenuMerge *self,
				 EggActionGroup *action_group, gint pos)
{
  g_return_if_fail (EGG_IS_MENU_MERGE(self));
  g_return_if_fail (EGG_IS_ACTION_GROUP(action_group));
  g_return_if_fail (g_list_find(self->action_groups, action_group) == NULL);

  g_object_ref(action_group);
  self->action_groups = g_list_insert(self->action_groups, action_group, pos);

  /* dirty all nodes, as action bindings may change */
  egg_menu_merge_dirty_all(self);
}

void
egg_menu_merge_remove_action_group (EggMenuMerge *self,
				    EggActionGroup *action_group)
{
  g_return_if_fail (EGG_IS_MENU_MERGE(self));
  g_return_if_fail (EGG_IS_ACTION_GROUP(action_group));
  g_return_if_fail (g_list_find(self->action_groups, action_group) != NULL);

  self->action_groups = g_list_remove(self->action_groups, action_group);
  g_object_unref(action_group);

  /* dirty all nodes, as action bindings may change */
  egg_menu_merge_dirty_all(self);
}

GtkWidget *
egg_menu_merge_get_widget (EggMenuMerge *self, const gchar *path)
{
  GNode *node;

  /* ensure that there are no pending updates before we get the
   * widget */
  egg_menu_merge_ensure_update(self);

  node = egg_menu_merge_get_node(self, path, EGG_MENU_MERGE_UNDECIDED, FALSE);
  return NODE_INFO(node)->proxy;
}

static GNode *
get_child_node(EggMenuMerge *self, GNode *parent,
	       const gchar *childname, gint childname_length,
	       EggMenuMergeNodeType node_type,
	       gboolean create, gboolean top)
{
  GNode *child = NULL;

  g_return_val_if_fail(parent == NULL ||
	       (NODE_INFO(parent)->type != EGG_MENU_MERGE_MENUITEM &&
		NODE_INFO(parent)->type != EGG_MENU_MERGE_TOOLITEM), NULL);

  if (parent)
    {
      if (childname)
	{
	  for (child = parent->children; child != NULL; child = child->next)
	    {
	      if (strlen(NODE_INFO(child)->name) == childname_length &&
		  !strncmp(NODE_INFO(child)->name, childname, childname_length))
		{
		  /* if undecided about node type, set it */
		  if (NODE_INFO(child)->type == EGG_MENU_MERGE_UNDECIDED)
		    NODE_INFO(child)->type = node_type;
		  
		  /* warn about type mismatch */
		  if (node_type != EGG_MENU_MERGE_UNDECIDED &&
		      NODE_INFO(child)->type != node_type)
		    g_warning("node type doesn't match %d (%s is type %d)",
			      node_type, NODE_INFO(child)->name,
			      NODE_INFO(child)->type);
		  
		  return child;
		}
	    }
	}
      if (!child && create)
	{
	  EggMenuMergeNode *mnode;

	  mnode = g_chunk_new0(EggMenuMergeNode, merge_node_chunk);
	  mnode->type = node_type;
	  mnode->name = g_strndup(childname, childname_length);
	  mnode->dirty = TRUE;

	  if (top)
	    child = g_node_prepend_data(parent, mnode);
	  else
	    child = g_node_append_data(parent, mnode);
	}
    }
  else
    {
      /* handle root node */
      if (self->root_node)
	{
	  child = self->root_node;
	  if (strncmp(NODE_INFO(child)->name, childname, childname_length) !=0)
	    g_warning("root node name '%s' doesn't match '%s'",
		      childname, NODE_INFO(child)->name);
	  if (NODE_INFO(child)->type != EGG_MENU_MERGE_ROOT)
	    g_warning("base element must be of type ROOT");
	}
      else if (create)
	{
	  EggMenuMergeNode *mnode;

	  mnode = g_chunk_new0(EggMenuMergeNode, merge_node_chunk);
	  mnode->type = node_type;
	  mnode->name = g_strndup(childname, childname_length);
	  mnode->dirty = TRUE;
	  
	  child = self->root_node = g_node_new(mnode);
	}
    }

  return child;
}

static GNode *
egg_menu_merge_get_node(EggMenuMerge *self, const gchar *path,
			EggMenuMergeNodeType node_type, gboolean create)
{
  const gchar *pos, *end;
  GNode *parent, *node;

  end = path + strlen(path);
  pos = path;
  parent = node = NULL;
  while (pos < end)
    {
      const gchar *slash;
      gsize length;

      slash = strchr(pos, '/');
      if (slash)
	length = slash - pos;
      else
	length = strlen(pos);

      node = get_child_node(self, parent, pos, length, EGG_MENU_MERGE_UNDECIDED,
			    create, FALSE);
      if (!node)
	return NULL;

      pos += length + 1; /* move past the node name and the slash too */
      parent = node;
    }

  if (NODE_INFO(node)->type == EGG_MENU_MERGE_UNDECIDED)
    NODE_INFO(node)->type = node_type;
  return node;
}

guint
egg_menu_merge_new_merge_id( EggMenuMerge *self )
{
  return egg_menu_merge_next_merge_id( self );
}

static guint
egg_menu_merge_next_merge_id (EggMenuMerge *self)
{
  self->last_merge_id++;

  return self->last_merge_id;
}

static void
egg_menu_merge_node_prepend_ui_reference (EggMenuMergeNode *node,
					  guint merge_id, GQuark action_quark)
{
  NodeUIReference *reference;

  reference = g_new (NodeUIReference, 1);
  reference->action_quark = action_quark;
  reference->merge_id = merge_id;

  /* Prepend the reference */
  node->uifiles = g_list_prepend (node->uifiles, reference);

  node->dirty = TRUE;
}

static void
egg_menu_merge_node_remove_ui_reference (EggMenuMergeNode *node,
					 guint merge_id)
{
  GList *p;

  for (p = node->uifiles; p != NULL; p = p->next)
    {
      NodeUIReference *reference = p->data;
      
      if (reference->merge_id == merge_id)
	{
	  node->uifiles = g_list_remove_link (node->uifiles, p);
	  node->dirty = TRUE;
	  g_free (reference);

	  break;
	}
    }
}

/* -------------------- The UI file parser -------------------- */

typedef enum {
  STATE_START,
  STATE_ROOT,
  STATE_MENU,
  STATE_TOOLBAR,
  STATE_MENUITEM,
  STATE_TOOLITEM,
  STATE_END
} ParseState;

typedef struct _ParseContext ParseContext;
struct _ParseContext
{
  ParseState state;
  ParseState prev_state;

  EggMenuMerge *self;

  GNode *current;

  guint merge_id;
};

static void
start_element_handler (GMarkupParseContext *context,
		       const gchar *element_name,
		       const gchar **attribute_names,
		       const gchar **attribute_values,
		       gpointer user_data,
		       GError **error)
{
  ParseContext *ctx = user_data;
  EggMenuMerge *self = ctx->self;

  gint i;
  const gchar *node_name;
  const gchar *action;
  GQuark action_quark;
  gboolean top;

  gboolean raise_error = TRUE;

  node_name = NULL;
  action = NULL;
  action_quark = 0;
  top = FALSE;
  for (i = 0; attribute_names[i] != NULL; i++)
    {
      if (!strcmp(attribute_names[i], "name"))
	{
	  node_name = attribute_values[i];
	}
      else if (!strcmp(attribute_names[i], "action"))
	{
	  action = attribute_values[i];
	  action_quark = g_quark_from_string(attribute_values[i]);
	}
      else if (!strcmp(attribute_names[i], "position"))
	{
	  top = !strcmp(attribute_values[i], "top");
	}
      else
	{
	  gint line_number, char_number;
	  
	  g_markup_parse_context_get_position (context,
					       &line_number, &char_number);
	  g_set_error (error,
		       G_MARKUP_ERROR,
		       G_MARKUP_ERROR_UNKNOWN_ATTRIBUTE,
		       _("Unknown attribute '%s' on line %d char %d"),
		       attribute_names[i],
		       line_number, char_number);
	  return;
	}
    }

  /* Work out a name for this node.  Either the name attribute, or
   * the action, or the element name */
  if (node_name == NULL) 
    {
      if (action != NULL)
	node_name = action;
      else 
	node_name = element_name;
    }

  switch (element_name[0])
    {
    case 'u':
      if (ctx->state == STATE_START && !strcmp(element_name, "ui"))
	{
	  ctx->state = STATE_ROOT;
	  ctx->current = self->root_node;
	  raise_error = FALSE;

	  egg_menu_merge_node_prepend_ui_reference (NODE_INFO (ctx->current),
						    ctx->merge_id, action_quark);
	}
      break;
    case 'm':
      if (ctx->state == STATE_ROOT && !strcmp(element_name, "menubar"))
	{
	  ctx->state = STATE_MENU;
	  ctx->current = get_child_node(self, ctx->current,
					node_name, strlen(node_name),
					EGG_MENU_MERGE_MENUBAR,
					TRUE, FALSE);
	  if (NODE_INFO(ctx->current)->action_name == 0)
	    NODE_INFO(ctx->current)->action_name = action_quark;

	  egg_menu_merge_node_prepend_ui_reference (NODE_INFO (ctx->current),
						    ctx->merge_id, action_quark);
	  NODE_INFO(ctx->current)->dirty = TRUE;

	  raise_error = FALSE;
	}
      else if (ctx->state == STATE_MENU && !strcmp(element_name, "menu"))
	{
	  ctx->current = get_child_node(self, ctx->current,
					node_name, strlen(node_name),
					EGG_MENU_MERGE_MENU,
					TRUE, top);
	  if (NODE_INFO(ctx->current)->action_name == 0)
	    NODE_INFO(ctx->current)->action_name = action_quark;

	  egg_menu_merge_node_prepend_ui_reference (NODE_INFO (ctx->current),
						    ctx->merge_id, action_quark);
	  NODE_INFO(ctx->current)->dirty = TRUE;

	  raise_error = FALSE;
	}
      else if (ctx->state == STATE_MENU && !strcmp(element_name, "menuitem"))
	{
	  GNode *node;

	  ctx->state = STATE_MENUITEM;
	  node = get_child_node(self, ctx->current,
				node_name, strlen(node_name),
				EGG_MENU_MERGE_MENUITEM,
				TRUE, top);
	  if (NODE_INFO(node)->action_name == 0)
	    NODE_INFO(node)->action_name = action_quark;

	  egg_menu_merge_node_prepend_ui_reference (NODE_INFO (node),
						    ctx->merge_id, action_quark);
	  NODE_INFO(node)->dirty = TRUE;

	  raise_error = FALSE;
	}
      break;
    case 'p':
      if (ctx->state == STATE_ROOT && !strcmp(element_name, "popup"))
	{
	  ctx->state = STATE_MENU;
	  ctx->current = get_child_node(self, ctx->current,
					node_name, strlen(node_name),
					EGG_MENU_MERGE_POPUP,
					TRUE, FALSE);
	  if (NODE_INFO(ctx->current)->action_name == 0)
	    NODE_INFO(ctx->current)->action_name = action_quark;

	  egg_menu_merge_node_prepend_ui_reference (NODE_INFO (ctx->current),
						    ctx->merge_id, action_quark);
	  NODE_INFO(ctx->current)->dirty = TRUE;

	  raise_error = FALSE;
	}
      else if ((ctx->state == STATE_MENU || ctx->state == STATE_TOOLBAR) &&
	       !strcmp(element_name, "placeholder"))
	{
	  if (ctx->state == STATE_TOOLBAR)
	    ctx->current = get_child_node(self, ctx->current,
					  node_name, strlen(node_name),
					  EGG_MENU_MERGE_TOOLBAR_PLACEHOLDER,
					  TRUE, top);
	  else
	    ctx->current = get_child_node(self, ctx->current,
					  node_name, strlen(node_name),
					  EGG_MENU_MERGE_MENU_PLACEHOLDER,
					  TRUE, top);

	  egg_menu_merge_node_prepend_ui_reference (NODE_INFO (ctx->current),
						    ctx->merge_id, action_quark);
	  NODE_INFO(ctx->current)->dirty = TRUE;

	  raise_error = FALSE;
	}
      break;
    case 's':
      if ((ctx->state == STATE_MENU || ctx->state == STATE_TOOLBAR) &&
	       !strcmp(element_name, "separator"))
	{
	  GNode *node;

	  if (ctx->state == STATE_TOOLBAR)
	    ctx->state = STATE_TOOLITEM;
	  else
	    ctx->state = STATE_MENUITEM;
	  node = get_child_node(self, ctx->current,
				node_name, strlen(node_name),
				EGG_MENU_MERGE_SEPARATOR,
				TRUE, top);
	  if (NODE_INFO(node)->action_name == 0)
	    NODE_INFO(node)->action_name = action_quark;

	  egg_menu_merge_node_prepend_ui_reference (NODE_INFO (node),
						    ctx->merge_id, action_quark);
	  NODE_INFO(node)->dirty = TRUE;

	  raise_error = FALSE;
	}
      break;
    case 't':
      if (ctx->state == STATE_ROOT && !strcmp(element_name, "toolbar"))
	{
	  ctx->state = STATE_TOOLBAR;
	  ctx->current = get_child_node(self, ctx->current,
					node_name, strlen(node_name),
					EGG_MENU_MERGE_TOOLBAR,
					TRUE, FALSE);
	  if (NODE_INFO(ctx->current)->action_name == 0)
	    NODE_INFO(ctx->current)->action_name = action_quark;

	  egg_menu_merge_node_prepend_ui_reference (NODE_INFO (ctx->current),
						    ctx->merge_id, action_quark);
	  NODE_INFO(ctx->current)->dirty = TRUE;

	  raise_error = FALSE;
	}
      else if (ctx->state == STATE_TOOLBAR && !strcmp(element_name, "toolitem"))
	{
	  GNode *node;

	  ctx->state = STATE_TOOLITEM;
	  node = get_child_node(self, ctx->current,
				node_name, strlen(node_name),
				EGG_MENU_MERGE_TOOLITEM,
				TRUE, top);
	  if (NODE_INFO(node)->action_name == 0)
	    NODE_INFO(node)->action_name = action_quark;

	  egg_menu_merge_node_prepend_ui_reference (NODE_INFO (node),
						    ctx->merge_id, action_quark);
	  NODE_INFO(node)->dirty = TRUE;

	  raise_error = FALSE;
	}
      break;
    default:
      break;
    }
  if (raise_error)
    {
      gint line_number, char_number;
 
      g_markup_parse_context_get_position (context,
					   &line_number, &char_number);
      g_set_error (error,
		   G_MARKUP_ERROR,
		   G_MARKUP_ERROR_UNKNOWN_ELEMENT,
		   _("Unknown tag '%s' on line %d char %d"),
		   element_name,
		   line_number, char_number);
    }
}

static void
end_element_handler (GMarkupParseContext *context,
		     const gchar *element_name,
		     gpointer user_data,
		     GError **error)
{
  ParseContext *ctx = user_data;
  EggMenuMerge *self = ctx->self;

  //g_message("ending element %s (state=%d)", element_name, ctx->state);

  switch (ctx->state)
    {
    case STATE_START:
      g_warning("shouldn't get any end tags in start state");
      /* should we GError here? */
      break;
    case STATE_ROOT:
      if (ctx->current != self->root_node)
	g_warning("we are in STATE_ROOT, but the current node isn't the root");
      ctx->current = NULL;
      ctx->state = STATE_END;
      break;
    case STATE_MENU:
      ctx->current = ctx->current->parent;
      if (NODE_INFO(ctx->current)->type == EGG_MENU_MERGE_ROOT) /* menubar */
	ctx->state = STATE_ROOT;
      /* else, stay in STATE_MENU state */
      break;
    case STATE_TOOLBAR:
      ctx->current = ctx->current->parent;
      /* we conditionalise this test, in case we are closing off a
       * placeholder */
      if (NODE_INFO(ctx->current)->type == EGG_MENU_MERGE_ROOT)
	ctx->state = STATE_ROOT;
      /* else, stay in STATE_TOOLBAR state */
      break;
    case STATE_MENUITEM:
      ctx->state = STATE_MENU;
      break;
    case STATE_TOOLITEM:
      ctx->state = STATE_TOOLBAR;
      break;
    case STATE_END:
      g_warning("shouldn't get any end tags at this point");
      /* should do an error here */
      break;
    }
}

static void
cleanup (GMarkupParseContext *context,
	 GError *error,
	 gpointer user_data)
{
  ParseContext *ctx = user_data;
  EggMenuMerge *self = ctx->self;

  ctx->current = NULL;
  /* should also walk through the tree and get rid of nodes related to
   * this UI file's tag */

  egg_menu_merge_remove_ui (self, ctx->merge_id);
}

static GMarkupParser ui_parser = {
  start_element_handler,
  end_element_handler,
  NULL,
  NULL,
  cleanup
};

guint
egg_menu_merge_add_ui_from_string (EggMenuMerge *self,
				   const gchar *buffer, guint length,
				   GError **error)
{
  ParseContext ctx = { 0 };
  GMarkupParseContext *context;
  gboolean res = TRUE;

  g_return_val_if_fail(EGG_IS_MENU_MERGE(self), FALSE);
  g_return_val_if_fail(buffer != NULL, FALSE);

  ctx.state = STATE_START;
  ctx.self = self;
  ctx.current = NULL;
  ctx.merge_id = egg_menu_merge_next_merge_id (self);

  context = g_markup_parse_context_new(&ui_parser, 0, &ctx, NULL);
  if (length < 0)
    length = strlen(buffer);

  if (g_markup_parse_context_parse(context, buffer, length, error))
    {
      if (!g_markup_parse_context_end_parse(context, error))
	res = FALSE;
    }
  else
    res = FALSE;

  g_markup_parse_context_free (context);

  egg_menu_merge_queue_update(self);

  if (res)
    return ctx.merge_id;
  return 0;
}

guint
egg_menu_merge_add_ui_from_file (EggMenuMerge *self,
				 const gchar *filename,
				 GError **error)
{
  gchar *buffer;
  gint length;
  guint res;

  if (!g_file_get_contents (filename, &buffer, &length, error))
    return 0;

  res = egg_menu_merge_add_ui_from_string(self, buffer, length, error);
  g_free(buffer);

  return res;
}

/**
 * gtk_ui_manager_add_ui:
 * @self: a #GtkUIManager
 * @merge_id: the merge id for the merged UI, see gtk_ui_manager_new_merge_id()
 * @path: a path
 * @name: the name for the added UI element
 * @action: the name of the action to be proxied, or %NULL to add a separator
 * @type: the type of UI element to add.
 * @top: if %TRUE, the UI element is added before its siblings, otherwise it 
 *   is added after its siblings.
 * 
 * Adds a UI element to the current contents of @self. 
 *
 * If @type is %GTK_UI_MANAGER_AUTO, GTK+ inserts a menuitem, toolitem or 
 * separator if such an element can be inserted at the place determined by 
 * @path. Otherwise @type must indicate an element that can be inserted at 
 * the place determined by @path.
 * 
 * Since: 2.4
 **/
void
egg_menu_merge_add_ui (EggMenuMerge        *self,
		       guint                merge_id,
		       const gchar         *path,
		       const gchar         *name,
		       const gchar         *action,
		       EggMenuMergeNodeType type,
		       gboolean             top)
{
  GNode *node;
  GNode *child;
  EggMenuMergeNodeType node_type;
  GQuark action_quark = 0;

  g_return_if_fail (EGG_IS_MENU_MERGE (self));  
  g_return_if_fail (merge_id > 0);
  g_return_if_fail (name != NULL);

  node = egg_menu_merge_get_node (self, path, NODE_TYPE_UNDECIDED, FALSE);
  
  if (node == NULL)
    return;

  node_type = NODE_TYPE_UNDECIDED;

  switch (NODE_INFO (node)->type) 
    {
    case NODE_TYPE_MENUBAR:
    case NODE_TYPE_MENU:
    case NODE_TYPE_POPUP:
    case NODE_TYPE_MENU_PLACEHOLDER:
      switch (type) 
	{
	case EGG_MENU_MERGE_MENU:
	  node_type = NODE_TYPE_MENU;
	  break;
	case EGG_MENU_MERGE_MENUITEM:
	  node_type = NODE_TYPE_MENUITEM;
	  break;
	case EGG_MENU_MERGE_SEPARATOR:
	  node_type = NODE_TYPE_SEPARATOR;
	  break;
	case EGG_MENU_MERGE_MENU_PLACEHOLDER:
	  node_type = NODE_TYPE_MENU_PLACEHOLDER;
	  break;
	case EGG_MENU_MERGE_TOOLBAR_PLACEHOLDER:
	  node_type = NODE_TYPE_TOOLBAR_PLACEHOLDER;
	  break;
	default: ;
	  /* do nothing */
	}
      break;
    case NODE_TYPE_TOOLBAR:
    case NODE_TYPE_TOOLBAR_PLACEHOLDER:
      switch (type) 
	{
          /*case EGG_MENU_MERGE_AUTO:
	  if (action != NULL)
	      node_type = NODE_TYPE_TOOLITEM;
	  else
	      node_type = NODE_TYPE_SEPARATOR;
	  break;
          */
	case EGG_MENU_MERGE_TOOLITEM:
	  node_type = NODE_TYPE_TOOLITEM;
	  break;
	case EGG_MENU_MERGE_SEPARATOR:
	  node_type = NODE_TYPE_SEPARATOR;
	  break;
	case EGG_MENU_MERGE_MENU_PLACEHOLDER:
	  node_type = NODE_TYPE_MENU_PLACEHOLDER;
	  break;
	case EGG_MENU_MERGE_TOOLBAR_PLACEHOLDER:
	  node_type = NODE_TYPE_TOOLBAR_PLACEHOLDER;
	  break;
	default: ;
	  /* do nothing */
	}
      break;
    case NODE_TYPE_ROOT:
      switch (type) 
	{
	case EGG_MENU_MERGE_MENUBAR:
	  node_type = NODE_TYPE_MENUBAR;
	  break;
	case EGG_MENU_MERGE_TOOLBAR:
	  node_type = NODE_TYPE_TOOLBAR;
	  break;
	case EGG_MENU_MERGE_POPUP:
	  node_type = NODE_TYPE_POPUP;
	  break;
          /*case EGG_MENU_MERGE_ACCELERATOR:
	  node_type = NODE_TYPE_ACCELERATOR;
	  break;*/
	default: ;
	  /* do nothing */
	}
      break;
    default: ;
      /* do nothing */
    }

  if (node_type == NODE_TYPE_UNDECIDED)
    return;
   
  child = get_child_node (self, node,
			  name, strlen (name),
			  node_type, TRUE, top);

  if (action != NULL)
    action_quark = g_quark_from_string (action);

  egg_menu_merge_node_prepend_ui_reference (NODE_INFO (child), 
                                            merge_id, action_quark);

  if (NODE_INFO (node)->action_name == 0)
    NODE_INFO (child)->action_name = action_quark;

  NODE_INFO (child)->dirty = TRUE;

  egg_menu_merge_queue_update (self);

  g_object_notify (G_OBJECT (self), "ui");      
}


static gboolean
remove_ui (GNode *node, gpointer user_data)
{
  guint merge_id = GPOINTER_TO_UINT (user_data);

  egg_menu_merge_node_remove_ui_reference (NODE_INFO (node), merge_id);

  return FALSE; /* continue */
}

void
egg_menu_merge_remove_ui (EggMenuMerge *self, guint merge_id)
{
  g_node_traverse(self->root_node, G_POST_ORDER, G_TRAVERSE_ALL, -1,
		  remove_ui, GUINT_TO_POINTER (merge_id));

  egg_menu_merge_queue_update(self);
}

/* -------------------- Updates -------------------- */


static EggAction *
get_action_by_name (EggMenuMerge *merge, const char *action_name)
{
  GList *tmp;

  if (!action_name)
    return NULL;
  
  /* lookup name */
  for (tmp = merge->action_groups; tmp != NULL; tmp = tmp->next)
    {
      EggActionGroup *action_group = tmp->data;
      EggAction *action;
      
      action = egg_action_group_get_action (action_group, action_name);

      if (action)
	return action;
    }

  return NULL;
}

static gboolean
find_menu_position (GNode *node, GtkWidget **menushell_p, gint *pos_p)
{
  GtkWidget *menushell;
  gint pos;

  g_return_val_if_fail(node != NULL, FALSE);
  g_return_val_if_fail(NODE_INFO(node)->type == EGG_MENU_MERGE_MENU ||
		       NODE_INFO(node)->type == EGG_MENU_MERGE_POPUP ||
		       NODE_INFO(node)->type == EGG_MENU_MERGE_MENU_PLACEHOLDER ||
		       NODE_INFO(node)->type == EGG_MENU_MERGE_MENUITEM ||
		       NODE_INFO(node)->type == EGG_MENU_MERGE_SEPARATOR,
		       FALSE);

  /* first sibling -- look at parent */
  if (node->prev == NULL)
    {
      GNode *parent;

      parent = node->parent;
      switch (NODE_INFO(parent)->type)
	{
	case EGG_MENU_MERGE_MENUBAR:
	case EGG_MENU_MERGE_POPUP:
	  menushell = NODE_INFO(parent)->proxy;
	  pos = 0;
	  break;
	case EGG_MENU_MERGE_MENU:
	  menushell = NODE_INFO(parent)->proxy;
	  if (GTK_IS_MENU_ITEM(menushell))
	    menushell = gtk_menu_item_get_submenu(GTK_MENU_ITEM(menushell));
	  pos = 0;
	  break;
	case EGG_MENU_MERGE_MENU_PLACEHOLDER:
	  menushell = gtk_widget_get_parent(NODE_INFO(parent)->proxy);
	  g_return_val_if_fail(GTK_IS_MENU_SHELL(menushell), FALSE);
	  pos = g_list_index(GTK_MENU_SHELL(menushell)->children,
			     NODE_INFO(parent)->proxy) + 1;
	  break;
	default:
	  g_warning("%s: bad parent node type %d", G_STRLOC,
		    NODE_INFO(parent)->type);
	  return FALSE;
	}
    }
  else
    {
      GtkWidget *prev_child;
      GNode *sibling;

      sibling = node->prev;
      if (NODE_INFO(sibling)->type == EGG_MENU_MERGE_MENU_PLACEHOLDER)
	prev_child = NODE_INFO(sibling)->extra; /* second Separator */
      else
	prev_child = NODE_INFO(sibling)->proxy;

      g_return_val_if_fail(GTK_IS_WIDGET(prev_child), FALSE);
      menushell = gtk_widget_get_parent(prev_child);
      g_return_val_if_fail(GTK_IS_MENU_SHELL(menushell), FALSE);

      pos = g_list_index(GTK_MENU_SHELL(menushell)->children, prev_child) + 1;
    }

  if (menushell_p)
    *menushell_p = menushell;
  if (pos_p)
    *pos_p = pos;

  return TRUE;
}

static gboolean
find_toolbar_position (GNode *node, GtkWidget **toolbar_p, gint *pos_p)
{
  GtkWidget *toolbar;
  gint pos;

  g_return_val_if_fail(node != NULL, FALSE);
  g_return_val_if_fail(NODE_INFO(node)->type == EGG_MENU_MERGE_TOOLBAR ||
		       NODE_INFO(node)->type == EGG_MENU_MERGE_TOOLBAR_PLACEHOLDER ||
		       NODE_INFO(node)->type == EGG_MENU_MERGE_TOOLITEM ||
		       NODE_INFO(node)->type == EGG_MENU_MERGE_SEPARATOR,
		       FALSE);

  /* first sibling -- look at parent */
  if (node->prev == NULL)
    {
      GNode *parent;

      parent = node->parent;
      switch (NODE_INFO(parent)->type)
	{
	case EGG_MENU_MERGE_TOOLBAR:
	  toolbar = NODE_INFO(parent)->proxy;
	  pos = 0;
	  break;
	case EGG_MENU_MERGE_TOOLBAR_PLACEHOLDER:
	  toolbar = gtk_widget_get_parent(NODE_INFO(parent)->proxy);
	  g_return_val_if_fail(EGG_IS_TOOLBAR(toolbar), FALSE);
	  pos = egg_toolbar_get_item_index (EGG_TOOLBAR(toolbar),
					    EGG_TOOL_ITEM (NODE_INFO(parent)->proxy)) + 1;
	  break;
	default:
	  g_warning("%s: bad parent node type %d", G_STRLOC,
		    NODE_INFO(parent)->type);
	  return FALSE;
	}
    }
  else
    {
      GtkWidget *prev_child;
      GNode *sibling;

      sibling = node->prev;
      if (NODE_INFO(sibling)->type == EGG_MENU_MERGE_TOOLBAR_PLACEHOLDER)
	prev_child = NODE_INFO(sibling)->extra; /* second Separator */
      else
	prev_child = NODE_INFO(sibling)->proxy;

      g_return_val_if_fail(GTK_IS_WIDGET(prev_child), FALSE);
      toolbar = gtk_widget_get_parent(prev_child);
      g_return_val_if_fail(EGG_IS_TOOLBAR(toolbar), FALSE);

      pos = egg_toolbar_get_item_index (EGG_TOOLBAR(toolbar),
					EGG_TOOL_ITEM (prev_child)) + 1;
    }

  if (toolbar_p)
    *toolbar_p = toolbar;
  if (pos_p)
    *pos_p = pos;

  return TRUE;
}

enum {
  SEPARATOR_MODE_SMART,
  SEPARATOR_MODE_VISIBLE,
  SEPARATOR_MODE_HIDDEN
};

static void
update_smart_separators (GtkWidget *proxy)
{
  GtkWidget *parent = NULL;

  if (GTK_IS_MENU (proxy) || GTK_IS_TOOLBAR (proxy))
    parent = proxy;
  else if (GTK_IS_MENU_ITEM (proxy) || EGG_IS_TOOL_ITEM (proxy))
    parent = gtk_widget_get_parent (proxy);

  if (parent) 
    {
      gboolean visible;
      GList *children, *cur, *last;

      children = gtk_container_get_children (GTK_CONTAINER (parent));
      
      visible = FALSE;
      last = NULL;
      cur = children;
      while (cur) 
	{
	  if (GTK_IS_SEPARATOR_MENU_ITEM (cur->data) ||
	      EGG_IS_SEPARATOR_TOOL_ITEM (cur->data))
	    {
	      gint mode = 
		GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cur->data), 
						    "gtk-separator-mode"));
	      switch (mode) 
		{
		case SEPARATOR_MODE_VISIBLE:
		  gtk_widget_show (GTK_WIDGET (cur->data));
		  last = NULL;
		  visible = FALSE;
		  break;
		case SEPARATOR_MODE_HIDDEN:
		  gtk_widget_hide (GTK_WIDGET (cur->data));
		  break;
		case SEPARATOR_MODE_SMART:
		  if (visible)
		    {
		      gtk_widget_show (GTK_WIDGET (cur->data));
		      last = cur;
		      visible = FALSE;
		    }
		  else
		      gtk_widget_hide (GTK_WIDGET (cur->data));
		  break;
		}
	    }
	  else if (GTK_WIDGET_VISIBLE (cur->data))
	    {
	      last = NULL;
	      if (GTK_IS_TEAROFF_MENU_ITEM (cur->data))
		visible = FALSE;
	      else 
		visible = TRUE;
	    }
	  
	  cur = cur->next;
	}

      if (last)
	gtk_widget_hide (GTK_WIDGET (last->data));
    }
}

static void
update_node (EggMenuMerge *self, GNode *node)
{
  EggMenuMergeNode *info;
  GNode *child;
  EggAction *action;
#if 0
  GList *tmp;
#endif

  g_return_if_fail (node != NULL);
  g_return_if_fail (NODE_INFO(node) != NULL);

  info = NODE_INFO(node);

#if 0
  g_print("update_node name=%s dirty=%d (", info->name, info->dirty);
  for (tmp = info->uifiles; tmp != NULL; tmp = tmp->next)
    {
      NodeUIReference *ref = tmp->data;
      g_print("%s:%u", g_quark_to_string(ref->action_quark), ref->merge_id);
      if (tmp->next)
	g_print(", ");
    }
  g_print(")\n");
#endif

  if (NODE_INFO(node)->dirty)
    {
      const gchar *action_name;
      NodeUIReference *ref;

      if (info->uifiles == NULL) {
	/* We may need to remove this node.
	 * This must be done in post order
	 */
	goto recurse_children;
      }

      ref = info->uifiles->data;
      action_name = g_quark_to_string (ref->action_quark);
      action = get_action_by_name (self, action_name);

      NODE_INFO(node)->dirty = FALSE;

      /* Check if the node doesn't have an action and must have an action */
      if (action == NULL &&
	  info->type != EGG_MENU_MERGE_MENUBAR &&
	  info->type != EGG_MENU_MERGE_TOOLBAR &&
	  info->type != EGG_MENU_MERGE_SEPARATOR &&
	  info->type != EGG_MENU_MERGE_MENU_PLACEHOLDER &&
	  info->type != EGG_MENU_MERGE_TOOLBAR_PLACEHOLDER)
	{
	  /* FIXME: Should we warn here? */
	  goto recurse_children;
	}

      /* If the widget already has a proxy and the action hasn't changed, then
       * we don't have to do anything.
       */
      if (info->proxy != NULL &&
	  action == info->action)
	{
	  goto recurse_children;
	}
      
      if (info->action)
	g_object_unref (info->action);
      info->action = action;
      if (info->action)
	g_object_ref (info->action);

      switch (info->type)
	{
	case EGG_MENU_MERGE_MENUBAR:
	  if (info->proxy == NULL)
	    {
	      info->proxy = gtk_menu_bar_new ();
	      gtk_widget_show (info->proxy);
	      g_signal_emit (self, merge_signals[ADD_WIDGET], 0, info->proxy);
	    }
	  break;
	case EGG_MENU_MERGE_POPUP:
	  if (info->proxy == NULL)
	    {
	      info->proxy = gtk_menu_new ();
	      gtk_menu_set_accel_group (GTK_MENU (info->proxy),
					self->accel_group);
	    }
	  break;
	case EGG_MENU_MERGE_MENU:
	  {
	    GtkWidget *prev_submenu = NULL;
	    /* remove the proxy if it is of the wrong type ... */
	    if (info->proxy &&  G_OBJECT_TYPE(info->proxy) !=
		EGG_ACTION_GET_CLASS(info->action)->menu_item_type)
	      {
		prev_submenu = gtk_menu_item_get_submenu(GTK_MENU_ITEM(info->proxy));
		if (prev_submenu)
		  {
		    g_object_ref (prev_submenu);
		    gtk_menu_item_set_submenu(GTK_MENU_ITEM(info->proxy),NULL);
		  }
		gtk_container_remove(GTK_CONTAINER(info->proxy->parent),
				     info->proxy);
		info->proxy = NULL;
	    }
	    /* create proxy if needed ... */
	    if (info->proxy == NULL)
	      {
		GtkWidget *menushell;
		gint pos;

		if (find_menu_position(node, &menushell, &pos))
		  {
		    GtkWidget *menu;
		    info->proxy = egg_action_create_menu_item (info->action);
		    menu = gtk_menu_new();
		    gtk_menu_item_set_submenu(GTK_MENU_ITEM(info->proxy), menu);
		    gtk_menu_set_accel_group (GTK_MENU (menu), self->accel_group);
		    if (menushell)
		      gtk_menu_shell_insert (GTK_MENU_SHELL (menushell),
					     info->proxy, pos);
		  }
	      }
	    else
	      {
		egg_action_connect_proxy (info->action, info->proxy);
	      }
	    if (prev_submenu)
	      {
		gtk_menu_item_set_submenu (GTK_MENU_ITEM (info->proxy),
					   prev_submenu);
		g_object_unref (prev_submenu);
	      }
	  }
	  break;
	case EGG_MENU_MERGE_UNDECIDED:
	  g_warning("found 'undecided node!");
	  break;
	case EGG_MENU_MERGE_ROOT:
	  break;
	case EGG_MENU_MERGE_TOOLBAR:
	  if (info->proxy == NULL)
	    {
	      info->proxy = egg_toolbar_new ();
	      gtk_widget_show (info->proxy);
	      g_signal_emit (self, merge_signals[ADD_WIDGET], 0, info->proxy);
	    }
	  break;
	case EGG_MENU_MERGE_MENU_PLACEHOLDER:
	  /* create menu items for placeholders if necessary ... */
	  if (!GTK_IS_SEPARATOR_MENU_ITEM (info->proxy) ||
	      !GTK_IS_SEPARATOR_MENU_ITEM (info->extra))
	    {
	      if (info->proxy)
		gtk_container_remove(GTK_CONTAINER(info->proxy->parent),
				     info->proxy);
	      if (info->extra)
		gtk_container_remove(GTK_CONTAINER(info->extra->parent),
				     info->extra);
	      info->proxy = NULL;
	      info->extra = NULL;
	    }
	  if (info->proxy == NULL)
	    {
	      GtkWidget *menushell;
	      gint pos;

	      if (find_menu_position(node, &menushell, &pos))
		{
		  NODE_INFO(node)->proxy = gtk_separator_menu_item_new();
		  g_object_set_data (G_OBJECT (info->proxy),
				     "gtk-separator-mode",
				     GINT_TO_POINTER (SEPARATOR_MODE_HIDDEN));
		  gtk_menu_shell_insert(GTK_MENU_SHELL(menushell),
					NODE_INFO(node)->proxy, pos);

		  NODE_INFO(node)->extra = gtk_separator_menu_item_new();
		  g_object_set_data (G_OBJECT (info->extra),
				     "gtk-separator-mode",
				     GINT_TO_POINTER (SEPARATOR_MODE_HIDDEN));
		  gtk_menu_shell_insert(GTK_MENU_SHELL(menushell),
					NODE_INFO(node)->extra, pos+1);
		}
	    }
	  break;
	case EGG_MENU_MERGE_TOOLBAR_PLACEHOLDER:
	  /* create toolbar items for placeholders if necessary ... */
	  if (!EGG_IS_SEPARATOR_TOOL_ITEM (info->proxy) ||
	      !EGG_IS_SEPARATOR_TOOL_ITEM (info->extra))
	    {
	      if (info->proxy)
		gtk_container_remove(GTK_CONTAINER(info->proxy->parent),
				     info->proxy);
	      if (info->extra)
		gtk_container_remove(GTK_CONTAINER(info->extra->parent),
				     info->extra);
	      info->proxy = NULL;
	      info->extra = NULL;
	    }
	  if (info->proxy == NULL)
	    {
	      GtkWidget *toolbar;
	      gint pos;

	      if (find_toolbar_position(node, &toolbar, &pos))
		{
		  EggToolItem *item;

		  item = egg_separator_tool_item_new();
		  egg_toolbar_insert(EGG_TOOLBAR(toolbar), item, pos);
		  NODE_INFO(node)->proxy = GTK_WIDGET (item);
		  g_object_set_data (G_OBJECT (info->proxy),
				     "gtk-separator-mode",
				     GINT_TO_POINTER (SEPARATOR_MODE_HIDDEN));

		  item = egg_separator_tool_item_new();
		  egg_toolbar_insert(EGG_TOOLBAR(toolbar), item, pos+1);
		  NODE_INFO(node)->extra = GTK_WIDGET (item);
		  g_object_set_data (G_OBJECT (info->extra),
				     "gtk-separator-mode",
				     GINT_TO_POINTER (SEPARATOR_MODE_HIDDEN));
		}
	    }
	  break;
	case EGG_MENU_MERGE_MENUITEM:
	  /* remove the proxy if it is of the wrong type ... */
	  if (info->proxy &&  G_OBJECT_TYPE(info->proxy) !=
	      EGG_ACTION_GET_CLASS(info->action)->menu_item_type)
	    {
	      g_signal_handlers_disconnect_by_func (info->proxy,
						    G_CALLBACK (update_smart_separators),
						    0);  
	      gtk_container_remove(GTK_CONTAINER(info->proxy->parent),
				   info->proxy);
	      info->proxy = NULL;
	    }
	  /* create proxy if needed ... */
	  if (info->proxy == NULL)
	    {
	      GtkWidget *menushell;
	      gint pos;

	      if (find_menu_position(node, &menushell, &pos))
		{
		  info->proxy = egg_action_create_menu_item (info->action);

		  gtk_menu_shell_insert (GTK_MENU_SHELL (menushell),
					 info->proxy, pos);
		}
	    }
	  else
	    {
	      g_signal_handlers_disconnect_by_func (info->proxy,
						    G_CALLBACK (update_smart_separators),
						    0);
	      gtk_menu_item_set_submenu(GTK_MENU_ITEM(info->proxy), NULL);
	      egg_action_connect_proxy (info->action, info->proxy);
	    }
	  g_signal_connect (info->proxy, "notify::visible",
			    G_CALLBACK (update_smart_separators), 0);
	  break;
	case EGG_MENU_MERGE_TOOLITEM:
	  /* remove the proxy if it is of the wrong type ... */
	  if (info->proxy &&  G_OBJECT_TYPE(info->proxy) !=
	      EGG_ACTION_GET_CLASS(info->action)->toolbar_item_type)
	    {
	      g_signal_handlers_disconnect_by_func (info->proxy,
						    G_CALLBACK (update_smart_separators),
						    0);
	      gtk_container_remove(GTK_CONTAINER(info->proxy->parent),
				   info->proxy);
	      info->proxy = NULL;
	    }
	  /* create proxy if needed ... */
	  if (info->proxy == NULL)
	    {
	      GtkWidget *toolbar;
	      gint pos;

	      if (find_toolbar_position(node, &toolbar, &pos))
		{
		  info->proxy = egg_action_create_tool_item (info->action);

		  egg_toolbar_insert (EGG_TOOLBAR (toolbar),
					        EGG_TOOL_ITEM (info->proxy), pos);
		}
	    }
	  else
	    {
	      g_signal_handlers_disconnect_by_func (info->proxy,
						    G_CALLBACK (update_smart_separators),
						    0);
	      egg_action_connect_proxy (info->action, info->proxy);
	    }
	  g_signal_connect (info->proxy, "notify::visible",
			    G_CALLBACK (update_smart_separators), 0);
	  break;
	case EGG_MENU_MERGE_SEPARATOR:
	  if (NODE_INFO (node->parent)->type == EGG_MENU_MERGE_TOOLBAR ||
	      NODE_INFO (node->parent)->type == EGG_MENU_MERGE_TOOLBAR_PLACEHOLDER)
	    {
	      GtkWidget *toolbar;
	      gint pos;

	      if (EGG_IS_SEPARATOR_TOOL_ITEM(info->proxy))
		{
		  gtk_container_remove(GTK_CONTAINER(info->proxy->parent),
				       info->proxy);
		  info->proxy = NULL;
		}

	      if (find_toolbar_position(node, &toolbar, &pos))
		{
		  EggToolItem *item = egg_separator_tool_item_new();
		  egg_toolbar_insert (EGG_TOOLBAR (toolbar), item, pos);
		  info->proxy = GTK_WIDGET (item);
		  g_object_set_data (G_OBJECT (info->proxy),
				     "gtk-separator-mode",
				     GINT_TO_POINTER (SEPARATOR_MODE_SMART));
		  gtk_widget_show(info->proxy);
		}
	    }
	  else
	    {
	      GtkWidget *menushell;
	      gint pos;

	      if (GTK_IS_SEPARATOR_MENU_ITEM(info->proxy))
		{
		  gtk_container_remove(GTK_CONTAINER(info->proxy->parent),
				       info->proxy);
		  info->proxy = NULL;
		}

	      if (find_menu_position(node, &menushell, &pos))
		{
		  info->proxy = gtk_separator_menu_item_new();
		  g_object_set_data (G_OBJECT (info->proxy),
				     "gtk-separator-mode",
				     GINT_TO_POINTER (SEPARATOR_MODE_SMART));
		  gtk_menu_shell_insert (GTK_MENU_SHELL (menushell),
					 info->proxy, pos);
		  gtk_widget_show(info->proxy);
		}
	    }
	  break;
	}

      /* if this node has a widget, but it is the wrong type, remove it */
    }

 recurse_children:
  /* process children */
  child = node->children;
  while (child)
    {
      GNode *current;

      current = child;
      child = current->next;
      update_node (self, current);
    }

  if (info->proxy) 
    {
      if (info->type == EGG_MENU_MERGE_MENU) 
	update_smart_separators (gtk_menu_item_get_submenu (GTK_MENU_ITEM (info->proxy)));
      else if (info->type == EGG_MENU_MERGE_TOOLBAR)
	update_smart_separators (info->proxy);
    }

  /* handle cleanup of dead nodes */
  if (node->children == NULL && NODE_INFO(node)->uifiles == NULL)
    {
      if (NODE_INFO(node)->proxy)
	gtk_widget_destroy(NODE_INFO(node)->proxy);
      if ((NODE_INFO(node)->type == EGG_MENU_MERGE_MENU_PLACEHOLDER ||
	   NODE_INFO(node)->type == EGG_MENU_MERGE_TOOLBAR_PLACEHOLDER) &&
	  NODE_INFO(node)->extra)
	gtk_widget_destroy(NODE_INFO(node)->extra);
      g_chunk_free(NODE_INFO(node), merge_node_chunk);
      g_node_destroy(node);
    }
}

static gboolean
do_updates(EggMenuMerge *self)
{
  /* this function needs to check through the tree for dirty nodes.
   * For such nodes, it needs to do the following:
   *
   * 1) check if they are referenced by any loaded UI files anymore.
   *    In which case, the proxy widget should be destroyed, unless
   *    there are any subnodes.
   *
   * 2) lookup the action for this node again.  If it is different to
   *    the current one (or if no previous action has been looked up),
   *    the proxy is reconnected to the new action (or a new proxy widget
   *    is created and added to the parent container).
   */

  //g_message("do_updates");

  update_node (self, self->root_node);

  self->update_tag = 0;
  return FALSE;
}

static void
egg_menu_merge_queue_update (EggMenuMerge *self)
{
  if (self->update_tag != 0)
    return;

  self->update_tag = g_idle_add((GSourceFunc)do_updates, self);
}

void
egg_menu_merge_ensure_update (EggMenuMerge *self)
{
  if (self->update_tag != 0)
    {
      g_source_remove(self->update_tag);
      do_updates(self);
    }
}

static gboolean
dirty_traverse_func (GNode *node, gpointer data)
{
  NODE_INFO(node)->dirty = TRUE;
  return FALSE;
}

static void
egg_menu_merge_dirty_all (EggMenuMerge *self)
{
  g_node_traverse(self->root_node, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
		  dirty_traverse_func, NULL);
  egg_menu_merge_queue_update(self);
}
