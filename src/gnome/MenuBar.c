// WaterMark/GnoMoney
// mainMenu.c
// from Gtk tutorial

#include <gtk/gtk.h>
#include <strings.h>

#include "main.h"

static void menus_remove_accel(GtkWidget * widget, gchar * signal_name, gchar *
 path);
static gint menus_install_accel(GtkWidget * widget, gchar * signal_name, gchar
key, gchar modifiers, gchar * path);
void menus_init(void);
void menus_create(GtkMenuEntry * entries, int nmenu_entries);


/* this is the GtkMenuEntry structure used to create new menus.  The
 * first member is the menu definition string.  The second, the
 * default accelerator key used to access this menu function with
 * the keyboard.  The third is the callback function to call when
 * this menu item is selected (by the accelerator key, or with the
 * mouse.) The last member is the data to pass to your callback function.
 */

static GtkMenuEntry menu_items[] =
{
        {"<Main>/File/New", "<control>N", NULL, NULL},
        {"<Main>/File/Open", "<control>O", NULL, NULL},
        {"<Main>/File/Save", "<control>S", NULL, NULL},
        {"<Main>/File/Save as", NULL, NULL, NULL},
        {"<Main>/File/<separator>", NULL, NULL, NULL},
        {"<Main>/File/Quit", "<control>Q", NULL, "OK, I'll quit"},
        {"<Main>/Options/General..", NULL, NULL, NULL},
	{"<Main>/Help/About..", NULL, NULL, NULL}
};

/* calculate the number of menu_item's */
static int nmenu_items = sizeof(menu_items) / sizeof(menu_items[0]);

static int initialize = TRUE;
static GtkMenuFactory *factory = NULL;
static GtkMenuFactory *subfactory[1];
static GHashTable *entry_ht = NULL;

void get_main_menu(GtkWidget ** menubar, GtkAcceleratorTable ** table)
{
    if (initialize)
            menus_init();

    if (menubar)
            *menubar = subfactory[0]->widget;
    if (table)
            *table = subfactory[0]->table;
}

void menus_init(void)
{
    if (initialize) {
        initialize = FALSE;

        factory = gtk_menu_factory_new(GTK_MENU_FACTORY_MENU_BAR);
        subfactory[0] = gtk_menu_factory_new(GTK_MENU_FACTORY_MENU_BAR);

        gtk_menu_factory_add_subfactory(factory, subfactory[0], "<Main>");
        menus_create(menu_items, nmenu_items);
    }
}

void menus_create(GtkMenuEntry * entries, int nmenu_entries)
{
    char *accelerator;
    int i;

    if (initialize)
            menus_init();

    if (entry_ht)
            for (i = 0; i < nmenu_entries; i++) {
                accelerator = g_hash_table_lookup(entry_ht, entries[i].path);
                if (accelerator) {
                    if (accelerator[0] == '\0')
                            entries[i].accelerator = NULL;
                    else
                            entries[i].accelerator = accelerator;
                }
            }
    gtk_menu_factory_add_entries(factory, entries, nmenu_entries);

    for (i = 0; i < nmenu_entries; i++)
            if (entries[i].widget) {
                gtk_signal_connect(GTK_OBJECT(entries[i].widget), "install_accelerator",
                                   GTK_SIGNAL_FUNC(menus_install_accel),
                                   entries[i].path);
                gtk_signal_connect(GTK_OBJECT(entries[i].widget), "remove_accelerator",
                                   GTK_SIGNAL_FUNC(menus_remove_accel),
                                   entries[i].path);
            }
}

static gint menus_install_accel(GtkWidget * widget, gchar * signal_name, gchar
key, gchar modifiers, gchar * path)
{
    char accel[64];
    char *t1, t2[2];

    accel[0] = '\0';
    if (modifiers & GDK_CONTROL_MASK)
            strcat(accel, "<control>");
    if (modifiers & GDK_SHIFT_MASK)
            strcat(accel, "<shift>");
    if (modifiers & GDK_MOD1_MASK)
            strcat(accel, "<alt>");

    t2[0] = key;
    t2[1] = '\0';
    strcat(accel, t2);

/*    if (entry_ht) {
        t1 = g_hash_table_lookup(entry_ht, path);
        g_free(t1);
    } else
            entry_ht = g_hash_table_new(g_string_hash, g_string_equal);
*/
    g_hash_table_insert(entry_ht, path, g_strdup(accel));

    return TRUE;
}

static void menus_remove_accel(GtkWidget * widget, gchar * signal_name, gchar *
 path)
{
    char *t;

    if (entry_ht) {
        t = g_hash_table_lookup(entry_ht, path);
        g_free(t);

        g_hash_table_insert(entry_ht, path, g_strdup(""));
    }
}

void menus_set_sensitive(char *path, int sensitive)
{
    GtkMenuPath *menu_path;

    if (initialize)
            menus_init();

    menu_path = gtk_menu_factory_find(factory, path);
    if (menu_path)
            gtk_widget_set_sensitive(menu_path->widget, sensitive);
    else
            g_warning("Unable to set sensitivity for menu which doesn't exist:
%s", path);
}
