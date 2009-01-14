/*************************************************************
 * gnc-module.c -- loadable plugin/module system for gnucash
 * Copyright 2001 Linux Developers Group, Inc.
 *************************************************************/

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gmodule.h>
#include <guile/gh.h>
#include <sys/types.h>
#include <dirent.h>

#include "gnc-module.h"

static GHashTable * loaded_modules = NULL;
static GList      * module_info = NULL;

typedef struct {
  char * module_path;
  char * module_description;
  char * module_filepath;
  int    module_interface;
  int    module_age;
  int    module_revision;
} GNCModuleInfo;

typedef struct
{
  GModule       * gmodule;
  gchar         * filename;
  int           load_count;
  GNCModuleInfo * info;
  int           (* init_func)(int refcount);
} GNCLoadedModule;

static GNCModuleInfo * gnc_module_get_info(const char * lib_path);

/*************************************************************
 * gnc_module_system_search_dirs
 * return a list of dirs to look in for gnc_module libraries
 *************************************************************/

static GList * 
gnc_module_system_search_dirs(void) 
{
  const char *spath = g_getenv("GNC_MODULE_PATH");
  GList * list    = NULL;
  GString * token = g_string_new(NULL);
  int   escchar   = 0;
  const char *cpos;

  if(!spath) 
  {
    spath = DEFAULT_MODULE_PATH;
  }
  
  for(cpos = spath; *cpos; cpos++) 
  {
    switch(*cpos) 
    {
#ifndef G_OS_WIN32
    /* On windows, with '\' as the directory separator character,
       this additional de-quoting will make every path processing
       fail miserably. Anyway this should probably be thrown out
       altogether, because this additional level of de-quoting
       (after shell quoting) is completely unexpected and
       uncommon. */
    case '\\':
      if(!escchar) 
      {
        escchar = TRUE;      
      }
      else 
      {
        g_string_append_c(token, *cpos);
        escchar = FALSE;
      }
      break;
#endif
      
      /* This is ':' on UNIX machines and ';' under Windows. */
    case G_SEARCHPATH_SEPARATOR:
      if(!escchar) 
      {
        list = g_list_append(list, token->str);
        g_string_free(token, FALSE);
        token = g_string_new(NULL);
      }
      else 
      {
        g_string_append_c(token, *cpos);
        escchar = FALSE;
      }
      break;

    default:
      g_string_append_c(token, *cpos);
      escchar = FALSE;
      break;
    }
  }
  if(token->len) 
  {
    list = g_list_append(list, token->str);
    g_string_free(token, FALSE);
  }
  else 
  {
    g_string_free(token, TRUE);
  }
  return list;
}

/*************************************************************
 * gnc_module_system_setup_load_path
 * initialize the Guile load path 
 *************************************************************/

static void
gnc_module_system_setup_load_path(void)
{
  GList * dirs = gnc_module_system_search_dirs();
  GList * lp;

  if(dirs)
  {
    char *envt = g_strdup(g_getenv("LD_LIBRARY_PATH"));
    
    if(!envt)
    {
      envt = g_strdup("");
    }
    
    for(lp=dirs; lp; lp=lp->next) 
    {
      char *tmp = g_strdup_printf("%s:%s", envt, (char *) lp->data);
      g_free(envt);
      envt = tmp;
      g_free(lp->data);
    }
    g_list_free(dirs);
    
    if (!g_setenv("LD_LIBRARY_PATH", envt, TRUE))
    {
      g_warning ("gnc-module failed to set LD_LIBRARY_PATH");
    }
    g_free(envt);
  }
}

/*************************************************************
 * gnc_module_system_init
 * initialize the module system
 *************************************************************/

void
gnc_module_system_init(void)
{
  if (loaded_modules)
    return;

  loaded_modules = g_hash_table_new(g_direct_hash, g_direct_equal);

  gnc_module_system_setup_load_path();

  /* now crawl the GNC_MODULE_PATH to find likely libraries */
  gnc_module_system_refresh();
}


/*************************************************************
 * gnc_module_system_refresh 
 * build the database of modules by looking through the
 * GNC_MODULE_PATH
 *************************************************************/

void
gnc_module_system_refresh(void) 
{
  GList * search_dirs; 
  GList * current;

  if(!loaded_modules) 
  {
    gnc_module_system_init();
  }
  
  /* get the GNC_MODULE_PATH and split it into directories */
  search_dirs = gnc_module_system_search_dirs();

  /* look in each search directory */
  for(current = search_dirs; current; current = current->next) 
  {
      GDir *d = g_dir_open(current->data, 0,NULL);
      const gchar *dent = NULL;
      char * fullpath = NULL;
      GNCModuleInfo * info;

      if (!d) continue;

      while ((dent = g_dir_read_name(d)) != NULL)
      {
        /* is the file a loadable module? */

        /* Gotcha: On MacOS, G_MODULE_SUFFIX is defined as "so", but if we do
         * not build clean libtool modules with "-module", we get dynamic
         * libraries ending on .dylib */
        if (g_str_has_suffix(dent, "." G_MODULE_SUFFIX) ||
            g_str_has_suffix(dent, ".dylib"))
        {
          /* get the full path name, then dlopen the library and see
           * if it has the appropriate symbols to be a gnc_module */
          fullpath = g_build_filename((const gchar *)(current->data),
                                      dent, (char*)NULL);
          info     = gnc_module_get_info(fullpath);
          
          if(info) 
          {
            module_info = g_list_prepend(module_info, info);
          }
          g_free(fullpath);
        }
      }
      g_dir_close(d);

  }
  /* free the search dir strings */
  for(current = search_dirs; current; current=current->next) 
  {
    g_free(current->data);
  }
  g_list_free(current);
}


/*************************************************************
 *  gnc_module_system_modinfo 
 *  return the list of module information
 *************************************************************/

GList *
gnc_module_system_modinfo(void) 
{
  if(!loaded_modules) 
  {
    gnc_module_system_init();
  }
  
  return module_info;
}


/*
 *  gnc_module_get_symbol
 *  gets the munged symbol from the file
 */
static gboolean
gnc_module_get_symbol(GModule* gmodule, const char* symbol, gpointer res)
{
  gchar** strs;
  gchar* munged_symbol;
  gchar *basename;
  gboolean ret;

  g_return_val_if_fail(gmodule, FALSE);
  g_return_val_if_fail(symbol, FALSE);

  /* Separate the file from its extension */
  /* Note: This currently does not work with versioned libtool dlls,
   * as they are named like libgncmodbaz-0.dll */
  basename = g_path_get_basename(g_module_name(gmodule));
  strs = g_strsplit(basename, ".", 2);
  g_free(basename);

  /* Translate any dashes to underscores */
  g_strdelimit(strs[0], "-", '_');

  /* Create the symbol <filename>_<symbol> and retrieve that symbol */
  munged_symbol = g_strdup_printf("%s_%s", strs[0], symbol);
  ret = g_module_symbol(gmodule, munged_symbol, res);

  /* printf("(%d) Looking for symbol %s\n", ret, munged_symbol); */

  /* Free everything */
  g_strfreev(strs);
  g_free(munged_symbol);
  return ret;
}

/*************************************************************
 *  gnc_module_get_info
 *  check a proposed gnc_module by looking for specific symbols in it;
 *  if it's a gnc_module, return a struct describing it.
 *************************************************************/

static GNCModuleInfo *
gnc_module_get_info(const char * fullpath)
{
  GModule *gmodule;
  gpointer modsysver;
  GNCModuleInfo *info = NULL;
  gpointer initfunc, pathfunc, descripfunc, iface, revision, age;
  gchar * (* f_path)(void);
  gchar * (* f_descrip)(void);

/*   g_debug("(init) dlopening '%s'\n", fullpath); */
  gmodule = g_module_open(fullpath, G_MODULE_BIND_MASK);
  if (gmodule == NULL) {
      g_warning("Failed to dlopen() '%s': %s\n", fullpath, g_module_error());
      return NULL;
  }

  /* the modsysver tells us what the expected symbols and their
   * types are */
  if (!gnc_module_get_symbol(gmodule, "gnc_module_system_interface", &modsysver)) {
/*       g_debug("Module '%s' does not contain 'gnc_module_system_interface'\n", */
/*                 fullpath); */
      goto get_info_close;
  }

  if (*(int *)modsysver != 0) {
      g_warning("Module '%s' requires newer module system\n", fullpath);
      goto get_info_close;
  }

  if (!gnc_module_get_symbol(gmodule, "gnc_module_init", &initfunc) ||
      !gnc_module_get_symbol(gmodule, "gnc_module_path", &pathfunc) ||
      !gnc_module_get_symbol(gmodule, "gnc_module_description", &descripfunc) ||
      !gnc_module_get_symbol(gmodule, "gnc_module_current", &iface) ||
      !gnc_module_get_symbol(gmodule, "gnc_module_revision", &revision) ||
      !gnc_module_get_symbol(gmodule, "gnc_module_age", &age)) {
    g_warning("Module '%s' does not match module signature\n", fullpath);
    goto get_info_close;
  }

  /* we have found a gnc_module. */
  info = g_new0(GNCModuleInfo, 1);
  f_path                   = pathfunc;
  f_descrip                = descripfunc;
  info->module_path        = f_path();
  info->module_description = f_descrip();
  info->module_filepath    = g_strdup(fullpath);
  info->module_interface   = *(int *)iface;
  info->module_age         = *(int *)age;
  info->module_revision    = *(int *)revision;


get_info_close:
/*   g_debug("(init) closing '%s'\n", fullpath); */
  g_module_close(gmodule);

  return info;
}


/*************************************************************
 * gnc_module_locate 
 * find the best matching module for the name, interface pair
 *************************************************************/

static GNCModuleInfo * 
gnc_module_locate(const gchar * module_name, int iface) 
{
  GNCModuleInfo * best    = NULL;
  GNCModuleInfo * current = NULL;
  GList * lptr;

  if(!loaded_modules) 
  {
    gnc_module_system_init();
  }
  
  for(lptr = module_info; lptr; lptr = lptr->next) 
  {
    current = lptr->data;
    if(!strcmp(module_name, current->module_path) &&
       (iface >= (current->module_interface - current->module_age)) &&
       (iface <= current->module_interface)) 
    {
      if(best) 
      {
        if((current->module_interface > best->module_interface) ||
           ((current->module_interface == best->module_interface) &&
            (current->module_age > best->module_age)) ||
           ((current->module_interface == best->module_interface) &&
            (current->module_age == best->module_age) &&
            (current->module_revision > best->module_revision))) 
        {
          best = current;
        }
      }
      else 
      {
        best = current;
      }
    } 
  }
  return best;
}

static void
list_loaded (gpointer k, gpointer v, gpointer data) 
{
  GList ** l = data;
  *l = g_list_prepend(*l, v);
}

static GNCLoadedModule *
gnc_module_check_loaded(const char * module_name, gint iface)
{
  GNCModuleInfo * modinfo = gnc_module_locate(module_name, iface);
  GList * modules = NULL;
  GList * p = NULL;
  GNCLoadedModule * rv = NULL;

  if (modinfo == NULL)
  {
    return NULL;
  }

  if (!loaded_modules)
  {
    gnc_module_system_init();
  }

  /* turn the loaded-modules table into a list */
  g_hash_table_foreach(loaded_modules, list_loaded, &modules);

  /* walk the list to see if the file we want is already open */
  for (p=modules; p; p=p->next)
  {
    GNCLoadedModule * lm = p->data;
    if (!strcmp(lm->filename, modinfo->module_filepath))
    {
      rv = lm;
      break;
    }
  }
  g_list_free(modules);
  return rv;
}


/*************************************************************
 * gnc_module_load
 * Ensure that the module named by "module_name" is loaded.
 *************************************************************/

static GNCModule
gnc_module_load_common(char * module_name, gint iface, gboolean optional)
{

  GNCLoadedModule * info;
  GModule         * gmodule;
  GNCModuleInfo   * modinfo;

  if(!loaded_modules)
  {
    gnc_module_system_init();
  }

  info = gnc_module_check_loaded(module_name, iface);

  /* if the module's already loaded, just increment its use count.
   * otherwise, load it and check for the initializer
   * "gnc_module_init".  if we find that, assume it's a gnucash module
   * and run the function. */

  if (info)
  {
    /* module already loaded ... call the init thunk */
    if (info->init_func)
    {
      if (info->init_func(info->load_count))
      {
        info->load_count++;
        return info;
      }
      else
      {
        g_warning ("module init failed: %s", module_name);
        return NULL;
      }
    }
    else {
      g_warning ("module has no init func: %s", module_name);
      return NULL;
    }
    /* NOTREACHED */
    g_error("internal error");
    return NULL;
  }

  modinfo = gnc_module_locate(module_name, iface);
  if (!modinfo)
  {
    if (optional)
    {
      g_message ("Could not locate optional module %s interface v.%d",
	module_name, iface);
    }
    else
    {
      g_warning ("Could not locate module %s interface v.%d",
	module_name, iface);
    }
    return NULL;
  }

/*     if (modinfo) */
/*       g_debug("(init) loading '%s' from '%s'\n", module_name, */
/*               modinfo->module_filepath); */

  if ((gmodule = g_module_open(modinfo->module_filepath, 0)) != NULL)
  {
    gpointer initfunc;

    if (gnc_module_get_symbol(gmodule, "gnc_module_init", &initfunc))
    {
      /* stick it in the hash table */
      info = g_new0(GNCLoadedModule, 1);
      info->gmodule    = gmodule;
      info->filename   = g_strdup(modinfo->module_filepath);
      info->load_count = 1;
      info->init_func  = initfunc;
      g_hash_table_insert(loaded_modules, info, info);

      /* now call its init function.  this should load any dependent
       * modules, too.  If it doesn't return TRUE unload the module. */
      if (!info->init_func(0))
      {
	/* init failed. unload the module. */
	g_warning ("Initialization failed for module %s\n", module_name);
	g_hash_table_remove(loaded_modules, info);
	g_free(info->filename);
	g_free(info);
	/* g_module_close(module); */
	return NULL;
      }

      return info;
    }
    else
    {
      g_warning ("Module %s (%s) is not a gnc-module.\n", module_name,
		 modinfo->module_filepath);
      //lt_dlclose(handle);
    }
    return info;
  }

  g_warning ("Failed to open module %s: %s\n", module_name, g_module_error());

  return NULL;
}


GNCModule 
gnc_module_load(char * module_name, gint iface) 
{
  return gnc_module_load_common(module_name, iface, FALSE);
}

GNCModule 
gnc_module_load_optional(char * module_name, gint iface) 
{
  return gnc_module_load_common(module_name, iface, TRUE);
}

/*************************************************************
 * gnc_module_unload
 * unload a module (only actually unload it if the use count goes to 0)
 *************************************************************/

int
gnc_module_unload(GNCModule module)
{
  GNCLoadedModule * info;

  if(!loaded_modules)
  {
    gnc_module_system_init();
  }

  if ((info = g_hash_table_lookup(loaded_modules, module)) != NULL)
  {
    gpointer unload_thunk;
    int unload_val = TRUE;

    info->load_count--;
    if (gnc_module_get_symbol(info->gmodule, "gnc_module_end", &unload_thunk))
    {
      int (* thunk)(int) = unload_thunk;
      unload_val = thunk(info->load_count);
    }

    /* actually unload the module if necessary */
    if (info->load_count == 0)
    {
      /* now close the module and free the struct */
      /* g_debug("(unload) closing %s\n", info->filename); */
      /* g_module_close(info->gmodule); */
      g_hash_table_remove(loaded_modules, module);
      g_free(info);
    }
    return unload_val;
  }
  else
  {
    g_warning ("Failed to unload module %p (it is not loaded)\n", module);
    return 0;
  }
}

