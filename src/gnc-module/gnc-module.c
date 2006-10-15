/*************************************************************
 * gnc-module.c -- loadable plugin/module system for gnucash
 * Copyright 2001 Linux Developers Group, Inc.
 *************************************************************/

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>
#ifdef G_OS_WIN32
# undef DLL_EXPORT /* Will cause warnings in ltdl.h if defined */
# define LIBLTDL_DLL_IMPORT
#endif
#include <ltdl.h>
#include <guile/gh.h>
#include <sys/types.h>
#include <dirent.h>

#include "gnc-module.h"

#ifndef lt_ptr
# define lt_ptr lt_ptr_t
#endif

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
  lt_dlhandle   handle;
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
  char  * spath   = getenv("GNC_MODULE_PATH");
  GList * list    = NULL;
  GString * token = g_string_new(NULL);
  int   escchar   = 0;
  char  * cpos;

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
    char *envt = getenv("LD_LIBRARY_PATH");
    
    if(envt)
    {
      envt = g_strdup(envt);
    }
    else
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
    
    if (!g_setenv("LD_LIBRARY_PATH", envt, 1))
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
  if(loaded_modules == NULL) 
  {
    loaded_modules = g_hash_table_new(g_direct_hash, g_direct_equal);
    
    if(lt_dlinit() == 0)
    {
      gnc_module_system_setup_load_path();
      
      /* now crawl the GNC_MODULE_PATH to find likely libraries */
      gnc_module_system_refresh();
    }
    else
    {
      /* FIXME: there's no way to report this error to the caller. */
      g_warning ("gnc module system couldn't initialize libltdl");
    }
  }
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
      DIR *d = opendir(current->data);
      struct dirent * dent = NULL;
      char * fullpath = NULL;
      int namelen;
      GNCModuleInfo * info;

      if (!d) continue;

      while ((dent = readdir(d)) != NULL)
      {
        namelen = strlen(dent->d_name);
        
        /* is the file a .la shared lib? */
        if((namelen > 3) && (!strncmp(dent->d_name + namelen - 3, ".la", 3)))
        {
          /* get the full path name, then dlopen the library and see
           * if it has the appropriate symbols to be a gnc_module */
          fullpath = g_build_filename((const gchar *)(current->data), 
				      dent->d_name, (char*)NULL);
          info     = gnc_module_get_info(fullpath);
          
          if(info) 
          {
            module_info = g_list_prepend(module_info, info);
          }
          g_free(fullpath);
        }
      }
      closedir(d);

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


/*************************************************************
 *  gnc_module_get_info
 *  check a proposed gnc_module by looking for specific symbols in it;
 *  if it's a gnc_module, return a struct describing it.
 *************************************************************/

static GNCModuleInfo *
gnc_module_get_info(const char * fullpath) 
{
  lt_dlhandle handle;
  lt_ptr modsysver;

  //printf("(init) dlopening %s\n", fullpath);
  handle = lt_dlopen(fullpath);
  if (handle == NULL) {
      g_warning ("Failed to dlopen() '%s': %s\n", fullpath, lt_dlerror());
      return NULL;
  }

  modsysver   = lt_dlsym(handle, "gnc_module_system_interface");
    
  /* the modsysver tells us what the expected symbols and their
   * types are */
  if (!modsysver) {
      //printf("(init) closing %s\n", fullpath);
      //lt_dlclose(handle);
      return NULL;
  }

  if (*(int *)modsysver != 0) {
      /* unsupported module system interface version */
      /* printf("\n** WARNING ** : module '%s' requires newer module system\n",
         fullpath); */
      //lt_dlclose(handle);
      return NULL;
  }

  {
      lt_ptr initfunc    = lt_dlsym(handle, "gnc_module_init");
      lt_ptr pathfunc    = lt_dlsym(handle, "gnc_module_path");
      lt_ptr descripfunc = lt_dlsym(handle, "gnc_module_description");
      lt_ptr interface   = lt_dlsym(handle, "gnc_module_current");
      lt_ptr revision    = lt_dlsym(handle, "gnc_module_revision");
      lt_ptr age         = lt_dlsym(handle, "gnc_module_age");
      
      if (!(initfunc && pathfunc && descripfunc && interface &&
            revision && age)) {
          g_warning ("module '%s' does not match module signature\n",
                     fullpath);
          //lt_dlclose(handle);
          return NULL;
      }

      {
          /* we have found a gnc_module. */
          GNCModuleInfo * info = g_new0(GNCModuleInfo, 1);
          char * (* f_path)(void) = pathfunc;
          char * (* f_descrip)(void) = descripfunc;
          info->module_path        = f_path();
          info->module_description = f_descrip();
          info->module_filepath    = g_strdup(fullpath);
          info->module_interface   = *(int *)interface;
          info->module_age         = *(int *)age;
          info->module_revision    = *(int *)revision;
          //printf("(init) closing %s\n", fullpath);
          //lt_dlclose(handle);
          return info;
      }
  }
}


/*************************************************************
 * gnc_module_locate 
 * find the best matching module for the name, interface pair
 *************************************************************/

static GNCModuleInfo * 
gnc_module_locate(const gchar * module_name, int interface) 
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
       (interface >= (current->module_interface - current->module_age)) &&
       (interface <= current->module_interface)) 
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
gnc_module_check_loaded(const char * module_name, gint interface) 
{
  GNCModuleInfo * modinfo = gnc_module_locate(module_name, interface);
  GList * modules = NULL;
  GList * p = NULL;
  GNCLoadedModule * rv = NULL;

  if(modinfo == NULL) 
  {
    return NULL;
  }
  
  if(!loaded_modules) 
  {
    gnc_module_system_init();
  }
  
  /* turn the loaded-modules table into a list */
  g_hash_table_foreach(loaded_modules, list_loaded, &modules);

  /* walk the list to see if the file we want is already open */
  for(p=modules; p; p=p->next) 
  {
    GNCLoadedModule * lm = p->data;
    if(!strcmp(lm->filename, modinfo->module_filepath)) 
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
gnc_module_load_common(char * module_name, gint interface, gboolean optional)
{

  GNCLoadedModule * info;
  
  if(!loaded_modules) 
  {
    gnc_module_system_init();
  }
  
  info = gnc_module_check_loaded(module_name, interface);
  
  /* if the module's already loaded, just increment its use count.
   * otherwise, load it and check for the initializer
   * "gnc_module_init".  if we find that, assume it's a gnucash module
   * and run the function. */

  if(info) 
  {
    /* module already loaded ... call the init thunk */
    if(info->init_func) 
    {
      if(info->init_func(info->load_count)) 
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
  }
  else 
  {
    GNCModuleInfo * modinfo = gnc_module_locate(module_name, interface);
    lt_dlhandle   handle = NULL;
    
    //if(modinfo) 
    //printf("(load) dlopening %s\n", modinfo->module_filepath);

    if(modinfo && ((handle = lt_dlopen(modinfo->module_filepath)) != NULL)) 
    {
      lt_ptr initfunc = lt_dlsym(handle, "gnc_module_init");
      
      if(initfunc) 
      {
        /* stick it in the hash table */ 
        info = g_new0(GNCLoadedModule, 1);
        info->handle     = handle;
        info->filename   = g_strdup(modinfo->module_filepath);
        info->load_count = 1;
        info->init_func  = initfunc;
        g_hash_table_insert(loaded_modules, info, info);
        
        /* now call its init function.  this should load any dependent
         * modules, too.  If it doesn't return TRUE unload the module. */
        if(!info->init_func(0)) 
        {
          /* init failed. unload the module. */
          g_warning ("Initialization failed for module %s\n", module_name);
          g_hash_table_remove(loaded_modules, info);
          g_free(info->filename);
          g_free(info);
          //lt_dlclose(handle);
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
    else if (!optional)
    {
      g_warning ("Failed to open module %s", module_name);
      if(modinfo) printf(": %s\n", lt_dlerror());
      else g_warning (": could not locate %s interface v.%d\n",
                      module_name, interface);
      return NULL;
    }
    return NULL;
  }
}


GNCModule 
gnc_module_load(char * module_name, gint interface) 
{
  return gnc_module_load_common(module_name, interface, FALSE);
}

GNCModule 
gnc_module_load_optional(char * module_name, gint interface) 
{
  return gnc_module_load_common(module_name, interface, TRUE);
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
  
  if((info = g_hash_table_lookup(loaded_modules, module)) != NULL) 
  {
    lt_ptr unload_thunk = lt_dlsym(info->handle, "gnc_module_end");
    int    unload_val = TRUE;

    info->load_count--;
    if(unload_thunk) 
    {
      int (* thunk)(int) = unload_thunk;
      unload_val = thunk(info->load_count);
    }
    
    /* actually unload the module if necessary */
    if(info->load_count == 0) 
    {
      /* now close the module and free the struct */ 
      //printf("(unload) closing %s\n", info->filename);
      //lt_dlclose(info->handle);
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

