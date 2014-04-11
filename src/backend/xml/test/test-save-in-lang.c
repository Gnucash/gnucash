#include "config.h"
#include <glib.h>
#include <glib/gstdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "test-file-stuff.h"

#include "gnc-engine.h"
#include "TransLog.h"

#include "io-gncxml-v2.h"

const char *possible_envs[] = {
    "C", "af", "ar", "az", "be", "bg", "bg_BG.cp1251", "br", "ca", "cs",
    "cy", "cz", "da", "de", "de_AT", "el", "en", "en_AU", "en_CA", "en_GB",
    "eo", "es", "es_DO", "es_ES", "es_GT", "es_HN", "es_MX", "es_PA",
    "es_PE", "es_SV", "et", "et_EE", "eu", "fi", "fi_FI", "fr", "ga",
    "gd", "gl", "gr", "gv", "he", "hr", "hu", "id", "is", "it", "ja",
    "ja_JP", "ja_JP.SJIS", "ko", "ko_KR", "ko_KR.eucKR", "kw", "l10n",
    "locale.alias", "lt", "nl", "nn", "no", "no@nynorsk", "no_NO", "no_NY",
    "pl", "pl_PL", "pt", "pt_BR", "pt_PT", "ro", "ru", "ru_RU", "rudos",
    "rukoi8", "ruwin", "sk", "sl", "sl_SI", "sp", "sr", "sr_YU", "sv", "ta",
    "tr", "uk", "vi", "vi_VN.VISCII", "wa", "zh", "zh_CN", "zh_CN.EUC",
    "zh_CN.GB2312", "zh_TW", "zh_TW.Big5",
    NULL
};

const char *possible_vars[] = {
    "LANG", "LC_CTYPE", "LC_COLLATE", "LC_TIME", "LC_NUMERIC",
    "LC_MONETARY", "LC_MESSAGES",
    NULL
};

const char *diff_command = "cmp %s %s";
const char *test_dir = "test-files/xml2";
const char *base_env = "C";

static char*
gen_new_file_name(const char *filename, const char *env)
{
    char *ret;

    ret = g_new(char, strlen(filename) + strlen(env) + 2);
    strcpy(ret, filename);
    strcat(ret, "-");
    strcat(ret, env);

    return ret;
}

static int
run_command_get_return(const char *command)
{
    return system(command);
}

static char *
test_file(const char *filename)
{
    int i;

    for(i = 0; possible_envs[i] != NULL; i++)
    {
        QofBackendError err;
        QofSession *session;
        char *cmd;
        char *new_file = gen_new_file_name(filename, possible_envs[i]);
        QofSession *new_session;
        
        session = qof_session_new();

        qof_session_begin(session, filename, TRUE, FALSE);
        err = qof_session_pop_error (session);
        if (err)
        {
            qof_session_destroy(session);
            return g_strdup_printf("qof_session_begin errorid %d", err);
        }

        qof_session_load(session, NULL);
        err = qof_session_pop_error (session);
        if(err)
        {
            qof_session_destroy(session);
            return g_strdup_printf("qof_session_load errorid %d", err);
        }

        if (!g_setenv("LANG", possible_envs[i], TRUE))
          return g_strdup("setenv for LANG");

        new_session = qof_session_new();
        
        qof_session_begin(new_session, new_file, FALSE, FALSE);
        err = qof_session_pop_error (new_session);
        if(err)
        {
            g_free(new_file);
            qof_session_destroy(session);
            qof_session_destroy(new_session);
            return g_strdup_printf("qof_session_begin 2 with LANG=%s",
                                   possible_envs[i]);
        }

        qof_session_save(new_session, NULL);

        cmd = g_strdup_printf(diff_command, filename, new_file);

        if(run_command_get_return(cmd) != 0)
        {
            g_free(cmd);
            g_free(new_file);
            qof_session_destroy(session);
            qof_session_destroy(new_session);
            return g_strdup_printf("run_command_get_return with LANG=%s",
                                   possible_envs[i]);
        }

        g_free(new_file);
        g_free(cmd);
        qof_session_destroy(session);
        qof_session_destroy(new_session);
    }

    return NULL;
}

int
main(int argc, char **argv)
{
    GDir *adir;

    gnc_engine_init(argc, argv);
    xaccLogDisable();
    
    if((adir = g_dir_open(test_dir, 0, NULL)) == NULL)
    {
        failure_args("g_dir_open", __FILE__, __LINE__,
                     "couldn't open dir %s", test_dir);
    }
    else
    {
        const gchar *next_file;

        while((next_file = g_dir_read_name(adir)) != NULL)
        {
            struct stat file_info;
            char* filename;

            filename = g_build_filename(test_dir, next_file, (gchar*) NULL);

            if(g_stat(filename, &file_info) != 0)
            {
                failure_args("stat", __FILE__, __LINE__,
                             "couldn't stat file %s: %s", filename,
                             strerror(errno));
                g_free(filename);
                break;
            }

            if (!g_setenv("LANG", base_env, TRUE))
            {
              failure_args("setenv", __FILE__, __LINE__,
                           "setenv of LANG failed");
              g_free(filename);
              break;
            }

            if(!S_ISDIR(file_info.st_mode))
            {
                char *msg = test_file(filename);
                
                if(msg != NULL)
                {
                    failure_args("test_file", __FILE__, __LINE__,
                                 "failure testing file %s with msg %s",
                                 filename, msg);
                }
                g_free(msg);
            }

            g_free(filename);
        }
        g_dir_close(adir);
    }
    
    print_test_results();
    exit(get_rv());
}
