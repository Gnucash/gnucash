#include <glib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-test-stuff.h"
#include "test-stuff.h"
#include "gnc-book.h"
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
        GNCBook *book;
        char *cmd;
        char *new_file = gen_new_file_name(filename, possible_envs[i]);
        char *putenv_str;
        GNCBook *new_book;
        
        book = gnc_book_new();

        if(!gnc_book_begin(book, filename, TRUE, FALSE))
        {
            gnc_book_destroy(book);
            return g_strdup("gnc_book_begin");
        }

        if(!gnc_book_load(book))
        {
            int error = gnc_book_get_error(book);
            gnc_book_destroy(book);
            return g_strdup_printf("gnc_book_load errorid %d", error);
        }

        putenv_str = g_strdup_printf ("%s=%s", "LANG", possible_envs[i]);
        putenv (putenv_str);
        g_free (putenv_str);

        new_book = gnc_book_new();
        
        if(!gnc_book_begin(new_book, new_file, FALSE, FALSE))
        {
            g_free(new_file);
            gnc_book_destroy(book);
            gnc_book_destroy(new_book);
            return g_strdup_printf("gnc_book_begin 2 with LANG=%s",
                                   possible_envs[i]);
        }

        gnc_book_save(new_book);

        cmd = g_strdup_printf(diff_command, filename, new_file);

        if(run_command_get_return(cmd) != 0)
        {
            g_free(cmd);
            g_free(new_file);
            gnc_book_destroy(book);
            gnc_book_destroy(new_book);
            return g_strdup_printf("run_command_get_return with LANG=%s",
                                   possible_envs[i]);
        }

        g_free(new_file);
        g_free(cmd);
        gnc_book_destroy(book);
        gnc_book_destroy(new_book);
    }

    return NULL;
}

int
main(int argc, char **argv)
{
    DIR *adir;

    gnc_engine_init(argc, argv);
    xaccLogDisable();
    
    if((adir = opendir(test_dir)) == NULL)
    {
        failure_args("opendir", __FILE__, __LINE__,
                     "couldn't open dir %s", test_dir);
    }
    else
    {
        struct dirent *next_file;

        while((next_file = readdir(adir)) != NULL)
        {
            struct stat file_info;
            char* filename;
            char* putenv_str;

            filename = g_strdup_printf("%s/%s", test_dir, next_file->d_name);
            
            if(stat(filename, &file_info) != 0)
            {
                failure_args("stat", __FILE__, __LINE__,
                             "couldn't stat file %s: %s", filename,
                             strerror(errno));
                g_free(filename);
                break;
            }

            putenv_str = g_strdup_printf ("%s=%s", "LANG", base_env);
            putenv (putenv_str);
            g_free (putenv_str);

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
    }
    
    print_test_results();
    exit(get_rv());
}
