/*
 * FILE:
 * gnc-server.c
 *
 * FUNCTION:
 * Experimental gnucash server
 * Written as a demo, not real code.
 * A 'real' server would be a bit more architected than this;
 * this implementation doesn't hide interfaces sufficiently.
 */

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "gnc-engine.h"
#include "io-gncxml.h"

#include <fcgi_stdio.h>


/* ======================================================== */
/* XXX -- hack alert -- should do the below in some far more
 * elegant fashion ...  */

static void
reject_user_agent (const char *user_agent)
{
    printf("Content-type: text/html\r\n"
           "\r\n"
           "<html>\n"
           "<head><title>ERROR</title></head>\n"
           "<body bgcolor=#ffffff>\n"
           "<h1>Error - Wrong Browser</h1>\n"
           "Your browser was deteted to be %s<p>\n"
           "This server returns finacial data (XML) that only\n"
           "the GnuCash client understands.  You must use GnuCash\n"
           "to view this data\n"
           "</body></html>\n",
           user_agent);
}

static void
reject_method (const char * method)
{
    printf("Content-type: text/html\r\n"
           "\r\n"
           "<html>\n"
           "<head><title>ERROR</title></head>\n"
           "<body bgcolor=#ffffff>\n"
           "<h1>Error - Unsupported Method</h1>\n"
           "Your browser sent METHOD=%s\n"
           "<p>Only METHOD=POST is supported\n"
           "</body></html>\n",
           method);
}

static void
reject_session (const char * session)
{
    printf("Content-type: text/html\r\n"
           "\r\n"
           "<html>\n"
           "<head><title>ERROR</title></head>\n"
           "<body bgcolor=#ffffff>\n"
           "<h1>Error - Invalid Session ID</h1>\n"
           "Your browser sent the session id %s\n"
           "<p>This is not a valid session ID\n"
           "</body></html>\n",
           session);
}

static void
reject_auth (void)
{
    printf("Content-type: text/html\r\n"
           "\r\n"
           "<html>\n"
           "<head><title>ERROR</title></head>\n"
           "<body bgcolor=#ffffff>\n"
           "<h1>Error - Bad Login</h1>\n"
           "Your supplied a bad username or password\n"
           "<p>Try again\n"
           "</body></html>\n");
}

/* ======================================================== */
/* XXX -- hack alert -- cheesy user authentication and tracking
 * this should be replaced by something more professional
 *
 * This implementation uses gnucash GncGUID's to track sessions,
 * but the API is designed so that anything that can be converted
 * to a string & back will work.
 */

GList * logged_in_users = NULL;

/* The auth_user() routine authenticates the user.  If the
 * authentication fails, then NULL is returned. If the authentication
 * suceeds, then a string that uniquely identifies this session
 * should be returned.  (When the user logs off, the session
 * should become invalid.
 */

static const char *
auth_user (const char * name, const char *passwd)
{
    GncGUID *guid;
    const char *session_auth_string;

    /* hack alert - XXX - we do no authentication whatsoever,
     * any user is allowed to login.  We only reject null users.
     */
    if (!name || !passwd) return NULL;

    guid = g_new (GncGUID, 1);
    guid_new (guid);
    logged_in_users = g_list_prepend (logged_in_users, guid);
    session_auth_string = guid_to_string (guid); /* THREAD UNSAFE */
    return session_auth_string;
}

/*
 * The have_session() routine checks to see whether the given
 * session string corresponds to a valid session.  It returns
 * true if it does.
 */

static gboolean
have_session (const char *session_auth_string)
{
    GncGUID guid;
    GList *next = logged_in_users;

    string_to_guid (session_auth_string, &guid);

    while (next)
    {
        if (guid_equal (&guid, next->data)) return TRUE;
        next = next->next;
    }

    /* guid was not found */
    return FALSE;
}

/* ======================================================== */
/* handy utility routine for finding a cookie in the cookie string */

static const char *
find_cookie (const char * cookie_name)
{
    const char *cookie_string;
    size_t len;
    len = strlen (cookie_name);

    cookie_string = getenv ("HTTP_COOKIE");
    if (!cookie_string) return NULL;

    while (cookie_string)
    {
        if (!strncmp (cookie_string, cookie_name, len) &&
                ('=' == cookie_string[len]))
        {
            return cookie_string + len + 1;
        }
        cookie_string = strchr (cookie_string, ';');
        if (cookie_string) cookie_string ++;
    }

    return NULL;
}

/* ======================================================== */
/* simpleminded utility parses GET/POST string for username,
 * password.  Not only is it totally inflexible as to what
 * it looks for, but it also fails to url-decode, so that
 * characters like & cannot be used insode of passwords
 * XXX hack alert above should be fixed.
 */
static void
parse_for_login (char * bufp, char **namep, char **passwdp)
{
    if (!bufp) return;

    while (bufp)
    {
        if (!strncmp (bufp, "name=", 5))
        {
            *namep = bufp + 5;
        }
        else if (!strncmp (bufp, "passwd=", 7))
        {
            *passwdp = bufp + 7;
        }

        bufp = strchr (bufp, '&');
        if (bufp)
        {
            *bufp = 0x0;
            bufp++;
        }
    }
}

/* ======================================================== */

int
main (int argc, char *argv[])
{
    int err, fake_argc = 1;
    char * fake_argv[] = {"hello", 0};
    QofBook *book;
    Account *root;
    char *request_bufp, *reply_bufp;
    int rc, sz;

    /* intitialize the engine */
    gnc_engine_init (fake_argc, fake_argv);

    /* contact the database, which is a flat file for this demo */
    /* this should really be an SQL server */
    book = qof_book_new ();

    rc = gnc_book_begin (book, "file:/tmp/demo.xac", FALSE);
    if (!rc) goto bookerrexit;

    rc = gnc_book_load (book);
    if (!rc) goto bookerrexit;

    /* the root pointer points to our local cache of the data */
    root = gnc_book_get_root_account (book);

    /* --------------------------------------------------- */
    /* done with initialization, go into event loop */

    while (FCGI_Accept() >= 0)
    {
        GList *split_list;
        Query *q = NULL;
        const char *request_method;
        const char *user_agent;
        const char *auth_string;
        const char *content_length;
        int read_len = 0;
        int send_accts = 0;

        /* get the user agent; reject if wrong agent */
        user_agent = getenv ("HTTP_USER_AGENT");
        if (strncmp ("gnucash", user_agent, 7))
        {
            reject_user_agent (user_agent);
            continue;
        }

        /* get the request method */
        request_method = getenv ("REQUEST_METHOD");
        if (strcmp ("POST", request_method))
        {
            /* method=post is the only spported method*/
            reject_method(request_method);
            continue;
        }

        /* ----------------------------------------------- */
        /* look for an authentication cookie */
        auth_string = find_cookie ("gnc-server");

        /* found the cookie, lets make sure that it is valid */
        if (auth_string)
        {
            gboolean valid_session;
            valid_session = have_session (auth_string);
            if (!valid_session)
            {

                /* XXX invalid sessions are a sign of hacking;
                 * this event should be noted in a security log
                 * and the server admin contacted.
                 */
                reject_session (auth_string);
                continue;
            }
        }

        /* go ahead and read the message body.
         * we'll need this soon enough */
        content_length = getenv("CONTENT_LENGTH");
        read_len = atoi (content_length);

        /* read 'read_len' bytes from stdin ... */
        request_bufp = (char *) g_malloc (read_len);
        fread (request_bufp, read_len, 1, stdin);

        /* if no previously authenticated session,
         * authenticate now */
        if (!auth_string)
        {
            char *name = NULL, *passwd = NULL;
            parse_for_login (request_bufp, &name, &passwd);

            auth_string = auth_user (name, passwd);
            if (!auth_string)
            {
                reject_auth();
                g_free (request_bufp);
                continue;
            }
            send_accts = 1;
        }

        /* ----------------------------------------------- */
        /* send only the accounts to the user */
        if (send_accts)
        {
            /* print the HTTP header */
            printf("Content-type: text/gnc-xml\r\n"
                   "Set-Cookie: %s\r\n"
                   "Content-Length: %d\r\n"
                   "\r\n",
                   auth_string, sz);

            /* since this is the first time the user is logging
             * in, send them the full set of accounts.
             * (Do not send them any transactions yet).
             */
            gncxml_write_account_tree_to_buf(root, &reply_bufp, &sz);

            /* send the xml to the client */
            printf ("%s", reply_bufp);
            g_free (request_bufp);

            /* wait for the next request */
            continue;
        }

        /* ----------------------------------------------- */
        /* If we got to here, then the ser should be sending
         * us a query xml.
         * we should somehow error check that what we got
         * is really a valid query
         */

        /* conver the xml input into a gnucash query structure... */
        q = gncxml_read_query (request_bufp, read_len);
        xaccQuerySetGroup (q, root);

        /* hack -- limit to 30 splits ... */
        xaccQuerySetMaxSplits (q, 30);
        split_list = xaccQueryGetSplits (q);

        /* poke those splits into an ccount group structure */
        /* XXX not implemented */

        /* send the account group structure back to the user */
        /* XXX not implemented */

        xaccFreeQuery (q);
        g_free (request_bufp);

    }

bookerrexit:

    err = gnc_book_get_error (book);

    /* 500 Server Error */
    FCGI_SetExitStatus (500);

    printf("Content-type: text/plain\r\n\r\n"
           "error was %s\n", strerror (err));

    FCGI_Finish();

    /* close the book */
    qof_book_destroy (book);

    /* shut down the engine */
    gnc_engine_shutdown ();

    sleep (1);

    /* must return a non-zero error code, otherwise fastcgi
     * attempts to respawn this daemon. */
    return 500;
}

