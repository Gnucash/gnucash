
#include <stdio.h>
#include <pgsql/libpq-fe.h>

main ()
{
    FILE *fh;
    PGconn *conn;
    int i, row, col, rc, nrows, ncols;
    char * dbName = "gnc_bogus";
    PGresult *result;

    conn = PQsetdbLogin (NULL, NULL, NULL, NULL, dbName, NULL, NULL);

    if (CONNECTION_BAD == PQstatus(conn))
    {
        printf("Fatal Error: Connection to database '%s' failed:\n",
               dbName ? dbName : "(null)");
        printf("\t%s", PQerrorMessage(conn));
        PQfinish (conn);
        exit (1);
    }

    fh = fopen("/tmp/pgsql.trace", "w");
    PQtrace(conn, fh);

    rc = PQsendQuery (conn, "SELECT * FROM gncAccount;");
    if (!rc)
    {
        printf("Fatal Error: send query failed:\n");
        printf("\t%s", PQerrorMessage(conn));
        PQfinish (conn);
        exit (1);
    }


    i = 0;
    do
    {
        ExecStatusType status;
        int iacc, idesc;

        result = PQgetResult(conn);
        if (!result) break;

        status = PQresultStatus(result);

        if ((PGRES_COMMAND_OK != status) &&
                (PGRES_TUPLES_OK  != status))
        {
            printf ("Error: failed to get result to query\n");
            PQclear(result);
            PQfinish (conn);
            exit (2);
        }
        nrows = PQntuples (result);
        ncols = PQnfields(result);
        printf ("Info: query result %d has %d rows and %d cols\n",
                i, nrows, ncols);

        iacc = PQfnumber (result, "accountName");
        idesc = PQfnumber (result, "description");

        printf ("accountName ===   description\n");
        for (row = 0; row < nrows; row++)
        {
            printf ("%s ==== %s\n",
                    PQgetvalue(result, row, iacc),
                    PQgetvalue(result, row, idesc));
        }

        i++;
    }
    while (result);

    PQfinish (conn);
}
