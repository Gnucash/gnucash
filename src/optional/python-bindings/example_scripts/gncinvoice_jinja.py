#!/usr/bin/env python
# -*- coding: utf-8 -*-

##@file
# @ingroup python_bindings_examples
# @author Christoph Holtermann (c.holtermann (at) gmx.de)
# @date 2014-11
# @brief exports an invoice from gnucash using a template file, see \ref py_invoice_export
#
# Input is a template file that will be filled with information from
# gnucash Invoices. Jinja2 templating engine ist used. Templates can
# be Latex, Html or anything.
#
# Example templates for german invoices:
# - Invoice.tex.tmpl
# - Invoice_2.tex.tmpl
#
# This is a sequel to latex_invoices.py that exported to a lco file
# to be imported into a LaTeX letter.
# The approach used here is not as dependent on external files and
# more modular as it allows to use arbitrary templates
#
# Doxygen: see page \ref py_invoice_export
#
# Questions / Issues:
# - How much logic in the template, how much preprocessing in this file ?
# - Internationalization - currencies, formatting of numbers
# - Providing data of gnucash owner

try:
    import locale
    import sys
    import getopt
    import gnucash
    import str_methods
    import jinja2
    from gncinvoicefkt import *
except ImportError as import_error:
    print "Problem importing modules."
    print import_error
    sys.exit(2)

class Usage(Exception):
    def __init__(self, msg):
        self.msg = msg

def main(argv=None):
    if argv is None:
        argv = sys.argv
    try:
        # default values
        prog_name = argv[0]
        ignore_lock = True
        filename_template = None
        filename_output = None
        no_output = False
        list_invoices = False
        invoice_number = None
        invoice_id = None

        try:
            opts, args = getopt.getopt(argv[1:], "fhlI:t:o:", ["help"])
        except getopt.error, msg:
             raise Usage(msg)

        for opt in opts:
            if opt[0] in ["-f"]:
                print "ignoring lock"
                ignore_lock = True
            if opt[0] in ["-h","--help"]:
                raise Usage("Help:")
            if opt[0] in ["-I"]:
                invoice_id = opt[1]
                print "using invoice ID '" + str(invoice_id) + "'."
            if opt[0] in ["-o"]:
                filename_output = opt[1]
                print "using output file", filename_output
            if opt[0] in ["-t"]:
                filename_template = opt[1]
                print "using template file", filename_template
            if opt[0] in ["-l"]:
                list_invoices = True
                print "listing invoices"

        # Check for correct input
        if len(args)>1:
            print "opts:",opts,"args:",args
            raise Usage("Only one input possible !")
        if len(args)==0:
            raise Usage("No input given !")
        input_url = args[0]

        # Check for correct template
        if not filename_template:
            no_output = True
            if not list_invoices:
                raise Usage("No template given !")

        # Check for output file
        if not filename_output:
            if filename_template:
                filename_output = filename_template + ".out"
                print "no output filename given, will be:", filename_output

    except Usage, err:
        if err.msg == "Help:":
            retcode=0
        else:
            print >>sys.stderr, "Error:",err.msg
            print >>sys.stderr, "for help use --help"
            retcode=2

        print
        print "Usage:"
        print
        print "Invoke with",prog_name,"gnucash_url."
        print "where input is"
        print "   filename"
        print "or file://filename"
        print "or mysql://user:password@host/databasename"
        print
        print "-f             force open = ignore lock"
        print "-l             list all invoices"
        print "-h or --help   for this help"
        print "-I ID          use invoice ID"
        print "-t filename    use filename as template file"
        print "-o filename    use filename as output file"

        return retcode

    # Try to open the given input
    try:
        print "Opening", input_url, "."
        session = gnucash.Session(input_url, ignore_lock=ignore_lock)
    except Exception as exception:
        print "Problem opening input."
        print exception
        return 2

    book = session.book
    root_account = book.get_root_account()
    comm_table = book.get_table()
    EUR = comm_table.lookup("CURRENCY", "EUR")

    invoice_list = get_all_invoices(book)

    if list_invoices:
       for number,invoice in enumerate(invoice_list):
           print str(number)+")"
           print invoice

    if not (no_output):

        if invoice_id:
            invoice = book.InvoiceLookupByID(invoice_id)
            if not invoice:
                print "ID not found."
                return 2

        if invoice_number:
            invoice = invoice_list[invoice_number]

        print "Using the following invoice:"
        print invoice

        loader = jinja2.FileSystemLoader('.')
        env = jinja2.Environment(loader=loader)
        template = env.get_template(filename_template)

        #import IPython
        #IPython.embed()
        output = template.render(invoice=invoice, locale=locale)

        print "Writing output", filename_output, "."
        with open(filename_output, 'w') as f:
            f.write(output.encode('utf-8'))

if __name__ == "__main__":
    sys.exit(main())
