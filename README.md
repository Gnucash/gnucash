# Happy new year with GnuCash! #

This project exists for helping with starting a new financial year with GnuCash.

The following translations for this documentation exist:

- 🇬🇧 English: [README.md](README.md) (you are reading this)
- 🇩🇪 German / Deutsch: [REAMDE.de.md](README.de.md)

For proper archiving and legal reasons, it may be necessary to "freeze" the last year's GnuCash file
and create a new file with an identical account structure and balances, but without all the
transactions of the year past.  That is exactly what the program in this project does:

- Create a new GnuCash file.
- Duplicate the origin's accounts.
- Create one or more opening transactions to initialize the accounts with the correct value.
- Duplicate business entities such as vendors, customers, employees.

## Getting started ##

### Prerequisites ###

Make sure that you have GnuCash and its Python library installed as well as the sqlite backend
(`libdbd-sqlite3` on Debian).  You can test this by calling  
`python3 -c "import gnucash"`.  If there are no error messages, everything is should be working.

### Run the program ###

- To get a list of options, run `./new_year.py -h`
- To create a new file from an existing one, use the `-i` and `-o` options:  
  `./new_year.py -i examples/lastyear.gnucash -o new.gnucash`
  - This will read the GnuCash file `examples/lastyear.gnucash` and create a new file `new.gnucash`
    from its content.
  - You may now open `new.gnucash` with the GnuCash program to inspect the results.
  - If `new.gnucash` exists already, a warning will be printed and the transaction will be applied
    to the output file nonetheless, possibly adding more data than you want!

## Advanced: target accounts and configuration ##

It may be desirable to split the opening transaction into multiple parts, for example to have
separate transactions for assets and liabilities.  For this, you can define *target accounts* for
specific types of accounts.  One way for these definitions are the `--target-asset`,
`--target-liability` etc. command line options, which give account names (separated by colons "`:`")
for the opening transaction of a specific type.  Alternatively, users can also give these
definitions in a config file and specify it with the `--conf` option.

The config file uses the INI format and all configuration are in the `[DEFAULT]` section.
Configuration values are stored as key-value pairs, the keys are identical to command line options,
with hyphens `-` replaced by underscores `_`.

You can find an example configuration file in the `test_data/` directory.

## Help with development ##

Do you want to help with development? That's great!  Here are a number of ideas how you can
contribute to making this software even better:

- Use the software, find problems and [create an issue ticket](https://gitlab.com/wiese28/gnucash-happy-new-year/-/issues).
- If you have an idea how the software could be improved, you can also open a ticket.
- Improve this documentation or write translations.
- Fix bugs or implement new features yourself.  Please open a merge request to have your changes
  added here.
- Write tests (see below) for more parts of the code.
- Refactor the code to be more pythonic and to be a real Python package.  This may also include
  utilities to make the GnuCash code more straightforward to use.

### Testing ###

At the moment, the testing consists of little more than a proof of concept.  This should be
improved.

To run the tests, install pytest and run: `pytest tests`

If you want to write new tests, you can `import utils` to set `sys.path` accordingly.
