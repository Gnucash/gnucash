 ####################################################################
 # GnuCash.py -- Wrapper of Gnucash widgets and dialogs             #
 # Copyright (C) 2007 Ahmed Sayed Hassan, <ahmadsayed83@yahoo.com>  #
 #                                                                  #
 # This program is free software; you can redistribute it and/or    #
 # modify it under the terms of the GNU General Public License as   #
 # published by the Free Software Foundation; either version 2 of   #
 # the License, or (at your option) any later version.              #
 #                                                                  #
 # This program is distributed in the hope that it will be useful,  #
 # but WITHOUT ANY WARRANTY; without even the implied warranty of   #
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    #
 # GNU General Public License for more details.                     #
 #                                                                  #
 # You should have received a copy of the GNU General Public License#
 # along with this program; if not, contact:                        #
 #                                                                  #
 # Free Software Foundation           Voice:  +1-617-542-5942       #
 # 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       #
 # Boston, MA  02110-1301,  USA       gnu@gnu.org                   #
 #                                                                  #
 ####################################################################
 
__author__ = 'Ahmed Sayed Hassan <ahmadsayed83@yahoo.com>'

import os
import re
import dogtail.procedural
import dogtail.rawinput
import shutil
from dogtail.tree import Node
from dogtail.tree import root
from dogtail.tree import Window
from dogtail.tree import predicate
from dogtail.tree import Application
from dogtail.tree import Wizard
from dogtail.procedural import click
from time import sleep
from dogtail import tree, predicate
from dogtail.config import config
from dogtail.procedural import focus


# use the the following list to dismiss the code in this Order Cancel, No, Then Close, .... Add more if others
default_dismiss_list = ['Close', 'Cancel', 'No'] 
default_accept_list = ['OK', 'Yes', 'Apply', 'Accept']

def in_dismiss_list(button, dismiss_list=default_dismiss_list):
    """
        Check if the Button already in the dismiss 
        list you could override the default behaviour by passing
        your own dismiss list to the code
    """
    for button_name in dismiss_list:
        if button_name == button.name:
            return button
    return None

def in_accept_list(button, accept_list=default_accept_list):
    """
        Check if the Button already in the accept list 
        you could override the default behaviour by passing
        your own accept list to the code
    """
    for button_name in accept_list:
        if button_name == button.name:
            return button
    return None

def cleanup():
    """ 
        Remove only the First Entry of the recent 
        in order to avoid opening recent application 
    """
    os.system("gconftool-2 --unset /apps/gnucash/history/file0")

def ungraceful_close():
    """ kill gnuCash Process used when Hang expected """
    os.system("killall gnucash-bin")

def cleanup_all():
    """ 
        usefull to delete all GnuCash to run first 
        time wizard or when things go extemely wrong
    """
    ungraceful_close()
    sleep(5)
    os.system("gconftool-2 --recursive-unset /apps/gnucash")

class GnuCashApp (Application):

    """ Wrapper for GnuCash Application"""
    def __init__(self):
        Application.__init__(self, root.application("gnucash"))
        self.cwd_path = os.getcwd()
        self.data_file_dest = self.cwd_path + '/projects_under_test/'
        self.data_file_src = self.cwd_path + '/projects/'

    def __close(self):
        """ Close GnuCash Application """
        filemenu = self.menu("File")
        exitmenu = filemenu.menuItem("Quit")
        exitmenu.click()

    def close_without_saving(self):
        self.__close()
        focus.application('gnucash')
        click('Close Without Saving', roleName='push button')

    def close_and_save(self):
        self.__close()
        focus.application('gnucash')
        click('Save', roleName='push button')

    def open_data_file(self, data_file_name):
        """ 
        using GUI to open already exist project we have two 
        folders (projects) where we save the project 
        before applying test case procedure and (projects_under_test) 
        where we copy the current project and begin using it in the testcase 
        """
        data_file_path = self.data_file_src + data_file_name
        data_file_dest_path = self.data_file_dest + data_file_name
        shutil.copy(self.data_file_src+data_file_name, self.data_file_dest)
        open_data_file = Open()
        open_data_file.invoke()
        open_data_file.open_location(data_file_dest_path)

    def dismiss_all_dialogs(self):
        """ 
        dismiss all dialogs using procedural this should be migrated 
        to tree but there is a problem with the
        Yes/No dialogs apears after dismiss the first wizard
        """
        dialogs = self.findChildren(predicate.GenericPredicate(roleName='dialog'))
        if dialogs == None: return
        focus.application('gnucash')
        focus.dialog('GnuCash Tip Of The Day')
        click('Close', roleName='push button')
        if len(dialogs) == 2:
            focus.dialog('')
            click('Cancel', roleName='push button')
            click('Yes', roleName='push button')

    def add_new_account(self):
        """ Add new Account Just Call add new account """
        newAccount = NewAccount()
        newAccount.invoke()
        return newAccount

    def add_new_account_page(self):
        """ add new account page """
        self.menu("File").menu("New").child("New Accounts Page").click()
        account_page = self.tab("Accounts")
        return account_page

    def add_account (self, account_name, account_type=None, \
        account_parent=None, smallest_fraction=None, account_code=None,  \
         description=None, notes=None):
        """ Add account this function able to add only
             two levels parent child account """
        if account_parent != None:
            account_table = \
            self.findChild(\
            predicate.GenericPredicate(roleName='page tab', name='Accounts'))
            parent_cell = \
            account_table.findChild(\
            predicate.GenericPredicate(roleName='table cell', name=account_parent))
            parent_cell.rawClick()

        newAccount = NewAccount()
        newAccount.invoke()

        if account_code != None:
            newAccount.account_code = account_code
        if description != None:
            newAccount.description = description
        if notes != None:
            newAccount.notes = notes
        if smallest_fraction != None:
            newAccount.smallest_fraction_text = smallest_fraction
        if account_type != None:
            newAccount.account_type = account_type
        newAccount.account_name = account_name
        newAccount.accept()

    def reconcile_account(self, account_name):
        """ Only for user visible account """
        account_table = self.findChild(predicate.GenericPredicate(roleName='page tab', name='Accounts'))
        account_cell = account_table.findChild(predicate.GenericPredicate(roleName='table cell', name=account_name))
        account_cell.rawClick()

        reconcile = Reconcile()
        reconcile.invoke()
        return reconcile

    def create_account_summary(self):
        self.menu('Reports').menuItem('Account Summary').click()

    def create_transaction_report(self):
        self.menu('Reports').menuItem('Transaction Report').click()

class GnucashWindow(Window):
    """ A base for all Gnucash Dialogs dialogs """

    def __init__(self, node):
        Window.__init__(self, node)

    def initialize(self):
        """ Method to be overriden in the children """
        pass

    def invoke(self):
        """ Invoke the dialog, based on the overriden list of path """
        gnucash = GnuCashApp()
        for i in range(len(self.invoke_list)):
            if i == 0:
                menu_item = gnucash.menu(self.invoke_list[i])
            elif i == (len(self.invoke_list)-1):
                menu_item.child(self.invoke_list[i]).click()
            else:
                menu_item = menu_item.menu(self.invoke_list[i])
        sleep(1)
        dialog_list = gnucash.findChildren(predicate.GenericPredicate(roleName='dialog'))
        sleep(5)
        for dialog in dialog_list:
            result = re.search(self.dialog_name, dialog.name)
            if result != None:
                Window.__init__(self, dialog)
                self.initialize()
                return
            else:
                print "Not called"

    def dismiss(self):
        """ when calling dismiss the expected action is Cancel, Close and No """
        button_list = self.findChildren(\
        predicate.GenericPredicate(roleName='push button'))
        for button in button_list:
            dismiss_button = in_dismiss_list(button)
            if dismiss_button != None:
                if dismiss_button.showing:
                    dismiss_button.click()
                    return True
        return False

    def accept(self):
        """ when calling dismiss the expected action is Cancel, Close and No """
        button_list = self.findChildren(\
        predicate.GenericPredicate(roleName='push button'))
        for button in button_list:
            accept_button = in_accept_list(button)
            if accept_button != None:
                if accept_button.showing:
                    accept_button.click()
                    return True
        return False

class Open(GnucashWindow):
    """ Wrapper class for Open dialog """

    def __init__(self):
        self.invoke_list = ["File", "Open", "Open..."]
        self.dialog_name = 'Open'

    def initialize(self):
        """ Initialize Open Dialog location components """
        self.location_txt = self.findChild(predicate.IsLabelledAs('Location:'))

    def invoke(self):
        gnucash = GnuCashApp()
        gnucash.menu('File').menu('Open').menuItem('Open...').click()
        try:
            focus.application('gnucash')
            click('Continue Without Saving', roleName='push button')
        except:
            pass

        dialog = gnucash.findChild(predicate.GenericPredicate(roleName='dialog', name='Open'))
        Window.__init__(self, dialog)
        self.initialize()

    def __setattr__(self, name, value):
        if name == 'location':
            self.location_txt.text = value
        else:
            self.__dict__[name] = value

    def open_location(self, file_path):
        """ shorthand method to open location """
        self.location = file_path
        self.button('Open').click()

class ExportReport(GnucashWindow):
    """ Wrapper class for Export Report to HTML """

    def __init__(self):
        self.invoke_list = ["File", "Export", "Export Report"]
        self.dialog_name = 'Save HTML To File'

    def initialize(self):
        """ Initialize export reports components """
        self.name_txt = self.findChild(predicate.IsLabelledAs('Name:'))

    def __setattr__(self, name, value):
        if name == 'name':
            self.name_txt.text = value
        else:
            self.__dict__[name] = value

    def export_report(self, file_path):
        """ Shorthand method to export the report """
        self.name = file_path
        self.button('Export').click()

class NewAccount(GnucashWindow):
    """ Wrapper class for new Account Dialog """	

    def __init__(self):
        self.invoke_list = ["File", "New", "New Account..."]
        self.dialog_name = 'New Account - .'

    def initialize(self):
        """ Define Widgets called at gnucash Constructor after invoke """
        self.GeneralTab = self.tab("General")
        self.OpeninigBalanceTab = self.tab("Opening Balance")
        self.account_name_text = \
        self.GeneralTab.findChild (predicate.IsLabelledAs("Account name:"))
        self.account_code_text = \
        self.GeneralTab.findChild (predicate.IsLabelledAs("Account code:"))
        self.description_text  = \
        self.GeneralTab.findChild (predicate.IsLabelledAs("Description:"))
        self.notes_text = \
        self.GeneralTab.findChild (predicate.IsLabelledAs("Notes:"))
        self.smallest_fraction = \
        self.GeneralTab.findChild (predicate.IsLabelledAs("Smallest fraction:"))
        self.account_type_table  = \
        self.GeneralTab.findChild (predicate.IsLabelledAs("Account Type"))
        self.account_parent_table = \
        self.GeneralTab.findChild (predicate.IsLabelledAs("Parent Account"))
        self.place_holder_checkbox = \
        self.GeneralTab.findChild (predicate.GenericPredicate(\
        roleName="check box", name="Placeholder"))
        self.hidden_checkbox = \
        self.GeneralTab.findChild (predicate.GenericPredicate(\
        roleName="check box", name="Hidden"))
        self.tax_related_checkbox = \
        self.GeneralTab.findChild(predicate.GenericPredicate(\
        roleName="check box", name="Tax related"))
        self.ok_button = self.button("OK")

    def __setattr__(self, name, value):
        """ 
        Always set the account name as your last account 
        because it changes the dialog title which may lead to failure
        TODO: Fix up this issue, find a way to search using the regex, or implement it
        """
        if name == 'account_name':
            self.account_name_text.text = value
        elif name == 'account_code':
            self.account_code_text.text = value
        elif name == 'description':
            self.description_text.text = value
        elif name == 'notes':
            self.notes_text.text = value
        elif name == 'smallest_fraction_text':
            self.smallest_fraction.combovalue = value
        elif name == 'account_type':
            self.account_type_table.findChild(predicate.GenericPredicate\
            (roleName="table cell", name=value )).select()
        elif name == 'account_parent':
            self.account_parent_table.findChild(predicate.GenericPredicate\
            (roleName="table cell", \
            name='New top level account')).doAction('activate')
            self.account_parent_table.findChild(predicate.GenericPredicate\
            (roleName="table cell", name=value )).doAction('activate')
        elif name == 'hidden':
            if value and not self.hidden_checkbox.checked:
                self.hidden_checkbox.click()
            elif not value and self.hidden_checkbox.checked:
                self.hidden_checkbox.click()
        elif name == 'place_holder':
            if value and not self.place_holder_checkbox.checked:
                self.place_holder_checkbox.click()
            elif not value and self.place_holder_checkbox.checked:
                self.place_holder_checkbox.click()
        elif name == 'tax_related':
            if value and not self.tax_related_checkbox.checked:
                self.tax_related_checkbox.click()
            elif not value and self.tax_related_checkbox.checked:
                self.tax_related_checkbox.click()
        else:
            self.__dict__[name] = value

    def set_currency(self, value):
        """ TODO: Try to figure out why It does not work  """
        click('Select...', roleName='push button')
        focus.dialog('Select currency')
        click(value, roleName='menu item')

    def dismiss(self):
        self.button('Cancel').click()

    def accept(self):
        self.ok_button.click()

class Preferences(GnucashWindow):
    """ GnuCash prefernces Wrapper """
    class AccountingPeriodTab(Node):

        def __init__(self, initializer):
            """ 
            Init method for Accounting Dialog Calender 
            does not work, can not find a way to use it 
            """
            Node.__init__(self, initializer)
            self.include_grand_total_checkbox = \
            self.findChild(predicate.GenericPredicate \
            (roleName='check box', name='Include grand total'))
            self.include_non_currency_totals_checkbox = \
            self.findChild(predicate.GenericPredicate \
            (roleName='check box', name='Include non-currency totals'))
            self.start_date_relative_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Relative:', \
            description=\
            'Use the specified relative starting date for profit/loss calculations.'))
            self.start_date_absolute_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Absolute:', \
            description='Use the specified absolute starting date for profit/loss calculations.'))
            self.start_of_combobox = \
            self.findChild(predicate.GenericPredicate(\
            roleName='combo box', \
            name='Start of this year'))

    class Accounts(Node):

        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.charachter_text = \
            self.findChild(predicate.GenericPredicate(\
            roleName='text', \
            description='The character that will be used between components of an account name.  A legal value is any single character except letters and numbers, or any of the following strings: "colon" "slash", "backslash", "dash" and "period".'))
            self.income_expense_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button',\
             name='Income & expense'))
            self.credit_accounts_rb = \
            self.findChild(predicate.GenericPredicate(roleName='radio button', \
            name='Credit accounts'))
            self.none_rb = \
            self.findChild(predicate.GenericPredicate( \
            roleName='radio button', name='None'))
            self.user_formal_accounting_lables_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Use formal accounting labels'))
            self.currency_combo_box = \
            self.findChild(predicate.GenericPredicate(\
            roleName='combo box', \
            name='USD (US Dollar)'))
            self.choose_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Choose:'))
            self.local_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Locale:'))

    class Business(Node):

        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.open_in_new_window_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Open in new window'))
            self.number_of_rows_spin_button = \
            self.findChild(predicate.GenericPredicate(\
            roleName='spin button', \
            description='Default number of register rows to display in Invoices.'))
            self.accumulate_splits_on_post_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Accumulate splits on post'))
            self.invoice_tax_included_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', name='Tax included', \
            description='Whether tax is included by default in entries on Invoices. This setting is inherited by new customers and vendors.'))
            self.notify_when_due_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Notify when due'))
            self.days_in_advance_spin_button = \
            self.findChild(predicate.GenericPredicate(\
            roleName='spin button', \
            description='How many days in the future to warn about Bills coming due.'))
            self.bills_tax_included_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Tax included', \
            description='Whether tax is included by default in entries on Bills. This setting is inherited by new customers and vendors.'))

    class History(Node):

        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.use_24_hour_clock_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Use 24-hour clock'))
            self.local_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Locale:'))
            self.us_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='US:'))
            self.uk_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='UK:'))
            self.europe_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Europe:'))
            self.iso_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='ISO'))

    class General(Node):

        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.show_splash_screen_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Show splash screen'))
            self.new_search_limit_spin_button = \
            self.findChild(predicate.GenericPredicate(\
            roleName='spin button', \
            description="Default to 'new search' if fewer than this number of items is returned."))
            self.display_negative_amounts_in_red_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Display negative amounts in red'))
            self.automatic_decimal_point_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Automatic decimal point'))
            self.decimal_places_spin_button = \
            self.findChild(predicate.GenericPredicate(\
            roleName='spin button', \
            description='How many automatic decimal places will be filled in.'))
            self.display_tod_dialog_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Display "tip of the day" dialog'))
            self.perform_account_list_setup_on_new_file_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Perform account list setup on new file'))

    class OnlineBanking(Node):

        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.show_documentation_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Show documentation'))
            self.use_bayesian_matching_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Use bayesian matching'))
            self.match_display_threshold_spin_button = \
            self.findChild(\
            predicate.IsLabelledAs('Match display threshold'))
            self.auto_add_threshold_spin_button = \
            self.findChild(predicate.IsLabelledAs('Auto-add threshold'))
            self.auto_clear_threshold_spin_button = \
            self.findChild(predicate.IsLabelledAs('Auto-clear threshold'))
            self.commercial_atm_fees_threshold_spin_button = \
            self.findChild(predicate.IsLabelledAs('Commercial ATM fees threshold'))

    class Printing(Node):

        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.print_date_format_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Print date format'))
            self.print_blocking_chars_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Print blocking chars'))
            self.default_font = \
            self.findChild(predicate.IsLabelledAs('Default font:'))

    class Register(Node):

        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.use_system_theme_colors_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Use system theme colors'))
            self.double_mode_colors_alternate_with_transactions_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Double mode colors alternate with transactions'))
            self.draw_horizontal_lines_between_rows_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Draw horizontal lines between rows'))
            self.draw_vertical_lines_between_columns_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Draw vertical lines between columns'))
            self.automatic_credit_card_payment_cb = \
            self.findChild(\
            predicate.GenericPredicate(roleName='check box', \
            name='Automatic credit card payment'))
            self.automatic_interest_transfer_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Automatic interest transfer'))
            self.Check_cleared_transactions_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Check cleared transactions'))
            self.auto_raise_lists_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Auto-raise lists'))
            self.enter_moves_to_blank_transaction_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name="'Enter' moves to blank transaction"))

    class RegisterDefaults(Node):

        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.number_of_rows_spin_button = \
            self.findChild(predicate.GenericPredicate(\
            roleName='spin button', \
            description='Display this many rows when a register is created.'))
            self.register_opens_in_a_new_window_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Register opens in a new window'))
            self.double_line_mode_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Double line mode'))
            self.number_of_transactions_spin_button = \
            self.findChild(predicate.GenericPredicate(\
            roleName='spin button', \
            description='Show this many transactions in a register. A value of zero means show all transactions.'))
            self.transaction_journal_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Transaction journal'))
            self.auto_split_ledger_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Auto-split ledger'))
            self.basic_ledger_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name = 'Basic ledger'))

    class Reports(Node):

        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.report_opens_in_a_new_window_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Report opens in a new window'))
            self.locale_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Locale:'))
            self.choose_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Choose:'))
            self.select_currency_combo_box = \
            self.findChild(predicate.GenericPredicate(\
            roleName='combo box', \
            name='USD (US Dollar)'))

    class ScheduledTransactions(Node):

        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.notify_before_transactions_are_created_cb = \
            self.findChild(predicate.GenericPredicate(roleName='check box', \
            name='Notify before transactions are created '))
            self.create_days_spin_button = \
            self.findChild(predicate.GenericPredicate(\
            roleName='spin button', \
            description='Begin notifications this many days before the transaction is created.'))
            self.remind_days_spin_button = \
            self.findChild(predicate.GenericPredicate(\
            roleName='spin button', \
            description='Begin notifications this many days before the transaction is created.'))
            self.auto_create_new_transactions_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Auto-create new transactions'))
            self.run_when_data_file_opened_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Run when data file opened'))

    class Windows(Node):

        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.width_spin_button = \
            self.findChild(predicate.GenericPredicate(\
            roleName='spin button'))
            self.show_close_button_on_notebook_tabs_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Show close button on notebook tabs'))
            self.right_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Right'))
            self.left_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Left'))
            self.bottom_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Bottom'))
            self.top_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Top'))
            self.bring_the_most_recent_tab_to_the_front_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Bring the most recent tab to the front'))
            self.save_window_size_and_position_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Save window size and position'))
            self.use_system_default_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Use system default'))
            self.text_below_icons_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Text below icons'))
            self.priority_text_beside_icons_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Priority text beside icons'))
            self.icons_only_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Icons only'))
            self.text_only_rb = \
            self.findChild(predicate.GenericPredicate(\
            roleName='radio button', \
            name='Text only'))


    def __init__(self):
        self.invoke_list = ["Edit", "Preferences"]
        self.dialog_name = 'GnuCash Preferences'

    def initialize(self):
        """ define controls called after GnucashWindow call invoke """
        self.account_period = \
        self.AccountingPeriodTab(self.tab('Accounting Period'))
        self.accounts = \
        self.Accounts(self.tab('Accounts'))
        self.general = \
        self.General(self.tab('General'))
        self.online_banking = \
        self.OnlineBanking(self.tab('Online Banking'))
        self.printing = \
        self.Printing(self.tab('Printing'))
        self.register = self.Register(self.tab('Register'))
        self.register_defaults = \
        self.RegisterDefaults(self.tab('Register Defaults'))
        self.reports = self.Reports(self.tab('Reports'))
        self.scheduled_transactions = \
        self.ScheduledTransactions(self.tab('Scheduled Transactions'))
        self.windows = self.Windows(self.tab('Windows'))

class Find(GnucashWindow):
    """ 
    Wrapper for all gnucash Find include Find txns, \
    Find Customer, Find Job, ... 
    """

    def initialize(self):
        """ initialize Common components """

        self.add_btn = self.button('Add')
        self.remove_btn_list = \
        self.findChildren(\
        predicate.GenericPredicate(roleName='push button', name='Remove'))
        self.search_criteria_panel = \
        self.findChild(\
        predicate.GenericPredicate(roleName='scroll pane'))
        self.text_fields_list = \
        self.search_criteria_panel.findChildren(\
        predicate.GenericPredicate(roleName='text'))
        self.search_by_list = \
        self.search_criteria_panel.findChildren(\
        predicate.GenericPredicate(roleName='combo box'))
        self.is_case_insensitive_list = \
        self.search_criteria_panel.findChildren(\
        predicate.GenericPredicate(roleName='toggle button'))
        self.find_btn = self.button('Find')
        self.new_search_rb = \
        self.findChild(\
        predicate.GenericPredicate(roleName='radio button', \
        name='New search'))
        self.refine_current_search_rb = \
        self.findChild(\
        predicate.GenericPredicate(roleName='radio button', \
        name='Refine current search'))
        self.add_results_to_current_search_rb = \
        self.findChild(\
        predicate.GenericPredicate(roleName='radio button', \
        name='Add results to current search'))
        self.delete_results_from_current_search = \
        self.findChild(\
        predicate.GenericPredicate(roleName='radio button', \
        name='Delete results from current search'))

    def add_criteria(self):
        """ Add search critieria then refresh the children list """
        self.button('Add').click()
        self.text_fields_list = \
        self.search_criteria_panel.findChildren(\
        predicate.GenericPredicate(roleName='text'))

        self.search_by_list = \
        self.search_criteria_panel.findChildren(\
        predicate.GenericPredicate(roleName='combo box'))


    def find(self):
        """ Click find button and return a results appears in the find dialog"""
        self.find_btn.click()
        self.result_table = self.findChild(\
        predicate.GenericPredicate(roleName='table'))
        result_set = self.result_table.findChildren(\
        predicate.GenericPredicate(roleName='table cell'))
        return result_set

class FindTransaction(Find):
    """ Find Transaction Wrapper """

    def __init__(self):
        self.invoke_list = ["Edit", "Find..."]
        self.dialog_name = 'Find Transaction'

class TaxOption(GnucashWindow):
    """ Gnucash Taxoptions Dialog Wrapper """

    def __init__(self):
        self.invoke_list = ["Edit", "Tax Options"]
        self.dialog_name = 'Tax Information'

    def initialize(self):
        """ 
            intialize Tax options dialog components 
            TODO: Compelete the remaining fields after 
            thing became more clear with this dialog
        """
        self.income_rb = self.findChild(predicate.GenericPredicate(\
        roleName='radio button', name='Income'))
        self.expense_rb = self.findChild(predicate.GenericPredicate(\
        roleName='radio button', name='Expense'))
        self.select_subaccounts_btn = self.button('Select Subaccounts')
        self.tax_related_cb = self.findChild(predicate.GenericPredicate(\
        roleName='check box', name='Tax Related'))
        self.current_account_rb = self.findChild(predicate.GenericPredicate(\
        roleName='radio button', name='Current Account'))
        self.parent_account_rb = self.findChild(predicate.GenericPredicate(\
        roleName='radio button', name='Parent Account'))
        self.ok_btn = self.button('OK')
        self.cancel_btn = self.button('Cancel')


class StyleSheets(GnucashWindow):
    """ Style Sheet Dialogs Wrapper """

    def __init__(self):
        self.invoke_list = ["Edit", "Style Sheets"]
        self.dialog_name = 'Select HTML Style Sheet'

    def initialize(self):
        """ initilize the Style Sheets Components """
        self.edit_btn = self.button('Edit')
        self.new_btn = self.button('New')
        self.delete_btn = self.button('Delete')
        self.close_btn = self.button('Close')
        self.style_sheets_table = \
        self.findChild(predicate.GenericPredicate(roleName='table'))


class NewStyleSheet(Window):
    """ New Style Sheet dialog """

    def __init__(self, Node):
        Window.__init__(self, Node)
        self.name_text = \
        self.findChild(predicate.GenericPredicate(roleName='text'))
        self.template_combo = \
        self.findChild(predicate.GenericPredicate(roleName='combo box'))
        self.ok_btn = self.button('OK')
        self.cancel_btn = self.button('Cancel')

    def __setattr__(self, name, value):
        """ Set The Style Sheet """
        if name == 'style_name':
            self.name_text.text = value
        elif name == 'template':
            self.template_combo.combovalue = value
        else:
            self.__dict__[name] = value

    def accept(self):
        self.ok_btn.click()

    def dismiss(self):
        self.cancel_btn.click()

class HTMLStyleSheet(Window):
    """ 
        HTML Style Sheet dialog  
        TODO: Not compeleted yet compelete it only accept the default values
    """
    def __init__(self, Node):
        Window.__init__(self, Node)
        self.ok_btn = self.button('OK')
        self.cancel_btn = self.button('Cancel')
        self.defaults_btn = self.button('Defaults')

    def accept(self):
        self.ok_btn.click()

    def dismiss(self):
        self.cancel_btn.click()


class ResetWarnings(GnucashWindow):
    """ Reset Warning wrapper """

    def __init__(self):
        self.invoke_list = ["Actions", "Reset Warnings..."]
        self.dialog_name = 'Reset Warnings'

    def initialize(self):
        """ Reset Warning controls 
            TODO: confirm the selet all and unselect 
            those controls not visible while writing this code
        """
        self.select_all_btn = self.button('Select All')
        self.unselect_all_btn = self.button('Unselect All')
        self.ok_btn = self.button('OK')
        self.apply_btn = self.button('Apply')
        self.cancel_btn = self.button('Cancel')

class SetupAccountingPeriods(GnucashWindow):
    """ Setup Accounting Period """

    def __init__(self):
        self.invoke_list = ["Actions", "Close Books"]
        self.dialog_name = 'Setup Accounting Periods'


class NewXXX(GnucashWindow):
    """ 
    This class used to hold the common controls between the New Customer, 
    New Vendor and New Empolyee 
    TODO: Replace XXX with a nice name, this name is terrible, 
    i can not call other one
    """

    def add_new_XXX (self, company_name=None, name=None, \
        address_1=None, address_2=None, \
        phone=None, fax=None, email=None, notes=None):
        """ add new customer """
        if company_name != None:
            self.xxx.company_name_txt.text = company_name
        if name != None:
            self.xxx.name_txt.text = name
        if address_1 != None:
            self.xxx.address_1_txt.text = address_1
        if address_2 != None:
            self.xxx.address_2_txt.text = address_2
        if phone != None:
            self.xxx.phone_txt.text = phone
        if fax != None:
            self.xxx.fax_txt.text = fax
        if email != None:
            self.xxx.email_txt.text = email
        if notes != None:
            self.xxx.notes_txt.text = notes

    class XXX(Node):
        """ 
        Class for 
            Vendor Tab in New Vendor
            Customer in New Customer
            Empolyee in New Empolyee
        """

        def __init__(self, initialize):
            Node.__init__(self, initialize)
            self.identification_panel = \
            self.findChild(predicate.GenericPredicate(\
            roleName='panel', name='Identification'))
            self.idenetification_list = \
            self.identification_panel.findChildren(\
            predicate.GenericPredicate(roleName='text'))
            self.company_name_txt = self.idenetification_list[1]
            self.panel_list = \
            self.findChildren(predicate.GenericPredicate(roleName='panel'))
            for panel in self.panel_list:
                if re.search('. Address', panel.name) != None:
                    self.billing_information  = panel
                    break
#            self.billing_information = \
#            self.findChild(predicate.GenericPredicate\
#            (roleName='panel', name='Billing Address'))
            self.billing_information_elements = \
            self.billing_information.findChildren(predicate.GenericPredicate\
            (roleName='text'))
            self.active_cb = self.findChild(predicate.GenericPredicate(\
            roleName='check box', name='Active'))
            self.name_txt = self.billing_information_elements[0]
            self.address_1_txt = self.billing_information_elements[1]
            self.address_2_txt = self.billing_information_elements[2]
            self.address_3_txt = self.billing_information_elements[3]
            self.address_4_txt = self.billing_information_elements[4]
            self.phone_txt = self.billing_information_elements[5]
            self.fax_txt = self.billing_information_elements[6]
            self.email_txt = self.billing_information_elements[7]
            self.notes_panel = \
            self.findChild(predicate.GenericPredicate(\
            roleName='panel', \
            name='Notes'))
            self.notes_txt = \
            self.notes_panel.findChild(predicate.GenericPredicate(\
            roleName='text'))

    class YYYInformation(Node):
        """
        Class to wrap 
            1 - Billing Information in New Customer
            2 - Payment Information in New Vendor 
            (Not all member variable used in this case
        """
        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.currency_combobox = \
            self.findChild(predicate.GenericPredicate(roleName='combo box', \
            name='USD (US Dollar)'))
            self.terms_combobox = \
            self.findChild(predicate.GenericPredicate(roleName='combo box', \
            name='None'))
            self.tax_included_combobox = \
            self.findChild(predicate.GenericPredicate(roleName='combo box', \
            name='Use Global'))
            self.override_global_tax_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName= 'check box', \
            description='Override the global Tax Table?'))

    def __init__(self):
        pass

    def initialize(self):
        """ Initialize new generic components """
        pass

class NewCustomer(NewXXX):
    """ New Customer wrapper """
    class ShippingAddress(Node):

        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.shipping_address_elements = \
            self.findChildren(predicate.GenericPredicate(roleName='text'))
            self.name_txt = self.shipping_address_elements[0]
            self.address_1_txt = self.shipping_address_elements[1]
            self.address_2_txt = self.shipping_address_elements[2]
            self.address_3_txt = self.shipping_address_elements[3]
            self.address_4_txt = self.shipping_address_elements[4]
            self.phone_txt = self.shipping_address_elements[5]
            self.fax_txt = self.shipping_address_elements[6]
            self.email_txt = self.shipping_address_elements[7]

    def add_new_shipping_address(self, name=None, address_1=None, \
        address_2=None, address_3=None, address_4=None,
        phone=None, fax=None, email=None):
        if name != None:
            self.shipping_address.name_txt.text = name
        if address_1 != None:
            self.shipping_address.address_1_txt.text = address_1
        if address_2 != None:
            self.shipping_address.address_2_txt.text = address_2
        if address_3 != None:
            self.shipping_address.address_3_txt.text = address_3
        if address_4 != None:
            self.shipping_address.address_4_txt.text = address_4
        if phone != None:
            self.shipping_address.phone_txt.text = phone
        if fax != None:
            self.shipping_address.fax_txt.text = fax
        if email != None:
            self.shipping_address.email_txt.text = email
    def __init__(self):
        self.invoke_list = ["Business", "Customer", "New Customer..."]
        self.dialog_name = 'New Customer'

    def add_new_customer (self, company_name=None, name=None, \
        address_1=None, address_2=None, \
        phone=None, fax=None, email=None, notes=None):
        """ A wrapper for add_new_xxx """
        NewXXX.add_new_XXX(self, company_name, name, \
        address_1, address_2, \
        phone, fax, email, notes)


    def initialize(self):
        NewXXX.initialize(self)
        self.customer = self.XXX(self.tab('Customer'))
        self.xxx = self.customer
        self.billing_information = \
        self.YYYInformation(self.tab('Billing Information'))
        self.identification_panel = \
        self.customer.findChild(\
        predicate.GenericPredicate(roleName='panel', name='Identification'))
        self.identification_panel_element = \
        self.identification_panel.findChildren(\
        predicate.GenericPredicate(roleName='text'))
        self.customer.customer_number_txt = \
        self.identification_panel_element[0]
        self.billing_information = \
        self.YYYInformation(self.tab('Billing Information'))
        self.billing_information.info_elements = self.findChildren(\
        predicate.GenericPredicate(roleName='text'))
        self.billing_information.discount_txt = \
        self.billing_information.info_elements[0]
        self.billing_information.credit_limit_txt = \
        self.billing_information.info_elements[1]
        self.billing_information.tax_table_combobox = \
        self.findChild(predicate.GenericPredicate\
        (roleName='combo box', \
        description='What Tax Table should be applied to this customer?'))

        self.shipping_address = self.ShippingAddress(self.tab('Shipping Address'))
        self.ok_btn = self.button('OK')
        self.cancel_btn = self.button('Cancel')
        self.help_btn = self.button('Help')

class NewVendor(NewXXX):


    def __init__(self):
        self.invoke_list = ["Business", "Vendor", "New Vendor..."]
        self.dialog_name = 'New Vendor'

    def add_new_vendor (self, company_name=None, name=None, \
        address_1=None, address_2=None, \
        phone=None, fax=None, email=None, notes=None):
        """ A wrapper for add_new_xxx """
        NewXXX.add_new_XXX(self, company_name, name, \
        address_1, address_2, \
        phone, fax, email, notes)

    def initialize(self):
        NewXXX.initialize(self)
        self.vendor = self.XXX(self.tab('Vendor'))
        self.xxx = self.vendor
        self.identification_panel = \
        self.findChild(predicate.GenericPredicate(roleName='panel', name='Identification'))
        self.identification_panel_element = \
        self.identification_panel.findChildren(predicate.GenericPredicate(roleName='text'))
        self.vendor.vendor_number_txt = self.identification_panel_element[0]
        self.payment_information = \
        self.YYYInformation(self.tab('Payment Information'))
        self.ok_btn = self.button('OK')
        self.cancel_btn = self.button('Cancel')
        self.help_btn = self.button('Help')

class NewEmpolyee(NewXXX):
    """ Wrapper for New Empolyee Dialog """
    def __init__(self):
        self.invoke_list = ["Business", "Customer", "New Empolyee..."]
        self.dialog_name = 'New Empolyee'

    def initialize(self):
        NewXXX.initialize(self)
        self.empolyee = self.XXX(self.tab('Empolyee'))
        self.empolyee.empolyee_number_txt = \
        self.empolyee.findChild(predicate.IsLabelledAs('Employee Number: '))
        self.empolyee.username_txt = \
        self.empolyee.findChild(predicate.IsLabelledAs('Username: '))
        self.empolyee.language_txt = \
        self.empolyee.findChild(predicate.IsLabelledAs('Language: '))
        self.empolyee.default_hours_per_day_txt = \
        self.empolyee.findChild(predicate.IsLabelledAs('Default Hours per Day: '))
        self.empolyee.credit_account_cb = \
        self.empolyee.findChild(predicate.IsLabelledAs('Credit Account'))
        self.empolyee.currency_combobox = \
        self.empolyee.findChild(predicate.IsLabelledAs('Currency'))
        self.empolyee.default_rate = \
        self.empolyee.findChild(predicate.IsLabelledAs('Default Rate: '))
        self.empolyee.default_hours_per_day = \
        self.empolyee.findChild(predicate.IsLabelledAs('Default Hours per Day: '))

class FindCustomer(Find):
    """ Find Customer wrapper """

    def __init__(self):
        gnucash = GnuCashApp()
        self.invoke_list = ["Business", "Customer", "Find Customer..."]
        self.dialog_name = 'Find Customer'
        sleep(2)
        find_customer = gnucash.findChild(\
        predicate.GenericPredicate(roleName='dialog', name=self.dialog_name), \
        retry=False, requireResult=False)
        if find_customer != None:
            Window.__init__(self, find_customer)
            self.initialize()


class NewInvoice(GnucashWindow):
    """ New Invoice wrapper """

    def __init__(self):
        self.invoke_list = ["Business", "Customer", "New Invoice..."]
        self.dialog_name = 'New Invoice'

    def initialize(self):
        self.invoice_information_panel = \
        self.findChild(predicate.GenericPredicate(\
        roleName='panel', \
        name='Invoice Information'))
        self.invoice_id_txt = \
        self.findChild(predicate.GenericPredicate(\
        roleName='text', \
        description='The invoice ID number.  If left blank a reasonable number will be chosen for you.'))
        self.billing_information_panel = \
        self.findChild(predicate.GenericPredicate(\
        roleName='panel', \
        name='Billing Information'))
        self.billing_information_list = \
        self.billing_information_panel.findChildren(\
        predicate.GenericPredicate(roleName='text'), recursive=True)
        self.customer_txt = self.billing_information_list[0]
        #self.job_txt = self.billing_information_list[1]
        self.billing_id_txt = self.billing_information_list[1]
        self.notes_panel = \
        self.findChild(predicate.GenericPredicate(roleName='panel', name='Notes'))
        self.notes_txt = \
        self.notes_panel.findChild(predicate.GenericPredicate(roleName='text'))
        self.select_btn = self.button('Select...')

    def __setattr__(self, name, value):
        #TODO: Customer Text does not work
        if name == 'customer':
            self.select_btn.click()
            findCustomer = FindCustomer()
            findCustomer.text_fields_list[0].text = value
            cell = findCustomer.find()[0]
            dogtail.rawinput.click(cell.position[0], cell.position[1]+cell.size[1]*2)
            findCustomer.button('Select').click()
        elif name == 'billing_id':
            self.billing_id_txt.text = value
        elif name == 'notes':
            self.notes_txt.text = value
        else:
            self.__dict__[name] = value

class FindInvoice(Find):
    """ Find Invoice wrapper """

    def __init__(self):
        self.invoke_list = ["Business", "Customer", "Find Invoice..."]
        self.dialog_name = 'Find Invoice'

class NewBill(GnucashWindow):
    """ New Bill wrapper """
    # TODO: refactor the NewBill and NewInvoice

    def __init__(self):
        self.invoke_list = ["Business", "Vendor", "New Bill..."]
        self.dialog_name = 'New Bill'

    def initialize(self):
        self.invoice_information_panel = \
        self.findChild(predicate.GenericPredicate(\
        roleName='panel', \
        name='Invoice Information'))
        self.invoice_id_txt = \
        self.findChild(predicate.GenericPredicate(\
        roleName='text', \
        description='The invoice ID number.  If left blank a reasonable number will be chosen for you.'))
        self.billing_information_panel = \
        self.findChild(predicate.GenericPredicate(\
        roleName='panel', \
        name='Billing Information'))
        self.billing_information_list = \
        self.billing_information_panel.findChildren(predicate.GenericPredicate(roleName='text'), recursive=True)
        self.customer_txt = self.billing_information_list[0]
        #self.job_txt = self.billing_information_list[1]
        self.billing_id_txt = self.billing_information_list[1]
        self.notes_panel = \
        self.findChild(predicate.GenericPredicate(roleName='panel', name='Notes'))
        self.notes_txt = \
        self.notes_panel.findChild(predicate.GenericPredicate(roleName='text'))
        self.select_btn = self.button('Select...')

    def __setattr__(self, name, value):
        #TODO: Customer Text does not work
        if name == 'vendor':
            self.select_btn.click()
            find_vendor = FindVendor()
            find_vendor.text_fields_list[0].text = value
            cell = find_vendor.find()[0]
            dogtail.rawinput.click(cell.position[0], cell.position[1]+cell.size[1]*2)
            find_vendor.button('Select').click()
        elif name == 'billing_id':
            self.billing_id_txt.text = value
        elif name == 'notes':
            self.notes_txt.text = value
        else:
            self.__dict__[name] = value

class FindBill(Find):
    """ Find Bill wrapper """

    def __init__(self):
        self.invoke_list = ["Business", "Vendor", "Find Bill..."]
        self.dialog_name = 'Find Bill'

class NewJob(GnucashWindow):
    """ New Job wrapper """

    def __init__(self):
        self.invoke_list = ["Business", "Customer", "New Job..."]
        self.dialog_name = 'New Job .'


class FindJob(Find):
    """ Find Job wrapper """

    def __init__(self):
        self.invoke_list = ["Business", "Customer", "Find Job..."]
        self.dialog_name = 'Find Job'



class ProcessPayment(GnucashWindow):
    """ Process Payment i use the name of the menu because this dialog has no title ?!"""

    def __init__(self):
        self.invoke_list = ["Business", "Customer", "Process Payment..."]
        self.dialog_name = '' # TODO: Code update required here, to give this dialog an


class FindVendor(Find):
    """ Find Vendor wrapper """

    def __init__(self):
        self.invoke_list = ["Business", "Vendor", "Find Vendor..."]
        self.dialog_name = 'Find Vendor'
        # TODO: Refactor the following code snippet to be in the Find class itself
        sleep(2) #Sleep 2 seconds just to insure the dialog loaded if exist
        gnucash = GnuCashApp()
        find_vendor = gnucash.findChild(\
        predicate.GenericPredicate(roleName='dialog', name=self.dialog_name), \
        retry=False, requireResult=False)
        if find_vendor != None:
            Window.__init__(self, find_vendor)
            self.initialize()



class NewVendorJob(GnucashWindow):
    """ New Job for Vendor 
        TODO: refactor it with the new Job in the customer meny
    """

    def __init__(self):
        self.invoke_list = ["Business", "Vendor", "New Job..."]
        self.dialog_name = 'New Job .'


class FindVendorJob(Find):
    """ Find Job for Vendor """

    def __init__(self):
        self.invoke_list = ["Business", "Vendor", "Find Job..."]
        self.dialog_name = 'Find Job'


class Register(Node):
    """ Wrapper class for gnucash register.
        Current Limitation:
            1- This code could not read the data from register 
            validation done e.g in the account page
            2 - you must have gnucash not minimized and 
            the top level application.
        TODO: It is better to have register supported by a11y to 
        be able to be more readable by dogtail
    """

    def __init__(self, initializer):
        Node.__init__(self, initializer)
        dogtail.rawinput.click(self.position[0], self.position[1])
        self.column_val = 0
        self.prev_col_val = 0

    def __goto(self, x, y):
        sleep(1)
        self.prev_col_val = 0
        self.row_val = x
        self.column_val = y
        # Click to activate the Register widget
        dogtail.rawinput.click(self.position[0], self.position[1])
        for j in range(x):
            self.typeText('\n')
        for i in range(y):
            self.keyCombo("Tab")

    def set_cell_text(self, text):
        """ 
        based on the self.row and self.col set_cell_text get the relative 
        value and move press tab (move right) or Shift-Tab move left by 
        calculating the the difference from the current position
        """
        relative_pos = self.column_val - self.prev_col_val
        if relative_pos > 0 :
            for i in range(relative_pos):
                self.keyCombo("Tab")
        else:
            for i in range(abs(relative_pos)):
                self.keyCombo("<Shift>ISO_Left_Tab")
        self.keyCombo("<Control>a")
        self.keyCombo("Delete")
        self.typeText(text)
#        sleep(5)

    def end_trans(self):
        """ 
        end trans finalize the transaction entery 
        by click Enter and close the register 
        """
        self.keyCombo("Enter")
        GnuCashApp().menu('File').menuItem('Close').click()

    def __setattr__(self, name, value):
        if name == 'row':
            self.row_val = value
            self.__goto(int(value), 0)
        elif name == 'column':
            self.prev_col_val = self.column_val
            self.column_val = value
        elif name == 'date':
            self.prev_col_val = self.column_val
            self.column_val = 0
            self.set_cell_text(value)
        elif name == 'num':
            self.prev_col_val = self.column_val
            self.column_val = 1
            self.set_cell_text(value)
        elif name == 'description':
            self.prev_col_val = self.column_val
            self.column_val = 2
            self.set_cell_text(value)
        elif name == 'transfer':
            self.prev_col_val = self.column_val
            self.column_val = 3
            self.set_cell_text(value)
        elif name == 'deposite':
            self.prev_col_val = self.column_val
            self.column_val = 4
            self.set_cell_text(value)
        elif name == 'withdrawal':
            self.prev_col_val = self.column_val
            self.column_val = 5
            self.set_cell_text(value)
        else:
            self.__dict__[name] = value


class EditInvoice(Node):
    """ 
    Wrapper class for edit invoice 
    tab half dialog and half register 
    """
    class InvoiceRegister(Register):

        def __init__(self, initializer):
            Register.__init__(self, initializer)

        def __goto(self, x, y):
            sleep(1)
            self.prev_col_val = 0
            self.row_val = x
            self.column_val = y
            # Click to activate the Register widget
            dogtail.rawinput.click(self.position[0], self.position[1])
            for j in range(x):
                self.typeText('\n')
            for i in range(y):
                self.keyCombo("Tab")

        def __setattr__(self, name, value):
            if name == 'row':
                self.row_val = value
                self.__goto(int(value), 0)
            elif name == 'column':
                self.prev_col_val = self.column_val
                self.column_val = value
            elif name == 'date':
                self.prev_col_val = self.column_val
                self.column_val = 0
                self.set_cell_text(value)
            elif name == 'description':
                self.prev_col_val = self.column_val
                self.column_val = 1
                self.set_cell_text(value)
            elif name == 'action':
                self.prev_col_val = self.column_val
                self.column_val = 2
                self.set_cell_text(value)
            elif name == 'income_account':
                self.prev_col_val = self.column_val
                self.column_val = 3
                self.set_cell_text(value)
            elif name == 'quantity':
                self.prev_col_val = self.column_val
                self.column_val = 4
                self.set_cell_text(value)
            elif name == 'unit_price':
                self.prev_col_val = self.column_val
                self.column_val = 5
                self.set_cell_text(value)
            elif name == 'discount':
                self.prev_col_val = self.column_val
                self.column_val = 6
                self.set_cell_text(value)
            elif name == 'tax_table':
                self.prev_col_val = self.column_val
                self.column_val = 7
                self.set_cell_text(value)
            else:
                self.__dict__[name] = value

    def __init__(self, initializer):
        """ Pass a tab node to this initializer """
        Node.__init__(self, initializer)
        invoice_entries = self.findChild(predicate.GenericPredicate(\
        name = 'Invoice Entries', roleName = 'panel'))
        invoice_register = invoice_entries.findChild(\
        predicate.GenericPredicate(roleName = 'layered pane'))
        self.invoice_register = self.InvoiceRegister(invoice_register)

class BookOptions(GnucashWindow):
    """ Wapper class for Book options """

    def __init__(self):
        self.invoke_list = ["File", "Properties"]
        self.dialog_name = 'Book Options'

    def initialize(self):
        self.company_name_txt = \
        self.findChild(predicate.GenericPredicate(\
        roleName='text', \
        description='The name of your business'))
        self.company_address_txt  = \
        self.findChild(predicate.GenericPredicate(\
        roleName='text', \
        description='The address of your business'))
        self.company_contact_person_txt = \
        self.findChild(predicate.GenericPredicate(\
        roleName='text', \
        description='The contact person to print on invoices'))
        self.company_phone_number_txt = \
        self.findChild(predicate.GenericPredicate(\
        roleName='text', \
        description='The phone number of your business'))
        self.company_fax_number_txt = \
        self.findChild(predicate.GenericPredicate(\
        roleName='text', \
        description='The fax number of your business'))
        self.company_email_address_txt = \
        self.findChild(predicate.GenericPredicate(\
        roleName='text', \
        description='The email address of your business'))
        self.company_website_url_txt = \
        self.findChild(predicate.GenericPredicate(\
        roleName='text', \
        description='The URL address of your website'))
        self.company_id_txt = \
        self.findChild(predicate.GenericPredicate(\
        roleName='text', \
        description='The ID for your company (eg \'Tax-ID: 00-000000)'))
        #TODO: Add the remaining fields 

    def __setattr__(self, name, value):
        if name == 'company_name':
            self.company_name_txt.text = value
        elif name == 'company_address':
            self.company_address_txt.text = value
        elif name == 'company_phone_number':
            self.company_phone_number_txt.text = value
        elif name == 'company_fax_number':
            self.company_fax_number_txt.text = value
        elif name == 'company_email_address':
            self.company_email_address_txt.text = value
        elif name == 'company_id':
            self.company_id_txt.text = value
        else:
            self.__dict__[name] = value


class Question(Window):

    def __init__(self):
        gnucash = GnuCashApp()
        node = gnucash.findChild(predicate.GenericPredicate(\
        roleName='dialog', \
        name='Question'))
        Window.__init__(self, node)
        self.description_txt = \
        self.findChild(predicate.GenericPredicate(roleName='text'))
        self.post_to_account_combo = \
        self.findChild(predicate.GenericPredicate(roleName='combo box'))
        self.new_btn = self.button('New...')
        self.accumulate_splits_cb = \
        self.findChild(predicate.GenericPredicate(\
        roleName='check box', \
        name='Accumulate Splits?'))

    def __setattr__(self, name, value):
        if name == 'description':
            self.description_txt.text = value
        elif name == 'post_to_account':
            self.post_to_account_combo.combovalue = value
        elif name == 'accumulate_splits':
            if value and not self.accumulate_splits_cb.checked:
                self.accumulate_splits_cb.click()
            elif not value and self.accumulate_splits_cb.checked:
                self.accumulate_splits_cb.click()
        else:
            self.__dict__[name] = value
    def accept(self):
        self.button('OK').click()

class Reconcile(GnucashWindow):
    """ Wrapper Class for Reconcilation dialog """

    def __init__(self):
        self.invoke_list = ["Actions", "Reconcile..."]
        self.dialog_name = ". - Reconcile"

    def initialize(self):
        self.ending_balance_txt = \
        self.findChild(predicate.GenericPredicate( roleName='text' ))
        self.include_subaccount_cb = \
        self.findChild(predicate.GenericPredicate( \
        roleName='check box', \
        name='Include subaccounts'))
        self.enter_payment_btn = self.button('Enter Interest Payment...')
        self.ok_btn = self.button('OK')
        self.cancel_btn = self.button('Cancel')

    def __setattr__(self, name, value):
        if name == 'ending_balance':
            self.ending_balance_txt.text = value
        elif name == 'include_subaccount':
            if value and not self.include_subaccount_cb.checked:
                self.include_subaccount_cb.click()
            elif not value and self.include_subaccount_cb.checked:
                self.include_subaccount_cb.click()
        else:
            self.__dict__[name] = value

class ReconcileFrame(Node):

    class FundsTable(Node):
        """ Wrapper for table in Reconcile Frame """
        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.row_table_cell = 0
            self.column_table_cell = 0

        def __setattr__(self, name, value):
            if name == 'row':
                self.row_table_cell = value
            elif name == 'column':
                self.column_table_cell = value
            else:
                self.__dict__[name] = value

        def click(self, row=None, column=None):
            """ 
            Another row - column based controls i found no action assigned 
            with this table cell Only Activate which does not work because 
            dogtail does not detect the position correctly 
            """
            if row == None:
                row = self.row_table_cell
            if column == None:
                column = self.column_table_cell
            table_cell_list = self.findChildren\
            (predicate.GenericPredicate(roleName='table cell'), recursive=True)
            cell = table_cell_list[(row*5)+column]
            # when click i add double the cell hieght size[1] because dogtail doesn't detect the position correctly 
            # it detects it above its rendered position 
            dogtail.rawinput.click(cell.position[0], cell.position[1]+cell.size[1]*2)

    def __init__(self):
        config.childrenLimit = 1500
        gnucash = GnuCashApp()
        frame_list = gnucash.findChildren(predicate.GenericPredicate(roleName='frame'))
        for frame in frame_list:
            result = re.search(". Reconcile", frame.name)
            if (result != None):
                asset_rec = frame
        Node.__init__(self, asset_rec)
        self.funds_out_panel = \
        self.findChild(predicate.GenericPredicate(roleName='panel', name='Funds Out'))
        self.funds_out_table = \
        self.funds_out_panel.findChild(predicate.GenericPredicate(roleName='table'))
        self.funds_out = self.FundsTable(self.funds_out_table)
        self.funds_in_panel = \
        self.findChild(predicate.GenericPredicate(roleName='panel', name='Funds In'))
        self.funds_in_table = \
        self.funds_in_panel.findChild(predicate.GenericPredicate(roleName='table'))
        self.funds_in = self.FundsTable(self.funds_in_table)

    def __select_all_funds(self, funds_type):
        """ Select All funds out item in funds out table """
        funds_panel = self.findChild(\
        predicate.GenericPredicate(roleName='panel', name=funds_type))
        funds_table = \
        funds_panel.findChild(predicate.GenericPredicate(roleName='table'))
        funds = self.FundsTable(funds_table)
        tabel_cell_list = funds_panel.findChildren(\
        predicate.GenericPredicate(roleName='table cell'), recursive=True)
        for i in  range((len(tabel_cell_list)/5)):
            funds.row = i
            funds.col = 2
            funds.click()

    def select_all_funds_out(self):
        self.__select_all_funds('Funds Out')

    def select_all_funds_in(self):
        self.__select_all_funds('Funds In')

    def finish(self):
        self.menu('Reconcile').menuItem('Finish').click()

class NewAccountSetup(Wizard):

    def __init__(self, node):
        Wizard.__init__(self, node)

    def set_account_category(self, category_list):
        simple_checkbook = self.currentPage().child(name='A Simple Checkbook')
        self.currentPage().button('Clear All').click()
        categorize_table = \
        simple_checkbook.findAncestor(\
        predicate.GenericPredicate(roleName='table'))
        cells = categorize_table.findChildren(\
        predicate.GenericPredicate(roleName='table cell'))
        for category in category_list:
            for i, cell in enumerate(cells):
                if cell.name == category:
                    cells[i-1].doAction('toggle')

if __name__ == '__main__':
    """ This main Changes Frequently because it used to test most recent added widget """
    config.childrenLimit = 1500
    gnucash = GnuCashApp()
    export_report = ExportReport()
    export_report.invoke()
    export_report.export_report('testdffdsf')
