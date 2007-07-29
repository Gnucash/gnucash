""" wrapper code to help when scripting GnuCash testcases
    Author: Ahmed Sayed Hassan <ahmadsayed83@yahoo.com>
"""
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
from dogtail.procedural import click
from time import sleep
from dogtail import tree, predicate
from dogtail.config import config
from dogtail.procedural import focus


# use the the following list to dismiss the code in this Order Cancel, No, Then Close, .... Add more if others
default_dismiss_list = ['Close', 'Cancel', 'No'] 
default_accept_list = ['OK', 'Yes', 'Apply', 'Accept']
def in_dismiss_list(button, dismiss_list=default_dismiss_list):
    """Check if the Button already in the dismiss list you could override the default behaviour by passing
        your own dismiss list to the code
    """
    for button_name in dismiss_list:
        if button_name == button.name:
            return button
    return None

def in_accept_list(button, accept_list=default_accept_list):
    """Check if the Button already in the accept list you could override the default behaviour by passing
        your own accept list to the code
    """
    for button_name in accept_list:
        if button_name == button.name:
            return button
    return None

def cleanup():
    """ Remove only the First Entry of the recent in order to avoid opening recent application """
    os.system("gconftool-2 --unset /apps/gnucash/history/file0")

def ungraceful_close():
    """ kill gnuCash Process used when Hang expected """
    os.system("killall gnucash-bin")

def cleanup_all():
    """ usefull to delete all GnuCash to run first time wizard or when things go extemely wrong"""
    ungraceful_close()
    sleep(5)
    os.system("gconftool-2 --recursive-unset /apps/gnucash")

class GnuCashApp (Application):

    """ Wrapper for GnuCash Application"""
    def __init__(self):
        Application.__init__(self, root.application("gnucash"))
        self.data_file_path = '/home/ahmed/work/gnucash-dogtail/src/test-dogtail/'
        self.data_file_dest = self.data_file_path + 'projects_under_test/'
        self.data_file_src = self.data_file_path + 'projects/'

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
        """ using GUI to open already exist project we have two folders (projects) where we save the project before applying test case procedure and (projects_under_test) where we copy the current project and begin using it in the testcase """
        data_file_path = self.data_file_src + data_file_name
        data_file_dest_path = self.data_file_dest + data_file_name
        shutil.copy(self.data_file_src+data_file_name, self.data_file_dest)
        open_data_file = Open()
        open_data_file.invoke()
        open_data_file.open_location(data_file_dest_path)

    def dismiss_all_dialogs(self):
        """ dismiss all dialogs using procedural this should be migrated to tree but there is a problem with the
        Yes/No dialogs apears after dismiss the first wizard"""
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
    
    def add_account (self, account_name, account_type=None, account_parent=None, smallest_fraction=None, account_code=None,   description=None, notes=None):
        """ Add account this function able to add only two levels parent child account """
        if account_parent != None:
            account_table = self.findChild(predicate.GenericPredicate(roleName='page tab', name='Accounts'))
            parent_cell = account_table.findChild(predicate.GenericPredicate(roleName='table cell', name=account_parent))
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
            print self.dialog_name
            print dialog.name
            if result != None:
                Window.__init__(self, dialog)
                self.initialize()
                return
            else:
                print "Not called"

    def dismiss(self):
        """" when calling dismiss the expected action is Cancel, Close and No """
        button_list = self.findChildren(predicate.GenericPredicate(roleName='push button'))
        for button in button_list:
            dismiss_button = in_dismiss_list(button)
            if dismiss_button != None:
                if dismiss_button.showing:
                    dismiss_button.click()
                    return True
        return False

    def accept(self):
        """" when calling dismiss the expected action is Cancel, Close and No """
        button_list = self.findChildren(predicate.GenericPredicate(roleName='push button'))
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
        self.location_txt = self.findChild(predicate.IsLabelledAs('Location:'))

    def __setattr__(self, name, value):
        if name == 'location':
            self.location_txt.text = value
        else:
            self.__dict__[name]=value

    def open_location(self, file_path):
        self.location = file_path
        self.button('Open').click()


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
        """ Always set the account name as your last account because it changes the dialog title which may lead to failure
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
            self.account_type_table.findChild(predicate.GenericPredicate(roleName="table cell", name=value )).select()
        elif name == 'account_parent':
            self.account_parent_table.findChild(predicate.GenericPredicate(roleName="table cell", name='New top level account')).doAction('activate')
            self.account_parent_table.findChild(predicate.GenericPredicate(roleName="table cell", name=value )).doAction('activate')
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
            self.__dict__[name]=value

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
            """ Init method for Accounting Dialog Calender does not work, can not find a way to use it """
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
            self.enable_euro_support = \
            self.findChild(predicate.GenericPredicate(\
            roleName='check box', \
            name='Enable euro support'))
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
    """ Wrapper for all gnucash Find include Find txns, Find Customer, Find Job, ... """
    
    def __init__(self):
        pass

    def initialize(self):
        """ initialize Common components """

        self.add_btn = self.button('Add')
        self.remove_btn_list = \
        self.findChildren(\
        predicate.GenericPredicate(roleName='push button', name='Remove'))
        self.search_criteria_filler_list = \
        self.findChildren(\
        predicate.GenericPredicate(roleName='filler'))
        self.find_btn = self.button('Find')
        self.new_search_rb = \
        self.findChild(\
        predicate.GenericPredicate(roleName='radio button', name='New search'))
        self.refine_current_search_rb = \
        self.findChild(\
        predicate.GenericPredicate(roleName='radio button', name='Refine current search'))
        self.add_results_to_current_search_rb = \
        self.findChild(\
        predicate.GenericPredicate(roleName='radio button', name='Add results to current search'))
        self.delete_results_from_current_search = \
        self.findChild(\
        predicate.GenericPredicate(roleName='radio button', name='Delete results from current search'))



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
        """ intialize Tax options dialog components 
            TODO: Compelete the remaining fields after thing became more clear with this dialog
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
        self.style_sheets_table = self.findChild(predicate.GenericPredicate(roleName='table'))


class NewStyleSheet(Window):
    """ New Style Sheet dialog """

    def __init__(self, Node):
        Window.__init__(self, Node)
        self.name_text = self.findChild(predicate.GenericPredicate(roleName='text'))
        self.template_combo = self.findChild(predicate.GenericPredicate(roleName='combo box'))
        self.ok_btn = self.button('OK')
        self.cancel_btn = self.button('Cancel')

    def __setattr__(self, name, value):
        """ Set The Style Sheet """
        if name == 'style_name':
            self.name_text.text = value
        elif name == 'template':
            self.template_combo.combovalue = value
        else:
            self.__dict__[name]=value

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
            TODO: confirm the selet all and unselect those controls not visible while writing this code
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
    """ This class used to hold the common controls between the New Customer, New Vendor and New Empolyee 
        TODO: Replace XXX with a nice name, this name is terrible, i can not call other one
    """

    class XXX(Node):
        """ 
        Class for 
            Vendor Tab in New Vendor
            Customer in New Customer
            Empolyee in New Empolyee
        """

        def __init__(self, initialize):
            Node.__init__(self, initialize)
            #self.company_name_txt = self.findChild(\
            #predicate.IsLabelledAs('Company Name: '))
            self.billing_information = \
            self.findChild(predicate.GenericPredicate\
            (roleName='panel', name ='Billing Address'))
            self.billing_information_elements = \
            self.findChildren(predicate.GenericPredicate\
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

    class YYYInformation(Node):
        """
        Class to wrap 
            1 - Billing Information in New Customer
            2 - Payment Information in New Vendor (Not all member variable used in this case
        """
        def __init__(self, initializer):
            Node.__init__(self, initializer)
            self.currency_combobox = \
            self.findChild(predicate.GenericPredicate(roleName='combo box', name='USD (US Dollar)'))
            self.terms_combobox = \
            self.findChild(predicate.GenericPredicate(roleName='combo box', name='None'))
            self.info_elements = self.findChildren(predicate.GenericPredicate(roleName='text'))
            self.discount_txt = self.info_elements[0]
            self.credit_limit_txt = self.info_elements[1]
            self.tax_included_combobox = \
            self.findChild(predicate.GenericPredicate(roleName='combo box', name='Use Global'))
            self.override_global_tax_cb = \
            self.findChild(predicate.GenericPredicate(\
            roleName= 'check box', \
            description='Override the global Tax Table?'))
            self.tax_table_combobox = \
            self.findChild(predicate.GenericPredicate\
            (roleName='combo box', \
            description='What Tax Table should be applied to this customer?'))

    def __init__(self):
        pass

    def initialize(self):
        """ Initialize new generic components """
        self.ok_btn = self.button('OK')
        self.cancel_btn = self.button('Cancel')
        self.help_btn = self.button('Help')


class NewCustomer(NewXXX):
    """ New Customer wrapper """
    class ShippingAddress(Node):

        def __init__(self, initializer):
            Node.__init__(self, intializer)
            self.name_txt = self.findChild(predicate.IsLabelledAs('Name: '))
            self.address_txt = self.findChild(predicate.IsLabelledAs('Address: '))
            self.phone_txt = self.findChild(predicate.IsLabelledAs('Phone: '))
            self.fax_txt = self.findChild(predicate.IsLabelledAs('Fax: '))
            self.email_txt = self.findChild(predicate.IsLabelledAs('Email: '))
            
    def __init__(self):
        self.invoke_list = ["Business", "Customer", "New Customer..."]
        self.dialog_name = 'New Customer'
        

    def initialize(self):
        NewXXX.initialize(self)
        self.customer = self.XXX(self.tab('Customer'))
        self.billing_information = self.YYYInformation(self.tab('Billing Information'))
        self.identification_panel = \
        self.customer.findChild(predicate.GenericPredicate(roleName='panel', name='Identification'))
        self.identification_panel_element = \
        self.identification_panel.findChildren(predicate.GenericPredicate(roleName='text'))
        self.customer.customer_number_txt = self.identification_panel_element[0]
        self.billing_information = \
        self.YYYInformation(self.tab('Billing Information'))
        self.shipping_information = self.tab('Shipping Address')
        self.ok_btn = self.button('OK')
        self.cancel_btn = self.button('Cancel')
        self.help_btn = self.button('Help')

class NewVendor(NewXXX):

    def __init__(self):
        self.invoke_list = ["Business", "Customer", "New Vendor..."]
        self.dialog_name = 'New Vendor'

    def initialize(self):
        NewXXX.initialize(self)
        self.vendor = self.XXX(self.tab('Vendor'))
        self.vendor.vendor_number_txt = \
        self.vendor.findChild(predicate.IsLabelledAs('Vendor Number: '))
        self.payment_information = \
        self.YYYInformation(self.tab('Payment Information'))

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
        self.invoke_list = ["Business", "Customer", "Find Customer..."]
        self.dialog_name = 'Find Customer'


class NewInvoice(GnucashWindow):
    """ New Invoice wrapper """

    def __init__(self):
        self.invoke_list = ["Business", "Customer", "New Invoice..."]
        self.dialog_name = 'New Invoice'


class FindInvoice(Find):
    """ Find Invoice wrapper """

    def __init__(self):
        self.invoke_list = ["Business", "Customer", "Find Invoice..."]
        self.dialog_name = 'Find Invoice'

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

class NewVendor(GnucashWindow):
    """ New Vendor """

    def __init__(self):
        self.invoke_list = ["Business", "Vendor", "New Vendor..."]
        self.dialog_name = 'New Vendor'


class FindVendor(Find):
    """ Find Vendor wrapper """

    def __init__(self):
        self.invoke_list = ["Business", "Vendor", "Find Vendor..."]
        self.dialog_name = 'Find Vendor'



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
    """ Wrapper class for gnucash register 
        TODO: It is better to have register supported by a11y to be able to be more readable by dogtail
    """ 

    def __init__(self, initializer):
        Node.__init__(self, initializer)
        dogtail.rawinput.click(self.position[0], self.position[1])
        self.column_val = 0
        self.prev_col_val = 0

    def __goto(self, x, y):
        sleep(1)
        # Click to activate the Register widget
        dogtail.rawinput.click(self.position[0], self.position[1])
        for j in range(x):
            self.typeText('\n')
        for i in range(y):
            self.keyCombo("Tab")

    def set_cell_text(self, text):
        """ based on the self.row and self.col set_cell_text get the relative value and move press tab (move right) or Shift-Tab move left based on the the difference """
        relative_pos = self.column_val - self.prev_col_val
        print relative_pos
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
        """ end trans finalize the transaction entery by click Enter and close the register """
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
            self.__dict__[name]=value

class BookOptions(GnucashWindow):
    """ Wapper class for Book options """

    def __init__(self):
        self.invoke_list= ["File", "Properties"]
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
        else:
            self.__dict__[name]=value


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
            self.__dict__[name]=value

class Reconcile(GnucashWindow):
    """ Wrapper Class for Reconcilation dialog """
    
    def __init__(self):
        self.invoke_list = ["Actions", "Reconcile..."]
        self.dialog_name = ". - Reconcile"            # TODO: This line does not work as expected 
                                                      # current work around override the dialog_name

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
        if name =='ending_balance':
            self.ending_balance_txt.text = value
        elif name == 'include_subaccount':
            if value and not self.include_subaccount_cb.checked:
                self.include_subaccount_cb.click()
            elif not value and self.include_subaccount_cb.checked:
                self.include_subaccount_cb.click()
        else:
            self.__dict__[name]=value

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
                self.__dict__[name]=value

        def click(self, row=None, column=None):
            """ Another row - column based controls i found no action assigned with this table cell Only Activate which does not work because dogtail does not detect the position correctly """
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
        funds_panel = \
        self.findChild(predicate.GenericPredicate(roleName='panel', name=funds_type))
        funds_table = \
        funds_panel.findChild(predicate.GenericPredicate(roleName='table'))
        funds = self.FundsTable(funds_table)
        tabel_cell_list = funds_panel.findChildren(predicate.GenericPredicate(roleName='table cell'), recursive=True)
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

if __name__ == '__main__':
    """ This main Changes Frequently because it used to test most recent added widget """
    gnucash = GnuCashApp()
    book_options = BookOptions()
    book_options.invoke()
    book_options.dismiss()

