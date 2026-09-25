import sys
from gnucash import *
from gnucash import _sw_app_utils
from gnucash import _sw_core_utils
from gnucash._sw_core_utils import gnc_prefs_is_extra_enabled, gnc_prefs_is_debugging_enabled
from gi import require_version
require_version('Gtk', '4.0')
from gi.repository import Gtk
import os

sys.path.append(os.path.dirname(__file__))

# output file location if gnucash has been started with
# gnucash --extra
if gnc_prefs_is_extra_enabled():
    print("Python shell init file: %s" % (__file__))
    print("\n" + "The following string should appear translated in your preferred language:" + "\n")
    print("\n" + _("Welcome to GnuCash") +"\n")

# Importing the console class causes SIGTTOU to be thrown if GnuCash is
# started in the background.  This causes a hang if it is not handled, 
# so ignore it for the duration
import signal
old_sigttou = signal.signal(signal.SIGTTOU, signal.SIG_IGN)

import pycons.console as cons

# Restore the SIGTTOU handler
signal.signal(signal.SIGTTOU, old_sigttou)

# output debug information if gnucash has been started with
# gnucash --debug --extra
if gnc_prefs_is_extra_enabled() and gnc_prefs_is_debugging_enabled():
    print("Hello from python!\n")
    print("sys.modules.keys(): ", sys.modules.keys(), "\n")
    print("dir(_sw_app_utils): ", dir(_sw_app_utils), "\n")

    #session = app_utils.gnc_get_current_session()
    #root account can later on be accessed by session.get_book().get_root_account()

   #print("test", dir(root), root.__class__)
    print("dir(gnucash_core_c): ", dir(gnucash_core_c))

   #acct = Account(instance = root)

   #print("test3", dir(acct))
   #print(acct.GetName())
   #print(acct.GetBalance())
   #print(acct.GetSplitList())
   #print("test2", dir(gnucash.gnucash_core_c))

class Console (cons.Console):
    """ GTK python console """

    def __init__(self, argv=[], shelltype='python', banner=[],
                 filename=None, size=100, user_local_ns=None, user_global_ns=None):
        cons.Console.__init__(self, argv, shelltype, banner, filename, size,
                        user_local_ns=user_local_ns, user_global_ns=user_global_ns)
        self.buffer.create_tag('center',
                               justification=Gtk.Justification.CENTER,
                               font='Mono 4')
        self.figures = []
        self.callbacks = []
        self.last_figure = None
        self.active_canvas = None


    def key_press_event (self, controller, keyval, keycode, state):
        """ Handle key press event """
        return cons.Console.key_press_event (self, controller, keyval,
                                             keycode, state)

    def quit_event (self, widget):
        """ Event handler for closing of console window """
        self.quit()
        return False
    
    def refresh (self):
        """ Refresh drawing """
        for fig in self.figures:
            figure, canvas, anchor = fig
            canvas.draw()
        return False

    def quit (self):
        """ quit """

        self.write("\n" + _("Have a nice day!") + "\n")
        return super(Console, self).quit()


# Change this to "if True:" to switch on a python console at gnucash
# startup:
# shelltype can either be "python" or "ipython" (the latter is not yet fully functional)
if False:
    shelltype = "python"
    if shelltype=="python":
        shelltypeName = "Python"
    else:
        shelltypeName = "IPython"
    banner_style = 'title'
    # TRANSLATORS: %s is either Python or IPython
    banner = _("Welcome to GnuCash %s Shell") % shelltypeName
    window = Gtk.Window()
    window.set_default_size(800,600)
    application = Gtk.Application.get_default()
    if application:
        window.set_application(application)

    console = Console(argv = [], shelltype = shelltype, banner = [[banner, banner_style]],
                            size = 100, user_local_ns=locals(), user_global_ns=globals())

    window.connect('close-request', console.quit_event)
    window.set_child(console)
    window.present()
    console.view.grab_focus()
