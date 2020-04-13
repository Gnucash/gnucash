import sys
import gnucash._sw_app_utils as _sw_app_utils
from gnucash import *
from gnucash._sw_core_utils import gnc_prefs_is_extra_enabled, gnc_prefs_is_debugging_enabled
from gi import require_version
require_version('Gtk', '3.0')
from gi.repository import Gtk
import os
sys.path.append(os.path.dirname(__file__))
if gnc_prefs_is_extra_enabled():
    print("woop", os.path.dirname(__file__))
# Importing the console class causes SIGTTOU to be thrown if GnuCash is
# started in the background.  This causes a hang if it is not handled, 
# so ignore it for the duration
import signal
old_sigttou = signal.signal(signal.SIGTTOU, signal.SIG_IGN)

import pycons.console as cons

# Restore the SIGTTOU handler
signal.signal(signal.SIGTTOU, old_sigttou)

if gnc_prefs_is_extra_enabled() and gnc_prefs_is_debugging_enabled():
    print("Hello from python!")
    print("test", sys.modules.keys())
    print("test2", dir(_sw_app_utils))

   #root = _sw_app_utils.gnc_get_current_root_account()

   #print("test", dir(root), root.__class__)
    print("test3", dir(gnucash_core_c))

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
        self.view.connect ('key-press-event', self.key_press_event)
        self.view.connect ('button-press-event', self.button_press_event)
        self.view.connect ('scroll-event', self.scroll_event)


    def key_press_event (self, widget, event):
        """ Handle key press event """
        
        if self.active_canvas:
            self.active_canvas.emit ('key-press-event', event)
            return True
        return cons.Console.key_press_event (self, widget, event)

    def scroll_event (self, widget, event):
        """ Scroll event """
        if self.active_canvas:
            return True
        return False
 
    def button_press_event (self, widget, event):
        """ Button press event """
        return self.refresh()

    def quit_event (self, widget, event):
        """ Event handler for closing of console window """
        return self.quit()
    
    def refresh (self):
        """ Refresh drawing """
        for fig in self.figures:
            figure, canvas, anchor = fig
            canvas.draw()
        return False

    def quit (self):
        """ quit """

        self.write("\nHave a nice day !\n")
        return super(Console, self).quit()


# Change this to "if True:" to switch on a python console at gnucash
# startup:
# shelltype can either be "python" or "ipython" (the latter is not yet fully functional)
if False:
    shelltype = "python"
    title = "gnucash "+shelltype+" shell"
    banner_style = 'title'
    banner = "Welcome to "+title+"!\n"

    window = Gtk.Window(type = Gtk.WindowType.TOPLEVEL)
    window.set_position(Gtk.WindowPosition.CENTER)
    window.set_default_size(800,600)
    window.set_border_width(0)

    console = Console(argv = [], shelltype = shelltype, banner = [[banner, banner_style]],
                            size = 100, user_local_ns=locals(), user_global_ns=globals())

    window.connect('destroy-event', console.quit_event)
    window.connect('delete-event', console.quit_event)
    window.add (console)
    window.show_all()
    console.grab_focus()
