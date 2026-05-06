import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gtk, Gdk

def get_keyname(keyval):
    return Gdk.keyval_name(keyval)

print(get_keyname(Gdk.KEY_Return))
print(get_keyname(Gdk.KEY_Left))
print(get_keyname(Gdk.KEY_Shift_L))
print(get_keyname(Gdk.KEY_Caps_Lock))
print(get_keyname(Gdk.KEY_Clear))
