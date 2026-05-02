import timeit

setup = """
keyname = 'Return'
"""

stmt_list = """
if keyname in ['Return', 'KP_Enter', 'Tab', 'Right', 'Down', 'Up', 'Left',
               'Backspace', 'Home', 'End', 'Page_Up', 'Page_Down',
               'Shift_L', 'Shift_R', 'Control_L', 'Control_R',
               'Alt_L', 'Alt_R', 'Caps_Lock', 'Escape', 'Clear']:
    pass
"""

stmt_set = """
if keyname in {'Return', 'KP_Enter', 'Tab', 'Right', 'Down', 'Up', 'Left',
               'Backspace', 'Home', 'End', 'Page_Up', 'Page_Down',
               'Shift_L', 'Shift_R', 'Control_L', 'Control_R',
               'Alt_L', 'Alt_R', 'Caps_Lock', 'Escape', 'Clear'}:
    pass
"""

list_time = timeit.timeit(stmt_list, setup, number=10000000)
set_time = timeit.timeit(stmt_set, setup, number=10000000)

print(f"List (First Item): {list_time:.4f}s")
print(f"Set (First Item):  {set_time:.4f}s")
print(f"Improvement: {(list_time - set_time) / list_time * 100:.2f}%")
