import timeit

large_list = ['Tab', 'Return', 'Right', 'Down', 'Up', 'Left',
              'Backspace', 'Home', 'End', 'Page_Up', 'Page_Down',
              'Shift_L', 'Shift_R', 'Control_L', 'Control_R',
              'Alt_L', 'Alt_R', 'Caps_Lock', 'Escape', 'Clear']
large_set = set(large_list)

list_time = timeit.timeit("key in ['Tab', 'Return', 'Right', 'Down', 'Up', 'Left', 'Backspace', 'Home', 'End', 'Page_Up', 'Page_Down', 'Shift_L', 'Shift_R', 'Control_L', 'Control_R', 'Alt_L', 'Alt_R', 'Caps_Lock', 'Escape', 'Clear']", setup="key='Clear'", number=10000000)
set_time = timeit.timeit("key in {'Tab', 'Return', 'Right', 'Down', 'Up', 'Left', 'Backspace', 'Home', 'End', 'Page_Up', 'Page_Down', 'Shift_L', 'Shift_R', 'Control_L', 'Control_R', 'Alt_L', 'Alt_R', 'Caps_Lock', 'Escape', 'Clear'}", setup="key='Clear'", number=10000000)

print(f"List time (10M ops): {list_time:.4f}s")
print(f"Set time (10M ops):  {set_time:.4f}s")
