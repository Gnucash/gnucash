with open('gnucash/gnome/assistant-loan.cpp', 'r') as f:
    content = f.read()

search = """    rrr  = g_new0( RevRepaymentRow, 1 );
    rrr->date = *(GDate*)key;
    rrr->numCells = (gnc_numeric*)val;
    *l = g_list_append( *l, (gpointer)rrr );"""

replace = """    rrr  = g_new0( RevRepaymentRow, 1 );
    rrr->date = *(GDate*)key;
    rrr->numCells = (gnc_numeric*)val;
    *l = g_list_prepend( *l, (gpointer)rrr );"""

if search in content:
    content = content.replace(search, replace)
    with open('gnucash/gnome/assistant-loan.cpp', 'w') as f:
        f.write(content)
    print("Replaced hash_to_list successfully.")
else:
    print("Search string not found.")
