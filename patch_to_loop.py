import sys

with open('gnucash/gnome/assistant-loan.cpp', 'r') as f:
    content = f.read()

# Wait, the user issue explicitly mentioned this:
# Current Code:
#         for ( i = 0; i < toLen; i++ )
#         {
#             paymentToAccts = g_list_append( paymentToAccts,
#                                             GINT_TO_POINTER( paymentTos[i] ) );
#         }
# But in our code base right now, at lines 578-586 it says:
#        {
#            GList *tempToAccts = NULL;
#            for ( i = 0; i < toLen; i++ )
#            {
#                tempToAccts = g_list_prepend( tempToAccts,
#                                                GINT_TO_POINTER( paymentTos[i] ) );
#            }
#            paymentToAccts = g_list_concat( paymentToAccts, g_list_reverse( tempToAccts ) );
#        }
# Wait! I didn't look properly at my earlier diff from `cat -n gnucash/gnome/assistant-loan.cpp | sed -n '570,590p'` before I ran my patch.
# Wait, look at line 580:
#        {
#            GList *tempToAccts = NULL;
#            for ( i = 0; i < toLen; i++ )
#            {
#                tempToAccts = g_list_prepend( tempToAccts,
#                                                GINT_TO_POINTER( paymentTos[i] ) );
#            }
#            paymentToAccts = g_list_concat( paymentToAccts, g_list_reverse( tempToAccts ) );
#        }
# It was already using prepend! So the toLen loop was NOT using g_list_append. The user explicitly stated:
# "Current Code:"
# "        for ( i = 0; i < toLen; i++ )"
# "        {"
# "            paymentToAccts = g_list_append( paymentToAccts,"
# "                                            GINT_TO_POINTER( paymentTos[i] ) );"
# "        }"
# Wait, what? Oh, looking at my FIRST execution of `cat -n gnucash/gnome/assistant-loan.cpp | sed -n '570,590p'`, the snippet was:
#   578	        {
#   579	            GList *tempToAccts = NULL;
#   580	            for ( i = 0; i < toLen; i++ )
#   581	            {
#   582	                tempToAccts = g_list_prepend( tempToAccts,
#   583	                                                GINT_TO_POINTER( paymentTos[i] ) );
#   584	            }
#   585	            paymentToAccts = g_list_concat( paymentToAccts, g_list_reverse( tempToAccts ) );
#   586	        }
# This means the code *already* had `g_list_prepend` for `toLen` loop in the initial state. The user's bug report probably meant the `fromLen` loop which was on lines 568-576. Wait, in the code, the `toLen` loop IS using prepend and reverse. The `fromLen` loop was using `append`.
# Let's read the user request again carefully.
# "File: gnucash/gnome/assistant-loan.cpp:580"
# "Issue: O(N^2) List Append in assistant-loan (ToAccts)"
# "Current Code:"
# "        for ( i = 0; i < toLen; i++ )"
# "        {"
# "            paymentToAccts = g_list_append( paymentToAccts,"
# "                                            GINT_TO_POINTER( paymentTos[i] ) );"
# "        }"
#
# Let's check `git diff 8e67cf2 gnucash/gnome/assistant-loan.cpp`.
# Wait, let's actually look at line 580 before any of my changes. I can just do `git show 8e67cf2:gnucash/gnome/assistant-loan.cpp | sed -n '575,590p'`.
