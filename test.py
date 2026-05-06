# Ah, I see what the reviewer meant!
# The user issue description:
# Current Code:
#         for ( i = 0; i < toLen; i++ )
#         {
#             paymentToAccts = g_list_append( paymentToAccts,
#                                             GINT_TO_POINTER( paymentTos[i] ) );
#         }
# BUT the codebase *actually* has:
#         for ( i = 0; i < fromLen; i++ )
#         {
#             paymentFromAccts = g_list_append( paymentFromAccts,
#                                             GINT_TO_POINTER( paymentFroms[i] ) );
#             paymentToAccts = g_list_append( paymentToAccts,
#                                             GINT_TO_POINTER( paymentFroms[i] ) );
#         }
# and the `toLen` loop ALREADY uses prepend!
# Wait! In the user prompt:
# `for ( i = 0; i < toLen; i++ )`
# AND the reviewer said: "The patch attempts to solve the problem but misses the mark. While it successfully optimizes a very similar `fromLen` loop and another `g_list_append` in `loan_rev_hash_to_list`, it completely omits the `toLen` loop that was explicitly cited in the issue description."
# What?! Is there ANOTHER `toLen` loop? Let's check `grep -n -C 3 toLen gnucash/gnome/assistant-loan.cpp` for the whole file!
