
/* If a split has been pulled apart to make it fit into two (or more)
 * lots, then it becomes theoretically possible for each subsplit to
 * have a distinct price.  But this would be wrong: each subsplit should
 * have the same price, within rounding errors.  This routine will
 * examine the indicated split for sub-splits, and adjust the value
 * of each so that they all have the same price.
 */

#include "kvp_frame.h"
#include "TransactionP.h"

void
xaccScrubSubSplitPrice (Split *split)
{
   KvpValue *kval;
   KvpFrame *ksub;
   gnc_numeric src_amt, src_val;
   SplitList *node;

   if (!split) return;
   g_return_if_fail (split->parent);

   /* If there are no sub-splits, ten there's nothing to do. */
   kval = kvp_frame_get_value (split->kvp_data, "lot-split");
   if (!kval) return;  

   ksub = kvp_value_get_frame (kval);
   g_return_if_fail (ksub);

   /* Get 'price' of the indicated split */
   src_amt = xaccSplitGetAmount (split);
   src_val = xaccSplitGetValue (split);

   for (node=split->parent->splits; node; node=node->next)
   {
      Split *s = node->data;
      gnc_numeric dst_amt, dst_value, target_val;

      dst_amt = xaccSplitGetAmount (s);
      dst_val = xaccSplitGetValue (s);
      target_val = gnc_numeric_mul (dst_amt, src_val,
                        GNC_DENOM_AUTO, GNC_DENOM_REDUCE);
      target_val = gnc_numeric_div (target_val, src_amt,
                        gnc_numeric_denom(dst_val), GNC_DENOM_EXACT);
   }
}
