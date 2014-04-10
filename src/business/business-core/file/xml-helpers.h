
/* xml-helpers.h
 * Miscellaneous bogus helper routines.
 */

static inline void
maybe_add_int (xmlNodePtr ptr, const char *tag, gint val)
{
  if (val)
    xmlAddChild (ptr, int_to_dom_tree (tag, val));
}

static inline void
maybe_add_numeric (xmlNodePtr ptr, const char *tag, gnc_numeric val)
{
  if (!gnc_numeric_zero_p (val))
    xmlAddChild (ptr, gnc_numeric_to_dom_tree (tag, &val));
}

static inline void
maybe_add_string (xmlNodePtr ptr, const char *tag, const char *str)
{
  if (str && strlen(str) > 0)
    xmlAddChild (ptr, text_to_dom_tree (tag, str));
}

static inline void
maybe_add_guid (xmlNodePtr ptr, const char *tag, QofInstance *inst)
{
  if (inst)
    xmlAddChild (ptr, guid_to_dom_tree (tag,
          qof_instance_get_guid (inst)));
}

