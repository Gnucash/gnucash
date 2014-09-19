#ifndef kvp_frame_p_header
#define kvp_frame_p_header

/* Note that we keep the keys for this hash table in the
 * qof_string_cache, as it is very likely we will see the
 * same keys over and over again  */

struct _KvpFrame
{
    GHashTable  * hash;
};


#endif
