#! /usr/bin/perl

@files = <STDIN>;

# print @files;

foreach (@files)
{
	chop;
	$afile= $_;
	open (AF, $afile);
	open (OF, "> " . $afile . ".tmp");

	while (<AF>)
	{
		# s/GncObject_t/QofObject/g;
		# s/gncObjectLookup/qof_object_lookup/g;
		# s/gncObjectRegister/qof_object_register/g;
		# s/gncObjectGetTypeLabel/qof_object_get_type_label/g;
		# s/gncObjectRegisterBackend/qof_object_register_backend/g;
		# s/gncObjectLookupBackend/qof_object_lookup_backend/g;
		# s/gncObjectForeachBackend/qof_object_foreach_backend/g;
		# s/gncObjectInitialize/qof_object_initialize/g;

		# s/gncObjectShutdown/qof_object_shutdown/g;
		# s/gncObjectBookBegin/qof_object_book_begin/g;
		# s/gncObjectBookEnd/qof_object_book_end/g;
		# s/gncObjectIsDirty/qof_object_is_dirty/g;
		# s/gncObjectMarkClean/qof_object_mark_clean/g;

		# s/gncObjectForeachType/qof_object_foreach_type/g;
		# s/gncObjectForeach/qof_object_foreach/g;
		# s/gncObjectPrintable/qof_object_printable/g;
		# s/gncObjectx/qof_object_x/g;
		# s/gncObjectx/qof_object_x/g;
		# s/gncObjectx/qof_object_x/g;

		# s/foreachTypeCB/QofForeachTypeCB/g;
		# s/foreachBackendTypeCB/QofForeachBackendTypeCB/g;

		# s/gncQueryStringPredicate/qof_query_string_predicate/g;
		# s/gncQueryDatePredicate/qof_query_date_predicate/g;
		# s/gncQueryNumericPredicate/qof_query_numeric_predicate/g;
		# s/gncQueryGUIDPredicate/qof_query_guid_predicate/g;
		# s/gncQueryInt32Predicate/qof_query_int32_predicate/g;
		# s/gncQueryInt64Predicate/qof_query_int64_predicate/g;
		# s/gncQueryDoublePredicate/qof_query_double_predicate/g;
		# s/gncQueryBooleanPredicate/qof_query_boolean_predicate/g;
		# s/gncQueryCharPredicate/qof_query_char_predicate/g;
		# s/gncQueryKVPPredicate/qof_query_kvp_predicate/g;
		# s/gncQueryCorePredicateFree/qof_query_core_predicate_free/g;

		# s/COMPARE_LT/QOF_COMPARE_LT/g;
		# s/COMPARE_LTE/QOF_COMPARE_LTE/g;
		# s/COMPARE_EQUAL/QOF_COMPARE_EQUAL/g;
		# s/COMPARE_GT/QOF_COMPARE_GT/g;
		# s/COMPARE_GTE/QOF_COMPARE_GTE/g;
		# s/COMPARE_NEQ/QOF_COMPARE_NEQ/g;

		# s/STRING_MATCH_NORMAL/QOF_STRING_MATCH_NORMAL/g;
		# s/STRING_MATCH_CASEINSENSITIVE/QOF_STRING_MATCH_CASEINSENSITIVE/g;

		# s/DATE_MATCH_NORMAL/QOF_DATE_MATCH_NORMAL/g;
		# s/DATE_MATCH_ROUNDED/QOF_DATE_MATCH_ROUNDED/g;

		# s/NUMERIC_MATCH_ANY/QOF_NUMERIC_MATCH_ANY/g;
		# s/NUMERIC_MATCH_CREDIT/QOF_NUMERIC_MATCH_CREDIT/g;
		# s/NUMERIC_MATCH_DEBIT/QOF_NUMERIC_MATCH_DEBIT/g;

		# s/GUID_MATCH_ANY/QOF_GUID_MATCH_ANY/g;
		# s/GUID_MATCH_NONE/QOF_GUID_MATCH_NONE/g;
		# s/GUID_MATCH_NULL/QOF_GUID_MATCH_NULL/g;
		# s/GUID_MATCH_ALL/QOF_GUID_MATCH_ALL/g;
		# s/GUID_MATCH_LIST_ANY/QOF_GUID_MATCH_LIST_ANY/g;

		# s/CHAR_MATCH_ANY/QOF_CHAR_MATCH_ANY/g;
		# s/CHAR_MATCH_NONE/QOF_CHAR_MATCH_NONE/g;

		s/char_match_t/QofCharMatch/g;
		s/guid_match_t/QofGuidMatch/g;
		s/numeric_match_t/QofNumericMatch/g;
		s/date_match_t/QofDateMatch/g;
		s/string_match_t/QofStringMatch/g;
		s/query_compare_t/QofQueryCompare/g;

		s/gncQueryCoreInit/qof_query_core_init/g;
		s/gncQueryCoreShutdown/qof_query_core_shutdown/g;
		s/gncQueryCoreGetPredicate/qof_query_core_get_predicate/g;
		s/gncQueryCoreGetCompare/qof_query_core_get_compare/g;
		
		s/gncQueryCorePredicateEqual/qof_query_core_predicate_equal/g;
		
		print OF $_;
	}
   close OF;
   close AF;

	$rn = "mv " . $afile . ".tmp " . $afile;
	system ($rn);
}
