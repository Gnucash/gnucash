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
		s/GncObject_t/QofObject/g;
		s/gncObjectLookup/qof_object_lookup/g;
		s/gncObjectRegister/qof_object_register/g;
		s/gncObjectGetTypeLabel/qof_object_get_type_label/g;
		s/gncObjectRegisterBackend/qof_object_register_backend/g;
		s/gncObjectLookupBackend/qof_object_lookup_backend/g;
		s/gncObjectForeachBackend/qof_object_foreach_backend/g;
		s/gncObjectInitialize/qof_object_initialize/g;

		s/gncObjectShutdown/qof_object_shutdown/g;
		s/gncObjectBookBegin/qof_object_book_begin/g;
		s/gncObjectBookEnd/qof_object_book_end/g;
		s/gncObjectIsDirty/qof_object_is_dirty/g;
		s/gncObjectMarkClean/qof_object_mark_clean/g;

		s/gncObjectForeachType/qof_object_foreach_type/g;
		s/gncObjectForeach/qof_object_foreach/g;
		s/gncObjectPrintable/qof_object_printable/g;
		s/gncObjectx/qof_object_x/g;
		s/gncObjectx/qof_object_x/g;
		s/gncObjectx/qof_object_x/g;

		s/foreachTypeCB/QofForeachTypeCB/g;
		s/foreachBackendTypeCB/QofForeachBackendTypeCB/g;
		print OF $_;
	}
   close OF;
   close AF;

	$rn = "mv " . $afile . ".tmp " . $afile;
	system ($rn);
}
