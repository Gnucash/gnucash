Scheduled Transactions
======================

Overview
--------

- SX List
  - CRUD operations on SXes
  - Show Next "year"s worth of SX instances
    - gnc_sx_get_instances({now + 1yr})

- SinceLastRun
  - Last .. present (+ create-in-advance, reminder) instances
    - gnc_sx_get_instances(now)

TODO
----

- 'sx_updated' QOF events

- gnc_dense_cal
  - change number-month properties to display (width, length)
  - set_data(GncSxUpcomingInstances *future);
  - set_model(GncSxUpcomingInstances *model);
    - register callbacks for signals
    - set_data(...)

- transaction creation
  - verification routine
    - variable binding/requirements.
  - actual creation
    - error handling

- stateful editing of instances in since-last-run dialog
  - postponed/ignored/to-create constraints
    - processed_valid_reminders_list, contiguous-date logic
  - postponed/ignored/to-create transitions [?]
  - "allowed to finish" decision.

Pedantic Todo
----------------

- s/SchedXaction/Scheduled/
- s/temporal_state/instance_sequence_context/


<?c


?>

GtkTreeModelIface
-----------------

- signals
  - row_changed : sx_updated
  - row_inserted : sx_created
  - row_has_child_toggled : are there any children, here?
  - row_deleted : sx_deleted
  - rows_reordered : ???

- GtkTreeSortableIface
  - 
