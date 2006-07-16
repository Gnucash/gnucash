Scheduled Transactions
======================

Overview
--------

- SX List
  - CRUD operations on SXes
  - Show Next "year"s worth of SX instances
    - gnc_sx_get_instances({now + 1yr})

- SX Editor

- SinceLastRun
  - Last .. present (+ create-in-advance, reminder) instances
    - gnc_sx_get_instances(now)

TODO
----

- [x] sx list -> qof collection
- [ ] sx engine events
  - [x] sx list collection add/remove -- sx-list GNC_EVENT_ITEM_ADDED, _REMOVED
  - [ ] sx modified -- QOF_EVENT_MODIFY

- gnc_dense_cal
  - change number-month properties to display (width, length)
  - upcoming_instances_add_to_gnc_dense_cal(GncSxUpcomingInstances *future);
  - set_model(GncTemporalInstancesModel *mdl)
    - new interface creation.
    - register callbacks for signals

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

GtkTreeModelIface
-----------------

- signals
  - row_changed : sx_updated
  - row_inserted : sx_created
  - row_has_child_toggled : are there any children, here?
  - row_deleted : sx_deleted
  - rows_reordered : ???

- GtkTreeSortableIface
