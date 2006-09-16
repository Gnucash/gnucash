Scheduled Transactions
===============================================================

Overview
--------------

- SX List
  - CRUD operations on SXes
  - Show Next "year"s worth of SX instances
    - gnc_sx_get_instances({now + 1yr})

- SX Editor

- SinceLastRun
  - Last .. present (+ create-in-advance, reminder) instances
    - gnc_sx_get_instances(now)

TODO
----------

- meta
  - [ ] move files around
  - [ ] GncSxListTreeModelAdapter: s/real/adapted/

- core
  - [x] sx list -> qof collection
  - [ ] sx engine events
    - [x] sx list collection add/remove -- sx-list GNC_EVENT_ITEM_ADDED, _REMOVED
    - [ ] sx modified -- QOF_EVENT_MODIFY
  - [ ] sx upcoming instance model
    - [ ] implement sort model

- gnc_dense_cal
  - [ ] change number-of-month properties to display-named properties (width, length)
  - [x] set_model(GncTemporalInstancesModel *mdl)
    - [x] new interface creation.
    - [x] register callbacks for signals

- sx-from-trans
  - [ ] convert to GObject, hookup destroy/finalize

- use Recurrence instead of Freq Spec
  - [ ] XML migration, handling

- since-last-run
  - [ ] add reminders, postponed to SxInstanceModel
  - [ ] add obsolete flag to SxInstanceModel
  - [ ] add mutation support to sx instance model
    - [ ] state machine
  - [ ] add variable state to sx instance model
  - [ ] add sx_upcoming_instance_model()
      - [ ] add effect_auto_create()
  - [ ] add some sort of "ready to go" flag and api
    - [ ] variable setting, primarily
  - [ ] some sort of commit_changes()
    - ??? does effect_auto_create() imply or need commit_changes()?
  - [/] add variable table to instances

Pedantic Todo
----------------------

- s/SchedXaction/Scheduled/
- s/temporal_state/instance_sequence_context/
- change instance variable from 'i' to '__i' or something


============================================================

(eventually real documentation... (?))

Since Last Run
----------------------

+------------------+------------------+------------------+
|      Thing       |      State       |      Value       |
+------------------+------------------+------------------+
| - Foo            |                  |                  |
+------------------+------------------+------------------+
|   - 2006-08-27   |  [Postponed|v]   |                  |
+------------------+------------------+------------------+
|     - variable-a |                  |        42        |
+------------------+------------------+------------------+
|     - variable-b |                  |        75        |
+------------------+------------------+------------------+
|   - 2006-08-27   |  [To-Create|v]   |                  |
+------------------+------------------+------------------+
|     - variable-a |                  |        31        |
+------------------+------------------+------------------+
|     - variable-b |                  |  (value needed)  |
+------------------+------------------+------------------+


The since-last-run dialog is a key user interface.  More frequently than the
SX list or editor, the user will be in the process of creating transaction
instances through this interface.

The old SLR dialog has the following stages:

- Reminders
  - can be promoted to "to-create"
- Auto-created, with notification
- To-Create
  - postponed, to-create
  - ignore state.
- Created review
- Obsolete SX cleanup

The new SLR dialog will have the following:

- Creation
  (treemodel consisting of)
  - auto-created
  - reminder
  - postponed
  - to-create
  - [obsolete SX]?

There is no seperate to-review page.

Upcoming instance states
---------------------------------------

    reminder -> to-create
    postponed -> to-create
    to-create -> postponed  (with constraints)
    to-create -> ignore

Definitions:

    reminder: a transient upcoming transaction that will not be created.
    postponed: a historical to-create transaction that the user has
        explicitly deferred.
    to-create: an upcoming SX instance that should be created.
    ignore: a scheduled instance the user has explicitly prevented the
        instantiation of.

What does the SX need to store?
- postponed instance list.
- last state of created instance.

    void gnc_sx_instance_model_change_state(sx_id, instance_id, new_state, **gerror)
    boolean gnc_sx_instance_model_get_readiness(sx_id, instance_id)
    boolean gnc_sx_instance_model_set_variables(sx_id, instance_id, variable_name:string, value:string, **gerrror)

Formula Parsing
------------------------

A SXes formula is parsed in the context of:
- the template transaction
  - the accounts of the splits
- the sequence number
- the date of the transaction
- a variable-binding table.



