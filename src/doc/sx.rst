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
  - [x] move files around
  - [ ] GncSxListTreeModelAdapter: s/real/adapted/
  - [ ] generic tree model adapter setup code

- core
  - [x] sx list -> qof collection
  - [/] sx engine events
    - [x] sx list collection add/remove -- sx-list GNC_EVENT_ITEM_ADDED, _REMOVED
    - [x] sx modified -- QOF_EVENT_MODIFY
  - [/] sx upcoming instance model
    - [ ] implement sort model
  - [#] testing
    - [x] open SLR dialog, create new SX, see it populated
    - [x] open SX editor dialog, run SLR, see next-instance update
    - [/] unit testing

- sx editor page
  - [ ] make into split panel
  - [ ] {0, 1, 2, 4, 8, 12} month selection for dense calendar

- gnc_dense_cal
  - [ ] fix static variables that should be instance fields.
  - [ ] change number-of-month properties to display-named properties (width, length)
  - [?] better transient/floating window
  - [/] (re-format file)
  - [x] set_model(GncTemporalInstancesModel *mdl)
    - [x] new interface creation.
    - [x] register callbacks for signals
  - [x] remove clist usage

- sx-from-trans
  - [ ] convert to GObject
  - [x] hookup destroy/finalize

- use Recurrence instead of Freq Spec
  - [ ] XML migration, handling

- since-last-run
  - [ ] move "effect_change" up to app-utils/, test.
  - [?] add obsolete flag to SxInstanceModel
  - [x] add reminders, postponed to SxInstanceModel
  - [x] add mutation support to sx instance model
    - [x] state machine
  - [x] add variable state to sx instance model
    - [x] handle (hidden/system not for editing) variables.
  - [x] add sx_upcoming_instance_model()
      - [x] add effect_auto_create()
  - [x] add some sort of "ready to go" flag and api
    - [x] variable setting, primarily
  - [x] some sort of commit_changes()
  - [x] add variable table to instances
  - [x] ui: add 'review created transactions' checkbox to SLR dialog
        using txn search.

- destroy/cleanup
  - notes
    - dispose: should no longer hold references to other objects; callable
      multiple times; chain up at end
    - finalize: complete destruction; just before free; only called once;
      chain up at end.

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

There is no seperate to-review page, however the user may (optionally) want
to see the created transactions.  This is done using the transaction-search
functionality over the created transactions by ID.

Upcoming instance states
---------------------------------------

    reminder  -> to-create
    postponed -> to-create
    to-create -> postponed
    to-create -> ignore
    to-create -> created [terminal]

Definitions:

    reminder: a transient upcoming transaction that will not be created.
    postponed: a historical to-create transaction that the user has
        explicitly deferred.
    to-create: an upcoming SX instance that should be created.
    ignore: a scheduled instance the user has explicitly prevented the
        instantiation of.
    created: the instance has been created in this interaction cycle.

The SX need to store?
- last state of *created* instance
- postponed instance list

There is a constraint around a sequence of upcoming instance states.  In
short: the last-created state and a list of postponed instances are modeled,
but upcoming reminders are not.  As such, a reminder can never be before any
other (modeled) instance type.  For instance, the following sequences are
disallowed:

[...]
remind    <- will be lost/skipped over; must be converted to `postponed`.
to-create <- this will be the last-recorded state.
[...]

[...]
remind    <- same as previous; will be lost/skipped; must be `postponed`.
postponed
[...]

remind    <- same...
ignore
[...]


As such, the SinceLastRun model will enforce that there are no previous
`remind` instances at every state change.  They will be silently converted to
`postponed`-state transactions.

Formula Parsing
------------------------

A SXes formula is parsed in the context of:
- the template transaction
  - the accounts of the splits
- the sequence number
- the date of the transaction
- a variable-binding table.


Testing Notes
---------------------

- auto-create
  - auto-create with postponed instances shouldn't destroy postponed
    instances

- basic sequence stuff

dialog-sxsincelast.c:  ~L1241:
"Handle an interesting corner case of postponing or
ignoring the first instance. We only want to incrment the
counters for newly-discovered-as-to-be-created SXes."

- auto-create 
  - auto-create transactions can be created w/o user interaction
    - their state is transitioned to 'created', which is not modifyable
  
  - auto-create (+notify) transactions should be displayed, even if they are
    the only transactions created.
  
  - auto-create (-notify) transactions should not be displayed, unless there
    are other transactions.
  
  - Scenarios
    - only auto-create (-notify): no SLR, info dialog w/count (***)
    - only auto-create (+notify): SLR dialog, already created
    - others, auto-create (-notify): SLR dialog, incl. created 
    - others, auto-create (+notify): SLR dialog, incl. created
