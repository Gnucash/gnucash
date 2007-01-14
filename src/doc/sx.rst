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
  - [x] sx engine events
    - [x] sx list collection add/remove -- sx-list GNC_EVENT_ITEM_ADDED, _REMOVED
    - [x] sx modified -- QOF_EVENT_MODIFY
  - [/] sx upcoming instance model
!   - [ ] implement sort model
  - [x] rename, re-home gnc-sx-instance-model:sxsl_get_sx_vars
  - [x] rename, re-home gnc-sx-instance-model:parse_vars_from_formula

- unit testing
  - [ ] model updating in the face of change
    - [ ] insert sx
    - [ ] remove sx
    - [ ] update sx
      - [ ] add instances
      - [ ] remove instances
      - [ ] make "weird"
  - [x] ensure state consistency model is upheld
  - [ ] check variables-unbound logic
  - [ ] verify summary counts
  - [ ] check "since last run" states
    - [ ] -autocreate[, ±notify]
    - [ ] +autocreate, -notify
    - [ ] +autocreate, +notify
    - [ ] +autocreate, -notify, w/postponed
    - [ ] +autocreate, +notify, w/postponed
  - [ ] bugs
    - [?] Expired scheduled transactions never run - <http://bugzilla.gnome.org/show_bug.cgi?id=375892>

- bugs
! - [ ] crash with two sx lists open and SX mutation [[[
    (gnucash:17610): GLib-GObject-WARNING **: invalid unclassed pointer in cast to `GncSxListTreeModelAdapterType'
    sx list tree model adapter update
    
    (gnucash:17610): Gtk-CRITICAL **: gtk_tree_store_clear: assertion `GTK_IS_TREE_STORE (tree_store)' failed ]]]

  - [x] Scheduled Transactions on 31st/last put in following month - <http://bugzilla.gnome.org/show_bug.cgi?id=104844>

- sx list page
  - [/] make into split panel
    - [ ] fix slider position
  - [ ] {0, 1, 2, 4, 8, 12} month selection for dense calendar

- sx editor
  - [ ] clean up, reformat
  - [ ] model-ize
    - (check_consistent, especially...)

- gnc-frequency
  - [ ] clean up, reformat

- gnc_dense_cal
  - [ ] font handling: gdk -> pango
  - [ ] change number-of-month properties to display-named properties (width, length)
  - [?] better transient/floating window
  - [/] (re-format file)
  - [x] set_model(GncTemporalInstancesModel *mdl)
    - [x] new interface creation.
    - [x] register callbacks for signals
  - [x] remove clist usage

- sx-from-trans
  - [?] convert to GObject
  - [x] hookup destroy/finalize

- FreqSpec
  - [ ] type+ui-type -> type

- use Recurrence instead of FreqSpec
  - [ ] XML migration, handling

- since-last-run
! - [ ] rewrite adapter (re-)population logic
  - [x] move "effect_change" up to app-utils/, test.
  - [x] move state-change up to app-utils
  - [x] move variable-setting up to app-utils
  - [x] move summarization up to app-utils
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

Bugs to close after merge
--------------------------------------

- With many auto-create transactions but none with notify option, "Auto-Created Transactions Notification" druid page lists every existing transaction - http://bugzilla.gnome.org/show_bug.cgi?id=347116
- Since last run dialog does not allow for early finish, an... - http://bugzilla.gnome.org/show_bug.cgi?id=329384
- Since Last Run druid changes data before Apply - http://bugzilla.gnome.org/show_bug.cgi?id=333849
- Resize the "Since Last Run" window is incorrect - http://bugzilla.gnome.org/show_bug.cgi?id=353563
- Transaction reminders page has slightly incorrect instructions - http://bugzilla.gnome.org/show_bug.cgi?id=331069
- Transaction not highlighted in "Transaction Preparation" window - http://bugzilla.gnome.org/show_bug.cgi?id=342658
- Scrolling through variables list does not work - http://bugzilla.gnome.org/show_bug.cgi?id=343190
- Gnucash thinks the file has changed after cancelling out of the Since Last Run dialog and making no changes - http://bugzilla.gnome.org/show_bug.cgi?id=344494
- Transaction reminder with variable amount doesn't display value field - http://bugzilla.gnome.org/show_bug.cgi?id=147946
