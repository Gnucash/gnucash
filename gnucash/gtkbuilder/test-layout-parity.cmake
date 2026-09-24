# Verify every declarative resource and the preserved layout invariants
# that cannot be represented by a GtkBox default alone.
if (NOT DEFINED GTK4_BUILDER_TOOL OR NOT DEFINED GTKBUILDER_DIR OR
    NOT DEFINED GNC_SOURCE_DIR OR NOT DEFINED GTKBUILDER_RESOURCES OR
    NOT DEFINED AQB_ASSISTANT)
    message(FATAL_ERROR "GTK builder layout parity test was invoked without its source paths")
endif()

# gtkbuilder_SOURCES is the manifest authority for installed builder files.
# The remaining entries are the explicit manifests of the other CMake resource
# sets (AqBanking, OFX, and the main GnuCash GResource).
string(REPLACE "|" ";" gtkbuilder_resources "${GTKBUILDER_RESOURCES}")
set(builder_files)
foreach (builder_resource IN LISTS gtkbuilder_resources)
    list(APPEND builder_files "${GTKBUILDER_DIR}/${builder_resource}")
endforeach()
list(APPEND builder_files
    "${GNC_SOURCE_DIR}/import-export/aqb/assistant-ab-initial.glade"
    "${GNC_SOURCE_DIR}/import-export/aqb/dialog-ab-pref.glade"
    "${GNC_SOURCE_DIR}/import-export/aqb/dialog-ab.glade"
    "${GNC_SOURCE_DIR}/import-export/aqb/gnc-plugin-aqbanking.ui"
    "${GNC_SOURCE_DIR}/import-export/ofx/gnc-plugin-ofx.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-embedded-register-window.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-main-window.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-account-tree.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-basic-commands.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-bi-import.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-budget.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-business.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-csv-export.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-csv-import.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-customer-import.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-file-history.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-log-replay.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-page-account-tree.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-page-budget.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-page-invoice.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-page-owner-tree.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-page-register.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-page-report.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-page-sx-list.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-qif-import.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-register.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-plugin-report-system.ui"
    "${GNC_SOURCE_DIR}/ui/gnc-reconcile-window.ui")
foreach (builder_file IN LISTS builder_files)
    execute_process(
        COMMAND "${GTK4_BUILDER_TOOL}" validate "${builder_file}"
        RESULT_VARIABLE validate_result
        OUTPUT_VARIABLE validate_output
        ERROR_VARIABLE validate_error)
    if (NOT validate_result EQUAL 0)
        message(FATAL_ERROR "GTK4 Builder validation failed for ${builder_file}: ${validate_output}${validate_error}")
    endif()
    execute_process(
        COMMAND "${GTK4_BUILDER_TOOL}" enumerate "${builder_file}"
        RESULT_VARIABLE enumerate_result
        OUTPUT_VARIABLE enumerate_output
        ERROR_VARIABLE enumerate_error)
    if (NOT enumerate_result EQUAL 0)
        message(FATAL_ERROR "GTK4 Builder enumeration failed for ${builder_file}: ${enumerate_output}${enumerate_error}")
    endif()
endforeach()

function (read_object_properties_from_contents contents_var object_id out_var)
    set(contents "${${contents_var}}")
    string(REGEX MATCH "<object class=\"[^\"]+\" id=\"${object_id}\">"
           object_open "${contents}")
    if (NOT object_open)
        message(FATAL_ERROR "Missing widget ${object_id}")
    endif()

    string(FIND "${contents}" "${object_open}" object_offset)
    string(SUBSTRING "${contents}" ${object_offset} -1 object_suffix)

    # Retain all markup belonging to the target object, including direct
    # layout/style sections after children, while omitting complete nested
    # object blocks. GtkBuilder puts some direct layout properties after the
    # child object, so a first-<child> boundary is insufficient.
    set(properties "")
    set(remainder "${object_suffix}")
    set(object_depth 0)
    while (TRUE)
        string(REGEX MATCH "</?object([ \t\r\n][^>]*)?>" object_tag
               "${remainder}")
        if (NOT object_tag)
            message(FATAL_ERROR "Widget ${object_id} has no object boundary")
        endif()

        string(FIND "${remainder}" "${object_tag}" tag_offset)
        string(SUBSTRING "${remainder}" 0 ${tag_offset} before_tag)
        if (object_depth EQUAL 1)
            string(APPEND properties "${before_tag}")
        endif()

        string(LENGTH "${object_tag}" tag_length)
        math(EXPR remainder_offset "${tag_offset} + ${tag_length}")
        string(SUBSTRING "${remainder}" ${remainder_offset} -1 remainder)

        if (object_tag MATCHES "^</object")
            if (object_depth EQUAL 1)
                string(APPEND properties "${object_tag}")
                break()
            endif()
            math(EXPR object_depth "${object_depth} - 1")
        else()
            if (object_depth EQUAL 0)
                string(APPEND properties "${object_tag}")
            endif()
            if (object_tag MATCHES "/[ \t\r\n]*>$")
                continue()
            endif()
            math(EXPR object_depth "${object_depth} + 1")
        endif()
    endwhile()

    set(${out_var} "${properties}" PARENT_SCOPE)
endfunction()

function (read_object_properties file_name object_id out_var)
    if (IS_ABSOLUTE "${file_name}")
        file(READ "${file_name}" contents)
    else()
        file(READ "${GTKBUILDER_DIR}/${file_name}" contents)
    endif()
    read_object_properties_from_contents(contents "${object_id}" properties)
    set(${out_var} "${properties}" PARENT_SCOPE)
endfunction()

# Keep the object-boundary helper honest: a child property must never satisfy
# a parent assertion, including GtkWindow's property-based child topology.
set(object_boundary_fixture [=[
<object class="GtkWindow" id="parent">
  <property name="title">Parent</property>
  <property name="child">
    <object class="GtkLabel" id="child">
      <property name="label">child-only</property>
    </object>
  </property>
  <property name="focus-widget">
    <object class="GtkLabel" id="self-closing-child" />
  </property>
  <property name="resizable">true</property>
  <layout>
    <property name="row">5</property>
  </layout>
</object>
]=])
read_object_properties_from_contents(object_boundary_fixture "parent"
                                     parent_properties)
string(FIND "${parent_properties}"
       "<property name=\"title\">Parent</property>" parent_title_offset)
string(FIND "${parent_properties}" "child-only" child_property_offset)
string(FIND "${parent_properties}"
       "<property name=\"resizable\">true</property>"
       parent_resizable_offset)
string(FIND "${parent_properties}" "<layout>" parent_layout_offset)
string(FIND "${parent_properties}"
       "<property name=\"row\">5</property>" parent_row_offset)
if (parent_title_offset EQUAL -1 OR NOT child_property_offset EQUAL -1 OR
    parent_resizable_offset EQUAL -1 OR
    parent_layout_offset EQUAL -1 OR parent_row_offset EQUAL -1)
    message(FATAL_ERROR "Object property extraction crossed a child boundary")
endif()

function (require_box_property relative_file object_id name value)
    read_object_properties("${relative_file}" "${object_id}" properties)
    string(FIND "${properties}"
           "<object class=\"GtkBox\" id=\"${object_id}\">" box_offset)
    string(FIND "${properties}" "<property name=\"${name}\">${value}</property>" property_offset)
    if (box_offset EQUAL -1 OR property_offset EQUAL -1)
        message(FATAL_ERROR "GtkBox ${object_id} in ${relative_file} must retain ${name}=${value}")
    endif()
endfunction()

function (require_widget_property relative_file object_id name value_pattern)
    read_object_properties("${relative_file}" "${object_id}" properties)
    string(REGEX MATCH
           "<property name=\"${name}\"[^>]*>(${value_pattern})</property>"
           property_match "${properties}")
    if (NOT property_match)
        message(FATAL_ERROR "Widget ${object_id} in ${relative_file} must retain ${name} matching ${value_pattern}")
    endif()
endfunction()

function (require_layout_property relative_file object_id name value)
    read_object_properties("${relative_file}" "${object_id}" properties)
    string(FIND "${properties}" "<layout>" layout_offset)
    string(FIND "${properties}"
           "<property name=\"${name}\">${value}</property>" property_offset)
    if (layout_offset EQUAL -1 OR property_offset EQUAL -1)
        message(FATAL_ERROR "Widget ${object_id} in ${relative_file} must retain grid ${name}=${value}")
    endif()
endfunction()

function (require_window_property file_name window_id name value)
    read_object_properties("${file_name}" "${window_id}" properties)
    string(FIND "${properties}"
           "<object class=\"GtkWindow\" id=\"${window_id}\">" window_offset)
    string(FIND "${properties}" "<property name=\"${name}\">${value}</property>" property_offset)
    if (window_offset EQUAL -1 OR property_offset EQUAL -1)
        message(FATAL_ERROR "GtkWindow ${window_id} in ${file_name} must retain ${name}=${value}")
    endif()
endfunction()

# Legacy end-aligned action areas must stay grouped at the end with the
# standard six-pixel inter-button gap.
set(end_areas
    "assistant-qif-import.glade|load_progress_hbuttonbox1"
    "assistant-qif-import.glade|convert_progress_hbuttonbox1"
    "dialog-account.glade|dialog-action_area100"
    "dialog-account.glade|dialog-action_area200"
    "dialog-account.glade|dialog-action_area400"
    "dialog-billterms.glade|dialog-action_area"
    "dialog-choose-owner.glade|dialog-action_area3"
    "dialog-commodity.ui|dialog-action_area4"
    "dialog-doclink.ui|buttonbox"
    "dialog-find-account.glade|buttonbox"
    "dialog-imap-editor.ui|dialog-action_area1"
    "dialog-import.glade|dialog-action_area14"
    "dialog-new-user.glade|hbbox1"
    "dialog-price.ui|dialog-action_area2"
    "dialog-progress.glade|hbuttonbox1"
    "dialog-report.glade|dialog-action_area"
    "dialog-sx.ui|dialog-action_area24"
    "dialog-sx.ui|dialog-action_area23"
    "dialog-transfer.glade|hbbox"
    "gnc-plugin-page-budget.ui|dialog-action_area6"
    "gnc-plugin-page-budget.ui|dialog-action_area5"
    "gnc-tree-view-owner.glade|dialog-action_area13")
foreach (entry IN LISTS end_areas)
    string(REPLACE "|" ";" fields "${entry}")
    list(GET fields 0 relative_file)
    list(GET fields 1 object_id)
    require_box_property("${relative_file}" "${object_id}" "halign" "end")
    require_box_property("${relative_file}" "${object_id}" "spacing" "6")
endforeach()

# Legacy secondary action children must remain on the leading edge. The
# enclosing GtkBox expands, while only the original secondary child consumes
# that space; the primary action group therefore remains right-aligned.
set(secondary_areas
    "dialog-account.glade|dialog-action_area300|help_button"
    "dialog-bi-import-gui.glade|dialog-action_area3|helpbutton"
    "dialog-book-close.ui|dialog-action_area1|helpbutton"
    "dialog-commodity.ui|dialog-action_area6|help_button"
    "dialog-customer.glade|dialog-action_area1|helpbutton"
    "dialog-fincalc.ui|dialog-action_area10|help_button"
    "dialog-import.glade|dialog-action_area10|matcher__help"
    "dialog-invoice.glade|dialog-action_area2|helpbutton"
    "dialog-options.glade|buttonbox|helpbutton"
    "dialog-price.ui|dialog-action_area18|pd_help_button"
    "dialog-print-check.glade|dialog-action_area6|helpbutton"
    "dialog-search.glade|dialog-action_area3|help_button"
    "dialog-sx.ui|dialog-action_area17|help_button"
    "dialog-sx.ui|dialog-action_area25|helpbutton2"
    "dialog-vendor.glade|dialog-action_area1|helpbutton"
    "dialog-custom-report.glade|custom_report_actions|help_button"
    "dialog-customer-import-gui.glade|customer_import_actions|helpbutton"
    "dialog-employee.glade|employee_actions|helpbutton"
    "dialog-job.glade|job_actions|helpbutton"
    "dialog-order.glade|order_actions|helpbutton"
    "dialog-order.glade|new_order_actions|help_button"
    "dialog-preferences.glade|preferences_action_area|helpbutton2")
foreach (entry IN LISTS secondary_areas)
    string(REPLACE "|" ";" fields "${entry}")
    list(GET fields 0 relative_file)
    list(GET fields 1 box_id)
    list(GET fields 2 child_id)
    require_box_property("${relative_file}" "${box_id}" "hexpand" "1")
    require_box_property("${relative_file}" "${box_id}" "spacing" "6")
    require_widget_property("${relative_file}" "${child_id}" "hexpand" "1|true")
    require_widget_property("${relative_file}" "${child_id}" "halign" "start")
endforeach()

# These were the only non-default alignment deltas in the baseline matrix.
# Keeping their exact values prevents a builder regeneration from silently
# reintroducing the visually observable layout shifts.
set(alignment_invariants
    "assistant-csv-trans-import.glade|start_row|halign|start"
    "assistant-csv-trans-import.glade|end_row|halign|start"
    "assistant-loan.glade|pay_back_button|halign|start"
    "assistant-loan.glade|pay_next_button|halign|start"
    "dialog-commodities.ui|rename_namespace_button|halign|start"
    "dialog-commodity.ui|label824|halign|start"
    "dialog-date-close.glade|hbox2|valign|start"
    "dialog-employee.glade|label34|halign|end"
    "dialog-invoice.glade|hbox1|valign|start"
    "dialog-invoice.glade|page_proj_frame|valign|start"
    "dialog-invoice.glade|to_charge_frame|valign|start"
    "dialog-invoice.glade|yes_tt_reset|halign|start"
    "dialog-invoice.glade|no_tt_reset|halign|start"
    "dialog-new-user.glade|table1|valign|start"
    "dialog-sx.ui|end_on_date_button|halign|start"
    "dialog-sx.ui|n_occurrences_button|halign|start"
    "dialog-sx.ui|label847810|halign|start"
    "dialog-sx.ui|label847808|halign|start"
    "dialog-sx.ui|review_txn_toggle|halign|start"
    "gnc-frequency.ui|label847759|halign|start"
    "gnc-frequency.ui|semimonthly_first|halign|start"
    "gnc-frequency.ui|label847751|halign|start"
    "gnc-frequency.ui|label847760|halign|start"
    "gnc-frequency.ui|semimonthly_second|halign|start"
    "gnc-frequency.ui|label847752|halign|start"
    "gnc-frequency.ui|label847756|halign|start"
    "gnc-frequency.ui|monthly_day|halign|start"
    "gnc-frequency.ui|label847750|halign|start"
    "gnc-frequency.ui|monthly_weekend|halign|start"
    "gnc-plugin-page-register.glade|sort_save|halign|start")
foreach (entry IN LISTS alignment_invariants)
    string(REPLACE "|" ";" fields "${entry}")
    list(GET fields 0 relative_file)
    list(GET fields 1 object_id)
    list(GET fields 2 property_name)
    list(GET fields 3 expected_value)
    require_widget_property("${relative_file}" "${object_id}" "${property_name}" "${expected_value}")
endforeach()

require_widget_property("dialog-import.glade" "transaction_matcher_content"
                        "spacing" "6")
require_widget_property("dialog-import.glade" "scrolledwindow25"
                        "hexpand" "1|true")

set(grid_invariants
    "dialog-price.ui|date_hbox|row|2"
    "dialog-price.ui|remove_namespace_label|row|3"
    "dialog-price.ui|namespace_combo_we|row|3"
    "dialog-price.ui|label3|row|4"
    "dialog-price.ui|hbox3|row|5"
    "dialog-price.ui|label2|row|6"
    "dialog-price.ui|hbox2|row|7"
    "dialog-tax-info.glade|identity_edit_button|column|0"
    "dialog-tax-info.glade|identity_edit_button|row|2"
    "dialog-tax-info.glade|identity_edit_button|column-span|2")
foreach (entry IN LISTS grid_invariants)
    string(REPLACE "|" ";" fields "${entry}")
    list(GET fields 0 relative_file)
    list(GET fields 1 object_id)
    list(GET fields 2 property_name)
    list(GET fields 3 expected_value)
    require_layout_property("${relative_file}" "${object_id}" "${property_name}" "${expected_value}")
endforeach()

require_window_property("assistant-hierarchy.glade" "hierarchy_assistant" "default-width" "400")
require_window_property("assistant-hierarchy.glade" "hierarchy_assistant" "default-height" "550")
require_window_property("dialog-lot-viewer.glade" "lot_viewer_dialog" "default-width" "600")
require_window_property("dialog-lot-viewer.glade" "lot_viewer_dialog" "default-height" "400")

require_window_property("${AQB_ASSISTANT}" "aqbanking_init_assistant"
                        "default-width" "500")
require_window_property("${AQB_ASSISTANT}" "aqbanking_init_assistant"
                        "default-height" "500")
