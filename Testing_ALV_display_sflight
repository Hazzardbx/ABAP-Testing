*&---------------------------------------------------------------------*
*& Report ZKM_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zkm_test.

TYPES: BEGIN OF ty_sflight,
         carrid TYPE sflight-carrid,
         connid TYPE sflight-connid,
         fldate TYPE sflight-fldate,
         price  TYPE sflight-price,
         status TYPE icon_d,
       END OF ty_sflight.

"internal table to display
DATA: it_data TYPE TABLE OF ty_sflight.

"object from classes
DATA: ol_alv         TYPE REF TO cl_salv_table,
      lo_functions   TYPE REF TO cl_salv_functions,
      lo_alv_columns TYPE REF TO cl_salv_columns,
      lo_alv_column  TYPE REF TO cl_salv_column,
      lo_cols        TYPE REF TO cl_salv_columns_table,
      lo_col         TYPE REF TO cl_salv_column_table,
      lo_alv_display TYPE REF TO cl_salv_display_settings,
      lo_aggregation TYPE REF TO cl_salv_aggregations.

"multiselect fields
DATA: ol_selections TYPE REF TO cl_salv_selections.

* filling color column
DATA: ls_color TYPE lvc_s_colo.

*---------------------------------------------------------------------------
* class lcl_handle_events
*
CLASS lcl_handle_events DEFINITION.

  PUBLIC SECTION.
    METHODS: on_click FOR EVENT added_function OF cl_salv_events.
    METHODS: on_link_click FOR EVENT if_salv_events_actions_table~link_click OF cl_salv_events_table IMPORTING row column .

ENDCLASS.

*---------------------
* class implementation
*
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_click.
    IF sy-ucomm EQ 'FCT_BUTTON'.
      PERFORM get_selected_rows.
    ENDIF.
  ENDMETHOD.

  METHOD on_link_click.

    READ TABLE it_data INTO DATA(ls_data) INDEX row.
    SET PARAMETER ID 'DTB' FIELD ls_data-carrid.
    CALL TRANSACTION 'SE11' WITHOUT AUTHORITY-CHECK. "obsoleto

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------------

"object from event class
DATA: event_handler TYPE REF TO lcl_handle_events,
      lo_events     TYPE REF TO cl_salv_events_table.

"---------------------------------------------------------

START-OF-SELECTION.

  CREATE OBJECT event_handler.

  PERFORM get_data.           "sql query
  PERFORM create_alv.         "calls factory method
  PERFORM set_events.         "link events to alv
  PERFORM set_functions.      "enable standard functions
  PERFORM set_settings.       "enable style functions
  PERFORM set_columns.        "enable columns settings
  PERFORM set_aggregations.   "enable sum of a column
  PERFORM set_status.         "verification of a column condition
  PERFORM set_screen_status.  "allows alv to access a gui status
  PERFORM set_multiselection. "enable multiselect fields
  PERFORM display_alv.        "shows the alv

END-OF-SELECTION.

  "---------------------------------------------------------

FORM get_data.

  SELECT *
    FROM sflight
    INTO CORRESPONDING FIELDS OF TABLE it_data
    UP TO 20 ROWS.

  SORT it_data BY fldate ASCENDING.

ENDFORM.

FORM create_alv.

  "method factory to create an alv
  cl_salv_table=>factory(
    IMPORTING
      r_salv_table   = ol_alv
    CHANGING
      t_table        = it_data
  ).

ENDFORM.

FORM set_functions.

  "cl_salv_functions deals only standard functions of alv

  CREATE OBJECT lo_functions.

  lo_functions = ol_alv->get_functions( ).
  lo_functions->set_all( abap_true ).

ENDFORM.

FORM set_settings.

  "additional settings to custom alv format

  lo_alv_display = ol_alv->get_display_settings( ).
  lo_alv_display->set_striped_pattern( cl_salv_display_settings=>true ).
  lo_alv_display->set_list_header( 'Title Alv' ).
  lo_alv_display->set_striped_pattern( value = abap_true ).
  lo_alv_display->set_vertical_lines( value = abap_true ).
  lo_alv_display->get_fit_column_to_table_size( ).


ENDFORM.

FORM set_columns.

  lo_alv_columns = ol_alv->get_columns( ).

  lo_alv_column = lo_alv_columns->get_column( 'CARRID' ).
  lo_alv_column->set_long_text( 'Air Company' ).
  lo_alv_column->set_fixed_header_text( 'L' ).
  lo_alv_column->set_medium_text( '' ).
  lo_alv_column->set_short_text( '' ).
  lo_alv_column->set_output_length('40').
  lo_alv_column->set_tooltip( value = 'Company Air Code' ).
  lo_alv_column->set_optimized( 'X' ).
  lo_alv_column->set_alignment(
  value = if_salv_c_alignment=>centered
  ).

  "coloring col
  lo_cols = ol_alv->get_columns( ).
  lo_col ?= lo_cols->get_column( 'CARRID' ).
  ls_color-col = col_positive.
  lo_col->set_color( ls_color ).

  lo_alv_column = lo_alv_columns->get_column( 'CONNID' ).
  lo_alv_column->set_long_text( 'Cod, Connection' ).
  lo_alv_column->set_medium_text( '' ).
  lo_alv_column->set_short_text( '' ).
  lo_alv_column->set_output_length('40').
  lo_alv_column->set_optimized( 'X' ).
  lo_alv_column->set_tooltip( value = 'Connection Code to Flight' ).
  lo_alv_column->set_alignment(
  value = if_salv_c_alignment=>centered
  ).

  lo_alv_column = lo_alv_columns->get_column( 'FLDATE' ).
  lo_alv_column->set_long_text( 'Air Flight' ).
  lo_alv_column->set_medium_text( '' ).
  lo_alv_column->set_short_text( '' ).
  lo_alv_column->set_output_length('40').
  lo_alv_column->set_optimized( 'X' ).
  lo_alv_column->set_tooltip( value = 'Date Flight' ).
  lo_alv_column->set_alignment(
  value = if_salv_c_alignment=>centered
  ).

  lo_alv_column ?= lo_alv_columns->get_column( 'PRICE' ).
  lo_alv_column->set_long_text( 'Price' ).
  lo_alv_column->set_medium_text( '' ).
  lo_alv_column->set_short_text( '' ).
  lo_alv_column->set_output_length('40').
  lo_alv_column->set_optimized( abap_true ).
  lo_alv_column->set_decimals( value = '2' ).
  lo_alv_column->set_alignment(
  value = if_salv_c_alignment=>centered
  ).

  lo_alv_column ?= lo_alv_columns->get_column( 'STATUS' ).
  lo_alv_column->set_long_text( 'Status' ).
  lo_alv_column->set_medium_text( '' ).
  lo_alv_column->set_short_text( '' ).
  lo_alv_column->set_output_length('40').
  lo_alv_column->set_optimized( abap_true ).
  lo_alv_column->set_tooltip( value = 'Price Stats' ).
  lo_alv_column->set_alignment(
  value = if_salv_c_alignment=>centered
  ).

ENDFORM.

FORM set_aggregations.

  lo_aggregation = ol_alv->get_aggregations( ).

  lo_aggregation->add_aggregation(
    columnname = 'PRICE'
  ).

ENDFORM.

FORM set_status.

  LOOP AT it_data INTO DATA(ls_data).
    IF ls_data-price GT 100 AND ls_data-price LT 300.
      ls_data-status = icon_yellow_light.
    ELSEIF ls_data-price GT 300.
      ls_data-status = icon_green_light.
    ELSE.
      ls_data-status = icon_red_light.
    ENDIF.
    MODIFY it_data FROM ls_data.
  ENDLOOP.

ENDFORM.

FORM set_screen_status.

  ol_alv->set_screen_status(
  EXPORTING
    report        = sy-repid
    pfstatus      = 'MYSTATUS'
    set_functions = cl_salv_table=>c_functions_all
  ).

ENDFORM.

FORM set_multiselection.

  ol_alv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>cell ).

ENDFORM.

FORM get_selected_rows.

  DATA: lt_selected_rows TYPE salv_t_row.

  " get selected lines into internal table
  lt_selected_rows = ol_alv->get_selections( )->get_selected_rows( ).

  IF lt_selected_rows IS INITIAL.
    MESSAGE 'SELECT SOME LINES' TYPE 'I'.
    RETURN.
  ELSE.
    CLEAR it_data.
    LOOP AT lt_selected_rows INTO DATA(ls_selected_row).
      READ TABLE lt_selected_rows INTO ls_selected_row INDEX sy-tabix. "read selected lines by index

      "STILL NEED TO INSERT LOGIC.

    ENDLOOP.
  ENDIF.

ENDFORM.

FORM set_events.

  lo_events = ol_alv->get_event( ).

  SET HANDLER event_handler->on_click FOR lo_events.      "button evennt
  SET HANDLER event_handler->on_link_click FOR lo_events. "hotspot click

  DATA(o_col) = CAST cl_salv_column_table( ol_alv->get_columns( )->get_column( 'CARRID' ) ).
  o_col->set_cell_type( if_salv_c_cell_type=>hotspot ).

ENDFORM.

FORM display_alv.

  "display alv
  ol_alv->display( ).

ENDFORM.
