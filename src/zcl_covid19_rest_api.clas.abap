"! <p class="shorttext synchronized" lang="en">Covid 19: Rest API</p>
CLASS zcl_covid19_rest_api DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_covid19_rest_api.

  PROTECTED SECTION.
    "! <p class="shorttext synchronized" lang="en">Get RFC Destination</p>
    "!
    "! @parameter rv_rfcdest | <p class="shorttext synchronized" lang="en">RFC Destination</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    METHODS get_rfc_dest
      RETURNING
        VALUE(rv_rfcdest) TYPE rfcdest
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Get method for Latest Stats</p>
    "!
    "! @parameter rv_method | <p class="shorttext synchronized" lang="en">Method</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    METHODS get_stats_latest_method
      RETURNING
        VALUE(rv_method) TYPE string
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Convert response to Latest Statics</p>
    "!
    "! @parameter iv_response | <p class="shorttext synchronized" lang="en">Response</p>
    "! @parameter rs_stats | <p class="shorttext synchronized" lang="en">Stats</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    METHODS conv_stats_latest
      IMPORTING
        iv_response     TYPE string
      RETURNING
        VALUE(rs_stats) TYPE zif_covid19_rest_api~ty_stats_latest
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Get method for Global Stats</p>
    "!
    "! @parameter rv_method | <p class="shorttext synchronized" lang="en">Method</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    METHODS get_stats_global_method
      RETURNING
        VALUE(rv_method) TYPE string
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Convert response to Global Statics</p>
    "!
    "! @parameter iv_response | <p class="shorttext synchronized" lang="en">Response</p>
    "! @parameter rt_stats | <p class="shorttext synchronized" lang="en">Stats</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    METHODS conv_stats_global
      IMPORTING
        iv_response     TYPE string
      RETURNING
        VALUE(rt_stats) TYPE zif_covid19_rest_api~tty_stats_country
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Get method for Country Stats</p>
    "!
    "! @parameter iv_country | <p class="shorttext synchronized" lang="en">Country</p>
    "! @parameter rv_method | <p class="shorttext synchronized" lang="en">Method</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    METHODS get_stats_country_method
      IMPORTING
        iv_country       TYPE string
      RETURNING
        VALUE(rv_method) TYPE string
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Get Request Content for Country Stats</p>
    "!
    "! @parameter iv_country | <p class="shorttext synchronized" lang="en">Country</p>
    "! @parameter rv_content | <p class="shorttext synchronized" lang="en">Content</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    METHODS get_stats_country_req_content
      IMPORTING
        iv_country       TYPE string
      RETURNING
        VALUE(rv_content) TYPE string
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Convert response to Country Statics</p>
    "!
    "! @parameter iv_response | <p class="shorttext synchronized" lang="en">Response</p>
    "! @parameter rs_stats | <p class="shorttext synchronized" lang="en">Stats</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    METHODS conv_stats_country
      IMPORTING
        iv_response     TYPE string
      RETURNING
        VALUE(rs_stats) TYPE zif_covid19_rest_api~ty_stats_country
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Get method for History Stats</p>
    "!
    "! @parameter rv_method | <p class="shorttext synchronized" lang="en">Method</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    METHODS get_history_method
      RETURNING
        VALUE(rv_method) TYPE string
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Convert response to History</p>
    "!
    "! @parameter iv_response | <p class="shorttext synchronized" lang="en">Response</p>
    "! @parameter rt_history | <p class="shorttext synchronized" lang="en">History</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    METHODS conv_history
      IMPORTING
        iv_response       TYPE string
      RETURNING
        VALUE(rt_history) TYPE zif_covid19_rest_api~tty_history
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Get method for Country Historical Stats</p>
    "!
    "! @parameter iv_country | <p class="shorttext synchronized" lang="en">Country</p>
    "! @parameter rv_method | <p class="shorttext synchronized" lang="en">Method</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    METHODS get_history_country_method
      IMPORTING
        iv_country       TYPE string
      RETURNING
        VALUE(rv_method) TYPE string
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Get Request Content for Country History Stats</p>
    "!
    "! @parameter iv_country | <p class="shorttext synchronized" lang="en">Country</p>
    "! @parameter rv_content | <p class="shorttext synchronized" lang="en">Content</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    METHODS get_hist_country_req_content
      IMPORTING
        iv_country       TYPE string
      RETURNING
        VALUE(rv_content) TYPE string
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Convert response to Country Historical Stats</p>
    "!
    "! @parameter iv_response | <p class="shorttext synchronized" lang="en">Response</p>
    "! @parameter rt_history | <p class="shorttext synchronized" lang="en">History</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    METHODS conv_history_country
      IMPORTING
        iv_response       TYPE string
      RETURNING
        VALUE(rt_history) TYPE zif_covid19_rest_api~tty_history
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Call Rest API</p>
    "!
    "! @parameter iv_method | <p class="shorttext synchronized" lang="en">Method</p>
    "! @parameter rv_response | <p class="shorttext synchronized" lang="en">Response</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 exception</p>
    METHODS get_response
      IMPORTING
        !iv_method         TYPE csequence
        !iv_content        TYPE string OPTIONAL
      RETURNING
        VALUE(rv_response) TYPE string
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Check HTTP Status is success</p>
    "!
    "! @parameter iv_http_status | <p class="shorttext synchronized" lang="en">HTTP Status</p>
    "! @parameter rv_success | <p class="shorttext synchronized" lang="en">Is success</p>
    METHODS is_response_success
      IMPORTING
        !iv_http_status   TYPE csequence
      RETURNING
        VALUE(rv_success) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Generate HTTP Client</p>
    "!
    "! @parameter iv_rfcdest | <p class="shorttext synchronized" lang="en">RFC Destination</p>
    "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Telegram exception</p>
    METHODS create_http_client
      IMPORTING
        iv_rfcdest TYPE rfcdest
      RAISING
        zcx_covid19_exception.

    "! <p class="shorttext synchronized" lang="en">Fill HTTP Request header fields</p>
    "!
    "! @parameter rt_fields | <p class="shorttext synchronized" lang="en">Fields</p>
    METHODS fill_request_header_fields ABSTRACT
      RETURNING
        VALUE(rt_fields) TYPE tihttpnvp.

  PRIVATE SECTION.
    DATA mi_http_client TYPE REF TO if_http_client.
    DATA mo_rest_client TYPE REF TO cl_rest_http_client.

ENDCLASS.



CLASS zcl_covid19_rest_api IMPLEMENTATION.


  METHOD zif_covid19_rest_api~get_stats_latest.

    create_http_client( get_rfc_dest( ) ).

    DATA(lv_response) = get_response( get_stats_latest_method( ) ).

    rs_stats = conv_stats_latest( lv_response ).

  ENDMETHOD.


  METHOD conv_stats_latest.

    /ui2/cl_json=>deserialize( EXPORTING json         = iv_response
                                         pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
                               CHANGING  data         = rs_stats ).

  ENDMETHOD.


  METHOD zif_covid19_rest_api~get_stats_global.

    create_http_client( get_rfc_dest( ) ).

    DATA(lv_response) = get_response( get_stats_global_method( ) ).

    rt_stats = conv_stats_global( lv_response ).

  ENDMETHOD.


  METHOD conv_stats_global.

    /ui2/cl_json=>deserialize( EXPORTING json         = iv_response
                                         pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
                               CHANGING  data         = rt_stats ).

  ENDMETHOD.


  METHOD zif_covid19_rest_api~get_stats_country.

    create_http_client( get_rfc_dest( ) ).

    DATA(lv_response) = get_response( iv_method  = get_stats_country_method( iv_country )
                                      iv_content = get_stats_country_req_content( iv_country ) ).

    rs_stats = conv_stats_country( lv_response ).

  ENDMETHOD.


  METHOD get_stats_country_req_content.
    CLEAR: rv_content.
  ENDMETHOD.


  METHOD conv_stats_country.

    /ui2/cl_json=>deserialize( EXPORTING json         = iv_response
                                         pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
                               CHANGING  data         = rs_stats ).

  ENDMETHOD.


  METHOD zif_covid19_rest_api~get_history.

    create_http_client( get_rfc_dest( ) ).

    DATA(lv_response) = get_response( get_history_method( ) ).

    rt_history = conv_history( lv_response ).

  ENDMETHOD.


  METHOD conv_history.

    /ui2/cl_json=>deserialize( EXPORTING json         = iv_response
                                         pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
                               CHANGING  data         = rt_history ).

  ENDMETHOD.


  METHOD zif_covid19_rest_api~get_history_country.

    create_http_client( get_rfc_dest( ) ).

    DATA(lv_response) = get_response( iv_method  = get_history_country_method( iv_country )
                                      iv_content = get_hist_country_req_content( iv_country ) ).

    rt_history = conv_history_country( lv_response ).

  ENDMETHOD.


  METHOD get_hist_country_req_content.
    CLEAR: rv_content.
  ENDMETHOD.


  METHOD conv_history_country.

    /ui2/cl_json=>deserialize( EXPORTING json         = iv_response
                                         pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
                               CHANGING  data         = rt_history ).

  ENDMETHOD.


  METHOD create_http_client.

    DATA: lv_reason        TYPE string,
          lv_utc_timestamp TYPE timestampl.

    " Create the HTTP client instance
    cl_http_client=>create_by_destination( EXPORTING  destination              = iv_rfcdest
                                           IMPORTING  client                   = mi_http_client
                                           EXCEPTIONS destination_not_found    = 1
                                                      internal_error           = 2
                                                      argument_not_found       = 3
                                                      destination_no_authority = 4
                                                      plugin_not_active        = 5
                                                      OTHERS                   = 5 ).
    IF sy-subrc <> 0.
      " log the exception
      " Ping Destination Failed get time stamp field lv_utc_timestamp.
      zcx_covid19_exception=>raise_by_syst( ).
      RETURN.
    ENDIF.

    IF mi_http_client IS NOT BOUND.
      zcx_covid19_exception=>raise( 'Client not generated' ).
    ENDIF.

    mi_http_client->request->set_header_fields( fill_request_header_fields( ) ).

  ENDMETHOD.


  METHOD get_response.

    DATA: BEGIN OF ls_error,
            ok          TYPE abap_bool,
            error_code  TYPE i,
            description TYPE string,
          END OF ls_error.

    IF mi_http_client IS NOT BOUND.
      RETURN.
    ENDIF.

    cl_http_utility=>set_request_uri( request = mi_http_client->request
                                      uri     = iv_method ).

    mi_http_client->request->set_cdata( iv_content ).

    " Create REST Client object
    DATA(lo_rest_client) = NEW cl_rest_http_client( io_http_client = mi_http_client ).

    TRY.
        lo_rest_client->if_rest_client~get( ).
      CATCH cx_rest_client_exception.
    ENDTRY.

    TRY.
        DATA(li_response) = lo_rest_client->if_rest_client~get_response_entity( ).
        DATA(lv_http_status) = li_response->get_header_field( '~status_code' ).

        "Receive the response data in JSON.
        DATA(lv_response) = li_response->get_string_data( ).

        IF is_response_success( lv_http_status ) = abap_false.

          /ui2/cl_json=>deserialize( EXPORTING json = lv_response
                                     CHANGING  data = ls_error ).

          IF ls_error-description IS INITIAL.
            "HTTP Request Failed
            DATA(lv_reason) = li_response->get_header_field( '~status_reason' ).
          ELSE.
            lv_reason = ls_error-description.
          ENDIF.

          " STOP Processing
          zcx_covid19_exception=>raise( lv_reason ).

        ENDIF.

        " Refresh the response to clear http memory of previous calls
        IF mi_http_client IS BOUND.
          mi_http_client->refresh_response( ).
          " close http session (exception http_no_memory)
          mi_http_client->close( ).
        ENDIF.

        rv_response = lv_response.

        " Collect into the exception table.
      CATCH cx_rest_client_exception INTO DATA(lx_rest_client_exception).
        RAISE EXCEPTION TYPE zcx_covid19_exception
          EXPORTING
            previous = lx_rest_client_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD is_response_success.

    TYPES: BEGIN OF lty_range_code,
             sign   TYPE ddsign,
             option TYPE ddoption,
             low    TYPE i,
             high   TYPE i,
           END OF lty_range_code,
           ltt_range_code TYPE STANDARD TABLE OF lty_range_code WITH EMPTY KEY.

    DATA(lt_r_success_values) = VALUE ltt_range_code( ( sign   = 'I'
                                                        option = 'EQ'
                                                        low    = cl_rest_status_code=>gc_success_ok )
                                                      ( sign   = 'I'
                                                        option = 'EQ'
                                                        low    = cl_rest_status_code=>gc_success_created )
                                                      ( sign   = 'I'
                                                        option = 'EQ'
                                                        low    = cl_rest_status_code=>gc_success_accepted ) ).

    IF iv_http_status IN lt_r_success_values.
      rv_success = abap_true.
    ELSE.
      rv_success = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD get_rfc_dest.
    zcx_covid19_exception=>raise( 'Method get_rfc_dest not implemented' ).
  ENDMETHOD.


  METHOD get_stats_country_method.
    zcx_covid19_exception=>raise( 'Method get_stats_country_method not implemented' ).
  ENDMETHOD.


  METHOD get_stats_global_method.
    zcx_covid19_exception=>raise( 'Method get_stats_global_method not implemented' ).
  ENDMETHOD.


  METHOD get_stats_latest_method.
    zcx_covid19_exception=>raise( 'Method get_stats_latest_method not implemented' ).
  ENDMETHOD.


  METHOD get_history_method.
    zcx_covid19_exception=>raise( 'Method get_history_method not implemented' ).
  ENDMETHOD.


  METHOD get_history_country_method.
    zcx_covid19_exception=>raise( 'Method get_history_country_method not implemented' ).
  ENDMETHOD.


ENDCLASS.
