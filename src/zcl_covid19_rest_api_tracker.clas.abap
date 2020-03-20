"! <p class="shorttext synchronized" lang="en">Covid 19: Rest API</p>
"! <strong>Consuming APIs:</strong>
"! <ul>
"! <li>https://github.com/ExpDev07/coronavirus-tracker-api</li>
"! </ul>
CLASS zcl_covid19_rest_api_tracker DEFINITION
  PUBLIC
  INHERITING FROM zcl_covid19_rest_api
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS gc_rfc_dest TYPE rfcdest VALUE 'COVID19_TRACKER' ##NO_TEXT.

    TYPES:
      BEGIN OF ty_latest,
        confirmed TYPE i,
        deaths    TYPE i,
        recovered TYPE i,
      END OF ty_latest.

    TYPES:
      BEGIN OF ty_coordinates,
        latitude  TYPE string,
        longitude TYPE string,
      END OF ty_coordinates.

    TYPES:
      BEGIN OF ty_timeline,
        date  TYPE timestamp,
        cases TYPE i,
      END OF ty_timeline,
      tty_timeline TYPE HASHED TABLE OF ty_timeline WITH UNIQUE KEY date.

    TYPES:
      BEGIN OF ty_timeline_latest,
        latest   TYPE i,
        timeline TYPE tty_timeline,
      END OF ty_timeline_latest.

    TYPES:
      BEGIN OF ty_timelines,
        confirmed TYPE ty_timeline_latest,
        deaths    TYPE ty_timeline_latest,
        recovered TYPE ty_timeline_latest,
      END OF ty_timelines.

    TYPES:
      BEGIN OF ty_location,
        id           TYPE i,
        country      TYPE string,
        country_code TYPE string,
        province     TYPE string,
        coordinates  TYPE ty_coordinates,
        latest       TYPE ty_latest,
        timelines    TYPE ty_timelines,
      END OF ty_location,
      tty_location TYPE HASHED TABLE OF ty_location WITH UNIQUE KEY id.

    CONSTANTS:
      BEGIN OF gc_methods,
        latest    TYPE string VALUE 'latest' ##NO_TEXT,
        locations TYPE string VALUE 'locations' ##NO_TEXT,
      END OF gc_methods.

  PROTECTED SECTION.
    METHODS:
      get_rfc_dest REDEFINITION,
      fill_request_header_fields REDEFINITION,

      get_stats_latest_method REDEFINITION,
      conv_stats_latest REDEFINITION,

      get_stats_global_method REDEFINITION,
      conv_stats_global REDEFINITION,

      get_stats_country_method REDEFINITION,
      conv_stats_country REDEFINITION,

      get_history_country_method REDEFINITION,
      conv_history_country REDEFINITION.

    METHODS conv_response_locations
      IMPORTING
        iv_response         TYPE string
      RETURNING
        VALUE(rt_locations) TYPE tty_location.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_covid19_rest_api_tracker IMPLEMENTATION.


  METHOD get_rfc_dest.
    rv_rfcdest = gc_rfc_dest.
  ENDMETHOD.


  METHOD fill_request_header_fields.
    CLEAR: rt_fields. " No header fields needed
  ENDMETHOD.


  METHOD get_stats_latest_method.
    rv_method = gc_methods-latest.
  ENDMETHOD.


  METHOD conv_stats_latest.

    DATA:
      BEGIN OF ls_response,
        latest TYPE ty_latest,
      END OF ls_response.

    /ui2/cl_json=>deserialize( EXPORTING json = iv_response
                               CHANGING  data = ls_response ).

    rs_stats = VALUE #( cases     = ls_response-latest-confirmed
                        deaths    = ls_response-latest-deaths
                        recovered = ls_response-latest-recovered ).

  ENDMETHOD.


  METHOD get_stats_global_method.
    rv_method = gc_methods-locations.
  ENDMETHOD.


  METHOD conv_stats_global.

    FIELD-SYMBOLS: <ls_stats> LIKE LINE OF rt_stats.

    DATA(lt_locations) = conv_response_locations( iv_response ).

    LOOP AT lt_locations INTO DATA(ls_location).

      IF line_exists( rt_stats[ country = ls_location-country ] ).
        READ TABLE rt_stats ASSIGNING <ls_stats> WITH KEY country = ls_location-country.
      ELSE.
        INSERT VALUE #( country = ls_location-country ) INTO TABLE rt_stats ASSIGNING <ls_stats>.
      ENDIF.

      <ls_stats>-cases     = <ls_stats>-cases     + ls_location-latest-confirmed.
      <ls_stats>-deaths    = <ls_stats>-deaths    + ls_location-latest-deaths.
      <ls_stats>-recovered = <ls_stats>-recovered + ls_location-latest-recovered.

      UNASSIGN <ls_stats>.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_stats_country_method.
    rv_method = gc_methods-locations.
  ENDMETHOD.


  METHOD conv_stats_country.

    DATA(lt_locations) = conv_response_locations( iv_response ).

    LOOP AT lt_locations INTO DATA(ls_location).
      rs_stats-country   = ls_location-country.
      rs_stats-cases     = rs_stats-cases     + ls_location-latest-confirmed.
      rs_stats-deaths    = rs_stats-deaths    + ls_location-latest-deaths.
      rs_stats-recovered = rs_stats-recovered + ls_location-latest-recovered.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_history_country_method.

    create_http_client( get_rfc_dest( ) ).

    DATA(lv_response) = get_response( get_stats_global_method( ) ).

    DATA(lt_locations) = conv_response_locations( lv_response ).

    TRY.
        DATA(lv_country_code) = lt_locations[ country = iv_country ]-country_code.
      CATCH cx_sy_itab_line_not_found.
        zcx_covid19_exception=>raise( 'Country Code not found' ).
    ENDTRY.

    rv_method = |{ gc_methods-locations }?country_code={ lv_country_code }&timelines=1|.

  ENDMETHOD.


  METHOD conv_history_country.

    DATA(lt_locations) = conv_response_locations( iv_response ).

    LOOP AT lt_locations INTO DATA(ls_location).
    ENDLOOP.

  ENDMETHOD.


  METHOD conv_response_locations.

    DATA:
      BEGIN OF ls_locations,
        locations TYPE tty_location,
      END OF ls_locations.

    /ui2/cl_json=>deserialize( EXPORTING json = iv_response
                               CHANGING  data = ls_locations ).

    rt_locations = ls_locations-locations.

  ENDMETHOD.


ENDCLASS.
