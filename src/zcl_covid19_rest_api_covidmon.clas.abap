"! <p class="shorttext synchronized" lang="en">Covid 19: Rest API for Coronavirus Monitor</p>
"! <strong>Consuming APIs:</strong>
"! <ul>
"! <li>https://rapidapi.com/astsiatsko/api/coronavirus-monitor</li>
"! </ul>
CLASS zcl_covid19_rest_api_covidmon DEFINITION
  PUBLIC
  INHERITING FROM zcl_covid19_rest_api
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS gc_rfc_dest TYPE rfcdest VALUE 'COVID19_RAPIDAPI' ##NO_TEXT.

    CONSTANTS:
      BEGIN OF gc_methods,
        latest          TYPE string VALUE 'worldstat.php' ##NO_TEXT,
        country         TYPE string VALUE 'cases_by_particular_country.php' ##NO_TEXT,
        history_country TYPE string VALUE 'cases_by_particular_country.php' ##NO_TEXT,
      END OF gc_methods.

  PROTECTED SECTION.
    METHODS:
      get_rfc_dest REDEFINITION,
      fill_request_header_fields REDEFINITION,
      get_stats_latest_method REDEFINITION,
      get_stats_country_method REDEFINITION,
      get_history_method REDEFINITION,
      get_history_country_method REDEFINITION,

      conv_stats_latest REDEFINITION,
      conv_history_country REDEFINITION,

      get_stats_country_req_content REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_covid19_rest_api_covidmon IMPLEMENTATION.


  METHOD get_rfc_dest.
    rv_rfcdest = gc_rfc_dest.
  ENDMETHOD.


  METHOD fill_request_header_fields.
    rt_fields = VALUE #( ( name = 'x-rapidapi-host' value = 'coronavirus-monitor.p.rapidapi.com' )
                         ( name = 'x-rapidapi-key'  value = '448cc0e5dbmshb275d28846f4384p1ea405jsn798e4b58bff5' ) ).
  ENDMETHOD.


  METHOD get_stats_country_method.
    rv_method = gc_methods-country.
  ENDMETHOD.


  METHOD get_stats_latest_method.
    rv_method = gc_methods-latest.
  ENDMETHOD.


  METHOD get_history_method.

  ENDMETHOD.


  METHOD get_history_country_method.
    rv_method = |{ gc_methods-history_country }/?country={ iv_country }|.
  ENDMETHOD.


  METHOD conv_stats_latest.

    DATA:
      BEGIN OF ls_latest_str,
        total_cases            TYPE string,
        total_deaths           TYPE string,
        total_recovered        TYPE string,
        new_cases              TYPE string,
        new_deaths             TYPE string,
        statistic_taken_at(20) TYPE c,
      END OF ls_latest_str,
      BEGIN OF ls_latest,
        total_cases        TYPE i,
        total_deaths       TYPE i,
        total_recovered    TYPE i,
        new_cases          TYPE i,
        new_deaths         TYPE i,
        statistic_taken_at TYPE timestamp,
      END OF ls_latest.

    /ui2/cl_json=>deserialize( EXPORTING json = iv_response
                               CHANGING  data = ls_latest_str ).

    REPLACE ALL OCCURRENCES OF ',' IN ls_latest_str-total_cases WITH space.
    REPLACE ALL OCCURRENCES OF ',' IN ls_latest_str-total_deaths WITH space.
    REPLACE ALL OCCURRENCES OF ',' IN ls_latest_str-total_recovered WITH space.
    REPLACE ALL OCCURRENCES OF ',' IN ls_latest_str-new_cases WITH space.
    REPLACE ALL OCCURRENCES OF ',' IN ls_latest_str-new_deaths WITH space.

    ls_latest-total_cases = condense( ls_latest_str-total_cases ).
    ls_latest-total_deaths = condense( ls_latest_str-total_deaths ).
    ls_latest-total_recovered = condense( ls_latest_str-total_recovered ).
    ls_latest-new_cases = condense( ls_latest_str-new_cases ).
    ls_latest-new_deaths = condense( ls_latest_str-new_deaths ).
    ls_latest-statistic_taken_at = |{
                                      ls_latest_str-statistic_taken_at(4)
                                    }{
                                      ls_latest_str-statistic_taken_at+5(2)
                                    }{
                                      ls_latest_str-statistic_taken_at+8(2)
                                    }{
                                      ls_latest_str-statistic_taken_at+11(2)
                                    }{
                                      ls_latest_str-statistic_taken_at+14(2)
                                    }{
                                      ls_latest_str-statistic_taken_at+17(2)
                                    }|.

    rs_stats = VALUE #( cases      = ls_latest-total_cases
                        new_cases  = ls_latest-new_cases
                        deaths     = ls_latest-total_deaths
                        new_deaths = ls_latest-new_deaths
                        recovered  = ls_latest-total_recovered
                        date       = ls_latest-statistic_taken_at ).

  ENDMETHOD.


  METHOD conv_history_country.

    TYPES:
      BEGIN OF ty_details_str,
        id                TYPE string,
        country_name      TYPE string,
        total_cases       TYPE string,
        new_cases         TYPE string,
        active_cases      TYPE string,
        total_deaths      TYPE string,
        new_deaths        TYPE string,
        total_recovered   TYPE string,
        serious_critical  TYPE string,
        region            TYPE string,
        total_cases_per1m TYPE string,
        record_date       TYPE string,
      END OF ty_details_str,
      tty_details_str TYPE HASHED TABLE OF ty_details_str WITH UNIQUE KEY id.

    TYPES:
      BEGIN OF ty_result_str,
        country         TYPE string,
        stat_by_country TYPE tty_details_str,
      END OF ty_result_str.

    DATA: ls_result  TYPE ty_result_str,
          ls_history LIKE LINE OF rt_history.

    /ui2/cl_json=>deserialize( EXPORTING json = iv_response
                               CHANGING  data = ls_result ).

    LOOP AT ls_result-stat_by_country INTO DATA(ls_stats).

      REPLACE ALL OCCURRENCES OF ',' IN ls_stats-total_cases       WITH space.
      REPLACE ALL OCCURRENCES OF ',' IN ls_stats-new_cases         WITH space.
      REPLACE ALL OCCURRENCES OF ',' IN ls_stats-active_cases      WITH space.
      REPLACE ALL OCCURRENCES OF ',' IN ls_stats-total_deaths      WITH space.
      REPLACE ALL OCCURRENCES OF ',' IN ls_stats-new_deaths        WITH space.
      REPLACE ALL OCCURRENCES OF ',' IN ls_stats-total_recovered   WITH space.
      REPLACE ALL OCCURRENCES OF ',' IN ls_stats-serious_critical  WITH space.
      REPLACE ALL OCCURRENCES OF ',' IN ls_stats-total_cases_per1m WITH space.

      ls_history-country = ls_result-country.
      ls_history-cases = condense( ls_stats-total_cases ).
      ls_history-new_cases = condense( ls_stats-new_cases ).
      ls_history-active_cases = condense( ls_stats-active_cases ).
      ls_history-deaths = condense( ls_stats-total_deaths ).
      ls_history-recovered = condense( ls_stats-total_recovered ).
      ls_history-critical = condense( ls_stats-new_deaths ).
      ls_history-new_deaths = condense( ls_stats-serious_critical ).
      ls_history-date = |{
                           ls_stats-record_date(4)
                         }{
                           ls_stats-record_date+5(2)
                         }{
                           ls_stats-record_date+8(2)
                         }{
                           ls_stats-record_date+11(2)
                         }{
                           ls_stats-record_date+14(2)
                         }{
                           ls_stats-record_date+17(2)
                         }|.

      INSERT ls_history INTO TABLE rt_history.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_stats_country_req_content.

    DATA: BEGIN OF ls_detail,
            country TYPE string,
          END OF ls_detail.

    ls_detail-country = iv_country.

    rv_content = /ui2/cl_json=>serialize( data        = ls_detail
                                          pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  ENDMETHOD.


ENDCLASS.
