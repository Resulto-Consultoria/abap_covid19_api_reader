"! <p class="shorttext synchronized" lang="en">Covid 19: Rest API</p>
"! <strong>Consuming APIs:</strong>
"! <ul>
"! <li>https://github.com/javieraviles/covidAPI</li>
"! </ul>
CLASS zcl_covid19_rest_api_herokuapp DEFINITION
  PUBLIC
  INHERITING FROM zcl_covid19_rest_api
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS gc_rfc_dest TYPE rfcdest VALUE 'COVID19_GLOBAL' ##NO_TEXT.

    CONSTANTS:
      BEGIN OF gc_methods,
        get_latest TYPE string VALUE 'all' ##NO_TEXT,
        get_global TYPE string VALUE 'countries' ##NO_TEXT,
      END OF gc_methods.

  PROTECTED SECTION.
    METHODS:
      get_rfc_dest REDEFINITION,
      fill_request_header_fields REDEFINITION,
      get_stats_latest_method REDEFINITION,
      get_stats_global_method REDEFINITION,
      get_stats_country_method REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_covid19_rest_api_herokuapp IMPLEMENTATION.


  METHOD get_rfc_dest.
    rv_rfcdest = gc_rfc_dest.
  ENDMETHOD.


  METHOD fill_request_header_fields.
    CLEAR: rt_fields. " No header fields needed
  ENDMETHOD.


  METHOD get_stats_country_method.
    rv_method = |{ gc_methods-get_global }/{ iv_country }|.
  ENDMETHOD.


  METHOD get_stats_global_method.
    rv_method = gc_methods-get_global.
  ENDMETHOD.


  METHOD get_stats_latest_method.
    rv_method = gc_methods-get_latest.
  ENDMETHOD.


ENDCLASS.
