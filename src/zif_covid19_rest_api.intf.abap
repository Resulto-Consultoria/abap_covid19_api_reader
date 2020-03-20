"! <p class="shorttext synchronized" lang="en">Covid19 Rest Api</p>
INTERFACE zif_covid19_rest_api
  PUBLIC.

  TYPES:
    BEGIN OF ty_stats_latest,
      cases         TYPE i,
      new_cases     TYPE i,
      deaths        TYPE i,
      new_deaths    TYPE i,
      recovered     TYPE i,
      new_recovered TYPE i,
      date          TYPE timestamp,
    END OF ty_stats_latest.

  TYPES:
    BEGIN OF ty_stats_country,
      country               TYPE string,
      cases                 TYPE i,
      today_cases           TYPE i,
      deaths                TYPE i,
      today_deaths          TYPE i,
      recovered             TYPE i,
      active                TYPE i,
      critical              TYPE i,
      cases_per_one_million TYPE i,
    END OF ty_stats_country,
    tty_stats_country TYPE HASHED TABLE OF ty_stats_country WITH UNIQUE KEY country.

  TYPES:
    BEGIN OF ty_stats_region,
      region    TYPE string,
      cases     TYPE i,
      deaths    TYPE i,
      recovered TYPE i,
    END OF ty_stats_region,
    tty_stats_region TYPE HASHED TABLE OF ty_stats_region WITH UNIQUE KEY region.

  TYPES:
    BEGIN OF ty_history,
      country      TYPE string,
      date         TYPE timestamp,
      cases        TYPE i,
      new_cases    TYPE i,
      active_cases TYPE i,
      deaths       TYPE i,
      new_deaths   TYPE i,
      recovered    TYPE i,
      critical     TYPE i,
    END OF ty_history,
    tty_history TYPE HASHED TABLE OF ty_history WITH UNIQUE KEY country date.

  "! <p class="shorttext synchronized" lang="en">Get Global Statics</p>
  "!
  "! @parameter rs_stats | <p class="shorttext synchronized" lang="en">Stats</p>
  "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
  METHODS get_stats_latest
    RETURNING
      VALUE(rs_stats) TYPE ty_stats_latest
    RAISING
      zcx_covid19_exception.

  "! <p class="shorttext synchronized" lang="en">Get Global Statics</p>
  "!
  "! @parameter rt_stats | <p class="shorttext synchronized" lang="en">Stats</p>
  "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
  METHODS get_stats_global
    RETURNING
      VALUE(rt_stats) TYPE tty_stats_country
    RAISING
      zcx_covid19_exception.

  "! <p class="shorttext synchronized" lang="en">Get Country Statics</p>
  "!
  "! @parameter iv_country | <p class="shorttext synchronized" lang="en">Country</p>
  "! @parameter rs_stats | <p class="shorttext synchronized" lang="en">Stats</p>
  "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
  METHODS get_stats_country
    IMPORTING
      iv_country      TYPE string
    RETURNING
      VALUE(rs_stats) TYPE ty_stats_country
    RAISING
      zcx_covid19_exception.

  "! <p class="shorttext synchronized" lang="en">Get cases history</p>
  "!
  "! @parameter rt_history | <p class="shorttext synchronized" lang="en">History</p>
  "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
  METHODS get_history
    RETURNING
      VALUE(rt_history) TYPE tty_history
    RAISING
      zcx_covid19_exception.

  "! <p class="shorttext synchronized" lang="en">Get cases history by country</p>
  "!
  "! @parameter iv_country | <p class="shorttext synchronized" lang="en">Country</p>
  "! @parameter rt_history | <p class="shorttext synchronized" lang="en">History</p>
  "! @raising zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
  METHODS get_history_country
    IMPORTING
      iv_country        TYPE string OPTIONAL
    RETURNING
      VALUE(rt_history) TYPE tty_history
    RAISING
      zcx_covid19_exception.

ENDINTERFACE.
