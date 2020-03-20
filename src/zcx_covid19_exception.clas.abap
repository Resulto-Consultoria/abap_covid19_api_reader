"! <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
CLASS zcx_covid19_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CONSTANTS gc_generic_error_msg TYPE string VALUE `An error occured (ZCX_COVID19_EXCEPTION)` ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">RSRSCAN1: Search String/Search Pattern</p>
    DATA text1 TYPE sstring .
    "! <p class="shorttext synchronized" lang="en">RSRSCAN1: Search String/Search Pattern</p>
    DATA text2 TYPE sstring .
    "! <p class="shorttext synchronized" lang="en">RSRSCAN1: Search String/Search Pattern</p>
    DATA text3 TYPE sstring .
    "! <p class="shorttext synchronized" lang="en">RSRSCAN1: Search String/Search Pattern</p>
    DATA text4 TYPE sstring .

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    "! @parameter textid | <p class="shorttext synchronized" lang="en">Text ID</p>
    "! @parameter previous | <p class="shorttext synchronized" lang="en">Previous exception</p>
    "! @parameter text1 | <p class="shorttext synchronized" lang="en">Message Text 1</p>
    "! @parameter text2 | <p class="shorttext synchronized" lang="en">Message Text 2</p>
    "! @parameter text3 | <p class="shorttext synchronized" lang="en">Message Text 3</p>
    "! @parameter text4 | <p class="shorttext synchronized" lang="en">Message Text 4</p>
    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !text1    TYPE sstring OPTIONAL
        !text2    TYPE sstring OPTIONAL
        !text3    TYPE sstring OPTIONAL
        !text4    TYPE sstring OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Triggers a Covid19 Exception for a Text</p>
    "!
    "! @parameter iv_text | <p class="shorttext synchronized" lang="en">Text</p>
    "! @parameter ix_previous | <p class="shorttext synchronized" lang="en">Previous exception</p>
    "! @raising   zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    CLASS-METHODS raise
      IMPORTING
        !iv_text     TYPE clike
        !ix_previous TYPE REF TO cx_root OPTIONAL
      RAISING
        zcx_covid19_exception .
    "! <p class="shorttext synchronized" lang="en">Triggers a Covid19 Exception from a System Message</p>
    "!
    "! @raising   zcx_covid19_exception | <p class="shorttext synchronized" lang="en">Covid19 Exception</p>
    CLASS-METHODS raise_by_syst
      RAISING
        zcx_covid19_exception .
  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_msg,
        msgv1 TYPE symsgv,
        msgv2 TYPE symsgv,
        msgv3 TYPE symsgv,
        msgv4 TYPE symsgv,
      END OF ty_msg.

    CLASS-METHODS set_msg_vars_for_clike
      IMPORTING
        iv_text TYPE csequence.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_covid19_exception IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->text1 = text1 .
    me->text2 = text2 .
    me->text3 = text3 .
    me->text4 = text4 .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD raise.

    DATA: lv_text TYPE string.

    IF iv_text IS INITIAL.
      lv_text = gc_generic_error_msg.
    ELSE.
      lv_text = iv_text.
    ENDIF.

    set_msg_vars_for_clike( lv_text ).

    RAISE EXCEPTION TYPE zcx_covid19_exception
      EXPORTING
        textid   = VALUE #( msgid = sy-msgid
                            msgno = sy-msgno
                            attr1 = sy-msgv1
                            attr2 = sy-msgv2
                            attr3 = sy-msgv3
                            attr4 = sy-msgv4 )
        previous = ix_previous.

  ENDMETHOD.


  METHOD raise_by_syst.

    RAISE EXCEPTION TYPE zcx_covid19_exception
      EXPORTING
        textid = VALUE scx_t100key( msgid = sy-msgid
                                    msgno = sy-msgno
                                    attr1 = sy-msgv1
                                    attr2 = sy-msgv2
                                    attr3 = sy-msgv3
                                    attr4 = sy-msgv4 ).

  ENDMETHOD.


  METHOD set_msg_vars_for_clike.

    DATA: BEGIN OF ls_msg,
            msgv1 TYPE symsgv,
            msgv2 TYPE symsgv,
            msgv3 TYPE symsgv,
            msgv4 TYPE symsgv,
          END OF ls_msg,
          lv_dummy TYPE string.

    ls_msg = iv_text.

    MESSAGE e001(00) WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4
                     INTO lv_dummy.

  ENDMETHOD.


ENDCLASS.
