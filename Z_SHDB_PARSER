*&---------------------------------------------------------------------*
*& Report  Z_SHDB_PARSER
*&
*&---------------------------------------------------------------------*
*& Программа генерации подпрограммы вызова BDC (батч-инпут) 
*& по записи в транзакции SHDB
*&---------------------------------------------------------------------*

REPORT z_shdb_parser.

DATA: t_table TYPE TABLE OF bdcdata.

PARAMETERS: p_group TYPE apq_grpn OBLIGATORY,
            p_user  TYPE apq_mapn OBLIGATORY,
            p_all   TYPE c AS CHECKBOX DEFAULT 'X'.

SET BLANK LINES ON.

PERFORM: load_data.
IF t_table IS INITIAL.
  MESSAGE 'данные не найдены' TYPE 'S' DISPLAY LIKE 'E'.
ELSE.
  IF p_all = 'X'.
    PERFORM fill_header.
  ENDIF.

  PERFORM  fill_body.

  IF p_all = 'X'.
    PERFORM  fill_footer.
  ENDIF.
ENDIF.


FORM load_data.
  DATA: qid TYPE apq_quid.

  SELECT SINGLE qid FROM apqi INTO qid
    WHERE groupid = p_group AND
          creator = p_user.

  CALL FUNCTION 'BDC_OBJECT_READ'
    EXPORTING
      queue_id         = qid
    TABLES
      dynprotab        = t_table
    EXCEPTIONS
      not_found        = 1
      system_failure   = 2
      invalid_datatype = 3
      OTHERS           = 4.
ENDFORM.

FORM fill_header.
  WRITE /: 'DEFINE dynpro.',
           '  clear lt_bdcdata.',
           '  lt_bdcdata-program = &1.',
           '  lt_bdcdata-dynpro = &2.',
           '  lt_bdcdata-dynbegin = ''X''.',
           '  append lt_bdcdata.',
           '  clear lt_bdcdata.',
           '  lt_bdcdata-fnam = ''BDC_OKCODE''.',
           '  lt_bdcdata-fval = &3.',
           '  append lt_bdcdata.',
           'END-OF-DEFINITION.',
           '',
           'DEFINE set_field.',
           '  clear lt_bdcdata.',
           '  lt_bdcdata-fnam = &1.',
           '  if not &2 is initial.',
           '    write &2 to lt_bdcdata-fval.',
           '    shift lt_bdcdata-fval left deleting leading space.',
           '  else.',
           '    clear lt_bdcdata-fval.',
           '  endif.',
           '  append lt_bdcdata.',
           'END-OF-DEFINITION.',
           '',
           'DATA: lt_bdcdata TYPE TABLE OF bdcdata WITH HEADER LINE,',
           '      lt_message TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,',
           '      lt_return  TYPE TABLE OF bapiret2,',
           '      ls_options TYPE ctu_params.',
           '',
           'REFRESH lt_bdcdata.'.
  SKIP.
ENDFORM.


FORM fill_body.
  DATA: ls_item TYPE bdcdata.

  LOOP AT t_table INTO ls_item WHERE dynbegin <> 'T'.
    IF ls_item-dynbegin = 'X'.
      PERFORM get_dynpro USING sy-tabix.
    ELSE.
      PERFORM get_field  USING sy-tabix.
    ENDIF.
  ENDLOOP.
ENDFORM.


FORM fill_footer.
  DATA: l_footer TYPE string,
        ls_item TYPE bdcdata,
        l_tcode TYPE string,
        lt_text TYPE TABLE OF string.

  READ TABLE t_table INTO ls_item WITH KEY dynbegin = 'T'.
  l_tcode = |CALL TRANSACTION '{ ls_item-fnam }' USING lt_bdcdata[]| .

  WRITE /: '',
           '',
           'ls_options-dismode = ''N''. " A - для тестов',
           '',
           l_tcode ,
           '  OPTIONS FROM ls_options',
           '    MESSAGES INTO lt_message[].',
           '',
           'CALL FUNCTION ''CONVERT_BDCMSGCOLL_TO_BAPIRET2''',
           '  TABLES',
           '    imt_bdcmsgcoll = lt_message',
           '    ext_return     = lt_return.',
           ' ',
           'READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = ''E''.',
           'IF sy-subrc = 0.',
           '  CALL FUNCTION ''C14ALD_BAPIRET2_SHOW''',
           '    TABLES',
           '      i_bapiret2_tab = lt_return.',
           'ENDIF.'.
ENDFORM.


FORM get_dynpro USING p_tabix.
  DATA: ls_item TYPE bdcdata,
        l_prog TYPE string,
        l_scr  TYPE string,
        l_ok   TYPE string,
        text   TYPE string.

  READ TABLE t_table INTO ls_item INDEX p_tabix.
  l_prog = ls_item-program.
  l_scr  = ls_item-dynpro.

  LOOP AT t_table INTO ls_item FROM p_tabix.
    IF ls_item-fnam = 'BDC_OKCODE'.
      l_ok = ls_item-fval.
      EXIT.
    ENDIF.
  ENDLOOP.

  text = |dynpro '{ l_prog }' '{ l_scr }' '{ l_ok }'.|.

  WRITE /: '', text.
ENDFORM.


FORM get_field USING p_tabix.
  DATA: ls_item TYPE bdcdata,
        l_field TYPE string,
        l_value TYPE string,
        text    TYPE string.

  READ TABLE t_table INTO ls_item INDEX p_tabix.
  l_field = ls_item-fnam.
  l_value = ls_item-fval.

  CHECK l_field NE: 'BDC_CURSOR', 'BDC_OKCODE', 'BDC_SUBSCR'.

  text = |set_field '{ l_field }' '{ l_value }'.|.
  WRITE: / text.
ENDFORM.
