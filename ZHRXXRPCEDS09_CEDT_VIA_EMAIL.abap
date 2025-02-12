*====> Selektions- und Aufbereitungsteil RPCEDTx0  <====================
*-----------------------------------------------------------------------
*YMLN2633264 18.04.2018 Change or skip form in national form (if found)
* 7.00 (ACR)
*YMLN1097628 28.09.2007 Retroactive accounting pages without differences
*YMLN1089688 04.09.2007 Runtime improvement
*YMLN1081479 07.08.2007 Corresponding Evaluation class for current form
*YMLN1053751 07.05.2007 Retroactive accounting pages - Evaluation class
*YMLN1036996 14.03.2007 Retroactive accounting pages - special cases
*YMLN1029241 26.02.2007 Retroactive accounting pages and rules
*YMLN1017654 16.11.2006 Suppression of retroactive accounting pages IV
*YMLN1003534 30.11.2006 Suppression of retroactive accounting pages III
*YMLN987644 09.10.2006 Suppression of retroactive accounting pages II
*YMLN970454 07.08.2006 Suppression of retroactive accounting pages
* 6.00 (AC0)
*WOGK005492 02.08.2004 changed routine process_pernr becasue of ABKRS
*                      change problem => note 760945
*VKIK000738 13.07.2004 routine process_pernr rewritten because of
*                      incentive wages problem
* 4.70 (L6B)
*XULK039599 10.07.2003 Change to MODIFY_INFOTYPES note 638300
*VKIK032705 01.07.2003 Multiple Checks enhancements
*VKIK016676 20.11.2002 payroll area change within a period
*VKIK016138 14.11.2002 external call for RP-INIT-BUFFER added
*VKIK007690 16.08.2002 field rt_2-pernr wrong initialized
* 4.70 (PL0)
*VKIK035688 08.05.2002 new routine alloc-zrt-to-windows (CE)
*VKIK011129 21.03.2002 Enhancements for CE
*                      unused coding disabled
* 4.70 (AL0)
*AL0K100825 07.02.2002 Fixed checkman error marked as "#EC CI_GENBUFF
*HKUK096053 30.01.2002 new infotype for ext. person ID added
*VKIK083918 19.12.2001 switch for protocol added
*                      field statist replaced by prt_prot
*VKIK057197 09.10.2001 coding of obsolete routines removed
*HKUK027781 21.08.2001 switches for time quotas corrected
*VKIK032558 21.08.2001 Customer-exit option 'Z' added
*VKIK023668 23.07.2001 suppress printing for ESS (part II)
*WLIK027494 26.06.2001 retro tax splits
*WLIK055667 06.06.2001 suppress printing for ESS
* 4.6C (L9C)
*HKUK008843 19.04.2001 init_form -> clear i512e_zk
*VKIK046117 14.03.2001 Cumul.type 'F' + 'H' table CRT added
*VKIK045853 13.03.2001 check on input for F4-help
*HKUK041440 08.02.2001 external call for testforms
*VKIK040627 05.02.2001 authority check on P0008 inserted (note 185539)
*XRRK036865 16.01.2000 sort xrt extended
*note 370330 2.01.2001 missing refresh for wage slips
*HKUK032455 04.12.2000 switches for time quotas corrected
*HKUK026274 16.10.2000 addition of time quotas
*HKUK026303 12.10.2000 time quotas from transfer pool
*WLIK024907 29.09.2000 EVCLS in T514F ignored
*VKIK019960 17.08.2000 table CRT (when paid) added - Cumul.type 'K'
*VKIK017127 26.07.2000 special check for 'PAGE' - empty pages
*VKIK010617 24.05.2000 country-exits with 'IF FOUND' added
*VKIK009589 15.05.2000 set fields via external call
*VKIK005681 17.04.2000 archive parameters added
*HKUK002256 10.04.2000 correction RP-SET-DATA-INTERVAL
*VKIK001942 18.03.2000 Now using FM for filling of EVP
* 4.6C (AHR)
*VKIK065961 01.12.1999 Dynamic line-size and line-count now
*VKIK061490 02.11.1999 Filling of xinfo changed
*HKUK058239 08.10.1999 Time quotas deduction dates
*WLIK058239 07.10.1999 Out-of-Sequence Reversals if PRT_OSR = X
*VKIK058239 29.09.1999 deletion of type-pool HRIFT
*VKIK057674 23.09.1999 RP-SET-DATA-INTERVAL implemented
*WOGK056281 06.09.1999 deakt.Fuction HRPY_PROCESS_FIRE_EVENT, now in PNP
*VKIK056231 03.09.1999 macro RP-SET-NAME-FORMAT replaced by Func.module
*VKIK056067 01.09.1999 check for non_authorized manual checks added
*VKIK054253 08.08.1999 transfer tabid into tabname during T512E process
* 4.6A (PH9)
*VKIK006529 19.05.1999 table RTS inserted (contains delta summary)
* 4.6A (AHR)
*VKIK042611 12.03.1999 replace old field by new field for table EVP
*HKUK031254 03.02.1999 time quotas
*WLIK023737 07.09.1998 check for dummies after cd_read_previous
*VKIK023737 03.09.1998 access to table XBT via window now available
*VKIK023565 31.08.1998 testforms suppresses wagetype texts
* 4.5A (PH4)
*VKIK008541 01.07.1998 RE549D replaced by function module
*VKIK006809 19.06.1998 global field RUECK now filled
* 4.0C (AHR)
*VKIK017443 05.05.1998 new exit-routines activated
*VKIK000212 12.12.1997 error handling redesigned (not marked)
*                      Forms xedt_set/get_infotypes added
*                      unused coding removed (not marked)
*                      corr. XRRK026873 deactivated (not marked)
* 4.0C (ALR)
*XDOK065442 03.12.1997 Form xedt_set_buffer added
*VKIK065442 27.11.1997 Evaluation class and field I512E-DIFAR
* 4.0A (P40)
*VKIK047993 04.11.1997 global EVP defined
*VKIK043489 29.10.1997 Enhancements for EMU (II)
*XRRK026873 08.10.1997 processing of generic wagetypes in superlines
*VKIK019212 26.09.1997 reading of infotypes P2006 and P2007 changed
*VKIK012152 18.09.1997 out-of-sequence reversals now ignored
* 4.0A (ALR)
*VKIK051109 23.08.1997 Evaluation class used by table T512E (redesign)
*VKIK041980 14.08.1997 Enhancements for EMU
*VKIK038792 09.08.1997 use tabname instead tabid from T512E (not marked)
*VKIK023587 14.07.1997 no infotypes are selected because of
*WXJK014138 24.07.1997 rework incentive wages form
*VKIK009593 23.05.1997 special F4-help for form
* 3.0H
*VKIK139428 20.03.1997 new country-specific exits
*VKIK131798c13.03.1997 new field 'page counter per pers.no.'
*          b04.03.1997 diff. now appended instead collected, performance
* 3.0G
*VKIK125373 22.01.1997 deductions and arrears now country independent
* 3.0F
*VKIK112124 20.11.1996 suppress printing, if all fields are 0 (diff.)
* 3.0E
*VKIK076816 04.05.1996 report class (for building address) now flexible
* 3.0D
*VKIK071508 26.04.1996 transfer table rgdir to EDT via tables
*VKIK071395 26.04.1996 set fields of PNP if special pay.
*YAEK062170 02.04.1996 new handling of FBS CD_CREATE_DUMMY_ENTRIES
* 3.0C
*VKIK045475 25.01.1996 memos (P0128) to employee's now available.
*VKIK031819 20.12.1995 fill per pernr missed during external call 3
*                      sort with splits v0typ v0znr
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&  Alteração nº: 1                                                    *
*&     Descrição....: Inclusão do i0034                                *
*&     Autor........: Rui Silva                                        *
*&     Mail.........: rui.silva@sbx.pt                                 *
*&     Pedido por...: Rui Batista                                      *
*&     Data.........: 08/11/2023                                       *
*&     Status.......: Entregue                                         *
*&     Tag..........: RPS08112023                                      *
*&     Notas........: Incidente 118885                                 *
*&---------------------------------------------------------------------*

INITIALIZATION.
  PERFORM init_parameters USING subrc.
  PERFORM init_initialization.

AT SELECTION-SCREEN OUTPUT.                                 "VKIK009593
  PERFORM re514v USING sy-langu molga formular form_txt.    "VKIK009593
  PERFORM at_sel_screen_output_natio IN PROGRAM (repid) IF FOUND.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR formular.          "VKIK009593
  GET CURSOR FIELD fld_name.                                "VKIK045853
  PERFORM check_input_for_field USING fld_name fld_input.   "VKIK045853
  CHECK fld_input NE space.                                 "VKIK045853
  CALL FUNCTION 'F4_POPUP_FORML'
    EXPORTING
      molga  = molga
      fclass = 'CEDT'
*     SPRSL  = SY-LANGU
    IMPORTING
      forml  = formular
*     vtext  = fedt_txt
    EXCEPTIONS
      OTHERS = 1.

START-OF-SELECTION.
  PERFORM init_sos USING subrc.
  PERFORM get_parameters USING xcedt subrc.
  PERFORM check_parameter USING xcedt subrc.
  IF subrc NE 0.
    STOP.
  ENDIF.
  PERFORM init_form USING xcedt subrc.
  rp-set-data-interval 'ALL' low-date pn-endda.             "VKIK057674
  rp-set-data-interval 'P0000' low-date high-date.          "HKUK002256
  PERFORM modify_infotypes USING $rinfo$[].                 "VKIK045475
  IF sy-batch EQ true.                                      "VKIK065961
    CALL FUNCTION 'HR_PL_DETERMINE_PRI_PARAMS'
      EXPORTING
        imp_linesize       = i514d-c_use
        imp_linecount      = i514d-r_use
        imp_repid          = repid
      IMPORTING
*       EXP_OLD_PARAMS     =
        exp_new_params     = pri_params
        exp_new_arc_params = arc_params.                   "VKIK005681
*   NEW-PAGE PRINT ON PARAMETERS pri_params NO DIALOG.      "VKIK005681
    NEW-PAGE PRINT ON                                       "VKIK005681
             PARAMETERS pri_params
             ARCHIVE PARAMETERS arc_params
             NO DIALOG.
  ELSE.
    NEW-PAGE                                                "VKIK061498
      LINE-SIZE i514d-c_use
      LINE-COUNT i514d-r_use
      NO-TITLE
      NO-HEADING.
  ENDIF.                                                    "VKIK065961
  PERFORM write_testforms USING xcedt.

* KGM@SBX - 12.02.2025 - Inserido
  REFRESH it_sender_email.
  SELECT * FROM t5pfd INTO CORRESPONDING FIELDS OF TABLE it_sender_email
   WHERE field = 'ZMRV0'
     AND begda LE sy-datum
     AND endda GE sy-datum.
* KGM@SBX - 12.02.2025 - Fim de alteração

GET pernr.
  rp-init-buffer.
  IF impmem = true.
    PERFORM fill_import_export_key USING repid
                                         pernr-pernr.
    PERFORM import_buffer.
  ENDIF.

  CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'            "HKUK002256
    EXCEPTIONS
      OTHERS = 1.

  REFRESH p0001. CLEAR p0001.                               "HKUK002256
  CALL FUNCTION 'HR_READ_INFOTYPE'                          "HKUK002256
    EXPORTING
      tclas           = 'A'
      pernr           = pernr-pernr
      infty           = '0001'
      begda           = low-date
      endda           = high-date
    TABLES
      infty_tab       = p0001
    EXCEPTIONS
      infty_not_found = 1
      OTHERS          = 2.


  PERFORM process_pernr                                     "VKIK032705
    USING pernr-pernr pn-paper pn-permo payty payid bondt mc_incl
      pnpabkrs[] subrc.                                     "VKIK016676
* PERFORM COMPRESS_XFORM USING COMPRESS.    "not available!

  IF exp_frm EQ false.

*   ** Obtenção do email no Infotipo 0105
    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'          "HKUK002256
      EXCEPTIONS
        OTHERS = 1.

    REFRESH p0105. CLEAR p0105.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        tclas           = 'A'
        pernr           = pernr-pernr
        infty           = '0105'
        begda           = low-date
        endda           = high-date
      TABLES
        infty_tab       = p0105
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.

    rp-provide-from-last p0105 '9010' sy-datum sy-datum.

** Impressão para empregados sem mail ou por opção
    IF p0105-usrid_long IS INITIAL AND
      (  s_print = 'X' OR s_prmail = 'X' ).
* AMP@SAPi2: Fim de inserção 16.03.2009
      PERFORM print_xform USING xform[].
* AMP@SAPi2: Inserido 16.03.2009
      PERFORM show_progress USING 'Empregado nº: '
                            pernr-pernr
                            ' - a preparar impressão...'.

** Envio por email para os empregados c/ as opções
    ELSEIF NOT p0105-usrid_long IS INITIAL AND
     ( s_email = 'X' OR s_prmail = 'X' )   AND
           NOT xform[] IS INITIAL.
      CLEAR pme02.
      pme02-bukrs = p0001-bukrs.
      PERFORM re549d USING 'PENTT' space entty subrc.

      IF subrc = 0.
        LOOP AT it_sender_email WHERE entty = entty.
        ENDLOOP.
        IF sy-subrc = 0.
*   CMS@SBX - 03.07.2012: Carlos.sa@sbx.pt
          PERFORM get_xinfo USING xinfo.
          PERFORM envia_por_email TABLES xform[]
                                   USING xinfo
                                         p0105-usrid_long
                                         it_sender_email-fdata
                                         subrc.

        ENDIF.

      ELSE.
        subrc = 8.
      ENDIF.
      IF subrc <> 0.
        PERFORM print_xform USING xform[].
        PERFORM show_progress USING 'Empregado nº: '
                      pernr-pernr
                      ' - a preparar impressão...'.
      ELSE.
        PERFORM show_progress USING 'Empregado nº: '
                                    pernr-pernr
                                    ' - A enviar e-mail...'.
      ENDIF.

*---------------Início-03/03/21-DSIJMS-TK#98800---------------*
    ELSEIF s_files = abap_true AND pernr-stat2 = 3.

      DATA: lv_dir      TYPE eps2filnam,
            lv_filename TYPE string.
      DATA: it_objbin    LIKE solisti1 OCCURS 10 WITH HEADER LINE.

      FREE MEMORY ID 'PDSZ'.
      FREE MEMORY ID 'PDFT'.

      PERFORM get_xinfo USING xinfo.
      PERFORM convert_payslip_to_pdf TABLES xform[]
                                      USING xinfo.

      IMPORT pdf_table FROM MEMORY ID 'PDFT'. "Importação do conteúdo do PDF
      IMPORT pdf_fsize FROM MEMORY ID 'PDSZ'.

      IF pdf_table[] IS NOT INITIAL.

*---------------Início-28/09/21-DSIJMS-TK#100537---------------*
*      lv_dir = 'F:\usr\SAP\Temp'. "caminho do Application Server
        lv_dir = '/usr/sap/tmp'. "caminho do Application Server
*--------------- Fim-28/09/21-DSIJMS-TK#100537-----------------*

        APPEND pernr TO it_pernr.

        CONCATENATE pernr-pernr '.pdf' INTO lv_filename.

* Converte pdf_table em objecto a ser anexado
        CALL FUNCTION 'QCE1_CONVERT'
          TABLES
            t_source_tab         = pdf_table
            t_target_tab         = it_objbin
          EXCEPTIONS
            convert_not_possible = 1
            OTHERS               = 2.

*---------------Início-28/09/21-DSIJMS-TK#100537---------------*
*      CONCATENATE lv_dir '\' lv_filename INTO DATA(lv_file_pernr_pdf).
        CONCATENATE lv_dir '/' lv_filename INTO DATA(lv_file_pernr_pdf).
*--------------- Fim-28/09/21-DSIJMS-TK#100537-----------------*

        OPEN DATASET lv_file_pernr_pdf FOR OUTPUT IN BINARY MODE.
        LOOP AT it_objbin ASSIGNING FIELD-SYMBOL(<fs_zip_bin_attach>).
          TRANSFER <fs_zip_bin_attach> TO lv_file_pernr_pdf. "LENGTH pdf_fsize.
        ENDLOOP.

        CLOSE DATASET lv_file_pernr_pdf.
      ENDIF.

*--------------- Fim-03/03/21-DSIJMS-TK#98800-----------------*
    ELSE.
** Validação para proc. empregado seguinte
      CHECK p0105-usrid_long IS INITIAL AND
      (  s_print = 'X' OR s_prmail = 'X' ).
    ENDIF.
* AMP@SAPi2: Fim de inserção 16.03.2009
  ELSE.
    PERFORM export_form_to_memory USING exp_frm pernr-pernr 0 subrc.
  ENDIF.
*  KGM@SBX - fim de alteração

  IF exp_frm EQ false.
    PERFORM print_xform USING xform[].
    IF xform[] IS NOT INITIAL AND p_chkbox = abap_true.

*---------------Início-18/05/21-DSIJMS-TK#98800---------------*
      IF p_chkbox = abap_true.
* Converter para PDF

        DATA: pdf_table TYPE rcl_bag_tline.
        DATA: pdf_fsize TYPE  i.
*        DATA: it_objbin    LIKE solisti1 OCCURS 10 WITH HEADER LINE.
        DATA: lv_dir_server TYPE eps2filnam.
*              lv_filename   TYPE string.

        FREE MEMORY ID 'PDSZ'.
        FREE MEMORY ID 'PDFT'.

        PERFORM get_xinfo USING xinfo.
        PERFORM convert_payslip_to_pdf TABLES xform[]
                                        USING xinfo.

        IMPORT pdf_table FROM MEMORY ID 'PDFT'. "Importação do conteúdo do PDF
        IMPORT pdf_fsize FROM MEMORY ID 'PDSZ'.

*---------------Início-27/09/21-DSIJMS-TK#100537---------------*
*        lv_dir_server = 'F:\usr\SAP\Temp'. "caminho do Application Server
*
*        APPEND pernr TO it_pernr.
*
*        CONCATENATE pernr-pernr '.pdf' INTO lv_filename.
*
** Converte pdf_table em objecto a ser anexado
*        CALL FUNCTION 'QCE1_CONVERT'
*          TABLES
*            t_source_tab         = pdf_table
*            t_target_tab         = it_objbin
*          EXCEPTIONS
*            convert_not_possible = 1
*            OTHERS               = 2.
*
*        CONCATENATE lv_dir_server '\' lv_filename INTO DATA(lv_file_pernr_pdf).

        lv_dir_server = '/usr/sap/tmp'. "caminho do Application Server
        APPEND pernr TO it_pernr.

        CONCATENATE pernr-pernr '.pdf' INTO lv_filename.

* Converte pdf_table em objecto a ser anexado
        CALL FUNCTION 'QCE1_CONVERT'
          TABLES
            t_source_tab         = pdf_table
            t_target_tab         = it_objbin
          EXCEPTIONS
            convert_not_possible = 1
            OTHERS               = 2.

*        CONCATENATE lv_dir_server '/' lv_filename INTO DATA(lv_file_pernr_pdf).
        CONCATENATE lv_dir_server '/' lv_filename INTO lv_file_pernr_pdf.
*--------------- Fim-27/09/21-DSIJMS-TK#100537-----------------*

        OPEN DATASET lv_file_pernr_pdf FOR OUTPUT IN BINARY MODE.
        LOOP AT it_objbin ASSIGNING <fs_zip_bin_attach>.
          TRANSFER <fs_zip_bin_attach> TO lv_file_pernr_pdf. "LENGTH pdf_fsize.
        ENDLOOP.

        CLOSE DATASET lv_file_pernr_pdf.

      ENDIF.
    ENDIF.
*--------------- Fim-18/05/21-DSIJMS-TK#98800-----------------*
  ELSE.
    PERFORM export_form_to_memory USING exp_frm pernr-pernr 0 subrc.
  ENDIF.
  CALL FUNCTION 'HRPY_PROCESS_SET_PERNR_STATUS'
    EXPORTING
      imp_pernr       = pernr-pernr
*     IMP_PROCESSID   = STPROCID                     "WOGK056281
*     IMP_STEPID      = STSTEPID                         "!
*     IMP_RUNID       = STRUNID                          "!
*     IMP_PARAID      = STPARAID                         "!
      imp_parcel      = pyparaid "!
      imp_set_suc     = true
*     IMP_SET_PRE     =
*     IMP_SET_ERR     =
    EXCEPTIONS
      unknown_process = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

END-OF-SELECTION.
*---------------Início-18/05/21-DSIJMS-TK#98800---------------*
  IF p_chkbox = abap_true.

    DATA: lv_filename     TYPE string,
          lv_dir_server   TYPE eps2filnam,
*---------------Início-11/10/21-DSIJMS-TK#100537---------------*
*          lv_month        TYPE t247-ltx,
          lv_month        TYPE t015m-monam,
*--------------- Fim-11/10/21-DSIJMS-TK#100537-----------------*
          lv_date_lastday TYPE sy-datum,
          lv_date_payroll TYPE string,
          lv_month_year   TYPE string,
          lv_year         TYPE sy-datum,
          lv_pernr        TYPE string,
          lv_zip_content  TYPE xstring,
          lv_filenamezip  TYPE string.

    DATA : BEGIN OF it_file_csv OCCURS 0,
             rec(500),
           END OF it_file_csv.

    DATA: it_files          TYPE TABLE OF eps2fili.

    DATA: wa_pdf TYPE xstring.

    DATA: lo_zip_final TYPE REF TO cl_abap_zip.

*---------------Início-27/09/21-DSIJMS-TK#100537---------------*
*    lv_dir_server = 'F:\usr\SAP\Temp'. "caminho do Application Server
    lv_dir_server = '/usr/sap/tmp'. "caminho do Application Server
*--------------- Fim-27/09/21-DSIJMS-TK#100537-----------------*

*----------------------Monta EXCEL DIRDET----------------------*
    it_file_csv = '[OPERATOR],externalCode,cust_mypayslipschild.externalCode,cust_mypayslipschild.externalName,cust_mypayslipschild.cust_PayrollPeriod,cust_mypayslipschild.cust_attachment'.
    APPEND it_file_csv.
    it_file_csv = 'Supported Operators,mypayslipchild.externalCode,External Code,External Name,PayrollPeriod,Attachment'.
    APPEND it_file_csv.

*---------------Início-11/10/21-DSIJMS-TK#100537---------------*
*    CALL FUNCTION 'ISP_GET_MONTH_NAME'
*      EXPORTING
*        language     = sy-langu
*        month_number = pn-pabrp
*      IMPORTING
*        longtext     = lv_month
*      EXCEPTIONS
*        calendar_id  = 1
*        date_error   = 2
*        not_found    = 3
*        wrong_input  = 4
*        OTHERS       = 5.
    SELECT SINGLE monam
      FROM t015m
      WHERE spras = @sy-langu AND
      monum = @pn-pabrp
      INTO @lv_month.
*--------------- Fim-11/10/21-DSIJMS-TK#100537-----------------*

    CONCATENATE pn-pabrj pn-pabrp sy-datum+6(2) INTO lv_year.

    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lv_year
      IMPORTING
        last_day_of_month = lv_date_lastday
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.

    CONCATENATE lv_date_lastday+4(2) '/' lv_date_lastday+6(2) '/' lv_date_lastday(4) INTO lv_date_payroll.
    CONCATENATE lv_month '/' pn-pabrj INTO lv_month_year SEPARATED BY space.

    LOOP AT it_pernr ASSIGNING FIELD-SYMBOL(<fs_pernr>).
      CONCATENATE <fs_pernr>-pernr  pn-pabrp pn-pabrj '_EX' INTO DATA(lv_pernrmesano).
      IF formular = 'ZAOU'.
        CONCATENATE space ',' <fs_pernr>-pernr  ',' lv_pernrmesano ',' lv_month_year ',' lv_date_payroll ',' pn-pabrp pn-pabrj '_' <fs_pernr>-pernr '_' 'EXEUR' '.pdf' INTO it_file_csv.
      ELSEIF formular = 'ZAOE'.
        CONCATENATE space ',' <fs_pernr>-pernr  ',' lv_pernrmesano ',' lv_month_year ',' lv_date_payroll ',' pn-pabrp pn-pabrj '_' <fs_pernr>-pernr '_' 'EXAOA' '.pdf' INTO it_file_csv.
      ELSE.
        CONCATENATE space ',' <fs_pernr>-pernr  ',' lv_pernrmesano ',' lv_month_year ',' lv_date_payroll ',' pn-pabrp pn-pabrj '_' <fs_pernr>-pernr '.pdf' INTO it_file_csv.
      ENDIF.
*      CONCATENATE space ',' <fs_pernr>-pernr  ',' lv_pernrmesano ',' lv_month_year ',' lv_date_payroll ',' pn-pabrp '_' pn-pabrj '_' <fs_pernr>-pernr '.pdf' INTO it_file_csv.
      APPEND it_file_csv.
    ENDLOOP.

*---------------Início-27/09/21-DSIJMS-TK#100537---------------*
*    CONCATENATE lv_dir_server '\DIR-DIRDET.csv' INTO DATA(lv_filename_dir_csv).
    CONCATENATE lv_dir_server '/DIR-DIRDET.csv' INTO DATA(lv_filename_dir_csv).
*--------------- Fim-27/09/21-DSIJMS-TK#100537-----------------*
    OPEN DATASET lv_filename_dir_csv FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    LOOP AT it_file_csv ASSIGNING FIELD-SYMBOL(<fs_file_csv>).
      TRANSFER <fs_file_csv> TO lv_filename_dir_csv.
    ENDLOOP.
    CLOSE DATASET lv_filename_dir_csv.
*----------------------Monta EXCEL DIRDET----------------------*

*----------------------Monta EXCEL Import----------------------*
    REFRESH it_file_csv.
    it_file_csv = 'File Name,Object Type,Import Order,Path'.
    APPEND it_file_csv.

    IF sy-sysid = 'PRD'.
      it_file_csv = 'My+Paylips-My+Paylips+Child_casaisenge.csv,cust_mypayslips,1,cust_mypayslips-cust_mypayslipschild'.
    ELSE.
      it_file_csv = 'My+Paylips-My+Paylips+Child_casaisengeT1.csv,cust_mypayslips,1,cust_mypayslips-cust_mypayslipschild'.
    ENDIF.
*    it_file_csv = 'My+Paylips-My+Paylips+Child_casaisengD.csv,cust_mypayslips,1,cust_mypayslips-cust_mypayslipschild'.
    APPEND it_file_csv.

*---------------Início-27/09/21-DSIJMS-TK#100537---------------*
*    CONCATENATE lv_dir_server '\import_sequence.csv' INTO DATA(lv_filename_import_csv).
    CONCATENATE lv_dir_server '/import_sequence.csv' INTO DATA(lv_filename_import_csv).
*--------------- Fim-27/09/21-DSIJMS-TK#100537-----------------*
    OPEN DATASET lv_filename_import_csv FOR OUTPUT IN TEXT MODE ENCODING DEFAULT."BINARY MODE.
    LOOP AT it_file_csv ASSIGNING <fs_file_csv>.
      TRANSFER <fs_file_csv> TO lv_filename_import_csv. "LENGTH pdf_fsize.
    ENDLOOP.
    CLOSE DATASET lv_filename_import_csv.
*----------------------Monta EXCEL Import----------------------*

    CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
      EXPORTING
        iv_dir_name            = lv_dir_server
      TABLES
        dir_list               = it_files
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.

    CREATE OBJECT lo_zip_final.

    LOOP AT it_files ASSIGNING FIELD-SYMBOL(<fs_files>).
      SPLIT <fs_files>-name AT '.' INTO DATA(lv_name) DATA(lv_file_format).

      READ TABLE it_pernr INTO DATA(wa_pernr) WITH KEY pernr = lv_name.
      IF sy-subrc = 0.
        lv_pernr = wa_pernr-pernr.
      ELSE.
        CLEAR wa_pernr.
      ENDIF.

*      IF lv_name = 'import' AND ( sy-sysid = 'DEV' OR sy-sysid = 'QLT' ).
*      IF lv_name = 'import' AND sy-sysid NE 'PRD'.
      IF lv_name = 'import_properties_TST' AND sy-sysid NE 'PRD'.
        CLEAR wa_pdf.

*        lv_filename = <fs_files>-name.
        lv_filename = 'import.properties'.

*---------------Início-27/09/21-DSIJMS-TK#100537---------------*
*        CONCATENATE lv_dir_server '\' <fs_files>-name INTO DATA(lv_file_path).
        CONCATENATE lv_dir_server '/' <fs_files>-name INTO DATA(lv_file_path).
*--------------- Fim-27/09/21-DSIJMS-TK#100537-----------------*

        OPEN DATASET lv_file_path FOR INPUT IN BINARY MODE.
        IF sy-subrc EQ 0.
          READ DATASET lv_file_path INTO wa_pdf.

          CALL METHOD lo_zip_final->add
            EXPORTING
              name    = lv_filename
              content = wa_pdf.

          CLOSE DATASET lv_file_path.

        ENDIF.

      ELSEIF lv_name = 'import_properties_PRD' AND sy-sysid = 'PRD'.
        CLEAR wa_pdf.

*        lv_filename = <fs_files>-name.
        lv_filename = 'import.properties'.

*---------------Início-27/09/21-DSIJMS-TK#100537---------------*
*        CONCATENATE lv_dir_server '\' <fs_files>-name INTO DATA(lv_file_path_prd).
        CONCATENATE lv_dir_server '/' <fs_files>-name INTO DATA(lv_file_path_prd).
*--------------- Fim-27/09/21-DSIJMS-TK#100537-----------------*

        OPEN DATASET lv_file_path_prd FOR INPUT IN BINARY MODE.
        IF sy-subrc EQ 0.
          READ DATASET lv_file_path_prd INTO wa_pdf.

          CALL METHOD lo_zip_final->add
            EXPORTING
              name    = lv_filename
              content = wa_pdf.

          CLOSE DATASET lv_file_path_prd.

        ENDIF.

      ELSEIF lv_name = lv_pernr.
        CLEAR wa_pdf.

        lv_filename = <fs_files>-name.

*---------------Início-27/09/21-DSIJMS-TK#100537---------------*
*        CONCATENATE lv_dir_server '\' <fs_files>-name INTO DATA(lv_folder_attach).
        CONCATENATE lv_dir_server '/' <fs_files>-name INTO DATA(lv_folder_attach).
*--------------- Fim-27/09/21-DSIJMS-TK#100537-----------------*

        OPEN DATASET lv_folder_attach FOR INPUT IN BINARY MODE.

        IF sy-subrc EQ 0.
          READ DATASET lv_folder_attach INTO wa_pdf.

          IF formular = 'ZAOU'.
            CONCATENATE 'attachments/' pn-pabrp pn-pabrj '_' lv_pernr '_EXEUR' '.pdf' INTO DATA(lv_folder).

          ELSEIF formular = 'ZAOE'.
            CONCATENATE 'attachments/' pn-pabrp pn-pabrj '_' lv_pernr '_EXAOA' '.pdf' INTO lv_folder.

          ELSE.
            CONCATENATE 'attachments/' pn-pabrp pn-pabrj '_' lv_pernr '.pdf' INTO lv_folder.
          ENDIF.

          CALL METHOD lo_zip_final->add
            EXPORTING
              name    = lv_folder
              content = wa_pdf.
          CLOSE DATASET lv_folder_attach.

        ENDIF.

        DELETE DATASET lv_folder_attach.

      ELSEIF lv_name = 'DIR-DIRDET'.
        lv_filename = <fs_files>-name.
*---------------Início-27/09/21-DSIJMS-TK#100537---------------*
*        CONCATENATE lv_dir_server '\' <fs_files>-name INTO DATA(lv_csv_path).
        CONCATENATE lv_dir_server '/' <fs_files>-name INTO DATA(lv_csv_path).
*--------------- Fim-27/09/21-DSIJMS-TK#100537-----------------*
        OPEN DATASET lv_csv_path FOR INPUT IN BINARY MODE.
        IF sy-subrc EQ 0.
          READ DATASET lv_csv_path INTO wa_pdf.

          CLEAR lv_filename.
          "Verificar ambientes - PRD = casaiseng
          IF sy-sysid = 'PRD'.
*          lv_filename = 'My+Paylips-My+Paylips+Child_casaisengD.csv'.
            lv_filename = 'My+Paylips-My+Paylips+Child_casaisenge.csv'.
          ELSE.
            lv_filename = 'My+Paylips-My+Paylips+Child_casaisengeT1.csv'.
          ENDIF.

          CALL METHOD lo_zip_final->add
            EXPORTING
              name    = lv_filename
              content = wa_pdf.
          CLOSE DATASET lv_csv_path.

        ENDIF.
        DELETE DATASET lv_csv_path.

      ELSEIF lv_name = 'import_sequence'.
        lv_filename = <fs_files>-name.
*---------------Início-27/09/21-DSIJMS-TK#100537---------------*
*        CONCATENATE lv_dir_server '\' <fs_files>-name INTO DATA(lv_sequence_path).
        CONCATENATE lv_dir_server '/' <fs_files>-name INTO DATA(lv_sequence_path).
*--------------- Fim-27/09/21-DSIJMS-TK#100537-----------------*
        OPEN DATASET lv_sequence_path FOR INPUT IN BINARY MODE.

        IF sy-subrc EQ 0.
          READ DATASET lv_sequence_path INTO wa_pdf.

          CALL METHOD lo_zip_final->add
            EXPORTING
              name    = lv_filename
              content = wa_pdf.
          CLOSE DATASET lv_sequence_path.

        ENDIF.
        DELETE DATASET lv_sequence_path.

      ENDIF.

    ENDLOOP.

    CALL METHOD lo_zip_final->save "Salva todos os Ficheiros em um ZIP
      RECEIVING
        zip = lv_zip_content.

    IF formular = 'ZAOU'.
      CONCATENATE 'casaisslipangolaexpeur_' sy-datum+6(2) sy-datum+4(2) sy-datum(4) '.zip' INTO lv_filenamezip.

    ELSEIF formular = 'ZAOE'.
      CONCATENATE 'casaisslipangolaexpaoa_' sy-datum+6(2) sy-datum+4(2) sy-datum(4) '.zip' INTO lv_filenamezip.

    ELSE.
      CONCATENATE 'casaisslipangola_' sy-datum+6(2) sy-datum+4(2) sy-datum(4) '.zip' INTO lv_filenamezip.
    ENDIF.

*-----------------Envia Ficheiro .ZIP para o SFTP----------------------*
    DATA: lo_http_client      TYPE REF TO if_http_client,
          lo_rest_client      TYPE REF TO if_rest_client,
          lo_rest_http_client TYPE REF TO cl_rest_http_client.

    DATA: lv_url_dest(25) TYPE c.  "VALUE 'ZSUCCESSFACTORS_SFTP'.

    IF sy-sysid = 'PRD'.
      lv_url_dest = 'Z_SUCCESSFACTORS_SFTP_PRD'.
    ELSE.
      lv_url_dest = 'Z_SUCCESSFACTORS_SFTP_TST'.
    ENDIF.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = lv_url_dest
      IMPORTING
        client                   = lo_http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6.

    CREATE OBJECT lo_rest_http_client
      EXPORTING
        io_http_client = lo_http_client.

    lo_rest_client = CAST if_rest_client( lo_rest_http_client ).

*  Create a request entity to upload data
    DATA(lo_rest_entity) = lo_rest_client->create_request_entity( iv_multipart = abap_true ).

    lo_rest_client->set_request_header(
      iv_name  = if_http_header_fields=>content_type
      iv_value = 'multipart/form-data' ).

*    Create the multi part form data
    DATA(lo_rest_data) = NEW cl_rest_multipart_form_data( ).

    lo_rest_data->set_file( iv_name = lv_filenamezip
                            iv_filename = lv_filenamezip
                            iv_type = 'application/zip'
                            iv_data = lv_zip_content ).

    lo_rest_data->write_to( lo_rest_entity ).

*    Post the entity to create the document
    TRY.
        lo_rest_client->post( lo_rest_entity ).
      CATCH cx_rest_client_exception cx_root.
        RETURN.
    ENDTRY.

  ENDIF.
*--------------- Fim-21/05/21-DSIJMS-TK#98800-----------------*

  stat-pagno = stat-pagno - 1.         "um Initialwert vermindern
  PERFORM add_protocol.
* IF STATIST NE SPACE.                                     "VKIK083918
  IF prt_prot NE space.                                     "VKIK083918
    NEW-PAGE.
    CALL FUNCTION 'HR_DISPLAY_ERROR_LIST'
      EXPORTING
        no_popup         = 'X'
        no_print         = ' '
        no_img           = 'X'
        linesize         = sy-linsz
*        TABLES
*       ERROR            =
      EXCEPTIONS
        invalid_linesize = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
* CALL FUNCTION 'HRPY_PROCESS_FIRE_EVENT'                  "WOGK056281
*      EXPORTING                                                "!
*           IMP_PROCESSID  = STPROCID                           "!
*           IMP_STEPID     = STSTEPID                           "!
*           IMP_RUNID      = STRUNID                            "!
*           IMP_PARAID     = STPARAID                           "!
*           IMP_EVENT_NAME = 'JOB_ENDED'
*      TABLES
*           IMP_CONTAINER  =  .

*-----------------------------------------------------------------------
*  Modul-Pool
*-----------------------------------------------------------------------
FORM init_parameters USING rc LIKE sy-subrc.
  CLEAR rc.
  formular = 'XF01'.
  andruck  = 'A '.
  rueckd   = true.
  rueckr   = 'J'.
  sort_rr  = '1'.
  sprache  = 'B'.
  only     = false.
  anz-test = '0'.
  exp_frm  = false.
  cur_fp = true.                                            "VKIK043489
  cur_ip = false.                                           "VKIK043489
  cur_alt = false.                                          "VKIK043489
  CLEAR cur_val.                                            "VKIK043489
  chk_ess = false.                                          "VKIK023668
  prt_prot = true.                                          "VKIK083918
  PERFORM init_parameters-natio.
  PERFORM init_parameters-mod.                              "VKIK017443
  PERFORM re514v USING sy-langu molga formular form_txt.    "VKIK009593
ENDFORM.

*---------------------------------------------------------------------*
*       FORM INIT_INITIALIZATION                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM init_initialization.
  repid = sy-repid.                                         "VKIK065961
* STATIST = TRUE.                                           "VKIK083918
  PERFORM init-natio.
  PERFORM init-mod.                                         "VKIK017443
ENDFORM.

*---------------------------------------------------------------------*
*       FORM GET_PARAMETERS                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PARM_EDT                                                      *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM get_parameters USING parm_edt STRUCTURE xcedt
                          rc LIKE sy-subrc.
  CLEAR rc.
  parm_edt-prt_form = formular.
  parm_edt-prt_aper = andruck.
  parm_edt-prt_rcal = rueckd.
  parm_edt-typ_rcal = rueckr.
  parm_edt-srt_rcal = sort_rr.
  parm_edt-prt_lang = sprache.
  parm_edt-prt_line = only.
  parm_edt-cnt_test = anz-test.
  parm_edt-exp_form = exp_frm.
  parm_edt-cur_fp   = cur_fp.                               "VKIK043489
  parm_edt-cur_ip   = cur_ip.                               "VKIK043489
  parm_edt-cur_alt  = cur_alt.                              "VKIK043489
  parm_edt-cur_val  = cur_val.                              "VKIK043489
  parm_edt-chk_ess  = chk_ess.                              "VKIK023668
  parm_edt-prt_prot = prt_prot.                             "VKIK083918
ENDFORM.

*---------------------------------------------------------------------*
*       FORM INIT_SOS                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM init_sos USING rc.
  DATA:                                                     "VKIK056231
    repid     LIKE sy-repid.
  CLEAR rc.
* RP-SET-NAME-FORMAT.                  "ev. Format fuer RP_EDIT_NAME
  repid = sy-repid.                                         "VKIK056231
  CALL FUNCTION 'RP_SET_NAME_FORMAT'                        "VKIK056231
    EXPORTING
      repid  = repid
    IMPORTING
      format = nfrmt.
  ldate = low-date.
  hdate = high-date.
  dedu_beg = low-date.                                      "HKUK058239
  dedu_end = high-date.                                     "HKUK058239
  sprkz = sy-langu.  "Default-Sprache = Sprache des Sachbearbeiter.
  sprkz-w = sy-langu. "Default-Sprache Werk = Sprache des Sachbearb.
  PERFORM set_pn_fields USING payty bondt.                  "VKIK071395
  PERFORM init_sos-natio USING rc.
  PERFORM init_sos-mod USING rc.                            "VKIK017443
ENDFORM.

*---------------------------------------------------------------------*
*       FORM INIT_FORM                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(XCEDT)                                                  *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM init_form USING VALUE(xcedt) STRUCTURE xcedt
                     rc LIKE sy-subrc.
  PERFORM set_currency USING xcedt rc.                      "VKIK047993
  PERFORM init_formdef.
  PERFORM read_form USING molga xcedt-prt_form rc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHECK_PARAMETER                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(XCEDT)                                                  *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM check_parameter USING VALUE(xcedt) STRUCTURE xcedt
                           rc LIKE sy-subrc.
  DATA:
    fehler    LIKE boolean.
  CLEAR rc.
  fehler = false.
  SELECT SINGLE * FROM t514d           "check form directory
    WHERE molga EQ molga
      AND forml EQ xcedt-prt_form.
  IF sy-subrc NE 0.                    "Form didn't exist
    CALL FUNCTION 'HR_APPEND_ERROR_LIST'
      EXPORTING
        arbgb = '3R'
        msgty = c_msgty-error
        msgno = 303
        msgv1 = xcedt-prt_form.
    fehler = true.
  ENDIF.
  IF xcedt-cnt_test CN '0123456789'.   "Number of test forms
    CALL FUNCTION 'HR_APPEND_ERROR_LIST'
      EXPORTING
        arbgb = '3R'
        msgty = c_msgty-error
        msgno = 304
        msgv1 = xcedt-cnt_test.
    fehler = true.
  ENDIF.
  IF ( xcedt-prt_aper+0(1) NE 'A'      "Print actual period (1)
  AND  xcedt-prt_aper+0(1) NE ' '                           "VKIK023737
  AND  xcedt-prt_aper+0(1) NE 'D' )
  OR ( xcedt-prt_aper+1(1) NE ' '      "Print actual period (2)
  AND  xcedt-prt_aper+1(1) NE 'F'                           "VKIK032558
  AND  xcedt-prt_aper+1(1) NE 'Z' ).                        "VKIK032558
    CALL FUNCTION 'HR_APPEND_ERROR_LIST'
      EXPORTING
        arbgb = '3R'
        msgty = c_msgty-error
        msgno = 305
        msgv1 = xcedt-prt_aper.
    fehler = true.
  ENDIF.
  IF  xcedt-prt_rcal NE 'L'            "Print recalculations
  AND xcedt-prt_rcal NE false
  AND xcedt-prt_rcal NE true.
    CALL FUNCTION 'HR_APPEND_ERROR_LIST'
      EXPORTING
        arbgb = '3R'
        msgty = c_msgty-error
        msgno = 306
        msgv1 = xcedt-prt_rcal.
    fehler = true.
  ENDIF.
  IF  xcedt-typ_rcal NE 'A'            "How to print recalculations
  AND xcedt-typ_rcal NE 'D'
  AND xcedt-typ_rcal NE 'J'
  AND xcedt-typ_rcal NE 'S'.
    CALL FUNCTION 'HR_APPEND_ERROR_LIST'
      EXPORTING
        arbgb = '3R'
        msgty = c_msgty-error
        msgno = 307
        msgv1 = xcedt-typ_rcal.
    fehler = true.
  ENDIF.
  IF  xcedt-srt_rcal NE '1'            "How to sort recalculations
  AND xcedt-srt_rcal NE '2'
  AND xcedt-srt_rcal NE '3'                                 "XRRK036865
  AND xcedt-srt_rcal NE 'Z'.                                "XRRK036865
    CALL FUNCTION 'HR_APPEND_ERROR_LIST'
      EXPORTING
        arbgb = '3R'
        msgty = c_msgty-error
        msgno = 308
        msgv1 = xcedt-srt_rcal.
    fehler = true.
  ENDIF.
  IF  xcedt-prt_lang NE 'A'            "Language for printing
  AND xcedt-prt_lang NE 'B'
  AND xcedt-prt_lang NE 'W'.
    CALL FUNCTION 'HR_APPEND_ERROR_LIST'
      EXPORTING
        arbgb = '3R'
        msgty = c_msgty-error
        msgno = 309
        msgv1 = xcedt-prt_lang.
    fehler = true.
  ENDIF.
  IF  xcedt-prt_line NE true           "Print superlines
  AND xcedt-prt_line NE false.
    CALL FUNCTION 'HR_APPEND_ERROR_LIST'
      EXPORTING
        arbgb = '3R'
        msgty = c_msgty-error
        msgno = 310
        msgv1 = xcedt-prt_line.
    fehler = true.
  ENDIF.
  IF  xcedt-cur_fp NE true             "using currency     "VKIK043489
  AND xcedt-cur_ip NE true
  AND xcedt-cur_alt NE true.
    CALL FUNCTION 'HR_APPEND_ERROR_LIST'
      EXPORTING
        arbgb = '3R'
        msgty = c_msgty-error
        msgno = 311.
    fehler = true.
  ENDIF.
  IF  xcedt-cur_alt EQ true.           "using alt. currency"VKIK043489
    SELECT SINGLE * FROM tcurc
      WHERE waers EQ xcedt-cur_val.
    IF sy-subrc NE 0.
      CALL FUNCTION 'HR_APPEND_ERROR_LIST'
        EXPORTING
          arbgb = '3R'
          msgty = c_msgty-error
          msgno = 312
          msgv1 = xcedt-prt_rcal.
      fehler = true.
    ENDIF.
  ENDIF.
  IF  xcedt-chk_ess NE true            "activate ESS-check?  "VKI023668
  AND xcedt-chk_ess NE false.
    CALL FUNCTION 'HR_APPEND_ERROR_LIST'
      EXPORTING
        arbgb = '3R'
        msgty = c_msgty-error
        msgno = 321
        msgv1 = xcedt-chk_ess.
    fehler = true.
  ENDIF.
  IF  xcedt-prt_prot NE true           "print protocol?     "VKIK083918
  AND xcedt-prt_prot NE false.
    CALL FUNCTION 'HR_APPEND_ERROR_LIST'
      EXPORTING
        arbgb = '3R'
        msgty = c_msgty-error
        msgno = 322
        msgv1 = xcedt-prt_prot.
    fehler = true.
  ENDIF.
  IF fehler = true.
    rc = 12.
  ENDIF.
ENDFORM.                               "CHECK_PARAMETER.

*---------------------------------------------------------------------*
*       FORM SET_PN_FIELDS                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($PAYTY)                                                 *
*  -->  VALUE($BONDT)                                                 *
*---------------------------------------------------------------------*
FORM set_pn_fields USING VALUE($payty) VALUE($bondt).       "VKIK071395
  CASE $payty.
    WHEN cd_c-regular.                                      "VKIK032705
*     do nothing special - all fields should be correct     "VKIK032705
    WHEN cd_c-bonus.
      pn-begda = pn-endda = pn-begps = pn-endps = $bondt.
      CLEAR: pn-paper, pn-pabrj, pn-pabrp.
    WHEN cd_c-correct.
      pn-begda = pn-endda = pn-begps = pn-endps = $bondt.
      CLEAR: pn-paper, pn-pabrj, pn-pabrp.
    WHEN cd_c-non_auth.                                     "VKIK056067
      pn-begda = pn-endda = pn-begps = pn-endps = $bondt.
      CLEAR: pn-paper, pn-pabrj, pn-pabrp.
    WHEN cd_c-supplemental.                                 "VKIK032705
      pn-begda = pn-endda = pn-begps = pn-endps = $bondt.
      CLEAR: pn-paper, pn-pabrj, pn-pabrp.
  ENDCASE.
ENDFORM.                               "(SET_PN_FIELDS)     "VKIK071395

*---------------------------------------------------------------------*
*       FORM SET_CURRENCY                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($XCEDT)                                                 *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM set_currency USING VALUE($xcedt) STRUCTURE xcedt       "VKIK047993
                        rc LIKE sy-subrc.
  IF $xcedt-cur_alt EQ true.           "use alter. curr.   "VKIK043489
    ip_curr = $xcedt-cur_val.                               "VKIK043489
  ELSEIF $xcedt-cur_fp EQ true.                             "use standard curr. "VKIK043489
    IF versc-waers IS INITIAL.
      CALL FUNCTION 'RP_GET_CURRENCY'                         "VKIK041980
        EXPORTING
          molga                         = molga
          begda                         = pn-begda
          endda                         = pn-endda
        IMPORTING
          waers                         = ip_curr
        EXCEPTIONS
          molga_not_in_t001p            = 1
          no_entry_found_in_table_t001  = 2
          no_entry_found_in_table_t500p = 3
          no_entry_found_in_table_t500c = 4
          OTHERS                        = 5.
      IF sy-subrc NE 0.
        rc = sy-subrc.
        CALL FUNCTION 'HR_APPEND_ERROR_LIST'
          EXPORTING
            arbgb = sy-msgid
            msgty = sy-msgty
            msgno = sy-msgno
            msgv1 = sy-msgv1
            msgv2 = sy-msgv2
            msgv3 = sy-msgv3
            msgv4 = sy-msgv4.
      ENDIF.
    ELSE.
      ip_curr = versc-waers.
    ENDIF.
  ENDIF.                                                    "VKIK043489
ENDFORM.                                                    "VKIK047993

*-----------------------------------------------------------------------
* Diese Routine stellt Testdaten bereit fuer Probedrucke
*-----------------------------------------------------------------------
FORM write_testforms                                        "HKUK041440
  USING xcedt STRUCTURE xcedt.

  PERFORM create_testform USING xcedt.
  PERFORM print_xform USING xform[].
ENDFORM.                                                    "HKUK041440

*---------------------------------------------------------------------*
*       FORM CREATE-TESTFORM                                          *
*---------------------------------------------------------------------*
*   Create test data and put them into global table xform
*---------------------------------------------------------------------*
FORM create_testform                                        "HKUK041440
  USING xcedt STRUCTURE xcedt.

  DATA:
    $prt_aper LIKE xcedt-prt_aper,
    cnt_test  TYPE i.

  CHECK xcedt-cnt_test NE '0'.
* initialize everything
  PERFORM refresh_pernr.
  PERFORM refresh_pernr_natio.
  PERFORM refresh_pernr_mod.
  $prt_aper = xcedt-prt_aper.
  xcedt-prt_aper = 'A '.
  cnt_test = xcedt-cnt_test.
* create XCEDT-CNT_TEST forms in table XFORM
  WHILE cnt_test GE 1.
    PERFORM get-test-data.
    PERFORM get-test-data-natio.
    PERFORM get-test-data-mod.
    PERFORM copy-rt-to-rt_2 USING pn-begda pn-endda.
    PERFORM create_form USING false.
    cnt_test = cnt_test - 1.
    REFRESH: crt, rt, rt_2.
    PERFORM refresh_period.
    PERFORM refresh_period_natio.
    PERFORM refresh_period_mod.
  ENDWHILE.
* reset everything
  xcedt-prt_aper = $prt_aper.          "Restore old value
  CLEAR: stat, rx-key.
  stat-pagno = 1.                      "initial value for test forms
  xcedt-cnt_test = '0'.
ENDFORM.                                                    "HKUK041440

*---------------------------------------------------------------------*
*       FORM GET-TEST-DATA                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get-test-data.
  rt_2-lgart = '/101'.
  rt_2-anzhl = 12345.
  rt_2-betpe = 12345.
  rt_2-betrg = 1234567.
  COLLECT rt_2.
  crt-lgart = '/101'.
  crt-anzhl = 12345.
  crt-betrg = 1234567.
  COLLECT crt.
  rx-key-pernr = '12345678'.
  rueck = 'R'.
ENDFORM.                               "GET-TEST-DATA.

*---------------------------------------------------------------------*
*       FORM MODIFY_INFOTYPES                                         *
*---------------------------------------------------------------------*
*       activates infotypes which are needed by form                  *
*---------------------------------------------------------------------*
*  -->  $RINFO$     list of defined infotypes                         *
*---------------------------------------------------------------------*
FORM modify_infotypes USING infty TYPE hrift_tt_lst.
  DATA:
    wa        LIKE rsitypelst.
  wa-mode = 'N'.                       "default for all
  MODIFY infty FROM wa TRANSPORTING mode                    "VKIK023587
    WHERE mode NE wa-mode.
  wa-mode = 'Y'.                       "special mode for needed infotype
  MODIFY infty FROM wa TRANSPORTING mode                    "VKIK023587
    WHERE number EQ '0000'
       OR number EQ '0001'
       OR number EQ '0002'
       OR number EQ '0006'
       OR number EQ '0007'
       OR number EQ '0008'                                  "VKIK040627
       OR number EQ '0016'
       OR number EQ '0032'
* RPS08112023->Begin
       OR number EQ '0034'
* RPS08112023->End
       OR number EQ '0041'
       OR number EQ '0128'.
*       OR NUMBER EQ '0709'.              "HKUK096053 deact.XULKK039599
  PERFORM modify_infotypes_natio USING infty.
  PERFORM modify_infotypes_mod   USING infty.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM PROCESS_PERNR                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($PERNR)                                                 *
*  -->  VALUE($PAPER)                                                 *
*  -->  VALUE($PERMO)                                                 *
*  -->  VALUE($PAYTY)                                                 *
*  -->  VALUE($PAYID)                                                 *
*  -->  VALUE($BONDT)                                                 *
*  -->  $RC                                                           *
*---------------------------------------------------------------------*
FORM process_pernr USING VALUE($pernr) LIKE pernr-pernr
                         VALUE($paper)
                         VALUE($permo)
                         VALUE($payty)
                         VALUE($payid)
                         VALUE($bondt)
                         VALUE($mc_incl)                    "VKIK032705
                         VALUE($abkrs) TYPE hrpy_tt_abkrs   "VKIK016676
                         $rc LIKE sy-subrc.
* DEF_RGDIR EVP.                                       "WOG note 760945
  DATA: eval_results    TYPE pay_t_eval_period.
  DATA: ls_evp LIKE evp,
        rc     LIKE sy-subrc.
  FIELD-SYMBOLS <wa_eval_results> TYPE pay_eval_period.

  stat-selpn = stat-selpn + 1.
*  CLEAR rt_2.                                         "WOG note 760945
*  REFRESH rt_2.
*  perform refresh_inw_buffer.             "WXJK014138 "DEL note 370330
  PERFORM refresh_pernr.
  PERFORM refresh_pernr_natio.
  PERFORM refresh_pernr_mod.
* PERFORM CHECK_PERNR                                      "VKIK016676
*   USING $PERNR $PAPER $PERMO $PAYTY $PAYID $BONDT $ABKRS[] $RC.
  PERFORM import_cd USING $pernr rgdir[] $rc.               "VKIK000738
  IF xcedt-cur_ip IS NOT INITIAL. "To Handle Inperiod Currency Issue
    READ TABLE rgdir INTO ls_evp WITH KEY inper = $paper
                                          fpper = $paper.
    IF sy-subrc = 0.
      PERFORM import_actual USING ls_evp rc.
      ip_curr = versc-waers.
    ENDIF.
  ENDIF.
  IF $rc EQ 0.                         "payroll result ?
    IF $payty IS INITIAL.                                   "KVHN1424442
      READ TABLE rgdir WITH KEY
               fpper = $paper inper = $paper iperm = $permo."KVHN1424442
      IF sy-subrc <> 0.                                     "KVHN1424442
        READ TABLE rgdir WITH KEY
                 fpper = $paper inper = $paper.             "KVHN1424442
        $permo = rgdir-iperm.                               "KVHN1424442
        CLEAR rgdir.                                        "KVHN1424442
      ENDIF.                                                "KVHN1424442
    ENDIF.                                                  "KVHN1424442
    PERFORM fill_evp
      TABLES eval_results                             "WOG note 760945
             $abkrs                                         "VKIK016676
      USING $paper $permo $payty $payid $bondt $mc_incl $rc. "VKIK032705

    IF $rc EQ 0.                                               " Note 1330746
      PERFORM fill_evp_natio IN PROGRAM (repid) IF FOUND
                             TABLES   rgdir eval_results
                              USING    $pernr
                             CHANGING $rc.
    ENDIF.                                                      " Note 1330746

  ENDIF.                                                    "VKIK000738
* no payroll directory OR no relevant payroll result available?
  IF $rc NE 0.                       "no payroll result?   "VKIK023668
    PERFORM fill_evp_ll
      TABLES eval_results                             "WOG note 760945
      USING $rc.
  ENDIF.
* check infotype 655 if ESS only? If yes AND switch on sel.screen is
* set - skip pernr during batch, ...
  IF $rc EQ 0.                                              "VKIK000738
    LOOP AT eval_results ASSIGNING <wa_eval_results>. "WOG note 760945
      PERFORM check_ess USING <wa_eval_results>-evp[] xcedt-chk_ess $rc.
      IF $rc EQ 12.                    "ess -> no print    "WLIK023668
        stat-skip_ess = stat-skip_ess + 1.                  "WLIK023668
*       If the ee can see one result in ess he can see all
        EXIT.                                         "WOG note 760945
      ENDIF.                                                "VKIK023668
    ENDLOOP.
  ENDIF.
  IF $rc EQ 0.
    PERFORM fill_per_pernr.                                 "VKIK031819
    LOOP AT eval_results ASSIGNING <wa_eval_results>. "WOG note 760945
      CLEAR rt_2. REFRESH rt_2.                       "WOG note 760945
*     pagpn = 1.                                      "WOG note 760945
      PERFORM process_evp TABLES <wa_eval_results>-evp[].    "!
    ENDLOOP.                                                 "!
  ENDIF.
ENDFORM.                               "PROCESS_PERNR

*---------------------------------------------------------------------*
*       FORM CHECK_PERNR                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(PERNR)                                                  *
*  -->  VALUE(INPER)                                                  *
*  -->  VALUE(IPERM)                                                  *
*  -->  VALUE(PAYTY)                                                  *
*  -->  VALUE(PAYID)                                                  *
*  -->  VALUE(BONDT)                                                  *
*  -->  $RC                                                           *
*---------------------------------------------------------------------*
FORM check_pernr
  USING VALUE(pernr) VALUE(inper) VALUE(iperm) VALUE(payty)
    VALUE(payid) VALUE(bondt)
    VALUE(iabkrs) TYPE hrpy_tt_abkrs $rc LIKE sy-subrc.     "VKIK016676
  cd-key-pernr = pernr.
  rp-imp-c2-cu.
  CALL FUNCTION 'CD_CHECK_PAYROLL_RESULT'
    EXPORTING
      inper           = inper
      inper_modif     = iperm
      pay_ident       = payid
      pay_type        = payty
      bonus_date      = bondt
      iabkrs          = iabkrs
    TABLES
      rgdir           = rgdir
    EXCEPTIONS
      no_record_found = 01.
  $rc = sy-subrc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM import_CD                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(PERNR)                                                  *
*  -->  rgdir                                                         *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM import_cd USING VALUE(pernr) LIKE pernr-pernr          "VKIK000738
                     rgdir TYPE tt_rgdir
                     rc LIKE sy-subrc.
  cd-key-pernr = pernr.
  rp-imp-c2-cu.
  rc = rp-imp-cd-subrc.
ENDFORM.

FORM check_ess USING VALUE($evp) TYPE tt_rgdir              "VKIK023668
                     VALUE($chk_ess) TYPE boolean
                     rc LIKE sy-subrc.
  DATA:
    aresults TYPE STANDARD TABLE OF pc261,
    is_retro TYPE boolean,
    ess_only TYPE boolean.
  FIELD-SYMBOLS:
    <rwa> TYPE pc261.
  IF $chk_ess EQ true.
* Get pay date
    CALL FUNCTION 'CD_SELECT_SRTZA'
      EXPORTING
        record_type = cd_c-actual
      TABLES
        in_rgdir    = $evp
        out_rgdir   = aresults.
    SORT aresults BY seqnr DESCENDING.
    is_retro = true.
    LOOP AT aresults ASSIGNING <rwa>.
      CALL FUNCTION 'CD_RETROCALC_PERIOD'
        EXPORTING
          entry = <rwa>
        IMPORTING
          calcd = is_retro.
      IF is_retro EQ false.
        EXIT.
      ENDIF.
    ENDLOOP.
* Check Infotype with pay date
    IF is_retro EQ false.
      CALL FUNCTION 'HR_CHECK_IF_ESS_PAYSLIP'
        EXPORTING
          pernr   = pernr-pernr
          result  = <rwa>
          molga   = molga
        IMPORTING
          essonly = ess_only
        EXCEPTIONS
          OTHERS  = 0.
      IF ess_only EQ true.
        rc = 12.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                                                    "VKIK023668

*---------------------------------------------------------------------*
* In dieser Routine werden alle Monate nacheinander abgearbeitet.     *
* Bei Rueckrechnungen (ausser bei Parameter RUECKD =  ) alle Monate,  *
* ab dem ersten abgerechneten Monat dieser Abrechnung.                *
* Falls RUECKD = 1 gesetzt ist, so wird die Originalperiode           *
* ausgelassen.                                                        *
*---------------------------------------------------------------------*
FORM process_evp                                            "VKIK041980
  TABLES $evp STRUCTURE pc261.                              "VKIK047993
  DATA:
    print_retro LIKE boolean,
    retrocalc   LIKE boolean,
    okay        LIKE boolean.
  CLEAR gv_same_pernr.
  LOOP AT $evp                                              "VKIK047993
    WHERE srtza EQ 'A'.                   "KVHN2363595
    IF $evp-dummy = 'X' AND NOT ( sy-cprog CS 'CLJN' ) .
      CONTINUE.
    ENDIF.
    evp = $evp.                        "global evp-struct. "VKIK047993
    PERFORM refresh_period.
    PERFORM refresh_period_natio.
    PERFORM refresh_period_mod.
    okay = false.
    PERFORM import_results USING $evp retrocalc okay print_retro.
    IF retrocalc EQ true.                                   "VKIK006809
      rueck = 'R'.                     "global field       "VKIK006809
    ENDIF.                                                  "VKIK006809
    IF okay EQ true.
      PERFORM edit_period
        USING xcedt-typ_rcal retrocalc versc-fpbeg versc-fpend.
    ENDIF.
    gv_same_pernr = 'X'.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM IMPORT_RESULTS                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($EVP)                                                   *
*  -->  $RECALC                                                       *
*---------------------------------------------------------------------*
FORM import_results USING VALUE($evp) LIKE pc261            "VKIK041980
                          $recalc LIKE boolean
                          okay LIKE boolean
                          prt_retro LIKE boolean.
  DATA:
    rc        LIKE sy-subrc.
  PERFORM import_actual USING $evp rc.
  PERFORM set_currency USING xcedt rc.
  PERFORM set_recalc USING $evp $recalc.
  IF $recalc EQ false.                 "actual period ?
    PERFORM check_prt_actual USING okay
                                   prt_retro
                                   xcedt-prt_aper(1)
                                   $evp.
    "Optional national enhancement for this form           "YMLN2633264
    PERFORM check_prt_actual_natio IN PROGRAM (repid) IF FOUND "N2633264
      USING okay prt_retro xcedt-prt_aper(1) $evp.          "N2633264
    IF okay EQ true.                   "Print actual       "VKIK043489
      IF xcedt-cur_alt  EQ true.       "alternative currency "VKIK043489
        PERFORM convert_result USING versc-molga
                                     $evp-fpbeg
                                     versc-waers     "from curr
                                     ip_curr         "to curr
                                     rc.
      ENDIF.                                                "VKIK043489
    ENDIF.                                                  "VKIK043489
  ELSE.
    PERFORM check_prt_retro USING okay prt_retro xcedt-prt_rcal $evp.
    IF okay EQ true.                   "Print retros
      PERFORM import_previous USING $evp.
      IF xcedt-typ_rcal  EQ 'A'        "Differences(detail) one page
      OR xcedt-typ_rcal  EQ 'S'        "Differences(sum) one page
      OR xcedt-cur_ip   EQ true        "Inperiod currency  "VKIK043489
      OR xcedt-cur_alt  EQ true.       "alternative currency "VKIK043489
*     or xcedt-typ_rcal  eq 'D'        "Differences/period/page
*     or xcedt-typ_rcal  eq 'J'        "full values/period/page
        PERFORM convert_result USING versc-molga
                                     $evp-ipend
                                     versc-waers     "from curr
                                     ip_curr         "to curr
                                     rc.
        PERFORM convert_old_result USING oversc-molga
                                         $evp-ipend
                                         oversc-waers    "from curr
                                         ip_curr         "to curr
                                         rc.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SET_RECALC                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($EVP)                                                   *
*  -->  $RECALC                                                       *
*---------------------------------------------------------------------*
FORM set_recalc USING VALUE($evp) STRUCTURE pc261
                      $recalc LIKE boolean.
  CALL FUNCTION 'CD_RETROCALC_PERIOD'
    EXPORTING
      entry  = $evp
    IMPORTING
      calcd  = $recalc
    EXCEPTIONS
      OTHERS = 1.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM IMPORT_ACTUAL                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($EVP)                                                   *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM import_actual USING VALUE($evp) LIKE pc261
                         rc LIKE sy-subrc.
  PERFORM import-direct-natio USING $evp rc.
  IF rc NE 0.
    IF  switch-p_lle EQ false
    AND switch-p_llg EQ false.
      CALL FUNCTION 'HR_APPEND_ERROR_LIST'
        EXPORTING
          pernr = pernr-pernr
          arbgb = '3R'
          msgty = c_msgty-error
          msgno = 319
          msgv1 = $evp-seqnr.
      CALL FUNCTION 'HR_APPEND_ERROR_LIST' "Cluster Authorization
        EXPORTING
          pernr = pernr-pernr
          arbgb = 'HRPAY99IMPEXP'
          msgty = c_msgty-error
          msgno = 018.
      REJECT.
    ELSE.
      CLEAR rc.
      MOVE-CORRESPONDING $evp TO versc.
    ENDIF.
  ELSE.
    IF $evp-dummy EQ true.
      MOVE-CORRESPONDING $evp TO versc.
      REFRESH rt. CLEAR rt.
    ENDIF.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM IMPORT_PREVIOUS_ORIGINAL                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($EVP)                                                   *
*---------------------------------------------------------------------*
FORM import_previous_original USING VALUE($evp) STRUCTURE pc261.
  IF xcedt-prt_aper(1) EQ 'D'.         "Print actual if differences ?
    CALL FUNCTION 'CD_READ_PREVIOUS_ORIGINAL'
      EXPORTING
        in_record  = $evp
      IMPORTING
        out_record = $evp
      TABLES
        rgdir      = rgdir
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc EQ 0.
      PERFORM import_old-natio USING $evp.
    ELSE.
      PERFORM refresh_old-natio.
    ENDIF.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM IMPORT_PREVIOUS                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($EVP)                                                   *
*---------------------------------------------------------------------*
FORM import_previous USING VALUE($evp) STRUCTURE pc261.
  DATA:                                                     "VKIK012152
    ignore    LIKE boolean.
  def_rgdir out_rgdir.
  IF xcedt-prt_rcal EQ 'L'             "print recalc if diff. ?
  OR xcedt-typ_rcal EQ 'A'             "print always recalc ?
  OR xcedt-typ_rcal EQ 'D'             "recalc as differences ?
  OR xcedt-typ_rcal EQ 'S'.            "recalc as summary ?
    CALL FUNCTION 'CD_READ_PREVIOUS'
      EXPORTING
        in_record = $evp
        exact     = true
      TABLES
        rgdir     = rgdir
        out_rgdir = out_rgdir
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc EQ 0 OR $evp-dummy EQ true.                 "WLIK023737
      IF $evp-dummy EQ true.                                "
        out_rgdir = $evp.                                   "
      ELSE.                                                 "
        READ TABLE out_rgdir INDEX 1.                       "
      ENDIF.                                                "WLIK023737
      IF xcedt-prt_osr NE true.                             "note 456778
        CALL FUNCTION 'HRPAY99_IGNORE_REVERSAL'                   "
          EXPORTING                                               "
            evp_new   = $evp
            evp_old   = out_rgdir
          IMPORTING
            ignore    = ignore
            old_rgdir = out_rgdir
          TABLES
            out_rgdir = out_rgdir
            rgdir     = rgdir.                                 "
      ENDIF.                                                "note 456778
      IF ignore IS INITIAL.
        PERFORM import_old-natio USING out_rgdir.
      ELSE.
        PERFORM refresh_old-natio.
      ENDIF.
    ELSEIF sy-subrc = 1.
      CALL FUNCTION 'CD_READ_PREVIOUS'
        EXPORTING
          in_record = $evp
        TABLES
          rgdir     = rgdir
          out_rgdir = out_rgdir
        EXCEPTIONS
          OTHERS    = 1.
      IF sy-subrc = 1.
        PERFORM refresh_old-natio.
      ELSE.
        LOOP AT out_rgdir.                        " KVHA to Handle Splits
          IF sy-subrc EQ 0 OR $evp-dummy EQ true.           "WLIK023737
            IF $evp-dummy EQ true.                                "
              out_rgdir = $evp.                                   "
            ENDIF.                                          "WLIK023737
            IF xcedt-prt_osr NE true.                             "note 456778
              CALL FUNCTION 'HRPAY99_IGNORE_REVERSAL'                   "
                EXPORTING                                               "
                  evp_new   = $evp
                  evp_old   = out_rgdir
                IMPORTING
                  ignore    = ignore
                  old_rgdir = out_rgdir
                TABLES
                  out_rgdir = out_rgdir
                  rgdir     = rgdir.                                 "
            ENDIF.                                                "note 456778
            IF ignore IS INITIAL.
              PERFORM import_old-natio USING out_rgdir.
            ELSE.
              PERFORM refresh_old-natio.
            ENDIF.
          ELSE.
            PERFORM refresh_old-natio.
          ENDIF.
          PERFORM save_split_results_o USING out_rgdir. " KVHA to Handle Splits
        ENDLOOP.                                        " KVHA to Handle Splits
        PERFORM saved_splits_to_tables_o.               " KVHA to Handle Splits
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHECK_PRT_ACTUAL                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OKAY                                                          *
*  -->  VALUE(RETRO_PRT)                                              *
*  -->  VALUE(PRT_APER)                                               *
*  -->  VALUE($EVP)                                                   *
*---------------------------------------------------------------------*
FORM check_prt_actual USING okay LIKE boolean               "VKIK041980
                            VALUE(retro_prt) LIKE boolean
                            VALUE(prt_aper) TYPE c
                            VALUE($evp) LIKE pc261.
  okay = false.
  CASE prt_aper.
    WHEN 'A'.                          "print always
      okay = true.
    WHEN ' '.                          "print never        "VKIK023737
      okay = false.
    WHEN 'D'.                          "check for differences
      okay = true.                     "or retros printed
      CHECK retro_prt EQ false
        AND switch-c_i512ed EQ true.
      PERFORM import_previous_original USING $evp.
      CALL FUNCTION 'HR_CONVERT_CURRENCY_RESULT'
        EXPORTING
          country_grouping = versc-molga
          conversion_date  = $evp-fpbeg
          foreign_currency = oversc-waers
          local_currency   = versc-waers
          pernr            = pernr-pernr         "GWY839032
        TABLES
          result_table     = ort
*         CUMULATED_RESULT_TABLE =
        EXCEPTIONS
          invalid_wagetype = 1
          error_conversion = 2
          OTHERS           = 3.
      okay = false.
      LOOP AT rt.
        READ TABLE i512ed WITH KEY rt-lgart BINARY SEARCH.
        CHECK sy-subrc EQ 0.
        READ TABLE ort WITH KEY rt.
        IF sy-subrc NE 0
        OR ort NE rt.
          okay = true.
          EXIT.
        ENDIF.
      ENDLOOP.
      CHECK okay EQ false.
      LOOP AT ort.
        READ TABLE i512ed WITH KEY ort-lgart BINARY SEARCH.
        CHECK sy-subrc EQ 0.
        READ TABLE rt WITH KEY ort.
        IF sy-subrc NE 0
        OR rt NE ort.
          okay = true.
          EXIT.
        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHECK_PRT_RETRO                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OKAY                                                          *
*  -->  RETRO_PRT                                                     *
*  -->  VALUE(PRT_RCAL)                                               *
*  -->  VALUE($EVP)                                                   *
*---------------------------------------------------------------------*
FORM check_prt_retro USING okay LIKE boolean                "VKIK041980
                           retro_prt LIKE boolean
                           VALUE(prt_rcal) TYPE c
                           VALUE($evp) LIKE pc261.
  DATA: save_rt  LIKE rt  OCCURS 20,                        "YMLN970454
        save_ort LIKE ort OCCURS 20.                        "YMLN970454
  okay = false.
  CASE prt_rcal.
    WHEN true.                         "print always
      okay = true.
    WHEN 'L'.                          "check for differences
      PERFORM import_previous USING $evp.
*     Check for rules and split-handling in T512E           "YMLN970454
      save_rt[] = rt[].                                     "YMLN970454
      REFRESH rt.                                          "YMLN1017654
*     LOOP AT RT.                                  " 970454"YMLN1017654
      LOOP AT save_rt INTO rt.                             "YMLN1017654
        REFRESH xrt. CLEAR xrt.                             "YMLN970454
        REFRESH zrt. CLEAR zrt.                             "YMLN987644
        CLEAR rt_2. MOVE-CORRESPONDING rt TO rt_2.         "YMLN1029241
        PERFORM read-i512e USING rt-lgart 'RT'.             "YMLN970454
        CHECK sy-subrc = 0.                                 "YMLN970454
        IF xrt[] IS INITIAL AND NOT zrt[] IS INITIAL.       "YMLN987644
          CLEAR xrt.                                        "YMLN987644
          MOVE-CORRESPONDING zrt TO xrt.                    "YMLN987644
          APPEND xrt.                                       "YMLN987644
        ENDIF.                                              "YMLN987644
        IF xrt[] IS INITIAL.                                "YMLN970454
*         DELETE RT.                               " 970454"YMLN1017654
        ELSE.                                               "YMLN970454
          rt-apznr = xrt-apznr.                             "YMLN970454
          rt-cntr1 = xrt-cntr1.                             "YMLN970454
          rt-cntr2 = xrt-cntr2.                             "YMLN970454
          rt-cntr3 = xrt-cntr3.                             "YMLN970454
          rt-alznr = xrt-alznr.                             "YMLN970454
          rt-c1znr = xrt-c1znr.                             "YMLN970454
          rt-btznr = xrt-btznr.                             "YMLN970454
          rt-abznr = xrt-abznr.                             "YMLN970454
          rt-v0typ = xrt-v0typ.                             "YMLN970454
          rt-v0znr = xrt-v0znr.                             "YMLN970454
          rt-abart = xrt-abart.                            "YMLN1097628
          rt-zeinh = xrt-zeinh.                            "YMLN1097628
*         MODIFY RT.                               " 970454"YMLN1017654
          COLLECT rt.                                      "YMLN1017654
        ENDIF. "XRT[] IS INITIAL.                           "YMLN970454
      ENDLOOP. "AT RT.                                      "YMLN970454
      save_ort[] = ort[].                                   "YMLN970454
      REFRESH ort.                                         "YMLN1017654
*     LOOP AT ORT.                                  "970454"YMLN1017654
      LOOP AT save_ort INTO ort.                           "YMLN1017654
        REFRESH xrt. CLEAR xrt.                             "YMLN970454
        REFRESH zrt. CLEAR zrt.                             "YMLN987644
        CLEAR rt_2. MOVE-CORRESPONDING ort TO rt_2.        "YMLN1029241
        PERFORM read-i512e USING ort-lgart 'RT'.            "YMLN970454
        CHECK sy-subrc = 0.                                 "YMLN970454
        IF xrt[] IS INITIAL AND NOT zrt[] IS INITIAL.       "YMLN987644
          CLEAR xrt.                                        "YMLN987644
          MOVE-CORRESPONDING zrt TO xrt.                    "YMLN987644
          APPEND xrt.                                       "YMLN987644
        ENDIF.                                              "YMLN987644
        IF xrt[] IS INITIAL.                                "YMLN970454
*         DELETE ORT.                              " 970454"YMLN1017654
        ELSE.                                               "YMLN970454
          ort-apznr = xrt-apznr.                            "YMLN970454
          ort-cntr1 = xrt-cntr1.                            "YMLN970454
          ort-cntr2 = xrt-cntr2.                            "YMLN970454
          ort-cntr3 = xrt-cntr3.                            "YMLN970454
          ort-alznr = xrt-alznr.                            "YMLN970454
          ort-c1znr = xrt-c1znr.                            "YMLN970454
          ort-btznr = xrt-btznr.                            "YMLN970454
          ort-abznr = xrt-abznr.                            "YMLN970454
          ort-v0typ = xrt-v0typ.                            "YMLN970454
          ort-v0znr = xrt-v0znr.                            "YMLN970454
          ort-abart = xrt-abart.                           "YMLN1097628
          ort-zeinh = xrt-zeinh.                           "YMLN1097628
*         MODIFY ORT.                              " 970454"YMLN1017654
          COLLECT ort.                                     "YMLN1017654
        ENDIF. "XRT[] IS INITIAL.                           "YMLN970454
      ENDLOOP. "AT ORT.                                     "YMLN970454
      CLEAR: xrt, xrt[], zrt, zrt[].                       "YMLN1036996
      LOOP AT rt.
        IF switch-c_i512er EQ true.
          READ TABLE i512er WITH KEY rt-lgart BINARY SEARCH.
          CHECK sy-subrc EQ 0.
        ENDIF.
        READ TABLE ort WITH KEY rt.
        IF sy-subrc NE 0
        OR ort NE rt.
          okay = true.
          retro_prt = true.
          EXIT.
        ENDIF.
      ENDLOOP.
*     CHECK OKAY EQ FALSE.                                  "YMLN970454
      IF okay EQ false.                                     "YMLN970454
        LOOP AT ort.
          IF switch-c_i512er EQ true.
            READ TABLE i512er WITH KEY ort-lgart BINARY SEARCH.
            CHECK sy-subrc EQ 0.
          ENDIF.
          READ TABLE rt WITH KEY ort.
          IF sy-subrc NE 0
          OR rt NE ort.
            okay = true.
            retro_prt = true.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF. "OKAY EQ FALSE.                                "YMLN970454
*     RT[] = save_RT. ORT[] = save_ORT.            " 970454"YMLN1017654
      rt[] = save_rt[]. ort[] = save_ort[].                "YMLN1017654
    WHEN false.                        "RR unterdruecken
      okay = false.
    WHEN OTHERS.
      okay = true.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM CONVERT_RESULT                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($MOLGA)                                                 *
*  -->  VALUE($CONV_DATE)                                             *
*  -->  VALUE($F_CURR)                                                *
*  -->  VALUE($L_CURR)                                                *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM convert_result USING VALUE($molga) LIKE t500l-molga    "VKIK041980
                          VALUE($conv_date) LIKE sy-datum
                          VALUE($f_curr) LIKE t500c-waers
                          VALUE($l_curr) LIKE t500c-waers
                          rc.
  DATA:
    $rc       LIKE sy-subrc.
  CALL FUNCTION 'HR_CONVERT_CURRENCY_RESULT'
    EXPORTING
      country_grouping             = $molga
      conversion_date              = $conv_date
      foreign_currency             = $f_curr
      local_currency               = $l_curr
      pernr                        = pernr-pernr
    TABLES
      result_table                 = rt
      cumulated_result_table       = crt
      subsequent_time_ticket_table = ls
      arrears_table                = arrrs
      deduction_table              = ddntk
    EXCEPTIONS
      invalid_wagetype             = 1
      error_conversion             = 2
      OTHERS                       = 3.
  IF sy-subrc NE 0.
    rc = sy-subrc.
  ENDIF.
  CALL FUNCTION 'HR_CONVERT_CURRENCY_INW'
    EXPORTING
      country_grouping  = $molga
      conversion_date   = $conv_date
      local_currency    = $l_curr
    TABLES
      time_ticket_table = le
    EXCEPTIONS
      error_conversion  = 1
      OTHERS            = 2.
  IF sy-subrc NE 0.
    rc = sy-subrc.
  ENDIF.
  PERFORM convert_result_natio USING $molga
                                     $conv_date
                                     $f_curr
                                     $l_curr
                                     $rc.
  IF $rc NE 0.
    rc = $rc.
  ENDIF.
  IF rc EQ 0.
    versc-waers = $l_curr.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM CONVERT_OLD_RESULT                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($MOLGA)                                                 *
*  -->  VALUE($CONV_DATE)                                             *
*  -->  VALUE($F_CURR)                                                *
*  -->  VALUE($L_CURR)                                                *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM convert_old_result USING VALUE($molga) LIKE t500l-molga "VKIK041980
                              VALUE($conv_date) LIKE sy-datum
                              VALUE($f_curr) LIKE t500c-waers
                              VALUE($l_curr) LIKE t500c-waers
                              rc LIKE sy-subrc.
  DATA:
    $rc      LIKE sy-subrc.
  CALL FUNCTION 'HR_CONVERT_CURRENCY_RESULT'
    EXPORTING
      country_grouping             = $molga
      conversion_date              = $conv_date
      foreign_currency             = $f_curr
      local_currency               = $l_curr
      pernr                        = pernr-pernr
    TABLES
      result_table                 = ort
      cumulated_result_table       = ocrt
      subsequent_time_ticket_table = ols
      arrears_table                = oarrrs
      deduction_table              = oddntk
    EXCEPTIONS
      invalid_wagetype             = 1
      error_conversion             = 2
      OTHERS                       = 3.
  IF sy-subrc NE 0.
    rc = sy-subrc.
  ENDIF.
  PERFORM convert_old_result_natio USING $molga
                                         $conv_date
                                         $f_curr
                                         $l_curr
                                         $rc.
  IF $rc NE 0.
    rc = $rc.
  ENDIF.
  IF rc EQ 0.
    oversc-waers = $l_curr.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM EDIT_PERIOD                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($TYPE_RR)                                               *
*  -->  VALUE($RETROCALC)                                             *
*  -->  VALUE($FPBEG)                                                 *
*  -->  VALUE($FPEND)                                                 *
*---------------------------------------------------------------------*
FORM edit_period
  USING VALUE($type_rr) VALUE($retrocalc) VALUE($fpbeg) VALUE($fpend).
  PERFORM copy-natio IN PROGRAM (repid)                     "VKIK010617
    USING $type_rr $retrocalc $fpbeg $fpend
    IF FOUND.
  CASE $type_rr.
    WHEN 'A'.                          "RR-Differenzen
      IF $retrocalc = true.            "RR?
        PERFORM copy-rt-to-rt_2 USING $fpbeg $fpend.
        PERFORM copy-ort-to-rt_2 USING $fpbeg $fpend.
        PERFORM copy_ls_ols_to_ls2 USING $fpend.            "WXJK014138
      ELSE.
        PERFORM copy-rt-to-rt_2 USING $fpbeg $fpend.
        PERFORM delete-from-rt_2.
        PERFORM copy_ls_to_ls2 USING $fpend.                "WXJK014138
        PERFORM create_form USING $retrocalc.
      ENDIF.
    WHEN 'D'.                          "RR-Differenzen auf extra Forml.
      IF $retrocalc = true.            "RR?
        PERFORM copy-rt-to-rt_2 USING $fpbeg $fpend.
        PERFORM copy-ort-to-rt_2 USING $fpbeg $fpend.
        PERFORM delete-from-rt_2.
        PERFORM copy_ls_ols_to_ls2 USING $fpend.            "WXJK014138
        PERFORM create_form USING $retrocalc.
      ELSE.
        PERFORM copy-rt-to-rt_2 USING $fpbeg $fpend.
        PERFORM copy_ls_to_ls2 USING $fpend.                "WXJK014138
        PERFORM create_form USING $retrocalc.
      ENDIF.
    WHEN 'J'.                          "RR auf extra Formular
      PERFORM copy-rt-to-rt_2 USING $fpbeg $fpend.
      PERFORM copy_ls_to_ls2 USING $fpend.                  "WXJK014138
      PERFORM create_form USING $retrocalc.
    WHEN 'S'.                          "RR-Differenzen
      IF $retrocalc = true.            "RR?
        PERFORM copy-rt-to-rt_2 USING ldate ldate.          "VKIK131798
        PERFORM copy-ort-to-rt_2 USING ldate ldate.
        PERFORM copy_ls_ols_to_ls2 USING $fpend.            "WXJK014138
      ELSE.
        PERFORM copy-rt-to-rt_2 USING $fpbeg $fpend.
        PERFORM delete-from-rt_2.
        PERFORM copy_ls_to_ls2 USING $fpend.                "WXJK014138
        PERFORM create_form USING $retrocalc.
      ENDIF.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
* Diese Routine setzt voraus, dass alle Ergebnistabellen (RT_2, CRT,  *
* usw. jetzt richtig gefuellt sind. Insbesondere muss die Tabelle     *
* RT_2 abhaengig vom Parameter RUECKR richtig gefuellt sein.          *
*---------------------------------------------------------------------*
FORM create_form USING VALUE($retrocalc).
  DATA:
    print_okay     LIKE boolean.       "Formular drucken?
  PERFORM fill_per_period.
  PERFORM fill_per_period_natio.
  PERFORM fill_per_period_mod.
  PERFORM fill_adrsp_for_period USING adrsp.
  PERFORM allocate-to-windows USING $retrocalc.             "VKIK045475
  PERFORM alloc-zrt-to-windows.                             "VKIK035688
* perform allocate-to-windows-ll.                           "WXJK014138
  PERFORM sort-xrt USING xcedt-srt_rcal.
  PERFORM delete-from-xrt.                                  "VKIK112124
  PERFORM delete-entries.
  PERFORM check_print USING print_okay.
  IF print_okay EQ true.
    PERFORM sort-xrt USING xcedt-srt_rcal.                  "VKIK032558
    PERFORM group-text-indicators.
    PERFORM sort-xrt USING xcedt-srt_rcal.
    PERFORM allocate-page-and-line-numbers.
    PERFORM sort-xrt USING xcedt-srt_rcal.
    PERFORM print-complete-payroll.
  ENDIF.
  CLEAR rt_2. REFRESH rt_2.
ENDFORM.                               "CREATE_FORM.

*---------------------------------------------------------------------*
*       FORM FILL_PER_PERIOD                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM fill_per_period.
  CALL FUNCTION 'RP_GET_CURRENCY'
    EXPORTING
      molga                         = molga
      begda                         = versc-fpbeg
      endda                         = versc-fpend
    IMPORTING
      waers                         = fp_curr
    EXCEPTIONS
      molga_not_in_t001p            = 1
      no_entry_found_in_table_t500c = 2
      no_entry_found_in_table_t500p = 3
      OTHERS                        = 4.
  PERFORM fill-xbt.     "Vorbereitung um Ueberweisungen anzudrucken.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SORT-XRT                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($SORT_RR)                                               *
*---------------------------------------------------------------------*
FORM sort-xrt USING VALUE($sort_rr).
*-----------------------------------------------------------------------
* In Abhängigkeit des Parameters SORT_RR wird die Tabelle XRT nach
* verschiedenen Feldern sortiert. Dies hat vor allem bei der Darstellung
* von Rückrechnungsergebnissen Auswirkungen.
*-----------------------------------------------------------------------
  CASE $sort_rr.
    WHEN '1'.
      SORT xrt BY pagno linno windo grppe kennz fconv sortf seqno seqn2
                  fpbeg fpend splts DESCENDING  btznr v0typ v0znr
                  apznr cntr1 cntr2 cntr3 alznr c1znr abznr
                  ltype colno lgart.
    WHEN '2'.
      SORT xrt BY pagno linno windo fpbeg fpend grppe kennz fconv
                  sortf seqno seqn2 splts DESCENDING  btznr v0typ v0znr
                  apznr cntr1 cntr2 cntr3 alznr c1znr abznr
                  ltype colno lgart.
    WHEN '3'.                                               "XRRK036865
      SORT xrt BY pagno linno windo grppe kennz fconv fpbeg fpend
               sortf seqno seqn2 splts DESCENDING  btznr v0typ v0znr
               apznr cntr1 cntr2 cntr3 alznr c1znr abznr
               ltype colno lgart.
    WHEN 'Z'.                                               "XRRK036865
      PERFORM sort-xrt-mod IN PROGRAM (repid) USING sort_rr IF FOUND.
  ENDCASE.
ENDFORM.                               "SORT-XRT.

*---------------------------------------------------------------------*
*       FORM CHECK_PRINT                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OKAY                                                          *
*---------------------------------------------------------------------*
FORM check_print
  USING okay.
  okay = false.
  CASE xcedt-prt_aper+1(1).
    WHEN 'F'.
      LOOP AT xrt
        WHERE delet EQ false
          AND tname NE 'PAGE '.                             "VKIK017127
        okay = true.
        EXIT.
      ENDLOOP.
    WHEN 'Z'.                                               "VKIK032558
      PERFORM check_print_mod IN PROGRAM (repid)            "VKIK032558
        TABLES xrt                                          "VKIK032558
        USING  okay                                         "VKIK032558
        IF FOUND.                                           "VKIK032558
    WHEN OTHERS.
      okay = true.
  ENDCASE.
ENDFORM.                               "CHECK_PRINT

*---------------------------------------------------------------------*
*       FORM READ_FORM                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($MOLGA)                                                 *
*  -->  VALUE($FORML)                                                 *
*---------------------------------------------------------------------*
FORM read_form USING VALUE($molga) LIKE t500l-molga
                     VALUE($forml)
                     rc LIKE sy-subrc.
  PERFORM gen-i514d USING $molga $forml rc.                 "VKIK061490
  PERFORM gen-i512q USING $molga $forml rc.
  PERFORM gen-i512e USING $molga $forml rc.
  PERFORM gen-i512f USING $molga $forml rc.
  PERFORM gen-i512n USING $molga $forml rc.
  PERFORM gen-i512d USING $molga $forml rc.
  PERFORM gen-i512g USING $molga $forml rc.
  PERFORM gen-i512p USING $molga $forml rc.
  PERFORM gen-i512s USING $molga $forml rc.                 "VKIK080595
ENDFORM.                               "READ_FORM

FORM gen-i514d USING VALUE($molga) LIKE t500l-molga         "VKIK061490
                     VALUE($forml)
                     rc LIKE sy-subrc.
  CLEAR i514d.
  SELECT SINGLE * FROM t514d
    INTO CORRESPONDING FIELDS OF i514d
    WHERE molga EQ $molga
      AND forml EQ $forml.
  IF i514d-c_max IS INITIAL.
    i514d-c_max = 132.
  ENDIF.
  IF i514d-r_max IS INITIAL.
    i514d-r_max = 99.
  ENDIF.
  IF i514d-c_use IS INITIAL
  OR i514d-cflag EQ true   .          "update via view instead PE51!
    i514d-c_use = i514d-c_max.
  ENDIF.
  IF i514d-r_use IS INITIAL
  OR i514d-cflag EQ true   .          "update via view instead PE51!
    i514d-r_use = i514d-r_max.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Baut die Tabelle I512E auf                                          *
*---------------------------------------------------------------------*
FORM gen-i512e USING VALUE($molga) LIKE t500l-molga         "VKIK051109
                     VALUE($forml)
                     rc LIKE sy-subrc.
  DATA:
    i512eg   TYPE t_512e,
    gen_evcl LIKE boolean,
    found    LIKE boolean.
  REFRESH i512e.
  REFRESH i512e_zk.                                         "HKUK008843
  SELECT * FROM t512e
    INTO CORRESPONDING FIELDS OF TABLE i512e
    WHERE molga EQ $molga
      AND forml EQ $forml.

  LOOP AT i512e                                             "HKUK026274
    WHERE tabna = 'ZKQ'                                     "HKUK026274
    OR    tabna = 'ZKT'                                     "HKUK026274
    OR    tabna = 'ZKX'.                                    "HKUK026274
    CHECK i512e-slart NE space.                             "HKUK026274
    APPEND i512e TO i512e_zk.                               "HKUK026274
    DELETE i512e.                                           "HKUK026274
    switch-p_zk_s = true.                                   "HKUK026274
    IF i512e-tabna = 'ZKX'.                                 "HKUK027781
      switch-p_zk_x = true.                                 "HKUK027781
    ENDIF.                                                  "HKUK027781
  ENDLOOP.                                                  "HKUK026274

  LOOP AT i512e.
    IF i512e-colno EQ space.
      i512e-colno = 'X'.
    ENDIF.
    gen_evcl = false.
    CASE i512e-tabna.
      WHEN 'R' OR 'RT   '.                                  "RT
        switch-p_rt = true.
        gen_evcl = true.
        i512e-tabna = 'RT   '.
      WHEN 'RTS  '.                    "RT summary        "VKIK006529
        switch-p_rts = true.
        gen_evcl = true.
        i512e-tabna = 'RTS  '.
      WHEN 'M' OR 'CRTM '.             "CRT monthly
        switch-p_crt_m = true.
        gen_evcl = true.
        i512e-tabna = 'CRTM '.
      WHEN 'O' OR 'CRTQ'.              "CRT quarterly
        switch-p_crt_q = true.
        gen_evcl = true.
        i512e-tabna = 'CRTQ '.
      WHEN 'U' OR 'CRTU '.             "CRT unlimited
        switch-p_crt_u = true.
        gen_evcl = true.
        i512e-tabna = 'CRTU '.
      WHEN 'C' OR 'CRT  '.             "CRT yearly
        switch-p_crt_y = true.
        gen_evcl = true.
        i512e-tabna = 'CRT  '.                              "VKIK054253
      WHEN 'F' OR 'CRTF '.             "CRT monthly when paid"VKIK046117
        switch-p_crt_f = true.
        gen_evcl = true.
        i512e-tabna = 'CRTF '.
      WHEN 'H' OR 'CRTH '.             "CRT quarter when paid"VKIK046117
        switch-p_crt_h = true.
        gen_evcl = true.
        i512e-tabna = 'CRTH '.
      WHEN 'K' OR 'CRTK '.             "CRT yearly when paid"VKIK019960
        switch-p_crt_k = true.
        gen_evcl = true.
        i512e-tabna = 'CRTK '.
      WHEN 'Z' OR 'ZRT  '.                                  "sum
        switch-p_zrt = true.                                "ZRT
        i512e-tabna = 'ZRT  '.
      WHEN 'XBT  '.                    "Bank transfer      "VKIK023737
        switch-p_xbt = true.
        gen_evcl = true.
      WHEN 'B' OR 'ARRRS'.             "Arrears            "VKIK125373
        switch-p_arrrs  = true.
        gen_evcl = true.
        i512e-tabna = 'ARRRS'.
      WHEN 'D' OR 'DDNTK'.             "Deductions not take"VKIK125373
        switch-p_ddntk  = true.
        gen_evcl = true.
        i512e-tabna = 'DDNTK'.
      WHEN 'P' OR 'TXT  '.             "memos
        switch-p_p0128 = true.
        i512e-tabna = 'TXT  '.
      WHEN 'Q' OR 'ZKQ  '.             "Zeitkontingente abw
        switch-p_zk_q = true.
        i512e-tabna = 'ZKQ  '.
*   begin: special conversions for compatibility             "HKUK031254
        IF i512e-lgart+0(2) = 'Y*'.                         "HKUK031254
          i512e-lgart+0(2) = 'S*'.                          "HKUK031254
        ENDIF.                                              "HKUK031254
*   end: special conversions for compatibility               "HKUK031254
      WHEN 'T' OR 'ZKT  '.             "Zeitkontingente anw
        switch-p_zk_t = true.
        i512e-tabna = 'ZKT  '.
*   begin: special conversions for compatibility             "HKUK031254
        IF i512e-lgart+0(2) = 'Y*'.                         "HKUK031254
          i512e-lgart+0(2) = 'S*'.                          "HKUK031254
        ENDIF.                                              "HKUK031254
*   end: special conversions for compatibility               "HKUK031254
      WHEN 'ZKX  '.      "Zeitkontingente accrued values     "HKUK026303
        switch-p_zk_x = true.                               "HKUK026303
      WHEN 'ZKSUM'.                                         "HKUK026274
      WHEN 'W' OR 'VAC  '.             "vacation
        switch-p_vac = true.
        i512e-tabna = 'VAC  '.
      WHEN '4' OR 'LLE  '.             "Einzell-LL LE
        switch-p_lle = true.                                "WXJK014138
        gen_evcl = true.
        i512e-tabna = 'LLE  '.
      WHEN '5' OR 'LLG  '.             "Gruppen-LL LE
        switch-p_llg = true.                                "WXJK014138
        gen_evcl = true.
        i512e-tabna = 'LLG  '.
      WHEN '6' OR 'LLEF '.             "Einzel-LL LE & LS
        switch-p_llef = true.                               "WXJK014138
        gen_evcl = true.
        i512e-tabna = 'LLEF '.
      WHEN '7' OR 'LLGF '.             "Gruppen-LL LE & LS
        switch-p_llgf = true.                               "WXJK014138
        gen_evcl = true.
        i512e-tabna = 'LLGF '.
      WHEN OTHERS.
        found = false.
        PERFORM test-i512e-natio USING found gen_evcl i512e.
        IF found EQ false.
          PERFORM test-i512e-mod USING found gen_evcl i512e.
          IF found EQ false.
            CALL FUNCTION 'HR_APPEND_ERROR_LIST'
              EXPORTING
                arbgb = '3R'
                msgty = c_msgty-warning
                msgno = 317
                msgv1 = i512e-tabna.
            rc = 4.
          ENDIF.
        ENDIF.
    ENDCASE.
    MODIFY i512e.
    IF gen_evcl EQ true
    AND i512e-lgart CP '#*#*++'.
      APPEND i512e TO i512eg.
    ENDIF.
  ENDLOOP.

* check table-id for incentive wages
  PERFORM check_switch_ll USING rc.                         "WXJK014138
  IF rc LE 4.
    PERFORM gen-evcl USING i512e[]
                           $molga
                           $forml                           "WLIK024907
                           i512eg
                           rc.
    PERFORM append-i512er-i512ed.                           "VKIK065442
    PERFORM gen-i512e-from-i512q.
  ENDIF.
ENDFORM.                               "GEN-I512E.

*---------------------------------------------------------------------*
*       FORM GEN-I512F                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM gen-i512f USING VALUE($molga) LIKE t500l-molga
                     VALUE($forml)
                     rc LIKE sy-subrc.
  REFRESH i512f.
  SELECT * FROM t512f
    INTO CORRESPONDING FIELDS OF TABLE i512f
    WHERE molga EQ $molga
      AND forml EQ $forml.
  LOOP AT i512f.
    IF i512f-ecoln IS INITIAL.
      i512f-ecoln = 131.
      MODIFY i512f.
    ENDIF.
  ENDLOOP.
ENDFORM.                               "GEN-I512F.

*---------------------------------------------------------------------*
*       FORM GEN-I512N                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($MOLGA)                                                 *
*  -->  VALUE($FORML)                                                 *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM gen-i512n USING VALUE($molga) LIKE t500l-molga
                     VALUE($forml)
                     rc LIKE sy-subrc.
  SELECT * FROM t512n
    INTO CORRESPONDING FIELDS OF TABLE i512n
    WHERE molga EQ $molga
      AND forml EQ $forml.
  LOOP AT i512n.
    PERFORM ixxxx_fname_chk USING i512n-schlw.
    PERFORM ixxxx_fname_chk USING i512n-wert1.
    PERFORM ixxxx_fname_chk USING i512n-wert2.
    PERFORM ixxxx_fname_chk USING i512n-drwert.
    MODIFY i512n.
  ENDLOOP.
  CLEAR i512n.
ENDFORM.                               "GEN-I512N

*---------------------------------------------------------------------*
*       FORM GEN-I512Q                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM gen-i512q USING VALUE($molga) LIKE t500l-molga
                     VALUE($forml)
                     rc LIKE sy-subrc.
  REFRESH i512q.
  SELECT * FROM t512q
    INTO CORRESPONDING FIELDS OF TABLE i512q
    WHERE molga EQ $molga
      AND forml EQ $forml.
  CLEAR fvar-adrs_lines.                                    "VKIK031819
  CLEAR fvar-adrsp_lines.                                   "VKIK051178
  LOOP AT i512q.
*   begin: special conversions for compatibility
    PERFORM ixxxx_fname_chk USING i512q-fname.
    IF  i512q-fname+0(3) EQ 'VAC'
    AND i512q-fname+5(1) EQ '-'.
      i512q-lgart+0(2) = i512q-fname+3(2).
      i512q-lgart+2(2) = i512q-fname+9(2).
      i512q-fname+3(2) = space.
      i512q-fname+9(2) = space.
      CONDENSE i512q-fname NO-GAPS.
    ENDIF.
*   end:   special conversions for compatibility
    i512q-strng = i512q-fname.
    PERFORM split_name
      USING i512q-tname i512q-fname i512q-strng.
    CASE i512q-tname.
      WHEN 'ZKQ  '.
        switch-p_zk_q = true.
*   begin: special conversions for compatibility             "HKUK031254
        IF i512q-lgart+0(2) = 'Y*'.                         "HKUK031254
          i512q-lgart+0(2) = 'S*'.                          "HKUK031254
        ENDIF.                                              "HKUK031254
*   end:   special conversions for compatibility             "HKUK031254
      WHEN 'ZKT  '.
        switch-p_zk_t = true.
*   begin: special conversions for compatibility             "HKUK031254
        IF i512q-lgart+0(2) = 'Y*'.                         "HKUK031254
          i512q-lgart+0(2) = 'S*'.                          "HKUK031254
        ENDIF.                                              "HKUK031254
*   end:   special conversions for compatibility             "HKUK031254
      WHEN 'ZKX  '.      "Zeitkontingente accrued values     "HKUK026303
        switch-p_zk_x = true.                               "HKUK026303

      WHEN 'XBT  '.
        switch-f_xbt = true.
      WHEN 'VAC  '.
        switch-f_vac = true.
      WHEN 'ADRS '.
        switch-f_adrs = true.
*         fname = LINE0, ... , LINE9
        IF i512q-fname+4(1) CO '0123456789'.
          fvar-adrs_lines = fvar-adrs_lines + 1.            "VKIK031819
        ENDIF.
      WHEN 'ADRSP'.
        switch-f_adrsp = true.
*         fname = LINE0, ... , LINE9
        IF i512q-fname+4(1) CO '0123456789'.
          fvar-adrsp_lines = fvar-adrsp_lines + 1.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    PERFORM test-i512q-natio USING i512q.
    PERFORM test-i512q-mod   USING i512q.
    MODIFY i512q.
  ENDLOOP.
ENDFORM.                               "GEN-I512Q.

*---------------------------------------------------------------------*
*       FORM GEN-EVCL                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  I512E                                                         *
*  -->  VALUE($MOLGA)                                                 *
*  -->  VALUE($512EG)                                                 *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM gen-evcl USING i512e TYPE t_512e                       "VKIK051109
                    VALUE($molga)
                    VALUE($forml)                           "WLIK024907
                    VALUE($512eg) TYPE t_512e
                    rc LIKE sy-subrc.

  DATA: class       TYPE t514d-class,                       "WLIK024907
        l_add_entry TYPE p512e-lfdnr,
        evcls       TYPE t514f-evcls.

  SELECT SINGLE class FROM t514d INTO (class)
                      WHERE molga EQ $molga
                        AND forml EQ $forml.
  IF sy-subrc EQ 0.
    SELECT SINGLE evcls FROM t514f INTO (evcls)
                        WHERE molga EQ $molga
                          AND class EQ class.
  ENDIF.
  IF evcls IS INITIAL OR evcls EQ '00'.
    evcls = 2.
  ENDIF.                                                    "WLIK024907

  IF sw_ex_win EQ true.
    l_add_entry = '2'.
  ELSE.
    l_add_entry = '1'.
  ENDIF.

  CALL FUNCTION 'HR_EVALUATION_CLASS_3'
    EXPORTING
      molga                       = $molga
      begda                       = pn-begda
      endda                       = pn-endda
      evcls                       = evcls             "WLIK024907
      add_entry                   = l_add_entry
    TABLES
      form_lga                    = i512e
      form_lga_gen                = $512eg
    EXCEPTIONS
      invalid_value_for_add_entry = 1
      invalid_evaluation_class    = 2
      OTHERS                      = 3.
  rc = sy-subrc.
ENDFORM.

*---------------------------------------------------------------------*
* Diese Routine stellt Eintraege in die Tabelle 512E, wenn RUECKR den *
* Wert 'S' hat. Dies wird getan, weil beim Andruck auf feste Positio- *
* nen von RT-Werten mit der Konvertierung 36 aus der Tabelle RT_2     *
* gelesen wird. Ab dieser Version werden aber nicht mehr alle Lohnar- *
* ten in die XRT gestellt, sondern nur die auch in der I512E sind.    *
* Es werden solche Lohnarten in die I512E aufgenommen, die aus der    *
* RT auf feste Positionen ausgegeben werden sollen, und die die       *
* Konvertierung 36 haben.                                             *
*---------------------------------------------------------------------*
FORM gen-i512e-from-i512q.
  CLEAR i512e.
  LOOP AT i512q
    WHERE lgart NE space
      AND tname EQ 'RT   '.
    i512e-lgart = i512q-lgart.
    i512e-tabna = i512q-tname.
    i512e-lfdnr = '99'.
    APPEND i512e.
  ENDLOOP.
  SORT i512e.
ENDFORM.                               "GEN-I512E-FROM-I512Q.

*---------------------------------------------------------------------*
*       FORM GEN-I512D                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM gen-i512d USING VALUE($molga) LIKE t500l-molga
                     VALUE($forml)
                     rc LIKE sy-subrc.
  REFRESH i512d.
  SELECT * FROM t512d
    INTO CORRESPONDING FIELDS OF TABLE i512d
    WHERE molga EQ $molga
      AND forml EQ $forml.
  SORT i512d BY ltype colno foffs.
  LOOP AT i512d.
*   begin: special conversions for compatibility
    PERFORM ixxxx_fname_chk USING i512d-fname.
*   end:   special conversions for compatibility
    i512d-strng = i512d-fname.
    PERFORM split_name
      USING i512d-tname i512d-fname i512d-strng.
    CASE i512d-tname.
      WHEN 'XBT  '.
        switch-f_xbt = true.
    ENDCASE.
    PERFORM test-i512d-natio USING i512d.
    PERFORM test-i512d-mod   USING i512d.
    MODIFY i512d.
  ENDLOOP.
ENDFORM.                               "GEN-I512D.

*---------------------------------------------------------------------*
*       FORM GEN-I512P                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM gen-i512p USING VALUE($molga)
                     VALUE($forml)
                     rc LIKE sy-subrc.
  DATA:
    x512p LIKE i512p OCCURS 0 WITH HEADER LINE,
    w512p LIKE i512p,
    maxln LIKE i512p-linno,
    linno LIKE i512p-linno.
  CLEAR i512p. REFRESH i512p.
  SELECT * FROM t512p
    INTO CORRESPONDING FIELDS OF TABLE x512p
    WHERE molga EQ $molga
      AND forml EQ $forml.                              "#EC CI_GENBUFF
  SORT x512p BY sprsl linno.
  LOOP AT x512p.
    AT NEW sprsl.
      linno = 1.                       "reset idx
    ENDAT.
    w512p = x512p.
    WHILE linno LE x512p-linno.
      w512p-linno = linno.
      APPEND w512p TO i512p.
      linno = linno + 1.
    ENDWHILE.
    AT END OF sprsl.
      IF linno GT maxln.               "get max linno.
        maxln = linno.
      ENDIF.
    ENDAT.
  ENDLOOP.
  CLEAR w512p.
  DO maxln TIMES.                      "no language!
    w512p-linno = sy-index.
    APPEND w512p TO i512p.
  ENDDO.
  SORT i512p BY sprsl linno.
ENDFORM.                               "GEN-I512P.

*---------------------------------------------------------------------*
*       FORM GEN-I512G                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM gen-i512g USING VALUE($molga) LIKE t500l-molga
                     VALUE($forml)
                     rc LIKE sy-subrc.
  CLEAR i512g. REFRESH i512g.
  SELECT * FROM t512g
    INTO CORRESPONDING FIELDS OF TABLE i512g
    WHERE molga EQ $molga
      AND forml EQ $forml.                              "#EC CI_GENBUFF
  SORT i512g BY sprsl windo grppe kennz lfdnr.
ENDFORM.                               "GEN-I512G.

*---------------------------------------------------------------------*
*       FORM GEN-I512S                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM gen-i512s USING VALUE($molga) LIKE t500l-molga         "VKIK080595
                     VALUE($forml)
                     rc LIKE sy-subrc.
  CLEAR i512s. REFRESH i512s.
  SELECT * FROM t512s
    INTO CORRESPONDING FIELDS OF TABLE i512s
    WHERE molga EQ $molga
      AND forml EQ $forml.                              "#EC CI_GENBUFF
  SORT i512s BY sprsl lgart.
ENDFORM.                               "GEN-I512S.

*---------------------------------------------------------------------*
*       FORM APPEND-I512ER-I512ED                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM append-i512er-i512ed.                                  "VKIK065442
  IF xcedt-prt_rcal = 'L'.                                 "YMLN1089688
* Getting the correct evaluation class for current form    "YMLN1081479
    DATA: class TYPE t514d-class.                                      "!
    DATA: evcls TYPE t514f-evcls.                                      "!
    SELECT SINGLE class INTO class                                     "!
      FROM t514d WHERE molga = molga AND forml = xcedt-prt_form.       "!
    IF sy-subrc = 0.                                                   "!
      SELECT SINGLE evcls INTO evcls                                   "!
        FROM t514f WHERE molga = molga AND class = class.              "!
      evcls = ( evcls - 1 ) * 2.                                       "!
    ENDIF.                                                   "YMLN1081479
    DATA: l512e TYPE t_512e  WITH HEADER LINE.               "YMLN1053751
    DATA: aklas TYPE t512w-aklas.                                      "!
* Moving evaluation class entries from I512E to L512E ...            "!
    LOOP AT i512e INTO l512e.                                "YMLN1053751
      CHECK evcls > 0.                                       "YMLN1081479
      CHECK l512e-lgart+2(2) <> '**'.                        "YMLN1081479
      CHECK l512e-lgart(2) = '**'.                           "YMLN1053751
      aklas = l512e-lgart.                                             "!
      DELETE i512e.                                                    "!
*   ... and substitute with corresponding wagetypes                  "!
      SELECT * FROM t512w                                              "!
        WHERE molga = molga                                            "!
*        AND BEGDA <= SY-DATUM                                        "!
*        AND ENDDA >= SY-DATUM.                             "YMLN1053751
          AND begda <= pn-begda
          AND endda >= pn-begda.
*     CHECK T512W-AKLAS+2(2) = AKLAS+2(2).    "YMLN1053751 "YMLN1081479
        CHECK t512w-aklas+evcls(2) = aklas+2(2).             "YMLN1081479
        l512e-lgart = t512w-lgart.                           "YMLN1053751
        APPEND l512e.                                                  "!
      ENDSELECT.                                                       "!
    ENDLOOP.                                                           "!
* Delete wagetypes that are both: direct and indirect selected       "!
* LOOP AT I512E.                               "YMLN1053751"YMLN1089688
*   DELETE L512E                               "YMLN1053751"YMLN1089688
*     WHERE LGART = I512E-LGART                "YMLN1053751"YMLN1089688
*       AND TABNA = I512E-TABNA.               "YMLN1053751"YMLN1089688
* ENDLOOP.                                     "YMLN1053751"YMLN1089688
    SORT i512e BY lgart tabna.                               "YMLN1089688
    LOOP AT l512e.                                           "YMLN1089688
      READ TABLE i512e WITH KEY lgart = l512e-lgart          "YMLN1089688
                                tabna = l512e-tabna          "YMLN1089688
                       BINARY SEARCH.                        "YMLN1089688
      CHECK sy-subrc = 0.                                    "YMLN1089688
      DELETE l512e.                                          "YMLN1089688
    ENDLOOP.                                                 "YMLN1089688
* Combine direct and indirect selected wagetypes           "YMLN1053751
    APPEND LINES OF l512e TO i512e.                                    "!
    SORT i512e.                                              "YMLN1053751
  ENDIF. "xcedt-prt_rcal = 'L'.                            "YMLN1089688
  LOOP AT i512e
    WHERE tabna EQ 'RT   '
      AND difar NE space.
    CASE i512e-difar.
      WHEN 'R'.
        i512er-lgart = i512e-lgart.
*       APPEND I512ER.                                     "YMLN1053751
        COLLECT i512er.                                    "YMLN1053751
        switch-c_i512er = true.
      WHEN 'D'.
        i512ed-lgart = i512e-lgart.
*       APPEND I512ED.                                     "YMLN1053751
        COLLECT i512ed.                                    "YMLN1053751
        switch-c_i512ed = true.
      WHEN '*'.
        i512er-lgart = i512e-lgart.
*       APPEND I512ER.                                     "YMLN1053751
        COLLECT i512er.                                    "YMLN1053751
        switch-c_i512er = true.
        i512ed-lgart = i512e-lgart.
*       APPEND I512ED.                                     "YMLN1053751
        COLLECT i512ed.                                    "YMLN1053751
        switch-c_i512ed = true.
    ENDCASE.
  ENDLOOP.
  SORT i512er.                                             "KVHN1996615
  SORT i512ed.                                             "KVHN1996615
ENDFORM.                               "APPEND-I512ER-I512ED.

* replace old field names by new field names if neccessary
FORM ixxxx_fname_chk USING fld_name.
  PERFORM ixxxx_fname_chg USING 'PN/'         'PN-'         fld_name.
  PERFORM ixxxx_fname_chg USING 'XRT-RUECKD'  'XRT-FPBEG '  fld_name.
* begin                                                    "VKIK042611
  PERFORM ixxxx_fname_chg USING 'EVP-BEGDA'   'EVP-FPBEG'   fld_name.
  PERFORM ixxxx_fname_chg USING 'EVP-ENDDA'   'EVP-FPEND'   fld_name.
  PERFORM ixxxx_fname_chg USING 'EVP-PAPER'   'EVP-FPPER'   fld_name.
  PERFORM ixxxx_fname_chg USING 'EVP-IAPER'   'EVP-INPER'   fld_name.
  PERFORM ixxxx_fname_chg USING 'EVP-IENDD'   'EVP-IPEND'   fld_name.
* end                                                      "VKIK042611
ENDFORM.

*---------------------------------------------------------------------*
*       FORM IXXXX_FNAME_CHG                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($OLD_VAL)                                               *
*  -->  VALUE($NEW_VAL)                                               *
*  -->  FLD_NAME                                                      *
*---------------------------------------------------------------------*
FORM ixxxx_fname_chg USING VALUE($old_val) TYPE c
                           VALUE($new_val) TYPE c
                           fld_name.
  SEARCH fld_name FOR $old_val.
  IF sy-subrc EQ 0
  AND sy-fdpos EQ 0.
    REPLACE $old_val WITH $new_val INTO fld_name.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SET-SPRKZ                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set-sprkz USING rc LIKE sy-subrc.
  DATA:
    pme01    TYPE pme01.
  rp_provide_from_last p0001  space pn-begda pn-endda.
  MOVE-CORRESPONDING p0001 TO pme01.
  pme01-molga = molga.
  CASE xcedt-prt_lang.
    WHEN 'A'.                          "waehle Sprache des Mitarbeiters.
      rp_provide_from_last p0002  space pn-begda pn-endda.
      sprkz = p0002-sprsl.
*     perform re549d using 'SPRSL' ' ' sprkz-w rc.         "VKIK008541
      CALL FUNCTION 'HR_FEATURE_BACKFIELD'
        EXPORTING
          feature                     = 'SPRSL'
          struc_content               = pme01
          kind_of_error               = space
        IMPORTING
          back                        = sprkz-w
*         STATUS                      =
        EXCEPTIONS
          dummy                       = 1
          error_operation             = 2
          no_backvalue                = 3
          feature_not_generated       = 4
          invalid_sign_in_funid       = 5
          field_in_report_tab_in_pe03 = 6
          OTHERS                      = 7.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*     endif.
*     if rc ne 0.
        sprkz-w = sy-langu.
      ENDIF.
    WHEN 'W'. "waehle Werkssprache aus Tabelle 549D.
*     perform re549d using 'SPRSL' ' ' sprkz-w rc.         "VKIK008541
      CALL FUNCTION 'HR_FEATURE_BACKFIELD'
        EXPORTING
          feature                     = 'SPRSL'
          struc_content               = pme01
          kind_of_error               = space
        IMPORTING
          back                        = sprkz-w
*         STATUS                      =
        EXCEPTIONS
          dummy                       = 1
          error_operation             = 2
          no_backvalue                = 3
          feature_not_generated       = 4
          invalid_sign_in_funid       = 5
          field_in_report_tab_in_pe03 = 6
          OTHERS                      = 7.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*     endif.
*     if rc ne 0.
        sprkz-w = sy-langu.
      ENDIF.
      sprkz = sprkz-w.
  ENDCASE.
  EXPORT sprkz TO MEMORY ID 'PAYSLIP_LANGUAGE'.     "KVHN1044207
ENDFORM.                               "SET-SPRKZ.

*---------------------------------------------------------------------*
*       FORM COPY-RT-TO-RT_2                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($FPBEG)                                                 *
*  -->  VALUE($FPEND)                                                 *
*---------------------------------------------------------------------*
FORM copy-rt-to-rt_2 USING VALUE($fpbeg) VALUE($fpend).    "VKIK131798b
  DATA:
    rt_2_wa   LIKE LINE OF rt_2.
  LOOP AT rt.
*   $RT_2 = RT.                                            "VKIK007690
    MOVE-CORRESPONDING rt TO rt_2_wa.                       "VKIK007690
    rt_2_wa-fpbeg = $fpbeg.
    rt_2_wa-fpend = $fpend.
    rt_2_wa-seqno = rx-key-seqno.             " WLIK027494
    APPEND rt_2_wa TO rt_2.
  ENDLOOP.
ENDFORM.                          "COPY-RT-TO-RT_2.        "VKIK131798b

*---------------------------------------------------------------------*
*       FORM COPY-ORT-TO-RT_2                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($FPBEG)                                                 *
*  -->  VALUE($FPEND)                                                 *
*---------------------------------------------------------------------*
FORM copy-ort-to-rt_2 USING VALUE($fpbeg) VALUE($fpend).   "VKIK131798b
  DATA:
    rt_2_wa   LIKE LINE OF rt_2.
  LOOP AT ort.
*   RT_2 = ORT.                                            "VKIK007690
    MOVE-CORRESPONDING ort TO rt_2_wa.                      "VKIK007690
    rt_2_wa-fpbeg = $fpbeg.
    rt_2_wa-fpend = $fpend.
    rt_2_wa-anzhl = - ort-anzhl.
    rt_2_wa-betrg = - ort-betrg.
    IF rt_2_wa-betrg EQ 0 AND rt_2_wa-anzhl EQ 0.
      rt_2_wa-betpe = 0.
    ELSE.
      rt_2_wa-betpe = - ort-betpe.
    ENDIF.
    rt_2_wa-seqno = orx-key-seqno.             " WLIK027494
*     collect rt_2.                                       "VKIK131798b
    APPEND rt_2_wa TO rt_2.                       "VKIK131798b
  ENDLOOP.
ENDFORM. "COPY-ORT-TO-RT_2                                "VKIK131798b

*---------------------------------------------------------------------*
*       FORM DELETE-FROM-RT_2                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM delete-from-rt_2.
  LOOP AT rt_2
    WHERE anzhl EQ 0
      AND betpe EQ 0
      AND betrg EQ 0.
    rt_2-delet = true.
    MODIFY rt_2.
  ENDLOOP.
ENDFORM.                               "DELETE-FROM-RT_2.

*---------------------------------------------------------------------*
*       FORM DELETE-FROM-XRT                                          *
*---------------------------------------------------------------------*
*       text                                                          *
*---------------------------------------------------------------------*
*       Keine USING-Parameter                                         *
*---------------------------------------------------------------------*
FORM delete-from-xrt.                                       "VKIK112124
* check sl_le is initial.                                   "WXJK014138
  LOOP AT xrt
    WHERE anzhl EQ 0
      AND betpe EQ 0
      AND betrg EQ 0.
    xrt-delet = true.
    MODIFY xrt.
  ENDLOOP.
ENDFORM. "DELETE-FROM-XRT                                   "VKIK112124

*---------------------------------------------------------------------*
*       FORM CORRECT-VERSION                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM correct-version.
ENDFORM.                               "CORRECT-VERSION.

*---------------------------------------------------------------------*
*       FORM REFRESH_PERNR                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM refresh_pernr.
  PERFORM refresh_inw_buffer.                          "INS note 370330
  rueck = space.
  filled-vac = false.
  filled-zk  = false.
  filled-adrs  = false.
  filled-adrsp = false.                                     "VKIK051178
  pagpn = 1.                           "VKIK131798C
  CLEAR vac. REFRESH vac.
  CLEAR zk. REFRESH zk.
  CLEAR adrs.
  CLEAR f0-key.
  CLEAR xinfo.
  CLEAR xform. REFRESH xform.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM REFRESH_PERIOD                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM refresh_period.
  CLEAR: zrt. REFRESH: zrt.
  CLEAR: xrt. REFRESH: xrt.
  CLEAR: txt. REFRESH: txt.
* CLEAR: CLINE, ELINE, LPAGE, SW, RTO, OXRT_WA.             "VKIK016989
  CLEAR: cline, eline, lpage, sw, oxrt_wa.       "VKIK016989"VKIK011129
  CLEAR: hx.
  CLEAR: rueck.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FILL_PER_PERNR                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM fill_per_pernr.
  DATA:
    rc        LIKE sy-subrc.
  PERFORM set-sprkz USING rc.
  IF switch-f_adrs = true.
    PERFORM fill_adrs USING filled-adrs adrs.
  ENDIF.
*  IF SWITCH-F_ADRSP = TRUE.
*    PERFORM FILL_ADRSP USING FILLED-ADRSP ADRSP.
*  ENDIF.
  IF switch-p_zk_q EQ true
  OR switch-p_zk_t EQ true
  OR switch-p_zk_x EQ true                                  "HKUK032455
  OR switch-p_zk_s EQ true.                                 "HKUK032455
    PERFORM fill-zk-mod.               "fill table ZK by customer
    PERFORM fill-zk-natio.             "fill table ZK country-dependent
    PERFORM fill-zk-standard.          "fill table ZK standard
  ENDIF.
  IF switch-p_vac EQ true
  OR switch-f_vac EQ true.
    PERFORM fill-vac.                                       "HKUK016696
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SPLIT_NAME                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TNAME                                                         *
*  -->  FNAME                                                         *
*  -->  VALUE($ORIGINAL)                                              *
*---------------------------------------------------------------------*
FORM split_name USING tname
                      fname
                      VALUE($original).
  DATA:
    max  LIKE sy-index,
    cnt  LIKE sy-index,
    itab TYPE t_str255 OCCURS 0 WITH HEADER LINE.
  CLEAR tname.
  IF $original NE TEXT-apo.
    SPLIT $original AT c_hyphen INTO TABLE itab.
    DESCRIBE TABLE itab LINES max.
    cnt = max - 1.                     "ignore field name
    DO cnt TIMES.
      READ TABLE itab INDEX sy-index.
      IF sy-index GT 1.
        CONCATENATE tname c_hyphen itab INTO tname.
      ELSE.
        CONCATENATE tname itab INTO tname.
      ENDIF.
    ENDDO.
    READ TABLE itab INDEX max.
    fname = itab.
  ELSE.
    fname = $original.
  ENDIF.
ENDFORM.                               "SPLIT_NAME

*---------------------------------------------------------------------*
*       FORM CONCAT_NAME                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  STR                                                           *
*  -->  VALUE($TNAME)                                                 *
*  -->  VALUE($FNAME)                                                 *
*---------------------------------------------------------------------*
FORM concat_name
  USING strng VALUE($tname) VALUE($fname).
  IF $tname IS INITIAL                 "Table name missing?
  OR $tname EQ 'SCHLW'.                                     "
    strng = $fname.                    "Direktwert
  ELSE.
    CONCATENATE $tname c_hyphen $fname INTO strng.
  ENDIF.
ENDFORM.                               "CONCAT_NAME

*---------------------------------------------------------------------*
*       FORM FILL_EVP                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  EVP                                                           *
*  -->  VALUE($PAPER)                                                 *
*  -->  VALUE($PERMO)                                                 *
*  -->  VALUE($PAYTY)                                                 *
*  -->  VALUE($PAYID)                                                 *
*  -->  VALUE($BONDT)                                                 *
*  -->  $RC                                                           *
*---------------------------------------------------------------------*
FORM fill_evp
  TABLES eval_results TYPE pay_t_eval_period          "WOG note 760945
         iabkrs TYPE hrpy_tt_abkrs                          "VKIK016676
  USING VALUE($paper) VALUE($permo) VALUE($payty) VALUE($payid)
    VALUE($bondt) VALUE($mc_incl) $rc.                      "VKIK032705
  DATA:
    rgdir_line   TYPE pc261.

  rgdir_line-bondt = $bondt.
  rgdir_line-inper = $paper.
  rgdir_line-iperm = $permo.
  rgdir_line-payty = $payty.
  rgdir_line-payid = $payid.
  rgdir_line-void  = cd_c-void_false.
  CALL FUNCTION 'HRO1_FILL_EVAL_DIR'
    EXPORTING
      payroll_dir          = rgdir[]
      rgdir_line           = rgdir_line
      iabkrs               = iabkrs[]                "VKIK016676
      all_results_of_run   = $mc_incl                "VKIK032705
    IMPORTING
*     EVAL_DIR             = EVP[]                "WOG note 760945
      eval_tab             = eval_results[]             "!
      payr_dir             = rgdir[]
    EXCEPTIONS
      no_evaluated_periods = 1
      OTHERS               = 2.
  $rc = sy-subrc.
  IF $rc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                               "FILL_EVP

*---------------------------------------------------------------------*
*       FORM FILL_EVP_LL                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  EVP                                                           *
*  -->  $RC                                                           *
*---------------------------------------------------------------------*
FORM fill_evp_ll                                            "WXJK014138
  TABLES eval_results TYPE pay_t_eval_period           "WOG note 760945
  USING $rc.

  DATA: evp TYPE pc261.                                "WOG note 760945
  FIELD-SYMBOLS <wa_eval_results> TYPE pay_eval_period.        "!

  CHECK switch-p_lle EQ true
     OR switch-p_llg EQ true.
  APPEND INITIAL LINE TO eval_results ASSIGNING <wa_eval_results>.
  CLEAR: $rc, evp.

  MOVE pn-permo TO: evp-iperm,      evp-permo.
  MOVE pn-pabrj TO: evp-inper+0(4), evp-fpper+0(4).
  MOVE pn-pabrp TO: evp-inper+4(2), evp-fpper+4(2).

  evp-fpbeg = pn-begda.
  evp-fpend = pn-endda.

  evp-srtza = 'A'.                     "Abrechnung
  APPEND evp TO <wa_eval_results>-evp.                 "WOG note 760945
ENDFORM.                               "FILL_EVP_LL

*---------------------------------------------------------------------*
*       FORM ADD_PROTOCOL                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM add_protocol.
  CALL FUNCTION 'HR_APPEND_ERROR_LIST'
    EXPORTING
      arbgb = '3R'
      msgty = c_msgty-info
      msgno = 313
      msgv1 = stat-selpn.
  CALL FUNCTION 'HR_APPEND_ERROR_LIST'
    EXPORTING
      arbgb = '3R'
      msgty = c_msgty-info
      msgno = 314
      msgv1 = stat-lfdno.
  IF xcedt-chk_ess EQ true.                  " WLIK055667
    CALL FUNCTION 'HR_APPEND_ERROR_LIST'
      EXPORTING
        arbgb = '3R'
        msgty = c_msgty-info
        msgno = 320
        msgv1 = stat-skip_ess.
  ENDIF.
  CALL FUNCTION 'HR_APPEND_ERROR_LIST'
    EXPORTING
      arbgb = '3R'
      msgty = c_msgty-info
      msgno = 315
      msgv1 = stat-frmno.
  CALL FUNCTION 'HR_APPEND_ERROR_LIST'
    EXPORTING
      arbgb = '3R'
      msgty = c_msgty-info
      msgno = 316
      msgv1 = stat-pagno.
  EXPORT per_sel = stat-selpn per_prnt = stat-lfdno fmt_form = stat-frmno
         fmt_page = stat-pagno skip_ess = stat-skip_ess TO MEMORY ID 'CEDT_MUL_PERIOD'.
ENDFORM.

*FORM EXTCALL USING                                         "VKIK057197
*             VALUE($PERNR)
*             VALUE($IABKR)
*             VALUE($IAPER)
*             VALUE($FORMULAR)
*             VALUE($ANDRUCK)
*             VALUE($RUECKD)
*             VALUE($RUECKR)
*             VALUE($SPRACHE)
*             VALUE($ONLY)
*             VALUE($PRINT).
*  DATA:
*    RC        LIKE SY-SUBRC.
** not longer supported!                                    "VKIK057197
*  rc = 12.                                                 "VKIK057197
*ENDFORM.                               "EXTCALL

*---------------------------------------------------------------------*
*       FORM XEDT_GET_EXIST                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  EXIST                                                         *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_get_exist USING exist LIKE boolean
                          rc LIKE sy-subrc.
  exist = true.
  CLEAR rc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_SET_EXP2MEM                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(SWITCH)        export form to memory?                   *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_set_exp2mem USING VALUE(switch) LIKE boolean
                            rc LIKE sy-subrc.
  exp_frm = switch.
  CLEAR rc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_SET_COMPRESS                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(SWITCH)        compress form?                           *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_set_compress USING VALUE(switch) LIKE boolean
                             rc LIKE sy-subrc.
  compress = switch.
  CLEAR rc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_SET_SELDATE                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($BEGDA)                                                 *
*  -->  VALUE($ENDDA)                                                 *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_set_seldate USING VALUE($begda) LIKE pn-begda     "VKIK031819
                            VALUE($endda) LIKE pn-endda
                            rc LIKE sy-subrc.
  pn-begda = $begda.
  pn-endda = $endda.
  CLEAR rc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_SET_REPORTCLASS                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($RCLAS)                                                 *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_set_reportclass USING VALUE($rclas)               "VKIK076816
                                rc LIKE sy-subrc.
  rclas = $rclas.
  CLEAR rc.
ENDFORM.                                                    "VKIK076816

*---------------------------------------------------------------------*
*       FORM XEDT_SET_FIELDS                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($FLDS)                                                  *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_set_fields USING VALUE($flds) TYPE tt_fld         "VKIK009589
                           rc LIKE sy-subrc.
  DATA:
    wa        TYPE tr_fld.
  FIELD-SYMBOLS:
    <fld>.
  CLEAR rc.
  LOOP AT $flds INTO wa.
    ASSIGN (wa-name) TO <fld>.
    IF sy-subrc EQ 0. "field found?
      <fld> = wa-value.
    ELSE.
      rc = sy-subrc.
    ENDIF.
  ENDLOOP.
ENDFORM.                                                    "VKIK009589

*---------------------------------------------------------------------*
*       FORM XEDT_INIT_PARAMETERS                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PARM_EDT                                                      *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_init_parameters USING parm_edt STRUCTURE xcedt
                                rc LIKE sy-subrc.
  PERFORM init_parameters USING rc.
* disable check in any case - main program has to do it by itself
  chk_ess = false.                                          "VKIK023668
* disable protocol in any case - main program has to do it by itself
  prt_prot = false.                                         "VKIK083918
  PERFORM get_parameters USING parm_edt rc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_GET_PARAMETERS                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VARIANT    name of a variant                                  *
*  -->  PARM_EDT                                                      *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_get_parameters USING variant LIKE rsvar-variant
                               parm_edt STRUCTURE xcedt
                               rc LIKE sy-subrc.
  DATA:
    repid    LIKE sy-repid.
  PERFORM init_parameters USING rc.
  IF NOT variant IS INITIAL.
    repid = sy-repid.
    CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
      EXPORTING
        report               = repid
        variant              = variant
      EXCEPTIONS
        variant_not_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
* disable check in any case - main program has to do it by itself
  chk_ess = false.                                          "VKIK023668
* disable protocol in any case - main program has to do it by itself
  prt_prot = false.                                         "VKIK083918
  PERFORM get_parameters USING parm_edt rc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_SET_PAYROLL_KIND                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($IABKR)                                                 *
*  -->  VALUE($IAPER)                                                 *
*  -->  VALUE($PAYTY)                                                 *
*  -->  VALUE($PAYID)                                                 *
*  -->  VALUE($BONDT)                                                 *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_set_payroll_kind USING VALUE($iabkr)              "VKIK031819
                                 VALUE($iaper)
                                 VALUE($payty)
                                 VALUE($payid)
                                 VALUE($bondt)
                                 VALUE($mc_incl)            "VKIK032705
                                 rc LIKE sy-subrc.
  SELECT SINGLE * FROM t549a WHERE abkrs EQ $iabkr.
  rc = sy-subrc.
  pn-permo = t549a-permo.
  pn-paper    = $iaper.
  pn-pabrj = pn-paper(4).
  pn-pabrp = pn-paper+4(2).
  payty = $payty.
  payid = $payid.
  bondt = $bondt.
  mc_incl = $mc_incl.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_SET_PAYROLL_AREA                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($IABKR)                                                 *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_set_payroll_area USING VALUE($abkrs) TYPE hrpy_tt_abkrs
                                 rc LIKE sy-subrc.          "VKIK016676
  CLEAR rc.
  pnpabkrs[] = $abkrs.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_INIT                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_init USING rc LIKE sy-subrc.
  PERFORM init_initialization.                              "VKIK010617
  PERFORM init_sos USING rc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_INIT_PRI                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($DIALOG)       display popup for print parameters?      *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_init_pri USING VALUE($dialog) LIKE boolean        "VKIK057197
                         rc LIKE sy-subrc.
* not longer supported! coding removed
  rc = 12.
ENDFORM.                                                    "VKIK057197

*---------------------------------------------------------------------*
*       FORM XEDT_PRI_ON                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_pri_on USING rc LIKE sy-subrc.                    "VKIK057197
* not longer supported! coding removed
  rc = 12.
ENDFORM.                                                    "VKIK057197

*---------------------------------------------------------------------*
*       FORM XEDT_PRI_OFF                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_pri_off USING rc LIKE sy-subrc.                   "VKIK057197
* not longer supported! coding removed
  rc = 12.
ENDFORM.                                                    "VKIK057197

*---------------------------------------------------------------------*
*       FORM XEDT_INIT_FORM                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(PARM_EDT)                                               *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_init_form USING VALUE(parm_edt) STRUCTURE xcedt
                          rc LIKE sy-subrc.
  DATA:
    progname LIKE sy-repid,
    subrc    LIKE sy-subrc.
  xcedt = parm_edt.
  formular = xcedt-prt_form.           "wegen T512N-lesen
  PERFORM check_parameter USING parm_edt rc.
  IF rc EQ 0.
    PERFORM init_form USING xcedt rc.
    progname = sy-repid.
    CALL FUNCTION 'RS_INFOTYPE_LIST'
      EXPORTING
        program                     = progname
      TABLES
        infotypes                   = fvar-infty_tab
      EXCEPTIONS
        program_cannot_be_generated = 1
        program_not_found           = 2
        OTHERS                      = 3.
    PERFORM modify_infotypes USING fvar-infty_tab.
    PERFORM export_formdef_to_memory USING molga xcedt-prt_form subrc.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_SELECT_FORM                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(PARM_EDT)                                               *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_select_form USING VALUE(parm_edt) STRUCTURE xcedt
                            rc LIKE sy-subrc.
  DATA:
    subrc     LIKE sy-subrc.
* XCEDT = PARM_EDT.                    "using old values
  formular = parm_edt-prt_form.        "wegen T512N-lesen
  PERFORM import_formdef_from_memory
    USING molga parm_edt-prt_form subrc.
  IF subrc NE 0.
    PERFORM xedt_init_form USING parm_edt rc.
  ENDIF.
  xcedt = parm_edt.                    "using actual values
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_GET_INFOTYPES                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xedt_get_infotypes USING itypes TYPE hrift_tt_lst      "VKIK000212
                              rc LIKE sy-subrc.
  CLEAR rc.
  itypes = fvar-infty_tab.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_SET_INFOTYPES                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xedt_set_infotypes USING VALUE(itypes) TYPE hrift_tt_cont
                              rc LIKE sy-subrc.             "VKIK000212
  DATA:
    ifty_name LIKE rsitype-name,
    wa_infty  LIKE rsitypelst,
    wa_itypes TYPE hrift_ts_cont.
  FIELD-SYMBOLS:
    <wa_it> TYPE table,
    <ift>.
  CLEAR: rc.
  LOOP AT itypes INTO wa_itypes.
    LOOP AT fvar-infty_tab INTO wa_infty
      WHERE number EQ wa_itypes-infty-number.
      CONCATENATE wa_infty-name '[]' INTO ifty_name.
      CONDENSE ifty_name NO-GAPS.
      ASSIGN (ifty_name) TO <ift>.
      IF sy-subrc EQ 0.
        ASSIGN wa_itypes-data TO <wa_it>.
*        <IFT> = <WA_IT>.         "UC
        CALL METHOD cl_hr_pnnnn_type_cast=>prelp_to_pnnnn_tab  "UC
          EXPORTING
            prelp_tab = <wa_it>
          IMPORTING
            pnnnn_tab = <ift>.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_INIT_BUFFER                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xedt_init_buffer USING rc LIKE sy-subrc.               "VKIK016138
  CLEAR: rc.
  rp-init-buffer.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_SET_BUFFER                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xedt_set_buffer USING buffer TYPE hrpay_buffer         "XDOK065442
                           rc LIKE sy-subrc.
  CLEAR: rc.
  CLEAR: tbuff,
         buffer_dir,
         del_pclx_tab,
         before_image_pclx,
         payr_buffer.

  tbuff[]             = buffer-tbuff[].
  buffer_dir[]        = buffer-buffer_dir[].
  del_pclx_tab[]      = buffer-del_pclx_tab[].
  before_image_pclx[] = buffer-before_image_pclx[].
  payr_buffer[]       = buffer-payr_buffer[].
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_CREATE_FORM_1                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($PERNR)                                                 *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_create_form_1 USING VALUE($pernr) LIKE pernr-pernr
                              rc LIKE sy-subrc.
  DATA:
    subrc     LIKE sy-subrc.
  CLEAR rc.
  pernr-pernr = $pernr.                "global value
  xcedt-chk_ess = false.       "ext. call process WLIK055667
  PERFORM process_pernr                                     "VKIK032705
    USING pernr-pernr pn-paper pn-permo payty payid bondt mc_incl
      pnpabkrs[] subrc.                                     "VKIK016676
  PERFORM compress_xform USING compress.
  IF exp_frm EQ false.
*   perform print_xform using xform[].
  ELSE.
    PERFORM export_form_to_memory USING exp_frm $pernr 0 rc.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_CREATE_FORM_2                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  $EVP                 directory with evaluation periods        *
*  -->  $RGDIR               global directory                         *
*  -->  VALUE($PERNR)                                                 *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_create_form_2 TABLES $evp   STRUCTURE pc261       "VKIK071508
                               $rgdir STRUCTURE pc261       "VKIK071508
                        USING VALUE($pernr) LIKE pernr-pernr
                              rc LIKE sy-subrc.
  DATA: ls_evp          LIKE evp.
  CLEAR rc.
  pernr-pernr = $pernr.                "global value
  CLEAR rt_2.
  REFRESH rt_2.
  PERFORM refresh_pernr.
  PERFORM refresh_pernr_natio.
  PERFORM refresh_pernr_mod.
  PERFORM fill_per_pernr.                                   "VKIK031819
  rgdir[] = $rgdir[].                                       "VKIK071508
  IF xcedt-cur_ip IS NOT INITIAL. "To Handle Inperiod Currency Issue
    READ TABLE rgdir INTO ls_evp WITH KEY inper = pn-paper
                                          fpper = pn-paper.
    IF sy-subrc = 0.
      PERFORM import_actual USING ls_evp rc.
      ip_curr = versc-waers.
    ENDIF.
  ENDIF.
  PERFORM process_evp
    TABLES $evp.                                            "VKIK071508
  PERFORM compress_xform USING compress.
  IF exp_frm EQ false.
*   perform print_xform using xform[].
  ELSE.
    PERFORM export_form_to_memory USING exp_frm $pernr 0 rc.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_CREATE_FORM_3                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($PERNR)                                                 *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_create_form_3 USING VALUE($pernr) LIKE pernr-pernr
                              rc LIKE sy-subrc.
  CLEAR rc.
  pernr-pernr = $pernr.
  CLEAR rt_2.
  REFRESH rt_2.
  PERFORM refresh_pernr.
  PERFORM refresh_pernr_natio.
  PERFORM refresh_pernr_mod.
  PERFORM fill_per_pernr.                                   "VKIK031819
  PERFORM refresh_period.
  PERFORM refresh_period_natio.
  PERFORM refresh_period_mod.
  PERFORM edit_period
    USING xcedt-typ_rcal false versc-fpbeg versc-fpend.
  PERFORM compress_xform USING compress.
  IF exp_frm EQ false.
*   perform print_xform using xform[].
  ELSE.
    PERFORM export_form_to_memory USING exp_frm $pernr 0 rc.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_CREATE_FORM_4                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($PERNR)                                                 *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_create_form_4 USING VALUE($pernr) LIKE pernr-pernr
                              rc LIKE sy-subrc.
  CLEAR rc.
  pernr-pernr = $pernr.
  PERFORM refresh_pernr.
  PERFORM refresh_pernr_natio.
  PERFORM refresh_pernr_mod.
  PERFORM fill_per_pernr.                                   "VKIK031819
  PERFORM refresh_period.
  PERFORM refresh_period_natio.
  PERFORM refresh_period_mod.
  PERFORM edit_period
    USING xcedt-typ_rcal false versc-fpbeg versc-fpend.
  PERFORM compress_xform USING compress.
  IF exp_frm EQ false.
*   perform print_xform using xform[].
  ELSE.
    PERFORM export_form_to_memory USING exp_frm $pernr 0 rc.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_CREATE_TESTFORM                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($PERNR)                                                 *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_create_testform USING VALUE(molga)                "HKUK041440
                                $xcedt STRUCTURE xcedt
                                rc LIKE sy-subrc.

  PERFORM create_testform USING $xcedt.
  PERFORM export_form_to_memory
    USING true '12345678' 0 rc.

ENDFORM.                                                    "HKUK041440

*---------------------------------------------------------------------*
*       FORM XEDT_GET_FORM                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  <--  $FORM                table with edited form                   *
*  <--  $INFO                form related informations                *
*  <--  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_get_form USING $form TYPE tt_form
                         $info LIKE pc407
                         rc LIKE sy-subrc.
  CLEAR rc.
  PERFORM get_xinfo USING $info.                            "VKIK061490
  $form = xform[].
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_PRINT_FORM                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  $FORM                table with edited form                   *
*  -->  $INFO                form related informations                *
*  <--  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_print_form USING VALUE($form) TYPE tt_form
                           VALUE($info) LIKE pc407
                           rc LIKE sy-subrc.
  CLEAR rc.
  DATA:
    $form_wa  LIKE pc408.
  SET BLANK LINES ON.
  LOOP AT $form INTO $form_wa.
    CASE $form_wa-ltype.
      WHEN f__ltype-cmd.
        IF $form_wa-linda EQ f__cmd-newpage.
          NEW-PAGE.
        ENDIF.
      WHEN f__ltype-txt.
        WRITE: / $form_wa-linda.
    ENDCASE.
  ENDLOOP.
  SET BLANK LINES OFF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_GET_MESSAGES                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  <--  $MSGTAB              table with messages                      *
*  <--  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_get_messages USING msgtab TYPE tt_msgtab
                             rc LIKE sy-subrc.
  CLEAR rc.
  CALL FUNCTION 'HR_GET_ERROR_LIST'
    TABLES
      error     = msgtab
*     errortexts =
    EXCEPTIONS
      no_errors = 1
      OTHERS    = 2.
  rc = sy-subrc.
  IF rc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.

* special routines for summary
*---------------------------------------------------------------------*
*       FORM XEDT_INIT_SUMLEVEL                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($SUMLV)        actual sum level                         *
*  <--  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_init_sumlevel USING VALUE($sumlv)
                              rc LIKE sy-subrc.
  sumlv_actual = $sumlv.
  CLEAR rc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_ADD_SUMTBL                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($SRCLV)        source level                             *
*  -->  VALUE($DSTLV)        destination level                        *
*  <--  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_add_sumtbl USING VALUE($srclv)
                           VALUE($dstlv)
                           rc LIKE sy-subrc.
  PERFORM add_to_sumtbl USING $srclv $dstlv rc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_FILL_RESULTS_FROM_SUMTBL                            *
*---------------------------------------------------------------------*
*       restore cluster tables from sum table                         *
*---------------------------------------------------------------------*
*  -->  VALUE($SUMLV)        actual sum level                         *
*  <--  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_fill_results_from_sumtbl USING VALUE($sumlv)
                                         rc LIKE sy-subrc.
  PERFORM fill_results_from_sumtbl USING $sumlv rc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XEDT_DELETE_FROM_SUMTBL                                  *
*---------------------------------------------------------------------*
*       delete sum level from sum table                               *
*---------------------------------------------------------------------*
*  -->  VALUE($SUMLV)        actual sum level                         *
*  <--  RC                                                            *
*---------------------------------------------------------------------*
FORM xedt_delete_from_sumtbl USING VALUE($sumlv)
                                   rc LIKE sy-subrc.
  PERFORM delete_from_sumtbl USING $sumlv rc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM EXPORT_FORM_TO_MEMORY                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($EXP_FRM)      export edited form to memory?            *
*  -->  VALUE($PERNR)        pers.no.                                 *
*  -->  VALUE($SEQNR)        seq.no.                                  *
*  <--  RC                                                            *
*---------------------------------------------------------------------*
FORM export_form_to_memory USING VALUE($exp_frm) LIKE boolean
                                 VALUE($pernr) LIKE pernr-pernr
                                 VALUE($seqnr)
                                 rc LIKE sy-subrc.
  CHECK $exp_frm EQ true.
  f0-key-pernr = $pernr.
  UNPACK $seqnr TO f0-key-seqno.
  PERFORM get_xinfo USING xinfo.                            "VKIK061490
  rp-exp-cm-f0.
  rc = sy-subrc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM EXPORT_FORMDEF_TO_MEMORY                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($MOLGA)                                                 *
*  -->  VALUE($FORML)                                                 *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM export_formdef_to_memory USING VALUE($molga)
                                    VALUE($forml)
                                    rc LIKE sy-subrc.
  fm-key-molga = $molga.
  fm-key-forml = $forml.
  EXPORT
    xcedt
    i512d
    i512e
    i512f
    i512g
    i512n
    i512p
    i512q
    i512s                                                   "VKIK080595
    i514d                                                   "VKIK061490
    switch
    switch_natio
    switch_mod
    fvar
    fvar_natio
    fvar_mod
*   sl_le                                                   "WXJK014138
  TO MEMORY
  ID fm-key.
  rc = sy-subrc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM IMPORT_FORMDEF_FROM_MEMORY                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE($MOLGA)                                                 *
*  -->  VALUE($FORML)                                                 *
*  <--  RC                                                            *
*---------------------------------------------------------------------*
FORM import_formdef_from_memory USING VALUE($molga)
                                      VALUE($forml)
                                      rc LIKE sy-subrc.
  fm-key-molga = $molga.
  fm-key-forml = $forml.
  IMPORT
    xcedt
    i512d
    i512e
    i512f
    i512g
    i512n
    i512p
    i512q
    i512s                                                   "VKIK080595
    i514d                                                   "VKIK061490
    switch
    switch_natio
    switch_mod
    fvar
    fvar_natio
    fvar_mod
*   sl_le                                                   "WXJK014138
  FROM MEMORY
  ID fm-key.
  rc = sy-subrc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM INIT_FORMDEF                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM init_formdef.
* CLEAR XCEDT.
  CLEAR i512d.           REFRESH i512d.
  CLEAR i512e_zk.        REFRESH i512e_zk.                  "HKUK008843
  CLEAR i512e.           REFRESH i512e.
  CLEAR i512f.           REFRESH i512f.
  CLEAR i512g.           REFRESH i512g.
  CLEAR i512n.           REFRESH i512n.
  CLEAR i512p.           REFRESH i512p.
  CLEAR i512q.           REFRESH i512q.
  CLEAR i512s.           REFRESH i512s.                     "VKIK080595
  CLEAR i514d.                                              "VKIK061490
  CLEAR switch.
  CLEAR switch_natio.
  CLEAR switch_mod.
  CLEAR fvar.                                               "VKIK031819
  CLEAR fvar_natio.                                         "VKIK031819
  CLEAR fvar_mod.                                           "VKIK031819
* clear sl_le.                                              "WXJK014138
ENDFORM.

FORM get_xinfo USING $info LIKE pc407.                      "VKIK061490
  $info-molga = molga.
  $info-forml = formular.
  $info-pcols = i514d-c_use.
  $info-psize = i514d-r_use.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM FILL_ADRSP_FOR_PERIOD                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FILLED                                                        *
*  -->  ADRSP                                                         *
*---------------------------------------------------------------------*
FORM fill_adrsp_for_period USING adrsp LIKE adrs.
  IF switch-f_adrsp = true.
    rp_provide_from_last p0001  space versc-fpbeg versc-fpend.
    PERFORM re500p USING p0001-werks.
    MOVE-CORRESPONDING t500p TO adrsp.
    PERFORM re001 USING p0001-bukrs.
    adrsp-inlnd = t001-land1.
    adrsp-anzzl = fvar-adrsp_lines.
    CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
      EXPORTING
        adrswa_in  = adrsp
      IMPORTING
        adrswa_out = adrsp
      EXCEPTIONS
        OTHERS     = 1.
  ENDIF.
ENDFORM.                               "FILL_ADRSP_FOR_PERIOD
"KVHA - To Handle Splits
*&---------------------------------------------------------------------*
*&      Form  SAVED_SPLITS_TO_TABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM saved_splits_to_tables_o.
  DATA: repna LIKE t596f-pgmna,
        rc    LIKE sy-subrc.

  oversc   = saved_oversc.
  oabc[]   = saved_oabc[].
  ort[]    = saved_ort[].
  ocrt[]   = saved_ocrt[].
  oc0[]    = saved_oc0[].
  ov0[]    = saved_ov0[].
  ovcp[]   = saved_ovcp[].
  odft[]   = saved_odft[].
  ogrt[]   = saved_ogrt[].
  ols[]    = saved_ols[].
  ostatus  = saved_ostatus.
  oarrrs[] = saved_oarrrs[].
  oddntk[] = saved_oddntk[].
  oaccr[]  = saved_oaccr[].
  obentab[] = saved_obentab[].
  ofund[]  = saved_ofund[].
  oaverage[] = saved_oaverage[].
  omodif[] = saved_omodif[].
  ocodist[] = saved_ocodist[].
  olifl[]  = saved_olifl[].
  olidi[]  = saved_olidi[].
  owpbp[]  = saved_owpbp[].
  oc1[]    = saved_oc1[].
  oalp[]   = saved_oalp[].
  oab[]    = saved_oab[].
  obt[]    = saved_obt[].
  CLEAR saved_oversc.
  CLEAR saved_oabc[].REFRESH saved_oabc.
  CLEAR saved_ort[].REFRESH saved_ort.
  CLEAR saved_ocrt[].REFRESH saved_ocrt.
  CLEAR saved_oc0[].REFRESH saved_oc0.
  CLEAR saved_ov0[].REFRESH saved_ov0.
  CLEAR saved_ovcp[].REFRESH saved_ovcp.
  CLEAR saved_odft[].REFRESH saved_odft.
  CLEAR saved_ogrt[].REFRESH saved_ogrt.
  CLEAR saved_ols[].REFRESH saved_ols.
  CLEAR saved_ostatus.
  CLEAR saved_oarrrs[].REFRESH saved_oarrrs.
  CLEAR saved_oddntk[].REFRESH saved_oddntk.
  CLEAR saved_oaccr[].REFRESH saved_oaccr.
  CLEAR saved_obentab[].REFRESH saved_obentab.
  CLEAR saved_ofund[].REFRESH saved_ofund.
  CLEAR saved_oaverage[].REFRESH saved_oaverage.
  CLEAR saved_omodif[].REFRESH saved_omodif.
  CLEAR saved_ocodist[].REFRESH saved_ocodist.
  CLEAR saved_olifl[].REFRESH saved_olifl.
  CLEAR saved_olidi[].REFRESH saved_olidi.
  CLEAR saved_owpbp[].REFRESH saved_owpbp.
  CLEAR saved_oc1[].REFRESH saved_oc1.
  CLEAR saved_oalp[].REFRESH saved_oalp.
  CLEAR saved_oab[].REFRESH saved_oab.
  CLEAR saved_obt[].REFRESH saved_obt.
  PERFORM re596f_xedt USING molga repna rc.
  IF rc EQ 0 AND repna IS NOT INITIAL.
    PERFORM saved_splits_to_tables_o_natio IN PROGRAM (repna) IF FOUND.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVE_SPLIT_RESULTS_O
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OUT_RGDIR  text
*----------------------------------------------------------------------*
FORM save_split_results_o  USING    $evp TYPE pc261.
  DATA: ls_components TYPE abap_compdescr,
        lo_strucdescr TYPE REF TO cl_abap_structdescr.
  DATA: lo_pay_access TYPE REF TO cl_hr_pay_access,
        lo_pay_result TYPE REF TO cl_hr_pay_result,
        repna         LIKE t596f-pgmna,
        rc            LIKE sy-subrc.

  CREATE OBJECT lo_pay_access.
  CALL METHOD lo_pay_access->read_pa_result
    EXPORTING
      pernr          = pernr-pernr
      period         = $evp
    IMPORTING
      payroll_result = lo_pay_result.
  lo_strucdescr ?= cl_abap_typedescr=>describe_by_data( lo_pay_result->inter ).
  LOOP AT lo_strucdescr->components INTO ls_components.
    CASE ls_components-name.
      WHEN 'VERSC'.
        saved_oversc = lo_pay_result->inter-versc.
      WHEN 'ABC'.
        APPEND LINES OF lo_pay_result->inter-abc TO saved_oabc.
      WHEN 'RT'.
        APPEND LINES OF lo_pay_result->inter-rt TO saved_ort.
      WHEN 'CRT'.
        APPEND LINES OF lo_pay_result->inter-crt TO saved_ocrt.
      WHEN 'C0'.
        APPEND LINES OF lo_pay_result->inter-c0 TO saved_oc0.
      WHEN 'V0'.
        APPEND LINES OF lo_pay_result->inter-v0 TO saved_ov0.
      WHEN 'VCP'.
        APPEND LINES OF lo_pay_result->inter-vcp TO saved_ovcp.
      WHEN 'DFT'.
        APPEND LINES OF lo_pay_result->inter-dft TO saved_odft.
      WHEN 'GRT'.
        APPEND LINES OF lo_pay_result->inter-grt TO saved_ogrt.
      WHEN 'LS'.
        APPEND LINES OF lo_pay_result->inter-ls TO saved_ols.
      WHEN 'STATUS'.
        saved_ostatus = lo_pay_result->inter-status.                         .
      WHEN 'ARRRS'.
        APPEND LINES OF lo_pay_result->inter-arrrs TO saved_oarrrs.
      WHEN 'DDNTK'.
        APPEND LINES OF lo_pay_result->inter-ddntk TO saved_oddntk.
      WHEN 'ACCR'.
        APPEND LINES OF lo_pay_result->inter-accr TO saved_oaccr.
      WHEN 'BENTAB'.
        APPEND LINES OF lo_pay_result->inter-bentab TO saved_obentab.
      WHEN 'FUND'.
        APPEND LINES OF lo_pay_result->inter-fund TO saved_ofund.
      WHEN 'AVERAGE'.
        APPEND LINES OF lo_pay_result->inter-average TO saved_oaverage.
      WHEN 'MODIF'.
        APPEND LINES OF lo_pay_result->inter-modif TO saved_omodif.
      WHEN 'CODIST'.
        APPEND LINES OF lo_pay_result->inter-codist TO saved_ocodist.
      WHEN 'LIFL'.
        APPEND LINES OF lo_pay_result->inter-lifl TO saved_olifl.
      WHEN 'LIDI'.
        APPEND LINES OF lo_pay_result->inter-lidi TO saved_olidi.
      WHEN 'WPBP'.
        APPEND LINES OF lo_pay_result->inter-wpbp TO saved_owpbp.
      WHEN 'C1'.
        APPEND LINES OF lo_pay_result->inter-c1 TO saved_oc1.
      WHEN 'ALP'.
        APPEND LINES OF lo_pay_result->inter-alp TO saved_oalp.
      WHEN 'AB'.
        APPEND LINES OF lo_pay_result->inter-ab TO saved_oab.
      WHEN 'BT'.
        APPEND LINES OF lo_pay_result->inter-bt TO saved_obt.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.
  PERFORM re596f_xedt USING molga repna rc.
  IF rc EQ 0 AND repna IS NOT INITIAL.
    PERFORM save_split_results_o_natio IN PROGRAM (repna)
                             USING lo_pay_result IF FOUND.
  ENDIF.
ENDFORM.
* internal routine (do not use!)
FORM re596f_xedt USING VALUE(molga) LIKE t500l-molga
                       repna LIKE t596f-pgmna
                       rc LIKE sy-subrc.
  STATICS:
    xedt_exist TYPE c,
    i596f      LIKE t596f.
  DATA:
    sname     LIKE t596f-sname    VALUE 'XEDT00'.
  sname+4(2) = molga.
  IF i596f-sname NE sname.
    PERFORM re596f_spezial USING sname i596f.
    CLEAR xedt_exist.
    PERFORM xedt_get_exist IN PROGRAM (i596f-pgmna)
      USING xedt_exist rc
      IF FOUND.
  ENDIF.
  repna = i596f-pgmna.
  IF xedt_exist IS INITIAL.            "ext. call not possible
    rc = 99.
  ENDIF.
ENDFORM.
* internal routine (do not use!)
FORM re596f_spezial USING VALUE(sname) LIKE t596f-sname
                          i596f LIKE t596f.
  CALL FUNCTION 'HRCCE_GET_REPORTNAME'
    EXPORTING
      smodn  = sname
*     DATE   = SY-DATUM
    IMPORTING
      i596f  = i596f
    EXCEPTIONS
*     NO_ENTRY_IN_TABLE       = 1
      OTHERS = 2.

  IF sy-subrc NE 0.
    CLEAR i596f.
  ENDIF.

ENDFORM.
"KVHA-To Handle Splits - Ends Here

**&---------------------------------------------------------------------*
**&      Form  GET_XINFO
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->$INFO      text
**----------------------------------------------------------------------*
*FORM get_xinfo USING $info LIKE pc407.                      "VKIK061490
*  $info-molga = molga.
*  $info-forml = formular.
*  $info-pcols = i514d-c_use.
*  $info-psize = i514d-r_use.
*ENDFORM.                    "GET_XINFO

INCLUDE rpcf4c00.                                           "VKIK045853

*---------------------------------------------------------------------*
*       FORM convert_payslip_to_pdf                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PFORM                                                         *
*  -->  PINFO                                                         *
*---------------------------------------------------------------------*
FORM convert_payslip_to_pdf TABLES $pform STRUCTURE pc408
                            USING  $pinfo TYPE      pc407.

  CONSTANTS:  def_name TYPE tdsfname VALUE 'SPOOL'.
  DATA: sform TYPE tdsfname,
        error TYPE sysubrc,
        cp    LIKE tcp00-cpcodepage.

* Read feature EDPDF
  CALL FUNCTION 'HR_FEATURE_BACKFIELD'
    EXPORTING
      feature       = 'EDPDF'
      struc_content = $pinfo
    IMPORTING
      back          = sform
    EXCEPTIONS
      OTHERS        = 7.

  IF sy-subrc <> 0
  OR sform IS INITIAL.
    sform = def_name.
  ENDIF.

  CASE sform.
    WHEN def_name.
      CALL FUNCTION 'SYSTEM_CODEPAGE'                       "KVHN970350
        IMPORTING                                           "KVHN970350
          current_dynamic_codepage = cp.                    "KVHN970350

*      IF cp(1) <> '4'.                                      "KVHN970350
      IF cp(1) = '4'.                              "Mod. PPRATA 14.04.2020
        PERFORM convert_using_spool TABLES $pform
                                    USING  $pinfo.
      ELSE.                                                 "KVHN970350
        MESSAGE e283(72).                                   "KVHN970350
      ENDIF.                                                "KVHN970350
    WHEN OTHERS.
      PERFORM convert_using_smartform TABLES $pform
                                      USING  $pinfo
                                             sform
                                             error.
      IF error <> 0.
*        IF cp(1) <> '4'.                                    "KVHN970350
        IF cp(1) = '4'.                              "Mod. PPRATA 14.04.2020
          PERFORM convert_using_spool TABLES $pform
                                      USING  $pinfo.
        ELSE.                                               "KVHN970350
          MESSAGE e283(72).                                 "KVHN970350
        ENDIF.                                              "KVHN970350
      ENDIF.
  ENDCASE.
ENDFORM.                    "convert_payslip_to_pdf
*---------------------------------------------------------------------*
*       FORM convert_using_smartform                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PFORM                                                         *
*  -->  PINFO                                                         *
*  -->  SFNAME                                                        *
*---------------------------------------------------------------------*
FORM convert_using_smartform TABLES $pform  STRUCTURE pc408
                             USING  $pinfo  TYPE      pc407
                                    $sfname TYPE      tdsfname
                                    $error  TYPE      sysubrc.

* Get the FB name from SmartForms
  DATA:  fname TYPE funcname.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname = $sfname
    IMPORTING
      fm_name  = fname
    EXCEPTIONS
      OTHERS   = 3.

  $error = sy-subrc.
  CHECK $error EQ 0.

* Call Smart Form
  DATA: sf_control_parameters TYPE ssfctrlop,
        job_output_info       TYPE ssfcrescl,
        sf_output_options     TYPE ssfcompop.

  sf_control_parameters-no_dialog = 'X'.
  sf_control_parameters-getotf    = 'X'.
  sf_control_parameters-langu     = sy-langu.

  CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
    EXPORTING
      i_language = sf_control_parameters-langu
*     I_APPLICATION          = 'SAPDEFAULT'
    IMPORTING
      e_devtype  = sf_output_options-tdprinter
    EXCEPTIONS
      OTHERS     = 5.
  $error = sy-subrc.
  CHECK $error EQ 0.

  CALL FUNCTION fname
    EXPORTING
      control_parameters = sf_control_parameters
      output_options     = sf_output_options
      pinfo              = $pinfo
      pform              = $pform[]
    IMPORTING
      job_output_info    = job_output_info
    EXCEPTIONS
      OTHERS             = 4.

  $error = sy-subrc.
  CHECK $error EQ 0.

* OTF to PDF conversion and export to memory
  PERFORM report_data_convert(saplhress00_rep)
          TABLES job_output_info-otfdata.
ENDFORM.                    "convert_using smartform
*&---------------------------------------------------------------------*
*&      Form  convert_using_spool
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->$PFORM     text
*      -->$PINFO     text
*----------------------------------------------------------------------*
FORM convert_using_spool TABLES $pform STRUCTURE pc408
                         USING  $pinfo TYPE pc407.

*   CMS@SBX - 03.07.2012: Carlos.sa@sbx.pt
* Get print parameters from user defaults and form definition
  CONSTANTS: dataset   TYPE pri_params-prdsn VALUE 'ESSEDT',
             lc_layout TYPE sypaart VALUE 'Z_RECIBO_SALARIO'.

  DATA: edtname    TYPE syrepid,
        listname   TYPE pri_params-plist,
        pri_params TYPE pri_params,
        rc         TYPE sysubrc,
        valid      TYPE xfeld.

** Declaração para a eliminação do spool.
  DATA : wa_tsp01sys TYPE tsp01sys.


  CONCATENATE dataset sy-uzeit INTO listname.

*  PERFORM xedt_get_repna IN PROGRAM rpcljnx0
*          USING $pinfo-molga edtname rc.
*  IF rc <> 0.
*    edtname = 'RPCEDTX0'.
*  ENDIF.
*  IF $pinfo-molga is initial or $pinfo-molga = '19'.
*    edtname = 'RPCEDTP0'.
*  ELSE.
*    edtname = 'RPCEDTX0'.
*  ENDIF.

  edtname = sy-repid.

  CALL FUNCTION 'HR_PL_DETERMINE_PRI_PARAMS'
    EXPORTING
      imp_linesize   = $pinfo-pcols
      imp_linecount  = $pinfo-psize
      imp_repid      = edtname
    IMPORTING
      exp_new_params = pri_params.

  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      data_set       = dataset  " dataset name
      expiration     = '1'  " one day in spool
      immediately    = space  " no immediate print
      in_parameters  = pri_params  " defaults
      list_name      = listname  " listname
      layout         = lc_layout " Layout saida
      new_list_id    = 'X'  " new spool id
      no_dialog      = 'X'  " no print popup
    IMPORTING
      out_parameters = pri_params
      valid          = valid
    EXCEPTIONS
      OTHERS         = 4.

  CHECK sy-subrc EQ 0
  AND valid NE space.

* Submit EDT to spool
  EXPORT p_p_form FROM $pform TO MEMORY ID '%%_P_FORM_%%'.
  EXPORT p_info   FROM $pinfo TO MEMORY ID '%%_P_INFO_%%'.

  SUBMIT rpcedt_list_to_memory
         TO SAP-SPOOL
         SPOOL PARAMETERS pri_params
         WITHOUT SPOOL DYNPRO
         AND RETURN.

* Get Spool ID
  DATA: spool_ids TYPE STANDARD TABLE OF rsporq,
        spoolname TYPE rspotype-rq0name,
        spooldsn  TYPE rspotype-rq2name,
        spoolid   TYPE tsp01-rqident.

  FIELD-SYMBOLS:  <spool> LIKE LINE OF spool_ids.

  spooldsn  = listname.
  spoolname = dataset.

  CALL FUNCTION 'RSPO_FIND_SPOOL_REQUESTS'
    EXPORTING
      rq0name       = spoolname
      rq2name       = spooldsn
    TABLES
      spoolrequests = spool_ids
    EXCEPTIONS
      OTHERS        = 0.

  READ TABLE spool_ids ASSIGNING <spool> INDEX 1.
  CHECK sy-subrc EQ 0.

  spoolid = <spool>-rqident.

* Convert to PDF
  DATA: pdf_table TYPE  rcl_bag_tline,
        pdf_fsize TYPE  i.

  CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
    EXPORTING
      src_spoolid   = spoolid
      no_dialog     = 'X'
    IMPORTING
      pdf_bytecount = pdf_fsize
    TABLES
      pdf           = pdf_table
    EXCEPTIONS
      OTHERS        = 0.

* Eliminar a ordem spool.
  wa_tsp01sys-rqident = spoolid.    "sy-spono.
  CALL FUNCTION 'RSPO_IDELETE_SPOOLREQ'
    EXPORTING
      spoolreq = wa_tsp01sys.
  IF sy-subrc <> 0.
  ENDIF.


  EXPORT pdf_table TO MEMORY ID 'PDFT'.
  EXPORT pdf_fsize TO MEMORY ID 'PDSZ'.

ENDFORM.  " convert_using_spool
