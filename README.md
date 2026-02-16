# RSB_-Purchase-Order-Print
Purchase Order Print

Classes 

class ZCL_ZFDP_EF_PO_GLO_GEN_DPC definition
  public
  inheriting from CL_FDP_EF_PURCHASE__03_DPC_EXT
  abstract
  create public .

public section.
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZFDP_EF_PO_GLO_GEN_DPC IMPLEMENTATION.
ENDCLASS.
**************
class CL_FDP_EF_PURCHASE__03_DPC_EXT definition
  public
  inheriting from CL_FDP_EF_PURCHASE__03_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
protected section.

  methods INVOICINGPARTYNO_GET_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_REQUEST_OBJECT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    exporting
      !ER_ENTITY type CL_FDP_EF_PURCHASE__03_MPC=>TS_INVOICINGPARTYNODE
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT .
  methods ITEMTAXCONDITION_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type CL_FDP_EF_PURCHASE__03_MPC=>TT_ITEMTAXCONDITIONSNODE
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT .
  methods TAXSUMMARYNODESE_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type CL_FDP_EF_PURCHASE__03_MPC=>TT_TAXSUMMARYNODE
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT .
  methods TOTALAMOUNTS_GET_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_REQUEST_OBJECT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    exporting
      !ER_ENTITY type CL_FDP_EF_PURCHASE__03_MPC=>TS_TOTALAMOUNTSNODE
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT .
  methods SUPPLIERS_GET_ENTITY_EXT
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_REQUEST_OBJECT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    exporting
      !ER_ENTITY type CL_FDP_EF_PURCHASE__03_MPC=>TS_SUPPLIERNODE
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT .
  methods SHIPTOPARTNERS_GET_ENTITY_EXT
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_REQUEST_OBJECT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    exporting
      !ER_ENTITY type CL_FDP_EF_PURCHASE__03_MPC=>TS_SHIPTOPARTYNODE
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT .

  methods PURCHASEORDERITE_GET_ENTITYSET
    redefinition .
  methods PURCHASEORDERLIM_GET_ENTITYSET
    redefinition .
  methods PURCHASEORDERS_GET_ENTITY
    redefinition .
private section.

  types:
    BEGIN OF TS_POITEMBATCHNUMBER,
             PURCHASEORDER     TYPE EBELN,
             PURCHASEORDERITEM TYPE EBELP,
             BATCH             TYPE CHARG_D,
           END OF TS_POITEMBATCHNUMBER .
  types:
    BEGIN OF TS_TAXLINE_IN,
             ConditionType    TYPE KSCHL,
             Description      TYPE VTXTK,
             ConditionRate    TYPE KBETR,
             ConditionValue   TYPE KWERT,
             DocumentCurrency TYPE WAERS,
           END OF TS_TAXLINE_IN .

  data MV_LANGUAGE type SY-LANGU .
  data MV_SENDER_COUNTRY type LAND1 .
  data MV_CHANGE_FLAG type CHAR1 .
  data MT_SOURCE_KEYS type /IWBEP/T_MGW_TECH_PAIRS .
  data MT_NAV_PATH type /IWBEP/T_MGW_TECH_NAVI .
  data MS_PURCHASE_ORDER type CL_FDP_EF_PURCHASE__03_MPC=>TS_PURCHASEORDERNODE .
  data:
    MT_ITEM_DETAILS TYPE TABLE OF CL_FDP_EF_PURCHASE__03_MPC=>TS_PURCHASEORDERITEMNODE .
  data:
    MT_TAXSUMMARY TYPE TABLE OF TS_TAXLINE_IN WITH KEY conditiontype .
  data MT_ITEMTAXCONDITIONS type CL_FDP_EF_PURCHASE__03_MPC=>TT_ITEMTAXCONDITIONSNODE .
  data MV_COMP_LANGU type SY-LANGU .
  data MV_SUPPL_COUNTRY type LAND1 .

  methods IS_PRINT_PRICE_IND_SET
    importing
      !IS_HEADER type CL_FDP_EF_PURCHASE_ORD_MPC=>TS_PURCHASEORDERNODE optional
      !IS_POITEM type CL_FDP_EF_PURCHASE_ORD_MPC=>TS_PURCHASEORDERITEMNODE optional
      !IS_POLIMITITEM type CL_FDP_EF_PURCHASE_ORD_MPC=>TS_PURCHASEORDERLIMITITEMNODE optional
      !IV_ENTITY_NAME type STRING
    returning
      value(RV_PRINT_IND) type ABAP_BOOL .
  methods SET_TAX_ITEM_HEADER
    importing
      value(IS_TAXCOM) type TAXCOM
      value(IS_ITEM) type TDS_ME_PO_ITEM
      value(IT_KOMV) type KOMV_T .
  methods _SET_INSTANCE_VARIABLES
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY .
ENDCLASS.



CLASS CL_FDP_EF_PURCHASE__03_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entity.

    DATA: ls_invoicing_party TYPE cl_fdp_ef_purchase__03_mpc=>ts_invoicingpartynode,
          ls_totalamount     TYPE cl_fdp_ef_purchase__03_mpc=>ts_totalamountsnode,
          ls_supplier        TYPE cl_fdp_ef_purchase__03_mpc=>ts_suppliernode,
          ls_shiptoparty     TYPE cl_fdp_ef_purchase__03_mpc=>ts_shiptopartynode.

    IF iv_entity_name = 'InvoicingPartyNode'.

      CALL METHOD me->invoicingpartyno_get_entity
        EXPORTING
          iv_entity_name          = iv_entity_name
          iv_entity_set_name      = iv_entity_set_name
          iv_source_name          = iv_source_name
          it_key_tab              = it_key_tab
*         io_request_object       =
          io_tech_request_context = io_tech_request_context
          it_navigation_path      = it_navigation_path
        IMPORTING
          er_entity               = ls_invoicing_party
          es_response_context     = es_response_context.

      copy_data_to_ref(
       EXPORTING
         is_data = ls_invoicing_party
       CHANGING
         cr_data = er_entity ).


    ELSEIF iv_entity_name = 'TotalAmountsNode'.

      CALL METHOD me->totalamounts_get_entity
        EXPORTING
          iv_entity_name          = iv_entity_name
          iv_entity_set_name      = iv_entity_set_name
          iv_source_name          = iv_source_name
          it_key_tab              = it_key_tab
*         io_request_object       = io_request_object
          io_tech_request_context = io_tech_request_context
          it_navigation_path      = it_navigation_path
        IMPORTING
          er_entity               = ls_totalamount
          es_response_context     = es_response_context.

      copy_data_to_ref(
        EXPORTING
            is_data = ls_totalamount
        CHANGING
            cr_data = er_entity ).

    ELSEIF iv_entity_name = 'SupplierNode'.

      CALL METHOD me->suppliers_get_entity_ext
        EXPORTING
          iv_entity_name          = iv_entity_name
          iv_entity_set_name      = iv_entity_set_name
          iv_source_name          = iv_source_name
          it_key_tab              = it_key_tab
*         io_request_object       =
          io_tech_request_context = io_tech_request_context
          it_navigation_path      = it_navigation_path
        IMPORTING
          er_entity               = ls_supplier
          es_response_context     = es_response_context.

      copy_data_to_ref(
       EXPORTING
         is_data = ls_supplier
       CHANGING
         cr_data = er_entity ).

    ELSEIF iv_entity_name = 'ShipToPartyNode'.

      CALL METHOD me->shiptopartners_get_entity_ext
        EXPORTING
          iv_entity_name          = iv_entity_name
          iv_entity_set_name      = iv_entity_set_name
          iv_source_name          = iv_source_name
          it_key_tab              = it_key_tab
*         io_request_object       =
          io_tech_request_context = io_tech_request_context
          it_navigation_path      = it_navigation_path
        IMPORTING
          er_entity               = ls_shiptoparty
          es_response_context     = es_response_context.

      copy_data_to_ref(
       EXPORTING
         is_data = ls_shiptoparty
       CHANGING
         cr_data = er_entity ).

    ELSE.

      TEST-SEAM ts_super1.                         "#EC TEST_SEAM_USAGE

        TRY.
            CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~get_entity
              EXPORTING
                iv_entity_name          = iv_entity_name
                iv_entity_set_name      = iv_entity_set_name
                iv_source_name          = iv_source_name
                it_key_tab              = it_key_tab
                it_navigation_path      = it_navigation_path
                io_tech_request_context = io_tech_request_context
              IMPORTING
                er_entity               = er_entity
                es_response_context     = es_response_context.
          CATCH /iwbep/cx_mgw_busi_exception.
          CATCH /iwbep/cx_mgw_tech_exception.
        ENDTRY.

      END-TEST-SEAM.

    ENDIF.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.


    DATA: lt_item_tax_condition TYPE cl_fdp_ef_purchase__03_mpc=>tt_itemtaxconditionsnode.
    DATA: lt_tax_summary TYPE cl_fdp_ef_purchase__03_mpc=>tt_taxsummarynode.

    IF iv_entity_set_name = 'ItemTaxConditionsNodeSet'.

      CALL METHOD me->itemtaxcondition_get_entityset
        EXPORTING
          iv_entity_name           = iv_entity_name
          iv_entity_set_name       = iv_entity_set_name
          iv_source_name           = iv_source_name
          it_filter_select_options = it_filter_select_options
          is_paging                = is_paging
          it_key_tab               = it_key_tab
          it_navigation_path       = it_navigation_path
          it_order                 = it_order
          iv_filter_string         = iv_filter_string
          iv_search_string         = iv_search_string
*         io_tech_request_context  =
        IMPORTING
          et_entityset             = lt_item_tax_condition
          es_response_context      = es_response_context.

      copy_data_to_ref(
       EXPORTING
         is_data = lt_item_tax_condition
       CHANGING
         cr_data = er_entityset ).


    ELSEIF iv_entity_set_name = 'TaxSummaryNodeSet'.

      CALL METHOD me->taxsummarynodese_get_entityset
        EXPORTING
          iv_entity_name           = iv_entity_name
          iv_entity_set_name       = iv_entity_set_name
          iv_source_name           = iv_source_name
          it_filter_select_options = it_filter_select_options
          is_paging                = is_paging
          it_key_tab               = it_key_tab
          it_navigation_path       = it_navigation_path
          it_order                 = it_order
          iv_filter_string         = iv_filter_string
          iv_search_string         = iv_search_string
*         io_tech_request_context  =
        IMPORTING
          et_entityset             = lt_tax_summary
          es_response_context      = es_response_context.

      copy_data_to_ref(
       EXPORTING
         is_data = lt_tax_summary
       CHANGING
         cr_data = er_entityset ).

    ELSE.

      TEST-SEAM ts_super2. "#EC TEST_SEAM_USAGE

      TRY.
          CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              it_order                 = it_order
              is_paging                = is_paging
              it_navigation_path       = it_navigation_path
              it_key_tab               = it_key_tab
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              er_entityset             = er_entityset
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception.
        CATCH /iwbep/cx_mgw_tech_exception.
      ENDTRY.

      END-TEST-SEAM.

    ENDIF.

  ENDMETHOD.


  METHOD invoicingpartyno_get_entity.

*  CONSTANTS: partner(2) type c value 'PI'.
    DATA ls_po_key     TYPE /iwbep/s_mgw_name_value_pair.
    DATA: lv_ebeln   TYPE ebeln.

    CLEAR: er_entity,
           es_response_context.

    READ TABLE it_key_tab
          WITH KEY name = 'PurchaseOrder'
          INTO ls_po_key.
    lv_ebeln = ls_po_key-value.


    IF lv_ebeln IS NOT INITIAL.
*      **Retrieve Invoicing Party details from oartner functions
      SELECT lifn2 FROM ekpa  WHERE ebeln = @lv_ebeln AND parvw = 'RS' INTO @DATA(lv_partner). "#EC CI_NOORDER
      ENDSELECT.

      IF sy-subrc <> 0.

      ELSE.

        IF lv_partner IS NOT INITIAL.
* Retrieve Invoicing Party Address from Supplier CDS
          SELECT SINGLE addressid  ##WARN_OK
            FROM i_supplier
            INTO @er_entity-adrnr
            WHERE supplier = @lv_partner.

          IF er_entity-adrnr IS NOT INITIAL.
* retrieve address line 1 - 8
            cl_fdp_ef_pur_ord_form_utility=>get_address_in_printform(
              EXPORTING
                iv_language        = mv_language
                iv_sender_country  = mv_sender_country
                iv_adrnr           = er_entity-adrnr
              IMPORTING
                ev_address_line1   = er_entity-address_line_1
                ev_address_line2   = er_entity-address_line_2
                ev_address_line3   = er_entity-address_line_3
                ev_address_line4   = er_entity-address_line_4
                ev_address_line5   = er_entity-address_line_5
                ev_address_line6   = er_entity-address_line_6
                ev_address_line7   = er_entity-address_line_7
                ev_address_line8   = er_entity-address_line_8
                ).
          ENDIF.
          SELECT SINGLE taxnumber3
            FROM i_supplier
            INTO @er_entity-stcd3
            WHERE supplier = @lv_partner.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD is_print_price_ind_set.

    rv_print_ind = abap_true.

    CASE iv_entity_name.
      WHEN 'PurchaseOrderItemNode'.
        IF is_poitem-prsdr IS INITIAL.
          rv_print_ind = abap_false.
        ENDIF.
      WHEN 'PurchaseOrderLimitItemNode'.
        IF is_polimititem-prsdr IS INITIAL.
          rv_print_ind = abap_false.
        ENDIF.
      WHEN 'PurchaseOrderNode'.
        SELECT COUNT(*)
          FROM i_purchaseorderitemenhanced
          WHERE purchaseorder = @is_header-ebeln
          AND priceistobeprinted IS INITIAL
          AND isdeleted IN ('','S')
          INTO @DATA(lv_count).
        IF sy-subrc EQ 0 AND lv_count >= 1.
          rv_print_ind = abap_false.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  method ITEMTAXCONDITION_GET_ENTITYSET.
    DATA: ls_po_key   TYPE /iwbep/s_mgw_name_value_pair,
          ls_item_key TYPE /iwbep/s_mgw_name_value_pair.

    CLEAR: et_entityset,
           es_response_context.

* Sanity Check
    READ TABLE it_key_tab
         WITH KEY name = 'PurchaseOrder'
         INTO ls_po_key.

    ASSERT ms_purchase_order-ebeln = ls_po_key-value.

* Item
    READ TABLE it_key_tab
         WITH KEY name = 'PurchaseOrderItem'
         INTO ls_item_key.

* Get Item Tax Conditions

    LOOP AT mt_itemtaxconditions
         REFERENCE INTO DATA(lr_item_tax_condition)
         WHERE purchase_order_item = ls_item_key-value.

      INSERT lr_item_tax_condition->* INTO TABLE et_entityset.
    ENDLOOP.
  endmethod.


  METHOD purchaseorderite_get_entityset.

    DATA ls_taxcom TYPE taxcom.
    DATA: t_komv TYPE TABLE OF komv.
    DATA ls_po_key TYPE /iwbep/s_mgw_name_value_pair.
    TYPES: BEGIN OF ty_hsn_tax_code,
             ebeln   TYPE ebeln,
             ebelp   TYPE ebelp,
             j_1bnbm TYPE j_1bnbmco1,
             mwskz   TYPE mwskz,
           END OF ty_hsn_tax_code.
    TYPES: BEGIN OF ty_tax_date,
             ebeln TYPE ebeln,
             ebelp TYPE ebelp,
             txdat TYPE txdat,
           END OF ty_tax_date.

    DATA: lt_codes    TYPE TABLE OF ty_hsn_tax_code,
          ls_codes    TYPE ty_hsn_tax_code,
          lt_tax_date TYPE TABLE OF ty_tax_date,
          ls_tax_date TYPE ty_tax_date.
    DATA: lb_tdt_handler_set TYPE abap_bool.

    TRY.

        TEST-SEAM ts_super3.                       "#EC TEST_SEAM_USAGE

          CALL METHOD super->purchaseorderite_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              is_paging                = is_paging
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              it_order                 = it_order
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              et_entityset             = et_entityset
              es_response_context      = es_response_context.

        END-TEST-SEAM.

        TEST-SEAM ts_et_entityset1.                "#EC TEST_SEAM_USAGE
        END-TEST-SEAM.

      CATCH /iwbep/cx_mgw_busi_exception.
      CATCH /iwbep/cx_mgw_tech_exception.
    ENDTRY.

    READ TABLE it_key_tab
     WITH KEY name = 'PurchaseOrder'
     INTO ls_po_key.

    IF ls_po_key-value IS NOT INITIAL.
*      fill taxcom with constant value for po
      ls_taxcom-ebeln = ls_po_key-value.
      ls_taxcom-xmwst = 'X'.
      ls_taxcom-bukrs = ms_purchase_order-bukrs.
      ls_taxcom-budat = ms_purchase_order-bedat.

      SELECT SINGLE lifnr
        FROM ekko
        WHERE ebeln = @ls_po_key-value
        INTO @ls_taxcom-lifnr.
    ENDIF.


    IF et_entityset IS NOT INITIAL.
*    Fetching Tax date
      CLEAR lt_tax_date.
      SELECT POitem~ebeln, POitem~ebelp, POitem~txdat FROM ekpo AS POitem
        FOR ALL ENTRIES IN @et_entityset
        WHERE POitem~ebeln = @et_entityset-ebeln AND POitem~ebelp = @et_entityset-ebelp
        INTO TABLE @lt_tax_date.

*    Fetching HSN/SAC code and Tax code
      CLEAR lt_codes.
      SELECT PurchaseOrderItems~PurchaseOrder,PurchaseOrderItems~PurchaseOrderItem,
         PurchaseOrderItems~consumptiontaxctrlcode, PurchaseOrderItems~taxcode
      FROM  i_purchaseorderitem AS PurchaseOrderItems
      FOR ALL ENTRIES IN @et_entityset
      WHERE  PurchaseOrderItems~purchaseorder = @et_entityset-ebeln AND
      PurchaseOrderItems~purchaseorderitem = @et_entityset-ebelp
      INTO TABLE @lt_codes.

    ENDIF.

    " Set mode for TDT (if not set, already)
    TRY.
        IF cl_fot_tdt_btt_code=>handler->get( ) IS INITIAL.
          cl_fot_tdt_btt_code=>handler->set( iv_btt_code = cl_fot_tdt_btt_code=>mc_btt_code-mm_purch_order ).
          lb_tdt_handler_set = abap_true.
        ENDIF.
      CATCH cx_fot_tdt_root INTO DATA(lx_tdt).
*      MESSAGE lx_tdt.
    ENDTRY.

    LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<ls_entityset>).

      CLEAR ls_codes.
      READ TABLE lt_codes INTO ls_codes WITH KEY ebeln = <ls_entityset>-ebeln ebelp = <ls_entityset>-ebelp.
      IF sy-subrc = 0 AND ls_codes IS NOT INITIAL.
        <ls_entityset>-j_1bnbm = ls_codes-j_1bnbm.
        <ls_entityset>-mwskz = ls_codes-mwskz.
      ENDIF.

      CLEAR ls_tax_date.
      READ TABLE lt_tax_date INTO ls_tax_date WITH KEY ebeln = <ls_entityset>-ebeln ebelp = <ls_entityset>-ebelp.
      IF sy-subrc = 0 AND ls_tax_date IS NOT INITIAL.
        ls_taxcom-txdat = ls_tax_date-txdat.
      ENDIF.

      IF <ls_entityset>-loekz IS INITIAL. "deleted indicator

        ls_taxcom-ebelp = ls_taxcom-kposn = <ls_entityset>-ebelp.
        ls_taxcom-matnr = <ls_entityset>-matnr.
        ls_taxcom-werks = <ls_entityset>-werks.
        ls_taxcom-wrbtr = <ls_entityset>-netwr.
        ls_taxcom-mwskz = <ls_entityset>-mwskz.

        CLEAR t_komv.

        TEST-SEAM ts_taxfc.                        "#EC TEST_SEAM_USAGE

          CALL FUNCTION 'CALCULATE_TAX_ITEM'
            EXPORTING
              i_taxcom                  = ls_taxcom
            IMPORTING
              e_taxcom                  = ls_taxcom
            TABLES
              t_xkomv                   = t_komv
            EXCEPTIONS
              mwskz_not_defined         = 1
              mwskz_not_found           = 2
              mwskz_not_valid           = 3
              steuerbetrag_falsch       = 4
              country_not_found         = 5
              txjcd_not_valid           = 6
              amounts_too_large_for_tax = 7
              OTHERS                    = 8.

        END-TEST-SEAM.

        IF sy-subrc = 0.
          <ls_entityset>-wmwst = ls_taxcom-wmwst.
          IF <ls_entityset>-retpo = 'X'.
            <ls_entityset>-wmwst = <ls_entityset>-wmwst * -1.
          ENDIF.

          set_tax_item_header(
            EXPORTING
              is_taxcom =  ls_taxcom                 " Communications Work Area for Tax Calculation
              is_item   =  <ls_entityset>                 " PO Purchase Order Item Node Structure-India
              it_komv   =  t_komv                 " Sorted Table Type of KOMV
          ).

        ENDIF.
        CLEAR ls_taxcom-wmwst.
      ENDIF.
    ENDLOOP.

    " Clear TDT, if it was set within this method.
    TRY.
        IF lb_tdt_handler_set = abap_true.
          cl_fot_tdt_btt_code=>handler->clear( ).
        ENDIF.
      CATCH cx_fot_tdt_root INTO DATA(lx_fot_tdt_root_1).
    ENDTRY.

    mt_item_details = et_entityset.

  ENDMETHOD.


  METHOD purchaseorderlim_get_entityset.
    TRY.

      TEST-SEAM  ts_super4. "#EC TEST_SEAM_USAGE

        CALL METHOD super->purchaseorderlim_get_entityset
          EXPORTING
            iv_entity_name           = iv_entity_name
            iv_entity_set_name       = iv_entity_set_name
            iv_source_name           = iv_source_name
            it_filter_select_options = it_filter_select_options
            is_paging                = is_paging
            it_key_tab               = it_key_tab
            it_navigation_path       = it_navigation_path
            it_order                 = it_order
            iv_filter_string         = iv_filter_string
            iv_search_string         = iv_search_string
            io_tech_request_context  = io_tech_request_context
          IMPORTING
            et_entityset             = et_entityset
            es_response_context      = es_response_context.

      END-TEST-SEAM.

       TEST-SEAM ts_et_entityset2. "#EC TEST_SEAM_USAGE
       END-TEST-SEAM.

      CATCH /iwbep/cx_mgw_busi_exception.
      CATCH /iwbep/cx_mgw_tech_exception.
    ENDTRY.

    LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<ls_entityset>).
* Checking price to be printed in output or not
      IF is_print_price_ind_set( is_polimititem = <ls_entityset> iv_entity_name = iv_entity_name ) EQ abap_false.
        CLEAR: <ls_entityset>-netpr,
               <ls_entityset>-netwr.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD purchaseorders_get_entity.
    TRY.

      TEST-SEAM ts_super5. "#EC TEST_SEAM_USAGE

        CALL METHOD super->purchaseorders_get_entity
          EXPORTING
            iv_entity_name          = iv_entity_name
            iv_entity_set_name      = iv_entity_set_name
            iv_source_name          = iv_source_name
            it_key_tab              = it_key_tab
            io_request_object       = io_request_object
            io_tech_request_context = io_tech_request_context
            it_navigation_path      = it_navigation_path
          IMPORTING
            er_entity               = er_entity
            es_response_context     = es_response_context.

      END-TEST-SEAM.

       TEST-SEAM ts_et_entityset3. "#EC TEST_SEAM_USAGE
       END-TEST-SEAM.

      CATCH /iwbep/cx_mgw_busi_exception.
      CATCH /iwbep/cx_mgw_tech_exception.
    ENDTRY.

    SELECT SINGLE spras FROM t001 INTO mv_comp_langu WHERE bukrs = er_entity-bukrs.

    CALL METHOD me->_set_instance_variables
      EXPORTING
        io_tech_request_context = io_tech_request_context.

     MOVE-CORRESPONDING er_entity TO ms_purchase_order.

  ENDMETHOD.


  METHOD set_tax_item_header.

    DATA: wa_komv              TYPE komv,
          wa_taxsummary        TYPE ts_taxline_in,
          wa_itemtaxconditions LIKE LINE OF mt_itemtaxconditions.
*prepare tax summary and item level tax from t_komv
    LOOP AT it_komv INTO wa_komv WHERE koaid = 'D'
                                 AND   kstat = space.

*prepare for item tax entity
      CLEAR wa_itemtaxconditions.

      wa_itemtaxconditions-purchase_order = is_taxcom-ebeln.
      wa_itemtaxconditions-purchase_order_item = is_taxcom-kposn.
      wa_itemtaxconditions-document_currency = is_item-waers.
      wa_itemtaxconditions-condition_rate_value_int_unit = '%'.

      wa_itemtaxconditions-condition_type = wa_komv-kschl.
      wa_itemtaxconditions-condition_rate_value = wa_komv-kbetr / 10.
      wa_itemtaxconditions-condition_amount = wa_komv-kwert.


      READ TABLE mt_itemtaxconditions TRANSPORTING condition_type_name
                                      INTO wa_itemtaxconditions
                                      WITH KEY condition_type = wa_komv-kschl.

      IF sy-subrc <> 0.

        SELECT vtext FROM t685t UP TO 1 ROWS INTO wa_itemtaxconditions-condition_type_name    "#EC CI_NOORDER
                                       WHERE kschl = wa_komv-kschl
                                       AND   kappl = wa_komv-kappl
                                       AND   spras = sy-langu.
         ENDSELECT.
      ENDIF.

      APPEND wa_itemtaxconditions TO mt_itemtaxconditions.

*prepare tax summary

      CLEAR wa_taxsummary.
      READ TABLE mt_taxsummary INTO wa_taxsummary WITH TABLE KEY conditiontype = wa_komv-kschl.

      IF sy-subrc <> 0.

        wa_taxsummary-conditiontype  = wa_komv-kschl.
        wa_taxsummary-conditionvalue = wa_komv-kwert.
        wa_taxsummary-conditionrate  = wa_komv-kbetr / 10.

        wa_taxsummary-description    = wa_itemtaxconditions-condition_type_name.

        wa_taxsummary-documentcurrency = is_item-waers.

        APPEND wa_taxsummary TO mt_taxsummary.
      ELSE.
        wa_taxsummary-conditionvalue = wa_taxsummary-conditionvalue + wa_komv-kwert.
        MODIFY TABLE mt_taxsummary FROM wa_taxsummary.
      ENDIF.

      CLEAR wa_komv.
    ENDLOOP.
  ENDMETHOD.    "#EC CI_VALPAR


  METHOD shiptopartners_get_entity_ext.

    DATA er_entity_super TYPE cl_fdp_ef_purchase_ord_mpc=>ts_shiptopartynode.
    DATA ls_po_key     TYPE /iwbep/s_mgw_name_value_pair.
    DATA lv_branch TYPE j_1bbranc_.
    DATA lv_stcd3 TYPE stcd3.
    DATA lv_adrnr TYPE adrnr.

    CLEAR er_entity.

    TRY.
        CALL METHOD me->shiptopartners_get_entity
          EXPORTING
            iv_entity_name          = iv_entity_name
            iv_entity_set_name      = iv_entity_set_name
            iv_source_name          = iv_source_name
            it_key_tab              = it_key_tab
            io_request_object       = io_request_object
            io_tech_request_context = io_tech_request_context
            it_navigation_path      = it_navigation_path
          IMPORTING
            er_entity               = er_entity_super
            es_response_context     = es_response_context.
      CATCH /iwbep/cx_mgw_busi_exception.
      CATCH /iwbep/cx_mgw_tech_exception.
    ENDTRY.

    MOVE-CORRESPONDING er_entity_super TO er_entity.

*     Sanity Check
    READ TABLE it_key_tab
         WITH KEY name = 'PurchaseOrder'
         INTO ls_po_key.

    IF ls_po_key-value IS NOT INITIAL.
      SELECT ebelp,werks
       FROM  ekpo
       INTO TABLE @DATA(lt_po_item)
       WHERE ebeln = @ls_po_key-value.

      IF sy-subrc = 0.
*         SORT lt_po_item BY purchaseorderitem ASCENDING.
        SORT lt_po_item BY ebelp ASCENDING.
      ENDIF.
    ENDIF.


    READ TABLE lt_po_item INDEX 1 INTO DATA(ls_po_item).
    IF sy-subrc = 0.
*      er_entity-adrnr = ls_po_item-itemdeliveryaddressid.
*Retrieve branch for plant.
      SELECT SINGLE j_1bbranch
        FROM T001w
        INTO lv_branch
        WHERE werks = ls_po_item-werks."werks = ls_po_item-plant.

      IF sy-subrc = 0 AND lv_branch IS NOT INITIAL.

* Retrieve GSTIN and adress number for ship to party
        SELECT gstin adrnr                              "#EC CI_NOORDER
          FROM j_1bbranch
          INTO (lv_stcd3, lv_adrnr)
          WHERE branch = lv_branch.
        ENDSELECT.
        IF sy-subrc = 0.
          er_entity-stcd3 = lv_stcd3.
        ENDIF.
      ENDIF.

      IF lv_adrnr IS NOT INITIAL.

        er_entity-adrnr = lv_adrnr.
* retrieve address line 1 - 8
        cl_fdp_ef_pur_ord_form_utility=>get_address_in_printform(
         EXPORTING
           iv_language            = mv_comp_langu
           iv_sender_country      = mv_suppl_country
           iv_adrnr               = er_entity-adrnr
           iv_street_has_priority = abap_true             "street has priority in ship-to-address
         IMPORTING
           ev_address_line1   = er_entity-address_line_1
           ev_address_line2   = er_entity-address_line_2
           ev_address_line3   = er_entity-address_line_3
           ev_address_line4   = er_entity-address_line_4
           ev_address_line5   = er_entity-address_line_5
           ev_address_line6   = er_entity-address_line_6
           ev_address_line7   = er_entity-address_line_7
           ev_address_line8   = er_entity-address_line_8
           ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD suppliers_get_entity_ext.

    DATA ls_po_key     TYPE /iwbep/s_mgw_name_value_pair.
    DATA lv_adrnr      TYPE adrnr.
    DATA er_entity_super TYPE cl_fdp_ef_purchase_ord_mpc=>ts_suppliernode.

    CLEAR: er_entity,
           es_response_context.

    TRY.

      TEST-SEAM ts_super6. "#EC TEST_SEAM_USAGE

        CALL METHOD me->suppliers_get_entity
          EXPORTING
            iv_entity_name          = iv_entity_name
            iv_entity_set_name      = iv_entity_set_name
            iv_source_name          = iv_source_name
            it_key_tab              = it_key_tab
            io_request_object       = io_request_object
            io_tech_request_context = io_tech_request_context
            it_navigation_path      = it_navigation_path
          IMPORTING
            er_entity               = er_entity_super
            es_response_context     = es_response_context.

      END-TEST-SEAM.

       TEST-SEAM ts_et_entityset4. "#EC TEST_SEAM_USAGE
       END-TEST-SEAM.

      CATCH /iwbep/cx_mgw_busi_exception.
      CATCH /iwbep/cx_mgw_tech_exception.
    ENDTRY.

    MOVE-CORRESPONDING er_entity_super TO er_entity.

    READ TABLE it_key_tab
            WITH KEY name = 'PurchaseOrder'
            INTO ls_po_key.

    IF ls_po_key-value IS NOT INITIAL.

      SELECT Supplier~taxnumber3 AS stcd3, Supplier~phonenumber1 as telf1, Supplier~phonenumber2 as telf2    "#EC CI_NOORDER
        INTO (@er_entity-stcd3, @er_entity-telf1, @er_entity-telf2)
        FROM i_supplier AS Supplier
         INNER JOIN i_purchaseorder AS nav_SUPPLIERPurchaseO_0
         ON nav_SUPPLIERPurchaseO_0~supplier = Supplier~supplier
         WHERE nav_SUPPLIERPurchaseO_0~purchaseorder = @ls_po_key-value.
      ENDSELECT.

    ENDIF.



  ENDMETHOD.


  METHOD taxsummarynodese_get_entityset.

    DATA: ls_taxsummary LIKE LINE OF mt_taxsummary,
          ls_entityset  TYPE mmpur_s_fdp_item_pricing_cond.
*          ls_entityset  TYPE cl_fdp_ef_purchase__03_mpc=>ts_taxsummarynode.

    LOOP AT mt_taxsummary INTO ls_taxsummary.

      ls_entityset-condition_type_name = ls_taxsummary-description.
      ls_entityset-condition_amount = ls_taxsummary-conditionvalue.
      ls_entityset-document_currency = ls_taxsummary-documentcurrency.

      APPEND ls_entityset TO et_entityset.

      CLEAR :ls_entityset,ls_taxsummary.

    ENDLOOP.

  ENDMETHOD.


  METHOD totalamounts_get_entity.

    DATA: iv_net_amount       TYPE netwr,
          iv_total_tax_amount TYPE wmwst,
          iv_gross_amount     TYPE wertv8,
          iv_amt_in_words     TYPE spell,
          iv_gross_in_words   TYPE string.
    TYPES: BEGIN OF ty_itab,
             text(255),
           END OF ty_itab.
    DATA: wa_itab type ty_itab.
    DATA: itab TYPE TABLE OF ty_itab.

    CONSTANTS: ic_rupee TYPE string VALUE 'Rupees',
               ic_paise TYPE string VALUE 'Paise'.

    CLEAR: iv_net_amount, iv_total_tax_amount.

    iv_net_amount = ms_purchase_order-purchaseordernetamount.

    LOOP AT mt_item_details ASSIGNING FIELD-SYMBOL(<ls_entityset>).
      iv_total_tax_amount = iv_total_tax_amount + <ls_entityset>-wmwst.

    ENDLOOP.

    iv_gross_amount = iv_net_amount + iv_total_tax_amount.

    CALL FUNCTION 'SPELL_AMOUNT'
      EXPORTING
        amount    = iv_gross_amount
        currency  = ms_purchase_order-waers
        filler    = ' '
        language  = sy-langu
      IMPORTING
        in_words  = iv_amt_in_words
      EXCEPTIONS
        not_found = 1
        too_large = 2
        OTHERS    = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    TRANSLATE iv_amt_in_words-word TO LOWER CASE.
    CLEAR itab.
    SPLIT iv_amt_in_words-word AT ' ' INTO TABLE itab.
    CLEAR: iv_amt_in_words-word, wa_itab.

    LOOP AT itab into wa_itab.
      TRANSLATE wa_itab-text+0(1) TO UPPER CASE.
      CONCATENATE iv_amt_in_words-word wa_itab-text INTO iv_amt_in_words-word SEPARATED BY ' '.
    ENDLOOP.
    iv_amt_in_words-word = iv_amt_in_words-word+1.

    TRANSLATE iv_amt_in_words-decword TO LOWER CASE.
    CLEAR itab.
    SPLIT iv_amt_in_words-decword AT ' ' INTO TABLE itab.
    CLEAR: iv_amt_in_words-decword, wa_itab.

    LOOP AT itab into wa_itab.
      TRANSLATE wa_itab-text+0(1) TO UPPER CASE.
      CONCATENATE iv_amt_in_words-decword wa_itab-text INTO iv_amt_in_words-decword SEPARATED BY ' '.
    ENDLOOP.
    iv_amt_in_words-decword = iv_amt_in_words-decword+1.

    CONCATENATE ic_rupee iv_amt_in_words-word 'and' iv_amt_in_words-decword ic_paise INTO iv_gross_in_words SEPARATED BY space.

    er_entity-Total_Tax_Amount = iv_total_tax_amount.
    er_entity-Gross_Amount = iv_gross_amount.
    er_entity-Gross_In_Words = iv_gross_in_words.
    er_entity-purchase_order = ms_purchase_order-ebeln.

  ENDMETHOD.


  METHOD _set_instance_variables.

    DATA: ls_source_key TYPE /iwbep/s_mgw_tech_pair.
    TRY.
        mt_source_keys = io_tech_request_context->get_source_keys( ).
        mt_nav_path    = io_tech_request_context->get_navigation_path( ).

* Language
        READ TABLE mt_source_keys
             INTO ls_source_key
             WITH KEY name = 'LANGU'.

        mv_language = ls_source_key-value.

* Sender Country
        READ TABLE mt_source_keys
             INTO ls_source_key
             WITH KEY name = 'LAND1'.

        mv_sender_country = ls_source_key-value.

* Change Flag
        READ TABLE mt_source_keys
             INTO ls_source_key
             WITH KEY name = 'CHANGE_FLAG'.

        mv_change_flag = ls_source_key-value.

        READ TABLE mt_source_keys INTO ls_source_key WITH KEY name = 'LIFN2'.
        IF sy-subrc EQ 0.
          SELECT SINGLE a~country FROM i_address WITH PRIVILEGED ACCESS AS a JOIN i_supplier AS s
            ON a~addressid = s~addressid
            WHERE s~supplier = @ls_source_key-value
            INTO @mv_suppl_country.
        ENDIF.

      CATCH cx_sy_ref_is_initial.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
*********************************************************************************************
class ZCL_ZFDP_EF_PO_GLO_GEN_DPC_EXT definition
  public
  inheriting from ZCL_ZFDP_EF_PO_GLO_GEN_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
protected section.

  methods POHEADER_GET_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_REQUEST_OBJECT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    exporting
      !ER_ENTITY type ZCL_ZFDP_EF_PO_GLO_GEN_MPC_EXT=>TS_PURCHASEORDERHEADER
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods POHEADER_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZFDP_EF_PO_GLO_GEN_MPC_EXT=>TT_PURCHASEORDERHEADER
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods PURCHASEORDERHEADERSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_REQUEST_OBJECT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    exporting
      !ER_ENTITY type ZCL_ZFDP_EF_PO_GLO_GEN_MPC_EXT=>TS_PURCHASEORDERHEADER
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods PURCHASEORDERHEA_GET_ENTITYSED
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZFDP_EF_PO_GLO_GEN_MPC_EXT=>TT_PURCHASEORDERHEADER
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
private section.
ENDCLASS.



CLASS ZCL_ZFDP_EF_PO_GLO_GEN_DPC_EXT IMPLEMENTATION.


  METHOD purchaseorderheaderset.
    ASSIGN it_key_tab[ 1 ] TO FIELD-SYMBOL(<purchaseorder>).
    IF <purchaseorder> IS ASSIGNED.
      er_entity-ebeln = <purchaseorder>-value.
    ENDIF.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
**  EXPORTING
**    iv_entity_name           =
**    iv_entity_set_name       =
**    iv_source_name           =
**    it_filter_select_options =
**    it_order                 =
**    is_paging                =
**    it_navigation_path       =
**    it_key_tab               =
**    iv_filter_string         =
**    iv_search_string         =
**    io_tech_request_context  =
**  IMPORTING
**    er_entityset             =
**    es_response_context      =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.

    DATA: POHeadToHead        TYPE TABLE OF ZCL_ZFDP_EF_PO_GLO_GEN_mPC_EXT=>ts_purchaseorderheader.
    DATA(lv_entityname) = io_tech_request_context->get_entity_set_name( ).

    CASE lv_entityname.
      WHEN 'PurchaseOrderHeaderSet'.
*        TRY.
*            poheader_get_entityset(
*              EXPORTING
*                iv_entity_name           = iv_entity_name
*                iv_entity_set_name       = iv_entity_set_name
*                iv_source_name           = iv_source_name
*                it_filter_select_options = it_filter_select_options               " Table of select options
*                is_paging                = is_paging                              " Paging structure
*                it_key_tab               = it_key_tab                             " Table for name value pairs
*                it_navigation_path       = it_navigation_path                     " Table of navigation paths
*                it_order                 = it_order                               " The sorting order
*                iv_filter_string         = iv_filter_string                       " Table for name value pairs
*                iv_search_string         = iv_search_string
*                io_tech_request_context  = io_tech_request_context
*              IMPORTING
*                et_entityset             =  DATA(Purchaseorderheader)        " Returning data
*                es_response_context      =  es_response_context
*            ).
*          CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
*          CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception(
*        ENDTRY.

          data : it_key_tabs TYPE /IWBEP/T_MGW_NAME_VALUE_PAIR .
          TYPES : tt_key_tabs TYPE /IWBEP/T_MGW_NAME_VALUE_PAIR .

          it_key_tabs = VALUE tt_key_tabs( ( name = 'PurchaseOrder' value = '4500000059') ).
        TRY.

        call METHOD poheader_get_entity
          EXPORTING
            iv_entity_name          = 'PurchaseOrderHeader'
            iv_entity_set_name      = 'PurchaseOrderHeaderSet'
            iv_source_name          = iv_source_name
            it_key_tab              = it_key_tabs                           " table for name value pairs
*            io_request_object       =                  " table of navigation paths
*            io_tech_request_context =
            it_navigation_path      = it_navigation_path                 " table of navigation paths
          IMPORTING
            er_entity               = DATA(Purchaseorderheader)                  " Returning data
*            es_response_context     =
          .
        CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
        CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception
        endtry.

        IF Purchaseorderheader IS NOT INITIAL.

          copy_data_to_ref( EXPORTING is_data = Purchaseorderheader
                            CHANGING cr_data = er_entityset ).
        ENDIF.

      WHEN OTHERS.
        TRY.

            CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~get_entityset
              EXPORTING
                iv_entity_name           = iv_entity_name                          " Obsolete
                iv_entity_set_name       = iv_entity_set_name                      " Obsolete
                iv_source_name           = iv_source_name                          " Obsolete
                it_filter_select_options = it_filter_select_options                " table of select options - Obsolete
                it_order                 = it_order                                " the sorting order - Obsolete
                is_paging                = is_paging                               " paging structure - Obsolete
                it_navigation_path       = it_navigation_path                      " table of navigation paths - Obsolete
                it_key_tab               = it_key_tab                              " table for name value pairs - Obsolete
                iv_filter_string         = iv_filter_string                        " the filter as a string containing ANDs and ORs etc -Obsolete
                iv_search_string         = iv_search_string                        " Obsolete
                io_tech_request_context  = io_tech_request_context
              IMPORTING
                er_entityset             = er_entityset
                es_response_context      = es_response_context.
          CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
          CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception

        ENDTRY.
    ENDCASE.


  ENDMETHOD.


  METHOD poheader_get_entityset.
    DATA:er_entity TYPE zcl_zfdp_ef_po_glo_gen_mpc_ext=>ts_purchaseorderheader.
    ASSIGN it_key_tab[ 1 ] TO FIELD-SYMBOL(<purchaseorder>).
    IF <purchaseorder> IS ASSIGNED.
      er_entity-ebeln = <purchaseorder>-value.
      APPEND er_entity TO et_entityset.
      CLEAR:er_entity.
    ENDIF.
  ENDMETHOD.


  method PURCHASEORDERHEA_GET_ENTITYSED.

        ASSIGN it_key_tab[ 1 ] TO FIELD-SYMBOL(<purchaseorder>).
    IF <purchaseorder> IS ASSIGNED.

    SELECT SINGLE ebeln,
       bsart as DocumentType
      from ekko
      WHERE ebeln = @<purchaseorder>-value
      INTO @data(et_data).
      endif.

      if et_data is NOT INITIAL.
        ET_ENTITYSET[ 1 ] = CORRESPONDING #( et_data ).

        ENDIF.

  endmethod.


  method POHEADER_GET_ENTITY.

*    DATA:er_entity TYPE zcl_zfdp_ef_po_glo_gen_mpc_ext=>ts_purchaseorderheader.
    ASSIGN it_key_tab[ 1 ] TO FIELD-SYMBOL(<purchaseorder>).
    IF <purchaseorder> IS ASSIGNED.
      er_entity-ebeln = <purchaseorder>-value.
    ENDIF.

  endmethod.
ENDCLASS.
************************************************************

class ZCL_ZFDP_EF_PO_GLO_GEN_DPC_EXT definition
  public
  inheriting from ZCL_ZFDP_EF_PO_GLO_GEN_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
protected section.

  methods POHEADER_GET_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_REQUEST_OBJECT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    exporting
      !ER_ENTITY type ZCL_ZFDP_EF_PO_GLO_GEN_MPC_EXT=>TS_PURCHASEORDERHEADER
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods POHEADER_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZFDP_EF_PO_GLO_GEN_MPC_EXT=>TT_PURCHASEORDERHEADER
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods PURCHASEORDERHEADERSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_REQUEST_OBJECT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    exporting
      !ER_ENTITY type ZCL_ZFDP_EF_PO_GLO_GEN_MPC_EXT=>TS_PURCHASEORDERHEADER
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods PURCHASEORDERHEA_GET_ENTITYSED
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZFDP_EF_PO_GLO_GEN_MPC_EXT=>TT_PURCHASEORDERHEADER
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
private section.
ENDCLASS.



CLASS ZCL_ZFDP_EF_PO_GLO_GEN_DPC_EXT IMPLEMENTATION.


  METHOD purchaseorderheaderset.
    ASSIGN it_key_tab[ 1 ] TO FIELD-SYMBOL(<purchaseorder>).
    IF <purchaseorder> IS ASSIGNED.
      er_entity-ebeln = <purchaseorder>-value.
    ENDIF.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
**  EXPORTING
**    iv_entity_name           =
**    iv_entity_set_name       =
**    iv_source_name           =
**    it_filter_select_options =
**    it_order                 =
**    is_paging                =
**    it_navigation_path       =
**    it_key_tab               =
**    iv_filter_string         =
**    iv_search_string         =
**    io_tech_request_context  =
**  IMPORTING
**    er_entityset             =
**    es_response_context      =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.

    DATA: POHeadToHead        TYPE TABLE OF ZCL_ZFDP_EF_PO_GLO_GEN_mPC_EXT=>ts_purchaseorderheader.
    DATA(lv_entityname) = io_tech_request_context->get_entity_set_name( ).

    CASE lv_entityname.
      WHEN 'PurchaseOrderHeaderSet'.
*        TRY.
*            poheader_get_entityset(
*              EXPORTING
*                iv_entity_name           = iv_entity_name
*                iv_entity_set_name       = iv_entity_set_name
*                iv_source_name           = iv_source_name
*                it_filter_select_options = it_filter_select_options               " Table of select options
*                is_paging                = is_paging                              " Paging structure
*                it_key_tab               = it_key_tab                             " Table for name value pairs
*                it_navigation_path       = it_navigation_path                     " Table of navigation paths
*                it_order                 = it_order                               " The sorting order
*                iv_filter_string         = iv_filter_string                       " Table for name value pairs
*                iv_search_string         = iv_search_string
*                io_tech_request_context  = io_tech_request_context
*              IMPORTING
*                et_entityset             =  DATA(Purchaseorderheader)        " Returning data
*                es_response_context      =  es_response_context
*            ).
*          CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
*          CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception(
*        ENDTRY.

          data : it_key_tabs TYPE /IWBEP/T_MGW_NAME_VALUE_PAIR .
          TYPES : tt_key_tabs TYPE /IWBEP/T_MGW_NAME_VALUE_PAIR .

          it_key_tabs = VALUE tt_key_tabs( ( name = 'PurchaseOrder' value = '4500000059') ).
        TRY.

        call METHOD poheader_get_entity
          EXPORTING
            iv_entity_name          = 'PurchaseOrderHeader'
            iv_entity_set_name      = 'PurchaseOrderHeaderSet'
            iv_source_name          = iv_source_name
            it_key_tab              = it_key_tabs                           " table for name value pairs
*            io_request_object       =                  " table of navigation paths
*            io_tech_request_context =
            it_navigation_path      = it_navigation_path                 " table of navigation paths
          IMPORTING
            er_entity               = DATA(Purchaseorderheader)                  " Returning data
*            es_response_context     =
          .
        CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
        CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception
        endtry.

        IF Purchaseorderheader IS NOT INITIAL.

          copy_data_to_ref( EXPORTING is_data = Purchaseorderheader
                            CHANGING cr_data = er_entityset ).
        ENDIF.

      WHEN OTHERS.
        TRY.

            CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~get_entityset
              EXPORTING
                iv_entity_name           = iv_entity_name                          " Obsolete
                iv_entity_set_name       = iv_entity_set_name                      " Obsolete
                iv_source_name           = iv_source_name                          " Obsolete
                it_filter_select_options = it_filter_select_options                " table of select options - Obsolete
                it_order                 = it_order                                " the sorting order - Obsolete
                is_paging                = is_paging                               " paging structure - Obsolete
                it_navigation_path       = it_navigation_path                      " table of navigation paths - Obsolete
                it_key_tab               = it_key_tab                              " table for name value pairs - Obsolete
                iv_filter_string         = iv_filter_string                        " the filter as a string containing ANDs and ORs etc -Obsolete
                iv_search_string         = iv_search_string                        " Obsolete
                io_tech_request_context  = io_tech_request_context
              IMPORTING
                er_entityset             = er_entityset
                es_response_context      = es_response_context.
          CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
          CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception

        ENDTRY.
    ENDCASE.


  ENDMETHOD.


  METHOD poheader_get_entityset.
    DATA:er_entity TYPE zcl_zfdp_ef_po_glo_gen_mpc_ext=>ts_purchaseorderheader.
    ASSIGN it_key_tab[ 1 ] TO FIELD-SYMBOL(<purchaseorder>).
    IF <purchaseorder> IS ASSIGNED.
      er_entity-ebeln = <purchaseorder>-value.
      APPEND er_entity TO et_entityset.
      CLEAR:er_entity.
    ENDIF.
  ENDMETHOD.


  method PURCHASEORDERHEA_GET_ENTITYSED.

        ASSIGN it_key_tab[ 1 ] TO FIELD-SYMBOL(<purchaseorder>).
    IF <purchaseorder> IS ASSIGNED.

    SELECT SINGLE ebeln,
       bsart as DocumentType
      from ekko
      WHERE ebeln = @<purchaseorder>-value
      INTO @data(et_data).
      endif.

      if et_data is NOT INITIAL.
        ET_ENTITYSET[ 1 ] = CORRESPONDING #( et_data ).

        ENDIF.

  endmethod.


  method POHEADER_GET_ENTITY.

*    DATA:er_entity TYPE zcl_zfdp_ef_po_glo_gen_mpc_ext=>ts_purchaseorderheader.
    ASSIGN it_key_tab[ 1 ] TO FIELD-SYMBOL(<purchaseorder>).
    IF <purchaseorder> IS ASSIGNED.
      er_entity-ebeln = <purchaseorder>-value.
    ENDIF.

  endmethod.
ENDCLASS.

******************************************************************************

class ZCL_ZFDP_EF_PO_GLO_GEN_MPC definition
  public
  inheriting from /IWBEP/CL_MGW_PUSH_ABS_MODEL
  create public .

public section.

  types:
     TS_HIERITEMPRICINGCONDITIONNOD type MMPUR_S_FDP_ITEM_PRICING_COND .
  types:
TT_HIERITEMPRICINGCONDITIONNOD type standard table of TS_HIERITEMPRICINGCONDITIONNOD .
  types:
   begin of ts_text_element,
      artifact_name  type c length 40,       " technical name
      artifact_type  type c length 4,
      parent_artifact_name type c length 40, " technical name
      parent_artifact_type type c length 4,
      text_symbol    type textpoolky,
   end of ts_text_element .
  types:
         tt_text_elements type standard table of ts_text_element with key text_symbol .
  types:
  begin of TS_INVOICINGPARTYNODE,
     LIFNR type C length 10,
     NAME1 type C length 35,
     ADRNR type C length 10,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
     STCD3 type C length 80,
     STCEG type C length 20,
  end of TS_INVOICINGPARTYNODE .
  types:
TT_INVOICINGPARTYNODE type standard table of TS_INVOICINGPARTYNODE .
  types:
     TS_ITEMPRICINGCONDITIONNODE type MMPUR_S_FDP_ITEM_PRICING_COND .
  types:
TT_ITEMPRICINGCONDITIONNODE type standard table of TS_ITEMPRICINGCONDITIONNODE .
  types:
     TS_ITEMTAXCONDITIONSNODE type MMPUR_S_FDP_ITEM_PRICING_COND .
  types:
TT_ITEMTAXCONDITIONSNODE type standard table of TS_ITEMTAXCONDITIONSNODE .
  types:
  begin of TS_ORDERINGADDRESS,
     LIFN2 type C length 10,
     NAME1 type C length 40,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
     TEL_NUMBER type C length 30,
     TELFX type C length 30,
     SMTP_ADDR type C length 241,
  end of TS_ORDERINGADDRESS .
  types:
TT_ORDERINGADDRESS type standard table of TS_ORDERINGADDRESS .
  types:
     TS_POCONFIGURATIONHIERITEMNODE type TDS_ME_PO_ITEM_CONFIG .
  types:
TT_POCONFIGURATIONHIERITEMNODE type standard table of TS_POCONFIGURATIONHIERITEMNODE .
  types:
     TS_POCONFIGURATIONITEMNODE type TDS_ME_PO_ITEM_CONFIG .
  types:
TT_POCONFIGURATIONITEMNODE type standard table of TS_POCONFIGURATIONITEMNODE .
  types:
     TS_POHIERSUBCONTRACTINGCOMPONE type TDS_ME_PO_ITEM_COMPONENT_BATCH .
  types:
TT_POHIERSUBCONTRACTINGCOMPONE type standard table of TS_POHIERSUBCONTRACTINGCOMPONE .
  types:
     TS_POHIERSUBCONTRACTINGCOMPON type TDS_ME_PO_ITEM_COMPONENTS .
  types:
TT_POHIERSUBCONTRACTINGCOMPON type standard table of TS_POHIERSUBCONTRACTINGCOMPON .
  types:
  begin of TS_POHIERSUBCONTRACTINGCOMPO,
     EBELN type C length 10,
     EBELP type C length 5,
     ETENR type C length 4,
     RESERVATIONITEM type C length 4,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_POHIERSUBCONTRACTINGCOMPO .
  types:
TT_POHIERSUBCONTRACTINGCOMPO type standard table of TS_POHIERSUBCONTRACTINGCOMPO .
  types:
     TS_POSUBCONTRACTINGCOMPONENTSB type TDS_ME_PO_ITEM_COMPONENT_BATCH .
  types:
TT_POSUBCONTRACTINGCOMPONENTSB type standard table of TS_POSUBCONTRACTINGCOMPONENTSB .
  types:
     TS_POSUBCONTRACTINGCOMPONENTSN type TDS_ME_PO_ITEM_COMPONENTS .
  types:
TT_POSUBCONTRACTINGCOMPONENTSN type standard table of TS_POSUBCONTRACTINGCOMPONENTSN .
  types:
  begin of TS_POSUBCONTRACTINGCOMPONENTST,
     EBELN type C length 10,
     EBELP type C length 5,
     ETENR type C length 4,
     RESERVATIONITEM type C length 4,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_POSUBCONTRACTINGCOMPONENTST .
  types:
TT_POSUBCONTRACTINGCOMPONENTST type standard table of TS_POSUBCONTRACTINGCOMPONENTST .
  types:
  begin of TS_PURCHASEORDERCHANGESNODE,
     EBELN type C length 10,
     CHTXT type C length 30,
  end of TS_PURCHASEORDERCHANGESNODE .
  types:
TT_PURCHASEORDERCHANGESNODE type standard table of TS_PURCHASEORDERCHANGESNODE .
  types:
  begin of TS_PURCHASEORDERHEADERSTTEXTS,
     EBELN type C length 10,
     DRUVO type C length 1,
     ESART type C length 4,
     TDOBJECT type C length 10,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     DRFLG type C length 2,
     TDOBNAME type C length 70,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERHEADERSTTEXTS .
  types:
TT_PURCHASEORDERHEADERSTTEXTS type standard table of TS_PURCHASEORDERHEADERSTTEXTS .
  types:
  begin of TS_PURCHASEORDERHEADERTEXTS,
     EBELN type C length 10,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERHEADERTEXTS .
  types:
TT_PURCHASEORDERHEADERTEXTS type standard table of TS_PURCHASEORDERHEADERTEXTS .
  types:
     TS_PURCHASEORDERHIERITEMBATCHN type TDS_ME_PO_ITEM_BATCH .
  types:
TT_PURCHASEORDERHIERITEMBATCHN type standard table of TS_PURCHASEORDERHIERITEMBATCHN .
  types:
  begin of TS_PURCHASEORDERHIERITEMCHANGE,
     EBELN type C length 10,
     EBELP type C length 5,
     CHTXT type C length 30,
  end of TS_PURCHASEORDERHIERITEMCHANGE .
  types:
TT_PURCHASEORDERHIERITEMCHANGE type standard table of TS_PURCHASEORDERHIERITEMCHANGE .
  types:
     TS_PURCHASEORDERHIERITEMNODE type TDS_ME_PO_ITEM .
  types:
TT_PURCHASEORDERHIERITEMNODE type standard table of TS_PURCHASEORDERHIERITEMNODE .
  types:
  begin of TS_PURCHASEORDERHIERITEMSTTEXT,
     EBELN type C length 10,
     EBELP type C length 5,
     DRUVO type C length 1,
     ESART type C length 4,
     PSTYP type C length 1,
     TDOBJECT type C length 10,
     LANGUAGE type C length 2,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     DRFLG type C length 2,
     DRPRI type C length 1,
     TDOBNAME type C length 70,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERHIERITEMSTTEXT .
  types:
TT_PURCHASEORDERHIERITEMSTTEXT type standard table of TS_PURCHASEORDERHIERITEMSTTEXT .
  types:
  begin of TS_PURCHASEORDERHIERITEMTEXTS,
     EBELN type C length 10,
     EBELP type C length 5,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERHIERITEMTEXTS .
  types:
TT_PURCHASEORDERHIERITEMTEXTS type standard table of TS_PURCHASEORDERHIERITEMTEXTS .
  types:
     TS_PURCHASEORDERHIERSCHEDULELI type TDS_ME_PO_SCHEDULELINE .
  types:
TT_PURCHASEORDERHIERSCHEDULELI type standard table of TS_PURCHASEORDERHIERSCHEDULELI .
  types:
     TS_PURCHASEORDERITEMBATCHNODE type TDS_ME_PO_ITEM_BATCH .
  types:
TT_PURCHASEORDERITEMBATCHNODE type standard table of TS_PURCHASEORDERITEMBATCHNODE .
  types:
  begin of TS_PURCHASEORDERITEMCHANGESNOD,
     EBELN type C length 10,
     EBELP type C length 5,
     CHTXT type C length 30,
  end of TS_PURCHASEORDERITEMCHANGESNOD .
  types:
TT_PURCHASEORDERITEMCHANGESNOD type standard table of TS_PURCHASEORDERITEMCHANGESNOD .
  types:
     TS_PURCHASEORDERITEMNODE type TDS_ME_PO_ITEM .
  types:
TT_PURCHASEORDERITEMNODE type standard table of TS_PURCHASEORDERITEMNODE .
  types:
  begin of TS_PURCHASEORDERITEMSTTEXTS,
     EBELN type C length 10,
     EBELP type C length 5,
     DRUVO type C length 1,
     ESART type C length 4,
     PSTYP type C length 1,
     TDOBJECT type C length 10,
     LANGUAGE type C length 2,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     DRFLG type C length 2,
     DRPRI type C length 1,
     TDOBNAME type C length 70,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERITEMSTTEXTS .
  types:
TT_PURCHASEORDERITEMSTTEXTS type standard table of TS_PURCHASEORDERITEMSTTEXTS .
  types:
  begin of TS_PURCHASEORDERITEMTEXTS,
     EBELN type C length 10,
     EBELP type C length 5,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERITEMTEXTS .
  types:
TT_PURCHASEORDERITEMTEXTS type standard table of TS_PURCHASEORDERITEMTEXTS .
  types:
  begin of TS_PURCHASEORDERLIMITITEMCHANG,
     EBELN type C length 10,
     EBELP type C length 5,
     CHTXT type C length 30,
  end of TS_PURCHASEORDERLIMITITEMCHANG .
  types:
TT_PURCHASEORDERLIMITITEMCHANG type standard table of TS_PURCHASEORDERLIMITITEMCHANG .
  types:
  begin of TS_PURCHASEORDERLIMITITEMNODE,
     EBELN type C length 10,
     EBELP type C length 5,
     PSTYP type C length 1,
     TXZ01 type C length 40,
     MATKL type C length 9,
     WERKS type C length 4,
     EXPECTED_VALUE type P length 8 decimals 3,
     NETPR type P length 7 decimals 3,
     NETWR type P length 9 decimals 3,
     WAERS type C length 5,
     MMPUR_SERVPROC_PERIOD_START type TIMESTAMP,
     MMPUR_SERVPROC_PERIOD_END type TIMESTAMP,
     ADRNR type C length 10,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
     SERVICEPERFORMER type C length 10,
     SERVICEPERFORMERNAME type C length 80,
     PRODUCTTYPE type C length 2,
     PEINH type P length 3 decimals 0,
     LABNR type C length 20,
     KTMNG type P length 7 decimals 3,
     WEMNG type P length 7 decimals 3,
     WAMNG type P length 7 decimals 3,
     PRSDR type C length 1,
     LOEKZ type C length 1,
     RETPO type C length 1,
     KZABS type FLAG,
     ELIKZ type C length 1,
  end of TS_PURCHASEORDERLIMITITEMNODE .
  types:
TT_PURCHASEORDERLIMITITEMNODE type standard table of TS_PURCHASEORDERLIMITITEMNODE .
  types:
  begin of TS_PURCHASEORDERLIMITITEMSTTEX,
     EBELN type C length 10,
     EBELP type C length 5,
     DRUVO type C length 1,
     ESART type C length 4,
     PSTYP type C length 1,
     TDOBJECT type C length 10,
     LANGUAGE type C length 2,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     DRFLG type C length 2,
     DRPRI type C length 1,
     TDOBNAME type C length 70,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERLIMITITEMSTTEX .
  types:
TT_PURCHASEORDERLIMITITEMSTTEX type standard table of TS_PURCHASEORDERLIMITITEMSTTEX .
  types:
  begin of TS_PURCHASEORDERLIMITITEMTEXTS,
     EBELN type C length 10,
     EBELP type C length 5,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERLIMITITEMTEXTS .
  types:
TT_PURCHASEORDERLIMITITEMTEXTS type standard table of TS_PURCHASEORDERLIMITITEMTEXTS .
  types:
     TS_PURCHASEORDERNODE type TDS_ME_PO_HEADER .
  types:
TT_PURCHASEORDERNODE type standard table of TS_PURCHASEORDERNODE .
  types:
     TS_PURCHASEORDERSCHEDULELINENO type TDS_ME_PO_SCHEDULELINE .
  types:
TT_PURCHASEORDERSCHEDULELINENO type standard table of TS_PURCHASEORDERSCHEDULELINENO .
  types:
  begin of TS_PURCHASINGGROUPNODE,
     EKGRP type C length 3,
     EKNAM type C length 18,
     TEL_NUMBER type C length 30,
     TELFX type C length 31,
     SMTP_ADDR type C length 241,
  end of TS_PURCHASINGGROUPNODE .
  types:
TT_PURCHASINGGROUPNODE type standard table of TS_PURCHASINGGROUPNODE .
  types:
  begin of TS_QUERYNODE,
     EBELN type C length 10,
     LAND1 type C length 3,
     LANGU type C length 1,
     CHANGE_FLAG type C length 1,
     LIFN2 type C length 10,
     OUTPUTTYPE type C length 30,
     OUTPUTPREVIEW type C length 1,
  end of TS_QUERYNODE .
  types:
TT_QUERYNODE type standard table of TS_QUERYNODE .
  types:
  begin of TS_SHIPTOPARTYNODE,
     ADRNR type C length 10,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
     STCD3 type C length 80,
  end of TS_SHIPTOPARTYNODE .
  types:
TT_SHIPTOPARTYNODE type standard table of TS_SHIPTOPARTYNODE .
  types:
  begin of TS_SUPPLIERNODE,
     LIFNR type C length 10,
     NAME1 type C length 35,
     ADRNR type C length 10,
     SMTP_ADDR type C length 241,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
     STCEG type C length 20,
     STCD3 type C length 80,
     TELF1 type C length 16,
     TELF2 type C length 16,
  end of TS_SUPPLIERNODE .
  types:
TT_SUPPLIERNODE type standard table of TS_SUPPLIERNODE .
  types:
     TS_TAXSUMMARYNODE type MMPUR_S_FDP_ITEM_PRICING_COND .
  types:
TT_TAXSUMMARYNODE type standard table of TS_TAXSUMMARYNODE .
  types:
  begin of TS_TOTALAMOUNTSNODE,
     PURCHASE_ORDER type C length 10,
     GROSS_AMOUNT type P length 13 decimals 2,
     TOTAL_TAX_AMOUNT type P length 13 decimals 2,
     GROSS_IN_WORDS type string,
  end of TS_TOTALAMOUNTSNODE .
  types:
TT_TOTALAMOUNTSNODE type standard table of TS_TOTALAMOUNTSNODE .
  types:
  begin of TS_PURCHASEORDERHEADER,
     BSART type C length 4,
     EBELN type C length 10,
  end of TS_PURCHASEORDERHEADER .
  types:
TT_PURCHASEORDERHEADER type standard table of TS_PURCHASEORDERHEADER .
  types:
  begin of TS_POHEADER,
     EBELN type C length 10,
  end of TS_POHEADER .
  types:
TT_POHEADER type standard table of TS_POHEADER .
  types:
  begin of TS_CUSTOMPOHEADFIELDS,
     BSART type C length 4,
     EBELN type C length 10,
  end of TS_CUSTOMPOHEADFIELDS .
  types:
TT_CUSTOMPOHEADFIELDS type standard table of TS_CUSTOMPOHEADFIELDS .

  constants GC_TOTALAMOUNTSNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'TotalAmountsNode' ##NO_TEXT.
  constants GC_TAXSUMMARYNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'TaxSummaryNode' ##NO_TEXT.
  constants GC_SUPPLIERNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'SupplierNode' ##NO_TEXT.
  constants GC_SHIPTOPARTYNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ShipToPartyNode' ##NO_TEXT.
  constants GC_QUERYNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'QueryNode' ##NO_TEXT.
  constants GC_PURCHASINGGROUPNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchasingGroupNode' ##NO_TEXT.
  constants GC_PURCHASEORDERSCHEDULELINENO type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderScheduleLineNode' ##NO_TEXT.
  constants GC_PURCHASEORDERNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderNode' ##NO_TEXT.
  constants GC_PURCHASEORDERLIMITITEMTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderLimitItemTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERLIMITITEMSTTEX type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderLimitItemSTTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERLIMITITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderLimitItemNode' ##NO_TEXT.
  constants GC_PURCHASEORDERLIMITITEMCHANG type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderLimitItemChangesNode' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMSTTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemSTTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemNode' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMCHANGESNOD type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemChangesNode' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMBATCHNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemBatchNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERSCHEDULELI type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierScheduleLineNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMSTTEXT type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemSTTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMCHANGE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemChangesNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMBATCHN type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemBatchNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHEADERTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHeaderTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERHEADERSTTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHeaderSTTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERHEADER type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHeader' ##NO_TEXT.
  constants GC_PURCHASEORDERCHANGESNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderChangesNode' ##NO_TEXT.
  constants GC_POSUBCONTRACTINGCOMPONENTST type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POSubcontractingComponentsTexts' ##NO_TEXT.
  constants GC_POSUBCONTRACTINGCOMPONENTSN type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POSubcontractingComponentsNode' ##NO_TEXT.
  constants GC_POSUBCONTRACTINGCOMPONENTSB type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POSubcontractingComponentsBatchNode' ##NO_TEXT.
  constants GC_POHIERSUBCONTRACTINGCOMPONE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POHierSubcontractingComponentsBatchNode' ##NO_TEXT.
  constants GC_POHIERSUBCONTRACTINGCOMPON type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POHierSubcontractingComponentsNode' ##NO_TEXT.
  constants GC_POHIERSUBCONTRACTINGCOMPO type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POHierSubcontractingComponentsTexts' ##NO_TEXT.
  constants GC_POHEADER type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POHeader' ##NO_TEXT.
  constants GC_POCONFIGURATIONITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POConfigurationItemNode' ##NO_TEXT.
  constants GC_POCONFIGURATIONHIERITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POConfigurationHierItemNode' ##NO_TEXT.
  constants GC_ORDERINGADDRESS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'OrderingAddress' ##NO_TEXT.
  constants GC_ITEMTAXCONDITIONSNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ItemTaxConditionsNode' ##NO_TEXT.
  constants GC_ITEMPRICINGCONDITIONNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ItemPricingConditionNode' ##NO_TEXT.
  constants GC_INVOICINGPARTYNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'InvoicingPartyNode' ##NO_TEXT.
  constants GC_HIERITEMPRICINGCONDITIONNOD type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'HierItemPricingConditionNode' ##NO_TEXT.
  constants GC_CUSTOMPOHEADFIELDS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'CustomPOHeadFields' ##NO_TEXT.

  methods GET_EXTENDED_MODEL
  final
    exporting
      !EV_EXTENDED_SERVICE type /IWBEP/MED_GRP_TECHNICAL_NAME
      !EV_EXT_SERVICE_VERSION type /IWBEP/MED_GRP_VERSION
      !EV_EXTENDED_MODEL type /IWBEP/MED_MDL_TECHNICAL_NAME
      !EV_EXT_MODEL_VERSION type /IWBEP/MED_MDL_VERSION
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods LOAD_TEXT_ELEMENTS
  final
    returning
      value(RT_TEXT_ELEMENTS) type TT_TEXT_ELEMENTS
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .

  methods DEFINE
    redefinition .
  methods GET_LAST_MODIFIED
    redefinition .
protected section.
private section.

  methods CREATE_NEW_ARTIFACTS
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
ENDCLASS.



CLASS ZCL_ZFDP_EF_PO_GLO_GEN_MPC IMPLEMENTATION.


  method CREATE_NEW_ARTIFACTS.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


DATA:
  lo_entity_type    TYPE REF TO /iwbep/if_mgw_odata_entity_typ,                      "#EC NEEDED
  lo_complex_type   TYPE REF TO /iwbep/if_mgw_odata_cmplx_type,                      "#EC NEEDED
  lo_property       TYPE REF TO /iwbep/if_mgw_odata_property,                        "#EC NEEDED
  lo_association    TYPE REF TO /iwbep/if_mgw_odata_assoc,                           "#EC NEEDED
  lo_assoc_set      TYPE REF TO /iwbep/if_mgw_odata_assoc_set,                       "#EC NEEDED
  lo_ref_constraint TYPE REF TO /iwbep/if_mgw_odata_ref_constr,                      "#EC NEEDED
  lo_nav_property   TYPE REF TO /iwbep/if_mgw_odata_nav_prop,                        "#EC NEEDED
  lo_action         TYPE REF TO /iwbep/if_mgw_odata_action,                          "#EC NEEDED
  lo_parameter      TYPE REF TO /iwbep/if_mgw_odata_property,                        "#EC NEEDED
  lo_entity_set     TYPE REF TO /iwbep/if_mgw_odata_entity_set.                      "#EC NEEDED


***********************************************************************************************************************************
*   ENTITY - PurchaseOrderHeader
***********************************************************************************************************************************
lo_entity_type = model->create_entity_type( iv_entity_type_name = 'PurchaseOrderHeader' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'DocumentType' iv_abap_fieldname = 'BSART' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'PurchaseOrder' iv_abap_fieldname = 'EBELN' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZFDP_EF_PO_GLO_GEN_MPC=>TS_PURCHASEORDERHEADER' ). "#EC NOTEXT


lo_entity_type = model->create_entity_type( iv_entity_type_name = 'POHeader' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'PurchaseOrder' iv_abap_fieldname = 'EBELN' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZFDP_EF_PO_GLO_GEN_MPC=>TS_POHEADER' ). "#EC NOTEXT


lo_entity_type = model->create_entity_type( iv_entity_type_name = 'CustomPOHeadFields' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'DocumentType' iv_abap_fieldname = 'BSART' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_true ).
lo_property->set_updatable( abap_true ).
lo_property->set_sortable( abap_true ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'PurchaseOrder' iv_abap_fieldname = 'EBELN' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_creatable( abap_true ).
lo_property->set_updatable( abap_true ).
lo_property->set_sortable( abap_true ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_true ).

lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZFDP_EF_PO_GLO_GEN_MPC=>TS_CUSTOMPOHEADFIELDS' ). "#EC NOTEXT


***********************************************************************************************************************************
*   ENTITY SETS
***********************************************************************************************************************************
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderHeader' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'PurchaseOrderHeaderSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_false ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
lo_entity_type = model->get_entity_type( iv_entity_name = 'POHeader' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'POHeaderSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_false ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
lo_entity_type = model->get_entity_type( iv_entity_name = 'CustomPOHeadFields' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'CustomPOHeadFieldsSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_false ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).


***********************************************************************************************************************************
*   new_associations
***********************************************************************************************************************************

 lo_association = model->create_association(
                            iv_association_name = 'POHeadToHead' "#EC NOTEXT
                            iv_left_type        = 'PurchaseOrderNode' "#EC NOTEXT
                            iv_right_type       = 'PurchaseOrderHeader' "#EC NOTEXT
                            iv_right_card       = '1' "#EC NOTEXT
                            iv_left_card        = '0' ). "#EC NOTEXT
* Referential constraint for association - POHeadToHead
lo_ref_constraint = lo_association->create_ref_constraint( ).
lo_ref_constraint->add_property( iv_principal_property = 'PurchaseOrder'   iv_dependent_property = 'PurchaseOrder' )."#EC NOTEXT
* Association Sets for association - POHeadToHead
lo_assoc_set = lo_association->create_assoc_set( iv_assoc_set_name = 'POHeadToHeadSet' ). "#EC NOTEXT
 lo_association = model->create_association(
                            iv_association_name = 'PoHeaderToHeader' "#EC NOTEXT
                            iv_left_type        = 'PurchaseOrderHeaderTexts' "#EC NOTEXT
                            iv_right_type       = 'POHeader' "#EC NOTEXT
                            iv_right_card       = '1' "#EC NOTEXT
                            iv_left_card        = '0' ). "#EC NOTEXT
* Referential constraint for association - PoHeaderToHeader
lo_ref_constraint = lo_association->create_ref_constraint( ).
lo_ref_constraint->add_property( iv_principal_property = 'PurchaseOrder'   iv_dependent_property = 'PurchaseOrder' )."#EC NOTEXT
* Association Sets for association - PoHeaderToHeader
lo_assoc_set = lo_association->create_assoc_set( iv_assoc_set_name = 'PoHeaderToHeaderSet' ). "#EC NOTEXT


   lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderHeaderTexts' ). "#EC NOTEXT
   lo_nav_property = lo_entity_type->create_navigation_property( iv_property_name  = 'PoHeaderToHeader' "#EC NOTEXT
                                                          iv_association_name = 'PoHeaderToHeader' ). "#EC NOTEXT
   lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
   lo_nav_property = lo_entity_type->create_navigation_property( iv_property_name  = 'POHeadToHead' "#EC NOTEXT
                                                          iv_association_name = 'POHeadToHead' ). "#EC NOTEXT
  endmethod.


  method DEFINE.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


data:
  lo_entity_type    type ref to /iwbep/if_mgw_odata_entity_typ, "#EC NEEDED
  lo_complex_type   type ref to /iwbep/if_mgw_odata_cmplx_type, "#EC NEEDED
  lo_property       type ref to /iwbep/if_mgw_odata_property, "#EC NEEDED
  lo_association    type ref to /iwbep/if_mgw_odata_assoc,  "#EC NEEDED
  lo_assoc_set      type ref to /iwbep/if_mgw_odata_assoc_set, "#EC NEEDED
  lo_ref_constraint type ref to /iwbep/if_mgw_odata_ref_constr, "#EC NEEDED
  lo_nav_property   type ref to /iwbep/if_mgw_odata_nav_prop, "#EC NEEDED
  lo_action         type ref to /iwbep/if_mgw_odata_action, "#EC NEEDED
  lo_parameter      type ref to /iwbep/if_mgw_odata_property, "#EC NEEDED
  lo_entity_set     type ref to /iwbep/if_mgw_odata_entity_set, "#EC NEEDED
  lo_complex_prop   type ref to /iwbep/if_mgw_odata_cmplx_prop. "#EC NEEDED

* Extend the model
model->extend_model( iv_model_name = 'FDP_EF_PURCHASE_ORDER_GLO_GEN_MD' iv_model_version = '0001' ). "#EC NOTEXT

model->set_schema_namespace( 'FDP_EF_PURCHASE_ORDER_SRV' ).


*
* Disable all the entity types that were disabled from reference model
*
* Disable entity type 'InvoicingPartner'
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'InvoicingPartner' ). "#EC NOTEXT
lo_entity_type->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.

IF lo_entity_type IS BOUND.
* Disable all the properties for this entity type
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'PartnerNumber' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'PartnerName' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine1' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine2' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine3' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine4' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine5' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine6' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine7' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine8' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'TelephoneNumber' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'Fax' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'EmailAddress' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.


endif.

* Disable entity type 'PurchaseOrderItemManufacturerNode'
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemManufacturerNode' ). "#EC NOTEXT
lo_entity_type->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.

IF lo_entity_type IS BOUND.
* Disable all the properties for this entity type
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'Manufacturer' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'PurchaseOrder' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'PurchaseOrderItem' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'ManufacturerPartNumber' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'ManufacturerName' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.


endif.


*Disable selected properties in a entity type
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ShippingType' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ShippingTypeDescription' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ZZ1_test_PDH' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ZZ1_test_PDHF' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'GoodsCountCorrection' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ShippingType' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'DeliveryDocumentBySupplier' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ShippingTypeDescription' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'DeliveryDocumentItemBySupplier' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'PPSOPTIONALITEM_TR' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'POSubcontractingComponentsNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'MaterialRevisionLevel' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderHierItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'PPSOPTIONALITEM_TR' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.

*Disable selected navigation properties in a entity type
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_nav_property = lo_entity_type->get_navigation_property( iv_name    = 'InvoicingPartner' ). "#EC NOTEXT
lo_nav_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_nav_property = lo_entity_type->get_navigation_property( iv_name    = 'PurchaseOrderItemManufacturerNode' ). "#EC NOTEXT
lo_nav_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderHierItemNode' ). "#EC NOTEXT
lo_nav_property = lo_entity_type->get_navigation_property( iv_name    = 'PurchaseOrderItemManufacturerNode' ). "#EC NOTEXT
lo_nav_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.


*
*Disable all the entity sets that were disabled from reference model
*
try.
lo_entity_set = model->get_entity_set( iv_entity_set_name = 'InvoicingPartnerSet' ). "#EC NOTEXT
IF lo_entity_set IS BOUND.
lo_entity_set->set_disabled( iv_disabled = abap_true ).
ENDIF.
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_set = model->get_entity_set( iv_entity_set_name = 'PurchaseOrderItemManufacturerNodeSet' ). "#EC NOTEXT
IF lo_entity_set IS BOUND.
lo_entity_set->set_disabled( iv_disabled = abap_true ).
ENDIF.
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.


*
*Disable all the associations, association sets that were disabled from reference model
*
try.
lo_association = model->get_association( iv_association_name = 'POHeader_InvoicingPartner' ). "#EC NOTEXT
lo_association->set_disabled( iv_disabled = abap_true ).
lo_ref_constraint = lo_association->get_ref_constraint( ).
lo_ref_constraint->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_association = model->get_association( iv_association_name = 'POItem_POManufacturer' ). "#EC NOTEXT
lo_association->set_disabled( iv_disabled = abap_true ).
lo_ref_constraint = lo_association->get_ref_constraint( ).
lo_ref_constraint->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_association = model->get_association( iv_association_name = 'POHierItem_POManufacturer' ). "#EC NOTEXT
lo_association->set_disabled( iv_disabled = abap_true ).
lo_ref_constraint = lo_association->get_ref_constraint( ).
lo_ref_constraint->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.


try.
lo_assoc_set  = model->get_association_set( iv_assoc_set_name = 'POHeader_InvoicingPartnerSet' ). "#EC NOTEXT
IF lo_assoc_set IS BOUND.
lo_assoc_set->set_disabled( iv_disabled = abap_true ).
ENDIF.
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_assoc_set  = model->get_association_set( iv_assoc_set_name = 'POItem_POManufacturerSet' ). "#EC NOTEXT
IF lo_assoc_set IS BOUND.
lo_assoc_set->set_disabled( iv_disabled = abap_true ).
ENDIF.
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_assoc_set  = model->get_association_set( iv_assoc_set_name = 'POHierItem_POManufacturerSet' ). "#EC NOTEXT
IF lo_assoc_set IS BOUND.
lo_assoc_set->set_disabled( iv_disabled = abap_true ).
ENDIF.
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
* New artifacts have been created in the service builder after the redefinition of service
create_new_artifacts( ).
  endmethod.


  method GET_EXTENDED_MODEL.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*



ev_extended_service  = 'FDP_EF_PURCHASE_ORDER_GLO_GEN_SRV'.                "#EC NOTEXT
ev_ext_service_version = '0001'.               "#EC NOTEXT
ev_extended_model    = 'FDP_EF_PURCHASE_ORDER_GLO_GEN_MD'.                    "#EC NOTEXT
ev_ext_model_version = '0001'.                   "#EC NOTEXT
  endmethod.


  method GET_LAST_MODIFIED.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


  constants: lc_gen_date_time type timestamp value '20250325074059'. "#EC NOTEXT
rv_last_modified = super->get_last_modified( ).
IF rv_last_modified LT lc_gen_date_time.
  rv_last_modified = lc_gen_date_time.
ENDIF.
  endmethod.


  method LOAD_TEXT_ELEMENTS.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


data:
  lo_entity_type    type ref to /iwbep/if_mgw_odata_entity_typ,           "#EC NEEDED
  lo_complex_type   type ref to /iwbep/if_mgw_odata_cmplx_type,           "#EC NEEDED
  lo_property       type ref to /iwbep/if_mgw_odata_property,             "#EC NEEDED
  lo_association    type ref to /iwbep/if_mgw_odata_assoc,                "#EC NEEDED
  lo_assoc_set      type ref to /iwbep/if_mgw_odata_assoc_set,            "#EC NEEDED
  lo_ref_constraint type ref to /iwbep/if_mgw_odata_ref_constr,           "#EC NEEDED
  lo_nav_property   type ref to /iwbep/if_mgw_odata_nav_prop,             "#EC NEEDED
  lo_action         type ref to /iwbep/if_mgw_odata_action,               "#EC NEEDED
  lo_parameter      type ref to /iwbep/if_mgw_odata_property,             "#EC NEEDED
  lo_entity_set     type ref to /iwbep/if_mgw_odata_entity_set.           "#EC NEEDED


DATA:
     ls_text_element TYPE ts_text_element.                   "#EC NEEDED
  endmethod.
ENDCLASS.
*************************************************************************************

class ZCL_ZFDP_EF_PO_GLO_GEN_MPC_EXT definition
  public
  inheriting from ZCL_ZFDP_EF_PO_GLO_GEN_MPC
  create public .

public section.
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZFDP_EF_PO_GLO_GEN_MPC_EXT IMPLEMENTATION.
ENDCLASS.
*********************************************************

class ZCL_ZFDP_EF_PURCHAS_03_DPC definition
  public
  inheriting from CL_FDP_EF_PURCHASE__03_DPC_EXT
  abstract
  create public .

public section.
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZFDP_EF_PURCHAS_03_DPC IMPLEMENTATION.
ENDCLASS.

**********************************************************************

class ZCL_ZFDP_EF_PURCHAS_03_DPC_EXT definition
  public
  inheriting from ZCL_ZFDP_EF_PURCHAS_03_DPC
  create public .

public section.

  constants OBJECT type TDOBJECT value 'EKPO' ##NO_TEXT. "EKPO" ##NO_TEXT.
  constants DELIVERY_DATE type CHAR16 value 'Delivery Date :' ##NO_TEXT.
  constants ITEM_TEXT type CHAR15 value 'Item Text :' ##NO_TEXT.
  constants PURCHASE_ORDER type TDOBNAME value 'PurchaseOrder' ##NO_TEXT.
  constants ITEM type EBELP value '00010' ##NO_TEXT.
  constants INDIA type LAND1 value 'IN' ##NO_TEXT.
  constants CIN type PARTY value 'CIN' ##NO_TEXT.
  constants ONLY type CHAR4 value 'ONLY' ##NO_TEXT.
  constants OBJECT_EKKO type TDOBJECT value 'EKKO' ##NO_TEXT.
  constants ID type THEAD-TDID value 'F03' ##NO_TEXT.
  constants MATERIAL_DESCRIPTION type CHAR25 value 'Material Description :' ##NO_TEXT.
  data ITERATION type SY-TABIX .

  methods HEADER_GET_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_REQUEST_OBJECT type ref to /IWBEP/IF_MGW_REQ_ENTITY
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    exporting
      !ER_ENTITY type ZCL_ZFDP_EF_PURCHAS_03_MPC=>TS_HEADER
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_ITEM_TEXT
    importing
      !IV_ID type THEAD-TDID
      !IV_NAME type THEAD-TDNAME
    returning
      value(ET_TEXT_LINE) type TTTEXT .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
protected section.

  methods ITEMS_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZFDP_EF_PURCHAS_03_MPC=>TT_ITEMS
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods READ_TEXT
    importing
      !PURCHASE_ORDER type EBELN
      !PURCHASE_ORDER_ITEM type EBELP
      value(DELIVERY_DATE) type DATS
    returning
      value(ITEM_TEXT) type STRING .
private section.
ENDCLASS.



CLASS ZCL_ZFDP_EF_PURCHAS_03_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entity.
*----------------------------------------------------------------------*
* Report  ZCL_ZFDP_EF_PURCHAS_03_DPC_EXT                               *
*----------------------------------------------------------------------*
* Title:          Purchase Order Print                                 *
* RICEF#:         MM 01                                                *
* Transaction:    ME23N                                                *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         RSB Transmission                                     *
*----------------------------------------------------------------------*
* Developer:      Asafwar Shivam                                       *
* Creation Date:  24-MAR-2025                                          *                       *
*----------------------------------------------------------------------*


    DATA: poheadtohead        TYPE TABLE OF zcl_zfdp_ef_purchas_03_mpc_ext=>ts_header.
    DATA(entityset) = io_tech_request_context->get_entity_set_name( ).

    CASE entityset.
        "  Calling Header Method
      WHEN text-019.                                                              "'HeaderSet'.
        TRY.
            header_get_entity(
              EXPORTING
                iv_entity_name          = iv_entity_name
                iv_entity_set_name      = iv_entity_set_name
                iv_source_name          = iv_source_name
                it_key_tab              = it_key_tab                              " table for name value pairs
                io_request_object       = io_tech_request_context                 " Request Details for Entity Read Operation
                io_tech_request_context = io_tech_request_context                 " Request Details for Entity Read Operation
                it_navigation_path      = it_navigation_path                      " table of navigation paths
              IMPORTING
                er_entity               = DATA(purchaseorderheader) ).

          CATCH /iwbep/cx_mgw_busi_exception. " Business Exception
          CATCH /iwbep/cx_mgw_tech_exception. " Technical Exception

        ENDTRY.

        IF purchaseorderheader IS NOT INITIAL.

          copy_data_to_ref( EXPORTING is_data = purchaseorderheader
                            CHANGING cr_data = er_entity ).
          CLEAR : purchaseorderheader.
        ENDIF.

      WHEN OTHERS.
        TRY.

            CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~get_entity
              EXPORTING
                iv_entity_name          = iv_entity_name
                iv_entity_set_name      = iv_entity_set_name
                iv_source_name          = iv_source_name
                it_key_tab              = it_key_tab
                io_tech_request_context = io_tech_request_context
                it_navigation_path      = it_navigation_path
              IMPORTING
                er_entity               = er_entity.

          CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
          CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception

        ENDTRY.

    ENDCASE.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.
*----------------------------------------------------------------------*
* Report  ZCL_ZFDP_EF_PURCHAS_03_DPC_EXT                               *
*----------------------------------------------------------------------*
* Title:          Purchase Order Print                                 *
* RICEF#:         MM 01                                                *
* Transaction:    ME23N                                                *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         RSB Transmission                                     *
*----------------------------------------------------------------------*
* Developer:      Asafwar Shivam                                       *
* Creation Date:  24-MAR-2025                                          *                       *
*----------------------------------------------------------------------*


    DATA: poheadtohead        TYPE TABLE OF zcl_zfdp_ef_purchas_03_mpc_ext=>ts_header.
    DATA(entityset) = io_tech_request_context->get_entity_set_name( ).

    "  calling items method
    IF  entityset = TEXT-018.                                                    "'ItemsSet'.
      TRY.
          CALL METHOD items_get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options                 " Table of select options
              is_paging                = is_paging                                " Paging structure
              it_key_tab               = it_key_tab                               " Table for name value pairs
              it_navigation_path       = it_navigation_path                       " Table of navigation paths
              it_order                 = it_order                                 " The sorting order
              iv_filter_string         = iv_filter_string                         " Table for name value pairs
              iv_search_string         = iv_search_string
*             io_tech_request_context  = io_tech_request_context
            IMPORTING                 "  MPORTING
              et_entityset             = DATA(po_entityset)                             " Returning data
              es_response_context      = es_response_context.
        CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
        CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception
      ENDTRY.

      IF po_entityset IS NOT INITIAL.

        copy_data_to_ref( EXPORTING is_data = po_entityset
                          CHANGING cr_data = er_entityset ).
        CLEAR : po_entityset.

      ENDIF.

    ELSE.
      TRY.
          CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~get_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name                           " Obsolete
              iv_entity_set_name       = iv_entity_set_name                       " Obsolete
              iv_source_name           = iv_source_name                           " Obsolete
              it_filter_select_options = it_filter_select_options                 " table of select options - Obsolete
              it_order                 = it_order                                 " the sorting order - Obsolete
              is_paging                = is_paging                                " paging structure - Obsolete
              it_navigation_path       = it_navigation_path                       " table of navigation paths - Obsolete
              it_key_tab               = it_key_tab                               " table for name value pairs - Obsolete
              iv_filter_string         = iv_filter_string                         " the filter as a string containing ANDs and ORs etc -Obsolete
              iv_search_string         = iv_search_string                         " Obsolete
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              er_entityset             = er_entityset
              es_response_context      = es_response_context.

        CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
        CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD get_item_text.
*----------------------------------------------------------------------*
* Report  ZCL_ZFDP_EF_PURCHAS_03_DPC_EXT                               *
*----------------------------------------------------------------------*
* Title:          Purchase Order Print                                 *
* RICEF#:         MM 01                                                *
* Transaction:    ME23N                                                *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         RSB Transmission                                     *
*----------------------------------------------------------------------*
* Developer:      Asafwar Shivam                                       *
* Creation Date:  24-MAR-2025                                          *                       *
*----------------------------------------------------------------------*


"   Fetching the line item Text details
    CALL FUNCTION 'READ_TEXT'                                       ##FM_SUBRC_OK
      EXPORTING
        client                  = sy-mandt                          " Client
        id                      = iv_id                             " Text ID of text to be read
        language                = /isdfps/cl_const_abc_123=>gc_e    " Language of text to be read
        name                    = iv_name                           " Name of text to be read
        object                  = object                            " Object of text to be read
      TABLES
        lines                   = et_text_line                      " Lines of text read
      EXCEPTIONS
        id                      = 1                                 " Text ID invalid
        language                = 2                                 " Invalid language
        name                    = 3                                 " Invalid text name
        not_found               = 4                                 " Text not found
        object                  = 5                                 " Invalid text object
        reference_check         = 6                                 " Reference chain interrupted
        wrong_access_to_archive = 7                                 " Archive handle invalid for access
        OTHERS                  = 8.


  ENDMETHOD.


  METHOD header_get_entity.
*----------------------------------------------------------------------*
* Report  ZCL_ZFDP_EF_PURCHAS_03_DPC_EXT                               *
*----------------------------------------------------------------------*
* Title:          Purchase Order Print                                 *
* RICEF#:         MM 01                                                *
* Transaction:    ME23N                                                *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         RSB Transmission                                     *
*----------------------------------------------------------------------*
* Developer:      Asafwar Shivam                                       *
* Creation Date:  24-MAR-2025                                          *                       *
*----------------------------------------------------------------------*


    DATA : bill_to_pan        TYPE char18,
           ship_to_address_no TYPE adrnr,
           ship_to_pan        TYPE char18,
           buyer_name         TYPE usr03,
           supplier_plant     TYPE lifnr,
           name               TYPE thead-tdname,
           dynamic1           TYPE char40,
           dynamic2           TYPE char40.

    "   data Declaration For Purchase Order.
    DATA(purchaseorder) = VALUE #( it_key_tab[ name = purchase_order ]-value OPTIONAL ).

    "   Fetching The Bill To Address Details.
    SELECT SINGLE
            address~addresseename1,                    "#EC CI_BUFFJOIN
            address~addresseename2,
            address~streetname,
            address~streetprefixname1,
            address~streetprefixname2,
            address~streetsuffixname1,
            address~cityname,
            address~postalcode,
            plant~adrnr,
            plant~werks AS plant,
            plant~j_1bbranch,
            address~region,
            purchasingdocument~companycode,
            purchasingdocument~supplier,
            purchasingdocument~supplyingplant,
            purchasingdocument~incotermsclassification,
            purchasingdocument~incotermstransferlocation,
            purchasingdocument~paymentterms,
            purchasingdocumentitem~customer,
            purchasingdocumentitem~subcontractor,
            purchasingdocumentitem~manualdeliveryaddressid,
            purchasingdocument~purchasingdocumentcategory,
            purchasingdocument~purchasingdocumenttype,
            purchasingdocument~createdbyuser,
            purchasingdocument~purchasinggroup,
            account_assignment~aufnr AS internal_order_no,
            order_master_data~ktext AS internal_order_desc
      FROM i_purchasingdocumentitem AS purchasingdocumentitem
                            LEFT OUTER JOIN i_purchasingdocument AS purchasingdocument
                                                                 ON purchasingdocument~purchasingdocument = purchasingdocumentitem~purchasingdocument
                            LEFT OUTER JOIN t001w AS plant
                                                  ON purchasingdocumentitem~plant = plant~werks
                            LEFT OUTER JOIN i_addrorgnamepostaladdress WITH PRIVILEGED ACCESS AS address
                                                                                              ON address~addressid = plant~adrnr
                            LEFT OUTER JOIN ekkn AS account_assignment
                                                  ON account_assignment~ebeln = purchasingdocument~purchasingdocument
                                                 AND account_assignment~ebelp = @item
                            LEFT OUTER JOIN aufk AS order_master_data
                            ON order_master_data~aufnr = account_assignment~aufnr
                            WHERE purchasingdocumentitem~purchasingdocument = @purchaseorder
                              AND purchasingdocumentitem~purchasingdocumentitem = @item
                            INTO  @DATA(bill_to_address).

    IF sy-subrc = 0.
      "   Fetching Document type name
      SELECT SINGLE
                purchasingdocumenttypename
          FROM i_purchasingdocumenttypetext
          WHERE purchasingdocumentcategory = @bill_to_address-purchasingdocumentcategory
            AND purchasingdocumenttype = @bill_to_address-purchasingdocumenttype
            AND language = @/isdfps/cl_const_abc_123=>gc_e
         INTO @DATA(po_doc_description).

      "   Fetching The Bill To Mail And Website
      SELECT SINGLE
                emailaddress~emailaddress,
                website~uri_length,
                website~uri_addr
        FROM i_addressemailaddress_2 AS emailaddress
        LEFT OUTER JOIN adr12 AS website
                              ON website~addrnumber = emailaddress~addressid
        WHERE emailaddress~addressid = @bill_to_address-adrnr
        INTO @DATA(bill_to_mailid_website).

      "   Fetching The Bill To Mail
      supplier_plant = |{ bill_to_address-plant ALPHA = IN }| .
      SELECT SINGLE emailaddress~emailaddress
        FROM i_supplier  AS supplier
        LEFT OUTER JOIN i_addressemailaddress_2 WITH PRIVILEGED ACCESS AS emailaddress
                     ON emailaddress~addressid = supplier~addressid
        WHERE supplier~supplier = @supplier_plant
        INTO @DATA(bill_to_email).


      "   Fetching The Bill To GSTIN.
      SELECT SINGLE gstin                               "#EC CI_NOORDER
       FROM  j_1bbranch
       WHERE branch = @bill_to_address-j_1bbranch
       INTO @DATA(bill_to_gstin).

      "   Fetching The Bill To PAN.
      DATA(length) = strlen( bill_to_gstin ).
      IF length > 5.
        length = length - 5.
        bill_to_pan = bill_to_gstin+2(length).
      ENDIF.

      IF bill_to_address-region IS NOT INITIAL.
        "     Fetching The Bill To State Code.
        SELECT SINGLE bezei           "#EC CI_NOORDER "#EC CI_SGLSELECT
          FROM t005u
          WHERE bland = @bill_to_address-region
          AND   land1 = @india
          INTO @DATA(bill_to_statecode).
      ENDIF.

      IF bill_to_address-companycode IS NOT INITIAL.
        "     Fetching The Bill To CIN.
        SELECT SINGLE companycodeparametervalue
          FROM  i_addlcompanycodeinformation
          WHERE companycode = @bill_to_address-companycode
            AND companycodeparametertype = @cin
          INTO @DATA(bill_to_cin).
      ENDIF.

    ENDIF.

    "   Supplier Details
    "   Fetching The Supplier PAN.
*********************************************************
    IF bill_to_address-purchasingdocumenttype <> 'ZSTO'.
      SELECT SINGLE  bptaxnumber
       FROM i_businesspartnertaxnumber
       WHERE businesspartner = @bill_to_address-supplier
       INTO @DATA(supplier_pan).

      IF sy-subrc = 0.
        CLEAR : length.
        length = strlen( supplier_pan ).
        IF length > 5.
          length = length - 5.
          supplier_pan = supplier_pan+2(length).
        ENDIF.
      ENDIF.

      "   Fetching The Supplier State Code And Address.
      SELECT SINGLE statecode~bezei AS supplier_state_code, "#EC CI_BUFFJOIN
                    supplier~adrnr AS address_no,
*                    supplier~telf1 as Telephone_no,
                    streetname AS streetname,
                    address~streetprefixname1 AS streetprefixname1,
                    address~streetprefixname2 AS streetprefixname2,
                    streetsuffixname1 AS streetsuffixname1,
                    address~addresseename1 AS name,
                    address~cityname AS cityname,
                    address~postalcode AS postalcode,
                    address~region AS region
               FROM lfa1 AS supplier
                    LEFT OUTER JOIN i_addrorgnamepostaladdress WITH PRIVILEGED ACCESS AS address
                                   ON address~addressid = supplier~adrnr
*                  LEFT OUTER JOIN t005u AS statecode
*                                 ON statecode~bland = address~region
*                                 AND statecode~land1 = @india
*                                 AND statecode~spras = @sy-langu
                     LEFT OUTER JOIN t005u AS statecode
                                   ON statecode~bland  = supplier~regio
                                   AND statecode~land1 = supplier~land1
                    WHERE lifnr = @bill_to_address-supplier
                    INTO @DATA(supplier).

      " Fetching Telephone no Details
      SELECT SINGLE  FROM lfa1
        FIELDS telf1
         WHERE lifnr = @bill_to_address-supplier
                     INTO @DATA(suppliertelno).

      " Fetching GST Details
      SELECT SINGLE FROM dfkkbptaxnum
        FIELDS taxnum
        WHERE partner = @bill_to_address-supplier AND taxtype = 'IN3'
        INTO @DATA(suppliergst).

      "   Fetching The Supplier Details Mail
      supplier_plant = |{ bill_to_address-plant ALPHA = IN }| .
      SELECT SINGLE emailaddress~emailaddress
        FROM i_supplier  AS supplier
        LEFT OUTER JOIN i_addressemailaddress_2 WITH PRIVILEGED ACCESS AS emailaddress
                                                                       ON emailaddress~addressid = supplier~addressid
        WHERE supplier~supplier = @bill_to_address-supplier
        INTO @DATA(supplier_details_email).

***********************************************************
    ELSEIF bill_to_address-purchasingdocumenttype  = 'ZSTO'.
      DATA : supplyingplant TYPE lfa1-lifnr.
      supplyingplant = bill_to_address-supplyingplant.
      supplyingplant = |{ supplyingplant ALPHA = IN }|.

      "   Fetching The Supplier State Code And Address.
      SELECT SINGLE statecode~bezei AS supplier_state_code, "#EC CI_BUFFJOIN
      supplier~adrnr AS address_no,
*      supplier~telf1 as Telephone_no,
      streetname AS streetname,
      address~streetprefixname1 AS streetprefixname1,
      address~streetprefixname2 AS streetprefixname2,
      streetsuffixname1 AS streetsuffixname1,
      address~addresseename1 AS name,
      address~cityname AS cityname,
      address~postalcode AS postalcode,
      address~region AS region
 FROM lfa1 AS supplier
      LEFT OUTER JOIN i_addrorgnamepostaladdress WITH PRIVILEGED ACCESS AS address
                     ON address~addressid = supplier~adrnr
*                  LEFT OUTER JOIN t005u AS statecode
*                                 ON statecode~bland = address~region
*                                 AND statecode~land1 = @india
*                                 AND statecode~spras = @sy-langu
       LEFT OUTER JOIN t005u AS statecode
                     ON statecode~bland  = supplier~regio
                     AND statecode~land1 = supplier~land1
      WHERE supplier~lifnr = @supplyingplant
      INTO @supplier.

      " Fetching Telephone no details
      SELECT SINGLE  FROM lfa1
       FIELDS telf1
        WHERE lifnr = @supplyingplant
                    INTO @suppliertelno.

      " Fetching GST Details
      SELECT SINGLE FROM dfkkbptaxnum
        FIELDS taxnum
        WHERE partner = @supplyingplant AND taxtype = 'IN3'
        INTO @suppliergst.

      SELECT SINGLE  bptaxnumber
     FROM i_businesspartnertaxnumber
     WHERE businesspartner = @supplyingplant
     INTO @supplier_pan.

      IF sy-subrc = 0.
        CLEAR : length.
        length = strlen( supplier_pan ).
        IF length > 5.
          length = length - 5.
          supplier_pan = supplier_pan+2(length).
        ENDIF.
      ENDIF.

      "   Fetching The Supplier Details Mail
      supplier_plant = |{ bill_to_address-plant ALPHA = IN }| .
      SELECT SINGLE emailaddress~emailaddress
        FROM i_supplier  AS supplier
        LEFT OUTER JOIN i_addressemailaddress_2 WITH PRIVILEGED ACCESS AS emailaddress
                                                                      ON emailaddress~addressid = supplier~addressid
        WHERE supplier~supplier = @supplyingplant
        INTO @supplier_details_email.


    ENDIF.
***************************************************************
    "   PO Details
    SELECT SINGLE
         ekko~revno AS revision_no,
*         angnr AS quote_no,      "Commented on 18/11/2025 as suggusted by Mahesh
         ekpo~anfnr AS quote_no,  "New logic 18/11/2025 as suggusted by Mahesh
         kdatb AS valid_from,
         kdate AS valid_to,
         erev~erdat AS revision_date
    FROM ekko
         INNER JOIN ekpo ON ekko~ebeln = ekpo~ebeln "New logic 18/11/2025 as suggusted by Mahesh
         LEFT OUTER JOIN erev
                      ON erev~revno = ekko~revno
                     AND erev~edokn = @purchaseorder
         WHERE ekko~ebeln = @purchaseorder
         INTO @DATA(revision_quote_no).

    SELECT SINGLE
        ekpo~requisitionername AS requester,
        esh_s_supplierquotation~quotationsubmissiondate AS quote_date
        FROM i_purchasingdocumentitem AS ekpo
                                      LEFT OUTER JOIN esh_s_supplierquotation AS esh_s_supplierquotation
                                                                              ON esh_s_supplierquotation~supplierquotation = ekpo~supplierquotation
                                      WHERE purchasingdocument = @purchaseorder
                                      AND purchasingdocumentitem = @item
                                      INTO @DATA(requster_quotedate).

    "  Fetching the Buyer Name
    IF bill_to_address-createdbyuser IS NOT INITIAL.
      CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
        EXPORTING
          user_name        = bill_to_address-createdbyuser
          read_db_directly = space
          cache_results    = /isdfps/cl_const_abc_123=>gc_x
        IMPORTING
          user_usr03       = buyer_name.
    ENDIF.

    "   Fetching Incoterms Details
    SELECT SINGLE incotermsclassificationname
                  FROM i_incotermsclassificationtext
                 WHERE language = @/isdfps/cl_const_abc_123=>gc_e
                   AND incotermsclassification = @bill_to_address-incotermsclassification
                  INTO @DATA(incoterm_text).

    "   Fetching PaymentTerms Details
    SELECT SINGLE paymenttermsconditiondesc
                  FROM i_paymenttermsconditionstext
                  WHERE paymentterms = @bill_to_address-paymentterms
                  AND   language = @/isdfps/cl_const_abc_123=>gc_e
                  INTO @DATA(paymentterms_text).

    "   Fetching ShipTo Details.
    IF bill_to_address-manualdeliveryaddressid IS NOT INITIAL.

      ship_to_address_no  = bill_to_address-manualdeliveryaddressid.

    ELSEIF bill_to_address-subcontractor IS NOT INITIAL.

      SELECT SINGLE addressid
        FROM i_supplier
        WHERE supplier = @bill_to_address-subcontractor
        INTO @ship_to_address_no.

    ELSEIF  bill_to_address-customer IS NOT INITIAL.

      SELECT SINGLE addressid
        FROM i_customer
        WHERE customer = @bill_to_address-customer
        INTO @ship_to_address_no.

    ENDIF.

    IF ship_to_address_no IS NOT INITIAL.

      "   Fetching The ship To Mail And Website
      SELECT SINGLE
               emailaddress~emailaddress,
               website~uri_length,
               website~uri_addr
        FROM i_addressemailaddress_2 AS emailaddress
                                    LEFT OUTER JOIN adr12 AS website
                                                          ON website~addrnumber = emailaddress~addressid
                                    WHERE emailaddress~addressid = @ship_to_address_no
                                    INTO @DATA(ship_to_mailid_website).

      "  Fetching The Ship To GSTIN
      SELECT SINGLE branch~gstin                       "#EC CI_BUFFJOIN
        FROM t001w AS plant
                   LEFT OUTER JOIN j_1bbranch AS branch
                                              ON plant~j_1bbranch = branch~branch
                   WHERE plant~adrnr = @ship_to_address_no
                   INTO @DATA(ship_to_gstin).

      "  Fetching The Ship To PAN.
      CLEAR : length.
      DATA(len) = strlen( ship_to_gstin ).
      IF len > 5.
        length = len - 5.
        ship_to_pan = ship_to_gstin+2(len).
      ENDIF.

      "   Fetching The Ship To State Code.
      SELECT SINGLE bezei                              "#EC CI_BUFFJOIN
        FROM i_addrorgnamepostaladdress AS address
                                        LEFT OUTER JOIN t005u AS region
                                                              ON region~bland = address~region
                                                             AND region~land1 = 'IN'
                                        WHERE address~addressid = @ship_to_address_no
                                        INTO @DATA(ship_to_statecode).
      "  Fetching Ship To Address
      SELECT SINGLE
          address~addresseename1,
          address~addresseename2,
          address~streetname,
          address~streetprefixname1,
          address~streetprefixname2,
          address~streetsuffixname1,
          address~cityname,
          address~postalcode,
          address~region
     FROM i_addrorgnamepostaladdress WITH PRIVILEGED ACCESS AS address
    WHERE address~addressid = @ship_to_address_no
     INTO @DATA(ship_to_address).

    ENDIF.

    "  Fetching Price Effective Date
    name = purchaseorder.
    yglobalutility=>readtext(
      EXPORTING
        iv_textid  = id                " Text ID
        iv_name    = name              " Name
        iv_textobj = object_ekko       " Texts: application object
      IMPORTING
        ev_text    = DATA(price_effective_date) ).


    "  Populating Dynamic Value Based On PO Document Category
    IF bill_to_address-purchasingdocumenttype = 'ZASO'.
      dynamic1 = bill_to_address-internal_order_no.
      dynamic2 = bill_to_address-internal_order_desc.
    ELSE.
      dynamic1 = revision_quote_no-valid_from+6(2) && '.' &&
                 revision_quote_no-valid_from+4(2) && '.' &&
                 revision_quote_no-valid_from+0(4).

      dynamic2 = revision_quote_no-valid_to+6(2) && '.' &&
                 revision_quote_no-valid_to+4(2) && '.' &&
                 revision_quote_no-valid_to+0(4).
    ENDIF.

    "    Populating Data to Entity.
    er_entity = VALUE #(
                         podocumenttype       = bill_to_address-purchasingdocumenttype
                         documentdiscription  = po_doc_description
                         billtoname1          = bill_to_address-addresseename1
                         billtoname2          = bill_to_address-addresseename2
                         billtostreet         = bill_to_address-streetname
                         billtostrsuppl1      = bill_to_address-streetprefixname1
                         billtostrsuppl2      = bill_to_address-streetprefixname2
                         billtostreet3        = bill_to_address-streetsuffixname1
                         billtocity1          = bill_to_address-cityname
                         billtopostcode1      = bill_to_address-postalcode

                         billtowebsite        = COND #( WHEN bill_to_address-plant+0(1) = 1 THEN TEXT-011
                                                        WHEN bill_to_address-plant+0(1) = 2 THEN TEXT-012 )

                         billtoemail          = bill_to_email
                         billtogstin          = bill_to_gstin
                         billtostatecode      = bill_to_statecode
                         billtopan            = bill_to_pan
                         billtocin            = bill_to_cin

                         shiptoemail          = bill_to_email
                         shiptowebsite        = COND #( WHEN bill_to_address-plant+0(1) = 1 THEN TEXT-011
                                                        WHEN bill_to_address-plant+0(1) = 2 THEN TEXT-012 )

                         sdname               = supplier-name
                         sdpan                = supplier_pan
                         sdstatecode          = supplier-supplier_state_code
                         sdemail              = supplier_details_email
                         vendorname           = COND #( WHEN bill_to_address-purchasingdocumenttype = 'ZSTO'
                                                             THEN bill_to_address-supplyingplant
                                                             ELSE bill_to_address-supplier )
                         sdtelno              = suppliertelno
                         sdgst                = suppliergst
                         sdaddress            = condense( |{ COND #( WHEN supplier-streetname        IS NOT INITIAL THEN |{ supplier-streetname } | )
                                                           }{ COND #( WHEN supplier-streetprefixname1 IS NOT INITIAL THEN |{ supplier-streetprefixname1 } | )
                                                           }{ COND #( WHEN supplier-streetprefixname2 IS NOT INITIAL THEN |{ supplier-streetprefixname2 } | )
                                                           }{ COND #( WHEN supplier-streetsuffixname1 IS NOT INITIAL THEN |{ supplier-streetsuffixname1 }, | )
                                                           }{ COND #( WHEN supplier-cityname          IS NOT INITIAL THEN |{ supplier-cityname } | )
                                                           }{ COND #( WHEN supplier-postalcode        IS NOT INITIAL THEN |{ supplier-postalcode }, | )
                                                           }{ COND #( WHEN supplier-supplier_state_code       IS NOT INITIAL THEN |{ supplier-supplier_state_code } | ) } | )
                          podpriceeffectivedate = price_effective_date
                          podrevisionno       = revision_quote_no-revision_no
                          podquoteno          = revision_quote_no-quote_no
                          podrequster         = requster_quotedate-requester
                          podrevisiondate     = revision_quote_no-revision_date
                          podquotedate        = requster_quotedate-quote_date
*                          podvalidfrom        = revision_quote_no-valid_from
                          podvalidfrom        = dynamic1
*                          podvalidto          = revision_quote_no-valid_to
                          podvalidto          = dynamic2

                          podpurchasegroup    = condense( |{ COND #( WHEN bill_to_address-purchasinggroup IS NOT INITIAL THEN |{ bill_to_address-purchasinggroup  } | )
                                                          }{ COND #( WHEN bill_to_address-purchasinggroup IS NOT INITIAL AND ( buyer_name-name1 IS NOT INITIAL ) THEN | / | )
                                                          }{ COND #( WHEN buyer_name-name1 IS NOT INITIAL THEN |{ buyer_name-name1 } | ) } | )
*                                                          }{ COND #( WHEN buyer_name-name2 IS NOT INITIAL THEN |{ buyer_name-name2 } | ) } | )


                          incoterms           = condense( |{ COND #( WHEN bill_to_address-incotermsclassification IS NOT INITIAL THEN |{ bill_to_address-incotermsclassification } | )
                                                          }{ COND #( WHEN incoterm_text    IS NOT INITIAL THEN |({ incoterm_text }) | )
                                                          }{ COND #( WHEN bill_to_address-incotermstransferlocation IS NOT INITIAL THEN |{ bill_to_address-incotermstransferlocation } | ) } | )

                          paymentterms        = condense( |{ COND #( WHEN bill_to_address-paymentterms IS NOT INITIAL THEN |{ bill_to_address-paymentterms }| )
                                                          }{ COND #( WHEN paymentterms_text IS NOT INITIAL THEN | ({ paymentterms_text }) | ) } | )
                       ).

    "   Ship to Details
    IF ship_to_address_no IS NOT INITIAL.
      er_entity-shiptogstin     = ship_to_gstin.
      er_entity-shiptostatecode = ship_to_statecode.
      er_entity-shiptopan       = ship_to_pan.
      er_entity-shiptocin       = bill_to_cin.

      er_entity-shiptoname1     = ship_to_address-addresseename1.
      er_entity-shiptoname2     = ship_to_address-addresseename2.
      er_entity-shiptostreet    = ship_to_address-streetname.
      er_entity-shiptostrsuppl1 = ship_to_address-streetprefixname1.
      er_entity-shiptostrsuppl2 = ship_to_address-streetprefixname2.
      er_entity-shiptostreet3   = ship_to_address-streetsuffixname1.
      er_entity-shiptopostcode1 = ship_to_address-postalcode.
      er_entity-shiptocity1     = ship_to_address-cityname.

    ELSE.
      er_entity-shiptogstin     = bill_to_gstin.
      er_entity-shiptostatecode = bill_to_statecode.
      er_entity-shiptopan       = bill_to_pan.
      er_entity-shiptocin       = bill_to_cin.

      er_entity-shiptoname1     = bill_to_address-addresseename1.
      er_entity-shiptoname2     = bill_to_address-addresseename2.
      er_entity-shiptostreet    = bill_to_address-streetname.
      er_entity-shiptostrsuppl1 = bill_to_address-streetprefixname1.
      er_entity-shiptostrsuppl2 = bill_to_address-streetprefixname2.
      er_entity-shiptostreet3   = bill_to_address-streetsuffixname1.
      er_entity-shiptocity1     = bill_to_address-cityname.
      er_entity-shiptopostcode1 = bill_to_address-postalcode.
    ENDIF.

  ENDMETHOD.


  METHOD items_get_entityset.
*----------------------------------------------------------------------*
* Report  ZCL_ZFDP_EF_PURCHAS_03_DPC_EXT                               *
*----------------------------------------------------------------------*
* Title:          Purchase Order Print                                 *
* RICEF#:         MM 01                                                *
* Transaction:    ME23N                                                *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         RSB Transmission                                     *
*----------------------------------------------------------------------*
* Developer:      Asafwar Shivam                                       *
* Creation Date:  24-MAR-2025                                          *                       *
*----------------------------------------------------------------------*

    "   Data Declaration
    DATA : entity          TYPE zcl_zfdp_ef_purchas_03_mpc=>ts_items,
           total_amount    TYPE pc207-betrg,
           amount_in_words TYPE char256,
           serial_no       TYPE posnr.

    DATA(purchase_order) = VALUE #( it_key_tab[ name = purchase_order ]-value OPTIONAL ).

    "   Fetching the po line items
    SELECT
          purchasingdocumentitem~purchasingdocument         AS purchase_order,
          purchasingdocumentitem~purchasingdocumentitem     AS serial_no,
          purchasingdocumentitem~material                   AS item_code,
          purchasingdocumentitem~purchasingdocumentitemtext AS item_description,
          delivery_date~eindt                               AS delivery_date,
          purchasingdocumentitem~consumptiontaxctrlcode     AS hsn_sac_code,
          purchasingdocumentitem~orderquantity              AS order_quantity,
          purchasingdocumentitem~orderquantityunit          AS uom,
          purchasingdocumentitem~plant,
          purchasingdocumentitem~taxcode,
          purchasingdocument~purchasingdocumenttype         AS bsart,
          purchasingdocument~documentcurrency AS waers
          FROM i_purchasingdocument AS purchasingdocument
                                    LEFT OUTER JOIN i_purchasingdocumentitem AS purchasingdocumentitem
                                                 ON purchasingdocument~purchasingdocument = purchasingdocumentitem~purchasingdocument
                                    LEFT OUTER JOIN eket AS delivery_date
                                                 ON delivery_date~ebeln = purchasingdocumentitem~purchasingdocument
                                                AND delivery_date~ebelp = purchasingdocumentitem~purchasingdocumentitem
                                              WHERE purchasingdocument~purchasingdocument = @purchase_order
                                                AND purchasingdocumentitem~purchasingdocumentdeletioncode <> @/isdfps/cl_const_abc_123=>gc_d
                                                AND purchasingdocumentitem~purchasingdocumentdeletioncode <> @/isdfps/cl_const_abc_123=>gc_l
                                    INTO TABLE @DATA(purchase_order_data).

    IF purchase_order IS NOT INITIAL AND  purchase_order_data IS NOT INITIAL.

      SORT purchase_order_data BY serial_no.

      "   Fetching the corresponding material detais
      SELECT productdocumentnumber AS drawing_no,
             productdocumentversion AS drawing_mode_no,
             serial_no,
             product
        FROM i_product AS product
        LEFT OUTER JOIN @purchase_order_data AS po_data
        ON po_data~item_code = product~product
        WHERE po_data~purchase_order = @purchase_order
        INTO TABLE @DATA(material_data).

      "   Fetching the amount details
      SELECT                                       "#EC CI_NO_TRANSFORM
          conditionratevalue AS rate,
          conditionamount AS discount,
          conditiontype,
          purchaseorder,
          purchaseorderitem
      FROM  i_purorditmpricingelementapi01
      FOR ALL ENTRIES IN @purchase_order_data
      WHERE purchaseorder = @purchase_order_data-purchase_order
      AND  purchaseorderitem = @purchase_order_data-serial_no
      AND  conditiontype IN ( @text-002, @text-003, @text-004, @text-005, @text-006, @text-007 )
      INTO TABLE @DATA(amount_data).       "RA01 RB00 RC00     "R000 R001 R002
    ENDIF.

    "   Fetching the GST detils
    SELECT                                             "#EC CI_BUFFJOIN
        po_data~purchase_order,
        po_data~serial_no,
        po_data~taxcode,
        po_data~plant,
        plant~land1,
        plant~regio,
        tax_clasification~knumh,
        tax_clasification~kschl,
        conditions~kbetr
     FROM @purchase_order_data AS po_data
                               LEFT OUTER JOIN t001w AS plant
                                                     ON po_data~plant = plant~werks
                               LEFT OUTER JOIN a003 AS tax_clasification
                                                    ON tax_clasification~mwskz = po_data~taxcode
                                                   AND tax_clasification~aland = plant~land1
                               LEFT OUTER JOIN konp AS conditions
     ON conditions~knumh = tax_clasification~knumh
     AND conditions~kschl = tax_clasification~kschl
     WHERE po_data~purchase_order = @purchase_order
     INTO TABLE @DATA(gst_data).

    "" Start of new changes from 17/11/2025 as there is a new logic for GST fields

    SELECT
           purchasingdocumentitem~purchasingdocument       AS ebeln,
           purchasingdocument~supplier                     AS lifnr,
           purchasingdocument~purchasingdocumentorderdate  AS bedat,
           purchasingdocumentitem~purchasingdocumentitem   AS ebelp,
           purchasingdocumentitem~material                 AS matnr,
           purchasingdocumentitem~plant                    AS werks,
           marc~steuc                                      AS steuc,
           t001w~regio                                     AS t001w_regio,
           lfa1~regio                                      AS lfa1_regio
                                       FROM i_purchasingdocument                  AS purchasingdocument
                                       LEFT OUTER JOIN i_purchasingdocumentitem   AS purchasingdocumentitem
                                                    ON purchasingdocument~purchasingdocument = purchasingdocumentitem~purchasingdocument
                                       LEFT OUTER JOIN marc  ON marc~matnr = purchasingdocumentitem~material
                                       LEFT OUTER JOIN t001w ON t001w~werks =  purchasingdocumentitem~plant
                                       LEFT OUTER JOIN lfa1  ON lfa1~lifnr  =  purchasingdocument~supplier
                                       WHERE purchasingdocument~purchasingdocument = @purchase_order
                                       INTO TABLE @DATA(regio_vals).

    DATA(t001w_regio) = VALUE #( regio_vals[ 1 ]-t001w_regio OPTIONAL ).
    DATA(lfa1_regio)  = VALUE #( regio_vals[ 1 ]-lfa1_regio OPTIONAL ).

    IF t001w_regio = lfa1_regio .

      SELECT
            regio_vals~ebelp,
            regio_vals~ebeln,
            regio_vals~steuc,
            a4aq~kschl,
            konp~kbetr
                               FROM @regio_vals AS regio_vals
                               LEFT OUTER JOIN a4aq ON a4aq~steuc = regio_vals~steuc
                               LEFT OUTER JOIN konp AS konp ON konp~knumh = a4aq~knumh
                                                           AND konp~kschl = a4aq~kschl
                               WHERE regio_vals~ebeln = @purchase_order
                               AND regio_vals~bedat BETWEEN a4aq~datab AND a4aq~datbi
                               INTO TABLE @DATA(gst_rec).

    ELSEIF t001w_regio <> lfa1_regio .
      SELECT
            regio_vals~ebelp,
            regio_vals~ebeln,
            regio_vals~steuc,
            a9ad~kschl,
            konp~kbetr
                               FROM @regio_vals AS regio_vals
                               LEFT OUTER JOIN a9ad ON a9ad~steuc = regio_vals~steuc
                               LEFT OUTER JOIN konp AS konp ON konp~knumh = a9ad~knumh
                                                           AND konp~kschl = a9ad~kschl
                               WHERE regio_vals~ebeln = @purchase_order
                               AND regio_vals~bedat BETWEEN a9ad~datab AND a9ad~datbi
                               INTO TABLE @gst_rec.
    ENDIF.
    "" End of new changes from 17/11/2025 as there is a new logic for GST fields

    "" Start of new changes from 16/01/2026 as there is a new logic for GST fields
    IF sy-subrc = 0.
    ENDIF.

    SELECT
     purchasingdocument~purchasingdocument           AS ebeln,
     t001w~regio                                     AS t001w_regio,
     t001w~land1                                     AS t001w_land1,
     lfa1~regio                                      AS lfa1_regio,
     purchasingdocumentitem~taxcode                  AS mwskz
     FROM i_purchasingdocument                  AS purchasingdocument
     LEFT OUTER JOIN i_purchasingdocumentitem   AS purchasingdocumentitem
     ON purchasingdocument~purchasingdocument = purchasingdocumentitem~purchasingdocument
     LEFT OUTER JOIN t001w ON t001w~werks =  purchasingdocumentitem~plant
     LEFT OUTER JOIN lfa1  ON lfa1~lifnr  =  purchasingdocument~supplier
     WHERE purchasingdocument~purchasingdocument = @purchase_order
     INTO TABLE @DATA(gst_new_vals).

    IF gst_new_vals IS NOT INITIAL.
      SELECT
            gst_new_vals~ebeln,
            gst_new_vals~t001w_land1,
            gst_new_vals~mwskz,
            a003~knumh,
            a003~kschl,
            konp~kbetr
            FROM @gst_new_vals AS gst_new_vals
            LEFT OUTER JOIN a003 AS a003 ON a003~mwskz = gst_new_vals~mwskz
                                        AND a003~aland = gst_new_vals~t001w_land1
            LEFT OUTER JOIN konp AS konp ON konp~knumh = a003~knumh
                                        AND konp~kschl = a003~kschl
         WHERE gst_new_vals~ebeln = @purchase_order
     INTO TABLE @DATA(gst_new_a003).
    ENDIF.

    "" End of new changes from 16/01/2026 as there is a new logic for GST fields


    "   Populating the data
    LOOP AT purchase_order_data ASSIGNING FIELD-SYMBOL(<fs_po_data>).

      DATA(material_details) = VALUE #( material_data[ product = <fs_po_data>-item_code ] OPTIONAL ).
      DATA(gst_details) = VALUE #( gst_data[ taxcode = <fs_po_data>-taxcode serial_no = <fs_po_data>-serial_no ] OPTIONAL ).

      serial_no = serial_no + 10.
      entity-purchaseorderitem = serial_no.
*      entity-purchaseorderitem = <fs_po_data>-serial_no.
      entity-purchaseorder = <fs_po_data>-purchase_order.

      entity-material = |{ <fs_po_data>-item_code ALPHA = OUT }|.

      entity-materialdescription = <fs_po_data>-item_description.

      IF entity-materialdescription IS NOT INITIAL.

        entity-materialdescriptiontext = TEXT-013.                                                                     "'Material Description : '.
        entity-deliverydatetext = TEXT-014.                                                                            "'; Delivery Date : '.
        entity-itemstexttext = TEXT-016.                                                                               " '; Items Text : '.
      ELSE.
        CLEAR : entity-itemstexttext.
        entity-deliverydatetext = TEXT-015.                                                                            "'Delivery Date : '.
      ENDIF.

      IF <fs_po_data>-delivery_date IS NOT INITIAL.
        entity-delivery_date       = <fs_po_data>-delivery_date+6(2) && '.' &&
                                     <fs_po_data>-delivery_date+4(2) && '.' &&
                                     <fs_po_data>-delivery_date+0(4).
        entity-itemstexttext = TEXT-016.                                                                                "'; Items Text : '.
      ELSE.
        CLEAR entity-deliverydatetext.
        entity-itemstexttext = TEXT-017.                                                                               "'Items Text : '.
        IF entity-materialdescription IS NOT INITIAL.
          entity-itemstexttext = TEXT-016.                                                                              "'; Items Text : '.
        ENDIF.
      ENDIF.

      entity-itemstext = me->read_text(  purchase_order      =  <fs_po_data>-purchase_order                " Purchasing Document Number
                                         purchase_order_item =  <fs_po_data>-serial_no                     " Item Number of Purchasing Document
                                         delivery_date       =  <fs_po_data>-delivery_date  ).             " Field of type DATS
      IF entity-itemstext IS INITIAL.
        CLEAR entity-itemstexttext.
      ENDIF.

      entity-drawingno = material_details-drawing_no.
      entity-drawingmodeno = material_details-drawing_mode_no.

      entity-hsn_sac = <fs_po_data>-hsn_sac_code.


      "" Start of changes on 18/11/2025 as we should fetch the unconverted value near UOM field
      DATA : uomconversion TYPE meins.

*      IF <fs_po_data>-uom IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = <fs_po_data>-uom
          language       = sy-langu
        IMPORTING
*         LONG_TEXT      =
          output         = uomconversion
*         SHORT_TEXT     =
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
*      ENDIF.

      entity-uom = uomconversion.
      entity-unitofmeasurement = uomconversion.

      "" End of changes on 18/11/2025

      entity-quantity = <fs_po_data>-order_quantity.

      "   Rate based on condition
      entity-rate =  COND #( WHEN  VALUE #( amount_data[ purchaseorder = <fs_po_data>-purchase_order
                                               purchaseorderitem = <fs_po_data>-serial_no
                                               conditiontype = TEXT-002 ]-rate OPTIONAL ) IS NOT INITIAL
                             THEN amount_data[ purchaseorder = <fs_po_data>-purchase_order
                                               purchaseorderitem = <fs_po_data>-serial_no
                                               conditiontype = TEXT-002 ]-rate
                             WHEN  VALUE #( amount_data[ purchaseorder = <fs_po_data>-purchase_order
                                               purchaseorderitem = <fs_po_data>-serial_no
                                               conditiontype = TEXT-003  ]-rate OPTIONAL ) IS NOT INITIAL
                             THEN amount_data[ purchaseorder = <fs_po_data>-purchase_order
                                               purchaseorderitem = <fs_po_data>-serial_no
                                               conditiontype = TEXT-003 ]-rate
                             WHEN  VALUE #( amount_data[ purchaseorder = <fs_po_data>-purchase_order
                                               purchaseorderitem = <fs_po_data>-serial_no
                                               conditiontype = TEXT-004  ]-rate OPTIONAL ) IS NOT INITIAL
                             THEN amount_data[ purchaseorder = <fs_po_data>-purchase_order
                                               purchaseorderitem = <fs_po_data>-serial_no
                                               conditiontype = TEXT-004 ]-rate ).

      "   Discount based on the condition
      DATA(discount_percentage) = COND #( WHEN VALUE #( amount_data[ purchaseorder = <fs_po_data>-purchase_order
                                                  purchaseorderitem = <fs_po_data>-serial_no
                                                  conditiontype = TEXT-005 ]-rate OPTIONAL ) IS NOT INITIAL
                                THEN amount_data[ purchaseorder = <fs_po_data>-purchase_order
                                                  purchaseorderitem = <fs_po_data>-serial_no
                                                  conditiontype = TEXT-005 ]-rate
                                WHEN VALUE #( amount_data[ purchaseorder = <fs_po_data>-purchase_order
                                                  purchaseorderitem = <fs_po_data>-serial_no
                                                  conditiontype = TEXT-006 ]-rate OPTIONAL ) IS NOT INITIAL
                                THEN amount_data[ purchaseorder = <fs_po_data>-purchase_order
                                                  purchaseorderitem = <fs_po_data>-serial_no
                                                  conditiontype = TEXT-006 ]-rate
                                WHEN VALUE #( amount_data[ purchaseorder = <fs_po_data>-purchase_order
                                                  purchaseorderitem = <fs_po_data>-serial_no
                                                  conditiontype = TEXT-007 ]-rate OPTIONAL ) IS NOT INITIAL
                                THEN amount_data[ purchaseorder = <fs_po_data>-purchase_order
                                                  purchaseorderitem = <fs_po_data>-serial_no
                                                  conditiontype = TEXT-007 ]-rate ).

      "  If discount value is negative then converting it into positive value
      IF discount_percentage < 0.
        discount_percentage = ( -1 ) * discount_percentage.
      ENDIF.

      DATA(condition_rb00) = VALUE #( amount_data[ purchaseorder     = <fs_po_data>-purchase_order
                                                   purchaseorderitem = <fs_po_data>-serial_no
                                                   conditiontype     = TEXT-006 ]-conditiontype OPTIONAL ) .

      IF condition_rb00 IS NOT INITIAL.
        "  Calculating discount amount
        entity-discount = discount_percentage.
      ELSE.
        "  Calculating discount percentage
        entity-discount = ( discount_percentage * entity-rate ) / 100 .
      ENDIF.

      entity-netprice = entity-rate - entity-discount.

      entity-total_value = entity-netprice * entity-quantity.

      total_amount = total_amount + entity-total_value.


*      entity-igst = COND #( WHEN VALUE #( gst_data[ taxcode = <fs_po_data>-taxcode
*                                           serial_no = <fs_po_data>-serial_no
*                                           kschl = TEXT-008 ]-kbetr OPTIONAL ) IS NOT INITIAL
*                            THEN gst_data[ taxcode = <fs_po_data>-taxcode
*                                           serial_no = <fs_po_data>-serial_no
*                                           kschl = TEXT-008 ]-kbetr / 10 ).

      DATA(lv_bsart) = <fs_po_data>-bsart.

      IF lv_bsart = 'ZIMO'.

*        entity-igst = COND #( WHEN a4aq_rec[ ebeln = <fs_po_data>-purchase_order
*                                             ebelp = <fs_po_data>-serial_no
*                                             kschl = TEXT-008 ]-kbetr IS NOT INITIAL
*
*                              THEN a4aq_rec[ ebeln = <fs_po_data>-purchase_order
*                                             ebelp = <fs_po_data>-serial_no
*                                             kschl = TEXT-008 ]-kbetr / 10
*
*                              ELSE a9ad_rec[ ebeln = <fs_po_data>-purchase_order
*                                             ebelp = <fs_po_data>-serial_no
*                                             kschl = TEXT-008 ]-kbetr / 10  ). "JIIG

        DATA(igst) = VALUE #( gst_new_a003[ ebeln = <fs_po_data>-purchase_order ]-kbetr OPTIONAL ).
        entity-igst = igst / 10.
      ELSE.
        entity-igst = REDUCE vfprc_element_amount( INIT lv_vfprc_element_amount TYPE vfprc_element_amount
                                                     FOR  ls_gst_rec IN gst_rec
                                                     WHERE ( ebeln = <fs_po_data>-purchase_order
                                                     AND     ebelp = <fs_po_data>-serial_no
                                                     AND     kschl = 'JIIG' )
                                                     NEXT lv_vfprc_element_amount = ls_gst_rec-kbetr / 10 ).

      ENDIF.

      entity-cgst = COND #( WHEN VALUE #( gst_data[ taxcode = <fs_po_data>-taxcode
                                        serial_no = <fs_po_data>-serial_no
                                        kschl = TEXT-009 ]-kbetr OPTIONAL ) IS NOT INITIAL
                         THEN gst_data[ taxcode = <fs_po_data>-taxcode
                                        serial_no = <fs_po_data>-serial_no
                                        kschl = TEXT-009 ]-kbetr / 10 ).
      IF entity-cgst IS INITIAL.
        entity-cgst = REDUCE vfprc_element_amount( INIT lv_vfprc_element_amount TYPE vfprc_element_amount
                                             FOR  ls_gst_rec IN gst_rec
                                             WHERE ( ebeln = <fs_po_data>-purchase_order
                                             AND     ebelp = <fs_po_data>-serial_no
                                             AND     kschl = 'JICG' )
                                             NEXT lv_vfprc_element_amount = ls_gst_rec-kbetr / 10 ).
      ENDIF.

      entity-sgst = COND #( WHEN VALUE #( gst_data[ taxcode = <fs_po_data>-taxcode
                                                serial_no = <fs_po_data>-serial_no
                                                kschl = TEXT-010 ]-kbetr OPTIONAL ) IS NOT INITIAL
                                THEN gst_data[ taxcode = <fs_po_data>-taxcode
                                               serial_no = <fs_po_data>-serial_no
                                                kschl = TEXT-010 ]-kbetr / 10 ).

      IF entity-sgst IS INITIAL.
        entity-sgst = REDUCE vfprc_element_amount( INIT lv_vfprc_element_amount TYPE vfprc_element_amount
                                               FOR  ls_gst_rec IN gst_rec
                                               WHERE ( ebeln = <fs_po_data>-purchase_order
                                               AND     ebelp = <fs_po_data>-serial_no
                                               AND     kschl = 'JISG' )
                                               NEXT lv_vfprc_element_amount = ls_gst_rec-kbetr / 10 ).
      ENDIF.





**      entity-igst = COND #( WHEN VALUE #( gst_data[ taxcode = <fs_po_data>-taxcode
**                                           serial_no = <fs_po_data>-serial_no
**                                           kschl = TEXT-008 ]-kbetr OPTIONAL ) IS NOT INITIAL
**                            THEN gst_data[ taxcode = <fs_po_data>-taxcode
**                                           serial_no = <fs_po_data>-serial_no
**                                           kschl = TEXT-008 ]-kbetr / 10 ).

**      entity-cgst = COND #( WHEN VALUE #( gst_data[ taxcode = <fs_po_data>-taxcode
**                                           serial_no = <fs_po_data>-serial_no
**                                           kschl = TEXT-009 ]-kbetr OPTIONAL ) IS NOT INITIAL
**                            THEN gst_data[ taxcode = <fs_po_data>-taxcode
**                                           serial_no = <fs_po_data>-serial_no
**                                           kschl = TEXT-009 ]-kbetr / 10 ).
**
**      entity-sgst = COND #( WHEN VALUE #( gst_data[ taxcode = <fs_po_data>-taxcode
**                                           serial_no = <fs_po_data>-serial_no
**                                           kschl = TEXT-010 ]-kbetr OPTIONAL ) IS NOT INITIAL
**                           THEN gst_data[ taxcode = <fs_po_data>-taxcode
**                                          serial_no = <fs_po_data>-serial_no
**                                           kschl = TEXT-010 ]-kbetr / 10 ).

      "   Appending value to the final entityset
      APPEND entity TO et_entityset.
      CLEAR : entity, condition_rb00.

    ENDLOOP.

    DATA(lv_curr) = VALUE #( purchase_order_data[ 1 ]-waers OPTIONAL ).

    "   Amount to words.
    CALL FUNCTION 'J_1IG_AMT_IN_WORDS'
      EXPORTING
        amt_in_num         = total_amount                 " HR Payroll: Amount
      IMPORTING
        amt_in_words       = amount_in_words
      EXCEPTIONS
        data_type_mismatch = 1                            " The imported amount too long
        OTHERS             = 2.

    "   Converting Amount Words to Camel Naming Convection
    IF sy-subrc = 0.
      IF lv_curr = 'INR'.

        CONDENSE amount_in_words.
        IF amount_in_words IS NOT INITIAL.
          TRANSLATE amount_in_words+1(199) TO LOWER CASE.
          amount_in_words = |{ amount_in_words } { only }|.
          entity-amountinwords = amount_in_words.
          entity-amountinwords = cl_hrpayus_format_string=>conv_first_chars_to_upper_case( entity-amountinwords ).
        ENDIF.
      ELSE.
        CONDENSE amount_in_words.
        IF amount_in_words IS NOT INITIAL.
          TRANSLATE amount_in_words+1(199) TO LOWER CASE.
          amount_in_words = |{ amount_in_words } { only }|.
          entity-amountinwords = amount_in_words.
          entity-amountinwords = cl_hrpayus_format_string=>conv_first_chars_to_upper_case( entity-amountinwords ).
          REPLACE ALL OCCURRENCES OF 'Rupees' IN entity-amountinwords WITH lv_curr.
        ENDIF.
      ENDIF.
    ENDIF.

    "  Populating Amount In Words
    MODIFY et_entityset FROM VALUE #( amountinwords = entity-amountinwords )
                        TRANSPORTING amountinwords
                        WHERE purchaseorderitem IS NOT INITIAL.

  ENDMETHOD.


  method READ_TEXT.
*----------------------------------------------------------------------*
* Report  ZCL_ZFDP_EF_PURCHAS_03_DPC_EXT                               *
*----------------------------------------------------------------------*
* Title:          Purchase Order Print                                 *
* RICEF#:         MM 01                                                *
* Transaction:    ME23N                                                *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         RSB Transmission                                     *
*----------------------------------------------------------------------*
* Developer:      Asafwar Shivam                                       *
* Creation Date:  24-MAR-2025                                          *
*----------------------------------------------------------------------*


   "   Data declaration
    DATA : text_lines TYPE TABLE OF tline,
           name       TYPE thead-tdname,
           id         TYPE thead-tdid,
           index      TYPE sy-tabix,
           lr_text_id TYPE RANGE OF thead-tdid,
           text_id1   TYPE string,
           text_id2   TYPE string,
           text_id3   TYPE string,
           text_id4   TYPE string,
           text_id5   TYPE string,
           text_id6   TYPE string,
           text_id10  TYPE string,
           text_id11  TYPE string.

    "   Fetching ID's from TVARVC table
    SELECT name, low, high
      FROM tvarvc
      WHERE name = @text-001
      INTO TABLE @DATA(lt_text_id).                     "#EC CI_NOORDER

    IF sy-subrc = 0.
      lr_text_id = VALUE #( FOR ls_text_id IN lt_text_id "#EC CI_STDSEQ
                                  WHERE ( name   = TEXT-001 ) "#EC CI_STDSEQ
                                        ( sign   = |{ /isdfps/cl_const_abc_123=>gc_i }|
                                          option = |{ /isdfps/cl_const_abc_123=>gc_e && /isdfps/cl_const_abc_123=>gc_q }|
                                          low    = ls_text_id-low ) ).
    ENDIF.


    name = purchase_order && purchase_order_item.

    "     Looping over text id
    LOOP AT lr_text_id ASSIGNING FIELD-SYMBOL(<fs_text_line>).

      index = sy-tabix.

      "       Calling the get_item_text method
      text_lines =  me->GET_ITEM_TEXT( iv_id = <fs_text_line>-low iv_name = name ).

      IF index = 1.
        text_id1 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).

      ELSEIF index = 2.
        text_id2 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).
      ELSEIF index = 3.
        text_id3 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).
      ELSEIF index = 4.
        text_id4 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).
      ELSEIF index = 5.
        text_id5 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).
      ELSEIF index = 6.
        text_id6 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).
      ELSEIF index = 7.
        text_id10 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).
      ELSEIF index = 8.
        text_id11 = REDUCE #( INIT text_line TYPE string
                                  FOR <lines> IN text_lines
                                  NEXT text_line = COND #( WHEN text_line IS NOT INITIAL
                                                           THEN text_line && ' ' && <lines>-tdline
                                                           ELSE <lines>-tdline ) ).
      ENDIF.

      CLEAR : index, text_lines.
    ENDLOOP.

    CONDENSE : text_id1, text_id2, text_id3,
               text_id4, text_id5, text_id6,
               text_id10, text_id11.

*      "     Formating Date
    IF delivery_date IS NOT INITIAL.
      DATA(date) = |{ delivery_date+6(2) }-{ delivery_date+4(2) }-{ delivery_date+0(4) }|.
    ENDIF.

    "     Concatinating the text lines
    DATA(text) = condense( |{ COND #( WHEN text_id1   IS NOT INITIAL THEN | { text_id1 }| )
                                     }{ COND #( WHEN text_id1   IS NOT INITIAL AND      text_id2 IS NOT INITIAL THEN |,| )
                                     }{ COND #( WHEN text_id2   IS NOT INITIAL THEN | { text_id2 }| )
                                     }{ COND #( WHEN text_id2   IS NOT INITIAL AND      text_id3 IS NOT INITIAL THEN |,| )
                                     }{ COND #( WHEN text_id3   IS NOT INITIAL THEN | { text_id3 }| )
                                     }{ COND #( WHEN text_id3   IS NOT INITIAL AND      text_id4 IS NOT INITIAL THEN |,| )
                                     }{ COND #( WHEN text_id4   IS NOT INITIAL THEN | { text_id4 }| )
                                     }{ COND #( WHEN text_id4   IS NOT INITIAL AND      text_id5 IS NOT INITIAL THEN |,| )
                                     }{ COND #( WHEN text_id5   IS NOT INITIAL THEN | { text_id5 }| )
                                     }{ COND #( WHEN text_id5   IS NOT INITIAL AND      text_id6 IS NOT INITIAL THEN |,| )
                                     }{ COND #( WHEN text_id6   IS NOT INITIAL THEN | { text_id6 }| )
                                     }{ COND #( WHEN text_id6   IS NOT INITIAL AND      text_id10 IS NOT INITIAL THEN |,| )
                                     }{ COND #( WHEN text_id10  IS NOT INITIAL THEN | { text_id10 }| )
                                     }{ COND #( WHEN text_id10  IS NOT INITIAL AND      text_id11 IS NOT INITIAL THEN |,| )
                                     }{ COND #( WHEN text_id11  IS NOT INITIAL THEN | { text_id11 }| )  } | ).

*    item_text =  |{ COND #( WHEN date IS NOT INITIAL THEN |{ delivery_date_text } { date }| )
*                      }{ COND #( WHEN date IS NOT INITIAL AND ( text IS NOT INITIAL ) THEN |;   | )
*                      }{ COND #( WHEN text IS NOT INITIAL THEN |{ text_item } { text }| ) } |.
    item_text = text.
    CLEAR : text.

  endmethod.

ENDCLASS.
*************************************************************************************

class ZCL_ZFDP_EF_PURCHAS_03_MPC definition
  public
  inheriting from /IWBEP/CL_MGW_PUSH_ABS_MODEL
  create public .

public section.

  types:
     TS_HIERITEMPRICINGCONDITIONNOD type MMPUR_S_FDP_ITEM_PRICING_COND .
  types:
TT_HIERITEMPRICINGCONDITIONNOD type standard table of TS_HIERITEMPRICINGCONDITIONNOD .
  types:
   begin of ts_text_element,
      artifact_name  type c length 40,       " technical name
      artifact_type  type c length 4,
      parent_artifact_name type c length 40, " technical name
      parent_artifact_type type c length 4,
      text_symbol    type textpoolky,
   end of ts_text_element .
  types:
         tt_text_elements type standard table of ts_text_element with key text_symbol .
  types:
  begin of TS_INVOICINGPARTYNODE,
     LIFNR type C length 10,
     NAME1 type C length 35,
     ADRNR type C length 10,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
     STCD3 type C length 80,
     STCEG type C length 20,
  end of TS_INVOICINGPARTYNODE .
  types:
TT_INVOICINGPARTYNODE type standard table of TS_INVOICINGPARTYNODE .
  types:
     TS_ITEMPRICINGCONDITIONNODE type MMPUR_S_FDP_ITEM_PRICING_COND .
  types:
TT_ITEMPRICINGCONDITIONNODE type standard table of TS_ITEMPRICINGCONDITIONNODE .
  types:
     TS_ITEMTAXCONDITIONSNODE type MMPUR_S_FDP_ITEM_PRICING_COND .
  types:
TT_ITEMTAXCONDITIONSNODE type standard table of TS_ITEMTAXCONDITIONSNODE .
  types:
  begin of TS_ORDERINGADDRESS,
     LIFN2 type C length 10,
     NAME1 type C length 40,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
     TEL_NUMBER type C length 30,
     TELFX type C length 30,
     SMTP_ADDR type C length 241,
  end of TS_ORDERINGADDRESS .
  types:
TT_ORDERINGADDRESS type standard table of TS_ORDERINGADDRESS .
  types:
     TS_POCONFIGURATIONHIERITEMNODE type TDS_ME_PO_ITEM_CONFIG .
  types:
TT_POCONFIGURATIONHIERITEMNODE type standard table of TS_POCONFIGURATIONHIERITEMNODE .
  types:
     TS_POCONFIGURATIONITEMNODE type TDS_ME_PO_ITEM_CONFIG .
  types:
TT_POCONFIGURATIONITEMNODE type standard table of TS_POCONFIGURATIONITEMNODE .
  types:
     TS_POHIERSUBCONTRACTINGCOMPONE type TDS_ME_PO_ITEM_COMPONENT_BATCH .
  types:
TT_POHIERSUBCONTRACTINGCOMPONE type standard table of TS_POHIERSUBCONTRACTINGCOMPONE .
  types:
     TS_POHIERSUBCONTRACTINGCOMPON type TDS_ME_PO_ITEM_COMPONENTS .
  types:
TT_POHIERSUBCONTRACTINGCOMPON type standard table of TS_POHIERSUBCONTRACTINGCOMPON .
  types:
  begin of TS_POHIERSUBCONTRACTINGCOMPO,
     EBELN type C length 10,
     EBELP type C length 5,
     ETENR type C length 4,
     RESERVATIONITEM type C length 4,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_POHIERSUBCONTRACTINGCOMPO .
  types:
TT_POHIERSUBCONTRACTINGCOMPO type standard table of TS_POHIERSUBCONTRACTINGCOMPO .
  types:
     TS_POSUBCONTRACTINGCOMPONENTSB type TDS_ME_PO_ITEM_COMPONENT_BATCH .
  types:
TT_POSUBCONTRACTINGCOMPONENTSB type standard table of TS_POSUBCONTRACTINGCOMPONENTSB .
  types:
     TS_POSUBCONTRACTINGCOMPONENTSN type TDS_ME_PO_ITEM_COMPONENTS .
  types:
TT_POSUBCONTRACTINGCOMPONENTSN type standard table of TS_POSUBCONTRACTINGCOMPONENTSN .
  types:
  begin of TS_POSUBCONTRACTINGCOMPONENTST,
     EBELN type C length 10,
     EBELP type C length 5,
     ETENR type C length 4,
     RESERVATIONITEM type C length 4,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_POSUBCONTRACTINGCOMPONENTST .
  types:
TT_POSUBCONTRACTINGCOMPONENTST type standard table of TS_POSUBCONTRACTINGCOMPONENTST .
  types:
  begin of TS_PURCHASEORDERCHANGESNODE,
     EBELN type C length 10,
     CHTXT type C length 30,
  end of TS_PURCHASEORDERCHANGESNODE .
  types:
TT_PURCHASEORDERCHANGESNODE type standard table of TS_PURCHASEORDERCHANGESNODE .
  types:
  begin of TS_PURCHASEORDERHEADERSTTEXTS,
     EBELN type C length 10,
     DRUVO type C length 1,
     ESART type C length 4,
     TDOBJECT type C length 10,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     DRFLG type C length 2,
     TDOBNAME type C length 70,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERHEADERSTTEXTS .
  types:
TT_PURCHASEORDERHEADERSTTEXTS type standard table of TS_PURCHASEORDERHEADERSTTEXTS .
  types:
  begin of TS_PURCHASEORDERHEADERTEXTS,
     EBELN type C length 10,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERHEADERTEXTS .
  types:
TT_PURCHASEORDERHEADERTEXTS type standard table of TS_PURCHASEORDERHEADERTEXTS .
  types:
     TS_PURCHASEORDERHIERITEMBATCHN type TDS_ME_PO_ITEM_BATCH .
  types:
TT_PURCHASEORDERHIERITEMBATCHN type standard table of TS_PURCHASEORDERHIERITEMBATCHN .
  types:
  begin of TS_PURCHASEORDERHIERITEMCHANGE,
     EBELN type C length 10,
     EBELP type C length 5,
     CHTXT type C length 30,
  end of TS_PURCHASEORDERHIERITEMCHANGE .
  types:
TT_PURCHASEORDERHIERITEMCHANGE type standard table of TS_PURCHASEORDERHIERITEMCHANGE .
  types:
     TS_PURCHASEORDERHIERITEMNODE type TDS_ME_PO_ITEM .
  types:
TT_PURCHASEORDERHIERITEMNODE type standard table of TS_PURCHASEORDERHIERITEMNODE .
  types:
  begin of TS_PURCHASEORDERHIERITEMSTTEXT,
     EBELN type C length 10,
     EBELP type C length 5,
     DRUVO type C length 1,
     ESART type C length 4,
     PSTYP type C length 1,
     TDOBJECT type C length 10,
     LANGUAGE type C length 2,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     DRFLG type C length 2,
     DRPRI type C length 1,
     TDOBNAME type C length 70,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERHIERITEMSTTEXT .
  types:
TT_PURCHASEORDERHIERITEMSTTEXT type standard table of TS_PURCHASEORDERHIERITEMSTTEXT .
  types:
  begin of TS_PURCHASEORDERHIERITEMTEXTS,
     EBELN type C length 10,
     EBELP type C length 5,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERHIERITEMTEXTS .
  types:
TT_PURCHASEORDERHIERITEMTEXTS type standard table of TS_PURCHASEORDERHIERITEMTEXTS .
  types:
     TS_PURCHASEORDERHIERSCHEDULELI type TDS_ME_PO_SCHEDULELINE .
  types:
TT_PURCHASEORDERHIERSCHEDULELI type standard table of TS_PURCHASEORDERHIERSCHEDULELI .
  types:
     TS_PURCHASEORDERITEMBATCHNODE type TDS_ME_PO_ITEM_BATCH .
  types:
TT_PURCHASEORDERITEMBATCHNODE type standard table of TS_PURCHASEORDERITEMBATCHNODE .
  types:
  begin of TS_PURCHASEORDERITEMCHANGESNOD,
     EBELN type C length 10,
     EBELP type C length 5,
     CHTXT type C length 30,
  end of TS_PURCHASEORDERITEMCHANGESNOD .
  types:
TT_PURCHASEORDERITEMCHANGESNOD type standard table of TS_PURCHASEORDERITEMCHANGESNOD .
  types:
     TS_PURCHASEORDERITEMNODE type TDS_ME_PO_ITEM .
  types:
TT_PURCHASEORDERITEMNODE type standard table of TS_PURCHASEORDERITEMNODE .
  types:
  begin of TS_PURCHASEORDERITEMSTTEXTS,
     EBELN type C length 10,
     EBELP type C length 5,
     DRUVO type C length 1,
     ESART type C length 4,
     PSTYP type C length 1,
     TDOBJECT type C length 10,
     LANGUAGE type C length 2,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     DRFLG type C length 2,
     DRPRI type C length 1,
     TDOBNAME type C length 70,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERITEMSTTEXTS .
  types:
TT_PURCHASEORDERITEMSTTEXTS type standard table of TS_PURCHASEORDERITEMSTTEXTS .
  types:
  begin of TS_PURCHASEORDERITEMTEXTS,
     EBELN type C length 10,
     EBELP type C length 5,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERITEMTEXTS .
  types:
TT_PURCHASEORDERITEMTEXTS type standard table of TS_PURCHASEORDERITEMTEXTS .
  types:
  begin of TS_PURCHASEORDERLIMITITEMCHANG,
     EBELN type C length 10,
     EBELP type C length 5,
     CHTXT type C length 30,
  end of TS_PURCHASEORDERLIMITITEMCHANG .
  types:
TT_PURCHASEORDERLIMITITEMCHANG type standard table of TS_PURCHASEORDERLIMITITEMCHANG .
  types:
  begin of TS_PURCHASEORDERLIMITITEMNODE,
     EBELN type C length 10,
     EBELP type C length 5,
     PSTYP type C length 1,
     TXZ01 type C length 40,
     MATKL type C length 9,
     WERKS type C length 4,
     EXPECTED_VALUE type P length 8 decimals 3,
     NETPR type P length 7 decimals 3,
     NETWR type P length 9 decimals 3,
     WAERS type C length 5,
     MMPUR_SERVPROC_PERIOD_START type TIMESTAMP,
     MMPUR_SERVPROC_PERIOD_END type TIMESTAMP,
     ADRNR type C length 10,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
     SERVICEPERFORMER type C length 10,
     SERVICEPERFORMERNAME type C length 80,
     PRODUCTTYPE type C length 2,
     PEINH type P length 3 decimals 0,
     LABNR type C length 20,
     KTMNG type P length 7 decimals 3,
     WEMNG type P length 7 decimals 3,
     WAMNG type P length 7 decimals 3,
     PRSDR type C length 1,
     LOEKZ type C length 1,
     RETPO type C length 1,
     KZABS type FLAG,
     ELIKZ type C length 1,
  end of TS_PURCHASEORDERLIMITITEMNODE .
  types:
TT_PURCHASEORDERLIMITITEMNODE type standard table of TS_PURCHASEORDERLIMITITEMNODE .
  types:
  begin of TS_PURCHASEORDERLIMITITEMSTTEX,
     EBELN type C length 10,
     EBELP type C length 5,
     DRUVO type C length 1,
     ESART type C length 4,
     PSTYP type C length 1,
     TDOBJECT type C length 10,
     LANGUAGE type C length 2,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     DRFLG type C length 2,
     DRPRI type C length 1,
     TDOBNAME type C length 70,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERLIMITITEMSTTEX .
  types:
TT_PURCHASEORDERLIMITITEMSTTEX type standard table of TS_PURCHASEORDERLIMITITEMSTTEX .
  types:
  begin of TS_PURCHASEORDERLIMITITEMTEXTS,
     EBELN type C length 10,
     EBELP type C length 5,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERLIMITITEMTEXTS .
  types:
TT_PURCHASEORDERLIMITITEMTEXTS type standard table of TS_PURCHASEORDERLIMITITEMTEXTS .
  types:
     TS_PURCHASEORDERNODE type TDS_ME_PO_HEADER .
  types:
TT_PURCHASEORDERNODE type standard table of TS_PURCHASEORDERNODE .
  types:
     TS_PURCHASEORDERSCHEDULELINENO type TDS_ME_PO_SCHEDULELINE .
  types:
TT_PURCHASEORDERSCHEDULELINENO type standard table of TS_PURCHASEORDERSCHEDULELINENO .
  types:
  begin of TS_PURCHASINGGROUPNODE,
     EKGRP type C length 3,
     EKNAM type C length 18,
     TEL_NUMBER type C length 30,
     TELFX type C length 31,
     SMTP_ADDR type C length 241,
  end of TS_PURCHASINGGROUPNODE .
  types:
TT_PURCHASINGGROUPNODE type standard table of TS_PURCHASINGGROUPNODE .
  types:
  begin of TS_QUERYNODE,
     EBELN type C length 10,
     LAND1 type C length 3,
     LANGU type C length 1,
     CHANGE_FLAG type C length 1,
     LIFN2 type C length 10,
     OUTPUTTYPE type C length 30,
     OUTPUTPREVIEW type C length 1,
  end of TS_QUERYNODE .
  types:
TT_QUERYNODE type standard table of TS_QUERYNODE .
  types:
  begin of TS_SHIPTOPARTYNODE,
     ADRNR type C length 10,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
     STCD3 type C length 80,
  end of TS_SHIPTOPARTYNODE .
  types:
TT_SHIPTOPARTYNODE type standard table of TS_SHIPTOPARTYNODE .
  types:
  begin of TS_SUPPLIERNODE,
     LIFNR type C length 10,
     NAME1 type C length 35,
     ADRNR type C length 10,
     SMTP_ADDR type C length 241,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
     STCEG type C length 20,
     STCD3 type C length 80,
     TELF1 type C length 16,
     TELF2 type C length 16,
  end of TS_SUPPLIERNODE .
  types:
TT_SUPPLIERNODE type standard table of TS_SUPPLIERNODE .
  types:
     TS_TAXSUMMARYNODE type MMPUR_S_FDP_ITEM_PRICING_COND .
  types:
TT_TAXSUMMARYNODE type standard table of TS_TAXSUMMARYNODE .
  types:
  begin of TS_TOTALAMOUNTSNODE,
     PURCHASE_ORDER type C length 10,
     GROSS_AMOUNT type P length 13 decimals 2,
     TOTAL_TAX_AMOUNT type P length 13 decimals 2,
     GROSS_IN_WORDS type string,
  end of TS_TOTALAMOUNTSNODE .
  types:
TT_TOTALAMOUNTSNODE type standard table of TS_TOTALAMOUNTSNODE .
  types:
  begin of TS_HEADER,
     PURCHASEORDER type EBELN,
     DOCUMENTDISCRIPTION type BATXT,
     BILLTONAME1 type AD_NAME1,
     BILLTONAME2 type string,
     BILLTOSTREET type string,
     BILLTOSTRSUPPL1 type string,
     BILLTOSTRSUPPL2 type string,
     BILLTOSTREET3 type string,
     BILLTOCITY1 type string,
     BILLTOPOSTCODE1 type string,
     BILLTOEMAIL type AD_SMTPADR,
     BILLTOWEBSITE type AD_URI,
     BILLTOGSTIN type J_1IGSTCD3,
     BILLTOSTATECODE type string,
     BILLTOPAN type J_1IGSTCD3,
     BILLTOCIN type PAVAL,
     SHIPTOEMAIL type AD_SMTPADR,
     SHIPTOWEBSITE type AD_URI,
     SHIPTOGSTIN type J_1IGSTCD3,
     SHIPTOSTATECODE type string,
     SHIPTOPAN type J_1IGSTCD3,
     SHIPTOCIN type PAVAL,
     SHIPTONAME1 type AD_NAME1,
     SHIPTONAME2 type AD_NAME1,
     SHIPTOSTREET type string,
     SHIPTOSTRSUPPL1 type string,
     SHIPTOSTRSUPPL2 type string,
     SHIPTOSTREET3 type string,
     SHIPTOCITY1 type string,
     SHIPTOPOSTCODE1 type string,
     SDNAME type string,
     SDPAN type string,
     SDSTATECODE type string,
     SDADDRESS type string,
     SDEMAIL type string,
     SDGST type string,
     SDTELNO type string,
     VENDORNAME type string,
     PODREVISIONNO type string,
     PODQUOTENO type ANGNR,
     PODREQUSTER type AFNAM,
     PODPRICEEFFECTIVEDATE type string,
     PODREVISIONDATE type DATS,
     PODQUOTEDATE type DATS,
     PODVALIDFROM type string,
     PODVALIDTO type string,
     PODPURCHASEGROUP type string,
     INCOTERMS type string,
     PAYMENTTERMS type string,
     PODOCUMENTTYPE type string,
     INTERNALORDERNO type string,
     INTERNALORDERDESC type string,
  end of TS_HEADER .
  types:
TT_HEADER type standard table of TS_HEADER .
  types:
  begin of TS_ITEMS,
     PURCHASEORDERITEM type EBELP,
     PURCHASEORDER type EBELN,
     MATERIAL type MATNR,
     MATERIALDESCRIPTION type string,
     DELIVERY_DATE type string,
     DRAWINGNO type DOKNR,
     DRAWINGMODENO type DOKVR,
     HSN_SAC type J_1BNBMCO1,
     UOM type MEINS,
     QUANTITY type BSTMG,
     RATE type VFPRC_ELEMENT_AMOUNT,
     DISCOUNT type VFPRC_ELEMENT_VALUE,
     NETPRICE type VFPRC_ELEMENT_AMOUNT,
     TOTAL_VALUE type VFPRC_ELEMENT_AMOUNT,
     AMOUNTINWORDS type string,
     IGST type KBETR,
     SGST type KBETR,
     CGST type KBETR,
     ITEMSTEXT type string,
     DELIVERYDATETEXT type string,
     ITEMSTEXTTEXT type string,
     MATERIALDESCRIPTIONTEXT type string,
     UNITOFMEASUREMENT type string,
  end of TS_ITEMS .
  types:
TT_ITEMS type standard table of TS_ITEMS .
  types:
  begin of TS_ITEMS_TEXT,
     PURCHASEORDERITEM type EBELP,
     PURCHASEORDER type EBELN,
     TEXTLINE1 type string,
     TEXTLINE2 type TDLINE,
     TEXTLINE3 type TDLINE,
     TEXTLINE4 type TDLINE,
     TEXTLINE5 type TDLINE,
     TEXTLINE6 type TDLINE,
     TEXTLINE10 type TDLINE,
     TEXTLINE11 type TDLINE,
  end of TS_ITEMS_TEXT .
  types:
TT_ITEMS_TEXT type standard table of TS_ITEMS_TEXT .

  constants GC_TOTALAMOUNTSNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'TotalAmountsNode' ##NO_TEXT.
  constants GC_TAXSUMMARYNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'TaxSummaryNode' ##NO_TEXT.
  constants GC_SUPPLIERNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'SupplierNode' ##NO_TEXT.
  constants GC_SHIPTOPARTYNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ShipToPartyNode' ##NO_TEXT.
  constants GC_QUERYNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'QueryNode' ##NO_TEXT.
  constants GC_PURCHASINGGROUPNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchasingGroupNode' ##NO_TEXT.
  constants GC_PURCHASEORDERSCHEDULELINENO type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderScheduleLineNode' ##NO_TEXT.
  constants GC_PURCHASEORDERNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderNode' ##NO_TEXT.
  constants GC_PURCHASEORDERLIMITITEMTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderLimitItemTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERLIMITITEMSTTEX type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderLimitItemSTTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERLIMITITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderLimitItemNode' ##NO_TEXT.
  constants GC_PURCHASEORDERLIMITITEMCHANG type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderLimitItemChangesNode' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMSTTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemSTTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemNode' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMCHANGESNOD type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemChangesNode' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMBATCHNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemBatchNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERSCHEDULELI type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierScheduleLineNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMSTTEXT type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemSTTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMCHANGE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemChangesNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMBATCHN type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemBatchNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHEADERTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHeaderTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERHEADERSTTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHeaderSTTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERCHANGESNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderChangesNode' ##NO_TEXT.
  constants GC_POSUBCONTRACTINGCOMPONENTST type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POSubcontractingComponentsTexts' ##NO_TEXT.
  constants GC_POSUBCONTRACTINGCOMPONENTSN type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POSubcontractingComponentsNode' ##NO_TEXT.
  constants GC_POSUBCONTRACTINGCOMPONENTSB type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POSubcontractingComponentsBatchNode' ##NO_TEXT.
  constants GC_POHIERSUBCONTRACTINGCOMPONE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POHierSubcontractingComponentsBatchNode' ##NO_TEXT.
  constants GC_POHIERSUBCONTRACTINGCOMPON type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POHierSubcontractingComponentsNode' ##NO_TEXT.
  constants GC_POHIERSUBCONTRACTINGCOMPO type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POHierSubcontractingComponentsTexts' ##NO_TEXT.
  constants GC_POCONFIGURATIONITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POConfigurationItemNode' ##NO_TEXT.
  constants GC_POCONFIGURATIONHIERITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POConfigurationHierItemNode' ##NO_TEXT.
  constants GC_ORDERINGADDRESS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'OrderingAddress' ##NO_TEXT.
  constants GC_ITEMTAXCONDITIONSNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ItemTaxConditionsNode' ##NO_TEXT.
  constants GC_ITEMS_TEXT type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'Items_Text' ##NO_TEXT.
  constants GC_ITEMS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'Items' ##NO_TEXT.
  constants GC_ITEMPRICINGCONDITIONNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ItemPricingConditionNode' ##NO_TEXT.
  constants GC_INVOICINGPARTYNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'InvoicingPartyNode' ##NO_TEXT.
  constants GC_HIERITEMPRICINGCONDITIONNOD type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'HierItemPricingConditionNode' ##NO_TEXT.
  constants GC_HEADER type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'Header' ##NO_TEXT.

  methods GET_EXTENDED_MODEL
  final
    exporting
      !EV_EXTENDED_SERVICE type /IWBEP/MED_GRP_TECHNICAL_NAME
      !EV_EXT_SERVICE_VERSION type /IWBEP/MED_GRP_VERSION
      !EV_EXTENDED_MODEL type /IWBEP/MED_MDL_TECHNICAL_NAME
      !EV_EXT_MODEL_VERSION type /IWBEP/MED_MDL_VERSION
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods LOAD_TEXT_ELEMENTS
  final
    returning
      value(RT_TEXT_ELEMENTS) type TT_TEXT_ELEMENTS
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .

  methods DEFINE
    redefinition .
  methods GET_LAST_MODIFIED
    redefinition .
protected section.
private section.

  methods CREATE_NEW_ARTIFACTS
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
ENDCLASS.



CLASS ZCL_ZFDP_EF_PURCHAS_03_MPC IMPLEMENTATION.


  method CREATE_NEW_ARTIFACTS.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


DATA:
  lo_entity_type    TYPE REF TO /iwbep/if_mgw_odata_entity_typ,                      "#EC NEEDED
  lo_complex_type   TYPE REF TO /iwbep/if_mgw_odata_cmplx_type,                      "#EC NEEDED
  lo_property       TYPE REF TO /iwbep/if_mgw_odata_property,                        "#EC NEEDED
  lo_association    TYPE REF TO /iwbep/if_mgw_odata_assoc,                           "#EC NEEDED
  lo_assoc_set      TYPE REF TO /iwbep/if_mgw_odata_assoc_set,                       "#EC NEEDED
  lo_ref_constraint TYPE REF TO /iwbep/if_mgw_odata_ref_constr,                      "#EC NEEDED
  lo_nav_property   TYPE REF TO /iwbep/if_mgw_odata_nav_prop,                        "#EC NEEDED
  lo_action         TYPE REF TO /iwbep/if_mgw_odata_action,                          "#EC NEEDED
  lo_parameter      TYPE REF TO /iwbep/if_mgw_odata_property,                        "#EC NEEDED
  lo_entity_set     TYPE REF TO /iwbep/if_mgw_odata_entity_set.                      "#EC NEEDED


***********************************************************************************************************************************
*   ENTITY - Header
***********************************************************************************************************************************
lo_entity_type = model->create_entity_type( iv_entity_type_name = 'Header' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'PurchaseOrder' iv_abap_fieldname = 'PURCHASEORDER' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Document_Description' iv_abap_fieldname = 'DOCUMENTDISCRIPTION' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BT_Name1' iv_abap_fieldname = 'BILLTONAME1' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BT_Name2' iv_abap_fieldname = 'BILLTONAME2' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BT_Street' iv_abap_fieldname = 'BILLTOSTREET' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BT_Str_suppl1' iv_abap_fieldname = 'BILLTOSTRSUPPL1' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BT_Str_suppl2' iv_abap_fieldname = 'BILLTOSTRSUPPL2' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BT_Street3' iv_abap_fieldname = 'BILLTOSTREET3' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BT_City1' iv_abap_fieldname = 'BILLTOCITY1' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BT_PostCode1' iv_abap_fieldname = 'BILLTOPOSTCODE1' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BT_Email' iv_abap_fieldname = 'BILLTOEMAIL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_conversion_exit( 'SXIDN' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BT_Website' iv_abap_fieldname = 'BILLTOWEBSITE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BT_Gstin' iv_abap_fieldname = 'BILLTOGSTIN' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BT_StateCode' iv_abap_fieldname = 'BILLTOSTATECODE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BT_Pan' iv_abap_fieldname = 'BILLTOPAN' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'BT_Cin' iv_abap_fieldname = 'BILLTOCIN' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ST_Email' iv_abap_fieldname = 'SHIPTOEMAIL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_conversion_exit( 'SXIDN' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ST_Website' iv_abap_fieldname = 'SHIPTOWEBSITE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ST_Gstin' iv_abap_fieldname = 'SHIPTOGSTIN' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ST_StateCode' iv_abap_fieldname = 'SHIPTOSTATECODE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ST_Pan' iv_abap_fieldname = 'SHIPTOPAN' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ST_Cin' iv_abap_fieldname = 'SHIPTOCIN' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ST_Name1' iv_abap_fieldname = 'SHIPTONAME1' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ST_Name2' iv_abap_fieldname = 'SHIPTONAME2' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ST_Street' iv_abap_fieldname = 'SHIPTOSTREET' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ST_Str_suppl1' iv_abap_fieldname = 'SHIPTOSTRSUPPL1' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ST_Str_suppl2' iv_abap_fieldname = 'SHIPTOSTRSUPPL2' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ST_Street3' iv_abap_fieldname = 'SHIPTOSTREET3' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ST_City1' iv_abap_fieldname = 'SHIPTOCITY1' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ST_PostCode1' iv_abap_fieldname = 'SHIPTOPOSTCODE1' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'SD_Name' iv_abap_fieldname = 'SDNAME' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'SD_Pan' iv_abap_fieldname = 'SDPAN' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'SD_Statecode' iv_abap_fieldname = 'SDSTATECODE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'SD_Address' iv_abap_fieldname = 'SDADDRESS' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'sd_email' iv_abap_fieldname = 'SDEMAIL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'SD_GST' iv_abap_fieldname = 'SDGST' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'SD_Telno' iv_abap_fieldname = 'SDTELNO' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Vendor_Name' iv_abap_fieldname = 'VENDORNAME' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'POD_Revisionno' iv_abap_fieldname = 'PODREVISIONNO' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'POD_QuoteNo' iv_abap_fieldname = 'PODQUOTENO' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'POD_Reauster' iv_abap_fieldname = 'PODREQUSTER' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'POD_PriceEffDate' iv_abap_fieldname = 'PODPRICEEFFECTIVEDATE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'POD_RevisionData' iv_abap_fieldname = 'PODREVISIONDATE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'POD_QuoteDate' iv_abap_fieldname = 'PODQUOTEDATE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'POD_ValidFrom' iv_abap_fieldname = 'PODVALIDFROM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'POD_ValodTo' iv_abap_fieldname = 'PODVALIDTO' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'POD_PurchaseGroup' iv_abap_fieldname = 'PODPURCHASEGROUP' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Incoterms' iv_abap_fieldname = 'INCOTERMS' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'PaymentTerms' iv_abap_fieldname = 'PAYMENTTERMS' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'PoDocumentType' iv_abap_fieldname = 'PODOCUMENTTYPE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'InternalOrderNo' iv_abap_fieldname = 'INTERNALORDERNO' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'InternalOrderDesc' iv_abap_fieldname = 'INTERNALORDERDESC' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZFDP_EF_PURCHAS_03_MPC=>TS_HEADER' ). "#EC NOTEXT


lo_entity_type = model->create_entity_type( iv_entity_type_name = 'Items' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'PurchaseOrderItem' iv_abap_fieldname = 'PURCHASEORDERITEM' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'PurchaseOrder' iv_abap_fieldname = 'PURCHASEORDER' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ItemCode' iv_abap_fieldname = 'MATERIAL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_conversion_exit( 'MATN1' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ItemDescription' iv_abap_fieldname = 'MATERIALDESCRIPTION' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'DeliveryDate' iv_abap_fieldname = 'DELIVERY_DATE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'DrawingNo' iv_abap_fieldname = 'DRAWINGNO' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'DrawingModeNo' iv_abap_fieldname = 'DRAWINGMODENO' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'HSN_SAC' iv_abap_fieldname = 'HSN_SAC' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'UOM' iv_abap_fieldname = 'UOM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_conversion_exit( 'CUNIT' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Qunatity' iv_abap_fieldname = 'QUANTITY' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Rate' iv_abap_fieldname = 'RATE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Discount' iv_abap_fieldname = 'DISCOUNT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'NetPrice' iv_abap_fieldname = 'NETPRICE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'TotalValue' iv_abap_fieldname = 'TOTAL_VALUE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'AmountInWords' iv_abap_fieldname = 'AMOUNTINWORDS' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'IGST' iv_abap_fieldname = 'IGST' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'SGST' iv_abap_fieldname = 'SGST' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'CGST' iv_abap_fieldname = 'CGST' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ItemsText' iv_abap_fieldname = 'ITEMSTEXT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'DeliveryDateText' iv_abap_fieldname = 'DELIVERYDATETEXT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ItemsTextText' iv_abap_fieldname = 'ITEMSTEXTTEXT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'MaterialDescriptionText' iv_abap_fieldname = 'MATERIALDESCRIPTIONTEXT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'UnitofMeasuremnet' iv_abap_fieldname = 'UNITOFMEASUREMENT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZFDP_EF_PURCHAS_03_MPC=>TS_ITEMS' ). "#EC NOTEXT


lo_entity_type = model->create_entity_type( iv_entity_type_name = 'Items_Text' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'PurchaseOrderItem' iv_abap_fieldname = 'PURCHASEORDERITEM' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'PurchaseOrder' iv_abap_fieldname = 'PURCHASEORDER' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'TextLine1' iv_abap_fieldname = 'TEXTLINE1' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'TextLine2' iv_abap_fieldname = 'TEXTLINE2' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'TextLine3' iv_abap_fieldname = 'TEXTLINE3' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'TextLine4' iv_abap_fieldname = 'TEXTLINE4' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'TextLine5' iv_abap_fieldname = 'TEXTLINE5' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'TextLine6' iv_abap_fieldname = 'TEXTLINE6' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'TextLine10' iv_abap_fieldname = 'TEXTLINE10' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'TextLine11' iv_abap_fieldname = 'TEXTLINE11' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZFDP_EF_PURCHAS_03_MPC=>TS_ITEMS_TEXT' ). "#EC NOTEXT


***********************************************************************************************************************************
*   ENTITY SETS
***********************************************************************************************************************************
lo_entity_type = model->get_entity_type( iv_entity_name = 'Header' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'HeaderSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_false ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
lo_entity_type = model->get_entity_type( iv_entity_name = 'Items' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'ItemsSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_false ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
lo_entity_type = model->get_entity_type( iv_entity_name = 'Items_Text' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'Items_TextSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_false ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).


***********************************************************************************************************************************
*   new_associations
***********************************************************************************************************************************

 lo_association = model->create_association(
                            iv_association_name = 'POHeader' "#EC NOTEXT
                            iv_left_type        = 'PurchaseOrderNode' "#EC NOTEXT
                            iv_right_type       = 'Header' "#EC NOTEXT
                            iv_right_card       = '1' "#EC NOTEXT
                            iv_left_card        = '1' ). "#EC NOTEXT
* Referential constraint for association - POHeader
lo_ref_constraint = lo_association->create_ref_constraint( ).
lo_ref_constraint->add_property( iv_principal_property = 'PurchaseOrder'   iv_dependent_property = 'PurchaseOrder' )."#EC NOTEXT
* Association Sets for association - POHeader
lo_assoc_set = lo_association->create_assoc_set( iv_assoc_set_name = 'POHeaderSet' ). "#EC NOTEXT
 lo_association = model->create_association(
                            iv_association_name = 'POItems' "#EC NOTEXT
                            iv_left_type        = 'PurchaseOrderNode' "#EC NOTEXT
                            iv_right_type       = 'Items' "#EC NOTEXT
                            iv_right_card       = 'N' "#EC NOTEXT
                            iv_left_card        = '1' ). "#EC NOTEXT
* Referential constraint for association - POItems
lo_ref_constraint = lo_association->create_ref_constraint( ).
lo_ref_constraint->add_property( iv_principal_property = 'PurchaseOrder'   iv_dependent_property = 'PurchaseOrder' )."#EC NOTEXT
* Association Sets for association - POItems
lo_assoc_set = lo_association->create_assoc_set( iv_assoc_set_name = 'POItemsSet' ). "#EC NOTEXT
 lo_association = model->create_association(
                            iv_association_name = 'POItems_Text' "#EC NOTEXT
                            iv_left_type        = 'PurchaseOrderNode' "#EC NOTEXT
                            iv_right_type       = 'Items_Text' "#EC NOTEXT
                            iv_right_card       = 'N' "#EC NOTEXT
                            iv_left_card        = '1' ). "#EC NOTEXT
* Referential constraint for association - POItems_Text
lo_ref_constraint = lo_association->create_ref_constraint( ).
lo_ref_constraint->add_property( iv_principal_property = 'PurchaseOrder'   iv_dependent_property = 'PurchaseOrder' )."#EC NOTEXT
* Association Sets for association - POItems_Text
lo_assoc_set = lo_association->create_assoc_set( iv_assoc_set_name = 'POItems_TextSet' ). "#EC NOTEXT


* Navigation Properties for entity - Header
lo_entity_type = model->get_entity_type( iv_entity_name = 'Header' ). "#EC NOTEXT
lo_nav_property = lo_entity_type->create_navigation_property( iv_property_name  = 'PurchaseOrderNode' "#EC NOTEXT
                                                          iv_association_name = 'POHeader' ). "#EC NOTEXT
* Navigation Properties for entity - Items
lo_entity_type = model->get_entity_type( iv_entity_name = 'Items' ). "#EC NOTEXT
lo_nav_property = lo_entity_type->create_navigation_property( iv_property_name  = 'PurchaseOrderNode' "#EC NOTEXT
                                                          iv_association_name = 'POItems' ). "#EC NOTEXT
* Navigation Properties for entity - Items_Text
lo_entity_type = model->get_entity_type( iv_entity_name = 'Items_Text' ). "#EC NOTEXT
lo_nav_property = lo_entity_type->create_navigation_property( iv_property_name  = 'PurchaseOrderNode' "#EC NOTEXT
                                                          iv_association_name = 'POItems_Text' ). "#EC NOTEXT


   lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
   lo_nav_property = lo_entity_type->create_navigation_property( iv_property_name  = 'Header' "#EC NOTEXT
                                                          iv_association_name = 'POHeader' ). "#EC NOTEXT
   lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
   lo_nav_property = lo_entity_type->create_navigation_property( iv_property_name  = 'ItemsSet' "#EC NOTEXT
                                                          iv_association_name = 'POItems' ). "#EC NOTEXT
   lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
   lo_nav_property = lo_entity_type->create_navigation_property( iv_property_name  = 'Items_TextSet' "#EC NOTEXT
                                                          iv_association_name = 'POItems_Text' ). "#EC NOTEXT
  endmethod.


  method DEFINE.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


data:
  lo_entity_type    type ref to /iwbep/if_mgw_odata_entity_typ, "#EC NEEDED
  lo_complex_type   type ref to /iwbep/if_mgw_odata_cmplx_type, "#EC NEEDED
  lo_property       type ref to /iwbep/if_mgw_odata_property, "#EC NEEDED
  lo_association    type ref to /iwbep/if_mgw_odata_assoc,  "#EC NEEDED
  lo_assoc_set      type ref to /iwbep/if_mgw_odata_assoc_set, "#EC NEEDED
  lo_ref_constraint type ref to /iwbep/if_mgw_odata_ref_constr, "#EC NEEDED
  lo_nav_property   type ref to /iwbep/if_mgw_odata_nav_prop, "#EC NEEDED
  lo_action         type ref to /iwbep/if_mgw_odata_action, "#EC NEEDED
  lo_parameter      type ref to /iwbep/if_mgw_odata_property, "#EC NEEDED
  lo_entity_set     type ref to /iwbep/if_mgw_odata_entity_set, "#EC NEEDED
  lo_complex_prop   type ref to /iwbep/if_mgw_odata_cmplx_prop. "#EC NEEDED

* Extend the model
model->extend_model( iv_model_name = 'FDP_EF_PURCHASE_ORDER_GLO_GEN_MD' iv_model_version = '0001' ). "#EC NOTEXT

model->set_schema_namespace( 'FDP_EF_PURCHASE_ORDER_SRV' ).


*
* Disable all the entity types that were disabled from reference model
*
* Disable entity type 'InvoicingPartner'
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'InvoicingPartner' ). "#EC NOTEXT
lo_entity_type->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.

IF lo_entity_type IS BOUND.
* Disable all the properties for this entity type
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'PartnerNumber' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'PartnerName' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine1' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine2' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine3' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine4' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine5' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine6' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine7' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'AddressLine8' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'TelephoneNumber' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'Fax' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'EmailAddress' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.


endif.

* Disable entity type 'PurchaseOrderItemManufacturerNode'
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemManufacturerNode' ). "#EC NOTEXT
lo_entity_type->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.

IF lo_entity_type IS BOUND.
* Disable all the properties for this entity type
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'Manufacturer' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'PurchaseOrder' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'PurchaseOrderItem' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'ManufacturerPartNumber' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_property = lo_entity_type->get_property( iv_property_name = 'ManufacturerName' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.


endif.


*Disable selected properties in a entity type
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ShippingType' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ShippingTypeDescription' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ZZ1_SUPPLIEREMAIL_PDH' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ZZ1_SUPPWEBSITE_PDH' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ZZ1_BSTDK_PDH' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ZZ1_EMAIL_PDH' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ZZ1_SUPPWEBSITE_PDHF' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ZZ1_EMAIL_PDHF' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ZZ1_SUPPLIEREMAIL_PDHF' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ZZ1_BSTDK_PDHF' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'GoodsCountCorrection' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ShippingType' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'DeliveryDocumentBySupplier' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ShippingTypeDescription' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'DeliveryDocumentItemBySupplier' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'PPSOPTIONALITEM_TR' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'POSubcontractingComponentsNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'MaterialRevisionLevel' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderHierItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'PPSOPTIONALITEM_TR' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.

*Disable selected navigation properties in a entity type
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_nav_property = lo_entity_type->get_navigation_property( iv_name    = 'InvoicingPartner' ). "#EC NOTEXT
lo_nav_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_nav_property = lo_entity_type->get_navigation_property( iv_name    = 'PurchaseOrderItemManufacturerNode' ). "#EC NOTEXT
lo_nav_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderHierItemNode' ). "#EC NOTEXT
lo_nav_property = lo_entity_type->get_navigation_property( iv_name    = 'PurchaseOrderItemManufacturerNode' ). "#EC NOTEXT
lo_nav_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.


*
*Disable all the entity sets that were disabled from reference model
*
try.
lo_entity_set = model->get_entity_set( iv_entity_set_name = 'InvoicingPartnerSet' ). "#EC NOTEXT
IF lo_entity_set IS BOUND.
lo_entity_set->set_disabled( iv_disabled = abap_true ).
ENDIF.
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_set = model->get_entity_set( iv_entity_set_name = 'PurchaseOrderItemManufacturerNodeSet' ). "#EC NOTEXT
IF lo_entity_set IS BOUND.
lo_entity_set->set_disabled( iv_disabled = abap_true ).
ENDIF.
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.


*
*Disable all the associations, association sets that were disabled from reference model
*
try.
lo_association = model->get_association( iv_association_name = 'POHeader_InvoicingPartner' ). "#EC NOTEXT
lo_association->set_disabled( iv_disabled = abap_true ).
lo_ref_constraint = lo_association->get_ref_constraint( ).
lo_ref_constraint->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_association = model->get_association( iv_association_name = 'POItem_POManufacturer' ). "#EC NOTEXT
lo_association->set_disabled( iv_disabled = abap_true ).
lo_ref_constraint = lo_association->get_ref_constraint( ).
lo_ref_constraint->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_association = model->get_association( iv_association_name = 'POHierItem_POManufacturer' ). "#EC NOTEXT
lo_association->set_disabled( iv_disabled = abap_true ).
lo_ref_constraint = lo_association->get_ref_constraint( ).
lo_ref_constraint->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.


try.
lo_assoc_set  = model->get_association_set( iv_assoc_set_name = 'POHeader_InvoicingPartnerSet' ). "#EC NOTEXT
IF lo_assoc_set IS BOUND.
lo_assoc_set->set_disabled( iv_disabled = abap_true ).
ENDIF.
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_assoc_set  = model->get_association_set( iv_assoc_set_name = 'POItem_POManufacturerSet' ). "#EC NOTEXT
IF lo_assoc_set IS BOUND.
lo_assoc_set->set_disabled( iv_disabled = abap_true ).
ENDIF.
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_assoc_set  = model->get_association_set( iv_assoc_set_name = 'POHierItem_POManufacturerSet' ). "#EC NOTEXT
IF lo_assoc_set IS BOUND.
lo_assoc_set->set_disabled( iv_disabled = abap_true ).
ENDIF.
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
* New artifacts have been created in the service builder after the redefinition of service
create_new_artifacts( ).
  endmethod.


  method GET_EXTENDED_MODEL.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*



ev_extended_service  = 'FDP_EF_PURCHASE_ORDER_GLO_GEN_SRV'.                "#EC NOTEXT
ev_ext_service_version = '0001'.               "#EC NOTEXT
ev_extended_model    = 'FDP_EF_PURCHASE_ORDER_GLO_GEN_MD'.                    "#EC NOTEXT
ev_ext_model_version = '0001'.                   "#EC NOTEXT
  endmethod.


  method GET_LAST_MODIFIED.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


  constants: lc_gen_date_time type timestamp value '20260108070302'. "#EC NOTEXT
rv_last_modified = super->get_last_modified( ).
IF rv_last_modified LT lc_gen_date_time.
  rv_last_modified = lc_gen_date_time.
ENDIF.
  endmethod.


  method LOAD_TEXT_ELEMENTS.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


data:
  lo_entity_type    type ref to /iwbep/if_mgw_odata_entity_typ,           "#EC NEEDED
  lo_complex_type   type ref to /iwbep/if_mgw_odata_cmplx_type,           "#EC NEEDED
  lo_property       type ref to /iwbep/if_mgw_odata_property,             "#EC NEEDED
  lo_association    type ref to /iwbep/if_mgw_odata_assoc,                "#EC NEEDED
  lo_assoc_set      type ref to /iwbep/if_mgw_odata_assoc_set,            "#EC NEEDED
  lo_ref_constraint type ref to /iwbep/if_mgw_odata_ref_constr,           "#EC NEEDED
  lo_nav_property   type ref to /iwbep/if_mgw_odata_nav_prop,             "#EC NEEDED
  lo_action         type ref to /iwbep/if_mgw_odata_action,               "#EC NEEDED
  lo_parameter      type ref to /iwbep/if_mgw_odata_property,             "#EC NEEDED
  lo_entity_set     type ref to /iwbep/if_mgw_odata_entity_set.           "#EC NEEDED


DATA:
     ls_text_element TYPE ts_text_element.                   "#EC NEEDED
  endmethod.
ENDCLASS.
*******************************************************************************************

class ZCL_ZFDP_EF_PURCHAS_03_MPC_EXT definition
  public
  inheriting from ZCL_ZFDP_EF_PURCHAS_03_MPC
  create public .

public section.

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZFDP_EF_PURCHAS_03_MPC_EXT IMPLEMENTATION.


  method DEFINE.

    super->define( ).

  endmethod.
ENDCLASS.

*********************************************************************************

class ZCL_ZFDP_EF_PURCHAS_04_DPC definition
  public
  inheriting from CL_FDP_EF_PURCHASE_ORD_DPC_EXT
  abstract
  create public .

public section.
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZFDP_EF_PURCHAS_04_DPC IMPLEMENTATION.
ENDCLASS.

************************************************************************************

class ZCL_ZFDP_EF_PURCHAS_04_DPC_EXT definition
  public
  inheriting from ZCL_ZFDP_EF_PURCHAS_04_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
protected section.

  methods PO_HEADER_GET_ENTITY
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IO_REQUEST_OBJECT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
    exporting
      !ER_ENTITY type ZCL_ZFDP_EF_PURCHAS_04_MPC=>TS_PO_HEADER
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods PO_ITEMS_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZFDP_EF_PURCHAS_04_MPC=>TT_PO_HEADER
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
private section.
ENDCLASS.



CLASS ZCL_ZFDP_EF_PURCHAS_04_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entity.

    DATA(lv_entityname) = io_tech_request_context->get_entity_type_name( ).
    DATA(lv_entityset) = io_tech_request_context->get_entity_set_name( ).

    CASE lv_entityset.
      WHEN 'PO_HeaderSet'.

        TRY.
            CALL METHOD po_header_get_entity
              EXPORTING
                iv_entity_name      = iv_entity_name
                iv_entity_set_name  = iv_entity_set_name
                iv_source_name      = iv_source_name
                it_key_tab          = it_key_tab                              " table for name value pairs
*               io_request_object   = io_request_object                       " table of navigation paths
               io_tech_request_context = io_tech_request_context
                it_navigation_path  = it_navigation_path               " table of navigation paths
              IMPORTING
                er_entity           = DATA(entity)            " Returning data
                es_response_context = es_response_context.
          CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
          CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception
        ENDTRY.

        IF entity IS NOT INITIAL.
          CALL METHOD copy_data_to_ref
            EXPORTING
              is_data = entity
            CHANGING
              cr_data = er_entity.
        ENDIF.

      WHEN OTHERS.

        TRY.
            super->/iwbep/if_mgw_appl_srv_runtime~get_entity(
              EXPORTING
                iv_entity_name          = iv_entity_name                          " Technical name - Obsolete
                iv_entity_set_name      = iv_entity_set_name                      " Obsolete
                iv_source_name          = iv_source_name                          " Obsolete
                it_key_tab              = it_key_tab                              " table for name value pairs- Obsolete
                it_navigation_path      = it_navigation_path                      " table of navigation paths- Obsolete
                io_tech_request_context = io_tech_request_context
              IMPORTING
                er_entity               = er_entity
                es_response_context     = es_response_context
            ).
          CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
          CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception
        ENDTRY.

    ENDCASE.


  ENDMETHOD.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET.

   DATA(lv_entityset) = io_tech_request_context->get_entity_set_name( ).
   DATA(lv_entity) = io_tech_request_context->get_entity_type_name( ).

    CASE lv_entityset.
      WHEN 'PO_HeaderSet'.

        TRY.
            CALL METHOD po_items_get_entityset
              EXPORTING
                iv_entity_name           = iv_entity_name
                iv_entity_set_name       = iv_entity_set_name
                iv_source_name           = iv_source_name
                it_filter_select_options = it_filter_select_options                 " Table of select options
                is_paging                = is_paging                                " Paging structure
                it_key_tab               = it_key_tab                               " Table for name value pairs
                it_navigation_path       = it_navigation_path                       " Table of navigation paths
                it_order                 = it_order                                 " The sorting order
                iv_filter_string         = iv_filter_string                         " Table for name value pairs
                iv_search_string         = iv_search_string
*                io_tech_request_context  = io_tech_request_context
              IMPORTING
                et_entityset             = data(entityset).                 " Returning data
*                es_response_context      =   .
          CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
          CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception
        ENDTRY.

        IF entityset IS NOT INITIAL.
          CALL METHOD copy_data_to_ref
            EXPORTING
              is_data = entityset
            CHANGING
              cr_data = er_entityset.
        ENDIF.

      WHEN OTHERS.

        TRY.
            super->/iwbep/if_mgw_appl_srv_runtime~get_entityset(
              EXPORTING
                iv_entity_name           = iv_entity_name                           " Obsolete
                iv_entity_set_name       = iv_entity_set_name                       " Obsolete
                iv_source_name           = iv_source_name                           " Obsolete
                it_filter_select_options = it_filter_select_options                 " table of select options - Obsolete
                it_order                 = it_order                                 " the sorting order - Obsolete
                is_paging                = is_paging                                " paging structure - Obsolete
                it_navigation_path       = it_navigation_path                       " table of navigation paths - Obsolete
                it_key_tab               = it_key_tab                               " table for name value pairs - Obsolete
                iv_filter_string         = iv_filter_string                         " the filter as a string containing ANDs and ORs etc -Obsolete
                iv_search_string         = iv_search_string                         " Obsolete
                io_tech_request_context  = io_tech_request_context
              IMPORTING
                er_entityset             = er_entityset
                es_response_context      = es_response_context
            ).
          CATCH /iwbep/cx_mgw_busi_exception. " business exception in mgw
          CATCH /iwbep/cx_mgw_tech_exception. " mgw technical exception
        ENDTRY.

    ENDCASE.

  endmethod.


  METHOD po_header_get_entity.

*    BREAK ntt_abap4.

    DATA(purchase_order) = VALUE #( it_key_tab[ name = 'EBELN' ]-value OPTIONAL ).
    er_entity-bsart = 'ABCD'.

    SELECT SINGLE
      ekpo~werks,
      t001w~adrnr,
      adrc~name1,
      adrc~street,
      adrc~str_suppl3,
      adrc~city1,
      adrc~post_code1
      FROM ekpo AS ekpo
      LEFT OUTER JOIN t001w  AS t001w
      ON t001w~werks = ekpo~werks
      LEFT OUTER JOIN adrc AS adrc
      ON adrc~addrnumber = t001w~adrnr
      INTO @DATA(ls_po)
      WHERE ebeln = @purchase_order
      AND ebelp = 10.

*      SELECT smpt_addr
*        from

  ENDMETHOD.


  method PO_ITEMS_GET_ENTITYSET.

*BREAK ntt_abap4.

    DATA(purchase_order) = VALUE #( it_key_tab[ name = 'EBELN' ]-value OPTIONAL ).

    SELECT WERKS, EBELN FROM EKPO
      INTO TABLE @et_entityset
      WHERE EBELN = @purchase_order.

  endmethod.
ENDCLASS.
****************************************************************************************

class ZCL_ZFDP_EF_PURCHAS_04_MPC definition
  public
  inheriting from /IWBEP/CL_MGW_PUSH_ABS_MODEL
  create public .

public section.

  types:
     TS_HIERITEMPRICINGCONDITIONNOD type MMPUR_S_FDP_ITEM_PRICING_COND .
  types:
TT_HIERITEMPRICINGCONDITIONNOD type standard table of TS_HIERITEMPRICINGCONDITIONNOD .
  types:
   begin of ts_text_element,
      artifact_name  type c length 40,       " technical name
      artifact_type  type c length 4,
      parent_artifact_name type c length 40, " technical name
      parent_artifact_type type c length 4,
      text_symbol    type textpoolky,
   end of ts_text_element .
  types:
         tt_text_elements type standard table of ts_text_element with key text_symbol .
  types:
  begin of TS_INVOICINGPARTNER,
     LIFN2 type C length 10,
     NAME1 type C length 40,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
     TEL_NUMBER type C length 30,
     TELFX type C length 30,
     SMTP_ADDR type C length 241,
  end of TS_INVOICINGPARTNER .
  types:
TT_INVOICINGPARTNER type standard table of TS_INVOICINGPARTNER .
  types:
     TS_ITEMPRICINGCONDITIONNODE type MMPUR_S_FDP_ITEM_PRICING_COND .
  types:
TT_ITEMPRICINGCONDITIONNODE type standard table of TS_ITEMPRICINGCONDITIONNODE .
  types:
  begin of TS_ORDERINGADDRESS,
     LIFN2 type C length 10,
     NAME1 type C length 40,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
     TEL_NUMBER type C length 30,
     TELFX type C length 30,
     SMTP_ADDR type C length 241,
  end of TS_ORDERINGADDRESS .
  types:
TT_ORDERINGADDRESS type standard table of TS_ORDERINGADDRESS .
  types:
     TS_POCONFIGURATIONHIERITEMNODE type TDS_ME_PO_ITEM_CONFIG .
  types:
TT_POCONFIGURATIONHIERITEMNODE type standard table of TS_POCONFIGURATIONHIERITEMNODE .
  types:
     TS_POCONFIGURATIONITEMNODE type TDS_ME_PO_ITEM_CONFIG .
  types:
TT_POCONFIGURATIONITEMNODE type standard table of TS_POCONFIGURATIONITEMNODE .
  types:
     TS_POHIERSUBCONTRACTINGCOMPONE type TDS_ME_PO_ITEM_COMPONENT_BATCH .
  types:
TT_POHIERSUBCONTRACTINGCOMPONE type standard table of TS_POHIERSUBCONTRACTINGCOMPONE .
  types:
     TS_POHIERSUBCONTRACTINGCOMPON type TDS_ME_PO_ITEM_COMPONENTS .
  types:
TT_POHIERSUBCONTRACTINGCOMPON type standard table of TS_POHIERSUBCONTRACTINGCOMPON .
  types:
  begin of TS_POHIERSUBCONTRACTINGCOMPO,
     EBELN type C length 10,
     EBELP type C length 5,
     ETENR type C length 4,
     RESERVATIONITEM type C length 4,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_POHIERSUBCONTRACTINGCOMPO .
  types:
TT_POHIERSUBCONTRACTINGCOMPO type standard table of TS_POHIERSUBCONTRACTINGCOMPO .
  types:
     TS_POSUBCONTRACTINGCOMPONENTSB type TDS_ME_PO_ITEM_COMPONENT_BATCH .
  types:
TT_POSUBCONTRACTINGCOMPONENTSB type standard table of TS_POSUBCONTRACTINGCOMPONENTSB .
  types:
     TS_POSUBCONTRACTINGCOMPONENTSN type TDS_ME_PO_ITEM_COMPONENTS .
  types:
TT_POSUBCONTRACTINGCOMPONENTSN type standard table of TS_POSUBCONTRACTINGCOMPONENTSN .
  types:
  begin of TS_POSUBCONTRACTINGCOMPONENTST,
     EBELN type C length 10,
     EBELP type C length 5,
     ETENR type C length 4,
     RESERVATIONITEM type C length 4,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_POSUBCONTRACTINGCOMPONENTST .
  types:
TT_POSUBCONTRACTINGCOMPONENTST type standard table of TS_POSUBCONTRACTINGCOMPONENTST .
  types:
  begin of TS_PURCHASEORDERCHANGESNODE,
     EBELN type C length 10,
     CHTXT type C length 30,
  end of TS_PURCHASEORDERCHANGESNODE .
  types:
TT_PURCHASEORDERCHANGESNODE type standard table of TS_PURCHASEORDERCHANGESNODE .
  types:
  begin of TS_PURCHASEORDERHEADERSTTEXTS,
     EBELN type C length 10,
     DRUVO type C length 1,
     ESART type C length 4,
     TDOBJECT type C length 10,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     DRFLG type C length 2,
     TDOBNAME type C length 70,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERHEADERSTTEXTS .
  types:
TT_PURCHASEORDERHEADERSTTEXTS type standard table of TS_PURCHASEORDERHEADERSTTEXTS .
  types:
  begin of TS_PURCHASEORDERHEADERTEXTS,
     EBELN type C length 10,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERHEADERTEXTS .
  types:
TT_PURCHASEORDERHEADERTEXTS type standard table of TS_PURCHASEORDERHEADERTEXTS .
  types:
     TS_PURCHASEORDERHIERITEMBATCHN type TDS_ME_PO_ITEM_BATCH .
  types:
TT_PURCHASEORDERHIERITEMBATCHN type standard table of TS_PURCHASEORDERHIERITEMBATCHN .
  types:
  begin of TS_PURCHASEORDERHIERITEMCHANGE,
     EBELN type C length 10,
     EBELP type C length 5,
     CHTXT type C length 30,
  end of TS_PURCHASEORDERHIERITEMCHANGE .
  types:
TT_PURCHASEORDERHIERITEMCHANGE type standard table of TS_PURCHASEORDERHIERITEMCHANGE .
  types:
     TS_PURCHASEORDERHIERITEMNODE type TDS_ME_PO_ITEM .
  types:
TT_PURCHASEORDERHIERITEMNODE type standard table of TS_PURCHASEORDERHIERITEMNODE .
  types:
  begin of TS_PURCHASEORDERHIERITEMSTTEXT,
     EBELN type C length 10,
     EBELP type C length 5,
     DRUVO type C length 1,
     ESART type C length 4,
     PSTYP type C length 1,
     TDOBJECT type C length 10,
     LANGUAGE type C length 2,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     DRFLG type C length 2,
     DRPRI type C length 1,
     TDOBNAME type C length 70,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERHIERITEMSTTEXT .
  types:
TT_PURCHASEORDERHIERITEMSTTEXT type standard table of TS_PURCHASEORDERHIERITEMSTTEXT .
  types:
  begin of TS_PURCHASEORDERHIERITEMTEXTS,
     EBELN type C length 10,
     EBELP type C length 5,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERHIERITEMTEXTS .
  types:
TT_PURCHASEORDERHIERITEMTEXTS type standard table of TS_PURCHASEORDERHIERITEMTEXTS .
  types:
     TS_PURCHASEORDERHIERSCHEDULELI type TDS_ME_PO_SCHEDULELINE .
  types:
TT_PURCHASEORDERHIERSCHEDULELI type standard table of TS_PURCHASEORDERHIERSCHEDULELI .
  types:
     TS_PURCHASEORDERITEMBATCHNODE type TDS_ME_PO_ITEM_BATCH .
  types:
TT_PURCHASEORDERITEMBATCHNODE type standard table of TS_PURCHASEORDERITEMBATCHNODE .
  types:
  begin of TS_PURCHASEORDERITEMCHANGESNOD,
     EBELN type C length 10,
     EBELP type C length 5,
     CHTXT type C length 30,
  end of TS_PURCHASEORDERITEMCHANGESNOD .
  types:
TT_PURCHASEORDERITEMCHANGESNOD type standard table of TS_PURCHASEORDERITEMCHANGESNOD .
  types:
     TS_PURCHASEORDERITEMMANUFACTUR type TDS_ME_PO_MPN .
  types:
TT_PURCHASEORDERITEMMANUFACTUR type standard table of TS_PURCHASEORDERITEMMANUFACTUR .
  types:
     TS_PURCHASEORDERITEMNODE type TDS_ME_PO_ITEM .
  types:
TT_PURCHASEORDERITEMNODE type standard table of TS_PURCHASEORDERITEMNODE .
  types:
  begin of TS_PURCHASEORDERITEMSTTEXTS,
     EBELN type C length 10,
     EBELP type C length 5,
     DRUVO type C length 1,
     ESART type C length 4,
     PSTYP type C length 1,
     TDOBJECT type C length 10,
     LANGUAGE type C length 2,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     DRFLG type C length 2,
     DRPRI type C length 1,
     TDOBNAME type C length 70,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERITEMSTTEXTS .
  types:
TT_PURCHASEORDERITEMSTTEXTS type standard table of TS_PURCHASEORDERITEMSTTEXTS .
  types:
  begin of TS_PURCHASEORDERITEMTEXTS,
     EBELN type C length 10,
     EBELP type C length 5,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERITEMTEXTS .
  types:
TT_PURCHASEORDERITEMTEXTS type standard table of TS_PURCHASEORDERITEMTEXTS .
  types:
  begin of TS_PURCHASEORDERLIMITITEMCHANG,
     EBELN type C length 10,
     EBELP type C length 5,
     CHTXT type C length 30,
  end of TS_PURCHASEORDERLIMITITEMCHANG .
  types:
TT_PURCHASEORDERLIMITITEMCHANG type standard table of TS_PURCHASEORDERLIMITITEMCHANG .
  types:
  begin of TS_PURCHASEORDERLIMITITEMNODE,
     EBELN type C length 10,
     EBELP type C length 5,
     PSTYP type C length 1,
     TXZ01 type C length 40,
     MATKL type C length 9,
     WERKS type C length 4,
     EXPECTED_VALUE type P length 8 decimals 3,
     NETPR type P length 7 decimals 3,
     NETWR type P length 9 decimals 3,
     WAERS type C length 5,
     MMPUR_SERVPROC_PERIOD_START type TIMESTAMP,
     MMPUR_SERVPROC_PERIOD_END type TIMESTAMP,
     ADRNR type C length 10,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
     SERVICEPERFORMER type C length 10,
     SERVICEPERFORMERNAME type C length 80,
     PRODUCTTYPE type C length 2,
     PEINH type P length 3 decimals 0,
     LABNR type C length 20,
     KTMNG type P length 7 decimals 3,
     WEMNG type P length 7 decimals 3,
     WAMNG type P length 7 decimals 3,
     PRSDR type C length 1,
     LOEKZ type C length 1,
     RETPO type C length 1,
     KZABS type FLAG,
     ELIKZ type C length 1,
  end of TS_PURCHASEORDERLIMITITEMNODE .
  types:
TT_PURCHASEORDERLIMITITEMNODE type standard table of TS_PURCHASEORDERLIMITITEMNODE .
  types:
  begin of TS_PURCHASEORDERLIMITITEMSTTEX,
     EBELN type C length 10,
     EBELP type C length 5,
     DRUVO type C length 1,
     ESART type C length 4,
     PSTYP type C length 1,
     TDOBJECT type C length 10,
     LANGUAGE type C length 2,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     DRFLG type C length 2,
     DRPRI type C length 1,
     TDOBNAME type C length 70,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERLIMITITEMSTTEX .
  types:
TT_PURCHASEORDERLIMITITEMSTTEX type standard table of TS_PURCHASEORDERLIMITITEMSTTEX .
  types:
  begin of TS_PURCHASEORDERLIMITITEMTEXTS,
     EBELN type C length 10,
     EBELP type C length 5,
     TEXT_ID type C length 4,
     TEXT_ID_DESCR type C length 30,
     LANGUAGE type C length 2,
     TEXT_CONTENT type string,
  end of TS_PURCHASEORDERLIMITITEMTEXTS .
  types:
TT_PURCHASEORDERLIMITITEMTEXTS type standard table of TS_PURCHASEORDERLIMITITEMTEXTS .
  types:
     TS_PURCHASEORDERNODE type TDS_ME_PO_HEADER .
  types:
TT_PURCHASEORDERNODE type standard table of TS_PURCHASEORDERNODE .
  types:
     TS_PURCHASEORDERSCHEDULELINENO type TDS_ME_PO_SCHEDULELINE .
  types:
TT_PURCHASEORDERSCHEDULELINENO type standard table of TS_PURCHASEORDERSCHEDULELINENO .
  types:
  begin of TS_PURCHASINGGROUPNODE,
     EKGRP type C length 3,
     EKNAM type C length 18,
     TEL_NUMBER type C length 30,
     TELFX type C length 31,
     SMTP_ADDR type C length 241,
  end of TS_PURCHASINGGROUPNODE .
  types:
TT_PURCHASINGGROUPNODE type standard table of TS_PURCHASINGGROUPNODE .
  types:
  begin of TS_QUERYNODE,
     EBELN type C length 10,
     LAND1 type C length 3,
     LANGU type C length 1,
     CHANGE_FLAG type C length 1,
     LIFN2 type C length 10,
     OUTPUTTYPE type C length 30,
     OUTPUTPREVIEW type C length 1,
  end of TS_QUERYNODE .
  types:
TT_QUERYNODE type standard table of TS_QUERYNODE .
  types:
  begin of TS_SHIPTOPARTYNODE,
     ADRNR type C length 10,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
  end of TS_SHIPTOPARTYNODE .
  types:
TT_SHIPTOPARTYNODE type standard table of TS_SHIPTOPARTYNODE .
  types:
  begin of TS_SUPPLIERNODE,
     LIFNR type C length 10,
     NAME1 type C length 35,
     ADRNR type C length 10,
     SMTP_ADDR type C length 241,
     ADDRESS_LINE_1 type C length 80,
     ADDRESS_LINE_2 type C length 80,
     ADDRESS_LINE_3 type C length 80,
     ADDRESS_LINE_4 type C length 80,
     ADDRESS_LINE_5 type C length 80,
     ADDRESS_LINE_6 type C length 80,
     ADDRESS_LINE_7 type C length 80,
     ADDRESS_LINE_8 type C length 80,
     STCEG type C length 20,
  end of TS_SUPPLIERNODE .
  types:
TT_SUPPLIERNODE type standard table of TS_SUPPLIERNODE .
  types:
  begin of TS_PO_HEADER,
     BSART type string,
     EBELN type string,
  end of TS_PO_HEADER .
  types:
TT_PO_HEADER type standard table of TS_PO_HEADER .

  constants GC_SUPPLIERNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'SupplierNode' ##NO_TEXT.
  constants GC_SHIPTOPARTYNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ShipToPartyNode' ##NO_TEXT.
  constants GC_QUERYNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'QueryNode' ##NO_TEXT.
  constants GC_PURCHASINGGROUPNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchasingGroupNode' ##NO_TEXT.
  constants GC_PURCHASEORDERSCHEDULELINENO type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderScheduleLineNode' ##NO_TEXT.
  constants GC_PURCHASEORDERNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderNode' ##NO_TEXT.
  constants GC_PURCHASEORDERLIMITITEMTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderLimitItemTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERLIMITITEMSTTEX type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderLimitItemSTTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERLIMITITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderLimitItemNode' ##NO_TEXT.
  constants GC_PURCHASEORDERLIMITITEMCHANG type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderLimitItemChangesNode' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMSTTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemSTTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemNode' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMMANUFACTUR type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemManufacturerNode' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMCHANGESNOD type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemChangesNode' ##NO_TEXT.
  constants GC_PURCHASEORDERITEMBATCHNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderItemBatchNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERSCHEDULELI type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierScheduleLineNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMSTTEXT type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemSTTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMCHANGE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemChangesNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHIERITEMBATCHN type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHierItemBatchNode' ##NO_TEXT.
  constants GC_PURCHASEORDERHEADERTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHeaderTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERHEADERSTTEXTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderHeaderSTTexts' ##NO_TEXT.
  constants GC_PURCHASEORDERCHANGESNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PurchaseOrderChangesNode' ##NO_TEXT.
  constants GC_PO_HEADER type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PO_Header' ##NO_TEXT.
  constants GC_POSUBCONTRACTINGCOMPONENTST type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POSubcontractingComponentsTexts' ##NO_TEXT.
  constants GC_POSUBCONTRACTINGCOMPONENTSN type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POSubcontractingComponentsNode' ##NO_TEXT.
  constants GC_POSUBCONTRACTINGCOMPONENTSB type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POSubcontractingComponentsBatchNode' ##NO_TEXT.
  constants GC_POHIERSUBCONTRACTINGCOMPONE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POHierSubcontractingComponentsBatchNode' ##NO_TEXT.
  constants GC_POHIERSUBCONTRACTINGCOMPON type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POHierSubcontractingComponentsNode' ##NO_TEXT.
  constants GC_POHIERSUBCONTRACTINGCOMPO type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POHierSubcontractingComponentsTexts' ##NO_TEXT.
  constants GC_POCONFIGURATIONITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POConfigurationItemNode' ##NO_TEXT.
  constants GC_POCONFIGURATIONHIERITEMNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'POConfigurationHierItemNode' ##NO_TEXT.
  constants GC_ORDERINGADDRESS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'OrderingAddress' ##NO_TEXT.
  constants GC_ITEMPRICINGCONDITIONNODE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ItemPricingConditionNode' ##NO_TEXT.
  constants GC_INVOICINGPARTNER type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'InvoicingPartner' ##NO_TEXT.
  constants GC_HIERITEMPRICINGCONDITIONNOD type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'HierItemPricingConditionNode' ##NO_TEXT.

  methods GET_EXTENDED_MODEL
  final
    exporting
      !EV_EXTENDED_SERVICE type /IWBEP/MED_GRP_TECHNICAL_NAME
      !EV_EXT_SERVICE_VERSION type /IWBEP/MED_GRP_VERSION
      !EV_EXTENDED_MODEL type /IWBEP/MED_MDL_TECHNICAL_NAME
      !EV_EXT_MODEL_VERSION type /IWBEP/MED_MDL_VERSION
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods LOAD_TEXT_ELEMENTS
  final
    returning
      value(RT_TEXT_ELEMENTS) type TT_TEXT_ELEMENTS
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .

  methods DEFINE
    redefinition .
  methods GET_LAST_MODIFIED
    redefinition .
protected section.
private section.

  methods CREATE_NEW_ARTIFACTS
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
ENDCLASS.



CLASS ZCL_ZFDP_EF_PURCHAS_04_MPC IMPLEMENTATION.


  method CREATE_NEW_ARTIFACTS.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


DATA:
  lo_entity_type    TYPE REF TO /iwbep/if_mgw_odata_entity_typ,                      "#EC NEEDED
  lo_complex_type   TYPE REF TO /iwbep/if_mgw_odata_cmplx_type,                      "#EC NEEDED
  lo_property       TYPE REF TO /iwbep/if_mgw_odata_property,                        "#EC NEEDED
  lo_association    TYPE REF TO /iwbep/if_mgw_odata_assoc,                           "#EC NEEDED
  lo_assoc_set      TYPE REF TO /iwbep/if_mgw_odata_assoc_set,                       "#EC NEEDED
  lo_ref_constraint TYPE REF TO /iwbep/if_mgw_odata_ref_constr,                      "#EC NEEDED
  lo_nav_property   TYPE REF TO /iwbep/if_mgw_odata_nav_prop,                        "#EC NEEDED
  lo_action         TYPE REF TO /iwbep/if_mgw_odata_action,                          "#EC NEEDED
  lo_parameter      TYPE REF TO /iwbep/if_mgw_odata_property,                        "#EC NEEDED
  lo_entity_set     TYPE REF TO /iwbep/if_mgw_odata_entity_set.                      "#EC NEEDED


***********************************************************************************************************************************
*   ENTITY - PO_Header
***********************************************************************************************************************************
lo_entity_type = model->create_entity_type( iv_entity_type_name = 'PO_Header' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'DocumentType' iv_abap_fieldname = 'BSART' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'PurchaseOrder' iv_abap_fieldname = 'EBELN' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZFDP_EF_PURCHAS_04_MPC=>TS_PO_HEADER' ). "#EC NOTEXT


***********************************************************************************************************************************
*   ENTITY SETS
***********************************************************************************************************************************
lo_entity_type = model->get_entity_type( iv_entity_name = 'PO_Header' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'PO_HeaderSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_false ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).


***********************************************************************************************************************************
*   new_associations
***********************************************************************************************************************************

 lo_association = model->create_association(
                            iv_association_name = 'POHeader' "#EC NOTEXT
                            iv_left_type        = 'PurchaseOrderNode' "#EC NOTEXT
                            iv_right_type       = 'PO_Header' "#EC NOTEXT
                            iv_right_card       = '1' "#EC NOTEXT
                            iv_left_card        = '1' ). "#EC NOTEXT
* Referential constraint for association - POHeader
lo_ref_constraint = lo_association->create_ref_constraint( ).
lo_ref_constraint->add_property( iv_principal_property = 'PurchaseOrder'   iv_dependent_property = 'PurchaseOrder' )."#EC NOTEXT
* Association Sets for association - POHeader
lo_assoc_set = lo_association->create_assoc_set( iv_assoc_set_name = 'POHeaderSet' ). "#EC NOTEXT


* Navigation Properties for entity - PO_Header
lo_entity_type = model->get_entity_type( iv_entity_name = 'PO_Header' ). "#EC NOTEXT
lo_nav_property = lo_entity_type->create_navigation_property( iv_property_name  = 'PurchaseOrderNode' "#EC NOTEXT
                                                          iv_association_name = 'POHeader' ). "#EC NOTEXT


   lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
   lo_nav_property = lo_entity_type->create_navigation_property( iv_property_name  = 'PO_Header' "#EC NOTEXT
                                                          iv_association_name = 'POHeader' ). "#EC NOTEXT
  endmethod.


  method DEFINE.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


data:
  lo_entity_type    type ref to /iwbep/if_mgw_odata_entity_typ, "#EC NEEDED
  lo_complex_type   type ref to /iwbep/if_mgw_odata_cmplx_type, "#EC NEEDED
  lo_property       type ref to /iwbep/if_mgw_odata_property, "#EC NEEDED
  lo_association    type ref to /iwbep/if_mgw_odata_assoc,  "#EC NEEDED
  lo_assoc_set      type ref to /iwbep/if_mgw_odata_assoc_set, "#EC NEEDED
  lo_ref_constraint type ref to /iwbep/if_mgw_odata_ref_constr, "#EC NEEDED
  lo_nav_property   type ref to /iwbep/if_mgw_odata_nav_prop, "#EC NEEDED
  lo_action         type ref to /iwbep/if_mgw_odata_action, "#EC NEEDED
  lo_parameter      type ref to /iwbep/if_mgw_odata_property, "#EC NEEDED
  lo_entity_set     type ref to /iwbep/if_mgw_odata_entity_set, "#EC NEEDED
  lo_complex_prop   type ref to /iwbep/if_mgw_odata_cmplx_prop. "#EC NEEDED

* Extend the model
model->extend_model( iv_model_name = 'FDP_EF_PURCHASE_ORDER_MDL' iv_model_version = '0001' ). "#EC NOTEXT

model->set_schema_namespace( 'FDP_EF_PURCHASE_ORDER_SRV' ).


*Disable selected properties in a entity type
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ZZ1_TEST_PDH' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'ZZ1_TEST_PDHF' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'PPSOPTIONALITEM_TR' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
try.
lo_entity_type = model->get_entity_type( iv_entity_name = 'PurchaseOrderHierItemNode' ). "#EC NOTEXT
lo_property    = lo_entity_type->get_property( iv_property_name = 'PPSOPTIONALITEM_TR' ). "#EC NOTEXT
lo_property->set_disabled( iv_disabled = abap_true ).
CATCH /iwbep/cx_mgw_med_exception.
*  No Action was taken as the OData Element is not a part of redefined service
ENDTRY.
* New artifacts have been created in the service builder after the redefinition of service
create_new_artifacts( ).
  endmethod.


  method GET_EXTENDED_MODEL.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*



ev_extended_service  = 'FDP_EF_PURCHASE_ORDER_SRV'.                "#EC NOTEXT
ev_ext_service_version = '0001'.               "#EC NOTEXT
ev_extended_model    = 'FDP_EF_PURCHASE_ORDER_MDL'.                    "#EC NOTEXT
ev_ext_model_version = '0001'.                   "#EC NOTEXT
  endmethod.


  method GET_LAST_MODIFIED.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


  constants: lc_gen_date_time type timestamp value '20250407055105'. "#EC NOTEXT
rv_last_modified = super->get_last_modified( ).
IF rv_last_modified LT lc_gen_date_time.
  rv_last_modified = lc_gen_date_time.
ENDIF.
  endmethod.


  method LOAD_TEXT_ELEMENTS.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


data:
  lo_entity_type    type ref to /iwbep/if_mgw_odata_entity_typ,           "#EC NEEDED
  lo_complex_type   type ref to /iwbep/if_mgw_odata_cmplx_type,           "#EC NEEDED
  lo_property       type ref to /iwbep/if_mgw_odata_property,             "#EC NEEDED
  lo_association    type ref to /iwbep/if_mgw_odata_assoc,                "#EC NEEDED
  lo_assoc_set      type ref to /iwbep/if_mgw_odata_assoc_set,            "#EC NEEDED
  lo_ref_constraint type ref to /iwbep/if_mgw_odata_ref_constr,           "#EC NEEDED
  lo_nav_property   type ref to /iwbep/if_mgw_odata_nav_prop,             "#EC NEEDED
  lo_action         type ref to /iwbep/if_mgw_odata_action,               "#EC NEEDED
  lo_parameter      type ref to /iwbep/if_mgw_odata_property,             "#EC NEEDED
  lo_entity_set     type ref to /iwbep/if_mgw_odata_entity_set.           "#EC NEEDED


DATA:
     ls_text_element TYPE ts_text_element.                   "#EC NEEDED
  endmethod.
ENDCLASS.
**************************************************************************************************

class ZCL_ZFDP_EF_PURCHAS_04_MPC_EXT definition
  public
  inheriting from ZCL_ZFDP_EF_PURCHAS_04_MPC
  create public .

public section.
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZFDP_EF_PURCHAS_04_MPC_EXT IMPLEMENTATION.
ENDCLASS.
******************************************************************************************************************
