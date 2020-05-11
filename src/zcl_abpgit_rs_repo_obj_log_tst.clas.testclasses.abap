*"* use this source file for your ABAP unit test classes

*CLASS ltcl_simple_transformation DEFINITION FINAL FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*
*    METHODS:
*      pos_st_repo_pull_seq_ok     FOR TESTING RAISING cx_static_check,
*      pos_st_repo_pull_seq_not_ok FOR TESTING RAISING cx_static_check,
*      pos_st_repo_pull_new_field  FOR TESTING RAISING cx_static_check,
*      pos_st_repo_pull_field_miss FOR TESTING RAISING cx_static_check.
*
*ENDCLASS.
*
*CLASS ltcl_simple_transformation IMPLEMENTATION.
*
*  METHOD pos_st_repo_pull_seq_ok.
*    "Test: Sequence of fields in input XML is equal to fields operated by simple transformation
*    "Result: Transformation should succeed
*
*    DATA lv_input_xml TYPE xstring.
*    DATA lt_result    TYPE abap_trans_resbind_tab.
*    DATA ls_data      TYPE cl_abapgit_res_repo_obj_log=>ty_request_pull_data.
*    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
*    FIELD-SYMBOLS <lv_value> TYPE any.
*
*    DATA lv_xml TYPE string.
*    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
*    add_xml '<repository>'.
*    add_xml '  <branch>a</branch>'.
*    add_xml '  <transportRequest>b</transportRequest>'.
*    add_xml '  <user>c</user>'.
*    add_xml '  <password>d</password>'.
*    add_xml '</repository>'.
*    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.
*
*    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
*    <lt_result>-name = cl_abapgit_res_repo_obj_log=>co_root_name_pull.
*    GET REFERENCE OF ls_data INTO <lt_result>-value.
*
*    CALL TRANSFORMATION abapgit_st_repo_pull
*      SOURCE XML lv_input_xml
*      RESULT     (lt_result).
*
*    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
*    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
*    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'b' ).
*    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'c' ).
*    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'd' ).
*
*  ENDMETHOD.
*
*  METHOD pos_st_repo_pull_seq_not_ok.
*    "Test: Sequence of fields in input XML is NOT equal to fields operated by simple transformation
*    "Result: Transformation should succeed
*
*    DATA lv_input_xml TYPE xstring.
*    DATA lt_result    TYPE abap_trans_resbind_tab.
*    DATA ls_data      TYPE cl_abapgit_res_repo_obj_log=>ty_request_pull_data.
*    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
*    FIELD-SYMBOLS <lv_value> TYPE any.
*
*    DATA lv_xml TYPE string.
*    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
*    add_xml '<repository>'.
*    add_xml '  <branch>a</branch>'.
*    add_xml '  <user>c</user>'. "exchanged by TRANSPORTREQUEST
*    add_xml '  <transportRequest>b</transportRequest>'. "exchanged by USER
*    add_xml '  <password>d</password>'.
*    add_xml '</repository>'.
*    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.
*
*    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
*    <lt_result>-name = cl_abapgit_res_repo_obj_log=>co_root_name_pull.
*    GET REFERENCE OF ls_data INTO <lt_result>-value.
*
*    CALL TRANSFORMATION abapgit_st_repo_pull
*      SOURCE XML lv_input_xml
*      RESULT     (lt_result).
*
*    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
*    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
*    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'b' ).
*    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'c' ).
*    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'd' ).
*
*  ENDMETHOD.
*
*  METHOD pos_st_repo_pull_new_field.
*    "Test: Input XML contains a field that is not part of ABAP structure
*    "Result: Transformation should succeed, new field should be ignored
*
*    DATA lv_input_xml TYPE xstring.
*    DATA lt_result    TYPE abap_trans_resbind_tab.
*    DATA ls_data      TYPE cl_abapgit_res_repo_obj_log=>ty_request_pull_data.
*    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
*    FIELD-SYMBOLS <lv_value> TYPE any.
*
*    DATA lv_xml TYPE string.
*    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
*    add_xml '<repository>'.
*    add_xml '  <branch>a</branch>'.
*    add_xml '  <transportRequest>b</transportRequest>'.
*    add_xml '  <dontexist>x</dontexist>'. "field don't exist in structure
*    add_xml '  <user>c</user>'.
*    add_xml '  <password>d</password>'.
*    add_xml '</repository>'.
*    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.
*
*    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
*    <lt_result>-name = cl_abapgit_res_repo_obj_log=>co_root_name_pull.
*    GET REFERENCE OF ls_data INTO <lt_result>-value.
*
*    CALL TRANSFORMATION abapgit_st_repo_pull
*      SOURCE XML lv_input_xml
*      RESULT     (lt_result).
*
*    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
*    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
*    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'b' ).
*    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'c' ).
*    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'd' ).
*
*  ENDMETHOD.
*
*  METHOD pos_st_repo_pull_field_miss.
*    "Test: In input XML a field defined in ABAP structure is missing
*    "Result: Transformation should succeed, missing field should be initial
*
*    DATA lv_input_xml TYPE xstring.
*    DATA lt_result    TYPE abap_trans_resbind_tab.
*    DATA ls_data      TYPE cl_abapgit_res_repo_obj_log=>ty_request_pull_data.
*    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
*    FIELD-SYMBOLS <lv_value> TYPE any.
*
*    DATA lv_xml TYPE string.
*    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
*    add_xml '<repository>'.
*    add_xml '  <branch>a</branch>'.
*    "add_xml '  <transportRequest>b</transportRequest>'. "field missing(!)
*    add_xml '  <user>c</user>'.
*    add_xml '  <password>d</password>'.
*    add_xml '</repository>'.
*    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.
*
*    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
*    <lt_result>-name = cl_abapgit_res_repo_obj_log=>co_root_name_pull.
*    GET REFERENCE OF ls_data INTO <lt_result>-value.
*
*    CALL TRANSFORMATION abapgit_st_repo_pull
*      SOURCE XML lv_input_xml
*      RESULT     (lt_result).
*
*    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
*    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
*    cl_abap_unit_assert=>assert_initial( act = ls_data-transportrequest ). "initial value expected
*    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'c' ).
*    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'd' ).
*
*  ENDMETHOD.
*
*ENDCLASS.
CLASS ltcl_test_get_method DEFINITION DEFERRED.
CLASS zcl_abpgit_rs_repo_obj_log_tst DEFINITION LOCAL FRIENDS ltcl_test_get_method.

CLASS ltcl_simple_transformation_v2 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS obj_log_serialize_ok FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_simple_transformation_v2 IMPLEMENTATION.
  METHOD obj_log_serialize_ok.
    DATA lv_xml TYPE string.
    DATA lt_response_data TYPE zcl_abpgit_rs_repo_obj_log_tst=>tt_obj_result.

    add_xml '<?xml version="1.0" encoding="utf-16"?>'.
    add_xml '<abapObjects:abapObjects xmlns:abapObjects="http://www.sap.com/adt/abapgit/abapObjects">'.
    add_xml ' <abapObjects:abapObject>'.
    add_xml '   <abapObjects:type>t1</abapObjects:type>'.
    add_xml '   <abapObjects:name>dummy_name</abapObjects:name>'.
    add_xml '   <abapObjects:package>dummy_package</abapObjects:package>'.
    add_xml '   <abapObjects:status>A</abapObjects:status>'.
    add_xml '   <abapObjects:msgType>W</abapObjects:msgType>'.
    add_xml '   <abapObjects:msgText>Warning</abapObjects:msgText>'.
    add_xml ' </abapObjects:abapObject>'.
    add_xml ' <abapObjects:abapObject>'.
    add_xml '   <abapObjects:type>t2</abapObjects:type>'.
    add_xml '   <abapObjects:name>dummy_name2</abapObjects:name>'.
    add_xml '   <abapObjects:package>dummy_package2</abapObjects:package>'.
    add_xml '   <abapObjects:status>I</abapObjects:status>'.
    add_xml '   <abapObjects:msgType>E</abapObjects:msgType>'.
    add_xml '   <abapObjects:msgText>Error</abapObjects:msgText>'.
    add_xml ' </abapObjects:abapObject>'.
    add_xml '</abapObjects:abapObjects>'.

    lt_response_data = VALUE #( ( obj_type = 't1' obj_status = 'A' obj_name = 'dummy_name'
                                  package = 'dummy_package' msg_type = 'W' msg_text = 'Warning' )
                                ( obj_type = 't2' obj_status = 'I' obj_name = 'dummy_name2'
                                  package = 'dummy_package2' msg_type = 'E' msg_text = 'Error' )  ).

    DATA lv_xml_output TYPE string.

    CALL TRANSFORMATION abapgit_st_repo_post_res_v2
      SOURCE OBJECTS = lt_response_data
      RESULT XML lv_xml_output.

    CALL TRANSFORMATION id SOURCE XML lv_xml        RESULT XML lv_xml.
    CALL TRANSFORMATION id SOURCE XML lv_xml_output RESULT XML lv_xml_output.

    cl_abap_unit_assert=>assert_equals( exp = lv_xml act = lv_xml_output ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_test_get_method DEFINITION FINAL FOR TESTING
DURATION SHORT
RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: request_stub TYPE REF TO cl_adt_rest_request_stub,
          response_spy TYPE REF TO cl_adt_rest_response_spy.

    CLASS-DATA:
          f_cut TYPE REF TO zcl_abpgit_rs_repo_obj_log_tst.

    CLASS-METHODS:
          class_setup RAISING cx_static_check.

    METHODS:
          get_call_ok FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_test_get_method IMPLEMENTATION.

  METHOD class_setup.
    "create code under test instance
    CREATE OBJECT f_cut.

  ENDMETHOD.

  METHOD get_call_ok.

    CREATE OBJECT request_stub.
    CREATE OBJECT response_spy.

    request_stub->add_header_field( EXPORTING key   = if_http_header_fields=>accept
                                              value = zcl_abpgit_rs_repo_obj_log_tst=>co_content_type_object_v2 ).

    request_stub->set_uri_attribute( EXPORTING value = 'dummy_key'
                                               name  = 'app_log_key' ).


    "configure abapgit_log test double
    DATA ro_log TYPE REF TO if_abapgit_log.
    ro_log ?= cl_abap_testdouble=>create( 'if_abapgit_log').

    "Prepare exporting parameter
    DATA et_item_status TYPE if_abapgit_log=>tty_item_status_out.
    DATA et_item        TYPE if_abapgit_log=>ty_item_status_out.

    DATA lv_item TYPE if_abapgit_definitions=>ty_item.
    lv_item-obj_type = 'CLAS'.
    lv_item-obj_name = 'TEST_CLASS'.
    lv_item-devclass = 'DEVC'.
    lv_item-inactive = abap_false.

    DATA lv_messages TYPE if_abapgit_log=>tty_msg.
    lv_messages = VALUE #( ( type ='S' text = 'Object TEST_CLASS imported' ) ).

    et_item-item     = lv_item.
    et_item-messages = lv_messages.
    et_item-status   = 'S'.

    INSERT et_item INTO et_item_status INDEX 1.

    "Configure call
    cl_abap_testdouble=>configure_call( ro_log )->set_parameter( name  = 'et_item_status'
                                                                 value = et_item_status ).
    "Which call
    ro_log->get_item_status(  ).

    "inject the double
    f_cut->lo_log = ro_log.

    "call to method under test.
      f_cut->get( request  = request_stub
                  response = response_spy  ).

    "get response data
     DATA lt_response_data TYPE zcl_abpgit_rs_repo_obj_log_tst=>tt_obj_result.
     response_spy->get_body_data( IMPORTING data = lt_response_data  ).

     DATA line_response_data LIKE LINE OF lt_response_data.

     READ TABLE lt_response_data INTO line_response_data INDEX 1.

    "assert
    cl_abap_unit_assert=>assert_equals( exp =  cl_rest_status_code=>gc_success_ok act = response_spy->get_status(  ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'S' act = line_response_data-obj_status ).
    cl_abap_unit_assert=>assert_equals( exp = lv_item-obj_name act = line_response_data-obj_name ).

  ENDMETHOD.

ENDCLASS.
