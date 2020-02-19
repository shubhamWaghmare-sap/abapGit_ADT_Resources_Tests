*"* use this source file for your ABAP unit test classes

CLASS ltcl_simple_transformation DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      pos_st_repo_pull_seq_ok     FOR TESTING RAISING cx_static_check,
      pos_st_repo_pull_seq_not_ok FOR TESTING RAISING cx_static_check,
      pos_st_repo_pull_new_field  FOR TESTING RAISING cx_static_check,
      pos_st_repo_pull_field_miss FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_simple_transformation IMPLEMENTATION.

  METHOD pos_st_repo_pull_seq_ok.
    "Test: Sequence of fields in input XML is equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repo_pull_test=>ty_request_pull_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository>'.
    add_xml '  <branch>a</branch>'.
    add_xml '  <transportRequest>b</transportRequest>'.
    add_xml '  <user>c</user>'.
    add_xml '  <password>d</password>'.
    add_xml '</repository>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_pull_test=>co_root_name_pull.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_pull
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'd' ).

  ENDMETHOD.

  METHOD pos_st_repo_pull_seq_not_ok.
    "Test: Sequence of fields in input XML is NOT equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repo_pull_test=>ty_request_pull_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository>'.
    add_xml '  <branch>a</branch>'.
    add_xml '  <user>c</user>'. "exchanged by TRANSPORTREQUEST
    add_xml '  <transportRequest>b</transportRequest>'. "exchanged by USER
    add_xml '  <password>d</password>'.
    add_xml '</repository>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_pull_test=>co_root_name_pull.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_pull
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'd' ).

  ENDMETHOD.

  METHOD pos_st_repo_pull_new_field.
    "Test: Input XML contains a field that is not part of ABAP structure
    "Result: Transformation should succeed, new field should be ignored

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repo_pull_test=>ty_request_pull_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository>'.
    add_xml '  <branch>a</branch>'.
    add_xml '  <transportRequest>b</transportRequest>'.
    add_xml '  <dontexist>x</dontexist>'. "field don't exist in structure
    add_xml '  <user>c</user>'.
    add_xml '  <password>d</password>'.
    add_xml '</repository>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_pull_test=>co_root_name_pull.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_pull
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'd' ).

  ENDMETHOD.

  METHOD pos_st_repo_pull_field_miss.
    "Test: In input XML a field defined in ABAP structure is missing
    "Result: Transformation should succeed, missing field should be initial

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repo_pull_test=>ty_request_pull_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository>'.
    add_xml '  <branch>a</branch>'.
    "add_xml '  <transportRequest>b</transportRequest>'. "field missing(!)
    add_xml '  <user>c</user>'.
    add_xml '  <password>d</password>'.
    add_xml '</repository>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_pull_test=>co_root_name_pull.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_pull
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_initial( act = ls_data-transportrequest ). "initial value expected
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'd' ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_simple_transformation_v2 DEFINITION FINAL FOR TESTING
DURATION SHORT
RISK LEVEL HARMLESS.

PRIVATE SECTION.
  METHODS: pos_st_repo_pull_seq_ok      FOR TESTING RAISING cx_static_check,
           pos_st_repo_pull_seq_not_ok  FOR TESTING RAISING cx_static_check,
           pos_st_repo_pull_new_field   FOR TESTING RAISING cx_static_check,
           pos_st_repo_pull_field_miss FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_simple_transformation_v2 IMPLEMENTATION.

  METHOD pos_st_repo_pull_seq_ok.
    "Test: Sequence of fields in input XML is equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA ls_data      TYPE zcl_abapgit_res_repo_pull_test=>ty_request_pull_data.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.


    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<abapgitrepo:repository xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">'.
    add_xml '  <abapgitrepo:branchName>a</abapgitrepo:branchName>'.
    add_xml '  <abapgitrepo:transportRequest>b</abapgitrepo:transportRequest>'.
    add_xml '  <abapgitrepo:remoteUser>c</abapgitrepo:remoteUser>'.
    add_xml '  <abapgitrepo:remotePassword>d</abapgitrepo:remotePassword>'.
    add_xml '</abapgitrepo:repository>'.

    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_pull_test=>co_root_name_pull.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_pull_v2
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'a' act = ls_data-branch ).
    cl_abap_unit_assert=>assert_equals( exp = 'b' act = ls_data-transportrequest ).
    cl_abap_unit_assert=>assert_equals( exp = 'c' act = ls_data-user ).
    cl_abap_unit_assert=>assert_equals( exp = 'd' act = ls_data-password ).

  ENDMETHOD.

  METHOD pos_st_repo_pull_seq_not_ok.
    "Test: Sequence of fields in input XML is NOT equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repo_pull_test=>ty_request_pull_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value>  TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<abapgitrepo:repository xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">'.
    add_xml '  <abapgitrepo:transportRequest>b</abapgitrepo:transportRequest>'. "Exchanged by Transport Request
    add_xml '  <abapgitrepo:branchName>a</abapgitrepo:branchName>'. "Exchanged by Branch Name
    add_xml '  <abapgitrepo:remoteUser>c</abapgitrepo:remoteUser>'.
    add_xml '  <abapgitrepo:remotePassword>d</abapgitrepo:remotePassword>'.
    add_xml '</abapgitrepo:repository>'.

    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_pull_test=>co_root_name_pull.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_pull_v2
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'a' act = ls_data-branch ).
    cl_abap_unit_assert=>assert_equals( exp = 'b' act = ls_data-transportrequest ).
    cl_abap_unit_assert=>assert_equals( exp = 'c' act = ls_data-user ).
    cl_abap_unit_assert=>assert_equals( exp = 'd' act = ls_data-password ).

  ENDMETHOD.

  METHOD pos_st_repo_pull_new_field.
    "Test: Input XML contains a field that is not part of ABAP structure
    "Result: Transformation should succeed, new field should be ignored

    DATA lv_input_xml TYPE xstring.
    DATA lt_result TYPE abap_trans_resbind_tab.
    DATA ls_data TYPE zcl_abapgit_res_repo_pull_test=>ty_request_pull_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_vlaue> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<abapgitrepo:repository xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">'.
    add_xml '  <abapgitrepo:branchName>a</abapgitrepo:branchName>'.
    add_xml '  <abapgitrepo:transportRequest>b</abapgitrepo:transportRequest>'.
    add_xml '  <abapgitrepo:newField>x</abapgitrepo:newField>'.
    add_xml '  <abapgitrepo:remoteUser>c</abapgitrepo:remoteUser>'.
    add_xml '  <abapgitrepo:remotePassword>d</abapgitrepo:remotePassword>'.
    add_xml '</abapgitrepo:repository>'.

    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_pull_test=>co_root_name_pull.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_pull_v2
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'd' ).

  ENDMETHOD.

  METHOD pos_st_repo_pull_field_miss.
    "Test: In input XML a field defined in ABAP structure is missing
    "Result: Transformation should succeed, missing field should be initial

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repo_pull_test=>ty_request_pull_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<abapgitrepo:repository xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">'.
    add_xml '  <abapgitrepo:branchName>a</abapgitrepo:branchName>'.
    "add_xml '  <abapgitrepo:transportRequest>b</abapgitrepo:transportRequest>'.
    add_xml '  <abapgitrepo:remoteUser>c</abapgitrepo:remoteUser>'.
    add_xml '  <abapgitrepo:remotePassword>d</abapgitrepo:remotePassword>'.
    add_xml '</abapgitrepo:repository>'.

    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repo_pull_test=>co_root_name_pull.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_pull_v2
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals(  act = lines( lt_result ) exp = 1  ).
    cl_abap_unit_assert=>assert_equals(  act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_initial( act = ls_data-transportrequest ). "initial value expected
    cl_abap_unit_assert=>assert_equals(  act = ls_data-user             exp = 'c' ).
    cl_abap_unit_assert=>assert_equals(  act = ls_data-password         exp = 'd' ).

  ENDMETHOD.

ENDCLASS.

*CLASS ltcl_post_method_test DEFINITION FINAL FOR TESTING
*RISK LEVEL HARMLESS
*DURATION SHORT.
*
*PRIVATE SECTION.
*  METHODS: post_method_test_happy_flow FOR TESTING.
*
*ENDCLASS.

*CLASS ltcl_post_method_test IMPLEMENTATION.
*
*  METHOD post_method_test_happy_flow.
*    "Given
*     DATA(request_stub) = new cl_adt_rest_request_stub(  ).
*     DATA(response_spy) = new cl_adt_rest_response_spy(  ).
*     DATA(repo_pull_resource) = new zcl_abapgit_res_repo_pull_test(  ).
*     DATA request_data TYPE zcl_abapgit_res_repo_pull_test=>ty_request_pull_data.
*
*     request_stub->add_header_field( EXPORTING key = if_http_header_fields=>content_type
*                                               value = zcl_abapgit_res_repo_pull_test=>co_content_type_repo_v3 ).
*
*     request_data-branch = 'refs/heads/master'.
*     request_stub->set_body_data( data = request_data ).
*
*     request_stub->set_uri_attribute( name = 'key' value = '000000000083' ).
*
*     try.
*         repo_pull_resource->post( exporting request = request_stub
*                                             response = response_spy ).
*       catch cx_adt_rest.
*           "handle exception
*     endtry.
*
*     cl_abap_unit_assert=>assert_equals( exp = cl_rest_status_code=>gc_success_ok
*                                         act = response_spy->get_status(  ) ).
*  ENDMETHOD.
*
*ENDCLASS.
