*"* use this source file for your ABAP unit test classes

CLASS ltcl_simple_transformation DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      pos_st_i_ext_req_seq_ok     FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_seq_not_ok FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_new_field  FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_field_miss FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_simple_transformation IMPLEMENTATION.

  METHOD pos_st_i_ext_req_seq_ok.
    "Test: Sequence of fields in input XML is equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abpgit_rs_repo_inf_ext_tst=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository_ext>'.
    add_xml '  <url>a</url>'.
    add_xml '  <user>b</user>'.
    add_xml '  <password>c</password>'.
    add_xml '</repository_ext>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abpgit_rs_repo_inf_ext_tst=>co_root_name_request.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_req
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url      exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user     exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password exp = 'c' ).

  ENDMETHOD.

  METHOD pos_st_i_ext_req_seq_not_ok.
    "Test: Sequence of fields in input XML is NOT equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abpgit_rs_repo_inf_ext_tst=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository_ext>'.
    add_xml '  <url>a</url>'.
    add_xml '  <password>c</password>'. "exchanged by USER
    add_xml '  <user>b</user>'. "exchanged by PASSWORD
    add_xml '</repository_ext>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abpgit_rs_repo_inf_ext_tst=>co_root_name_request.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_req
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url      exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user     exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password exp = 'c' ).

  ENDMETHOD.

  METHOD pos_st_i_ext_req_new_field.
    "Test: Input XML contains a field that is not part of ABAP structure
    "Result: Transformation should succeed, new field should be ignored

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abpgit_rs_repo_inf_ext_tst=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository_ext>'.
    add_xml '  <url>a</url>'.
    add_xml '  <dontexist>x</dontexist>'. "field don't exist in structure
    add_xml '  <user>b</user>'.
    add_xml '  <password>c</password>'.
    add_xml '</repository_ext>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abpgit_rs_repo_inf_ext_tst=>co_root_name_request.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_req
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url      exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user     exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password exp = 'c' ).

  ENDMETHOD.

  METHOD pos_st_i_ext_req_field_miss.
    "Test: In input XML a field defined in ABAP structure is missing
    "Result: Transformation should succeed, missing field should be initial

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abpgit_rs_repo_inf_ext_tst=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository_ext>'.
    add_xml '  <url>a</url>'.
    "add_xml '  <user>b</user>'.  "field missing(!)
    add_xml '  <password>c</password>'.
    add_xml '</repository_ext>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abpgit_rs_repo_inf_ext_tst=>co_root_name_request.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_req
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url      exp = 'a' ).
    cl_abap_unit_assert=>assert_initial( act = ls_data-user ). "initial value expected
    cl_abap_unit_assert=>assert_equals( act = ls_data-password exp = 'c' ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_simple_transformation_v2 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      pos_st_i_ext_req_seq_ok     FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_seq_not_ok FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_new_field  FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_field_miss FOR TESTING RAISING cx_static_check,
    pos_st_i_ext_res_serialize_ok FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_simple_transformation_v2 IMPLEMENTATION.

  METHOD pos_st_i_ext_req_field_miss.

    "Test: Input XML has a missing field from fields in ABAP Structure operated by ST
    "Result: Transformation should work, missing field is initial

    DATA lv_request_data TYPE zcl_abpgit_rs_repo_inf_ext_tst=>ty_request_data.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-16"?>'.
    add_xml '<abapgitexternalrepo:externalRepoInfoRequest xmlns:abapgitexternalrepo="http://www.sap.com/adt/abapgit/externalRepo">'.
    add_xml ' <abapgitexternalrepo:url>dummy_url</abapgitexternalrepo:url>'.
    "add_xml ' <abapgitexternalrepo:user>dummy_user</abapgitexternalrepo:user>'.    missing field
    add_xml ' <abapgitexternalrepo:password>dummy_password</abapgitexternalrepo:password>'.
    add_xml '</abapgitexternalrepo:externalRepoInfoRequest>'.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_rq_v2
      SOURCE XML lv_xml
      RESULT     REPOSITORY_EXTERNAL_REQ = lv_request_data.

    cl_abap_unit_assert=>assert_equals( exp = 'dummy_url'      act = lv_request_data-url ).
    cl_abap_unit_assert=>assert_initial( lv_request_data-user ).
    cl_abap_unit_assert=>assert_equals( exp = 'dummy_password' act = lv_request_data-password ).

  ENDMETHOD.

  METHOD pos_st_i_ext_req_new_field.

    "Test: Input XML has a new field which is not in the fields in ABAP Structure operated by ST
    "Result: Transformation should work

    DATA lv_request_data TYPE zcl_abpgit_rs_repo_inf_ext_tst=>ty_request_data.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-16"?>'.
    add_xml '<abapgitexternalrepo:externalRepoInfoRequest xmlns:abapgitexternalrepo="http://www.sap.com/adt/abapgit/externalRepo">'.
    add_xml ' <abapgitexternalrepo:user>dummy_user</abapgitexternalrepo:user>'. "exchanged by url
    add_xml ' <abapgitexternalrepo:url>dummy_url</abapgitexternalrepo:url>'.  "exchanged by user
    add_xml ' <abapgitexternalrepo:password>dummy_password</abapgitexternalrepo:password>'.
    add_xml ' <abapgitexternalrepo:newField>dummy_field</abapgitexternalrepo:newField>'.
    add_xml '</abapgitexternalrepo:externalRepoInfoRequest>'.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_rq_v2
      SOURCE XML lv_xml
      RESULT     REPOSITORY_EXTERNAL_REQ = lv_request_data.

    cl_abap_unit_assert=>assert_equals( exp = 'dummy_url'      act = lv_request_data-url ).
    cl_abap_unit_assert=>assert_equals( exp = 'dummy_user'     act = lv_request_data-user ).
    cl_abap_unit_assert=>assert_equals( exp = 'dummy_password' act = lv_request_data-password ).



  ENDMETHOD.

  METHOD pos_st_i_ext_req_seq_not_ok.
    "Test: Sequence of fields in Input XML is not same as the sequence of fields in Abap Structure operated by ST
    "Result: Transformation should work

    DATA lv_request_data TYPE zcl_abpgit_rs_repo_inf_ext_tst=>ty_request_data.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-16"?>'.
    add_xml '<abapgitexternalrepo:externalRepoInfoRequest xmlns:abapgitexternalrepo="http://www.sap.com/adt/abapgit/externalRepo">'.
    add_xml ' <abapgitexternalrepo:user>dummy_user</abapgitexternalrepo:user>'. "exchanged by url
    add_xml ' <abapgitexternalrepo:url>dummy_url</abapgitexternalrepo:url>'.  "exchanged by user
    add_xml ' <abapgitexternalrepo:password>dummy_password</abapgitexternalrepo:password>'.
    add_xml '</abapgitexternalrepo:externalRepoInfoRequest>'.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_rq_v2
      SOURCE XML lv_xml
      RESULT     REPOSITORY_EXTERNAL_REQ = lv_request_data.

    cl_abap_unit_assert=>assert_equals( exp = 'dummy_url'      act = lv_request_data-url ).
    cl_abap_unit_assert=>assert_equals( exp = 'dummy_user'     act = lv_request_data-user ).
    cl_abap_unit_assert=>assert_equals( exp = 'dummy_password' act = lv_request_data-password ).

  ENDMETHOD.

  METHOD pos_st_i_ext_req_seq_ok.
    "Test: Sequence of fields in Input XMl is same as the sequence of fields in Abap Structure operated by ST
    "Result: Transformation should work

    DATA ls_request_data TYPE zcl_abpgit_rs_repo_inf_ext_tst=>ty_request_data.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-16"?>'.
    add_xml '<abapgitexternalrepo:externalRepoInfoRequest xmlns:abapgitexternalrepo="http://www.sap.com/adt/abapgit/externalRepo">'.
    add_xml ' <abapgitexternalrepo:url>dummy_url</abapgitexternalrepo:url>'.
    add_xml ' <abapgitexternalrepo:user>dummy_user</abapgitexternalrepo:user>'.
    add_xml ' <abapgitexternalrepo:password>dummy_password</abapgitexternalrepo:password>'.
    add_xml '</abapgitexternalrepo:externalRepoInfoRequest>'.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_rq_v2
      SOURCE XML lv_xml
      RESULT     REPOSITORY_EXTERNAL_REQ = ls_request_data.

    cl_abap_unit_assert=>assert_equals( exp = 'dummy_url'      act = ls_request_data-url ).
    cl_abap_unit_assert=>assert_equals( exp = 'dummy_user'     act = ls_request_data-user ).
    cl_abap_unit_assert=>assert_equals( exp = 'dummy_password' act = ls_request_data-password ).

  ENDMETHOD.

  METHOD pos_st_i_ext_res_serialize_ok.
    "Test: Serialize response_data using ST
    "Result: Output XML by ST should be equal expected XML

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-16"?>'.
    add_xml '<abapgitexternalrepo:externalRepoInfo xmlns:abapgitexternalrepo="http://www.sap.com/adt/abapgit/externalRepo">'.
    add_xml '<abapgitexternalrepo:accessMode>public</abapgitexternalrepo:accessMode>'.
    add_xml '<abapgitexternalrepo:branch>'.
    add_xml '<abapgitexternalrepo:sha1>dummy_sha</abapgitexternalrepo:sha1>'.
    add_xml '<abapgitexternalrepo:name>dummy_name</abapgitexternalrepo:name>'.
    add_xml '<abapgitexternalrepo:type>t1</abapgitexternalrepo:type>'.
    add_xml '<abapgitexternalrepo:isHead>X</abapgitexternalrepo:isHead>'.
    add_xml '<abapgitexternalrepo:displayName>dummy_display_name</abapgitexternalrepo:displayName>'.
    add_xml '</abapgitexternalrepo:branch>'.
    add_xml '</abapgitexternalrepo:externalRepoInfo>'.


    DATA ls_response_data TYPE zcl_abpgit_rs_repo_inf_ext_tst=>ty_response_data.
    ls_response_data-access_mode ='public'.
    ls_response_data-branches = VALUE #( ( sha1         = 'dummy_sha'
                                           name         = 'dummy_name'
                                           type         = 't1'
                                           is_head      =  abap_true
                                           display_name = 'dummy_display_name' ) ).

    DATA lv_xml_output TYPE string.
    CALL TRANSFORMATION abapgit_st_repo_info_ext_rs_v2
      SOURCE REPOSITORY_EXTERNAL = ls_response_data
      RESULT XML lv_xml_output.

    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_xml.
    CALL TRANSFORMATION id SOURCE XML lv_xml_output RESULT XML lv_xml_output.


    cl_abap_unit_assert=>assert_equals( exp = lv_xml act = lv_xml_output ).

  ENDMETHOD.
ENDCLASS.

CLASS ltd_abapgit_git_branch_list DEFINITION INHERITING FROM cl_abapgit_git_branch_list.

PUBLIC SECTION.
  METHODS get_branches_only REDEFINITION.
ENDCLASS.

CLASS ltd_abapgit_git_branch_list IMPLEMENTATION.

  METHOD get_branches_only.
      DATA lt_branches_input TYPE if_abapgit_definitions=>ty_git_branch_list_tt.

      lt_branches_input = VALUE #( ( display_name = 'head'
                                     is_head      = abap_true
                                     name         = 'ref/head/branch'
                                     sha1         = 'asadasdasdasdasdasd'
                                     type         = 'HD' ) ).

      rt_branches = lt_branches_input.

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_post_method DEFINITION FINAL FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.

  CONSTANTS:
        co_test_user_name type string value 'test_user',
        co_test_pass      type string value 'dGVzdF9wYXNzd29yZA==',
        co_test_url       type string value 'http://githost.com/git_repo.git'.

  CLASS-DATA:
        request_stub TYPE REF TO cl_adt_rest_request_stub,
        response_spy TYPE REF TO cl_adt_rest_response_spy,
        f_cut        TYPE REF TO zcl_abpgit_rs_repo_inf_ext_tst.

  CLASS-METHODS:
        class_setup RAISING cx_static_check.

  METHODS:
        test_post_public_repo      FOR TESTING RAISING cx_static_check,
        test_post_private_repo_ok  FOR TESTING RAISING cx_static_check,
        test_post_private_repo_2fa FOR TESTING RAISING cx_static_check.


ENDCLASS.

class zcl_abpgit_rs_repo_inf_ext_tst definition local friends ltcl_post_method.

CLASS ltcl_post_method IMPLEMENTATION.

  METHOD class_setup.

    CREATE OBJECT f_cut.
    CREATE OBJECT request_stub.
    CREATE OBJECT response_spy.

  ENDMETHOD.

  METHOD test_post_public_repo.

    "prepare request
    request_stub->add_header_field( key   = if_http_header_fields=>content_type
                                    value = zcl_abpgit_rs_repo_inf_ext_tst=>co_content_type_request_v2 ).
    request_stub->add_header_field( key   = if_http_header_fields=>accept
                                    value = zcl_abpgit_rs_repo_inf_ext_tst=>co_content_type_response_v2 ).

    DATA request_data TYPE zcl_abpgit_rs_repo_inf_ext_tst=>ty_request_data.

    request_data-url      = co_test_url.

    request_stub->set_body_data( data = request_data ).

    "prepare test double
    DATA lv_abapgit_auth_helper TYPE REF TO zcl_abapgit_auth_check_helper.
    lv_abapgit_auth_helper ?= cl_abap_testdouble=>create( 'zcl_abapgit_auth_check_helper' ).

    "configure call
    cl_abap_testdouble=>configure_call( lv_abapgit_auth_helper )->returning( 'PUBLIC' ).
    "which call
    lv_abapgit_auth_helper->determine_access_level( co_test_url ).

    "Prepare test double
    DATA lv_branch_list TYPE REF TO ltd_abapgit_git_branch_list.
    CREATE OBJECT lv_branch_list EXPORTING iv_data = co_test_url.

    "inject test double
    f_cut->lv_abapgit_auth_helper  = lv_abapgit_auth_helper.
    f_cut->lo_branch_list  = lv_branch_list.

    "call method under test
    f_cut->post( request  = request_stub
                 response = response_spy  ).

    cl_abap_unit_assert=>assert_equals( exp = cl_rest_status_code=>gc_success_ok  act = response_spy->get_status(  ) ).

  ENDMETHOD.

  METHOD test_post_private_repo_ok.

    "prepare request
    request_stub->add_header_field( key   = if_http_header_fields=>content_type
                                    value = zcl_abpgit_rs_repo_inf_ext_tst=>co_content_type_request_v2 ).
    request_stub->add_header_field( key   = if_http_header_fields=>accept
                                    value = zcl_abpgit_rs_repo_inf_ext_tst=>co_content_type_response_v2 ).

    DATA request_data TYPE zcl_abpgit_rs_repo_inf_ext_tst=>ty_request_data.

    request_data-url      = co_test_url.
    request_data-user     = co_test_user_name.
    request_data-password = co_test_pass.

    request_stub->set_body_data( data = request_data ).

    "prepare test double
    DATA lv_abapgit_auth_helper TYPE REF TO zcl_abapgit_auth_check_helper.
    lv_abapgit_auth_helper ?= cl_abap_testdouble=>create( 'zcl_abapgit_auth_check_helper' ).

    "configure call
    cl_abap_testdouble=>configure_call( lv_abapgit_auth_helper )->returning( 'PRIVATE' ).
    "which call
    lv_abapgit_auth_helper->determine_access_level( co_test_url ).

    "configure call
    cl_abap_testdouble=>configure_call( lv_abapgit_auth_helper )->returning( abap_false ).
    "which call
    lv_abapgit_auth_helper->is_2fa_required( iv_url      = co_test_url
                                             iv_username = co_test_user_name
                                             iv_password = co_test_pass ).

    "Prepare test double
    DATA lv_branch_list TYPE REF TO ltd_abapgit_git_branch_list.
    CREATE OBJECT lv_branch_list EXPORTING iv_data = co_test_url.

    "inject test double
    f_cut->lv_abapgit_auth_helper  = lv_abapgit_auth_helper.
    f_cut->lo_branch_list  = lv_branch_list.

    "call method under test
    f_cut->post( request  = request_stub
                 response = response_spy  ).

    cl_abap_unit_assert=>assert_equals( exp = cl_rest_status_code=>gc_success_ok  act = response_spy->get_status(  ) ).


  ENDMETHOD.

  METHOD test_post_private_repo_2fa.

    DATA lv_http_status TYPE string.

    "prepare request
    request_stub->add_header_field( key   = if_http_header_fields=>content_type
                                    value = zcl_abpgit_rs_repo_inf_ext_tst=>co_content_type_request_v2 ).
    request_stub->add_header_field( key   = if_http_header_fields=>accept
                                    value = zcl_abpgit_rs_repo_inf_ext_tst=>co_content_type_response_v2 ).

    DATA request_data TYPE zcl_abpgit_rs_repo_inf_ext_tst=>ty_request_data.

    request_data-url      = co_test_url.
    request_data-user     = co_test_user_name.
    request_data-password = co_test_pass.

    request_stub->set_body_data( data = request_data ).

    "prepare test double
    DATA lv_abapgit_auth_helper TYPE REF TO zcl_abapgit_auth_check_helper.
    lv_abapgit_auth_helper ?= cl_abap_testdouble=>create( 'zcl_abapgit_auth_check_helper' ).

    "configure call
    cl_abap_testdouble=>configure_call( lv_abapgit_auth_helper )->returning( 'PRIVATE' ).
    "which call
    lv_abapgit_auth_helper->determine_access_level( co_test_url ).

    "configure call
    cl_abap_testdouble=>configure_call( lv_abapgit_auth_helper )->returning( abap_true ).
    "which call
    lv_abapgit_auth_helper->is_2fa_required( iv_url      = co_test_url
                                             iv_username = co_test_user_name
                                             iv_password = co_test_pass ).

    "Prepare test double
    DATA lv_branch_list TYPE REF TO ltd_abapgit_git_branch_list.
    CREATE OBJECT lv_branch_list EXPORTING iv_data = co_test_url.

    "inject test double
    f_cut->lv_abapgit_auth_helper  = lv_abapgit_auth_helper.
    f_cut->lo_branch_list  = lv_branch_list.

    "call method under test
    TRY.
      f_cut->post( request  = request_stub
                   response = response_spy  ).
      cl_abap_unit_assert=>fail( 'Exception not raised' ).

    CATCH cx_adt_rest_abapgit INTO DATA(lx_abapgit_rest_exception).
         lv_http_status  = lx_abapgit_rest_exception->get_text(  ).
*        lx_abapgit_rest_exception->properties->get_property(
*          EXPORTING
*            key   = 'http_status'
*          IMPORTING
*            value = lv_http_status
*        ).

      cl_abap_unit_assert=>assert_equals( act = lv_http_status exp = '2FA required' ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
