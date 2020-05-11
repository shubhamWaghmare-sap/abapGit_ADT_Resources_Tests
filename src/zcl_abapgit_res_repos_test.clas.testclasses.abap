*"* use this source file for your ABAP unit test classes

*CLASS ltcl_simple_transformation DEFINITION DEFERRED.
*CLASS cl_abapgit_res_repos DEFINITION LOCAL FRIENDS ltcl_simple_transformation.

CLASS ltcl_simple_transformation DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      pos_st_repo_post_seq_ok     FOR TESTING RAISING cx_static_check,
      pos_st_repo_post_seq_not_ok FOR TESTING RAISING cx_static_check,
      pos_st_repo_post_new_field  FOR TESTING RAISING cx_static_check,
      pos_st_repo_post_field_miss FOR TESTING RAISING cx_static_check,
      repo_v2_deserialize_ok      FOR TESTING RAISING cx_static_check,
      repo_v2_serialize_ok        FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_simple_transformation IMPLEMENTATION.

  METHOD pos_st_repo_post_seq_ok.
    "Test: Sequence of fields in input XML is equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos_test=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository>'.
    add_xml '  <branch>a</branch>'.
    add_xml '  <package>b</package>'.
    add_xml '  <transportRequest>c</transportRequest>'.
    add_xml '  <url>d</url>'.
    add_xml '  <user>e</user>'.
    add_xml '  <password>f</password>'.
    add_xml '</repository>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos_test=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url              exp = 'd' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'f' ).

  ENDMETHOD.

  METHOD pos_st_repo_post_seq_not_ok.
    "Test: Sequence of fields in input XML is NOT equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos_test=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository>'.
    add_xml '  <branch>a</branch>'.
    add_xml '  <package>b</package>'.
    add_xml '  <transportRequest>c</transportRequest>'.
    add_xml '  <user>e</user>'. "exchanged by URL
    add_xml '  <url>d</url>'. "exchanged by USER
    add_xml '  <password>f</password>'.
    add_xml '</repository>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos_test=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url              exp = 'd' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'f' ).

  ENDMETHOD.

  METHOD pos_st_repo_post_new_field.
    "Test: Input XML contains a field that is not part of ABAP structure
    "Result: Transformation should succeed, new field should be ignored

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos_test=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository>'.
    add_xml '  <branch>a</branch>'.
    add_xml '  <package>b</package>'.
    add_xml '  <transportRequest>c</transportRequest>'.
    add_xml '  <dontexist>x</dontexist>'. "field don't exist in structure
    add_xml '  <url>d</url>'.
    add_xml '  <user>e</user>'.
    add_xml '  <password>f</password>'.
    add_xml '</repository>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos_test=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url              exp = 'd' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'f' ).

  ENDMETHOD.

  METHOD pos_st_repo_post_field_miss.
    "Test: In input XML a field defined in ABAP structure is missing
    "Result: Transformation should succeed, missing field should be initial

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos_test=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<repository>'.
    add_xml '  <branch>a</branch>'.
    add_xml '  <package>b</package>'.
    add_xml '  <transportRequest>c</transportRequest>'.
    "add_xml '  <url>d</url>'. "field missing(!)
    add_xml '  <user>e</user>'.
    add_xml '  <password>f</password>'.
    add_xml '</repository>'.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos_test=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_initial( act = ls_data-url ). "initial value expected
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'f' ).

  ENDMETHOD.

  METHOD repo_v2_deserialize_ok.
    DATA: xml_data        TYPE string,
          repository_data TYPE zcl_abapgit_res_repos_test=>tt_request_data.

    xml_data = xml_data && |<?xml version="1.0" encoding="UTF-8"?><repositories>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |  <repository>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |    <branch>refs/heads/master</branch>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |    <package>TEST_YY</package>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |    <url>https://github.com/Wunderfitz/yy.git</url>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |  </repository>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |  <repository>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |    <branch>refs/heads/master</branch>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |    <package>TEST_LOGGER</package>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |    <url>https://github.com/epeterson320/ABAP-Logger.git</url>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |  </repository>| && cl_abap_char_utilities=>newline.
    xml_data = xml_data && |</repositories>|.

    CALL TRANSFORMATION abapgit_st_repo_post_v2
      SOURCE XML xml_data
      RESULT repositories = repository_data.

    cl_abap_unit_assert=>assert_not_initial( repository_data ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( repository_data ) ).
  ENDMETHOD.

  METHOD repo_v2_serialize_ok.
    DATA: repository_data TYPE zcl_abapgit_res_repos_test=>tt_request_data,
          xml_data        TYPE string.

    repository_data = VALUE #( ( url = 'https://github.com/Wunderfitz/yy.git' branch = 'refs/heads/master' package = 'TEST_YY' )
                               ( url = 'https://github.com/epeterson320/ABAP-Logger.git' branch = 'refs/heads/master' package = 'TEST_LOGGER' ) ).

    CALL TRANSFORMATION abapgit_st_repo_post_v2
      SOURCE repositories = repository_data
      RESULT XML xml_data.

    cl_abap_unit_assert=>assert_not_initial( xml_data ).
    cl_abap_unit_assert=>assert_true( boolc( xml_data CS '<package>TEST_YY</package>' ) ).

  ENDMETHOD.

ENDCLASS.

CLASS ltd_abapgit_provider_default DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_abapgit_provider.
ENDCLASS.

CLASS ltd_abapgit_provider_default IMPLEMENTATION.

  METHOD lif_abapgit_provider~list_repositories.

  ENDMETHOD.

  METHOD lif_abapgit_provider~perform_import.
    CASE is_request_data-url.
      WHEN 'https://github.com/Wunderfitz/jak.git'.
        cl_abap_unit_assert=>assert_equals( exp = 'refs/heads/master' act = is_request_data-branch ).
        cl_abap_unit_assert=>assert_equals( exp = 'TESCHD_JAK' act = is_request_data-package ).
      WHEN 'https://github.com/Wunderfitz/yy.git'.
        cl_abap_unit_assert=>assert_equals( exp = 'refs/heads/master' act = is_request_data-branch ).
        cl_abap_unit_assert=>assert_equals( exp = 'TESCHD_YY' act = is_request_data-package ).
      WHEN OTHERS.
        cl_abap_unit_assert=>fail( msg = |Unexpected Git repository { is_request_data-url } | ).
    ENDCASE.
  ENDMETHOD.

  METHOD lif_abapgit_provider~link_repo.
    CASE is_request_data-url.
      WHEN 'https://github.com/Wunderfitz/jak.git'.
        cl_abap_unit_assert=>assert_equals( exp = 'refs/heads/master' act = is_request_data-branch ).
        cl_abap_unit_assert=>assert_equals( exp = 'TESCHD_JAK' act = is_request_data-package ).
      WHEN 'https://github.com/Wunderfitz/yy.git'.
        cl_abap_unit_assert=>assert_equals( exp = 'refs/heads/master' act = is_request_data-branch ).
        cl_abap_unit_assert=>assert_equals( exp = 'TESCHD_YY' act = is_request_data-package ).
      WHEN OTHERS.
        cl_abap_unit_assert=>fail( msg = |Unexpected Git repository { is_request_data-url } | ).
    ENDCASE.
  ENDMETHOD.

  METHOD lif_abapgit_provider~set_authentication_info.

  ENDMETHOD.

  METHOD lif_abapgit_provider~validate_package.
    CASE iv_package.
      WHEN 'TESCHD_JAK'.
        " Good
      WHEN 'TESCHD_YY'.
        " Good
      WHEN OTHERS.
        cl_abap_unit_assert=>fail( msg = |Unexpected package { iv_package } | ).
    ENDCASE.
  ENDMETHOD.

  METHOD lif_abapgit_provider~validate_transport_request.

    cl_abap_unit_assert=>assert_initial( iv_transport_request ).

  ENDMETHOD.

  METHOD lif_abapgit_provider~is_tr_check_required.
    CLEAR: rv_is_required.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_abapgit_repos_resource DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: request_stub           TYPE REF TO cl_adt_rest_request_stub,
          response_spy           TYPE REF TO cl_adt_rest_response_spy,
          abapgit_repos_resource TYPE REF TO zcl_abapgit_res_repos_test,
          request_data_v2        TYPE zcl_abapgit_res_repos_test=>tt_request_data.
    METHODS:
      setup,
      no_content_type_header FOR TESTING RAISING cx_static_check,
      standard_v2 FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_abapgit_repos_resource IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT me->request_stub.
    CREATE OBJECT me->response_spy.
    CREATE OBJECT me->abapgit_repos_resource.
    me->abapgit_repos_resource->set_abapgit_provider( io_abapgit_provider = NEW ltd_abapgit_provider_default( ) ).
  ENDMETHOD.

  METHOD no_content_type_header.
    me->abapgit_repos_resource->post( EXPORTING request  = me->request_stub
                                                response = me->response_spy ).

    cl_abap_unit_assert=>assert_equals( exp = cl_rest_status_code=>gc_client_error_bad_request act = me->response_spy->get_status( ) ).
  ENDMETHOD.

  METHOD standard_v2.

    me->request_stub->add_header_field( EXPORTING key   = if_http_header_fields=>content_type
                                                  value = zcl_abapgit_res_repos_test=>co_content_type_repo_v2 ).

    me->request_data_v2 = VALUE #( ( url = 'https://github.com/Wunderfitz/jak.git' branch = 'refs/heads/master' package = 'TESCHD_JAK' )
                                   ( url = 'https://github.com/Wunderfitz/yy.git' branch = 'refs/heads/master' package = 'TESCHD_YY' ) ).

    me->request_stub->set_body_data( data = me->request_data_v2 ).

    me->abapgit_repos_resource->post( EXPORTING request  = me->request_stub
                                                response = me->response_spy ).

    cl_abap_unit_assert=>assert_equals( exp = cl_rest_status_code=>gc_success_created act = me->response_spy->get_status( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_simple_transformation_v3 DEFINITION FINAL FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.
  METHODS:
    pos_st_repo_post_seq_ok     FOR TESTING RAISING cx_static_check,
    pos_st_repo_post_seq_not_ok FOR TESTING RAISING cx_static_check,
    pos_st_repo_post_new_field  FOR TESTING RAISING cx_static_check,
    pos_st_repo_post_field_miss FOR TESTING RAISING cx_static_check,
    repo_v3_serialize_ok FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_simple_transformation_v3 IMPLEMENTATION.

  METHOD pos_st_repo_post_seq_ok.
    "Test: Sequence of fields in input XML is equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos_test=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<abapgitrepo:repository xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">'.
    add_xml '  <abapgitrepo:branchName>a</abapgitrepo:branchName>'.
    add_xml '  <abapgitrepo:package>b</abapgitrepo:package>'.
    add_xml '  <abapgitrepo:transportRequest>c</abapgitrepo:transportRequest>'.
    add_xml '  <abapgitrepo:url>d</abapgitrepo:url>'.
    add_xml '  <abapgitrepo:remoteUser>e</abapgitrepo:remoteUser>'.
    add_xml '  <abapgitrepo:remotePassword>f</abapgitrepo:remotePassword>'.
    add_xml '</abapgitrepo:repository>'.

    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos_test=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post_v3
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url              exp = 'd' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'f' ).


  ENDMETHOD.

  METHOD pos_st_repo_post_seq_not_ok.
    "Test: Sequence of fields in input XML is not equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos_test=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<abapgitrepo:repository xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">'.
    add_xml '  <abapgitrepo:branchName>a</abapgitrepo:branchName>'.
    add_xml '  <abapgitrepo:package>b</abapgitrepo:package>'.
    add_xml '  <abapgitrepo:transportRequest>c</abapgitrepo:transportRequest>'.
    add_xml '  <abapgitrepo:remoteUser>e</abapgitrepo:remoteUser>'. "exchanged by url
    add_xml '  <abapgitrepo:url>d</abapgitrepo:url>'. "exchanged by remoteUser
    add_xml '  <abapgitrepo:remotePassword>f</abapgitrepo:remotePassword>'.
    add_xml '</abapgitrepo:repository>'.

    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos_test=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post_v3
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url              exp = 'd' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'f' ).


  ENDMETHOD.

  METHOD pos_st_repo_post_new_field.
    "Test: Input XML contains a new field that is not part of the ABAP structure
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos_test=>ty_request_data.

    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<abapgitrepo:repository xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">'.
    add_xml '  <abapgitrepo:branchName>a</abapgitrepo:branchName>'.
    add_xml '  <abapgitrepo:package>b</abapgitrepo:package>'.
    add_xml '  <abapgitrepo:transportRequest>c</abapgitrepo:transportRequest>'.
    add_xml '  <abapgitrepo:newField>x</abapgitrepo:newField>'.
    add_xml '  <abapgitrepo:url>d</abapgitrepo:url>'.
    add_xml '  <abapgitrepo:remoteUser>e</abapgitrepo:remoteUser>'.
    add_xml '  <abapgitrepo:remotePassword>f</abapgitrepo:remotePassword>'.
    add_xml '</abapgitrepo:repository>'.

    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos_test=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post_v3
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url              exp = 'd' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'f' ).


  ENDMETHOD.

  METHOD pos_st_repo_post_field_miss.
    "Test: A field that is part of the ABAP structure is missing in the input XML
    "Result: Transformation should succeed, missing field should be initial

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos_test=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.
    add_xml '<?xml version="1.0" encoding="utf-8"?>'.
    add_xml '<abapgitrepo:repository xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">'.
    add_xml '  <abapgitrepo:branchName>a</abapgitrepo:branchName>'.
    add_xml '  <abapgitrepo:package>b</abapgitrepo:package>'.
    add_xml '  <abapgitrepo:transportRequest>c</abapgitrepo:transportRequest>'.
   "add_xml '  <abapgitrepo:url>d</abapgitrepo:url>'.  url is missing
    add_xml '  <abapgitrepo:remoteUser>e</abapgitrepo:remoteUser>'.
    add_xml '  <abapgitrepo:remotePassword>f</abapgitrepo:remotePassword>'.
    add_xml '</abapgitrepo:repository>'.

    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos_test=>co_root_name_post.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post_v3
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_initial( act = ls_data-url ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password         exp = 'f' ).


  ENDMETHOD.

  METHOD repo_v3_serialize_ok.
    DATA xml_data_output TYPE string.
    DATA repository_data TYPE zcl_abapgit_res_repos_test=>ty_request_data.

    repository_data-url = 'https://github.com/Wunderfitz/yy.git'.
    repository_data-branch = 'refs/heads/master'.
    repository_data-package = 'TEST_YY'.


    CALL TRANSFORMATION abapgit_st_repo_post_v3
      SOURCE repository =  repository_data
      RESULT XML xml_data_output.

    DATA lv_xml TYPE string.

    add_xml '<?xml version="1.0" encoding="utf-16"?>'.
    add_xml '<abapgitrepo:repository xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">'.
    add_xml '<abapgitrepo:branchName>refs/heads/master</abapgitrepo:branchName>'.
    add_xml '<abapgitrepo:package>TEST_YY</abapgitrepo:package>'.
    add_xml '<abapgitrepo:transportRequest/>'.
    add_xml '<abapgitrepo:url>https://github.com/Wunderfitz/yy.git</abapgitrepo:url>'.
    add_xml '<abapgitrepo:remoteUser/>'.
    add_xml '<abapgitrepo:remotePassword/>'.
    add_xml '</abapgitrepo:repository>'.


   DATA xml_data_input TYPE string.
   CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML xml_data_input.

   CALL TRANSFORMATION id SOURCE XML xml_data_output RESULT XML xml_data_output.

   cl_abap_unit_assert=>assert_equals( exp = xml_data_input act = xml_data_output ).

  ENDMETHOD.


ENDCLASS.

CLASS ltcl_simple_transformation_v4 DEFINITION FINAL FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.
  METHODS:
    pos_st_repo_post_seq_ok FOR TESTING RAISING cx_static_check,
    repo_v4_serialize_ok    FOR TESTING RAISING cx_static_check,
    pos_st_repo_post_seq_not_ok FOR TESTING RAISING cx_static_check,
    pos_st_repo_post_new_field FOR TESTING RAISING cx_static_check,
    pos_st_repo_post_field_miss FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_simple_transformation_v4 IMPLEMENTATION.

  METHOD pos_st_repo_post_seq_ok.
    "Test: Sequence of fields in input XML is equal to fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos_test=>tt_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.

    add_xml '<?xml version="1.0" encoding="utf-16"?><abapgitrepo:repositories xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">'.
    add_xml '  <abapgitrepo:repository>'.
    add_xml '    <abapgitrepo:branchName>a</abapgitrepo:branchName>'.
    add_xml '    <abapgitrepo:package>b</abapgitrepo:package>'.
    add_xml '    <abapgitrepo:transportRequest>c</abapgitrepo:transportRequest>'.
    add_xml '    <abapgitrepo:url>d</abapgitrepo:url>'.
    add_xml '    <abapgitrepo:remoteUser>e</abapgitrepo:remoteUser>'.
    add_xml '    <abapgitrepo:remotePassword>f</abapgitrepo:remotePassword>'.
    add_xml '  </abapgitrepo:repository>'.
    add_xml '  <abapgitrepo:repository>'.
    add_xml '    <abapgitrepo:branchName>a2</abapgitrepo:branchName>'.
    add_xml '    <abapgitrepo:package>b2</abapgitrepo:package>'.
    add_xml '    <abapgitrepo:transportRequest>c2</abapgitrepo:transportRequest>'.
    add_xml '    <abapgitrepo:url>d2</abapgitrepo:url>'.
    add_xml '    <abapgitrepo:remoteUser>e2</abapgitrepo:remoteUser>'.
    add_xml '    <abapgitrepo:remotePassword>f2</abapgitrepo:remotePassword>'.
    add_xml '  </abapgitrepo:repository>'.
    add_xml '</abapgitrepo:repositories>'.


    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos_test=>co_root_name_post_v2.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post_v4
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_not_initial( ls_data ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( ls_data ) ).

    DATA lv_first_repository TYPE zcl_abapgit_res_repos_test=>ty_request_data.

    READ TABLE ls_data INTO lv_first_repository INDEX 1.

    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-url              exp = 'd' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-password         exp = 'f' ).

    READ TABLE ls_data INTO lv_first_repository INDEX 2.

    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-branch           exp = 'a2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-package          exp = 'b2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-transportrequest exp = 'c2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-url              exp = 'd2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-user             exp = 'e2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-password         exp = 'f2' ).


  ENDMETHOD.

  METHOD pos_st_repo_post_seq_not_ok.
    "Test: Sequence of fields in input XML is not equal to sequence of fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos_test=>tt_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.

    add_xml '<?xml version="1.0" encoding="utf-16"?><abapgitrepo:repositories xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">'.
    add_xml '  <abapgitrepo:repository>'.
    add_xml '    <abapgitrepo:branchName>a</abapgitrepo:branchName>'.
    add_xml '    <abapgitrepo:transportRequest>c</abapgitrepo:transportRequest>'. "Exchanged with package
    add_xml '    <abapgitrepo:package>b</abapgitrepo:package>'. "Exchanged with transportRequest
    add_xml '    <abapgitrepo:url>d</abapgitrepo:url>'.
    add_xml '    <abapgitrepo:remoteUser>e</abapgitrepo:remoteUser>'.
    add_xml '    <abapgitrepo:remotePassword>f</abapgitrepo:remotePassword>'.
    add_xml '  </abapgitrepo:repository>'.
    add_xml '  <abapgitrepo:repository>'.
    add_xml '    <abapgitrepo:branchName>a2</abapgitrepo:branchName>'.
    add_xml '    <abapgitrepo:transportRequest>c2</abapgitrepo:transportRequest>'.
    add_xml '    <abapgitrepo:url>d2</abapgitrepo:url>'.
    add_xml '    <abapgitrepo:remoteUser>e2</abapgitrepo:remoteUser>'. "Exchanged with package
    add_xml '    <abapgitrepo:package>b2</abapgitrepo:package>'. "Exchanged with remoteUser
    add_xml '    <abapgitrepo:remotePassword>f2</abapgitrepo:remotePassword>'.
    add_xml '  </abapgitrepo:repository>'.
    add_xml '</abapgitrepo:repositories>'.


    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos_test=>co_root_name_post_v2.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post_v4
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_not_initial( ls_data ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( ls_data ) ).

    DATA lv_first_repository TYPE zcl_abapgit_res_repos_test=>ty_request_data.

    READ TABLE ls_data INTO lv_first_repository INDEX 1.

    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-url              exp = 'd' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-password         exp = 'f' ).

    READ TABLE ls_data INTO lv_first_repository INDEX 2.

    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-branch           exp = 'a2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-package          exp = 'b2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-transportrequest exp = 'c2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-url              exp = 'd2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-user             exp = 'e2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-password         exp = 'f2' ).



  ENDMETHOD.

  METHOD pos_st_repo_post_new_field.
    "Test: Input XML has a new field which does not exist in the fields operated by simple transformation
    "Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos_test=>tt_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.

    add_xml '<?xml version="1.0" encoding="utf-16"?><abapgitrepo:repositories xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">'.
    add_xml '  <abapgitrepo:repository>'.
    add_xml '    <abapgitrepo:branchName>a</abapgitrepo:branchName>'.
    add_xml '    <abapgitrepo:package>b</abapgitrepo:package>'.
    add_xml '    <abapgitrepo:transportRequest>c</abapgitrepo:transportRequest>'.
    add_xml '    <abapgitrepo:url>d</abapgitrepo:url>'.
    add_xml '    <abapgitrepo:newField>x</abapgitrepo:newField>'.
    add_xml '    <abapgitrepo:remoteUser>e</abapgitrepo:remoteUser>'.
    add_xml '    <abapgitrepo:remotePassword>f</abapgitrepo:remotePassword>'.
    add_xml '  </abapgitrepo:repository>'.
    add_xml '  <abapgitrepo:repository>'.
    add_xml '    <abapgitrepo:branchName>a2</abapgitrepo:branchName>'.
    add_xml '    <abapgitrepo:package>b2</abapgitrepo:package>'.
    add_xml '    <abapgitrepo:transportRequest>c2</abapgitrepo:transportRequest>'.
    add_xml '    <abapgitrepo:url>d2</abapgitrepo:url>'.
    add_xml '    <abapgitrepo:remoteUser>e2</abapgitrepo:remoteUser>'.
    add_xml '    <abapgitrepo:remotePassword>f2</abapgitrepo:remotePassword>'.
    add_xml '    <abapgitrepo:newField>x</abapgitrepo:newField>'.
    add_xml '  </abapgitrepo:repository>'.
    add_xml '</abapgitrepo:repositories>'.


    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos_test=>co_root_name_post_v2.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post_v4
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_not_initial( ls_data ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( ls_data ) ).

    DATA lv_first_repository TYPE zcl_abapgit_res_repos_test=>ty_request_data.

    READ TABLE ls_data INTO lv_first_repository INDEX 1.

    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-url              exp = 'd' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-user             exp = 'e' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-password         exp = 'f' ).

    READ TABLE ls_data INTO lv_first_repository INDEX 2.

    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-branch           exp = 'a2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-package          exp = 'b2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-transportrequest exp = 'c2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-url              exp = 'd2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-user             exp = 'e2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-password         exp = 'f2' ).


  ENDMETHOD.

  METHOD pos_st_repo_post_field_miss.
    "Test: Input XML is missing a field which exists in the fields operated by simple transformation
    "Result: Transformation should succeed and the missing field should be initial

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE zcl_abapgit_res_repos_test=>tt_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.
    FIELD-SYMBOLS <lv_value> TYPE any.

    DATA lv_xml TYPE string.

    add_xml '<?xml version="1.0" encoding="utf-16"?><abapgitrepo:repositories xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">'.
    add_xml '  <abapgitrepo:repository>'.
    add_xml '    <abapgitrepo:branchName>a</abapgitrepo:branchName>'.
    add_xml '    <abapgitrepo:package>b</abapgitrepo:package>'.
    add_xml '    <abapgitrepo:transportRequest>c</abapgitrepo:transportRequest>'.
    add_xml '    <abapgitrepo:url>d</abapgitrepo:url>'.
    add_xml '    <abapgitrepo:remotePassword>f</abapgitrepo:remotePassword>'.
    add_xml '  </abapgitrepo:repository>'.
    add_xml '  <abapgitrepo:repository>'.
    add_xml '    <abapgitrepo:branchName>a2</abapgitrepo:branchName>'.
    add_xml '    <abapgitrepo:package>b2</abapgitrepo:package>'.
    add_xml '    <abapgitrepo:transportRequest>c2</abapgitrepo:transportRequest>'.
    add_xml '    <abapgitrepo:url>d2</abapgitrepo:url>'.
    add_xml '    <abapgitrepo:remoteUser>e2</abapgitrepo:remoteUser>'.
    add_xml '  </abapgitrepo:repository>'.
    add_xml '</abapgitrepo:repositories>'.


    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = zcl_abapgit_res_repos_test=>co_root_name_post_v2.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_post_v4
      SOURCE XML lv_input_xml
      RESULT     (lt_result).

    cl_abap_unit_assert=>assert_not_initial( ls_data ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( ls_data ) ).

    DATA lv_first_repository TYPE zcl_abapgit_res_repos_test=>ty_request_data.

    READ TABLE ls_data INTO lv_first_repository INDEX 1.

    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-branch           exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-package          exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-transportrequest exp = 'c' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-url              exp = 'd' ).
    cl_abap_unit_assert=>assert_initial( lv_first_repository-user ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-password         exp = 'f' ).

    READ TABLE ls_data INTO lv_first_repository INDEX 2.

    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-branch           exp = 'a2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-package          exp = 'b2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-transportrequest exp = 'c2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-url              exp = 'd2' ).
    cl_abap_unit_assert=>assert_equals( act = lv_first_repository-user             exp = 'e2' ).
    cl_abap_unit_assert=>assert_initial( lv_first_repository-password ).


  ENDMETHOD.


  METHOD repo_v4_serialize_ok.
    DATA: repositories_data TYPE zcl_abapgit_res_repos_test=>tt_request_data,
          xml_data        TYPE string.

    DATA repository_data TYPE zcl_abapgit_res_repos_test=>ty_request_data.

    repository_data-url = 'url_1'.
    repository_data-branch = 'refs/heads/master'.
    repository_data-package = 'package_1'.

    INSERT repository_data INTO TABLE repositories_data.

    repository_data-url = 'url_2'.
    repository_data-branch = 'refs/heads/master'.
    repository_data-package = 'package_2'.

    INSERT repository_data INTO TABLE repositories_data.

    CALL TRANSFORMATION abapgit_st_repo_post_v4
      SOURCE repositories =  repositories_data
      RESULT XML xml_data.

    DATA lv_xml TYPE string.

    add_xml '<?xml version="1.0" encoding="utf-16"?><abapgitrepo:repositories xmlns:abapgitrepo="http://www.sap.com/adt/abapgit/repositories">'.
    add_xml '<abapgitrepo:repository>'.
    add_xml '<abapgitrepo:branchName>refs/heads/master</abapgitrepo:branchName>'.
    add_xml '<abapgitrepo:package>package_1</abapgitrepo:package>'.
    add_xml '<abapgitrepo:transportRequest/>'.
    add_xml '<abapgitrepo:url>url_1</abapgitrepo:url>'.
    add_xml '<abapgitrepo:remoteUser/>'.
    add_xml '<abapgitrepo:remotePassword/>'.
    add_xml '</abapgitrepo:repository>'.
    add_xml '<abapgitrepo:repository>'.
    add_xml '<abapgitrepo:branchName>refs/heads/master</abapgitrepo:branchName>'.
    add_xml '<abapgitrepo:package>package_2</abapgitrepo:package>'.
    add_xml '<abapgitrepo:transportRequest/>'.
    add_xml '<abapgitrepo:url>url_2</abapgitrepo:url>'.
    add_xml '<abapgitrepo:remoteUser/>'.
    add_xml '<abapgitrepo:remotePassword/>'.
    add_xml '</abapgitrepo:repository>'.
    add_xml '</abapgitrepo:repositories>'.


   DATA xml_data_input TYPE string.
   CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML xml_data_input.

   CALL TRANSFORMATION id SOURCE XML xml_data RESULT XML xml_data.

   cl_abap_unit_assert=>assert_equals( exp = xml_data_input act = xml_data ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_test_get_method_v2 DEFINITION FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.
METHODS:
  test_get_method FOR TESTING RAISING cx_static_check.

CLASS-METHODS:
  class_setup RAISING cx_static_check.

CLASS-DATA:
  request_stub TYPE REF TO cl_adt_rest_request_stub,
  response_spy TYPE REF TO cl_adt_rest_response_spy,
  f_cut        TYPE REF TO zcl_abapgit_res_repos_test.

ENDCLASS.

CLASS zcl_abapgit_res_repos_test DEFINITION LOCAL FRIENDS ltcl_test_get_method_v2.

CLASS ltcl_test_get_method_v2 IMPLEMENTATION.

  METHOD test_get_method.
    request_stub->add_header_field( key   = if_http_header_fields=>accept
                                    value = zcl_abapgit_res_repos_test=>co_content_type_repos_v2 ).

    "Create Test Double
    DATA lv_repo TYPE REF TO if_abapgit_persist_repo.
    lv_repo ?= cl_abap_testdouble=>create( 'if_abapgit_persist_repo' ).

    "Create return data
    DATA rt_repos TYPE if_abapgit_persistence=>tt_repo.
    rt_repos = VALUE #( ( url         = 'test_url'
                          key         = 'dummy_key'
                          branch_name = 'dummy_branch'
                          package     = 'dummy_package'
                          created_by  = 'dummy_user'
                          created_at  = '20201001'
                          app_log_key = 'dummy_log_key') ).

    "configure call
     cl_abap_testdouble=>configure_call( lv_repo  )->returning( rt_repos ).

     "which call
     lv_repo->list( iv_with_status = abap_true ).

     "inject test double
     f_cut->li_repo = lv_repo.

     "Call method under test
     f_cut->get( request  = request_stub
                 response = response_spy ).

     "Check and Assert
     DATA lt_repo_w_links TYPE zcl_abapgit_res_repos_test=>tt_repo_w_links.
     response_spy->get_body_data( IMPORTING data = lt_repo_w_links ).

     DATA ls_repo_w_links TYPE zcl_abapgit_res_repos_test=>ty_repo_w_links.
     READ TABLE lt_repo_w_links INTO ls_repo_w_links INDEX 1.

     cl_abap_unit_assert=>assert_equals( act = ls_repo_w_links-key exp = 'dummy_key' ).
     cl_abap_unit_assert=>assert_equals( act = ls_repo_w_links-url exp = 'test_url' ).
     cl_abap_unit_assert=>assert_equals( act = ls_repo_w_links-created_by exp = 'dummy_user' ).
     cl_abap_unit_assert=>assert_initial( ls_repo_w_links-deserialized_by ).

     DATA lt_repo_links TYPE if_atom_types=>link_t.
     lt_repo_links = ls_repo_w_links-links.
     DESCRIBE TABLE lt_repo_links LINES DATA(count).

     cl_abap_unit_assert=>assert_equals( act = count exp = 6 ).

  ENDMETHOD.

  METHOD class_setup.
    CREATE OBJECT request_stub.
    CREATE OBJECT response_spy.
    CREATE OBJECT f_cut.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_test_post_method_v3 DEFINITION FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.
METHODS:
  test_post_method FOR TESTING RAISING cx_static_check.

CLASS-METHODS:
  class_setup RAISING cx_static_check.

CLASS-DATA:
  request_stub TYPE REF TO cl_adt_rest_request_stub,
  response_spy TYPE REF TO cl_adt_rest_response_spy,
  f_cut        TYPE REF TO zcl_abapgit_res_repos_test.

ENDCLASS.

CLASS zcl_abapgit_res_repos_test DEFINITION LOCAL FRIENDS ltcl_test_post_method_v3.

CLASS ltcl_test_post_method_v3 IMPLEMENTATION.

  METHOD test_post_method.
    request_stub->add_header_field( key   = if_http_header_fields=>content_type
                                    value = zcl_abapgit_res_repos_test=>co_content_type_repo_v3 ).

    "Prepare request data
    DATA ls_request_data TYPE zcl_abapgit_res_repos_test=>ty_request_data.
    ls_request_data = VALUE #( url     = 'https://github.com/Wunderfitz/jak.git'
                               branch  = 'refs/heads/master'
                               package = 'TESCHD_JAK').

    request_stub->set_body_data( data = ls_request_data ).

    "Set default abapgit_provider implementation
    f_cut->set_abapgit_provider( io_abapgit_provider = NEW ltd_abapgit_provider_default(  ) ).

    "Call method
    f_cut->post( request  = request_stub
                 response = response_spy ).

    "assert results
    cl_abap_unit_assert=>assert_equals( exp = cl_rest_status_code=>gc_success_ok
                                        act = response_spy->get_status(  ) ).

  ENDMETHOD.

  METHOD class_setup.
    CREATE OBJECT request_stub.
    CREATE OBJECT response_spy.
    CREATE OBJECT f_cut.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_test_post_method_v4 DEFINITION FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.

PRIVATE SECTION.
METHODS:
  test_post_method FOR TESTING RAISING cx_static_check.

CLASS-METHODS:
  class_setup RAISING cx_static_check.

CLASS-DATA:
  request_stub TYPE REF TO cl_adt_rest_request_stub,
  response_spy TYPE REF TO cl_adt_rest_response_spy,
  f_cut        TYPE REF TO zcl_abapgit_res_repos_test.

ENDCLASS.

CLASS zcl_abapgit_res_repos_test DEFINITION LOCAL FRIENDS ltcl_test_post_method_v4.

CLASS ltcl_test_post_method_v4 IMPLEMENTATION.

  METHOD test_post_method.
    request_stub->add_header_field( key   = if_http_header_fields=>content_type
                                    value = zcl_abapgit_res_repos_test=>co_content_type_repo_v4 ).

    "Prepare request data
    DATA lt_request_data TYPE zcl_abapgit_res_repos_test=>tt_request_data.
    lt_request_data = VALUE #( ( url     = 'https://github.com/Wunderfitz/jak.git'
                                 branch  = 'refs/heads/master'
                                 package = 'TESCHD_JAK')
                               ( url     = 'https://github.com/Wunderfitz/yy.git'
                                 branch  = 'refs/heads/master'
                                 package = 'TESCHD_YY') ).

    request_stub->set_body_data( data = lt_request_data ).

    "Set default abapgit_provider implementation
    f_cut->set_abapgit_provider( io_abapgit_provider = NEW ltd_abapgit_provider_default(  ) ).

    "Call method
    f_cut->post( request  = request_stub
                 response = response_spy ).

    "assert results
    cl_abap_unit_assert=>assert_equals( exp = cl_rest_status_code=>gc_success_created
                                        act = response_spy->get_status(  ) ).

  ENDMETHOD.

  METHOD class_setup.
    CREATE OBJECT request_stub.
    CREATE OBJECT response_spy.
    CREATE OBJECT f_cut.
  ENDMETHOD.

ENDCLASS.
