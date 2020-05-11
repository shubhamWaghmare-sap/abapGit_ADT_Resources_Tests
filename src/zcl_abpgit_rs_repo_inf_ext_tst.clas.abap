CLASS zcl_abpgit_rs_repo_inf_ext_tst DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_request_data,
        url      TYPE string,
        user     TYPE string,
        password TYPE string,
      END OF ty_request_data.
    TYPES:
      BEGIN OF ty_response_data,
        access_mode TYPE string,
        branches    TYPE if_abapgit_definitions=>ty_git_branch_list_tt,
      END OF ty_response_data.


    CONSTANTS co_content_type_request_v1  TYPE string VALUE 'application/abapgit.adt.repo.info.ext.request.v1+xml' ##NO_TEXT.
    CONSTANTS co_content_type_response_v1 TYPE string VALUE 'application/abapgit.adt.repo.info.ext.response.v1+xml' ##NO_TEXT.
    CONSTANTS co_root_name_request        TYPE string VALUE 'REPOSITORY_EXTERNAL_REQ' ##NO_TEXT.
    CONSTANTS co_st_name_request          TYPE string VALUE 'ABAPGIT_ST_REPO_INFO_EXT_REQ' ##NO_TEXT.
    CONSTANTS co_st_name_response         TYPE string VALUE 'ABAPGIT_ST_REPO_INFO_EXT_RES' ##NO_TEXT.
    CONSTANTS co_root_name_response       TYPE string VALUE 'REPOSITORY_EXTERNAL' ##NO_TEXT.
    CONSTANTS co_content_type_request_v2  TYPE string VALUE 'application/abapgit.adt.repo.info.ext.request.v2+xml' ##NO_TEXT.
    CONSTANTS co_content_type_response_v2 TYPE string VALUE 'application/abapgit.adt.repo.info.ext.response.v2+xml' ##NO_TEXT.
    CONSTANTS co_st_name_request_v2       TYPE string VALUE 'ABAPGIT_ST_REPO_INFO_EXT_RQ_V2' ##NO_TEXT.
    CONSTANTS co_st_name_response_v2      TYPE string VALUE 'ABAPGIT_ST_REPO_INFO_EXT_RS_V2' ##NO_TEXT.

    METHODS post REDEFINITION.
    METHODS constructor.


  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
         lv_abapgit_http       TYPE REF TO cl_abapgit_http,
         lv_abapgit_auth_helper        TYPE REF TO zcl_abapgit_auth_check_helper,
         lo_branch_list        TYPE REF TO cl_abapgit_git_branch_list.

    METHODS:
         set_abapgit_http_instance IMPORTING io_abapgit_http TYPE REF TO cl_abapgit_http.
ENDCLASS.



CLASS ZCL_ABPGIT_RS_REPO_INF_EXT_TST IMPLEMENTATION.


  METHOD post.

    DATA:
      ls_request_data  TYPE ty_request_data,
      ls_response_data TYPE ty_response_data.

    DATA(ls_requested_content_type) = request->get_inner_rest_request( )->get_header_field( iv_name = if_http_header_fields=>content_type ).

    "TODO:  remove case co_content_type_request_v1 after 2005 release
    "     case co_content_type_request_v2 to handle externalRepositoryInfo in emf model for abapGit Repositories view.
    CASE ls_requested_content_type.

      WHEN co_content_type_request_v1.
          "Request Content Handler
          "Wrap the request content handler in a cl_adt_rest_comp_cnt_handler in order to ensure that the client sends a correct 'Content-Type:' header
           DATA(lo_request_content_handler) = cl_adt_rest_comp_cnt_handler=>create(
                request         = request
                content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                            st_name      = co_st_name_request
                            root_name    = co_root_name_request
                            content_type = co_content_type_request_v1 ) ).

          "Response Content Handler
           DATA(lo_response_content_handler) = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                st_name      = co_st_name_response
                root_name    = co_root_name_response
                content_type = co_content_type_response_v1 ).

      WHEN co_content_type_request_v2.
          "Request Content Handler
          "Wrap the request content handler in a cl_adt_rest_comp_cnt_handler in order to ensure that the client sends a correct 'Content-Type:' header
           lo_request_content_handler = cl_adt_rest_comp_cnt_handler=>create(
                request         = request
                content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                            st_name      = co_st_name_request_v2
                            root_name    = co_root_name_request
                            content_type = co_content_type_request_v2 ) ).

          "Response Content Handler
           lo_response_content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                st_name      = co_st_name_response_v2
                root_name    = co_root_name_response
                content_type = co_content_type_response_v2 ).

      WHEN OTHERS.
           response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      ENDCASE.


    "Validation of request 'Accept:' header
    cl_adt_rest_comp_cnt_handler=>create( request = request content_handler = lo_response_content_handler )->check_cnt_type_is_supported( ).

    "Retrieve request data
    request->get_body_data(
      EXPORTING
        content_handler = lo_request_content_handler
      IMPORTING
        data            = ls_request_data ).

    TRY.
        "check for user/password information in passed URL and filter it
        IF cl_abapgit_url=>has_credentials( |{ ls_request_data-url }| ).
          ls_request_data-url = cl_abapgit_url=>decomposed_url( ls_request_data-url ).
          IF ls_request_data-user IS INITIAL AND ls_request_data-password IS INITIAL.
            ls_request_data-user = cl_abapgit_url=>user( ls_request_data-url ).
            ls_request_data-password = cl_abapgit_url=>password( ls_request_data-url ).
          ENDIF.
        ENDIF.

        "Set logon information if supplied
        IF ls_request_data-user     IS NOT INITIAL AND
           ls_request_data-password IS NOT INITIAL.
          cl_abapgit_default_auth_info=>refresh( ).
          cl_abapgit_default_auth_info=>set_auth_info( iv_user     = ls_request_data-user
                                                       iv_password = ls_request_data-password ).
        ENDIF.

        "Check whether passed repo URL has public or privated access

        "ls_response_data-access_mode = cl_abapgit_http=>determine_access_level( ls_request_data-url ).
        IF lv_abapgit_auth_helper IS INITIAL.
           lv_abapgit_auth_helper = NEW zcl_abapgit_auth_check_helper(  ).
        ENDIF.

        ls_response_data-access_mode = lv_abapgit_auth_helper->determine_access_level( ls_request_data-url ).

        "Check whether two factor authentication is enabled
          IF ls_request_data-user IS NOT INITIAL AND ls_request_data-password IS NOT INITIAL.
            IF lv_abapgit_auth_helper IS INITIAL.
              lv_abapgit_auth_helper = NEW zcl_abapgit_auth_check_helper(  ).
            ENDIF.

            " Is 2fa enabled for this account?
            IF lv_abapgit_auth_helper->is_2fa_required( iv_url      = ls_request_data-url
                                                        iv_username = ls_request_data-user
                                                        iv_password = ls_request_data-password ) = abap_true.
              cx_abapgit_exception=>raise( '2FA required' ).
            ENDIF.
          ENDIF.

        "Retrieve list of branches for repo
        IF  ls_response_data-access_mode = 'PUBLIC'  OR
          ( ls_response_data-access_mode = 'PRIVATE' AND
            ls_request_data-user         IS NOT INITIAL AND
            ls_request_data-password     IS NOT INITIAL ).

            IF lo_branch_list IS INITIAL.
              lo_branch_list = cl_abapgit_git_transport=>branches( ls_request_data-url ).
            ENDIF.

          DATA(lt_branches_input) = lo_branch_list->get_branches_only( ).
          APPEND LINES OF lt_branches_input TO ls_response_data-branches.
        ENDIF.

        "Prepare Response
        response->set_body_data( content_handler = lo_response_content_handler data = ls_response_data ).
        response->set_status( cl_rest_status_code=>gc_success_ok ).

      CATCH cx_abapgit_exception INTO DATA(lx_abapgit_exception).
        cx_adt_rest_abapgit=>raise_with_error( lx_abapgit_exception ).
      CATCH cx_abapgit_2fa_comm_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD SET_ABAPGIT_HTTP_INSTANCE.
    lv_abapgit_http = io_abapgit_http.
  ENDMETHOD.

  METHOD CONSTRUCTOR.

    SUPER->CONSTRUCTOR( ).
    CREATE OBJECT lv_abapgit_http TYPE cl_abapgit_http.

  ENDMETHOD.

ENDCLASS.
