CLASS zcl_abapgit_res_repo_pull_test DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF  ty_s_result,
        reason  TYPE string,
        message TYPE string,
      END OF ty_s_result.
    TYPES:
      BEGIN OF ty_request_pull_data,
        branch           TYPE string,
        transportrequest TYPE string,
        user             TYPE string,
        password         TYPE string,
      END OF ty_request_pull_data.

    TYPES: BEGIN OF ty_repo_w_links.
             INCLUDE  TYPE if_abapgit_persistence=>ty_repo.
    TYPES:   links TYPE if_atom_types=>link_t.
    TYPES: END OF ty_repo_w_links.
    TYPES:
      tt_repo_w_links TYPE STANDARD TABLE OF ty_repo_w_links WITH DEFAULT KEY.

    CONSTANTS co_class_name             TYPE seoclsname VALUE 'CL_ABAPGIT_RES_REPOS' ##NO_TEXT.
    CONSTANTS co_resource_type          TYPE string     VALUE 'REPOS' ##NO_TEXT.             "EC NOTEXT
    CONSTANTS co_st_name_pull           TYPE string     VALUE 'ABAPGIT_ST_REPO_PULL' ##NO_TEXT.
    CONSTANTS co_st_name_post_res       TYPE string     VALUE 'ABAPGIT_ST_REPO_POST_RES'.
    CONSTANTS co_root_name_pull         TYPE string     VALUE 'REPOSITORY' ##NO_TEXT.
    CONSTANTS co_root_name_post_res     TYPE string     VALUE 'OBJECTS'.
    CONSTANTS co_content_type_repo_v1   TYPE string     VALUE 'application/abapgit.adt.repo.v1+xml' ##NO_TEXT.
    CONSTANTS co_content_type_repos_v1  TYPE string     VALUE 'application/abapgit.adt.repos.v1+xml' ##NO_TEXT.
    CONSTANTS co_content_type_object_v1 TYPE string     VALUE 'application/abapgit.adt.repo.object.v1+xml' ##NO_TEXT.
    CONSTANTS co_st_name_get            TYPE string     VALUE 'ABAPGIT_ST_REPOS' ##NO_TEXT.
    CONSTANTS co_root_name_get          TYPE string     VALUE 'REPOSITORIES' ##NO_TEXT.
    CONSTANTS co_content_type_repo_v2   TYPE string     VALUE 'application/abapgit.adt.repo.v2+xml' ##NO_TEXT.
    CONSTANTS co_st_name_pull_v2        TYPE string     VALUE 'ABAPGIT_ST_REPO_PULL_V2' ##NO_TEXT.
    CONSTANTS co_content_type_repo_v3   TYPE string     VALUE 'application/abapgit.adt.repo.v3+xml' ##NO_TEXT.
    CONSTANTS co_content_type_repos_v2  TYPE string     VALUE 'application/abapgit.adt.repos.v2+xml' ##NO_TEXT.

    METHODS post REDEFINITION.
    METHODS get  REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS validate_request_data
      IMPORTING
        !is_request_data TYPE ty_request_pull_data
      RAISING
        cx_abapgit_exception .

ENDCLASS.



CLASS ZCL_ABAPGIT_RES_REPO_PULL_TEST IMPLEMENTATION.


  METHOD get.

    DATA(ls_requested_content_type) = request->get_inner_rest_request( )->get_header_field( iv_name = if_http_header_fields=>content_type ).

    "TODO:  remove case co_content_type_repos_v1 after 2005 release
    "     case co_content_type_repos_v2 to handle pull for emf model of abapGit Repositories view.

    CASE ls_requested_content_type.
      WHEN co_content_type_repos_v1.
        DATA(lo_resp_content_handler) = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text(
                                                                                   content_type      = co_content_type_repos_v1
                                                                                   strict_conversion = abap_true
                                                                                 ).
      WHEN co_content_type_repos_v2.
        lo_resp_content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text(
                                                                                   content_type      = co_content_type_repos_v2
                                                                                   strict_conversion = abap_true
                                                                                 ).

      WHEN OTHERS.
        response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).

    ENDCASE.



    "validation of request 'Accept:' header
    cl_adt_rest_comp_cnt_handler=>create( request = request content_handler = lo_resp_content_handler )->check_cnt_type_is_supported( ).

    TRY.
        response->set_body_data(
                  content_handler = lo_resp_content_handler
                  data            = |DEMO STATUS| ).

      CATCH cx_st_error cx_abapgit_exception INTO DATA(lx_error).
        cx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_error
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.


  METHOD post.

    DATA:
      ls_request_data TYPE ty_request_pull_data,
      result_request  TYPE sadt_status_message,
      lv_repo_key     TYPE if_abapgit_persistence=>ty_value.

    TRY.
        "Get Repository Key
        request->get_uri_attribute( EXPORTING name = 'key' mandatory = abap_true
                                    IMPORTING value = lv_repo_key ).

        "TODO:  remove case co_content_type_repos_v1 after 2005 release
        "     case co_content_type_repo_v3 to handle pull request for emf model of abapGit Repositories view.

        DATA(ls_requested_content_type) = request->get_inner_rest_request( )->get_header_field( iv_name = if_http_header_fields=>content_type ).

        CASE ls_requested_content_type.
          WHEN co_content_type_repo_v1.
            DATA(lo_request_content_handler) = cl_adt_rest_comp_cnt_handler=>create(
                request         = request
                content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                                      st_name      = co_st_name_pull
                                      root_name    = co_root_name_pull
                                      content_type = co_content_type_repo_v1 ) ).

          WHEN co_content_type_repo_v3.
            lo_request_content_handler = cl_adt_rest_comp_cnt_handler=>create(
                request         = request
                content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                                      st_name      = co_st_name_pull_v2
                                      root_name    = co_root_name_pull
                                      content_type = co_content_type_repo_v3 ) ).

          WHEN OTHERS.
            response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
        ENDCASE.


        "Retrieve request data
        request->get_body_data(
          EXPORTING
            content_handler = lo_request_content_handler
          IMPORTING
            data            = ls_request_data ).



        "Prerequisite check for request values
        "validate_request_data( ls_request_data ).

        GET PARAMETER ID 'A4C_AGIT_PULL_SYNC' FIELD DATA(lv_pull_sync).
        IF lv_pull_sync <> 'X'.
          "[A4C_AGIT] START asynchronous/background processing ------------------

          DATA lo_job_scheduler TYPE REF TO if_cbo_job_scheduler.
          DATA lo_job_action    TYPE REF TO if_cbo_job_action.

*-------- check if a different action is still running
          DATA(ls_repo) = cl_abapgit_persist_factory=>get_repo( )->read( iv_key = lv_repo_key iv_with_status = abap_true ).
          IF ls_repo-status = if_abapgit_app_log=>c_run_status-running.
            CASE ls_repo-action.
              WHEN if_abapgit_app_log=>c_action_push.
                response->set_body_data( content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text( )
                                         data            = |Another Push is currently running| ) .
              WHEN if_abapgit_app_log=>c_action_pull.
                response->set_body_data( content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text( )
                                         data            = |Another Pull is currently running| ) .
              WHEN OTHERS.
                cx_abapgit_exception=>raise( 'Unknown Action Type' ).
            ENDCASE.
            response->set_status( cl_rest_status_code=>gc_client_error_conflict ). "409
            EXIT.
          ELSEIF ls_repo-status = if_abapgit_app_log=>c_run_status-initial.
            CASE ls_repo-action.
              WHEN if_abapgit_app_log=>c_action_push.
                response->set_body_data( content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text( )
                     data            = |Another Push action is waiting to be executed| ) .
              WHEN if_abapgit_app_log=>c_action_pull.
                response->set_body_data( content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text( )
                     data            = |Another Pull action is waiting to be executed| ) .
              WHEN OTHERS.
                cx_abapgit_exception=>raise( 'Unknown Action Type' ).
            ENDCASE.
            response->set_status( cl_rest_status_code=>gc_client_error_conflict ). "409
            EXIT.
          ENDIF.

          "Create new log in history table before starting batch processing
          DATA(lo_log_factory) = cl_abapgit_app_log_factory=>get_instance( ).


          DATA(lo_log) = lo_log_factory->create_new( iv_repo_key    = lv_repo_key
                                            iv_repo_branch = ls_request_data-branch
                                            iv_repo_action = if_abapgit_app_log=>c_action_pull ).
          lo_log->save( ).

          DATA ls_alog_key TYPE tsa4c_agit_applog_key.
          ls_alog_key-app_log = lo_log->get_data( )-app_log.

          "Execute background job
          lo_job_scheduler = NEW cl_cbo_job_scheduler( ).


          lo_job_action = NEW cl_abapgit_repo_pull_action(
          is_alog_key = ls_alog_key
          iv_repo_key = lv_repo_key
          is_req_data = ls_request_data ).



          "create new job from action
          lo_job_scheduler->start_job(
            EXPORTING
              io_action  = lo_job_action
              iv_job_user = sy-uname
            IMPORTING
              es_job_key = DATA(ls_job_key) ).
          IF ls_job_key IS NOT INITIAL.
            lo_log->set_batch_job( iv_jobname = ls_job_key-job_name iv_jobcount = ls_job_key-job_count ).
          ENDIF.

          response->set_status( cl_rest_status_code=>gc_success_accepted ).

          "[A4C_AGIT] END asynchronous/background processing ------------------
        ELSE.
          "OLD: START synchronous/dialog processing ------------------


          "set log-on information if supplied
          IF ls_request_data-user IS NOT INITIAL AND ls_request_data-password IS NOT INITIAL.
            cl_abapgit_default_auth_info=>refresh( ).
            cl_abapgit_default_auth_info=>set_auth_info( iv_user = ls_request_data-user
                                                         iv_password = ls_request_data-password ).
          ENDIF.

          "set the default transport request
          IF ls_request_data-transportrequest IS NOT INITIAL.
            cl_abapgit_default_transport=>get_instance( )->set( CONV #( ls_request_data-transportrequest ) ).
          ENDIF.





          "create online repo
          cl_abapgit_factory=>get_environment( )->set_repo_action( if_abapgit_app_log=>c_action_pull ).
          DATA(lo_repo) = cl_abapgit_repo_srv=>get_instance( )->get( lv_repo_key ).
          lo_repo->refresh( ).

          DATA(ls_checks) = lo_repo->deserialize_checks( ).

          "settings to overwrite existing objects
          LOOP AT ls_checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_overwrite>).
            <ls_overwrite>-decision = 'Y'.
          ENDLOOP.

          LOOP AT ls_checks-warning_package ASSIGNING FIELD-SYMBOL(<ls_warning_package>).
            <ls_warning_package>-decision = 'Y'.
          ENDLOOP.

          ls_checks-transport-transport = ls_request_data-transportrequest.

          "get log
          lo_log_factory = cl_abapgit_app_log_factory=>get_instance( ).


          lo_log = lo_log_factory->create_new( iv_repo_key    = lv_repo_key
                                                      iv_repo_branch = ls_request_data-branch
                                                      iv_repo_action = if_abapgit_app_log=>c_action_pull ).

          lo_log->save( ).

          "import objects
          lo_repo->deserialize( is_checks = ls_checks ii_log = lo_log ).

          DATA(lv_run_status) = lo_log->if_abapgit_log~get_status( ).
          CASE lv_run_status.
            WHEN if_abapgit_app_log=>c_run_status-success.
              lo_log->add_text( iv_text = 'Repository pulled successfully'  iv_type = 'S' ).
            WHEN if_abapgit_app_log=>c_run_status-warning.
              lo_log->add_text( iv_text = 'Repository pulled with warnings' iv_type = 'W' ).
            WHEN OTHERS. "no other value expected
              lo_log->add_text( iv_text = 'Repository pulled with error(s)' iv_type = 'E' ).
          ENDCASE.
          lo_log->set_run_status( lv_run_status ).

          response->set_status( cl_rest_status_code=>gc_success_ok ).

          "OLD: END synchronous/dialog processing ------------------
        ENDIF.
*---- Handle issues
      CATCH cx_abapgit_exception cx_abapgit_app_log cx_a4c_logger cx_cbo_job_scheduler cx_uuid_error cx_abapgit_not_found INTO DATA(lx_exception).
        IF lo_log IS BOUND.
          lo_log->add_exception( lx_exception ).
          lo_log->add_text( iv_text = 'Repository pull aborted' iv_type = 'A' ).
          lo_log->set_run_status( if_abapgit_app_log=>c_run_status-aborted ).
        ENDIF.
        ROLLBACK WORK.
        cx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.


  METHOD validate_request_data.

*    "check whether git url is well formed
*    cl_abapgit_url=>validate( |{ is_request_data-url }| ).
*
*    "check whether package is already used
*    cl_abapgit_repo_srv=>get_instance( )->validate_package( CONV #( is_request_data-package ) ).
*
*    "check whether git url is already used
*    DATA(lt_repo_list) = cl_abapgit_repo_srv=>get_instance( )->list( ).
*    LOOP AT lt_repo_list ASSIGNING FIELD-SYMBOL(<ls_repo_list>).
*      IF cl_http_utility=>if_http_utility~unescape_url( cl_abapgit_url=>name( is_request_data-url ) ) EQ <ls_repo_list>->get_name( ).
*        MESSAGE e002(A4C_AGIT_ADT) WITH is_request_data-url <ls_repo_list>->get_package( ) INTO DATA(lv_msg).
*        cx_abapgit_exception=>raise_t100( ).
*      ENDIF.
*    ENDLOOP.
*
*    "transport request exists
*    SELECT SINGLE * FROM e070 INTO @DATA(ls_e070)
*      WHERE
*      trkorr = @is_request_data-transportrequest.
*
*    IF sy-subrc NE 0.
*      MESSAGE e003(A4C_AGIT_ADT) WITH is_request_data-transportrequest INTO lv_msg.
*      cx_abapgit_exception=>raise_t100( ).
*    ELSEIF ls_e070-trstatus NE 'D'.
*      MESSAGE e004(A4C_AGIT_ADT) WITH is_request_data-transportrequest INTO lv_msg.
*      cx_abapgit_exception=>raise_t100( ).
*    ELSEIF ls_e070-as4user NE sy-uname.
*      MESSAGE e005(A4C_AGIT_ADT) WITH is_request_data-transportrequest sy-uname INTO lv_msg.
*      cx_abapgit_exception=>raise_t100( ).
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
