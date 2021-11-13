{
  client = rec {
    common = rec {
      api_server_get_shared_set_url = "${api_server_url}/get-shared-set";
      api_server_get_shared_sets_ids_and_names_url =
        "${api_server_url}/get-shared-sets-ids-and-names";
      api_server_sign_in_url = "${api_server_url}/sign-in";
      api_server_sign_up_url = "${api_server_url}/sign-up";
      api_server_share_set_url = "${api_server_url}/share-set";
      api_server_unshare_set_url = "${api_server_url}/unshare-set";
      api_server_update_shared_set_url = "${api_server_url}/update-shared-set";
      api_server_url = "http://localhost:3000";
    };
    dev = {
      nontouchscreen_client_dev_www_server_port = "8000";
      touchscreen_client_dev_www_server_port = "8001";
    } // common;
  };
  server = rec {
    common = {
      allowed_request_headers = ["authorization" "content-type"];
      allowed_request_methods = ["DELETE" "GET" "POST"];
      api_server_port = "3000";
    };
    dev = {
      allowed_request_origins = "*";
      database_host = "127.0.0.1";
      database_name = "lang_memorizer";
      database_port = "5432";
      database_user_name = "db_admin";
      database_user_password = "123";
    } // common;
    prod = {
      allowed_request_origins = ["http://localhost:8000" "http://localhost:8001"];
      database_host = "127.0.0.1";
      database_name = "lang_memorizer";
      database_port = "5432";
      database_user_name = "db_admin";
      database_user_password = "123";
    } // common;
  };
}
