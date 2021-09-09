{
  client = rec {
    common = rec {
      api_server_url = "http://localhost:3000";
      api_server_sign_up_url = "${api_server_url}/sign-up";
      api_server_sign_in_url = "${api_server_url}/sign-in";
    };
    dev = {
      nontouchscreen_client_dev_www_server_port = "8000";
      touchscreen_client_dev_www_server_port = "8001";
    } // common;
  };
  server = rec {
    common = {
      allowed_request_headers = ["content-type"];
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
