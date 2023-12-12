{application, 'mrserver', [
	{description, "Imersia Mixed Reality Server"},
	{vsn, "3.2.0"},
	{id, "c226be1-dirty"},
	{modules, ['acme_challenge','api_sup','base32','bits','comms_tests','gf256','imersia_actions','imersia_admin_api','imersia_analytics_api','imersia_auth','imersia_automations_api','imersia_automations_handler','imersia_automations_sup','imersia_automations_worker','imersia_billing','imersia_channel_sup','imersia_channel_worker','imersia_channels_api','imersia_companion_socket','imersia_companion_ssl_socket','imersia_companion_sup','imersia_companion_worker','imersia_context_api','imersia_csv','imersia_db','imersia_developer','imersia_entanglements_sup','imersia_entanglements_worker','imersia_entanglements_worker_fsm','imersia_file_send','imersia_files','imersia_files_api','imersia_geobot_sup','imersia_geobot_sup_sup','imersia_geobot_worker','imersia_geobots_api','imersia_geobots_log_api','imersia_geobots_send_api','imersia_geojson','imersia_geojson_api','imersia_get_file','imersia_license','imersia_license_sup','imersia_license_worker','imersia_local_files','imersia_login','imersia_maptile','imersia_maths_leex','imersia_maths_yecc','imersia_metadata_api','imersia_misc','imersia_mnesia','imersia_mnesia_analytics','imersia_mnesia_automation','imersia_mnesia_channel','imersia_mnesia_context','imersia_mnesia_geobot','imersia_mnesia_metadata','imersia_mnesia_passcode','imersia_mnesia_user','imersia_mongo','imersia_mongo_analytics','imersia_mongo_automation','imersia_mongo_channel','imersia_mongo_context','imersia_mongo_geobot','imersia_mongo_metadata','imersia_mongo_passcode','imersia_mongo_user','imersia_oldserver','imersia_passcodes_api','imersia_remote','imersia_sessions_api','imersia_settings','imersia_smtp','imersia_user_api','imersia_user_password_api','imersia_user_tokens_api','imersia_vars_api','imersia_velocity_engine_sup','mrserver_app','mrserver_sup','qrcode','qrcode_mask','qrcode_matrix','qrcode_reedsolomon','test_analytics','test_api','test_db','test_maths','test_smtp','test_types']},
	{registered, [mrserver_sup]},
	{applications, [kernel,stdlib,crypto,ssl,inets,mnesia,observer,wx,gun,cowboy,ej,uuid,eunit,geode,emagick,mongodb,gen_smtp,bcrypt,simple_oauth2,iso8601,edate,etest_http,jiffy,mimerl,etoml,zucchini,jsx,bson]},
	{optional_applications, []},
	{mod, {mrserver_app, []}},
	{env, [
  {version, "3.2.0-2023.07.30"},
  {admin_exception, "exploremine"}
]}
]}.