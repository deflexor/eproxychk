-ifndef(EPROXYCHK_HRL).
-define(EPROXYCHK_HRL, "eproxychk.hrl").

-define(UA, "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)").
-define(GOOGLE_IP, "8.8.8.8").
-define(MAP_VAL(K, S), proplists:get_value(K, S)).
-define(MAP_VAL(K, S, D), proplists:get_value(K, S, D)).
-define(MAP_VALB(K, S), proplists:get_value(list_to_binary(K), S)).
-define(MAP_VALB(K, S, D), proplists:get_value(list_to_binary(K), S, D)).
-define(MAP_VALBB(K, S), binary_to_list(proplists:get_value(list_to_binary(K), S))).
-define(MAP_UPD(K, V, S), lists:keystore(K, 1, S, {K, V}) ).
-define(MAP_UPDB(K, V, S), lists:keystore(list_to_binary(K), 1, S, {list_to_binary(K), V}) ).
-define(LOG_ERR(S), error_logger:error_msg("~p~n", [S]) ).
-define(LOG_INFO(S), error_logger:info_msg("~p~n", [S]) ).
-define(GEO_DB_FILE, "IpToCountry.csv").

-record(proxyserver, {
          host,
          type = unknown,
          anonymity = unknown,
          country = unknown,
          speed = unknown,
          stability = unknown,
          scans = 0,
          ok_scans = 0,
          last_update = never
}).

-record(country, {
          country,
          cn
}).

-record(settings, {
          key,
          val
}).

-record(geoip, {
          ip_range,
          registry,
          cn,
          country
}).

-record(geoipz, {
          inserted,
          ip_from,
          ip_to,
          registry,
          cn,
          country
}).



-endif.
