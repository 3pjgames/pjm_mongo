-record(config, {host = "localhost" :: string(),
                 port = 27017 :: integer(),
                 pool_name = pjm_mongo_pool :: atom(),
                 pool_size = 5 :: integer(),
                 database = pjm_mongo :: atom(),
                 read_mode = master :: atom(),
                 write_mode = safe :: atom()}).
