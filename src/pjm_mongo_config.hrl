-record(config, {host = "localhost",
                 port = 27017,
                 pool_name = pjm_mongo_pool,
                 pool_size = 5,
                 database = pjm_mongo,
                 read_mode = master,
                 write_mode = safe}).
