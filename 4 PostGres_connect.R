

############################################################
#                                                          #
#                    PostGRES SQL aaa                      #
#                                                          #
############################################################
          #################################
          #        Postgres connect       #
          #################################


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "Speciale",
                 host = "localhost", port = 5432,
                 user = "postgres", password = rstudioapi::askForPassword("Please enter your password"))
