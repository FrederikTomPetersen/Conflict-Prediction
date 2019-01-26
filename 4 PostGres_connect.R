

############################################################
#                                                          #
#                      PostGRES SQL                        #
#                                                          #
############################################################

          #################################
          #        Postgres connect       #
          #################################
#Henter PostgresSQL database driver
drv <- dbDriver("PostgreSQL")

# Connecter til egen lokal database
con <- dbConnect(drv, dbname = "Speciale",
                 host = "localhost", port = 5432,
                 user = "postgres", password = rstudioapi::askForPassword("Please enter your password"))

# For at replikere hele projektet skal der således oprettes en lokaldatabase.
#Her er PostGresSQL programmet PGadmin4 anbefalelsesværdigt.