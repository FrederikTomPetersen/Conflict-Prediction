

#
#     GET RAW TEXT FROM GDELT API 3 months
#
# Det ser desværre ud til at der er et maximun på 250 artikler..

# terms to search

my_terms <-  c("war", "conflict", "protest", "instability")


#domains to search
my_domains <-  c("nypost.com", "washingtonpost.com", "wsj.com")


?get_data_ft_v2_api
records_terms <- get_data_ft_v2_api(terms = my_terms, domains = my_domains,
                              modes = c("Artlist"), timespans = my_timespan)

#themes to search 

df_gkg <- 
  get_gdelt_codebook_ft_api(code_book = "gkg")
View(df_gkg)


Themes <-  df_gkg %>% 
  select(idGKGTheme, isAgressionAct, isMilitaryEvent) %>% 
  filter(isAgressionAct == T | isMilitaryEvent == T)

my_themes <- Themes$idGKGTheme
#Jeg tror generelt at jeg har for mange themes med her. 3200 er alligevel en del. 

my_themes <- Themes[1:10,]$idGKGTheme


#Define timespan
#Det skal defineres som en string, se http://asbcllc.com/blog/2017/august/intro_to_programming_with_gdeltr2/index.html
my_timespan <- "12 weeks"



# Definer hvilke lande som artiklerne må komme fra. Her kunne man overveje de lande, der er naboer eller som har interesse i et land

df_countries <- get_gdelt_codebook_ft_api(code_book = "countries")
View(df_countries)

#my_countries



#The Final call

records <- get_data_ft_v2_api(terms = my_terms, domains = my_domains, 
                   gkg_themes = my_themes, 
                   modes = c("Artlist"), timespans = my_timespan)