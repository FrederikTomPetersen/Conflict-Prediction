

          ############################################################
          #                                                          #
          #                    Test af GdeltR2 builded               #
          #                                                          #
          ############################################################

#devtools::install_github("hrbrmstr/hrbrthemes")
#devtools::install_github("abresler/gdeltr2")
        
                    
library("devtools")
library("gdeltr2")
library("hrbrthemes")
library("tidyverse")
library("countrycode")
          
setwd("C:/Users/Frederik/Documents/konflikt/")
            
All_eventdb_url <-  get_urls_gdelt_event_log()
Event_url_list <- All_eventdb_url$urlData 
Event_udvalg_1 <- Event_url_list[1]# er en komprimeret udgave af årene 1970-2014
Event_udvalg_5 <- Event_url_list[5]
Event_udvalg_1_5 <- Event_url_list[2:6]


stability_locations <-  get_codes_stability_locations()
          
?get_data_locations_instability_api()
  
 
 A_code <- c("BC", "BH", "BN", "CD", "CF", "CG", "SG","UG") 
 
 A_sub <-  stability_locations %>% 
    filter(codeCountry %in% A_code)
 
 A_sub <-  A_sub$idLocation
 


 
Location_all <- stability_locations$idLocation %>% 
  unique() %>% 
  filter(stability_locations$codeCountry %in% (A_code))

 
All_tone <-  get_data_locations_instability_api(location_ids = Location_all,
                        random_locations = NULL, variable_names = "tone", days_moving_average = NA,
                        time_periods = "daily", use_multi_locations = F, return_wide = T,
                        visualize = F, nest_data = F, return_message = T) 

save(All_tone, file= "All_tone")


medgeo <-   left_join(All_tone, Gadm_2, by = c("nameLocation" = "NAME_1"))


left_join(test_data, kantrowitz, by.x = "first_name", by.y = "name")
left_join(test_data, kantrowitz, by = c("first_name" = "name"))          
          
          
          
          
African_sub_tone <-  get_data_locations_instability_api(location_ids = A_sub,
    random_locations = NULL, variable_names = c("instability", "conflict",
                                                "tone", "protest", "artvolnorm"), days_moving_average = NA,
    time_periods = "daily", use_multi_locations = F, return_wide = T,
    visualize = F, nest_data = F, return_message = T)
          

str(medgeo,10)



