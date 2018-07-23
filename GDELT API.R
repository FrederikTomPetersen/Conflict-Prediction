

          ############################################################
          #                                                          #
          #                    Test af GdeltR2 bulided               #
          #                                                          #
          ############################################################

library("gdeltr2")
library("hrbrthemes")
          
#The package?s data acquisition functions begin with get_urls_ for acquiring data store log information, 
#get_codes_ for acquiring code books and get_data_ for downloading and reading data.

#The data tidying functions begin with parse_ and they apply to a number of the features in 
#the gkg and vgkg data stores that will get described in further detail farther below.          


#
  #Get events
          
event_log <-   get_urls_gdelt_event_log()
event_url_list <- event_log$urlData

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



