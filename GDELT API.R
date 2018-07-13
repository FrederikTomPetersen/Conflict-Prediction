

          ############################################################
          #                                                          #
          #                    Test af GdeltR2 bulided               #
          #                                                          #
          ############################################################

library("gdeltr2")
library("hrbrthemes")
          
#The packageâs data acquisition functions begin with get_urls_ for acquiring data store log information, 
#get_codes_ for acquiring code books and get_data_ for downloading and reading data.

#The data tidying functions begin with parse_ and they apply to a number of the features in 
#the gkg and vgkg data stores that will get described in further detail farther below.          


#
          #Get events
          
  event_log <-   get_urls_gdelt_event_log()


          get_urls_gkg_tv_daily_summaries          
          
 stability_locations <-  get_codes_stability_locations()
          
 ?get_data_locations_instability_api()
  
 
 A_code <- c("BC", "BH", "BN", "CD", "CF", "CG") 
 Yemen <-  "YM"
 
 
 
 Yemen_GDELT <-  get_data_locations_instability_api(location_ids = Yemen,
                                                     random_locations = NULL, variable_names = c("instability", "conflict",
                                                                                                 "tone", "protest", "artvolnorm"), days_moving_average = NA,
                                                     time_periods = "daily", use_multi_locations = F, return_wide = T,
                                                     visualize = F, nest_data = F, return_message = T)
 
 Yemen_GDELT_vizualize <-  get_data_locations_instability_api(location_ids = Yemen,
                                                    random_locations = NULL, variable_names = c("instability", "conflict",
                                                                                                "tone", "protest", "artvolnorm"), days_moving_average = NA,
                                                    time_periods = "daily", use_multi_locations = F, return_wide = T,
                                                    visualize = T, nest_data = F, return_message = T)
 
 
          African_tone_visualize <-  get_data_locations_instability_api(location_ids = A_code,
                                             random_locations = NULL, variable_names = c("instability", "conflict",
                                                                                         "tone", "protest", "artvolnorm"), days_moving_average = NA,
                                             time_periods = "daily", use_multi_locations = F, return_wide = T,
                                             visualize = T, nest_data = F, return_message = T)
          
          African_tone <-  get_data_locations_instability_api(location_ids = A_code,
                                                                        random_locations = NULL, variable_names = c("instability", "conflict",
                                                                                                                    "tone", "protest", "artvolnorm"), days_moving_average = NA,
                                                                        time_periods = "daily", use_multi_locations = F, return_wide = T,
                                                                        visualize = F, nest_data = F, return_message = T)
          
plot(African_tone_visualize)
