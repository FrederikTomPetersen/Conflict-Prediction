
cat("\014")  

################################################################################################
##                                Modellering af prædiktionsmodeller                          ##
################################################################################################

# Analysen trækker på 4 R filer - én for hver responsvariable. 

setwd(PredictModels)

source("Models_cwstart.R") 
source("Models_cwm.R") 
source("Models_death.R") 
source("Models_tilfælde.R") 

# Hver af disse filer udregner en række prædiktionsmodeller for det pågældende responsvariable.
# hver fil gemmer resultater af analysen samt en række modeller, der kan trækkes ind i R. For en fuldstændig replikation

#Importance analysen og Tuning analysen er i selvstændige filer