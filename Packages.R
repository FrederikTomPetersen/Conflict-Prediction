
#Installation af pakker og hentning af relvante pakker 



# devtools::install_github("hadley/devtools")
# devtools::install_github("hadley/dplyr")
# devtools::install_github("hafen/trelliscopejs")
# devtools::install_github("abresler/gdeltr2")


Packages <- c("devtools",
              "dplyr",
              "caret",
              "sf",
              "trelliscopejs",
              "gdeltr2")


lapply(Packages, library, character.only = TRUE)
