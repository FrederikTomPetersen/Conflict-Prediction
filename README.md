# Ethnic-Conflict-Prediction

----------Project in progres------------

This project aims to utilze GDELT event data to predict conflict and civilwar in particular. 

The project is largely written in R. However I have used a local postgres database to store my data and performe some tidying of the data and to group the data into country month which is the primary entity on the study. In order to replicate the study completely one need to adjust the "PostGres_connect" scirpt or make a work around for fetching, saving and loading data.

From the master script it is possible to execute the project and the scripts serves as an overview of the progress of the project.

  1) Loading R packages necessary for project  
  2) Defining custom functions to be called throughout the other scripts
  3) DataLoad - fetching data from online sources and saving them to postgres
  4) In postgres - run SQL-scripts to tidy and group data
  5) DataGather - loading data from postgres and joining data from different sources into the final dataset
  6) Visualizing - visualizing the data
  7) Modelling - modelling the data and running models
  

