# demographic-decomposition-for-motor-vehicle
Using demographic decomposition to explain differences in MV death rates between population groups

Background
-------
This work is part of Chapter 2 in my dissertation, [Under the Hood: Revealing Patterns of Motor Vehicle Fatalities in the United States](https://repository.upenn.edu/edissertations/2396/). Some analyses from my dissertation are omitted from this Github repo for clarity and flow. 

Data required
------

1) **Vital Statistics data from the CDC WONDER online database**

Use the following saved links and download txt files (click "export") of motor vehicle deaths grouped by age, gender, and race (non-Hispanic):
* All MV deaths for the population of interest, 2001-2010: https://wonder.cdc.gov/controller/saved/D76/D7F505
* Pedestrian MV deaths for the population of interest, 2001-2010: https://wonder.cdc.gov/controller/saved/D76/D7F510
* Passenger vehicle MV deaths for the population of interest, 2001-2010: https://wonder.cdc.gov/controller/saved/D76/D7F511

2) **National Household Travel Survey**

Download Trip and Person files in CSV for the 2001 and 2009 surveys.
* Link to download 2001 data: https://nhts.ornl.gov/2001/download/Ascii.zip
* Link to download 2009 data: https://nhts.ornl.gov/2009/download/Ascii.zip

Rename "DAYV2PUB.CSV" as "trip.csv" and "PERV2PUB.CSV" as "person.csv"
 
R Dependencies
--------
* tidyverse
* reshape2
* here
* ggthemes
* knitr

Description of R scripts
-------
- **functions.r** contains all functions for the project
- **read_clean.r** sources code from utils.r; reads in data and cleans data
- **analysis_plots.r** performs analysis and plots graphs

How to Use
-------
1) Download R Studio and install packages listed under Dependencies
2) Download Data above and store as follows:

This project directory
|--Data

|----cdc_travel_2001_2010.txt

|----cdc_travel_2001_2010_ped.txt

|----cdc_travel_2001_2010_pveh.txt

|----travel

|--------2001

|------------person.csv

|------------trip.csv

|--------2009

|------------person.csv

|------------trip.csv


3) run "open -a Rstudio read_clean.r" and "open -a Rstudio analysis_plots.r"
