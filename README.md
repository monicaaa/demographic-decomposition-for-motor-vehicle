# demographic-decomposition-for-motor-vehicle
Using demographic decomposition to explain differences in MV death rates between population groups

Background
-------
This work is part of Chapter 2 in my dissertation, [Under the Hood: Revealing Patterns of Motor Vehicle Fatalities in the United States](https://repository.upenn.edu/edissertations/2396/). Some analyses from my dissertation are omitted from this Github repo for clarity and flow. 

Data required
------

1) **Vital Statistics data from the CDC WONDER online database**

Save all data in a folder called "data".

Use the following saved links and download txt files (click "export") of motor vehicle deaths grouped by age, gender, and race (non-Hispanic):
* All MV deaths for the population of interest, 2001-2010: https://wonder.cdc.gov/controller/saved/D76/D7F505
* Pedestrian MV deaths for the population of interest, 2001-2010: https://wonder.cdc.gov/controller/saved/D76/D7F510
* Passenger vehicle MV deaths for the population of interest, 2001-2010: https://wonder.cdc.gov/controller/saved/D76/D7F511

2) **National Household Travel Survey**

Download Trip and Person files in CSV for the 2001 and 2009 surveys.
* Link to download 2001 data: https://nhts.ornl.gov/2001/download/Ascii.zip
* Link to download 2009 data: https://nhts.ornl.gov/2009/download/Ascii.zip

From the 2001 data download, save "DAYPUB.csv" as "trip2001.csv" and save "PERPUB.csv" as "person2001.csv". 

For the 2009 data download, save "DAYV2PUB.CSV" as "trip.csv" and save "PERV2PUB.CSV" as "person.csv".

3) **2000 Population Standard**

When you pull from this repo, "pop2000stand.csv" will be saved. 
 
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
2) Download Data above and store as according to instructions above
3) run "open -a Rstudio read_clean.r" and "open -a Rstudio analysis_plots.r"

Future Release
-------
- include Rmd reports with analysis
