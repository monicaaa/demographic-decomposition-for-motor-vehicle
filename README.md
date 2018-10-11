# demographic-decomposition-for-motor-vehicle
Using demographic decomposition to explain differences in MV death rates between population groups

Background
-------
This work is published in Chapter 2 of my dissertation, [Under the Hood: Revealing Patterns of Motor Vehicle Fatalities in the United States](https://repository.upenn.edu/edissertations/2396/). Some analyses from my dissertation are omitted from this Github repo for clarity and flow. 

Data required
------

1) **Vital Statistics data from the CDC WONDER online database**

Use the following saved links to export txt files of motor vehicle deaths grouped by age, gender, and race (non-Hispanic):
* All MV deaths for the population of interest, 2001-2010: https://wonder.cdc.gov/controller/saved/D76/D7F505
* Pedestrian MV deaths for the population of interest, 2001-2010: https://wonder.cdc.gov/controller/saved/D76/D7F510
* Passenger vehicle MV deaths for the population of interest, 2001-2010: https://wonder.cdc.gov/controller/saved/D76/D7F511

2) **National Household Travel Survey**

Download Trip and Person files in CSV for the 2001 and 2009 surveys. 
* Link to download 2001 data: https://nhts.ornl.gov/2001/download/Ascii.zip
* Link to download 2009 data: https://nhts.ornl.gov/2009/download/Ascii.zip

R Dependencies
--------
* dplyr
* reshape2
* ggplot2
* ggthemes
* foreign
* stargazer
* stringr
* purrr
* lazyeval
* xtable
* tidyr
* srvyr
* broom
* scales 

Description of R scripts
-------
- **utils.r** contains all functions for the project
- **config.r** sources code from utils.r and reads in data
- **clean.r** runs functions to clean all necessary data 
- **analysis.r** performs analysis and plots graphs
