# demographic-decomposition-for-motor-vehicle
Using demographic decomposition to explain differences in MV death rates between population groups

Background
-------
This work is published in Chapter 2 of my dissertation, [Under the Hood: Revealing Patterns of Motor Vehicle Fatalities in the United States](https://repository.upenn.edu/edissertations/2396/). Some analyses from my dissertation are omitted from this Github repo for clarity and flow. 

Data required
------

1) Vital Statistics from the CDC WONDER online database. Use the following links to export txt files of motor vehicle deaths by age, gender, and race (non-Hispanic):
* CDC travel comparison (all MV): https://wonder.cdc.gov/controller/saved/D76/D7F505
* CDC travel comparison (Pedestrian): https://wonder.cdc.gov/controller/saved/D76/D7F510
* CDC travel comparison (Passenger Vehicle): https://wonder.cdc.gov/controller/saved/D76/D7F511

2) National Household Travel Survey
* Download 2001 and 2009 Trip and Person files: http://nhts.ornl.gov/download.shtml

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
- utils.r contains all functions for the project
- config.r sources code from utils.r and reads in data
- clean.r runs functions to clean all necessary data 
