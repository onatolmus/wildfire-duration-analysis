# Wildfire Duration Analysis
## Survival analysis on duration of wildfires using the Cox model with time-dependent covariates

This analysis demonstrates how the stratified Cox model with time-dependent covariates can be used to identify main elements of wildfire duration. It uses the historical wildfires recorded in Alberta, Canada from 2006 to 2018. This data set is published by the government of Alberta with an Open Government License (https://open.alberta.ca/licence). This data set (af-historic-wildfires-2006-2018-data.csv) and its corresponding dictionary (af-historic-wildfires-2006-2018-data-dictionary.pdf) can be found in the Data folder. This data set includes time updates for each wildfire so that the Cox model with time-dependent covariates can be used. There is no censoring in the data.

The data processing section is conducted in Python. Running the Cox model and its diagnostics is done in R. 

### Data Processing
To rerun the Python file "data_processing.py", please download the below files from the Canadian Wildlan Fire Information System (CWFIS) Data Services using this link: https://cwfis.cfs.nrcan.gc.ca/downloads/fwi_obs/ 
- "cwfis_fwi2000sv3.0opEC_2015.csv"  
- "cwfis_fwi2010sopEC.csv"
Extract the files if needed and add these in the following folder: Data/FWI.

As a result of the data processing three files are created:
- "df_cox.csv" : contains all the wildfires in a format ready to input into the Cox model with time-dependent covariates
- "df_cox_l.csv" : contains lightning-caused fires in a format ready to input into the Cox model with time-dependent covariates
- "df_cox_p.csv" : contains people-caused fires in a format ready to input into the Cox model with time-dependent covariates

3 CSV files mentioned above are provided in this repository.

The Python file "data_processing.py" reads the data in "Data" folder and processes the data to create the data format to input into the Cox model.

This file also creates the descriptive statistics and plots that are used in the analysis. This code reads the following data: 
- original data: 'Data/af-historic-wildfires-2006-2018-data.csv' 
- weather stations data: 'Data/FWI/cwfis_allstn2022.csv'
- FWI variables data 2000s: 'Data/FWI/cwfis_fwi2000sv3.0opEC_2015.csv'
- FWI variables data 2010s: 'Data/FWI/cwfis_fwi2010sopEC.csv'


By using the above data sets, this Python file creates a combined data for the Cox model with time-dependent covariates.
To create the outputs of this Python file, the following packages are used: numpy, pandas, matplotlib.pyplot, datetime, time and seaborn

### Run the Cox Model
The Cox model and its residual diagnostics are run in R. 
The R file "cox_model.R" reads the csv files provided in this repository ("df_cox.csv","df_cox_l.csv","df_cox_p.csv").
These CSV files can also be recreated by running the Python file "data_processing.py".

The following libraries are needed to run the R file: ggplot2, tidyverse, survival, survminer, MASS, grid and gridExtra. Please install these if needed.
The survival curves and the residual plots can be generated by running the R file.
Upon reading the CSV files, The R code "cox_model.R" runs Cox models with different setups for lightning- and people-caused fires.
The proportional hazards assumption and the other residual diagnostic checks are performed in the same code.