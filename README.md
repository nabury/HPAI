# HPAI Bangladesh model and plot code

Model and plotting code to accompany the thesis Bradbury, N (2023) *Computational approaches for optimising avian influenza outbreak control and disseminating research findings*

For key references, please see the [References](https://github.com/nabury/HPAI/wiki/References)

---

### Analysis folder

Contains files with code to recreate the plots for chapters 3-5 in files named Chapter 3.R, Chapter 4.R and Chapter 5.R respectively.  
Download the entire folder and set it analysis folder as your working directory within the Chapter files. 
The analysis folder contains all the data/results files needed to recreate the plots.

**Note:** Please contact me directly if you require the data files 'bangladeshpoultrydata_clean.rds' and 'HPAI_Outbreaks_2012_All.xlsx' for the Chapter 3 analysis as these contain identifying information

---

### HPAI app folder
Contains all the code for the app, hosted at https://epimodelling.shinyapps.io/HPAIapp/  
For further information see the [User Guide](https://github.com/nabury/HPAI/wiki/HPAI-App-User-Guide)

---

### HPAI model folder

**Files containing the model code and required data**

##### Code
**HPAI model verification.R** contains all the code required specifically for the verification model.  
**HPAI control.R** contains the code to set up and run simulations of the HPAI Bangladesh model.     
**HPAI_functions.R** contains the functions required in the HPAI control.R file.  

##### Data files
**management_options.rds** Data frame of all potential management options.   
Columns:   
limit - 20 (low capacity), 50 (medium capacity) or 100 (high capacity)  
control - 1 (IP culling), 2 (ring culling), 3 (ring vaccination), 4 (active surveillance)  
radius - ring control radius in km  
type - reactive or proactive (active surveillance only)  
priority - distance, population or density (active surveillance only)  
coverage - 5 (low capacity), 10 (medium capacity) or 25 (high capacity). Active surveillance only. 

**w2_C_div_parameters.rds** Thinned parameter estimates for wave 2 at the division level.  
**w5_C_div_parameters.rds** Thinned parameter estimates for wave 5 at the division level. 

**Due to file size, please contact me directly if you require a copy of these files**  
**dhaka_div_data.rds** Holding data for the Dhaka division. Contains locations and chicken numbers.  
**dhaka_div_distance_matrix.rds** Distance matrix for all premises in the Dhaka division.  
