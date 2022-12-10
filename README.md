# HPAI
**HPAI Bangladesh model and plot code**

**Model code**
**HPAI model verification.R** contains all the code required specifically for the verification model.  
**HPAI control.R** contains the code to set up and run simulations of the HPAI Bangladesh model.     
**HPAI_functions.R** contains the functions required in the HPAI control.R file.  

**Data files**  
Save in a directory named "R files" to automate selection of required data files when running simulations using HPAI control.R  
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

**Please contact me directly for a copy of these files**  
**dhaka_div_data.rds** Holding data for the Dhaka division. Contains locations and chicken numbers.  
**dhaka_div_distance_matrix.rds** Distance matrix for all premises in the Dhaka division.  

**Analysis folder** Contains files with code to recreate the data analysis and plots for each relevant chapter.  
**App folder** Contains all the code for the app, hosted at https://epimodelling.shinyapps.io/HPAIapp/  
