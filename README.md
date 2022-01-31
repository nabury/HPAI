# HPAI
**HPAI Bangladesh model and plot code**

**Model code**
**HPAI control.R** contains the code to set up and run simulations of the HPAI Bangladesh model. Requires HPAI_functions.R as a source file.   
**HPAI_functions.R** contains the functions required in the HPAI control.R file

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

**Please contact me directly for a copy of these files**
**dhaka_dist_data.rds** Holding data for the Dhaka district. Contains locations and chicken numbers.  
**dhaka_div_data.rds** Holding data for the Dhaka division. Contains locations and chicken numbers.  
dhaka_dist_data.rds
dhaka_div_distance_matrix.rds
