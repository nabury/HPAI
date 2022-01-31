# HPAI
**HPAI Bangladesh model and plot code**

**HPAI control.R** contains the code to set up and run simulations of the HPAI Bangladesh model. Requires HPAI_functions.R as a source file. 
**HPAI_functions.R** contains the functions required in the HPAI control.R file

**Data files**  
Save in a directory named "R files"  
**management_options.rds** Data frame of all potential management options.   
Columns:   
limit - 20 (low capacity), 50 (medium capacity) or 100 (high capacity)  
control - 1 (IP culling), 2 (ring culling), 3 (ring vaccination), 4 (active surveillance)  
radius - ring control radius in km  
type - reactive or proactive (active surveillance only)  
priority - distance, population or density (active surveillance only)  
coverage - 5 (low capacity), 10 (medium capacity) or 25 (high capacity). Active surveillance only.  
