## ECON122 Final Project - Priyanka Agarwal and Seungho (Samuel) Lee

This project can be seen at https://samuellee19.github.io/econ122_sqf/.

Since GitHub has a problem loading Shiny app from shinyapps.io, we added a link with a screenshot from the dashboard. The interactive dashboard can be accessed at https://samuellee19.shinyapps.io/SFQ_Final_Project/ (Note: it could take some time if the server is in the idle state)

The followings are the list of files used in this project, as well as, files uploaded on this GitHub repository.

### Data
Link: http://bit.ly/DSFinalProjectData    

Files:    
1. `2003.csv`: 2003 Stop and Frisk Data			
2. `2004.csv`: 2004 Stop and Frisk Data				
3. `2005.csv`: 2005 Stop and Frisk Data				
4. `2006.csv`: 2006 Stop and Frisk Data
5. `2007.csv`: 2007 Stop and Frisk Data				
6. `SQF1718.csv`: 2017 and 2018 Stop and Frisk Data for Visualization	
7. `2008.csv`: 2008 Stop and Frisk Data			
8. `X2018_sqf_codebook.csv`: 2018 Stop and Frisk Data's Variable Information    
9. `2009.csv`: 2009 Stop and Frisk Data	     
10. `X2018_sqf_codebook.xlsx`: 2018 Stop and Frisk Data's Variable Information as Excel      
11. `2010.csv`: 2010 Stop and Frisk Data		  
12. `X2018_sqf_database.csv`: 2018 Stop and Frisk Data	   
13. `2011.csv`: 2011 Stop and Frisk Data			
14. `X2018_sqf_database.xlsx`: 2018 Stop and Frisk Data	    
15. `2012.csv`: 2012 Stop and Frisk Data		 
16. `crime_by_precinct.csv`: Crime Information 2000-2018      
17. `2013.csv`: 2013 Stop and Frisk Data	  
18. `2014.csv`: 2014 2013 Stop and Frisk Data			
19. `2016_sqf_database.csv`: 2016 2013 Stop and Frisk Data        
20. `sqf-2015.csv`: 2015 2013 Stop and Frisk Data     
21. `2017_sqf_database.csv`: 2017 2013 Stop and Frisk Data     
22. `sqf-2017.csv`: 2017 2013 Stop and Frisk Data     
23. `2018_sqf_database.csv`: 2018 2013 Stop and Frisk Data    
24. `sqf-2017.xlsx`: 2017 2013 Stop and Frisk Data    

### Data
The SQF datasets were transformed into sf objects for map projections. Standard data munging is not available for sf objects, so we generated separate sf objects to shorten time consumed from the transformation processes. 

Hence, creating sf objects were performed with following settings:    
- EPSG:2263   
- NAD83   
- WGS84 Bounds      
Further details can be seen on Samuel's parts (`Part_Samuel.Rmd`, `app.R`).     

Link: https://bit.ly/2EHm3q4 

Files:    
1. `plot_locations_0407.RData`: 2004-2007 SQF SF Objects     
2. `plot_locations_0407SF.RData`: 2004-2007 SQF SF Objects limited to `Seven Major Felonies`   
3. `plot_locations_0812.RData`: 2008-2012 SQF SF Objects   
4. `plot_locations_0812SF.RData`: 2008-2012 SQF SF Objects limited to `Seven Major Felonies`   
5. `plot_locations_1316.RData`: 2013-2016 SQF SF Objects   
6. `plot_locations_1316SF.RData`: 2013-2016 SQF SF Objects limited to `Seven Major Felonies`    
7. `plot_locations_1718.RData`: 2017-2018 SQF SF Objects    
8. `plot_locations_1718SF.RData`: 2017-2018 SQF SF Objects limited to `Seven Major Felonies`   

### Repo Files       
Files:    
1. `Part_Priyanka.Rmd`: Codes from Priyanka's part   
2. `Part_Samuel.Rmd`: Data Transformation Code from Samuel's part   
3. `SFQ_Final_Project`: Relevant Code for Shiny App Implementation   
4. `README.md`: List of Files and Explanation