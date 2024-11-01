# enviroscreen_2.0

## Colorado EnviroScreen
Colorado EnviroScreen is an interactive environmental justice mapping tool. Version 1.0 of Colorado EnviroScreen launched on June 28, 2022.

The tool enables users to identify disproportionately impacted communities based on the definition in Colorado’s [Environmental Justice Act](https://leg.colorado.gov/sites/default/files/2021a_1266_signed.pdf) (HB21-1266) so that communities directly benefit from:

- Money and other resources. For example, CDPHE’s Environmental Justice Advisory Board will use EnviroScreen to determine where to distribute environmental justice grants created by the new law.

- Enhanced opportunities to participate in Air Quality Control Commission rulemaking and permitting decisions.

- Priority for enforcement and compliance initiatives under [an agreement](https://drive.google.com/file/d/14o0E-LhS7c_uzifH4xaztrmhJA9mh4Ia/view) between CDPHE and the U.S. Environmental Protection Agency.

## 2.0 Updates 

The material within this repository is an incomplete draft until the public release of the 2.0 version of Colorado EnviroScreen in Fall 2024. 



### running the workflow 

- Download the repo from [github](https://github.com/GeospatialCentroid/enviroscreen_2.0) 

- Open the `enviroscreen_2.0.Rprof` file 

- `census api key` : you will need a census api key to run the tidycensus functions. You can get one [here](https://api.census.gov/data/key_signup.html)

- ensure that you are on the main branch of the repo 
  - select terminal tab in rstudio 
  - run `git status`
  - ensure you see `On branch main`
  - if not, try `git checkout main`, run `git status` to double check 
  
- download the source data from [link] -- add link once this exists 

- move the source data to the `data` folder and extract 

- if needed rename the extracted folder `raw` 
  - this naming strucutre is essential for file indexing within the code 
  
  
- once complete you should be able to run the `0_main.R` script to kick off the full workflow. 

*note* : there are multiple cases where functions have a `overwrite` elements associated with them. These are used to prevent redudent API call or time consuming processed then they are not needed. If you are running a process for the first time set the paramter `overwrite <- TRUE` else set `overwrite <- FALSE`

*note2* : most of the indicator score calculations will run quite quitely. Yet some require some significant processing time. The core culperate of this are the buffer based methods. `getAir`, `getOtherAir`, `getMining`, `getOilandGas`,`getFlood` and `getStreams`. Of these `getOilandGas` is really the only one to be weary of. Run time for me have been on the hours time scale. There are 90,000 oil and gas sites to evaluate and that takes times. Print statements are included in these slow funcitons so there is some validation that the method is progressing. 







  





### Repo Structure 


**allcodeastext.txt** : this is print out of the `scripts/exportAllCode.R`. It reads in all .R scripts within the repo and puts them in one file. Can be helpful to troubleshoot specific errors if you are trying to find the specific funtion that called it. 


**0_main.R** : primary processing script for the project. Once the raw data is download from [add source once finalized] the script can be ran to produce all assocaited results.  


- Data 
  **raw** 
  - Data sets downloaded from the source.
  - In cases where data is require to altered outside of a the R environment (mostly .xsl files) both the original and the processed inputs are provided. 
  
  **processed**
  - outputs from the data gathering function. 
  - ACS and EJSCREEN data is gather and format, the specific indicator functions that use these processed features to generate specific indicator scores.  
  - geographies folder contains the geographic datasets used by the code based as these are not final products in any meaningful way 
  
  **products**
  - final results for independent layers 
  - organized by conponent score calculations 
    


- Functions 
  *all function are read into environment when running workflow* 
  - componentScoreCalculations 
    - functions for gathering the individual indicator score and producing the various group component score values and final enviroscreen values 
    - organized by component categories  
  
  - gahteringDataSources
    - Functions for pulling in datasets directly from a web accessible source 
    - datasets are stored in processed folders 
  
  
  - indicatorScoreCalculations 
    - specific processing functions for selected indicators, these are generate going to populate values for the products data folder

  - Temp 
    - rements of methods ands test that should probably just be removed 
    
- Scripts
  - The difference between scripts and functions is a bit tenuous. Effectively Scripts are `source` into the environment if needed. They are ran when sourced and usualy have a file.exist condition to check to see if the output exists. In a way these steps are just a bit different as they are not used to gatherDataSources, generate indicator ScoreCalculations, or componentScoreCalculations.
  - Some helper scripts as well to evaluate the difference between runs or export the final datasets  
  
  
- temp 
  - Things that don't directly effect the processing workflow but might be helpful.
  

- utilities 
  - helper functions that are used throughout the workflow or by multiple indication processing funcitons 
  

  


