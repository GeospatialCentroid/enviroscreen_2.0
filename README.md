# enviroscreen_2.0

## Colorado EnviroScreen
Colorado EnviroScreen is an interactive environmental justice mapping tool. Version 1.0 of Colorado EnviroScreen launched on June 28, 2022.

The tool enables users to identify disproportionately impacted communities based on the definition in Colorado’s [Environmental Justice Act](https://leg.colorado.gov/sites/default/files/2021a_1266_signed.pdf) (HB21-1266) so that communities directly benefit from:

- Money and other resources. For example, CDPHE’s Environmental Justice Advisory Board will use EnviroScreen to determine where to distribute environmental justice grants created by the new law.

- Enhanced opportunities to participate in Air Quality Control Commission rulemaking and permitting decisions.

- Priority for enforcement and compliance initiatives under [an agreement](https://drive.google.com/file/d/14o0E-LhS7c_uzifH4xaztrmhJA9mh4Ia/view) between CDPHE and the U.S. Environmental Protection Agency.

## 2.0 Updates 

The material within this repository is an incomplete draft until the public release of the 2.0 version of Colorado EnviroScreen in Fall 2024. 



### Repo Structure 

**0_main.R** : primary processing script for the project 


- Data 
  **raw** 
  - Data sets downloaded from the source (aim for nothing but a download to have happened to these layers )
  
  **processed**
  - intermediate layers that are not representative of a final product 
    - datasets required to produce products but also requires some direct processing 
  
  **products**
  - final results for independent layers 
    


- Functions 
  *all function are read into environment when running workflow* 
  - componentScoreCalculations 
    - functions for gathering the individual indicator score and producing the various group component score values and final enviroscreen values 
  
  - gahteringDataSources
    - Functions for pulling in datasets directly from a web accessible source 
  
  
  - indicatorScoreCalculations 
    - specific processing functions for selected indicators, these are generate going to populate values for the products data folder
    
    
- Scripts 
  *Scripts are `source` into the environment if needed, usually based on the presence of a specific dataset* 
  - Methods and processing that likely only need to be ran once, though this is not a perfect description.
  - generally I expect the functions folder to be used more then the scripts... 
  

- temp 
  - Things that can and will get deleted, move out of this folder once they have a home 
  
- utilities 
  - helper functions that are used throughout the workflow or by multiple indication processing funcitons 
  

  


