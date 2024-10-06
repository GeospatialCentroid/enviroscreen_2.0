

### move to a better location 
#### 
Force <- FALSE
if(Force == TRUE){
  googledrive::drive_download(file = as_id("https://drive.google.com/file/d/16ZK9gxpeug93EEgGyBiSsh3MH3STsr4Z/view?usp=drive_link"),
                              path = "data/raw/ejscreen/ejscreenDataBlocks.zip")
  #unzip
  unzip(zipfile = "data/raw/ejscreen/ejscreenDataBlocks.zip",exdir = "data/raw/ejscreen")
  
  ## census tracts
  googledrive::drive_download(file = as_id("https://drive.google.com/file/d/1am5SiLJidYQDsg6DWDMNrvvd6vo2xVhu/view?usp=drive_link"),
                              path = "data/raw/ejscreen/ejscreenDataTracts.zip")
  #unzip
  unzip(zipfile = "data/raw/ejscreen/ejscreenDataTracts.zip",exdir = "data/raw/ejscreen")
}
