## code to prepare dataset
# This code should be run ONLY when the underlying file, in data-raw, are updated
# We convert GCDKit's spider.data into a tibble of the appropriate format.

library(tidyverse)

##### Normalization values ########
ee <- scan("data-raw/spider.data",
           skip = 1, what = list(name = "", elements = "", nvalues = ""),
           sep = "\n", quiet = TRUE)
nm <- map(ee$elements,function(x){strsplit(x,",")[[1]]})
vl <- map(ee$nvalues,function(x){as.numeric(strsplit(x,",")[[1]] ) })

GCDKitNormScheme <-map2(nm,vl,function(x,y){names(y)<-x;return(y)} )
names(GCDKitNormScheme) <- ee$name

usethis::use_data(GCDKitNormScheme, overwrite = TRUE)


#### Atacazo dataset (for testing purposes) ####
atacazo <- read_delim("data-raw/atacazo.csv",delim=";")
usethis::use_data(atacazo, overwrite = TRUE)
