## code to prepare dataset
# This code should be run ONLY when the underlying file, in data-raw, are updated
# We convert GCDKit's spider.data into a tibble of the appropriate format.

library(tidyverse)

# Inner function
.f <- function(nm,val){
  ee <- str_split(val,",")[[1]]
  nn <- str_split(nm,",")[[1]]
  ee <- as.numeric(ee)
  names(ee) <- nn

  return(ee)
}

.fv <- Vectorize(.f)

scan("data-raw/spider.data",
     skip = 1, what = list(name = "", elements = "", nvalues = ""),
     sep = "\n", quiet = TRUE) %>%
  as_tibble %>%
  mutate(scheme = .fv(elements,nvalues)) %>%
  select(-c("elements","nvalues")) %>%
  {.} -> GCDKitNormScheme
names(GCDKitNormScheme$scheme)<-GCDKitNormScheme$name

usethis::use_data(GCDKitNormScheme, overwrite = TRUE)
