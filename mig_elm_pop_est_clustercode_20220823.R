library(tidyverse)
library(dplyr)

data_elm_ADYs <- read.csv(file = "data_elm_ADYs_20220823.csv")

data_elm_ADYs %>% dplyr::count(migrantstatus)

## Estimating population dynamics with c and s regcodes

## Firstly I must establish when the loop/model will be ran from.
min_arry <- min(na.omit(data_elm_ADYs$arryear))
max_arry <- max(na.omit(data_elm_ADYs$depyear))
years <- min_arry:max_arry

ids <- as.character(data_elm_ADYs$IDp)

elm_pop_20220824 <-as.data.frame(matrix(NA,length(years),ncol=nrow(data_elm_ADYs)))
colnames(elm_pop_20220824) <- ids
rownames(elm_pop_20220824) <- years

# fill in the migration information 
for(x in 1:(ncol(elm_pop_20220824))){ # for each individual
  id <- colnames(elm_pop_20220824)[x]  ### get individual
  id_row <- data_elm_ADYs[data_elm_ADYs$IDp==id,]
  for(z in 1:nrow(id_row)){
    id_row_x <- id_row[z,]
    arr_year <- id_row_x$arryear
    dep_year <- id_row_x$depyear
    birth_location <- id_row_x$cregcode
    marriage_location <- id_row_x$sregcode
    migrant_status <- id_row_x$migrantstatus
    if(migrant_status=="emigrant") {
      migrant_status <- paste0(migrant_status,", married in ", marriage_location, sep="")
    }
    else{if(migrant_status=="resident_returned") {
      migrant_status <- paste0(migrant_status,", married in ", marriage_location, sep="")
    }
      migrant_status <- ifelse(migrant_status=="immigrant", paste0(migrant_status, ", born in ", birth_location, sep=""), migrant_status)
    }
    print(x)
    for(y in 1:(nrow(elm_pop_20220824))){ ### For each year
      year <- as.numeric(rownames(elm_pop_20220824)[y])
      elm_pop_20220824[y,x] <- ifelse((year+1)>arr_year & (year-1) < dep_year, migrant_status, NA)
    } 
  }
}

write.csv(elm_pop_20220824, "elm_pop_20220824.csv")