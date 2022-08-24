library(tidyverse)
library(dplyr)


data_lin_ADYs <- read.csv(file = "data_lin_ADYs_20220823.csv")

data_lin_ADYs %>% dplyr::count(migrantstatus)

## Estimating population dynamics with c and s regcodes

## Firstly I must establish when the loop/model will be ran from.
min_arry <- min(na.omit(data_lin_ADYs$arryear))
max_arry <- max(na.omit(data_lin_ADYs$depyear))
years <- min_arry:max_arry

ids <- as.character(data_lin_ADYs$IDp)

lin_pop_20220823 <-as.data.frame(matrix(NA,length(years),ncol=nrow(data_lin_ADYs)))
colnames(lin_pop_20220823) <- ids
rownames(lin_pop_20220823) <- years

# fill in the migration information 
for(x in 1:(ncol(lin_pop_20220823))){ # for each individual
  id <- colnames(lin_pop_20220823)[x]  ### get individual
  id_row <- data_lin_ADYs[data_lin_ADYs$IDp==id,]
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
    for(y in 1:(nrow(lin_pop_20220823))){ ### For each year
      year <- as.numeric(rownames(lin_pop_20220823)[y])
      lin_pop_20220823[y,x] <- ifelse((year+1)>arr_year & (year-1) < dep_year, migrant_status, NA)
    } 
  }
}

write.csv(lin_pop_20220823, "lin_pop_20220824.csv")
