###
## moving and naming files by date  
## Author Ames Fowler 
## inital date: 20.11.20
##
##


library(purrr)
library(tidyverse)
library(data.table)


# define date----------------------------------------
date <- Sys.Date() # add days as needed 
# date <- "2020-11-20" %>% as.Date()

# Define paths -----------------------
path <-  "G:/DCIM"
outpath <- paste0("./rawdata/Estes/",format(date, "%y%m%d"))

ifelse(dir.exists(outpath), print("Dir exist"),dir.create(outpath)) 
  
# list all folders ---------------- 
dirs <- list.dirs(path = path)
?list.dirs()
 

# list all files that are jpeg of the defined date---------------- 
# Function  to map
file_list_by_date <- function (path, date=date){
  ldir <-  normalizePath(path)
  files <- list.files(ldir, full.names = T) %>% as.data.frame() 
  names(files) <- "fullnames"
  files <- files %>% 
    mutate(mtime =  file.info(dir(path = ldir, full.names = TRUE), extra_cols = FALSE)[,"mtime"] %>%
           as.Date())%>% 
    subset(.,grepl(".JPG",files$fullnames))%>%
    subset(mtime == date) 
  files
  return(files)
}



to_move <- map(.x = dirs, .f = ~ file_list_by_date(path = .x, date = date)) %>% Reduce(full_join,.)

#Write a new name ---------------------------

to_move <- to_move %>% 
  mutate(out_name = paste0(outpath,"/DJI_", fullnames %>%
                             row_number() %>% 
                             sprintf("%04d", .)
                           ,".JPG"))


# Move the Files------------------
#to_move <- to_move[1:3,]
map2(.x =to_move$fullnames, .y =to_move$out_name, .f=~ file.copy(from = .x, to = .y, copy.date = TRUE ))
