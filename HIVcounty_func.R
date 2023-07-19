
read_county_data <- function(directory_loc,
                             county_demo_file_subloc,
                             county_zip_file_subloc,
                             county_zip_list,
                             county_vl_file_subloc
                             ) {
  
  county_demo.df = read_csv(paste(directory_loc, county_demo_file_subloc, sep="")) %>% 
    mutate(across(where(is.character), toupper))
  
  county_zip.df = read_csv(paste(directory_loc, county_zip_file_subloc, sep="")) %>% 
    mutate(across(where(is.character), toupper))
  
  county_vl.df = read_csv(paste(directory_loc, county_vl_file_subloc, sep="")) %>% 
    mutate(across(where(is.character), toupper))
  
  county_diagnosis.df = left_join(county_demo.df, county_zip.df,
                                  by = c( "rsd_zip_cd" = "zipcode"))
  
  county_diagnosis.df = left_join(county_diagnosis.df, county_vl.df,
                                  by = c("UCI"))
  #Filter not diagnosed in county
  

  county_diagnosis.df  = county_diagnosis.df %>% 
    filter(rsd_zip_cd %in% county_zip_list)
  
  return(county_diagnosis.df)
}