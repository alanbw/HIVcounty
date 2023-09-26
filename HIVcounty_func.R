pad.zip <-  function(x){str_pad(x,side = 'left',pad = '0',width = 5)}

pvalFormat <- function(p.values, method = 'none', replace = FALSE, math = TRUE,empty.cell.value = '-'){
  ## Formats p-values for reports, can report adjusted pvalues
  ##    Inputs:
  ##       - p.value: numeric p-value
  ##       - method: pvalue adjustment, passed to p.adjust.methods
  ##       - replace: if TRUE, replaces p-values with their adjusted value
  ##    Outputs:
  ##       - out: formatted p-value
  
  p.values <- suppressWarnings(as.numeric(p.values))
  out      <- rep(NA, length(p.values))
  sig      <- p.adjust(p.values, method)
  if(replace) p.values <- sig
  
  for(i in 1:length(p.values)){
    if(is.na(p.values[i])){out[i] <- NA}else{
      if(p.values[i] >= .001){
        out[i] <- formatC(p.values[i], format = 'f', digits = 3)
      }
      
      if(p.values[i] < .001){
        out[i] <- '< .001'
      }
      
      if(sig[i] > 0.01 & sig[i] <= 0.05){
        out[i] <- paste(out[i], '*', sep = '')
      }
      
      if(sig[i] > 0.001 & sig[i] <= 0.01) {
        out[i] <- paste(out[i], '**', sep = '')
      }
      
      if(sig[i] <= 0.001){
        out[i] <- paste(out[i], '***', sep = '')
      }}
  }
  
  out[is.na(out)] <- empty.cell.value
  return(out)
}

read_county_data <- function(directory_loc,
                             county_demo_file_subloc,
                             county_zip_file_subloc,
                             county_zip_list
                             ) {
  
  county_demo.df = read_csv(paste(directory_loc, county_demo_file_subloc, sep="")) %>% 
    mutate(across(where(is.character), toupper)) %>%
    mutate(across(.cols = c(rsd_zip_cd,cur_zip_cd), pad.zip))
  
  county_zip.df = read_csv(paste(directory_loc, county_zip_file_subloc, sep="")) %>%
    mutate(across(where(is.character), toupper)) %>%
    mutate(across(.cols = c(rsd_zip_cd), pad.zip))

  
  county.df = left_join(county_demo.df, county_zip.df,
                                  by = c( "rsd_zip_cd" = "rsd_zip_cd"))
  
  #county.df  = county_demo.df
  
  #Filter not diagnosed in county
  

  county.df  = county.df %>% 
    filter(rsd_zip_cd %in% county_zip_list) %>%
    filter(cur_zip_cd %in% county_zip_list)
  
  county.df = county.df %>% 
    filter(`Diagnosis year` <= 2022) %>%
    filter(is.na(death_year) | death_year > 2022)
  
  return(county.df)
}

univariate_reg <- function(county.df,
                           predictors,
                           outcome_var,
                           random_effect_var) {
  
  uni_reg_res.df = tibble(
    variable = NULL,
    estimate_uni = NULL,
    se_uni = NULL,
    p_val_uni = NULL
  )
  
  for (reg_variable_ind in reg_variable.vec) {
    
    regression_formula <- paste0(outcome_var, " ~ ", reg_variable_ind, " + (1 | ", random_effect_var , ")")
    
    uni_reg = glmer(as.formula(regression_formula), 
                    family = binomial,
                    data = county.df)
    
    uni_reg_sum = summary(uni_reg)
    uni_reg_res_TEMP.df = tibble(
      variable = reg_variable_ind,
      estimate_uni = uni_reg_sum$coefficients[2,c(1)],
      se_uni = uni_reg_sum$coefficients[2,c(2)],
      p_val_uni = uni_reg_sum$coefficients[2,c(4)]
    )
    
    uni_reg_res.df = bind_rows(uni_reg_res.df, uni_reg_res_TEMP.df)
  }
  
  
  return(list(model = uni_reg,
              model.summary = uni_reg_sum,
              summary.table = uni_reg_res.df))
}

Lasso_tune_lambda <- function(county.df,
                              predictors,
                              outcome_var,
                              random_effect_var) {
  
  
  county.df_TEMP = county.df %>% 
    select(all_of(predictors), sym(outcome_var), sym(random_effect_var)) %>%
    mutate(random_effect_var_fac =  as.factor(!! rlang::sym(random_effect_var))) %>%
    mutate(outcome_var =  !! rlang::sym(outcome_var)) %>%
    na.omit()
  
  regression_formula <- as.formula(paste0(outcome_var, " ~ ", 
                                          paste0(predictors, 
                                                 collapse = " + ")))
  
  N<-nrow(county.df_TEMP)
  ind<-sample(N,N)
  lambda <- seq(500,0,by=-5)
  #used for QC to shorten run time
  # lambda <- seq(500,0,by=-100)
  
  family <- binomial(link = logit)
  
  kk<-5
  nk <- floor(N/kk)
  
  Devianz_ma<-matrix(Inf,ncol=kk,nrow=length(lambda))
  
  ## first fit good starting model
  PQL<-glmmPQL(outcome_var~1,
               random = ~1|random_effect_var_fac,
               family=family,
               data=county.df_TEMP)
  
  Delta.start<-c(as.numeric(PQL$coef$fixed),rep(0,6),as.numeric(t(PQL$coef$random$random_effect_var_fac)))
  Q.start<-as.numeric(VarCorr(PQL)[1,1])
  
  ## loop over the folds  
  for(j in 1:length(lambda))
  {
    print(paste("Iteration ", j,sep=""))
    
    for (i in 1:kk)
    {
      if (i < kk)
      {
        indi <- ind[(i-1)*nk+(1:nk)]
      }else{
        indi <- ind[((i-1)*nk+1):N]
      }
      
      county.train<-county.df_TEMP[-indi,]
      county.test<-county.df_TEMP[indi,]
      
      glm2 <- try(glmmLasso(fix = regression_formula,
                            rnd = list(random_effect_var_fac=~1),
                            family = binomial(link = logit), 
                            data =county.train,
                            lambda=lambda[j],
                            switch.NR=FALSE,
                            final.re=FALSE,
                            control=list()), #list(start=Delta.start,q_start=Q.start)),
                  silent=TRUE) 
      
      if(!inherits(glm2, "try-error"))
      {  
        y.hat<-predict(glm2,as.data.frame(county.test))     
        Devianz_ma[j,i]<-sum(family$dev.resids(county.test$outcome_var,y.hat,wt=rep(1,length(y.hat))))
      }
    }
    print(sum(Devianz_ma[j,]))
  }
  
  Devianz_vec<-apply(Devianz_ma,1,sum)
  opt2<-which.min(Devianz_vec)
  lambda_tune = lambda[opt2]
  
  return(lambda_tune)
}

Lasso_reg <- function(county.df,
                      predictors,
                      outcome_var,
                      random_effect_var,
                      lambda_tune) {
  
  county.df_TEMP = county.df %>% 
    select(all_of(predictors), sym(outcome_var), sym(random_effect_var)) %>%
    mutate(random_effect_var_fac =  as.factor(!! rlang::sym(random_effect_var))) %>%
    mutate(outcome_var =  !! rlang::sym(outcome_var)) %>%
    na.omit()
  
  regression_formula <- as.formula(paste0(outcome_var, " ~ ", 
                                          paste0(predictors, 
                                                 collapse = " + ")))
  
  glm2_final <- glmmLasso(fix = regression_formula,
                          rnd = list(random_effect_var_fac=~1),
                          family = binomial(link = logit), 
                          data =county.df_TEMP,
                          lambda=lambda_tune,
                          switch.NR=FALSE,
                          final.re=TRUE,
                          control=list()) #list(start=Delta.start,q_start=Q.start))
  
  multi_reg_sum = summary(glm2_final)
  
  multi_reg_res.df = tibble(
    variable = rownames(multi_reg_sum$coefficients)[c(2:nrow(multi_reg_sum$coefficients))],
    estimate_multi = multi_reg_sum$coefficients[2:nrow(multi_reg_sum$coefficients),c(1)],
    se_multi = multi_reg_sum$coefficients[2:nrow(multi_reg_sum$coefficients),c(2)],
    p_val_multi = multi_reg_sum$coefficients[2:nrow(multi_reg_sum$coefficients),c(4)]
  )
  
  return(return(list(model = glm2_final,
                     model.summary = multi_reg_sum,
                     summary.table = multi_reg_res.df)))
}

geo_map <- function(county.df,
                    county_zip_list,
                    outcome_var,
                    geo_zip_var,
                    title_a) {
  
  zcta_data <- get_acs(
    geography = "zcta",
    variables = "B19013_001",
    zcta = county_zip_list, 
    geometry = TRUE
  )
  
  county_summary.df = county.df %>%
    group_by(!! rlang::sym(geo_zip_var)) %>%
    summarize(n_tot = n(),
              n_outcome = sum(!! rlang::sym(outcome_var))) %>%
    ungroup() %>%
    mutate(per_outcome = n_outcome/n_tot) %>%
    mutate(rsd_zip_cd_cat = as.character(!! rlang::sym(geo_zip_var)))
  
  
  zcta_level.df = left_join(zcta_data,
                            county_summary.df,
                            by = c("GEOID" = "rsd_zip_cd_cat"))
  
  zcta_level.df = left_join(zcta_level.df,
                            zip_code_db %>% select(zipcode, lat, lng),
                            by = c("GEOID" = "zipcode")) %>%
    filter(!is.na(lat))
  
  break_num = 3
  pal_fun <- colorQuantile("YlOrRd", NULL, n = break_num)
  p_popup <- paste0("<strong>", title_a, ": </strong>", zcta_level.df$per_outcome)
  breaks_qt <- classIntervals(zcta_level.df$per_outcome, n = break_num, style = "quantile")
  
  HIV_geomap = leaflet(zcta_level.df) %>%
    addPolygons(
      stroke = FALSE, 
      fillColor = ~pal_fun(per_outcome),
      fillOpacity = 0.8, smoothFactor = 0.5,
      popup = p_popup) %>%
    addTiles()  %>%
    addLegend("bottomright", 
              colors = brewer.pal(break_num, "YlOrRd"), 
              labels = paste0("up to ", format(breaks_qt$brks[-1], digits = 2)),
              title =  title_a) %>%
    addCircleMarkers(zcta_level.df$lng, zcta_level.df$lat, radius=zcta_level.df$n_tot/50, 
                     color = "blue", 
                     stroke = FALSE, fillOpacity = 0.5) 
  
  return(HIV_geomap)
  
}
