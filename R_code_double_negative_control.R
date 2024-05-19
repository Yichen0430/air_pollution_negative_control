library(openxlsx)
library(dplyr)

get_CI <- function(fit, int){
  beta<-fit$coef
  se<-sqrt(diag(vcov(fit)))
  p<-2 * pt(abs(beta/se), df = df.residual(fit), lower.tail = FALSE)
  RR<-cbind(beta,se,confint.default(fit),p)
  colnames(RR)<-c("beta","se","lcl","ucl","p")
  RR1<-RR[2,]
  RR1<-data.frame(t(RR1)) 
  return(RR1)
} 

######################## Main analysis ########################
RR<-data.frame()
for (i in c("Stroke","HF","AF")){
  
  if (i=="Stroke"){
    overall_data1$n<-overall_data1$n_Stroke
    overall_data1$n_lag1<-overall_data1$n_Stroke_lag1
  } else if (i=="HF"){
    overall_data1$n<-overall_data1$n_HF
    overall_data1$n_lag1<-overall_data1$n_HF_lag1
  } else if (i=="AF"){
    overall_data1$n<-overall_data1$n_AF
    overall_data1$n_lag1<-overall_data1$n_AF_lag1
  }
  
  # Single-pollutant models
  rr_single<-data.frame()
  
  # PM2.5 < 9
  df_tmp<-overall_data1%>%filter(low_pm25_9==1)
  
  mod1 <- glm(n_lag1 ~  pm25 + 
                pm25_lead1 + 
                summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC + 
                PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year), 
              data=df_tmp, offset=offset_t_lag1,family='quasipoisson',na.action=na.omit)
  df_tmp$expected_n_lag1_pm25 <- fitted(mod1)
  
  mod1_final <- glm( n ~  pm25 + 
                       expected_n_lag1_pm25 + 
                       summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC + 
                       PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                       pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                     data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  
  summary(mod1_final)
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"PM25<9"
  rr_single<-rbind(rr_single,rr1)
  
  # NO2 < 40
  df_tmp<-overall_data1%>%filter(low_no2_40==1)
  
  mod1 <- glm( n_lag1 ~  no2 + 
                 no2_lead1 + 
                 summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                 PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                 pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
               data=df_tmp, offset=offset_t_lag1,family='quasipoisson',na.action=na.omit)
  df_tmp$expected_n_lag1_no2 <- fitted(mod1)
  
  mod1_final <- glm( n ~  no2 + 
                       expected_n_lag1_no2 + 
                       summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                       PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                       pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                     data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  summary(mod1_final)
  
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"NO2<40"
  rr_single<-rbind(rr_single,rr1)

  
  # O3_warm < 45
  df_tmp<-overall_data1%>%filter(low_o3_warm_45==1)
  mod1 <- glm( n_lag1 ~  ozone_warm + 
                 ozone_warm_lead1 + 
                 summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                 PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                 pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
               data=df_tmp, offset=offset_t_lag1,family='quasipoisson',na.action=na.omit)
  df_tmp$expected_n_lag1_ozone_warm <- fitted(mod1)
  
  mod1_final <- glm( n ~  ozone_warm + 
                       expected_n_lag1_ozone_warm + 
                       summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                       PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                       pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                     data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  summary(mod1_final)
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"O3_warm<45"
  rr_single<-rbind(rr_single,rr1)
  
  
  # Three-pollutant models
  rr_three<-data.frame()
  
  # PM2.5 < 9
  df_tmp<-overall_data1%>%filter(low_pm25_9==1)
  
  mod1 <- glm( n_lag1 ~  pm25 + 
                 no2 +  ozone_warm + 
                 pm25_lead1 + 
                 summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC + 
                 PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                 pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year), data=df_tmp, offset=offset_t_lag1,family='quasipoisson',na.action=na.omit)
  df_tmp$expected_n_lag1_pm25 <- fitted(mod1)
  
  mod1_final <- glm( n ~  pm25 + 
                       no2 + ozone_warm + 
                       expected_n_lag1_pm25 + 
                       summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC + 
                       PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                       pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                     data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  summary(mod1_final)
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"PM25<9"
  rr_three<-rbind(rr_three,rr1)
  
  # NO2 < 40
  df_tmp<-overall_data1%>%filter(low_no2_40==1)
  
  mod1 <- glm( n_lag1 ~  no2 + 
                 pm25 + ozone_warm +
                 no2_lead1 +
                 summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                 PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                 pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
               data=df_tmp, offset=offset_t_lag1,family='quasipoisson',na.action=na.omit)
  df_tmp$expected_n_lag1_no2 <- fitted(mod1)
  
  mod1_final <- glm( n ~  no2 + 
                       pm25 +  ozone_warm + 
                       expected_n_lag1_no2 + 
                       summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                       PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                       pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                     data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  summary(mod1_final)
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"NO2<40"
  rr_three<-rbind(rr_three,rr1)
  
  
  # O3_warm < 45
  df_tmp<-overall_data1%>%filter(low_o3_warm_45==1)
  
  mod1 <- glm( n_lag1 ~  ozone_warm + 
                 pm25 + no2 + 
                 ozone_warm_lead1 + 
                 summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC + 
                 PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year), 
               data=df_tmp, offset=offset_t_lag1,family='quasipoisson',na.action=na.omit)
  df_tmp$expected_n_lag1_ozone_warm <- fitted(mod1)
  
  mod1_final <- glm( n ~  ozone_warm + 
                       pm25 +  no2 +
                       expected_n_lag1_ozone_warm + 
                       summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                       PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                       pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                     data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  summary(mod1_final)
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"O3_warm<45"
  rr_three<-rbind(rr_three,rr1)
  
  rr_single$pol<-"single"
  rr_three$pol<-"three"
  rr_subgroup<-rbind(rr_single,rr_three)
  rr_subgroup$outcome<-i
  
  RR<-rbind(RR,rr_subgroup)
  
}


######################## Stratified analysis ########################

# Restrict the datasets to low exposures
subgroup_data_byage1<-subgroup_data_byage%>%filter(low_pm25_9==1) # low_no2_40==1 or low_o3_warm_45==1
subgroup_data_bysex1<-subgroup_data_bysex%>%filter(low_pm25_9==1)
subgroup_data_byrace1<-subgroup_data_byrace%>%filter(low_pm25_9==1)
subgroup_data_bydual1<-subgroup_data_bydual%>%filter(low_pm25_9==1)

RR<-data.frame()
for (ii in c("Stroke","HF","AF")){
  
  if (ii=="Stroke"){
    subgroup_data_byage1$n<-subgroup_data_byage1$n
    subgroup_data_bysex1$n<-subgroup_data_bysex1$n
    subgroup_data_byrace1$n<-subgroup_data_byrace1$n
    subgroup_data_bydual1$n<-subgroup_data_bydual1$n
    subgroup_data_byage1$n_lag1<-subgroup_data_byage1$n_lag1
    subgroup_data_bysex1$n_lag1<-subgroup_data_bysex1$n_lag1
    subgroup_data_byrace1$n_lag1<-subgroup_data_byrace1$n_lag1
    subgroup_data_bydual1$n_lag1<-subgroup_data_bydual1$n_lag1
    
  } else if (ii=="HF"){
    subgroup_data_byage1$n<-subgroup_data_byage1$n_HF
    subgroup_data_bysex1$n<-subgroup_data_bysex1$n_HF
    subgroup_data_byrace1$n<-subgroup_data_byrace1$n_HF
    subgroup_data_bydual1$n<-subgroup_data_bydual1$n_HF
    subgroup_data_byage1$n_lag1<-subgroup_data_byage1$n_HF_lag1
    subgroup_data_bysex1$n_lag1<-subgroup_data_bysex1$n_HF_lag1
    subgroup_data_byrace1$n_lag1<-subgroup_data_byrace1$n_HF_lag1
    subgroup_data_bydual1$n_lag1<-subgroup_data_bydual1$n_HF_lag1
    
  } else if (ii=="AF"){
    subgroup_data_byage1$n<-subgroup_data_byage1$n_AF
    subgroup_data_bysex1$n<-subgroup_data_bysex1$n_AF
    subgroup_data_byrace1$n<-subgroup_data_byrace1$n_AF
    subgroup_data_bydual1$n<-subgroup_data_bydual1$n_AF
    subgroup_data_byage1$n_lag1<-subgroup_data_byage1$n_AF_lag1
    subgroup_data_bysex1$n_lag1<-subgroup_data_bysex1$n_AF_lag1
    subgroup_data_byrace1$n_lag1<-subgroup_data_byrace1$n_AF_lag1
    subgroup_data_bydual1$n_lag1<-subgroup_data_bydual1$n_AF_lag1
  }
  
  # Single-pollutant models
  rr_subgroup_single<-data.frame()
  
  # Subgroup - age
  for (i in unique(subgroup_data_byage1$age_gp)){
    print(i)
    sub_data<-subgroup_data_byage1[subgroup_data_byage1$age_gp==i,]
    
    mod1 <- glm(n_lag1 ~  pm25 + 
                  pm25_lead1 + 
                  summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                  PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                  pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                data=sub_data,  
                offset=offset_subgroup_t_lag1,family='quasipoisson',na.action=na.omit)
    sub_data$expected_n_lag1_pm25 <- fitted(mod1)
    
    mod1_final <- glm( n ~  pm25 + 
                         expected_n_lag1_pm25 + 
                         summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                         PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                         pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                       data=sub_data,
                       offset=offset_subgroup_t,family='quasipoisson',na.action=na.omit)
    summary(mod1_final)
    rr1<-get_CI(mod1_final,0) 
    rr1$subgroup<-i
    rr_subgroup_single<-rbind(rr_subgroup_single,rr1)
    
  }
  
  
  # Subgroup - sex
  for (i in unique(subgroup_data_bysex1$Sex_gp)){
    print(i)
    sub_data<-subgroup_data_bysex1[subgroup_data_bysex1$Sex_gp==i,]
    
    mod1 <- glm( n_lag1 ~  pm25 + 
                   pm25_lead1 + 
                   summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                   PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                   pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                 data=sub_data,  
                 offset=offset_subgroup_t_lag1,family='quasipoisson',na.action=na.omit)
    sub_data$expected_n_lag1_pm25 <- fitted(mod1)
    
    mod2_final <- glm( n ~  pm25 + 
                         expected_n_lag1_pm25 + 
                         summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                         PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                         pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                       data=sub_data, offset=offset_subgroup_t,family='quasipoisson',na.action=na.omit)
    summary(mod2_final)
    rr2<-get_CI(mod2_final,0) 
    rr2$subgroup<-i
    rr_subgroup_single<-rbind(rr_subgroup_single,rr2)
  }
  
  # Subgroup - race
  for (i in unique(subgroup_data_byrace1$race)){
    print(i)
    sub_data<-subgroup_data_byrace1[subgroup_data_byrace1$race==i,]
    
    mod1 <- glm( n_lag1 ~  pm25 + 
                   pm25_lead1 +
                   summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                   PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                   pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                 data=sub_data,  
                 offset=offset_subgroup_t_lag1,family='quasipoisson',na.action=na.omit)
    sub_data$expected_n_lag1_pm25 <- fitted(mod1)
    
    mod3_final <- glm( n ~  pm25 + 
                         expected_n_lag1_pm25 + 
                         summer_mean_rh  +  winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                         PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                         pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                       data=sub_data, offset=offset_subgroup_t,family='quasipoisson',na.action=na.omit)
    
    summary(mod3_final)
    rr3<-get_CI(mod3_final,0) 
    rr3$subgroup<-i
    rr_subgroup_single<-rbind(rr_subgroup_single,rr3)
    
  }
  
  # Subgroup - Dual
  for (i in unique(subgroup_data_bydual1$Dual_gp)){
    print(i)
    sub_data<-subgroup_data_bydual1[subgroup_data_bydual1$Dual_gp==i,]
    
    mod1 <- glm( n_lag1 ~  pm25 + 
                   pm25_lead1 + 
                   summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                   PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                   pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                 data=sub_data,  
                 offset=offset_subgroup_t_lag1,family='quasipoisson',na.action=na.omit)
    sub_data$expected_n_lag1_pm25 <- fitted(mod1)
    
    mod4_final <- glm( n ~  pm25 + 
                         expected_n_lag1_pm25 + 
                         summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                         PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                         pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                       data=sub_data, offset=offset_subgroup_t,family='quasipoisson',na.action=na.omit)
    summary(mod4_final)
    rr4<-get_CI(mod4_final,0) 
    rr4$subgroup<-i
    rr_subgroup_single<-rbind(rr_subgroup_single,rr4)
  }
  
  rr_subgroup_single<-rr_subgroup_single%>%
    mutate(subgroup=factor(subgroup,levels=c("64<Age<75","74<Age<85","Age>84",
                                             "Male","Female",
                                             "White","Black","Other",
                                             "Dual-eligible","Non-dual eligible"
    )))%>%
    arrange(subgroup)
  

  # Three-pollutant models
  rr_subgroup_three<-data.frame()
  # Subgroup - age
  for (i in unique(subgroup_data_byage1$age_gp)){
    print(i)
    sub_data<-subgroup_data_byage1[subgroup_data_byage1$age_gp==i,]
    
    mod1 <- glm( n_lag1 ~  pm25 + 
                   no2 + ozone_warm +
                   pm25_lead1 + 
                   summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                   PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                   pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                 data=sub_data,  
                 offset=offset_subgroup_t_lag1,family='quasipoisson',na.action=na.omit)
    sub_data$expected_n_lag1_pm25 <- fitted(mod1)
    
    mod1_final <- glm( n ~  pm25 + 
                         no2 + ozone_warm +
                         expected_n_lag1_pm25 + 
                         summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                         PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                         pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                       data=sub_data,
                       offset=offset_subgroup_t,family='quasipoisson',na.action=na.omit)
    summary(mod1_final)
    rr1<-get_CI(mod1_final,0) 
    rr1$subgroup<-i
    rr_subgroup_three<-rbind(rr_subgroup_three,rr1)
    
  }
  
  # Subgroup - sex
  for (i in unique(subgroup_data_bysex1$Sex_gp)){
    print(i)
    sub_data<-subgroup_data_bysex1[subgroup_data_bysex1$Sex_gp==i,]
    
    mod1 <- glm( n_lag1 ~  pm25 + 
                   no2 + ozone_warm +
                   pm25_lead1 + 
                   summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                   PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                   pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                 data=sub_data,  
                 offset=offset_subgroup_t_lag1,family='quasipoisson',na.action=na.omit)
    sub_data$expected_n_lag1_pm25 <- fitted(mod1)
    
    mod2_final <- glm( n ~  pm25 + 
                         no2 + ozone_warm +
                         expected_n_lag1_pm25 + 
                         summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                         PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                         pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                       data=sub_data, offset=offset_subgroup_t,family='quasipoisson',na.action=na.omit)
    summary(mod2_final)
    rr2<-get_CI(mod2_final,0) 
    rr2$subgroup<-i
    rr_subgroup_three<-rbind(rr_subgroup_three,rr2)
  }
  
  # Subgroup - race
  for (i in unique(subgroup_data_byrace1$race)){
    print(i)
    sub_data<-subgroup_data_byrace1[subgroup_data_byrace1$race==i,]
    
    mod1 <- glm( n_lag1 ~  pm25 + 
                   no2 + ozone_warm +
                   pm25_lead1 + 
                   summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                   PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                   pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                 data=sub_data,  
                 offset=offset_subgroup_t_lag1,family='quasipoisson',na.action=na.omit)
    sub_data$expected_n_lag1_pm25 <- fitted(mod1)
    
    mod3_final <- glm( n ~  pm25 + 
                         no2 + ozone_warm +
                         expected_n_lag1_pm25 +
                         summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                         PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                         pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                       data=sub_data, offset=offset_subgroup_t,family='quasipoisson',na.action=na.omit)
    
    summary(mod3_final)
    rr3<-get_CI(mod3_final,0) 
    rr3$subgroup<-i
    rr_subgroup_three<-rbind(rr_subgroup_three,rr3)
    
  }
  
  # Subgroup - Dual
  for (i in unique(subgroup_data_bydual1$Dual_gp)){
    print(i)
    sub_data<-subgroup_data_bydual1[subgroup_data_bydual1$Dual_gp==i,]
    
    mod1 <- glm( n_lag1 ~  pm25 + 
                   no2 +
                   ozone_warm +
                   pm25_lead1 +
                   summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                   PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                   pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                 data=sub_data,  
                 offset=offset_subgroup_t_lag1,family='quasipoisson',na.action=na.omit)
    sub_data$expected_n_lag1_pm25 <- fitted(mod1)
    
    mod4_final <- glm( n ~  pm25 +
                         no2 + ozone_warm +
                         expected_n_lag1_pm25 +
                         summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                         PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                         pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                       data=sub_data, offset=offset_subgroup_t,family='quasipoisson',na.action=na.omit)
    summary(mod4_final)
    rr4<-get_CI(mod4_final,0) 
    rr4$subgroup<-i
    rr4
    rr_subgroup_three<-rbind(rr_subgroup_three,rr4)
  }
  
    rr_subgroup_three<-rr_subgroup_three%>%
    mutate(subgroup=factor(subgroup,levels=c("64<Age<75","74<Age<85","Age>84",
                                             "Male","Female",
                                             "White","Black","Other",
                                             "Dual-eligible","Non-dual eligible"
    )))%>%
    arrange(subgroup)
  
  rr_subgroup_single$pol<-"single"
  rr_subgroup_three$pol<-"three"
  rr_subgroup<-rbind(rr_subgroup_single,rr_subgroup_three)
  rr_subgroup$outcome<-ii
  RR<-rbind(RR,rr_subgroup)
}


######################## GLM model ########################

RR<-data.frame()
for (i in c("Stroke","HF","AF")){

    if (i=="Stroke"){
      overall_data1$n<-overall_data1$n
      overall_data1$n_lag1<-overall_data1$n_lag1
    } else if (i=="HF"){
    overall_data1$n<-overall_data1$n_HF
    overall_data1$n_lag1<-overall_data1$n_HF_lag1
    } else if (i=="AF"){
    overall_data1$n<-overall_data1$n_AF
    overall_data1$n_lag1<-overall_data1$n_AF_lag1
    }

  # Single-pollutant models
  rr_single<-data.frame()

  # PM2.5 < 9
  df_tmp<-overall_data1%>%filter(low_pm25_9==1)
  
  mod1_final <- glm( n ~  pm25 + summer_mean_rh+ winter_mean_rh + average_winter_tmpC+ average_summer_tmpC + 
                  PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year), 
                  data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  summary(mod1_final)
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"PM25<9"
  rr_single<-rbind(rr_single,rr1)


  # NO2 < 40
  df_tmp<-overall_data1%>%filter(low_no2_40==1)
  
  mod1_final <- glm( n ~  no2 + 
                  summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                  PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                  pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                 data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  
  summary(mod1_final)
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"NO2<40"
  rr_single<-rbind(rr_single,rr1)


  # Warm-season O3 < 45
  df_tmp<-overall_data1%>%filter(low_o3_warm_45==1)
  
  mod1_final <- glm( n ~  ozone_warm + 
                  summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                  PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                  pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                 data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  
  summary(mod1_final)
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"O3_warm<45"
  rr_single<-rbind(rr_single,rr1)


  # Three-pollutant models
  rr_three<-data.frame()
  
  # PM2.5 < 9
  df_tmp<-overall_data1%>%filter(low_pm25_9==1)
  mod1_final <- glm( n ~  pm25 + 
                  no2 +  ozone_warm +
                  summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC + 
                  PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                  pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year), data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  
  summary(mod1_final)
  rr1<-get_CI(mod1_final,0)
  rr1$exposure<-"PM25<9"
  rr_three<-rbind(rr_three,rr1)
  
  # NO2 < 40
  df_tmp<-overall_data1%>%filter(low_no2_40==1)
  
  mod1_final <- glm( n ~  no2 + 
                  pm25 + ozone_warm +
                  summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                  PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                  pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                 data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  
  summary(mod1_final)
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"NO2<40"
  rr_three<-rbind(rr_three,rr1)
  
  
  # Warm-season O3 < 45
  df_tmp<-overall_data1%>%filter(low_o3_warm_45==1)
  
  mod1_final <- glm( n ~  ozone_warm + 
                  pm25 + no2 + 
                  summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                  PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                  pct_blk + hispanic + poverty + popdensity + medianhousevalue + medhouseholdincome+pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                 data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  
  summary(mod1_final)
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"O3_warm<45"
  rr_three<-rbind(rr_three,rr1)
  
  rr_single$pol<-"single"
  rr_three$pol<-"three"
  rr_subgroup<-rbind(rr_single,rr_three)
  
  rr_subgroup$outcome<-i
  RR<-rbind(RR,rr_subgroup)
}


######################## Exposure-response curves ########################
library(mgcv)
library(splines)
library(ggeffects)
library(ggplot2)
library(cowplot)

p1_pm25 <- quantile(overall_data1$pm25, 0.01)
p99_pm25 <- quantile(overall_data1$pm25, 0.99)

p1_no2 <- quantile(overall_data1$no2, 0.01)
p99_no2 <- quantile(overall_data1$no2, 0.99)

p1_o3 <- quantile(overall_data1$ozone_warm, 0.01)
p99_o3 <- quantile(overall_data1$ozone_warm, 0.99)

######################## Stroke ########################
# PM2.5 < 9
df_lowpm_9<-overall_data%>%filter(included_low_pm25_9==1)
df_lowpm_9 <- df_lowpm_9[df_lowpm_9$pm25 > p1_pm25 & df_lowpm_9$pm25 < p99_pm25,]

mod1 <- gam(n_Stroke ~ ns(pm25,df=3) + no2 + ozone_warm + 
              summer_mean_rh + winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC + 
              PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
              pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + 
              education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year) + offset(log(total_number)),
            data=df_lowpm_9,family='quasipoisson',na.action=na.omit)

predicts1<-ggpredict(mod1, terms = "pm25", condition = c(total_number = median(df_lowpm_9$total_number))) 
predicts1$pollutant<-"pm25"

erplot_pm25<-ggplot(predicts1, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+theme_bw()+
  xlab(expression('PM'[2.5]~'(ug/m'^3~')')) +
  ylab(expression(paste("Number of hospitalizations")))
erplot_pm25


# NO2 < 40
df_lowno2_40<-overall_data%>%filter(included_low_no2_40==1)
df_lowno2_40 <- df_lowno2_40[df_lowno2_40$no2 > p1_no2 & df_lowno2_40$no2 < p99_no2,]

mod2 <- gam(n_Stroke ~  ns(no2,df=3) + pm25 +  ozone_warm + 
              summer_mean_rh  + winter_mean_rh + average_winter_tmpC + average_summer_tmpC + 
              PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
              pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + 
              education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year) + offset(log(total_number)),
            data=df_lowno2_40, family='quasipoisson', na.action=na.omit)

predicts2<-ggpredict(mod2, terms = "no2",condition = c(total_number = median(df_lowno2_40$total_number)))
predicts2$pollutant<-"no2"

erplot_no2<-ggplot(predicts2, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+theme_bw()+
  xlab(expression('NO'[2]~'(μg/m'^3~')')) +
  ylab(expression(paste("Number of hospitalizations")))
erplot_no2

# Warm-season O3 < 45
df_lowo3_45<-overall_data%>%filter(included_low_o3_warm_45==1)
df_lowo3_45 <- df_lowo3_45[df_lowo3_45$ozone_warm > p1_o3 & df_lowo3_45$ozone_warm < p99_o3,]

mod3 <- gam(n_Stroke ~  ns(ozone_warm,df=3) + pm25 +  no2 +
              summer_mean_rh + winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
              PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct +
              pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + 
              education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year) + offset(log(total_number)),
            data=df_lowo3_45, family='quasipoisson',na.action=na.omit)

predicts3<-ggpredict(mod3, terms = "ozone_warm", condition = c(total_number = median(df_lowo3_45$total_number)))
predicts3$pollutant<-"ozone_warm"

erplot_o3<-ggplot(predicts3, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+theme_bw()+
  xlab(expression('Warm-season O'[3]~'(μg/m'^3~')')) + 
  ylab(expression(paste("Number of hospitalizations")))
erplot_o3


erplot1<-plot_grid(erplot_pm25, erplot_no2,erplot_o3,
                   align="hv",nrow = 1,ncol = 3, scale=0.92) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))  

predicts_Stroke<-rbind(predicts1,predicts2,predicts3)
predicts_Stroke$outcome<-"Stroke"

######################## HF ########################
# PM2.5 < 9
df_lowpm_9<-overall_data%>%filter(included_low_pm25_9==1)
df_lowpm_9 <- df_lowpm_9[df_lowpm_9$pm25 > p1_pm25 & df_lowpm_9$pm25 < p99_pm25,]

mod1 <- gam(n_HF ~ ns(pm25,df=3) + no2 + ozone_warm + 
              summer_mean_rh + winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC + 
              PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
              pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + 
              education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year) + offset(log(total_number)),
            data=df_lowpm_9,family='quasipoisson',na.action=na.omit)

predicts1<-ggpredict(mod1, terms = "pm25", condition = c(total_number = median(df_lowpm_9$total_number))) 
predicts1$pollutant<-"pm25"

erplot_pm25<-ggplot(predicts1, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+theme_bw()+
  xlab(expression('PM'[2.5]~'(ug/m'^3~')')) +
  ylab(expression(paste("Number of hospitalizations")))
erplot_pm25


# NO2 < 40
df_lowno2_40<-overall_data%>%filter(included_low_no2_40==1)
df_lowno2_40 <- df_lowno2_40[df_lowno2_40$no2 > p1_no2 & df_lowno2_40$no2 < p99_no2,]

mod2 <- gam(n_HF ~  ns(no2,df=3) + pm25 +  ozone_warm + 
              summer_mean_rh  + winter_mean_rh + average_winter_tmpC + average_summer_tmpC + 
              PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
              pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + 
              education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year) + offset(log(total_number)),
            data=df_lowno2_40, family='quasipoisson', na.action=na.omit)

predicts2<-ggpredict(mod2, terms = "no2",condition = c(total_number = median(df_lowno2_40$total_number)))
predicts2$pollutant<-"no2"

erplot_no2<-ggplot(predicts2, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+theme_bw()+
  xlab(expression('NO'[2]~'(μg/m'^3~')')) +
  ylab(expression(paste("Number of hospitalizations")))
erplot_no2


# Warm-season O3 < 45
df_lowo3_45<-overall_data%>%filter(included_low_o3_warm_45==1)
df_lowo3_45<-df_lowo3_45[df_lowo3_45$ozone_warm > p1_o3 & df_lowo3_45$ozone_warm < p99_o3,]

mod3 <- gam(n_HF ~  ns(ozone_warm,df=3) + pm25 +  no2 +
              summer_mean_rh + winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
              PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct +
              pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + 
              education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year) + offset(log(total_number)),
            data=df_lowo3_45, family='quasipoisson',na.action=na.omit)

predicts3<-ggpredict(mod3, terms = "ozone_warm",condition = c(total_number = median(df_lowpm_9$total_number)))
predicts3$pollutant<-"ozone_warm"

erplot_o3<-ggplot(predicts3, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+theme_bw()+
  xlab(expression('Warm-season O'[3]~'(μg/m'^3~')')) + 
  ylab(expression(paste("Number of hospitalizations")))
erplot_o3

erplot2<-plot_grid(erplot_pm25, erplot_no2,erplot_o3,
                   align="hv",nrow = 1,ncol = 3, scale=0.92) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))  

predicts_HF<-rbind(predicts1,predicts2,predicts3)
predicts_HF$outcome<-"HF"

######################## AF ########################
# PM2.5 < 9
df_lowpm_9<-overall_data%>%filter(included_low_pm25_9==1)
df_lowpm_9 <- df_lowpm_9[df_lowpm_9$pm25 > p1_pm25 & df_lowpm_9$pm25 < p99_pm25,]

mod1 <- gam(n_AF ~ ns(pm25,df=3) + no2 + ozone_warm + 
              summer_mean_rh + winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC + 
              PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
              pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + 
              education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year) + offset(log(total_number)),
            data=df_lowpm_9,family='quasipoisson',na.action=na.omit)

predicts1<-ggpredict(mod1, terms = "pm25", condition = c(total_number = median(df_lowpm_9$total_number)))
predicts1$pollutant<-"pm25"

erplot_pm25<-ggplot(predicts1, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+theme_bw()+
  xlab(expression('PM'[2.5]~'(ug/m'^3~')')) +
  ylab(expression(paste("Number of hospitalizations")))
erplot_pm25

# NO2 < 40
df_lowno2_40<-overall_data%>%filter(included_low_no2_40==1)
df_lowno2_40 <- df_lowno2_40[df_lowno2_40$no2 > p1_no2 & df_lowno2_40$no2 < p99_no2,]

mod2 <- gam(n_AF ~  ns(no2,df=3) + pm25 +  ozone_warm + 
              summer_mean_rh  + winter_mean_rh + average_winter_tmpC + average_summer_tmpC + 
              PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
              pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + 
              education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year) + offset(log(total_number)),
            data=df_lowno2_40, family='quasipoisson', na.action=na.omit)

predicts2<-ggpredict(mod2, terms = "no2",condition = c(total_number = median(df_lowno2_40$total_number)))
predicts2$pollutant<-"no2"

erplot_no2<-ggplot(predicts2, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+theme_bw()+
  xlab(expression('NO'[2]~'(μg/m'^3~')')) +
  ylab(expression(paste("Number of hospitalizations")))
erplot_no2

# Warm-season O3 < 45
df_lowo3_45<-overall_data%>%filter(included_low_o3_warm_45==1)
df_lowo3_45 <- df_lowo3_45[df_lowo3_45$ozone_warm > p1_o3 & df_lowo3_45$ozone_warm < p99_o3,]

mod3 <- gam(n_AF ~  ns(ozone_warm,df=3) + pm25 +  no2 +
              summer_mean_rh + winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
              PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct +
              pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + 
              education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year) + offset(log(total_number)),
            data=df_lowo3_45, family='quasipoisson',na.action=na.omit)

predicts3<-ggpredict(mod3, terms = "ozone_warm",condition = c(total_number = median(df_lowo3_45$total_number)))
predicts3$pollutant<-"ozone_warm"

erplot_o3<-ggplot(predicts3, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+theme_bw()+
  xlab(expression('Warm-season O'[3]~'(μg/m'^3~')')) + 
  ylab(expression(paste("Number of hospitalizations")))
erplot_o3

erplot3<-plot_grid(erplot_pm25, erplot_no2,erplot_o3,
                   align="hv",nrow = 1,ncol = 3, scale=0.92) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))  

predicts_AF<-rbind(predicts1,predicts2,predicts3)
predicts_AF$outcome<-"AF"


p1<-erplot1 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))  + 
  ggtitle("(A) Stroke") + theme(plot.title = element_text(size = 13, hjust=0.035, vjust=0, face = "bold"))

p2<-erplot2 + ggtitle("(B) Heart Failure") + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))  + 
  theme(plot.title = element_text(size = 13, hjust=0.035, vjust=0, face = "bold"))

p3<-erplot3 + ggtitle("(C) Atrial Fibrillation and Flutter") + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))  + 
  theme(plot.title = element_text(size = 13, hjust=0.035, vjust=0, face = "bold"))

plot_grid(p1, p2, p3,align="hv",nrow = 3,ncol = 1) + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) 


######################## Sensitivity analysis ########################
library(performance)

QAIC <- function(model) {
  phi <- summary(model)$dispersion
  loglik <- sum(dpois(model$y, model$fitted.values, log=TRUE))
  return(-2*loglik + 2*summary(model)$df[3]*phi)
}

GET_performance<-function(model,data){
  perform_matrix<-model_performance(model)
  para<-cbind(QAIC(model),
              (cor(fitted(model),data$n, method="spearman"))^2)
  colnames(para)<-c("QAIC","Pseudo_R2") 
  para<-data.frame(para)
  return(para)
}

RR<-data.frame()
Per<-data.frame()
for (i in c("Stroke","HF","AF")){
  
  if (i=="Stroke"){
    overall_data$n<-overall_data$n_Stroke
    overall_data$n_lag1<-overall_data$n_Stroke_lag1
  } else if (i=="HF"){
    overall_data$n<-overall_data$n_HF
    overall_data$n_lag1<-overall_data$n_HF_lag1
  } else if (i=="AF"){
    overall_data$n<-overall_data$n_AF
    overall_data$n_lag1<-overall_data$n_AF_lag1
  }
  
  df_tmp<-overall_data%>%filter(low_pm25_9==1)
  
  rr_single<-data.frame()
  per_single<-data.frame()
  mod1 <- glm(n_lag1 ~  pm25 + pm25_lead1, data=df_tmp, offset=offset_t_lag1,family='quasipoisson',na.action=na.omit)
  df_tmp$expected_n_lag1_pm25 <- fitted(mod1)
  
  mod1_final <- glm( n ~  pm25 + expected_n_lag1_pm25 + 
                       summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC + 
                       PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                       pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year), data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  
  per1<-GET_performance(mod1_final,df_tmp)
  per1$exposure<-"PM2.5<9"
  per_single<-rbind(per_single,per1)
  
  summary(mod1_final)
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"PM2.5<9"
  rr_single<-rbind(rr_single,rr1)
  
  # NO2
  df_tmp<-overall_data%>%filter(low_no2_40==1)
  
  mod1 <- glm( n_lag1 ~  no2 + no2_lead1, data=df_tmp, offset=offset_t_lag1,family='quasipoisson',na.action=na.omit)
  df_tmp$expected_n_lag1_no2 <- fitted(mod1)
  
  mod1_final <- glm( n ~  no2 + 
                       expected_n_lag1_no2 + 
                       summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                       PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                       pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                     data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  summary(mod1_final)
  
  per1<-GET_performance(mod1_final,df_tmp)
  per1$exposure<-"NO2<40"
  per_single<-rbind(per_single,per1)
  
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"NO2<40"
  rr_single<-rbind(rr_single,rr1)

  
  # O3_warm < 45
  df_tmp<-overall_data%>%filter(low_o3_warm_45==1)
  
  mod1 <- glm( n_lag1 ~  ozone_warm + ozone_warm_lead1, data=df_tmp, offset=offset_t_lag1,family='quasipoisson',na.action=na.omit)
  df_tmp$expected_n_lag1_ozone_warm <- fitted(mod1)
  
  mod1_final <- glm( n ~  ozone_warm + 
                       expected_n_lag1_ozone_warm + 
                       summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                       PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                       pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                     data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  summary(mod1_final)
  
  per1<-GET_performance(mod1_final,df_tmp)
  per1$exposure<-"O3_warm<45"
  per_single<-rbind(per_single,per1)
  
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"O3_warm<45"
  rr_single<-rbind(rr_single,rr1)
  
  
  # Three-pollutant models
  rr_three<-data.frame()
  per_three<-data.frame()
  df_tmp<-overall_data%>%filter(low_pm25_9==1)
  
  mod1 <- glm( n_lag1 ~  pm25 + pm25_lead1, data=df_tmp, offset=offset_t_lag1,family='quasipoisson',na.action=na.omit)
  df_tmp$expected_n_lag1_pm25 <- fitted(mod1)
  
  mod1_final <- glm( n ~  pm25 + 
                       no2 + ozone_warm + 
                       expected_n_lag1_pm25 +
                       summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC + 
                       PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                       pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                     data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  summary(mod1_final)
  
  per1<-GET_performance(mod1_final,df_tmp)
  per1$exposure<-"PM2.5<9"
  per_three<-rbind(per_three,per1)
  
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"PM2.5<9"
  rr_three<-rbind(rr_three,rr1)
  
  # NO2
  df_tmp<-overall_data%>%filter(low_no2_40==1)
  
  mod1 <- glm( n_lag1 ~  no2 + no2_lead1, data=df_tmp, offset=offset_t_lag1,family='quasipoisson',na.action=na.omit)
  df_tmp$expected_n_lag1_no2 <- fitted(mod1)
  
  mod1_final <- glm( n ~  no2 + 
                       pm25 +  ozone_warm + 
                       expected_n_lag1_no2 + 
                       summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                       PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                       pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                     data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  summary(mod1_final)
  
  per1<-GET_performance(mod1_final,df_tmp)
  per1$exposure<-"NO2<40"
  per_three<-rbind(per_three,per1)
  
  rr1<-get_CI(mod1_final,0) 
  rr1$exposure<-"NO2<40"
  rr_three<-rbind(rr_three,rr1)
  
  # O3_warm < 45
  df_tmp<-overall_data%>%filter(low_o3_warm_45==1)
  
  mod1 <- glm( n_lag1 ~  ozone_warm + ozone_warm_lead1, data=df_tmp, offset=offset_t_lag1,family='quasipoisson',na.action=na.omit)
  df_tmp$expected_n_lag1_ozone_warm <- fitted(mod1)
  
  mod1_final <- glm( n ~  ozone_warm + 
                       pm25 +  no2 +
                       expected_n_lag1_ozone_warm + 
                       summer_mean_rh  +   winter_mean_rh  + average_winter_tmpC+ average_summer_tmpC +
                       PctEye + PctLDL + Pctmam + LungCancerRate + amb_visit_pct + a1c_exm_pct + 
                       pct_blk + hispanic + poverty + popdensity + medianhousevalue + pct_owner_occ + education + nearest_hospital_km + smoke_rate + mean_bmi + as.factor(year),
                     data=df_tmp, offset=offset_t,family='quasipoisson',na.action=na.omit)
  summary(mod1_final)
  
  per1<-GET_performance(mod1_final,df_tmp)
  per1$exposure<-"O3_warm<45"
  per_three<-rbind(per_three,per1)
  
  rr1<-get_CI(mod1_final,0)
  rr1$exposure<-"O3_warm<45"
  rr_three<-rbind(rr_three,rr1)
  
  per_single$pol<-"single"
  per_three$pol<-"three"
  per_subgroup<-rbind(per_single,per_three)
  per_subgroup$outcome<-i
  
  rr_single$pol<-"single"
  rr_three$pol<-"three"
  rr_subgroup<-rbind(rr_single,rr_three)
  rr_subgroup$outcome<-i
  
  RR<-rbind(RR,rr_subgroup)
  Per<-rbind(Per,per_subgroup)
  
}

