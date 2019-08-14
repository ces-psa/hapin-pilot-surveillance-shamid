#------------------------------------------------------------------------------*
# Code the HAPIN Pneumonia case definition <12 yrs old
#------------------------------------------------------------------------------*
if (!"all_c36" %in% ls()) source ("scripts/read_and_harmonize.R")

c36 <- all_c36 %>% transmute(
  # HAPIN Pneumonia case definition for <12 months old
      fast_breathing=case_when(age<2~
                resp_rate_man_1>60 | resp_rate_man_2>60,
                age<12~
                resp_rate_man_1>50 | resp_rate_man_2>50,
                age<24~
                resp_rate_man_1>40 | resp_rate_man_2>40),
      chest_indrawing=case_when(age<2~
                chest_indrawing==1 |
                chest_indrawing_severe==1,
                age>=2~
                chest_indrawing==1),
      
      #Keep these variables in dataframe
      age,
      record_id,
      date,
      cough,
      diff_breath,
      nasal_flare,
      head_nod,
      tracheal_tugging,
      intercostal_recession=NA,
      cyanosis,
      stridor,
      grunting,
      
      diff_breath_obs = fast_breathing | 
        (chest_indrawing == 1 & !is.na(chest_indrawing))|
        (nasal_flare == 1 & !is.na(nasal_flare)) |           
        (tracheal_tugging == 1 & !is.na(tracheal_tugging)) |   
        (head_nod == 1 & !is.na(head_nod)) |
        (intercostal_recession==1 & !is.na(intercostal_recession)) |
        (cyanosis == 1 & !is.na(cyanosis)) | 
        (stridor == 1 & !is.na(stridor)) | 
        (grunting == 1 & !is.na(grunting)),
        
      general_danger=
          (diff_drinking == 1 & !is.na(diff_drinking)) |
          #diff_drinking includes difficulty breastfeeding      
          (convulsion_now == 1 & !is.na(convulsion_now)) | 
          (convulsion_any == 1 & !is.na(convulsion_any)) |       
          #   (stridor == 1 & !is.na(stridor)) |     
          (lethargy == 1 & !is.na(lethargy)) | 
          (vomits_everything == 1 & !is.na(vomits_everything)) |  
          #in the first data set, vomits everything was asked but not persistent vomiting
          (vomit_persist == 1 & !is.na(vomit_persist)) |
          (malnutrition == 1 & !is.na(malnutrition)) |
          (hypothermia == 1 & !is.na(hypothermia)) |
          (fever == 1 & !is.na(fever)) ,
        
      hypoxemia =    (oxyhb_avg_massimo <=92 | oxyhb_avg_nelcor <=92),
        #not sure if this should be massimo or nelcor and if if should be average or any reading; leaving as either for now
             #oxy_admin_device %in% 
        #( 1 = nasal cannula -- is this always high flow nasal cannula? 
          # 2 face mask 
          # 3 ventilator 
          # 4 cephalic chamber
          # 5 positive airway pressure
        #)
        
     #create variable for HAPIN pneumonia
     
     hapin_pneumo = (
       (cough == 1 & !is.na(cough)) |
         (diff_breath == 1 & !is.na(diff_breath)) |
         (diff_breath_obs == 1 & !is.na(diff_breath_obs))
       ) & (
       # but uses other danger signs
       (hypoxemia == 1 & !is.na(hypoxemia))| 
       (general_danger == 1 & !is.na(general_danger))
       ),
       
     #create variable for needing ultrasound
  
     hapin_needs_LUS = (
       (cough == 1 & !is.na(cough)) |
       (diff_breath == 1 & !is.na(diff_breath)) |
       (diff_breath_obs == 1 & !is.na(diff_breath_obs))
       ) & (
         # but uses other danger signs
       (general_danger == 1 & !is.na(general_danger))    
       )
     )
    
  summarize(c36, hapin_needs_LUS=sum(hapin_needs_LUS, na.rm = TRUE))
count(c36, hapin_needs_LUS)
