#------------------------------------------------------------------------------*
# Manage Jalapa penumonia surveillance data (PINS)
#------------------------------------------------------------------------------*


# Load used packages
library(package = "tidyverse")



#------------------------------------------------------------------------------*
# Get data from RedCap Exports ----
#------------------------------------------------------------------------------*

# Data originally collected in the Emory RedCap Pilot project
em_pilot <- read_csv(file = "data/export/HAPINGuatemala_DATA_2018-09-17_1807.csv")

# Data collected for the first version of the pneumonia surveillance in GT
surv1 <- read_csv(file = "data/export/HAPINVigilanciaGuate_DATA_2019-07-30_0923.csv")

# Data collected for the second version of the pneumonia surveillance in GT
surv2 <- read_csv(file = "data/export/PilotoVigilanciaNeum_DATA_2019-08-14_1136.csv")




#------------------------------------------------------------------------------*
# Extract pneumonia screening (c36) and lung ultrasound (c34) datasets ----
#------------------------------------------------------------------------------*

# c36 ----

surv1_c36 <- surv1 %>%
  # select necessary variables
  select(
    record_id, redcap_repeat_instrument,
    everything(), -matches("_v[0-9]$")
  ) %>%
  # Only keep records with data
  #grepl() returns TRUE when a pattern is found in the corresponding character string
  #here we only keep ids that start with 34
  filter(
    !is.na(id), grepl("^34", id)
  ) %>%
  # harmonize variable names
  transmute( #transmute function renames variable and drops existing names
    interviewer,
    version = 1,
    record_id = as.character(record_id),
    date = today,
    dob = dateofbirth,
    age = age,
    municipio = municipio,
    com_jalapa = com_jalapa,
    facility_type = tipo_servicio,
    cough = a1,
    cough_days = a1_1,
    diff_breath = a2,
    breath_days = a2_1,
    referral_source = b1,
    chest_indrawing = c1,
    resp_rate_mas_1 = c3,
    resp_rate_mas_2 = c4,
    resp_rate_mas_avg = c5,
    tachypnea = c6,
    oxyhb_60s = d1,
    oxyhb_90s = d2,
    oxyhb_120s = d3,
    oxyhb_avg_nelcor = d4, #not sure is this is nelcor or masimo
    oxy_sat_measurement_conditions = d5,
    diff_drinking = e1,
    vomits_everything = e2,
    convulsion_now = e3_1,
    convulsion_any = e3,
    lethargy = e4,
    fever = e5,
    hypothermia = e6,
    unable_eat = e7,
    grunting = e8,
    intercostal_tiraje_severe = e9,
    cyanosis = e10,
    pneumo_severa_oms = e11
    
  ) %>%
  print()


surv2_c36 <- surv2 %>%
  #select all variables except those that start with c34 and those without dates
  select(everything(), -matches("^c34_")) %>%
  filter( !grepl("^3[35][0-9]{3}$", record_id), !is.na(today) ) %>%
  transmute(
    interviewer,
    version = 2,
    record_id,
    date = today,
    dob = dateofbirth,
    age_months = age,
    sex = sexo,
    municipio = municipio_e4acf8,
    com_jalapa = com_jalapa_3d81ce,
    facility_type = tipo_servicio,
    hosp_ward = servicio_hospital,
    residence = municipio_nino,
    cough = a1,
    cough_days = as.numeric(a1_1),
    diff_breath = a2,
    breath_days = a2_1,
    eligible = c36_elegible,
    referral_source = b1,
    referral_source_other = b1_1,
    prior_abx = b3,
    diff_drinking = c1,
    vomit_persist = c2,
    vomits_everything = c2_1,
    diarrhea = c3,
    convulsion_now = c4,
    convulsion_any = c5,
    stridor = c5_1,
    lethargy = c6,
    unable_eat = c7,
    no_movement = c7_1,
    grunting = c8,
    intercostal_tiraje_severe = c9,
    cyanosis = c10,
    nasal_flare = c11,
    tracheal_tugging = c12,
    chest_indrawing = c13, #not sure if this is the same as intercostal retraction/tiraje
    head_nod = c14,
    wheezing = c16,
    crackles = c17,
    temp_taken = d1,
    temp_type = d1_1,
    temp = d1_2,
    resp_rate_type = d2,
    resp_rate_man_1 = d2_1,
    resp_rate_man_2 = d2_2,
    resp_rate_man_avg = d2_3,
    resp_rate_mas_1 = d2_5,
    resp_rate_mas_2 = d2_6,
    resp_rate_mas_avg = d2_7,
    oxy_sat_method = d3,
    oxyhb_60s = d3_2,
    oxyhb_90s = d3_3,
    oxyhb_120s = d3_4,
    oxyhb_avg_nelcor = d3_5,
    oxyhb_avg_massimo = d3_9,
    oxy_sat_measurement_conditions = d4,
    oxy_admin_device = d4_1,
    oxy_units = d4_2,
    oxy_lper_min = d4_3,
    tachypnea = d5,
    fever = d6_1,
    hypothermia = d6_2,
    hypoxemia = d7,
    weight_kg = e1_1,
    weight_type = e2,
    weight_clothes = e2_1,
    height_cm = e3_1,
    malnutrition = e5,
    current_abx = f1,
    current_abx_type = f1_2,
    cxr = f2,
    discharge = f4,
    discharge_other = f4_1,
    pneumo_hosp = f5,
    pneumo_hosp_date = f6,
    
    diagnosis = f9,
    diagnosis_other = f9_1,
    wood_stove = f10,
    LUS_consent = f12,
    pneumo_who = neumo_oms,
    pneumo_who_severe = neumo_severa_oms,
    pneumo_hapin = neumo_severa_oms
    
    
  ) %>%
  print()



all_c36 <- bind_rows(surv1_c36, surv2_c36) %>%
  print()





# c34 ----

em_c34 <- em_pilot %>%
  select(record_id, redcap_event_name, matches("^c34_")) %>%
  filter(!is.na(c34_date)) %>%
  print()

gt_c34 <- surv2 %>%
  select(record_id, redcap_event_name, matches("^c34_")) %>%
  filter(!is.na(c34_date),
         #remove the Hapin cases from the LUS data
