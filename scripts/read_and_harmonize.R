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
surv2 <- read_csv(file = "data/export/PilotoVigilanciaNeum_DATA_2019-07-30_0921.csv")




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
  filter(
    !is.na(id), grepl("^34", id)
  ) %>%
  # harmonize variable names
  transmute(
    version = 1,
    record_id = as.character(record_id),
    date = today,
    cough = a1,
    cough_days = a1_1,
    diff_breath = a2,
    breath_days = a2_1
  ) %>%
  print()




surv2_c36 <- surv2 %>%
  select(everything(), -matches("^c34_")) %>%
  filter( !grepl("^3[35][0-9]{3}$", record_id), !is.na(today) ) %>%
  transmute(
    version = 2,
    record_id,
    date = today,
    cough = a1,
    cough_days = as.numeric(a1_1),
    diff_breath = a2,
    breath_days = a2_1
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
  filter(!is.na(c34_date))

