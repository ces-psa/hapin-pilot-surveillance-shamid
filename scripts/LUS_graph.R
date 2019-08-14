
#Run read_and_harmonize.R

ls()
if (!"gt_c34" %in% ls()) source ("scripts/read_and_harmonize.R")

gt_c34 %>% mutate(c34_by=recode(c34_by, IP="IPF")) %>% 
  filter (!c34_by %in% c("MPL", "CLO")) %>% 
#graph dates of LUS by sonographer
#ggplot is already loaded as part of tidyverse
#color refers to points or lines not fill
ggplot ()+
  geom_histogram(
    aes(x=c34_date, fill=c34_by)
    )+
  facet_grid(c34_by~.)
  
#Find out what errors are in dates (these were corrected in RedCap)
#gt_c34 %>% 
#  filter (c34_date < as.Date("2018-01-01") | c34_by == "MPL") %>% 
# select(record_id, c34_date, c34_by)
#ID 340295 should be 2018 not 2017 and corresponds to c36 in version 1 so we should exclude it -- this has been fixed in RedCap
#gt_c34 %>% 
#  filter (c34_by=="IP") %>% 
#  select(record_id, c34_date, c34_by)


gt_c34 %>% mutate(month=substr(c34_date, 1, 7)) %>% 
  group_by(c34_by, month) %>% 
  summarize(count=n() ) %>% 
  spread (c34_by, count)



c34_by
c34_date
