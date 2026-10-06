

# Load full dmr parameters
params <- read_csv("other/REF_parameter.csv")%>%
  select(PARAMETER_CODE,PARAMETER_DESC)%>%
  setNames(c("parameter_code","parameter_desc"))%>%
    mutate(parameter_code = str_pad(parameter_code,5,"left","0"))%>%
  distinct()

write_csv(params,"other/dmr_parameters.csv")

# Load all WQS
wqs <- read_csv("other/national_wqs.csv")%>%
  select(ENTITY_ABBR,CRITERION_ID,POLLUTANT_NAME,CRITERION_VALUE,UNIT_NAME)%>%
  mutate(parameter_code = NA,
         parameter_desc = NA)%>%
  filter(!ENTITY_ABBR == "PR")

write_csv(wqs, "other/wqs_needs_join.csv")

# Load Puerto Rico Joins
join <- read_csv("Data/full_join.csv")%>%
  mutate(ENTITY_ABBR = "PR")%>%
  select(ENTITY_ABBR,CRITERION_ID,POLLUTANT_NAME,CRITERION_VALUE,UNIT_NAME,parameter_code)%>%
  mutate(parameter_code = str_pad(parameter_code,5,side = "left",pad = "0"))%>%
  left_join(params, by = "parameter_code")


write_csv(join,"other/PR_joined.csv")

