rm(list=ls())

library(tidyverse)

infolder <- dirname(rstudioapi::getActiveDocumentContext()$path)
outfolder <- infolder |> file.path('..')









# Tables ------------------------------------------------------------------

folder <- file.path(outfolder,'00_algemene_informatie')
unlink(folder,recursive=TRUE)
dir.create(folder)

CL_TABLES <- tribble(
  ~waarde, ~label,
  'NKREDNEM', 'Aantal kredietnemers',
  'NINWRR' , 'Aantal inwoners volgens het Rijksregister',
  )

objects <- c('CL_TABLES')
for (name in objects) {
  filename <- str_c(folder,'/',name,'.xlsx')
  writexl::write_xlsx(get(name),filename)
}












# Ruwe data ---------------------------------------------------------------

folder <- file.path(outfolder,'01_data_ruw')
unlink(folder,recursive=TRUE)
dir.create(folder)

file.copy(
  file.path(infolder,'2023_personen_met_betalingsachterstand.xlsx'),
  folder
  )








    
 
  
 
    

# Gekuiste data -----------------------------------------------------------

folder <- file.path(outfolder,'02_data_clean')
unlink(folder,recursive=TRUE)
dir.create(folder)


file <- file.path(infolder,'2023_personen_met_betalingsachterstand.xlsx')

TABLE_NKREDNEM <- file |>
  readxl::excel_sheets() |>
  set_names() |>
  map_dfr(readxl::read_xlsx,path=file,col_types='text',.id="sheet") |>
  rename(
    'achtrst_totaal' ='Aantal kredietnemers met minstens één niet geregulariseerd achterstallig krediet',
    'achtrst_hypkred'='Aantal kredietnemers met minstens één niet geregulariseerd achterstallig hypothecair krediet',
    'achtrst_lening' ='Aantal kredietnemers met minstens één niet geregulariseerd achterstallige lening op afbetaling',
    'achtrst_verkoop'='Aantal kredietnemers met minstens één niet geregulariseerd achterstallige verkoop op afbetaling',
    'achtrst_kropen' ='Aantal kredietnemers met minstens één niet geregulariseerd achterstallige kredietopening',
    ) |>
  pivot_longer(achtrst_totaal|achtrst_hypkred|achtrst_lening|achtrst_verkoop|achtrst_kropen) |>
  mutate(
    REF_AREA = case_when(
      sheet=='Gewesten'~str_c('0',`NIS code`),
      !is.na(Postcode)~Postcode
      ),
    REF_AREA_CLASS = case_when(
      sheet=='Gewesten'~ 'NIS5-20190101',
      !is.na(Postcode) ~ 'PC', 
      ), 
    TIME_PERIOD = ifelse(
      str_detect(sheet,'^[0-9]{4}$'),
      str_c(sheet,'1231'),
      str_c(...3,'1231')
      ),
    KREDTYPE = name,
    OBS_VALUE = case_when(
      value %in% c('< 3','<3')     ~ NA,
      is.na(value)                 ~ NA,
      TRUE ~ value,
      ),
    OBS_VALUE_C = if_else(value %in% c('< 3','<3'),'<3',NA),
    OBS_STATUS = 'A' ,
    CONF_STATUS = 'F',
    .keep='none'
    )

DSD_NKREDNEM <- tribble(
  ~variable, ~label, ~codelijst,
  'REF_AREA'      ,'Geografisch gebied'                ,'CL_REF_AREA',
  'REF_AREA_CLASS','Geografisch gebied calssificatie'  ,'CL_REF_AREA_CLASS',
  'TIME_PERIOD'   ,'Observatie periode'                ,'CL_TIME_PERIOD',
  'KREDTYPE'      ,'Type kredietnemers'                ,'CL_KREDTYPE',
  'OBS_VALUE'     ,'Geobserveerde waarde'              ,'CL_OBS_VALUE',
  'OBS_VALUE_C'   ,'Geobserveerde waarde karakter'     ,'CL_OBS_VALUE_C',
  'OBS_STATUS'    ,'Status van de geobserveerde waarde','CL_OBS_STATUS',
  'CONF_STATUS'   ,'Vertrouwelijkheid van de geobserveerde waarde','CL_CONF_STATUS'
  )

tmp_nis <- infolder |>
  file.path('REFNIS_2019.csv') |>
  read_csv2(col_types=cols(.default='c')) |>
  mutate(
    waarde=`Code NIS`,
    label=`Administratieve eenheden`,
    .keep='none'
    )
tmp_postcodes <- infolder |>
  file.path('Postcodes 2022.xls') |>
  readxl::read_xls(col_types = 'text') |>
  mutate(
    waarde=Postcode,
    label=Plaatsnaam,
    .keep='none'
    )
CL_REF_AREA <- bind_rows(tmp_nis,tmp_postcodes)

CL_REF_AREA_CLASS <- tribble(
  ~waarde, ~label,
  'NIS5-20190101', "NIS-code niveau's 1,2,3,4 op 01/01/2019",
  'PC' , 'Postcode',
  )

CL_TIME_PERIOD <- tribble(
  ~waarde, ~label,
  'YYYY12', NA,  
  )

CL_KREDTYPE <- tribble(
  ~waarde, ~label,
  'totaal'         ,'Alle kredietnemers', 
  'achtrst_totaal' ,'Minstens één niet geregulariseerd achterstallig krediet',
  'achtrst_hypkred','Minstens één niet geregulariseerd achterstallig hypothecair krediet',
  'achtrst_lening' ,'Minstens één niet geregulariseerd achterstallige lening op afbetaling',
  'achtrst_verkoop','Minstens één niet geregulariseerd achterstallige verkoop op afbetaling',
  'achtrst_kropen' ,'Minstens één niet geregulariseerd achterstallige kredietopening',
  )

CL_OBS_VALUE <- tribble(
  ~waarde, ~label,
  '[integer]', NA,
  )

CL_OBS_VALUE_C <- tribble(
  ~waarde, ~label,
  '<3', 'Kleine waarde, gemaskeerd',
  )

CL_OBS_STATUS <-  tribble(
  ~waarde, ~label,
  'A','Normale waarde',
  'E','Geschatte waarde',
  'F','Voorspelde waarde',
  )

CL_CONF_STATUS <-  tribble(
  ~waarde, ~label,
  'F', 'Free (free for publication)',
  'A', 'Primary confidentiality due to small counts',
  'O', 'Primary confidentiality due to dominance by one unit',
  'T', 'Primary confidentiality due to dominance by two units',
  'S', 'Secondary confidentiality set and managed by the receiver, not for publication',
  )

# data_clean |> filter(!REF_AREA %in% CL_REF_AREA$waarde) |> View()
# data_clean |> filter(!str_detect(TIME_PERIOD,'^[0-9]{4}1231$')) |> View()
# data_clean |> filter(!KREDTYPE %in% CL_KREDTYPE$waarde) |> View()
# data_clean |> filter(!OBS_VALUE %in% CL_OBS_VALUE$waarde & !str_detect(OBS_VALUE,'^[0-9]+$')) |> View()
# data_clean |> filter(!OBS_STATUS %in% CL_OBS_STATUS$waarde) |> View()
# data_clean |> filter(!CONF_STATUS %in% CL_CONF_STATUS$waarde) |> View()

objects <- c(
  'TABLE_NKREDNEM','DSD_NKREDNEM','CL_REF_AREA','CL_REF_AREA_CLASS',
  'CL_TIME_PERIOD','CL_KREDTYPE','CL_OBS_VALUE','CL_OBS_VALUE_C','CL_OBS_STATUS','CL_CONF_STATUS'
  )
for (name in objects) {
  filename <- str_c(folder,'/',name,'.xlsx')
  writexl::write_xlsx(get(name),filename)
  }




https://sdmx.org/wp-content/uploads/CL_CONF_STATUS_1_3_2022.docx




# Geaggregeerde data ------------------------------------------------------


# tmp_nis <- infolder |>
#   file.path('REFNIS_2019_gooddata.xlsx') |>
#   readxl::read_xlsx(col_types='text')
# 
# tmp_postcodes <- 
#   r'{\\WV162699\fs_kb_dkb\svr\4_Beveiligde_data\Algemene Datasets-beheer\02.POSTCODEdata\00.maakdata\2023-06-16\inputdata}' |>
#   file.path('Conversion Postal code_Refnis code_va01012019.xlsx') |>
#   readxl::read_xlsx() |>
#   select(POSTCODE=`Postal code`,NIS5=`Refnis code`) |>
#   left_join(tmp_nis,join_by(NIS5)) |>
#   mutate(Npostcodes=n(),.by=POSTCODE)
# 
# 
# data_aggregatie <- data_clean |>
#   left_join(tmp_postcodes,join_by(POSTCODE),relationship="many-to-many") |>
#   pivot_longer(REFNIS|NIS2|NIS3|NIS4|NIS5,names_to='NISSOURCE',values_to='REFNIS',values_drop_na=TRUE) |>
#   mutate(OBS_VALUE_N=if_else(Npostcodes>1,NA,OBS_VALUE_N,OBS_VALUE_N)) |>
#   summarize(OBS_VALUE_N=sum(OBS_VALUE_N),.by=c(TIME_PERIOD,PARAM_C,NISSOURCE,REFNIS)) |>
#   slice_max(OBS_VALUE_N,by=c(REFNIS,TIME_PERIOD,PARAM_C)) |>
#   arrange(NISSOURCE,REFNIS,TIME_PERIOD,PARAM_C)










# SDC ---------------------------------------------------------------------









# Afgeleide parameters ----------------------------------------------------


folder <- file.path(outfolder,'05_data_derivedparameters')
unlink(folder,recursive=TRUE)
dir.create(folder)


CL_PARAMETER_BETALINGSACHTERSTAND <- tribble(
  ~waarde, ~label,
  'N_personen_achterstallig_krediet'               ,'Aantal kredietnemers met minstens één niet geregulariseerd achterstallig krediet',
  'N_personen_achterstallig_hypothecair_krediet'   ,'Aantal kredietnemers met minstens één niet geregulariseerd achterstallig hypothecair krediet',
  'N_personen_achterstallige_lening_op_afbetaling' ,'Aantal kredietnemers met minstens één niet geregulariseerd achterstallige lening op afbetaling',
  'N_personen_achterstallige_verkoop_op_afbetaling','Aantal kredietnemers met minstens één niet geregulariseerd achterstallige verkoop op afbetaling',
  'N_personen_achterstallige_kredietopening'       ,'Aantal kredietnemers met minstens één niet geregulariseerd achterstallige kredietopening',
  'AR_01_1','Percentage kredietnemers met minstens één niet geregulariseerd achterstallig krediet ten opzichte van totale bevolking van 18 jaar en ouder',
  'AR_01_2','Percentage kredietnemers met minstens één niet geregulariseerd achterstallig hypothecair krediet ten opzichte van totale bevolking van 18 jaar en ouder',
  'AR_01_3','Percentage kredietnemers met minstens één niet geregulariseerd achterstallige lening op afbetaling ten opzichte van totale bevolking van 18 jaar en ouder',
  'AR_01_4','Percentage kredietnemers met minstens één niet geregulariseerd achterstallige verkoop op afbetaling ten opzichte van totale bevolking van 18 jaar en ouder',
  'AR_01_5','Percentage kredietnemers met minstens één niet geregulariseerd achterstallige kredietopening ten opzichte van totale bevolking van 18 jaar en ouder',
  )


CL_PARAMETER_DEMOGRAFIE <- tribble(
  ~waarde, ~label,
  'N_inwoners','Aantal inwoners'
  )

CL_AGE  <- tibble(
  waarde=as.character(0:140),
  label=str_c(waarde,' jaar')
  ) |>
  add_row(waarde='18-Inf',label='18 jaar en ouder',.before=1)


    
objects <- c(
  'CL_PARAMETER_BETALINGSACHTERSTAND','CL_PARAMETER_DEMOGRAFIE','CL_AGE'
  )
for (name in objects) {
  filename <- str_c(folder,'/',name,'.xlsx')
  writexl::write_xlsx(get(name),filename)
  }














# # bestaande codes L&G ---------------------------------------------------------
# 
# 
# 
# data_svr_prod_abb <- folder |>
#   file.path('data_svr_prod_abb.csv') |>
#   read_csv() |>
#   mutate(confidential='FALSE')
# 
# confidential_source_informix <- folder |>
#   file.path('confidential_source_informix.csv') |>
#   read_delim(delim=',',quote='') |>
#   mutate(across(everything(),\(x) str_replace_all(x,'"',''))) |>
#   rename_with(\(x) str_replace_all(x,'"','')) |>
#   mutate(confidential='TRUE')
# 
# DBtables <- data_svr_prod_abb |>
#   bind_rows(confidential_source_informix) |>
#   mutate(across(everything(),str_squish)) |>
#   select(where(\(x) length(unique(x))!=1 )) |>
#   arrange(table_name)
# 
# write.csv2(DBtables,file.path(folder,'DB_tables.csv'))
# 
# 
# 
# 
# 






# # CL_REFNIS.xlsx  ---------------------------------------------------------
# 
# r'{\\WV162699\fs_kb_dkb\svr\4_Beveiligde_data\Algemene Datasets\01.NISdata}' |>
#   file.path('NISdata.csv') |>
#   read_csv2() |>
#   writexl::write_xlsx(file.path(outfolder,'CL_REFNIS.xlsx'))
# 
# 
# 
# # CL_POSTCODE.xlsx --------------------------------------------------------
# 
# tmp <- r'{\\WV162699\fs_kb_dkb\svr\4_Beveiligde_data\Algemene Datasets\02.POSTCODEdata}' |>
#   file.path('POSTCODEdata.csv') |>
#   read_csv2() |>
#   rename_with(toupper) |>
#   writexl::write_xlsx(file.path(outfolder,'CL_POSTCODE.xlsx'))
  
  
  
  
  