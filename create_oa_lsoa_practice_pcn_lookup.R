# ----------------------------------------------- #
# Desc: Create the OA, LSOA, Practice, PCN lookup #
# File: create_oa_lsoa_practice_pcn_lookup.R      #
# ----------------------------------------------- #

fnCreateLookup <- function(inif_file_settings){
  # OA to LSOA Lookup
  # =================
  
  # Load the key index which will be the OA to LSOA lookup file
  df_oa_lsoa_lookup <- read.csv(ini_file_settings$oa_lsoa_lookup$filename)
  # write.csv(df_oa_lsoa_lookup %>% filter(LSOA11CD %in% (df_postcode_lookup %>% distinct(lsoa11cd) %>% .$lsoa11cd)), 'D:\\RWorkspace\\Primary_Care_Mapping_App\\OA11LSOA11BA\\OA11LSOA11BA.csv')
  
  # Select only the required fields and filter out any Welsh OAs as we only have partial 
  # data from Wales in the GP registration data
  df_oa_lsoa_lookup <- df_oa_lsoa_lookup %>% 
    select(
      as.integer(
        c(
          ini_file_settings$oa_lsoa_lookup$oa11cd, 
          ini_file_settings$oa_lsoa_lookup$lsoa11cd
        )
      )
    ) %>%
    rename_with(.fn = function(x){c('oa11cd', 'lsoa11cd')}) %>%
    filter(str_sub(oa11cd, 1, 1)!='W')

  # PCN Membership
  # ==============
  df_pcn_members <- read_excel(path = ini_file_settings$pcn_members$filename, sheet = ini_file_settings$pcn_members$sheet)

  # Select only the required fields and filter out any data with an end date
  df_pcn_members <- df_pcn_members %>% 
    select(
      as.integer(
        c(
          ini_file_settings$pcn_members$prac_code, 
          ini_file_settings$pcn_members$prac_name,
          ini_file_settings$pcn_members$ccg_code_prac,
          ini_file_settings$pcn_members$ccg_name_prac,
          ini_file_settings$pcn_members$pcn_code,
          ini_file_settings$pcn_members$pcn_name,
          ini_file_settings$pcn_members$ccg_code_pcn,
          ini_file_settings$pcn_members$ccg_name_pcn,
          ini_file_settings$pcn_members$end_date
        )
      )
    ) %>%
    rename_with(.fn = function(x){c(
      'prac_code', 'prac_name',
      'ccg_code_prac', 'ccg_name_prac',
      'pcn_code', 'pcn_name',
      'ccg_code_pcn', 'ccg_name_pcn',
      'end_date')}
    ) %>%
    filter(is.na(end_date)) %>%
    select(-end_date)
  
  # GP Registration
  # ===============
  
  # Load the GP registration data
  df_gp_reg <- read.csv(ini_file_settings$gp$filename)
  # write.csv(df_gp_reg %>% filter(LSOA_CODE %in% (df_postcode_lookup %>% distinct(lsoa11cd) %>% .$lsoa11cd)), 'D:\\RWorkspace\\Primary_Care_Mapping_App\\GPREGLSOA\\GPREGLSOA.csv')

  # Select only the required fields and filter out any Welsh OAs as we only have partial 
  # data from Wales in the GP registration data
  df_gp_reg <- df_gp_reg %>% 
    select(
      as.integer(
        c(
          ini_file_settings$gp_registration$prac_code, 
          ini_file_settings$gp_registration$lsoa11cd, 
          ini_file_settings$gp_registration$popn
        )
      )
    ) %>%
    rename_with(.fn = function(x){c('prac_code', 'lsoa11cd', 'popn')}) %>%
    filter(str_sub(lsoa11cd, 1, 1)!='W')
  
  # Join in the PCN code
  df_gp_reg <- df_gp_reg %>% 
    left_join(
      df_pcn_members %>% select(prac_code, pcn_code),
      by = c('prac_code' = 'prac_code'))

  # Self join to the total registered population by LSOA
  df_gp_reg <- df_gp_reg %>% 
    left_join(
      df_gp_reg %>% group_by(lsoa11cd) %>% summarise(total_popn = sum(popn)),
      by = c('lsoa11cd' = 'lsoa11cd')
    )
  
  # Calculate the percentage of each LSOA registered to each practice and add in a random number
  # between 0 and 1 to be used as a tie breaker between practices with equal percentage of LSOA
  df_practice_lookup <- df_gp_reg %>%
    mutate(pct_of_lsoa = popn / total_popn, tie_breaker = runif(NROW(df_gp_reg))) %>%
    group_by(lsoa11cd) %>%
    arrange(lsoa11cd, desc(pct_of_lsoa), desc(tie_breaker)) %>%
    slice_head(n = 1) %>%
    ungroup()

  # Calculate the percentage of each LSOA registered to each PCN and add in a random number
  # between 0 and 1 to be used as a tie breaker between PCNs with equal percentage of LSOA
  df_pcn_lookup <- df_gp_reg %>% 
    group_by(pcn_code, lsoa11cd) %>% 
    summarise(popn = sum(popn), total_popn = mean(total_popn)) %>%
    ungroup()
  df_pcn_lookup <- df_pcn_lookup %>%
    mutate(pct_of_lsoa = popn / total_popn, tie_breaker = runif(NROW(df_pcn_lookup))) %>%
    group_by(lsoa11cd) %>%
    arrange(lsoa11cd, desc(pct_of_lsoa), desc(tie_breaker)) %>%
    slice_head(n = 1) %>%
    ungroup()

  # Join the practice and pcn lookup to the oa to lsoa lookup and return it as a data frame
  df_oa_lsoa_practice_pcn_lookup <- df_oa_lsoa_lookup %>% 
    left_join(df_practice_lookup %>% select(prac_code, lsoa11cd), by = c('lsoa11cd'= 'lsoa11cd')) %>%
    left_join(df_pcn_lookup %>% select(pcn_code, lsoa11cd), by = c('lsoa11cd'= 'lsoa11cd'))
  
  # Tidy up
  rm(list = c('df_gp_reg', 'df_oa_lsoa_lookup', 'df_pcn_members', 'df_pcn_lookup', 'df_practice_lookup'))
  
  return(df_oa_lsoa_practice_pcn_lookup)
}