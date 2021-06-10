#' @importFrom stringi stri_unescape_unicode
NULL

#' Fetch data from UNICEF
#'
#' Fetch data from UNICEF API.
#'
#' @param indicator character: A CETS type indicator code.
#' @return data.frame
#' @keywords internal
fetch_unicef <- function(indicator) {

  # Create URL
  u <- create_url_unicef(indicator)

  # Fetch data
  resp <- httr::GET(u)
  df <- suppressMessages(httr::content(resp, encoding = 'UTF-8', as = 'parsed'))

  # Standardize data
  df <- standardize_unicef(df, indicator)

  return(df)
}

#' standardize_unicef
#' @param df data.frame: A data frame with data from the UNICEF API.
#' @inheritParams fetch_unicef
#' @return data.frame
#' @noRd
standardize_unicef <- function(df, indicator) {

  # Add ISO3 country codes
  if (any('Areas' %in% names(df))) {
    df$iso3c <- suppressWarnings(
      countrycode::countrycode(df$Areas, 'country.name', 'iso3c'))
  } else if (any('Country' %in% names(df))) {
    df$iso3c <- suppressWarnings(
      countrycode::countrycode(df$Country, 'country.name', 'iso3c'))
  } else if (any('Geographic area' %in% names(df))) {
    df$iso3c <- suppressWarnings(
      countrycode::countrycode(df$`Geographic area`, 'country.name', 'iso3c'))
  }

  # Select rows with country data
  df <- df[!is.na(df$iso3c), ]

  # Add WDI code
  df$indicator <- indicator

  # Create note column
  if (any('CDDATASOURCE' %in% names(df))) {
    df$note <- df$CDDATASOURCE
  } else if ('Data Source' %in% names(df)) {
    df$note <- df$`Data Source`
  } else if ('DATA_SOURCE' %in% names(df)) {
    df$note <- df$DATA_SOURCE
  } else {
    df$note <- ''
  }
  df$note <- ifelse(is.na(df$note), '', df$note)

  # Select and rename columns
  df <- df[c('iso3c', 'TIME_PERIOD', 'indicator', 'OBS_VALUE', 'note')]
  names(df) <- c('iso3c', 'year', 'indicator', 'value', 'note')

  # Remove anything after hyphen in year
  # Note: This can lead to duplicated country-years
  # that need to be removed with filter_unicef()
  df$year <- sub('-.*', '', df$year)

  # Select latest year of survey
  # based on information in footnote
  tmp <- regmatches(df$note, gregexpr('\\d{4}-\\d{2}', df$note))
  tmp2 <- regmatches(df$note, gregexpr('\\d{4}-\\d{4}', df$note))
  df$year_tmp <-
    ifelse(grepl('\\d{4}-\\d{2}', df$note),
           paste0(substr(tmp, 1, 2), substr(tmp, 6, 7)),
           df$year)
  df$year_tmp <-
    ifelse(grepl('\\d{4}-\\d{4}', df$note),
           substr(tmp2, 6, 9),
           df$year_tmp)
  df$year_tmp <-
    ifelse(df$year_tmp == '1900' & df$year == '1999',
           '2000',
           df$year_tmp)
  df$year <- df$year_tmp
  df$year_tmp <- NULL

  # Filter surveys
  df <- apply_filter_unicef(df)

  # Recode footnotes
  df$note <- recode_unicef_footnotes(df$note)

  # General standardization
  df <- standardize_all(df)

  return(df)
}

#' create_url_who
#' @inheritParams fetch_unicef
#' @return character
#' @noRd
create_url_unicef <- function(indicator) {

  # Base URL
  base_url <- 'https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/'

  # Indicator groups
  # Countdown - coverage indicators
  cd2030 <- c('SH.MLR.NETS.ZS', 'SH.STA.ANV4.ZS', 'SH.STA.ORTH',
              'SN.ITK.VITA.ZS', 'SH.MLR.IPTP.ZS', 'SH.STA.ARIC.ZS',
              'SH.STA.BRTC.ZS', 'SH.STA.BFED.ZS')
  # Nutrition
  nutrition <- c('SH.STA.BRTW.ZS', 'SN.ITK.SALT.ZS', 'SH.STA.IYCF.ZS')
  # Maternal, newborn and child health
  mnch <- c('SH.STA.ORCF.ZS', 'SH.STA.ANVC.ZS')
  # PT - Child Protection
  pt_cp <- c('SP.REG.BRTH.ZS', 'SP.REG.BRTH.UR.ZS', 'SP.REG.BRTH.RU.ZS',
             'SP.REG.BRTH.MA.ZS','SP.REG.BRTH.FE.ZS','SP.M18.2024.FE.ZS',
             'SP.M15.2024.FE.ZS')
  # HIV_AIDS - HIV/AIDS
  hiv_aids <- c('SH.HIV.KNOW.FE.ZS', 'SH.HIV.KNOW.MA.ZS',
                'SH.CON.AIDS.FE.ZS', 'SH.CON.AIDS.MA.ZS',
                'SH.HIV.1524.KW.FE.ZS', 'SH.HIV.1524.KW.MA.ZS',
                'SH.CON.1524.HR.FE.ZS', 'SH.CON.1524.HR.MA.ZS')

  # Modify url based on indicator endpoints / data flows
  if (indicator %in% cd2030) {
    base_url <- paste0(base_url, 'CD2030,CDCOV,1.0/')
  } else if (indicator %in% nutrition) {
    base_url <- paste0(base_url, 'UNICEF,NUTRITION,1.0/')
  } else if (indicator %in% mnch) {
    base_url <- paste0(base_url, 'UNICEF,MNCH,1.0/')
  } else if (indicator %in% pt_cp) {
    base_url <- paste0(base_url, 'UNICEF,PT,1.0/')
  } else if (indicator %in% hiv_aids) {
    base_url <- paste0(base_url, 'UNICEF,HIV_AIDS,1.0/')
  }

  # Check access
  if (httr::http_error('https://sdmx.data.unicef.org/'))
    rlang::abort(c('Couldn\'t connect to UNICEF API.',
                   i = 'Check your Internet connection.'))

  # Create main URL
  unicef_code <- recode_unicef_codes(indicator)
  the_url <- sprintf('%s%s...?format=csv', base_url, unicef_code)

  return(the_url)
}

#' recode_unicef_codes
#' @inheritParams fetch_unicef
#' @return character
#' @noRd
recode_unicef_codes <- function(indicator) {

  out <-
    dplyr::recode(
      indicator,
      # Pregnant women receiving prenatal care of at least four visits
      'SH.STA.ANV4.ZS' = 'A2.PCT..TOTAL',
      # IPT of malaria in pregnancy (% of pregnant women)
      'SH.MLR.IPTP.ZS' = 'A3.PCT..TOTAL',
      # Births attended by skilled health staff (% of total)
      'SH.STA.BRTC.ZS' = 'A6.PCT..TOTAL',
      # Use of insecticide-treated bed nets
      'SH.MLR.NETS.ZS' = 'A16.PCT..TOTAL',
      # ARI treatment (% of children under 5 taken to a health provider)
      'SH.STA.ARIC.ZS' = 'A17.PCT..TOTAL',
      # Diarrhea treatment (ORS packet)
      'SH.STA.ORTH' = 'A19.PCT..TOTAL',
      # Exclusive breastfeeding (% of children under 6 months)
      'SH.STA.BFED.ZS' = 'A26.PCT..TOTAL',
      # Vitamin A supplementation coverage rate
      'SN.ITK.VITA.ZS' = 'A29.PCT..TOTAL',
      # Low-birthweight babies (% of births)
      'SH.STA.BRTW.ZS' = '.NT_BW_LBW._T._T._T._T._T._T',
      # Consumption of iodized salt (% of households),
      'SN.ITK.SALT.ZS' = '.NT_IOD_ANY_TH._T._T._T._T._T._T',
      # Infant and young child feeding practices, all 3 IYCF
      'SH.STA.IYCF.ZS' = '.NT_CF_MAD._T.M6T23._T._T._T._T',
      # Diarrhea treatment (oral rehydration and continued feeding)
      'SH.STA.ORCF.ZS' = '.MNCH_ORTCF._T.Y0T4._T._T._T._T.',
      # Pregnant women receiving prenatal care (%)
      'SH.STA.ANVC.ZS' = '.MNCH_ANC1.F.Y15T49._T._T._T._T.',
      # Completeness of birth registration (%)
      'SP.REG.BRTH.ZS' = '.PT_CHLD_Y0T4_REG._T.Y0T4._T._T._T._T',
      # Completeness of birth registration, urban (%)
      'SP.REG.BRTH.UR.ZS' = '.PT_CHLD_Y0T4_REG._T.Y0T4._T._T.U._T',
      # Completeness of birth registration, rural (%)
      'SP.REG.BRTH.RU.ZS' = '.PT_CHLD_Y0T4_REG._T.Y0T4._T._T.R._T',
      # Completeness of birth registration, male (%)
      'SP.REG.BRTH.MA.ZS' = '.PT_CHLD_Y0T4_REG.M.Y0T4._T._T._T._T',
      # Completeness of birth registration, female (%)
      'SP.REG.BRTH.FE.ZS' = '.PT_CHLD_Y0T4_REG.F.Y0T4._T._T._T._T',
      # Women who were first married by age 18 (% of women ages 20-24)
      'SP.M18.2024.FE.ZS' = '.PT_F_20-24_MRD_U18._T.Y20T24._T._T._T',
      # Women who were first married by age 15 (% of women ages 20-24)
      'SP.M15.2024.FE.ZS' = '.PT_F_20-24_MRD_U15._T.Y20T24._T._T._T',
      # Comprehensive correct knowledge of HIV/AIDS, ages 15-49, female
      'SH.HIV.KNOW.FE.ZS' = '.HVA_PREV_KNOW.Y15T49.F._T._T.',
      # Comprehensive correct knowledge of HIV/AIDS, ages 15-49, male
      'SH.HIV.KNOW.MA.ZS' = '.HVA_PREV_KNOW.Y15T49.M._T._T.',
      # Condom use at last high-risk sex, adult female (% ages 15-49)
      'SH.CON.AIDS.FE.ZS' = '.HVA_PREV_CNDM_REG.Y15T49.F._T._T.',
      # Condom use at last high-risk sex, adult male (% ages 15-49)
      'SH.CON.AIDS.MA.ZS' = '.HVA_PREV_CNDM_REG.Y15T49.M._T._T.'#,
      # Comprehensive correct knowledge of HIV/AIDS, ages 15-24, female
      # 'SH.HIV.1524.KW.FE.ZS' = '.HVA_PREV_KNOW.Y15T19+Y20T24.F._T._T.',
      # Comprehensive correct knowledge of HIV/AIDS, ages 15-24, male
      # 'SH.HIV.1524.KW.MA.ZS' = '.HVA_PREV_KNOW.Y15T19+Y20T24.M._T._T.',
      # Condom use at last high-risk sex, youth female (% ages 15-24)
      #'SH.CON.1524.HR.FE.ZS' = '.HVA_PREV_CNDM_REG.Y15T19+Y20T24.F._T._T.',
      # Condom use at last high-risk sex, youth male (% ages 15-24)
      # 'SH.CON.1524.HR.MA.ZS' = '.HVA_PREV_CNDM_REG.Y15T19+Y20T24.M._T._T.',
    )

  return(out)
}

#' recode_unicef_footnotes
#' @param x character: A vector with UNICEF notes (CDDATASOURCE).
#' @return character
#' @noRd
recode_unicef_footnotes <- function(x) {
  acronyms <- unicef_survey_acronyms
  survey_names <- recode_unicef_survey_acronym(acronyms)
  for (i in seq_along(survey_names)) {
    x <- sub(paste0('\\b', acronyms[i], '\\b'), survey_names[i], x)
  }
  return(x)
}

#' recode_unicef_survey_acronym
#' @param x character: A vector with UNICEF survey acronyms.
#' @return character
#' @noRd
recode_unicef_survey_acronym <- function(x) {
  dplyr::recode(
    x,
    # National survey
    'NS' =  'National Survey',
    'NSS' =  'National Survey',
    'ONS' = 'National Survey',
    'NUCS' = 'National Health Survey Report',
    'NHC' = 'National Health Commission',
    'NHS' = 'National Health Survey',
    'NHMS' = 'National Health and Morbidity Survey',
    'NNS' = 'National Nutrition Survey',
    # DHS
    'CDHS' = 'Continuous Demographic and Health Survey',
    'MIDHS' = 'Multiple Indicator Demographic and Health Survey',
    'DHS KIR' = 'Demographic and Health Survey Key Indicators Report',
    'DHS-MICS' = 'Demographic and Health Survey, Multiple Indicator Cluster Survey',
    'DHS-Style' = 'Demographic and Health Survey',
    'SDHS' = 'Demographic and Health Survey',
    'DHS' =  'Demographic and Health Survey',
    # MICS
    'MICS' = 'Multiple Indicator Cluster Survey',
    # Malaria
    'MIS' = 'Malaria Indicator Survey',
    # HIV/AIDS
    'AIS' = 'AIDS Indicator Survey',
    'AISMIS' = 'HIV/AIDS and Malaria Indicator Survey',
    # Other surveys
    'ECOM' = 'Enqu\\u00eate laise aupr\\u00e8s des M\\u00e9nages pour l\'\\u00e9valuation de la pauvret\\u00e9',
    'ENISED'  = 'Etude Nationale d\'Evaluation d\'Indicateurs SocioEconomiques et D\\u00e9mographiques',
    'IHLCA'  = 'Integrated Household Living Conditions Survey',
    'NFHS' = 'National Family Health Survey',
    'PNDS' = 'Pesquisa Nacional de Demografia e Sa\\u00fade da Crian\\u00e7a e da Mulher',
    'SHHS' = 'Sudan Household and Health Survey',
    'FHS' = 'Family Health Survey',
    'EIP' = 'R\\u00e9publique du Enqu\\u00eate sur les Indicateurs du Paludisme',
    'ENSOMD' = 'L\\u2019Enque^te Nationale sur le Suivi des indicateurs des Objectifs du Mille\\u00b4naire pour le De\\u00b4veloppement',
    'ENPSF' = 'National Survey on Population and Family Health',
    'ENDESA' = 'Encuesta Nicarag\\u00fcense de Demograf\\u00eda y Salud', #'Nicaragua National Demographic and Health Survey'
    'EDSFPAPFAM' = 'Deuxieme Enquete enne sur la Sante de la Famille',
    'ENHOGAR' = 'Encuesta Nacional de Hogares de Prop\\u00f3sitos M\\u00faltiples',
    'ENSMI' = 'Encuesta Nacional de Salud. Materno Infantil',
    'ENS' = 'Encuesta National de Salud',
    'MCSDHM' = 'Multisectoral Continuous Survey Demographic and Health Module',
    # 'EASF' = 'Family Health Survey',
    'PHS' = 'Population and Health Survey',
    'RCOS' = 'Rapid Survey on Children',
    'MoH' = 'MoH PNDS',
    'WMS' = 'Welfare Monitoring Survey',
    'SUSENAS' = 'National Socioeconomic Survey',
    'DGEEC' = 'Direcc\\u00f3n General de Estadística, Encuestas y Censos',
    'ICHNS' = 'Integrated Child Health and Nutrition Survey',
    'EDSA' = 'Encuesta de Demografía y Salud',
    'SMART' = 'Standardized Monitoring and Assessment of Relief and Transitions Survey',
    'PAPFAM' = 'Pan Arab Project for Family Health Survey'

   ) %>%
    stringi::stri_unescape_unicode()
}

#' apply_filter_unicef
#' @param df data.frame: A data frame with data from the UNICEF API.
#' @return data.frame
#' @noRd
apply_filter_unicef <- function(df) {

  mis_dhs_mics <-
    c('SH.MLR.IPTP.ZS', 'SH.MLR.NETS.ZS')
  ais_dhs_mics <-
    c('SH.HIV.KNOW.FE.ZS', 'SH.HIV.KNOW.MA.ZS',
      'SH.HIV.1524.KW.FE.ZS','SH.HIV.1524.KW.MA.ZS',
      'SH.CON.1524.HR.FE.ZS', 'SH.CON.1524.HR.MA.ZS',
      'SH.CON.AIDS.FE.ZS', 'SH.CON.AIDS.MA.ZS')
  dhs_mics <-
    c('SH.STA.ARIC.ZS', 'SH.STA.ORCF.ZS', 'SH.STA.ORTH',
      'SH.STA.ANV4.ZS', 'SH.STA.ANVC.ZS', 'SH.STA.BRTC.ZS',
      'SH.STA.BFED.ZS', 'SH.STA.BRTW.ZS', 'SN.ITK.SALT.ZS',
      'SN.ITK.VITA.ZS', 'SP.REG.BRTH.RU.ZS', 'SP.REG.BRTH.UR.ZS',
      'SP.REG.BRTH.ZS', 'SP.REG.BRTH.MA.ZS', 'SP.REG.BRTH.FE.ZS',
      'SP.M18.2024.FE.ZS','SP.M15.2024.FE.ZS','SH.STA.IYCF.ZS')

  if (any(df$indicator %in% mis_dhs_mics))
    df <- filter_unicef(df, priority =
                          c('MIS|Malaria Indicator Survey',
                            'AISMIS|HIV/AIDS and Malaria Indicator Survey',
                            'DHS|Demographic and Health Survey',
                            'MICS|Multiple Indicator Cluster Survey'))
  if (any(df$indicator %in% ais_dhs_mics))
    df <- filter_unicef(df, priority =
                          c('AIS|AIDS Indicator Survey',
                            'AISMIS|HIV/AIDS and Malaria Indicator Survey',
                            'DHS|Demographic and Health Survey',
                            'MICS|Multiple Indicator Cluster Survey'))
  if (any(df$indicator %in% dhs_mics))
    df <- filter_unicef(df, priority =
                          c('DHS|[Dd]emographic and [Hh]ealth [Ss]urvey', #|Demographic Survey'
                            'MICS|[Mm]ultiple [Ii]ndicator [Cc]luster [Ss]urvey')
    )
  return(df)

}
