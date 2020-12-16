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
  df <- utils::read.csv(u, fileEncoding = 'UTF-8')

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
  df$iso3c <- suppressWarnings(
    countrycode::countrycode(df$Areas, 'country.name', 'iso3c'))

  # Select rows with country data
  df <- df[!is.na(df$iso3c), ]

  # Add WDI code
  df$indicator <- indicator

  # Select and rename columns
  df <- df[c('iso3c', 'TIME_PERIOD', 'indicator', 'OBS_VALUE', 'CDDATASOURCE')]
  names(df) <- c('iso3c', 'year', 'indicator', 'value', 'note')
  df$note <- ifelse(is.na(df$note), '', df$note)

  # Remove anything after hyphen in year
  # Note: This can lead to duplicated country-years
  # that need to be removed with filter_unicef()
  df$year <- sub('-.*', '', df$year)

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
  base_url <- 'https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/CD2030,CDCOV,1.0/'

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

  dplyr::recode(indicator,
                # Pregnant women receiving prenatal care of at least four visits
                'SH.STA.ANV4.ZS' = 'A2',
                # Use of insecticide-treated bed nets
                'SH.MLR.NETS.ZS' = 'A16',
                # Diarrhea treatment (ORS packet)
                'SH.STA.ORTH' = 'A19',
                # Vitamin A supplementation coverage rate
                'SN.ITK.VITA.ZS' = 'A29',
                )
}

#' recode_unicef_footnotes
#' @param x character: A vector with UNICEF notes (CDDATASOURCE).
#' @return character
#' @noRd
recode_unicef_footnotes <- function(x) {
  acronyms <- unicef_survey_acronyms
  survey_names <- recode_unicef_survey_acronym(acronyms)
  for (i in seq_along(survey_names)) {
    x <- sub(acronyms[i], survey_names[i], x)
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
    'NS' =  'National Survey',
    'NUCS' = 'National Health Survey Report',
    'NHC' = 'National Health Commission',
    'NHS' = 'National Health Survey',
    'NHMS' = 'National Health and Morbidity Survey',
    'DHS KIR' = 'Demographic and Health Survey Key Indicators Report',
    'DHS-MICS' = 'Demographic and Health Survey, Multiple Indicator Cluster Survey',
    'DHS-Style' = 'Demographic and Health Survey',
    'DHS' =  'Demographic and Health Survey',
    'MICS' = 'Multiple Indicator Cluster Survey',
    'MIS' = 'Malaria Indicator Survey',
    'AIS' = 'AIDS Indicator Survey',
    'AISMIS' = 'HIV/AIDS and Malaria Indicator Survey',
    'ECOM' = 'Enquête laise auprès des Ménages pour l\'évaluation de la pauvreté',
    'ENISED'  = 'Etude Nationale d\'Evaluation d\'Indicateurs SocioEconomiques et Démographiques 2015',
    'IHLCA'  = 'Integrated Household Living Conditions Survey',
    'NFHS' = 'National Family Health Survey',
    'PNDS' = 'Pesquisa Nacional de Demografia e Saúde da Criança e da Mulher',
    'SHHS' = 'Sudan Household and Health Survey',
    'FHS' = 'Family Health Survey',
    'EIP' = 'République du Enquête sur les Indicateurs du Paludisme',
    'ENSOMD' = 'L’Enque^te Nationale sur le Suivi des indicateurs des Objectifs du Mille´naire pour le De´veloppement',
    'ENPSF' = 'National Survey on Population and Family Health',
    'EDSFPAPFAM' = 'Deuxieme Enquete enne sur la Sante de la Famille',
    'ENHOGAR' = 'Encuesta Nacional de Hogares de Propósitos Múltiples',
    'ENSMI' = 'Encuesta Nacional de Salud. Materno Infantil',
    'MCSDHM' = 'Multisectoral Continuous Survey Demographic and Health Module',
    # 'EASF' = 'Family Health Survey',
    'PHS' = 'Population and Health Survey',
    'RCOS' = 'Rapid Survey on Children',
    'MoH' = 'MoH PNDS'
  )
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
      'SP.REG.BRTH.ZS', 'SH.STA.IYCF.ZS')

  if (any(df$indicator %in% mis_dhs_mics))
    df <- filter_unicef(df, priority = c('MIS', 'AISMIS', 'DHS', 'MICS'))
  if (any(df$indicator %in% dhs_mics))
    df <- filter_unicef(df, priority = c('DHS', 'MICS'))
  if (any(df$indicator %in% ais_dhs_mics))
    df <- filter_unicef(df, priority = c('AIS', 'AISMIS', 'DHS', 'MICS'))

  return(df)

}
