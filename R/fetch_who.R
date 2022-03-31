#' Fetch data from WHO
#'
#' Fetch data from WHO API.
#'
#' @param indicator character: A CETS type indicator code.
#' @return data.frame
#' @keywords internal
fetch_who <- function(indicator) {

  # Create URL
  u <- create_url_who(indicator)

  # Fetch data
  df <- jsonlite::fromJSON(u)$value

  # Standardize data
  df <- standardize_who(df, indicator)

  return(df)
}

#' standardize_who
#' @param df data.frame: A data frame with data from the WHO API.
#' @inheritParams fetch_who
#' @return data.frame
#' @noRd
standardize_who <- function(df, indicator) {

  # Select rows with country data
  df <- df[df$SpatialDimType == "COUNTRY", ]

  # Select rows that has no second dimension (eg. AGEGROUP)
  df <- df[is.na(df$Dim2), ]

  # Add note column
  df$note <- data.table::fifelse(
    df$Value != "",
    sprintf(
      "Plausible bound is %s.",
      gsub(".*[[]|[]]", "", df$Value)
    ),
    ""
  )

  # Add WDI code
  df$indicator <- indicator

  # Select and rename columns
  df <- df[c("SpatialDim", "TimeDim", "indicator", "NumericValue", "note")]
  names(df) <- c("iso3c", "year", "indicator", "value", "note")

  # General standardization
  df <- standardize_all(df)

  # Recode values for select indicators
  if (indicator == "SH.MED.NUMW.P3") {
    df$value <- df$value / 10
  } # Convert data per 10,000 to per 1,000
  if (indicator == "SH.MED.PHYS.ZS") {
    df$value <- df$value / 10
  } # Convert data per 10,000 to per 1,000
  if (indicator == "SH.MED.BEDS.ZS") {
    df$value <- df$value / 10
  } # Convert data per 10,000 to per 1,000

  return(df)
}

#' create_url_who
#' @inheritParams fetch_who
#' @return character
#' @noRd
create_url_who <- function(indicator) {

  # Base URL
  base_url <- "https://ghoapi.azureedge.net/api/"

  # Check access
  if (httr::http_error(base_url)) {
    rlang::abort(c("Couldn't connect to WHO API.",
      i = "Check your Internet connection."
    ))
  }

  # Create main URL
  who_code <- recode_who_codes(indicator)
  the_url <- paste0(base_url, who_code)

  # Both sex indicators
  btsx_indicators <- c(
    "SH.DYN.NCOM.ZS", "SH.STA.SUIC.P5",
    "SH.ALC.PCAP.LI", "SH.STA.AIRP.P5",
    "SH.STA.POIS.P5", "SH.PRV.SMOK",
    "SH.STA.OWAD.ZS", "SH.STA.WASH.P5",
    "SH.STA.TRAF.P5"
  )

  # Add filters
  if (grepl("[.]MA([.][A-Z]{2})?", indicator)) {
    the_url <- paste0(the_url, "?$filter=Dim1%20eq%20%27MLE%27")
  } else if (grepl("[.]FE([.][A-Z]{2})?", indicator)) {
    the_url <- paste0(the_url, "?$filter=Dim1%20eq%20%27FMLE%27")
  } else if (indicator %in% btsx_indicators) {
    the_url <- paste0(the_url, "?$filter=Dim1%20eq%20%27BTSX%27")
  }

  return(the_url)
}

#' recode_who_codes
#' @inheritParams fetch_who
#' @return character
#' @noRd
recode_who_codes <- function(indicator) {
  dplyr::recode(indicator,
    # Mortality cardiovascular diseases, cancer etc.
    "SH.DYN.NCOM.ZS" = "NCDMORT3070",
    "SH.DYN.NCOM.MA.ZS" = "NCDMORT3070",
    "SH.DYN.NCOM.FE.ZS" = "NCDMORT3070",
    # Crude suicide rates
    "SH.STA.SUIC.P5" = "SDGSUICIDE",
    "SH.STA.SUIC.FE.P5" = "SDGSUICIDE",
    "SH.STA.SUIC.MA.P5" = "SDGSUICIDE",
    # Total alcohol per capita consumption
    "SH.ALC.PCAP.LI" = "SA_0000001688",
    "SH.ALC.PCAP.FE.LI" = "SA_0000001688",
    "SH.ALC.PCAP.MA.LI" = "SA_0000001688",
    # Ambient and household air pollution attributable death rate
    "SH.STA.AIRP.P5" = "SDGAIRBODA",
    "SH.STA.AIRP.FE.P5" = "SDGAIRBODA",
    "SH.STA.AIRP.MA.P5" = "SDGAIRBODA",
    # Mortality rate attributed to exposure to unsafe WASH service
    "SH.STA.WASH.P5" = "SDGWSHBOD",
    "SH.STA.WASH.FE.P5" = "SDGWSHBOD",
    "SH.STA.WASH.MA.P5" = "SDGWSHBOD",
    # Mortality rate attributed to unintentional poisoning
    "SH.STA.POIS.P5" = "SDGPOISON",
    "SH.STA.POIS.P5.MA" = "SDGPOISON",
    "SH.STA.POIS.P5.FE" = "SDGPOISON",
    # Estimated road traffic death rate
    "SH.STA.TRAF.P5" = "RS_198",
    "SH.STA.TRAF.FE.P5" = "RS_198",
    "SH.STA.TRAF.MA.P5" = "RS_198",
    # Prevalence of anaemia in children under 5 years
    "SH.ANM.CHLD.ZS" = "NUTRITION_ANAEMIA_CHILDREN_PREV",
    # Prevalence of anaemia in non-pregnant women
    "SH.ANM.NPRG.ZS" = "NUTRITION_ANAEMIA_NONPREGNANT_PREV",
    # Prevalence of anaemia in women of reproductive age
    "SH.ANM.ALLW.ZS" = "NUTRITION_ANAEMIA_REPRODUCTIVEAGE_PREV",
    # Prevalence of anaemia in pregnant women
    "SH.PRG.ANEM" = "NUTRITION_ANAEMIA_PREGNANT_PREV",
    # Civil registration coverage of cause-of-death
    "SP.REG.DTHS.ZS" = "WHS10_8",
    # Hospital beds
    "SH.MED.BEDS.ZS" = "WHS6_102",
    # Estimate of current tobacco use prevalence
    "SH.PRV.SMOK" = "M_Est_tob_curr_std",
    "SH.PRV.SMOK.FE" = "M_Est_tob_curr_std",
    "SH.PRV.SMOK.MA" = "M_Est_tob_curr_std",
    # Nursing and midwifery personnel density
    "SH.MED.NUMW.P3" = "HWF_0006",
    "SH.MED.PHYS.ZS" = "HWF_0001",
    # Prevalence of overweight among adults
    "SH.STA.OWAD.FE.ZS" = "NCD_BMI_25A",
    "SH.STA.OWAD.MA.ZS" = "NCD_BMI_25A",
    "SH.STA.OWAD.ZS" = "NCD_BMI_25A",
    # Antenatal care attendees who were positive for syphilis
    "SH.PRG.SYPH.ZS" = "PercposANC",
    # Health expenditure
    "SH.XPD.CHEX.GD.ZS" =	"GHED_CHEGDP_SHA2011",
    "SH.XPD.CHEX.PC.CD" =	"GHED_CHE_pc_US_SHA2011",
    "SH.XPD.CHEX.PP.CD" =	"GHED_CHE_pc_PPP_SHA2011",
    "SH.XPD.GHED.CH.ZS" =	"GHED_GGHE-DCHE_SHA2011",
    "SH.XPD.GHED.GE.ZS" =	"GHED_GGHE-DGGE_SHA2011",
    "SH.XPD.GHED.GD.ZS" =	"GHED_GGHE-DGDP_SHA2011",
    "SH.XPD.GHED.PC.CD" =	"GHED_GGHE-D_pc_US_SHA2011",
    "SH.XPD.GHED.PP.CD" =	"GHED_GGHE-D_pc_PPP_SHA2011",
    "SH.XPD.PVTD.CH.ZS" =	"GHED_PVT-DCHE_SHA2011",
    "SH.XPD.PVTD.PC.CD" =	"GHED_PVT-D_pc_US_SHA2011",
    "SH.XPD.PVTD.PP.CD" =	"GHED_PVT-D_pc_PPP_SHA2011",
    "SH.XPD.EHEX.CH.ZS" =	"GHED_EXTCHE_SHA2011",
    "SH.XPD.EHEX.PC.CD" =	"GHED_EXT_pc_US_SHA2011",
    "SH.XPD.EHEX.PP.CD" =	"GHED_EXT_pc_PPP_SHA2011",
    "SH.XPD.OOPC.CH.ZS" =	"GHED_OOPSCHE_SHA2011",
    "SH.XPD.OOPC.PC.CD" =	"GHED_OOP_pc_US_SHA2011",
    "SH.XPD.OOPC.PP.CD" =	"GHED_OOP_pc_PPP_SHA2011"
    #Add new indicator later
  )
}
