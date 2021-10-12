library(dplyr)

# Create vector with WDI years
wdi_years <- 1960:2020

# Create vectors of WDI country and region codes
wdi_countries <- wbstats::wb_countries("en")
wdi_country_codes <- wdi_countries %>%
  filter(region != "Aggregates") %>%
  select(iso3c)
wdi_country_codes <- wdi_country_codes$iso3c
wdi_aggregate_codes <- wdi_countries %>%
  filter(region == "Aggregates") %>%
  select(iso3c)
wdi_aggregate_codes <- wdi_aggregate_codes$iso3c
wdi_aggregate_codes <-
  c(wdi_aggregate_codes, "")

# Create vectors with WDI indicator codes
eurostat_indicators <- c("SP.POP.TOTL", "SP.DYN.CBRT.IN")
unpd_indicators <- c("SP.POP.TOTL", "SP.DYN.TFRT.IN", "SP.POP.AG00.MA.IN")
# ifelse(nchar(as.character(0:25)) == 1,
#        sprintf('SP.POP.AG0%s.MA.IN', 0:25),
#        sprintf('SP.POP.AG%s.MA.IN', 0:25)))
unicef_indicators <-
  c(
    "SH.STA.ANV4.ZS", "SH.MLR.NETS.ZS", "SH.STA.ORTH", "SN.ITK.VITA.ZS",
    "SH.MLR.IPTP.ZS", "SH.STA.ARIC.ZS", "SH.STA.BRTC.ZS", "SH.STA.BFED.ZS",
    "SH.STA.BRTW.ZS", "SN.ITK.SALT.ZS", "SH.STA.IYCF.ZS", "SH.STA.ORCF.ZS",
    "SH.STA.ANVC.ZS", "SP.REG.BRTH.ZS", "SP.REG.BRTH.UR.ZS", "SP.REG.BRTH.RU.ZS",
    "SP.REG.BRTH.MA.ZS", "SP.REG.BRTH.FE.ZS", "SP.M18.2024.FE.ZS", "SP.M15.2024.FE.ZS",
    "SH.HIV.KNOW.FE.ZS", "SH.HIV.KNOW.MA.ZS", "SH.CON.AIDS.FE.ZS", "SH.CON.AIDS.MA.ZS" # ,
    # 'SH.HIV.1524.KW.FE.ZS', 'SH.HIV.1524.KW.MA.ZS', 'SH.CON.1524.HR.FE.ZS',
    # 'SH.CON.1524.HR.MA.ZS'
  )
who_indicators <-
  c(
    "SH.DYN.NCOM.ZS", "SH.DYN.NCOM.FE.ZS", "SH.DYN.NCOM.MA.ZS",
    "SH.STA.SUIC.P5", "SH.STA.SUIC.FE.P5", "SH.STA.SUIC.MA.P5",
    "SH.ALC.PCAP.LI", "SH.ALC.PCAP.FE.LI", "SH.ALC.PCAP.MA.LI",
    "SH.STA.AIRP.P5", "SH.STA.AIRP.FE.P5", "SH.STA.AIRP.MA.P5",
    "SH.STA.WASH.P5", "SH.STA.WASH.FE.P5", "SH.STA.WASH.MA.P5",
    "SH.STA.POIS.P5", "SH.STA.POIS.P5.MA", "SH.STA.POIS.P5.FE",
    "SH.STA.TRAF.P5", "SH.STA.TRAF.FE.P5", "SH.STA.TRAF.MA.P5",
    "SH.ANM.CHLD.ZS", "SH.ANM.NPRG.ZS", "SH.ANM.ALLW.ZS",
    "SH.PRG.ANEM", "SP.REG.DTHS.ZS", "SH.MED.BEDS.ZS",
    "SH.PRV.SMOK", "SH.PRV.SMOK.FE", "SH.PRV.SMOK.MA",
    "SH.MED.NUMW.P3", "SH.MED.PHYS.ZS", "SH.PRG.SYPH.ZS",
    "SH.STA.OWAD.ZS", "SH.STA.OWAD.FE.ZS", "SH.STA.OWAD.MA.ZS",
    "SH.XPD.CHEX.GD.ZS", "SH.XPD.CHEX.PC.CD","SH.XPD.CHEX.PP.CD",
    "SH.XPD.GHED.CH.ZS","SH.XPD.GHED.GE.ZS","SH.XPD.GHED.GD.ZS",
    "SH.XPD.GHED.PC.CD","SH.XPD.GHED.PP.CD","SH.XPD.PVTD.CH.ZS",
    "SH.XPD.PVTD.PC.CD","SH.XPD.PVTD.PP.CD","SH.XPD.EHEX.CH.ZS",
    "SH.XPD.EHEX.PC.CD","SH.XPD.EHEX.PP.CD","SH.XPD.OOPC.CH.ZS",
    "SH.XPD.OOPC.PC.CD","SH.XPD.OOPC.PP.CD"
  )
unicef_survey_acronyms <-
  c(
    "NS", "DHS KIR", "DHS-MICS", "DHS-Style", "DHS", "MICS", "MIS", "AIS",
    "AISMIS", "ECOM", "ENISED", "IHLCA", "NFHS", "PNDS", "SHHS", "FHS",
    "EIP", "ENSOMD", "ENPSF", "EDSFPAPFAM", "ENHOGAR", "ENSMI", "MCSDHM",
    "EASF", "NUCS", "NHC", "PHS", "RCOS", "NHMS", "NHS", "MoH", "ENS",
    "WMS", "ENDESA", "SUSENAS", "DGEEC", "ICHNS", "EDSA", "MIDHS", "CDHS",
    "SDHS", "SMART", "PAPFAM", "ONS", "NNS"
  )

# Save internal package vectors
usethis::use_data(eurostat_indicators,
  unpd_indicators,
  unicef_indicators,
  who_indicators,
  unicef_survey_acronyms,
  wdi_aggregate_codes,
  wdi_country_codes,
  wdi_years,
  internal = TRUE,
  overwrite = TRUE,
  compress = "bzip2",
  version = 3
)
