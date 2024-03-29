#' Supported indicators
#'
#' A dataset containing the available indicators for [fetch_indicator()].
#'
#' List of available indicators, by data source.
#'
#' Eurostat:
#' * SP.DYN.CBRT.IN
#' * SP.POP.TOTL
#'
#' UNPD:
#' * SP.DYN.TFRT.IN
#' * SP.POP.AG00.MA.IN - SP.POP.AG25.MA.IN
#' * SP.POP.TOTL
#'
#' UNICEF:
#' * SH.MLR.NETS.ZS
#' * SH.STA.ANV4.ZS
#' * SH.STA.ORTH
#' * SN.ITK.VITA.ZS
#' * SH.MLR.IPTP.ZS
#' * SH.STA.ARIC.ZS
#' * SH.STA.BRTC.ZS
#' * SH.STA.BFED.ZS
#' * SH.STA.BRTW.ZS
#' * SN.ITK.SALT.ZS
#' * SH.STA.IYCF.ZS
#' * SH.STA.ORCF.ZS
#' * SH.STA.ANVC.ZS
#' * SP.REG.BRTH.ZS
#' * SP.REG.BRTH.UR.ZS
#' * SP.REG.BRTH.RU.ZS
#' * SH.HIV.KNOW.FE.ZS
#' * SH.HIV.KNOW.MA.ZS
#' * SH.CON.AIDS.FE.ZS
#' * SH.CON.AIDS.MA.ZS
#' * SP.REG.BRTH.MA.ZS
#' * SP.REG.BRTH.FE.ZS
#' * SP.M18.2024.FE.ZS
#' * SP.M15.2024.FE.ZS
#'
#' WHO:
#' * SH.ALC.PCAP.LI, SH.ALC.PCAP.MA.LI, SH.ALC.PCAP.FE.LI
#' * SH.ANM.ALLW.ZS
#' * SH.ANM.CHLD.ZS
#' * SH.ANM.NPRG.ZS
#' * SH.DYN.NCOM.ZS, SH.DYN.NCOM.MA.ZS, SH.DYN.NCOM.FE.ZS
#' * SH.MED.BEDS.ZS
#' * SH.MED.NUMW.P3
#' * SH.MED.PHYS.ZS
#' * SH.PRG.ANEM
#' * SH.PRG.SYPH.ZS
#' * SH.PRV.SMOK, SH.PRV.SMOK.MA, SH.PRV.SMOK.FE
#' * SH.STA.AIRP.P5, SH.STA.AIRP.MA.P5, SH.STA.AIRP.FE.P5
#' * SH.STA.OWAD.ZS, SH.STA.OWAD.MA.ZS, SH.STA.OWAD.FE.ZS
#' * SH.STA.POIS.P5, SH.STA.POIS.P5.MA, SH.STA.POIS.P5.FE
#' * SH.STA.SUIC.P5, SH.STA.SUIC.MA.P5, SH.STA.SUIC.FE.P5
#' * SH.STA.TRAF.P5, SH.STA.TRAF.MA.P5 ,SH.STA.TRAF.FE.P5
#' * SH.STA.WASH.P5, SH.STA.WASH.MA.P5, SH.STA.WASH.FE.P5
#' * SP.REG.DTHS.ZS, SH.XPD.CHEX.GD.ZS, SH.XPD.CHEX.PC.CD
#' * SH.XPD.CHEX.PP.CD, SH.XPD.GHED.CH.ZS, SH.XPD.GHED.GE.ZS
#' * SH.XPD.GHED.GD.ZS, SH.XPD.GHED.PC.CD, SH.XPD.GHED.PP.CD
#' * SH.XPD.PVTD.CH.ZS, SH.XPD.PVTD.PC.CD, SH.XPD.PVTD.PP.CD
#' * SH.XPD.EHEX.CH.ZS, SH.XPD.EHEX.PC.CD, SH.XPD.EHEX.PP.CD
#' * SH.XPD.OOPC.CH.ZS, SH.XPD.OOPC.PC.CD, SH.XPD.OOPC.PP.CD

#'
#' @format data.frame
#'
"indicatorlist"

#' Population
#'
#' An example dataset with population data (SP.POP.TOTL) from Eurostat.
#'
#' \describe{
#'   \item{iso3c}{ISO3 country code}
#'   \item{year}{Year}
#'   \item{indicator}{Indicator code}
#'   \item{value}{Data value}
#'   \item{note}{Footnote}
#'   \item{source}{Data source}
#' }
#' @usage data("population")
#' @format data.frame
"population"

#' Bed nets
#'
#' An example dataset with use of insecticide-treated bed nets (SH.MLR.NETS.ZS)
#' from UNICEF.
#'
#' \describe{
#'   \item{iso3c}{ISO3 country code}
#'   \item{year}{Year}
#'   \item{indicator}{Indicator code}
#'   \item{value}{Data value}
#'   \item{note}{Footnote}
#'   \item{source}{Data source}
#' }
#' @usage data("bednets")
#' @format data.frame
"bednets"
