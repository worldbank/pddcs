x <- wbgUsefulData::wbCountryMeta
x <- x[x$region_value == 'Aggregates',]
wb_aggreate_codes <- c(x$iso3, '')
wb_aggreate_codes <- unique(wb_aggreate_codes)

#usethis::use_data(wb_aggreate_codes, internal = TRUE, overwrite = TRUE)



# Create vectors with WDI indicator codes
eurostat_indicators <- c('SP.POP.TOTL', 'SP.DYN.CBRT.IN')
unpd_indicators <- c('SP.POP.TOTL', 'SP.DYN.TFRT.IN',
                     ifelse(nchar(as.character(0:25)) == 1,
                            sprintf('SP.POP.AG0%s.MA.IN', 0:25),
                            sprintf('SP.POP.AG%s.MA.IN', 0:25)))
unicef_indicators <- c('SH.STA.ANV4.ZS', 'SH.MLR.NETS.ZS', 'SH.STA.ORTH', 'SN.ITK.VITA.ZS')
who_indicators <-
  c('SH.DYN.NCOM.ZS', 'SH.DYN.NCOM.FE.ZS', 'SH.DYN.NCOM.MA.ZS', 'SH.STA.SUIC.P5',
    'SH.STA.SUIC.FE.P5', 'SH.STA.SUIC.MA.P5', 'SH.ALC.PCAP.LI', 'SH.ALC.PCAP.FE.LI',
    'SH.ALC.PCAP.MA.LI', 'SH.STA.AIRP.P5', 'SH.STA.AIRP.FE.P5', 'SH.STA.AIRP.MA.P5',
    'SH.STA.WASH.P5', 'SH.STA.POIS.P5', 'SH.STA.POIS.P5.MA', 'SH.STA.POIS.P5.FE',
    'SH.STA.TRAF.P5', 'SH.ANM.CHLD.ZS', 'SH.ANM.NPRG.ZS', 'SH.ANM.ALLW.ZS',
    'SH.PRG.ANEM', 'SP.REG.DTHS.ZS', 'SH.MED.BEDS.ZS', 'SH.PRV.SMOK',
    'SH.PRV.SMOK.FE', 'SH.PRV.SMOK.MA', 'SH.MED.NUMW.P3', 'SH.MED.PHYS.ZS',
    'SH.STA.OWAD.FE.ZS', 'SH.STA.OWAD.MA.ZS', 'SH.STA.OWAD.ZS', 'SH.PRG.SYPH.ZS')


# Save internal package vectors
usethis::use_data(eurostat_indicators,
                  unpd_indicators,
                  unicef_indicators,
                  who_indicators,
                  wb_aggreate_codes,
                  internal = TRUE,
                  overwrite = TRUE)

