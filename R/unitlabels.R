#' @title Insert parameter and unit labels in different markup languges
#' @description Given a user defined 'short-hand' code for a parameter-unit this functions returns a label in the selected markup language.
#' @details User defined 'short-hand' code for a parameter-unit labels and their conversions into different markup languages are stored in a data file \code{data/definitions.txt}. Edit this file to add or adjust definitions. Short hand code can take any form, but may not contain R's reserved words (\url{http://cran.r-project.org/doc/manuals/r-release/R-lang.html#Reserved-words})  
#' @param sh User defined 'short-hand' code for a parameter-unit label
#' @param output The desired markup languages to be returned
#' @return A character string in the desired markup
#' @example
#' ul("T.degC")
#' ul("T.degC", output="R.short")
#' ul("T.degC", output="Rmarkdown")
#' ul("T.degC", output="latex")
#' @export      
ul <- function(sh, output="R"){
	labels.list <- list(
	T.degC =  expression(paste(Temp., " (", degree, "C)"))
	,Salinity = "Salinity"
	,TA.umolL = expression(paste(A[T], " (", mu, "mol ",L^{-1},")" ))
	,pH = "pH (NBS)"
	,Cla.ugL = expression(paste("[",Chl, italic(a),"]", " (", mu, "g ",L^{-1},")" ))
	,pCO2.uatm = expression(paste(pCO[2], " (", mu, "atm)" ))
	,SS.tot.mgL = expression(paste("[SPM]", " (mg ",L^{-1},")" ))
	,O2.umolL = expression(paste("[",O[2],"]", " (", mu, "mol ",L^{-1},")" ))
	,DOC.mmolL = expression(paste("[",DOC,"]", " (mmol ",L^{-1},")" ))
	,NH4.umolL = expression(paste("[",NH[4],"]", " (", mu, "mol ",L^{-1},")" ))
	,NO2.umolL = expression(paste("[",NO[2],"]", " (", mu, "mol",L^{-1},")" ))
	,SiO3.umolL = expression(paste("[",Si(OH)[4],"]", " (", mu, "mol ",L^{-1},")" ))
	,PO4.umolL = expression(paste("[",PO[4],"]", " (", mu, "mol ",L^{-1},")" ))
	,NO3.umolL = expression(paste("[",NO[3],"]", " (", mu, "mol ",L^{-1},")" ))
	,PPF.bot = expression(paste(PPF, " (", mu, "mol photons ",m^{2}, s^{-1},")" ))
	,DON.umolL = expression(paste("[",DON,"]", " (", mu, "mol",L^{-1},")" ))
	,ratio.SS.org = "POM:SPM"
	,Flux.CO2.mmolm2d = expression(paste(CO[2]," Flux", " (mmol ",m^{-2},d^{-1},")" ))
	,DN.umolL = expression(paste("[",DN,"]", " (", mu, "mol ",L^{-1},")" ))
  ,POM.mgL = expression(paste("[POM]", " (mg ",L^{-1},")" ))
	,pCO2air.uatm = expression(paste(pCO[2][ air], " (", mu, "atm)" ))
  ,rain.mm = "Rainfall (mm)"
  ,wind10.ms = expression(paste(italic(u)[10]," (m ",s^{-1},")"))
  ,kw.cmh = expression(paste(italic(k)[w]," (cm ",h^{-1},")"))
  ,CO2.umolkgSW = expression(paste("[",CO[2],"]", " (", mu, "mol ",L^{-1},")" ))
  ,CO3.umolkgSW = expression(paste("[",CO[3]^{-2},"]", " (", mu, "mol ",L^{-1},")" ))
  ,HCO3.umolkgSW = expression(paste("[",HCO[3]^{-1},"]", " (", mu, "mol ",L^{-1},")" ))
  ,DIC.mmolL = expression(paste("[DIC] (","mmol ",L^{-1},")" ))
  ,pO2.matm = expression(paste(pO[2], " (matm)" ))
	,Flux.CO2.Mmold = expression(paste(CO[2]," Transport", " (Mmol ",d^{-1},")" ))
	,Flux.C.Ggy = expression(paste("C Transport", " (Gg ",y^{-1},")" ))
  ,waterarea.km = expression(paste("Area", " (",km^{2},")" ))
  ,FCO2.molCm2y = expression(paste(C," Flux", " (molC ",m^{-2}," ",y^{-1},")" ))
  ,PPF.umolphotonsm2s = expression(paste("PPF", " (", mu, "mol photons ",m^{-2}," ",s^{-1},")" ))
)
  
labels.list.short <- list(
	  T.degC =  expression(paste(Temp.))
	  ,Salinity = "Salinity"
	  ,TA.umolL = expression(paste(A[T]))
	  ,pH = "pH"
	  ,Cla.ugL = expression(paste("[",Chl, italic(a),"]"))
	  ,pCO2.uatm = expression(paste(pCO[2]))
	  ,SS.tot.mgL = expression(paste("[SPM]"))
	  ,O2.umolL = expression(paste("[",O[2],"]"))
	  ,DOC.mmolL = expression(paste("[",DOC,"]"))
	  ,NH4.umolL = expression(paste("[",NH[4],"]"))
	  ,NO2.umolL = expression(paste("[",NO[2],"]"))
	  ,SiO3.umolL = expression(paste("[",Si(OH)[4],"]"))
	  ,PO4.umolL = expression(paste("[",PO[4],"]"))
	  ,NO3.umolL = expression(paste("[",NO[3],"]"))
	  ,PPF.bot = expression(paste(PPF))
	  ,DON.umolL = expression(paste("[",DON,"]"))
	  ,ratio.SS.org = "POM:SPM"
	  ,Flux.CO2.mmolm2d = expression(paste(CO[2]," Flux"))
	  ,DN.umolL = expression(paste("[",DN,"]"))
	  ,POM.mgL = expression(paste("[POM]"))
	  ,pCO2air.uatm = expression(paste(pCO[2][ air]))
	  ,rain.mm = "Rainfall (mm)"
	  ,wind10.ms = expression(paste(italic(u)[10]))
	  ,kw.cmh = expression(paste(italic(k)[w]))
	  ,CO2.umolkgSW = expression(paste("[",CO[2],"]"))
	  ,CO3.umolkgSW = expression(paste("[",CO[3]^{-2},"]"))
	  ,HCO3.umolkgSW = expression(paste("[",HCO[3]^{-1},"]"))
	  ,DIC.mmolL = expression(paste("[DIC]"))
	  ,pO2.matm = expression(paste(pO[2]))
	  ,Flux.CO2.Mmold = expression(paste(CO[2]," Transport"))
	  ,Flux.C.Ggy = expression(paste("C Transport"))
	  ,waterarea.km = expression(paste("Area"))
	)

latex.labels.list <- list(
	T.degC =  "Water temperature (\\degC)"
	,Salinity = "Salinity"
	,TA.umolL = "\\AT~ (\\umolL)"
	,pH = "pH (NBS)"
	,Cla.ugL = "\\Chla~ (\\ugL)"
	,pCO2.uatm = "\\pCO~ (\\uatm)"
	,SS.tot.mgL = "SPM (\\mgL)"
	,O2.umolL = "\\Otwo~ (\\umolL)"
	,DOC.mmolL = "DOC (\\mmolL)"
	,NH4.umolL = "\\NHfour~ (\\umolL)"
	,NO2.umolL = "\\NOtwo~ (\\umolL)"
	,SiO3.umolL = " \\SiO~ (\\umolL)"
	,PO4.umolL = " \\PO~ (\\umolL)"
	,NO3.umolL = " \\NOthree~ (\\umolL)"
	,PPF.bot = "PPF"
	,DON.umolL = " DON~ (\\umolL)"
	,ratio.SS.org = "POM:SPM"
	,Flux.CO2.mmolm2d = "\\FCO~ (\\unitCOflux)" 
	,DN.umolL = " DN~ (\\umolL)"
  ,POM.mgL = " POM~ (\\mgL)"
	,pCO2air.uatm = "\\pCOair~ (\\uatm)"
  ,rain.mm = "Monthly rainfall (mm)"
  ,wind10.ms = "\\uten~ (\\ms)"
  ,kw.cmh = "\\kw~ (\\cmh)"
  ,CO2.umolkgSW = "CO2"
  ,CO3.umolkgSW = "CO3"
  ,HCO3.umolkgSW = "HCO3"
  ,DIC.mmolL = " DIC (\\mmolL)"
  ,pO2.matm = "\\pOwater~ (\\matm)"
	,Flux.CO2.Mmold = "\\FCO~ (\\unitCOfluxar)"
	,Flux.CO2.Mmoly = "\\FCO~ (\\unitCOfluxyearar)"
	,Flux.C.Ggy = "\\FC~ (\\GgCyr)"
)



if(output=="R") return(labels.list[[label]])
if(output=="R.short") return(labels.list.short[[label]])
if(output=="latex") return(latex.labels.list[[label]])
}


