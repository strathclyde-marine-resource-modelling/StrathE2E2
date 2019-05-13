#
# monthly_averages_of_final_year.R
#
#' Summarise the final year for nitrate ammonia and chl
#'
#' @param model model object
#' @param output model output
#' @param aggregates aggregated model output
#'
#' @return monthly averages
#'
#' @export
#
monthly_averages_of_final_year <- function(model, output, aggregates) {

	run		<- el(model, "run")
	data		<- el(model, "data")

	nyears		<- el(run, "nyears")
	identifier	<- el(run, "identifier")
	resultsdir	<- el(run, "resultsdir")

	physical.parms	<- el(data, "physical.parameters")
	si_depth	<- el(physical.parms, "si_depth")
	so_depth	<- el(physical.parms, "so_depth")
	d_depth		<- el(physical.parms, "d_depth")
	x_shallowprop	<- el(physical.parms, "x_shallowprop")

	nitrate_d	<- el(output, "nitrate_d")
	ammonia_d	<- el(output, "ammonia_d")

	s_nitrate	<- el(aggregates, "s_nitrate")
	s_ammonia	<- el(aggregates, "s_ammonia")
	s_phyt		<- el(aggregates, "s_phyt")
	herb		<- el(aggregates, "herb")
	carn		<- el(aggregates, "carn")
	benthslar	<- el(aggregates, "benthslar")
	benthclar	<- el(aggregates, "benthclar")

	xvolume_si<-si_depth*x_shallowprop
	xvolume_so<-so_depth*(1-x_shallowprop)
	xd_volume<-d_depth*(1-x_shallowprop)

	xs_volume <- xvolume_si + xvolume_so

	C_to_chl<-40

	monthlyfinal<-data.frame(rep(0,12))

	for(mon in 1:12){
		monthlyfinal[mon,1]<-mean(s_nitrate[((nyears-1)*360+((mon-1)*30)+1):((nyears-1)*360+(mon*30))])/xs_volume
		monthlyfinal[mon,2]<-mean(nitrate_d[((nyears-1)*360+((mon-1)*30)+1):((nyears-1)*360+(mon*30))])/xd_volume
		monthlyfinal[mon,3]<-mean(s_ammonia[((nyears-1)*360+((mon-1)*30)+1):((nyears-1)*360+(mon*30))])/xs_volume
		monthlyfinal[mon,4]<-mean(ammonia_d[((nyears-1)*360+((mon-1)*30)+1):((nyears-1)*360+(mon*30))])/xd_volume
		monthlyfinal[mon,5]<-mean(((((s_phyt[((nyears-1)*360+((mon-1)*30)+1):((nyears-1)*360+(mon*30))]))*12*106)/16)/C_to_chl)/xs_volume
		monthlyfinal[mon,6]<-mean(herb[((nyears-1)*360+((mon-1)*30)+1):((nyears-1)*360+(mon*30))])/(xs_volume+xd_volume)
		monthlyfinal[mon,7]<-mean(carn[((nyears-1)*360+((mon-1)*30)+1):((nyears-1)*360+(mon*30))])/(xs_volume+xd_volume)
		monthlyfinal[mon,8]<-mean(benthslar[((nyears-1)*360+((mon-1)*30)+1):((nyears-1)*360+(mon*30))])/(xs_volume+xd_volume)
		monthlyfinal[mon,9]<-mean(benthclar[((nyears-1)*360+((mon-1)*30)+1):((nyears-1)*360+(mon*30))])/(xs_volume+xd_volume)
	}

	names(monthlyfinal)<-c("surfnitratemMm3","deepnitratemMm3","surfammoniamMm3","deepammoniamMm3","surfchlmgm3","onmizoomMNm3","carnzoomMNm3","benthslarmMNm3","benthclarmMNm3")

	filename = csvname(resultsdir, "model_monthlyresults", identifier)
	writecsv(monthlyfinal, filename, row.names=FALSE)
	write.table(monthlyfinal, file=paste(resultsdir,"model_monthlyresults",identifier,".csv",sep=""),sep=",",row.names=FALSE)

	monthlyfinal
}

