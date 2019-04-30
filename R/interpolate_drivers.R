#
# interpolate_drivers.R
#
#' Interpolate drivers over time that model is to be run
#'
#' @param run model run settings
#' @param drivers model drivers
#'
#' @return model output
#'
#' @importFrom deSolve ode
#'
#' @export
#
interpolate_drivers <- function(run, drivers) {

	# unpack:
	drtimes <- el(run, "drtimes")
	sprectimes <- el(run, "sprectimes")

	driverso_inflow <- el(drivers, "driverso_inflow")
	driverd_inflow <- el(drivers, "driverd_inflow")
	driversi_inflow <- el(drivers, "driversi_inflow")
	driversi_outflow <- el(drivers, "driversi_outflow")
	driverd_outflow <- el(drivers, "driverd_outflow")
	drivers_upwell <- el(drivers, "drivers_upwell")
	driverso_si_flow <- el(drivers, "driverso_si_flow")
	driversslight <- el(drivers, "driversslight")
	driverso_logespm <- el(drivers, "driverso_logespm")
	driversi_logespm <- el(drivers, "driversi_logespm")
	driverso_temp <- el(drivers, "driverso_temp")
	driverd_temp <- el(drivers, "driverd_temp")
	driversi_temp <- el(drivers, "driversi_temp")
	driverv_dif <- el(drivers, "driverv_dif")
	driverriver <- el(drivers, "driverriver")
	driverboundso_det <- el(drivers, "driverboundso_det")
	driverboundd_det <- el(drivers, "driverboundd_det")
	driverboundsi_det <- el(drivers, "driverboundsi_det")
	driverboundso_amm <- el(drivers, "driverboundso_amm")
	driverboundd_amm <- el(drivers, "driverboundd_amm")
	driverboundsi_amm <- el(drivers, "driverboundsi_amm")
	driverboundso_nit <- el(drivers, "driverboundso_nit")
	driverboundd_nit <- el(drivers, "driverboundd_nit")
	driverboundsi_nit <- el(drivers, "driverboundsi_nit")
	driverboundso_phyt <- el(drivers, "driverboundso_phyt")
	driverboundd_phyt <- el(drivers, "driverboundd_phyt")
	driverboundsi_phyt <- el(drivers, "driverboundsi_phyt")
	driverboundriv_amm <- el(drivers, "driverboundriv_amm")
	driverboundriv_nit <- el(drivers, "driverboundriv_nit")
	driverboundriv_det <- el(drivers, "driverboundriv_det")
	driverso_atm_amm <- el(drivers, "driverso_atm_amm")
	driverso_atm_nit <- el(drivers, "driverso_atm_nit")
	driversi_atm_amm <- el(drivers, "driversi_atm_amm")
	driversi_atm_nit <- el(drivers, "driversi_atm_nit")
	driver_s1_pd <- el(drivers, "driver_s1_pd")
	driver_s2_pd <- el(drivers, "driver_s2_pd")
	driver_s3_pd <- el(drivers, "driver_s3_pd")
	driver_d1_pd <- el(drivers, "driver_d1_pd")
	driver_d2_pd <- el(drivers, "driver_d2_pd")
	driver_d3_pd <- el(drivers, "driver_d3_pd")
	drivers_wave <- el(drivers, "drivers_wave")
	driverpfish_sp <- el(drivers, "driverpfish_sp")
	driverpfish_rec <- el(drivers, "driverpfish_rec")
	driverdfish_sp <- el(drivers, "driverdfish_sp")
	driverdfish_rec <- el(drivers, "driverdfish_rec")
	driverbs_sp <- el(drivers, "driverbs_sp")
	driverbs_rec <- el(drivers, "driverbs_rec")
	driverbc_sp <- el(drivers, "driverbc_sp")
	driverbc_rec <- el(drivers, "driverbc_rec")
	driver_migfish_imig <- el(drivers, "driver_migfish_imig")
	driver_migfish_emig <- el(drivers, "driver_migfish_emig")


	# _____Interpolate drivers over time that model is to be run_____


	#Need to derive the volume-balancing outflows from the offshore surface and deep, and from inner to outer
	#First, these are the flows we actually know
	fdriverso_inflow	<- matrix(ncol=2,c(drtimes,(driverso_inflow(drtimes))))
	fdriverd_inflow		<- matrix(ncol=2,c(drtimes,(driverd_inflow(drtimes))))
	fdriversi_inflow	<- matrix(ncol=2,c(drtimes,(driversi_inflow(drtimes))))
	fdriversi_outflow	<- matrix(ncol=2,c(drtimes,(driversi_outflow(drtimes))))
	fdriverd_outflow	<- matrix(ncol=2,c(drtimes,(driverd_outflow(drtimes))))
	fdrivers_upwell		<- matrix(ncol=2,c(drtimes,(drivers_upwell(drtimes))))
	fdriverso_si_flow	<- matrix(ncol=2,c(drtimes,(driverso_si_flow(drtimes))))

	#Derived outflow from si to so
	fdriversi_so_flow <- matrix(ncol=2,c(drtimes,rep(NA,length(drtimes))))
	fdriversi_so_flow[,2]<-fdriverso_si_flow[,2] + fdriversi_inflow[,2] - fdriversi_outflow[,2]
	#Volume balance check for the inshore zone
	#   plot(fdriverso_si_flow[1:361,2] + fdriversi_inflow[1:361,2] - fdriversi_outflow[1:361,2]  - fdriversi_so_flow[1:361,2],type="l")
	#   abline(h=0)
	#   sum((fdriverso_si_flow[1:361,2] + fdriversi_inflow[1:361,2] - fdriversi_outflow[1:361,2]  - fdriversi_so_flow[1:361,2]))

	#Derived outflow from so
	fdriverso_outflow <- matrix(ncol=2,c(drtimes,rep(NA,length(drtimes))))
	fdriverso_outflow[,2]<-fdriverso_inflow[,2] + fdrivers_upwell[,2] + fdriversi_so_flow[,2]  -  fdriverso_si_flow[,2]
	#Volume balance check for the offshore surface zone
	#   plot(fdriverso_inflow[,2] + fdrivers_upwell[,2] + fdriversi_so_flow[,2]  -  fdriverso_si_flow[,2] - fdriverso_outflow[,2],type="l")
	#   abline(h=0)
	#   sum((fdriverso_inflow[,2] + fdrivers_upwell[,2] + fdriversi_so_flow[,2]  -  fdriverso_si_flow[,2] - fdriverso_outflow[,2]))

	forc = list(
		fdriversslight		= matrix(ncol=2,c(drtimes,(driversslight(drtimes)))),

		fdriverso_logespm	= matrix(ncol=2,c(drtimes,(driverso_logespm(drtimes)))),
		fdriversi_logespm	= matrix(ncol=2,c(drtimes,(driversi_logespm(drtimes)))),

		fdriverso_temp		= matrix(ncol=2,c(drtimes,(driverso_temp(drtimes)))),
		fdriverd_temp		= matrix(ncol=2,c(drtimes,(driverd_temp(drtimes)))),
		fdriversi_temp		= matrix(ncol=2,c(drtimes,(driversi_temp(drtimes)))),

		fdriverv_dif		= matrix(ncol=2,c(drtimes,(driverv_dif(drtimes)))),

		fdriverso_inflow	= fdriverso_inflow,
		fdriverd_inflow		= fdriverd_inflow,
		fdriversi_inflow	= fdriversi_inflow,

		fdriverso_outflow	= fdriverso_outflow,
		fdriverd_outflow	= fdriverd_outflow,
		fdriversi_outflow	= fdriversi_outflow,

		fdriverso_si_flow	= fdriverso_si_flow,
		fdriversi_so_flow	= fdriversi_so_flow,

		fdrivers_upwell		= fdrivers_upwell,

		fdriverriver		= matrix(ncol=2,c(drtimes,(driverriver(drtimes)))),

		fdriverboundso_det	= matrix(ncol=2,c(drtimes,(driverboundso_det(drtimes)))),
		fdriverboundd_det	= matrix(ncol=2,c(drtimes,(driverboundd_det(drtimes)))),
		fdriverboundsi_det	= matrix(ncol=2,c(drtimes,(driverboundsi_det(drtimes)))),

		fdriverboundso_amm	= matrix(ncol=2,c(drtimes,(driverboundso_amm(drtimes)))),
		fdriverboundd_amm	= matrix(ncol=2,c(drtimes,(driverboundd_amm(drtimes)))),
		fdriverboundsi_amm	= matrix(ncol=2,c(drtimes,(driverboundsi_amm(drtimes)))),

		fdriverboundso_nit	= matrix(ncol=2,c(drtimes,(driverboundso_nit(drtimes)))),
		fdriverboundd_nit	= matrix(ncol=2,c(drtimes,(driverboundd_nit(drtimes)))),
		fdriverboundsi_nit	= matrix(ncol=2,c(drtimes,(driverboundsi_nit(drtimes)))),

		fdriverboundso_phyt	= matrix(ncol=2,c(drtimes,(driverboundso_phyt(drtimes)))),
		fdriverboundd_phyt	= matrix(ncol=2,c(drtimes,(driverboundd_phyt(drtimes)))),
		fdriverboundsi_phyt	= matrix(ncol=2,c(drtimes,(driverboundsi_phyt(drtimes)))),

		fdriverboundriv_amm	= matrix(ncol=2,c(drtimes,(driverboundriv_amm(drtimes)))),
		fdriverboundriv_nit	= matrix(ncol=2,c(drtimes,(driverboundriv_nit(drtimes)))),
		fdriverboundriv_det	= matrix(ncol=2,c(drtimes,(driverboundriv_det(drtimes)))),

		fdriverso_atm_amm	= matrix(ncol=2,c(drtimes,(driverso_atm_amm(drtimes)))),
		fdriverso_atm_nit	= matrix(ncol=2,c(drtimes,(driverso_atm_nit(drtimes)))),
		fdriversi_atm_amm	= matrix(ncol=2,c(drtimes,(driversi_atm_amm(drtimes)))),
		fdriversi_atm_nit	= matrix(ncol=2,c(drtimes,(driversi_atm_nit(drtimes)))),

		fdriver_s1_erosion	= matrix(ncol=2,c(drtimes,(driver_s1_pd(drtimes)))),
		fdriver_s2_erosion	= matrix(ncol=2,c(drtimes,(driver_s2_pd(drtimes)))),
		fdriver_s3_erosion	= matrix(ncol=2,c(drtimes,(driver_s3_pd(drtimes)))),
		fdriver_d1_erosion	= matrix(ncol=2,c(drtimes,(driver_d1_pd(drtimes)))),
		fdriver_d2_erosion	= matrix(ncol=2,c(drtimes,(driver_d2_pd(drtimes)))),
		fdriver_d3_erosion	= matrix(ncol=2,c(drtimes,(driver_d3_pd(drtimes)))),

		fdriver_s_wave		= matrix(ncol=2,c(drtimes,(drivers_wave(drtimes)))),

		fdriverpfish_sp		= matrix(ncol=2,c(sprectimes,(driverpfish_sp(sprectimes)))),
		fdriverpfish_rec	= matrix(ncol=2,c(sprectimes,(driverpfish_rec(sprectimes)))),
		fdriverdfish_sp		= matrix(ncol=2,c(sprectimes,(driverdfish_sp(sprectimes)))),
		fdriverdfish_rec	= matrix(ncol=2,c(sprectimes,(driverdfish_rec(sprectimes)))),


		fdriverbs_sp		= matrix(ncol=2,c(sprectimes,(driverbs_sp(sprectimes)))),
		fdriverbs_rec		= matrix(ncol=2,c(sprectimes,(driverbs_rec(sprectimes)))),
		fdriverbc_sp		= matrix(ncol=2,c(sprectimes,(driverbc_sp(sprectimes)))),
		fdriverbc_rec		= matrix(ncol=2,c(sprectimes,(driverbc_rec(sprectimes)))),

		fdrivermfish_im		= matrix(ncol=2,c(sprectimes,(driver_migfish_imig(sprectimes)))),
		fdrivermfish_em		= matrix(ncol=2,c(sprectimes,(driver_migfish_emig(sprectimes))))
	)
}

