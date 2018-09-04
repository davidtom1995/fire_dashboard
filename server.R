

library(shiny)
library(ncdf4)
library(RColorBrewer)
library(lattice)
library(abind)
library(chron)


calibration_plot = function(dat, ftype){
  dat1 = dat[dat$n.fires != 0 & dat$dpft == ftype-1,]
  dat0 = dat[dat$n.fires == 0 & dat$dpft == ftype-1,]
  maxf <- ceiling(max(dat$fire.index)*1000)/1000
  minf <- floor(min(dat$fire.index)*1000)/1000
  brks <- seq(minf,maxf, by=(maxf-minf)/16)
  h <- hist(dat0$fire.index,breaks=brks, plot=F)
  cnts <- h$counts/sum(h$counts)
  mds <- h$mids
  h1 <- hist(dat1$fire.index,breaks=brks, plot=F)
  cnts1 <- h1$counts/sum(h1$counts)
  mds1 <- h1$mids
  cntmax1 <- 0.1*ceiling(max(cnts)*10)+0.1
  cntmax2 <- 0.1*ceiling(max(cnts1)*10)+0.1
  cntmax = 0.5 #max(cntmax1, cntmax2)
  pfire = h1$counts/(h$counts+h1$counts)
  plot(pfire~mds1, type="o", col=rgb(.6,.6,.6), pch=20, lwd=2, ylim=c(0,1))
  text(y = 1-0.1, x = brks[8], labels = paste(pfts[ftype]), cex=1.2)
  pfire[h1$counts < 50] = NA
  points(pfire~mds1, type="o", col=rgb(.1,.1,.1), pch=20, lwd=2)
  abline(0,1, col="red")
}


shinyServer(function(input, output) {


    p<- reactivePoll(1000,session = NULL,
      checkFunc = function() {
        setwd("/home/david/codes/infisim_analysis_india/src")
        if (file.exists("code1.txt"))
          file.info("code1.txt")$mtime[1]
        else
          ""
        },
      valueFunc = function() {
        setwd("/home/david/codes/infisim_analysis_india/src")
        readLines("code1.txt")
      })

    output$code1<-renderText({paste(p(),collapse="\n")})


    q<- reactivePoll(1000,session = NULL,
                     checkFunc = function() {
                       setwd("/home/david/Documents/FIRE_PROJECT_FINAL/Fire_simulator_codes/tensorflow_code/")
                       if (file.exists("tf_running_code.txt"))
                         file.info("tf_running_code.txt")$mtime[1]
                       else
                         ""
                     },
                     valueFunc = function() {
                       setwd("/home/david/Documents/FIRE_PROJECT_FINAL/Fire_simulator_codes/tensorflow_code/")
                       readLines("tf_running_code.txt")
                     })

    output$code2<-renderText({paste(q(),collapse="\n")})


#variables--------------------------------------------------------------------------------

  observeEvent({
    input$Spin_date
    input$Sim_start_date
    input$spin
    input$Sim_end_date
    input$Spin_bio0},{
      setwd("/home/david/Documents/FIRE_PROJECT_FINAL/Fire_simulator_codes/fire_calcFuel/params_newdata/")
      x<-input$Spin_date
      simi<-input$Sim_start_date
      simf<-input$Sim_end_date
      i<-input$spin
      v<-input$Spin_bio0



      latin<- readLines("sim_config.r")
      latin[8]=paste("spinup",i,sep = "      ")
      latin[9]=paste("spin_bio0",v,sep = "   ")
      latin[10]=paste("spin_date0",x,sep ="  ")
      latin[11]=paste("sim_date0",simi,sep = "   ")
      latin[13]=paste("sim_datef",simf,sep = "   ")
      writeLines(latin,"sim_config.r")
      setwd("/home/david/Documents/FIRE_PROJECT_FINAL/Fire_simulator_codes/fire_aggregateData/params_newdata/")
      jatin<-readLines("sim_config.r")
      jatin[8]=paste("spinup",i,sep = "      ")
      jatin[9]=paste("spin_bio0",v,sep = "   ")
      jatin[10]=paste("spin_date0",x,sep ="  ")
      jatin[11]=paste("sim_date0",simi,sep = "   ")
      jatin[13]=paste("sim_datef",simf,sep = "   ")
      writeLines(latin,"sim_config.r")
      cat("done replacing files","\n")

      output$selected_dates<- renderText({paste0("You have selected this range:","  ",input$Sim_start_date," to ",input$Sim_end_date)})

      ncpath <- "/home/david/"
      ncname <- "fire.2001-2015"
      ncfname <- paste(ncpath, ncname, ".nc", sep="")
      ncin<- nc_open(ncfname)
      fire_array<-ncvar_get(ncin,"fire_events" )
      lon<- ncvar_get(ncin, "lon")
      lat<- ncvar_get(ncin, "lat")
      time<- ncvar_get(ncin,"time")
      tunits <- ncatt_get(ncin,"time","units")
      tustr <- strsplit(tunits$value, " ")
      tdstr <- strsplit(unlist(tustr)[3], "-")
      tmonth <- as.integer(unlist(tdstr)[2])
      tday <- as.integer(unlist(tdstr)[3])
      tyear <- as.integer(unlist(tdstr)[1])
      time_div<-chron(time,origin=c(tmonth, tday, tyear))
      time1<- as.Date(time_div)
      lon0 = which(lon<=66.5)[length(which(lon<=66.5))]
      lonf = which(lon>100.5)[1]
      lat0 = which(lat<=6.5)[length(which(lat<=6.5))]
      latf = which(lat>38.5)[1]
      longitude<-lon[lon0:lonf]
      latitude<- lat[lat0:latf]


      simi<-as.Date(input$Sim_start_date)
      simf<-as.Date(input$Sim_end_date)
      time_selected<- which(time1>=simi & time1<=simf)
      final_time<- time1[time_selected[1]:time_selected[length(which(time1>=simi & time1<=simf))]]

      fire_january<-fire_array[lon0:lonf,lat0:latf,which(as.numeric(strftime(final_time, "%m"))==1 )]
      fire_january<-apply(fire_january,FUN = mean,MARGIN = c(1,2))
      fire_february<-fire_array[lon0:lonf,lat0:latf,which(as.numeric(strftime(final_time, "%m"))==2 )]
      fire_february<-apply(fire_february,FUN = mean,MARGIN = c(1,2))
      fire_march<-fire_array[lon0:lonf,lat0:latf,which(as.numeric(strftime(final_time, "%m"))==3 )]
      fire_march<-apply(fire_march,FUN = mean,MARGIN = c(1,2))
      fire_april<-fire_array[lon0:lonf,lat0:latf,which(as.numeric(strftime(final_time, "%m"))==4 )]
      fire_april<-apply(fire_april,FUN = mean,MARGIN = c(1,2))
      fire_may<-fire_array[lon0:lonf,lat0:latf,which(as.numeric(strftime(final_time, "%m"))==5 )]
      fire_may<-apply(fire_may,FUN = mean,MARGIN = c(1,2))
      fire_june<-fire_array[lon0:lonf,lat0:latf,which(as.numeric(strftime(final_time, "%m"))==6 )]
      fire_june<-apply(fire_june,FUN = mean,MARGIN = c(1,2))
      fire_july<-fire_array[lon0:lonf,lat0:latf,which(as.numeric(strftime(final_time, "%m"))==7 )]
      fire_july<-apply(fire_july,FUN = mean,MARGIN = c(1,2))
      fire_august<-fire_array[lon0:lonf,lat0:latf,which(as.numeric(strftime(final_time, "%m"))==8 )]
      fire_august<-apply(fire_august,FUN = mean,MARGIN = c(1,2))
      fire_september<-fire_array[lon0:lonf,lat0:latf,which(as.numeric(strftime(final_time, "%m"))==9 )]
      fire_september<-apply(fire_september,FUN = mean,MARGIN = c(1,2))
      fire_october<-fire_array[lon0:lonf,lat0:latf,which(as.numeric(strftime(final_time, "%m"))==10 )]
      fire_october<-apply(fire_october,FUN = mean,MARGIN = c(1,2))
      fire_november<-fire_array[lon0:lonf,lat0:latf,which(as.numeric(strftime(final_time, "%m"))==11 )]
      fire_november<-apply(fire_november,FUN = mean,MARGIN = c(1,2))
      fire_december<-fire_array[lon0:lonf,lat0:latf,which(as.numeric(strftime(final_time, "%m"))==12 )]
      fire_december<-apply(fire_december,FUN = mean,MARGIN = c(1,2))
      cat("plotting observed values","\n")
      output$january_plot<-renderPlot({image(longitude,latitude,fire_january, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$february_plot<-renderPlot({image(longitude,latitude,fire_february, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$march_plot<-renderPlot({image(longitude,latitude,fire_march,col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100))})
      output$april_plot<-renderPlot({image(longitude,latitude,fire_april,col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100))})
      output$may_plot<-renderPlot({image(longitude,latitude,fire_may,col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100))})
      output$june_plot<-renderPlot({image(longitude,latitude,fire_june, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$july_plot<-renderPlot({image(longitude,latitude,fire_july, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$august_plot<-renderPlot({image(longitude,latitude,fire_august, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$september_plot<-renderPlot({image(longitude,latitude,fire_september, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$october_plot<-renderPlot({image(longitude,latitude,fire_october, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$november_plot<-renderPlot({image(longitude,latitude,fire_november, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$december_plot<-renderPlot({image(longitude,latitude,fire_december, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      cat("DONE", "\n")

      cat("reading predicted nc file", "\n")
      ncpath <- "/home/david/Documents/Predicted_fire_nc_data/"
      ncname <- "fire.2007-1-1-2007-12-31"
      ncfname <- paste(ncpath, ncname, ".nc", sep="")
      ncin<- nc_open(ncfname)
      fire_array<-ncvar_get(ncin,"fire" )
      lon<- ncvar_get(ncin, "lon")
      lat<- ncvar_get(ncin, "lat")
      time<- ncvar_get(ncin,"time")
      tunits <- ncatt_get(ncin,"time","units")

      tustr <- strsplit(tunits$value, " ")
      tdstr <- strsplit(unlist(tustr)[3], "-")
      tmonth <- as.integer(unlist(tdstr)[2])
      tday <- as.integer(unlist(tdstr)[3])
      tyear <- as.integer(unlist(tdstr)[1])
      time_div<-chron(time/24,origin=c(tmonth, tday, tyear))
      time1<- as.Date(time_div)

      simi<-as.Date(input$Sim_start_date)
      simf<-as.Date(input$Sim_end_date)

      time_selected<- which(time1>=simi & time1<=simf)
      final_time<- time1[time_selected[1]:time_selected[length(which(time1>=simi & time1<=simf))]]

      fire_january_predicted<-fire_array[,,which(as.numeric(strftime(final_time, "%m"))==01 )]
      fire_january_predicted<-apply(fire_january_predicted,FUN = mean,MARGIN = c(1,2))
      fire_february_predicted<-fire_array[,,which(as.numeric(strftime(final_time, "%m"))==2 )]
      fire_february_predicted<-apply(fire_february_predicted,FUN = mean,MARGIN = c(1,2))
      fire_march_predicted<-fire_array[,,which(as.numeric(strftime(final_time, "%m"))==3 )]
      fire_march_predicted<-apply(fire_march_predicted,FUN = mean,MARGIN = c(1,2))
      fire_april_predicted<-fire_array[,,which(as.numeric(strftime(final_time, "%m"))==4 )]
      fire_april_predicted<-apply(fire_april_predicted,FUN = mean,MARGIN = c(1,2))
      fire_may_predicted<-fire_array[,,which(as.numeric(strftime(final_time, "%m"))==5 )]
      fire_may_predicted<-apply(fire_may_predicted,FUN = mean,MARGIN = c(1,2))
      fire_june_predicted<-fire_array[,,which(as.numeric(strftime(final_time, "%m"))==6 )]
      fire_june_predicted<-apply(fire_june_predicted,FUN = mean,MARGIN = c(1,2))
      fire_july_predicted<-fire_array[,,which(as.numeric(strftime(final_time, "%m"))==7 )]
      fire_july_predicted<-apply(fire_july_predicted,FUN = mean,MARGIN = c(1,2))
      fire_august_predicted<-fire_array[,,which(as.numeric(strftime(final_time, "%m"))==8 )]
      fire_august_predicted<-apply(fire_august_predicted,FUN = mean,MARGIN = c(1,2))
      fire_september_predicted<-fire_array[,,which(as.numeric(strftime(final_time, "%m"))==9 )]
      fire_september_predicted<-apply(fire_september_predicted,FUN = mean,MARGIN = c(1,2))
      fire_october_predicted<-fire_array[,,which(as.numeric(strftime(final_time, "%m"))==10 )]
      fire_october_predicted<-apply(fire_october_predicted,FUN = mean,MARGIN = c(1,2))
      fire_november_predicted<-fire_array[,,which(as.numeric(strftime(final_time, "%m"))==11 )]
      fire_november_predicted<-apply(fire_november_predicted,FUN = mean,MARGIN = c(1,2))
      fire_december_predicted<-fire_array[,,which(as.numeric(strftime(final_time, "%m"))==12 )]
      fire_december_predicted<-apply(fire_december_predicted,FUN = mean,MARGIN = c(1,2))

      cat("plotting predicted plots", "\n")

      output$january_predic_plot<-renderPlot({image(lon,lat,fire_january_predicted, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$february_predic_plot<-renderPlot({image(lon,lat,fire_february_predicted, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$march_predic_plot<-renderPlot({image(lon,lat,fire_march_predicted,col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100))})
      output$april_predic_plot<-renderPlot({image(lon,lat,fire_april_predicted,col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100))})
      output$may_predic_plot<-renderPlot({image(lon,lat,fire_may_predicted,col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100))})
      output$june_predic_plot<-renderPlot({image(lon,lat,fire_june_predicted, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$july_predic_plot<-renderPlot({image(lon,lat,fire_july_predicted, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$august_predic_plot<-renderPlot({image(lon,lat,fire_august_predicted, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$september_predic_plot<-renderPlot({image(lon,lat,fire_september_predicted, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$october_predic_plot<-renderPlot({image(lon,lat,fire_october_predicted, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$november_predic_plot<-renderPlot({image(lon,lat,fire_november_predicted, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})
      output$december_predic_plot<-renderPlot({image(lon,lat,fire_december_predicted, col=colorRampPalette(colors = c("black", "red" ,"yellow", "white"))(100) )})

      cat("DONE", "\n")





      }
  )






  observeEvent({
    input$learn_rate
    input$batch_size
    input$n_steps},{
    setwd("/home/david/Documents/FIRE_PROJECT_FINAL/Fire_simulator_codes/tensorflow_code/")
      a<-input$learn_rate
      b<-input$batch_size
      c<-input$n_steps

      patin<-readLines("nn_const_data_fire_v2.py")
      patin[15]=paste("__learn_rate","=",a,sep = " ")
      patin[16]=paste("__batch_size","=",b,sep = " ")
      patin[17]=paste("__n_steps","=",c,sep = " ")
      writeLines(patin,"nn_const_data_fire_v2.py")
    })

#codes-------------------------------------------------------------------------------------
  observeEvent(input$simulate,{
    setwd("/home/david/Documents/FIRE_PROJECT_FINAL/Fire_simulator_codes/fire_calcFuel/src/")
    system("LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/david/Documents/FIRE_PROJECT_FINAL/Fire_simulator_codes/libgsm_v2/lib:/usr/local/netcdf-cxx-legacy/lib && ./fire")

  })


  observeEvent(input$TF_RUN,{
    setwd("/home/david/Documents/FIRE_PROJECT_FINAL/Fire_simulator_codes/tensorflow_code/")
    cat("wait....")
    system("sh runtf")

  })
  observeEvent(input$analysis,{
    setwd("/home/david/codes/infisim_analysis_india/src")
    system("LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/david/codes/libgsm/lib:/usr/local/netcdf-cxx-legacy/lib && ./fp")
  })
#--------------------------------------------------------------------------------------------------------------------------------

  dat<- read.csv("/home/david/Documents/FIRE_PROJECT_FINAL/Fire_simulator_codes/tensorflow_code/train_forest.csv")
  nn = read.delim(file = "/home/david/Documents/FIRE_PROJECT_FINAL/Fire_simulator_codes/tensorflow_code/y_predic.txt", header = F)
  dat$fire.index = nn$V1
  pfts <- c("X","AGR","NLE","BLE","MD", "DD", "GR", "SC")

  par(mfrow = c(3,2))
  par(cex = 0.8)
  par(mar = c(2,4,0,0))
  par(oma = c(1,1,1,1))
  par(tcl = -0.5)            # tick length
  par(mgp = c(2,0.6,0))

  # for(ftype in 3:8){


      output$predic_NLE<- renderPlot({calibration_plot(dat,3)})
      output$predic_BLE<- renderPlot({calibration_plot(dat,4)})
      output$predic_MD<- renderPlot({calibration_plot(dat,5)})
      output$predic_DD<- renderPlot({calibration_plot(dat,6)})
      output$predic_GR<- renderPlot({calibration_plot(dat,7)})
      output$predic_SC<- renderPlot({calibration_plot(dat,8)})






#       
#       

      
      
      #---------------------------OBSERVED_DATA-----------------------------------------------
      

    
      

  
    #    legend("topright", legend=c("% non-occurences", "% occurences"), col=c("red","blue"), lty=1, lwd=2)
    
})