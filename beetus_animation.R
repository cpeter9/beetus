# Gentry Carelink import / analysis
# Author: Christopher Peters
# e-mail: cpeter9@gmail.com

# install.packages("zoo")
library(zoo)

# install.packages("ggplot2")
library(ggplot2)

beetus.df1 <- read.csv("C:/R_stuff/gentrys_beetus_folder/gentrys_beetus_folder/data/gentry_03012013.csv",
                      skip = 11, header = TRUE, stringsAsFactors = FALSE)
beetus.df1$Date <- as.POSIXct(beetus.df1$Date, "%m/%d/%Y", tz = "UTC")

beetus.df2 <- read.csv("C:/R_stuff/gentrys_beetus_folder/gentrys_beetus_folder/data/gentry_03252013.csv",
                      skip = 11, header = TRUE, stringsAsFactors = FALSE)
beetus.df2$Date <- as.POSIXct(beetus.df2$Date, "%d/%m/%y", tz = "UTC")

beetus.df3 <- read.csv("C:/R_stuff/gentrys_beetus_folder/gentrys_beetus_folder/data/gentry_04172013.csv",
                       skip = 10, header = TRUE, stringsAsFactors = FALSE)
beetus.df3$Date <- as.POSIXct(beetus.df3$Date, "%d/%m/%y", tz = "UTC")

beetus.df <- rbind(beetus.df1, beetus.df2, beetus.df3)

# beetus.df <- beetus.df[!duplicated(beetus.df$Timestamp), ]

beetus.df$Date <- as.Date(beetus.df$Date, format = "%m/%d/%Y")
# beetus.df$Date <- as.Date(paste("2012", substr(beetus.df$Date, 6, 10), sep = "-"))

beetus.df$Timestamp <- paste(beetus.df$Date, beetus.df$Time, sep = " ")

beetus.df$Timestamp <- as.POSIXct(beetus.df$Timestamp, format = "%Y-%m-%d %H:%M:%S")

# Factor to convert mmmol/L to mg/DL is 18.0182
mmol_to_mg <- 18.0182

# from meter
beetus.df$meter_blood_glucose <- beetus.df$BG.Reading..mmol.L. * mmol_to_mg
beetus.df$sensor_blood_glucose <- beetus.df$Sensor.Glucose..mmol.L. * mmol_to_mg

# BG
# beetus.df$bg <- beetus.df$sensor_blood_glucose # turning off sensor data for now
# beetus.df$bg[!is.na(beetus.df$meter_blood_glucose)] <- beetus.df$meter_blood_glucose[!is.na(beetus.df$meter_blood_glucose)]
beetus.df$bg <- beetus.df$meter_blood_glucose

# Change name of correction bolus estimate
names(beetus.df)[26] <- "correction_bolus"

# Bg input
names(beetus.df)[25] <- "bg_input"
beetus.df$bg_input <- with(beetus.df, bg_input * mmol_to_mg)


beetus.df <- beetus.df[!is.na(beetus.df$bg) | (!is.na(beetus.df$correction_bolus) & beetus.df$correction_bolus > 0), ]

# Correct time, convert to decimal hours
beetus.df$Time     <- as.POSIXct(beetus.df$Timestamp)
beetus.df$Time <- as.POSIXlt(beetus.df$Time)$hour + 
                    as.POSIXlt(beetus.df$Time)$min/60 + 
                    as.POSIXlt(beetus.df$Time)$sec/3600

beetus.df$Date <- as.Date(beetus.df$Date)

# ggplot() + 
#   geom_line(data = beetus.df[beetus.df$Date >= "2012-09-05" & beetus.df$Date <= "2012-09-10" & !is.na(beetus.df$bg), ],
#             aes(x = Time, y = bg, colour = factor(Date), group = factor(Date)), size = 2) +
#               scale_x_continuous(breaks = seq(0, 24, 2)) +
#               scale_y_continuous(limits = c(50, 280), breaks = seq(50, 280, 20)) +
#               xlab("Time") +
#               geom_hline(yintercept = c(90, 150)) +
#               ggtitle("Gentry's Beetus Data") +
#               scale_colour_discrete(name = "Date") +
#               theme(axis.text.y = element_text(colour = "black", size = 14),
#                     axis.text.x = element_text(colour = "black", size = 14, angle= -90, hjust=0)) +
#                      geom_hline(aes(yintercept = 70), colour = "red", size = 2) +
#                      geom_point(data = beetus.df[beetus.df$Date >= "2012-09-07" & beetus.df$Date <= "2012-09-10" & !is.na(beetus.df$correction_bolus), ],
#                                 aes(x = Time, y = bg_input, size = correction_bolus), colour = "blue")


# install.packages("animation")
  library(animation)

# install.packages("ggthemes")
  library(ggthemes)

# install.packages("pspline")
library(pspline)

# beetus.df <- beetus.df[beetus.df$Date > ""]

# Fit p-spline
source("pspline.R")

beetus.df <- beetus.df[!is.na(beetus.df$bg) & !is.na(beetus.df$Time), ]

# install.packages("splines")
library(splines)

# Sort all times
beetus.df <- beetus.df[order(beetus.df$Date), ]
beetus.df$Date <- as.Date(beetus.df$Date)

saveMovie({
for(i in beetus.df$Date[beetus.df$Date > as.Date("2013-02-01")]){
    
  i <- as.Date(i)
  end <- as.Date(i) 
  days_included <- 7
  
  temp <- beetus.df[beetus.df$Date <= end & beetus.df$Date >= as.Date((end - days_included)), ] 
  
  if(length(temp$bg) >= 5){
  plot(temp$Time, temp$bg, main = paste(i), ylim = c(0, 300), xlim = c(0, 24), 
       col = ifelse(temp$bg < 70 | temp$bg > 200, "red", "black"),
       cex = ifelse(temp$bg < 70 | temp$bg > 200, 2, 1))
    fit <- sm.spline(temp$Time, temp$bg, spar = 10, norder = 4)
      lines(fit, col = "blue")
      lines(sm.spline(temp$Time, temp$bg, df = 10), lty = 2, col = "purple")
      abline(a = 0, b = 0, h = 70, col = "orange")
      abline(a = 0, b = 0, h = 120, col = "orange")
  } else {plot(temp$Time, temp$bg, main = paste(i), ylim = c(0, 300), xlim = c(0, 24))}
  }
}, interval = 0.1, movie.name = "test.gif", ani.width = 600, ani.height = 600)

  

predict(fit, as.data.frame(list(Time = beetus.df$Time, bg = NA)))


spline <- signal.fit(beetus$bg)

saveMovie({
  for(i in beetus.df[!duplicated(beetus.df$Date), "Date"][-c(1:3)]){
    end <- as.Date(i) 
    days_included <- 7
    print(ggplot(beetus.df[beetus.df$Date >= (end - days_included) & beetus.df$Date < end, ], aes(x = Time, y = bg)) +
            geom_smooth(linetype = 0) +
            geom_hline(yintercept = 70, colour = "red", size = 1) +
            geom_hline(yintercept = 150, colour = "red", size = 1) +
            geom_point() +
            ylab("Blood Glucose") +
            xlab(paste("Seven days ending:", format(end, "%B %d, %Y"))) +
            scale_y_continuous(limits = c(60, 250), breaks = seq(0, 250, 25)) +
            scale_x_continuous(limits = c(0, 23), breaks = seq(0, 24, 2)) +
            theme_few() +
            theme(axis.title = element_text(size = 16),
                  axis.text = element_text(size = 20))
          )
  }
}, interval = 0.75, movie.name = "test.gif", ani.width = 600, ani.height = 600)


end <- as.Date("2013-02-12")
days_included <- 7
ggplot(beetus.df[beetus.df$Date >= (end - days_included) & beetus.df$Date < end, ], aes(x = Time, y = bg)) +
  geom_smooth(linetype = 0) +
  geom_hline(yintercept = 70, colour = "red", size = 1) +
  geom_hline(yintercept = 150, colour = "red", size = 1) +
  geom_point() +
  ylab("Blood Glucose") +
  xlab(paste("Seven days ending:", format(end, "%B %d, %Y"))) +
  scale_y_continuous(limits = c(60, 250), breaks = seq(0, 250, 25)) +
  scale_x_continuous(limits = c(0, 23), breaks = seq(0, 24, 2)) +
  theme_few() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 20))

library(MASS)

fitdistr(beetus.df$bg[!is.na(beetus.df$bg)], "weibull")

ggplot(beetus.df, aes(x = bg)) + 
  geom_histogram() +
  stat_function(fun = dweibull,
                args = c(shape = 2.75,
                         scale = 160.05),
                colour = "blue")


quantile(beetus.df$bg[as.Date(beetus.df$Date) > as.Date("2013-03-01")], seq(0, 1, 0.05), na.rm = TRUE)

##########
# Random Graphs
##########

