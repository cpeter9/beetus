# Gentry Carelink import / analysis
# Author: Christopher Peters
# e-mail: cpeter9@gmail.com

# install.packages("zoo")
library(zoo)

# install.packages("ggplot2")
library(ggplot2)

beetus.df <- read.csv("C:/R_stuff/gentrys_beetus_folder/gentrys_beetus_folder/data/gentry_03012013.csv",
                      skip = 11, header = TRUE, stringsAsFactors = FALSE)

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

# beetus.df <- beetus.df[beetus.df$Date > ""]

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
                  axis.text = element_text(size = 20)))
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




