
library(RPostgreSQL)
library(rgdal)
library(rgeos)
library(leaflet)
library(sp)
library(DT)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(dplyr)

###################### Functions #######################
subset_data <- function(data, day, dir) {

	# Select the corresponoding week

	sunday <- floor_date(ymd(day),"week")
	saturday <- as.Date(sunday) + 6

	select_data <- data %>% mutate(date = as.Date(curdatetime)) %>%
	filter(date >= sunday & date <= saturday) %>% filter(direction == dir) %>%
	select(curdatetime, speed, volume)

	week_data <- select_data %>% group_by(curdatetime) %>% summarize(sum_vol=sum(na.omit(volume)))
	aux_speed <- select_data %>% group_by(curdatetime) %>% summarize(ave_speed=mean(na.omit(speed)))
	week_data$ave_speed <- ifelse(aux_speed$ave_speed > 65, 30, aux_speed$ave_speed)

	return(week_data)

}


process_data <- function(week_data){
  # Function process volume and data from sensors  
  # and select a specific week (sunday to saturday) and 
  # specific intersection and direction to show
  # Input
  #       day: Day of the week to show in POSIXt "%Y:%m:%d"
  #       int: Intersection name in "character"
  #       dir: Traffic direction in "character" ---> note: EB, WB, NB, SB and None directions available
  # Output
  #       week_data: processed volume and speed data for 
  #                   the corresponding week as a "list"
  
  week_data <- week_data %>% 
    mutate(date = as.Date(curdatetime)) %>%
    mutate(wday = wday(date)) %>%
    mutate(hour = format(week_data$curdatetime, format = "%H:%M:%S")) %>%
    mutate(hour = as.POSIXct(hour, format = "%H:%M:%S")) 
  
  week_data <- arrange(week_data, curdatetime) %>% group_by(date)
  
  #Estimate hourly volume and speed 
  week_data$hourlyvolume <- c(NA, NA, rollapply(week_data$sum_vol, 4, sum), NA)
  week_data$hourlyspeed <- c(NA, NA, rollmean(week_data$ave_speed, 4), NA)
  
  return(week_data)

}

vol_plot <- function(week_example, int, dir){
  # Function to plot hourly volume of the selected week
  # Input
  #       day: Day of the week to show in POSIXt "%Y:%m:%d"
  #       int: Intersection name in "character"
  #       dir: Traffic direction in "character"
  # Output
  #       figure: plot of hourly-volume per day hour for the seven days
  
  # Volumen plot

  plot_data <- subset(week_example, !(wday %in% c(1, 7)))
  plot_data <- subset(plot_data, hour >= as.POSIXct('5', format = "%H"))

  p <- ggplot(data = plot_data, aes(x = hour, y = hourlyvolume, color = period)) + 
    ggtitle(paste(int, "-", dir)) + ylab("Volume (vph)") +
    scale_x_datetime(name = "Hour", date_breaks = "1 hour", labels = date_format("%H:%M")) + 
    stat_summary(fun.data=mean_se, geom="errorbar") + stat_summary(geom="line") +
    stat_summary(geom="point")

  p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=11),
    axis.title.x = element_text(size=16),
    axis.text.y = element_text(size=11),
    axis.title.y=element_text(size=16), plot.title=element_text(size=20, face='bold'))

  p <- p + theme(legend.title=element_blank())

  return(p)
}

speed_plot <- function(week_example, int, dir){
  # Function to plot hourly-averaged speed of the selected week
  # Input
  #       day: Day of the week to show in POSIXt "%Y:%m:%d"
  #       int: Intersection name in "character"
  #       dir: Traffic direction in "character"
  # Output
  #       figure: plot of hourly-averaged speed per day hour for the seven days

  
  # Speed plot
  plot_data <- subset(week_example, !(wday %in% c(1, 7)))
  plot_data <- subset(plot_data, hour >= as.POSIXct('5', format = "%H"))

  p <- ggplot(data = plot_data, aes(x = hour, y = hourlyspeed, color = period)) + 
    ggtitle(paste(int, "-", dir)) + ylab("Speed (mph)") +
    scale_x_datetime(name = "Hour", date_breaks = "1 hour", 
      labels = date_format("%H:%M"), expand = c(0, 0)) + 
    stat_summary(fun.data=mean_se, geom="errorbar") + stat_summary(geom="line") +
    stat_summary(geom="point")

  p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=11),
    axis.title.x = element_text(size=16),
    axis.text.y = element_text(size=11),
    axis.title.y=element_text(size=16), plot.title=element_text(size=20, face='bold'))

  p <- p + theme(legend.title=element_blank())

  return(p)
}

intersection_peak_filter <- function (data, peak_filter) {

  if (length(peak_filter) == 3) {

    return(data)

  } else if (length(peak_filter) == 2) {

    if (!('off_peak' %in% peak_filter)) {

      return(subset(data, (am_peak == 1) | (pm_peak == 1) | (off_peak == 0)))

    } else {

      if (!('am_peak' %in% peak_filter)) {

        return(subset(data, (pm_peak == 1) | (off_peak == 1)))

      } else {

        return(subset(data, (am_peak == 1) | (off_peak == 1)))
      }

    } 

  } else if (length(peak_filter == 1)) {

    if ('am_peak' %in% peak_filter) {

      return(subset(data, am_peak == 1))

    } else if ('pm_peak' %in% peak_filter) {

      return(subset(data, pm_peak == 1))
    
    } else {

      return(subset(data, off_peak == 1))

    } 

  } else {

    return(subset(data, am_peak == 0 & pm_peak == 0 & off_peak == 0))

  }

}


intersection_mod_filter <- function(data, mod_filter) {

  if (length(mod_filter) == 1) {

    if ('autos' %in% mod_filter) {

      return(subset(data, autos == 1))

    } else if ('heavy' %in% mod_filter) {

      return(subset(data, heavy == 1))

    } else if ('peds' %in% mod_filter) {

      return(subset(data, peds == 1)) 

    } else {

      return(subset(data, bike == 1))

    }

    } else if (length(mod_filter) == 2) {

      if ('autos' %in% mod_filter) {

        if ('heavy' %in% mod_filter) {

          return(subset(data, autos == 1 | heavy == 1))

        } else if ('peds' %in% mod_filter) {

          return(subset(data, autos == 1 | peds == 1))

        } else {

          return(subset(data, autos == 1 | bike == 1))

        }

      } else if ('heavy' %in% mod_filter) {

        if ('peds' %in% mod_filter) {

          return(subset(data, heavy == 1 | peds == 1))

        } else {

          return(subset(data, heavy == 1 | bike == 1))

        }

      } else {

        return(subset(data, bike == 1 | peds == 1))

      }

    } else if (length(mod_filter) == 3) {

      if (!('autos' %in% mod_filter)) {

        return(subset(data, heavy == 1 | bike == 1 | peds == 1))

      } else if (!('heavy' %in% mod_filter)) {

        return(subset(data, autos == 1 | bike == 1 | peds == 1))

      } else {

        if ('peds' %in% mod_filter) {

          return(subset(data, autos == 1 | heavy == 1 | peds == 1))

        } else {

          return(subset(data, autos == 1 | heavy == 1 | bike == 1))

        }
        
      }

    } else if (length(mod_filter == 4)) {

      return(data)

    } else {

      return(subset(data, autos == 0 & heavy == 0 & peds == 0 & bike == 0))

    }
  }


intersection_counts_filter <- function(data, counts_filter) {

  if (length(counts_filter) > 1) {

    return(data)

  } else if (counts_filter == 1) {

    return(subset(data, tmc == 1))

  } else {

    return(subset(data, tmc == 0))
    
  }
}



################### APC functions ##############

dwell_time_plot <- function(data, corr_name) {

  pd <- position_dodge(0.1)

  p <- ggplot(data=data, aes(x=hour, y=dwell_time, color=as.factor(year))) 
  p <- p + geom_line(size=1.5) 
  p <- p + geom_line(aes(y = dwell_time - error), linetype='dotted')
  p <- p + geom_line(aes(y = dwell_time + error), linetype='dotted')
  p <- p + xlab("Hour") + scale_x_continuous(breaks=c(0:23))
  p <- p + scale_y_continuous(limits = c(0, 30), breaks=c(0:5 * 5))
  p <- p + ggtitle(paste(corr_name, ' Corridor Weekday Dwell Time by Hour')) + ylab("Dwell Time (mins)")
  p <- p + scale_fill_discrete(name= "Year")
  p <- p + theme(axis.text.x = element_text(size=14), plot.title=element_text(hjust = 0.5, size=18),
    axis.text.y= element_text(size=14), legend.title=element_blank(), legend.text=element_text(size=14),
    axis.title.y = element_text(size=16), axis.title.x = element_text(size=16))

  return(p)

}

################### New WAVE functions ##############

# wave_speed_agg_clean <- function(data) {

  

#   return(data)

# }

wave_speed_process <- function(data) {

  data$speed <- ifelse(data$speed > 55, 35, data$speed)


  data <- dplyr::group_by(data, period, hour, minute) %>% dplyr::summarize(avg_speed = mean(speed), std_speed = sd(speed))
 
  data <- dplyr::arrange(data, hour, minute)

  data$hourlyspeed <- c(NA, NA, rollmean(data$avg_speed, 4), NA)
  data$hourlyspeed_std <- c(NA, NA, rollapply(data$avg_speed, 4, sd), NA)

  data$index <- 1:nrow(data)

  return(data) 
}

wave_speed_plot <- function(data, intname, direction) {

  q <- ggplot(data=data, aes(x=index, y=hourlyspeed, color=as.factor(period))) + geom_line() + geom_point()
  q <- q + scale_x_continuous(limits=c(0, 96), breaks=1:12 * 8, labels=1:12 * 2,
    name="Time of Day", expand = c(0, 0))
  q <- q + geom_errorbar(aes(ymin=hourlyspeed - hourlyspeed_std, ymax=hourlyspeed + hourlyspeed_std))
  q <- q + theme(axis.text.x = element_text(size=11), 
    axis.title.x = element_text(size=16),
    axis.text.y = element_text(size=11),
    axis.title.y=element_text(size=16), plot.title=element_text(size=20, face='bold'), 
    legend.title=element_blank())
  q <- q + ggtitle(paste(intname, "-", direction)) + ylab("Speed (mph)")

  pp <- ggplotly(q)

  return(pp)

}

wave_vol_process <- function(data) {

  data <- dplyr::group_by(data, period, hour, minute) %>% dplyr::summarize(avg_vol = mean(volume), std_vol = sd(volume))
 
  data <- dplyr::arrange(data, hour, minute)

  data$hourlyvolume <- c(NA, NA, rollmean(data$avg_vol, 4), NA)
  data$hourlyvolume_std <- c(NA, NA, rollapply(data$avg_vol, 4, sd), NA)

  data$index <- 1:nrow(data)

  return(data) 
}

wave_vol_plot <- function(data, intname, direction) {

  q <- ggplot(data=data, aes(x=index, y=hourlyvolume, color=as.factor(period))) + geom_line() + geom_point()
  q <- q + scale_x_continuous(limits=c(0, 96), breaks=1:12 * 8, labels=1:12 * 2,
    name="Time of Day", expand = c(0, 0))
  q <- q + geom_errorbar(aes(ymin=hourlyvolume - hourlyvolume_std, ymax=hourlyvolume + hourlyvolume_std))
  q <- q + theme(axis.text.x = element_text(size=11), 
    axis.title.x = element_text(size=16),
    axis.text.y = element_text(size=11),
    axis.title.y=element_text(size=16), plot.title=element_text(size=20, face='bold'), 
    legend.title=element_blank())
  q <- q + ggtitle(paste(intname, "-", direction)) + ylab("Number of Cars")

  pp <- ggplotly(q)

  return(pp)

}

