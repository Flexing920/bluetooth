
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

## plotting functions are currently in server file itself

################### New WAVE functions ##############



wave_speed_process <- function(data) {

  data$speed <- ifelse(data$speed > 55, 35, data$speed)


  data <- dplyr::group_by(data, period, hour, minute) %>% dplyr::summarize(avg_speed = mean(speed), std_speed = sd(speed))
 
  data <- dplyr::arrange(data, hour, minute)

  data$hourlyspeed <- c(NA, NA, rollmean(data$avg_speed, 4), NA)
  data$hourlyspeed_std <- c(NA, NA, rollapply(data$avg_speed, 4, sd), NA)

  data <- subset(data, hour > 5)

  data$index <- 1:nrow(data)


  return(data) 
}

wave_speed_plot <- function(data, intname, direction) {


  q <- ggplot(data=data, aes(x=index, y=hourlyspeed, color=as.factor(period))) + geom_line() + geom_point()
  q <- q + scale_x_continuous(limits=c(0, 72), breaks=0:3 * 24, labels=0:3 * 6 + 6,
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

  data <- subset(data, hour > 5)

  data$index <- 1:nrow(data)


  return(data) 
}

wave_vol_plot <- function(data, intname, direction) {
	  

  q <- ggplot(data=data, aes(x=index, y=hourlyvolume, color=as.factor(period))) + geom_line() + geom_point()
  q <- q + scale_x_continuous(limits=c(0, 72), breaks=0:3 * 24, labels=0:3 * 6 + 6,
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

