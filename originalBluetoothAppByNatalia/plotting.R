text_axis_legend_size=9
text_axis_size=20
title_size=25
label_angle=45
testdata<-read.csv("lalalala",header=TRUE)

plot1_data<-testdata[testdata$day_type=="Weekday",]
# Change to weekend for other options.

  p1_data<-plot1_data
  #Because of the data frame, the first column ("order") was added to avoid moving column names
  # p1_data<-setNames(p1_data,c("order","tod","dir","day_type","seq","period","location_o","location_d","ttime"))     
  p1_data$period<-factor(p1_data$period,levels=c("Period 1","Period 2"))
  
  plot_total<-ggplot()+geom_line(data=p1_data,aes(x=tod,y=ttime.y.mn,group=period,color=period),size=1.5)
  plot_total<-plot_total+geom_ribbon(data=p1_data,aes(ymax=ttime.y.mn+2*ttime.y.n,ymin=ttime.y.mn-2*ttime.y.n,group=period,color=period,x=tod),alpha=0.1)
  plot_total<-plot_total+facet_wrap(~ dir)        
  plot_total<-plot_total+
    theme(axis.text.x = element_text(size=text_axis_legend_size),axis.title.x=element_text(size=text_axis_size),
                               axis.text.y = element_text(size=text_axis_legend_size),axis.title.y=element_text(size=text_axis_size))+
    xlab("Hour")+
    ylab("Travel Time (sec)")+
    theme(legend.position="bottom")+
    theme(plot.title = element_text(size=title_size, face="bold"))+
    theme(axis.text.x = element_text(angle = label_angle, hjust = 1))
  
  plot_total
  