df<-read.csv("equity data.csv",header=TRUE)
library(shiny)
library(ggplot2)
library(classInt)
library(ggthemes)
library(grid)

rank_and_nb_group<-function(df, var, order="descending"){
  df<-df
  df$var <- df[[var]]
  if(order=="descending"){
    d.order<-df[order(-df$var),]
  }
  if(order=="ascending"){
    d.order<-df[order(df$var),]
  }
  ranks<-1:length(df$var)
  d.rank<-cbind(d.order,ranks)
  names<-paste(d.rank$ranks,".",sep="")
  names<-paste(names,d.rank$Name)
  d.graph<-cbind(d.rank,names)
  
  breaks<-classIntervals(d.graph$var,3,style="jenks")
  d.graph$color<-NA
  d.graph$color[d.graph$var<=breaks$brks[2]]<-"green"
  d.graph$color[d.graph$var>breaks$brks[2] & d.graph$var<=breaks$brks[3]]<-"yellow"
  d.graph$color[d.graph$var>breaks$brks[3]]<-"red"
  d.graph$round<-round(d.graph$var,2)
  d.graph$textfont<-"plain"
  d.graph$textfont[d.graph$Name=="Louisville"]<-"bold"
  d.graph$linecolor<-"white"
  d.graph$linecolor[d.graph$Name=="Louisville"]<-"black"
  d.graph
  
  p<-ggplot(data=d.graph,aes(x=factor(names, levels=rev(unique(names))),
                        y=var*100,fill=factor(color)))+guides(fill=FALSE)
  p<-p+geom_bar(stat="identity",color=rev(d.graph$linecolor))+coord_flip()+theme_tufte()
  if(order=="ascending"){
    p<-p+scale_fill_manual(values=c("green3","red2","yellow2"))
  }
  if(order=="descending"){
    p<-p+scale_fill_manual(values=c("red2","green3","yellow2"))
  }
  p<-p+theme(axis.text.y=element_text(hjust=0,face=rev(d.graph$textfont),
                                      size=12))
  p<-p+theme(axis.ticks=element_blank(),axis.text.x=element_blank())
  p<-p+geom_text(aes(label=round),hjust=1.1,size=5,fontface="bold")
  p<-p+labs(title="",x="",
            y="")
  p
}


shinyServer(
  function(input, output) {
    output$plot1 <- renderPlot({
      if(input$peer_list=="Current"){
        df<-subset(df, Current == 1)
      }
      if(input$peer_list=="Baseline"){
        df<-subset(df, Baseline ==1)
      }
      var1<-df[input$var1]
      var2<-df[input$var2]
      df$textfont<-"plain"
      df$textfont[df$Display=="LOU"]<-"bold"
      df$textcolor<-"black"
      df$textcolor[df$Display=="LOU"]<-"blue"
      p<-ggplot(df, aes(x=var1,y=var2))
      p<-p+geom_smooth(method="lm",se=FALSE, color="black", size=.5)
      p<-p+geom_text(aes(label=Display),fontface=df$textfont, color=df$textcolor)
      p<-p+theme_bw()
      title_text<-paste(input$var1,"and",input$var2,sep=" ")
      p<-p+labs(title=title_text,x=input$var1,
                     y=input$var2)
      p
    })
    output$plot2<-renderPlot({
      if(input$peer_list=="Current"){
        df<-subset(df, Current == 1)
      }
      if(input$peer_list=="Baseline"){
        df<-subset(df, Baseline ==1)
      }
      p2<-rank_and_nb_group(df,input$var1, order="descending")
      p2<-p2+labs(title=input$var1)
      p2
  })
    output$plot3<-renderPlot({
      if(input$peer_list=="Current"){
        df<-subset(df, Current == 1)
      }
      if(input$peer_list=="Baseline"){
        df<-subset(df, Baseline ==1)
      }
      p3<-rank_and_nb_group(df,input$var2, order="descending")
      p3<-p3+labs(title=input$var2)
      p3
    })
    output$plot4<-renderPlot({
      if(input$peer_list=="Current"){
        df<-subset(df, Current == 1)
      }
      if(input$peer_list=="Baseline"){
        df<-subset(df, Baseline ==1)
      }
      var3<-df[[input$var3]]
      var4<-df[[input$var4]]
      df$var<-var4-var3
      d.order<-df[order(-df$var),]
      ranks<-1:length(df$var)
      d.rank<-cbind(d.order,ranks)
      names<-paste(d.rank$ranks,".",sep="")
      names<-paste(names,d.rank$Name)
      d.graph<-cbind(d.rank,names)
      
      breaks<-classIntervals(d.graph$var,3,style="jenks")
      d.graph$color<-NA
      d.graph$color[d.graph$var<=breaks$brks[2]]<-"green"
      d.graph$color[d.graph$var>breaks$brks[2] & d.graph$var<=breaks$brks[3]]<-"yellow"
      d.graph$color[d.graph$var>breaks$brks[3]]<-"red"
      d.graph$round<-round(d.graph$var,2)
      d.graph$textfont<-"plain"
      d.graph$textfont[d.graph$Name=="Louisville"]<-"bold"
      d.graph$linecolor<-"white"
      d.graph$linecolor[d.graph$Name=="Louisville"]<-"black"
      d.graph
      
      p<-ggplot(data=d.graph,aes(x=factor(names, levels=rev(unique(names))),
                                 y=var*100,fill=factor(color)))+guides(fill=FALSE)
      p<-p+geom_bar(stat="identity",color=rev(d.graph$linecolor))+coord_flip()+theme_tufte()
      p<-p+scale_fill_manual(values=c("red2","green3","yellow2"))
      p<-p+theme(axis.text.y=element_text(hjust=0,face=rev(d.graph$textfont),
                                          size=12))
      p<-p+theme(axis.ticks=element_blank(),axis.text.x=element_blank())
      p<-p+geom_text(aes(label=round),hjust=1.1,size=5,fontface="bold")
      title_text<-paste(input$var4,"minus",input$var3,sep=" ")
      p<-p+labs(title=title_text, y="", x="")
      p
    })
    output$plot5<-renderPlot({
      if(input$peer_list=="Current"){
        df<-subset(df, Current == 1)
      }
      if(input$peer_list=="Baseline"){
        df<-subset(df, Baseline ==1)
      }
      var3<-df[[input$var3]]
      var4<-df[[input$var4]]
      var2<-var4-var3
      var1<-df[input$var1]
      df$textfont<-"plain"
      df$textfont[df$Display=="LOU"]<-"bold"
      df$textcolor<-"black"
      df$textcolor[df$Display=="LOU"]<-"blue"
      p<-ggplot(df, aes(x=var1,y=var2))
      p<-p+geom_smooth(method="lm",se=FALSE, color="black", size=.5)
      p<-p+geom_text(aes(label=Display),fontface=df$textfont, color=df$textcolor)
      p<-p+theme_bw()
      title_text<-paste(input$var1,"and",input$var4,"minus",input$var3,sep=" ")
      y_text<-paste(input$var4,"minus",input$var3,sep=" ")
      p<-p+labs(title=title_text,x=input$var1,
                y=y_text)
      p
    })



})    