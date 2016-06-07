df<-read.csv("equity data.csv",header=TRUE)
library(shiny)
library(ggplot2)
library(classInt)
library(ggthemes)
library(grid)

rank_and_nb_group<-function(df, var, order="descending"){
  df<-df
  df$var <- var
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
  d.graph$round<-format(round(d.graph$var, 2), nsmall = 2)
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
    
    #matching names with inputs
    var1 <- reactive({
      switch(input$var1, 
             "Low Income Female Life Expectancy" = df$le_agg_q1_F,
             "Low Income Male Life Expectancy" = df$le_agg_q1_M,
             "Low Income Smoking Percent"=df$cur_smoke_q1,
             "Low Income Obesity Rate"=df$bmi_obese_q1,
             "Low Income Exercise in last 30 days"=df$exercise_any_q1)
             
    })
    var2 <- reactive({
      switch(input$var2, 
             "Low Income Female Life Expectancy" = df$le_agg_q1_F,
             "Low Income Male Life Expectancy" = df$le_agg_q1_M,
             "Low Income Smoking Percent"=df$cur_smoke_q1,
             "Low Income Obesity Rate"=df$bmi_obese_q1,
             "Low Income Exercise in last 30 days"=df$exercise_any_q1)
    })
    var3 <- reactive({
      switch(input$var3, 
             "Low Income Female Life Expectancy" = df$le_agg_q1_F,
             "Low Income Male Life Expectancy" = df$le_agg_q1_M,
             "Low Income Smoking Percent"=df$cur_smoke_q1,
             "Low Income Obesity Rate"=df$bmi_obese_q1,
             "Low Income Exercise in last 30 days"=df$exercise_any_q1)
    })
    var4 <- reactive({
      switch(input$var4, 
             "Low Income Female Life Expectancy" = df$le_agg_q1_F,
             "Low Income Male Life Expectancy" = df$le_agg_q1_M,
             "Low Income Smoking Percent"=df$cur_smoke_q1,
             "Low Income Obesity Rate"=df$bmi_obese_q1,
             "Low Income Exercise in last 30 days"=df$exercise_any_q1)
    })

    order_one <- reactive({
      if(input$var1=="Low Income Obesity Rate"|
         input$var1=="Low Income Smoking Percent"){
        order<-"ascending"
      }else{
        order<-"descending"
        }
      order
    })
    
    order_two <- reactive({
      if(input$var2=="Low Income Obesity Rate"|
         input$var2=="Low Income Smoking Percent"){
        order<-"ascending"
      }else{
        order<-"descending"
      }
      order
    })
    

    output$rank1<-renderPlot({
      df$var1 <- var1()
      if(input$peer_list=="Current"){
        df<-subset(df, Current == 1)
      }
      if(input$peer_list=="Baseline"){
        df<-subset(df, Baseline ==1)
      }
      p2<-rank_and_nb_group(df,df$var1, order=order_one())
      p2<-p2+labs(title=input$var1)
      p2
  })
    output$rank2<-renderPlot({
      df$var2<-var2()
      if(input$peer_list=="Current"){
        df<-subset(df, Current == 1)
      }
      if(input$peer_list=="Baseline"){
        df<-subset(df, Baseline ==1)
      }
      p3<-rank_and_nb_group(df,df$var2, order=order_two())
      p3<-p3+labs(title=input$var2)
      p3
    })
    output$rank3<-renderPlot({
      df$var3<-var3()
      df$var4<-var4()
      df$var<-df$var3-df$var4
      if(input$peer_list=="Current"){
        df<-subset(df, Current == 1)
      }
      if(input$peer_list=="Baseline"){
        df<-subset(df, Baseline ==1)
      }
      d.order<-df[order(df$var),]
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
    output$scatter1 <- renderPlot({
      df$var1_s1<-var1()
      df$var2_s1<-var2()
      if(input$peer_list=="Current"){
        df<-subset(df, Current == 1)
      }
      if(input$peer_list=="Baseline"){
        df<-subset(df, Baseline ==1)
      }
      df$textfont<-"plain"
      df$textfont[df$Display=="LOU"]<-"bold"
      df$textcolor<-"black"
      df$textcolor[df$Display=="LOU"]<-"blue"
      p<-ggplot(df, aes(x=var1_s1,y=var2_s1))
      p<-p+geom_smooth(method="lm",se=FALSE, color="black", size=.5)
      p<-p+geom_text(aes(label=Display),fontface=df$textfont, color=df$textcolor)
      p<-p+theme_bw()
      title_text<-paste(input$var1,"and",input$var2,sep=" ")
      p<-p+labs(title=title_text,x=input$var1,
                y=input$var2)
      p
    })
    output$scatter2<-renderPlot({
      df$var3<-var3()
      df$var4<-var4()
      df$var2<-df$var3-df$var4
      df$var1<-var1()
      if(input$peer_list=="Current"){
        df<-subset(df, Current == 1)
      }
      if(input$peer_list=="Baseline"){
        df<-subset(df, Baseline ==1)
      }
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