df<-read.csv("poverty data.csv",header=TRUE)
library(shiny)
library(ggplot2)
library(classInt)
library(ggthemes)
library(grid)
library(stringr)

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
  if(all(d.graph$var<=2 & d.graph$var>=-2)){
    d.graph$round<-format(round(d.graph$var, 2), nsmall = 2)
  }else{
    d.graph$round<-format(round(d.graph$var, 0), nsmall = 0)
  }
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
  p<-p+theme(plot.title=element_text(size=16, face="bold"))
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
             "Low Income - All Races" = df$Low.income,
             "Poor Locale - All Races"= df$Poor.locale,
             "Limited Education - All Races"= df$Limited.education,
             "No Health Insurance - All Races" = df$No.health.insurance,
             "Unemployment - All Races" = df$Unemployment,
             "At Least Doubly Disadvantaged - All Races" = df$At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - All Races" = df$Low.income.and.poor.locale,
             "Low Income and Limited Education - All Races" = df$Low.income.and.limited.education,
             "Low Income and No Health Insurance - All Races" = df$Low.income.and.no.health.insurance,
             "Low Income and Unemployment - All Races" = df$Low.income.and.unemployment,
             "Low Income - Black" = df$B.Low.income,
             "Poor Locale - Black" = df$B.Poor.locale,
             "Limited Education - Black" = df$B.Limited.education,
             "No Health Insurance - Black" = df$B.No.health.insurance,
             "Unemployment - Black" = df$B.Unemployment,
             "At Least Doubly Disadvantaged - Black" = df$B.At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - Black" = df$B.Low.income.and.poor.locale,
             "Low Income and Limited Education - Black" = df$B.Low.income.and.limited.education,
             "Low Income and No Health Insurance - Black" = df$B.Low.income.and.no.health.insurance,
             "Low Income and Unemployment - Black" = df$B.Low.income.and.unemployment,
             "Low Income - Hispanic" = df$H.Low.income,
             "Poor Locale - Hispanic"= df$H.Poor.locale,
             "Limited Education - Hispanic" = df$H.Limited.education,
             "No Health Insurance - Hispanic" = df$H.No.health.insurance,
             "Unemployment - Hispanic"  = df$H.Unemployment,
             "At Least Doubly Disadvantaged - Hispanic" = df$H.At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - Hispanic" = df$H.Low.income.and.poor.locale,
             "Low Income and Limited Education - Hispanic" = df$H.Low.income.and.limited.education,
             "Low Income and No Health Insurance - Hispanic" = df$H.Low.income.and.no.health.insurance,
             "Low Income and Unemployment - Hispanic" = df$H.Low.income.and.unemployment,
             "Low Income - White" = df$W.Low.income,
             "Poor Locale - White" = df$W.Poor.locale,
             "Limited Education - White" = df$W.Limited.education,
             "No Health Insurance - White" = df$W.No.health.insurance,
             "Unemployment - White" = df$W.Unemployment,
             "At Least Doubly Disadvantaged - White" = df$W.At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - White" = df$W.Low.income.and.poor.locale,
             "Low Income and Limited Education - White" = df$W.Low.income.and.limited.education,
             "Low Income and No Health Insurance - White" = df$W.Low.income.and.no.health.insurance,
             "Low Income and Unemployment - White" = df$W.Low.income.and.unemployment,
             "Years of Potential Life Lost Rate" = df$Years.of.Potential.Life.Lost.Rate,
             "Percent in Fair or Poor Health" = df$Fair_Poor_Health_Percent,
             "Physically unhealthy days" = df$Physically.Unhealthy.Days,
             "Mentally unhealthy days" = df$Mentally.Unhealthy.Days,
             "Female Life Expectancy - Low Income" = df$le_agg_q1_F,
             "Male Life Expectancy - Low Income" = df$le_agg_q1_M,
             "Smoking Percent- Low Income"=df$cur_smoke_q1,
             "Obesity Rate - Low Income"=df$bmi_obese_q1,
             "Exercise in last 30 days - Low Income"=df$exercise_any_q1,
             "Hospital Mortality Rate Index" = df$mort_30day_hosp_z,
             "Social Capital Index" = df$scap_ski90pcm,
             "Income Segregation" = df$cs00_seg_inc,
             "Poverty Segregation" = df$cs00_seg_inc_pov25,
             "Segregation of Affluence" = df$cs00_seg_inc_aff75,
             "Racial Segregation" = df$cs_race_theil_2000,
             "Inequality - Gini Index" = df$gini99,
             "Fraction Middle Class" = df$frac_middleclass,
             "Labor Force Participation" = df$cs_labforce,
             "Median House Value" = df$median_house_value,
             "Economic Mobility" = df$e_rank_b,
             "Percent Black" = df$percent_black,
             "Percent Hispanic" = df$percent_hispanic,
             "Percent White" = df$percent_white)
             
    })
    var2 <- reactive({
      switch(input$var2, 
             "Low Income - All Races" = df$Low.income,
             "Poor Locale - All Races"= df$Poor.locale,
             "Limited Education - All Races"= df$Limited.education,
             "No Health Insurance - All Races" = df$No.health.insurance,
             "Unemployment - All Races" = df$Unemployment,
             "At Least Doubly Disadvantaged - All Races" = df$At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - All Races" = df$Low.income.and.poor.locale,
             "Low Income and Limited Education - All Races" = df$Low.income.and.limited.education,
             "Low Income and No Health Insurance - All Races" = df$Low.income.and.no.health.insurance,
             "Low Income and Unemployment - All Races" = df$Low.income.and.unemployment,
             "Low Income - Black" = df$B.Low.income,
             "Poor Locale - Black" = df$B.Poor.locale,
             "Limited Education - Black" = df$B.Limited.education,
             "No Health Insurance - Black" = df$B.No.health.insurance,
             "Unemployment - Black" = df$B.Unemployment,
             "At Least Doubly Disadvantaged - Black" = df$B.At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - Black" = df$B.Low.income.and.poor.locale,
             "Low Income and Limited Education - Black" = df$B.Low.income.and.limited.education,
             "Low Income and No Health Insurance - Black" = df$B.Low.income.and.no.health.insurance,
             "Low Income and Unemployment - Black" = df$B.Low.income.and.unemployment,
             "Low Income - Hispanic" = df$H.Low.income,
             "Poor Locale - Hispanic"= df$H.Poor.locale,
             "Limited Education - Hispanic" = df$H.Limited.education,
             "No Health Insurance - Hispanic" = df$H.No.health.insurance,
             "Unemployment - Hispanic"  = df$H.Unemployment,
             "At Least Doubly Disadvantaged - Hispanic" = df$H.At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - Hispanic" = df$H.Low.income.and.poor.locale,
             "Low Income and Limited Education - Hispanic" = df$H.Low.income.and.limited.education,
             "Low Income and No Health Insurance - Hispanic" = df$H.Low.income.and.no.health.insurance,
             "Low Income and Unemployment - Hispanic" = df$H.Low.income.and.unemployment,
             "Low Income - White" = df$W.Low.income,
             "Poor Locale - White" = df$W.Poor.locale,
             "Limited Education - White" = df$W.Limited.education,
             "No Health Insurance - White" = df$W.No.health.insurance,
             "Unemployment - White" = df$W.Unemployment,
             "At Least Doubly Disadvantaged - White" = df$W.At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - White" = df$W.Low.income.and.poor.locale,
             "Low Income and Limited Education - White" = df$W.Low.income.and.limited.education,
             "Low Income and No Health Insurance - White" = df$W.Low.income.and.no.health.insurance,
             "Low Income and Unemployment - White" = df$W.Low.income.and.unemployment,
             "Years of Potential Life Lost Rate" = df$Years.of.Potential.Life.Lost.Rate,
             "Percent in Fair or Poor Health" = df$Fair_Poor_Health_Percent,
             "Physically unhealthy days" = df$Physically.Unhealthy.Days,
             "Mentally unhealthy days" = df$Mentally.Unhealthy.Days,
             "Female Life Expectancy - Low Income" = df$le_agg_q1_F,
             "Male Life Expectancy - Low Income" = df$le_agg_q1_M,
             "Smoking Percent- Low Income"=df$cur_smoke_q1,
             "Obesity Rate - Low Income"=df$bmi_obese_q1,
             "Exercise in last 30 days - Low Income"=df$exercise_any_q1,
             "Hospital Mortality Rate Index" = df$mort_30day_hosp_z,
             "Social Capital Index" = df$scap_ski90pcm,
             "Income Segregation" = df$cs00_seg_inc,
             "Poverty Segregation" = df$cs_00_seg_inc_pov25,
             "Segregation of Affluence" = df$cs_00_seg_inc_aff75,
             "Racial Segregation" = df$cs_race_theil_2000,
             "Inequality - Gini Index" = df$gini99,
             "Fraction Middle Class" = df$frac_middleclass,
             "Labor Force Participation" = df$cs_labforce,
             "Median House Value" = df$median_house_value,
             "Economic Mobility" = df$e_rank_b,
             "Percent Black" = df$percent_black,
             "Percent Hispanic" = df$percent_hispanic,
             "Percent White" = df$percent_white)
    })
    var3 <- reactive({
      switch(input$var3, 
             "Low Income - All Races" = df$Low.income,
             "Poor Locale - All Races"= df$Poor.locale,
             "Limited Education - All Races"= df$Limited.education,
             "No Health Insurance - All Races" = df$No.health.insurance,
             "Unemployment - All Races" = df$Unemployment,
             "At Least Doubly Disadvantaged - All Races" = df$At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - All Races" = df$Low.income.and.poor.locale,
             "Low Income and Limited Education - All Races" = df$Low.income.and.limited.education,
             "Low Income and No Health Insurance - All Races" = df$Low.income.and.no.health.insurance,
             "Low Income and Unemployment - All Races" = df$Low.income.and.unemployment,
             "Low Income - Black" = df$B.Low.income,
             "Poor Locale - Black" = df$B.Poor.locale,
             "Limited Education - Black" = df$B.Limited.education,
             "No Health Insurance - Black" = df$B.No.health.insurance,
             "Unemployment - Black" = df$B.Unemployment,
             "At Least Doubly Disadvantaged - Black" = df$B.At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - Black" = df$B.Low.income.and.poor.locale,
             "Low Income and Limited Education - Black" = df$B.Low.income.and.limited.education,
             "Low Income and No Health Insurance - Black" = df$B.Low.income.and.no.health.insurance,
             "Low Income and Unemployment - Black" = df$B.Low.income.and.unemployment,
             "Low Income - Hispanic" = df$H.Low.income,
             "Poor Locale - Hispanic"= df$H.Poor.locale,
             "Limited Education - Hispanic" = df$H.Limited.education,
             "No Health Insurance - Hispanic" = df$H.No.health.insurance,
             "Unemployment - Hispanic"  = df$H.Unemployment,
             "At Least Doubly Disadvantaged - Hispanic" = df$H.At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - Hispanic" = df$H.Low.income.and.poor.locale,
             "Low Income and Limited Education - Hispanic" = df$H.Low.income.and.limited.education,
             "Low Income and No Health Insurance - Hispanic" = df$H.Low.income.and.no.health.insurance,
             "Low Income and Unemployment - Hispanic" = df$H.Low.income.and.unemployment,
             "Low Income - White" = df$W.Low.income,
             "Poor Locale - White" = df$W.Poor.locale,
             "Limited Education - White" = df$W.Limited.education,
             "No Health Insurance - White" = df$W.No.health.insurance,
             "Unemployment - White" = df$W.Unemployment,
             "At Least Doubly Disadvantaged - White" = df$W.At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - White" = df$W.Low.income.and.poor.locale,
             "Low Income and Limited Education - White" = df$W.Low.income.and.limited.education,
             "Low Income and No Health Insurance - White" = df$W.Low.income.and.no.health.insurance,
             "Low Income and Unemployment - White" = df$W.Low.income.and.unemployment,
             "Years of Potential Life Lost Rate" = df$Years.of.Potential.Life.Lost.Rate,
             "Percent in Fair or Poor Health" = df$Fair_Poor_Health_Percent,
             "Physically unhealthy days" = df$Physically.Unhealthy.Days,
             "Mentally unhealthy days" = df$Mentally.Unhealthy.Days,
             "Female Life Expectancy - Low Income" = df$le_agg_q1_F,
             "Male Life Expectancy - Low Income" = df$le_agg_q1_M,
             "Smoking Percent- Low Income"=df$cur_smoke_q1,
             "Obesity Rate - Low Income"=df$bmi_obese_q1,
             "Exercise in last 30 days - Low Income"=df$exercise_any_q1,
             "Hospital Mortality Rate Index" = df$mort_30day_hosp_z,
             "Social Capital Index" = df$scap_ski90pcm,
             "Income Segregation" = df$cs00_seg_inc,
             "Poverty Segregation" = df$cs_00_seg_inc_pov25,
             "Segregation of Affluence" = df$cs_00_seg_inc_aff75,
             "Racial Segregation" = df$cs_race_theil_2000,
             "Inequality - Gini Index" = df$gini99,
             "Fraction Middle Class" = df$frac_middleclass,
             "Labor Force Participation" = df$cs_labforce,
             "Median House Value" = df$median_house_value,
             "Economic Mobility" = df$e_rank_b,
             "Percent Black" = df$percent_black,
             "Percent Hispanic" = df$percent_hispanic,
             "Percent White" = df$percent_white)
    })
    var4 <- reactive({
      switch(input$var4, 
             "Low Income - All Races" = df$Low.income,
             "Poor Locale - All Races"= df$Poor.locale,
             "Limited Education - All Races"= df$Limited.education,
             "No Health Insurance - All Races" = df$No.health.insurance,
             "Unemployment - All Races" = df$Unemployment,
             "At Least Doubly Disadvantaged - All Races" = df$At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - All Races" = df$Low.income.and.poor.locale,
             "Low Income and Limited Education - All Races" = df$Low.income.and.limited.education,
             "Low Income and No Health Insurance - All Races" = df$Low.income.and.no.health.insurance,
             "Low Income and Unemployment - All Races" = df$Low.income.and.unemployment,
             "Low Income - Black" = df$B.Low.income,
             "Poor Locale - Black" = df$B.Poor.locale,
             "Limited Education - Black" = df$B.Limited.education,
             "No Health Insurance - Black" = df$B.No.health.insurance,
             "Unemployment - Black" = df$B.Unemployment,
             "At Least Doubly Disadvantaged - Black" = df$B.At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - Black" = df$B.Low.income.and.poor.locale,
             "Low Income and Limited Education - Black" = df$B.Low.income.and.limited.education,
             "Low Income and No Health Insurance - Black" = df$B.Low.income.and.no.health.insurance,
             "Low Income and Unemployment - Black" = df$B.Low.income.and.unemployment,
             "Low Income - Hispanic" = df$H.Low.income,
             "Poor Locale - Hispanic"= df$H.Poor.locale,
             "Limited Education - Hispanic" = df$H.Limited.education,
             "No Health Insurance - Hispanic" = df$H.No.health.insurance,
             "Unemployment - Hispanic"  = df$H.Unemployment,
             "At Least Doubly Disadvantaged - Hispanic" = df$H.At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - Hispanic" = df$H.Low.income.and.poor.locale,
             "Low Income and Limited Education - Hispanic" = df$H.Low.income.and.limited.education,
             "Low Income and No Health Insurance - Hispanic" = df$H.Low.income.and.no.health.insurance,
             "Low Income and Unemployment - Hispanic" = df$H.Low.income.and.unemployment,
             "Low Income - White" = df$W.Low.income,
             "Poor Locale - White" = df$W.Poor.locale,
             "Limited Education - White" = df$W.Limited.education,
             "No Health Insurance - White" = df$W.No.health.insurance,
             "Unemployment - White" = df$W.Unemployment,
             "At Least Doubly Disadvantaged - White" = df$W.At.least.doubly.disadvantaged,
             "Low Income and Poor Locale - White" = df$W.Low.income.and.poor.locale,
             "Low Income and Limited Education - White" = df$W.Low.income.and.limited.education,
             "Low Income and No Health Insurance - White" = df$W.Low.income.and.no.health.insurance,
             "Low Income and Unemployment - White" = df$W.Low.income.and.unemployment,
             "Years of Potential Life Lost Rate" = df$Years.of.Potential.Life.Lost.Rate,
             "Percent in Fair or Poor Health" = df$Fair_Poor_Health_Percent,
             "Physically unhealthy days" = df$Physically.Unhealthy.Days,
             "Mentally unhealthy days" = df$Mentally.Unhealthy.Days,
             "Female Life Expectancy - Low Income" = df$le_agg_q1_F,
             "Male Life Expectancy - Low Income" = df$le_agg_q1_M,
             "Smoking Percent- Low Income"=df$cur_smoke_q1,
             "Obesity Rate - Low Income"=df$bmi_obese_q1,
             "Exercise in last 30 days - Low Income"=df$exercise_any_q1,
             "Hospital Mortality Rate Index" = df$mort_30day_hosp_z,
             "Social Capital Index" = df$scap_ski90pcm,
             "Income Segregation" = df$cs00_seg_inc,
             "Poverty Segregation" = df$cs_00_seg_inc_pov25,
             "Segregation of Affluence" = df$cs_00_seg_inc_aff75,
             "Racial Segregation" = df$cs_race_theil_2000,
             "Inequality - Gini Index" = df$gini99,
             "Fraction Middle Class" = df$frac_middleclass,
             "Labor Force Participation" = df$cs_labforce,
             "Median House Value" = df$median_house_value,
             "Economic Mobility" = df$e_rank_b,
             "Percent Black" = df$percent_black,
             "Percent Hispanic" = df$percent_hispanic,
             "Percent White" = df$percent_white)
    })

    order_one <- reactive({
      if(input$var1=="Low Income Female Life Expectancy"|
         input$var1=="Low Income Male Life Expectancy"|
         input$var1=="Low Income Exercise in last 30 days"|
         input$var1=="Social Capital Index"|
         input$var1=="Fraction Middle Class"|
         input$var1=="Absolute Mobility"|
         input$var1=="Median House Value"|
         input$var1=="Labor Force Participation"|
         input$var1=="Percent Black"|
         input$var1=="Percent Hispanic"|
         input$var1=="Percent White"){
        order<-"descending"
      }else{
        order<-"ascending"
        }
      order
    })
    
    order_two <- reactive({
      if(input$var2=="Low Income Female Life Expectancy"|
         input$var2=="Low Income Male Life Expectancy"|
         input$var2=="Low Income Exercise in last 30 days"|
         input$var2=="Social Capital Index"|
         input$var2=="Fraction Middle Class"|
         input$var2=="Absolute Mobility"|
         input$var2=="Median House Value"|
         input$var2=="Labor Force Participation"|
         input$var2=="Percent Black"|
         input$var2=="Percent Hispanic"|
         input$var2=="Percent White"){
        order<-"descending"
      }else{
        order<-"ascending"
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
      p<-p+scale_fill_manual(values=c("green3","red2","yellow2"))
      p<-p+theme(axis.text.y=element_text(hjust=0,face=rev(d.graph$textfont),
                                          size=12))
      p<-p+theme(axis.ticks=element_blank(),axis.text.x=element_blank())
      p<-p+theme(plot.title=element_text(size=16, face="bold"))
      p<-p+geom_text(aes(label=round),hjust=1.1,size=5,fontface="bold")
      title_text<-paste(input$var3,"minus",input$var4,sep=" ")
      title_text_wrap<-str_wrap(title_text, width = 70)
      p<-p+labs(title=title_text_wrap, y="", x="")
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
      title_text_wrap<-str_wrap(title_text, width = 70)
      p<-p+labs(title=title_text_wrap,x=input$var1,
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
      title_text_wrap<-str_wrap(title_text, width = 70)
      y_text<-paste(input$var4,"minus",input$var3,sep=" ")
      y_text_wrap<-str_wrap(y_text, width=50)
      p<-p+labs(title=title_text_wrap,x=input$var1,
                y=y_text_wrap)
      p
    })

})    