library(shiny) 
library(lme4)
library(lattice)


shinyServer( function(input,output, session) {
  
  
  Dataset <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  })
  

  output$vars <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    cols <- names(Dataset())
    selectInput("vars", "Select variables:",choices=cols, selected=cols, multiple=TRUE)  
  })
 
  output$table <- renderDataTable({
    if (is.null(input$vars) || length(input$vars)==0) return(NULL)
    return((Dataset()[,c(input$vars),drop=FALSE]))
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- Dataset()
    summary(dataset)
  })
  
  
  # Select a response variable 
  output$varselect <- renderUI({
    cols <- names(Dataset())
    selectInput("variable", "Select a response variable:",choices=cols, selected=cols)  
  })
  
  # Select a fixed effect variable 
  output$varselect2 <- renderUI({
    cols <- names(Dataset())
    selectInput("variable2", "Select a fixed effect variable:",choices=cols, selected=cols)  
  })
  
  # Select a random effect variable 
  output$varselect3 <- renderUI({
    cols <- names(Dataset())
    selectInput("variable3", "Select a random effect variable:",choices=cols, selected=cols)  
  })
  
  # Select a interaction variable 
  output$varselect4 <- renderUI({
    cols <- names(Dataset())
    selectInput("variable4", "Select a interaction variable:",choices=cols, selected=cols)
  })
  
  
  # Make a formula
  output$formula <- renderUI({
    
  dataset <- Dataset()
  datafile <- dataset[,c(input$variable,input$variable2,input$variable3,input$variable4)]
  
  varname <- colnames(datafile) 
 
 
  result1 <- paste(varname[1],"~ ","as.factor(",varname[2], ")", ":",varname[4], "+", "(1|",varname[3],")")
  
  if (!input$intercept) result1<- paste(result1, "-1") 
  
  textInput("formula","lme4 Formula:", result1)
  
  })
  
  
  out<-reactive({
    
    if (is.null(input$vars) || length(input$vars)==0) return(NULL)
    
    mydata<-Dataset()[,c(input$vars),drop=FALSE]
    
    myformula<-as.formula(input$formula)
    tex<-input$text
    m1 <- lmer(myformula, data=mydata, REML=tex)
    
    out<-list()
    out[[1]]<-m1
    
    out
  })
  
  
  output$plot1<-renderPlot({
    var1<-which(colnames(Dataset())==input$variable)
    var2<-which(colnames(Dataset())==input$variable3)
    var3<-which(colnames(Dataset())==input$variable2)
    var4<-which(colnames(Dataset())==input$variable4)
    
    ## Stage 1
    N<-length(unique(Dataset()[,var2]))
    id<-sort(unique(Dataset()[,var2]))
    
    BETA<-matrix(0,N,4)
    for (i in 1:N){
      y.temp<-Dataset()[,var1][Dataset()[,var2]==id[i]]
      t.temp<-Dataset()[,var4][Dataset()[,var2]==id[i]]
      reg.temp<-lm(y.temp~t.temp)
      BETA[i,]<-c(reg.temp$coef, unique(Dataset()[,var3][Dataset()[,var2]==id[i]]), id[i])
    }
    
    ## Stage 2 
    b0<-BETA[,1];b1<-BETA[,2]; x<-BETA[,3]
    two_b0<-lm(b0~1)
    two_b1<-lm(b1~as.factor(x)-1)
    
    ## PLOTTING 
    par(mfrow=c(2,2))
    y.temp<-Dataset()[,var1][Dataset()[,var2]==id[1]]
    t.temp<-Dataset()[,var4][Dataset()[,var2]==id[1]]
    
    plot(y.temp~t.temp,type="l",ylim=c(min(Dataset()[,var1]),max(Dataset()[,var1])), ylab="response",xlab="time",main="Original data")
    
    for (i in 2:N){
      y.temp<-Dataset()[,var1][Dataset()[,var2]==id[i]]
      t.temp<-Dataset()[,var4][Dataset()[,var2]==id[i]]
      lines(y.temp~t.temp)
    }
    
    
    plot(y.temp~t.temp,type="n",ylim=c(min(Dataset()[,var1]),max(Dataset()[,var1])),ylab="response",xlab="time",
         main=paste("treat : ",unique(Dataset()[,var3])[1]),col="grey")
    for(i in 1:dim(BETA)[1]){
      if (x[i]==1){
        if (is.na(BETA[i,2])==FALSE){
          abline(a=BETA[i,1],b=BETA[i,2],col="black")
        }
      }
    } 
    
    abline(a=two_b0$coef, b=two_b1$coef[1],col="red",lwd=3)
    
    plot(y.temp~t.temp,type="n",ylim=c(min(Dataset()[,var1]),max(Dataset()[,var1])),ylab="response",xlab="time",
         main=paste("treat : ",unique(Dataset()[,var3])[2]),col="grey")
    for(i in 1:dim(BETA)[1]){
      if (x[i]==2){
        if (is.na(BETA[i,2])==FALSE){
          abline(a=BETA[i,1],b=BETA[i,2],col="black")
        }
      }
    } 
    abline(a=two_b0$coef, b=two_b1$coef[2],col="red",lwd=3)
    
    plot(y.temp~t.temp,type="n",ylim=c(min(Dataset()[,var1]),max(Dataset()[,var1])),ylab="response",xlab="time",
         main=paste("treat : ",unique(Dataset()[,var3])[3]),col="grey")
    for(i in 1:dim(BETA)[1]){
      if (x[i]==3){
        if (is.na(BETA[i,2])==FALSE){
          abline(a=BETA[i,1],b=BETA[i,2],col="black")
        }
      }
    } 
    abline(a=two_b0$coef, b=two_b1$coef[3],col="red",lwd=3)
  })
  
  output$plot1.description<-renderUI({
    var1<-which(colnames(Dataset())==input$variable)
    var2<-which(colnames(Dataset())==input$variable3)
    var3<-which(colnames(Dataset())==input$variable2)
    var4<-which(colnames(Dataset())==input$variable4)
    
    ## Stage 1
    N<-length(unique(Dataset()[,var2]))
    id<-sort(unique(Dataset()[,var2]))
    
    BETA<-matrix(0,N,4)
    for (i in 1:N){
      y.temp<-Dataset()[,var1][Dataset()[,var2]==id[i]]
      t.temp<-Dataset()[,var4][Dataset()[,var2]==id[i]]
      reg.temp<-lm(y.temp~t.temp)
      BETA[i,]<-c(reg.temp$coef, unique(Dataset()[,var3][Dataset()[,var2]==id[i]]), id[i])
    }
    
    ## Stage 2 
    b0<-BETA[,1];b1<-BETA[,2]; x<-BETA[,3]
    two_b0<-lm(b0~1)
    two_b1<-lm(b1~as.factor(x)-1)
    mycoef<-round(c(as.numeric(two_b0$coef),as.numeric(two_b1$coef)),4)
    
    str1<-paste("[ treat : ",unique(Dataset()[,var3])[1],"] ",mycoef[1]," + ",mycoef[2]," × time")
    str2<-paste("[ treat : ",unique(Dataset()[,var3])[2],"] ",mycoef[1]," + ",mycoef[3]," × time")
    str3<-paste("[ treat : ",unique(Dataset()[,var3])[3],"] ",mycoef[1]," + ",mycoef[4]," × time") 
    HTML(paste(str1,str2,str3, sep="<br/>"))
    
  })
  

  out<-reactive({
  
    if (is.null(input$vars) || length(input$vars)==0) return(NULL)
    
    mydata<-Dataset()[,c(input$vars),drop=FALSE]
    
    myformula<-as.formula(input$formula)
    tex<-input$text
    m1 <- lmer(myformula, data=mydata, REML=tex)
    
    out<-list()
    out[[1]]<-m1
    
    out
  })
  
  output$lmm <- renderPrint({
    
    if (is.null(input$vars) || length(input$vars)==0) return(NULL)
    my.out<-out()[[1]]
    print(summary(my.out))
    
  })   
  
  
  # Make a formula2
  output$formula2 <- renderUI({
    
    dataset <- Dataset()
    datafile <- dataset[,c(input$variable,input$variable2,input$variable3,input$variable4),drop=FALSE]
    
    varname <- colnames(datafile) 
    
    
    result2 <- paste(varname[1],"~ ", "as.factor(",varname[2], ")",  ":",varname[4], "+", "(1 + ", varname[4],"|",varname[3],")")
    
    if (!input$intercept) result2<- paste(result2, "-1") 
    
    textInput("formula2"," LRT comparison Formula:", result2)
    
  })
  
  
  ##### LRT Tests
  output$null<-renderPrint({
    
    fixed.form <- paste(input$formula)
    print(fixed.form)
  })
  
  output$alter<-renderPrint({
    
    fixed.form2 <- paste(input$formula2)
    print(fixed.form2)
  })
  
  output$LRT <- renderPrint({
  
    dataset <- Dataset()
    
    fixed.form <- paste(input$formula)
    fixed.form2 <- paste(input$formula2)
    
    result1 <- lmer(fixed.form,data=dataset)
    result2 <- lmer(fixed.form2,data=dataset)
    
    anova1<-anova(result1,result2)
    print(anova1)
    
  })
  
  # Fixed Effect Test 
  
  output$Satterth <- renderPrint({
    library(lmerTest)
    mydata<-Dataset()[,c(input$vars),drop=FALSE]
    myformula<-as.formula(input$formula)
    tex<-input$text
    m1.sa<-lmer(myformula, data=mydata, REML=tex)
    summary(m1.sa)$coef[,c(3,5)]
  })
  
  
  
  output$Kenward <- renderPrint({ 
    mydata<-Dataset()[,c(input$vars),drop=FALSE]
    myformula<-as.formula(input$formula)
    tex<-input$text
    library(pbkrtest)
    m2<-lmer(myformula, data=mydata, REML=tex)
    #summary(m2)
    summ<-summary(m2)
    df.KR<-get_ddf_Lb(m2, fixef(m2))
    pvalue.KR<-2*(1-pt(abs(summ$coef[,3]), df.KR))
    kr.matrix<-matrix(c(rep(df.KR,4), as.numeric(pvalue.KR)),ncol=2)
    rownames(kr.matrix)<-names(summ$coef[,1])
    colnames(kr.matrix)<-c("df","Pr(>|t|)")
    kr.matrix
  })
  
   output$coef <- renderPrint({
    
    if (is.null(input$vars) || length(input$vars)==0) return(NULL)
    my.out<-out()[[1]]
    print(ranef(my.out,condVar=TRUE))
    
  })   
   
    output$lmmPlot <- renderPlot({
      if (is.null(input$vars) || length(input$vars)==0) return(NULL)
      my.out<-out()[[1]]
      eff<-ranef(my.out,condVar=TRUE)
      qqmath(eff)    
    
  })
  
  
    gee1<-reactive({
      dataset <- Dataset()
      datafile <- dataset[,c(input$variable,input$variable2,input$variable3,input$variable4)]
      
      varname <- colnames(datafile) 
      
      
      result3 <- paste(varname[1],"~ ", "as.factor(",varname[2], ")",  "+",varname[4])
      
      myformula3<-as.formula(result3)
      id<-datafile[,3]
      fit.ex1<-gee(myformula3,data=dataset,id=id,corstr = "independence")
      fit.ex1_1<-summary(fit.ex1)
      print(fit.ex1_1)
      
    })
    
    
    output$gee.out<-renderPrint({
      gee1()
    })

 
  
})

