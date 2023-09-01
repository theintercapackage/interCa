library(FactoMineR)
library(factoextra)
library(writexl)
library(shiny)
library(waiter)
library(readr)
library(readxl)
library(ggrepel)
source('global.R')

shinyServer(function(input, output,session) {
  # #choices<-reactive({
  #  # all_objects=ls()
  #   input$data_option
  # data.frames_only <- all_objects[sapply(mget(all_objects), class) == "data.frame"]
  # data.frames_only=as.vector(data.frames_only)
  # data.frames_only
  # })
  
 #  observeEvent(input$category, {
 #    print(cat(input$category))
 #       updateSelectInput(session, "category", choices = choices(),selected = NULL)
 # })
  
  data <- reactive({
    if(input$data_option=="wg93 dataset"){
      data=ca::wg93
    }else if(input$data_option=="global enviroment"){
      name=input$category
      data=eval(parse(text =name))
     data=sapply(data,factor)
      
    }else{
    infile <- input$file1

    if (is.null(infile))
      return(NULL)
    print(str(infile))

    if(infile$type == "text/csv") {
      read_csv(infile$datapath,col_names = input$header)->data
    } else if(infile$type == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
      read_xlsx(infile$datapath,col_names = input$header)->data
    }

    index <- 1:ncol(data)
    data[ , index] <- lapply(data[ , index], as.factor)
    }
    return(data)

  })
  output$contents<-DT::renderDataTable(data(),options=list(pageLength=10,scrollX = TRUE,scrollY = TRUE))



  run_mca<-reactive({
    data()
    source('interca.R')
    interca(data(),10)->res
    return(res)
  })

  output$scree<-renderPlot({
    if(input$show_scree==T){
    run_mca()->result
    result$plot

    }
  })


  scree_plot<-reactive({
    if(input$show_scree==T){
      run_mca()->result
      result$plot

    }
  })
  plot_pdf <- reactive({
    pdf("plot.pdf", width = 8, height = 6)
    print(scree_plot())
    dev.off()
  })

  plot_axis <- reactive({
    pdf("plot.pdf", width = 8, height = 6)
    print(axis())
    dev.off()
  })
  plot_plane <- reactive({
    pdf("plot.pdf", width = 8, height = 6)
    print(plane())
    dev.off()
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      "plot.pdf"
    },
    content = function(file) {
      plot_pdf()
      file.copy("plot.pdf", file)
    }
  )
  output$download_axis <- downloadHandler(
    filename = function() {
      "plot.pdf"
    },
    content = function(file) {
      plot_axis()
      file.copy("plot.pdf", file)
    }
  )
  output$download_plane <- downloadHandler(
    filename = function() {
      "plot.pdf"
    },
    content = function(file) {
      plot_plane()
      file.copy("plot.pdf", file)
    }
  )

  list_results<-eventReactive(input$run,{

    interca(data(),input$num_axes)->res
      return(res)

  })

  table_results_coords<-reactive({
    if(input$show_coords==T){
      list_results()->tmp
      round(tmp[[1]],2)
    }

  })
  table_results_ecoords<-reactive({
    if(input$show_ecoords==T){
      list_results()->tmp
      round(tmp[[2]],2)
    }

  })
  table_results_ctr<-reactive({
    if(input$show_ctr==T){
      list_results()->tmp
      round(tmp[[3]],2)
    }

  })
  table_results_cor<-reactive({
    if(input$show_cor==T){
      list_results()->tmp
      round(tmp[[4]],2)
    }

  })



  output$results_coords<-DT::renderDataTable(table_results_coords(),
                                      options=list(pageLength=5,scrollX = TRUE,scrollY = TRUE))
  output$results_ecoords<-DT::renderDataTable(table_results_ecoords(),
                                             options=list(pageLength=5,scrollX = TRUE,scrollY = TRUE))
  output$results_ctr<-DT::renderDataTable(table_results_ctr(),
                                             options=list(pageLength=5,scrollX = TRUE,scrollY = TRUE))
  output$results_cor<-DT::renderDataTable(table_results_cor(),
                                             options=list(pageLength=5,scrollX = TRUE,scrollY = TRUE))

  output$show_coords_label<-renderText({
    if(input$show_coords==T){
      return(" Coordinates")}

  })
  output$show_ecoords_label<-renderText({
    if(input$show_ecoords==T){
    return("interpretive Coordinates")}
  })
  output$show_ctr_label<-renderText({
    if(input$show_ctr==T){
    return("ctr index")}
  })
  output$show_cor_label<-renderText({
    if(input$show_cor==T){
    return("Coordinates")}
  })

 slider_axis_table<-eventReactive(input$do_axis,{
   if(input$cb_slider_axis==T){

     list_results()->etable
     etable[[2]]->df

     df[,input$which.axis]->df
     avg=mean(abs(df))
     threshold=as.numeric(input$slider_axis)
     which(abs(df)>threshold)->pos
     df[pos]->df
     mylist=list(df,avg)

     return(mylist)
   }


 })

 slider_plane_table<-eventReactive(input$do_plane,{
   if(input$cb_slider_plane==T){
     list_results()->eplane
     eplane[[2]]->df
     df[,input$which.xaxis]->first
     df[,input$which.yaxis]->second
     avg=mean(abs(first)+abs(second))
     threshold=as.numeric(input$slider_plane)
     which(abs(first)+abs(second)>threshold)->pos
     first[pos]->first
     second[pos]->second
     mylist=list(first,second,avg)
     return(mylist)
   }


 })

 axis_table<-eventReactive(input$do_axis,{
   as.data.frame(round(slider_axis_table()[[1]],2))->axis_table
   axis_table=as.data.frame(axis_table)
   colnames(axis_table)=c("Interpretive coordinates")
   return(axis_table)
 })

 output$download_axis_table <- downloadHandler(
   filename = function() {
     "axis.xlsx"
   },
   content = function(file) {
     openxlsx::write.xlsx(axis_table(), file,row.names=T)
   }
 )

 plane_table<-eventReactive(input$do_plane,{
   cbind(as.data.frame(round(slider_plane_table()[[1]],2)),as.data.frame(round(slider_plane_table()[[2]],2)))->plane_table
   plane_table=as.data.frame(plane_table)
   colnames(plane_table)=c("x-axis interpretive coordinates","y-axis interpretive coordinates")
   return(plane_table)
 })

 output$download_plane_table <- downloadHandler(
   filename = function() {
     "plane.xlsx"
   },
   content = function(file) {
     openxlsx::write.xlsx(plane_table(), file,row.names=T)
   }
 )


 output$slider_axis_table<-DT::renderDataTable( axis_table(),
                                                 options=list(pageLength=5,scrollX = TRUE,scrollY = TRUE))

 output$slider_plane_table<-DT::renderDataTable( plane_table(),
                                               options=list(pageLength=5,scrollX = TRUE,scrollY = TRUE))

 axis<-eventReactive(input$do_axis,{
   source('plot1d.R')
   source('plot1dslider.R')
   if(input$cb_slider_axis==T){
     plot1dslider(slider_axis_table()[[1]],slider_axis_table()[[2]])->axis
   }

   return(axis)
 })

 output$axis<-renderPlot({
   axis()
  })

 plane<-eventReactive(input$do_plane,{
   source('plot2d.R')
   source('plot2dslider.R')
   if(input$cb_slider_plane==T){

     plot2dslider(slider_plane_table()[[1]],slider_plane_table()[[2]],slider_plane_table()[[3]])->plane
   }else{
     plot2d(list_results(),input$which.xaxis,input$which.yaxis)->plane
   }
   return(plane)
 })


  output$plane<-renderPlot({
   plane()

  })

 observeEvent(req(input$which.xaxis<=input$num_axes & input$which.yaxis<=input$num_axes),{
    list_results()->df
    df[[2]]->thedata1
    thedata1[,input$which.xaxis]->first1
    thedata1[,input$which.yaxis]->second1
    print(first1)
    print(second1)
    themax1=round(max(abs(first1)+abs(second1)),2)
    themin1=round(min(abs(first1)+abs(second1)),2)
    ave1=round(mean(abs(first1)+abs(second1)),2)
    print(themax1)
    print(themin1)
    print(ave1)
    updateSliderInput(session,"slider_plane",value=ave1,min=themin1,max=themax1,step=0.05)
  })

  observe({
    condition<-as.integer(input$which.axis)<=5
   req(condition)
    list_results()->myd
    myd[[2]]->thedata

    thedata[,input$which.axis]->thedata
    themin=round(min(abs(thedata)),2)
    themax=round(max(abs(thedata)),2)
    ave=round(mean(abs(thedata)),2)
    updateSliderInput(session,"slider_axis",value=ave,min=themin,max=themax,step=0.05)


  })

  observeEvent(input$do_axis, {

    req(input$which.axis>=input$num_axes | input$which.axis<=0)
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "
      please specify a value <= number of selected axes!",
      type = "error"
    )

  })

  observeEvent(input$do_plane, {

    req(input$which.xaxis>input$num_axes | input$which.yaxis>input$num_axes | input$which.xaxis<=0 | input$which.yaxis<=0)
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "
      please specify a value for  x-axis and y-axis in interval [1, number of selected axes]",
      type = "error"
    )

  })


  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      #e=new.env()
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(axis=axis(),axis_table=axis_table())

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  output$report_plane <- downloadHandler(
    
    filename = "report_plane.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report_plane.Rmd")
      file.copy("report_plane.Rmd", tempReport, overwrite = TRUE)
      params <- list(plane=plane(),plane_table=plane_table())
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )



  observeEvent(input$run, {
    withProgressWaitress({
      for (i in 1:100) {
        incProgressWaitress(5)
        Sys.sleep(0.001)
      }
    }, selector = "#run", max = 100, theme = "overlay-percent")
  })





})
