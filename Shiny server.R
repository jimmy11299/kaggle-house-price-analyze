#Shiny server
server<-function(input,output){
  #第一頁
  output$test <- renderDataTable(test_1,options = list(scrollX = TRUE))
  test_1a<-reactive(test_1)
  output$download <- downloadHandler(
    filename = function() {"房價資料表.csv"},
    content = function(file) {
      write.csv(test_1a(), file)}
  )
  output$data_histogram_density_plot <-renderPlotly(
    ggplotly(ggplot(as.data.frame(test_3),aes(x = V2))+  
      geom_histogram(bins = 30, alpha = 0.5) +
      labs(x="房價", y="數量",,caption="資料來源 : kaggle平台")+
      theme(text=element_text(size=20,  family="wqy-microhei"))
    ))
  #第二頁
  output$table1<-renderTable(as.data.frame(c6))
  output$image4<-renderPlotly(
    ggplotly(ggplot(df_pd2,aes(x = predict))+  
      geom_histogram(bins = 30, alpha = 0.5) +
      labs(x="房價", y="數量",caption="資料來源 : kaggle平台")+
      theme(text=element_text(size=20,  family="wqy-microhei"))
  ))
  output$plot_h2o.RF<-renderPlot(h2o.varimp_plot(rf_f2, num_of_features = 10))
  output$table_h2o.RF<-renderDataTable(as.data.frame(h2o.varimp(rf_f2)))
  #第三頁
  output$image5<-renderPlotly(
    ggplotly(ggplot(df_gbm_predict,aes(x = predict))+  
      geom_histogram(bins = 30, alpha = 0.5) +
      labs(x="房價", y="數量",caption="資料來源 : kaggle平台")+
      theme(text=element_text(size=20,  family="wqy-microhei"))
  ))
  output$table2<-renderTable(b6)
  output$plot3<-renderPlot(h2o.varimp_plot(h2o.final, num_of_features = 10))
  output$table3<-renderDataTable(as.data.frame(h2o.varimp(h2o.final)))
  #第四頁
  output$plot_dl1<-renderPlotly(
    ggplotly(ggplot(df_pdl,aes(x = predict))+  
      geom_histogram(bins = 30, alpha = 0.5) +
      labs(x="房價", y="數量",caption="資料來源 : kaggle平台")+
      theme(text=element_text(size=20,  family="wqy-microhei"))
  ))
  output$table_dl2<-renderTable(d6)
  output$plot_dl2<-renderPlot(h2o.varimp_plot(dl, num_of_features = 10))
  output$table_dl1<-renderDataTable(as.data.frame(h2o.varimp(dl)))
}

shinyApp(ui = ui, server = server)
