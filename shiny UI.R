#Shiny UI
#使用shiny進行資料視覺化
library(shinydashboard)
library(shinythemes)
library(shiny)
library(ggplot2)
library(png)
library(data.table)
library(h2o)
library(randomForest)
library(DT)

ui<-dashboardPage(
  dashboardHeader(title="kaggle房價分析"),
  #第一頁 
  dashboardSidebar( 
    sidebarMenu( 
      menuItem("原始資料", tabName = "id_1", 
               icon = icon("dashboard")),
      menuItem("模型選擇", icon = icon("bar-chart-o"), startExpanded = TRUE,
               menuSubItem("h2o-randomForest模型", tabName = "id_2", 
                           icon = icon("thumbs-up")),
               menuSubItem("h2o-(gbdt/gbm)模型", tabName = "id_3", 
                           icon = icon("thumbs-down")),
               menuSubItem("h2o-deeplearning模型", tabName = "id_4", 
                           icon = icon("github-alt")))
    )
  ),
  dashboardBody(
    #第二頁
    tabItems( 
      tabItem(tabName = "id_1", # 對應選項1
              fluidPage(
                box(width = 7,
                    titlePanel (tags$b("房價資料表")),
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    dataTableOutput("test"),
                    downloadButton("download", "Download .csv")),
                box(width = 5,
                    height = 500,
                    titlePanel (tags$b("原始資料圖")), 
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("data_histogram_density_plot")),
                box(width = 5,
                    titlePanel(tags$b("資料描述")),
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h4(
                      tags$div("此資料來自kaggle數據分析平台，",
                               tags$br(),
                               "作為數據分析練習使用。",
                               tags$br(),
                               "資料集經過清洗(補足缺失值、移除極值)，",
                               tags$br(),
                               "並將資料以四種方式進行建模，",
                               tags$br(),
                               "最後挑選出誤差最小的模型對資料進行預測與視覺化。"
                      ))
                )
              )
      ),
      tabItem(tabName = "id_2",
              fluidPage(
                box(width=7,
                    titlePanel(tags$b("h2o.RF模型預測圖")),
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("image4")),
                box(width=4,
                    height = 500,
                    titlePanel(tags$b("h2o.RF模型變數重要度前十名")),
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("plot_h2o.RF")),
                box(width=7,
                    titlePanel(tags$b("h2o.RF模型變數重要度排序")),
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    DT::dataTableOutput("table_h2o.RF")),
                box(width=4,
                    height = 300,
                    titlePanel(tags$b("h2o.RF模型估計法比較")),
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    tableOutput("table1")),
                box(width = 4,
                    titlePanel(tags$b("h2o.RF模型分析概述")),
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h4(
                      "此資料來自kaggle平台，",
                      tags$br(),
                      "作為數據分析練習使用。",
                      tags$br(),
                      "1.透過分析結果繪出房價的預測圖。",
                      tags$br(),
                      "2.將變數依影響力從1~10名進行排序。",
                      tags$br(),
                      "3.透過測試資料與交叉驗證對模型進行評估。")
                )
              )
      ),
      tabItem(tabName = "id_3",
              fluidPage(
                box(width=7,
                    height = 500,
                    titlePanel(tags$b("h2o.GBM模型預測圖")),
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("image5")),
                box(width=4,
                    height = 500,
                    titlePanel(tags$b("h2o.GBM變數重要度前十名")),
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("plot3")),
                box(width=7,
                    titlePanel(tags$b("h2o.GBM變數重要度排序")),
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    DT::dataTableOutput("table3")),
                box(width=4,
                    titlePanel(tags$b("h2o.GBM模型估計法比較")),
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    tableOutput("table2")),
                box(width = 4,
                    titlePanel(tags$b("h2o.GBM分析概述")),
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h5(
                      "此資料來自kaggle平台，",
                      tags$br(),
                      "作為數據分析練習使用。",
                      tags$br(),
                      "1.透過分析結果繪出房價的預測圖。",
                      tags$br(),
                      "2.將變數依影響力從1~10名進行排序。",
                      tags$br(),
                      "3.透過測試資料與交叉驗證對模型進行評估。")
                )
              )),
      tabItem(tabName = "id_4",
              fluidPage(
                box(width=7,
                    height = 500,
                    titlePanel(tags$b("h2o.dl模型預測圖")),
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("plot_dl1")),
                box(width=4,
                    height = 500,
                    titlePanel(tags$b("h2o.dl變數重要度前十名")),
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("plot_dl2")),
                box(width=7,
                    titlePanel(tags$b("h2o.dl變數重要度排序")),
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    DT::dataTableOutput("table_dl1")),
                box(width=4,
                    titlePanel(tags$b("h2o.dl模型估計法比較")),
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    tableOutput("table_dl2")),
                box(width = 4,
                    titlePanel(tags$b("h2o.dl分析概述")),
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h4(
                      "此資料來自kaggle平台，",
                      tags$br(),
                      "作為數據分析練習使用。",
                      tags$br(),
                      "1.透過分析結果繪出房價的預測圖。",
                      tags$br(),
                      "2.將變數依影響力從1~10名進行排序。",
                      tags$br(),
                      "3.透過測試資料與交叉驗證對模型進行評估。")
                )
              )) 
    )
  )
)
