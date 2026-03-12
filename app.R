
#########################################################3
###########################################################
library(shiny)
library(plotly)
library(dplyr)
library(readxl)
library(shinyWidgets)
library(shinyjs)

# ================= LOAD DATA =================
teachers <- read_excel("teachers_master.xlsx")
late_data <- read_excel("late_records.xlsx")
attrition <- read_excel("attrition_data.xlsx")

colnames(teachers) <- make.names(colnames(teachers))
colnames(attrition) <- make.names(colnames(attrition))
colnames(late_data) <- make.names(colnames(late_data))

ui <- uiOutput("app_page")

server <- function(input, output, session){
  
  logged_in <- reactiveVal(FALSE)
  current_tab <- reactiveVal("dashboard")
  
  # ================= LOGIN UI =================
  login_ui <- fluidPage(
    
    tags$head(
      tags$style(HTML("
      body {
        margin: 0;
        padding: 0;
        background-color: #0f1b2b;
        font-family: 'Segoe UI', sans-serif;
      }

      .main-container { display: flex; height: 100vh; }

      .left-panel {
        flex: 1;
        position: relative;
        background: 
          linear-gradient(to right, 
            rgba(10,20,35,0.65) 0%, 
            rgba(10,20,35,0.55) 40%, 
            rgba(10,20,35,0.85) 100%
          ),
          url('https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQG48O1XHX4TeW2o0ftRmBXVB6RXzT0HP75bQ&s') no-repeat center;
        background-size: 85%;
        background-position: center;
      }

      .school-name {
        position: absolute;
        bottom: 20px;
        left: 30px;
        color: white;
        font-size: 22px;
      }

      .right-panel {
        flex: 1;
        background: linear-gradient(135deg, #0f1b2b, #16263d);
        display: flex;
        justify-content: center;
        align-items: center;
      }

      .login-card {
        width: 380px;
        background: #0b1624;
        padding: 45px;
        border-radius: 18px;
        text-align: center;
        color: white;
        box-shadow: 0 0 25px rgba(138, 43, 226, 0.6),
                    0 0 60px rgba(138, 43, 226, 0.4);
      }

      .btn-login {
        width: 100%;
        background: #00bcd4;
        border: none;
        padding: 10px;
        border-radius: 5px;
        color: white;
        font-weight: bold;
      }
      "))
    ),
    
    div(class = "main-container",
        div(class = "left-panel",
            div(class = "school-name", "Ideal International School"),
            
        ),
        div(class = "right-panel",
            div(class = "login-card", tags$a(href='https://google.com',
                                             tags$img(src = "mynalanda.png", width = "120px", style = "margin-bottom:20px;display:block; margin-left:auto; margin-right:auto;")),
                h4("myNalanda Solutions & Services Pvt. Ltd."),
                textInput("username", "Username"),
                passwordInput("password", "Password"),
                selectInput("role", "Login As",
                            choices = c("","Teacher", "Admin", "Student")),
                actionButton("login", "Login", class = "btn-login",color="#000814"),
                
            )
        )
    )
  )
  
  
  # ================= DASHBOARD UI =================
  dashboard_ui <- fluidPage(
    
    tags$head(
      tags$style(HTML("
        body { margin:0; background-color:#1f2a33;
               font-family:'Segoe UI', sans-serif; color:#00e5ff;}
        
        .main-layout{ display:flex; height:100vh; }
        .sidebar{
          width:230px;
          background-color:#18232b;
          padding:20px;
          color:white;
        }
        .content{
          flex:1;
          padding:20px 40px;
          overflow-y:auto;
        }
        .menu{ cursor:pointer; padding:6px 0; }
        .menu:hover{ color:#00e5ff; }
        
        .info-row{
          display:flex;
          justify-content:space-between;
          font-size:14px;
          margin-bottom:30px;
          color:#4fc3f7;
        }
        .kpi-row{
          display:flex;
          justify-content:space-between;
          margin-bottom:40px;
        }
        .kpi-circle{
          width:140px;
          height:140px;
          border-radius:50%;
          border:2px solid #00e5ff;
          display:flex;
          flex-direction:column;
          justify-content:center;
          align-items:center;
          color:#00e5ff;
          box-shadow:0 0 20px rgba(0,229,255,0.3);
        }
        .kpi-value{ font-size:28px; font-weight:bold; }
        .kpi-label{ font-size:12px; text-align:center; color:white; }
        
        .performance-box{
          background:#102027;
          padding:15px;
          border-top:6px solid #00bcd4;
        }
        
        .teacher-box {
          background:#0b113d;
          padding:20px;
          border-radius:10px;
          border:1px solid #7b2cbf;
          box-shadow:0 0 15px #7b2cbf;
          margin-bottom:20px;
          color:white;
        }
        
        .profile-circle {
          width:180px;
          height:180px;
          border-radius:50%;
          margin:auto;
          background:black;
          box-shadow:0 0 40px #00e5ff;
        }
      "))
    ),
    
    div(class="main-layout",
        
        # Sidebar
        div(class="sidebar",
            h4("Ideal International"),
            div(class="menu",
                onclick="Shiny.setInputValue('nav','dashboard',{priority:'event'})",
                "Dashboard"),
            div(class="menu",
                onclick="Shiny.setInputValue('nav','teacher',{priority:'event'})",
                "Teacher"),
            div(class="menu",
                onclick="Shiny.setInputValue('nav','attrition',{priority:'event'})",
                "Late Count & Attrition"),
            br(),
            actionButton("logout","Logout",
                         style="background:#00bcd4;color:white;width:100%;")
        ),
        
        # Content
        div(class="content",
            uiOutput("page_content")
        )
    )
  )
  # ================= LOGIN LOGIC =================
  observeEvent(input$login,{
    if(input$username=="admin" && input$password=="1234"){
      logged_in(TRUE)
    } else {
      showModal(modalDialog("Invalid credentials"))
    }
  })
  
  observeEvent(input$logout,{
    logged_in(FALSE)
    current_tab("dashboard")
  })
  
  observeEvent(input$nav, {
    current_tab(input$nav)
  })
  
  output$app_page <- renderUI({
    if(!logged_in()) login_ui else dashboard_ui
  })
  
  # ================= GAUGE FUNCTION =================
  create_gauge <- function(value, max_value, label, suffix=""){
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = value,
      
      number = list(
        suffix = suffix,
        font = list(size=22, color="#00e5ff")
      ),
      
      title = list(
        text = label,
        font = list(size=14, color="#00e5ff")
      ),
      
      gauge = list(
        axis = list(range = list(0, max_value)),
        bar = list(color="#00e5ff"),
        bgcolor = "#1f2a33",
        borderwidth = 1,
        bordercolor = "#00e5ff"
      )
    ) %>%
      layout(
        paper_bgcolor="#1f2a33",
        plot_bgcolor="#1f2a33",
        font=list(color="#00e5ff")
      )
  }
  
  # ================= PAGE CONTENT =================
  output$page_content <- renderUI({
    
    # ================= DASHBOARD TAB =================
    if(current_tab()=="dashboard"){
      
      total_teachers <- nrow(teachers)
      
      high_risk <- if("Risk_Level" %in% colnames(attrition)){
        nrow(dplyr::filter(attrition, Risk_Level=="High"))
      } else { 0 }
      
      total_late <- if("Late_Count" %in% colnames(late_data)){
        sum(late_data$Late_Count, na.rm = TRUE)
      } else { 0 }
      
      overall_score <- if("Overall_Score" %in% colnames(teachers)){
        round(mean(teachers$Overall_Score, na.rm=TRUE),1)
      } else { 0 }
      
      tagList(
        
        fluidRow(
          column(3, h4(paste("Total Teachers:", total_teachers))),
          column(3, h4(paste("High Risk:", high_risk))),
          column(3, h4(paste("Total Late Count:", total_late))),
          column(3, h4(paste("Avg Overall Score:", overall_score,"%")))
        ),
        
        br(),
        
        fluidRow(
          column(3, plotlyOutput("g1")),
          column(3, plotlyOutput("g2")),
          column(3, plotlyOutput("g3")),
          column(3, plotlyOutput("g4"))
        ),
        
        br(),
        
        fluidRow(
          column(12, plotlyOutput("area_chart", height="400px"))
        )
      )
    }
    
    
    # ================= TEACHER TAB =================
    else if(current_tab()=="teacher"){
      
      teacher_choices <- teachers$Teacher_Name
      
      tagList(
        
        selectInput("teacher_select","Teacher Name",
                    choices = teacher_choices),
        
        br(),
        
        uiOutput("teacher_full_ui")
      )
    }
    
    
    # ================= ATTRITION TAB =================
    else if(current_tab()=="attrition"){
      
      teacher_choices <- if("Teacher_Name" %in% colnames(teachers)){
        teachers$Teacher_Name
      } else { NULL }
      
      tagList(
        selectInput("late_teacher","Select Teacher",
                    choices = teacher_choices),
        br(),
        plotlyOutput("late_trend"),
        br(),
        selectInput("risk_filter","Filter Risk Level",
                    choices=c("All","High","Medium","Low")),
        tableOutput("attr_table")
      )
    }
  })
  output$teacher_full_ui <- renderUI({
    
    req(input$teacher_select)
    
    teacher_data <- teachers %>%
      dplyr::filter(Teacher_Name == input$teacher_select)
    
    req(nrow(teacher_data) > 0)
    
    # ================= LATE CALC =================
    late_current <- late_data %>%
      dplyr::filter(Teacher_ID == teacher_data$Teacher_ID,
                    Month == max(Month)) %>%
      dplyr::summarise(total = sum(Late_Count, na.rm=TRUE)) %>%
      dplyr::pull(total)
    
    if(length(late_current)==0) late_current <- 0
    
    # ================= LEAVE CALC =================
    unplanned_leave <- if("Unplanned_Leaves" %in% colnames(teacher_data)){
      teacher_data$Unplanned_Leaves
    } else { 0 }
    
    leave_balance <- if("Leave_Balance" %in% colnames(teacher_data)){
      teacher_data$Leave_Balance
    } else { 0 }
    
    # ================= SCORES =================
    int_score <- teacher_data$Int_BM_Score
    ext_score <- teacher_data$Ext_BM_Score
    compliance <- teacher_data$Compliance_Score
    cca <- teacher_data$CCA_Score
    overall <- teacher_data$Overall_Score
    
    fluidPage(
      
      fluidRow(
        
        column(4,
               div(style="background:#0b113d;padding:15px;border-radius:10px;border:1px solid #7b2cbf;color:white;",
                   h4("Teacher Overview"),
                   h5(paste("Late (Current Month):", late_current)),
                   h5(paste("Unplanned Leaves:", unplanned_leave)),
                   br(),
                   h5("Leave Balance"),
                   div(style="display:flex;gap:5px;",
                       lapply(1:12, function(i){
                         color <- if(i <= leave_balance) "#00e5ff" else "#1f2a33"
                         div(style=paste0("width:14px;height:18px;background:",color,
                                          ";border:1px solid #00e5ff;"))
                       })
                   )
               )
        ),
        
        column(4,
               div(style="text-align:center;",
                   div(style="width:180px;height:180px;border-radius:50%;
                     margin:auto;background:black;
                     box-shadow:0 0 40px #00e5ff;")
               )
        ),
        
        column(4,
               div(style="background:#0b113d;padding:15px;border-radius:10px;border:1px solid #7b2cbf;color:white;",
                   h4("Basic Information"),
                   tags$ul(
                     tags$li(paste("Date of Birth:", teacher_data$DOB)),
                     tags$li(paste("Qualification:", teacher_data$Qualification)),
                     tags$li(paste("Experience(Current):", teacher_data$Experience_Current)),
                     tags$li(paste("Section:", teacher_data$Section)),
                     tags$li(paste("Classes per Week:", teacher_data$Classes_per_Week)),
                     tags$li(paste("Subjects:", teacher_data$Subjects))
                   )
               )
        )
      ),
      
      br(),
      
      fluidRow(
        column(4,
               div(style="background:#0b113d;padding:15px;border-radius:10px;color:white;",
                   h4("Compliance Score"),
                   h3(paste(round(compliance,1),"/10"))
               )
        ),
        
        column(4,
               div(style="background:#0b113d;padding:15px;border-radius:10px;color:white;",
                   h4("Teaching Delivery (Int. BM)"),
                   h3(round(int_score,1)),
                   progressBar(id=NULL,value=int_score,total=100,status="success",display_pct=TRUE)
               )
        ),
        
        column(4,
               div(style="background:#0b113d;padding:15px;border-radius:10px;color:white;",
                   h4("Teaching Delivery (Ext. BM)"),
                   h3(round(ext_score,1)),
                   progressBar(id=NULL,value=ext_score,total=100,status="warning",display_pct=TRUE)
               )
        )
      ),
      
      br(),
      
      fluidRow(
        column(4,
               div(style="background:#0b113d;padding:15px;border-radius:10px;color:white;",
                   h4("CCA Contribution"),
                   h3(paste(round(cca,1),"/10")),
                   progressBar(id=NULL,value=cca,total=10,status="success",display_pct=TRUE)
               )
        ),
        
        column(8,
               div(style="background:#0b113d;padding:15px;border-radius:10px;color:white;",
                   h4("Teacher Score Overview"),
                   plot_ly(
                     type="indicator",
                     mode="gauge+number",
                     value=overall,
                     gauge=list(axis=list(range=list(0,100))),
                     title=list(text="Overall Score")
                   )
               )
        )
      )
    )
  })
  
  # ================= GAUGE OUTPUTS =================
  output$g1 <- renderPlotly({
    req("Int_BM_Score" %in% colnames(teachers))
    create_gauge(round(mean(teachers$Int_BM_Score, na.rm=TRUE),1),
                 100,"(Int. BM)","%")
  })
  
  output$g2 <- renderPlotly({
    req("Ext_BM_Score" %in% colnames(teachers))
    create_gauge(round(mean(teachers$Ext_BM_Score, na.rm=TRUE),1),
                 100,"(Ext. BM)","%")
  })
  
  output$g3 <- renderPlotly({
    req("Compliance_Score" %in% colnames(teachers))
    create_gauge(round(mean(teachers$Compliance_Score, na.rm=TRUE),1),
                 10,"Compliance Score","/10")
  })
  
  output$g4 <- renderPlotly({
    req("CCA_Score" %in% colnames(teachers))
    create_gauge(round(mean(teachers$CCA_Score, na.rm=TRUE),1),
                 10,"CCA Contribution","/10")
  })
  
  # ================= AREA CHART =================
  output$area_chart <- renderPlotly({
    
    req("Overall_Score" %in% colnames(teachers))
    
    performance_data <- teachers %>%
      arrange(Overall_Score) %>%
      mutate(Index = row_number())
    
    plot_ly(
      performance_data,
      x = ~Index,
      y = ~Overall_Score,
      type = 'scatter',
      mode = 'lines',
      fill = 'tozeroy',
      line = list(color="#00e5ff", width=3)
    ) %>%
      layout(
        title = "Performance Landscape",
        paper_bgcolor="#001019",
        plot_bgcolor="#001019",
        font=list(color="#00e5ff"),
        xaxis=list(title="Teacher Index"),
        yaxis=list(title="Overall Score")
      )
  })
  
  # ================= EXISTING PLOTS =================
  output$int_ext_plot <- renderPlotly({
    
    req("Int_BM_Score" %in% colnames(teachers))
    req("Ext_BM_Score" %in% colnames(teachers))
    req("Teacher_Name" %in% colnames(teachers))
    
    plot_ly(teachers,
            x = ~Teacher_Name,
            y = ~Int_BM_Score,
            type="bar",
            name="Internal") %>%
      add_trace(y = ~Ext_BM_Score,
                name="External") %>%
      layout(barmode="group",
             title="Internal vs External BM Scores")
  })
  
  output$overall_plot <- renderPlotly({
    
    req("Overall_Score" %in% colnames(teachers))
    req("Teacher_Name" %in% colnames(teachers))
    
    plot_ly(teachers,
            x=~Teacher_Name,
            y=~Overall_Score,
            type="bar") %>%
      layout(title="Overall Score by Teacher")
  })
  
  # ================= LATE TREND =================
  output$late_trend <- renderPlotly({
    
    req(input$late_teacher)
    
    teacher_id <- teachers %>%
      filter(Teacher_Name==input$late_teacher) %>%
      pull(Teacher_ID)
    
    data <- late_data %>%
      filter(Teacher_ID==teacher_id)
    
    if(nrow(data)==0) return(NULL)
    
    plot_ly(data,
            x=~Month,
            y=~Late_Count,
            type="scatter",
            mode="lines+markers") %>%
      layout(title="Monthly Late Trend")
  })
  
  # ================= ATTRITION TABLE =================
  output$attr_table <- renderTable({
    
    if(input$risk_filter=="All"){
      attrition
    } else {
      filter(attrition, Risk_Level==input$risk_filter)
    }
  })
}

shinyApp(ui, server)

###############

