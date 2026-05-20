# ============================================================
# SPOTIFY ANALYTICS DASHBOARD — Looker-style Charts
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(readxl)
library(scales)

data_music <- read_excel(file.choose())

# ---- HELPER: Format angka ----
fmt_num <- function(x) {
  dplyr::case_when(
    x >= 1e9 ~ paste0(round(x / 1e9, 1), "B"),
    x >= 1e6 ~ paste0(round(x / 1e6, 1), "M"),
    x >= 1e3 ~ paste0(round(x / 1e3, 1), "K"),
    TRUE     ~ as.character(round(x))
  )
}

# ---- WARNA PALETTE (Looker style) ----
PAL <- c("#4285F4","#34A853","#FBBC04","#EA4335",
         "#9C27B0","#00BCD4","#FF5722","#607D8B")

# ---- CSS ----
custom_css <- "
/* === Global === */
body, .content-wrapper { background: #F8F9FA !important; font-family: 'Google Sans', 'Roboto', sans-serif !important; }

/* === Header === */
.skin-blue .main-header .navbar,
.skin-blue .main-header .logo { background: #1E1E2E !important; border-bottom: none !important; }
.skin-blue .main-header .logo { font-size: 14px !important; font-weight: 600 !important; letter-spacing: 0.3px; color: #fff !important; }
.skin-blue .main-header .navbar .sidebar-toggle { color: #aaa !important; }

/* === Sidebar === */
.main-sidebar, .left-side { background: #1E1E2E !important; }
.skin-blue .main-sidebar .sidebar .sidebar-menu a { color: #94A3B8 !important; font-size: 12.5px !important; border-radius: 6px; margin: 2px 8px; }
.skin-blue .main-sidebar .sidebar .sidebar-menu .active a,
.skin-blue .main-sidebar .sidebar .sidebar-menu a:hover { background: #2D2D42 !important; color: #fff !important; border-left: 3px solid #4285F4 !important; }
.skin-blue .sidebar-menu > li.active > a { border-left: 3px solid #4285F4 !important; }

/* === Sidebar inputs === */
.sidebar-section { padding: 0 14px; }
.sidebar-section .control-label {
  color: #64748B !important;
  font-size: 10.5px !important;
  text-transform: uppercase;
  letter-spacing: 0.8px;
  margin-bottom: 4px;
  margin-top: 14px;
  display: block;
}
.selectize-input {
  border: 1px solid #2D2D42 !important;
  border-radius: 6px !important;
  background: #2D2D42 !important;
  color: #E2E8F0 !important;
  font-size: 12px !important;
  box-shadow: none !important;
  min-height: 34px !important;
}
.selectize-input.focus { border-color: #4285F4 !important; box-shadow: 0 0 0 2px rgba(66,133,244,0.15) !important; }
.selectize-dropdown { background: #2D2D42 !important; border: 1px solid #3D3D52 !important; border-radius: 6px !important; }
.selectize-dropdown-content .option { color: #CBD5E1 !important; font-size: 12px !important; }
.selectize-dropdown-content .option:hover,
.selectize-dropdown-content .option.active { background: #4285F4 !important; color: #fff !important; }

/* === Slider === */
.irs--shiny .irs-bar { background: #4285F4 !important; border-color: #4285F4 !important; }
.irs--shiny .irs-handle { background: #fff !important; border: 2px solid #4285F4 !important; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
  background: #4285F4 !important; color: #fff !important; font-size: 10px !important; border-radius: 4px !important;
}
.irs--shiny .irs-grid-text { color: #64748B !important; font-size: 10px !important; }
.irs--shiny .irs-line { background: #2D2D42 !important; }

/* === Value boxes === */
.small-box {
  border-radius: 12px !important;
  border: none !important;
  box-shadow: 0 1px 4px rgba(0,0,0,0.1) !important;
  overflow: hidden;
}
.small-box h3 { font-size: 28px !important; font-weight: 700 !important; }
.small-box p  { font-size: 12px !important; opacity: 0.85; font-weight: 500; }
.small-box .icon { opacity: 0.2; font-size: 60px !important; top: 10px !important; }
.bg-blue   { background: linear-gradient(135deg, #4285F4, #3367D6) !important; }
.bg-green  { background: linear-gradient(135deg, #34A853, #2E8B3E) !important; }
.bg-yellow { background: linear-gradient(135deg, #FBBC04, #E0A800) !important; color: #fff !important; }

/* === Chart cards (Looker style) === */
.box {
  border-radius: 12px !important;
  border: 1px solid #E2E8F0 !important;
  box-shadow: 0 1px 3px rgba(0,0,0,0.06) !important;
  background: #fff !important;
}
.box-header {
  background: #fff !important;
  border-bottom: 1px solid #F1F5F9 !important;
  padding: 14px 20px 12px !important;
  border-radius: 12px 12px 0 0 !important;
}
.box-header .box-title {
  font-size: 13px !important;
  font-weight: 600 !important;
  color: #1E293B !important;
  letter-spacing: 0.1px;
}
.box-body { padding: 16px 20px !important; }

/* === Insight box === */
.insight-card {
  background: #F0F7FF;
  border: 1px solid #BFDBFE;
  border-radius: 10px;
  padding: 14px 18px;
  display: flex;
  gap: 24px;
  flex-wrap: wrap;
}
.insight-item { display: flex; flex-direction: column; gap: 2px; }
.insight-label { font-size: 10px; color: #64748B; text-transform: uppercase; letter-spacing: 0.7px; font-weight: 600; }
.insight-value { font-size: 14px; color: #1E40AF; font-weight: 600; }

/* === DataTable === */
.dataTables_wrapper .dataTables_filter input {
  border: 1px solid #E2E8F0 !important; border-radius: 6px !important; padding: 4px 10px !important; font-size: 12px !important;
}
table.dataTable thead th {
  background: #F8FAFC !important; color: #475569 !important;
  font-size: 11.5px !important; font-weight: 600 !important;
  border-bottom: 2px solid #E2E8F0 !important; border-top: none !important;
  padding: 10px 12px !important;
}
table.dataTable tbody td { font-size: 12px !important; color: #334155 !important; padding: 8px 12px !important; }
table.dataTable tbody tr:hover { background: #F0F7FF !important; }
.dataTables_info, .dataTables_paginate { font-size: 11.5px !important; color: #64748B !important; }
"

# ---- UI ----
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$span(
      tags$i(class = "fab fa-spotify", style = "color:#1DB954; margin-right:8px;"),
      "Spotify Analytics"
    )
  ),
  
  dashboardSidebar(
    tags$head(tags$style(HTML(custom_css))),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("chart-bar"))
    ),
    tags$div(class = "sidebar-section",
             tags$label("Artist", class = "control-label"),
             selectInput("artist", label = NULL,
                         choices = c("All", unique(data_music$artist_name)), width = "100%"),
             tags$label("Genre", class = "control-label"),
             selectInput("genre", label = NULL,
                         choices = c("All", unique(data_music$genre)), width = "100%"),
             tags$label("Filter Streams", class = "control-label"),
             sliderInput("stream", label = NULL,
                         min   = min(data_music$streams, na.rm = TRUE),
                         max   = max(data_music$streams, na.rm = TRUE),
                         value = c(min(data_music$streams, na.rm = TRUE),
                                   max(data_music$streams, na.rm = TRUE)),
                         width = "100%")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dash",
              # Value boxes
              fluidRow(
                valueBoxOutput("total_song",   width = 4),
                valueBoxOutput("total_stream", width = 4),
                valueBoxOutput("avg_stream",   width = 4)
              ),
              
              # Row 1
              fluidRow(
                box(width = 6, title = "Top 10 Lagu — Streams Tertinggi",
                    plotlyOutput("topPlot", height = "300px")),
                box(width = 6, title = "Distribusi Genre Musik Global",
                    plotlyOutput("genrePlot", height = "300px"))
              ),
              
              # Row 2
              fluidRow(
                box(width = 6, title = "Durasi Popularitas vs Streams",
                    plotlyOutput("scatter", height = "300px")),
                box(width = 6, title = "Distribusi Kategori Popularitas",
                    plotlyOutput("barPop", height = "300px"))
              ),
              
              # Insight
              fluidRow(
                box(width = 12, title = "Insight Otomatis", uiOutput("insight"))
              ),
              
              # Row 3
              fluidRow(
                box(width = 6, title = "Top 10 Artist — Total Streams",
                    plotlyOutput("artistPlot", height = "300px")),
                box(width = 6, title = "Distribusi Streams per Genre (Boxplot)",
                    plotlyOutput("boxPlot", height = "300px"))
              ),
              
              # Table
              fluidRow(
                box(width = 12, title = "Tabel Data", DTOutput("table"))
              )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  # ---- Theme Looker ----
  theme_looker <- function() {
    theme_minimal(base_size = 12) +
      theme(
        plot.background    = element_rect(fill = "white", color = NA),
        panel.background   = element_rect(fill = "white", color = NA),
        panel.grid.major.x = element_line(color = "#F1F5F9", linewidth = 0.5),
        panel.grid.major.y = element_line(color = "#F1F5F9", linewidth = 0.5),
        panel.grid.minor   = element_blank(),
        axis.text          = element_text(color = "#64748B", size = 10),
        axis.title         = element_blank(),
        axis.ticks         = element_blank(),
        legend.position    = "bottom",
        legend.text        = element_text(size = 9, color = "#475569"),
        legend.title       = element_blank(),
        plot.margin        = margin(8, 8, 8, 8)
      )
  }
  
  # ---- Filter ----
  df <- reactive({
    tmp <- data_music
    if (input$artist != "All") tmp <- tmp %>% filter(artist_name == input$artist)
    if (input$genre  != "All") tmp <- tmp %>% filter(genre == input$genre)
    tmp %>% filter(streams >= input$stream[1], streams <= input$stream[2])
  })
  
  # ---- Value Boxes ----
  output$total_song <- renderValueBox({
    valueBox(format(nrow(df()), big.mark = ","),
             "Total Lagu", icon = icon("music"), color = "blue")
  })
  output$total_stream <- renderValueBox({
    tot <- sum(df()$streams, na.rm = TRUE)
    valueBox(fmt_num(tot), "Total Streams", icon = icon("play"), color = "green")
  })
  output$avg_stream <- renderValueBox({
    avg <- mean(df()$streams, na.rm = TRUE)
    valueBox(fmt_num(avg), "Rata-rata Streams", icon = icon("chart-line"), color = "yellow")
  })
  
  # ---- Plotly config (hilangkan toolbar berantakan) ----
  cfg <- function(p) {
    p %>% config(displayModeBar = FALSE) %>%
      layout(
        font   = list(family = "Google Sans, Roboto, sans-serif", size = 11, color = "#475569"),
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        margin = list(l = 0, r = 0, t = 10, b = 0, pad = 0)
      )
  }
  
  # ---- Top 10 Lagu ---- (track name dipotong max 20 karakter)
  output$topPlot <- renderPlotly({
    top10 <- df() %>%
      arrange(desc(streams)) %>%
      head(10) %>%
      mutate(
        label_short = ifelse(nchar(track_name) > 20,
                             paste0(substr(track_name, 1, 18), "…"),
                             track_name)
      )
    
    plot_ly(top10,
            x = ~streams,
            y = ~reorder(label_short, streams),
            type = "bar", orientation = "h",
            marker = list(color = "#4285F4",
                          line  = list(color = "white", width = 0.5)),
            text  = ~paste0(fmt_num(streams)),
            textposition = "outside",
            hovertemplate = paste0("<b>%{y}</b><br>",
                                   "Artist: ", top10$artist_name,
                                   "<br>Streams: %{x:,.0f}<extra></extra>")
    ) %>%
      layout(
        xaxis = list(
          title     = "",            # <-- hapus judul sumbu
          showgrid  = TRUE,
          gridcolor = "#F1F5F9",
          zeroline  = FALSE,
          tickformat = ".2s"
        ),
        yaxis = list(
          title     = "",            # <-- hapus judul sumbu
          showgrid  = FALSE,
          automargin = TRUE,
          tickfont  = list(size = 11)
        )
      ) %>% cfg()
  })
  
  # ---- Genre Pie (donut, tanpa label luar, % di dalam hover saja) ----
  output$genrePlot <- renderPlotly({
    tmp <- df() %>% count(genre, sort = TRUE) %>% filter(!is.na(genre))
    
    top8 <- tmp %>% head(8)
    others_n <- sum(tmp$n) - sum(top8$n)
    if (others_n > 0) top8 <- bind_rows(top8, tibble(genre = "Others", n = others_n))
    
    plot_ly(top8,
            labels = ~genre, values = ~n,
            type   = "pie",
            marker = list(
              colors = c("#4285F4","#34A853","#FBBC04","#EA4335",
                         "#9C27B0","#00BCD4","#FF5722","#607D8B","#B0BEC5"),
              line   = list(color = "white", width = 2)
            ),
            textinfo      = "percent",        # hanya % di dalam slice
            textposition  = "inside",
            insidetextorientation = "radial",
            hovertemplate = "<b>%{label}</b><br>%{value} lagu<br>%{percent}<extra></extra>",
            hole = 0.4
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "v",
          x = 1, y = 0.5,
          font = list(size = 10),
          itemsizing = "constant"
        )
      ) %>% cfg()
  })
  
  # ---- Scatter (hapus judul sumbu otomatis) ----
  output$scatter <- renderPlotly({
    d <- df()
    trends <- unique(d$trend)
    pal_scatter <- setNames(PAL[seq_along(trends)], trends)
    
    plot_ly(d,
            x = ~days, y = ~streams,
            color = ~trend, colors = pal_scatter,
            type  = "scatter", mode = "markers",
            marker = list(size = 7, opacity = 0.65,
                          line = list(width = 0)),
            text  = ~paste0("<b>", track_name, "</b><br>",
                            "Days: ", days, "<br>",
                            "Streams: ", fmt_num(streams)),
            hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(
          title     = "Hari di chart",         # singkat & jelas
          titlefont = list(size = 11, color = "#94A3B8"),
          showgrid  = TRUE, gridcolor = "#F1F5F9",
          zeroline  = FALSE, tickformat = ",d"
        ),
        yaxis = list(
          title     = "Streams",
          titlefont = list(size = 11, color = "#94A3B8"),
          showgrid  = TRUE, gridcolor = "#F1F5F9",
          zeroline  = FALSE, tickformat = ".2s"
        ),
        legend = list(orientation = "h", y = -0.2,
                      x = 0.5, xanchor = "center",
                      font = list(size = 10))
      ) %>% cfg()
  })
  
  # ---- Bar Popularitas (hapus judul sumbu) ----
  output$barPop <- renderPlotly({
    tmp <- df() %>% count(popularity_category) %>% arrange(desc(n))
    
    plot_ly(tmp,
            x = ~n,
            y = ~reorder(popularity_category, n),
            type = "bar", orientation = "h",
            marker = list(color = "#34A853",
                          line  = list(color = "white", width = 0.5)),
            text = ~n, textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>%{x} lagu<extra></extra>"
    ) %>%
      layout(
        xaxis = list(
          title     = "",
          showgrid  = TRUE, gridcolor = "#F1F5F9",
          zeroline  = FALSE, tickformat = ",d"
        ),
        yaxis = list(
          title      = "",
          showgrid   = FALSE,
          automargin = TRUE,
          tickfont   = list(size = 11)
        )
      ) %>% cfg()
  })
  
  # ---- Top Artist (hapus judul sumbu, potong nama panjang) ----
  output$artistPlot <- renderPlotly({
    tmp <- df() %>%
      group_by(artist_name) %>%
      summarise(total_streams = sum(streams, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_streams)) %>%
      head(10) %>%
      mutate(
        label_short = ifelse(nchar(artist_name) > 20,
                             paste0(substr(artist_name, 1, 18), "…"),
                             artist_name)
      )
    
    plot_ly(tmp,
            x = ~total_streams,
            y = ~reorder(label_short, total_streams),
            type = "bar", orientation = "h",
            marker = list(color = "#9C27B0",
                          line  = list(color = "white", width = 0.5)),
            text = ~fmt_num(total_streams), textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Total: %{x:,.0f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(
          title     = "",
          showgrid  = TRUE, gridcolor = "#F1F5F9",
          zeroline  = FALSE, tickformat = ".2s"
        ),
        yaxis = list(
          title      = "",
          showgrid   = FALSE,
          automargin = TRUE,
          tickfont   = list(size = 11)
        )
      ) %>% cfg()
  })
  
  # ---- Boxplot (hapus judul sumbu) ----
  output$boxPlot <- renderPlotly({
    top_genre <- df() %>% count(genre, sort = TRUE) %>% head(8)
    d <- df() %>% filter(genre %in% top_genre$genre)
    
    plot_ly(d,
            x = ~streams, y = ~genre,
            color = ~genre, colors = PAL,
            type  = "box",
            boxpoints = "outliers",
            marker = list(size = 4, opacity = 0.5),
            hovertemplate = "<b>%{y}</b><br>Streams: %{x:,.0f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(
          title     = "Streams",
          titlefont = list(size = 11, color = "#94A3B8"),
          showgrid  = TRUE, gridcolor = "#F1F5F9",
          zeroline  = FALSE, tickformat = ".2s"
        ),
        yaxis = list(
          title      = "",
          showgrid   = FALSE,
          automargin = TRUE,
          tickfont   = list(size = 11)
        ),
        showlegend = FALSE
      ) %>% cfg()
  })
  # ---- Insight ----
  output$insight <- renderUI({
    if (nrow(df()) == 0)
      return(tags$div(class = "insight-card", "Tidak ada data sesuai filter."))
    
    top     <- df() %>% arrange(desc(streams)) %>% slice(1)
    dom_cat <- names(sort(table(df()$popularity_category), decreasing = TRUE))[1]
    dom_trn <- names(sort(table(df()$trend),              decreasing = TRUE))[1]
    top_art <- df() %>%
      group_by(artist_name) %>%
      summarise(s = sum(streams, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(s)) %>% slice(1)
    total_lagu   <- nrow(df())
    total_stream <- sum(df()$streams, na.rm = TRUE)
    
    tags$div(
      style = "display:flex; flex-direction:column; gap:14px;",
      
      # --- Baris atas: Lagu terpopuler (featured card) ---
      tags$div(
        style = paste(
          "background: linear-gradient(135deg, #1E1E2E 0%, #2D2D42 100%);",
          "border-radius: 12px;",
          "padding: 18px 22px;",
          "display: flex;",
          "align-items: center;",
          "gap: 20px;"
        ),
        # Ikon musik
        tags$div(
          style = paste(
            "width: 52px; height: 52px;",
            "background: rgba(29,185,84,0.2);",
            "border-radius: 50%;",
            "display: flex; align-items: center; justify-content: center;",
            "flex-shrink: 0;"
          ),
          tags$i(class = "fas fa-music", style = "color:#1DB954; font-size:20px;")
        ),
        # Teks
        tags$div(
          style = "flex:1;",
          tags$p(style = "font-size:10px; color:#64748B; text-transform:uppercase;
                        letter-spacing:0.8px; margin:0 0 4px;",
                 "Lagu Terpopuler"),
          tags$p(style = "font-size:17px; font-weight:700; color:#fff; margin:0 0 2px;",
                 top$track_name),
          tags$p(style = "font-size:12px; color:#94A3B8; margin:0;",
                 paste0("oleh ", top$artist_name))
        ),
        # Streams badge
        tags$div(
          style = paste(
            "background: rgba(29,185,84,0.15);",
            "border: 1px solid rgba(29,185,84,0.3);",
            "border-radius: 8px;",
            "padding: 10px 16px;",
            "text-align: center;",
            "flex-shrink: 0;"
          ),
          tags$p(style = "font-size:10px; color:#1DB954; text-transform:uppercase;
                        letter-spacing:0.7px; margin:0 0 4px;", "Streams"),
          tags$p(style = "font-size:20px; font-weight:700; color:#1DB954; margin:0;",
                 fmt_num(top$streams))
        )
      ),
      
      # --- Baris bawah: 4 metric cards ---
      tags$div(
        style = "display:grid; grid-template-columns: repeat(4, minmax(0,1fr)); gap:12px;",
        
        # Card 1 — Top Artist
        tags$div(
          style = paste(
            "background:#fff; border:1px solid #E2E8F0;",
            "border-radius:10px; padding:14px 16px;",
            "border-top: 3px solid #9C27B0;"
          ),
          tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:8px;",
                   tags$i(class = "fas fa-user", style = "color:#9C27B0; font-size:13px;"),
                   tags$span(style = "font-size:10px; color:#64748B; text-transform:uppercase;
                             letter-spacing:0.7px;", "Top Artist")
          ),
          tags$p(style = "font-size:14px; font-weight:700; color:#1E293B; margin:0 0 2px;",
                 top_art$artist_name),
          tags$p(style = "font-size:11px; color:#94A3B8; margin:0;",
                 paste0(fmt_num(top_art$s), " total streams"))
        ),
        
        # Card 2 — Kategori Dominan
        tags$div(
          style = paste(
            "background:#fff; border:1px solid #E2E8F0;",
            "border-radius:10px; padding:14px 16px;",
            "border-top: 3px solid #FBBC04;"
          ),
          tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:8px;",
                   tags$i(class = "fas fa-star", style = "color:#FBBC04; font-size:13px;"),
                   tags$span(style = "font-size:10px; color:#64748B; text-transform:uppercase;
                             letter-spacing:0.7px;", "Kategori Dominan")
          ),
          tags$p(style = "font-size:14px; font-weight:700; color:#1E293B; margin:0 0 2px;",
                 dom_cat),
          tags$p(style = "font-size:11px; color:#94A3B8; margin:0;",
                 paste0(sum(df()$popularity_category == dom_cat), " lagu"))
        ),
        
        # Card 3 — Trend Dominan
        tags$div(
          style = paste(
            "background:#fff; border:1px solid #E2E8F0;",
            "border-radius:10px; padding:14px 16px;",
            "border-top: 3px solid #4285F4;"
          ),
          tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:8px;",
                   tags$i(class = "fas fa-chart-line", style = "color:#4285F4; font-size:13px;"),
                   tags$span(style = "font-size:10px; color:#64748B; text-transform:uppercase;
                             letter-spacing:0.7px;", "Trend Dominan")
          ),
          tags$p(style = "font-size:14px; font-weight:700; color:#1E293B; margin:0 0 2px;",
                 dom_trn),
          tags$p(style = "font-size:11px; color:#94A3B8; margin:0;",
                 paste0(sum(df()$trend == dom_trn), " lagu"))
        ),
        
        # Card 4 — Total Data
        tags$div(
          style = paste(
            "background:#fff; border:1px solid #E2E8F0;",
            "border-radius:10px; padding:14px 16px;",
            "border-top: 3px solid #34A853;"
          ),
          tags$div(style = "display:flex; align-items:center; gap:8px; margin-bottom:8px;",
                   tags$i(class = "fas fa-database", style = "color:#34A853; font-size:13px;"),
                   tags$span(style = "font-size:10px; color:#64748B; text-transform:uppercase;
                             letter-spacing:0.7px;", "Total Data")
          ),
          tags$p(style = "font-size:14px; font-weight:700; color:#1E293B; margin:0 0 2px;",
                 paste0(format(total_lagu, big.mark = ","), " lagu")),
          tags$p(style = "font-size:11px; color:#94A3B8; margin:0;",
                 paste0(fmt_num(total_stream), " total streams"))
        )
      )
    )
  })
  # ---- Table ----
  output$table <- renderDT({
    datatable(df(),
              options  = list(pageLength = 10, scrollX = TRUE,
                              dom = "ftip",
                              language = list(search = "Cari:")),
              rownames = FALSE,
              class    = "compact stripe hover")
  })
}

shinyApp(ui, server)
