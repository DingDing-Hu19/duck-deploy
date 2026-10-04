# ============================================================
# 鸭芯智选 · 简单模式（R Shiny 参考实现 · V5.0）
# ------------------------------------------------------------
# 与专家模式同一套算法（加载 duck_tool_V5_expert.R 算法层）
# 面向基层人员：只选育种目标，自动完成清洗→筛指标→打分→留种
# 启动：直接 Rscript 本文件（端口 8687，浏览器打开 http://127.0.0.1:8687/）
# ============================================================

V5_FILE <- "./duck_tool_V5_expert.R"

# ---------- 0. 加载 V5 算法层（截断 shinyApp，只取函数） ----------
load_v5_algo <- function(v5_file) {
  txt <- readLines(v5_file, encoding = "UTF-8")
  app_line <- grep("^shinyApp", txt)
  if (length(app_line) > 0) txt <- txt[seq_len(app_line[1] - 1)]
  tf <- tempfile(fileext = ".R")
  writeLines(txt, tf, useBytes = TRUE)
  source(tf, encoding = "UTF-8")
  unlink(tf)
  invisible(TRUE)
}
load_v5_algo(V5_FILE)

# ---------- 1. 育种目标映射 / 异常识别 / 一键全流程 ----------
SIMPLE_GOAL_MAP <- list(
  save   = c("FCR", "RFI"),
  grow   = c("ADG_g", "FBW_kg", "Gain_kg"),
  fat    = c("SkinFat_Rate", "AbFat_Rate"),
  meat  = c("BMP"),
  rhythm = c("CV_Daily_FI", "Cosinor_A", "Fano", "Day_FI_Ratio"))

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) names(df)[1] else hit[1]
}

flag_anomalies <- function(prod, index_df = NULL) {
  out <- list()
  if ("Day_Bout_Ratio" %in% names(prod) && "TFB_Day" %in% names(prod)) {
    thr <- stats::quantile(prod$TFB_Day, 0.05, na.rm = TRUE)
    d <- prod %>% filter(Day_Bout_Ratio > 0.97 | TFB_Day < thr) %>%
      mutate(异常类型 = "采食节律异常",
             具体表现 = ifelse(Day_Bout_Ratio > 0.97,
                             "夜间几乎不采食（白天占比>97%）",
                             "日访饲次数过低（群体最低5%）"))
    if (nrow(d) > 0) out <- c(out, list(d))
  }
  if ("Gain_kg" %in% names(prod)) {
    thr <- stats::quantile(prod$Gain_kg, 0.05, na.rm = TRUE)
    d <- prod %>% filter(Gain_kg < thr) %>%
      mutate(异常类型 = "体重增长异常", 具体表现 = "总增重过低（群体最低5%）")
    if (nrow(d) > 0) out <- c(out, list(d))
  }
  if (!is.null(index_df) && "Index" %in% names(index_df)) {
    d <- index_df %>% filter(Index < stats::quantile(Index, 0.05, na.rm = TRUE)) %>%
      mutate(异常类型 = "综合得分极低", 具体表现 = "综合得分处于群体最低5%")
    if (nrow(d) > 0) out <- c(out, list(d))
  }
  if (length(out) == 0) return(data.frame(Animal_ID = character(0), 异常类型 = character(0), 具体表现 = character(0)))
  bind_rows(out) %>% select(Animal_ID, 异常类型, 具体表现) %>% distinct()
}

run_simple_all <- function(feed_files, bw_files, ped_path = NULL, idmap_path = NULL,
                           goals = c("save"), ratio = 0.3, sex_balance = TRUE,
                           auto_time = TRUE, experiment_start = NA,
                           day_start = "06:00", day_end = "18:00") {
  t0 <- Sys.time()
  target <- unique(unlist(SIMPLE_GOAL_MAP[goals]))
  use_ped <- !is.null(ped_path)
  cfg <- list(
    auto_time_range = auto_time, experiment_start = if (auto_time) NA else experiment_start,
    training_end = NA, training_time = NA,
    week_length = 7, feed_sd = 3, bw_sd = 3, imi_threshold = 300, min_intake = 1,
    hff_method = "median", hff_cutoff = NA, day_start = day_start, day_end = day_end,
    target_trait = target, indicator_min_r = 0.3, indicator_max_n = 3,
    use_pedigree = use_ped, use_reml_h2 = use_ped,
    fixed_effects = "Sex", retention_ratio = ratio, sex_balance = sex_balance,
    run_retention = TRUE)

  feed_raw <- read_matching_excel_files(feed_files, "feed")
  bw_raw   <- read_matching_excel_files(bw_files, "weight")
  ped  <- if (use_ped) read_pedigree(ped_path) else NULL
  idmap <- if (!is.null(idmap_path)) read_idmap(idmap_path) else NULL
  clean <- run_clean_v5(feed_raw$data, bw_raw$data, cfg)
  bout  <- build_bouts(clean$feed, cfg$imi_threshold, cfg$min_intake)
  feeding <- calc_individual_feeding(bout)
  prod  <- calc_production(clean$feed, clean$bw, feeding)
  prod  <- assign_hff_lff(prod, method = cfg$hff_method, cutoff = cfg$hff_cutoff)
  prod$HFF <- ifelse(prod$Feed_Frequency_Group == "HFF", 1, 0)
  prod  <- prod %>%
    left_join(compute_daynight_v5(bout, day_start, day_end), by = "Animal_ID") %>%
    left_join(compute_behavior_cv_v5(bout), by = "Animal_ID") %>%
    left_join(compute_cosinor_v5(bout), by = "Animal_ID") %>%
    left_join(compute_fano_v5(bout), by = "Animal_ID")
  rhythm     <- calc_rhythm(bout)
  weekly_fcr <- calc_weekly_fcr(clean$feed, clean$bw, cfg$week_length)
  link <- link_animals(clean$feed, clean$bw, ped, idmap)
  if ("eID" %in% names(link) && "Sex" %in% names(link)) {
    prod <- prod %>% left_join(link %>% select(eID, Sex) %>% distinct(eID, .keep_all = TRUE),
                               by = c("Animal_ID" = "eID"))
  }

  MAX_TRAITS <- 8
  meas <- intersect(target, names(prod))
  if (length(meas) > MAX_TRAITS) meas <- meas[seq_len(MAX_TRAITS)]
  ind_slots <- MAX_TRAITS - length(meas)
  ind_traits <- if (ind_slots > 0) {
    select_indicator_traits(prod, target, cfg$indicator_min_r, cfg$indicator_max_n)
  } else {
    data.frame(Indicator = character(0))
  }
  if (nrow(ind_traits) > ind_slots) ind_traits <- ind_traits[seq_len(ind_slots), , drop = FALSE]
  idx_traits <- unique(c(meas, ind_traits$Indicator))
  n_meas <- length(meas); n_ind <- length(idx_traits) - n_meas
  w <- numeric(length(idx_traits)); names(w) <- idx_traits
  if (n_ind > 0 && n_meas > 0) {
    w[meas] <- 0.7 / n_meas
    w[idx_traits[!idx_traits %in% meas]] <- 0.3 / n_ind
  } else {
    w[] <- 1 / length(idx_traits)
  }
  cfg$index_traits <- idx_traits
  cfg$weights <- w
  cfg$weights_note <- if (n_ind > 0) "目标性状合计70% / 自动补充性状合计30%（组内均分）" else "参与打分性状均分"
  rr <- run_retention_module(prod, link, cfg)

  growth <- clean$bw_week_stats
  growth_plot_data <- data.frame(
    Week = growth[[pick_col(growth, c("Week_Number", "周次", "Week"))]],
    Mean_BW = growth[[pick_col(growth, c("Mean_BW", "BW", "体重", "末重", "Avg_BW"))]])

  list(
    A = list(
      summary = data.frame(
        指标 = c("评估路线", "参与打分性状数", "参与打分性状", "总个体", "留种个体",
                  "留种比例", "用时(秒)"),
        值 = c(rr$genetic$mode, length(idx_traits), paste(idx_traits, collapse = "、"),
                rr$retention$total, rr$retention$n_keep,
                paste0(round(rr$retention$ratio * 100), "%"),
                as.character(round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1))),
        stringsAsFactors = FALSE),
      retained = rr$retention$retained,
      ratio = rr$retention$ratio,
      all_index = rr$genetic$index,
      indicator_traits = rr$indicator_traits,
      warnings = rr$warnings %||% character(0)),
    B = list(
      week_feed = clean$feed_week_stats,
      week_bw = clean$bw_week_stats,
      weekly_fcr = weekly_fcr,
      growth = growth_plot_data),
    C = list(
      rhythm_daily = rhythm$daily,
      rhythm_hourly = rhythm$hourly,
      daynight = prod %>% select(any_of(c("Animal_ID", "Day_FI_g", "Night_FI_g", "Day_Bouts",
                                          "Night_Bouts", "Day_FI_Ratio", "Day_Bout_Ratio"))),
      cv = prod %>% select(any_of(c("Animal_ID", "CV_Duration", "CV_FR", "CV_Daily_Bouts",
                                    "CV_Daily_FI", "CV_Daily_TFD"))),
      anomalies = flag_anomalies(prod, rr$genetic$index)),
    prod = prod,
    elapsed = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

export_simple_xlsx <- function(res, path) {
  wb <- list(
    "A_留种名单" = res$A$retained,
    "A_全部个体得分" = res$A$all_index,
    "A_指示性状" = res$A$indicator_traits,
    "B_周统计_采食" = res$B$week_feed,
    "B_周统计_体重" = res$B$week_bw,
    "B_每周FCR" = res$B$weekly_fcr,
    "C_昼夜分配" = res$C$daynight,
    "C_行为变异性" = res$C$cv,
    "C_异常个体" = res$C$anomalies)
  wb <- wb[!sapply(wb, is.null)]
  if (length(wb) == 0) stop("无结果可导出")
  openxlsx::write.xlsx(wb, path)
  invisible(path)
}

# ---------- 2. UI ----------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Microsoft YaHei', sans-serif; }
    .btn-run { background-color:#28a745; color:#fff; font-weight:bold; width:100%; }
    .alert-y { background-color:#fff3cd; color:#664d03; padding:8px 12px; border-radius:6px; }
  "))),
  titlePanel(div(
    h3("鸭芯智选 · 简单模式", style = "margin-bottom:2px;"),
    p("三步选出好鸭：传数据 → 选目标 → 拿结果。不用懂遗传育种，软件自动完成清洗、筛指标、打分、排名。",
      style = "color:#6c757d; font-size:13px; margin-top:0;")
  )),
  sidebarLayout(
    sidebarPanel(width = 3,
      h4("① 上传数据"),
      fileInput("feed_files", "采食表（必传，可多选）", multiple = TRUE,
                accept = c(".xls", ".xlsx"), width = "100%"),
      fileInput("bw_files", "体重表（必传，可多选）", multiple = TRUE,
                accept = c(".xls", ".xlsx"), width = "100%"),
      fileInput("ped_file", "系谱（可选，有系谱更准）", accept = c(".xlsx", ".xls"), width = "100%"),
      fileInput("idmap_file", "ID对照表（可选）", accept = c(".xlsx", ".xls"), width = "100%"),
      hr(),
      h4("时间范围（可选）"),
      checkboxInput("auto_time", "自动使用数据全部时间（默认）", value = TRUE),
      conditionalPanel("!input.auto_time",
        dateInput("exp_start", "试验开始日期（早于此日期的记录将被过滤）",
                  value = Sys.Date() - 30)),
      hr(),
      h4("② 育种目标（可多选）"),
      checkboxInput("goal_save", "吃得省（料重比低、省饲料）", value = TRUE),
      checkboxInput("goal_grow", "长得快（日增重高、出栏重）", value = TRUE),
      checkboxInput("goal_fat", "皮脂厚（烤鸭用，未测自动用文献关联指标）", value = FALSE),
      checkboxInput("goal_meat", "胸肌厚（瘦肉多，未测自动用文献关联指标）", value = FALSE),
      checkboxInput("goal_rhythm", "采食规律（节律稳定）", value = FALSE),
      hr(),
      h4("③ 留种设置"),
      sliderInput("ratio", "留种比例", min = 5, max = 80, value = 30, post = " %"),
      checkboxInput("sex_balance", "尽量公母均衡", value = TRUE),
      br(),
      actionButton("run_simple", "开始选留种鸭", class = "btn-run"),
      br(), br(),
      textOutput("mode_note"),
      uiOutput("warn_box")
    ),
    mainPanel(width = 9,
      tabsetPanel(id = "simple_tabs", type = "tabs",
        tabPanel("🦆 选留种鸭", value = "A",
          br(), tableOutput("summary_tbl"), br(),
          h4("留种名单（前", textOutput("keep_pct", inline = TRUE), "）"),
          DT::dataTableOutput("retained_tbl"),
          br(), h4("综合得分分布"), plotOutput("index_plot", height = "340px"),
          br(), downloadButton("dl_xlsx", "导出全部结果（Excel）")),
        tabPanel("📊 生产报表", value = "B",
          br(), h4("周统计 · 采食"), DT::dataTableOutput("week_feed_tbl"),
          br(), h4("周统计 · 体重"), DT::dataTableOutput("week_bw_tbl"),
          br(), h4("每周 FCR"), plotOutput("fcr_plot", height = "300px"),
          DT::dataTableOutput("fcr_tbl"),
          br(), h4("周平均体重（生长曲线）"), plotOutput("growth_plot", height = "300px")),
        tabPanel("🔍 采食行为", value = "C",
          br(), h4("日访饲节律"), plotOutput("daily_plot", height = "300px"),
          br(), h4("24 小时访饲节律（按周）"), plotOutput("hourly_plot", height = "320px"),
          br(), h4("昼夜分配"), DT::dataTableOutput("daynight_tbl"),
          br(), h4("行为变异性"), DT::dataTableOutput("cv_tbl"),
          br(), h4("异常个体预警（暂缓复测，不淘汰）"), DT::dataTableOutput("anomaly_tbl"))
      )
    )
  )
)

# ---------- 3. Server ----------
server <- function(input, output, session) {
  goals <- reactive({
    g <- c()
    if (input$goal_save) g <- c(g, "save")
    if (input$goal_grow) g <- c(g, "grow")
    if (input$goal_fat) g <- c(g, "fat")
    if (input$goal_meat) g <- c(g, "meat")
    if (input$goal_rhythm) g <- c(g, "rhythm")
    if (length(g) == 0) g <- "save"
    g
  })

  output$mode_note <- renderText({
    if (is.null(input$ped_file)) {
      "未上传系谱：将自动采用权威文献遗传参数（不影响使用）"
    } else {
      "已上传系谱：将用本场数据实测遗传力，评估更准"
    }
  })

  res <- eventReactive(input$run_simple, {
    req(input$feed_files, input$bw_files)
    withProgress(message = "简单模式计算中（约 1-2 分钟）……", value = 0.3, {
      incProgress(0.1, detail = "清洗数据")
      ped_path <- if (is.null(input$ped_file)) NULL else input$ped_file$datapath
      idmap_path <- if (is.null(input$idmap_file)) NULL else input$idmap_file$datapath
      r <- run_simple_all(input$feed_files, input$bw_files, ped_path, idmap_path,
                          goals = goals(), ratio = input$ratio / 100,
                          sex_balance = input$sex_balance,
                          auto_time = input$auto_time,
                          experiment_start = if (input$auto_time) NA else input$exp_start)
      incProgress(0.9, detail = "完成")
      r
    })
  })

  output$keep_pct <- renderText({ paste0(round(res()$A$ratio * 100), "%") })
  output$summary_tbl <- renderTable({ res()$A$summary }, width = "100%")

  output$warn_box <- renderUI({
    w <- res()$A$warnings
    a <- res()$C$anomalies
    txt <- character(0)
    if (length(w) > 0) txt <- c(txt, paste0("⚠ ", w))
    if (nrow(a) > 0) txt <- c(txt, paste0("⚠ 检出 ", nrow(a), " 条异常记录，见『采食行为』页（暂缓复测，不淘汰）"))
    if (length(txt) == 0) return(NULL)
    div(class = "alert-y", HTML(paste(txt, collapse = "<br/>")))
  })

  output$retained_tbl <- DT::renderDataTable({
    DT::datatable(res()$A$retained, options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })

  output$index_plot <- renderPlot({
    idx <- res()$A$all_index
    keep_ids <- res()$A$retained$Animal_ID
    idx$Is_Retained <- idx$Animal_ID %in% keep_ids
    thr <- min(idx$Index[idx$Is_Retained], na.rm = TRUE)
    ggplot(idx, aes(x = Index, fill = Is_Retained)) +
      geom_histogram(bins = 40, alpha = 0.85, color = "white", linewidth = 0.2) +
      geom_vline(xintercept = thr, linetype = "dashed", color = "black") +
      scale_fill_manual(values = c("TRUE" = PAL_RPBG[1], "FALSE" = PAL_RPBG[2]), name = "是否留种") +
      theme_bw(base_size = 13) + labs(x = "综合得分", y = "个体数",
        caption = paste0("虚线 = 留种阈值（前 ", round(res()$A$ratio * 100), "%）"))
  })

  output$week_feed_tbl <- DT::renderDataTable({
    DT::datatable(res()$B$week_feed, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  output$week_bw_tbl <- DT::renderDataTable({
    DT::datatable(res()$B$week_bw, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  output$fcr_tbl <- DT::renderDataTable({
    DT::datatable(res()$B$weekly_fcr, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  output$fcr_plot <- renderPlot({
    ggplot(res()$B$weekly_fcr, aes(x = factor(Week_Number), y = Weekly_FCR)) +
      geom_col(fill = PAL_RPBG[2], width = 0.6) + theme_bw(base_size = 13) +
      labs(x = "周次", y = "每周 FCR")
  })
  output$growth_plot <- renderPlot({
    ggplot(res()$B$growth, aes(x = Week, y = Mean_BW)) +
      geom_line(color = PAL_RPBG[1], linewidth = 1) + geom_point(color = PAL_RPBG[1]) +
      theme_bw(base_size = 13) + labs(x = "周次", y = "平均体重(kg)", title = "周平均体重（生长曲线）")
  })

  output$daily_plot <- renderPlot({
    ggplot(res()$C$rhythm_daily, aes(x = Date, y = Mean_Bouts_Per_Duck)) +
      geom_line(color = PAL_RPBG[3], linewidth = 1) + geom_point(color = PAL_RPBG[3]) +
      theme_bw(base_size = 13) + labs(x = "日期", y = "平均访饲次数/只/天")
  })
  output$hourly_plot <- renderPlot({
    rd <- res()$C$rhythm_hourly
    ggplot(rd, aes(x = Hour_Block, y = Mean_Bouts_Per_Duck,
                   color = factor(Week_Number), group = factor(Week_Number))) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_color_manual(values = rep(PAL_RPBG, length.out = dplyr::n_distinct(rd$Week_Number))) +
      scale_x_continuous(breaks = 0:23) + theme_bw(base_size = 13) +
      labs(x = "时刻 (h)", y = "平均访饲次数/只", color = "周次")
  })
  output$daynight_tbl <- DT::renderDataTable({
    DT::datatable(res()$C$daynight, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  output$cv_tbl <- DT::renderDataTable({
    DT::datatable(res()$C$cv, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  output$anomaly_tbl <- DT::renderDataTable({
    DT::datatable(res()$C$anomalies, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$dl_xlsx <- downloadHandler(
    filename = function() paste0("简单模式_结果_", Sys.Date(), ".xlsx"),
    content = function(file) export_simple_xlsx(res(), file))
}

# ===== 命令行入口 =====
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 2) {
    library(jsonlite)
    cfg <- fromJSON(args[1])
    feed_files <- data.frame(datapath = cfg$feed_files, name = basename(cfg$feed_files))
    bw_files <- data.frame(datapath = cfg$bw_files, name = basename(cfg$bw_files))
    res <- run_simple_all(feed_files, bw_files,
                          ped_path = cfg$ped_path,
                          idmap_path = cfg$idmap_path,
                          goals = cfg$goals,
                          ratio = cfg$ratio,
                          sex_balance = cfg$sex_balance)
    out <- list(summary = res$A$summary,
                retained = res$A$retained,
                all_index = res$A$all_index,
                indicator_traits = res$A$indicator_traits,
                weekly_fcr = res$B$weekly_fcr,
                anomalies = res$C$anomalies)
    write_json(out, cfg$output, dataframe = "rows", auto_unbox = TRUE)
  }
}
