# ============================================================
# 肉鸭智能化采食行为分析工具 V5.0 —— 专家模式
# 四川农业大学 张雅晨团队 · 大挑项目 · 留种决策模块
# ============================================================
# 六大模块：
#   ① 数据与清洗     ② 个体指标     ③ 组间时序相关
#   ④ 创新行为指标   ⑤ 遗传评估与留种（核心）  ⑥ 深度学习探索
# 核心链路：数据接入与ID打通 → 采食行为指标 → 指示性状筛选
#           → 遗传评估（PBLUP / 文献参数）→ 选择指数（按性状方向加权）→ 留种名单
# 说明：重型遗传评估在模块⑤点『运行遗传评估与留种』时执行；清洗阶段只做轻量指标与指示性状筛选。
# ============================================================

options(shiny.maxRequestSize = 4096 * 1024^2, stringsAsFactors = FALSE)

# ============================================================
# 0. 加载包
# ============================================================
required_pkgs <- c(
  "shiny", "readxl", "dplyr", "tidyr", "stringr", "lubridate",
  "ggplot2", "plotly", "DT", "openxlsx", "rlang", "Matrix",
  "randomForest", "xgboost", "cluster"
)
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(paste0("缺少 R 包，请先运行：install.packages(c(",
              paste0('"', missing_pkgs, '"', collapse = ", "), "))"), call. = FALSE)
}
library(shiny); library(readxl); library(dplyr); library(tidyr)
library(stringr); library(lubridate); library(ggplot2)
library(plotly); library(DT); library(openxlsx); library(rlang)
library(Matrix)
library(randomForest); library(xgboost); library(cluster)

# ============================================================
# 0.1 统一配色（v4.4『红紫蓝绿』方案精简保留）
# 红 #e41a1c · 蓝 #377eb8 · 绿 #4daf4a · 紫 #984ea3 · 橙 #ff7f00 · 黄 #ffff33
# 热图类仍用红-白-蓝连续梯度（离散色板不适合连续相关）
# ============================================================
PAL_RPBG <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33")

# ============================================================
# 0.2 图 + PNG 下载（v4.4 plot_box 对应）
# UI 端：图右上角固定“下载PNG”按钮
# server 端：每个图改为 reactive 返回 ggplot，renderPlot 渲染，
#            register_plot_dl 注册下载（ggsave 重绘，与界面同源）
# ============================================================
plot_box_ui <- function(plot_id, height = "600px") {
  div(
    style = "position:relative;",
    plotOutput(plot_id, height = height),
    div(
      style = "position:absolute; top:6px; right:25px; z-index:100;",
      downloadButton(paste0("dl_", plot_id), "下载PNG", class = "btn-sm")
    )
  )
}

register_plot_dl <- function(output, plot_id, plot_reactive,
                             w = 10, h = 6, dpi = 150) {
  output[[paste0("dl_", plot_id)]] <- downloadHandler(
    filename = function() paste0(plot_id, "_", format(Sys.Date(), "%Y%m%d"), ".png"),
    content = function(file) {
      p <- tryCatch(plot_reactive(), error = function(e) NULL)
      if (is.null(p)) {
        stop("图表当前不可用：请先运行对应分析（清洗/组间/创新/遗传评估）后再导出。",
             call. = FALSE)
      }
      ggsave(file, plot = p, width = w, height = h, dpi = dpi, bg = "white")
    })
}

for (pkg in c("plyr", "data.table")) {
  if (paste0("package:", pkg) %in% search()) {
    try(detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE), silent = TRUE)
  }
}
select <- dplyr::select; filter <- dplyr::filter; lag <- dplyr::lag
lead <- dplyr::lead; first <- dplyr::first; last <- dplyr::last
nth <- dplyr::nth; summarise <- dplyr::summarise; summarize <- dplyr::summarize
mutate <- dplyr::mutate; arrange <- dplyr::arrange; rename <- dplyr::rename
group_by <- dplyr::group_by; ungroup <- dplyr::ungroup; count <- dplyr::count
distinct <- dplyr::distinct; slice_tail <- dplyr::slice_tail
slice_head <- dplyr::slice_head; slice_sample <- dplyr::slice_sample
pull <- dplyr::pull; across <- dplyr::across; n_distinct <- dplyr::n_distinct
case_when <- dplyr::case_when; bind_rows <- dplyr::bind_rows
bind_cols <- dplyr::bind_cols; row_number <- dplyr::row_number
hour <- lubridate::hour; minute <- lubridate::minute; second <- lubridate::second
wday <- lubridate::wday; yday <- lubridate::yday; mday <- lubridate::mday

`%||%` <- function(x, y) if (is.null(x)) y else x

format_p <- function(x) {
  if (is.null(x)) return(x)
  if (!is.numeric(x)) return(x)
  ifelse(is.na(x), "", ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)))
}

# ============================================================
# 1. 基础辅助函数（读Excel / 字段匹配 / 时间解析）
# ============================================================

read_excel_clean <- function(path, sheet = 1, skip = 0) {
  dat <- readxl::read_excel(path, sheet = sheet, skip = skip, guess_max = 100000)
  dat <- as.data.frame(dat, check.names = FALSE)
  if (nrow(dat) > 0) {
    nonempty_matrix <- lapply(dat, function(x) {
      if (inherits(x, "POSIXt") || inherits(x, "Date") || is.numeric(x)) !is.na(x)
      else { z <- trimws(as.character(x)); !is.na(x) & z != "" }
    })
    keep_row <- Reduce(`|`, nonempty_matrix)
    dat <- dat[keep_row, , drop = FALSE]
  }
  if (ncol(dat) > 0) {
    keep_col <- sapply(dat, function(x) {
      if (inherits(x, "POSIXt") || inherits(x, "Date") || is.numeric(x)) any(!is.na(x))
      else { z <- trimws(as.character(x)); any(!is.na(x) & z != "") }
    })
    dat <- dat[, keep_col, drop = FALSE]
  }
  dat
}

clean_names_simple <- function(nms) {
  nms <- str_replace_all(nms, "\u00a0", " ")
  nms <- str_replace_all(nms, "\\r|\\n|\\t", "")
  str_trim(nms)
}

find_col <- function(dat, patterns, required = FALSE) {
  nms <- names(dat)
  hit <- unique(unlist(lapply(patterns, function(p) grep(p, nms, ignore.case = TRUE, perl = TRUE))))
  if (length(hit) == 0) {
    if (required) stop(paste0("无法找到需要的字段。候选关键词：",
                              paste(patterns, collapse = " / "),
                              "\n当前字段：\n", paste(nms, collapse = " | ")), call. = FALSE)
    return(NULL)
  }
  nms[hit[1]]
}

get_required_patterns <- function(dataset_type) {
  if (identical(dataset_type, "feed")) {
    list(
      animal = c("^耳标$", "耳标", "animal.*id", "tag"),
      time1 = c("^记录时间1$", "记录时间1", "记录.*时间.*1", "time.*1", "start.*time"),
      time2 = c("^记录时间2$", "记录时间2", "记录.*时间.*2", "time.*2", "end.*time"),
      intake = c("^处理后采食量\\s*\\(?g\\)?$", "^处理后采食量", "处理后.*采食量", "processed.*feed.*intake"),
      duration = c("^采食时长$", "采食.*时长", "feeding.*duration", "duration")
    )
  } else {
    list(
      animal = c("^耳标$", "耳标", "animal.*id", "tag"),
      time1 = c("^记录时间1$", "记录时间1", "记录.*时间.*1", "time.*1"),
      time2 = c("^记录时间2$", "记录时间2", "记录.*时间.*2", "time.*2"),
      bw1 = c("^体重1\\s*\\(?kg\\)?$", "^体重1", "weight1"),
      bw2 = c("^体重2\\s*\\(?kg\\)?$", "^体重2", "weight2"),
      bw3 = c("^体重3\\s*\\(?kg\\)?$", "^体重3", "weight3"),
      mean_bw = c("^平均体重\\s*\\(?kg\\)?$", "^平均体重", "mean.*weight", "average.*weight")
    )
  }
}

sheet_matches_dataset <- function(header_dat, dataset_type) {
  pats <- get_required_patterns(dataset_type)
  if (is.null(header_dat) || ncol(header_dat) == 0) return(FALSE)
  names(header_dat) <- clean_names_simple(names(header_dat))
  all(vapply(pats, function(p) !is.null(find_col(header_dat, p, FALSE)), logical(1)))
}

# 读取多个 Excel 文件，每个文件读取【全部 sheet】并合并（V5 关键改动）
read_matching_excel_files <- function(files, dataset_type) {
  if (is.null(files) || nrow(files) == 0) {
    stop(if (identical(dataset_type, "feed")) "请至少上传 1 个采食 Excel 文件。"
         else "请至少上传 1 个体重 Excel 文件。", call. = FALSE)
  }
  result_list <- list(); qc_list <- list()
  data_idx <- 0L; qc_idx <- 0L
  for (i in seq_len(nrow(files))) {
    path <- files$datapath[i]; file_name <- files$name[i]
    sheets <- readxl::excel_sheets(path)
    for (sheet_name in sheets) {
      qc_idx <- qc_idx + 1L
      header <- tryCatch({
        h <- readxl::read_excel(path, sheet = sheet_name, n_max = 5,
                                guess_max = 100, col_names = TRUE)
        as.data.frame(h, check.names = FALSE)
      }, error = function(e) NULL)
      matched <- FALSE; status <- "跳过：字段结构不匹配"; reason <- ""; row_n <- 0L
      if (!is.null(header)) {
        matched <- tryCatch(sheet_matches_dataset(header, dataset_type), error = function(e) FALSE)
      } else {
        status <- "跳过：工作表读取失败"; reason <- "无法读取表头"
      }
      if (matched) {
        tmp <- tryCatch(read_excel_clean(path, sheet = sheet_name), error = function(e) NULL)
        if (!is.null(tmp)) {
          names(tmp) <- clean_names_simple(names(tmp))
          row_n <- nrow(tmp)
          if (row_n > 0) {
            tmp$.Source_File <- file_name; tmp$.Source_Sheet <- sheet_name
            data_idx <- data_idx + 1L; result_list[[data_idx]] <- tmp
            status <- "已读取"; reason <- "包含完整数据字段"
          } else { status <- "跳过：工作表为空"; reason <- "字段匹配，但没有有效记录" }
        } else { status <- "跳过：工作表读取失败"; reason <- "完整读取失败" }
      }
      qc_list[[qc_idx]] <- data.frame(
        数据集 = if (identical(dataset_type, "feed")) "采食数据" else "体重数据",
        Source_File = file_name, Source_Sheet = sheet_name,
        状态 = status, 读取记录数 = row_n, 说明 = reason, stringsAsFactors = FALSE)
    }
  }
  if (length(result_list) == 0) {
    stop(paste0(if (identical(dataset_type, "feed")) "采食" else "体重",
                "数据中没有识别到包含完整字段的工作表。"), call. = FALSE)
  }
  list(data = bind_rows(result_list), qc = bind_rows(qc_list))
}

safe_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- as.character(x); x <- str_replace_all(x, ",", ""); x <- str_trim(x)
  x[x %in% c("", "NA", "NaN", "NULL", "null", "-")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

parse_datetime_flexible <- function(x) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = "Asia/Shanghai"))
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = "Asia/Shanghai"))
  if (is.numeric(x)) return(as.POSIXct(x * 86400, origin = "1899-12-30", tz = "Asia/Shanghai"))
  x <- str_trim(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "Asia/Shanghai")
  formats <- c(
    "%Y/%m/%d %H:%M:%S", "%Y-%m-%d %H:%M:%S", "%Y.%m.%d %H:%M:%S",
    "%Y/%m/%d %H:%M", "%Y-%m-%d %H:%M", "%Y.%m.%d %H:%M",
    "%Y/%m/%d", "%Y-%m-%d", "%Y.%m.%d",
    "%m/%d/%Y %H:%M:%S", "%m-%d-%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m-%d-%Y %H:%M",
    "%m/%d/%Y", "%m-%d-%Y",
    "%d/%m/%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M",
    "%d/%m/%Y", "%d-%m-%Y")
  for (fmt in formats) {
    idx <- is.na(out) & !is.na(x)
    if (!any(idx)) break
    parsed <- suppressWarnings(as.POSIXct(strptime(x[idx], format = fmt, tz = "Asia/Shanghai")))
    out[idx] <- parsed
  }
  idx <- is.na(out) & !is.na(x)
  if (any(idx)) {
    parsed <- suppressWarnings(parse_date_time(
      x[idx],
      orders = c("Ymd HMS", "Ymd HM", "Y/m/d HMS", "Y/m/d HM", "Y-m-d HMS", "Y-m-d HM",
                 "Y.m.d HMS", "Y.m.d HM", "mdy HMS", "mdy HM", "m/d/Y HMS", "m/d/Y HM",
                 "dmy HMS", "dmy HM", "d/m/Y HMS", "d/m/Y HM",
                 "Ymd", "Y/m/d", "Y-m-d", "Y.m.d", "mdy", "m/d/Y", "dmy", "d/m/Y"),
      tz = "Asia/Shanghai", quiet = TRUE))
    out[idx] <- parsed
  }
  out
}

parse_duration_seconds <- function(x) {
  if (inherits(x, "difftime")) return(as.numeric(x, units = "secs"))
  if (is.numeric(x)) return(as.numeric(x) * 86400)
  x <- str_trim(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  out <- rep(NA_real_, length(x))
  idx <- !is.na(x) & str_detect(x, "^\\d+\\.\\d{1,2}:\\d{2}:\\d{2}$")
  if (any(idx)) {
    parts <- str_match(x[idx], "^(\\d+)\\.(\\d{1,2}):(\\d{2}):(\\d{2})$")
    out[idx] <- as.numeric(parts[, 2]) * 86400 + as.numeric(parts[, 3]) * 3600 +
      as.numeric(parts[, 4]) * 60 + as.numeric(parts[, 5])
  }
  idx <- is.na(out) & !is.na(x) & str_detect(x, "^\\d+:\\d{2}:\\d{2}$")
  if (any(idx)) {
    parts <- str_match(x[idx], "^(\\d+):(\\d{2}):(\\d{2})$")
    out[idx] <- as.numeric(parts[, 2]) * 3600 + as.numeric(parts[, 3]) * 60 + as.numeric(parts[, 4])
  }
  idx <- is.na(out) & !is.na(x) & str_detect(x, "^\\d+:\\d{2}$")
  if (any(idx)) {
    parts <- str_match(x[idx], "^(\\d+):(\\d{2})$")
    out[idx] <- as.numeric(parts[, 2]) * 3600 + as.numeric(parts[, 3]) * 60
  }
  idx <- is.na(out) & !is.na(x) & str_detect(x, "^\\d+(\\.\\d+)?$")
  if (any(idx)) out[idx] <- suppressWarnings(as.numeric(x[idx]))
  out
}

parse_training_time <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))) return("00:00:00")
  x <- trimws(as.character(x))
  if (grepl("^\\d{1,2}:\\d{2}$", x)) x <- paste0(x, ":00")
  valid <- grepl("^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$", x)
  if (!valid) stop("训练结束时间格式错误，请输入 HH:MM:SS，例如 18:30:00。", call. = FALSE)
  x
}

parse_clock_hhmm <- function(x, default = "06:00") {
  if (is.null(x) || length(x) == 0) return(default)
  x <- trimws(as.character(x))
  if (!nzchar(x) || is.na(x)) return(default)
  if (grepl("^\\d{1,2}:\\d{2}:\\d{2}$", x)) {
    parts <- as.integer(strsplit(x, ":")[[1]]); return(sprintf("%02d:%02d", parts[1], parts[2]))
  }
  if (grepl("^\\d{1,2}:\\d{2}$", x)) {
    parts <- as.integer(strsplit(x, ":")[[1]]); return(sprintf("%02d:%02d", parts[1], parts[2]))
  }
  default
}

clock_to_minutes <- function(x) {
  x <- parse_clock_hhmm(x)
  parts <- as.integer(strsplit(x, ":")[[1]])
  parts[1] * 60 + parts[2]
}

make_experimental_week <- function(datetime, start_datetime, week_length) {
  if (length(datetime) == 0) {
    return(data.frame(Experimental_Day = numeric(0), Week = character(0),
                      Week_Number = numeric(0)))
  }
  day_num <- floor(as.numeric(difftime(as.POSIXct(datetime), as.POSIXct(start_datetime),
                                       units = "days"))) + 1
  week_num <- floor((day_num - 1) / week_length) + 1
  data.frame(Experimental_Day = day_num, Week = paste0("Week ", week_num), Week_Number = week_num)
}

remove_weekly_outliers <- function(dat, value_col, week_col = "Week_Number", sd_multiplier = 3) {
  stats <- dat %>%
    group_by(.data[[week_col]]) %>%
    summarise(
      N = sum(!is.na(.data[[value_col]]) & is.finite(.data[[value_col]])),
      Mean = ifelse(N > 0, mean(.data[[value_col]], na.rm = TRUE), NA_real_),
      SD = ifelse(N > 1, sd(.data[[value_col]], na.rm = TRUE), NA_real_), .groups = "drop") %>%
    mutate(
      Lower = case_when(is.na(SD) ~ -Inf, SD == 0 ~ Mean, TRUE ~ Mean - sd_multiplier * SD),
      Upper = case_when(is.na(SD) ~ Inf, SD == 0 ~ Mean, TRUE ~ Mean + sd_multiplier * SD))
  dat2 <- dat %>% left_join(stats, by = week_col)
  is_outlier <- !is.na(dat2[[value_col]]) & is.finite(dat2[[value_col]]) &
    (dat2[[value_col]] < dat2$Lower | dat2[[value_col]] > dat2$Upper)
  dat2$QC_Outlier <- is_outlier
  list(data = dat2, stats = stats)
}

first_non_na <- function(x) {
  x <- as.character(x); x <- x[!is.na(x) & trimws(x) != ""]
  if (length(x) == 0) return(NA_character_)
  x[1]
}
safe_mean <- function(x) { x <- x[!is.na(x) & is.finite(x)]; if (length(x) == 0) NA_real_ else mean(x) }
safe_median <- function(x) { x <- x[!is.na(x) & is.finite(x)]; if (length(x) == 0) NA_real_ else median(x) }
safe_sd <- function(x) { x <- x[!is.na(x) & is.finite(x)]; if (length(x) < 2) NA_real_ else sd(x) }

palette_choices <- list(
  "单色-浅蓝" = c("#9ecae1"), "单色-深蓝" = c("#2c7fb8"),
  "单色-绿" = c("#31a354"), "单色-橙" = c("#e6550d"),
  "单色-紫" = c("#756bb1"), "单色-灰" = c("#636363"),
  "暖色系" = c("#a30543", "#f36f43", "#fbda83"),
  "冷色系" = c("#e9f4a3", "#80cba4", "#4965b0"),
  "六色混合" = c("#a30543", "#f36f43", "#fbda83", "#e9f4a3", "#80cba4", "#4965b0"),
  "蓝色渐变" = c("#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c"),
  "黄绿渐变" = c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837"),
  "红紫蓝绿" = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33"),
  "Pastel" = c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6", "#ffffcc"),
  "深色系" = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02"))
heat_palette_choices <- list(
  "红-白-蓝(默认)" = c("#b2182b", "#f7f7f7", "#2166ac"),
  "红-白-绿" = c("#d73027", "#ffffbf", "#1a9850"),
  "紫-白-橙" = c("#762a83", "#f7f7f7", "#e08214"),
  "蓝-白-红" = c("#2166ac", "#f7f7f7", "#b2182b"),
  "深红-白-深蓝" = c("#67001f", "#f7f7f7", "#053061"),
  "橙-白-紫" = c("#f1a340", "#f7f7f7", "#998ec3"),
  "绿-白-红" = c("#1a9850", "#ffffbf", "#d73027"))

sample_for_plot <- function(dat, group_cols, max_n = 5000) {
  if (nrow(dat) <= max_n) return(dat)
  set.seed(1234)
  dat %>% group_by(across(all_of(group_cols))) %>%
    group_modify(~ { if (nrow(.x) > max_n) dplyr::slice_sample(.x, n = max_n) else .x }) %>%
    ungroup()
}

DT_fast <- function(dat, page_length = 25, filter = "top", scroll_x = TRUE) {
  datatable(dat, rownames = FALSE, filter = filter,
            options = list(pageLength = page_length, scrollX = scroll_x,
                           processing = TRUE, deferRender = TRUE))
}

# ============================================================
# 2. ID 打通与系谱处理（V5 核心新增）
# ============================================================

# 读取系谱：翅号 / 父本笼号 / 母本笼号 / 性别（一个笼只有一只鸭）
read_pedigree <- function(path) {
  if (is.null(path) || length(path) == 0 || is.na(path) || !file.exists(path)) {
    return(data.frame(ID = character(0), Sire_Cage = character(0),
                      Dam_Cage = character(0), Sex = character(0),
                      stringsAsFactors = FALSE))
  }
  ped <- read_excel_clean(path)
  nms <- names(ped)
  id_col <- find_col(ped, c("^翅号$", "翅号", "id", "animal"), required = TRUE)
  sire_col <- find_col(ped, c("^父本笼号$", "父本", "sire", "father"))
  dam_col <- find_col(ped, c("^母本笼号$", "母本", "dam", "mother"))
  sex_col <- find_col(ped, c("^性别$", "性别", "sex"))
  out <- data.frame(ID = as.character(ped[[id_col]]), stringsAsFactors = FALSE)
  out$Sire_Cage <- if (!is.null(sire_col)) as.character(ped[[sire_col]]) else NA_character_
  out$Dam_Cage <- if (!is.null(dam_col)) as.character(ped[[dam_col]]) else NA_character_
  out$Sex <- if (!is.null(sex_col)) as.character(ped[[sex_col]]) else NA_character_
  out <- out %>% filter(!is.na(ID) & trimws(ID) != "") %>% distinct(ID, .keep_all = TRUE)
  out
}

# 读取 eID-ID 对照表
read_idmap <- function(path) {
  if (is.null(path) || length(path) == 0 || is.na(path) || !file.exists(path)) {
    return(data.frame(eID = character(0), ID = character(0), stringsAsFactors = FALSE))
  }
  m <- read_excel_clean(path)
  eid_col <- find_col(m, c("^eID$", "eid", "耳标", "电子耳标", "rfid"), required = TRUE)
  id_col <- find_col(m, c("^ID$", "id", "翅号", "个体号"), required = TRUE)
  data.frame(eID = as.character(m[[eid_col]]), ID = as.character(m[[id_col]]),
             stringsAsFactors = FALSE) %>%
    filter(!is.na(eID) & !is.na(ID) & trimws(eID) != "" & trimws(ID) != "") %>%
    distinct(eID, .keep_all = TRUE)
}

# 打通：耳标(eID) -> ID(翅号) -> 系谱(父母笼号/性别)
# 无系谱或无对照表时退化：ID 取 eID 自身，无父母信息、性别未知，Has_Pedigree=FALSE
link_animals <- function(feed, bw, ped, idmap) {
  feed_ids <- unique(as.character(feed$Animal_ID))
  bw_ids <- unique(as.character(bw$Animal_ID))
  both <- intersect(feed_ids, bw_ids)
  if (is.null(idmap) || nrow(idmap) == 0 ||
      is.null(ped) || nrow(ped) == 0) {
    return(data.frame(eID = both, ID = both,
                      Sire_Cage = NA_character_, Dam_Cage = NA_character_,
                      Sex = NA_character_, Has_Pedigree = FALSE,
                      Has_Both_Data = TRUE, stringsAsFactors = FALSE))
  }
  mapped <- idmap %>% filter(eID %in% both) %>% select(eID, ID)
  mapped <- mapped %>% left_join(ped %>% select(ID, Sire_Cage, Dam_Cage, Sex), by = "ID")
  mapped <- mapped %>% mutate(
    Has_Pedigree = !is.na(Sire_Cage) | !is.na(Dam_Cage),
    Has_Both_Data = TRUE)
  mapped
}

# ============================================================
# 3. 清洗与指标计算（沿用 V4.4 成熟算法，精简参数）
# ============================================================

run_clean_v5 <- function(feed_raw, bw_raw, cfg) {
  # ---- 采食标准化 ----
  feed <- feed_raw
  names(feed) <- clean_names_simple(names(feed))
  col_animal <- find_col(feed, c("^耳标$", "耳标"), required = TRUE)
  col_t1 <- find_col(feed, c("^记录时间1$", "记录时间1"), required = TRUE)
  col_t2 <- find_col(feed, c("^记录时间2$", "记录时间2"))
  col_intake <- find_col(feed, c("^处理后采食量\\s*\\(?g\\)?$", "^处理后采食量"), required = TRUE)
  col_dur <- find_col(feed, c("^采食时长$", "采食.*时长"), required = TRUE)
  col_house <- find_col(feed, c("^栋舍$", "栋舍"))
  col_pen <- find_col(feed, c("^栏圈$", "栏圈"))
  col_group <- find_col(feed, c("^群组$", "群组"))

  feed <- feed %>%
    mutate(
      Animal_ID = as.character(.data[[col_animal]]),
      Time1 = parse_datetime_flexible(.data[[col_t1]]),
      Time2 = if (!is.null(col_t2)) parse_datetime_flexible(.data[[col_t2]]) else Time1,
      Feed_Intake_g = safe_numeric(.data[[col_intake]]),
      Feed_Duration_sec = parse_duration_seconds(.data[[col_dur]]),
      House = if (!is.null(col_house)) as.character(.data[[col_house]]) else NA_character_,
      Pen = if (!is.null(col_pen)) as.character(.data[[col_pen]]) else NA_character_,
      Group = if (!is.null(col_group)) as.character(.data[[col_group]]) else NA_character_) %>%
    filter(!is.na(Animal_ID), !is.na(Time1), trimws(Animal_ID) != "")

  # 时间 QC
  t_min <- min(feed$Time1, na.rm = TRUE); t_max <- max(feed$Time1, na.rm = TRUE)
  datetime_qc <- data.frame(Start = t_min, End = t_max, N = nrow(feed))

  # 自动时间范围：勾选后以数据实际最早/最晚时间为准，覆盖手动输入
  if (isTRUE(cfg$auto_time_range)) {
    cfg$experiment_start <- as.Date(t_min)
    cfg$training_end <- as.Date(t_max)
    cfg$training_time <- format(t_max, "%H:%M:%S")
  }

  # NA QC（全部字段）
  na_qc <- lapply(names(feed), function(col) {
    v <- feed[[col]]
    na_n <- if (inherits(v, "POSIXt") || is.numeric(v)) sum(is.na(v))
            else sum(is.na(v) | trimws(as.character(v)) == "")
    data.frame(字段 = col, 类型 = class(v)[1], 缺失数 = na_n,
               记录数 = nrow(feed),
               缺失率 = round(na_n / nrow(feed) * 100, 2), stringsAsFactors = FALSE)
  }) %>% bind_rows()

  # 数据截止时间（自动模式=最晚记录；手动未提供则不截断）
  training_end <- if (!is.na(cfg$training_end))
    as.POSIXct(paste0(as.Date(cfg$training_end), " ", cfg$training_time), tz = "Asia/Shanghai")
  else NA
  if (!is.na(training_end)) feed <- feed %>% filter(Time1 <= training_end)

  # 试验开始日期：过滤该日期之前的记录（数据起点）
  start_dt <- cfg$experiment_start
  if (!is.na(start_dt)) {
    feed_before <- feed
    feed <- feed %>% filter(as.Date(Time1) >= as.Date(start_dt))
    feed <- feed %>% arrange(Time1)
  }
  if (nrow(feed) == 0) {
    stop("数据过滤后为空：请检查『试验开始日期』是否晚于数据最早记录时间（",
         as.character(min(feed_before$Time1, na.rm = TRUE)), "）。", call. = FALSE)
  }

  # 周次
  wk <- make_experimental_week(feed$Time1, start_dt, cfg$week_length)
  feed <- bind_cols(feed, wk)

  # 周内异常值（μ±kσ）
  fo <- remove_weekly_outliers(feed, "Feed_Intake_g", "Week_Number", cfg$feed_sd)
  feed <- fo$data; feed_week_stats <- fo$stats
  feed$Feed_Intake_g[feed$QC_Outlier] <- NA_real_

  # ---- 体重标准化 ----
  bw <- bw_raw
  names(bw) <- clean_names_simple(names(bw))
  col_bw_animal <- find_col(bw, c("^耳标$", "耳标"), required = TRUE)
  col_bw_t1 <- find_col(bw, c("^记录时间1$", "记录时间1"), required = TRUE)
  col_mean <- find_col(bw, c("^平均体重\\s*\\(?kg\\)?$", "^平均体重"), required = TRUE)
  col_b1 <- find_col(bw, c("^体重1\\s*\\(?kg\\)?$", "^体重1"))
  col_b2 <- find_col(bw, c("^体重2\\s*\\(?kg\\)?$", "^体重2"))
  col_b3 <- find_col(bw, c("^体重3\\s*\\(?kg\\)?$", "^体重3"))
  col_bw_house <- find_col(bw, c("^栋舍$", "栋舍"))
  col_bw_pen <- find_col(bw, c("^栏圈$", "栏圈"))
  col_bw_group <- find_col(bw, c("^群组$", "群组"))

  bw <- bw %>%
    mutate(
      Animal_ID = as.character(.data[[col_bw_animal]]),
      Time1 = parse_datetime_flexible(.data[[col_bw_t1]]),
      BW_kg = safe_numeric(.data[[col_mean]]),
      BW1_kg = if (!is.null(col_b1)) safe_numeric(.data[[col_b1]]) else NA_real_,
      BW2_kg = if (!is.null(col_b2)) safe_numeric(.data[[col_b2]]) else NA_real_,
      BW3_kg = if (!is.null(col_b3)) safe_numeric(.data[[col_b3]]) else NA_real_,
      House = if (!is.null(col_bw_house)) as.character(.data[[col_bw_house]]) else NA_character_,
      Pen = if (!is.null(col_bw_pen)) as.character(.data[[col_bw_pen]]) else NA_character_,
      Group = if (!is.null(col_bw_group)) as.character(.data[[col_bw_group]]) else NA_character_) %>%
    filter(!is.na(Animal_ID), !is.na(Time1), !is.na(BW_kg), trimws(Animal_ID) != "")

  if (!is.na(training_end)) bw <- bw %>% filter(Time1 <= training_end)
  if (!is.na(start_dt)) bw <- bw %>% filter(as.Date(Time1) >= as.Date(start_dt))
  wk_b <- make_experimental_week(bw$Time1, start_dt, cfg$week_length)
  bw <- bind_cols(bw, wk_b)
  bo <- remove_weekly_outliers(bw, "BW_kg", "Week_Number", cfg$bw_sd)
  bw <- bo$data; bw_week_stats <- bo$stats
  bw$BW_kg[bw$QC_Outlier] <- NA_real_

  list(feed = feed, bw = bw, datetime_qc = datetime_qc, na_qc = na_qc,
       feed_week_stats = feed_week_stats, bw_week_stats = bw_week_stats)
}

# 单次采食 bout 划分（间隔阈值法）
build_bouts <- function(feed, imi_threshold_sec = 300, min_intake_g = 1) {
  feed <- feed %>%
    arrange(Animal_ID, Time1) %>%
    group_by(Animal_ID) %>%
    mutate(
      Gap_sec = as.numeric(difftime(Time1, lag(Time1), units = "secs")),
      New_Bout = is.na(Gap_sec) | Gap_sec >= imi_threshold_sec,
      Bout_ID = cumsum(New_Bout)) %>%
    group_by(Animal_ID, Bout_ID) %>%
    summarise(
      Time1 = first(Time1), Time2 = last(Time2),
      Date = as.Date(first(Time1)),
      Feed_Intake_g = sum(Feed_Intake_g, na.rm = TRUE),
      Feed_Duration_sec = sum(Feed_Duration_sec, na.rm = TRUE),
      FR_g_sec = ifelse(Feed_Duration_sec > 0, Feed_Intake_g / Feed_Duration_sec, NA_real_),
      .groups = "drop") %>%
    arrange(Animal_ID, Time1) %>%
    group_by(Animal_ID) %>%
    mutate(
      IMI_sec = as.numeric(difftime(Time1, lag(Time1), units = "secs")),
      IMI_sec = ifelse(is.na(IMI_sec), NA_real_, IMI_sec)) %>%
    ungroup() %>%
    filter(Feed_Intake_g >= min_intake_g)
  feed
}

# 个体采食行为指标
calc_individual_feeding <- function(bout) {
  bout %>%
    group_by(Animal_ID) %>%
    summarise(
      TFB = n(),                                  # 总访饲次数
      FI_g = sum(Feed_Intake_g, na.rm = TRUE),    # 总采食量 g
      AMS_g = safe_mean(Feed_Intake_g),           # 单次平均采食量
      TFD_sec = sum(Feed_Duration_sec, na.rm = TRUE), # 总采食时间
      AFBD_sec = safe_mean(Feed_Duration_sec),    # 单次平均采食时长
      IMI_sec = safe_median(IMI_sec),             # 平均餐间间隔
      FR_g_sec = safe_mean(FR_g_sec),             # 平均采食速率
      N_Days = n_distinct(Date), .groups = "drop") %>%
    mutate(TFB_Day = TFB / N_Days,
           FI_Day_g = FI_g / N_Days)
}

# 生产性能：日增重 / FCR / RFI
calc_production <- function(feed, bw, individual_feeding) {
  bw_terminal <- bw %>%
    group_by(Animal_ID) %>%
    arrange(Animal_ID, Time1) %>%
    summarise(
      IBW_kg = first(BW_kg), FBW_kg = last(BW_kg),
      IBW_Day = first(Experimental_Day), FBW_Day = last(Experimental_Day),
      N_W = n(), .groups = "drop") %>%
    mutate(
      Gain_kg = FBW_kg - IBW_kg,
      Test_Days = FBW_Day - IBW_Day,
      ADG_g = ifelse(Test_Days > 0, Gain_kg * 1000 / Test_Days, NA_real_))

  # ADG 稳健过滤：剔除 |ADG-中位数| > 4*MAD 的个体（体重记录异常/换栏干扰），避免污染 RFI/FCR
  adg_med <- median(bw_terminal$ADG_g, na.rm = TRUE)
  adg_mad <- mad(bw_terminal$ADG_g, na.rm = TRUE)
  if (is.finite(adg_med) && is.finite(adg_mad) && adg_mad > 0) {
    bw_terminal$ADG_g <- ifelse(abs(bw_terminal$ADG_g - adg_med) > 4 * adg_mad,
                                NA_real_, bw_terminal$ADG_g)
  }

  prod <- individual_feeding %>%
    left_join(bw_terminal, by = "Animal_ID") %>%
    mutate(
      ADFI_g = FI_g / N_Days,
      MBW = (IBW_kg + FBW_kg) / 2,
      FCR = ifelse(Gain_kg > 0, ADFI_g / ADG_g, NA_real_),
      Feed_Efficiency = 1 / FCR)

  # RFI：以 ADG 和 MBW 回归采食量后的残差
  rfi_dat <- prod %>% filter(is.finite(ADG_g), is.finite(MBW), is.finite(ADFI_g), n() >= 20)
  if (nrow(rfi_dat) >= 20) {
    fit <- lm(ADFI_g ~ ADG_g + MBW, data = rfi_dat)
    rfi_dat$RFI <- resid(fit)
    prod <- prod %>% left_join(rfi_dat %>% select(Animal_ID, RFI), by = "Animal_ID")
  } else {
    prod$RFI <- NA_real_
  }
  prod
}

# HFF / LFF 分组
assign_hff_lff <- function(prod, method = "median", cutoff = NULL) {
  d <- prod %>% filter(is.finite(TFB_Day))
  if (is.null(cutoff) || is.na(cutoff)) cutoff <- median(d$TFB_Day, na.rm = TRUE)
  d <- d %>% mutate(Feed_Frequency_Group = ifelse(TFB_Day >= cutoff, "HFF", "LFF"))
  attr(d, "hff_cutoff") <- cutoff
  d
}

# 组间时序相关：每日/每周访饲次数、24h 节律
calc_rhythm <- function(bout) {
  daily <- bout %>% group_by(Date) %>%
    summarise(Mean_Bouts_Per_Duck = n() / n_distinct(Animal_ID), .groups = "drop")
  weekly <- bout %>%
    mutate(Week_Number = make_experimental_week(Time1, min(Time1), 7)$Week_Number) %>%
    group_by(Week_Number, Date) %>%
    summarise(Mean_Bouts_Per_Duck = n() / n_distinct(Animal_ID), .groups = "drop")
  hourly <- bout %>%
    mutate(Hour_Block = hour(Time1),
           Week_Number = make_experimental_week(Time1, min(Time1), 7)$Week_Number) %>%
    group_by(Week_Number, Hour_Block) %>%
    summarise(Mean_Bouts_Per_Duck = n() / n_distinct(Animal_ID), .groups = "drop")
  list(daily = daily, weekly = weekly, hourly = hourly)
}

# 每周 FCR
calc_weekly_fcr <- function(feed, bw, week_length = 7) {
  feed_daily <- feed %>% group_by(Animal_ID, Date = as.Date(Time1), Week_Number) %>%
    summarise(FI_day = sum(Feed_Intake_g, na.rm = TRUE), .groups = "drop")
  bw_daily <- bw %>% group_by(Animal_ID, Date = as.Date(Time1)) %>%
    summarise(BW_day = last(BW_kg), .groups = "drop")
  merged <- feed_daily %>% left_join(bw_daily, by = c("Animal_ID", "Date")) %>%
    arrange(Animal_ID, Date) %>%
    group_by(Animal_ID) %>%
    mutate(
      BW_prev = lag(BW_day),
      Gain_day = (BW_day - BW_prev) * 1000) %>%
    ungroup() %>%
    filter(is.finite(Gain_day), is.finite(FI_day), Gain_day > 0) %>%
    group_by(Animal_ID, Week_Number) %>%
    summarise(Weekly_FCR = sum(FI_day, na.rm = TRUE) / sum(Gain_day, na.rm = TRUE),
              .groups = "drop")
  merged
}

# 创新行为指标
compute_daynight_v5 <- function(bout, day_start = "06:00", day_end = "18:00") {
  ds <- clock_to_minutes(day_start); de <- clock_to_minutes(day_end)
  is_daytime <- function(Time1) {
    mins <- hour(Time1) * 60 + minute(Time1)
    if (ds < de) mins >= ds & mins < de
    else if (ds > de) mins >= ds | mins < de
    else rep(FALSE, length(mins))
  }
  bout %>% mutate(.Is_Day = is_daytime(Time1)) %>%
    group_by(Animal_ID) %>%
    summarise(
      Day_FI_g = sum(Feed_Intake_g[.Is_Day], na.rm = TRUE),
      Night_FI_g = sum(Feed_Intake_g[!.Is_Day], na.rm = TRUE),
      Day_Bouts = sum(.Is_Day), Night_Bouts = sum(!.Is_Day),
      .groups = "drop") %>%
    mutate(
      Total_FI_g = Day_FI_g + Night_FI_g,
      Total_Bouts = Day_Bouts + Night_Bouts,
      Day_FI_Ratio = ifelse(Total_FI_g > 0, Day_FI_g / Total_FI_g, NA_real_),
      Day_Bout_Ratio = ifelse(Total_Bouts > 0, Day_Bouts / Total_Bouts, NA_real_))
}

compute_behavior_cv_v5 <- function(bout, min_bouts_single = 20, min_days_daily = 3) {
  single_cv <- bout %>%
    group_by(Animal_ID) %>%
    summarise(
      N_Bouts = n(), N_Days = n_distinct(Date),
      CV_Duration = ifelse(N_Bouts >= min_bouts_single & safe_mean(Feed_Duration_sec) > 0,
                           safe_sd(Feed_Duration_sec) / safe_mean(Feed_Duration_sec), NA_real_),
      CV_FR = ifelse(N_Bouts >= min_bouts_single & safe_mean(FR_g_sec) > 0,
                     safe_sd(FR_g_sec) / safe_mean(FR_g_sec), NA_real_),
      Robust_CV_IMI = ifelse(N_Bouts >= min_bouts_single & safe_median(IMI_sec) > 0,
                             IQR(IMI_sec, na.rm = TRUE) / safe_median(IMI_sec), NA_real_),
      .groups = "drop")
  daily_summ <- bout %>%
    group_by(Animal_ID, Date) %>%
    summarise(Daily_Bouts = n(), Daily_FI = sum(Feed_Intake_g, na.rm = TRUE),
              Daily_TFD = sum(Feed_Duration_sec, na.rm = TRUE), .groups = "drop")
  daily_cv <- daily_summ %>%
    group_by(Animal_ID) %>%
    summarise(
      N_Days_Daily = n(),
      CV_Daily_Bouts = ifelse(N_Days_Daily >= min_days_daily & safe_mean(Daily_Bouts) > 0,
                              safe_sd(Daily_Bouts) / safe_mean(Daily_Bouts), NA_real_),
      CV_Daily_FI = ifelse(N_Days_Daily >= min_days_daily & safe_mean(Daily_FI) > 0,
                           safe_sd(Daily_FI) / safe_mean(Daily_FI), NA_real_),
      CV_Daily_TFD = ifelse(N_Days_Daily >= min_days_daily & safe_mean(Daily_TFD) > 0,
                            safe_sd(Daily_TFD) / safe_mean(Daily_TFD), NA_real_),
      .groups = "drop")
  single_cv %>% left_join(daily_cv, by = "Animal_ID")
}

compute_cosinor_v5 <- function(bout, min_bouts = 20) {
  hourly_full <- bout %>% mutate(Hour_Block = hour(Time1)) %>%
    count(Animal_ID, Hour_Block, name = "n") %>%
    tidyr::complete(Animal_ID, Hour_Block = 0:23, fill = list(n = 0))
  hourly_full %>% group_by(Animal_ID) %>%
    group_modify(~ {
      t <- .x$Hour_Block; y <- .x$n
      na_result <- data.frame(Cosinor_M = NA_real_, Cosinor_A = NA_real_,
                              Peak_Hour = NA_real_, Cosinor_R2 = NA_real_, stringsAsFactors = FALSE)
      if (sum(y) < min_bouts) return(na_result)
      fit <- tryCatch(lm(y ~ cos(2 * pi * t / 24) + sin(2 * pi * t / 24)), error = function(e) NULL)
      if (is.null(fit)) return(na_result)
      coefs <- coef(fit)
      b0 <- coefs[1]
      b_cos <- if (length(coefs) >= 2 && !is.na(coefs[2])) coefs[2] else 0
      b_sin <- if (length(coefs) >= 3 && !is.na(coefs[3])) coefs[3] else 0
      A <- sqrt(b_cos^2 + b_sin^2)
      peak <- (atan2(b_sin, b_cos) * 24 / (2 * pi)) %% 24
      data.frame(Cosinor_M = b0, Cosinor_A = A, Peak_Hour = peak,
                 Cosinor_R2 = summary(fit)$r.squared, stringsAsFactors = FALSE)
    }) %>% ungroup()
}

compute_fano_v5 <- function(bout) {
  bout %>% mutate(Hour_Block = hour(Time1)) %>%
    count(Animal_ID, Date, Hour_Block, name = "n") %>%
    tidyr::complete(tidyr::nesting(Animal_ID, Date), Hour_Block = 0:23, fill = list(n = 0)) %>%
    group_by(Animal_ID) %>%
    summarise(
      N_Hour_Cells = n(),
      Mean_Bouts_Per_Hour = mean(n, na.rm = TRUE),
      Var_Bouts_Per_Hour = ifelse(length(n) > 1, var(n, na.rm = TRUE), NA_real_),
      Fano = ifelse(length(n) > 1 & mean(n, na.rm = TRUE) > 0,
                    var(n, na.rm = TRUE) / mean(n, na.rm = TRUE), NA_real_),
      .groups = "drop")
}

# ============================================================
# 4. 遗传评估与留种（V5 核心新增）
# ============================================================

# ---- 4.1 指示性状筛选：与目标性状的 Spearman 相关 ----
# target_trait 支持向量（多目标）：对每个目标分别筛选，结果取并集后按 |r| 排序
empty_indicator_tbl <- function() {
  data.frame(Indicator = character(0), Target = character(0), N = integer(0),
             Spearman_r = numeric(0), P = numeric(0), Abs_r = numeric(0),
             stringsAsFactors = FALSE)
}

select_indicator_traits <- function(prod, target_trait, min_r = 0.3, max_n = 5) {
  target_trait <- target_trait[!is.na(target_trait) & target_trait != "---"]
  if (length(target_trait) == 0) return(empty_indicator_tbl())
  # 只保留本场已测量的目标性状
  measured <- target_trait[target_trait %in% names(prod)]
  # 目标全部未测（如皮脂率/腹脂率等屠宰性状）→ 按文献遗传相关 rG 筛代理指示性状
  if (length(measured) == 0) {
    return(select_lit_indicator_traits(target_trait, prod, max_n))
  }
  cand <- c("TFB", "TFB_Day", "FI_g", "FI_Day_g", "AMS_g", "TFD_sec", "AFBD_sec",
            "IMI_sec", "FR_g_sec", "IBW_kg", "FBW_kg", "Gain_kg", "ADG_g",
            "MBW", "Day_FI_Ratio", "Day_Bout_Ratio",
            "CV_Duration", "CV_FR", "CV_Daily_Bouts", "CV_Daily_FI", "Cosinor_A", "Fano")
  # 剔除目标性状的直接函数变换（如 Feed_Efficiency=1/FCR 与 FCR 完全共线，无信息量）
  exclude_map <- c(FCR = "Feed_Efficiency", RFI = "Feed_Efficiency")
  for (tr in measured) {
    ex <- exclude_map[tr]
    if (!is.na(ex)) cand <- setdiff(cand, ex)
  }
  cand <- cand[cand %in% names(prod)]
  if (length(cand) == 0) return(empty_indicator_tbl())
  res <- lapply(measured, function(tr) {
    lapply(cand, function(v) {
      if (v == tr) return(NULL)
      tmp <- prod %>% select(x = all_of(v), y = all_of(tr)) %>%
        filter(is.finite(x), is.finite(y))
      if (nrow(tmp) >= 5) {
        t <- tryCatch(cor.test(tmp$x, tmp$y, method = "spearman", exact = FALSE),
                      error = function(e) NULL)
        if (!is.null(t)) {
          data.frame(Indicator = v, Target = tr, N = nrow(tmp),
                     Spearman_r = unname(t$estimate), P = t$p.value,
                     stringsAsFactors = FALSE)
        } else NULL
      } else NULL
    })
  })
  res <- bind_rows(res)
  if (nrow(res) == 0) return(empty_indicator_tbl())
  res %>% mutate(Abs_r = abs(Spearman_r)) %>%
    arrange(desc(Abs_r)) %>%
    # 多目标并集：同一指示性状与不同目标的 r 都保留，但按最强相关排序
    filter(Abs_r >= min_r) %>%
    group_by(Indicator) %>%
    slice_max(Abs_r, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    slice_head(n = max_n)
}

# ---- 4.2 亲缘矩阵 A（Henderson 递归法）----
# ped: data.frame(ID, Sire_Cage, Dam_Cage)；父母笼号视为 founder（无父母信息）
build_A_matrix <- function(ped) {
  all_ids <- unique(c(ped$ID, ped$Sire_Cage, ped$Dam_Cage))
  all_ids <- all_ids[!is.na(all_ids) & trimws(all_ids) != ""]
  n <- length(all_ids)
  idx <- setNames(seq_len(n), all_ids)
  # 拓扑排序：父母节点（笼号）在前，个体在后
  animal_ids <- unique(ped$ID[!is.na(ped$ID) & trimws(ped$ID) != ""])
  founder_nodes <- setdiff(all_ids, animal_ids)
  ord <- c(founder_nodes, animal_ids)
  ord <- ord[!is.na(ord)]
  ord_idx <- setNames(seq_along(ord), ord)
  si <- match(ped$Sire_Cage, ord); da <- match(ped$Dam_Cage, ord)
  A <- matrix(0, length(ord), length(ord))
  for (i in seq_along(ord)) {
    id <- ord[i]
    if (id %in% animal_ids) {
      prow <- which(ped$ID == id)[1]
      s <- si[prow]; d <- da[prow]
      if (!is.na(s) && !is.na(d)) {
        A[i, i] <- 1 + 0.5 * A[s, d]
        if (i > 1) for (j in 1:(i - 1)) A[i, j] <- A[j, i] <- 0.5 * (A[j, s] + A[j, d])
      } else if (!is.na(s) || !is.na(d)) {
        p <- if (!is.na(s)) s else d
        A[i, i] <- 1 + 0.5 * A[p, p]
        if (i > 1) for (j in 1:(i - 1)) A[i, j] <- A[j, i] <- 0.5 * A[j, p]
      } else {
        A[i, i] <- 1
      }
    } else {
      A[i, i] <- 1
    }
  }
  rownames(A) <- colnames(A) <- ord
  A
}

# ---- 4.3 PBLUP 动物模型：y = Xb + Zu + e ----
# 单性状、每个体一条记录；λ = (1-h²)/h²
# prod 需含 id_col（默认为系谱翅号列 Animal_ID）；若传入 eID，可先用 link 映射
run_pblup <- function(prod, ped, trait, h2 = 0.3, fixed_effects = c("Sex"),
                      id_col = "Animal_ID") {
  if (!id_col %in% names(prod)) stop("prod 中缺少个体 ID 列：", id_col, call. = FALSE)
  dat <- prod %>% filter(is.finite(.data[[trait]]))
  # prod 可能已带 Sex（来自系谱映射），与系谱表 Sex 重名时用后缀避免覆盖，
  # 缺失的再从系谱补齐，保证固定效应列恒为 Sex
  if ("Sex" %in% names(dat)) {
    dat <- dat %>% left_join(ped %>% select(ID, Sex), by = setNames("ID", id_col),
                             suffix = c("", ".ped"))
    if ("Sex.ped" %in% names(dat)) {
      dat$Sex <- ifelse(is.na(dat$Sex), dat$Sex.ped, dat$Sex)
      dat$Sex.ped <- NULL
    }
  } else {
    dat <- dat %>% left_join(ped %>% select(ID, Sex), by = setNames("ID", id_col))
  }
  if (nrow(dat) < 10) stop("PBLUP 需要至少 10 个有表型的个体。", call. = FALSE)

  A <- build_A_matrix(ped)
  animals <- intersect(dat[[id_col]], rownames(A))
  dat <- dat %>% filter(.data[[id_col]] %in% animals)
  if (nrow(dat) < 10) stop("PBLUP 需要至少 10 个同时有表型和系谱的个体。", call. = FALSE)

  y <- dat[[trait]]
  n <- length(y)
  # 固定效应设计阵
  fe_cols <- intersect(fixed_effects, names(dat))
  if (length(fe_cols) > 0) {
    fmla <- as.formula(paste("~", paste(fe_cols, collapse = " + ")))
    X <- model.matrix(fmla, data = dat)
  } else {
    X <- matrix(1, n, 1); colnames(X) <- "(Intercept)"
  }
  # 随机效应设计阵（个体）
  Z <- matrix(0, n, length(animals))
  for (i in seq_along(animals)) Z[dat[[id_col]] == animals[i], i] <- 1
  A_sub <- A[animals, animals]
  Ai <- tryCatch(solve(A_sub), error = function(e) {
    MASS::ginv(as.matrix(A_sub))
  })
  lam <- (1 - h2) / h2
  # MME: [X'X  X'Z; Z'X  Z'Z + λA⁻¹] [b;u] = [X'y; Z'y]
  LHS <- rbind(cbind(crossprod(X), crossprod(X, Z)),
               cbind(crossprod(Z, X), crossprod(Z) + lam * Ai))
  RHS <- rbind(crossprod(X, y), crossprod(Z, y))
  sol <- tryCatch(solve(LHS, RHS), error = function(e) NULL)
  if (is.null(sol)) stop("MME 求解失败，请检查固定效应与亲缘矩阵。", call. = FALSE)
  b <- sol[seq_len(ncol(X)), 1]
  u <- sol[(ncol(X) + 1):length(sol), 1]
  names(u) <- animals
  out <- data.frame(Animal_ID = animals, EBV = u, stringsAsFactors = FALSE)
  out <- out %>% left_join(dat %>% select(Animal_ID = all_of(id_col),
                                          !!sym(trait), Sex), by = "Animal_ID")
  list(result = out, h2 = h2, lambda = lam, n_animals = length(animals),
       fixed = fe_cols, A = A)
}

# ---- 4.3b 多性状 PBLUP：对每个勾选性状单独跑动物模型，合并 EBV ----
# traits: 参与指数的性状向量；h2_by_trait: 各性状的遗传力（命名向量，缺省用默认值）
run_pblup_multi <- function(prod, ped, traits, h2_by_trait = NULL,
                            fixed_effects = c("Sex"), id_col = "Animal_ID") {
  if (length(traits) == 0) stop("未选择参与指数的性状。", call. = FALSE)
  default_h2 <- 0.3
  ebv_list <- lapply(traits, function(tr) {
    if (!tr %in% names(prod)) return(NULL)
    h2i <- if (!is.null(h2_by_trait) && tr %in% names(h2_by_trait)) h2_by_trait[[tr]] else default_h2
    pb <- tryCatch(
      run_pblup(prod, ped, tr, h2 = h2i, fixed_effects = fixed_effects, id_col = id_col),
      error = function(e) NULL)
    if (is.null(pb)) return(NULL)
    pb$result %>% mutate(Trait = tr, H2_Used = h2i)
  })
  ebv <- bind_rows(ebv_list)
  if (nrow(ebv) == 0) stop("所有勾选性状的 PBLUP 均失败。", call. = FALSE)
  list(ebv = ebv,
       n_animals = length(unique(ebv$Animal_ID)),
       traits = unique(ebv$Trait))
}

# ---- 4.3c 遗传力实测（REML 方差组分估计）----
# 模型：y = Xb + Zu + e，Var(u) = A·σ²a，Var(e) = I·σ²e，h² = σ²a/(σ²a+σ²e)
# 采用直接最大化 REML 对数似然（对数参数化保证方差为正，无需额外包）：
#   ℓ_REML ∝ -0.5[ log|V| + log|X'V⁻¹X| + y'Py ]，V = ZAZ'σ²a + Iσ²e，
#   P = V⁻¹ - V⁻¹X(X'V⁻¹X)⁻¹X'V⁻¹；用 optim(BFGS) 求 -ℓ 最小值。
estimate_h2_reml <- function(prod, ped, trait, fixed_effects = c("Sex"),
                             id_col = "Animal_ID") {
  if (!id_col %in% names(prod)) stop("prod 中缺少个体 ID 列：", id_col, call. = FALSE)
  dat <- prod %>% filter(is.finite(.data[[trait]]))
  if (nrow(dat) < 10) stop("遗传力估计需要至少 10 个有表型的个体。", call. = FALSE)
  A <- build_A_matrix(ped)
  animals <- intersect(dat[[id_col]], rownames(A))
  dat <- dat %>% filter(.data[[id_col]] %in% animals)
  if (nrow(dat) < 10) stop("遗传力估计需要至少 10 个同时有表型和系谱的个体。", call. = FALSE)

  y <- as.numeric(dat[[trait]])
  n <- length(y)
  fe_cols <- intersect(fixed_effects, names(dat))
  if (length(fe_cols) > 0) {
    fmla <- as.formula(paste("~", paste(fe_cols, collapse = " + ")))
    X <- model.matrix(fmla, data = dat)
  } else {
    X <- matrix(1, n, 1); colnames(X) <- "(Intercept)"
  }
  Z <- matrix(0, n, length(animals))
  for (i in seq_along(animals)) Z[dat[[id_col]] == animals[i], i] <- 1
  A_sub <- A[animals, animals]
  q <- length(animals)
  p <- ncol(X)
  var_y <- var(y)

  # 预计算与 y、X 无关的量
  ZA <- Z %*% A_sub                 # n × q，供 V 计算

  # 返回 -REML 对数似然（最小化）
  reml_nll <- function(par) {
    s2a <- exp(par[1]); s2e <- exp(par[2])
    V <- ZA %*% t(Z) * s2a + diag(s2e, n)             # ZAZ'σ²a + Iσ²e
    Vi <- tryCatch(solve(V), error = function(e) MASS::ginv(as.matrix(V)))
    M <- crossprod(X, Vi %*% X)
    logdetV <- as.numeric(determinant(V, logarithm = TRUE)$modulus)
    logdetM <- as.numeric(determinant(M, logarithm = TRUE)$modulus)
    ytVi <- crossprod(y, Vi)
    ytPy <- as.numeric(ytVi %*% y) -
      as.numeric(ytVi %*% X %*% solve(M) %*% crossprod(X, Vi %*% y))
    0.5 * (logdetV + logdetM + ytPy)
  }
  # 初始值：表型方差的 30%/70%（对数参数）
  par0 <- c(log(0.3 * var_y), log(0.7 * var_y))
  opt <- tryCatch(
    optim(par0, reml_nll, method = "BFGS",
          control = list(maxit = 500, reltol = 1e-8)),
    error = function(e) NULL)
  if (is.null(opt) || opt$convergence > 1) {
    # BFGS 失败回退 Nelder-Mead
    opt <- tryCatch(
      optim(par0, reml_nll, method = "Nelder-Mead",
            control = list(maxit = 1000, reltol = 1e-8)),
      error = function(e) NULL)
  }
  if (is.null(opt)) stop("REML 优化失败。", call. = FALSE)
  s2a <- exp(opt$par[1]); s2e <- exp(opt$par[2])
  h2 <- s2a / (s2a + s2e)
  list(trait = trait, h2 = h2, sigma2_a = s2a, sigma2_e = s2e,
       n_animals = q, n_obs = n, n_iter = opt$counts[1], converged = opt$convergence == 0)
}

# 多性状遗传力实测：对每个勾选性状跑 REML，返回汇总表
estimate_h2_multi <- function(prod, ped, traits, fixed_effects = c("Sex"),
                              id_col = "Animal_ID") {
  if (length(traits) == 0) stop("未选择需要估计遗传力的性状。", call. = FALSE)
  res <- lapply(traits, function(tr) {
    if (!tr %in% names(prod)) return(NULL)
    tryCatch(estimate_h2_reml(prod, ped, tr, fixed_effects, id_col),
             error = function(e) NULL)
  })
  res <- Filter(Negate(is.null), res)
  if (length(res) == 0) stop("所有性状的遗传力估计均失败。", call. = FALSE)
  out <- data.frame(
    Trait = vapply(res, `[[`, "", "trait"),
    h2 = vapply(res, `[[`, 0, "h2"),
    sigma2_a = vapply(res, `[[`, 0, "sigma2_a"),
    sigma2_e = vapply(res, `[[`, 0, "sigma2_e"),
    N_Animals = vapply(res, `[[`, 0L, "n_animals"),
    N_Obs = vapply(res, `[[`, 0L, "n_obs"),
    N_Iter = vapply(res, `[[`, 0L, "n_iter"),
    Converged = vapply(res, `[[`, FALSE, "converged"),
    stringsAsFactors = FALSE)
  out
}

# ---- 4.4 文献参数模式：选择指数（无系谱场景）----
# 内置文献遗传参数（来源见遗传参数表；h² 为该性状在对应群体的代表值）
# 注：皮脂率/腹脂率/胸肌率为屠宰性状，本场 prod 无此列时不会进入指数计算，
#     仅作为『目标性状参考』展示，供用户理解遗传力水平。
literature_params <- function() {
  data.frame(
    Trait = c("FCR", "RFI", "ADG_g", "FBW_kg", "FI_Day_g", "FR_g_sec",
              "TFB_Day", "AMS_g", "ADFI_g", "IBW_kg",
              "SkinFat_Rate", "AbFat_Rate", "BMP", "AbFat_Wt", "SkinFat_Wt"),
    h2 = c(0.29, 0.41, 0.38, 0.39, 0.31, 0.54, 0.54, 0.54, 0.31, 0.39,
           0.55, 0.56, 0.38, 0.63, 0.60),
    Breed = c("北京鸭 Pekin duck", "北京鸭 Pekin duck", "北京鸭 Pekin duck",
              "北京鸭 Pekin duck", "北京鸭 Pekin duck", "北京鸭 Pekin duck",
              "北京鸭 Pekin duck", "北京鸭 Pekin duck", "北京鸭 Pekin duck",
              "北京鸭 Pekin duck",
              "北京鸭×绿头鸭 F2", "北京鸭×绿头鸭 F2", "北京鸭×绿头鸭 F2",
              "北京鸭×绿头鸭 F2", "北京鸭×绿头鸭 F2"),
    Source = c("Li et al. 2020, Poultry Science 99:2375-2384",
               "Zhang et al. 2017, AJAS (doi:10.5713/ajas.15.0577)",
               "Zhang et al. 2017, AJAS (doi:10.5713/ajas.15.0577)",
               "Zhang et al. 2017, AJAS (doi:10.5713/ajas.15.0577)",
               "Li et al. 2020, Poultry Science 99:2375-2384",
               "Li 2020 0.54 / Chapuis 2024, Animal 18:101234 (0.59)",
               "Li 2020 0.54 / Chapuis 2024, Animal 18:101234 (0.56)",
               "Li 2020 0.54-0.62 / Chapuis 2024 0.62",
               "Li 2020 0.31 / Chapuis 2024 0.49",
               "参考 Zhang 2017 BW42 (0.39)",
               "Cai et al. 2023, J Anim Sci Biotechnol 14:88 (doi:10.1186/s40104-023-00875-8)",
               "Cai et al. 2023, J Anim Sci Biotechnol 14:88",
               "Cai et al. 2023, J Anim Sci Biotechnol 14:88",
               "Cai et al. 2023, J Anim Sci Biotechnol 14:88",
               "Cai et al. 2023, J Anim Sci Biotechnol 14:88"),
    Method_N = c("动物模型/REML，5,594只(3-6周龄自动采食)",
                 "REML sire-dam 模型，2,020只",
                 "REML sire-dam 模型，2,020只",
                 "REML sire-dam 模型，2,020只(42日龄体重)",
                 "动物模型/REML，5,594只",
                 "动物模型/REML，5,594只",
                 "动物模型/REML，5,594只",
                 "动物模型/REML，5,594只",
                 "动物模型/REML，5,594只",
                 "动物模型/REML，2,020只(参考)",
                 "REML/动物模型，988只(8周龄)",
                 "REML/动物模型，988只(8周龄)",
                 "REML/动物模型，988只(8周龄)",
                 "REML/动物模型，988只(8周龄)",
                 "REML/动物模型，988只(8周龄)"),
    stringsAsFactors = FALSE)
}

# 文献遗传相关 rG 参考表（用于：目标性状本场未测时，按文献 rG 筛选代理指示性状）
# 性状名为软件内部统一命名；Species 标注物种，鸡的数据仅作替代证据（rG 不可直接等同鸭）
literature_corr <- function() {
  data.frame(
    Target = c("RFI", "RFI", "FCR", "FCR", "ADG_g", "ADG_g", "FBW_kg", "FBW_kg",
               "FI_Day_g", "TFB_Day", "TFB_Day", "AMS_g", "AFBD_sec", "TFD_sec",
               "SkinFat_Rate", "SkinFat_Rate", "AbFat_Rate", "AbFat_Rate"),
    Indicator = c("FI_Day_g", "FCR", "ADG_g", "FI_Day_g", "FBW_kg", "FI_Day_g",
                  "ADG_g", "FCR", "RFI", "AMS_g", "AFBD_sec", "AFBD_sec",
                  "TFD_sec", "AFBD_sec", "RFI", "FCR", "RFI", "FCR"),
    Genetic_r = c(0.77, 0.54, -0.80, 0.54, 0.92, 0.49, 0.92, -0.64,
                  0.77, -0.91, -0.65, 0.73, 0.60, 0.60, 0.58, 0.51, 0.58, 0.51),
    SE = c(0.17, 0.05, 0.11, 0.05, 0.08, 0.15, 0.08, 0.14,
           0.17, 0.05, 0.13, 0.11, 0.11, 0.11, 0.159, 0.17, 0.159, 0.17),
    Species = c(rep("北京鸭 Pekin duck", 10),
                "北京鸭 Pekin duck", "北京鸭 Pekin duck", "北京鸭 Pekin duck",
                "北京鸭 Pekin duck",
                rep("肉鸡 Broiler（替代证据）", 4)),
    Source = c(rep("Zhang et al. 2017, AJAS", 8),
               "Zhang et al. 2017, AJAS",
               rep("Li et al. 2021, BMC Genomics 22:334", 5),
               rep("Chen et al. 2021, Poultry Science 100:461（鸡，替代证据）", 4)),
    stringsAsFactors = FALSE)
}

# 目标性状全部本场未测（如皮脂率/腹脂率/胸肌率）时：
# 按文献 rG 绝对值从高到低，筛出与本场可测性状相关的代理指示性状
select_lit_indicator_traits <- function(targets, prod, max_n = 5) {
  targets <- targets[!is.na(targets) & targets != "---"]
  if (length(targets) == 0) return(empty_indicator_tbl())
  lc <- literature_corr()
  rows <- lapply(targets, function(tr) {
    sub <- lc %>% filter(Target == tr, Indicator %in% names(prod))
    if (nrow(sub) == 0) return(NULL)
    sub %>% mutate(Target = tr)
  })
  res <- bind_rows(rows)
  if (nrow(res) == 0) return(empty_indicator_tbl())
  res %>%
    arrange(desc(abs(Genetic_r))) %>%
    group_by(Indicator) %>%
    slice_max(abs(Genetic_r), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    slice_head(n = max_n) %>%
    mutate(N = NA_real_, Spearman_r = NA_real_, P = NA_real_,
           Abs_r = abs(Genetic_r),
           rG_Source = paste0(Source, "；", Species))
}


# traits_subset：只对用户勾选的性状计算（默认全部可匹配性状）
# 目标性状本场未测时（如皮脂率），不依赖目标性状，直接用勾选性状计算
run_literature_index <- function(prod, target_trait, lit, weights = NULL, traits_subset = NULL) {
  dat <- prod
  # 多目标支持：若任一目标性状本场可测，用第一个可测目标过滤有效个体
  targets <- target_trait[target_trait %in% names(prod)]
  if (length(targets) > 0) dat <- prod %>% filter(is.finite(.data[[targets[1]]]))
  traits <- lit$Trait[lit$Trait %in% names(dat)]
  if (!is.null(traits_subset)) traits <- intersect(traits_subset, traits)
  if (length(traits) == 0) stop("文献参数表与数据无可匹配性状。", call. = FALSE)
  if (is.null(weights)) weights <- setNames(rep(1 / length(traits), length(traits)), traits)
  ebv_list <- lapply(traits, function(tr) {
    h2 <- lit$h2[lit$Trait == tr]
    p <- dat[[tr]]
    m <- mean(p, na.rm = TRUE); s <- sd(p, na.rm = TRUE)
    # 注意：不能用 ifelse(s > 0, ...)——s>0 是标量，ifelse 会把整个向量坍缩成第一个值
    ebv <- if (is.finite(s) && s > 0) h2 * (p - m) / s else rep(NA_real_, length(p))
    data.frame(Animal_ID = dat$Animal_ID, Trait = tr,
               EBV = ebv,
               stringsAsFactors = FALSE)
  })
  ebv <- bind_rows(ebv_list)
  idx <- ebv %>% group_by(Animal_ID) %>%
    summarise(Index = sum(EBV * weights[first(Trait)], na.rm = TRUE),
              .groups = "drop")
  list(index = idx, ebv = ebv, h2_used = lit %>% filter(Trait %in% traits))
}

# ---- 4.5 选择指数与留种名单 ----
# 性状「期望方向」：Dir = -1 越低越好，+1 越高越好。
# 选择指数按方向加权 Index = Σ(EBV × Dir × 权重)，权重只需填正数（重要性），
# 避免 FCR / RFI / 采食量这类「越低越好」的性状被当成正向指标而选反。
trait_meta <- function() {
  data.frame(
    Trait = c("FCR", "RFI", "ADFI_g", "FI_Day_g", "TFD_sec", "AFBD_sec", "IMI_sec",
              "FR_g_sec", "TFB_Day", "AMS_g", "IBW_kg", "FBW_kg", "Gain_kg", "ADG_g", "MBW",
              "Day_FI_Ratio", "Day_Bout_Ratio", "CV_Duration", "CV_FR",
              "CV_Daily_Bouts", "CV_Daily_FI", "Cosinor_A", "Fano", "Feed_Efficiency",
              "SkinFat_Rate", "AbFat_Rate", "BMP"),
    Label = c("饲料转化比", "剩余采食量", "平均日采食量", "日采食量", "总采食时长", "单次采食时长", "餐间间隔",
              "采食速率", "日访饲次数", "单次采食量", "初重", "终重", "总增重", "日增重", "代谢体重",
              "白天采食占比", "白天访饲占比", "采食时长变异", "采食速率变异",
              "日访饲次数变异", "日采食量变异", "节律振幅", "Fano指数", "饲料效率",
              "皮脂率", "腹脂率", "胸肌率"),
    Dir = c(-1, -1, -1, -1, -1, -1, -1,
            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, -1, -1,
            -1, -1, 1, -1, 1,
            1, -1, 1),
    stringsAsFactors = FALSE)
}

trait_dir <- function(traits) {
  m <- trait_meta(); d <- setNames(m$Dir, m$Trait)
  out <- unname(d[traits]); out[is.na(out)] <- 1; out
}

trait_label <- function(traits) {
  m <- trait_meta(); l <- setNames(m$Label, m$Trait)
  out <- unname(l[traits]); out[is.na(out)] <- traits[is.na(out)]; out
}

# 选择指数可选性状（UI 分组 + 「指示性状→指数」共用同一份，避免两边不一致）
INDEX_TRAIT_CHOICES <- list(
  "效率与增重" = c("FCR", "RFI", "ADG_g", "FBW_kg", "Gain_kg", "IBW_kg", "MBW"),
  "采食行为" = c("FI_Day_g", "ADFI_g", "TFB_Day", "AMS_g", "FR_g_sec",
                  "TFD_sec", "AFBD_sec", "IMI_sec"),
  "节律与稳定性" = c("Day_FI_Ratio", "Day_Bout_Ratio", "CV_Duration", "CV_FR",
                      "CV_Daily_Bouts", "CV_Daily_FI", "Cosinor_A", "Fano"))
INDEX_TRAIT_VALUES <- unlist(INDEX_TRAIT_CHOICES, use.names = FALSE)

selection_index <- function(ebv_result, weights = NULL) {
  # ebv_result: data.frame(Animal_ID, Trait, EBV) 或单性状
  # 多性状时 EBV 先按性状标准化（除以各自 EBV 标准差），
  # 消除量纲/方差差异后按权重×方向加权，权重才是"真权重"。
  if ("Trait" %in% names(ebv_result)) {
    traits <- unique(as.character(ebv_result$Trait))
    if (is.null(weights)) weights <- setNames(rep(1, length(traits)), traits)
    wdf <- data.frame(Trait = traits, W = as.numeric(weights[traits]), stringsAsFactors = FALSE)
    wdf$W[is.na(wdf$W)] <- 0
    if (all(wdf$W == 0)) wdf$W <- 1   # 权重全为 0 时退化为等权，避免指数恒为 0
    ddf <- data.frame(Trait = traits, Dir = trait_dir(traits), stringsAsFactors = FALSE)
    ebv_result <- ebv_result %>%
      mutate(Trait = as.character(Trait)) %>%
      left_join(ddf, by = "Trait") %>% left_join(wdf, by = "Trait") %>%
      mutate(Dir = ifelse(is.na(Dir), 1, Dir),
             W = ifelse(is.na(W), 0, W))
    ebv_result <- ebv_result %>%
      group_by(Trait) %>%
      mutate(EBV_sd = sd(EBV, na.rm = TRUE),
             EBV_z = ifelse(is.finite(EBV_sd) & EBV_sd > 0,
                            EBV / EBV_sd, 0)) %>%
      ungroup() %>%
      mutate(Contrib = EBV_z * Dir * W)
    idx <- ebv_result %>%
      group_by(Animal_ID) %>%
      summarise(Index = sum(Contrib, na.rm = TRUE), .groups = "drop")
  } else {
    idx <- ebv_result %>% mutate(Index = EBV) %>% select(Animal_ID, Index)
  }
  idx %>% mutate(Rank = row_number(desc(Index)))
}

make_retention_list <- function(index_df, prod, retention_ratio = 0.3,
                                sex_balance = FALSE, key_col = "Animal_ID",
                                prod_key = key_col,
                                phenotype_cols = c("TFB_Day", "FI_Day_g", "ADG_g", "FCR", "RFI", "Sex")) {
  idx <- index_df %>% arrange(desc(Index))
  n_total <- nrow(idx)
  n_keep <- max(1, round(n_total * retention_ratio))
  pheno_cols <- intersect(phenotype_cols, names(prod))
  if (length(pheno_cols) > 0) {
    prod_sub <- prod %>% select(all_of(c(prod_key, pheno_cols))) %>%
      distinct(.data[[prod_key]], .keep_all = TRUE)
    names(prod_sub)[1] <- key_col
    idx <- idx %>% left_join(prod_sub, by = key_col)
  }
  sexes <- if ("Sex" %in% names(idx)) unique(idx$Sex[!is.na(idx$Sex)]) else character(0)
  if (sex_balance && length(sexes) == 2) {
    per <- ceiling(n_keep / 2)
    keep <- idx %>% filter(Sex %in% sexes) %>%
      group_by(Sex) %>% arrange(desc(Index), .by_group = TRUE) %>%
      slice_head(n = per) %>% ungroup() %>%
      arrange(desc(Index)) %>% slice_head(n = n_keep)
    # 某一性别候选不足时，用整体排名补足到 n_keep，避免少留
    if (nrow(keep) < n_keep) {
      fill <- idx %>% filter(!.data[[key_col]] %in% keep[[key_col]]) %>%
        slice_head(n = n_keep - nrow(keep))
      keep <- bind_rows(keep, fill)
    }
  } else {
    keep <- idx %>% slice_head(n = n_keep)
  }
  not <- idx %>% filter(!.data[[key_col]] %in% keep[[key_col]])
  keep$Retained <- TRUE
  not$Retained <- FALSE
  list(retained = keep, not_retained = not, n_keep = nrow(keep), n_target = n_keep,
       total = n_total, ratio = retention_ratio,
       sex_balanced = sex_balance && length(sexes) == 2)
}

# ---- 4.6 留种决策主流程（PBLUP / 文献参数 → 选择指数 → 留种名单）----
# 供 run_pipeline_v5 与 Shiny 服务器共用，避免两处逻辑各自漂移。
# cfg 需含：target_trait, index_traits, weights, h2_by_trait, use_pedigree,
#           use_reml_h2, fixed_effects, retention_ratio, sex_balance
# 返回 list(genetic, retention, indicator_traits, key_metrics, index_traits, warnings, error)
run_retention_module <- function(prod, link, cfg) {
  warnings <- character(0)
  target <- cfg$target_trait %||% "FCR"
  target <- target[!is.na(target) & target != "---"]
  req_traits <- cfg$index_traits %||% target

  not_in_prod <- setdiff(req_traits, names(prod))
  if (length(not_in_prod) > 0) {
    warnings <- c(warnings, paste0("性状 [", paste(not_in_prod, collapse = ", "),
                                   "] 本场未测量（多为屠宰性状），已从指数中剔除。"))
  }
  index_traits <- intersect(req_traits, names(prod))
  ind_traits <- select_indicator_traits(prod, target,
                                        cfg$indicator_min_r %||% 0.3,
                                        cfg$indicator_max_n %||% 5)
  if (length(index_traits) == 0) {
    return(list(genetic = NULL, retention = NULL, indicator_traits = ind_traits,
                key_metrics = NULL, index_traits = index_traits, warnings = warnings,
                error = "参与指数的性状均无本场数据，无法构建选择指数。"))
  }

  use_ped <- isTRUE(cfg$use_pedigree) && nrow(link %>% filter(Has_Pedigree)) >= 20
  if (isTRUE(cfg$use_pedigree) && !use_ped) {
    warnings <- c(warnings, paste0("系谱可用个体不足 20（当前 ",
                                   nrow(link %>% filter(Has_Pedigree)),
                                   " 只），已自动切换为『文献参数』路线。"))
  }
  genetic <- list(mode = if (use_ped) "PBLUP" else "文献参数")
  retention <- NULL; idx <- NULL
  ratio <- cfg$retention_ratio %||% 0.3
  sex_balance <- isTRUE(cfg$sex_balance)

  if (use_ped) {
    ped_animals <- link %>% filter(Has_Pedigree) %>% select(ID, Sire_Cage, Dam_Cage, Sex)
    prod_gen <- prod %>%
      left_join(link %>% select(eID, ID) %>% distinct(eID, .keep_all = TRUE),
                by = c("Animal_ID" = "eID")) %>%
      mutate(Wing_ID = ifelse(is.na(ID), Animal_ID, ID))
    h2_by_trait <- cfg$h2_by_trait %||% NULL
    h2_est <- NULL
    if (isTRUE(cfg$use_reml_h2)) {
      h2_est <- tryCatch(
        estimate_h2_multi(prod_gen, ped_animals, index_traits,
                          fixed_effects = cfg$fixed_effects, id_col = "Wing_ID"),
        error = function(e) NULL)
      if (!is.null(h2_est) && nrow(h2_est) > 0) {
        h2_by_trait <- setNames(as.numeric(h2_est$h2), h2_est$Trait)
      }
    }
    genetic$h2_est <- h2_est
    genetic$h2_used <- h2_by_trait
    pblup <- tryCatch(run_pblup_multi(prod_gen, ped_animals, index_traits,
                                      h2_by_trait = h2_by_trait,
                                      fixed_effects = cfg$fixed_effects,
                                      id_col = "Wing_ID"),
                      error = function(e) NULL)
    genetic$pblup <- pblup
    if (!is.null(pblup)) {
      ebv <- pblup$ebv %>%
        left_join(link %>% select(ID, eID) %>% distinct(ID, .keep_all = TRUE),
                  by = c("Animal_ID" = "ID"))
      idx <- selection_index(ebv, cfg$weights)
      idx <- idx %>%
        left_join(link %>% select(eID, ID) %>% distinct(ID, .keep_all = TRUE),
                  by = c("Animal_ID" = "ID"))
      retention <- make_retention_list(idx, prod_gen, ratio, sex_balance,
                                       key_col = "Animal_ID", prod_key = "Wing_ID")
      genetic$ebv_tbl <- ebv
    } else {
      warnings <- c(warnings, "PBLUP 求解失败，未生成留种名单。")
    }
  } else {
    lit <- literature_params()
    li <- run_literature_index(prod, target, lit, cfg$weights, traits_subset = index_traits)
    genetic$literature <- li
    idx <- selection_index(li$ebv, cfg$weights)
    retention <- make_retention_list(idx, prod, ratio, sex_balance)
    genetic$ebv_tbl <- li$ebv
  }
  genetic$index <- idx
  genetic$retention <- retention
  genetic$index_traits <- index_traits

  targets_meas <- target[target %in% names(prod)]
  n_valid <- if (length(targets_meas) > 0) {
    nrow(prod %>% filter(is.finite(.data[[targets_meas[1]]])))
  } else if (length(index_traits) > 0) {
    nrow(prod %>% filter(is.finite(.data[[index_traits[1]]])))
  } else 0
  h2_txt <- if (!is.null(genetic$pblup)) {
    h2_used <- genetic$h2_used %||% cfg$h2_by_trait
    src <- if (!is.null(genetic$h2_est)) "实测REML" else "文献/输入"
    if (!is.null(h2_used) && length(h2_used) > 0)
      paste0("[", src, "] ", paste(names(h2_used), "=",
                                   sprintf("%.2f", as.numeric(h2_used)), collapse = "; "))
    else "-"
  } else if (!is.null(genetic$literature)) {
    paste0("[文献] ", paste(genetic$literature$h2_used$Trait, "=",
                             sprintf("%.2f", genetic$literature$h2_used$h2), collapse = "; "))
  } else "-"
  key_metrics <- data.frame(
    指标 = c("目标性状", "参与指数性状", "可用个体数", "系谱覆盖", "评估模式",
             "指示性状数", "遗传力(h²)", "留种数", "留种比例"),
    数值 = c(paste(target, collapse = " + "),
             paste(index_traits, collapse = " + "),
             n_valid,
             paste0(nrow(link %>% filter(Has_Pedigree)), " / ", nrow(link)),
             genetic$mode,
             nrow(ind_traits),
             h2_txt,
             if (!is.null(retention)) retention$n_keep else "-",
             if (!is.null(retention)) sprintf("%.0f%%", retention$ratio * 100) else "-"),
    stringsAsFactors = FALSE)

  list(genetic = genetic, retention = retention, indicator_traits = ind_traits,
       key_metrics = key_metrics, index_traits = index_traits,
       warnings = warnings, error = NULL)
}

# ============================================================
# 4.7 AI 智能选种探索（模块⑥ 辅助层）
# 由 Duck_AI_Module（ML_00~ML_07）移植，数据直连 run_pipeline_v5 的 prod
# （不走 Excel 中转）；权重全部可调，默认沿用原模块写死参数：
#   目标权重 0.33/0.33/0.34（Trait_score 融合）
#   最终评分 0.7/0.3（Trait_score × StabilityScore）
# 本模块不进留种主链路，仅作「指示性状」的机器学习证据补充。
# ============================================================

# 行为特征分组：与 V5 prod 列名核对后的完整名单
# （原 Duck_AI_Module 的 Day_Avg_Meal/Night_Avg_Meal 等 V4.4 专有列
#   在 V5 中对应 AMS_g/AFBD_sec，已替换；不存在的列自动跳过）
ml_feature_groups_v5 <- function() {
  list(
    "基础采食" = c("TFB", "TFB_Day", "FI_g", "FI_Day_g", "AMS_g"),
    "单次采食" = c("TFD_sec", "AFBD_sec", "IMI_sec", "FR_g_sec"),
    "昼夜分配" = c("Day_FI_g", "Night_FI_g", "Day_Bouts", "Night_Bouts",
                   "Total_FI_g", "Total_Bouts", "Day_FI_Ratio", "Day_Bout_Ratio"),
    "行为稳定性" = c("CV_Duration", "CV_FR", "Robust_CV_IMI",
                    "CV_Daily_Bouts", "CV_Daily_FI", "CV_Daily_TFD"),
    "节律" = c("Cosinor_M", "Cosinor_A", "Cosinor_R2", "Peak_Hour"),
    "聚集度" = c("Mean_Bouts_Per_Hour", "Var_Bouts_Per_Hour", "Fano")
  )
}

ml_features_v5 <- function(prod) {
  feats <- unlist(ml_feature_groups_v5(), use.names = FALSE)
  feats[feats %in% names(prod)]
}

# ML_00：K-means 采食模式聚类（Silhouette 定 K，TFB 最高簇判 HFF）
# 输出同时给出模块③的 TFB 中位数分组作对照——两种方法口径不同，
# 模块③分组用于组间分析主链路，此处聚类仅作探索对比。
ml_cluster <- function(prod, seed = 123) {
  cluster_features <- c("TFB", "FI_g", "AMS_g", "IMI_sec", "Robust_CV_IMI", "Fano")
  cluster_features <- cluster_features[cluster_features %in% names(prod)]
  if (length(cluster_features) < 3) {
    return(list(error = "用于聚类的行为指标不足（至少 3 个）。"))
  }
  d <- prod %>% select(Animal_ID, all_of(cluster_features))
  for (i in seq_along(cluster_features)) {
    col <- cluster_features[i]
    if (any(is.na(d[[col]]))) d[[col]][is.na(d[[col]])] <- median(d[[col]], na.rm = TRUE)
  }
  X <- d %>% select(-Animal_ID)
  # 方向统一：单次采食量 AMS_g 反向（「次数多-单次量小」与频率方向一致）
  X_direction <- X
  if ("AMS_g" %in% names(X_direction)) X_direction$AMS_g <- -X_direction$AMS_g
  X_scaled <- scale(X_direction)
  sil_results <- data.frame(K = 2:6, Silhouette = NA_real_)
  for (k in 2:6) {
    set.seed(seed)
    km <- kmeans(X_scaled, centers = k, nstart = 25)
    sil <- silhouette(km$cluster, dist(X_scaled))
    sil_results$Silhouette[sil_results$K == k] <- mean(sil[, 3])
  }
  best_k <- sil_results$K[which.max(sil_results$Silhouette)]
  set.seed(seed)
  km <- kmeans(X_scaled, centers = best_k, nstart = 50)
  out <- d %>% mutate(Cluster = km$cluster)
  # 回连生产性状供 HFF/LFF 性能比较
  out <- out %>% left_join(
    prod %>% select(Animal_ID, ADG_g, FCR, RFI, TFB_Day),
    by = "Animal_ID")
  summ <- out %>% group_by(Cluster) %>%
    summarise(N = n(), Mean_TFB = safe_mean(TFB), Mean_FI = safe_mean(FI_g),
              Mean_AMS = safe_mean(AMS_g), .groups = "drop")
  hff_cluster <- summ$Cluster[which.max(summ$Mean_TFB)]
  out$Feeding_Pattern <- ifelse(out$Cluster == hff_cluster, "HFF", "LFF")
  if ("Feed_Frequency_Group" %in% names(prod)) {
    out <- out %>% left_join(prod %>% select(Animal_ID, Feed_Frequency_Group),
                             by = "Animal_ID")
  } else {
    out$Feed_Frequency_Group <- NA_character_
  }
  compare <- out %>% group_by(Feeding_Pattern) %>%
    summarise(N = n(), ADG_mean = safe_mean(ADG_g), FCR_mean = safe_mean(FCR),
              RFI_mean = safe_mean(RFI), .groups = "drop")
  list(result = out, summary = summ, silhouette = sil_results, best_k = best_k,
       compare = compare, features = cluster_features)
}

# ML_01：单目标 Spearman 相关 + RF 重要性 + XGB 预测评价
ml_single_target <- function(prod, target, features, seed = 123,
                             ntree = 300, nrounds = 120) {
  ml_data <- prod %>% select(Animal_ID, all_of(features), all_of(target)) %>% drop_na()
  if (nrow(ml_data) < 30) {
    return(list(error = paste0(target, " 有效个体不足 30（", nrow(ml_data),
                               " 只），该目标跳过。")))
  }
  cor_results <- data.frame(Feature = features, Correlation = NA_real_,
                            P_value = NA_real_, stringsAsFactors = FALSE)
  for (i in seq_along(features)) {
    ct <- tryCatch(cor.test(ml_data[[features[i]]], ml_data[[target]],
                            method = "spearman", exact = FALSE), error = function(e) NULL)
    if (!is.null(ct)) {
      cor_results$Correlation[i] <- as.numeric(ct$estimate)
      cor_results$P_value[i] <- ct$p.value
    }
  }
  cor_results$Abs_Correlation <- abs(cor_results$Correlation)
  cor_results <- cor_results %>% arrange(desc(Abs_Correlation))
  d_train <- ml_data %>% select(-Animal_ID)
  for (j in seq_len(ncol(d_train))) {
    if (is.numeric(d_train[[j]]) && any(is.na(d_train[[j]]))) {
      d_train[[j]][is.na(d_train[[j]])] <- median(d_train[[j]], na.rm = TRUE)
    }
  }
  set.seed(seed)
  idx <- sample(seq_len(nrow(d_train)), round(0.8 * nrow(d_train)))
  train <- d_train[idx, ]; test <- d_train[-idx, ]
  rf <- tryCatch(randomForest(as.formula(paste(target, "~ .")), data = train,
                              importance = TRUE, ntree = ntree), error = function(e) NULL)
  imp_df <- NULL
  if (!is.null(rf)) {
    imp <- importance(rf)
    # randomForest 4.x 列名为 %IncMSE / IncNodePurity，旧版为 X.IncMSE，统一兼容
    inc_mse_col <- grep("IncMSE", colnames(imp), value = TRUE)[1]
    if (is.na(inc_mse_col)) inc_mse_col <- colnames(imp)[1]
    imp_df <- data.frame(Feature = rownames(imp),
                         IncMSE = imp[, inc_mse_col],
                         IncNodePurity = imp[, "IncNodePurity"],
                         stringsAsFactors = FALSE) %>%
      arrange(desc(IncMSE))
  }
  eval_df <- NULL; pred_df <- NULL
  ok <- tryCatch({
    x_train <- as.matrix(train[, features]); y_train <- train[[target]]
    x_test <- as.matrix(test[, features]); y_test <- test[[target]]
    dtrain <- xgb.DMatrix(x_train, label = y_train)
    dtest <- xgb.DMatrix(x_test, label = y_test)
    xgb_m <- xgb.train(params = list(objective = "reg:squarederror",
                                     eval_metric = "rmse", eta = 0.1,
                                     max_depth = 3, subsample = 0.8,
                                     colsample_bytree = 0.8),
                       data = dtrain, nrounds = nrounds, verbose = 0)
    pred <- predict(xgb_m, dtest)
    eval_df <- data.frame(Target = target, Model = "XGBoost",
                          R2 = round(cor(y_test, pred)^2, 4),
                          RMSE = round(sqrt(mean((y_test - pred)^2)), 4),
                          MAE = round(mean(abs(y_test - pred)), 4),
                          stringsAsFactors = FALSE)
    pred_df <- data.frame(Animal_ID = ml_data$Animal_ID[-idx],
                          Actual = y_test, Predicted = pred,
                          Error = y_test - pred, stringsAsFactors = FALSE)
    TRUE
  }, error = function(e) FALSE)
  list(cor = cor_results, importance = imp_df, evaluation = eval_df,
       prediction = pred_df, n = nrow(ml_data), target = target, xgb_ok = ok)
}

# ML_02：多目标 RF 重要性归一化 + 目标权重融合 → Trait_score
ml_fusion <- function(target_results, w_target = NULL) {
  nms <- names(target_results)
  w_target <- as.numeric(w_target)
  if (is.null(w_target) || length(w_target) != length(nms)) {
    w_target <- rep(1 / length(nms), length(nms))
  }
  parts <- list()
  for (i in seq_along(nms)) {
    r <- target_results[[nms[i]]]
    if (!is.null(r$importance) && nrow(r$importance) > 0) {
      df <- r$importance %>% select(Feature, IncMSE)
      names(df)[2] <- paste0(nms[i], "_importance")
      parts[[nms[i]]] <- df
    }
  }
  if (length(parts) == 0) return(list(error = "所有目标的 RF 重要性均失败，无法融合。"))
  merged <- parts[[1]]
  if (length(parts) > 1) {
    for (i in 2:length(parts)) merged <- full_join(merged, parts[[i]], by = "Feature")
  }
  merged <- merged %>% mutate(across(contains("importance"), ~ replace_na(.x, 0)))
  scale01 <- function(x) if (max(x) - min(x) > 0) (x - min(x)) / (max(x) - min(x)) else 0
  for (i in seq_along(nms)) {
    merged[[paste0(nms[i], "_score")]] <- scale01(merged[[paste0(nms[i], "_importance")]])
  }
  merged$Trait_score <- 0
  for (i in seq_along(nms)) {
    merged$Trait_score <- merged$Trait_score + w_target[i] * merged[[paste0(nms[i], "_score")]]
  }
  merged <- merged %>% arrange(desc(Trait_score))
  merged$Rank <- seq_len(nrow(merged))
  merged
}

# ML_04：多次重复 RF 的稳定性验证（重要性均值 / SD / Top10 频率）
ml_stability <- function(prod, targets, features, n_rep = 10,
                         ntree = 200, seed0 = 1) {
  all_parts <- list()
  for (trait in targets) {
    ml_data <- prod %>% select(Animal_ID, all_of(features), all_of(trait)) %>% drop_na()
    if (nrow(ml_data) < 30) next
    res <- lapply(seq_len(n_rep), function(r) {
      set.seed(seed0 + r)
      rf <- tryCatch(randomForest(as.formula(paste(trait, "~ .")),
                                  data = ml_data %>% select(-Animal_ID),
                                  importance = TRUE, ntree = ntree),
                     error = function(e) NULL)
      if (is.null(rf)) return(NULL)
      imp <- importance(rf)
      df <- data.frame(Feature = rownames(imp),
                       Importance = imp[, "IncNodePurity"],
                       stringsAsFactors = FALSE)
      df <- df %>% arrange(desc(Importance))
      df$Rank <- seq_len(nrow(df))
      df$Seed <- r; df$Trait <- trait
      df
    })
    all_parts[[trait]] <- bind_rows(res)
  }
  if (length(all_parts) == 0) {
    return(list(error = "稳定性验证数据不足（各目标有效个体均 <30）。"))
  }
  bind_rows(all_parts) %>%
    group_by(Feature) %>%
    summarise(Mean_Importance = mean(Importance, na.rm = TRUE),
              SD_Importance = sd(Importance, na.rm = TRUE),
              Mean_Rank = mean(Rank, na.rm = TRUE),
              Top10_Frequency = mean(Rank <= 10, na.rm = TRUE),
              Appear_Count = n(), .groups = "drop") %>%
    arrange(desc(Mean_Importance))
}

# ML_05：最终智能指示性状评分（Trait_score × 权重 + StabilityScore × 权重）
ml_final_score <- function(trait_score, stability, w_imp = 0.7, w_stab = 0.3) {
  final <- trait_score %>% select(Feature, Trait_score) %>%
    left_join(stability, by = "Feature")
  if (nrow(final) == 0) return(list(error = "综合评分无可用特征。"))
  max_rank <- max(final$Mean_Rank, na.rm = TRUE)
  rng_sd <- max(final$SD_Importance, na.rm = TRUE) - min(final$SD_Importance, na.rm = TRUE)
  final <- final %>% mutate(
    RankScore = ifelse(max_rank > 1, 1 - (Mean_Rank - 1) / (max_rank - 1), 1),
    SD_norm = ifelse(rng_sd > 0,
                     (SD_Importance - min(SD_Importance, na.rm = TRUE)) / rng_sd, 0.5),
    SDScore = 1 - SD_norm,
    StabilityScore = 0.4 * Top10_Frequency + 0.4 * RankScore + 0.2 * SDScore,
    FinalScore = w_imp * Trait_score + w_stab * StabilityScore) %>%
    arrange(desc(FinalScore))
  final$Rank <- seq_len(nrow(final))
  final
}

# ML_07：育种报告（指标分类 / 推荐等级 / 证据等级 / 生物学解释）
ml_report <- function(final_rank) {
  final_rank %>% mutate(
    Trait_Category = case_when(
      grepl("Ratio", Feature) ~ "采食节律",
      grepl("CV|Robust", Feature) ~ "行为稳定性",
      grepl("FI|ADFI", Feature) ~ "采食能力",
      grepl("Bouts|Meal|Duration|TFD|IMI|AFBD", Feature) ~ "采食模式",
      TRUE ~ "综合行为指标"),
    Recommendation_Level = case_when(FinalScore >= 0.65 ~ "核心指标",
                                     FinalScore >= 0.45 ~ "重点关注",
                                     TRUE ~ "辅助指标"),
    Evidence_Level = case_when(Top10_Frequency >= 0.8 & FinalScore >= 0.65 ~ "强证据",
                               Top10_Frequency >= 0.5 ~ "中等证据",
                               TRUE ~ "探索性指标"),
    Biological_Interpretation = case_when(
      Trait_Category == "采食能力" ~ "反映个体采食水平和营养摄入能力，与生长性能和饲料利用效率密切相关。",
      Trait_Category == "行为稳定性" ~ "反映采食行为波动程度，可评价个体行为一致性和生产稳定性。",
      Trait_Category == "采食节律" ~ "反映昼夜采食分配模式，可揭示个体采食时间策略差异。",
      Trait_Category == "采食模式" ~ "反映采食事件组织方式、频率和持续特征，可用于描述行为模式差异。",
      TRUE ~ "综合反映个体采食行为特征，可作为智能育种候选指标。"),
    Breeding_Application = case_when(
      Recommendation_Level == "核心指标" ~ "建议优先纳入智能育种候选性状体系。",
      Recommendation_Level == "重点关注" ~ "建议结合生产性能和遗传参数进一步验证。",
      TRUE ~ "可作为探索性行为表型进行持续积累。"))
}

# 总调度：模块⑥ 一次运行全链路
ml_run_all <- function(prod, targets, w_target = NULL,
                       w_imp = 0.7, w_stab = 0.3, n_rep = 10) {
  targets <- targets[!is.na(targets) & targets != "---"]
  if (length(targets) == 0) return(list(error = "请至少选择一个预测目标性状。"))
  if (length(w_target) != length(targets)) w_target <- rep(1 / length(targets), length(targets))
  features <- ml_features_v5(prod)
  if (length(features) < 5) return(list(error = "行为特征不足（至少 5 个）。"))
  clu <- ml_cluster(prod)
  tr <- list()
  for (i in seq_along(targets)) {
    tr[[targets[i]]] <- ml_single_target(prod, targets[i], features)
  }
  fusion <- ml_fusion(tr, w_target)
  if (!is.data.frame(fusion)) {
    return(list(error = fusion$error, cluster = clu, targets = tr))
  }
  stab <- ml_stability(prod, targets, features, n_rep = n_rep)
  final <- ml_final_score(fusion, stab, w_imp = w_imp, w_stab = w_stab)
  if (!is.data.frame(final)) {
    return(list(error = final$error, cluster = clu, targets = tr,
                fusion = fusion, stability = stab))
  }
  report <- ml_report(final)
  list(cluster = clu, targets = tr, fusion = fusion, stability = stab,
       final = final, top10 = final %>% slice_head(n = 10),
       report = report, features = features)
}

# ============================================================
# 5. 主流程（供 Shiny 与命令行验证共用）
# ============================================================

run_pipeline_v5 <- function(feed_files, bw_files, ped_path, idmap_path, cfg) {
  t0 <- Sys.time()
  # 1) 读取（全部 sheet）
  feed_raw <- read_matching_excel_files(feed_files, "feed")
  bw_raw <- read_matching_excel_files(bw_files, "weight")
  ped <- read_pedigree(ped_path)
  idmap <- read_idmap(idmap_path)

  # 2) 清洗
  clean <- run_clean_v5(feed_raw$data, bw_raw$data, cfg)

  # 3) 指标
  bout <- build_bouts(clean$feed, cfg$imi_threshold, cfg$min_intake)
  ind_feeding <- calc_individual_feeding(bout)
  prod <- calc_production(clean$feed, clean$bw, ind_feeding)
  prod <- assign_hff_lff(prod, method = cfg$hff_method, cutoff = cfg$hff_cutoff)
  prod$HFF <- ifelse(prod$Feed_Frequency_Group == "HFF", 1, 0)

  # 创新指标
  daynight <- compute_daynight_v5(bout, cfg$day_start, cfg$day_end)
  cv <- compute_behavior_cv_v5(bout)
  cosinor <- compute_cosinor_v5(bout)
  fano <- compute_fano_v5(bout)
  prod <- prod %>%
    left_join(daynight, by = "Animal_ID") %>%
    left_join(cv, by = "Animal_ID") %>%
    left_join(cosinor, by = "Animal_ID") %>%
    left_join(fano, by = "Animal_ID")

  # 节律 / 周FCR
  rhythm <- calc_rhythm(bout)
  weekly_fcr <- calc_weekly_fcr(clean$feed, clean$bw, cfg$week_length)

  # 4) ID 打通（eID → 翅号 → 系谱），并把性别回连到 prod 供留种性别均衡使用
  link <- link_animals(clean$feed, clean$bw, ped, idmap)
  prod <- prod %>%
    left_join(link %>% select(eID, Sex) %>% distinct(eID, .keep_all = TRUE),
              by = c("Animal_ID" = "eID"))

  # 5) 指示性状筛选（轻量，清洗阶段即算好，供模块⑤展示与「指示性状→指数」使用）
  ind_traits <- select_indicator_traits(prod, cfg$target_trait %||% "FCR",
                                        cfg$indicator_min_r %||% 0.3,
                                        cfg$indicator_max_n %||% 5)

  # 6) 遗传评估与留种：默认不在清洗阶段跑（重型计算留给模块⑤按用户设置执行）
  genetic <- NULL; retention <- NULL; key_metrics <- NULL
  if (isTRUE(cfg$run_retention)) {
    rr <- run_retention_module(prod, link, cfg)
    genetic <- rr$genetic; retention <- rr$retention; key_metrics <- rr$key_metrics
    ind_traits <- rr$indicator_traits
  }

  list(
    feed_raw_qc = feed_raw$qc, bw_raw_qc = bw_raw$qc,
    clean = clean, bout = bout, feeding = ind_feeding, prod = prod,
    rhythm = rhythm, weekly_fcr = weekly_fcr,
    link = link, indicator_traits = ind_traits,
    genetic = genetic, retention = retention, key_metrics = key_metrics,
    elapsed = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

# ============================================================
# 6. UI（专家模式六大模块）
# ============================================================

ui <- navbarPage(
  title = "鸭芯智选 · 肉鸭智能化采食行为分析工具 V5.0",
  theme = NULL,
  windowTitle = "鸭芯智选 V5.0",

  # ---- 模块① 数据与清洗 ----
  tabPanel("① 数据与清洗",
    sidebarLayout(
      sidebarPanel(
        h4("数据上传"),
        fileInput("feed_files", "采食原始数据（.xls/.xlsx，可多选，自动读取全部Sheet）",
                  multiple = TRUE, accept = c(".xls", ".xlsx")),
        fileInput("bw_files", "体重原始数据（.xls/.xlsx，可多选，自动读取全部Sheet）",
                  multiple = TRUE, accept = c(".xls", ".xlsx")),
        fileInput("ped_file", "系谱（可选：翅号/父本笼号/母本笼号/性别）",
                  accept = c(".xls", ".xlsx")),
        fileInput("idmap_file", "eID-ID对照表（可选）", accept = c(".xls", ".xlsx")),
        helpText("系谱与对照表为可选。不上传系谱时，软件自动使用『文献遗传参数』路线完成留种决策（无文献支持的性状默认 h²=0.3，会给出提示）。"),
        hr(),
        h4("清洗参数"),
        checkboxInput("auto_time_range", "自动读取数据最早/最晚时间（推荐，勾选后覆盖下方日期）",
                      value = TRUE),
        dateInput("exp_start", "试验开始日期（取消勾选自动时生效）",
                  value = Sys.Date() - 30),
        helpText("试验开始日期 = 数据起点：早于此日期的记录会被过滤，并作为试验第1周的起点。",
                 "数据自动截止到最晚记录时间，无需手动设置结束日期。"),
        numericInput("week_length", "周长度（天）", value = 7, min = 1, max = 30),
        numericInput("feed_sd", "采食异常阈值（均值±k倍SD）", value = 3, min = 1, max = 10),
        numericInput("bw_sd", "体重异常阈值（均值±k倍SD）", value = 3, min = 1, max = 10),
        numericInput("imi_threshold", "餐间隔阈值（秒，>则算新一次采食）", value = 300, min = 30),
        numericInput("min_intake", "最小单次采食量（g）", value = 1, min = 0),
        actionButton("run_clean", "运行数据清洗", class = "btn-primary btn-block"),
        br(), br(),
        uiOutput("clean_status")
      ),
      mainPanel(
        downloadButton("dl_clean_xlsx", "下载本板块 Excel", class = "btn-sm"),
        br(), br(),
        h4("工作表读取情况"), DT::dataTableOutput("sheet_qc"),
        br(),
        tabsetPanel(
          tabPanel("时间范围 QC",
                   uiOutput("time_qc_hint"),
                   DT::dataTableOutput("time_qc_tbl")),
          tabPanel("字段缺失 QC（全部字段，可用顶部筛选框筛选）",
                   DT::dataTableOutput("na_qc_tbl"))
        ),
        h4("周统计（采食）"), DT::dataTableOutput("week_feed_tbl"),
        h4("周统计（体重）"), DT::dataTableOutput("week_bw_tbl")
      )
    )
  ),

  # ---- 模块② 个体指标 ----
  tabPanel("② 个体指标",
    sidebarLayout(
      sidebarPanel(
        h4("个体指标"),
        actionButton("run_indicator", "计算个体指标", class = "btn-primary btn-block"),
        hr(),
        numericInput("ind_max_n", "表格最大行数", value = 500),
        uiOutput("indicator_status")
      ),
      mainPanel(
        downloadButton("dl_ind_xlsx", "下载本板块 Excel", class = "btn-sm"),
        br(), br(),
        tabsetPanel(
          tabPanel("采食行为",
                   DT::dataTableOutput("ind_feeding_tbl"),
                   br(),
                   h4("采食速率 FR（按周）"),
                   plot_box_ui("fr_violin", "500px"),
                   br(),
                   h4("采食间隔 IMI（按周）"),
                   plot_box_ui("imi_violin", "500px"),
                   br(),
                   h4("采食时长（按周）"),
                   plot_box_ui("duration_violin", "500px")),
          tabPanel("生产性能",
                   DT::dataTableOutput("prod_tbl"),
                   br(),
                   h4("总体生长曲线（每日终末体重均值）"),
                   plot_box_ui("overall_growth_plot", "450px"),
                   br(),
                   h4("HFF / LFF 分组生长曲线"),
                   uiOutput("group_growth_hint"),
                   plot_box_ui("group_growth_plot", "450px"))
        )
      )
    )
  ),

  # ---- 模块③ 组间时序相关 ----
  tabPanel("③ 组间时序相关",
    sidebarLayout(
      sidebarPanel(
        h4("HFF/LFF 分组"),
        selectInput("hff_method", "分组方法", choices = c("中位数" = "median",
                                                          "自定义阈值" = "custom")),
        numericInput("hff_cutoff", "自定义阈值（次/天，仅自定义时生效）",
                     value = NA, min = 1),
        actionButton("run_group", "运行组间分析", class = "btn-primary btn-block"),
        hr(),
        uiOutput("group_status")
      ),
      mainPanel(
        downloadButton("dl_grp_xlsx", "下载本板块 Excel", class = "btn-sm"),
        br(), br(),
        tabsetPanel(
          tabPanel("HFF/LFF 指标对比",
                   DT::dataTableOutput("hff_tbl"),
                   br(),
                   h4("HFF vs LFF 分布"),
                   plot_box_ui("hff_plot", "520px")),
          tabPanel("访饲节律（日）", plot_box_ui("rhythm_daily_plot", "400px")),
          tabPanel("访饲节律（24h）", plot_box_ui("rhythm_hourly_plot", "400px")),
          tabPanel("每周访饲次数", plot_box_ui("weekly_bouts_plot", "400px")),
          tabPanel("每周 FCR",
                   DT::dataTableOutput("weekly_fcr_tbl"),
                   br(),
                   h4("每周 FCR 分布"),
                   plot_box_ui("weekly_fcr_plot", "480px"))
        )
      )
    )
  ),

  # ---- 模块④ 创新行为指标 ----
  tabPanel("④ 创新行为指标",
    sidebarLayout(
      sidebarPanel(
        h4("创新行为指标"),
        textInput("day_start", "白天开始(HH:MM)", value = "06:00"),
        textInput("day_end", "白天结束(HH:MM)", value = "18:00"),
        actionButton("run_innovation", "计算创新指标", class = "btn-primary btn-block"),
        hr(),
        uiOutput("innovation_status")
      ),
      mainPanel(
        downloadButton("dl_inno_xlsx", "下载本板块 Excel", class = "btn-sm"),
        br(), br(),
        tabsetPanel(
          tabPanel("昼夜分配",
                   DT::dataTableOutput("daynight_tbl"),
                   br(),
                   h4("白天采食占比 vs FCR"),
                   plot_box_ui("daynight_scatter", "440px")),
          tabPanel("行为变异性 CV",
                   DT::dataTableOutput("cv_tbl"),
                   br(),
                   h4("CV vs FCR（分面散点）"),
                   plot_box_ui("cv_fcr_scatter", "480px")),
          tabPanel("余弦节律拟合",
                   DT::dataTableOutput("cosinor_tbl"),
                   br(),
                   h4("余弦振幅 vs FCR"),
                   plot_box_ui("cosinor_scatter", "440px"),
                   br(),
                   h4("采食峰值时刻分布"),
                   plot_box_ui("phase_plot", "420px")),
          tabPanel("Fano 指数", DT::dataTableOutput("fano_tbl")),
          tabPanel("创新指标×FCR 相关", DT::dataTableOutput("inno_corr_tbl"))
        )
      )
    )
  ),

  # ---- 模块⑤ 遗传评估与留种（核心） ----
  tabPanel("⑤ 遗传评估与留种",
    sidebarLayout(
      sidebarPanel(
        h4("① 育种目标"),
        checkboxGroupInput("target_trait", "目标性状（育种目标，可多选）",
                    choices = c(
                      "FCR 饲料转化比" = "FCR",
                      "RFI 剩余采食量" = "RFI",
                      "ADG_g 日增重" = "ADG_g",
                      "FBW_kg 终重" = "FBW_kg",
                      "FI_Day_g 日采食量" = "FI_Day_g",
                      "FR_g_sec 采食速率" = "FR_g_sec",
                      "TFB_Day 日访饲次数" = "TFB_Day",
                      "AMS_g 单次采食量" = "AMS_g",
                      "ADFI_g 平均日采食量" = "ADFI_g",
                      "IBW_kg 初重" = "IBW_kg",
                      "Gain_kg 总增重" = "Gain_kg",
                      "Day_FI_Ratio 白天采食占比" = "Day_FI_Ratio",
                      "皮脂率 SkinFat_Rate" = "SkinFat_Rate",
                      "腹脂率 AbFat_Rate" = "AbFat_Rate",
                      "胸肌率 BMP" = "BMP"),
                    selected = "FCR"),
        helpText("目标性状 = 育种希望改良的性状（如烤鸭看皮脂率、饲料效率看 FCR/RFI）。",
                 "多选时：指示性状取各目标性状筛选结果的并集。",
                 "右侧三项皮脂率/腹脂率/胸肌率为屠宰性状，本场无法活体测得，",
                 "选中后自动按文献遗传相关匹配代理指示性状。"),
        fluidRow(
          column(6, numericInput("indicator_min_r", "指示性状阈值(|r|≥)", value = 0.3, min = 0)),
          column(6, numericInput("indicator_max_n", "指示性状上限", value = 5, min = 1, max = 10))
        ),
        actionButton("use_indicators", "把筛出的指示性状设为指数性状",
                     class = "btn-default btn-block"),
        hr(),
        h4("② 遗传评估路线"),
        radioButtons("use_pedigree", "评估路线",
                     choices = c("路线A：PBLUP动物模型（有系谱）" = "TRUE",
                                 "路线B：文献遗传参数选择指数（无系谱）" = "FALSE"),
                     selected = "TRUE"),
        helpText("路线A：本场有系谱 → 构建亲缘矩阵 A，动物模型估计育种值（EBV），",
                 "结果最可靠，且支持本场 REML 实测遗传力（推荐有系谱时选）。",
                 "路线B：无系谱 → 用文献遗传力 h² 对标准化表型加权，",
                 "近似 EBV = h²×(P-μ)/σ，适合小场、无系谱或目标性状未测的场景。",
                 "注：系谱可用个体 <20 只时，即使选路线A也会自动回退到路线B并提示。"),
        checkboxGroupInput("fixed_effects", "固定效应",
                           choices = c("性别" = "Sex"), selected = "Sex"),
        checkboxInput("use_reml_h2", "路线A运行时自动实测本场 h²（失败回退输入值）",
                      value = TRUE),
        hr(),
        h4("③ 选择指数性状与权重"),
        helpText("指数 = Σ(各性状EBV × 方向 × 权重)。方向已自动处理：FCR/RFI/采食量等越低越好，",
                 "增重/终重等越高越好，权重只需填正数（数值越大越重要）。",
                 "权重在运行时自动归一化（总和=1），不影响排名。",
                 "可点①的『把筛出的指示性状设为指数性状』一键带入。"),
        checkboxGroupInput("index_traits_eff", "效率与增重",
                           choices = INDEX_TRAIT_CHOICES[["效率与增重"]],
                           selected = c("FCR", "ADG_g", "FBW_kg")),
        checkboxGroupInput("index_traits_feeding", "采食行为",
                           choices = INDEX_TRAIT_CHOICES[["采食行为"]]),
        checkboxGroupInput("index_traits_rhythm", "节律与稳定性",
                           choices = INDEX_TRAIT_CHOICES[["节律与稳定性"]]),
        uiOutput("index_weights_ui"),
        hr(),
        h4("④ 留种设置"),
        numericInput("retention_ratio", "留种比例（前 %）", value = 30, min = 1, max = 90),
        checkboxInput("sex_balance", "留种时尽量性别均衡", value = TRUE),
        actionButton("run_genetic", "运行遗传评估与留种", class = "btn-primary btn-block"),
        helpText("修改上方任何参数后，需重新点击『运行遗传评估与留种』，右侧结果才会更新（右侧显示的是最近一次运行的结果）。"),
        hr(),
        uiOutput("genetic_status")
      ),
      mainPanel(
        downloadButton("dl_mod5_xlsx", "下载本板块 Excel", class = "btn-sm"),
        br(), br(),
        uiOutput("lit_hint"),
        h4("关键指标卡"),
        DT::dataTableOutput("key_metrics_tbl"),
        br(),
        tabsetPanel(
          tabPanel("① 指示性状筛选", DT::dataTableOutput("indicator_tbl"),
                   plot_box_ui("indicator_plot", "400px")),
          tabPanel("② 遗传力估计（REML实测 vs 文献）", uiOutput("h2_est_panel")),
          tabPanel("③ 育种值（EBV）", DT::dataTableOutput("ebv_tbl")),
          tabPanel("④ 选择指数分布", plot_box_ui("index_plot", "450px")),
          tabPanel("⑤ 留种名单（最终结果）", DT::dataTableOutput("retain_tbl")),
          tabPanel("附表·表型相关热图", uiOutput("corr_hint"),
                   plot_box_ui("corr_heatmap", "560px")),
          tabPanel("附表·文献参数表", DT::dataTableOutput("lit_tbl")),
          tabPanel("附表·文献遗传相关 rG", DT::dataTableOutput("lit_corr_tbl"))
        )
      )
    )
  ),

  # ---- 模块⑥ AI 智能选种探索 ----
  tabPanel("⑥ AI 智能选种探索",
    sidebarLayout(
      sidebarPanel(
        h4("AI 智能选种探索（辅助层）"),
        p("用机器学习为『指示性状筛选』补充证据：",
          "识别非线性关联、量化特征重要性、验证稳定性。"),
        checkboxGroupInput("ml_targets", "预测目标性状",
          choices = c("日增重 ADG_g" = "ADG_g",
                      "饲料转化比 FCR" = "FCR",
                      "剩余采食量 RFI" = "RFI"),
          selected = c("ADG_g", "FCR", "RFI")),
        h5("目标权重（Trait_score 融合，默认 0.33/0.33/0.34）"),
        numericInput("ml_w_adg", "ADG 权重", 0.33, min = 0, max = 1, step = 0.05),
        numericInput("ml_w_fcr", "FCR 权重", 0.33, min = 0, max = 1, step = 0.05),
        numericInput("ml_w_rfi", "RFI 权重", 0.34, min = 0, max = 1, step = 0.05),
        h5("最终评分权重（默认 0.7 / 0.3）"),
        numericInput("ml_w_imp", "重要性得分权重", 0.7, min = 0, max = 1, step = 0.05),
        numericInput("ml_w_stab", "稳定性得分权重", 0.3, min = 0, max = 1, step = 0.05),
        actionButton("run_ml", "运行 AI 智能选种分析", class = "btn-primary btn-block"),
        br(), br(),
        downloadButton("ml_report_dl", "下载育种报告 Excel"),
        hr(),
        uiOutput("ml_status")
      ),
      mainPanel(
        h4("模块说明"),
        uiOutput("ml_hint"),
        hr(),
        tabsetPanel(
          tabPanel("采食模式聚类",
                   DT::dataTableOutput("ml_cluster_tbl"),
                   br(),
                   h4("HFF/LFF 性能比较（K-means 聚类 vs 模块③ TFB 中位数）"),
                   DT::dataTableOutput("ml_cluster_compare_tbl")),
          tabPanel("特征重要性",
                   DT::dataTableOutput("ml_importance_tbl"),
                   br(),
                   h4("模型评价（XGBoost 预测）"),
                   DT::dataTableOutput("ml_eval_tbl")),
          tabPanel("综合评分 Top10", DT::dataTableOutput("ml_final_tbl")),
          tabPanel("稳定性验证", DT::dataTableOutput("ml_stability_tbl")),
          tabPanel("育种报告",
                   uiOutput("ml_report_hint"),
                   br(),
                   plot_box_ui("ml_report_score_plot", "380px"),
                   br(),
                   DT::dataTableOutput("ml_report_tbl"))
        )
      )
    )
  )
)

# ============================================================
# 7. Server
# ============================================================

server <- function(input, output, session) {

  rv <- reactiveValues()

  # ---------- 模块① 数据与清洗 ----------
  cfg_clean <- reactive({
    list(
      auto_time_range = isTRUE(input$auto_time_range),
      experiment_start = as.Date(input$exp_start),
      # 训练期结束日期/时间控件已移除：数据默认截止到最晚记录，无需手动设置
      training_end = NA,
      training_time = NA,
      week_length = as.integer(input$week_length),
      feed_sd = input$feed_sd, bw_sd = input$bw_sd,
      imi_threshold = as.numeric(input$imi_threshold),
      min_intake = as.numeric(input$min_intake)
    )
  })

  observeEvent(input$run_clean, {
    validate(need(input$feed_files, "请上传采食数据。"),
             need(input$bw_files, "请上传体重数据。"))
    # 系谱 / eID-ID 对照表为可选：不上传则自动走『文献遗传参数』路线
    ped_path <- if (!is.null(input$ped_file)) input$ped_file$datapath else NULL
    idmap_path <- if (!is.null(input$idmap_file)) input$idmap_file$datapath else NULL
    withProgress(message = "数据读取与清洗中…", value = 0.2, {
      tryCatch({
        res <- run_pipeline_v5(input$feed_files, input$bw_files,
                               ped_path, idmap_path,
                               c(cfg_clean(), list(
                                 target_trait = "FCR",
                                 indicator_min_r = 0.3, indicator_max_n = 5,
                                 use_pedigree = !is.null(ped_path), h2 = 0.3,
                                 fixed_effects = "Sex",
                                 index_traits = c("FCR", "ADG_g", "FBW_kg"),
                                 weights = c(FCR = 0.5, ADG_g = 0.3, FBW_kg = 0.2),
                                 retention_ratio = 0.3, sex_balance = TRUE,
                                 hff_method = "median", hff_cutoff = NA,
                                 run_retention = FALSE,
                                 day_start = "06:00", day_end = "18:00")))
        rv$pipe <- res
        incProgress(1, detail = "完成")
      }, error = function(e) {
        showNotification(paste0("清洗失败：", conditionMessage(e)), type = "error", duration = 10)
      })
    })
  })

  # 系谱文件上传状态 → 自动切换评估路线
  # 上传系谱：默认路线A（PBLUP）+ 自动实测本场 h²；未上传：自动路线B（文献参数）
  # ignoreNULL=FALSE：页面加载时 input$ped_file=NULL 也触发一次，确保未传系谱自动切路线B
  observeEvent(input$ped_file, {
    if (!is.null(input$ped_file)) {
      updateRadioButtons(session, "use_pedigree", selected = "TRUE")
      updateCheckboxInput(session, "use_reml_h2", value = TRUE)
      showNotification("已上传系谱：默认使用路线A（PBLUP + 本场实测遗传力），可手动改为路线B",
                       type = "message", duration = 6)
    } else {
      updateRadioButtons(session, "use_pedigree", selected = "FALSE")
      updateCheckboxInput(session, "use_reml_h2", value = FALSE)
      showNotification("未上传系谱：已自动切换路线B（文献遗传参数），留种决策仍可正常完成",
                       type = "warning", duration = 8)
    }
  }, ignoreNULL = FALSE)

  output$sheet_qc <- DT::renderDataTable({
    req(rv$pipe)
    bind_rows(rv$pipe$feed_raw_qc, rv$pipe$bw_raw_qc) %>% DT_fast(10)
  })
  output$time_qc_hint <- renderUI({
    req(rv$pipe)
    dq <- rv$pipe$clean$datetime_qc
    tags$div(class = "alert alert-info",
             paste0("数据时间范围：", format(dq$Start, "%Y-%m-%d %H:%M"),
                    " ～ ", format(dq$End, "%Y-%m-%d %H:%M"),
                    "，共 ", dq$N, " 条采食记录。",
                    if (input$auto_time_range)
                      "（当前为『自动读取时间范围』，试验周次与截断按此范围计算）"
                    else "（手动指定开始/结束日期）"))
  })
  output$time_qc_tbl <- DT::renderDataTable({
    req(rv$pipe)
    dq <- rv$pipe$clean$datetime_qc
    dq %>% mutate(Start = format(Start, "%Y-%m-%d %H:%M:%S"),
                  End = format(End, "%Y-%m-%d %H:%M:%S")) %>% DT_fast(5)
  })
  output$na_qc_tbl <- DT::renderDataTable({
    req(rv$pipe)
    rv$pipe$clean$na_qc %>% DT_fast(50)
  })
  output$week_feed_tbl <- DT::renderDataTable({
    req(rv$pipe); rv$pipe$clean$feed_week_stats %>% DT_fast(10)
  })
  output$week_bw_tbl <- DT::renderDataTable({
    req(rv$pipe); rv$pipe$clean$bw_week_stats %>% DT_fast(10)
  })
  output$clean_status <- renderUI({
    if (is.null(rv$pipe)) return(NULL)
    tags$div(class = "alert alert-success",
             paste0("清洗完成：采食 ", nrow(rv$pipe$clean$feed),
                    " 行，体重 ", nrow(rv$pipe$clean$bw),
                    " 行，耗时 ", round(rv$pipe$elapsed, 1), " 秒"))
  })

  # ---------- 模块② 个体指标 ----------
  observeEvent(input$run_indicator, {
    req(rv$pipe)
    showNotification("个体指标已更新（随清洗结果自动计算）", type = "message")
  })
  output$ind_feeding_tbl <- DT::renderDataTable({
    req(rv$pipe); rv$pipe$feeding %>% mutate(across(where(is.numeric), ~round(.x, 2))) %>% DT_fast(input$ind_max_n)
  })
  output$prod_tbl <- DT::renderDataTable({
    req(rv$pipe); rv$pipe$prod %>%
      select(Animal_ID, Feed_Frequency_Group, TFB, TFB_Day, FI_g, FI_Day_g, AMS_g,
             TFD_sec, AFBD_sec, IMI_sec, FR_g_sec, IBW_kg, FBW_kg, Gain_kg,
             ADG_g, ADFI_g, FCR, RFI, Feed_Efficiency) %>%
      mutate(across(where(is.numeric), ~round(.x, 2))) %>% DT_fast(input$ind_max_n)
  })
  # ---- 模块② 单次采食 3 张小提琴图（v4.4 一比一）----
  make_violin_week <- function(dat, yvar, ylab) {
    observed_weeks <- sort(unique(dat$Week_Number[!is.na(dat$Week_Number)]))
    max_week <- if (length(observed_weeks) > 0) max(observed_weeks) else 1
    all_weeks <- seq_len(max_week)
    dat <- dat %>% filter(!is.na(.data[[yvar]]), is.finite(.data[[yvar]]),
                          !is.na(Week_Number))
    validate(need(nrow(dat) > 0, paste0("没有可用于绘图的 ", ylab, " 数据。")))
    week_labels <- paste0("Week ", all_weeks)
    dat$Plot_Group <- factor(dat$Week_Number, levels = all_weeks, labels = week_labels)
    plot_dat <- sample_for_plot(dat, "Plot_Group", 5000)
    pal <- rep(PAL_RPBG, length.out = length(week_labels))
    fill_map <- setNames(pal, week_labels)
    p <- ggplot(plot_dat, aes(x = Plot_Group, y = .data[[yvar]], fill = Plot_Group)) +
      geom_violin(color = NA, alpha = 0.82, trim = TRUE,
                  scale = "width", adjust = 1.15) +
      scale_fill_manual(values = fill_map, guide = "none") +
      geom_boxplot(width = 0.16, fill = "white", color = "#666666",
                   outlier.shape = NA, linewidth = 0.55) +
      stat_summary(fun = median, geom = "point", shape = 95,
                   size = 7, color = "#555555") +
      scale_x_discrete(drop = FALSE) +
      theme_bw(base_size = 13) +
      labs(x = "Week", y = ylab) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank())
    if (nrow(dat) >= 20) {
      q <- quantile(dat[[yvar]], probs = c(0.01, 0.99), na.rm = TRUE, names = FALSE)
      if (is.finite(q[1]) && is.finite(q[2]) && q[1] < q[2]) p <- p + coord_cartesian(ylim = q)
    }
    p
  }
  bout_with_week <- reactive({
    req(rv$pipe)
    wk <- rv$pipe$clean$feed %>% distinct(Animal_ID, Date = as.Date(Time1), Week_Number)
    rv$pipe$bout %>% left_join(wk, by = c("Animal_ID", "Date"))
  })
  p_fr_violin <- reactive({
    req(rv$pipe)
    make_violin_week(bout_with_week(), "FR_g_sec", "采食速率 (g/s)")
  })
  output$fr_violin <- renderPlot({ p_fr_violin() })
  register_plot_dl(output, "fr_violin", p_fr_violin, 10, 6)
  p_imi_violin <- reactive({
    req(rv$pipe)
    make_violin_week(bout_with_week(), "IMI_sec", "采食间隔 (s)")
  })
  output$imi_violin <- renderPlot({ p_imi_violin() })
  register_plot_dl(output, "imi_violin", p_imi_violin, 10, 6)
  p_duration_violin <- reactive({
    req(rv$pipe)
    make_violin_week(bout_with_week(), "Feed_Duration_sec", "采食时长 (s)")
  })
  output$duration_violin <- renderPlot({ p_duration_violin() })
  register_plot_dl(output, "duration_violin", p_duration_violin, 10, 6)

  # ---- 模块② 生长曲线 2 张（v4.4 一比一）----
  daily_terminal_bw <- reactive({
    req(rv$pipe)
    rv$pipe$clean$bw %>%
      filter(is.finite(BW_kg), !is.na(Time1)) %>%
      mutate(Date = as.Date(Time1)) %>%
      arrange(Animal_ID, Time1) %>%
      group_by(Animal_ID, Date) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      mutate(Exp_Day = as.numeric(difftime(Date, min(Date, na.rm = TRUE), units = "days")))
  })
  p_overall_growth_plot <- reactive({
    req(rv$pipe)
    dat <- daily_terminal_bw() %>%
      group_by(Exp_Day) %>%
      summarise(Mean_BW_kg = mean(BW_kg, na.rm = TRUE),
                N = n(), .groups = "drop") %>%
      filter(is.finite(Mean_BW_kg)) %>% arrange(Exp_Day)
    validate(need(nrow(dat) > 0, "没有足够的每日终末体重数据。"))
    ggplot(dat, aes(x = Exp_Day, y = Mean_BW_kg)) +
      geom_line(linewidth = 1.2, color = PAL_RPBG[2], na.rm = TRUE) +
      geom_point(size = 3, color = PAL_RPBG[2], na.rm = TRUE) +
      theme_bw(base_size = 13) +
      labs(x = "试验日龄 (d)", y = "每日终末体重均值 (kg)",
           subtitle = "每只鸭每天取最后一次有效体重，再计算当日群体均值") +
      theme(plot.subtitle = element_text(size = 10))
  })
  output$overall_growth_plot <- renderPlot({ p_overall_growth_plot() })
  register_plot_dl(output, "overall_growth_plot", p_overall_growth_plot, 10, 6)
  output$group_growth_hint <- renderUI({
    if (is.null(rv$pipe) || !("Feed_Frequency_Group" %in% names(rv$pipe$prod))) {
      return(tags$div(class = "alert alert-warning",
                      "需先在模块③『组间分析』完成 HFF/LFF 分组后才显示此图。"))
    }
    tags$div(class = "alert alert-info",
             "HFF/LFF 分组生长曲线：分别计算两组每日终末体重均值。")
  })
  p_group_growth_plot <- reactive({
    req(rv$pipe)
    validate(need("Feed_Frequency_Group" %in% names(rv$pipe$prod),
                  "请先在模块③运行『组间分析』完成 HFF/LFF 分组。"))
    group_map <- rv$pipe$prod %>% select(Animal_ID, Feed_Frequency_Group)
    dat <- daily_terminal_bw() %>%
      left_join(group_map, by = "Animal_ID") %>%
      filter(Feed_Frequency_Group %in% c("HFF", "LFF"),
             is.finite(BW_kg), is.finite(Exp_Day)) %>%
      group_by(Feed_Frequency_Group, Exp_Day) %>%
      summarise(Mean_BW_kg = mean(BW_kg, na.rm = TRUE), N = n(), .groups = "drop") %>%
      filter(is.finite(Mean_BW_kg)) %>% arrange(Feed_Frequency_Group, Exp_Day)
    validate(need(nrow(dat) > 0, "没有足够的 HFF/LFF 每日终末体重数据。"))
    ggplot(dat, aes(x = Exp_Day, y = Mean_BW_kg,
                    color = Feed_Frequency_Group, group = Feed_Frequency_Group)) +
      geom_line(linewidth = 1.2, na.rm = TRUE) +
      geom_point(size = 3, na.rm = TRUE) +
      scale_color_manual(values = c("HFF" = PAL_RPBG[1], "LFF" = PAL_RPBG[2])) +
      theme_bw(base_size = 13) +
      labs(x = "试验日龄 (d)", y = "每日终末体重均值 (kg)",
           color = "采食频率分组",
           subtitle = "每只鸭每天取最后一次有效体重，再分别计算 HFF/LFF 当日均值") +
      theme(plot.subtitle = element_text(size = 10))
  })
  output$group_growth_plot <- renderPlot({ p_group_growth_plot() })
  register_plot_dl(output, "group_growth_plot", p_group_growth_plot, 10, 6)
  output$indicator_status <- renderUI({
    if (is.null(rv$pipe)) return(NULL)
    tags$div(class = "alert alert-info",
             paste0("个体数：", nrow(rv$pipe$prod),
                    " ｜ 有效 FCR 个体：", sum(is.finite(rv$pipe$prod$FCR))))
  })

  # ---------- 模块③ 组间时序相关 ----------
  observeEvent(input$run_group, {
    req(rv$pipe)
    cutoff <- if (input$hff_method == "custom") input$hff_cutoff else NULL
    rv$pipe$prod <- assign_hff_lff(rv$pipe$prod, input$hff_method, cutoff)
    showNotification("组间分析完成", type = "message")
  })
  output$hff_tbl <- DT::renderDataTable({
    req(rv$pipe)
    rv$pipe$prod %>% group_by(Feed_Frequency_Group) %>%
      summarise(N = n(), TFB_Day = round(mean(TFB_Day, na.rm = TRUE), 2),
                FI_Day_g = round(mean(FI_Day_g, na.rm = TRUE), 1),
                ADG_g = round(mean(ADG_g, na.rm = TRUE), 1),
                FCR = round(mean(FCR, na.rm = TRUE), 2),
                RFI = round(mean(RFI, na.rm = TRUE), 2), .groups = "drop") %>% DT_fast(10)
  })
  p_rhythm_daily_plot <- reactive({
    req(rv$pipe)
    dat <- rv$pipe$rhythm$daily
    ggplot(dat, aes(x = Date, y = Mean_Bouts_Per_Duck)) +
      geom_line(color = PAL_RPBG[2], linewidth = 1) + geom_point(color = PAL_RPBG[2]) +
      theme_bw(base_size = 13) + labs(x = "日期", y = "平均访饲次数/只/天")
  })
  output$rhythm_daily_plot <- renderPlot({ p_rhythm_daily_plot() })
  register_plot_dl(output, "rhythm_daily_plot", p_rhythm_daily_plot, 10, 6)
  p_rhythm_hourly_plot <- reactive({
    req(rv$pipe)
    dat <- rv$pipe$rhythm$hourly
    n_week <- n_distinct(dat$Week_Number)
    ggplot(dat, aes(x = Hour_Block, y = Mean_Bouts_Per_Duck,
                    color = factor(Week_Number), group = factor(Week_Number))) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_color_manual(values = rep(PAL_RPBG, length.out = n_week)) +
      scale_x_continuous(breaks = 0:23) +
      theme_bw(base_size = 13) +
      labs(x = "时刻 (h)", y = "平均访饲次数/只", color = "周次")
  })
  output$rhythm_hourly_plot <- renderPlot({ p_rhythm_hourly_plot() })
  register_plot_dl(output, "rhythm_hourly_plot", p_rhythm_hourly_plot, 10, 6)
  p_weekly_bouts_plot <- reactive({
    req(rv$pipe)
    dat <- rv$pipe$rhythm$weekly
    weeks_present <- sort(unique(dat$Week_Number))
    week_colors <- setNames(rep(PAL_RPBG, length.out = length(weeks_present)),
                            as.character(weeks_present))
    ggplot(dat, aes(x = Date, y = Mean_Bouts_Per_Duck,
                    group = factor(Week_Number), color = factor(Week_Number))) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = week_colors) +
      theme_bw() +
      labs(x = "日期", y = "平均访饲次数/只", color = "周次")
  })
  output$weekly_bouts_plot <- renderPlot({ p_weekly_bouts_plot() })
  register_plot_dl(output, "weekly_bouts_plot", p_weekly_bouts_plot, 10, 6)
  output$weekly_fcr_tbl <- DT::renderDataTable({
    req(rv$pipe); rv$pipe$weekly_fcr %>%
      mutate(Weekly_FCR = round(Weekly_FCR, 2)) %>% DT_fast(10)
  })
  p_weekly_fcr_plot <- reactive({
    req(rv$pipe)
    make_violin_week(rv$pipe$weekly_fcr, "Weekly_FCR", "每周 FCR")
  })
  output$weekly_fcr_plot <- renderPlot({ p_weekly_fcr_plot() })
  register_plot_dl(output, "weekly_fcr_plot", p_weekly_fcr_plot, 10, 6)
  output$group_status <- renderUI({
    if (is.null(rv$pipe)) return(NULL)
    cutoff <- attr(rv$pipe$prod, "hff_cutoff")
    tags$div(class = "alert alert-info",
             paste0("HFF/LFF 分组阈值：", round(cutoff, 1), " 次/天"))
  })
  p_hff_plot <- reactive({
    req(rv$pipe)
    dat <- rv$pipe$prod %>%
      select(Animal_ID, Feed_Frequency_Group, any_of(c("TFB", "FR_g_sec", "FCR", "RFI"))) %>%
      filter(!is.na(Feed_Frequency_Group)) %>%
      pivot_longer(cols = c(TFB, FR_g_sec, FCR, RFI),
                   names_to = "Indicator", values_to = "Value") %>%
      filter(!is.na(Value), is.finite(Value))
    validate(need(nrow(dat) > 0, "请先运行『组间分析』完成 HFF/LFF 分组。"))
    group_n <- rv$pipe$prod %>%
      distinct(Animal_ID, Feed_Frequency_Group) %>%
      count(Feed_Frequency_Group) %>%
      mutate(Label = paste0(Feed_Frequency_Group, " (n=", n, ")"))
    plot_dat <- dat %>%
      left_join(group_n %>% select(Feed_Frequency_Group, Label),
                by = "Feed_Frequency_Group") %>%
      mutate(Group_Label = factor(Label, levels = group_n$Label))
    fill_map <- c("HFF" = PAL_RPBG[1], "LFF" = PAL_RPBG[2])
    ggplot(plot_dat, aes(x = Group_Label, y = Value, fill = Feed_Frequency_Group)) +
      geom_violin(color = NA, alpha = 0.85, trim = TRUE,
                  scale = "width", adjust = 1.15) +
      geom_boxplot(width = 0.16, fill = "white", color = "#666666",
                   outlier.shape = NA, linewidth = 0.55) +
      stat_summary(fun = median, geom = "point", shape = 95,
                   size = 7, color = "#555555") +
      facet_wrap(~ Indicator, scales = "free_y") +
      scale_fill_manual(values = fill_map, guide = "none") +
      theme_bw(base_size = 13) +
      labs(x = "采食频率分组", y = "指标值") +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank())
  })
  output$hff_plot <- renderPlot({ p_hff_plot() })
  register_plot_dl(output, "hff_plot", p_hff_plot, 10, 6)

  # ---------- 模块④ 创新行为指标 ----------
  observeEvent(input$run_innovation, {
    req(rv$pipe)
    bout <- rv$pipe$bout
    daynight <- compute_daynight_v5(bout, input$day_start, input$day_end)
    cv <- compute_behavior_cv_v5(bout)
    cosinor <- compute_cosinor_v5(bout)
    fano <- compute_fano_v5(bout)
    prod2 <- rv$pipe$prod %>%
      left_join(daynight, by = "Animal_ID") %>%
      left_join(cv, by = "Animal_ID") %>%
      left_join(cosinor, by = "Animal_ID") %>%
      left_join(fano, by = "Animal_ID")
    rv$pipe$prod <- prod2
    showNotification("创新指标计算完成", type = "message")
  })
  output$daynight_tbl <- DT::renderDataTable({
    req(rv$pipe); rv$pipe$prod %>% select(Animal_ID, Day_FI_g, Night_FI_g, Day_Bouts,
                                          Night_Bouts, Day_FI_Ratio, Day_Bout_Ratio) %>%
      mutate(across(where(is.numeric), ~round(.x, 3))) %>% DT_fast(50)
  })
  output$cv_tbl <- DT::renderDataTable({
    req(rv$pipe); rv$pipe$prod %>% select(Animal_ID, CV_Duration, CV_FR, Robust_CV_IMI,
                                          CV_Daily_Bouts, CV_Daily_FI, CV_Daily_TFD) %>%
      mutate(across(where(is.numeric), ~round(.x, 3))) %>% DT_fast(50)
  })
  output$cosinor_tbl <- DT::renderDataTable({
    req(rv$pipe); rv$pipe$prod %>% select(Animal_ID, Cosinor_M, Cosinor_A, Peak_Hour, Cosinor_R2) %>%
      mutate(across(where(is.numeric), ~round(.x, 3))) %>% DT_fast(50)
  })
  output$fano_tbl <- DT::renderDataTable({
    req(rv$pipe); rv$pipe$prod %>% select(Animal_ID, Fano) %>%
      mutate(across(where(is.numeric), ~round(.x, 3))) %>% DT_fast(50)
  })
  output$inno_corr_tbl <- DT::renderDataTable({
    req(rv$pipe)
    innov_vars <- c("Day_FI_Ratio", "Day_Bout_Ratio", "CV_Duration", "CV_FR",
                    "CV_Daily_Bouts", "CV_Daily_FI", "Cosinor_A", "Fano")
    dat <- rv$pipe$prod
    innov_vars <- innov_vars[innov_vars %in% names(dat)]
    res <- expand.grid(Innovation = innov_vars, Performance = "FCR", stringsAsFactors = FALSE)
    res$Spearman_r <- NA_real_; res$P_value <- NA_real_; res$N <- NA_integer_
    for (i in seq_len(nrow(res))) {
      tmp <- dat %>% select(x = all_of(res$Innovation[i]), y = all_of("FCR")) %>%
        filter(is.finite(x), is.finite(y))
      if (nrow(tmp) >= 5) {
        t <- tryCatch(cor.test(tmp$x, tmp$y, method = "spearman", exact = FALSE),
                      error = function(e) NULL)
        if (!is.null(t)) {
          res$Spearman_r[i] <- unname(t$estimate); res$P_value[i] <- t$p.value; res$N[i] <- nrow(tmp)
        }
      }
    }
    res %>% mutate(Spearman_r = round(Spearman_r, 3), P_value = format_p(P_value)) %>% DT_fast(50)
  })
  output$innovation_status <- renderUI({
    if (is.null(rv$pipe) || !("Day_FI_Ratio" %in% names(rv$pipe$prod))) return(NULL)
    tags$div(class = "alert alert-info", "创新行为指标已并入个体表，可在模块⑤用作指示性状。")
  })

  # ---- 模块④ 创新指标×FCR 可视化（v4.4 照搬，红紫蓝绿配色）----
  p_daynight_scatter <- reactive({
    req(rv$pipe)
    dat <- rv$pipe$prod %>% filter(is.finite(Day_FI_Ratio), is.finite(FCR))
    validate(need(nrow(dat) >= 5, "请先运行『计算创新指标』，且存在昼夜分配与 FCR 数据。"))
    cor_res <- tryCatch(cor.test(dat$Day_FI_Ratio, dat$FCR, method = "spearman", exact = FALSE),
                        error = function(e) NULL)
    sub_txt <- if (!is.null(cor_res)) {
      paste0("Spearman r = ", round(cor_res$estimate, 3),
             ", P = ", format_p(cor_res$p.value), ", n = ", nrow(dat))
    } else paste0("n = ", nrow(dat))
    ggplot(dat, aes(x = Day_FI_Ratio, y = FCR)) +
      geom_point(size = 2.5, alpha = 0.7, color = PAL_RPBG[4]) +
      geom_smooth(method = "lm", se = TRUE, color = PAL_RPBG[1], fill = "#fdd0a2", alpha = 0.3) +
      theme_bw(base_size = 13) +
      labs(x = "白天采食量占比 (Day / Total)", y = "FCR", subtitle = sub_txt) +
      theme(plot.subtitle = element_text(size = 11, color = "#666666"))
  })
  output$daynight_scatter <- renderPlot({ p_daynight_scatter() })
  register_plot_dl(output, "daynight_scatter", p_daynight_scatter, 10, 6)
  p_cv_fcr_scatter <- reactive({
    req(rv$pipe)
    plot_dat <- rv$pipe$prod %>%
      select(Animal_ID, FCR, CV_Duration, CV_FR, Robust_CV_IMI,
             CV_Daily_Bouts, CV_Daily_FI, CV_Daily_TFD) %>%
      pivot_longer(cols = -c(Animal_ID, FCR),
                   names_to = "CV_Type", values_to = "CV_Value") %>%
      filter(is.finite(CV_Value), is.finite(FCR))
    validate(need(nrow(plot_dat) >= 5, "请先运行『计算创新指标』，且存在 CV 与 FCR 数据。"))
    anno <- plot_dat %>%
      group_by(CV_Type) %>%
      summarise(r = tryCatch(cor(CV_Value, FCR, method = "spearman",
                                 use = "complete.obs"),
                             error = function(e) NA_real_),
                N = n(), .groups = "drop") %>%
      mutate(Label = paste0("r = ", round(r, 2), " (n=", N, ")"))
    ggplot(plot_dat, aes(x = CV_Value, y = FCR)) +
      geom_point(size = 1.8, alpha = 0.5, color = PAL_RPBG[4]) +
      geom_smooth(method = "lm", se = FALSE, color = PAL_RPBG[1], linewidth = 0.9) +
      facet_wrap(~ CV_Type, scales = "free_x", ncol = 3) +
      geom_text(data = anno, aes(x = -Inf, y = Inf, label = Label),
                hjust = -0.05, vjust = 1.2, size = 3.5, color = "#333333",
                inherit.aes = FALSE) +
      theme_bw(base_size = 12) +
      labs(x = "CV", y = "FCR", subtitle = "每个子图：Spearman r 与样本量") +
      theme(plot.subtitle = element_text(size = 10, color = "#666666"),
            strip.background = element_rect(fill = "#e6f2ff"),
            strip.text = element_text(size = 10, face = "bold"))
  })
  output$cv_fcr_scatter <- renderPlot({ p_cv_fcr_scatter() })
  register_plot_dl(output, "cv_fcr_scatter", p_cv_fcr_scatter, 10, 6)
  p_cosinor_scatter <- reactive({
    req(rv$pipe)
    dat <- rv$pipe$prod %>% filter(is.finite(Cosinor_A), is.finite(FCR))
    validate(need(nrow(dat) >= 5, "请先运行『计算创新指标』，且存在余弦拟合与 FCR 数据。"))
    cor_res <- tryCatch(cor.test(dat$Cosinor_A, dat$FCR, method = "spearman", exact = FALSE),
                        error = function(e) NULL)
    sub_txt <- if (!is.null(cor_res)) {
      paste0("Spearman r = ", round(cor_res$estimate, 3),
             ", P = ", format_p(cor_res$p.value), ", n = ", nrow(dat))
    } else paste0("n = ", nrow(dat))
    ggplot(dat, aes(x = Cosinor_A, y = FCR)) +
      geom_point(size = 2.5, alpha = 0.7, color = PAL_RPBG[3]) +
      geom_smooth(method = "lm", se = TRUE, color = PAL_RPBG[1], fill = "#fdd0a2", alpha = 0.3) +
      theme_bw(base_size = 13) +
      labs(x = "余弦节律幅度 A", y = "FCR", subtitle = sub_txt) +
      theme(plot.subtitle = element_text(size = 11, color = "#666666"))
  })
  output$cosinor_scatter <- renderPlot({ p_cosinor_scatter() })
  register_plot_dl(output, "cosinor_scatter", p_cosinor_scatter, 10, 6)
  p_phase_plot <- reactive({
    req(rv$pipe)
    dat <- rv$pipe$prod %>% filter(is.finite(Peak_Hour))
    validate(need(nrow(dat) >= 5, "请先运行『计算创新指标』，且存在峰值时刻数据。"))
    ggplot(dat, aes(x = Peak_Hour)) +
      geom_histogram(bins = 24, fill = PAL_RPBG[5], color = "white", boundary = 0) +
      scale_x_continuous(breaks = 0:23, limits = c(0, 24)) +
      theme_bw(base_size = 13) +
      labs(x = "采食峰值时刻 (h)", y = "个体数", title = "个体采食峰值时刻分布")
  })
  output$phase_plot <- renderPlot({ p_phase_plot() })
  register_plot_dl(output, "phase_plot", p_phase_plot, 10, 6)

  # ---------- 模块⑤ 遗传评估与留种 ----------
  genetic_cfg <- reactive({
    traits <- c(input$index_traits_eff %||% character(0),
                input$index_traits_feeding %||% character(0),
                input$index_traits_rhythm %||% character(0))
    weights <- sapply(traits, function(tr) input[[paste0("w_", tr)]] %||% 0.2, USE.NAMES = TRUE)
    weights <- weights[is.finite(as.numeric(weights))]
    # 权重自动归一化：总和=1（归一化不改变排名与留种结果，只统一量纲）
    if (length(weights) > 0) {
      w_names <- names(weights)
      weights <- as.numeric(weights) / sum(as.numeric(weights), na.rm = TRUE)
      names(weights) <- w_names
    } else weights <- NULL
    h2_by_trait <- sapply(traits, function(tr) input[[paste0("h2_", tr)]] %||% 0.3,
                          USE.NAMES = TRUE)
    list(
      target_trait = input$target_trait %||% "FCR",
      indicator_min_r = input$indicator_min_r,
      indicator_max_n = as.integer(input$indicator_max_n),
      use_pedigree = isTRUE(as.logical(input$use_pedigree)),
      use_reml_h2 = isTRUE(input$use_reml_h2),
      index_traits = traits,
      weights = if (length(weights) > 0) weights else NULL,
      h2_by_trait = h2_by_trait,
      fixed_effects = input$fixed_effects %||% "Sex",
      retention_ratio = input$retention_ratio / 100,
      sex_balance = isTRUE(input$sex_balance)
    )
  })

  # 动态权重/h² 输入：跟随勾选性状
  output$index_weights_ui <- renderUI({
    traits <- c(input$index_traits_eff %||% character(0),
                input$index_traits_feeding %||% character(0),
                input$index_traits_rhythm %||% character(0))
    if (length(traits) == 0) {
      return(tags$div(class = "alert alert-warning", "请至少勾选一个参与指数的性状。"))
    }
    lit <- literature_params()
    h2_defaults <- setNames(lit$h2, lit$Trait)
    w_defaults <- c(FCR = 0.5, RFI = 0.3, ADG_g = 0.3, FBW_kg = 0.2, FI_Day_g = 0.2,
                    FR_g_sec = 0.2, TFB_Day = 0.2, AMS_g = 0.2, IBW_kg = 0.2)
    dirs <- setNames(trait_dir(traits), traits)
    labels <- setNames(trait_label(traits), traits)
    rows <- lapply(traits, function(tr) {
      h2v <- if (tr %in% names(h2_defaults)) h2_defaults[[tr]] else 0.3
      has_lit <- tr %in% names(h2_defaults)
      # 标注跟随"自动实测"勾选状态：勾了 → 实测优先；未勾 → 区分有无文献
      h2_hint <- if (isTRUE(input$use_reml_h2)) "（实测优先）"
                 else if (has_lit) "（文献 h²）" else "（无文献，默认0.3，可改）"
      dir_txt <- if (isTRUE(dirs[[tr]] < 0)) "↓ 越低越好" else "↑ 越高越好"
      tagList(
        fluidRow(
          column(6, numericInput(paste0("w_", tr), paste0(tr, " 权重"),
                                 value = if (tr %in% names(w_defaults)) w_defaults[[tr]] else 0.2,
                                 min = 0, step = 0.05)),
          column(6, numericInput(paste0("h2_", tr), paste0(tr, " h²", h2_hint),
                                 value = round(h2v, 2), min = 0.05, max = 0.95, step = 0.01))
        ),
        helpText(paste0(labels[[tr]], " · 方向：", dir_txt, "（指数已按方向加权，权重填正数）"))
      )
    })
    do.call(tagList, rows)
  })

  observeEvent(input$run_genetic, {
    req(rv$pipe)
    cfg <- genetic_cfg()
    withProgress(message = "遗传评估与留种计算中…", value = 0.4, {
      rr <- tryCatch(run_retention_module(rv$pipe$prod, rv$pipe$link, cfg),
                     error = function(e) e)
      if (inherits(rr, "error")) {
        showNotification(paste0("遗传评估失败：", conditionMessage(rr)),
                         type = "error", duration = 10)
        return()
      }
      for (w in rr$warnings) showNotification(w, type = "warning", duration = 8)
      if (!is.null(rr$error)) {
        showNotification(rr$error, type = "error", duration = 10)
        return()
      }
      rv$pipe$indicator_traits <- rr$indicator_traits
      rv$pipe$genetic <- rr$genetic
      rv$pipe$retention <- rr$retention
      rv$pipe$key_metrics <- rr$key_metrics
      rv$pipe$genetic$run_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      if (!is.null(rr$genetic$h2_est)) {
        rv$pipe$h2_est <- rr$genetic$h2_est
        # 实测成功后，把③中对应性状的 h² 输入框回填为实测值
        for (i in seq_len(nrow(rr$genetic$h2_est))) {
          tr_i <- as.character(rr$genetic$h2_est$Trait[i])
          updateNumericInput(session, paste0("h2_", tr_i),
                             value = round(as.numeric(rr$genetic$h2_est$h2[i]), 2))
        }
        showNotification("已用本场实测遗传力更新③中的 h² 输入框（可再手动微调）",
                         type = "message", duration = 8)
      }
      # 运行成功后，把③中权重输入框回写为归一化后的值（总和=1），便于复核与微调
      if (length(cfg$weights) > 0) {
        for (tr_n in names(cfg$weights)) {
          updateNumericInput(session, paste0("w_", tr_n),
                             value = round(as.numeric(cfg$weights[[tr_n]]), 3))
        }
        showNotification("③权重已归一化并回填输入框（总和=1，归一化不改变排名）",
                         type = "message", duration = 6)
      }
      incProgress(1, detail = "完成")
    })
  })

  # 一键把①中筛出的指示性状带入③的选择指数
  observeEvent(input$use_indicators, {
    req(rv$pipe)
    cfg <- genetic_cfg()
    ind <- select_indicator_traits(rv$pipe$prod, cfg$target_trait,
                                   cfg$indicator_min_r, cfg$indicator_max_n)
    if (nrow(ind) == 0) {
      showNotification("未筛出指示性状：请确认目标性状本场已测量，或降低 |r| 阈值。",
                       type = "warning", duration = 8)
      return()
    }
    keep <- intersect(unique(ind$Indicator), names(rv$pipe$prod))
    keep <- intersect(keep, INDEX_TRAIT_VALUES)
    if (length(keep) == 0) {
      showNotification("筛出的指示性状均不在可选指数性状中，请手动在③中勾选。",
                       type = "warning", duration = 8)
      return()
    }
    rv$pipe$indicator_traits <- ind
    updateCheckboxGroupInput(session, "index_traits_eff",
                             selected = intersect(keep, INDEX_TRAIT_CHOICES[["效率与增重"]]))
    updateCheckboxGroupInput(session, "index_traits_feeding",
                             selected = intersect(keep, INDEX_TRAIT_CHOICES[["采食行为"]]))
    updateCheckboxGroupInput(session, "index_traits_rhythm",
                             selected = intersect(keep, INDEX_TRAIT_CHOICES[["节律与稳定性"]]))
    showNotification(paste0("已用指示性状 [", paste(keep, collapse = ", "),
                            "] 构建指数（权重可用默认值再微调）。"),
                     type = "message", duration = 8)
  })

  output$key_metrics_tbl <- DT::renderDataTable({
    req(rv$pipe$key_metrics); rv$pipe$key_metrics %>% DT_fast(20)
  })
  output$indicator_tbl <- DT::renderDataTable({
    req(rv$pipe$indicator_traits)
    dat <- rv$pipe$indicator_traits
    if (nrow(dat) == 0) return(dat)
    dat <- dat %>%
      mutate(Spearman_r = round(Spearman_r, 3), P = format_p(P))
    # 目标性状未测、走文献 rG 代理时：Spearman_r 为 NA，改显示文献遗传相关与来源
    if ("Genetic_r" %in% names(dat) && all(is.na(dat$Spearman_r))) {
      dat <- dat %>%
        mutate(Spearman_r = NULL, P = NULL,
               rG = round(Genetic_r, 3),
               rG_Source = rG_Source) %>%
        select(Indicator, Target, rG, rG_Source, Abs_r)
    }
    dat %>% DT_fast(20)
  })
  p_indicator_plot <- reactive({
    req(rv$pipe$indicator_traits)
    dat <- rv$pipe$indicator_traits
    if (nrow(dat) == 0) return(NULL)
    if ("Genetic_r" %in% names(dat) && all(is.na(dat$Spearman_r))) {
      ggplot(dat, aes(x = reorder(Indicator, Genetic_r), y = Genetic_r, fill = Abs_r >= 0.3)) +
        geom_col() + coord_flip() + theme_bw(base_size = 13) +
        scale_fill_manual(values = c("TRUE" = PAL_RPBG[3], "FALSE" = PAL_RPBG[2]), guide = "none") +
        labs(x = NULL, y = "文献遗传相关 rG（与目标性状）")
    } else {
      ggplot(dat, aes(x = reorder(Indicator, Spearman_r), y = Spearman_r, fill = Abs_r >= 0.3)) +
        geom_col() + coord_flip() + theme_bw(base_size = 13) +
        scale_fill_manual(values = c("TRUE" = PAL_RPBG[3], "FALSE" = PAL_RPBG[2]), guide = "none") +
        labs(x = NULL, y = "Spearman r（与目标性状）")
    }
  })
  output$indicator_plot <- renderPlot({ p_indicator_plot() })
  register_plot_dl(output, "indicator_plot", p_indicator_plot, 10, 6)
  output$ebv_tbl <- DT::renderDataTable({
    req(rv$pipe$genetic)
    dat <- rv$pipe$genetic$ebv_tbl
    if (is.null(dat)) return(NULL)
    dat %>% mutate(across(where(is.numeric) & !any_of("EBV"), ~round(.x, 3)),
                   EBV = round(EBV, 4)) %>% DT_fast(50)
  })
  output$retain_tbl <- DT::renderDataTable({
    req(rv$pipe$genetic$retention)
    rv$pipe$genetic$retention$retained %>%
      mutate(across(where(is.numeric), ~round(.x, 2))) %>% DT_fast(50)
  })
  p_index_plot <- reactive({
    req(rv$pipe$genetic$index)
    idx <- rv$pipe$genetic$index
    keep_ids <- rv$pipe$genetic$retention$retained$Animal_ID
    idx$Is_Retained <- idx$Animal_ID %in% keep_ids
    thr <- min(idx$Index[idx$Is_Retained], na.rm = TRUE)
    ratio_pct <- round(rv$pipe$genetic$retention$ratio * 100)
    ggplot(idx, aes(x = Index, fill = Is_Retained)) +
      geom_histogram(bins = 40, alpha = 0.85, color = "white", linewidth = 0.2) +
      geom_vline(xintercept = thr, linetype = "dashed", color = "black") +
      scale_fill_manual(values = c("TRUE" = PAL_RPBG[1], "FALSE" = PAL_RPBG[2]), name = "是否留种") +
      theme_bw(base_size = 13) +
      labs(x = "选择指数", y = "个体数",
           caption = paste0("虚线 = 留种阈值（前 ", ratio_pct, "%）"))
  })
  output$index_plot <- renderPlot({ p_index_plot() })
  register_plot_dl(output, "index_plot", p_index_plot, 10, 6)
  output$lit_tbl <- DT::renderDataTable({
    literature_params() %>% mutate(h2 = round(h2, 2)) %>% DT_fast(20)
  })
  # 附表·文献遗传相关 rG：目标未测时用于匹配代理指示性状，含屠宰性状相关数据
  output$lit_corr_tbl <- DT::renderDataTable({
    literature_corr() %>%
      mutate(Genetic_r = round(Genetic_r, 3), SE = round(SE, 3)) %>%
      rename(目标性状 = Target, 代理指示性状 = Indicator, rG = Genetic_r,
             标准误 = SE, 物种 = Species, 来源 = Source) %>%
      DT_fast(30)
  })
  output$corr_hint <- renderUI({
    req(rv$pipe)
    tags$div(class = "alert alert-info",
             HTML(paste0(
               "<b>表型相关热图说明</b><br>",
               "展示 prod 表中主要表型指标两两间的 Spearman 相关系数（颜色越深相关越强）。<br>",
               "用途：①识别与目标性状高相关的『指示性状』；②发现冗余指标（|r|≈1 可精简）；",
               "③为选择指数权重设计提供依据。展示字段：",
               paste(intersect(c("TFB_Day","FI_Day_g","AMS_g","TFD_sec","AFBD_sec",
                                 "IMI_sec","FR_g_sec","IBW_kg","FBW_kg","ADG_g",
                                 "ADFI_g","FCR","RFI"), names(rv$pipe$prod)), collapse = "、"))))
  })
  p_corr_heatmap <- reactive({
    req(rv$pipe)
    cols <- intersect(c("TFB_Day","FI_Day_g","AMS_g","TFD_sec","AFBD_sec",
                        "IMI_sec","FR_g_sec","IBW_kg","FBW_kg","ADG_g",
                        "ADFI_g","FCR","RFI"), names(rv$pipe$prod))
    if (length(cols) < 3) return(NULL)
    dat <- rv$pipe$prod %>% select(all_of(cols)) %>%
      mutate(across(everything(), as.numeric))
    corm <- cor(dat, method = "spearman", use = "pairwise.complete.obs")
    corm_long <- as.data.frame(as.table(corm))
    names(corm_long) <- c("Var1", "Var2", "r")
    ggplot(corm_long, aes(x = Var1, y = Var2, fill = r)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
      scale_fill_gradient2(low = "#4575b4", mid = "white", high = "#d73027",
                           midpoint = 0, limits = c(-1, 1)) +
      theme_bw(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = NULL, y = NULL, fill = "Spearman r",
           title = "表型相关热图（Spearman）")
  })
  output$corr_heatmap <- renderPlot({ p_corr_heatmap() })
  register_plot_dl(output, "corr_heatmap", p_corr_heatmap, 10, 6)
  output$lit_hint <- renderUI({
    g <- rv$pipe$genetic
    target <- isolate(input$target_trait) %||% "FCR"
    if (is.null(g)) {
      txt <- paste0(
        "<b>尚未运行遗传评估。</b><br>",
        "请依次设置①育种目标 → ②评估路线 → ③指数性状与权重，再点④的『运行遗传评估与留种』。",
        "清洗阶段已完成个体指标与指示性状筛选（轻量计算），重型评估在点击运行时才执行。")
    } else if (g$mode == "PBLUP") {
      txt <- paste0(
        "<b>当前评估模式：", g$mode, "</b><br>",
        "已用本场系谱构建亲缘矩阵（A 矩阵），对③中所选性状逐个建立动物模型估计育种值（EBV）。",
        "指数已按性状方向自动加权（FCR/RFI/采食量越低越好，增重/终重越高越好）。")
    } else {
      txt <- paste0(
        "<b>当前评估模式：", g$mode, "</b><br>",
        "未使用系谱，改用文献遗传参数构建选择指数（来源：Li et al. 2020 Poultry Science; ",
        "Zhang et al. 2017 AJAS; Cai et al. 2023 JASB）。指数同样已按性状方向自动加权。",
        "注：文献参数仅为参考，有系谱时建议改用路线A。")
    }
    if (any(target %in% c("SkinFat_Rate", "AbFat_Rate", "BMP"))) {
      slaughter_targets <- target[target %in% c("SkinFat_Rate", "AbFat_Rate", "BMP")]
      txt <- paste0(txt, "<br><b>『", paste(slaughter_targets, collapse = " / "),
                    "』本场未测：</b>已按文献遗传相关自动匹配代理指示性状（见①表与『附表·文献遗传相关 rG』），",
                    "留种由下方勾选的活体性状完成。")
    }
    tags$div(class = "alert alert-info", HTML(txt))
  })
  output$genetic_status <- renderUI({
    g <- rv$pipe$genetic
    if (is.null(g) || is.null(g$index)) return(NULL)
    r <- g$retention
    txt <- paste0("评估完成：模式=", g$mode, "，参与个体=", nrow(g$index))
    if (!is.null(g$run_at)) {
      txt <- paste0(txt, "（运行于 ", g$run_at, "）")
    }
    if (!is.null(r)) {
      txt <- paste0(txt, "，留种=", r$n_keep, " 只（目标 ",
                    sprintf("%.0f%%", r$ratio * 100),
                    if (isTRUE(r$sex_balanced)) "，已性别均衡" else "", "）")
    }
    if (!is.null(g$h2_est)) {
      txt <- paste0(txt, "<br>已实测遗传力（REML）：",
                    paste(g$h2_est$Trait, "=",
                          sprintf("%.2f", g$h2_est$h2), collapse = "; "))
    }
    if (length(g$index_traits) > 0) {
      txt <- paste0(txt, "<br>指数性状：", paste(g$index_traits, collapse = ", "),
                    "（方向已自动加权）")
    }
    tags$div(class = "alert alert-success", HTML(txt))
  })

  output$h2_est_panel <- renderUI({
    if (is.null(rv$pipe$h2_est) || nrow(rv$pipe$h2_est) == 0) {
      return(tags$div(class = "alert alert-warning",
        "本次运行未进行本场遗传力实测（无系谱或未勾选『自动实测』时为空）。",
        "当前使用的 h² 见③各性状输入框与『附表·文献参数表』。"))
    }
    tagList(
      DT::dataTableOutput("h2_est_tbl"),
      plot_box_ui("h2_est_plot", "420px"))
  })
  output$h2_est_tbl <- DT::renderDataTable({
    req(rv$pipe$h2_est)
    rv$pipe$h2_est %>%
      mutate(h2 = round(h2, 4), sigma2_a = round(sigma2_a, 4),
             sigma2_e = round(sigma2_e, 4)) %>% DT_fast(50)
  })
  p_h2_est_plot <- reactive({
    req(rv$pipe$h2_est)
    dat <- rv$pipe$h2_est
    lit <- literature_params()
    dat <- dat %>% left_join(lit %>% select(Trait, Lit_h2 = h2), by = "Trait")
    p <- ggplot(dat, aes(x = reorder(Trait, h2), y = h2)) +
      geom_col(aes(fill = Converged), width = 0.6) +
      geom_point(aes(y = Lit_h2), shape = 17, size = 3, color = PAL_RPBG[2],
                 na.rm = TRUE) +
      scale_fill_manual(values = c("TRUE" = PAL_RPBG[3], "FALSE" = PAL_RPBG[1])) +
      coord_flip() + theme_bw(base_size = 13) +
      labs(x = NULL, y = "遗传力 h²", fill = "已收敛",
           title = "本场实测遗传力（REML），▲=文献参考值")
    p
  })
  output$h2_est_plot <- renderPlot({ p_h2_est_plot() })
  register_plot_dl(output, "h2_est_plot", p_h2_est_plot, 10, 6)

  # ---------- 模块⑥ AI 智能选种探索 ----------
  output$ml_hint <- renderUI({
    tags$div(
      class = "alert alert-info",
      HTML(paste0(
        "<b>AI 智能选种探索模块（辅助层，不进留种主链路）</b><br>",
        "1. <b>采食模式聚类</b>：K-means（Silhouette 定 K），TFB 最高簇判为 HFF，",
        "与模块③『TFB 中位数分组』并列对照——两种方法口径不同，此处仅作探索对比。<br>",
        "2. <b>随机森林 + XGBoost</b>：对每个目标性状做特征重要性排序与预测评价，",
        "与模块⑤『表型相关筛选』交叉验证，识别非线性关联。<br>",
        "3. <b>多目标融合</b>：三目标 RF 重要性归一化后按目标权重加权（默认 0.33/0.33/0.34）。<br>",
        "4. <b>稳定性验证</b>：10 次随机重复 RF，输出重要性均值/SD/Top10 频率。<br>",
        "5. <b>最终评分与报告</b>：Trait_score×0.7 + StabilityScore×0.3，",
        "自动分级（核心/重点关注/辅助）并生成育种报告 Excel。")))
  })
  observeEvent(input$run_ml, {
    req(rv$pipe)
    targets <- input$ml_targets
    if (is.null(targets) || length(targets) == 0) {
      showNotification("请至少选择一个预测目标性状", type = "error")
      return()
    }
    w_target <- c(ADG_g = as.numeric(input$ml_w_adg),
                  FCR = as.numeric(input$ml_w_fcr),
                  RFI = as.numeric(input$ml_w_rfi))[targets]
    w_imp <- as.numeric(input$ml_w_imp)
    w_stab <- as.numeric(input$ml_w_stab)
    if (is.na(w_imp) || is.na(w_stab)) { w_imp <- 0.7; w_stab <- 0.3 }
    withProgress(message = "AI 智能选种分析运行中…", value = 0.1, {
      rv$ml <- ml_run_all(rv$pipe$prod, targets, w_target = w_target,
                          w_imp = w_imp, w_stab = w_stab)
      incProgress(1, detail = "完成")
    })
    if (!is.null(rv$ml$error)) {
      showNotification(rv$ml$error, type = "error")
    } else {
      showNotification("AI 智能选种分析完成", type = "message")
    }
  })
  output$ml_status <- renderUI({
    if (is.null(rv$ml)) return(NULL)
    if (!is.null(rv$ml$error)) {
      return(tags$div(class = "alert alert-warning", rv$ml$error))
    }
    feats <- length(rv$ml$features)
    n_final <- nrow(rv$ml$final)
    n_core <- sum(rv$ml$report$Recommendation_Level == "核心指标", na.rm = TRUE)
    tags$div(class = "alert alert-success",
             paste0("行为特征 ", feats, " 个；聚类最优 K = ",
                    rv$ml$cluster$best_k, "；综合评分 ", n_final,
                    " 个指标，其中核心指标 ", n_core, " 个。"))
  })
  output$ml_cluster_tbl <- DT::renderDataTable({
    req(rv$ml, rv$ml$cluster$result)
    rv$ml$cluster$result %>%
      select(Animal_ID, Cluster, Feeding_Pattern, Feed_Frequency_Group,
             TFB, FI_g, AMS_g) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>% DT_fast(input$ind_max_n)
  })
  output$ml_cluster_compare_tbl <- DT::renderDataTable({
    req(rv$ml, rv$ml$cluster$compare)
    rv$ml$cluster$compare %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>% DT_fast(20)
  })
  output$ml_importance_tbl <- DT::renderDataTable({
    req(rv$ml, rv$ml$fusion)
    rv$ml$fusion %>% DT_fast(50)
  })
  output$ml_eval_tbl <- DT::renderDataTable({
    req(rv$ml, rv$ml$targets)
    evals <- lapply(rv$ml$targets, function(x) x$evaluation)
    evals <- evals[!sapply(evals, is.null)]
    if (length(evals) == 0) return(NULL)
    bind_rows(evals) %>% DT_fast(20)
  })
  output$ml_final_tbl <- DT::renderDataTable({
    req(rv$ml, rv$ml$final)
    rv$ml$final %>%
      select(Rank, Feature, Trait_score, StabilityScore, FinalScore,
             Mean_Rank, Top10_Frequency) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>% DT_fast(50)
  })
  output$ml_stability_tbl <- DT::renderDataTable({
    req(rv$ml, rv$ml$stability)
    rv$ml$stability %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>% DT_fast(50)
  })
  output$ml_report_hint <- renderUI({
    req(rv$ml, rv$ml$report)
    top <- rv$ml$report %>% slice_head(n = 1)
    tags$div(class = "alert alert-info",
             HTML(paste0(
               "<b>推荐指标（按 FinalScore 排序）</b><br>",
               "当前最高分指标：<b>", top$Feature, "</b>（",
               top$Trait_Category, " / ", top$Recommendation_Level, " / ",
               top$Evidence_Level, "）<br>",
               top$Biological_Interpretation)))
  })
  output$ml_report_tbl <- DT::renderDataTable({
    req(rv$ml, rv$ml$report)
    rv$ml$report %>%
      select(Rank, Feature, Trait_Category, Recommendation_Level,
             Evidence_Level, FinalScore, Biological_Interpretation,
             Breeding_Application) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>% DT_fast(50)
  })
  # 育种报告：各指标 FinalScore 排序柱状图
  output$ml_report_score_plot <- renderPlot({
    req(rv$ml, rv$ml$report)
    rep <- rv$ml$report
    rep %>%
      mutate(Feature = factor(Feature, levels = rep$Feature[order(rep$FinalScore)])) %>%
      ggplot(aes(x = Feature, y = FinalScore, fill = FinalScore)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      geom_text(aes(label = round(FinalScore, 2)), hjust = -0.15, size = 3.6) +
      scale_fill_gradient(low = PAL_RPBG[4], high = PAL_RPBG[1]) +
      coord_flip() +
      labs(x = "", y = "FinalScore",
           title = "育种报告：各指标综合评分（FinalScore 由高到低）") +
      theme_minimal(base_size = 13)
  })
  # ---- 各板块「下载 Excel」：把本板块所有表格打包成多 sheet Excel ----
  export_pipe_xlsx <- function(wb, file, label) {
    wb <- wb[!sapply(wb, is.null)]
    if (length(wb) == 0) {
      wb <- list(说明 = data.frame(
        信息 = paste0("暂无", label, "结果：请先在本板块完成数据清洗 / 分析。"),
        stringsAsFactors = FALSE))
    }
    openxlsx::write.xlsx(wb, file)
  }
  output$dl_clean_xlsx <- downloadHandler(
    filename = function() paste0("鸭芯智选_模块1_清洗结果_", Sys.Date(), ".xlsx"),
    content = function(file) {
      export_pipe_xlsx(list(
        "工作表读取" = if (!is.null(rv$pipe)) bind_rows(rv$pipe$feed_raw_qc, rv$pipe$bw_raw_qc) else NULL,
        "时间范围QC" = if (!is.null(rv$pipe)) rv$pipe$clean$datetime_qc else NULL,
        "字段缺失QC" = if (!is.null(rv$pipe)) rv$pipe$clean$na_qc else NULL,
        "周统计_采食" = if (!is.null(rv$pipe)) rv$pipe$clean$feed_week_stats else NULL,
        "周统计_体重" = if (!is.null(rv$pipe)) rv$pipe$clean$bw_week_stats else NULL),
        file, "清洗")
    })
  output$dl_ind_xlsx <- downloadHandler(
    filename = function() paste0("鸭芯智选_模块2_个体指标_", Sys.Date(), ".xlsx"),
    content = function(file) {
      export_pipe_xlsx(list(
        "采食行为" = if (!is.null(rv$pipe)) rv$pipe$feeding else NULL,
        "生产性能_全表" = if (!is.null(rv$pipe)) rv$pipe$prod else NULL),
        file, "个体指标")
    })
  output$dl_grp_xlsx <- downloadHandler(
    filename = function() paste0("鸭芯智选_模块3_组间时序_", Sys.Date(), ".xlsx"),
    content = function(file) {
      hff_tab <- NULL
      if (!is.null(rv$pipe)) {
        hff_tab <- rv$pipe$prod %>% group_by(Feed_Frequency_Group) %>%
          summarise(N = n(), TFB_Day = round(mean(TFB_Day, na.rm = TRUE), 2),
                    FI_Day_g = round(mean(FI_Day_g, na.rm = TRUE), 1),
                    ADG_g = round(mean(ADG_g, na.rm = TRUE), 1),
                    FCR = round(mean(FCR, na.rm = TRUE), 2),
                    RFI = round(mean(RFI, na.rm = TRUE), 2), .groups = "drop")
      }
      export_pipe_xlsx(list(
        "HFF_LFF对比" = hff_tab,
        "每周FCR" = if (!is.null(rv$pipe)) rv$pipe$weekly_fcr else NULL,
        "个体表_含分组" = if (!is.null(rv$pipe)) rv$pipe$prod else NULL),
        file, "组间时序")
    })
  output$dl_inno_xlsx <- downloadHandler(
    filename = function() paste0("鸭芯智选_模块4_创新行为指标_", Sys.Date(), ".xlsx"),
    content = function(file) {
      inno_tab <- NULL
      if (!is.null(rv$pipe)) {
        innov_vars <- c("Day_FI_Ratio", "Day_Bout_Ratio", "CV_Duration", "CV_FR",
                        "CV_Daily_Bouts", "CV_Daily_FI", "Cosinor_A", "Fano")
        dat <- rv$pipe$prod
        innov_vars <- innov_vars[innov_vars %in% names(dat)]
        res <- expand.grid(Innovation = innov_vars, Performance = "FCR", stringsAsFactors = FALSE)
        res$Spearman_r <- NA_real_; res$P_value <- NA_real_; res$N <- NA_integer_
        for (i in seq_len(nrow(res))) {
          tmp <- dat %>% select(x = all_of(res$Innovation[i]), y = all_of("FCR")) %>%
            filter(is.finite(x), is.finite(y))
          if (nrow(tmp) >= 5) {
            ct <- tryCatch(cor.test(tmp$x, tmp$y, method = "spearman", exact = FALSE),
                           error = function(e) NULL)
            if (!is.null(ct)) {
              res$Spearman_r[i] <- ct$estimate; res$P_value[i] <- ct$p.value; res$N[i] <- nrow(tmp)
            }
          }
        }
        inno_tab <- res
      }
      export_pipe_xlsx(list(
        "创新指标×FCR相关" = inno_tab,
        "个体表_含创新指标" = if (!is.null(rv$pipe)) rv$pipe$prod else NULL),
        file, "创新指标")
    })
  output$dl_mod5_xlsx <- downloadHandler(
    filename = function() paste0("鸭芯智选_模块5_遗传评估与留种_", Sys.Date(), ".xlsx"),
    content = function(file) {
      export_pipe_xlsx(list(
        "关键指标卡" = if (!is.null(rv$pipe$key_metrics)) rv$pipe$key_metrics else NULL,
        "指示性状" = if (!is.null(rv$pipe$indicator_traits)) rv$pipe$indicator_traits else NULL,
        "遗传力实测REML" = if (!is.null(rv$pipe$h2_est)) rv$pipe$h2_est else NULL,
        "育种值EBV" = if (!is.null(rv$pipe$genetic$ebv_tbl)) rv$pipe$genetic$ebv_tbl else NULL,
        "留种名单" = if (!is.null(rv$pipe$genetic$retention$retained)) rv$pipe$genetic$retention$retained else NULL,
        "文献参数表" = literature_params(),
        "文献遗传相关rG" = literature_corr()),
        file, "遗传评估与留种")
    })
  output$ml_report_dl <- downloadHandler(
    filename = function() paste0("duck_ai_breeding_report_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(rv$ml)
      if (!is.null(rv$ml$error)) stop("尚无可用结果，请先运行 AI 智能选种分析。")
      wb <- list(
        "Final_Ranking" = rv$ml$report,
        "Top10_Indicators" = rv$ml$top10,
        "Trait_Importance" = rv$ml$fusion,
        "Stability" = rv$ml$stability,
        "Cluster_Result" = rv$ml$cluster$result,
        "Cluster_Summary" = rv$ml$cluster$summary,
        "Silhouette" = rv$ml$cluster$silhouette,
        "Model_Evaluation" = bind_rows(lapply(rv$ml$targets, function(x) x$evaluation)),
        "Summary" = data.frame(
          Item = c("分析对象", "行为指标数量", "目标性状", "模型方法",
                   "稳定性验证", "最终筛选策略"),
          Value = c("肉鸭采食行为表型", length(rv$ml$features),
                    paste(names(rv$ml$targets), collapse = " / "),
                    "Random Forest + XGBoost",
                    "10次随机重复训练",
                    "多目标融合 + 稳定性加权评分"),
          stringsAsFactors = FALSE)
      )
      wb <- wb[!sapply(wb, is.null)]
      write.xlsx(wb, file)
    })
}

# ============================================================
# 8. 启动
# ============================================================
# ===== 命令行入口 =====
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 2) {
    library(jsonlite)
    cfg <- fromJSON(args[1])
    feed_files <- data.frame(datapath = cfg$feed_files, name = basename(cfg$feed_files))
    bw_files <- data.frame(datapath = cfg$bw_files, name = basename(cfg$bw_files))
    res <- run_pipeline_v5(feed_files, bw_files,
                           ped_path = cfg$ped_path,
                           idmap_path = cfg$idmap_path,
                           cfg = cfg$params)
    out <- list(prod = res$prod,
                indicator_traits = res$indicator_traits,
                weekly_fcr = res$weekly_fcr)
    write_json(out, cfg$output, dataframe = "rows", auto_unbox = TRUE)
  }
}
