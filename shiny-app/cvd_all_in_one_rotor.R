library(readxl)
'%!in%' <- function(x,y)!('%in%'(x,y))
# ------------------------------------------------------------
# Half-height & FWHM from melt-curve derivatives (base R only)
# ------------------------------------------------------------
halfmax_fwhm <- function(x,
                         Y,                      # numeric vector (one trace) OR data.frame/matrix (multiple columns)
                         series_names = NULL,   # optional names for columns of Y
                         spar_signal = 0.35,    # smoothing strength (0.25???0.55 typical)
                         peak_min_sep_deg = 0.8,# min separation between peaks (??C)
                         rel_prom = 0.05        # keep peaks with prominence >= rel_prom * signal range
) {
  # -- coerce Y to matrix; remember series names
  if (is.vector(Y)) {
    Y <- matrix(Y, ncol = 1)
    if (is.null(series_names)) series_names <- "y"
  } else {
    Y <- as.matrix(Y)
    if (is.null(series_names)) {
      series_names <- colnames(Y)
      if (is.null(series_names)) series_names <- paste0("y", seq_len(ncol(Y)))
    }
  }
  
  # -- clean & order x once
  stopifnot(length(x) == nrow(Y))
  ok <- is.finite(x)
  x  <- x[ok]; Y <- Y[ok, , drop = FALSE]
  o  <- order(x); x <- x[o]; Y <- Y[o, , drop = FALSE]
  
  # -- collapse duplicate x by averaging (helps smooth.spline)
  if (any(duplicated(x))) {
    agg_once <- function(y) tapply(y, INDEX = x, FUN = mean)
    xu <- sort(unique(x))
    Yc <- sapply(seq_len(ncol(Y)), function(j) as.numeric(agg_once(Y[, j])))
    colnames(Yc) <- series_names
    x <- xu; Y <- Yc
  }
  
  n <- length(x)
  if (n < 5) return(data.frame())
  
  # helper: smooth one vector
  smooth_vec <- function(xx, yy, spar) stats::predict(stats::smooth.spline(xx, yy, spar = spar), xx)$y
  
  # helper: candidate peaks (indices where slope sign + -> - )
  local_maxima <- function(ys) {
    dy  <- diff(ys)
    sgn <- sign(dy); sgn[sgn == 0] <- NA
    which(head(sgn, -1) > 0 & tail(sgn, -1) < 0) + 1
  }
  
  # helper: nearest minima (shoulders) around peak i on ys
  shoulders_lr <- function(ys, i) {
    # walk left to a local minimum (slope - -> +)
    l <- i
    while (l > 2 && ys[l-1] > ys[l]) l <- l - 1
    while (l > 2 && ys[l-1] < ys[l]) l <- l - 1
    # walk right to a local minimum
    r <- i
    while (r < n-1 && ys[r+1] > ys[r]) r <- r + 1
    while (r < n-1 && ys[r+1] < ys[r]) r <- r + 1
    c(max(1, l), min(n, r))
  }
  
  # helper: linear crossing between indices j and j+1 for target level
  interp_cross <- function(xv, yv, level, j) {
    a <- yv[j]; b <- yv[j+1]
    if (is.na(a) || is.na(b)) return(NA_real_)
    if ((a - level) * (b - level) > 0) return(NA_real_)
    t <- if (b == a) 0 else (level - a) / (b - a)
    xv[j] + t * (xv[j+1] - xv[j])
  }
  
  # ??C -> approx index distance
  idx_per_deg <- mean(diff(seq_len(n)) / diff(x))
  min_dist    <- max(1L, floor(peak_min_sep_deg * idx_per_deg))
  
  out <- list()
  
  for (j in seq_len(ncol(Y))) {
    y  <- Y[, j]
    ok <- is.finite(y)
    if (!any(ok)) next
    ys <- smooth_vec(x, y, spar = spar_signal)
    
    # auto absolute prominence threshold from this trace's dynamic range
    prom_abs <- rel_prom * (max(ys, na.rm = TRUE) - min(ys, na.rm = TRUE))
    
    cand <- local_maxima(ys)
    if (!length(cand)) next
    
    # greedy keep with min separation
    kept <- integer(0)
    for (i in cand) {
      if (!length(kept) || (i - kept[length(kept)] >= min_dist)) kept <- c(kept, i)
    }
    
    for (pi in kept) {
      sh <- shoulders_lr(ys, pi)
      li <- sh[1]; ri <- sh[2]
      base <- min(ys[li], ys[ri])
      height <- ys[pi] - base
      if (!is.finite(height) || height < prom_abs) next
      
      half <- base + height / 2
      
      # left crossing: search from pi-1 down to li
      xl <- NA_real_
      for (jj in seq(pi - 1L, li, by = -1L)) {
        xl <- interp_cross(x, ys, half, jj)
        if (is.finite(xl)) break
      }
      
      # right crossing: search from pi to ri-1
      xr <- NA_real_
      for (jj in seq(pi, ri - 1L)) {
        xr <- interp_cross(x, ys, half, jj)
        if (is.finite(xr)) break
      }
      
      out[[length(out) + 1L]] <- data.frame(
        series              = series_names[j],
        peak_index          = pi,
        peak_T              = x[pi],
        peak_value_smooth   = ys[pi],
        left_shoulder_T     = x[li],
        left_shoulder_value = ys[li],
        right_shoulder_T    = x[ri],
        right_shoulder_value= ys[ri],
        baseline_min        = base,
        half_height_level   = half,
        left_half_T         = xl,
        right_half_T        = xr,
        FWHM                = if (is.finite(xl) && is.finite(xr)) xr - xl else NA_real_,
        prominence_est      = height,
        stringsAsFactors    = FALSE
      )
    }
  }
  
  if (!length(out)) data.frame() else do.call(rbind, out)
}
# ------------------------------------------------------------
# Shoulder-aware peak detection via 2nd derivative (base R)
# ------------------------------------------------------------
# ------------------------------------------------------------
# ROI-gated 2nd-derivative peak detector with proximity merge
# base R (smooth.spline + splinefun)
# ------------------------------------------------------------
# ------------------------------------------------------------
# ROI-gated 2nd-derivative peak detector with proximity merge
# base R (smooth.spline + splinefun)
# ------------------------------------------------------------
detect_peaks_2nd_roi <- function(
    x, y,
    roi_min = 60, roi_max = 70,     # hard window; nothing outside can be detected
    spar_signal = 0.35,             # smoothing (0.30???0.55 typical)
    rel_curv_thresh = 0.18,         # keep minima with |d2| >= rel * max(|d2|) in ROI
    abs_curv_thresh = NA,           # or set an absolute threshold; NA = auto via rel
    merge_close_deg = 1.0,          # merge minima/refined Tm closer than this
    min_sep_deg = 1.2,              # final required separation for calling "double"
    refine_zero_window_deg = 1.5,   # ????C search around curvature min for dy=0
    max_peaks = 2                   # cap number of peaks reported
){
  stopifnot(length(x) == length(y))
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  o <- order(x); x <- x[o]; y <- y[o]
  
  # --- ROI gating
  m <- x >= roi_min & x <= roi_max
  if (!any(m)) return(list(class="none_in_roi", peaks_T=numeric(0),
                           details=data.frame()))
  xw <- x[m]; yw <- y[m]
  if (length(xw) < 5) return(list(class="insufficient_data", peaks_T=numeric(0),
                                  details=data.frame()))
  
  # --- smooth + derivatives
  ys <- predict(smooth.spline(xw, yw, spar = spar_signal), xw)$y
  sf  <- splinefun(xw, ys, method = "natural")
  dy  <- sf(xw, deriv = 1)
  d2y <- sf(xw, deriv = 2)
  
  # --- candidate curvature minima inside ROI (strictly negative lobes)
  n <- length(xw)
  idx_min <- which(d2y[-c(1,n)] < d2y[-c(1,2)] & d2y[-c(n-1,n)] < d2y[-c(1,n-1)]) + 1L
  if (!length(idx_min)) {
    # fallback: main apex within ROI
    pi <- which.max(ys)
    return(list(class="single", peaks_T=xw[pi],
                details=data.frame(Tm=xw[pi], d2=d2y[pi], source="apex")))
  }
  
  # --- magnitude threshold
  if (is.na(abs_curv_thresh)) abs_curv_thresh <- rel_curv_thresh * max(abs(d2y), na.rm=TRUE)
  cand <- idx_min[d2y[idx_min] <= -abs_curv_thresh]
  if (!length(cand)) {
    pi <- which.max(ys)
    return(list(class="single", peaks_T=xw[pi],
                details=data.frame(Tm=xw[pi], d2=d2y[pi], source="apex")))
  }
  
  # helper: ??C -> indices
  idx_per_deg <- mean(diff(seq_len(n)) / diff(xw))
  to_idx <- function(deg) max(1L, floor(deg * idx_per_deg))
  
  # --- refine each min to a nearby dy==0 (true apex) if present
  refine_to_zero <- function(i){
    rng <- which(abs(xw - xw[i]) <= refine_zero_window_deg)
    if (length(rng) < 3) return(i)
    s <- sign(dy[rng]); s[s==0] <- NA
    zc <- which(head(s,-1) > 0 & tail(s,-1) < 0)  # + to -
    if (!length(zc)) return(i)
    j <- rng[zc[1]]
    a <- dy[j]; b <- dy[j+1]
    t <- if (b == a) 0 else -a/(b-a)
    x_ref <- xw[j] + t*(xw[j+1]-xw[j])
    which.min(abs(xw - x_ref))
  }
  refined <- vapply(cand, refine_to_zero, 1L)
  
  # --- merge close-by candidates (by temperature distance), keep stronger (more -d2)
  merge_idx <- to_idx(merge_close_deg)
  keep <- logical(length(refined)); keep[] <- TRUE
  ord  <- order(d2y[refined])  # most negative first
  chosen <- integer(0)
  for (k in ord) {
    if (!keep[k]) next
    i <- refined[k]
    chosen <- c(chosen, i)
    # suppress neighbors within merge window
    close <- which(abs(refined - i) < merge_idx)
    keep[close] <- FALSE
    keep[k] <- TRUE
  }
  refined <- sort(unique(chosen))
  
  # --- enforce final separation
  sep_idx <- to_idx(min_sep_deg)
  final <- integer(0)
  for (i in refined) {
    if (!length(final) || all(abs(i - final) >= sep_idx)) final <- c(final, i)
  }
  
  # limit to max_peaks (prefer strongest curvature)
  if (length(final) > max_peaks) {
    ord <- order(d2y[final])  # most negative first
    final <- final[ord][seq_len(max_peaks)]
    final <- sort(final)
  }
  
  # if nothing left, fall back to apex in ROI
  if (!length(final)) {
    pi <- which.max(ys)
    return(list(class="single", peaks_T=xw[pi],
                details=data.frame(Tm=xw[pi], d2=d2y[pi], source="apex")))
  }
  
  class <- if (length(final) >= 2) "double" else "single"
  det <- data.frame(Tm = xw[final], d2 = d2y[final], source = "2nd-deriv")
  
  # ensure ROI apex present for reference
  main_idx <- which.max(ys)
  if (!any(abs(det$Tm - xw[main_idx]) < 1e-6)) {
    det <- rbind(det, data.frame(Tm=xw[main_idx], d2=d2y[main_idx], source="apex"))
  }
  det <- det[order(det$Tm), ]
  
  list(class=class, peaks_T=det$Tm, details=det)
}


cvd_all_in_one_rotor <- function(input_dir){
  # 1) Path to your CSV
  csvs <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  if (length(csvs) == 0) {
    stop("No CSV file found in: ", input_dir)
  }
  if (length(csvs) > 1) {
    stop("Multiple CSV files found: ", paste(basename(csvs), collapse = ", "))
  }
  
  # 2) find Excel(s)
  xls  <- list.files(input_dir, pattern = "\\.(xlsx|xls)$", full.names = TRUE, ignore.case = TRUE)
  if (length(xls) == 0) {
    stop("No Excel file found in: ", input_dir)
  }
  if (length(xls) > 1) {
    stop("Multiple Excel files found: ", paste(basename(xls), collapse = ", "))
  }
  f   <- csvs
  
  # 2) Read all lines
  raw <- readLines(f, encoding = "UTF-8")
  
  # 3) Locate every header line containing ???Melt analysis of???
  starts <- grep("Melt analysis of", raw, ignore.case = TRUE)
  
  # 4) Extract each table into a list
  melt_tables <- lapply(seq_along(starts), function(i) {
    # data begins on the very next line (which contains your "ID; ???" header)
    start <- starts[i] + 1
    # and runs up to just before the next header (or end of file)
    end   <- if (i < length(starts)) starts[i+1] - 1 else length(raw)
    
    block <- raw[start:end]
    # drop any purely-empty or ???;;;?????? rows
    block <- block[!grepl("^\\s*;*\\s*$", block)]
    
    # parse as a semicolon CSV with comma decimals
    read.table(
      text           = block,
      sep            = ";",
      dec            = ",",
      header         = TRUE,
      stringsAsFactors = FALSE,
      check.names    = FALSE
    )
  })
  
  # 6) rename each table
  names(melt_tables)[grep(pattern = "Green", raw[starts])] <- "fam_data"
  names(melt_tables)[grep(pattern = "Orange", raw[starts])] <- "rox_data"
  names(melt_tables)[grep(pattern = "Red", raw[starts])] <- "cy5_data"
  names(melt_tables)[grep(pattern = "Yellow", raw[starts])] <- "hex_data"
  for (i in 1:length(melt_tables)) {
    melt_tables[[i]] <- as.data.frame(t(melt_tables[[i]]))
    melt_tables[[i]] <- cbind(rownames(melt_tables[[i]]), melt_tables[[i]])
    colnames(melt_tables[[i]]) <- c("Temperature", melt_tables[[i]][1,2:ncol(melt_tables[[i]])])
    melt_tables[[i]] <- melt_tables[[i]][3:nrow(melt_tables[[i]]),]
    melt_tables[[i]] <- as.data.frame(apply(melt_tables[[i]], 2, as.numeric))
  }
  cy5_data <- as.data.frame(melt_tables$cy5_data)
  rox_data <- as.data.frame(melt_tables$rox_data)
  fam_data <- as.data.frame(melt_tables$fam_data)
  hex_data <- as.data.frame(melt_tables$hex_data)
  colnames(cy5_data)[2:ncol(cy5_data)] <- paste0("w", trimws(colnames(cy5_data)[2:ncol(cy5_data)]))
  colnames(rox_data)[2:ncol(rox_data)] <- paste0("w", trimws(colnames(rox_data)[2:ncol(rox_data)]))
  colnames(fam_data)[2:ncol(fam_data)] <- paste0("w", trimws(colnames(fam_data)[2:ncol(fam_data)]))
  colnames(hex_data)[2:ncol(hex_data)] <- paste0("w", trimws(colnames(hex_data)[2:ncol(hex_data)]))
  
  
  well_info <- read_excel(xls, sheet = 1)
  well_info <- well_info[well_info$Selected == "Yes",]
  well_info <- as.data.frame(well_info[,c(2,3,4,5)])
  colnames(well_info) <- c("Well", "Sample", "Content", "Target")
  well_info$Well <- paste0("w",well_info$Well)
  
  a1298_data <- as.data.frame(cy5_data[,c("Temperature", well_info[grep(pattern = "\\bCVDM1\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  c677_data <- as.data.frame(rox_data[,c("Temperature", well_info[grep(pattern = "\\bCVDM1\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  fv_data <- as.data.frame(fam_data[,c("Temperature", well_info[grep(pattern = "\\bCVDM1\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  fii_data <- as.data.frame(hex_data[,c("Temperature", well_info[grep(pattern = "\\bCVDM1\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  pai_data <- as.data.frame(cy5_data[,c("Temperature", well_info[grep(pattern = "\\bCVDM2\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  fxiii_data <- as.data.frame(rox_data[,c("Temperature", well_info[grep(pattern = "\\bCVDM2\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  hpai_data <- as.data.frame(cy5_data[,c("Temperature", well_info[grep(pattern = "\\bCVDM3\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  fgb_data <- as.data.frame(rox_data[,c("Temperature", well_info[grep(pattern = "\\bCVDM3\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  fvcamb_data <- as.data.frame(cy5_data[,c("Temperature", well_info[grep(pattern = "\\bCVDM4\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  apob_data <- as.data.frame(rox_data[,c("Temperature", well_info[grep(pattern = "\\bCVDM4\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  ace_data <- as.data.frame(cy5_data[,c("Temperature", well_info[grep(pattern = "\\bCVDM5\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  lta_data <- as.data.frame(rox_data[,c("Temperature", well_info[grep(pattern = "\\bCVDM5\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  apoe2_data <- as.data.frame(cy5_data[,c("Temperature", well_info[grep(pattern = "\\bCVDM6\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  apoe1_data <- as.data.frame(rox_data[,c("Temperature", well_info[grep(pattern = "\\bCVDM6\\b", x = well_info$Target, ignore.case = TRUE),"Well"])])
  
  data_list_11 <- list(fv_data, fii_data,  c677_data, a1298_data, fxiii_data, fgb_data, hpai_data,  apob_data,fvcamb_data, ace_data, lta_data)
  names(data_list_11) <- c("FV-LEI","FII", "C677T", "A1298C", "FXIII", "FGB", "HPAI","APOB",  "FV CAMB", "ACE", "LTA")
  data_list_apoe <- list(apoe1_data, apoe2_data)
  names(data_list_apoe) <- list("APOE1", "APOE2")
  data_list_graph_cvd <- list(fv_data, fii_data,  c677_data, a1298_data, fxiii_data, pai_data, fgb_data, hpai_data,  apob_data,fvcamb_data, ace_data, lta_data, apoe1_data, apoe2_data)
  names(data_list_graph_cvd) <- c("FV-LEI","FII", "C677T", "A1298C", "FXIII", "PAI", "FGB", "HPAI","APOB",  "FV CAMB", "ACE", "LTA", "APOE1", "APOE2")
  pai_data_graph <- pai_data
  
  
  well_info_pai <- well_info[grep(pattern = "\\bCVDM2\\b", x = well_info$Target, ignore.case = TRUE),]
  well_info_fv <- well_info[grep(pattern = "\\bCVDM1\\b", x = well_info$Target, ignore.case = TRUE),]
  well_info_fii <- well_info[grep(pattern = "\\bCVDM1\\b", x = well_info$Target, ignore.case = TRUE),]
  well_info_1298 <- well_info[grep(pattern = "\\bCVDM1\\b", x = well_info$Target),]
  well_info_677 <- well_info[grep(pattern = "\\bCVDM1\\b", x = well_info$Target),]
  well_info_fxiii <- well_info[grep(pattern = "\\CVDM2", x = well_info$Target, ignore.case = TRUE),]
  well_info_fgb <- well_info[grep(pattern = "\\bCVDM3\\b", x = well_info$Target, ignore.case = TRUE),]
  well_info_hpai <-well_info[grep(pattern = "\\bCVDM3\\b", well_info$Target, ignore.case = TRUE),]
  well_info_fvcamb <-well_info[grep(pattern = "\\bCVDM4\\b", well_info$Target, ignore.case = TRUE),]
  well_info_apob <-well_info[grep(pattern = "\\bCVDM4\\b", well_info$Target, ignore.case = TRUE),]
  well_info_ace <-well_info[grep(pattern = "CVDM5", well_info$Target, ignore.case = TRUE),]
  well_info_lta <-well_info[grep(pattern = "CVDM5", well_info$Target, ignore.case = TRUE),]
  well_info_apoe1 <-well_info[grep(pattern = "CVDM6", well_info$Target, ignore.case = TRUE),]
  well_info_apoe2 <-well_info[grep(pattern = "CVDM6", well_info$Target, ignore.case = TRUE),]
  
  well_list_11 <- list(well_info_fv, well_info_fii,  well_info_677, well_info_1298, well_info_fxiii, well_info_fgb, well_info_hpai,  well_info_apob, well_info_fvcamb, well_info_ace, well_info_lta)
  names(well_list_11) <- c("FV-LEI","FII", "C677T", "A1298C", "FXIII", "FGB", "HPAI",  "APOB","FV CAMB", "ACE", "LTA")
  well_list_apoe <- list(well_info_apoe1, well_info_apoe2)
  names(well_list_apoe) <- list("APOE1", "APOE2")
  
  
  fv_melt <- c(64,68)
  fii_melt <- c(66,70)
  c677_melt <- c(63,69)
  a1298_melt <- c(63,70)
  fxiii_melt <- c(66,71)
  fgb_melt <- c(50,57)
  hpai_melt <- c(63,67)
  apob_melt <- c(62,67)
  fvcamb_melt <- c(64,67)
  lta_melt <- c(63,70)
  ace_melt <- c(62,67)
  apoe1_melt <- c(55,67)
  apoe2_melt <- c(64,71)
  
  melt_for_11 <- c(mean(fv_melt), mean(fii_melt), mean(c677_melt), mean(a1298_melt), mean(fxiii_melt), mean(fgb_melt), mean(hpai_melt), mean(apob_melt), mean(fvcamb_melt),  mean(ace_melt),mean(lta_melt))
  names(melt_for_11) <- c("FV-LEI","FII", "C677T", "A1298C", "FXIII", "FGB", "HPAI",  "APOB","FV CAMB", "ACE", "LTA")
  melt_for_apoe <- c(mean(apoe1_melt), mean(apoe2_melt))
  names(melt_for_apoe) <- list("APOE1", "APOE2")
  melt_list_cvd <- list(fv_melt, fii_melt, c677_melt, a1298_melt, fxiii_melt, fgb_melt, hpai_melt, apob_melt, fvcamb_melt, ace_melt, lta_melt, apoe1_melt, apoe2_melt)
  names(melt_list_cvd) <- c("FV-LEI","FII", "C677T", "A1298C", "FXIII", "FGB", "HPAI",  "APOB","FV CAMB", "ACE", "LTA", "APOE1", "APOE2")
  
  
  ##result tables
  result_tb_list_11 <- list()
  result_tb_names <- c()
  for (i in 1:11) {
    if (ncol(data_list_11[[i]]) < 2) {
      result_table <- data.frame()
      next
    }
    result_table <- data.frame(row.names = colnames(data_list_11[[i]])[2:ncol(data_list_11[[i]])], min_dips = c(2:ncol(data_list_11[[i]])), max_dips = c(2:ncol(data_list_11[[i]])), peak_1 = c(2:ncol(data_list_11[[i]])), peak_2 = c(2:ncol(data_list_11[[i]])), Tm = c(2:ncol(data_list_11[[i]])), patient = c(2:ncol(data_list_11[[i]])), genotype = c(2:ncol(data_list_11[[i]])))
    result_tb_list_11 <- c(result_tb_list_11, list(result_table))
    result_tb_names <- c(result_tb_names, names(well_list_11[i]))
  }
  names(result_tb_list_11) <- result_tb_names
  well_list_11 <- well_list_11[result_tb_names]
  data_list_11 <- data_list_11[result_tb_names]
  ##for pai
  reference_pai_genotype <- "4G-5G"
  reference_pai_well <- well_info_pai$Well[well_info_pai$Content == "Positive Control"]
  
  if (ncol(pai_data) > 1) {
    pai_data <- as.data.frame(pai_data)
    pai_data <- pai_data[pai_data[,1] < 78,]
    pai_data <- pai_data[pai_data[,1] > 60,]
    all_peak <- c()
    min_dips <- c()
    max_dips <- c()
    FWHM <- c()
    result_tb_pai <- data.frame(row.names = colnames(pai_data)[2:ncol(pai_data)], min_dips = c(2:ncol(pai_data)), max_dips = c(2:ncol(pai_data)), peak_1 = c(2:ncol(pai_data)), FWHM = c(2:ncol(pai_data)), patient = c(2:ncol(pai_data)), genotype = c(2:ncol(pai_data)))
    for (i in 1:nrow(result_tb_pai)) {
      for (j in 1:nrow(well_info_pai)) {
        if (rownames(result_tb_pai)[i] == well_info_pai$Well[j]) {
          result_tb_pai$patient[i] <- well_info_pai$Sample[j]
        }
      }
    }
    for (i in 2:ncol(pai_data)) {
      if (max(pai_data[,i]) < 0.5) {
        min_dips <- c(min_dips, NA)
        max_dips <- c(max_dips, NA)
      } else {
        dips_fwhm <- halfmax_fwhm(pai_data[,1], pai_data[,i], spar_signal = 0.35, peak_min_sep_deg = 0.8, rel_prom = 0.05)
        min_dips <- c(min_dips, dips_fwhm$left_half_T)
        max_dips <- c(max_dips, dips_fwhm$right_half_T)
        FWHM <- c(FWHM, dips_fwhm$FWHM)
      }
    }
    
    result_tb_pai$min_dips <- min_dips
    result_tb_pai$max_dips <- max_dips
    result_tb_pai$FWHM <- FWHM
    for (i in 1:nrow(result_tb_pai)) {
      if ("NTC" %in% well_info_pai$Content[which(well_info_pai$Well %in% rownames(result_tb_pai)[i])]){
        result_tb_pai$genotype[i] <- "NTC"
      } 
      if ("Positive Control" %in% well_info_pai$Content[which(well_info_pai$Well %in% rownames(result_tb_pai)[i])]){
        result_tb_pai$genotype[i] <- "Positive Control"
      }
      if (is.na(result_tb_pai$min_dips[i]) && is.na(result_tb_pai$max_dips[i])) {
        result_tb_pai$peak_1[i] <- NA
      }
    }
    ##get genotype
    
    for (i in 1:nrow(result_tb_pai)) {
      if ("NTC" %in% result_tb_pai$genotype[i]){
        next
      }
      if ("Positive Control" %in% result_tb_pai$genotype[i]){
        next
      }
      if (is.na(result_tb_pai$peak_1[i])){
        result_tb_pai$genotype[i] <- "No Peaks Detected within the Boundaries."
        next
      }
      if (result_tb_pai$FWHM[i] > (result_tb_pai[reference_pai_well, "FWHM"]-1.25) ) {
        result_tb_pai$genotype[i] <- "4G-5G"
      } else if (result_tb_pai$max_dips[i] < result_tb_pai[reference_pai_well,2]-0.75){
        result_tb_pai$genotype[i] <- "5G-5G"
      } else if (result_tb_pai$min_dips[i] > result_tb_pai[reference_pai_well,1]+1){
        result_tb_pai$genotype[i] <- "4G-4G"
      }
      
    }    
    
    result_tb_pai$Parameter <- c(rep(x = "PAI", nrow(result_tb_pai)))
    result_tb_pai <- cbind(rownames(result_tb_pai),result_tb_pai)
    result_tb_pai <- result_tb_pai[,c(1,6,8,7,4)]
    colnames(result_tb_pai) <- c("Well" ,"Sample Name", "Parameter", "Genotype", "peak_1")
  } else {
    result_tb_pai <- data.frame()
  }
  ##for two peak 11 parameter
  if(nrow(list2DF(result_tb_list_11)) != 0){
    for (i in 1:length(result_tb_list_11)) {
      for (j in 1:nrow(result_tb_list_11[[i]])) {
        for (k in 1:nrow(well_list_11[[i]])) {
          if(rownames(result_tb_list_11[[i]])[j] == well_list_11[[i]]$Well[k]){
            result_tb_list_11[[i]]$patient[j] <- well_list_11[[i]]$Sample[k]
          }
        }
      }
    }
    
    for (i in 1:length(data_list_11)) {
      all_peak <- c()
      min_dips <- c()
      max_dips <- c()
      true_peak_rfu <- c()
      melt_range <- melt_list_cvd[[names(data_list_11)[i]]]
      lower_bound <- melt_range[1] - 3
      upper_bound <- melt_range[2] + 3
      for (j in 2:ncol(data_list_11[[i]])) {
        if (max(data_list_11[[i]][,j]) < 0) {
          min_dips <- c(min_dips, NA)
          max_dips <- c(max_dips, NA)
        } else {
          min_dips <- c(min_dips, min(data_list_11[[i]][which(data_list_11[[i]][,j] > max(data_list_11[[i]][,j]/2)),1]))
          max_dips <- c(max_dips, max(data_list_11[[i]][which(data_list_11[[i]][,j] > max(data_list_11[[i]][,j]/2)),1]))
        }
        for (k in 2:nrow(data_list_11[[i]]-1)) {
          if(max(data_list_11[[i]][,j]) > 0){
            if(data_list_11[[i]][k,1] > lower_bound && data_list_11[[i]][k,1] < upper_bound){
              if (data_list_11[[i]][k+1,j] < data_list_11[[i]][k,j] && data_list_11[[i]][k-1,j] < data_list_11[[i]][k,j]) {
                all_peak <- c(all_peak, data_list_11[[i]][k,j])
              }
            } 
          } else {
            all_peak <- c(all_peak, 0)
          }
        }
        
        true_peak_rfu <- sort(all_peak, decreasing = TRUE)
        if (length(true_peak_rfu) == 0) {
          result_tb_list_11[[i]]$peak_1[j-1] <- NA
        } else {
          result_tb_list_11[[i]]$peak_1[j-1] <- true_peak_rfu[1]
        }
        
        if (length(true_peak_rfu) > 1) {
          if (true_peak_rfu[2] > true_peak_rfu[1]/2) {
            result_tb_list_11[[i]]$peak_2[j-1] <- true_peak_rfu[2]
          }
          else {
            result_tb_list_11[[i]]$peak_2[j-1] <- NA
          }
        } else {
          result_tb_list_11[[i]]$peak_2[j-1] <- NA
        }
        all_peak <- c()
        true_peak_rfu <- c()
      }
      result_tb_list_11[[i]]$min_dips <- min_dips
      result_tb_list_11[[i]]$max_dips <- max_dips
    }
    for (i in 1:length(result_tb_list_11)) {
      tm <- (melt_for_11[[names(result_tb_list_11)[i]]])
      for (j in 1:nrow(result_tb_list_11[[i]])) {
        if ("NTC" %in% well_info$Content[which(well_info$Well %in% rownames(result_tb_list_11[[i]])[j])]) {
          result_tb_list_11[[i]]$genotype[j] <- "NTC"
          next
        }
        if ("Positive Control" %in% well_info$Content[which(well_info$Well %in% rownames(result_tb_list_11[[i]])[j])]){
          result_tb_list_11[[i]]$genotype[j] <- "Positive Control"
          next
        }
        if (is.na(result_tb_list_11[[i]]$min_dips[j]) && is.na(result_tb_list_11[[i]]$max_dips[j])) {
          result_tb_list_11[[i]]$Tm[j] <- NA
          next
        }
        if (is.na(result_tb_list_11[[i]]$peak_2[j]) && names(result_tb_list_11)[i] %!in% c("HPAI", "FV-LEI", "FII", "FV CAMB")) {
          
          result_tb_list_11[[i]]$Tm[j] <- as.numeric(data_list_11[[names(result_tb_list_11[i])]][which(data_list_11[[names(result_tb_list_11[i])]][,j+1] %in% result_tb_list_11[[i]]$peak_1),1])
          
          
        } else if (is.na(result_tb_list_11[[i]]$peak_2[j]) == TRUE && names(result_tb_list_11)[i] == "HPAI"){
          second_peak <- detect_peaks_2nd_roi(hpai_data[,1],
                                              hpai_data[,j+1],
                                              roi_min = hpai_melt[1]-2,
                                              roi_max = hpai_melt[2]+2,
                                              merge_close_deg = 2.5, max_peaks = 2)
          if(second_peak$class == "double") {
            
            result_tb_list_11[[i]]$genotype[j] <- "Heterozygous"
            
          } else {
            
            result_tb_list_11[[i]]$Tm[j] <- data_list_11[[names(result_tb_list_11[i])]][which(data_list_11[[names(result_tb_list_11[i])]][,j+1] %in% result_tb_list_11[[i]]$peak_1),1]
            
          }
          
        } else if(is.na(result_tb_list_11[[i]]$peak_2[j]) == TRUE && names(result_tb_list_11)[i] == "FV-LEI"){
          second_peak <- detect_peaks_2nd_roi(fv_data[,1],
                                              fv_data[,j+1],
                                              roi_min = fv_melt[1]-2,
                                              roi_max = fv_melt[2]+2,
                                              merge_close_deg = 2.5, max_peaks = 2)
          if(second_peak$class == "double") {
            
            result_tb_list_11[[i]]$genotype[j] <- "Heterozygous"
            second_peak <- c()
          } else {
            
            result_tb_list_11[[i]]$Tm[j] <- data_list_11[[names(result_tb_list_11[i])]][which(data_list_11[[names(result_tb_list_11[i])]][,j+1] %in% result_tb_list_11[[i]]$peak_1),1]
            second_peak <- c()
          }
        } else if(is.na(result_tb_list_11[[i]]$peak_2[j]) == TRUE && names(result_tb_list_11)[i] == "FII"){
          second_peak <- detect_peaks_2nd_roi(fii_data[,1],
                                              fii_data[,j+1],
                                              roi_min = fii_melt[1]-2,
                                              roi_max = fii_melt[2]+2,
                                              merge_close_deg = 2.5, max_peaks = 2)
          if(second_peak$class == "double") {
            
            result_tb_list_11[[i]]$genotype[j] <- "Heterozygous"
            second_peak <- c()
          } else {
            
            result_tb_list_11[[i]]$Tm[j] <- data_list_11[[names(result_tb_list_11[i])]][which(data_list_11[[names(result_tb_list_11[i])]][,j+1] %in% result_tb_list_11[[i]]$peak_1),1]
            second_peak <- c()
          }
        }  else if(is.na(result_tb_list_11[[i]]$peak_2[j]) == TRUE && names(result_tb_list_11)[i] == "FXIII"){
          second_peak <- detect_peaks_2nd_roi(fxiii_data[,1],
                                              fxiii_data[,j+1],
                                              roi_min = fxiii_melt[1]-2,
                                              roi_max = fxiii_melt[2]+2,
                                              merge_close_deg = 2.5, max_peaks = 2)
          if(second_peak$class == "double") {
            
            result_tb_list_11[[i]]$genotype[j] <- "Heterozygous"
            
          } else {
            
            result_tb_list_11[[i]]$Tm[j] <- data_list_11[[names(result_tb_list_11[i])]][which(data_list_11[[names(result_tb_list_11[i])]][,j+1] %in% result_tb_list_11[[i]]$peak_1),1]
            
          }
        } else if(is.na(result_tb_list_11[[i]]$peak_2[j]) == TRUE && names(result_tb_list_11)[i] == "FV CAMB"){
          second_peak <- detect_peaks_2nd_roi(fvcamb_data[,1], 
                                              fvcamb_data[,j+1], 
                                              roi_min = fvcamb_melt[1]-2, roi_max = fvcamb_melt[2]+2,
                                                merge_close_deg = 2.5, max_peaks = 2)
          if(second_peak$class == "double") {
            
            result_tb_list_11[[i]]$genotype[j] <- "Heterozygous"
          
            } else {
        
            result_tb_list_11[[i]]$Tm[j] <- data_list_11[[names(result_tb_list_11[i])]][which(data_list_11[[names(result_tb_list_11[i])]][,j+1] %in% result_tb_list_11[[i]]$peak_1),1]
          }
        }
        else {
          result_tb_list_11[[i]]$genotype[j] <- "Heterozygous"
        }
      }
      for (j in 1:nrow(result_tb_list_11[[i]])) {
        if (is.na(result_tb_list_11[[i]]$Tm[j])) {
          result_tb_list_11[[i]]$genotype[j] <- "No Peaks Detected Within the Boundaries."
        } else if ("NTC" %in% well_info$Content[which(well_info$Well %in% rownames(result_tb_list_11[[i]])[j])]){
          result_tb_list_11[[i]]$genotype[j] <- "NTC"
        } else if ("Positive Control" %in% well_info$Content[which(well_info$Well %in% rownames(result_tb_list_11[[i]])[j])]){
          result_tb_list_11[[i]]$genotype[j] <- "Positive Control"
        } else if (result_tb_list_11[[i]]$genotype[j] == "Heterozygous" && names(result_tb_list_11[i]) == "ACE"){
          result_tb_list_11[[i]]$genotype[j] <- "INS-DEL"
        } else if (result_tb_list_11[[i]]$Tm[j] > tm && names(result_tb_list_11[i]) == "ACE"){
          result_tb_list_11[[i]]$genotype[j] <- "DEL-DEL"
        } else if (result_tb_list_11[[i]]$Tm[j] < tm && names(result_tb_list_11[i]) == "ACE"){
          result_tb_list_11[[i]]$genotype[j] <- "INS-INS"
        } else if ( result_tb_list_11[[i]]$genotype[j] == "Heterozygous"){
          result_tb_list_11[[i]]$genotype[j] <- "Heterozygous"
        } else if (result_tb_list_11[[i]]$Tm[j] > tm && names(result_tb_list_11[i]) != "A1298C"){
          result_tb_list_11[[i]]$genotype[j] <- "Wild Type"
        } else if (result_tb_list_11[[i]]$Tm[j] < tm && names(result_tb_list_11[i]) != "A1298C"){
          result_tb_list_11[[i]]$genotype[j] <- "Homozygous Mutant"
        } else if (result_tb_list_11[[i]]$Tm[j] > tm && names(result_tb_list_11[i]) == "A1298C"){
          result_tb_list_11[[i]]$genotype[j] <- "Homozygous Mutant"
        } else if (result_tb_list_11[[i]]$Tm[j] < tm && names(result_tb_list_11[i]) == "A1298C"){
          result_tb_list_11[[i]]$genotype[j] <- "Wild Type"
        }
        
      }
      
    }
  } else {
    result_tb_list_11 <- list()
  }
  
  ##for apoe
  if(ncol(data_list_apoe[[1]]) >1 && ncol(data_list_apoe[[1]]) == ncol(data_list_apoe[[2]])){
    result_tb_list_apoe <- list()
    result_tb_names_apoe <- c()
    for (i in 1:2) {
      if (ncol(data_list_apoe[[i]]) < 2) {
        result_table_apoe <- data.frame()
        next
      }
      result_table_apoe <- data.frame(row.names = colnames(data_list_apoe[[i]])[2:ncol(data_list_apoe[[i]])], min_dips = c(2:ncol(data_list_apoe[[i]])), max_dips = c(2:ncol(data_list_apoe[[i]])), peak_1 = c(2:ncol(data_list_apoe[[i]])), peak_2 = c(2:ncol(data_list_apoe[[i]])), Tm = c(2:ncol(data_list_apoe[[i]])), patient = c(2:ncol(data_list_apoe[[i]])), genotype = c(2:ncol(data_list_apoe[[i]])))
      result_tb_list_apoe <- c(result_tb_list_apoe, list(result_table_apoe))
      result_tb_names_apoe <- c(result_tb_names_apoe, names(well_list_apoe[i]))
    }
    names(result_tb_list_apoe) <- result_tb_names_apoe
    well_list_apoe <- well_list_apoe[result_tb_names_apoe]
    data_list_apoe <- data_list_apoe[result_tb_names_apoe]
    for (i in 1:length(result_tb_list_apoe)) {
      for (j in 1:nrow(result_tb_list_apoe[[i]])) {
        for (k in 1:nrow(well_list_apoe[[i]])) {
          if(rownames(result_tb_list_apoe[[i]])[j] == well_list_apoe[[i]]$Well[k]){
            result_tb_list_apoe[[i]]$patient[j] <- well_list_apoe[[i]]$Sample[k]
          }
        }
      }
    }
    
    for (i in 1:length(data_list_apoe)) {
      all_peak <- c()
      min_dips <- c()
      max_dips <- c()
      true_peak_rfu <- c()
      melt_range <- melt_list_cvd[[names(data_list_apoe)[i]]]
      lower_bound <- melt_range[1] - 3
      upper_bound <- melt_range[2] + 3
      for (j in 2:ncol(data_list_apoe[[i]])) {
        if (max(data_list_apoe[[i]][,j]) < 0.125) {
          min_dips <- c(min_dips, NA)
          max_dips <- c(max_dips, NA)
        } else {
          min_dips <- c(min_dips, min(data_list_apoe[[i]][which(data_list_apoe[[i]][,j] > max(data_list_apoe[[i]][,j]/2)),1]))
          max_dips <- c(max_dips, max(data_list_apoe[[i]][which(data_list_apoe[[i]][,j] > max(data_list_apoe[[i]][,j]/2)),1]))
        }
        for (k in 1:nrow(data_list_apoe[[i]])) {
          if(k > 1 && k < nrow(data_list_apoe[[i]])){
            if(data_list_apoe[[i]][k,1] > lower_bound && data_list_apoe[[i]][k,1] < upper_bound){
              if (data_list_apoe[[i]][k+1,j] < data_list_apoe[[i]][k,j] && data_list_apoe[[i]][k-1,j] < data_list_apoe[[i]][k,j]) {
                all_peak <- c(all_peak, data_list_apoe[[i]][k,j])
              }
            }
          }
        }
        true_peak_rfu <- sort(all_peak, decreasing = TRUE)
        
        if (length(true_peak_rfu) != 0) {
          result_tb_list_apoe[[i]]$peak_1[j-1] <- true_peak_rfu[1]
        }
        if (length(true_peak_rfu) > 1) {
          if (true_peak_rfu[2] > true_peak_rfu[1]/3) {
            result_tb_list_apoe[[i]]$peak_2[j-1] <- true_peak_rfu[2]
          } else {
            result_tb_list_apoe[[i]]$peak_2[j-1] <- NA
          }
        } else {
          result_tb_list_apoe[[i]]$peak_2[j-1] <- NA
        }
        all_peak <- c()
        true_peak_rfu <- c()
      }
      result_tb_list_apoe[[i]]$min_dips <- min_dips
      result_tb_list_apoe[[i]]$max_dips <- max_dips
    }
    for (i in 1:length(result_tb_list_apoe)) {
      tm <- mean(melt_for_apoe[names(result_tb_list_apoe[i])])
      for (j in 1:nrow(result_tb_list_apoe[[i]])) {
        if ("NTC" %in% well_info$Content[which(well_info$Well %in% rownames(result_tb_list_apoe[[i]])[j])]) {
          result_tb_list_apoe[[i]]$genotype[j] <- "NTC"
          next
        }
        if ("Positive Control" %in% well_info$Content[which(well_info$Well %in% rownames(result_tb_list_apoe[[i]])[j])]) {
          result_tb_list_apoe[[i]]$genotype[j] <- "Positive Control"
          next
        }
        if (is.na(result_tb_list_apoe[[i]]$min_dips[j]) && is.na(result_tb_list_apoe[[i]]$max_dips[j])) {
          result_tb_list_apoe[[i]]$Tm[j] <- NA
          next
        }
        if (is.na(result_tb_list_apoe[[i]]$peak_2[j])) {
          result_tb_list_apoe[[i]]$Tm[j] <- data_list_apoe[[names(result_tb_list_apoe[i])]][which(data_list_apoe[[names(result_tb_list_apoe[i])]][,j+1] %in% result_tb_list_apoe[[i]]$peak_1),1]
        } else {
          result_tb_list_apoe[[i]]$genotype[j] <- "Heterozygous"
        }
      }
      for (j in 1:nrow(result_tb_list_apoe[[i]])) {
        if (is.na(result_tb_list_apoe[[i]]$Tm[j])) {
          result_tb_list_apoe[[i]]$genotype[j] <- NA
        } else if ("NTC" %in% well_info$Content[which(well_info$Well %in% rownames(result_tb_list_apoe[[i]])[j])]){
          result_tb_list_apoe[[i]]$genotype[j] <- "NTC"
        } else if("Positive Control" %in% well_info$Content[which(well_info$Well %in% rownames(result_tb_list_apoe[[i]])[j])]){
          result_tb_list_apoe[[i]]$genotype[j] <- "Positive Control"
        } else if ( result_tb_list_apoe[[i]]$genotype[j] == "Heterozygous"){
          result_tb_list_apoe[[i]]$genotype[j] <- "Heterozygous"
        } else if (result_tb_list_apoe[[i]]$Tm[j] > tm && names(result_tb_list_apoe[i]) != "APOE1"){
          result_tb_list_apoe[[i]]$genotype[j] <- "Wild Type"
        } else if (result_tb_list_apoe[[i]]$Tm[j] < tm && names(result_tb_list_apoe[i]) != "APOE1"){
          result_tb_list_apoe[[i]]$genotype[j] <- "Homozygous Mutant"
        } else if (result_tb_list_apoe[[i]]$Tm[j] > tm && names(result_tb_list_apoe[i]) == "APOE1"){
          result_tb_list_apoe[[i]]$genotype[j] <- "Homozygous Mutant"
        } else if (result_tb_list_apoe[[i]]$Tm[j] < tm && names(result_tb_list_apoe[i]) == "APOE1"){
          result_tb_list_apoe[[i]]$genotype[j] <- "Wild Type"
        }
        
      }
    }
    apoe1_table <- result_tb_list_apoe[[1]]
    apoe1_table$Parameter <- "APOE1"
    apoe1_table <- cbind(Well = rownames(apoe1_table), apoe1_table)
    
    apoe2_table <- result_tb_list_apoe[[2]]
    apoe2_table$Parameter <- "APOE2"
    apoe2_table <- cbind(Well = rownames(apoe2_table), apoe2_table)
    
    result_tb_list_apoe_final <- result_tb_list_apoe[[1]]
    for(i in 1:nrow(result_tb_list_apoe[[1]])){
      if (is.na(result_tb_list_apoe[[1]]$genotype[i]) || is.na(result_tb_list_apoe[[2]]$genotype[i])){
        result_tb_list_apoe_final$genotype[i] <- "No Peaks Detected within the Boundaries."
      }
      if(result_tb_list_apoe[[1]]$genotype[i] == "Wild Type" && result_tb_list_apoe[[2]]$genotype[i] == "Homozygous Mutant"){
        result_tb_list_apoe_final$genotype[i] <- "E2,E2"
      } else if(result_tb_list_apoe[[1]]$genotype[i] == "Wild Type" && result_tb_list_apoe[[2]]$genotype[i] == "Wild Type"){
        result_tb_list_apoe_final$genotype[i] <- "E3,E3"
      } else if(result_tb_list_apoe[[1]]$genotype[i] == "Homozygous Mutant" && result_tb_list_apoe[[2]]$genotype[i] == "Wild Type"){
        result_tb_list_apoe_final$genotype[i] <- "E4,E4"
      } else if(result_tb_list_apoe[[1]]$genotype[i] == "Wild Type" && result_tb_list_apoe[[2]]$genotype[i] == "Heterozygous"){
        result_tb_list_apoe_final$genotype[i] <- "E2,E3"
      } else if(result_tb_list_apoe[[1]]$genotype[i] == "Heterozygous" && result_tb_list_apoe[[2]]$genotype[i] == "Heterozygous"){
        result_tb_list_apoe_final$genotype[i] <- "E2,E4"
      } else if(result_tb_list_apoe[[1]]$genotype[i] == "Heterozygous" && result_tb_list_apoe[[2]]$genotype[i] == "Wild Type"){
        result_tb_list_apoe_final$genotype[i] <- "E3,E4"
      } else if(result_tb_list_apoe[[1]]$genotype[i] == "NTC" && result_tb_list_apoe[[2]]$genotype[i] == "NTC"){
        result_tb_list_apoe_final$genotype[i] <- "NTC"
      } else {
        result_tb_list_apoe_final$genotype[i] <- "Positive Control"
      }
      
      
    }
    result_tb_list_apoe_final$Parameter <- "APOE"
    result_tb_list_apoe_final <- cbind(Well = rownames(result_tb_list_apoe_final), result_tb_list_apoe_final)
    result_tb_list_apoe_final <- result_tb_list_apoe_final[,c(1,7,9,8)]
    colnames(result_tb_list_apoe_final) <- c("Well", "Sample Name", "Parameter", "Genotype")
    apoe_table_final <- list(APOE1 = apoe1_table, APOE2 = apoe2_table, APOE = result_tb_list_apoe_final)
    
  } else if (ncol(data_list_apoe[[1]]) >1 && ncol(data_list_apoe[[1]]) != ncol(data_list_apoe[[2]])){
    apoe_table_final <- list(data.frame(), data.frame(), data.frame())
    print("APOE genotypes cannot be calculated, please exclude the wells with only one of the APOE parameters were loaded.")
  } else {
    apoe_table_final <- list(data.frame(), data.frame(), data.frame())
  }
  
  
  
  if(length(result_tb_list_11) > 0){
    for (i in 1:length(result_tb_list_11)) {
      result_tb_list_11[[i]]$Parameter <- c(rep(names(result_tb_list_11[i]), nrow(result_tb_list_11[[i]])))
      result_tb_list_11[[i]] <- cbind(rownames(result_tb_list_11[[i]]), result_tb_list_11[[i]])
      result_tb_list_11[[i]] <- result_tb_list_11[[i]][,c(1,7,9,8,4,5)]
      colnames(result_tb_list_11[[i]]) <- c("Well", "Sample Name", "Parameter", "Genotype", "peak_1", "peak_2")
    }
  }
  
  result_tb_cvd <- c(result_tb_list_11, list(result_tb_pai), apoe_table_final)
  names(result_tb_cvd) <- c(names(result_tb_list_11), "PAI", "APOE1", "APOE2", "APOE")
  
  result_tb_cvd <- result_tb_cvd[sapply(result_tb_cvd, function(x) dim(x)[1]) > 0]
  
  return(list(result_tb_cvd,
              data_list_graph_cvd))
  

  }
