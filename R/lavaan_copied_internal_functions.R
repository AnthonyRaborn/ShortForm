# A copy of the internal lavaan functions
# it is copied here to help pass the automated CRAN checks
# all code here is owned by the various authors of the lavaan package, with minor modifications

lav_partable_full <- function(partable = NULL,
                              strict.exo = FALSE, free = FALSE,
                              start = FALSE) {
  stopifnot(
    !is.null(partable$lhs), !is.null(partable$op),
    !is.null(partable$rhs)
  )
  meanstructure <- any(partable$op == "~1")
  nblocks <- lav_partable_nblocks(partable)
  lv.names <- vnames(partable, type = "lv")
  ov.names <- vnames(partable, type = "ov")
  ov.names.x <- vnames(partable, type = "ov.x")
  ov.names.nox <- vnames(partable, type = "ov.nox")
  lv.names.x <- vnames(partable, type = "lv.x")
  ov.names.y <- vnames(partable, type = "ov.y")
  lv.names.y <- vnames(partable, type = "lv.y")
  lvov.names.y <- c(ov.names.y, lv.names.y)
  ov.names.ord <- vnames(partable, type = "ov.ord")
  ov.names.ind <- vnames(partable, type = "ov.ind")
  l.lhs <- r.rhs <- op <- character(0)
  l.lhs <- rep(lv.names, each = length(ov.names.nox))
  l.rhs <- rep(ov.names.nox, times = length(lv.names))
  l.op <- rep("=~", length(l.lhs))
  ov.lhs <- ov.rhs <- ov.op <- character(0)
  OV <- ov.names.nox
  nx <- length(OV)
  idx <- lower.tri(matrix(0, nx, nx), diag = TRUE)
  ov.lhs <- rep(OV, each = nx)[idx]
  ov.rhs <- rep(OV, times = nx)[idx]
  ov.op <- rep("~~", length(ov.lhs))
  if (!strict.exo && length(ov.names.x) > 0L) {
    OV <- ov.names.x
    nx <- length(OV)
    idx <- lower.tri(matrix(0, nx, nx), diag = TRUE)
    more.lhs <- rep(OV, each = nx)[idx]
    more.rhs <- rep(OV, times = nx)[idx]
    ov.lhs <- c(ov.lhs, more.lhs)
    ov.rhs <- c(ov.rhs, more.rhs)
    ov.op <- c(ov.op, rep("~~", length(more.lhs)))
  }
  lv.lhs <- lv.rhs <- lv.op <- character(0)
  nx <- length(lv.names)
  idx <- lower.tri(matrix(0, nx, nx), diag = TRUE)
  lv.lhs <- rep(lv.names, each = nx)[idx]
  lv.rhs <- rep(lv.names, times = nx)[idx]
  lv.op <- rep("~~", length(lv.lhs))
  r.lhs <- r.rhs <- r.op <- character(0)
  if (any(partable$op == "~")) {
    eqs.names <- unique(c(
      partable$lhs[partable$op == "~"],
      partable$rhs[partable$op == "~"]
    ))
    eqs.y <- eqs.names
    if (strict.exo) {
      x.idx <- which(eqs.names %in% ov.names.x)
      if (length(x.idx) > 0L) {
        eqs.y <- eqs.names[-x.idx]
      }
    }
    eqs.x <- eqs.names
    r.lhs <- rep(eqs.y, each = length(eqs.x))
    r.rhs <- rep(eqs.x, times = length(eqs.y))
    idx <- which(r.lhs == r.rhs)
    r.lhs <- r.lhs[-idx]
    r.rhs <- r.rhs[-idx]
    bad.idx <- which(r.lhs %in% ov.names.ind & r.rhs %in%
      lv.names)
    if (length(bad.idx) > 0L) {
      r.lhs <- r.lhs[-bad.idx]
      r.rhs <- r.rhs[-bad.idx]
    }
    r.op <- rep("~", length(r.rhs))
  }
  int.lhs <- int.rhs <- int.op <- character(0)
  if (meanstructure) {
    if (strict.exo) {
      int.lhs <- c(ov.names.nox, lv.names)
    }
    else {
      int.lhs <- c(ov.names, lv.names)
    }
    int.rhs <- rep("", length(int.lhs))
    int.op <- rep("~1", length(int.lhs))
  }
  th.lhs <- th.rhs <- th.op <- character(0)
  if (length(ov.names.ord) > 0L) {
    tmp <- strsplit(
      vnames(partable, "th"),
      "\\|"
    )
    th.lhs <- sapply(tmp, function(x) x[1])
    th.rhs <- sapply(tmp, function(x) x[2])
    th.op <- rep("|", length(th.lhs))
  }
  delta.lhs <- delta.rhs <- delta.op <- character(0)
  if (nblocks > 1L && length(ov.names.ord) > 0L) {
    delta.lhs <- ov.names.ord
    delta.rhs <- ov.names.ord
    delta.op <- rep("~*~", length(delta.lhs))
  }
  lhs <- c(l.lhs, ov.lhs, lv.lhs, r.lhs, int.lhs, th.lhs, delta.lhs)
  rhs <- c(l.rhs, ov.rhs, lv.rhs, r.rhs, int.rhs, th.rhs, delta.rhs)
  op <- c(l.op, ov.op, lv.op, r.op, int.op, th.op, delta.op)
  block <- 1L
  if (nblocks > 1) {
    block <- rep(1:nblocks, each = length(lhs))
    lhs <- rep(lhs, times = nblocks)
    op <- rep(op, times = nblocks)
    rhs <- rep(rhs, times = nblocks)
  }
  LIST <- data.frame(
    lhs = lhs, op = op, rhs = rhs, block = block,
    stringsAsFactors = FALSE
  )
  if (free) {
    LIST$free <- rep(0L, nrow(LIST))
  }
  if (start) {
    LIST$start <- rep(0, nrow(LIST))
  }
  LIST
}

vnames <- function(partable, type = NULL, ..., warn = FALSE, ov.x.fatal = FALSE) {
  if (length(partable$lhs) == 0) {
    return(character(0L))
  }
  dotdotdot <- list(...)
  type.list <- c(
    "ov", "ov.x", "ov.nox", "ov.model", "ov.y",
    "ov.num", "ov.ord", "ov.ind", "ov.orphan", "ov.interaction",
    "th", "th.mean", "lv", "lv.regular", "lv.formative",
    "lv.x", "lv.y", "lv.nox", "lv.nonnormal", "lv.interaction",
    "eqs.y", "eqs.x"
  )
  stopifnot(is.list(partable), !missing(type), type %in% c(
    type.list,
    "all"
  ))
  if (length(type) == 1L && type == "all") {
    type <- type.list
  }
  if (is.null(partable$block)) {
    partable$block <- rep(1L, length(partable$lhs))
  }
  nblocks <- lav_partable_nblocks(partable)
  block.select <- lav_partable_block_values(partable)
  ndotdotdot <- length(dotdotdot)
  if (ndotdotdot > 0L) {
    dot.names <- names(dotdotdot)
    block.select <- rep(TRUE, length(partable$lhs))
    for (dot in seq_len(ndotdotdot)) {
      block.var <- dot.names[dot]
      block.val <- dotdotdot[[block.var]]
      if (is.null(partable[[block.var]])) {
        stop(
          "lavaan ERROR: selection variable `", block.var,
          " not found in the parameter table."
        )
      }
      else {
        if (!all(block.val %in% partable[[block.var]])) {
          stop(
            "lavaan ERROR: ", block.var, " column does not contain value `",
            block.val, "'"
          )
        }
        block.select <- (block.select & partable[[block.var]] %in%
          block.val)
      }
    }
    block.select <- unique(partable$block[block.select])
    if (length(block.select) == 0L) {
      warnings("lavaan WARNING: no blocks selected.")
    }
  }
  OUT <- vector("list", length = nblocks)
  OUT$ov <- vector("list", length = nblocks)
  OUT$ov.x <- vector("list", length = nblocks)
  OUT$ov.nox <- vector("list", length = nblocks)
  OUT$ov.model <- vector("list", length = nblocks)
  OUT$ov.y <- vector("list", length = nblocks)
  OUT$ov.num <- vector("list", length = nblocks)
  OUT$ov.ord <- vector("list", length = nblocks)
  OUT$ov.ind <- vector("list", length = nblocks)
  OUT$ov.orphan <- vector("list", length = nblocks)
  OUT$ov.interaction <- vector("list", length = nblocks)
  OUT$th <- vector("list", length = nblocks)
  OUT$th.mean <- vector("list", length = nblocks)
  OUT$lv <- vector("list", length = nblocks)
  OUT$lv.regular <- vector("list", length = nblocks)
  OUT$lv.formative <- vector("list", length = nblocks)
  OUT$lv.x <- vector("list", length = nblocks)
  OUT$lv.y <- vector("list", length = nblocks)
  OUT$lv.nox <- vector("list", length = nblocks)
  OUT$lv.nonnormal <- vector("list", length = nblocks)
  OUT$lv.interaction <- vector("list", length = nblocks)
  OUT$eqs.y <- vector("list", length = nblocks)
  OUT$eqs.x <- vector("list", length = nblocks)
  for (b in block.select) {
    lv.names <- unique(partable$lhs[partable$block == b &
      (partable$op == "=~" | partable$op == "<~")])
    int.names <- unique(partable$rhs[partable$block == b &
      grepl(":", partable$rhs)])
    n.int <- length(int.names)
    if (n.int > 0L) {
      ok.idx <- logical(n.int)
      for (iv in seq_len(n.int)) {
        NAMES <- strsplit(int.names[iv], ":", fixed = TRUE)[[1L]]
        if (sum(NAMES %in% lv.names) > 0L) {
          ok.idx[iv] <- TRUE
        }
      }
      lv.interaction <- int.names[ok.idx]
      lv.names <- c(lv.names, lv.interaction)
    }
    else {
      lv.interaction <- character(0L)
    }
    if ("lv" %in% type) {
      OUT$lv[[b]] <- lv.names
    }
    if ("lv.regular" %in% type) {
      out <- unique(partable$lhs[partable$block == b &
        partable$op == "=~"])
      OUT$lv.regular[[b]] <- out
    }
    if ("lv.interaction" %in% type) {
      OUT$lv.interaction[[b]] <- lv.interaction
    }
    if ("lv.formative" %in% type) {
      out <- unique(partable$lhs[partable$block == b &
        partable$op == "<~"])
      OUT$lv.formative[[b]] <- out
    }
    if (!(length(type) == 1L && type %in% c(
      "lv", "lv.regular",
      "lv.nonnormal"
    ))) {
      eqs.y <- unique(partable$lhs[partable$block == b &
        partable$op == "~"])
    }
    if ("eqs.y" %in% type) {
      OUT$eqs.y[[b]] <- eqs.y
    }
    if (!(length(type) == 1L && type %in% c(
      "lv", "lv.regular",
      "lv.nonnormal", "lv.x"
    ))) {
      eqs.x <- unique(partable$rhs[partable$block == b &
        (partable$op == "~" | partable$op == "<~")])
    }
    if ("eqs.x" %in% type) {
      OUT$eqs.x[[b]] <- eqs.x
    }
    if (!(length(type) == 1L && type %in% c(
      "lv", "lv.regular",
      "lv.nonnormal"
    ))) {
      v.ind <- unique(partable$rhs[partable$block == b &
        partable$op == "=~"])
    }
    if (!(length(type) == 1L && type %in% c(
      "lv", "lv.regular",
      "lv.nonnormal", "lv.x", "lv.y"
    ))) {
      ov.ind <- v.ind[!v.ind %in% lv.names]
      ov.y <- eqs.y[!eqs.y %in% c(lv.names, ov.ind)]
      ov.x <- eqs.x[!eqs.x %in% c(lv.names, ov.ind, ov.y)]
    }
    if (!(length(type) == 1L && type %in% c(
      "lv", "lv.regular",
      "lv.nonnormal", "lv.x", "lv.y"
    ))) {
      ov.cov <- c(partable$lhs[partable$block == b & partable$op ==
        "~~" & !partable$lhs %in% lv.names], partable$rhs[partable$block ==
        b & partable$op == "~~" & !partable$rhs %in%
        lv.names])
      ov.int <- partable$lhs[partable$block == b & (partable$op ==
        "~1" | partable$op == "|") & !partable$lhs %in%
        lv.names]
      ov.tmp <- c(ov.ind, ov.y, ov.x)
      ov.extra <- unique(c(ov.cov, ov.int))
      ov.names <- c(ov.tmp, ov.extra[!ov.extra %in% ov.tmp])
    }
    if ("ov" %in% type) {
      OUT$ov[[b]] <- ov.names
    }
    if ("ov.ind" %in% type) {
      OUT$ov.ind[[b]] <- ov.ind
    }
    if ("ov.interaction" %in% type) {
      ov.int.names <- ov.names[grepl(":", ov.names)]
      n.int <- length(ov.int.names)
      if (n.int > 0L) {
        ov.names.noint <- ov.names[!ov.names %in% ov.int.names]
        ok.idx <- logical(n.int)
        for (iv in seq_len(n.int)) {
          NAMES <- strsplit(ov.int.names[iv], ":", fixed = TRUE)[[1L]]
          if (all(NAMES %in% ov.names.noint)) {
            ok.idx[iv] <- TRUE
          }
        }
        ov.interaction <- ov.int.names[ok.idx]
      }
      else {
        ov.interaction <- character(0L)
      }
      OUT$ov.interaction[[b]] <- ov.interaction
    }
    if (any(type %in% c(
      "ov.x", "ov.nox", "ov.num", "ov.model",
      "th.mean", "lv.nonnormal"
    ))) {
      if (is.null(partable$user)) {
        partable$user <- rep(1L, length(partable$lhs))
      }
      vars <- c(
        partable$lhs[partable$block == b & partable$op ==
          "~1" & partable$user == 1], partable$lhs[partable$block ==
          b & partable$op == "~~" & partable$user == 1],
        partable$rhs[partable$block == b & partable$op ==
          "~~" & partable$user == 1]
      )
      idx.no.x <- which(ov.x %in% vars)
      if (length(idx.no.x)) {
        if (ov.x.fatal) {
          stop(
            "lavaan ERROR: model syntax contains variance/covariance/intercept formulas\n  involving (an) exogenous variable(s): [",
            paste(ov.x[idx.no.x], collapse = " "), "];\n  Please remove them and try again."
          )
        }
        if (warn) {
          warning(
            "lavaan WARNING: model syntax contains variance/covariance/intercept formulas\n  involving (an) exogenous variable(s): [",
            paste(ov.x[idx.no.x], collapse = " "), "];\n  Please use fixed.x=FALSE or leave them alone"
          )
        }
        ov.x <- ov.x[-idx.no.x]
      }
      ov.tmp.x <- ov.x
      if (!is.null(partable$exo)) {
        ov.cov <- c(
          partable$lhs[partable$block == b &
            partable$op == "~~" & partable$exo == 1L],
          partable$rhs[partable$block == b & partable$op ==
            "~~" & partable$exo == 1L]
        )
        ov.int <- partable$lhs[partable$block == b &
          partable$op == "~1" & partable$exo == 1L]
        ov.extra <- unique(c(ov.cov, ov.int))
        ov.tmp.x <- c(ov.tmp.x, ov.extra[!ov.extra %in%
          ov.tmp.x])
      }
      ov.names.x <- ov.tmp.x
    }
    if ("ov.x" %in% type) {
      OUT$ov.x[[b]] <- ov.names.x
    }
    if ("ov.orphan" %in% type) {
      OUT$ov.orphan[[b]] <- ov.extra
    }
    if (any(type %in% c(
      "ov.nox", "ov.num", "ov.model",
      "th.mean", "lv.nonnormal"
    ))) {
      ov.names.nox <- ov.names[!ov.names %in% ov.names.x]
    }
    if ("ov.nox" %in% type) {
      OUT$ov.nox[[b]] <- ov.names.nox
    }
    if ("ov.model" %in% type) {
      if (any(partable$block == b & partable$op == "~" &
        partable$exo == 1L)) {
        OUT$ov.model[[b]] <- ov.names.nox
      }
      else {
        OUT$ov.model[[b]] <- ov.names
      }
    }
    if (any(type %in% c(
      "ov.ord", "th", "th.mean", "ov.num",
      "lv.nonnormal"
    ))) {
      tmp <- unique(partable$lhs[partable$block == b &
        partable$op == "|"])
      ord.names <- ov.names[ov.names %in% tmp]
    }
    if ("ov.ord" %in% type) {
      OUT$ov.ord[[b]] <- ord.names
    }
    if (any(type %in% c("ov.num", "lv.nonnormal"))) {
      ov.num <- ov.names.nox[!ov.names.nox %in% ord.names]
    }
    if ("ov.num" %in% type) {
      OUT$ov.num[[b]] <- ov.num
    }
    if ("lv.nonnormal" %in% type) {
      lv.reg <- unique(partable$lhs[partable$block ==
        b & partable$op == "=~"])
      if (length(lv.reg) > 0L) {
        out <- unlist(lapply(lv.reg, function(x) {
          tmp.ind <- unique(partable$rhs[partable$block ==
            b & partable$op == "=~" & partable$lhs ==
            x])
          if (!all(tmp.ind %in% ov.num)) {
            return(x)
          }
          else {
            return(character(0))
          }
        }))
        OUT$lv.nonnormal[[b]] <- out
      }
      else {
        OUT$lv.nonnormal[[b]] <- character(0)
      }
    }
    if (any(c("th", "th.mean") %in% type)) {
      TH.lhs <- partable$lhs[partable$block == b & partable$op ==
        "|"]
      TH.rhs <- partable$rhs[partable$block == b & partable$op ==
        "|"]
    }
    if ("th" %in% type) {
      if (length(ord.names) > 0L) {
        out <- unlist(lapply(ord.names, function(x) {
          idx <- which(x == TH.lhs)
          TH <- unique(paste(TH.lhs[idx], "|", TH.rhs[idx],
            sep = ""
          ))
          sort(TH)
        }))
      }
      else {
        out <- character(0L)
      }
      OUT$th[[b]] <- out
    }
    if ("th.mean" %in% type) {
      if (length(ov.names.nox) > 0L) {
        out <- unlist(lapply(ov.names.nox, function(x) {
          if (x %in% ord.names) {
            idx <- which(x == TH.lhs)
            TH <- unique(paste(TH.lhs[idx], "|", TH.rhs[idx],
              sep = ""
            ))
            sort(TH)
          }
          else {
            x
          }
        }))
      }
      else {
        out <- character(0L)
      }
      OUT$th.mean[[b]] <- out
    }
    if (any(c("lv.x", "lv.nox") %in% type)) {
      tmp <- lv.names[!lv.names %in% c(v.ind, eqs.y)]
      lv.names.x <- lv.names[lv.names %in% tmp]
    }
    if ("lv.x" %in% type) {
      OUT$lv.x[[b]] <- lv.names.x
    }
    if ("ov.y" %in% type) {
      tmp <- eqs.y[!eqs.y %in% c(v.ind, eqs.x, lv.names)]
      OUT$ov.y[[b]] <- ov.names[ov.names %in% tmp]
    }
    if ("lv.y" %in% type) {
      tmp <- eqs.y[!eqs.y %in% c(v.ind, eqs.x) & eqs.y %in%
        lv.names]
      OUT$lv.y[[b]] <- lv.names[lv.names %in% tmp]
    }
    if ("lv.nox" %in% type) {
      OUT$lv.nox[[b]] <- lv.names[!lv.names %in% lv.names.x]
    }
  }
  if (length(type) == 1L) {
    OUT <- OUT[[type]]
    if (ndotdotdot == 0L) {
      OUT <- unique(unlist(OUT))
    }
    else if (length(block.select) == 1L) {
      OUT <- OUT[[block.select]]
    }
    else {
      OUT <- OUT[block.select]
    }
  }
  else {
    OUT <- OUT[type]
  }
  OUT
}

lav_partable_nblocks <- function(partable) {
  if (is.null(partable$block)) {
    nblocks <- 1L
  }
  else {
    tmp <- partable$block[partable$block > 0L]
    nblocks <- length(unique(stats::na.omit(tmp)))
  }
  nblocks
}

lav_partable_block_values <- function(partable) {
  if (is.null(partable$block)) {
    block.values <- 1L
  }
  else {
    tmp <- partable$block[partable$block > 0L]
    block.values <- unique(stats::na.omit(tmp))
  }
  block.values
}
