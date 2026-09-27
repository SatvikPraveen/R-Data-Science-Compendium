# Figures for the paper, drawn from the summarised results only.

source(file.path("analysis", "scripts", "00-setup.R"))

palette_methods <- c(t = "#1b9e77", bca = "#d95f02", percentile = "#7570b3",
                     normal = "#e7298a", basic = "#66a61e")

# Figure 1: coverage by sample size, one panel per skewness level ------------
cov <- utils::read.csv(file.path(results_dir, "coverage-performance.csv"))
cov <- cov[cov$measure == "coverage", ]

grDevices::png(file.path(figures_dir, "coverage.png"), width = 2400,
               height = 900, res = 300)
op <- graphics::par(mfrow = c(1, 3), mar = c(4, 4, 2.5, 0.5), mgp = c(2.4, 0.7, 0),
                    cex = 0.75)
sigmas <- sort(unique(cov$sigma))
ns <- sort(unique(cov$n))
for (s in sigmas) {
  d <- cov[cov$sigma == s, ]
  graphics::plot(NA, xlim = range(log2(ns)) + c(-0.2, 0.2),
                 ylim = c(min(cov$estimate) - 0.01, 0.97), xaxt = "n",
                 xlab = "Sample size n", ylab = "Coverage of nominal 95% interval",
                 main = bquote(sigma == .(s)))
  graphics::axis(1, at = log2(ns), labels = ns)
  graphics::abline(h = 0.95, lty = 2, col = "grey40")
  offsets <- seq(-0.12, 0.12, length.out = length(palette_methods))
  for (m in seq_along(palette_methods)) {
    dm <- d[d$method == names(palette_methods)[m], ]
    dm <- dm[order(dm$n), ]
    x <- log2(dm$n) + offsets[m]
    graphics::lines(x, dm$estimate, col = palette_methods[m])
    graphics::segments(x, dm$estimate - 1.96 * dm$mcse, x,
                       dm$estimate + 1.96 * dm$mcse, col = palette_methods[m])
    graphics::points(x, dm$estimate, pch = 19, cex = 0.6,
                     col = palette_methods[m])
  }
  if (s == sigmas[1]) {
    graphics::legend("bottomright", legend = names(palette_methods),
                     col = palette_methods, pch = 19, lty = 1, bty = "n",
                     cex = 0.9)
  }
}
graphics::par(op)
grDevices::dev.off()

# Figure 2: bias of naive and nested CV estimates ----------------------------
cvp <- utils::read.csv(file.path(results_dir, "cv-selection-performance.csv"))
cvp <- cvp[cvp$measure == "bias", ]
cvp$label <- sprintf("n=%d\np=%d", cvp$n, cvp$p)
cvp$series <- paste(cvp$method, cvp$aggregation, sep = ", ")

grDevices::png(file.path(figures_dir, "cv-bias.png"), width = 2400,
               height = 1050, res = 300)
op <- graphics::par(mfrow = c(1, 2), mar = c(4.5, 4.2, 2.5, 0.5),
                    mgp = c(2.6, 0.7, 0), cex = 0.75)
cols <- c("naive, fold" = "#d95f02", "naive, pooled" = "#fdae6b",
          "nested, fold" = "#7570b3", "nested, pooled" = "#1b9e77")
offsets <- stats::setNames(c(-0.24, -0.08, 0.08, 0.24), names(cols))
for (b in sort(unique(cvp$beta))) {
  d <- cvp[cvp$beta == b, ]
  labs <- unique(d$label[order(d$p, d$n)])
  graphics::plot(NA, xlim = c(0.5, length(labs) + 0.5),
                 ylim = range(c(cvp$estimate - 2 * cvp$mcse,
                                cvp$estimate + 2 * cvp$mcse, 0)),
                 xaxt = "n", xlab = "", ylab = "Bias of RMSE estimate",
                 main = if (b == 0) "Pure noise (beta = 0)" else
                   sprintf("One true predictor (beta = %.1f)", b))
  graphics::axis(1, at = seq_along(labs), labels = labs, padj = 0.5)
  graphics::abline(h = 0, lty = 2, col = "grey40")
  for (m in names(cols)) {
    dm <- d[d$series == m, ]
    x <- match(dm$label, labs) + offsets[[m]]
    graphics::segments(x, dm$estimate - 1.96 * dm$mcse, x,
                       dm$estimate + 1.96 * dm$mcse, col = cols[m], lwd = 1.5)
    graphics::points(x, dm$estimate, pch = 19, col = cols[m])
  }
  if (b == 0) {
    graphics::legend("bottomright", legend = names(cols), col = cols,
                     pch = 19, bty = "n", cex = 0.85)
  }
}
graphics::par(op)
grDevices::dev.off()

message("wrote figures to ", figures_dir)
