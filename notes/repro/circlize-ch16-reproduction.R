# Reproduce circlize book chapter 16 with ggcircos.
#   https://jokergoo.github.io/circlize_book/book/a-complex-example-of-chord-diagram.html
# Run from the package root:  Rscript notes/repro/circlize-ch16-reproduction.R
suppressMessages(library(ggcircos)); suppressMessages(library(ggplot2))

data_file <- file.path(tempdir(), "chromatin_transition.RData")
if (!file.exists(data_file)) {
  download.file(
    "https://jokergoo.github.io/circlize_book/data/chromatin_transition.RData",
    data_file, quiet = TRUE
  )
}
load(data_file)
diag(mat) = 0
n <- nrow(mat); all_states <- rownames(mat)
links <- data.frame(from = paste0("R_", rep(seq_len(n), each = n)),
                    to = paste0("C_", rep(seq_len(n), n)), bp = as.vector(mat), stringsAsFactors = FALSE)
links <- links[links$bp > 0, ]
ids <- unique(c(links$from, links$to)); idx <- as.integer(sub("^[RC]_", "", ids))
state_col <- c("TssA"="#E41A1C","TssAFlnk"="#E41A1C","TxFlnk"="#E41A1C","Tx"="#E41A1C","TxWk"="#E41A1C",
  "EnhG"="#E41A1C","Enh"="#E41A1C","ZNF/Rpts"="#E41A1C","Het"="#377EB8","TssBiv"="#377EB8",
  "BivFlnk"="#377EB8","EnhBiv"="#377EB8","ReprPC"="#377EB8","ReprPCWk"="#377EB8","Quies"="black")
nodes <- data.frame(id = ids, state = all_states[idx], stringsAsFactors = FALSE)
nodes$col <- state_col[nodes$state]
q <- quantile(links$bp, 0.7)
links$ccol <- state_col[all_states[as.integer(sub("^R_", "", links$from))]]
links$calpha <- ifelse(links$bp > q, 0.63, 0.13)
r_i <- as.integer(sub("^R_", "", links$from)); c_i <- as.integer(sub("^C_", "", links$to))
links$m1 <- meth_mat_1[cbind(r_i, c_i)]; links$m2 <- meth_mat_2[cbind(r_i, c_i)]
# --- per-link methylation rectangles on both ends
mk <- function(side, val) {
  keep <- links[[side]] %in% ids & !is.na(val)
  data.frame(id = links[[side]][keep], m = val[keep], stringsAsFactors = FALSE)
}
trk_m1 <- rbind(mk("from", links$m1), mk("to", links$m1))
trk_m2 <- rbind(mk("from", links$m2), mk("to", links$m2))
# --- kb track: total bp per sector
trk_kb <- data.frame(id = ids, kb = rowSums(mat)[idx]/1000, stringsAsFactors = FALSE)
p <- ggcircos(links, nodes = nodes, gap = 0.02, weight = "bp",
              tracks = c(kb = 0.12, m1 = 0.06, m2 = 0.06)) +
  geom_chord(aes(fill = ccol, alpha = calpha), curvature = 0) +
  geom_node_arc(aes(fill = col)) +
  geom_track(data = trk_kb, aes(x = kb), node = id, band = "kb", fill = "grey60") +
  geom_track(data = trk_m1, aes(x = m), node = id, band = "m1", fill = "#E41A1C") +
  geom_track(data = trk_m2, aes(x = m), node = id, band = "m2", fill = "#377EB8") +
  geom_track_axis(band = "kb", limits = c(0, 2200)) +
  scale_fill_identity(guide = "none") + scale_alpha_identity(guide = "none")
png("/tmp/repro/FINAL3.png", 900, 900, res=110); print(p); dev.off()
cat("saved\n")
