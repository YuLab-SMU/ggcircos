# Reproduce circlize book chapter 16 with ggcircos.
#   https://jokergoo.github.io/circlize_book/book/a-complex-example-of-chord-diagram.html
# Run:  Rscript notes/repro/circlize-ch16-reproduction.R
# Writes /tmp/repro/ggcircos-circlize-ch16.png (edit `out` below to change it).
suppressMessages(library(ggcircos))
suppressMessages(library(ggplot2))

data_file <- "/tmp/repro/chromatin_transition.RData"
if (!file.exists(data_file)) {
  dir.create(dirname(data_file), showWarnings = FALSE, recursive = TRUE)
  download.file(
    "https://jokergoo.github.io/circlize_book/data/chromatin_transition.RData",
    data_file, quiet = TRUE
  )
}
load(data_file)

out <- "/tmp/repro/ggcircos-circlize-ch16.png"

# -- the original figure's data prep (circlize book ch. 16) -------------------
diag(mat) <- 0                                  # keep only changed states
n_states <- nrow(mat)
all_states <- rownames(mat)
rownames(mat) <- paste0("R_", seq_len(n_states))
colnames(mat) <- paste0("C_", seq_len(n_states))
dimnames(meth_mat_1) <- dimnames(mat)
dimnames(meth_mat_2) <- dimnames(mat)

state_col <- c(TssA = "#E41A1C", TssAFlnk = "#E41A1C", TxFlnk = "#E41A1C",
               Tx = "#E41A1C", TxWk = "#E41A1C", EnhG = "#E41A1C",
               Enh = "#E41A1C", `ZNF/Rpts` = "#E41A1C", Het = "#377EB8",
               TssBiv = "#377EB8", BivFlnk = "#377EB8", EnhBiv = "#377EB8",
               ReprPC = "#377EB8", ReprPCWk = "#377EB8", Quies = "black")

links <- data.frame(
  from = paste0("R_", rep(seq_len(n_states), each = n_states)),
  to   = paste0("C_", rep(seq_len(n_states), n_states)),
  bp   = as.vector(mat),
  stringsAsFactors = FALSE
)
links <- links[links$bp > 0, ]
ids <- unique(c(links$from, links$to))
r_i <- as.integer(sub("^R_", "", links$from))
c_i <- as.integer(sub("^C_", "", links$to))

# link colour: the *source* state; opacity: two tiers, big transitions stand out
q70 <- quantile(links$bp, 0.7)
links$ccol <- state_col[all_states[r_i]]
links$calpha <- ifelse(links$bp > q70, 0.63, 0.13)

# per-link methylation on both ends
links$m1 <- meth_mat_1[cbind(r_i, c_i)]
links$m2 <- meth_mat_2[cbind(r_i, c_i)]
mk_tracks <- function(val) {
  keep <- !is.na(val) & links$from %in% ids
  keep_to <- !is.na(val) & links$to %in% ids
  rbind(
    data.frame(id = links$from[keep], m = val[keep], stringsAsFactors = FALSE),
    data.frame(id = links$to[keep_to], m = val[keep_to], stringsAsFactors = FALSE)
  )
}
trk_m1 <- mk_tracks(links$m1)
trk_m2 <- mk_tracks(links$m2)

nodes <- data.frame(id = ids, stringsAsFactors = FALSE)
nodes$col <- state_col[all_states[as.integer(sub("^[RC]_", "", ids))]]
trk_kb <- data.frame(id = ids, kb = rowSums(mat)[as.integer(sub("^[RC]_", "", ids))] / 1000,
                     stringsAsFactors = FALSE)

# -- the plot ----------------------------------------------------------------
# gap = 0.00278 matches circlize's small.gap = 1 degree; sector widths follow
# their total weight through weight = "bp", exactly like chordDiagram()
p <- ggcircos(links, nodes = nodes, gap = 0.00278, weight = "bp",
              tracks = c(kb = 0.12, m1 = 0.06, m2 = 0.06)) +
  geom_chord(aes(fill = ccol, alpha = calpha)) +
  geom_node_arc(aes(fill = col)) +
  geom_track(data = trk_kb, aes(x = kb), node = id, band = "kb", fill = "grey60") +
  geom_track(data = trk_m1, aes(x = m), node = id, band = "m1", fill = "#E41A1C") +
  geom_track(data = trk_m2, aes(x = m), node = id, band = "m2", fill = "#377EB8") +
  geom_track_axis(band = "kb", limits = c(0, 2200)) +
  scale_fill_identity(guide = "none") +
  scale_alpha_identity(guide = "none")

png(out, 900, 900, res = 110)
print(p)
dev.off()
message("wrote ", out)
