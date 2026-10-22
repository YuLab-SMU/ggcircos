load("/tmp/repro/chromatin.RData")
suppressMessages(library(circlize))
diag(mat) = 0
all_states = rownames(mat); n_states = nrow(mat)
rownames(mat) = paste0("R_", seq_len(n_states))
colnames(mat) = paste0("C_", seq_len(n_states))
dimnames(meth_mat_1) = dimnames(mat); dimnames(meth_mat_2) = dimnames(mat)
state_col = c("TssA"="#E41A1C","TssAFlnk"="#E41A1C","TxFlnk"="#E41A1C","Tx"="#E41A1C","TxWk"="#E41A1C",
  "EnhG"="#E41A1C","Enh"="#E41A1C","ZNF/Rpts"="#E41A1C","Het"="#377EB8","TssBiv"="#377EB8",
  "BivFlnk"="#377EB8","EnhBiv"="#377EB8","ReprPC"="#377EB8","ReprPCWk"="#377EB8","Quies"="black")
state_col2 = c(state_col, state_col); names(state_col2) = c(rownames(mat), colnames(mat))
colmat = rep(state_col2[rownames(mat)], n_states)
colmat = rgb(t(col2rgb(colmat)), maxColorValue = 255)
qati = quantile(mat, 0.7)
colmat[mat > qati] = paste0(colmat[mat > qati], "A0")
colmat[mat <= qati] = paste0(colmat[mat <= qati], "20")
dim(colmat) = dim(mat)
circos.par(cell.padding = c(0,0,0,0), points.overflow.warning = FALSE)
cdm_res = chordDiagram(mat, col = colmat, grid.col = state_col2, directional = TRUE,
  annotationTrack = "grid", big.gap = 10, small.gap = 1,
  preAllocateTracks = list(track.height = 0.1), link.target.prop = FALSE)
circos.track(track.index = 2, panel.fun = function(x, y) {
  if (abs(CELL_META$cell.start.degree - CELL_META$cell.end.degree) > 3) {
    sn = CELL_META$sector.index; i_state = as.numeric(gsub("(C|R)_", "", sn))
    circos.text(CELL_META$xcenter, CELL_META$ycenter, i_state, col="white", font=2, cex=0.7, adj=c(0.5,0.5), niceFacing=TRUE)
    xlim = CELL_META$xlim; breaks = seq(0, xlim[2], by = 4e5)
    circos.axis(major.at = breaks, labels = paste0(breaks/1000, "KB"), labels.cex = 0.5)
  }
}, bg.border = NA)
abs_max = quantile(abs(c(meth_mat_1, meth_mat_2) - 0.5), 0.95, na.rm = TRUE)
col_fun = colorRamp2(c(0.5-abs_max, 0.5, 0.5+abs_max), c("blue","white","red"))
ylim = get.cell.meta.data("ylim", sector.index = rownames(mat)[1], track.index = 1)
y1 = ylim[1] + (ylim[2]-ylim[1])*0.4; y2 = ylim[2]
for (i in seq_len(nrow(cdm_res))) {
  if (cdm_res$value1[i] > 0) {
    for (side in c(1, 2)) {
      xx = if (side == 1) cdm_res[i,"x1"] else cdm_res[i,"x2"]
      sect = if (side == 1) cdm_res$rn[i] else cdm_res$cn[i]
      m1 = meth_mat_1[cdm_res$rn[i], cdm_res$cn[i]]; m2 = meth_mat_2[cdm_res$rn[i], cdm_res$cn[i]]
      v = if (side == 1) m1 else m2
      circos.rect(xx, y1, xx - abs(cdm_res[i,"value1"]), y1 + (y2-y1)*0.45,
        col = col_fun(v), border = col_fun(v), sector.index = sect, track.index = 1)
      circos.rect(xx, y1 + (y2-y1)*0.55, xx - abs(cdm_res[i,"value1"]), y2,
        col = col_fun(0.5 + (v - 0.5)), border = col_fun(0.5 + (v - 0.5)),
        sector.index = sect, track.index = 1)
    }
  }
}
circos.clear()
