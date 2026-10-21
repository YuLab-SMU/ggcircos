# AGENTS.md — ggcircos

仅记录**经过验证**、可复用的项目约定与易错点。上游文档：`notes/spec.md`（契约）、
`notes/issues.md`（缺陷账本）、`notes/positioning.md`（定位）。改语法先改 spec，再改代码。

## 构建与验证（本机固定流程）

- 走 `Makefile`：`make check1` = `rd`（document）+ `build1`（含 vignette）+ `rcmdcheck`。
- **日常改动别走 `make check1`，直接跑测试目录**（实测 2026-10-21：`load_all` +
  `test_dir` 全套 ≈ **15 秒**，而 `check1` 要打包 + 装包 + 查文档，分钟级）：

  ```bash
  NOT_CRAN=true Rscript -e 'suppressMessages(pkgload::load_all(".", quiet=TRUE)); \
    testthat::test_dir("tests/testthat", reporter="summary", stop_on_failure=FALSE)'
  ```

  `check1` 只在一个改动收口、或动了 `man/`、`DESCRIPTION`、vignette 时才跑。
- 本机无 `pdflatex`：`make check1` 固定出现 PDF manual 1 ERROR + 1 WARNING，属环境缺失。
  **判断包是否干净用 `--no-manual`**：
  `Rscript -e 'rcmdcheck::rcmdcheck("../ggcircos_0.1.0.tar.gz", args="--no-manual")'`，
  目标 0/0/0（vignette 已包含在 tarball 里，`re-building of vignette outputs` 必须 OK）。
- 快照协议：`NOT_CRAN=true` 才会跑 vdiffr；验收只认 `testthat::snapshot_accept("snapshots/")`，
  不要手工 `cp` 覆盖 `.svg`。快照变了先渲染目检（Edge 转 PNG），再 accept。
- 无头渲染 SVG→PNG 一律 Microsoft Edge（见全局 AGENTS.md）。

## ggplot2 4.0 扩展的四个坑（每个都咬过一次，均有回归测试）

1. **非美学列会被丢掉**。`data$kind` 这种自定列到不了 `draw_panel()`（ggplot2 只保留美学）。
   要带信息就用真美学名（`label`/`group`）或在 `mapping` 里显式 `aes()`。
2. **`pathGrob` 默认 `fill` 是白色**。画线必须 `gpar(fill = NA)`，否则闭合路径会填出白色
   圆盘盖掉下面所有图层（B8：加轴整图变空）。
3. **`pathGrob(id=)` 要数值**。字符 id 被静默忽略，整层连成一团。用
   `match(group, unique(group))`。
4. **`CoordRadial$transform()` 在 `rotate_angle = TRUE` 时会自己翻文字角度**。我们自己算
   旋转（`CoordCircos$screen_angle()`，spec §5.4），所以必须 `rotate_angle = FALSE`（B15）。

## 几何不变量（改任何一条，spec 与测试同时改）

- `track_offset` 默认 0.06，不变量 `1 + track_offset ≥ 节点环外沿 1.05`（B18：旧默认 0.02
  让第一条带盖住节点环，灰色叠灰色看不出来）。
- 轨道带起点是**累计**宽度：`r0_i = 1 + offset + Σ_{j<i}(width_j + offset)`（B18 的根因）。
- 布局是数据：角度只有一个来源（layout 表），屏幕角只有一个所有者（`CoordCircos$screen_angle()`），
  geom 不得自己 `atan2`。
- `clockwise` 是唯一方向旋钮；`direction` 一律报 classed error。

## 测试与文档纪律

- 每条 B 类修复：一个复现 + 一个回归测试，写进 `notes/issues.md`（含根因，不只写症状）。
- positioning.md 的三条 demo（D1 facet / D2 多轨+轴 / D3 layout-as-data）以**可执行断言**
  形式活在 `tests/testthat/test-demos.R`，README 与 vignette 的图必须与它们同源。
- vignette 的每个 chunk 必须真跑通（`rmarkdown::render` 验证），不许用 `eval=FALSE` 藏坏例子。
  写文档示例是**最高收益的测试**：B19（图对象丢边属性、tidygraph 端点不解析）就是照 vignette
  示例跑出来的。
