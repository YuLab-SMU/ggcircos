# 复现 circlize book 第 16 章（复杂弦图）

参照图：<https://jokergoo.github.io/circlize_book/book/a-complex-example-of-chord-diagram.html>
（chromatin state transition；数据 `chromatin_transition.RData`，15 个状态 × 两组样本的甲基化）。

## 文件

| 文件 | 说明 |
|---|---|
| `circlize-ch16-original.R` | 用 circlize 0.4.18 重画的原图代码（删去了它原文里的 bug） |
| `circlize-ch16-original.png` | 原图渲染结果 |
| `circlize-ch16-reproduction.R` | 同一张图用 **ggcircos** 写一遍 |
| `ggcircos-reproduction.png` | 复现结果 |
| `side-by-side.png` | 并排对照 |

## 复现到了什么

原图的每个构成，ggcircos 都能表达，且都是**普通 ggplot2 图层**：

| 原图元素 | ggcircos 写法 | 状态 |
|---|---|---|
| 30 个 sector（每个状态出现两次：R_* 源 / C_* 目标） | `nodes` + 等弧/sector 宽按权重 | ✅ |
| sector 弧宽 = 出权总和 | `weight = "bp"` | ✅（与原图语义一致，实测 sector 宽 = 行和） |
| 方向性弦，颜色按源状态 | `geom_chord(aes(fill = ...))` | ✅ |
| 弦的透明度按转移量两档 | `aes(alpha = ...)` + `scale_alpha_identity()` | ✅ |
| 状态索引 + 刻度轴（0KB..NKB） | `geom_track_axis()` | ✅ |
| 每链接两个甲基化细矩形（两端各一） | `geom_track(data = 每行一条链接)` | ✅（B37 的按行切分弧让它成立） |
| 颜色：红/蓝/黑三色状态组 | 字符串列 + `scale_fill_identity()` | ✅ |

## 复现暴露的问题

**1. `fill` 映射解析成 `NA` 时静默用默认色（真问题，会导致图"看起来对但全是错的"）**
第一次尝试里我把状态名错写成了索引，`fill` 得到 `NA`，ggplot2 静默换成默认调色板
（`#F8766D`），图照常画出。建议加检查：`geom_node_arc()` / `geom_chord()` 的 `fill`
映射到一列全是 `NA` 时应当报错或警告。（未修，记录在此。）

**2. 弦的曲率无法精确对齐 circlize 的 `h.ratio`**
circlize 的 `chordDiagram()` 默认 `h.ratio = 0.5`；本包 `curvature = "auto"` 的语义是
"两端相对时到达圆心"，`curvature = 0` 又变成接近直线。**没有参数能表达
`h.ratio = 0.5` 那种"弧线但不过圆心"的形态**——这是复现中最明显的差距。
建议：给 `geom_chord()` 增加 `h_ratio =` 参数，或让 `curvature` 接受 `"circlize"`。

**3. 单节点多行轨道：bin 等分，与链接槽位不对应 —— 已修（`slot =`）**
原图的甲基化矩形**贴在各自链接的 slot 上**。本包 `geom_track` 原本按数据顺序
**等分**节点弧（B37），画出的矩形与链接错位。已实现 **`geom_track(slot = edge_id)`**
（spec §4.10）：行弧段直接取自 layout 的链接槽位，矩形与弦的脚完全重合。
这是复现里唯一的硬缺口，修掉之后这张图才算真正复现。

**结论**：复现证明了包的可用性——circlize 书里最复杂的示例图，每个元素都能用
ggcircos 的普通图层表达，且数据对得上（甲基化矩形贴在各自链接的 slot 上）。

