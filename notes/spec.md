# ggcircos 实现规格（spec）

> 状态：**v0.2 设计规格**，2026-10-19（v0.1 → v0.2 收录了架构决策 A1–A6，见 [`issues.md`](issues.md)）。
> 上游文档：`notes/revival-proposal.md`（为什么这么做）、`notes/issues.md`（审核问题清单，问题编号 A1–A6 / B1–B12 / C1–C4）。
> 本文只回答"做成什么样"：每个函数签什么名、收什么、出什么、什么情况下报什么错、怎么算做完。
> 文中所有 `⚠ 决策` 都是**已定**的取舍，不是待讨论项；确实还没定的放在 §17「开放问题」。
>
> **v0.2 的新增规则**（都是架构级的，先于所有实现）：
> §1.4 平台基线（A1）、§5.1 坐标系重做（A1/A2）、§5.4 屏幕角唯一来源（A2）、
> §4.6 槽位几何契约（A3）、§8 layout 只读边界（A4）、§9.1 facet 语义（A5）、§6.4 外部 position（A6）。

---

## 1. 范围

### 1.1 目标

| # | 目标 | 可验收的形式 |
|---|---|---|
| G1 | 弦图 / 环形关系图是普通 ggplot2 图 | `ggcircos(x) + geom_chord(...)`，图层顺序无关 |
| G2 | 语法是 ggplot2 语法 | 数据第一个参数、`aes()` 映射、`+` 加层、`scale_*()` 生效、`facet_*()` 不报错 |
| G3 | 组合 | 多弦层、多轨道层、多 sector、`facet_wrap()` |
| G4 | 不依赖别的包的内部函数 | `Imports` 只有 `ggplot2`、`rlang`、`cli` |
| G5 | 不藏状态 | 图对象可保存/重载/序列化后继续加层；layout 只读 |
| G6 | 能表达 BLAST/BRIG 场景 | `sectors = ` + 块间缝 + 双向 ribbon |
| G7 | 好看 | 出图与 `notes/figures/target-design.png` 同构 |
| G8 | 跟上 ggplot2 4.0 的坐标体系 | 坐标层建在 `coord_radial()` 上（§1.4），不使用已 superseded 的 `coord_polar()` |

### 1.2 非目标（v0.1 明确不做）

- **不做** circlize 的逐像素兼容；不实现 `chordDiagram()` 的所有参数。
- **不做** 交互（`plotly`/`ggiraph`）；只保证 `ggplotly()` 不因本包报错。
- **多半径节点**（同一个节点挂多层弧）**已支持**：`nodes` 表里同一个 `id` 可以出现多次，
  每次一行、一层弧，半径取该行的 `r` 列（§4.9）。
- **不做** 一图多环（一棵树上挂多个 circos）；一张图一个 layout。
- **自环（`from == to`）**：`self_loops = FALSE`（默认）时丢弃并 `cli_inform()` 提示；
  `self_loops = TRUE` 时画成**向内折的小回环**（§4.7，v0.2）。
- **不做** 自动轴范围推断：轴必须显式给 `limits`（§6.5）。
- **不做** ggplot2 3.x 兼容（§1.4）。

### 1.3 版本与兼容策略

- 目标版本 `0.1.0`；`0.0.0.9000` 的 API 视为 pre-release，**允许破坏性变更**。
- 旧函数提供一层 shim（§11），shim 打 `lifecycle::deprecated()` 风格 warning（不引入 lifecycle 依赖，用 `cli::cli_warn(class = "ggcircos_warning_deprecated")`）。
- v0.2 起坐标层重做（§5.1），`clockwise` 的**实测方向会和 0.0.0.9000 相反**（旧版标着 clockwise 实际逆时针）。这是修正，写进 NEWS，不算破坏性变更。

### 1.4 平台基线：只支持 ggplot2 ≥ 4.0 ⚠ 决策（A1）

- `DESCRIPTION`：`Imports: ggplot2 (>= 4.0.0)`。
- **不再支持 3.5.***。理由：`coord_polar()` 在 4.0 已 superseded，两条坐标路径（polar / radial）的维护成本大于兼容收益；当前实现只在 4.0.3 上验证过，3.5 从未验证。
- CI 矩阵**只测 4.0.\***（spec §14.5 原要求的 3.5 + 4.0 双矩阵作废）。
- 坐标层一律用 `coord_radial()`（§5.1），不得使用 `coord_polar()`；`CoordPolar` 只允许出现在**读取**它的 `panel_params` 结构的兼容代码里，且必须有测试覆盖。

---

## 2. 记号

| 记号 | 含义 | 取值域 |
|---|---|---|
| `θ` | 角度，弧度 | `[0, 2π)` |
| `r` | 归一化半径，节点环固定在 `1` | `[0, R]`，`R` 由轨道决定 |
| `2π` 分数 | `gap` / `sector_gap` / `width` 这类"宽度"参数 | `[0, 1)` 表示占一整圈的比例 |
| `d` | 两角的最短有向差 | `(-π, π]` |
| `p` | 槽位填充率（slot fill） | `(0, 1]`，默认 `0.9`；语义见 §4.6 |
| `slot_w` | 一个槽位的宽度（弧度） | `p · node_w / 该侧边数`（按侧，见 §4.6） |
| `screen°` | 标签在画布上的旋转角（度） | 由 §5.4 唯一给出 |

`θ` 与屏幕上角度的换算**不在本包里手写**：屏幕角由坐标对象自己的 `panel_params` 导出（§5.4），`θ` 本身只用于数据列。

---

## 3. 包结构

```
ggcircos/
├── DESCRIPTION              # Imports: ggplot2, rlang, cli
├── NAMESPACE                # roxygen 生成
├── R/
│   ├── ggcircos.R           # 头函数 ggcircos()
│   ├── circos-layout.R      # circos_layout() + S3 方法 + 槽位分配
│   ├── coord-circos.R       # coord_circos() / coord_arc()
│   ├── geom-chord.R         # GeomChord + StatChord + geom_chord()
│   ├── geom-node.R          # geom_node_arc/text/point
│   ├── geom-sector.R        # geom_sector_label()
│   ├── geom-track.R         # geom_track() + GeomTrack + position_track()
│   ├── geom-track-axis.R    # geom_track_axis()
│   ├── theme.R              # theme_circos()
│   ├── layer-add.R          # 三个 ggplot_add 方法（只读解析 layout）
│   ├── deprecated.R         # geom_curve_link / geom_circos / geom_link_lab ...
│   ├── utils.R              # 角度工具、wrap、rescale、aes 捕获
│   └── reexports.R          # ggplot2 的 %>% / aes / ...
├── tests/testthat/
│   ├── test-layout.R
│   ├── test-coord.R
│   ├── test-chord.R
│   ├── test-track.R
│   ├── test-contract.R      # “图层不许改 plot 状态”
│   ├── test-deprecated.R
│   └── _snaps/              # vdiffr 快照
├── vignettes/
│   ├── correlation.Rmd
│   ├── genomes.Rmd
│   └── tracks.Rmd
└── notes/                   # 本目录，.Rbuildignore 排除
```

---

## 4. 数据模型

### 4.1 `circos_layout` 对象

```r
circos_layout(
  data,                     # matrix / data.frame / tbl_graph / igraph / circos_layout
  nodes     = NULL,         # 显式节点表
  sectors   = NULL,         # 列名或命名向量：节点 -> sector
  order     = NULL,         # NULL | "hclust" | 字符向量 | function(nodes)
  gap       = 0.01,         # 节点间缝，占整圈比例
  sector_gap= 0.05,         # sector 间额外缝，占整圈比例
  start     = 0,            # 第一个节点弧起点的 θ（弧度）
  slot_fill = 0.9,          # 槽位占节点弧的比例
  self_loops= FALSE,
  tracks    = NULL,         # 命名数值向量：轨道名 -> 宽度（相对 r=1）
  ...
)
```

返回一个 S3 对象，结构固定：

```r
structure(
  list(
    nodes   = <tibble>,   # n_nodes 行，列见 4.2
    links   = <tibble>,   # n_links 行，列见 4.2
    sectors = <tibble>,   # n_sectors 行
    tracks  = <tibble>,   # 已声明的轨道带
    params  = <list>      # gap / sector_gap / start / slot_fill / ...
  ),
  class = c("circos_layout", "list")
)
```

访问器（全部导出，**返回副本，纯读**，见 §8.4）：

| 函数 | 返回 |
|---|---|
| `circos_layout(x, ...)` | 构造 layout |
| `layout_nodes(layout)` | `nodes` tibble（副本） |
| `layout_links(layout)` | `links` tibble（副本） |
| `layout_sectors(layout)` | `sectors` tibble（副本） |
| `layout_tracks(layout)` | `tracks` tibble（副本） |
| `layout_params(layout)` | `params` list（副本） |
| `circos_layout_of(plot)` | 从图里取 layout（缺则 `NULL`） |

**⚠ 决策（A4）**：`$nodes` / `$links` / `$sectors` / `$tracks` / `$params` 仍然是公开字段（为了 `print` 与调试），但**文档声明为内部结构，不作为 API**；所有正式读写一律走访问器。访问器返回 `data.frame` 副本，`identical()` 检查保证改动不影响 layout 本身。

### 4.2 列契约

**`nodes`**

| 列 | 类型 | 说明 |
|---|---|---|
| `id` | chr | 节点标识，唯一（由 matrix 行列名、`nodes$id` 或 1..n 转字符串） |
| `label` | chr | 显示名，默认 = `id` |
| `sector` | chr | 所属 sector，默认 `"all"` |
| `order` | int | 环上顺序，1..n |
| `n_links` | int | 入射边数（不含自环） |
| `theta0` `theta1` | dbl | 节点弧的起止 θ |
| `theta` | dbl | 节点弧中点 θ（**= y 美学**） |
| `r` | dbl | 节点环半径（恒为 1，**= x 美学**） |

用户提供的节点表里其它列**全部保留**（可用于 `geom_node_*` 映射）。

**`links`**

| 列 | 类型 | 说明 |
|---|---|---|
| `from` `to` | chr | 端点 id |
| `theta` `theta_end` | dbl | 两端槽位中点 θ（`geom_chord(type="line")` 用） |
| `theta0` `theta1` | dbl | `from` 端槽位起止 |
| `theta_end0` `theta_end1` | dbl | `to` 端槽位起止 |
| `x` `y` `xend` `yend` | dbl | `r`/`theta`、`r`/`theta_end`（**由 `geom_chord()` 自己映射，不放 plot 级**，见 §7-2） |
| `edge_id` | int | 行号，用于 `group` |

用户数据里的其它列（`r` 相关、`identity`、`value`…）全部保留。
**⚠ 决策**：`from`/`to` 列若与用户列重名，一律以本包语义为准，原值存为 `user_from`/`user_to`。

**`sectors`**：`sector`(chr)、`theta0`、`theta1`、`theta`、`n_nodes`(int)、`r`(dbl, = 1)、`crosses_zero`(lgl，`theta0 > theta1` 即跨 0)。

**`tracks`**：`track`(chr)、`width`(dbl)、`r0`、`r1`（半径区间，`r0 >= 1`）。

### 4.3 构造算法（必须逐条实现并单测）

记 `N = nrow(nodes)`，`S = n_sectors`，`G = gap * 2π`，`SG = sector_gap * 2π`。

1. **归一化输入**：见 §4.4。得到 `nodes`（id 去重、顺序按 `order`）与 `links`（from/to 必须存在，否则报错）。
2. **排序**：`order = NULL` 保留输入顺序；`"hclust"` 用 `hclust(as.dist(1 - W))` 排序，`W` 是 from×to 的对称权重矩阵（无权重列时用边计数）；字符向量按给定顺序；函数按返回值。
3. **sector 分组**：sector 顺序 = 首次出现顺序；`sectors` 参数为命名向量时按该向量的名字排序。
   **扇区必须连续（R1）**：`order`（`"hclust"`、显式向量、函数）只在**扇区内部**生效，排完之后
   再按扇区首次出现顺序做一次稳定分组。否则 `sectors =` + `order = "hclust"` 会把同扇区节点打散，
   切换次数超过预算的 `S`，弧越界重叠（实测溢出 0.57–1.51 弧度）。
4. **角度总预算**：`span = 2π - N*G - S*SG`（`S ≤ 1` 时没有 sector 缝）；要求 `span > 0`，否则
   `ggcircos_error_layout: "gap too large: N*gap + S*sector_gap >= 1 turn"`。
   **⚠ 决策（B4）**：`gap` 的默认值是自适应的，不是字面常量。默认值按公式
   `gap = min(gap_user, 0.2 / N)` 计算：节点少时用 `gap_user = 0.01`（行为不变），
   节点多时自动收窄，保证 `N*G ≤ 0.2`（留 20% 圈给缝）。`sector_gap` 同理
   收窄到 `min(sector_gap_user, 0.2 / max(S, 1))`，保证 `S*SG ≤ 0.2`。
   **⚠ 决策（R13）**：`layout_params()` **同时**给用户值和生效值两列——
   `gap` / `sector_gap` 是调用者传入的，`gap_eff` / `sector_gap_eff` 是收窄后真正用掉的。
   覆盖成一个字段会让人无法回答"我传的 0.5 到底变成了多少"；双列两者都能查到。
5. **节点弧**：每个节点宽 `w = span / N`；**带 `weight` 时改为 `w_i = span · Σ|w|_i / ΣΣ|w|`**
   （节点弧按它承载的权重分配，和 circlize 的 `chordDiagram()` 一致；见 §4.6-2）。第 i 个节点
   `theta0 = start + 已用角度`，`theta1 = theta0 + w`，节点之间加 `G`；每个 sector 块的**两端**
   再加 `SG`——含跨 `start` 的那一处（B33：只留 `S-1` 处会让两个 sector 读作一整圈被咬掉一口，
   与 circlize 的 `gap.after` 每端都留宽缝不一致）。
   最终把 `theta*` 对 `2π` 取模（`start` 可为任意实数）。
   **⚠ 决策（B3）**：`gap < 0` / `sector_gap < 0` 直接 `ggcircos_error_layout`；
   显式 `gap` / `sector_gap` 先按 B4 收窄，收窄后 `span ≤ 0` 仍报 `ggcircos_error_layout`。
6. **槽位分配**：对每个节点，**每一侧**（`from` 端 / `to` 端）的入射边按 `(对方节点 order, edge_id)` 排序，各自等分节点弧。
   该侧槽位宽 `slot_w = p * node_w / 该侧边数`，**槽位之间留 `(1 - p) * node_w / 该侧边数` 的空隙**，
   ribbon 的脚只覆盖槽位本身（完整契约见 §4.6）。`from` 与 `to` 两端**独立**分配（各自用自己的边数做分母）。
7. **自环**：`from == to` 的边在 `self_loops = FALSE` 时丢弃，并 `cli_inform()` 一次（消息里给条数）；
   `self_loops = TRUE` 时画成向内折的小回环，几何见 §4.7。
8. **轨道带**：`tracks = c(values = .2, heat = .1)` →
   `r0 = 1 + track_offset + 累计(width) + 累计(track_offset)`，`r1 = r0 + width`；`track_offset` 默认 **0.06**，可在 `ggcircos()` 里覆盖。**不变量**：第一个带必须落在节点环之外——`geom_node_arc()` 默认占 `r ∈ [1, 1.04]`（`offset = 0`，环脚与飘带齐平，B34），故 `track_offset ≥ 0.04`（旧默认 0.02 让第一轨从 1.02 起，直接盖住节点环；灰色轨道叠灰色环，肉眼看不出来）。
   **⚠ 决策（B2）**：0 条链接（空 `data.frame`）是**合法输入**——节点表仍按 §4.4 建立，
   `links` 为 0 行但列齐全（§4.2 的所有列都在，值为空向量），图可画节点环。
9. **不变量自检**（失败即包 bug，`ggcircos_error_internal` 而不是裸 `stopifnot`）：
   - 所有 `theta` ∈ `[0, 2π)`；
   - 节点弧两两不相交；
   - 每个槽位 ⊂ 所属节点弧；
   - 相邻槽位之间的空隙 = `(1 - p) · slot_w`（容差 `1e-9`，§4.6）；
   - `sum(节点弧) + gaps == 2π`（容差 `1e-9`）。
   **⚠ 决策（B5）**：所有会流入不变量自检的输入（`sector` 含 `NA`、`theta` 非有限值、
   `slot_fill ∉ (0, 1]`）必须在构造阶段就 `ggcircos_error_<kind>`，不允许走到自检才炸。

### 4.6 槽位几何契约（A3）⚠ 决策

**这一节定义 `slot_fill` 到底管什么，是 §4.2 列契约、§6.1 ribbon 几何、§6.4 轨道宽度的共同依据。**

1. **节点弧**是 `[theta0, theta1]`（宽 `node_w`），由 §4.3-5 给出。
2. **槽位按侧独立分配**（B16）：节点 `i` 的 `from` 侧有 `d_out` 条边、`to` 侧有 `d_in` 条边，
   **两侧各自**把整条节点弧等分——`from` 侧每份 `s_out = node_w / d_out`，
   `to` 侧每份 `s_in = node_w / d_in`。**不是**按总度数 `n_links = d_out + d_in` 分
   （那会浪费弧：一个 3 出 1 入的节点若按 4 分，出边只用 3/4 的弧）。
   槽位起止是 `theta0 + gap/2 + (k-1)·(slot_k + gap)` 与 `+ slot_k`，
   其中 `gap = (1 - p) · node_w / d_side`。**等权时 `slot_k = p · s_side`、`gap = (1-p)·s_side`，
   与原公式逐位相同**。

   **加权（`weight =`）时**：`slot_k = p · node_w · |w_k| / Σ_side |w|`，`gap` 不变。

   **绝对缝 `slot_gap`（R21）**：给了数值时，缝不再是 `(1-p)` 的比例，而是**固定弧度**
   `min(slot_gap, node_w / (4·d_side))`——上限是"节点弧的四分之一留给缝"，
   保证链接很多的节点仍有可见的 ribbon。槽位吃掉剩余弧：`slot_k = (node_w - d_side·gap) · share_k`。
   这和 circlize 的 `diffHeight`（固定 2mm）是同一个思路：比例式的缝在窄弧上会消失。
   即每个槽位按自己那条边的权重分配该侧可用弧，空出来的 `(1-p)` 仍是 `d_side` 个等宽缝。
   负权重按绝对值计（与 circlize 相同）；全零权重报 `ggcircos_error_data`。
   `n_links` 列仍是**总度数**（信息列，供用户映射），只是不作为槽位分母。
3. **ribbon 的脚 = 槽位本身**。`GeomChord` 画 ribbon 时，环脚的角范围是
   `[theta0, theta1]`（`from` 端）与 `[theta_end0, theta_end1]`（`to` 端），
   **不得画到槽位之外**。`p = 1` 时槽位铺满节点弧，`p < 1` 时槽位之间出现可见空隙。
4. **`type = "line"` 的端点**是槽位中点 `theta` / `theta_end`，与 `p` 无关（线没有宽度）。
5. **扇区的起止**（`sectors$theta0/theta1`）取该 sector 所有节点弧的**环形连续段**，
   而不是 `min()/max()`：从 sector 内第一个节点弧起，沿 θ 增大方向逐节点并段，
   遇到 `gap` 或 `sector_gap` 就停；若段跨过 `θ = 0`，`theta0 > theta1`。`theta`（弧中点）由连续段计算。
   （B1 的修法就是这一条。）
6. **不变量**（写进测试 `test-layout.R` 的 `"the slot geometry contract holds for every p"`）：
   - 对每条链接的**每一侧**：`theta1 - theta0 == p · node_w / 该侧边数`；
   - 同一节点同侧相邻槽位间隔 `(1 - p) · node_w / 该侧边数`；
   - 所有槽位 ⊂ 所属节点弧；
   - `layout_sectors()` 的 `theta0/theta1` 与节点表的弧一致（跨 0 情况 `theta0 > theta1`）。

### 4.4 输入适配

| 输入 | 节点 | 边/权重 |
|---|---|---|
`matrix`（方阵、`rownames == colnames`） | 行列名 | 上三角（含对角？**不含**）非 `NA` 元素 |
`matrix`（非方阵 / 行列名不同） | 行名 ∪ 列名 | 全部非 `NA` 元素（二部图：两列分开也行）|
`data.frame` | `from`/`to` 两列的唯一值 | 每行一条边 + 其余列 |
`tbl_graph`（tidygraph） | `nodes` | `edges` |
`igraph` | `V()$name` 或 `1..n` | `E()` + `E()$weight` |
`circos_layout` | 原样返回（幂等） | — |

**⚠ 决策（B38）**：外来列名用 `columns =` 映射，不给每种格式写一个方法。
`columns = c(from = "qaccver", to = "saccver")`：名字是本包要的列，值是人家的列。
映射是副本，调用方的 data.frame 不被改动；**没映射的列随 links 表一起走**，
图层还能用（`weight = pident`、`aes(fill = pident)` 都行）。

这么做而不是写 `circos_layout.blast()`：BLAST 表输出、GFF3、BED、InterProScan
各有一套拼写，而前两者生态里已有 reader（`ggchord::read_blast()` / `read_gff3()`），
用户把读出来的表丢进来即可。一个参数覆盖所有格式，也覆盖了还没人写方法的格式。

**⚠ 决策**：不引入 `corrr`。`matrix` 走 base R，不再经 `as_cordf()/stretch()`。

**⚠ 决策**：`tbl_graph` / `igraph` 走 `Suggests`；缺包时报
`ggcircos_error_dependency: "Input is a <class>; install the '<pkg>' package or supply a data.frame."`

### 4.7 自环：向内折的小回环（v0.2）

`from == to` 的边没有两个不同的节点可连，普通 chord 会退化成一条零长度线。
circlize 的做法是**向内折**：从节点弧上的一个 slot 出发，朝圆心走一小段，再折回来。
形状像一个向内凹陷的舌状环，不占用别的节点的角，也不穿过圆心。

契约：

1. **占一个 slot，成对**。自环的 `from` 端与 `to` 端是同一个节点，`allocate_slots()` 会
   在它的 `from` 侧和 `to` 侧各给一个 slot（各自按该侧边数等分，现有逻辑不变）。
   两者的中点就是回环的两个"脚"；通常相邻，但不强求。
2. **折到带宽内的深度**。回环的最低点半径 `r_loop = r_node * (1 - h)`，`h` 是这个 slot
   的角宽相对于**整个节点弧**的比例，再乘一个系数 `loop_depth`（默认 `0.5`）：
   `h = loop_depth * slot_span / node_width`。**目的**：节点弧越窄，slot 越窄，回环越浅，
   不会因为一个窄节点上的自环而捅到圆心把别的弦挡死。
   `loop_depth` 是 `ggcircos()` 的参数，取值 `(0, 1]`，超出报 `ggcircos_error_layout`。
3. **形状是二次 Bezier 的向内折**，不是另一个圆弧。两端点在 `r_node` 上，
   控制点在内侧半径 `r_loop` 处、角度取两端的**中点**，于是曲线向圆心凹。
   用同一个 `chord_bezier(..., w = -1)`（`w < 0` 让 `h2` 反号，曲线朝内），
   这样它和普通 chord 是同一套几何，只是方向相反。
4. **列契约不变**：自环的行同样带 `theta/theta_end/theta0/theta1/…` 与 `x/y/xend/yend`，
   `from == to == 该节点 id`。新增一列 `self_loop`（logical），供 `geom_chord()` 分支。
5. **`n_links` 不含自环**（§4.2 已如此），因为自环不是"入射边"；但 `allocate_slots()` 的
   两侧分母**要算自环**，否则它的 slot 宽度会被算成整条弧，盖住所有正经链接。
   这是唯一一处需要同时"计入分母、不计入 n_links"的地方，加测试钉住。
6. **剖面（ribbon）与线（line）都要能画**：`geom_chord(type = "ribbon")` 画内折的带，
   `type = "line"` 画内折的曲线。二者曲线中心线相同，只是 ribbon 在两侧各偏移半个 slot 宽。

**为什么向内折而不是绕满整弧**（spec §17-1 的决策，2026-10-22 定）：
整段弧会把一个自环画成和自己重合的整圈，和节点本身混在一起，读不出来是"链接"；
向内折保持"一条边"的读法，且与 circlize 的 `circos.link(sector.index, sector.index)`
视觉一致，用户在从 circlize 迁移时不会感到陌生。

**矩阵输入不含自环**：`circos_layout.matrix()` 取 `upper.tri`，对角线永远在内三角之外，
所以 `cor(x)` 这类矩阵**不会**产生自环行——即使 `self_loops = TRUE`。要画自环必须用
`from`/`to` 表（或 `nodes` + 显式 links）。这是有意的：相关矩阵的对角线是 1，不是一条链接。

**回归测试**：
- 自环行存在且 `self_loop == TRUE`，`from == to`；
- 回环最低点半径 `< r_node` 且 `> 0`（没穿圆心，也确实折了）；
- 节点弧窄时回环更浅（`h` 随 `slot_span / node_width` 单调）；
- `self_loops = FALSE` 仍然丢弃并 `cli_inform()`；
- `n_links` 不含自环，但两邻边的 slot 宽度按"含自环"的边数等分。

---

### 4.9 多半径节点：同一个节点挂多层弧

一个节点 id 可以占据**几个半径**：节点表里同一个 `id` 出现多次，每一行是一**层弧**，
半径取该行的 `r` 列。这让"同一个实体在几个半径上各有一组连接"成为可能——比较两个
网络的同一批蛋白、同一份基因在不同组装里的位置。

**规则**：

1. **弧的身份是 `(id, r)`**。同一个 `(id, r)` 出现两次 → `ggcircos_error_node`。
2. **角度几何按 id 算**：同一个 id 的所有层**共用** `theta0`/`theta1`/`theta`/`width`/
   `order`/`sector`，即它们**径向对齐**（同一个节点在环上的位置，向上叠了几层）。
   角度预算按 **id 个数**算，不是按弧行数——否则叠层会把环挤窄。
3. **槽位按弧算**：每一层 `(id, r)` **独立**地划分 id 的那段共享弧。
   所以不同层可以用到相同的角度，但它们在不同半径上，视觉上分得开。
4. **链接要指明层**：链接表可带 `from_r` / `to_r` 列。端点的 id 只有一层时半径是隐含的；
   有多层而没给半径 → `ggcircos_error_node`（消息列出各层的半径）——不猜。
5. **`n_links` 按弧算**：它是"落在这条弧上的链接数"。
6. **画布要装得下**：径向上限取**最外层弧**的半径，不是布局的基准半径。
7. **sector 仍按 id 算**：sector 是 id 的连续段，层不参与。
8. **不变量按 id 检**：同一 id 的多层弧角度相同是**设计**，不是重叠违规；
   "弧两两不相交"因此改成"**不同 id** 的弧不相交"。

**为什么径向对齐而不是各占一段角**：径向对齐时"同一个节点"在环上的位置只有一个，
叠层只是把它向上复制——这正是"多层弧"的字面意思，也和 `geom_node_arc()` 现有的
`.data$r` 画法一致（绘制侧早就按每行的 `r` 画弧，只是 layout 侧把 `r` 钉死成 1）。
各占一段角则是"同一个实体出现在环的两个位置"，那是**另一个**特性（同一 id 的两个
互不相邻的弧），与 B-plan 的 `(组, id)` 身份类似，本版不做。

**与 B-plan 的关系**：B-plan 让节点身份是 `(组, id)`，多半径让它是 `(组, id, r)`。
两者可叠加：一个 panel 里，同一个 id 在两个半径上各有一层。

**回归测试**：
- 单半径布局的节点表与改动前**逐位相同**（这是兼容性的底线）；
- 一个 id 两层：两层的 `theta0/theta1` 相同、`r` 不同，`n_links` 各算各的；
- 链接的 `from_r`/`to_r` 指到对应层；缺半径且有歧义时报错；
- 角度预算按 id 计：叠层不改变 id 之间的间隙；
- 画布上限随最外层半径增长。

---

### 4.10 轨道行对齐链接槽位（复现 circlize ch.16 暴露的缺口）

`geom_track` 把一个节点的多行**等分**该节点的弧（B37），行序就是数据顺序。
这画不出"每行对应一条链接"的轨道——circlize 的甲基化矩形贴在**每条链接自己的
slot** 上，宽度正比于该链接的权重。

**契约**：`geom_track()` 增加 `slot =` 参数，命名轨道数据里的一列；该列的值是
**这条链接在 layout 的链接表里的行号**（`edge_id`）。给定后：

1. 行的弧段不再是等分，而是**该行对应链接的槽位**（`theta0/theta1` 直接取自 layout）。
   **槽位分两端**：行的节点 id 等于链接的 `to` 且不等于 `from` 时，取 `theta_end0/1`
   与 `xend`；否则取 `theta0/1` 与 `x`。一条链接在两个节点上各留一段弧，混用会把
   目标侧的矩形画到环的另一边（B47）；
2. `x`（值）仍由 `PositionTrack` 映射进 band，行为不变；
3. 槽位与节点弧的对应由 layout 保证——轨道行"贴"在链接脚上，与弦的脚完全重合；
4. `slot` 列引用不存在的 `edge_id` → `ggcircos_error_data`。
5. **用户自带的 `edge_id` 列不被占用**（B48）：`allocate_slots()` 把链接按 layout
   顺序编号写进 `edge_id`，这是本包的行键。调用方若已有同名列，重命名为 `user_edge_id`
   保留，不覆盖——否则调用方的编号静默消失，`slot =` 会对上错误的链接。
6. **`by =` 下按 `(group, user_edge_id)` 匹配**（B49）：每个 panel 各自从 1 编号，
   堆叠后的 `edge_id` 必然重复（1, 1, ...），只能拿调用方的编号定位。
   调用方没给 `edge_id` 时退回 `(group, edge_id)`。

这是"轨道与链接对齐"的最小机制：不引入新几何，只是让 `track_angles` 接受
**外部给定的弧段**。

**回归测试**：轨道行的 `ymin/ymax` 与其链接的 `theta0/theta1` 逐位相等；
一行一条链接时矩形完全覆盖该链接的 slot；引用非法行号报错；
`by =` 两个 panel 各一条链接时，行落在**各自 panel** 的槽位上（B49）；
用户 `edge_id` 保留为 `user_edge_id`（B48）。

---

### 4.5 错误

统一 `cli::cli_abort(..., class = "ggcircos_error_<kind>")`，`<kind>` ∈
`layout` / `node` / `data` / `band` / `dependency` / `internal`。

消息模板（必须带"怎么改"）：

| 情形 | 类 | 文案骨架 |
|---|---|---|
| 矩阵行列名不一致 | `_data` | `"Row and column names differ: …; ggcircos needs a square matrix with identical names, or supply a data.frame with from/to."` |
| 边引用了不存在的节点 | `_node` | `"Link refers to unknown node 'x'. Known nodes: a, b, … Use nodes = to supply the full node table."` |
| `gap` 过大 | `_layout` | 见 4.3-4 |
| 轨道名未声明 | `_band` | `"Unknown track 'v'. Declared tracks: values, heat. Declare it in ggcircos(tracks = ...) or pass band = c(r0, r1)."` |
| 缺可选依赖 | `_dependency` | 见 4.4 |

---

## 5. 坐标系（A1 / A2 重做）

### 5.1 `coord_circos()` ⚠ 决策（A1/A2）

```r
coord_circos(theta = "y",
             clockwise = TRUE,
             start = -pi / 2,
             clip = "off",
             r.axis = FALSE)
```

**实现基础改为 `coord_radial()`**：`CoordCircos <- ggproto("CoordCircos", CoordRadial, ...)`，
`coord_circos()` 构造时把参数翻译成 `CoordRadial` 的字段：

| `coord_circos()` | 翻译为 `CoordRadial` | 说明 |
|---|---|---|
| `theta = "y"` | `theta = "y"`、`r = "x"` | `y` 是弧度、`x` 是半径 |
| `clockwise = TRUE` | `reverse = "none"`、`arc = c(start, start + 2π)` 直接给 | θ 增大 = 顺时针 |
| `clockwise = FALSE` | `reverse = "theta"` | θ 增大 = 逆时针 |
| `start` | `arc[1]` | 见下 |
| `clip` | `clip` | |
| `r.axis = FALSE` | `r.axis.inside = TRUE` 且 `render_axis_*` 返回空轴 | 关掉半径轴 |

**θ = 0 的位置固定为 12 点钟**，`start` 是"第一个节点在环上的 θ"，**不是屏幕偏移**。
`CoordRadial` 的 `arc` 取 `c(-π/2, 3π/2)`，使得 `θ = 0`（数据里第一个节点）落在 12 点；
角度映射由 `CoordRadial$transform()` 内部完成，本包不手写。

**`clockwise` 的语义**（唯一合法用法）：

- `clockwise = TRUE`：θ 增大在屏幕上是**顺时针**。
- `clockwise = FALSE`：θ 增大在屏幕上是**逆时针**。
- **不接受** `direction`（`CoordPolar` 的旧参数）；传进来就 `ggcircos_error_layout`。

**兼容说明**：0.0.0.9000 里"`clockwise = TRUE` 实际逆时针"是 bug，v0.2 修正。
用户从旧版迁移时，视觉上会镜像一次——写进 NEWS。

**用户覆盖坐标系时**：`+ coord_circos(...)` 替换掉头函数装的坐标，layout 的 θ 不变（θ 是数据列，
不依赖坐标），但 §5.4 的屏幕角要重新从新的 `panel_params` 取。`+ coord_circos(clockwise = FALSE)`
是合法用法（镜像），必须保留测试（spec §14.3）。

**⚠ 决策（B15）——`rotate_angle` 必须是 `FALSE`**：`CoordRadial$transform()` 在
`rotate_angle = TRUE` 时会对带 `angle` 列的数据额外调用 `flip_data_text_angle()`
（ggplot2 4.0 的行为）。本包的标签角度由 §5.4 的 `screen_angle()` 一次算好，
再被 ggplot2 翻一次就变成**镜像**（`rotate = TRUE` 出水平标签、`FALSE` 出旋转标签）。
坐标层与 geom 层只能有一个角度主人：`rotate_angle = FALSE`。
回归测试：`geom_node_text(rotate = TRUE/FALSE)` 的 grob `rot` 分别"不全为 0" / "全为 0"。

### 5.2 `coord_arc()`

线性布局：节点排在一条竖线上，弦向一侧鼓。

```r
coord_arc(xlim = c(-1, 1), ylim = NULL, clip = "off")
```

- 继承 `CoordCartesian`（不变）。
- `x` 仍是半径（节点在 `x = 0`），`y` 仍是 θ；弦的鼓包方向 = `x` 负方向。
- `xlim` 必须给足，否则弦被裁（旧版线性布局的 bug）；默认留出 `[-1, 0]` 的鼓包空间。
- **⚠ 决策**：`layout = "arc"` 时 `r` 恒为 `0`，节点弧的 `theta0/1` 退化为 `y ± w/2`。
- **⚠ 线性布局里 `y` 是普通坐标，不是角度（R16）**：弦的 y 跨度必须是**不绕回**的差值
  （`b0 - a1`、`t1 - t0`），**不能**用"最短弧" `ang_diff()`。用最短弧时，跨过 θ=0 的弦
  端点会落到 `[0, 2π]` 之外（实测 -2.65 / 8.87），整条弧被裁掉，画面上只剩半截。
  鼓包深度同时限在 `-4/3`（半圆的深度），否则超过半圈的长弦会冲出 x 范围。
  回归测试：`test-tracks-and-contract.R` 的 `"the linear layout keeps every chord inside the panel (R16)"`。
- **纵横比锁 1:1（R17）**：`CoordArc` 继承 `CoordFixed`、`ratio = 1`。半径单位必须等于角度单位，
  否则 1 个半径单位 ≈ 4.8 个弧度的像素（y 轴跨整圈、x 轴只有 1.3），每条弦都被拉成细长 U 形。
  代价：角度轴占整圈，所以线性布局要**偏高的画布**（约 1:2 或更窄），方形画布下 panel 会是一条窄柱。
- **鼓包深度（线性布局专用）**：`bulge_cart()` 让跨整圈的弦鼓 0.8 个半径单位、短弦按比例；
  这是设计常量（圆里那个深度由几何决定，直线里没有唯一答案）。实测：跨度 π 的弦鼓 0.4，
  在 1:1 单位下读起来就是正常的弧。

### 5.3 `is_linear()`

`coord_arc()` 返回 `TRUE`，`coord_circos()` 返回 `FALSE`。所有几何在 `draw_panel` 里都必须判断，两条路径都要有测试（§14）。

### 5.4 屏幕角契约（A2）⚠ 决策

**屏幕角 = 点从圆心指向外的方向**（度，12 点为 0、顺时针为正）。**不是切线**——
标签（和轴刻度）沿径向读，文字基线再相对它转 90°（见下）。

**屏幕角 = 标签在画布上的旋转角**（度，转换前）。它是 `geom_node_text()`、`geom_sector_label()`
与 `GeomTrackAxis` 的共同输入，**只有一个来源**。

**唯一来源：坐标对象导出的 `screen_angle()` 方法。**

```r
CoordCircos$screen_angle <- function(self, panel_params, theta) {
    # θ 是数据弧度；返回画布角度（度），12 点 = 0，顺时针为正
    arc    <- panel_params$arc          # CoordRadial 给的 c(start, end)
    trange <- panel_params$theta.range
    u <- (theta - trange[1]) / diff(trange)     # [0, 1]
    if (identical(self$reverse, "theta")) u <- 1 - u
    rad <- arc[1] + u * diff(arc)
    # CoordRadial 内部 x = r*sin(θ)+.5, y = r*cos(θ)+.5 → 屏幕极性角
    (rad * 180 / pi) %% 360
}
```

要点：

1. **禁止 `atan2` 反推**。旧实现用 `atan2(y - 0.5, x - 0.5)` 从变换后坐标反推屏幕角
   （`circos_screen_angle()`）。这在 `y` 的 limits 被覆盖、坐标带 `expand`、`inner_radius ≠ 0`
   或换用其他坐标时都会失效——旧版"一半标签歪掉"就是这个根因，当前实现只是换了个地方同样反推。
2. **必须与 `transform()` 共用参数**：`panel_params$arc`、`panel_params$theta.range`、
   `self$reverse` 是同一套来源，屏幕角和点位置不可能不一致。
3. **标签逻辑（左半圈翻转）**：
   - `rotate = FALSE`：水平文字，`sin(屏幕角) < 0`（左半圈）时 `hjust = 1`，否则 `hjust = 0`。
     屏幕角以 12 点为 0，所以"左"对应 `sin`；用 `cos` 翻的是**下半圈**（R6）。
   - `rotate = TRUE`：旋转角 = 屏幕角；左半圈 + 180°，`hjust = 1`。
4. **`coord_arc()` 的 `screen_angle()` 恒返回 0**（线性布局不旋转）。
   ⚠ 实现里不要为 `reverse == "theta"` 再翻一次 `u`：`arc` 本身已经是递减的
   `c(start + 2π, start)`，再翻一次正好抵消，逆时针圈会报出和顺时针一样的角度（R2）。
   **文字旋转角 = 径向角 − 90°**，再折叠到 (−90, 90] 以免倒立；节点标签与 `geom_track_axis()`
   共用这条规则（轴刻度原本直接把径向角当 `rot`，文字于是竖成一列）。
5. **测试**：对 `clockwise = TRUE/FALSE` × `start ∈ {0, -π/2, π}` 的每一种组合，
   屏幕角与"把 (r, θ) 和 (r+ε, θ) 两点 `transform()` 后的连线方向"一致（容差 `1e-6`）。
6. `circos_screen_angle()` 这个内部函数**删除**，不要留兼容包装。

---

## 6. 图层

### 6.1 `geom_chord()`

```r
geom_chord(
  mapping  = NULL,
  data     = NULL,          # NULL = 用图的默认 data（links）
  from     = NULL,          # data 非 NULL 时必填：未加引号的列名
  to       = NULL,
  type     = c("ribbon", "line"),
  curvature= "auto",        # "auto" | 数值（>0 向内鼓）
  ncp      = 40,            # 每条曲线的采样点数
  linewidth= NULL,
  alpha    = 0.5,           # ribbon 默认 0.5；line 默认 0.9
  arrow    = NULL,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
```

**美学**

| 美学 | 必需 | 默认 | 说明 |
|---|---|---|---|
| `x` `y` `xend` `yend` | ✓（自动） | 来自 layout | 端点（r, θ） |
| `fill` | ribbon | `"grey50"` | 带状填充 |
| `colour` | line | `"black"` | 线色；ribbon 的描边默认 `NA` |
| `linewidth` | — | `0.3`（line）/ `0.1`（ribbon 描边） | 用 `linewidth`，**不用 `size`** |
| `linetype` | — | `1` | |
| `alpha` | — | 见上 | |
| `theta0` `theta1` `theta_end0` `theta_end1` | ribbon | 由 stat 提供 | **槽位起止**（§4.6），不算用户美学，从数据列读 |

**几何定义**（`type = "ribbon"`，非线性的情况）

**⚠ 决策（B34）：这一段是 `circlize::circos.link()` 的移植，不再自研。**
先后试过四种自研几何（角度偏移脊线、平面偏移脊线、同侧两条边、残差折叠），
每种都"看着差不多"但都不是它的形状；失败的判据是**多边形面积**——
错误配对算出 5.98（≈ 整个圆盘 `2π`），正确是 0.24，差 25 倍。
自研期间脚的位置、端点半径全对，只有"中间围出多少面积"错，肉眼很难发现。

两端**槽位**为 `[a0, a1]`、`[b0, b1]`（§4.6）。两条边是**交叉配对**：

```
d1 = Q(a0, b1)      # 低角端 -> 对方高角端
d2 = Q(a1, b0)      # 高角端 -> 对方低角端
```

`Q(p, q)` 是带权二次贝塞尔 `circlize:::getQuadraticPoints()`：

```
t1, t2  = deg(p) %% 360, deg(q) %% 360          # |t1-t2| = 180 时给 t1 加 0.001
rmin    = min(rou1, rou2)
beta    = (t1 - t2) %% 360；beta > 180 时取 360 - beta
h_auto  = rmin * (1 - h.ratio * cos(beta/2))    # h.ratio 默认 0.5
h2      = h_auto - rmin * (1 - cos(beta/2))
dis     = ½·|(x1,y1) - (x2,y2)|
控制点  P0 = (-dis, 0)、P1 = (0, (1+w)/w · h2)、P2 = (dis, 0)
曲线    B(t) = Σ 二次贝塞尔 / 带权分母，w 默认 1
采样    按弧长 / (2π/500) 取点，最少 3 个且为奇数
旋转    alpha = -90 + (360 - (t1+t2)/2)，再平移到两端中点
```

**闭合弧取短向**：`arc` 只覆盖脚自身宽度（几度）。`circlize:::arc.points()`
用的是 `(t2-t1) %% 360`（可能绕一整圈），直接照搬会把脚变成 350° 的长弧、
多边形围住整个圆盘。

多边形顶点序列 = `d1` + `弧(b1→b0, r=1)` + `rev(d2)` + `弧(a1→a0, r=1)`。

性质（写进测试）：

- 多边形面积是"一条带"，远小于 `0.5π`（错误配对会到 `2π` 量级）；
- 顶点全部在 `r ∈ [0, 1]`，脚上的点恰在 `r = 1`；
- `chord_bezier()` 与 `circlize:::getQuadraticPoints()` 逐点一致（实测 1.4e-15）。

**⚠ 决策（A3）**：环脚的角跨度**严格等于槽位宽度** `p · node_w / 该侧边数`（按侧分配，见 §4.6），不画到节点弧边界。
`p = 1` 时槽位铺满节点弧（无可见空隙）；`p < 1` 时槽位之间留 `(1-p)` 的空隙，
ribbon 的脚之间也应有对应空隙。**回归测试**：对任一 `p ∈ {0.5, 0.9, 1}`，
`ggplot_build()` 后 `for each link: theta1 - theta0` 恰为 `p · node_w / 该侧边数`（容差 `1e-9`）。

性质（必须写进测试）：

- `d = 0` 时曲线退化为沿环的弧（`k = 1`）；
- `|d| = π` 时控制点落在圆心（`k = 0`，中点 `r = 0.25`）——**不是**负半径。
  ⚠ 曾写成 `k = -1/3`「过圆心」，但控制点在两端的**射线**上，负半径等于把控制点放到
  圆心对面，控制多边形变成 `1, -1/3, +1/3, -1`（非单调），曲线在中间折回去（R19）；
  实测放开后在 55 条链路上采到 `r = -1e-3`，曲线穿到洞外（B34 复核时再次确认）。
  `k` 一律钳到 `≥ 0`：链路最深处到圆心附近，且永不回头。
- 端点切线方向是**径向**（`P1 - P0` 与半径同向）——这是"脚"看起来贴着环的原因；
- 曲线始终在 `r ∈ [0, 1]`。

`type = "line"`：同一条贝塞尔，但只画 `theta → theta_end` 的**中心线**（不画弧脚），用 `polylineGrob`。

**曲率参数**：`curvature = "auto"` → 用上面公式；数值 `c` → `k = 1 - c`（同样钳到 `≥ 0`，
所以 `c = 1` 是"控制点在圆心"而不是穿过去）。

**性能**：一层只产出 **1 个 grob**（`pathGrob`/`polylineGrob` 带 `id`），不是每条边一个 grob。
旧实现是 `lapply(seq_len(nrow), curveGrob)`，1k 条边就 1k 个 grob。

**`data` 非 NULL 时**：`from`/`to` 在该层 `ggplot_add` 时按 layout 查槽位，补上 `theta*` 列。
**约束**：第二层只能**取 layout 已有连线的子集**（同一对节点换配色/线型是常见用法）；
某条边不在 layout 里时报 `ggcircos_error_data`：
`"link 'a'-'b' is not in the layout; pass all links to ggcircos() — a second chord layer can only subset them."`
（允许多层共用节点弧的槽位，是"槽位属于 layout、不属于图层"的直接结果。）

### 6.2 节点层

```r
geom_node_arc(mapping = NULL, data = NULL, width = 0.04, offset = 0,
              linewidth = 0.2, ...)
geom_node_text(mapping = NULL, data = NULL, rotate = FALSE, offset = 0.10,
               nudge_x = 0, ...)
geom_node_point(mapping = NULL, data = NULL, offset = 0.02, ...)
```

- `data = NULL` 时用 layout 的 `nodes`（在 `ggplot_add` 里注入，只读）。
- 默认 mapping：`geom_node_arc` → `aes(x = r + offset, y = theta, fill = "grey30")`；
  `geom_node_text` → `aes(x = r + offset, y = theta, label = label)`；
  `geom_node_point` → `aes(x = r + offset, y = theta)`。
- **标签落在"最外一圈之带"之外（R15）**：`ggplot_add.ggcircos_node` 把 `max(tracks$r1, 1) - 1`
  加进 `offset`，声明轨道后节点文字不会被第一条带埋住；`geom_node_arc()` 画的是环本身，不加这个偏移。
- `geom_node_arc` 画的是**环带扇区**：`xmin = r + offset`、`xmax = r + offset + width`、
  `ymin = theta0`、`ymax = theta1`，用 `GeomRect` 的绘制逻辑（`GeomNodeArc` 只做 mapping 组装）。
- `rotate = TRUE`：旋转角由 §5.4 的 `screen_angle()` 给出（**不是**从变换后坐标反推），
  左半圈自动 `+180°` 并翻转 `hjust`；`rotate = FALSE` 时全部水平，左半圈 `hjust = 1`、右半圈 `hjust = 0`。
- `geom_node_text` 不接收 `angle` 美学（和 `rotate` 冲突，直接 `cli_abort()` 说明）。
- **`repel = TRUE` 沿径向外推避让（B40）**：按角序遍历，若相邻两标签在该半径上的
  **弧距** `< 1.15 * step`（`step` 默认 0.055），后者外移 `step`。用弧距而非常数角度，
  因为"挤不挤"取决于屏幕上的一行字有多高，而扇形越往外越宽。
  **角度不变**——角度承载节点身份，标签只能沿自己那条辐条移动。
  **不用 `ggrepel`**：它在矩形空间里解冲突，实测会把标签排成水平直线，丢掉环形语义。
- **⚠ 决策（B6）**：`geom_node_arc(linewidth = )` 必须真正生效——`GeomNodeArc$required_aes`
  加上 `linewidth`，或改为在 `default_aes` 里用 `params$linewidth` 注入；当前参数被静默忽略。

#### 6.2.1 `rotate` 的**传输路径**（B13）⚠ 决策

`rotate` / `nudge_x` / `angle` 是**图层级设置**，由 `geom` 在 `draw_panel()` 里消费——
它们不是数据列，也不是 `aes()` 能表达的东西。因此（2026-10-19 实现）：

1. `geom_node_text(rotate = TRUE)` 把它写进 `layer$ggcircos`（tag），**不**进 `ggplot_add` 的
   `geom_params`——由 `ggplot_add.ggcircos_node` 读出来，**再塞进 `object$geom_params`**。
2. `geom_params` 里的 `rotate` 必须在 `geometry_params`（§8.2 的丢弃名单）里**豁免**，
   否则 `drop_geometry_params()` 会把它删掉，`draw_panel()` 只拿到 `rotate = FALSE` 的默认值。
   **这个 bug 曾经真的存在**：整条 `rotate` 路径是哑的，所有标签都是水平的（B13）。
3. 因此 §8.2 的"只许写自己这层"契约有一条**显式例外**：
   `ggplot_add` 可以把 `rotate` / `nudge_x` 写进**自己这一层**的 `geom_params`。
   这不违反 A4（那是禁止碰 `plot$data`/`scales` 等），只是把"允许写什么"写清楚。
4. `GeomNodeText$draw_panel(rotate = FALSE)` / `GeomSectorLabel$draw_panel(angle = "horizontal")`
   的形参名**固定**为这两个名字，测试要断言形参存在。

### 6.3 `geom_sector_label()`

```r
geom_sector_label(mapping = NULL, data = NULL, offset = 0.16,
                  angle = c("horizontal", "radial"), ...)
```

在 sector 弧**中点**、`r + offset` 处放标签。`angle = "radial"` 时按 §5.4 的屏幕角旋转；
`angle = "horizontal"` 时水平 + 左半圈翻 `hjust`；数值原样传给 `GeomText$angle`。

**⚠ 决策（B1）**：`sectors$theta` 必须是**环形连续段**的中点（§4.6-5），不是 `min()`/`max()`
的中点。跨 `θ = 0` 的 sector 用 `crosses_zero = TRUE` 标记，标签位置按连续段算。
**回归测试**：`start = 3π/2` 时每个 sector 的标签角落在该 sector 节点弧的并集内（容差 `1e-9`）。

`angle` 的传输路径同 §6.2.1：`"radial"` → `rotate = TRUE`，`"horizontal"` → `rotate = FALSE`，
数值 → `geom_params$angle`（`draw_panel` 的形参是 `angle`）。

### 6.4 `geom_track()` 与 `position_track()`

```r
geom_track(
  mapping = NULL,
  data    = NULL,              # 轨道数据，必填
  geom    = "col",             # 白名单，见下
  node    = NULL,              # 未加引号的列名：节点 id
  band,                        # c(r0, r1) 或已声明轨道名
  stack   = FALSE,
  scales  = c("free", "fixed"),
  limits  = NULL,              # scales = "fixed" 时必填
  baseline = NULL,             # "free" 的下界；NULL = 全非负时取 0，否则取 min
  width   = 0.9,               # 柱/扇区宽度，占节点弧比例
  offset  = NULL,              # band 为名字时忽略
  position = NULL,             # ⚠ A6：NULL = 内部新建 position_track()
  ...
)
```

**白名单与内部装配**

| `geom` | `x` 必需？ | stat | Geom | 说明 |
|---|---|---|---|---|
| `"col"` / `"bar"` | 是 | `"identity"` | `GeomTrackBar`（继承 `GeomBar`，只覆写 `setup_data` 管 `width`） | 堆叠用 `stack = TRUE` |
| `"area"` | 是 | `"identity"` | `GeomArea` | |
| `"boxplot"` | 是 | `StatTrackBox`（本包） | `GeomCrossbar` | 分位数由 stat 算，不用 `stat_boxplot` |
| `"violin"` | 是 | `StatTrackViolin`（本包） | `GeomTrackViolin` | 密度轮廓由 stat 算，不用 `stat_ydensity` |
| `"density"` | 是 | `StatTrackDensity`（本包） | `GeomTrackDensity` | 不闭合的密度曲线（ridgeline），与 violin 同一份 KDE |
| `"point"` | 否（缺省 = 带中心） | `"identity"` | `GeomPoint` | 标数值就写 `x` |
| `"tile"` | 否 | `"identity"` | `GeomTrackTile` | 热图轨道：径向填满 band（`band_fill`） |
| `"text"` | 否（缺省 = 带中心） | `"identity"` | `GeomText` | |

**同节点的多行等分该节点弧（B37）**：除**聚合型** geom（`"boxplot"`、`"violin"`、`"density"`）外，
每个节点的第 `k` 行占 `theta0 + (arc - bin_w·m)/2 + (k-1)·bin_w`，`bin_w = arc · width / m`
（`m` 为该节点行数）。tile 靠它出热图，area/line 靠它出覆盖度剖面，col 靠它把同一节点的多次测量
画成一排刷子。聚合型 geom 例外，因为它们的 stat 先把多行折成**一个标记**，那个标记该占满整条弧。
**每节点只有一行时 `bin_w = arc · width`，与以 `theta` 为中心逐位相同**，单值轨道不受影响。

**聚合与否由 `track_geom_spec()` 的 `aggregate = TRUE` 声明，不靠 geom 名字逐个列举。**
`track_angles()`（整弧 vs 切分）与 `ggplot_add` 的 `node` 补映射都读这一个标记。B41 的根因
正是"按名字列举"：加 violin 时两处都要加，漏一处就静默出错。

不在表里 → `ggcircos_error_data`（消息列出可用值）。
`x` 必需而缺失时报：`"geom_track(geom = 'col') needs an x aesthetic; map the value column, e.g. aes(x = value)."`

**为什么 `boxplot` 要自己的 stat**：数据里的 `y` 是节点 id（字符），而坐标系的 `y` 必须是弧度。
`stat_boxplot` 需要一个离散 y 轴做分组，与 `scale_y_continuous(limits = c(0, 2*pi))` 直接冲突。
`StatTrackBox` 把"按 `node` 分组算分位数"和"`node` → `theta`"一次做完，输出
`xmin`/`xlower`/`xmiddle`/`xupper`/`xmax` + `y = theta` + `width`，下游只是 `GeomCrossbar`。

**为什么 `violin` 也要自己的 stat（B41）**：环里的 violin 是**横躺**的——值沿半径、
密度决定该值处占据多少**弧**。`stat_ydensity`（`geom_violin` 用的那个）把密度放在 `x`（水平
宽度）、值放在 `y`，正好差 90°；而且它要求离散的 `x` 做分组，会撞同一个 y 轴问题。所以：

- `StatTrackViolin` 按 `node` 分组调 `stats::density()`，输出**闭合轮廓**的两段：
  值 `x = c(at, rev(at))`、弧 `y = theta + c(w, rev(-w)) * half`，`half = arc * width / 2`，
  `w` 归一化到 `[0, 1]`（每个节点自己的最大密度 = 满弧宽）。一个节点一个 `group`，
  `GeomTrackViolin`（继承 `GeomPolygon`）只负责把它画成填充多边形。
- **半径方向仍然由 `PositionTrack` 管**：stat 只写值 `x`，位置把它仿射映射进 `[r0, r1]`。
  violin 不接管半径，因此 `scales`/`limits`/`baseline` 对它与对 col 生效一致。
- **密度不能走过 `position`**：`violinwidth` 是 stat 的产物而不是美学，`PositionTrack`
  拿到的 frame 里已经没有它了（ggplot2 按美学筛列）。B41 的第一版就死在这里——
  在 position 里读 `data$violinwidth` 永远拿到 `NULL`，所有点挤在 `theta` 上，图上什么都没有。
  所以**角度偏移必须在 stat 里就写进 `y`**，position 只处理半径。
- **不能靠 `after_stat()` 补 `y`**：track 的 `y` 是 `ggplot_add` 阶段由 `track_angles()`
  写好的列（`track_defaults()` 映射 `.circos_theta`），`default_aes` 里再写
  `y = after_stat(y)` 会覆盖那个映射，让所有节点塌到第一个角度。
- **`order` 是保留名**：`GeomPolygon$draw_panel` 内部用 `order()` 排序，
  stat 输出一个叫 `order` 的列会撞上 `"argument 1 is not a vector"`。轮廓点序用行序本身
  （stat 按节点分组输出），不额外写列。

参数：`n`（密度点数，默认 64）、`adjust`（`stats::density()` 的带宽倍数）、
`trim`（默认 `TRUE`，丢掉观测范围外的密度尾巴；`FALSE` 时尾巴会伸出数据范围，
`geom_track()` 的径向映射会按位置把它夹到 band 里）。样本量 `< 3` 或 `sd == 0` 的节点
画不出密度，返回 `NULL`（该节点不出轮廓，其余照画），不静默画成直线。

**为什么 `density` 是独立 geom 而不是 `violin` 的参数（§17-7 收尾）**

两者共用同一份 KDE，差别在**闭合与否**，而这决定了画什么、以及接缝怎么处理：

| | `"violin"` | `"density"` |
|---|---|---|
| 形状 | 闭合多边形（`theta ± w·half` 两侧 + 首尾相接） | **不闭合的单侧曲线**：从节点中心线 `theta` 升起，`theta + w·half` |
| aes | `fill` 是主色 | `colour` 是主色（要填充就用 `"violin"`，不重复造一遍） |
| 用途 | 看单个节点的分布形状 | 同一带里**叠多个分布**比较（ridgeline）；或多个节点共用一条带 |

`"density"` 在极坐标里是 ridgeline：每个节点的曲线贴着自己的辐条升高，多个节点并排就是
一圈山脊。这正是填充体做不到的事——填充会互相遮住，线不会。

三条实现契约：

1. **点序是单向的**：`x = at`、`y = theta + w · half`，按值升序排列。不折返、不闭合，
   `GeomTrackDensity`（继承 `GeomPath`）直接连成线。`w` 归一化到 `[0, 1]`，与 violin 一致，
   所以两种 geom 在同一 `band` 里高度可比。
2. **不画地平线**。试做过 `floor =` 选项（在每个节点中心角画一条径向基线），
   几何上是连贯的，但它横穿曲线占据的整个半径，读起来像"轴上多了一笔"而不是
   山脊的地面；`trim = TRUE` 时曲线两端本来也不落在中心线上（实测 gap 0.01–0.09 rad），
   硬贴一条线反而制造了不存在的关系。**需要地平线就叠一层 `geom = "col"` 或
   自己加 `geom_segment()`**，不要塞进这个 geom。
3. **接缝切分不能复用多边形的裁剪器**。`seam_pieces()` 是 Sutherland-Hodgman **闭合多边形**
   裁剪，它的输出带着一整圈边界，对线没有意义。折线用 `seam_folds()`：顺序走顶点，
   遇到相邻两点落在不同圈（`floor(y / 2π)`）就在缝处插入一个折点，收掉当前段、
   在下一圈从 `0` 起一段新线。两段在圆上是同一个位置，所以线视觉上连续。

**数据流（必须按这个顺序；ggplot2 的构建顺序是 stat → setup_data → position）**

1. `geom_track()` 在 `ggplot_add` 时（`layer-add.R`）从图里取 layout，把"节点 id → θ"和
   "节点弧宽"两张查找表塞进 layer 的 `geom_params` 与 `position` 对象
   （**只读**，不改 `plot$data`/`plot$mapping`/`plot$scales`）。
2. **角度方向**：`StatTrackBox`（boxplot）/ `StatTrackViolin`（violin）/
   `StatTrackDensity`（density）或 `GeomTrackBar$setup_data`（bar 类）
   写入 `y = theta`、`width = 节点弧宽 * width`；
   `GeomTrackBar` 先写 `width` 再调父类 `setup_data`（父类用 `width` 算 `ymin/ymax`）。
   `tile`/`point`/`text` 不需要这一步。`node` 列保留；`group` 缺省设为 `node`。
   **聚合型 geom 的 stat 需要 `node` 这个美学才分得开组**，由 `ggplot_add` 补映射
   （读 `track_geom_spec()$aggregate`，不是读 geom 名字）。
3. **半径方向**：`PositionTrack$compute_layer()`（所有 geom 共用）——若 `y` 还不存在则写入
   `y = theta`；把 `x`/`xmin`/`xmax`/`xmiddle` 映射进 `[r0, r1]`；`stack = TRUE` 时先按节点累计再映射。
4. 之后交给 geom 自己的 `draw_panel`，一切照常。

**`position_track()` 签名**（导出，可单独用）

```r
position_track(band, node = NULL, geom_name = NULL, anchored = FALSE,
               band_fill = FALSE, stack = FALSE,
               scales = c("free", "fixed"), limits = NULL, baseline = NULL,
               width = 0.9, offset = 0)
```

**⚠ 决策（R9）：没有 `node_table` 逃生口，position 只持向量。**
§6.4 早先写的 `position_track(band, node_table, ...)` 与 §8.2 直接矛盾：
§8.2 要求 position 只持有派生快照（`node_ids`、`node_theta` 这类普通向量），
不持有 `data.frame` 或 layout 对象，这样同一个 `PositionTrack` 才能被多层共用
而互不污染。§8.2 的理由更硬（它是一条可测的隔离契约），所以这里跟随它：

- `node =` 只是一个**列名字符串**，不是节点表；
- 节点 id 与角度快照由 `ggplot_add.ggcircos_track()` 在图层数据上算好再传；
- 想脱离 `geom_track()` 手搓一层，就自己调 `position_track()` 并把
  `y` 写成节点角度——轨迹的 `y` 本来就是角度（§4.2），不需要节点表。

**归一化契约**（`position_track`）

设带为 `[r0, r1]`，节点 `i` 的值为 `v`：

| 模式 | 公式 |
|---|---|
| `scales = "free"`，`stack = FALSE` | `x' = r0 + (r1-r0) * (v - base) / (max_i - base)`，`base = baseline`（NULL 且全非负 → 0，否则 `min_i`） |
| `scales = "fixed"`，`stack = FALSE` | `x' = r0 + (r1-r0) * (v - L1) / (L2 - L1)`，`L = limits` |
| `stack = TRUE` | 按节点 `c = cumsum(v)/sum(v)`；`xmin' = r0 + (r1-r0)*c_prev`，`xmax' = r0 + (r1-r0)*c` |
| 有 `xmin/xmax/xmiddle`（boxplot） | 三个都过同一个仿射映射；`stack = TRUE` 报 `ggcircos_error_data` |

边界：

- `max_i == min_i`（该节点值全等）→ `x'` 取 `(r0+r1)/2`，不报错；
- `stack = TRUE` 且存在 `< 0` 的值 → `ggcircos_error_data`（"stacking needs non-negative values"）；
- `limits` 缺省且 `scales = "fixed"` → `ggcircos_error_data`。

**手工逃生口**（⚠ A6）：`position_track(band = c(0.2, 0.4), stack = FALSE, node_table = layout_nodes(layout))`
可以直接挂在任何 `geom_*` 上（例如自算好分位数的 `geom_crossbar()`）。

**`geom_track(position = )` 的规则**（A6）：

- `position = NULL`（默认）：`geom_track()` 内部新建 `position_track(band, node, ...)`，
  用自身参数填满，行为不变。
- `position = <PositionTrack>`：**外部传入的 position 原样使用**，`geom_track()` 不再覆盖它的
  `band`/`stack`/`scales`/`limits`/`node`。`ggplot_add` 只补 `position$node_table`
  （从 layout 取，覆盖用户可能给的空值）。这与"手工逃生口"是同一套语义。
- 传入不是 `PositionTrack` 的对象 → `ggcircos_error_data`，说明 `position` 只接受
  `position_track()` 的返回值（`"position must come from position_track(); see ?geom_track"`）。
- **回归测试**：同一个 `position_track()` 对象挂在两个 `geom_track()` 层上，两层 `ggplot_build()` 的
  `x` 映射完全一致（说明没有被其中一层改写）。

### 6.5 `geom_track_axis()`

```r
geom_track_axis(band, breaks = NULL, limits = NULL, labels = waiver(),
                text.size = 3, line.size = 0.2, tick.length = 0.02, ...)
```

- **`limits` 可省（B39）**：省略时从**同一 band 的轨道图层**读它归一化用的范围
  （要求那层 `scales = "fixed"`）。轨道已经知道自己的范围，轴再问一遍是纯重复，
  而且两者会漂移。没有可读的轨道层时报 `ggcircos_error_data`，不画空轴。
  显式传入的 `limits` 优先。
- `breaks = NULL` → `pretty(limits, 3)`。
- 画三样东西：`[r0, r1]` 两端沿节点环的弧线、每个 break 在其**自身半径**上的短径向刻度、
  刻度标签（放在该 break 半径再往外 `2·tick.length` 处，**切向**排，同 circlize 的 `circos.axis`）。
  轴整体立在第一个节点的起始角（那里是 gap，不遮数据）。
  ⚠ 刻度线 grob 必须 `fill = NA`（grid 默认填白，两个环会把轴下面的图层整个刷掉，B8）。
- 只对声明的 band 生效；band 名未声明 → `ggcircos_error_band`。

### 6.6 图例

- `fill`/`colour` 由普通 scale 产出图例，不需要 `ggnewscale`。
- 每层独立 scale 冲突（两条弦层各要一个 `fill` scale）时，用户用 `ggnewscale::new_scale_fill()`——**本包不再自动调用**（旧版在 `dodpos` 分支里偷偷调 `new_scale_color()`，是隐藏行为）。

---

## 7. 头函数 `ggcircos()`

```r
ggcircos(
  data,                       # §4.4 支持的输入
  nodes     = NULL,
  sectors   = NULL,
  order     = NULL,
  layout    = c("circular", "arc"),
  gap       = 0.01,
  sector_gap= 0.05,
  slot_fill = 0.9,
  start     = 0,
  tracks    = NULL,
  track_offset = 0.06,
  self_loops = FALSE,
  ...
)
```

**必须做的 5 件事**（不多不少）：

1. `layout <- circos_layout(...)`；把 `data` 换成 `layout$links`。
2. `ggplot(layout$links)`，**不带 mapping**。
   **⚠ 决策（B36）**：`x`/`y`/`xend`/`yend` 是弦图层自己的事，`geom_chord()` 在
   `ggplot_add()` 里把它们映射上（§6.1）。挂在 plot 级会让每个标准 ggplot2 图层
   都继承用不到的列：`geom_rect()` 要的是 `xmin`/`ymin`，继承了 `x`/`y` 反而报
   `Column \`xend\` not found`。清空之后，标准层只需要自己的 `aes()` 和 `data`，
   不必写 `inherit.aes = FALSE`（回归测试 `test-demos.R` 的 D3）。
   曾试过保留 `x`/`y`：`geom_rect()` 依然失败，故全空。
3. `+ coord_circos()`（或 `coord_arc()`）+ `theme_circos()`。
4. `+ scale_y_continuous(limits = c(0, 2*pi), expand = c(0, 0))` +
   `scale_x_continuous(limits = c(0, xmax), expand = c(0, 0))`，
   其中 `xmax = max(tracks$r1, 1) + 0.22`（常量 `label_space`）：这 0.22 留给**环外标签**。
   原来的 `+0.05/+0.06` 让节点文字正好压在 x 上限上——无轨道时被顶到画布边缘，
   有轨道时被第一条带埋掉（R15）。
5. `attr(plot$data, "circos_layout") <- layout`，**此后 layout 只读**。

**不做的**：不加任何 `geom_*`（连弦也不加）。用户可以只画节点，也可以加三层弦。

**必须能组合**：

| 组合 | 期望 |
|---|---|
| `ggcircos(x)` 单独 | 空图（有坐标系、无图层）——合法 |
| `+ facet_wrap(~ group)` | 见 §9.1（甲方案：切弦，共用节点环） |
| `+ coord_circos(clockwise = FALSE)` | 镜像，标签跟着翻（§5.1 / §5.4） |
| `+ theme(...)` | 覆盖 `theme_circos()` |
| 保存为 `.rds` 再读回加层 | 正常（layout 在 data 属性里，可序列化） |

**⚠ 决策**：`yrange`/`xrange` 固定由头函数设定，用户改 `scale_y_*` 的 `limits` 会导致角度错位；
文档必须写"不要改 y 的 limits"。**⚠ 决策（A2）**：由 `coord_circos()` 在 `setup_panel_params`
里检测 `theta.range` 是否仍覆盖 `[0, 2π]`，偏离就 `cli_warn(class = "ggcircos_warning_scale")`
（当前实现没有这条警告，实测覆盖后静默画成错误图形）。

---

## 8. 语法契约：layout 只读（A4）⚠ 决策

### 8.1 `ggplot_add` 只许读，不许写

四个方法：`ggplot_add.ggcircos_chord`、`.ggcircos_track`、`.ggcircos_node`、`.ggcircos_axis`。

**允许**：读 `circos_layout_of(plot)`；把解析结果写进**自己这一层**的 `data`/`mapping`/`geom_params`。

**禁止**（用测试守住，§14.3）：

- 改 `plot$data`、`plot$mapping`、`plot$scales`、`plot$theme`、`plot$coordinates`、`plot$facet`；
- 往 `plot$plot_env` 里塞东西；
- 依赖图层添加顺序（`+` 的左右顺序只影响遮挡）。

**⚠ 决策（B13）——`geom_params` 白名单**：`geometry_params`（在 `re_add()` 前被丢弃的
名字列表）**不得**包含 `rotate` / `nudge_x` / `angle`。这三个是标签层 `draw_panel()` 的形参
（§6.2.1），`ggplot_add` 必须能把它们塞进自己这一层的 `geom_params`。
它们被丢弃过一次，导致 `rotate` 整条路径失效——测试要断言
`geom_node_text(rotate = TRUE)` 的 `ggplot_build()` 结果里 `angle` 不全为 0。

**为什么还需要 `ggplot_add`**：`geom_track(data = dat, node = id)` 里的 `id` 要按 layout 翻译成 θ，
这件事只能在拿到 plot 的时候做。所以它存在，但只做"翻译"，不做"累积"。

**旧版对比**：旧版在每一层里 `attr(plot$data, "node") <- refdat`（累积修改），
再加 `xmaxtmp` 累加——这就是"多轨不叠加 + 顺序敏感"的根源。

### 8.2 layer 与 position 只能持有**派生快照**，不持有 layout

- `ggplot_add.ggcircos_track` 现在会把整个 layout 的节点表塞进 `object$position$node_table`
  （`R/layer-add.R:277`）。**改成**：position 只持有两个普通向量
  `node_ids`、`node_theta`（以及可选的 `node_arc`），不再持有 `data.frame` 或 layout 对象。
- 归位后，同一个 `PositionTrack` 对象可以被多层共用而互不污染（§6.4 的回归测试）。
- `ggplot_add` 注入给 layer 的 `data` 必须是**副本**；测试断言"改 layer 的 data 不改 layout"。

### 8.3 目标 12.1 layout 是不可变值（访问器返回副本）

- `layout_nodes()` / `layout_links()` / `layout_sectors()` / `layout_tracks()` / `layout_params()`
  一律返回**副本**（`data.frame` 的 `[` 复制语义已足够，但要显式测试）。
- `circos_layout_of(plot)` 返回的是 layout 对象本身（只读约定），文档明确"不要修改返回值"。
- 每次公开访问时**不做**完整不变量自检（代价太高），只在构造时自检一次；
  但测试里要对"改动副本不影响 layout"写成断言。

---

## 9. 美学与 scale 总表

### 9.1 facet 语义（A5）⚠ 决策

**v0.1（甲方案，当前实现）**：facet 切的是**弦**，节点环在所有 panel 共用。

- `ggcircos()` 的 `links` 数据里有 facet 变量，`facet_wrap(~ g)` 把弦表按 `g` 切开；
- 每个 panel **重复画完整的节点环**（所有 `geom_node_*` 层在每 panel 都是全部节点）；
- 适合"同一批节点、不同样本的连线对比"。

**规格要求**：

| 项 | 要求 |
|---|---|
| `geom_chord` 按 facet 切 | 是：每 panel 只画属于该 panel 的弦 |
| `geom_node_*` 按 facet 切 | **否**：每 panel 画全部节点（这是甲方案的已知行为） |
| README / vignette 的描述 | 只能按甲方案写；**不得**说"每个样本一个独立的环" |

**v0.2（乙方案，`by=` 参数）**：facet 按变量**重建 layout**，每 panel 自己的节点子集。

- **契约**：`ggcircos(..., by = <节点表列名>)` 表示"按这一列分组，每组一个独立的环"。
  分组的列必须在**节点表**里（`nodes=` 给的，或从矩阵/表的行名推出），因为它决定
  **哪些节点属于哪个 panel**，不是哪些链接。
- `circos_layout(..., by = col)` 返回一个 **layout 列表**（class `circos_layouts`），
  每个元素是一个完整的 `circos_layout`，带一个 `panel` 属性记它对应的分组值。
  `by = NULL`（默认）时行为不变，仍返回单个 `circos_layout`。
- `ggcircos(..., by = col)` 时：plot 的数据是**所有组链接**拼起来，多一列 facet 变量；
  每个 panel 的节点环、弦、轨道都来自该组的 layout。`facet_wrap(~ <同一列>)` 收尾。
- **每 panel 独立排序**（这正是乙方案相对甲方案的价值）：同一套节点在不同样本里
  可以按各自的权重排序，弧长也各自按该组权重归一。
- **facet 变量必须与 `by` 一致**：`ggcircos(by = "sample")` 后必须
  `facet_wrap(~ sample)`，用别的列分面会 `ggcircos_error_data`。
- **节点身份是 `(组, id)` 二元组**：节点表里同一个 `id` 可以出现在不同组（每个样本
  各有自己的 `a`、`b`……），只要**组内唯一**。所以"分组"是在每个组内重建一个独立环，
  不是把一张大图切块。
- **链接归位**：一条链接属于哪个 panel，优先看它**自己携带的组列**（与 `by` 同名的列）；
  链接表没有这一列时，才用两端节点所在的组——此时两端分属不同组就**丢弃并
  `cli::cli_inform()` 一次**（独立环里无处安放）。**推荐**链接表直接给组列，语义最明确。
- **轨道也按 panel 放置**：`geom_track()` / `geom_track_axis()` 在 `by =` 下可用。
  每行按**自己那一组**的节点表定位（`track_angles()` 拿到的是各组的 layout 列表），
  所以两个 panel 可以把同一个 id 放在不同的角度上。轨道数据**必须带分组列**
  （与 `by =` 同名），否则报 `ggcircos_error_data`——一行属于哪个环是它自己的属性。
  **轨道带是共享的**：`tracks =` 是全局设置，各组 band 半径相同（代码里断言这一点，
  不一致就报 internal error），所以 `PositionTrack` 只需一个 band。
  `geom_track_axis()` 的刻度落在**各 panel 自己的首节点之前**。
- **定位文档 D1 的"多环"图**：在乙方案实现前，D1 只能用甲方案的"共享节点环"版本；
  乙方案落地后 D1 升级为真正的"每样本独立环"，README 与定位文档可以据此更新宣传图。

### 9.2 美学总表

| 美学 | chord | node_arc | node_text | node_point | track |
|---|---|---|---|---|---|
| `x` | ✓(r) | ✓ | ✓ | ✓ | ✓(值) |
| `y` | ✓(θ) | ✓ | ✓ | ✓ | 自动 |
| `xend`/`yend` | ✓ | — | — | — | — |
| `fill` | ✓ | ✓(默认 `"grey30"`，不映射 sector) | — | — | ✓ |
| `colour` | ✓ | ✓ | ✓ | ✓ | ✓ |
| `linewidth` | ✓ | ✓ | — | — | ✓ |
| `linetype` | ✓ | ✓ | — | — | ✓ |
| `alpha` | ✓ | ✓ | ✓ | ✓ | ✓ |
| `label` | — | — | ✓ | — | ✓ |
| `angle` | — | — | ✗（用 `rotate`） | — | ✓ |
| `group` | ✓ | ✓ | ✓ | ✓ | ✓ |

所有美学走标准 scale：`scale_fill_gradient2()`、`scale_colour_viridis_d()` 等直接可用。
**⚠ 决策**：`geom_node_arc()` 的 `fill` 默认是常量 `"grey30"`，**不**默认映射 `sector`——
否则弦层的 fill scale 会被节点层抢占。要按 sector 上色就显式 `aes(fill = sector)`。

---

## 10. 主题

```r
theme_circos(bgcolor = "white", grid = FALSE, axis = FALSE,
             legend.position = "right", margin = 10)
```

= `theme_void()` + 背景色 + 合理的图例与边距。导出。
旧版 `theme_blank()` 是内部函数、没导出——这是它"看着能用但改不动"的一部分。

**⚠ 决策（B9）**：`bgcolor` 必须**同时**设置 `plot.background` 与 `panel.background`，
否则 `bgcolor = "white"` 的默认渲染是 ggplot2 的灰底，与名字相反。
`theme_circos()` **不**基于 `theme_void()` 搭建（`theme_void()` 会把 `panel.background`
清成 `element_blank()`），而是从 `theme()` 起手、末尾显式写
`panel.background = element_rect(fill = bgcolor, colour = NA)`。

**坐标层换成 `CoordRadial` 后的实际元素名**（2026-10-19 在 ggplot2 4.0.3 实测确认）：

| 名字 | 存在于 element hierarchy？ | `theme_circos()` 怎么处理 |
|---|---|---|
| `axis.text.theta` | ✅ | `axis = FALSE` 时 `element_blank()`；`TRUE` 时 `element_text()` |
| `axis.ticks.theta` | ✅ | 同上，`element_line()` |
| `axis.line.theta` | ✅ | 同上 |
| `axis.title.theta` | ❌ theta 轴没有 title 槽 | 不写 |
| `panel.grid.major.theta` | ❌ **不存在**（4.0.3 实测） | 不写；`CoordRadial$render_bg()` 自己按这个名字拼线，靠 scale 无 breaks 来关闭 |
| `panel.grid.major.r` | ❌ 同上 | 不写 |

⚠ 换成 `CoordRadial` 之前，spec 写的"`grid = TRUE` 打开 `panel.grid.major.theta/.r`"
是**错的**：给不存在的元素赋值只会触发
`The ... theme element is not defined in the element hierarchy` 的 warning，一行线都没有。
现在 `grid` 开关实际切换的是 `axis.text/ticks/line.theta`（四个 polar 元素里唯一真实存在的三个）。
**回归测试**：`theme_circos()` 应用后 `ggplot_build()` 无 theme 相关 warning（`plot_theme()`）。

---

## 11. 废弃与迁移

| 旧 API | 新 API | 处理 |
|---|---|---|
| `ggcircos(x)` | `ggcircos(x)` | 名字保留，行为变（不再自动加弦层）；NEWS 注明 |
| `geom_curve_link()` | `geom_chord()` | shim + 弃用警告 |
| `geom_circos(geom=, mapping=, pwidth=, ...)` | `geom_track(geom=, mapping=, band=, ...)` | shim，把 `pwidth` 映射到 band 宽度、`offset` 映射到 `track_offset`；弃用警告 |
| `geom_link_lab()` | `geom_node_text()` | shim |
| `as.tbl_lk()` / `tidy_link()` | `circos_layout()` | shim：`as.tbl_lk()` 返回 `circos_layout`，警告 |
| `tl_extract()` | `layout_nodes()` / `layout_links()` | 删除（无用户） |
| `%|||%`、`generate_curvature*()` | — | 删除 |

**⚠ 决策（A7）**：shim 的验收是**"能跑 + 给 warning + 参数映射符合附录 C"**，
**不要求**"出图与新版一致（同一 vdiffr 快照）"。理由：旧版本身画得不对（多轨不叠加、
标签角度错位），要求"一致"等于要求"一致地错"。`test-deprecated.R` 只断言：
能构建、warning 类为 `ggcircos_warning_deprecated`、`pwidth`/`offset` 映射到正确的
`band` 区间（数值断言）。

---

## 12. 文档

每个导出函数：roxygen + `@examples`（可运行，不依赖网络）+ `@seealso`。
必须有的 3 篇 vignette：

1. `correlation`：矩阵 → 弦图 → 排序 → 配色 → 加节点轨道。（对应提案 §3.2-A）
2. `genomes`：sector + 双基因组 + ribbon。（对应 §3.2-B，覆盖 open issue #1）
3. `tracks`：三种轨道 + 轴 + 分面（**分面按 §9.1 甲方案描述**）。（对应 §3.2-C）

README：安装、30 秒上手、三张图、与 circlize/ggraph 的分工说明。
**README 不得**宣传"每个 facet 一个独立的环"（v0.1 是甲方案）。

---

## 13. 性能与规模

| 场景 | 目标 |
|---|---|
| 1k 条线弦 | `ggplot_build` + `ggplotGrob` < 1.5 s |
| 1k 条 ribbon | < 2 s |
| 5k 条 | < 8 s，且只在 `ncp ≤ 20` 时保证 |
| 200 节点 | 标签不重叠的判断不做（交给用户/ggrepel） |

**grob 数量目标（⚠ 决策 / B11）**：

- **离散美学**（`fill`/`colour` 映射到 ≤ 32 个水平）：每层 **≤ 1 + 水平数** 个 `pathGrob`。
- **连续美学**：分块数 > 24 时把 `fill`/`colour` 量化到 16 级/通道的 rgb 网格再分块（每块用首行精确颜色，误差 ≤ 8/255），每层 **≤ 25** 个 `pathGrob`；**不得**退化成"每条边一个"。
  当前实测：`aes(fill = r)` 51 条边 → 51 个 grob，违反此条。
- 任何情况下不得出现"每行一个 grob"的旧实现形态（§6.1 的历史问题）。

---

## 14. 测试规范

### 14.1 单元测试（逻辑）

- **layout**：2/3/11/200 节点的角度不变量；`gap`/`sector_gap`/`slot_fill` 边界；`start` 非零；`order` 四种取值；自环丢弃；重复边各自占槽；`from` 不存在报 `ggcircos_error_node`；`gap` 过大报 `ggcircos_error_layout`。
- **槽位（§4.6）**：每个节点的槽位数量 = 入射边数；槽位互不重叠且都在节点弧内；`sum(slot_w) ≤ node_w`；**相邻槽位空隙 = `(1-p)·slot_w`**；`p ∈ {0.5, 0.9, 1}` 各一组。
- **扇区跨 0（B1）**：`start = 3π/2` 时每个 sector 的 `theta0/theta1/theta` 落在其节点弧并集内；`crosses_zero` 标记正确。
- **屏幕角（§5.4）**：`clockwise` × `start` 组合下，`screen_angle()` 与"两点 `transform()` 连线的方向"一致（容差 `1e-6`）。
- **track 归一化**：四种模式各一组；全等值；负值 + stack 报错；`limits` 缺失报错。
- **边界输入（C2）**：空 `data.frame`（0 边，合法）、单节点、NA `sector`（classed error）、负 `gap`（classed error）、`nodes` 表重复 id（classed error）、矩阵含 NA。
- **错误类（B5）**：每条 `ggcircos_error_<kind>` 的 `class` 断言，不接受裸 `stopifnot`。

### 14.2 图测试（结构）

`ggplot_build()` 之后断言：层数、`x`/`y` 范围、`fill` 值落在预期集合、`width` 列、
**槽位跨度 = `p·node_w/该侧边数`**。

### 14.3 契约测试（防回归）

- 加任意层前后，`plot$data` / `plot$scales` / `plot$mapping` / `plot$coordinates` / `plot$facet`
  的 `identical()` 检查（除新增图层外不变）。
- 图层顺序无关：`+ A + B` 与 `+ B + A` 的 `ggplot_build()$data` 逐层等价（顺序取反）。
- **访问器返回副本（A4）**：改 `layout_nodes(l)` 的返回值，`l` 不变。
- **position 不持有 layout（§8.2）**：同一 `PositionTrack` 挂两层，`x` 映射一致。
- `facet_wrap()`（按 §9.1 甲方案断言：每 panel 的弦数正确、节点数 = 全部节点）、
  `coord_circos(clockwise = FALSE)`（镜像）、`.rds` 往返各有测试。
- **scale 覆盖警告**：`+ scale_y_continuous(limits = ...)` 时触发 `ggcircos_warning_scale`。

### 14.4 视觉测试

`vdiffr::expect_doppelganger()` 至少 8 张：基础弦图、ribbon + 槽位（3 个 `p`）、
sector 跨 0、双基因组、三轨、线性/arc、`clockwise = FALSE`、废弃 shim。

### 14.5 门禁

```bash
make check1              # 0 error 0 warning（含 examples）
make crancheck           # 允许的 note：CRAN incoming（需联网）
```

CI 矩阵：ubuntu（R release / oldrel）× **ggplot2 `4.0.*` 单一版本线**（§1.4）。
`R CMD check` 在 CI 上显式加 `--no-manual`（或装 texlive），避免 PDF manual 的假红（C4）。

---

## 15. 里程碑与交付

### M0 — 止血（1–2 天，不改架构）

| 文件 | 改动 | 验收 |
|---|---|---|
| `R/method-ggplot-add.R` | `get_continuous_norm` 分支用 `xmaxtmp` 起算，多轨真叠加 | 两轨 `x` 区间不相交 |
| `R/ggplot-add-utilities.R` | join 键不丢（保留用户列） | `fill = 节点列` 能跑 |
| `R/method-ggplot-add.R` | 标签角度改用 `coord$transform` | 右半圆判定与真实一致 |
| `R/ggcircos.R` | 线性布局 `expand` 给足 | 弦不被裁 |
| 全包 | `aes_`/`aes_string` → tidy eval；`size` → `linewidth` | 无弃用警告 |
| `DESCRIPTION`, `README.md`, `tests/` | 真标题/描述、基础测试 | `make check1` 0/0 |

M0 是"新骨架之前先把旧代码变成可信基线"，**M1 会把其中大部分代码删掉**。

### M1 — 骨架（约 1 周）

交付：`circos-layout.R`、`coord-circos.R`、`geom-chord.R`、`geom-node.R`、`layer-add.R`、`theme.R`、`ggcircos.R` 重写；
`deprecated.R` 提供 shim。

验收：

```r
ggcircos(cor(mtcars), order = "hclust") +
  geom_chord(aes(fill = r)) + geom_node_arc() + geom_node_text() + theme_circos()
```

- 与 `notes/figures/target-design.png` 视觉一致（`vdiffr` 快照）；
- `facet_wrap()` 不报错；
- §14.3 契约测试全绿；
- `Imports` 只剩 `ggplot2`、`rlang`、`cli`。

### M2 — 轨道与外圈（约 1 周）

交付：`geom-track.R`、`geom-track-axis.R`、`geom-sector.R`、`layout = "arc"`；
vignette `genomes`、`tracks`。

验收：提案 §3.2 的 A/B/C 三段示例全部可跑；两条轨道半径区间不相交；
`facet_wrap(~ group)` 按 **§9.1 甲方案**断言（每 panel 弦数正确、节点环在每 panel 完整复制）。

### M3 — 打磨（3–5 天）

- `0.1.0`、NEWS、pkgdown、三篇 vignette 全部通过构建；
- 性能达标（§13，含"连续 fill 分桶 ≤ 25 grob"）；
- `R CMD check --as-cran` 只剩网络相关 note。

### M1.5 — 架构收口（v0.2 新增，**排在 M1 之前**）

A1–A6 的落地，顺序固定：

1. `DESCRIPTION` 改 `ggplot2 (>= 4.0.0)`；删掉所有 3.5 兼容分支（§1.4）。
2. `CoordCircos` 改为继承 `CoordRadial`；`coord_circos()` 参数按 §5.1 重做；
   删 `circos_screen_angle()`，加 `screen_angle()` 方法（§5.4）。
3. `circos_layout()` 实现 §4.6 的槽位几何 + 扇区连续段（B1）+ `gap` 自适应（B4）
   + 负 gap / NA sector / 空输入校验（B2/B3/B5）。
4. `position_track()` 只持有向量（§8.2）；`geom_track(position = )` 生效（§6.4/A6）。
5. `theme_circos()` 设 `panel.background`（§10/B9）；`CoordRadial` 下打开
   `panel.grid.major.theta/.r` 与 `axis.text.theta`。
6. 契约测试与 8 张快照补齐（§14.3/14.4）——**先有测试再改实现**，否则无法判断是否改坏。

---

## 16. 依赖与元数据

`DESCRIPTION` 目标形态：

```
Package: ggcircos
Title: Circular Layouts and Chord Diagrams for 'ggplot2'
Version: 0.1.0
Authors@R: c(person("Shuangbin", "Xu", role = c("aut","cre"), ...),
             person(...contributors...))
Description: Build chord diagrams, circular tracks and genome-level link
    maps with the grammar of graphics. Layouts are computed as data, so
    every layer is an ordinary 'ggplot2' layer and scales, facets and
    themes work as usual.
License: GPL-3
Imports: ggplot2 (>= 4.0.0), rlang, cli
Suggests: tidygraph, igraph, testthat (>= 3.0.0), vdiffr, knitr, rmarkdown
Config/testthat/edition: 3
```

**⚠ 决策**：不写 `LazyData: true`（没有 data）；不加 `compiler` 之类；`Roxygen: list(markdown = TRUE)` 保留。
`ggplot2` 下限从 3.5.0 提到 4.0.0（§1.4）。

---

## 17. 开放问题（不阻塞 M1）

1. **自环**怎么画：**已定**（2026-10-22），画成**向内折的小回环**，契约见 §4.7（B43）。
   选向内折而不是整段弧的理由：整弧会把自环画成与自己重合的一圈，和节点本身混在一起；
   向内折保持"一条边"的读法，且与 circlize 的 `circos_link(sector, sector)` 视觉一致。
2. **多半径节点**（同一个节点挂多层弧）：**已落地**（§4.9，B46）。`nodes` 表里同一个
   `id` 可以按 `r` 列出现多次，各层径向对齐、槽位独立、链接用 `from_r`/`to_r` 指层。
3. **`direction = -1`**（逆时针）是否与 `clockwise = FALSE` 重复：**已定**，只留 `clockwise`（§5.1）。
4. **标签避让**：是否内建 `ggrepel` 支持（`geom_node_text_repel()`）。v0.1 不做。
5. **`geom_track_axis()` 的断点能否从 track 层自动取**（要跨层读，和 §8 的"只读"原则不冲突但需要设计）。
6. **是否回到 CRAN**：取决于 §16 的依赖是否能保持住。
7. **`violin` / `density` 轨道**：**都已落地**（§6.4，B41/B42）。`"violin"` 画闭合轮廓
   多边形，`"density"` 画不闭合的 ridgeline 曲线；两者共用同一份 KDE，差别在闭合与否、
   主色是 `fill` 还是 `colour`，以及接缝切分方式（多边形裁剪 vs 只打折不插点）。
   **成本**：KDE 是每节点一次 `stats::density()`，实测 200 节点 × 40 样本、`n = 64`：
   `ggplot_build()` 0.32 s，同样数据换成 `col` 轨道 0.06 s，净开销约 0.26 s，不构成瓶颈。
   真正贵的是每节点一个 grob（多边形与线同量级）。
8. **facet 乙方案（§9.1）**：**已落地**（`by =` 参数，2026-10-22）。契约见 §9.1：
   `circos_layout(by=)` 返回 `circos_layouts` 列表，每 panel 一个独立 layout、
   自己的节点子集与排序；`ggcircos(by=)` 把它接进 facet。位置文档 D1 的"多环"图
   现在可以按"每样本独立环"来做。

---

## 附录 A：数据字典（一屏看懂）

```
circos_layout
├── nodes   : id label sector order n_links theta0 theta1 theta width r  [+ 用户列]
├── links   : from to theta theta_end theta0 theta1 theta_end0 theta_end1
│             x y xend yend edge_id                              [+ 用户列]
├── sectors : sector theta0 theta1 theta n_nodes r crosses_zero
├── tracks  : track width r0 r1
└── params  : gap sector_gap slot_fill start track_offset radius layout
              （gap / sector_gap 是用户值；gap_eff / sector_gap_eff 是生效值，R13）

槽位（§4.6，按侧分配）：
  节点弧 [theta0, theta1] 宽 node_w；from 侧有 d_out 条边、to 侧有 d_in 条边
  每侧各自等分：s_out = node_w / d_out，s_in = node_w / d_in
  槽位宽 slot_w = p · s_side，居中放置；第 k 个槽位：
    theta0_k = node_theta0 + (k-1)·s_side + (1-p)·s_side/2
    theta1_k = theta0_k + slot_w
  ribbon 的脚 = 槽位本身
  sectors 的 theta0/theta1 是环形连续段；跨 0 时 theta0 > theta1

绘图用的两组坐标：
  chord  : x=r  y=theta  xend=r  yend=theta_end        （line）
           + theta0/theta1/theta_end0/theta_end1      （ribbon 的脚 = 槽位）
  track  : x=值（被 position_track 映射到 band）  y=theta（由 node 查表）
```

## 附录 B：三个公式

1. 角度预算：`span = 2π - N·gap_eff·2π - S·sector_gap_eff·2π`，
   其中 `gap_eff = min(gap_user, 0.2/N)`、`sector_gap_eff = min(sector_gap_user, 0.2/max(S,1))`
2. 槽位：`s = node_w / n_links`，`slot_w = p · s`，槽位之间留 `(1-p)·s`
3. 贝塞尔控制半径：`k = 1 - (4/(3π))·|d|`，`d` 为两端最短有向角差
   → `|d| = π` 时过圆心；端点切线为径向

## 附录 C：与旧 API 对照

| 旧 | 新 | 备注 |
|---|---|---|
| `ggcircos(tbl, layout="linear", direction="right")` | `ggcircos(x, layout="arc")` | `direction` 由 `coord_arc()` 处理 |
| `geom_circos(data=, geom=geom_col, mapping=aes(y=id,x=v), pwidth=.3, offset=.02)` | `geom_track(data=, geom="col", mapping=aes(x=v), node=id, band=c(r0,r1))` | `pwidth` → band 宽 |
| `geom_circos(..., grid.params=, axis.params=)` | `geom_track_axis()` | 显式 `limits` |
| `geom_link_lab(hjust=, offset=, geom="text")` | `geom_node_text(rotate=, offset=)` | |
| `tidy_link(x)` | 内部：`layout_links(circos_layout(x))` | |
| `tl_extract(name="node")(plot$data)` | `layout_nodes(circos_layout_of(plot))` | |
