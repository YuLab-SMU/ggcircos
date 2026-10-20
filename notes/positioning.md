# ggcircos 定位说明（positioning）

> 状态：v0.1，2026-10-19。上游文档：[spec.md](spec.md)（做成什么样）、[revival-proposal.md](revival-proposal.md)（为什么重写）。
> 本文回答三个问题：市场里已经有什么、我们凭什么、怎么证明。**不写功能清单**——功能看 spec。

---

## 0. 一句话定位

> **环形布局的 ggplot2 引擎**：circlize 能画的它都能画，而且能分面、能拼图、能进 scale/legend 体系。

反面写法（不要用）：
- ❌ "又一个弦图包" —— 2026 年已经有三个了。
- ❌ "比 circlize 更好看 / 参数更多" —— 比不过，也不该比。
- ❌ "支持 igraph / tbl_graph / 矩阵" —— 用户不为此迁移。

---

## 1. 三条结论

1. **唯一不可复制的优势**：环形布局第一次成为 ggplot2 的**数据**，而不是一块自己管理的画布。由此才有分面、标准 scale、图例、拼图、序列化。
2. **"ggplot2 + 弦图"已不稀缺**：2026 年 7–8 月有两个 ggplot2 原生弦图包上了 CRAN（ggchord2 做流向、ggchord 做 BLAST），circlizePlus 2025 年也发过"ggplot2 风格写 circos"的论文。
3. **没人占的格子**：环形多轨（track / axis / sector）+ 真 grammar（facet / scale / 组合）。这个交集恰好就是 spec 的 M1+M2，也是全部定位的落点。

---

## 2. 竞争地图（2026-10 查证）

月下载量取自 CRAN logs（2026-10-19 查询，仅 CRAN 包）。

| 包 | 定位 | 月下载 | 结构性做不到 |
|---|---|---|---|
| [circlize](https://cloud.r-project.org/web/packages/circlize/index.html) 0.4.18 | base graphics 的环形全能王 | 47,375 | facet、ggplot scale/legend、patchwork 拼图、`.rds` 往返 |
| [circlizePlus](https://cloud.r-project.org/web/packages/circlizePlus/index.html) 0.9.1 | circlize 的 `+` 语法糖（渲染仍走 circlize） | 375 | 同上；只是写法好看 |
| [ggchord](https://dangjem.github.io/ggchord/) 0.10.0 | ggplot2 分层，专攻 BLASTN / 多序列比对 | 314 | 数据模型焊死三张表（seq / ribbon / gene），没有通用轨道与任意布局 |
| [ggchord2](https://nrennie.gitlab.io/ggchord2/) 0.0.1 | ggplot2 分层，类别流向弦图，支持 facet | 402 | 没有环形轨道 / 轴，不做基因组坐标 |
| [ggraph](https://cloud.r-project.org/web/packages/ggraph/index.html) | 布局即数据的成功案例 | 67,052 | 圆形只到网络布局，没有节点弧槽位、没有轨道 |
| ggbio / OmicCircos / BioCircos | Bioconductor / widget 路 | 105（BioCircos） | 自带 track 拼图体系而不是 facet；依赖重或不可组合 |

两个事实值得记住：

- **ggraph 的 67k 证明**的是"布局即数据"这条路能走通，不是"弦图"本身有 67k。我们该去 ggraph 的地盘讲，不去 circlize 的地盘讲。
- **ggchord / ggchord2 是 2026 年的新玩家**（各 300–400/月），说明这个细分还没有赢家，但也说明叙事已经被抢先说了半句。

---

## 3. 优势：分三类，别混着卖

### 3.1 硬优势（别人结构上做不到）

1. **`facet_wrap()`** —— circlize 是 base graphics，这辈子做不了。多组学多样本的环状组图，这是最痛的点。
2. **标准 `scale_*` / 图例 / 主题 / 保存 / 拼图** —— `scale_fill_gradient2()` 直接出图例，`patchwork` 直接拼，`.rds` 存回来还能加层。
3. **一套 layout 同时喂 chord / node / track / axis** —— ggchord 焊死比对、ggchord2 焊死流向，只有我们能跨场景复用同一套坐标。
4. **`layout_nodes()` 是导出数据** —— 别人可以拿它写自己的 geom 或包，这是生态位和长期价值。

### 3.2 软优势（能做但更省事，别当主打）

- 每层 1 个 grob：实测 1000 条 ribbon `ggplotGrob()` 1.22 s。
- `Imports` 只有 3 个包：CRAN 友好，且不再像旧版那样命悬 ggtreeExtra 内部函数。
- 作者位置：YuLab / ggtree 生态的分发渠道，这是 ggchord2、ggchord 都没有的。

### 3.3 伪优势（别写进 README）

参数多、好看、更快、"支持若干种输入格式"。这些在 circlize 面前都是劣势。

---

## 4. 三条 demo 与验收标准

定位靠图证明，不靠 bullet。三条都要进 README 第一屏 + 对应 vignette。

### D1 — 一次出多个环（杀手图）—— **已落地（2026-10-20）**

**目的**：证明 facet 不是"顺手支持"，而是结构性差异。

**数据**：4–6 个样本，每样本同一套节点、不同权重（相关性矩阵 / 物种丰度 / 基因家族计数均可）。

**验收（可执行断言）**：见 `tests/testthat/test-demos.R` 的 D1。

```r
p <- ggcircos(links_by_sample, order = "hclust") +
  geom_chord(aes(fill = r)) + geom_node_text() +
  facet_wrap(~ sample)
b <- ggplot_build(p)
length(unique(b$layout$layout$PANEL)) == n_samples   # 每样本一个 panel
length(b$data) == 3                                  # 每层一份数据，不是每 panel 一坨
```

**v0.1 的真实语义（spec §9.1 甲方案）**：分面**切开弦**（每 panel 只画本组链接），
**节点环共享**（每 panel 复制同一套弧）。所以"一次出多个环"成立，但多个环的
**节点顺序相同**——这是设计，不是缺陷；每 panel 独立排序是 v0.2 的开放问题（spec §17）。
README 的 facets 图与 `vignette("from-circlize")` 的 facet 一节都是这张图。

### D2 — BRIG 式整图（场景入口）

**目的**：证明"比对块 + 组学轨道 + 轴"能在同一张图、同一套坐标里出现，且还能分面。

**数据**：2 个基因组的 BLAST 块 + 每基因组的 GC / 深度 / 基因密度三轨。

**验收**：见 `tests/testthat/test-demos.R` 的 D2（三条带两两不相交 + 每条落在自己
band 内 + 轴刻度落在 break 半径上）。README 的 tracks 图就是这张。

> 落地时修出 B18（三条带重叠）与默认 `track_offset` 盖住节点环两个 bug——这张
> demo 图的价值正在这里：它逼出了两条轨道测不出来的问题。

**注意**：**不做 BLAST 文件 IO**。ggchord 已经做完 `read_blast()` / `read_gff3()`，我们兼容它的列名即可（`qaccver` / `saccver` / `qstart` / `qend` / `sstart` / `send` / `pident`）。

### D3 — layout 即数据（可扩展性证明）—— **已落地（2026-10-20）**

**目的**：证明"布局是数据"不是口号，是别人能依赖的接口。

**验收**：`test-demos.R` 的 D3——只用 `layout_nodes()` + 普通 `geom_segment()` +
`coord_circos()`，不调用任何 `geom_node_*` 画出节点环；`ggplot_build()` 通过，
角度范围与包自己的图层一致。README 的 as-data 图与 `vignette("correlation")`
最后一节都是这张。

为它顺手补的口子：`coord_circos(limits = c(0, 2*pi))` 现在接受数值向量（读作角度
范围），否则手搭图层要自己构造 `list(theta = )`。

---

## 5. 明确不做

| 不做 | 理由 |
|---|---|
| BLAST / GFF / FASTA 读取 | ggchord 做得比我们好，兼容它的列名即可 |
| 交互（plotly / ggiraph） | 只保证 `ggplotly()` 不因本包报错 |
| circlize 参数平替 | 比覆盖度必输，且会把 API 拖回 base graphics 心智 |
| 自环、多半径节点（v0.1） | 已在 spec §1.2 声明，别在文档里假装没有 |

---

## 6. 分发与迁移

- **主渠道**：CRAN + pkgdown + ggplot2 extension gallery。Bioconductor 不必须（依赖轻是优势，别自己加重）。
- **迁移入口**：一篇 `circlize → ggcircos` 对照 vignette，这是所有新包真正的转化瓶颈。最小对照：

| circlize | ggcircos |
|---|---|
| `chordDiagram(mat)` | `ggcircos(mat) + geom_chord()` |
| `circos.track(...)` | `ggcircos(tracks = c(...)) + geom_track(...)` |
| `circos.text(...)` | `geom_node_text()` / `geom_track(geom = "text")` |
| `circos.axis(...)` | `geom_track_axis(band = , limits = )` |
| `circos.initialize(factors = )` | `sectors =` |

- **社区**：ggplot2 extension 社区（ggraph / ggchord2 的地盘）> circos 社区（circlize 的地盘）。

---

## 7. 风险（诚实版）

1. **市场小**：这个细分目前月下载三位数，对比 circlize 47k、ggraph 67k。别用"替代 circlize"讲故事，用"circlize 做不到的场景"讲故事。
2. **论文新颖性被部分抢占**：circlizePlus（Frontiers in Genetics, 2025）已经发过"ggplot2 风格写环形图"。若要发方法学论文，主张必须落在 **layout as data + facet + 统一 track 抽象**，并配与 circlize 的定量对比（表达力覆盖、代码行数、运行时），否则第一轮就会被问"与 circlizePlus 何异"。
3. **功能空洞是真实的**：self-loop（ggchord2 已支持）、标签防重叠（ggrepel）、violin / density 轨道，是用户最先问的三件事。可以放 v0.1 之后，但不能在 README 里假装不存在。
4. **ggplot2 4.0 的坐标迁移**：`coord_polar()` 在 4.0 已标 superseded，推荐 `coord_radial()`。当前 `CoordCircos` 继承 `CoordPolar`（3.5 与 4.0 双版本可用），但未来要评估是否改为继承 `CoordRadial`，或双实现按版本分派。

---

## 8. 工程现状核对（2026-10-19）

已修：

- `allocate_slots()` 的槽位宽度分母少一次索引（`nodes$width[ep$node_order] / pmax(tabulate(...))`），端点向量长度 `2*n_links` 与计数向量长度 `n` 靠 R 循环补齐，节点度数不均时**槽位宽度错位**并触发 warning。已改为 `n_slots[ep$node_order]`，并加了不等度数的回归测试（`test-layout.R`）。
- `coord_circos()` 未设置 `r` 字段，导致 polar 背景向 theme 要 `panel.grid.major.`（空名），每次出图都打印 `Theme element ... is missing`。已补 `r = "x"`。
- `geom_track(node = id)` 这个**文档里写着的**用法其实是坏的：`is.character(node)` 在 `enquo()` 之前就把 promise 求值了，报了 `object 'id' not found`（`make check1` 的 examples 因此 ERROR）。改为先捕获再解析，`node = id` / `node = "id"` / `node = col`（变量存字符串）三种写法都通，并加了回归测试。
- `utils::tail()` 未 import（`R CMD check` NOTE），已补 `@importFrom utils tail`。

核对结果：

- `testthat` 122 通过 / 0 失败 / 0 警告。
- `make check1` 只剩 PDF manual 的 1 ERROR + 1 WARNING —— 本机没装 `pdflatex`，属环境缺失（AGENTS.md 已记录同类判据），examples、Rd、tests 均 OK。
- `cor(mtcars)` 弦图与 `notes/figures/target-design.png` 同构；两轨 `x` 区间不相交；1000 条 ribbon 1.22 s。

下一步（按 D1→D2→D3 的顺序做 demo，而不是先补功能）：

1. D1 图 + 断言；
2. D2 图 + 断言（兼容 ggchord 列名）；
3. 迁移 vignette；
4. self-loop / 标签避让再排期。
