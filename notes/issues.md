# ggcircos 审核问题清单（issues）

> 状态：v1，2026-10-19。来源：对 `master` 当前实现（含当日修复）的逐文件审核 + R 4.6.1 / ggplot2 4.0.3 实测。
> 上游文档：[spec.md](spec.md)（做成什么样）、[revival-proposal.md](revival-proposal.md)（为什么重写）、[positioning.md](positioning.md)（凭什么）。
>
> **排序原则**：A 类属于"架构与语法"——它们改的是规则本身，必须先定；B 类才是在规则之上的实现缺陷。
> 每条的 ID 在后续 commit / issue 里直接引用，不要改号。

---

## 0. 一句话

骨架成立（layout-as-data、1 层 1 grob、facet 可用、`.rds` 往返可用），但**三条规则没有钉死**：
布局的几何定义（槽位与 ribbon 的关系）、坐标系的语义（`clockwise` / 屏幕角）、图层与 layout 的边界（谁能写 layout）。
这三条不定，后面的修复都是"跟着错的标准修"。

---

## A 类：架构与语法（先定规则，再动代码）

### A1. 坐标层应迁到 `coord_radial()`（ggplot2 4.0）

**现状**：`CoordCircos` 继承 `CoordPolar`（`R/coord-circos.R:75`）。ggplot2 4.0 已把 `coord_polar()` 标为 superseded，官方替代是 `coord_radial()`。

**实测差异**（4.0.3）：

| | `coord_polar()`（当前） | `coord_radial()`（4.0 推荐） |
|---|---|---|
| 内外半径 | 内圈固定 0.4 | `inner_radius` 可调，默认内圈 0.4 |
| 半径轴 | 无内建 | `r.axis.inside` / `r.axis` |
| 角度轴 | 无 `axis.text.theta` | 有 `axis.text.theta` / `axis.line.theta` 等整套元素 |
| 部分圆弧 | 不支持 | `arc = c(start, end)`，配合 `polar_bbox()` |
| 方向 | `direction`（1 / -1） | `reverse`（`"theta"` / `"thetar"` / `"r"` / `"none"`） |
| 未来 | 可能被移除 | 官方维护 |

**要改的**：`CoordCircos <- ggproto("CoordCircos", CoordRadial, ...)`；`coord_circos()` 的参数按 4.0 命名重做（`direction` → `reverse` 语义、`start` 保留、`clip` 保留）。
**连带影响**：`r.axis = FALSE` 的实现方式、`empty_axis()` 是否还需要、`geom_node_text` 的屏幕角来源（§A2）全部要跟着改。
**测试**：CI 矩阵必须加 `ggplot2 4.0.*`（spec §14.5 已要求，目前只在 4.0.3 跑过）。若同时支持 3.5，需要按版本分派（`utils::packageVersion("ggplot2") >= "4.0"`）。

**决策点**：**是否放弃 3.5**。建议放弃——`Imports: ggplot2 (>= 4.0.0)`，只维护一条坐标路径，少一半兼容负担。

### A2. `clockwise` 语义与**屏幕角**的定义

**现状**：`coord_circos(clockwise = TRUE)` 时 θ 增大为**逆时针**（实测：θ = 0 → 屏幕 3 点钟，θ = 90° → 12 点）。文档写的是顺时针。`clockwise = FALSE` 反而顺时针。
**更严重的是**：`circos_screen_angle()`（`R/coord-circos.R:121`）用 `atan2(y - 0.5, x - 0.5)` 由**变换后坐标**反推屏幕角——但当 `y` 的 limits 被用户覆盖、或坐标带 `expand`、或改成 `CoordRadial` 后，这个反推不再与真实几何一致。旧版"标签角度手算"的根因就是这个，当前实现只是换了个地方同样地反推。

**要定的规则（写进 spec §5.4）**：

1. **θ 的定义**：θ 增大方向 = 画布上的旋转方向，只有两个合法值 **`clockwise = TRUE|FALSE`**（不接受 `direction`）。
2. **θ = 0 的位置**：固定 12 点钟（不改），`start` 是"环形图上第一个节点弧的 θ"，不是屏幕偏移。
3. **屏幕角的唯一来源**：不许 `atan2` 反推。由 `coord$transform()` 对 **(r, θ) 与 (r + ε, θ)** 两点分别变换，取两点差向量求角度；或者更稳的做法——坐标对象**导出**一个 `theta_to_screen_angle()` 方法，内部直接用 `panel_params$theta.range` + `arc` 计算，与 `transform()` 共用同一套参数。
4. **左半圈判定**：屏幕角余弦 < 0（沿用），但必须从第 3 条的角度求。

### A3. 槽位与 ribbon 的几何契约（`slot_fill` 到底管什么）

**现状**：`allocate_slots()` 按 `p * node_w / n_links` 算槽位宽（`R/circos-layout.R:542-548`），但 `chord_paths_polar()` 画**满槽**的环脚。`p < 1` 时 ribbon 的脚会盖住槽位间隙，同一节点的槽位脚宽看起来不一致。

**这是 spec 与实现的二选一，必须定**：

- **方案甲（推荐）：槽位是不重叠的区间，ribbon 的脚画满槽位。**
  `p` = 1 时槽位之间**没有**间隙；`p < 1` 时槽位之间留 `(1-p)·slot_w` 的空隙，**ribbon 的脚只覆盖槽位本身**。即"槽位宽 = p·slot_w"，ribbon 按槽位边界画，不是按满宽。
  好处：槽位语义唯一（一个 slot 就是一段不重叠的角区间），后续自环、多半径节点都靠它扩展。
  代价：`chord_paths_polar` 要用 `theta1 - theta0` 的真实宽度画弧脚（本来就是），但要**把 `theta0/theta1` 定义成"槽位的起止"而不是"槽位中子区间的起止"**——现在两者混在一起。

- **方案乙：`p` 只影响节点弧的分配，不影响 ribbon 尺寸。**
  那样 `p` 就该改名（比如 `node_fill`），并且说明"弦是满槽的"。

**建议甲**。定完后要写一条不变量测试：`for each link: theta1 - theta0 == p * node_w / n_links`（当前实测满足），且相邻槽位之间 `theta0(next) - theta1(this) == (1-p) * slot_w`（当前不满足/不检查）。

### A4. 图层与 layout 的边界：谁能写 layout

**现状**：
- spec §8 说 `ggplot_add` 只读 layout；
- 实际 `ggplot_add.ggcircos_track` 会 `object$position$node_table <- layout_node_table(lay)`（`R/layer-add.R:277`），把 layout 塞进 position 的**可变状态**；
- `layout_coords()` 返回裸 list，外部能直接改（`l$nodes$theta <- 0` 成功）。

**要定的规则**：

1. **layout 是不可变值对象**。所有访问器返回副本；`$nodes` / `$links` 不再对外暴露（改成 `layout_nodes()` 等访问器）。R 做不到真正冻结，但可以：(a) 在访问器里 `structure(..., .lock = TRUE)` 并在内部写入口检查；(b) 至少在文档和测试里断言"不变量自检在每次公开访问时重跑"。
2. **layer 只能持有从 layout 派生出的**只读快照（`node_table` 是副本，不是引用）。
3. **position 不得持有 layout 本身**，只持有"band + node→theta 映射"这两张**普通向量**；这样 `position_track()` 也能脱离 layout 单独用（spec §6.4 的"手工逃生口"）。
4. 契约测试扩展为：加层前后 `plot$data` / `plot$scales` / `plot$mapping` / `plot$coordinates` 全 `identical()`；两个层共用同一个 `position_track()` 对象时互不污染（新测试）。

### A5. "一张图一个 layout"在 facet 下的语义

**实测（3 个 facet 变量值）**：facet 能跑，但每个 panel 都画出**全部 18 个节点**（`geom_node_text` 在三个 panel 各 18 行），弦表按 facet 变量切开（每 panel 只画自己那部分弦）。也就是说**分面 = 同一张环的弦被切片，节点环被完整复制到每个 panel**——这不是"每个 panel 一个独立的环"。

**这不是 bug，是定位 §4 D1 的验收标准与实现脱节**。D1 写的是"每样本一个独立的环"，实现给的是"每样本一份弦 + 一圈全样本节点"。必须定：

- **方案甲（当前行为）**：facet 切的是**弦**，节点环共用。适合"同一批节点、不同样本的连线对比"。D1 的断言要改成"同一环、不同弦"。
- **方案乙（定位里写的）**：facet 按变量**重建 layout**，每个 panel 有自己的节点子集。需要在 `ggplot_add` 里检测 `plot$facet`，并把 facet 变量提升到**节点表**（否则每个 panel 不知道自己的节点子集）。牵动 `circos_layout()` 的输入契约——**A 类里影响最大的一条，要先定**。

**建议**：甲是 v0.1 的现实，乙是 v0.2 的目标。把甲写进 spec §7 的"必须能组合"表并如实描述，把乙写进 §17 开放问题，**不要**在 README 用乙的说法宣传。

### A6. `position_track()` 无法作为一层的外部参数传入

实测 `geom_track(..., position = pos)` 被当作未知参数忽略（`Ignoring unknown parameters: position`），因为 `geom_track()` 的签名里 `position` 被 `...` 吃掉后并没有传给 `layer()`——它内部**总是**新建一个 `position_track()`。

后果：spec §6.4 的"手工逃生口"（把同一个 position 挂在多个 `geom_*` 上复用坐标映射）**对 `geom_track()` 本身不成立**，只对用户自己的 `ggplot2::geom_*` 成立。这是语法层面的不一致，属于 A 类。

**要定**：`geom_track(position = ...)` 是否允许用户覆盖内部 position。若允许，`position_track(band = ...)` 的 `node_table` 从哪来（用户自己传 `layout_nodes()`？）。建议允许并文档化这条路径，否则 spec 里的"逃生口"只是一半。

### A7. 废弃 shim 的验收标准与 spec 不一致

**现状**：spec §11 要求"旧函数能跑、给 warning、**出图与新版一致**（`vdiffr` 同一快照）"。实测 `geom_circos(pwidth = 0.3, offset = 0.02)` 的轨道落在 `1.041..1.28`，而等价的 `geom_track(band = c(1.02, 1.24))` 落在 `1.034..1.24`——**不一致**，因为 shim 里 `band <- pwidth + offset` 的语义与 spec 附录 C 写的不符。

**要定**：shim 的验收是"能跑不报错"还是"出图同快照"。建议：**v0.1 只保证能跑 + warning + 映射表写清楚**，不做 vdiffr 同快照（旧版本身画得就不对，要求"一致"等于要求"一致地错"）。

---

## B 类：实现缺陷（在 A 类规则定完之后修）

### B1. `geom_sector_label()` 在 sector 跨 θ=0 时画在圆对面

`circos_sectors_table()` 用 `min/max` 求扇区跨度（`R/circos-layout.R:486-497`），弧度是环形的，跨 0 就塌。

```r
lay <- circos_layout(links, nodes, sectors = "sect", start = 3 * pi / 2)
layout_sectors(lay)   # A: theta0 0.419, theta1 5.644 —— 实际 A 跨 0 占 0.419..5.707
```

A 的标签会落进 B 的扇区。**修法**：只取「同 sector 相邻且未被 gap 隔开」的节点弧，按环形连续段求起止，再判断跨 0。属于 A3 的几何契约的一部分，应该并到 A3 一起定。

### B2. 空 `data.frame`（0 条链接）直接崩

```r
circos_layout(data.frame(from = character(0), to = character(0)))
#> Error in `$<-.data.frame`: replacement has 1 row, data has 0
```

spec §7 说"空图合法"。`allocate_slots()` 会对 0 行表写入标量列。修法与 A4 一起做（layout 的构造要允许 0 边）。

### B3. `gap < 0` / `sector_gap < 0` 不校验

`circos_layout(cor(mtcars), gap = -0.1)` 被接受，节点弧互相重叠，不变量 `n*(w+g) == 2π` 恰好成立，绕过了自检。`stopifnot` 只查了 `slot_fill`（`R/circos-layout.R:226-232`）。

### B4. `gap` 默认值在节点多时直接爆

`gap = 0.01` 在 120 个节点时 `"Gap too large"`。默认参数量产矩阵会随机失败。
**要定**：`gap` 默认改成自适应（如 `gap = min(0.01, 0.2 / n)`），还是保持字面值但文档写明"节点多时手动调小"。建议自适应——这是"好用"的必要条件。

### B5. NA sector 触发裸 `stopifnot`

`circos_assign_sectors()` 对列名分支直接 `as.character(nd[[sectors]])`（`R/circos-layout.R:426`），NA 传到不变量自检才炸，用户看到 `in_turn(nd$theta) is not TRUE` 而不是 classed error。spec §4.5 要求所有错误走 `ggcircos_error_<kind>`。

### B6. `geom_node_arc()` 的 `linewidth` 参数被忽略

传入的参数进了 `params`，但 `GeomNodeArc$default_aes` 自己写死 `linewidth = 0.2`，`required_aes` 又不含它。

### B7. `geom_track(tile)` 不可用 —— **已修（2026-10-20）**

两层原因：
1. `track_angles()` 把 tile 的角跨度写进 `data$width`/`height`，但这两个是
   `GeomTile` 的**美学**，`default_aes`（= 1 弧度）在 `compute_aesthetics` 阶段把数据列覆盖掉；
2. `GeomTile$setup_data` 又用 `width`/`height` 美学反推 `xmin/xmax/ymin/ymax`，把一切打回原形。

修法：tile 走 rect 通道——`track_angles()` 按"同节点的行等分该节点弧"算 `ymin/ymax`（bin），
`position_track(band_fill = TRUE)` 把 `xmin/xmax` 填成带边界，`GeomTrackTile$setup_data`
透传不改。回归测试：`test-tracks-and-contract.R` 的 `"tile tracks split each node arc"`。

### B8. `geom_track_axis()` 的标签全压在 `theta0[1]` —— **已修（2026-10-20）**

三层原因，比报告里写的深：
1. tick 端点用 `band[1]/band[2]`，**完全没读 `br[i]`**——刻度根本没标在断点上；
2. `kind` 列不是美学，ggplot2 在 `draw_panel` 前把它丢了，`data$kind %||% "ring"`
   的 fallback 让**每一行都成了 ring**（标签也当线画）；
3. 最致命：`pathGrob` 的默认 `fill` 是**白色**——两个环路径各填出一个白色圆盘，
   把轴下面的所有图层整个刷掉。所以"轴一上，图就空"。

修法：刻度画在每个 break 自己的半径上（径向尺），标签放各自半径、切向排；
`kind` 换成"label 非空 = 文本行"；`gpar(fill = NA)`；`id` 必须是数值
（`match(group, unique(group))`，字符 id 会被 pathGrob 忽略、整层连成一团）。
回归测试：`"the axis draws lines, not white blobs"`。

### B9. `theme_circos()` 没设 `panel.background`

`bgcolor` 只设了 `plot.background`，示例图全是 ggplot2 默认灰底。默认值写着 `"white"` 但看着是灰的。

### B10. `coord_circos(theta = "x")` 没有测试

唯一合法的路径是 `"y"`，非法的分支有 `ggcircos_abort`，但没有任何测试覆盖"非法 theta 报 classed error"。

### B11. 连续 `fill` 时 grob 数退化成每行一个 —— **已修（2026-10-20）**

`draw_panel` 按 `fill/colour/linewidth/linetype/alpha` 逐行分块（`R/geom-chord.R`）。离散 fill 是 11 个 chunk（好），连续 fill（`aes(fill = r)` 是最常见用法）是 **51 个 pathGrob**——正好是 spec §6.1 要消灭的旧实现问题。

修法（ponytail）：分块数 > 24 时把 `fill`/`colour` 量化到 16 级 rgb 网格再分块，
每块用首行的**精确**颜色绘制（误差 ≤ 半格 = 8/255，肉眼不可分）。
实测：55 条边 51 → 18 个 grob；几何 diff 为 0；离散/无 fill/line 路径不受影响。
回归测试：`"continuous fill does not explode into one grob per link"`。

### B12. `geom_track(text)` 的 `x` 语义没定义 —— **已修（2026-10-20）**

无 `x` 时固定在带中心（正确），有 `x` 时文字放在映射后的位置，但没有文档说明。
不会崩，但会让人困惑。修法就是文档化：`@param geom` 写明"后三种无 `x` 落在带中心，
有 `x` 落在缩放后的值上（同柱的外沿）"。

### B13. `geom_node_text(rotate = )` 整条路径是哑的 —— **已修（2026-10-19）**

`rotate` 在 `layer$ggcircos`（tag）里，而 `layer-add.R` 的 `geometry_params` 把它列进
"不许进 ggplot2"的丢弃名单，`re_add()` 一过 `drop_geometry_params()` 就没了；
`GeomNodeText$draw_panel(rotate = FALSE)` 于是永远收到默认值。
**所有标签一直是水平的**——`geom_node_text(rotate = TRUE)` 和 `FALSE` 出图一模一样
（我拿文件大小和 SVG 内容逐字节对比过）。

修法：`rotate` / `nudge_x` / `angle` 从 `geometry_params` 移出（spec §8.2 白名单例外），
由 `ggplot_add.ggcircos_node` 显式写进 `object$geom_params`。
规则已落 spec §6.2.1；回归测试见 `test-tracks-and-contract.R` 的
`"rotate reaches the geom"`。**这条之前没被审核发现，是因为没有断言
`ggplot_build()` 后 `angle` 列的取值。**

### B14. `CoordRadial` 的 polar grid 元素在 4.0.3 里不存在 —— **已修（2026-10-19）**

A1 迁 `coord_radial()` 时 spec 写的"`grid = TRUE` 打开 `panel.grid.major.theta/.r`"不成立：
4.0.3 的 element hierarchy 里没有这两个名字（它们是 `CoordRadial$render_bg()` 内部拼的），
赋值只会触发 `The ... element is not defined in the element hierarchy` 的 warning。
已改为只操作真实存在的 `axis.text/ticks/line.theta`。规则落 spec §10。

### B15. `rotate_angle = TRUE` 让 ggplot2 自己再翻一次角度 —— **已修（2026-10-19）**

迁到 `CoordRadial` 后，我在构造里设了 `rotate_angle = TRUE`（照抄 `coord_radial()` 的意图）。
但 4.0 的 `CoordRadial$transform()` 里有一句：`rotate_angle = TRUE` 时对带 `angle` 列的
数据调用 `flip_data_text_angle()`——**它会把我们算好的屏幕角再翻一次**。
结果：`geom_node_text(rotate = TRUE)` 渲染出水平标签，`rotate = FALSE` 渲染出旋转标签，
整条 `rotate` 路径在白盒层面是活的、在画面上是反的（比 B13 更难发现：`geom_params$rotate`
值是对的，`circos_label_angle()` 的返回值也是对的）。

修法：`CoordCircos` 构造时 `rotate_angle = FALSE`——我们自己 §5.4 算角度，不需要 ggplot2 再插一手。
**教训**：坐标层与 geom 层的角度计算必须只有一个主人；`rotate_angle` 是 ggplot2 的同一个
扩展点，设成 TRUE 就等于把 §5.4 的"唯一来源"原则违反了一半。
回归测试：`test-tracks-and-contract.R` 的 `"rotate reaches the geom"` 直接断言
grob 的 `rot`：`TRUE` 下不全为 0、`FALSE` 下全为 0——这就是它抓到的。

### B16. 槽位按**总度数**划分，而非按侧 —— **已修（2026-10-19，M1.5-3）**

`allocate_slots()` 把 `from`/`to` 两端 `rbind` 成一张端点表，再用
`tabulate(ep$node_order)` 计数——这个计数是**两侧之和**。于是节点弧被分成
`总度数` 份，每一侧只占其中自己那几份，**剩下的份额被浪费**：一个 3 出 1 入的
节点，出边只用到弧的 3/4，弧的 1/4 空着，ribbon 脚挤在一起。

spec §4.6 写的"两端独立分配"（以及 §4.3-6"入射边按…排序，等分节点弧"）指的是**按侧**
划分：`s = node_w / 该侧边数`。`cor(mtcars)` 每个节点 5 出 5 入（总 10），两侧都是 5，
恰好相等，所以这个 bug 在最常用的测试数据上**看不出来**——直到写 §4.6 的不变量测试
（`test-layout.R` 的 `"the slot geometry contract holds for every p"`）才暴露：
非对称图里 `slot_w` 与 `p·node_w/该侧边数` 对不上。

修法：`side_order <- ep$node_order + ifelse(side=="to", n, 0L)`，按 `2n` 个桶分别计数，
`slot_w = node_w / n_slots[side_order]`。回归测试即上面那条。

### B17. `ggcircos(start = )` 是死参数 —— **已修（2026-10-19，M1.5-2）**

`ggcircos()` 把 `start` 传给了 `circos_layout()`（layout 的 θ 用对了），但装坐标时
`coord_circos()` 用的是它自己的默认 `start = -pi/2`，**没接图层的 `start`**。
结果：`ggcircos(m, start = 3*pi/2)` 的节点在数据里排到 4.71 起，画出来却仍从 12 点钟
开始——`start` 改了 layout 没改屏幕，等于白给。

修法：`ggcircos()` 里 `coord_circos(start = start)`。这条改动让 8 张快照的起点位置变了
（`hclust` 首节点从 12 点移到 9 点），是**修正**不是回归，已 `snapshot_accept`。
**教训**：参数在两个地方（layout 与 coord）各存一份默认值时，最容易"传了一半"。
`start` 的屏幕落点现在由 `coord_circos` 的 `arc` 唯一决定，与 layout 的 `start` 同源。

### B18. 三条以上轨道的 band 互相重叠 —— **已修（2026-10-20）**

`circos_track_bands()` 用 `c(0, head(tracks, -1) + track_offset)` 算每条带的起点，
只加了**上一条**的宽度而不是**累计**宽度。两条轨道恰好正确，三条时第三条直接落在
第二条上（`tracks = c(gc=.12, depth=.12, gene=.12)` → depth 与 gene 都是 1.24–1.36）。

为什么拖到现在：默认 `track_offset = 0.02` 让第一条带从 1.02 起，**盖住节点环
（1.01–1.05）**，灰轨道叠灰环谁也看不见谁——D2 demo 渲染出来"轨道是空的"才把这层
挖出来。修了两处：
1. `r0 = radius + track_offset + cumsum(width + offset)` 去掉最后一项（正确累计）；
2. 默认 `track_offset` 0.02 → **0.06**，不变量 `1 + track_offset ≥ 节点环外沿 1.05`
   写进 spec §4.4 与 `@param track_offset`。

回归测试：`test-demos.R` 的 D2（三条带两两不相交 + 每条落在自己 band 内）。
3 张快照随之外移（stacked-tracks / track-axis / deprecated-wrappers），已目检 + accept。

### B19. 图对象输入丢边属性，tidygraph 端点不解析 —— **已修（2026-10-20）**

`graph_to_input()` 只保留 `weight`（改名 `r`），其它边属性（`w`、`pident`…）全丢，
`geom_chord(aes(fill = w))` 报 "object 'w' not found"。
`circos_layout.tbl_graph()` 更糟：tidygraph 的边表用**1 基整数索引**存端点，
直接 `as.character(edges[[1]])` 得到 "1","2"，与节点名对不上，layout 报
"order must list every node"。

修法：边属性按原名列进 links（`weight` 仍映射为 `r`）；tbl_graph 端点先过
`ids[edges[[1]]]` 解析成节点名。回归测试：`"graph inputs keep their edge attributes"`。

### B20. 扇区与 `order` 同用 → 弧重叠、整圈溢出 —— **已修（2026-10-20，审核 R1）**

`sectors =` 声明后，`order`（`"hclust"` 最常见，显式向量也会）把同一扇区的节点打散，
但预算按 `S-1` 个扇区缝算、`offset` 按**实际切换次数**加缝：

```r
circos_layout(lk, nodes = nodes, sectors = "genome", order = "hclust")
# sectors: B,A,A,B,A,B,A,B → 切换 6 次，预算 1 次
# 4w + 3g + 3sg = 6.849 > 2π = 6.283   → 弧重叠 0.57 弧度
```

扇区标签随之落到别的扇区节点上（`notes/figures/review-sector-overflow.png`）。
`order = "hclust"` + `sectors =` 是 README/vignette 都会用的自然组合。

修法：`circos_group_sectors()` 在排序之后按扇区首次出现顺序做一次**稳定**分组——
`order` 仍只决定扇区**内部**的顺序。回归测试：`test-layout.R` 的
`"sectors stay contiguous whatever the order says (R1)"`（三种 order × 弧+缝=2π 断言）。

### B21. 标签被顶到画布边缘 / 被第一条带埋住 —— **已修（2026-10-20，审核 R15）**

`ggcircos()` 的 `xmax = max(tracks$r1, 1) + 0.06`，而 `geom_node_text(offset = 0.06)`
正好把锚点放在 `r + 0.06`：

- 无轨道：`xmax = 1.06`，标签锚点也是 1.06 → 贴在面板边缘，环几乎顶满画布；
- 有轨道：轨道内沿也是 `1 + track_offset = 1.06` → 标签被第一条带盖住。

修法三件：`label_space = 0.22`（真正的环外留白）、标签锚点改为
`max(tracks$r1, 1) + offset`（`ggplot_add.ggcircos_node` 加 `push`，`geom_node_arc` 不加）、
`sector_label` 默认 offset 0.16 以避开节点文字。回归测试：
`"labels sit outside the outermost band and inside the panel (R15)"`。

顺带修了左半圈判据（审核 R6）：屏幕角以 12 点为 0，`left` 应为 `sin(screen) < 0`；
原来用 `cos`，翻的是**下半圈**，9 点附近的标签会朝环内对齐。

### B22. 线性（arc）布局的弦被画到面板外，只剩半截 —— **已修（2026-10-20，审核 R16）**

`chord_paths_cartesian()` 与 polar 版共用"最短弧"差值 `ang_diff() = wrap_pi(b - a)`。
在极坐标里 `y` 是角度，绕回由 `sin/cos` 消化；在**线性**坐标里 `y` 就是画布坐标，
最短弧一旦跨过 θ=0，端点就落到面板外：

```r
# n1..n6 六节点，n_i -> n_{i+3}（半圈）
# 路径 y 实测: -2.65 / 7.82 / 8.87 ...  面板只有 [0, 2π]
```

于是每条弦只画出半边、在画布上下缘被切掉——`notes/figures` 里那张 arc 快照就是这状态。
修法：线性版改用**不绕回**的差值（`b0 - a1`、`t1 - t0`），两端天然落在 `[0, 2π)`；
鼓包深度限在 `-4/3`（半圆深度），超过半圈的长弦不再冲出 x 范围。
代价（已写进 `shortcut:` 注释）：`|d| > 3π/4` 的长弦深度相同，在左缘重叠成束。

回归测试：`"the linear layout keeps every chord inside the panel (R16)"`。

**同一处的第二个问题（审核 R17）**：修掉出界后图**仍然不对**——每条弦被拉成细长的 U 形。
根因不在路径而在坐标：`coord_arc()` 用 `CoordCartesian`，x、y 单位长度不等，
面板里 1 个半径单位 ≈ 4.8 个弧度的像素（y 轴跨整圈，x 轴只有约 1.3），
所以"深 1.0、跨 π"的弧在屏幕上是"宽 0.76W、高 0.5W"的胖 U。

修法两条（缺一不可，已写进 spec §5.2）：
1. `CoordArc` 改继承 `CoordFixed`、`ratio = 1`——半径单位必须等于角度单位；
2. `bulge_cart()` 的深度改按跨度定：跨整圈的弦鼓 0.8 个半径单位、短弦按比例
   （直线布局没有"几何决定的"深度，这是个设计常量）。

代价：角度轴占整圈 + 1:1 单位 → 线性布局要**偏高的画布**（约 1:2 或更窄）；
方形画布下 panel 会是一条窄柱。已写进 `coord_arc()` 的 `@details`。
回归测试：`"the linear layout draws arcs at 1:1 units, not stretched loops (R17)"`。

### B23. 同侧槽位索引错位，两条边可能占同一个槽 —— **已修（2026-10-20）**

`ep$k <- sequence(n_slots)[rank(side_order, ties.method = "first")]`，而
`n_slots <- pmax(tabulate(...), 1L)` 给**没有端点的侧**也留了一个位置。
`sequence()` 因此比实际端点多出若干元素，`k` 出现缺号与重复：

```r
# cor(mtcars)，mpg 的 from 侧有 10 条边
k 落在 1..9 且有重复  ->  两条边共用同一个槽位，另一段弧空着
```

修法：`k` 改用按侧的行内计数——`before`（累计偏移）直接按行累加，
`k` 只是同一件事的视图。修完后 `theta0` 精确等于
`theta0 + gap/2 + (k-1)(slot+gap)`（实测误差 8.9e-16）。

**回归测试**：`"slots inside a node are placed in order and never collide (R18)"`
（cor(mtcars) + 星形图 + 三角环三种数据，逐侧逐节点断言位置与不重复）。
7 张快照随之更新——旧基线里存的是错位后的位置。

### B24. 缺少"按权重画"的几何（对比 circlize 暴露） —— **已加（2026-10-20）**

和 `circlize::chordDiagram()` 画同一份 `cor(mtcars)`：circlize 的**节点弧 ∝ Σ|r|**、
**连边宽 ∝ |r|**；ggcircos 是**每个节点等弧**、**每条边等宽**，值只体现为颜色。
信息量差一档——左边一眼看出 cyl/disp/mpg 是主成分，右边所有变量平等。

修法：新增 `ggcircos(..., weight = )` / `circos_layout(..., weight = )`：

- `weight = NULL`（默认）：仍是等弧等宽，与 v0.1 逐位相同；
- `weight = "r"`（矩阵自带该列）：节点弧 ∝ Σ|w|、槽位 ∝ |w|，负值按绝对值；
- 权重列不存在 / 非数值 / 含 NA / 全零 → `ggcircos_error_data`。

回归测试：`"weight makes the geometry follow the values (R18)"`。
尚存差异（不影响语义）：标签朝向（circlize 默认切向，我们水平）、
以及我们的圆盘因为要给图例留位置而略小。

### B25. 弦在圆心中间"折"回去 —— **已修（2026-10-20）**

`bulge_radius(d)` 用 `k = 1 - (4/3π)|d|`，`|d| > 3π/4` 时 `k < 0`。控制点落在两端的**射线**上，
负半径等于把控制点放到圆心**对面**：`|d| = π` 时控制多边形是 `1 → -1/3 → +1/3 → -1`（非单调），
三次曲线先跑过头再折回来——就是图中央那些捏尖、打结的小折。

`-1/3` 这个数本来是为了"中点落在 r = 0"，但它只在"控制点沿弦"的前提下成立；
沿射线时必然折返。修法：`k` 钳到 `≥ 0`。代价是 `|d| = π` 的中点落在 `r = 0.25` 而不是圆心——
视觉上仍然是穿过中心的细腰，而且**顺带得到了设计图里那个中心白孔**。

回归测试：`"no chord folds over the centre (R19)"`（每条路径的半径不得为负；
显式 `curvature = 1` 同样断言）。spec §6.1 里"`k = -1/3` 过圆心"的说法一并改掉。

顺带修：`geom_chord(curvature = 1)` 会报 `Ignoring empty aesthetic: alpha`——
alpha 默认值原本只在 `curvature = "auto"` 时设置，显式传 curvature 就漏成 `NULL`。

### B26. ribbon 的宽度沿长度收放，边缘会拐 —— **已修（2026-10-20）**

两条边各用**自己**的最短弧 `ang_diff(a1, b0)` / `ang_diff(b1, a0)` 建贝塞尔。
这两个差值**符号可能相反**（实测 `-2.46` 与 `+1.96`），两条边于是朝相反方向绕行，
mid-parameter 处几乎交于一点：

```r
# 一条 ribbon 的角宽度，沿 t 取 40 个点
w(0)=2.41  w(中)=0.29  w(1)=2.00   # 中段收成 1/8
```

图上就是"带子两头宽、中间捏细"，边缘看起来在拐弯。
前面还试过给两边**共用**一个 `k`——深度对齐了，但参数化不同，仍然收窄。

修法：**一条中心线 + 沿线的半宽**。中心线走两个脚中点 `ca → cb`（最短弧，方向唯一），
半宽按 `|a1-a0|/2 → |b1-b0|/2` 在 t 上线性插值，上下两条边 = 中心线 ± 半宽。
这样两条边天然同向、端点精确落在脚上、中段平滑收窄。

回归测试：`"a ribbon keeps its width and lands on both feet (R20)"` 与
`"both edges of a ribbon are the same curve entered from opposite ends (R20)"`。
7 张快照重播（形状确实变了）。

### B27. 旋转标签镜像 / 轴刻度竖成一列 —— **已修（2026-10-20，审核 R2）**

两层原因：

1. `screen_angle()` 里对 `reverse == "theta"` **又翻了一次 `u`**，而 `panel_params$arc`
   已经是递减的 `c(4.712, -1.571)`。两次反转互相抵消，逆时针圈报出和顺时针一样的角度
   ——`rotate = TRUE` 的标签于是镜像。
2. 返回的是**径向角**（点从圆心指向外的方向），但代码把它**直接当 `rot` 用**。
   grid 的文字角度从 +x 起逆时针，径向角从 12 点起顺时针，两者差 90°：
   文字因此横跨弧线（节点标签）或竖成一列（轴刻度）。

修法：`screen_angle()` 明确返回径向角、只做一次方向映射；
**文字旋转角 = 径向角 − 90°**，再折到 (−90, 90] 防倒立。节点标签与 `geom_track_axis()`
共用这条规则。

回归测试（spec §5.4-5 要求、一直没写的那条）：
`"screen_angle is the radial direction of the point"`——对 `clockwise × start` 的每种组合，
断言 `screen_angle()` 与 `transform()` 出来的径向方向逐点一致（容差 1e-6）；
外加 `"rotated labels run along the ring, upright on both halves"`。

### B28. 相邻 ribbon 糊在一起，缺可调的缝 —— **已加 `slot_gap`（2026-10-20）**

和 circlize 并排看，我们的相邻 ribbon 在脚部糊成一片。原因不是没有缝，
而是缝**按比例**算：`gap = (1 - slot_fill) · node_w / d_side`。节点弧宽、链接多时，
0.9 的比例只剩 0.005 rad，屏幕上不到一个像素。

circlize 用的是 `diffHeight = 2mm`（**固定物理高度**），所以窄弧上也留得出缝。

新增 `slot_gap`（`ggcircos()` / `circos_layout()` 同名参数，默认 `NULL` 保持原行为）：
给定数值时缝是**固定弧度**，上限 `node_w / (4·d_side)`（最多四分之一弧留给缝）；
槽位吃掉剩余弧。配合 `geom_chord(..., colour = "white", linewidth = 0.15)` 的细白描边，
相邻带子就能逐条分辨。

回归测试：`"slot_gap gives every seam the same absolute width (R21)"`。
**默认值没动**：把 `colour` 默认改成白色会让 `type = "line"` 的曲线也变白（同一美学），
9 个测试报错，评估后放弃；描边仍需用户显式指定。

### B29. 单扇区时 `sector_span()` 把整段旋转一格 —— **已修（2026-10-20，审核 R5）**

`sector_span()` 用 `which.max(ahead)` 找"扇区不连续处"。**只有一个扇区时没有扇区缝**，
所有间隙相等，`which.max` 取到中间那个普通节点缝，整段旋转一格：

```r
circos_layout(data.frame(from="a1", to="a2"), nodes=data.frame(id=c("a1","a2")), start = 5.8)
# 节点:  a1 5.800→2.596,  a2 2.658→5.737
# 修前:  sector theta0 = 2.658（a2 的起点）theta1 = 2.596   -> 中点落在扇区外
# 修后:  sector theta0 = 5.800（a1 的起点）theta1 = 5.737   -> 正确
```

修法：所有间隙在 1e-12 内相等时视为"无扇区缝"，取 `k = n`（不旋转）。

回归测试：`"a single sector's span starts at its first node (R5)"`（三个 start 值 + 双扇区对照）。

### B30. 轴刻度标签竖成一列、互相叠压 —— **已修（2026-10-20，审核 R14）**

两个原因叠加：
1. 标签用了径向角当旋转角（R2 的连带），每个变成**竖排的数字列**；
2. 半径位置是 `br + 2 * tick_length`——三个断点挤在同一小段半径上。

修法：标签**保持水平**（沿半径堆叠时旋转毫无意义，circlize 的轴也是水平读），
且各自贴着自己的刻度（`br + 1.4 * tick_length`）。现在三个值沿半径均匀散开。

回归测试并入 `test-tracks-and-contract.R` 的轴测试。

### B31. `stack = TRUE` 吞负值 + `value` 无法送达 position —— **已修（2026-10-20，审核 R4）**

两层原因：

1. `track_angles()` 写 `data$x <- 0` 当占位符。**data 里叫 `x` 的列会盖过 `aes(x = v)`**，
   position 收到的 `x` 恒为 0，`stack` 的负值守卫（写在 `track_map_values()` 里、本身是对的）
   永远看不到负值。
2. 想用 `geom_track(value = v)` 绕开，也走不通：**ggplot2 只把 aesthetics 传给 position**，
   额外的数据列根本到不了 `compute_panel()`；而 `GeomRect$setup_data` 会从 `xmin/xmax`
   反推 `x`，进一步把它变成常量。

修法（不再依赖 aesthetics 传值）：
- `geom_track(value = )` 把值列**存到 position 对象上**（`position$values`，普通数值向量，
  符合 spec §8.2"position 只持向量"），`PositionTrack$compute_panel()` 直接用它；
- `x` 仍要有映射（GeomRect 需要），但映射到一个**不叫 `x` 的列**；用户的数据列真叫 `x` 时，
  先改名为 `.circos_value` 再映射；
- `track_angles()` 用保留在 layer data 里的 `.circos_has_value` 列判断是否需要占位符，
  不再依赖会在后续步骤丢失的 data-frame attribute；
- `ggcircos()` 的 `scale_x_continuous()` 改用 `scales::oob_keep`，避免 track 原始值在进入
  `PositionTrack` 之前被 plot 的半径上限先行 squish。

结果：`value =` 与 `aes(x = )` 产出完全相同的几何；负值 `stack` 报
`ggcircos_error_data`（"stack = TRUE needs non-negative values, found -1"）；
数据里存在 `x` 列时不再被吞。
回归测试：`"geom_track(value = ) reaches the position and guards stacking (R4)"`（四条断言）。

### B32. `scales = "free"` 被全局归一化而非按节点 —— **已修（2026-10-20，审核 R3）**

position 收到的 ggplot2 数据不再包含原始 `node` 列，原实现因此把所有行当成一个组；节点间量级相差较大时，小量级节点几乎缩成零宽。

修法：在 `ggplot_add()` 阶段将节点 id 与 `track_angles()` 计算出的节点角度作为 position 向量快照；`compute_panel()` 按仍保留的 `y` 角度把行关联回节点，再独立计算每节点范围。这样不依赖已被 ggplot2 丢弃的任意列。

回归测试：`"free track scales are normalised independently per node (R3)"`，两节点分别取值 1–2 和 50–100，断言两组都到达各自 band 外沿。

### B33. sector 只有一处宽缝，两个 sector 读作一整圈 —— **已修（2026-10-20，视觉对照 circlize）**

`circos_layout_build()` 按 `S-1` 处 sector 缝做预算，且 wrap 边界（最后一个节点 → 第一个节点）
被硬编码成普通缝。两个 sector 的图因此只剩**一处**宽缝：两段弧在另一处几乎接上，
整圈看起来是一个环被咬掉一口，而不是两个独立的块。

对照 circlize：`circos.par(gap.after = c(..., 30, ..., 30))` 在**每个 sector 的两端**都留宽缝，
这是它表达"块"的标准做法（截图对比见本轮验证）。

修法：
- 边界改为环形判定：`boundary <- c(sector[-1] != sector[-n], sector[1] != sector[n])`；
- 预算与 `span` 按 `S` 处缝（`S ≤ 1` 时为 0）：`span = 2π - N*G - S*SG`；
- `sector_gap` 的 20% 收窄改为 `0.2 / max(S, 1)`，保证 `S*SG ≤ 0.2`；
- `check_layout_invariants()` 的期望缝同步，wrap 那处也是 `G + SG`。

回归测试：`"a sector is a closed block: both of its ends get a seam (B33)"`
（两处 `G + SG`、其余 `G`、环仍精确闭合）。

### B34. 飘带几何反复修不好 → 改为直接移植 circlize —— **已修（2026-10-21）**

这条记的是**一次失败的过程**，因为反复返工的根因值得记住。

先后试过四种自研几何，每种都"看着差不多"，但都不是 circlize 的形状：

| 尝试 | 做法 | 结果 |
|---|---|---|
| 1 | 单脊线 + **角度**偏移 | 脊线俯冲时角宽不变，物理宽度 0.32 → 0.08，收成针尖 |
| 2 | 单脊线 + **平面**偏移 | 宽度恒定了，但多条带在中心堆叠挤压 |
| 3 | 两条边 `a0→b0` / `a1→b1` | 脚对了，飘带在中心交叉成蝴蝶结 |
| 4 | 两条边 + 残差折叠 | 部分对，55 条里仍有 2 条反向 |

**根因（两条，都是最后才看准的）**：

1. **配对方式**。circlize 的两条边是**交叉配对**：`d1 = (θ11 → θ22)`、
   `d2 = (θ12 → θ21)`，不是我按"同侧角"理解的 `a0→b0` / `a1→b1`。
   用错配对时多边形面积是 5.98（≈ 整个圆盘 2π），正确的应该是 0.24（量级差 25 倍）。
   这个错误很隐蔽：脚的位置、两端半径都对，只有"中间围出来的面积"错。
2. **闭合弧的方向**。`arc.points()` 走的是 `(t2-t1) %% 360`，即**可能绕长路**；
   我一度"修正"成短弧，反而与 circlize 不一致。而脚只需要短弧 ——
   正确的做法是：配对用 circlize 的交叉方式，闭合弧取短向（只覆盖脚自身宽度）。

**最终做法：直接移植，不再自研。**

- `chord_bezier()`：`circlize:::getQuadraticPoints()` 的逐行移植
  （带权二次贝塞尔 + 自适应采样 + 角度旋转），与原件逐点误差 **1.4e-15**；
- `chord_arc()`：`arc.points()` 的移植，但闭合弧取短向；
- `chord_ribbon_outline()`：按 `circos.link()` 的分支做交叉配对，
  多边形 `d1 → arc2 → rev(d2) → arc1`；
- `chord_paths_polar()` 改为调用上述函数，旧的极坐标近似全部删除。

顺带修掉的两个显式缺陷：

- **黑色描边**：`GeomChord$default_aes` 的 `colour = "black"` 对所有 type 生效，
  而 spec §6.1 写明 ribbon 描边默认 `NA`（circlize 的 `link.border = NA`）；
- **节点环白缝**：`geom_node_arc()` 默认 `offset = 0.01` 把环推到 `[1.01,1.05]`，
  与 `r = 1` 的飘带脚之间空出 0.01。改为 `0`。

**这一步的教训**：判断"像不像"不能靠看一眼，要找一个**可量化的判据**。
有效的判据是**多边形面积**（错误配对 5.98 vs 正确 0.24）和
**逐点比对 circlize 原件**（1e-15）。

回归测试：
- `"a ribbon outline is a closed band, ported from circlize (B34)"`
  （闭合、不出环、无整圈尖峰、有面积但不是整盘）；
- `"a ribbon covers a band's worth of area, not the whole disc (B34)"`
  （面积量级：全部 `< 0.5π`，中位数 `< 0.3`）。


---

### B35. 飘带交叉处被挖成白色空洞 —— **已修（2026-10-21）**

两个 sector 那张图中间有一块白，看上去像"重叠的部分变白了"。

根因：`draw_panel()` 为了减少 grob 数量，把**同样式**的多条飘带合并进一个
`grid.path()`（B11 的优化）。`grid.path()` 必须选一个填充规则，两个都不对：

- `"evenodd"`（当时用的）：重叠区被数到**两次**，按偶数挖空 —— 交叉处直接变白；
- `"winding"`：重叠区填了，但**自交**的飘带会被抵消掉。

circlize 是每条 link 单独 `polygon()`，重叠处按 alpha 叠加，所以从来不会有这个洞。

修法：改成**每条 link 一个 grob**，各自的 alpha 独立混合，与 circlize 同语义。
代价是 grob 数量回到 O(link 数)，这对 grid 是常态（`geom_polygon()` 也一样）。

**这一步顺带推翻了 B11 的优化前提**：B11 当年为了"连续填充不炸成一条一 grob"
把多条合并成一条路径 —— 那个优化和正确渲染互斥。原来的测试
`"continuous fill does not explode into one grob per link (B11)"` 断言合并后的
数量上限，现已替换为 `"overlapping ribbons blend instead of punching a white hole (B35)"`
（断言 grob 数 == link 数，即每条 link 独立绘制）。

回归测试：`"overlapping ribbons blend instead of punching a white hole (B35)"`。

### B36. plot 级挂了弦图专用的 mapping，标准 ggplot2 图层叠不上去 —— **已修（2026-10-21）**

`ggcircos()` 过去返回 `ggplot(links, aes(x, y, xend, yend))`。弦图层需要这四列，
但挂在 plot 级，等于要求**所有**图层都继承它们。用户叠一个标准层就撞墙：

```r
ggcircos(m) + geom_point(data = nd, aes(x = r, y = theta))
#> Error in `.data$xend`: Column `xend` not found in `.data`.
```

只能给每一层补一句 `inherit.aes = FALSE`。这是把内部实现细节漏到了用户界面。

试过两种收敛：

| plot 级 mapping | 结果 |
|---|---|
| `x, y, xend, yend`（旧） | `geom_rect`/`geom_point` 等全要 `inherit.aes = FALSE` |
| 只留 `x, y` | `geom_rect` 仍失败（它要 `xmin`/`ymin`，继承了 `x`/`y` 反而找不到列） |
| **全空** | 11 个标准几何层加 `annotate()`，全部直接可叠 |

保留 `x`/`y` 换来的便利是"裸写 `geom_text(aes(label=))` 自动落在节点上"，
但代价是 `geom_rect`/`geom_tile` 这类不映射 `x`/`y` 的层照样要 `inherit.aes`。
全空更彻底，也更符合 ggplot2 的惯例（`ggplot2` 自己的 `ggplot()` 默认不带 mapping）。

`geom_chord()` 不受影响：它在 `ggplot_add()` 里用 `chord_coordinates()` 自己映射
四个坐标加槽位列（§6.1），本来就不依赖 plot 级。

回归测试：`test-demos.R` 的 `"standard ggplot2 layers stack without
inherit.aes = FALSE (D3)"`（断言 `length(plot$mapping) == 0`，并逐个叠
point/text/label/tile/rect/segment/path/violin/annotate 与 rds 往返）。

### B37. 只有 tile 按行切分节点弧，col/area/bar 多行叠成实心带 —— **已修（2026-10-21）**

写公众号示例时发现：给每个节点 3 个值、用 `geom = "col"`，轨道出来是一条等高实心带，
没有起伏。查出来 `track_angles()` 的按行等分弧只写在 `if (identical(geom, "tile"))`
分支里，其余 geom 一律 `ymin/ymax = theta ± arc*width/2`，即**每一行都占满整条节点弧**。
于是同一节点的 3 根柱子完全重叠，只看得见最高的那根；`area` 更没法画覆盖度剖面，
而那恰恰是轨道最典型的用途。

修法：切分弧对所有 geom 生效，只有 `boxplot` 例外（`StatTrackBox` 先把多行聚合成
一个标记，那个标记该占满整条弧）。

**每节点只有一行时 `bin_w = arc * width`，与以 `theta` 为中心逐位相同**（实测误差
4.4e-16），所以最常见的单值轨道不受影响。

回归测试：`"multi-row tracks split the node arc for every non-aggregating geom (B37)"`
（col/area/bar 的 bin 宽 = 弧/25、无缝拼满节点弧、boxplot 保持整弧、单行逐位不变）。
受影响快照：`stacked-tracks`、`track-axis`（都是每节点多行的 col 轨道）。

**教训**：B7 修 tile 时把切分逻辑塞进了 tile 分支，其实那是「一个节点有多行」的性质，
不是 tile 的性质。geom 分支只该用来决定 `band_fill`/聚合这类真差异。

### B38. 外来列名（BLAST / GFF）进不来 —— **已加 `columns =`（2026-10-21）**

定位文档写"兼容 ggchord 的列名即可"，但代码里一行映射都没有：`grep qaccver R/` 零命中。
BLAST 表输出的端点是 `qaccver`/`saccver`，直接喂进去报
`The data.frame is missing required columns from and to`。这是生信用户第一步就撞的墙。

修法：`columns = c(from = "qaccver", to = "saccver")`，一个参数覆盖所有格式，
而不是给 BLAST 写一个方法（GFF3、BED、InterProScan 各有一套拼写）。
映射是副本，没映射的列随 links 表一起走，`weight = pident` 之类照常可用。

回归测试：`"columns = maps a foreign link table onto from/to (B38)"`。

### B39. 轴必须重复写一遍轨道范围 + `scales::squish()` 截断标签 —— **已修（2026-10-21）**

两个问题叠在一起：

1. `geom_track_axis()` 强制要 `limits`，用户已经在 `geom_track()` 里写过
   `limits = c(0, 1)`，轴这里还要再写一遍。两处会漂移。
2. 顺着查标签生成，发现 `format(scales::squish(breaks))` 是**真 bug**：
   `scales::squish()` 的默认 `range = c(0, 1)`，任何大于 1 的断点会被**压成 1.0**。
   `limits = c(0, 2)` 的轴因此读出 `0.0 / 0.5 / 1.0`（末尾三个 1.0 挤在一起），
   数据没错、刻度没错，只有标签错。

修法：
- 轨道层把自己的 `scales`/`limits`/`baseline` 记进 layer tag；
  `ggplot_add.ggcircos_axis()` 在 `limits` 缺省时从同 band 的 `scales = "fixed"`
  轨道层读，读不到报 `ggcircos_error_data`（不画空轴）；显式 `limits` 优先。
- 标签改成 `format(breaks)`。断点按构造就在 `limits` 内，需要格式化不需要截断。

顺带修掉两个空值判断的坑：省略的参数进 tag 是 `numeric(0)` 不是 `NULL`，
`is.null()` 判不出来，得用 `length()`。

回归测试：`"geom_track_axis() takes its range from the track layer (B39)"`
（自动取范围、显式覆盖、>1 的断点标签正确、无轨道时报错）。

### B40. 密集标签必然叠死，`ggrepel` 在环形布局上不适用 —— **已加 `repel =`（2026-10-21）**

120 个 contig 的图，顶部和底部的标签挤成一团，完全读不出来。spec §17-4 当时把
"内建 ggrepel 支持" 列为 v0.1 不做，但生信里 contig 动辄上百，这不是锦上添花。

**先试了 `ggrepel`，不合适**。它不报错，但实测（60 与 120 节点）把标签排成水平直线、
靠引线连回节点，环形读法丢掉了——它在**矩形空间**里解冲突，不理解极坐标。

修法：`geom_node_text(repel = TRUE)`，按角序遍历，相邻两标签在该半径上的**弧距**
`< 1.15 * step` 时后者外移 `step`（默认 0.055）。要点：

- 用**弧距**而不是常数角度：扇形越往外越宽，同样角度差在 r=1 处比 r=1.5 处更挤；
- **角度不变**：角度承载节点身份，标签只能沿自己那条辐条移动；
- 40/60 节点角度本来够宽，不触发（`unique(r) == 1`），120 节点才拉开台阶。

回归测试：`"repel = TRUE staggers only the crowded stretches (B40)"`
（稀疏不动、密集外移、只增不减、角度逐个保持）。

### B41. `violin` 轨道的三个独立坑：聚合标记、位置接管、接缝夹取 —— **已加 `geom = "violin"`（2026-10-22）**

spec §17-7 原把 `violin` / `density` 排在 M2 之后（"按节点算 KDE，成本高一个量级"）。
实测 KDE 本身不是问题（200 节点 × 40 样本、`n = 64`：`ggplot_build()` 0.32 s，
同数据 `col` 轨道 0.06 s，即 KDE + 多边形的净开销约 0.26 s），真正的成本是**几何方向**：
环里的 violin 是横躺的——值沿半径、密度决定该值处占多少弧，和 `geom_violin()` 正好差 90°。
`stat_ydensity` 还要求离散 x 做分组，会撞上 y 轴已是弧度的老问题（同 `StatTrackBox`）。
于是照 boxplot 的模式新写 `StatTrackViolin` + `GeomTrackViolin`（继承 `GeomPolygon`）。
落地过程中撞了三个互不相干的坑，每个都能单独让轨道"画出来但看不见"：

**坑 1：聚合标记只按名字列举，violin 不在名单里。**
`track_angles()` 用 `identical(geom, "boxplot")` 决定"整弧 vs 切分"，
`ggplot_add` 又用 `identical(params$geom_name, "boxplot")` 补 `node` 映射。
加 violin 时两处都得加，漏一处就静默出错（`node` 全是 `-1`，6 个节点折成 1 个多边形）。
修法：`track_geom_spec()` 增加 `aggregate` 字段，两处都读它——
**"这个 geom 会不会把多行折成一行"是 geom 的属性，不是名字。**

**坑 2：密度的角度偏移不能走过 `position`。**
第一版把密度放在 stat 出的 `violinwidth` 列里，想让 `PositionTrack` 换算成角度。
不行：ggplot2 交给 position 的只有**美学**，stat 的额外列到那里已经没了，
position 读 `data$violinwidth` 永远拿到 `NULL`，所有行压在 `theta` 上，图上什么都没有。
修法：偏移必须在 stat 里就写进 `y`，position 只管半径。

**坑 3：接缝节点的 `ymin` 被坐标轴夹掉，半弧宽算错。**
`ymin/ymax` 是 stat 拿半弧宽的来源，但跨 `θ=0` 的节点 `ymin < 0`，
而 y 轴是 `limits = c(0, 2*pi)` + `oob = squish`，**在 stat 之前**就把它夹到 0。
于是 `(ymax - ymin)/2` 得到 0.52 而不是 0.68，跨缝那个 violin 画窄了，
而且闭合轮廓被压成一条贴缝的平边。
修法：`track_angles()` 另写一列 `.circos_half`，由 `ggplot_add` 映射成美学——
**轴不会碰不属于它的美学列**。跨缝时轮廓按 `seam_pieces()` 在缝处切开、
另一半接到 `0` 之后（同一个圆上的同一位置），一个节点两个多边形。

**顺带修掉的两处契约漏洞**（都是读代码发现的，不是猜的）：

- `stack = TRUE` 在聚合型 geom 上被静默接受（`boxplot` 早就有这问题）：
  stat 已经把节点折成一个标记，再堆叠只会画出一个顶到带顶的标记。现在报 `ggcircos_error_data`。
- `PositionTrack` 原先**只**按 `y` 反查节点（`match(data$y, node_angles)`），
  而聚合 stat 已经把 `y` 改成轮廓角，`match()` 全 `NA`，半径退化成原始值
  （0.2–0.8 的值画到半径 0.2–0.8，跑到图中间）。现在节点列优先，角度只作退路。

**R 的 `$` 部分匹配也咬了一次**：`params$n` 在有 `na.rm` 的 list 上会部分匹配到
`params$na.rm`（`FALSE`），带宽参数静默变成 logical。
`setup_params` 里一律用 `params[["n"]]`。

回归测试（`test-tracks-and-contract.R`，五个 `test_that`）：几何方向与整弧占满、
stat→position 交接后角度仍然散开、跨缝切两片且圆周测得的峰值为半弧、无密度时出声、
`stack` 被拒、`n`/`adjust`/`trim` 生效。
三个坑各自单独复现过一次（把修复回退，对应断言必红），确认测试抓的是真因不是巧合。

### B42. `density` 轨道：四个"参数明明传了却没到"的坑 —— **已加 `geom = "density"`（2026-10-22）**

`density` 与 `violin` 共用同一份 KDE（抽成 `node_density()`），差别只在**闭合与否**：
violin 走 `theta ± w·half` 两侧闭上，density 只走 `theta + w·half` 一侧、不闭合，
于是同一带里可以叠多个节点的分布（ridgeline），填充体做不到这件事。

几何本身很顺，卡住的全是 ggplot2 的参数传递。四个坑彼此独立，症状都是"画出来但没变/没画"：

**坑 1：`GeomTrackDensity$draw_panel` 带 `...`，`geom_params` 里的参数全被丢掉。**
`Geom$parameters()` 的实现是：`draw_panel` 若含 `...`，它就转而读 `draw_group()` 的形参；
而 `draw_group` 是继承来的、没有自定形参，于是只认 `na.rm`。
`geom_track(..., floor = TRUE)` 一路传到 `geom_params` 都对，**到 `draw_panel` 前被过滤掉**，
分支永远不执行。修法：`draw_panel` 显式列出 `lineend`/`linejoin`/`linemitre`/`arrow`/`na.rm`
（从 `GeomPath` 抄来），再加 `extra_params` 声明自定参数。**`...` 看着更省事，但它让参数表不可见。**

**坑 2：geom 级参数被 stat 抢走。**
ggplot2 按名字在 stat 与 geom 之间分配 `layer(params=)`，同一个名字优先给 stat；
`baseline` 又恰好已被 `geom_track()` 用作"free 刻度的下界"。
所以自定的 geom 参数不能从 `geom_track(...)` 走，得在 `ggplot_add` 里直接写进
`object$geom_params`——spec §6.2.1 早为 `rotate`/`nudge_x` 留过这个例外，照抄即可。
**注意写入时机必须在 `drop_geometry_params()` 之后**，否则刚写进去就被当"轨道级设置"剥掉。

**坑 3：`after_stat()` 才能映射 stat 产出的列。**
`geom_add` 里的 mapping 是在 stat **之前**求值的，写 `.data$.circos_lo` 会直接报
"data pronoun 里没有这一列"。stat 产出的列必须经 `after_stat()`。

**坑 4：`coord_munch()` 会把两点直线插成弧。**
极坐标下"固定角度、变化半径"的基线是一条**径向直线**，但给 `GeomPath` 会被
`coord_munch()` 按弧长重采样成 4–5 个点，画成弯的。用 `coord$transform()` + `polylineGrob()`
直接画，且**单位要用默认值**（transform 出来已是 0–1 npc，再报 `"native"` 会被二次缩放，
基线只剩 9% 长）。

**另外 `seam_folds()` 的第一版写错了**：我按"最短步长"展开角度序列再切圈，
但密度曲线采样很密（实测相邻步长最大 0.056 rad，半圈是 3.14），**永远不会跳整圈**，
于是展开只会把曲线自身的**下降段**误判成回绕，一次跨缝被切成 3 段、其中一段横跨整圈。
修法：不做展开，每个点落在哪一圈就是哪一圈。

**一个被砍掉的设计**：`floor = TRUE` 曾用来在每个节点中心角画一条径向"地平线"。
几何上连贯（曲线从它升起），但它横穿曲线占据的整个半径，读起来像"轴上多画了一笔"；
而且 `trim = TRUE` 时曲线两端本来就不落在中心线上（实测 gap 0.01–0.09 rad），
硬贴一条线是在制造不存在的关系。**已删除**，需要地平线请叠 `geom = "col"` 或自加
`geom_segment()`；spec §6.4 记了这条决定与理由。

回归测试（`test-tracks-and-contract.R`）：开放曲线 vs 闭合轮廓的行数关系与单侧性、
两个 geom 共用同一 KDE（逐点相等）、跨缝只切不插点且单段不跨整圈、`stack` 被拒、无密度时出声。

### B43. `self_loops = TRUE` 只会报错；自环从未能画 —— **已加小回环（2026-10-22）**

`from == to` 的边在 v0.1 里两条路都是死路：`self_loops = FALSE`（默认）丢弃并提示，
`self_loops = TRUE` 直接 `ggcircos_abort()`。spec §17-1 一直挂着"小回环还是整段弧"这个决策。

**决策**：向内折的小回环（用户 2026-10-22 选定，与 circlize 一致）。整段弧会把自环画成
和自己重合的一整圈，跟节点本身分不开，读不出"这是一条链接"；向内折保持一条边的读法。
契约落在 spec §4.7，实现落在 `self_loop_outline()` / `self_loop_ribbon()`。

**三个实现坑**（每个都能让自环画歪）：

1. **`n_links` 不能算自环，但 slot 分母要算。** 自环不是"入射边"（§4.2），
   可它确确实实占着节点弧上的一段；如果分母里不算它，它那条 slot 会被算成整条弧，
   盖住所有正经链接。所以 `n_links = tabulate(非自环)`，而 `allocate_slots()` 照旧看
   from/to 两侧的所有行。加测试钉住（`a` 只有 1 条真出边，但自环仍分到 slot）。
2. **ribbon 必须用同一对脚。** 自环的 from 侧与 to 侧 slot 宽度不同（两侧按不同边数等分，
   实测 0.68 vs 1.36）。第一版直接把两条不同宽的曲线连起来，band 撑到 0.04–1.43，
   填成一整块扇形。修法：取较窄的 slot 作回环footprint，两条边都用它，
   只差折叠深度——深度不同才让中间形成带。
3. **几何列要跟着 mapping 走。** `self_loop` / `.circos_node_width` / `.circos_loop_depth`
   得进 `chord_coordinates()` 才能到 `draw_panel()`；但不能无条件加，因为显式
   `geom_chord(from=, to=)` 的用户表里没有这些列，无条件映射会让 ggplot2 找不到列而报错。
   所以 `chord_coordinates(data)` 按 frame 有没有这些列决定映不映射，
   `layout_slots()` 则负责在显式路径下补上它们。

**深度公式**：`depth = loop_depth * slot_span / node_width`（夹在 `[0.05, 0.95]`）。
窄节点上的自环折得浅，不会一刀捅到圆心挡住别的弦。`loop_depth ∈ (0, 1]`，否则
`ggcircos_error_layout`。

**顺带发现的既有行为**（未改，记一笔）：矩阵输入只取上三角，所以 `cor(x)` 的对角线
（自环的自然来源）不会变成自环行。要画自环，用 `from`/`to` 表显式给。

回归测试（`test-layout.R`，三个 `test_that`）：自环被保留并标记、`n_links` 排除自环
但 slot 仍占位、回环半径 < 1 且 > 0.05、窄节点折得更浅、ribbon 两条边共用脚且只在自身
slot 内、`loop_depth` 校验与深度随参数变化、两种 `type` 都能渲染。

### B44. facet 乙方案：`by =` 一次出多个独立环 —— **已落地（2026-10-22）**

spec §9.1 的 A-plan（甲方案）只切弦、节点环共享；D1 杀手图要的"每样本独立环"一直
挂在 §17-8。现在 `ggcircos(..., by = <节点表列>)` + `facet_wrap(~ <同列>)` 给出乙方案：
每个 panel 自己一套节点、自己的排序与弧长。

**关键设计**（都写进 spec §9.1）：

- **节点身份是 `(组, id)` 二元组**：同一 id 可以在不同组各出现一次（每样本各有自己的
  `a`、`b`……），组内唯一即可。"分组"是在组内**重建**一个环，不是把大图切块。
- **链接归位优先看自己的组列**：链接表有与 `by` 同名的列就用它（最明确）；没有才按两端
  节点组推断，此时跨组的线丢弃并 `cli_inform()`。
- **数据流**：`circos_layout(by=)` 返回 `circos_layouts` 列表；`ggcircos(by=)` 把各组的
  nodes/links/sectors **堆叠**成带分组列的合并表挂到 plot 上，`facet_wrap` 再按该列切。
  **几何层完全不用改**——它照旧读"一张表"，只是这张表现在带了个 panel 列。

**踩到的坑**：

1. **空表加列会报错**。没有 `tracks` 的组其 `layout_tracks()` 是 0 行，`d[[key]] <- k`
   在 0 行 frame 上赋长度 1 的值会 `replacement has 1 row, data has 0`。
   写成 `d[[key]] <- if (nrow(d)) k else character(0)`。
2. **`require_layout()` 在多 layout 下返回 NULL 会炸**。chord 层与 node 层改成先查
   `circos_layouts_of(plot)`：有就走"堆叠表"路径，没有才要单个 layout。
3. **`print.circos_layout` 的 `@export` 一度被我顺手删掉**，S3 注册丢失，`print(layout)`
   退化成 list 打印，`test-layout.R` 的 read-only 断言抓到。教训：加新 S3 method 时
   别动到旁边那条的 roxygen 标签。

**首版范围**：`geom_chord()`、`geom_node_arc/text/point()`、`geom_sector_label()` 支持；
**`geom_track()` / `geom_track_axis()` 报清晰的 `ggcircos_error_data`**——轨道要每 panel
自己的 band 与 `track_angles()` 按组查角度，是下一步。

回归测试：`tests/testthat/test-facet-by.R`（7 个 `test_that`）：每组独立建 layout、
不同节点数导致不同弧宽、显式组列 + 组内重名、跨组链接丢弃提示、`ggcircos(by=)` 出的
两 panel 各自节点集、track/from-to 的清晰报错、`by` 缺列报错。

### B45. `by =` 下轨道无处安放 —— **已支持每 panel 自己的轨道（2026-10-22）**

B44 落地时把 `geom_track()` 在 `by =` 下明确拒了（"轨道需要每 panel 自己的 band"）。
现在补上。

**为什么其实比预想的容易**：**轨道带是共享的**。`tracks =` 是一个全局设置，
各组的 band 半径相同（实测两组都是 r ∈ [1.06, 1.26]），所以 `PositionTrack` 仍然只需
一个 band，不必按 panel 分裂。真正要按组算的只有**角度**：一行落在哪个角度，取决于
它属于哪一环、那一环的节点表怎么排。

**改动**：

- `track_angles()` 收下 `layouts`（各组 layout 列表）+ `group`（行属于哪组），
  逐组查该组的节点表得到每行的 `theta` 与 `arc`；
- 顺带把"算完 theta/arc 之后写 y/ymin/ymax"这段抽成 `track_finish_angles()`，
  单 layout 与多 layout 共用。**`ymin` 改成从 `theta - arc/2` 算**，不再从
  `nt$theta0` 取——两者对单 layout 等价，但多 layout 下每个组的 `theta0` 不同，
  用 `theta`/`arc` 才都对；
- `ggplot_add.ggcircos_track()` 分成两支；`ggplot_add.ggcircos_axis()` 同理，
  刻度落在**各 panel 自己的首节点之前**（用堆叠后的节点表）。

**踩到的坑**：

1. **`circos_layout()` 的具名参数在 `by` 分支里全丢了**。generic 把 `tracks`、
   `sectors`、`order`、`gap`… 都声明成形参，`by` 分支却只转发 `nodes` 和 `...`，
   于是每组 layout **没有轨道带**（`layout_tracks()` 是 0 行）。修法：`by` 分支
   显式转发每一个形参。**这正是"转发 `...` 看起来等价"的一个反例。**
2. **错误发生在加层时，不是 build 时**。测试里写 `expect_error(ggplot_build(p) + facet_wrap(...))`
   永远测不到——layer 早就加过了，`ggplot_add` 里就抛了。改成 `expect_error(p + geom_track(...))`。
3. **built 的 layer 里没有分组列**（ggplot2 只保留美学），所以验证"落在自己那组的角度"
   要靠 `PANEL`，不能靠分组列。

回归测试（`test-facet-by.R` 追加三个 `test_that`）：轨道每行的角度等于**自己那组**的
theta（用 4 节点 vs 2 节点的两组，角度必然不同，错用另一张表就会露馅）、轨道数据缺分组列
时报 classed error、轴在每个 panel 各画一份。

### B46. 多半径节点：同一个节点挂多层弧 —— **已落地（2026-10-22）**

spec §17-2 挂了很久："多半径节点超出 v0.1，但 layout 列留了 `r`，以后可扩展"。
那个 `r` 列是**真的留了**——`node_defaults()` 画节点弧时用的就是 `.data$r`，
所以绘制侧一直支持，只是 layout 侧把每个节点的 `r` 钉死成 1。

**契约**（spec §4.9）：节点表里同一个 `id` 可以出现多次，每行一层弧，半径取该行的 `r`；
同一 id 的各层**径向对齐**（共用 theta0/theta1/width），角度预算按 **id 个数**算；
**槽位按弧**（每层独立划分那段共享弧）；链接用 `from_r`/`to_r` 指层，缺了且有歧义就报错。

**实现上最关键的一点是把"id 表"和"弧表"分开**：

- **id 表**（每 id 一行）负责几何——顺序、sector、间隙、弧宽。它是环的真相。
- **弧表**（每 `(id, r)` 一行）负责绘制与槽位。它带着 id 表的几何，加上自己的 `r`。

改动集中在 layout 内部，**几何层一行没改**（因为 `geom_node_arc()` 本来就按 `.data$r` 画）。

**踩到的坑**（都是"看起来对、其实错"的类型）：

1. **弧表按用户顺序排，覆盖了 `order =` 的结果**。节点表的行序**就是**环序，
   弧表必须跟着 id 的环上位置排，否则 `order =` 和间隙预算一起失效。
   ——表现为 `order = rev(ids)` 完全不起作用。
2. **`order` 这个形参遮蔽了 `base::order`**。`circos_layout_build()` 有个叫 `order` 的参数，
   于是 `order(arc_idx, method = "radix")` 调的是**用户传的排序函数**，
   报 "unused argument (method = ...)"。要用 `base::order()` 显式调用。
3. **字符串 key 匹配半径会失配**：`format(1, digits=15)` 得 `"1"`，`format(1.0, ...)` 得 `"1.0"`，
   同一个半径匹配不上自己。`(id, r)` 的比较必须用数值。
4. **0 行的 frame 没有列**。`real$.circos_from_arc` 在 0 行时是 `NULL` 而非空向量，
   `tabulate(NULL)` 报 "bin must be numeric"；`mapply` 在 0 行输入时返回 `list` 而非整数向量。
5. **`names(members) <- keys` 漏了一行**，于是 `members[[k]]` 取到 `NULL`，
   端点检查全 FALSE，所有链接被静默丢弃。表现是"链接莫名其妙全没了"。

**顺带修掉一个既有警告**：`R CMD check` 一直在报
`checking code files for non-ASCII characters`——R 源码里有 39 个 `§` 和 16 个 em dash
（注释与 cli 消息里的"spec §4.9"之类）。**`R CMD check --as-cran` 会因此报 warning**，
包就不是 portable 了。已全部换成 ASCII（`sec.4.9`、`--`）。

回归测试：`tests/testthat/test-multi-radius.R`（8 个 `test_that`）：两层径向对齐、
角度预算按 id 计、链接的端点半径、歧义报错、同 id 同半径报错、画布装得下最外层、
**单半径布局逐位不变**（兼容性底线）、以及 `by =` 与多半径叠加（节点身份是 `(组, id, r)`）。

### B47. 轨道行与链接槽位对齐：`geom_track(slot =)` —— **已落地（2026-10-22）**

复现 circlize 书第 16 章时发现的硬缺口：那张图的甲基化矩形**贴在每条链接自己的槽位上**，
宽度也跟着链接走。本包 `geom_track` 把一个节点的多行按数据顺序**等分**该节点弧（B37），
既不按值加权、也不知道链接的槽位顺序——画出来的矩形与链接**错位**，也就是说
复现图里的甲基化信息是错的（对不上各自的链接）。这不是"细节还原度"问题。

**机制**（spec §4.10）：`geom_track(slot = edge_id)` 指名轨道数据里哪一列是链接行号，
该行就占它那条链接的槽位弧段。不引入新几何——只是让 `track_angles` 接受**外部给定的弧段**。

**⚠ 首版把两端的槽位混成了一个（同日修正）**：`track_align_slots()` 最初无条件取
`links$theta0/theta1`，也就是**永远用 from 端**。一条链接在**两个**节点上各留了一段弧，
两端是不同的角区间；把 to 端的行也画到 from 端，等于目标侧的矩形出现在环的另一边。
这个错**只在复现图上看出来**（甲基化矩形一半贴错位置），当时的单元测试没测到，
因为测试数据每行恰好都放在 from 端。

修法：用行自己的节点 id 与链接两端比对，决定取 from 槽还是 to 槽（自环两端同节点，
取 from，即它的脚）；半径同理（`x` vs `xend`）。

**教训**：测试数据要覆盖"两端不对称"的情况。每行都放在 from 端的测试，等于没测槽位选择。

回归测试：`"a slot-aligned row sits on the slot of ITS node, not always the from end (B47)"`
——三行分别放在 link1-from / link1-to / link2-to，逐行与对应槽位比对；
把修复回退后该测试连红三条，确认抓的是真因不是巧合。

顺带修掉的两处（都是复现过程里撞出来的）：
- `column_or_string()` 把"列名在调用环境里不存在"当成致命错误，于是 `slot = edge_id`
  这种"只存在于图层数据里的列名"根本传不进来；改成落到"它是一个列名"。
- 复现脚本一度在 `library(ggcircos)` 下失败、在 `load_all()` 下成功——**装的是旧版**。
  所有"神秘通过/神秘失败"都跟着当时装的是哪一份。改完代码要么 `load_all()`，要么先重装。

### B48. 调用方的 `edge_id` 列被静默覆盖 —— **已修（2026-10-23）**

`allocate_slots()` 无条件执行 `links$edge_id <- seq_len(nrow(links))`，把链接按 layout
顺序编号写进 `edge_id`。这是本包的行键（槽位表靠它对齐），但调用方若**自己带着**同名列
（`slot = edge_id` 的自然写法），会被直接覆盖：实测用户给的 7 进去、1 出来，**无警告**。

后果比 B47 更隐蔽：`slot =` 仍然能跑通、也不报错，只是所有行都对到了**错误的链接**上，
画出一张看起来正常的错图。

**修法**：赋值前先保护——同名列重命名为 `user_edge_id` 保留。`by =` 路径本来就因
`rbind` 去重自动走到这一步，单 layout 路径漏了。

**教训**：用自己的列名当内部行键，必须先把同名列让开。这不是风格问题，
是"用户数据被静默吃掉"。

回归测试：`"a caller's edge_id column is kept, not overwritten (B48)"`。

### B49. `slot =` 在 `by =` 图上完全不工作 —— **已修（2026-10-23）**

`by =` 的每个 panel 各自从 1 给链接编号，堆叠后的链接表 `edge_id` 必然重复（1, 1, ...）。
`track_align_slots()` 拿 `(group, edge_id)` 去 match，第二行匹配不上，直接报
"Some track rows name a link that is not in the layout"——**错误信息还指向用户的数据**，
而错的其实是包的匹配键。

这意味着 B47 的功能在任何分面图上都不可用，而分面恰恰是逐链接标注最需要它的场合。

**修法**：`by =` 路径改用 `(group, user_edge_id)` 匹配（B48 的修复让这个列存在）；
调用方没给 `edge_id` 时退回 `(group, edge_id)`。

**教训**：B47 的回归测试只测了单 layout 的 from/to 两端，没测分面——**功能的
"最需要它的场合"没进测试，就等于没测**。补了三个 panel 的用例，并把两个 panel 做成
节点数不同，否则对称的两个环弧恰好一样，错匹配看不出来。

回归测试：`"slot = works on a by= plot: every panel numbers its own links (B49)"`
——回退修复后该测试变红。

### B50. 非方阵错误信息里多一个 `}` —— **已修（2026-10-23）**

`circos_layout.matrix()` 的 cli 模板写成 `{.val {nrow(x)}}x{ncol(x)}}`，多了一个右花括号。
用户看到的是 `The matrix is 2x3}; a weight matrix must be square.`——这是包在**报错**
时给人的第一印象，一个字都不能错。

**教训**：cli 模板的括号错误不会让代码失败，只在用户真的踩到那条错误路径时才现形。
错误信息也值得一个测试。

### B51. `pretty_within()` 可能返回重复刻度 —— **已修（2026-10-23）**

极度退化的区间（如 `c(0, 0)`）下 `pretty()` 会返回重复值，环上就会出现两个叠在一起的
标签。`unique()` 一行修掉。目前尚无实测触发，属预防性。

### B52. `geom_circos()` 不传 `geom` 就报错 —— **已修（2026-10-24）**

弃用包装 `geom_circos()` 的本意是让重写前的旧脚本继续能跑，而 `geom` 是**可选**参数
（文档里默认走柱状轨道）。实测不传就直接死：

```r
geom_circos(aes(x = value), data = old)
#> ERR: `geom_circos()` cannot translate ""; use "col", "bar", ...
```

根因在 `missing()` 的位置：判定写在辅助函数 `geom_circos_geom_name()` 里，而调用方传进去的是
`substitute(geom)`——参数**已经**被供上了（只是供的是"缺失"标记），所以辅助函数里
`missing(geom)` 为 FALSE，`as.character()` 把标记变成 `""`，落到校验分支报错。
辅助函数自己的兜底 `quote(geom_col)` 说明"col"本来就是想要的默认值。

**修法**：默认判定移到包装层，那里还看得见用户的调用形状：
`if (missing(geom)) "col" else geom_circos_geom_name(substitute(geom))`。两行。

**教训**：`missing()` 只在**参数被绑定的那一层**有意义。传 `substitute(x)` 过了函数边界，
缺失信息就丢了，而且不会报错——只会把 `""` 带进后续逻辑。

回归测试：`"geom_circos() works without a geom argument (B52)"`。

### B53. `repel` 在 `by =` 图上跨 panel 递推 —— **已修（2026-10-24）**

`geom_node_text(repel = TRUE)` 在 `ggplot_add` 时对节点表做避让。`by =` 图的节点表是
**各 panel 堆叠**在一起的，而当时**没有按分组列切开**——于是一个 panel 里同角度的标签
被当成邻居。实测两个 panel、节点 id 相同的图：P2 的三个标签全部被外推一格
（r = 1.055），而在它自己的 panel 里根本不碰撞。N 个 panel 就有第 N 个 panel 凭空外推 N−1 格。

**修法**：`repel_node_labels()` 增加 `group` 参数，按 `circos_by_key` 分组、每组各自走一遍。

**教训**：`by =` 的堆叠表里"同一列"可能横跨互不相干的坐标空间。凡是**按空间关系**做判断的
逻辑（避让、排序、邻接），都必须先按 panel 切开。

回归测试：`"repel staggers inside each panel, not across them (B53)"`。

### B54. `repel` 从不比较跨缝的那一对标签 —— **已修（2026-10-24）**

避让走的是"按角度排序后逐个比对前一个"。角度是个环，但排序不是：
最后一个标签（θ ≈ 2π）和第一个（θ ≈ 0）在屏幕上是邻居，却**从来没有被比较过**。

实测 150 个节点的真实布局：跨缝那对的弧间距 0.056，避让阈值 0.063——**碰撞了，但第一个标签
保持 r = 1**，而其余 150 个里有 136 个被推开。节点少（约 120 以下）时间距超过阈值，
所以 demo 图里一直看不出来。

**修法**：每圈走完补一次跨缝比较，`(theta[first] + 2π − theta[last]) * r[last]`，
小于阈值就把第一个从最后一个的半径往外推。

**教训**：凡是"一圈上相邻"的概念，线性遍历都漏掉首尾这一对。测试数据要造**跨缝**的情形，
不能只造顺序排列的。

回归测试：`"repel compares the two labels that straddle the seam (B54)"`。

### B55. violin / density 里退化的节点静默消失 —— **已修（2026-10-24）**

`node_density()` 对"少于 3 个有限值"或"完全没有离散度"的节点返回 NULL，stat 跳过它。
**全部**节点都不可画时会报一个清楚的错（`abort_no_density`），但**只有部分**节点不可画时，
一个字都不说。

实测一个正常节点 + 一个三值相同的节点：图照常画出，只剩 "a"，没有任何提示。
环上少一个小提琴，读起来是"这个节点没有测到分布"——和事实正好相反。
`node_density()` 的注释写着"A caller decides whether that is worth a message"，但没有调用方去决定。

**修法**：两个 stat 各自收集跳过的节点，最后 `cli::cli_inform` 一次，报出数量与节点名。

**教训**：注释里写"由调用方决定"的地方，要确认真有一个调用方决定了；否则就是约定悬空。

回归测试：`"a node with no drawable distribution is reported, not silently dropped (B55)"`。

### B56. B48 的重命名会撞上用户已有的 `user_edge_id` —— **已修（2026-10-24）**

B48 把调用方的 `edge_id` 改名保护起来，但**目标名写死**成 `user_edge_id`。
调用方若本来就有一个 `user_edge_id` 列，改名就撞成两列同名，其中一列按名字取不到。

实测：输入 `edge_id = 7, user_edge_id = 99`，输出只剩 `user_edge_id = 7`——**99 那列静默消失**。
一个为了"防止静默丢列"而做的修复，自己又静默丢了一列。

**修法**：改名目标改用 `make.unique()` 挑一个没被占用的名字。同时发现 B48 真正的落点在
`circos_layout_build()` 的 shadow 重命名循环（`edge_id` 本来就在 `reserved_link_cols` 里，
早就被改名了），我上一轮加在 `allocate_slots()` 里的那段是**死代码**，一并删除。

**教训**：同一件事有两处代码在做的时候，先确认哪一处真的在跑——否则修的是影子。

回归测试：`"a caller column named user_edge_id survives the edge_id rename (B56)"`。

### F1. 基因组区间轨道：`chr + start + end` —— **已落地（2026-10-25）**

review 之后判断"包要完整还缺什么"时，第一项就是它。`geom_track()` 只认单一 `x`，
而基因组里一个基因、一段 CNV、一个 peak **本身就是区间**；压成一个位置值会丢掉
"这个 feature 有多宽"。circlize 的 `circos.genomicTrack()` / `genomicRect()` 全是这个模型。

**机制**（spec §4.11）：不给 track 加新概念，而是让"值"从标量变成区间。节点弧本来是
`[0, 2π)` 分给各节点；区间只是说"这段弧里我占 `[t(a), t(b)]`"。所以现有仿射映射原样可用，
只需在进入 position 之前把 `start/end` 归一化成节点内相对位置：
`pos = (v − origin) / (L_i − origin)`，`L_i` 是 `domain` 列（染色体长度），
没给时取该节点自己的 `max(xmax)`。每个节点**各自归一化**——100 Mb 的染色体和 5 Mb 的
contig 都铺满自己的弧，这是 per-node domain 的意义。

新参数：`domain =`（节点表列名）、`origin =`（默认 0）。新几何：**无**——复用 `"tile"`，
它的 `setup_data()` 本来就原样透传数据。

**⚠ 过程中两个自我纠正**

1. **初稿说要新增 `GeomTrackRect`，撤掉了。** 写契约时发现 `GeomTrackTile` 已经能画
   "径向填满 band、角度由 `ymin`/`ymax` 决定"的矩形，正是区间要的形状。实测它当时画不出来，
   于是又写了一条"补 `track_finish_angles()` 的第三分支"。实现时核对调用图发现
   **两条路根本不交汇**：`track_finish_angles()` 只被 `track_angles()` 调用，而区间走的是
   `track_place_intervals()`（在 `ggplot_add` 里直接取代前者）。那个分支是**死代码**——
   实测禁用后行为毫无变化，已删除。

2. **第一版回归测试抓不到 bug。** 用 `start = 0, end = 500` 在 1000 长的节点上测，
   区间正好是半弧，而"被等分"也是半弧（每节点一行时 `bin_w = arc·width`），两者数值巧合相同。
   改成断言**比例**（1000 上的 0-200 必须是 20% 弧，不是 50%）才真正钉住。

**教训**：写"某函数需要加分支"之前先确认它真的会被调用；写测试用例前先确认
"错的实现会给出不同的数值"——数值巧合相同的断言等于没断言。

回归测试：`tests/testthat/test-interval-track.R`（5 个用例：铺满整弧、各自归一化、
无 domain 按数据范围、不被邻行分割、五条错误路径）。

### F2. 轨道内的阈值线：`geom_track_hline()` —— **已落地（2026-10-25）**

`geom_hline(yintercept = 0.5)` 在本包里画到**角度 0.5** 上，不是轨道值 0.5 上——
因为 track 的 `y` 就是角度（§4.2）。基因组图几乎一定要画阈值（拷贝数 0 线、显著性
阈值、均值线），而手工把"轨道值 → 半径"换算一遍既啰嗦、又容易和 `scales`/`limits`
的设定脱节。

**机制**（spec §6.4b）：`value` 经过**与 `geom_track()` 同一条**归一化——直接借用它那一层
的 `PositionTrack`（同 `scales`/`limits`/`baseline`），而不是重建一份。这是本 geom 存在的
唯一理由：线必须永远和它标注的数据在同一把尺子上。线**按节点分段**画（每个节点弧内一段），
读起来是"每个节点自己的阈值"，而不是一根穿过间隙的圆环。

**⚠ 实现时踩的两个坑**

1. **忘了注册 S3 方法**。`ggplot_add.ggcircos_hline` 写好了，但 NAMESPACE 里没有它，
   于是 `p + geom_track_hline()` 静默走了父类方法——`data` 根本没被填，报的是
   "geom_segment requires missing aesthetics"，指向完全无关的地方。
   `devtools::document()` 之后才注册上。**新 S3 方法一定要跑一次 document 再测。**

2. **`re_add()` 的 class 清单是硬编码的**，漏了 `ggcircos_hline` 会让去 class 失败。
   每加一个 `ggcircos_*` 图层类型，都要同步改 `re_add()` 的 `setdiff()` 列表。

回归测试：`tests/testthat/test-track-hline.R`（5 个用例：值是值不是角度、跟随
`scales`/`limits`、按节点分段跳过间隙、超界夹到边界并提示、向量画多条线）。
把归一化换成"直接用原值"后连红 3 条，确认抓的是真因。

### F3. `column_or_string()` 在 knitr 环境里抛错 —— **已修（2026-10-25）**

补区间轨道 vignette 时发现：`domain = len` 这种**裸符号列名**在 knitr chunk 里
报 `object 'len' not found`，而在控制台和普通函数里都正常。

根因是 `column_or_string()` 先 `eval_tidy()` 求值符号、失败才回退到"当列名"。
knitr 的求值环境链让那次失败以不同方式逃出 `tryCatch`。这个 bug 影响**所有**用裸符号
传列名的参数（`node =`、`slot =`、`domain =`）——只要用户在脚本或包里调，不是只在 notebook 里。

**修法**：符号分两种读法，都要支持——
`node = id`（列名，符号只存在于数据里）与 `col <- "id"; node = col`（符号存着名称）。
用手写的 `get0(..., ifnotfound = NULL)` 取一次：拿到字符串就用它，否则用符号自己的名字。
不依赖 `eval_tidy` 的异常路径。

**教训**："先求值、失败再当字面量"这种控制流，在不同环境链下行为会变。
能用无异常的方式判定，就别用异常做分支。

回归测试：vignette `tracks.Rmd` 的区间一节直接用 `domain = len`
（既有测试 `"the node column can be unquoted"` 也覆盖 `node = col` 的写法）。

## C 类：测试与工程（不阻塞，但要有）

### C1. 没有视觉回归快照 —— **已建基线（2026-10-19）**

spec §14.4 要求的基线快照已建：`tests/testthat/test-snapshots.R` + `_snaps/snapshots/` 下 10 张 SVG，
覆盖 spec 列的全部场景（基础弦图 / arc / facet / 多轨 / sector / slot-fill / 轴 / clockwise=FALSE / 白底 / 废弃 shim）。

`NOT_CRAN=true` 下跑 `test_dir("tests/testthat", filter = "snapshots")` 会生成新快照；
不带该变量时 `skip_on_cran()` 会跳过（`R CMD check` 里 vdiffr 会自动 skip，不影响 CI 判红）。

**这 10 张快照是 M1.5 的"改前照片"，因此 5 张里留着已知 bug——这是刻意的**：

| 快照 | 记录的已知 bug | 修好后如何变 |
|---|---|---|
| `theme-circos-paints-the-panel` | **B9**：panel 是 `#EBEBEB`，只有 plot 区域是白的 | panel 变成 `#FFFFFF` |
| `sectors-and-sector-labels` | **B1**：`ref`/`qry` 标签与对应扇区反了（跨 0 的 min/max） | 两个标签各归其位 |
| `stacked-tracks` | **B7**：`tile` 轨道画不出来（width/height = 1，全落在带中心） | 出现真正的色块格 |
| `track-axis` | **B8**：刻度贴在节点边界上，break 标签只有几不可见的残余 | 断点刻度在环上，标签可读 |
| `slot-fill-leaves-gaps-between-feet` | **A3**：脚仍画满槽位（虽然这版已经把 `theta0/1` 改成 `n_slots` 索引，但脚宽度未按槽位收窄） | 脚之间出现 `(1-p)` 的空隙 |

修这三条（B1/B7/B8/B9/A3）时必须用 `NOT_CRAN=true` 跑一遍、逐张确认 SVG 差异**只来自**预期 bug 的修复，
没有别的漂移，再接受快照。**未知差异 = 回归**，不要直接 `snapshot_accept()`。

### C2. 边界输入测试缺失 —— **已补（2026-10-22）**

新增 `tests/testthat/test-edge-inputs.R`，每条断言**错误类**而不只是"不报错"：
空 df（无 nodes → `_data`；有 nodes → 合法、列齐全）、单节点整圈、NA sector
（`_node`）、负 `gap`/`sector_gap`（`_layout`）、NA link（`_data`）、非法/不等长矩阵
（`_data`）、weight 非数值/Inf/缺列（`_data`）。

**两处原本以为缺测、查完是既有设计**：
- `nodes` 表无 `id` 列时用**第一列**当 id（宽容，`data.frame(name=)` 能用）；
- 只给 `from`/`to` 未给 `nodes` 时，未见的 id **自动成为节点**（只有显式给了
  `nodes` 而缺该 id 才报 `_node`）。

**一个 R 层无法拦截的真实陷阱**（记入已知限制，不修）：`data.frame(from=c("a","b"), to="b")`
在 `data.frame()` 构造时就把 `to` **静默 recycle** 成两行，进入 `circos_layout()` 时
已不可辨。这是 R 语言行为，除非要求用户传 list。显式 `NA` 会被拦下（`_data`）。

### C3. CI 矩阵只有 4.0.3 —— **已建 workflow（2026-10-22）**

按 spec §14.5：`ubuntu` × R `release`/`oldrel-1`，两行都**显式 pin ggplot2 4.0.3**
（§1.4 已定只支持 ≥4.0，3.5 矩阵作废）。判据 `--no-manual --as-cran`、
`error-on: warning`（与 AGENTS.md 的本地判据一致；PDF manual 属环境缺失）。
文件 `.github/workflows/R-CMD-check.yaml`，YAML 已过合法性检查。
**未在本机实跑 Actions**（无 runner）；真跑依赖上游 runner 能装到 ggplot2 4.0.3。

spec §14.5 要求 `3.5.*` 与 `4.0.*` 双版本。A1 定了之后，这里可以直接收到只测 4.0。

### C4. `R CMD check` 的 PDF manual 依赖

本机无 `pdflatex`，`make check1` 固定 1 ERROR + 1 WARNING。属环境问题，但 CI 上要显式配置（`--no-manual` 或装 texlive），否则红的永远是这条，掩盖真问题。

### C5. README 的 tracks 图与代码不同源 —— **已重渲染（2026-10-22）**

做 B43（自环）时按 AGENTS.md 重渲染 README，发现 `man/figures/README-tracks-1.png`
换了字节。追下去：用**干净 HEAD** 的代码重渲染同一 chunk，得到的结果与仓库里那张已提交的
PNG 有 7.2 万像素不同，而我这次的改动对它**零像素影响**（HEAD 重渲染 vs 新代码重渲染
逐像素相同）。结论：那张图是更早的代码/设备渲染的陈旧产物，一直没跟上。

这正是 AGENTS.md 反复警告的"图停在旧实现上"。已用当前代码重渲染并接受。
**教训**：改 README 的几何/默认值时，别只看 git 有没有提示图变了——用同一份代码
重渲染两次对比，才能分清"图真变了"和"图早就是旧的"。

---

## 处理顺序建议

**第一步：定规则（只写 spec，不写代码）** —— ✅ 已落 spec v0.2（A1–A6）

| 顺序 | 条目 | 决定什么 |
|---|---|---|
| 1 | A1 | 坐标层是否只支持 ggplot2 ≥ 4.0（`coord_radial`）——这条决定 CI 矩阵与所有后续实现的兼容面 |
| 2 | A2 | `clockwise` 的方向语义 + 屏幕角的唯一来源（禁 `atan2` 反推） |
| 3 | A3 | `slot_fill` 的几何契约（槽位宽 vs ribbon 脚宽），连带 B1 的扇区跨 0 |
| 4 | A4 | layout 的只读边界（访问器返回副本、position 只持有向量） |
| 5 | A5 | facet 语义（甲：切弦；乙：重建 layout）——**决定 D1 那张杀手图能不能做** |
| 6 | A6 | `geom_track(position = )` 是否允许外部 position |

**第二步：M1.5 架构收口** —— ✅ 已完成（2026-10-19）

落地清单：A1（`CoordRadial` + `ggplot2>=4.0`）、A2（`screen_angle()`，删 `atan2`）、
A3（按侧槽位 B16、扇区连续段 B1、`gap` 自适应 B4、输入校验 B3/B5）、A4（position 只持向量、
共享隔离）、A6（`geom_track(position=)`）、B9（panel 背景）、B13/B15（`rotate` 路径）、
B17（`start` 传播）、C1（10 张快照）。
`R CMD check --no-manual` 0/0/0；`testthat` 375 通过（含 10 快照）。

**第三步：能用** —— B2（空图合法，已改报错为 classed，待决定要不要真支持空图）；快照已建。

**第四步：主线功能** —— B7（tile 轨道）/ B8（轨道轴）。

**第五步：打磨** —— B11（连续 fill 分桶）/ B12（text 轨道语义）/ A7（shim 验收）。

---

## 附：本次审核的实测环境

| 项 | 值 |
|---|---|
| R | 4.6.1 (2026-06-24) |
| ggplot2 | 4.0.3 |
| `testthat` | 375 通过 / 0 失败 / 0 警告（含 10 张 vdiffr 快照） |
| `R CMD check --no-manual` | Status: OK |
| 1k ribbon | 0.67 s（spec 目标 < 2 s） |
| 5k ribbon | 3.45 s（spec 目标 < 8 s） |
