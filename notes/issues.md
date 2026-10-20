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

---

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

### C2. 边界输入测试缺失

空 df、单节点、NA sector、负 `gap`、重复 node id、`nodes` 表缺 `id`、矩阵含 NA——多数没测，或只测了"不报错"而没测"报什么错"（错误类）。

### C3. CI 矩阵只有 4.0.3

spec §14.5 要求 `3.5.*` 与 `4.0.*` 双版本。A1 定了之后，这里可以直接收到只测 4.0。

### C4. `R CMD check` 的 PDF manual 依赖

本机无 `pdflatex`，`make check1` 固定 1 ERROR + 1 WARNING。属环境问题，但 CI 上要显式配置（`--no-manual` 或装 texlive），否则红的永远是这条，掩盖真问题。

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
