# ggplot2 Style Guide for AI Agents

This document defines the principles for creating visualizations in R. When generating ggplot2 code, prioritize these rules over default library settings. 

---

## 0. Pre-Flight: Required Checks Before Any Code

> **Do not write any `ggplot()` call until you have enough from the user to avoid wrong assumptions.**

AI agents fail most often by misreading data or making unsupported claims. **Ask the user for guidance** instead of inferring: what question should the chart answer? What are the variable names and units? How should NAs or aggregation be handled? You do not need to understand every detail of the data—when in doubt, ask. A short, targeted question is better than a long automatic inspection that still gets the story wrong.

### Step 1 — Get Minimum Required Input (Ask if Missing)

Before writing code, you need:

- **The question the chart must answer** (or a clear description of the message).
- **Variable names** for x, y, and any grouping/color.
- **Units** for plotted variables (e.g. "ktoe", "billion USD").
- **Row meaning** (e.g. one row = country-year) and whether aggregation is needed.

If any of these are unclear, **ask the user**; do not guess from file names or column names. Optionally ask about NAs, audience, joins, or category definitions only when relevant.

### Step 2 — Optional Short Diagnostic (Only When Helpful)

If the setup is non-trivial, you may output a one- to two-line confirmation, e.g. "Plotting [y] vs [x] by [group]; one row = [grain]. Aggregation: [yes/no]." Do not produce a long diagnostic block unless the user or the task clearly benefits from it.

### Step 3 — Fix These Before Plotting

**Join keys and time variables: same type in every table.** The join key (e.g. `id`, `ipums_id`) and any time variable used in a join (e.g. `year`) must have the **same class** in every table. A `double` vs `character` mismatch drops rows silently. Coerce immediately after loading — **character** for id keys, **integer** for calendar years (using `as.integer(as.numeric(year))` handles both character and double input).

```r
# Coerce before any join — apply to every table that has these columns
id_df    <- id_df    |> mutate(ipums_id = as.character(ipums_id),
                                year     = as.integer(as.numeric(year)))
units_sf <- units_sf |> mutate(ipums_id = as.character(ipums_id),
                                year     = as.integer(as.numeric(year)))
```

**Time variables used in joins or on axes: same type in every table.** The same type-consistency rule applies to **time variables** (e.g. `year`, `date`) whenever they are used in a join (e.g. `full_join(x, y, by = join_by(id, year))`) or on an axis. If one table has `year` as character and another as double, joins can drop rows or produce wrong matches with no error, and plot axes can behave inconsistently. Coerce the time variable to a **single type in all sources** — e.g. integer for calendar years (`as.integer(as.numeric(year))` so it works whether the column was character or double), or `Date`/`POSIXct` for dates — immediately after loading each table, before any join or plot.

```r
# Coerce year to integer in every table that has it (handles character or double from read_dta/readRDS)
ntl      <- ntl      |> mutate(year = as.integer(as.numeric(year)))
loss_df  <- loss_df  |> mutate(year = as.integer(as.numeric(year)))
builtup_df <- builtup_df |> mutate(year = as.integer(as.numeric(year)))
# Then join by (ipums_id, year) and use year on x-axis; no type mismatch
```

**Avoid duplicate column names (no .x / .y) when joining to a master.** When one table is the canonical “master” (e.g. id.rds with unit key and country) and you join another table to it (e.g. a shapefile that also has `country`), dplyr will create `country.x` and `country.y` if both tables have the same column name. To prevent this: **from the non-master table, select only the join key and columns that do not exist in the master** before joining; then join to the full master so identifiers and attributes (e.g. country) come from the master in a single column. Alternatively, if the duplicate columns are guaranteed identical (e.g. same country codes in both), you can join on multiple columns (e.g. `by = join_by(ipums_id, country)`) so only one copy is kept — but prefer dropping the duplicate from the non-master so the master remains the single source of truth.

```r
# Master = id_df (canonical id + country). Shapefile has ipums_id, country, geometry.
# Bad: join brings both countrys -> country.x, country.y
# id_sf <- units_sf |> inner_join(id_df |> select(ipums_id, country), by = join_by(ipums_id))

# Good: from shapefile keep only join key and columns not in master (e.g. geometry), then join full master
cols_from_sf <- c("ipums_id", setdiff(names(units_sf), names(id_df)))
id_sf <- units_sf |> select(all_of(cols_from_sf)) |> inner_join(id_df, by = join_by(ipums_id))
# Result: one country column (from id_df), no .x/.y
```

**Time variables (plotting):** Parse before plotting — never pass a character vector to a date axis.

```r
library(lubridate)
df$date <- lubridate::ymd(df$date)                              # ISO strings
df$date <- zoo::as.yearqtr(df$date, format = "%Y-Q%q")         # Quarterly strings
```

**Factor order:** Default alphabetical ordering is almost always wrong.

```r
df$size    <- fct_relevel(df$size, "Small", "Medium", "Large") # ordinal
df$country <- fct_reorder(df$country, df$value)                # ranked bars
```

**Aggregation:** A jagged sawtooth `geom_line()` means multiple rows per X value.

```r
df_agg <- df |>
  group_by(year, category) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
```

**Grouping with quantiles and `cut()`:** `cut(x, breaks = quantile(x, probs = ...))` can throw *"breaks are not unique"* when the variable has many ties (e.g. lots of zeros or discrete values). Use **`dplyr::ntile(x, n)`** for equal-count groups (e.g. tertiles) so breaks are never an issue, or ensure unique breaks (e.g. `breaks = unique(quantile(..., na.rm = TRUE))`) and handle the resulting number of levels.

```r
# Prefer: equal-count groups, no break uniqueness problem
df <- df |> mutate(
  tertile = factor(ntile(value, 3), levels = 1:3, labels = c("Low", "Medium", "High"))
)

# If you must use cut(): ensure unique breaks
br <- quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
br <- unique(br)
if (length(br) < 2L) stop("Too few unique quantiles to form groups")
df <- df |> mutate(tertile = cut(x, breaks = br, include.lowest = TRUE, ...))
```

---

## 1. Chart Type Selection

**Choose the geom that best reveals the structure of the data.** The default geoms (line, bar, filled map) are often, but not always, right. Consider alternatives — especially when data is sparse, skewed, or has a strong relational structure. But do not go for complex graphs just for the sake of it. If possible, use a simple chart type that serves the data - more complex types (Sankey, bump, etc.) are useful for when they add clear value.

| Goal | Recommended geom | Notes |
|---|---|---|
| Distribution (1 continuous var) | `geom_histogram` or `geom_density` | Histogram for counts; density for shape; filter zeros when meaningful (e.g. "cells with ≥1 event"). Consider overlaying a **density line** (e.g. `geom_density(aes(y = after_stat(count)))` with same scaling, or a frequency polygon) so the shape is easier to read than bars alone. |
| Comparison (categorical) | `geom_col` + `coord_flip()` | Never `geom_bar()` on pre-aggregated data |
| Change over time | `geom_line` + direct labels or facets | See Sections 4 & 5 |
| Correlation (2 continuous) | `geom_point` + `geom_smooth` | Use `coord_fixed()` for honest slope. **State the trend-line method and what the band represents** in the subtitle (e.g. "LOESS smooth, 95% CI") — see **Scatterplots and trend lines** below. **Omit trend/CI when the story is a path over time** (e.g. year-by-year points); see Section 2 anti-patterns. |
| Correlation (massive n, >500) | `geom_bin2d()` or `geom_hex()` | Avoids the "solid block of ink" problem; see **Hex/bin readability** below |
| Correlation with heavy overplotting (discrete x or dense bands) | `geom_hex()` / `geom_bin2d()`, or **highlight subset** (e.g. top N in color, rest grey) + trend line | When points pile up at similar x (e.g. many names per length), a plain scatter is confusing. Use hex/bin2d, lower alpha + clear `geom_smooth()`, or highlight a meaningful subset so the trend is clear without a solid block of ink. |
| Uncertainty / CI over time | `geom_ribbon()` + `geom_line()` | State what the band represents (SE, 95% CI, or min/max range). Use `alpha = 0.15–0.25`; match ribbon fill to line color. Never label as "error bars" without specifying the interval type. |
| Part-of-whole | `geom_col` (100% stacked) | Only if the part-whole relationship is the point **and** no single segment dominates (see Section 2: no stacked bars when one segment dominates) |
| Distribution across groups | `geom_violin` or `geom_boxplot` | Prefer violin for large n; keep groups few and consider log scale for skewed values so the shape is interpretable |
| Ranked comparison | `geom_col` (ordered) + `coord_flip()` | Sort by value via `fct_reorder` |
| Before vs. after (2 points per group) | `ggalt::geom_dumbbell()` | Much cleaner than grouped bars |
| Rankings over time | `ggbump::geom_bump()` | Shows rank changes without Y-axis clutter |
| Distributions (many groups) | `ggridges::geom_density_ridges()` | Better than 10 overlapping density plots |
| Flow / group transitions | `ggalluvial::geom_alluvial()` | For showing how groups shift between states |
| Sparse spatial events | `geom_point` or `geom_sf` overlay on base map | Use instead of filled grid when most cells are zero |
| Skewed spatial counts | `geom_sf(aes(fill = ...))` with `trans = "log1p"` | `sqrt` is often insufficient for extreme skew |
| Composition over time (shares) | `geom_area(position = "stack")` (stacked area) | Good when a few categories make up most of the total; see **Stacked areas** below |
| Derived ratios / rates | Compute in data, then line or bar | e.g. occurrence of x per y, growth rates — when they answer a clear question |

The following should only be used in very specific cases:

| **Flow (source → stages → destination)** | **Sankey / alluvial** | Rare but powerful when the story is *how* quantities move between stages; see **Sankey and flow diagrams** below |
| **Exactly two time points, rank or rate of change** | **Slope graph (slopegraph)** | Tufte-style; steepness shows rate of change; use when intermediate points add noise; see **Slope graphs** below |
| **Sequential breakdown (start → steps → end)** | **Waterfall** | E.g. revenue bridge, budget variance, YoY change decomposition; see **Waterfall charts** below |
| **Hierarchical part-whole (many categories, 2–3 levels)** | **Treemap** | When stacked bars or pies are too flat; limit depth for readability; see **Treemaps** below |
| **Performance vs target (dashboards)** | **Bullet chart** | Single metric vs target and bands; compact; use when space and context matter |

### Consider Derived Metrics and Richer Views

When the data supports it, **consider derived variables** that add insight beyond raw counts or sums: e.g. division by population or area, year-on-year change, or other metrics of x by y. These often merit their own chart (line or bar) and can answer questions like "How has intensity changed?" Compute them in a `mutate()` or `summarise()` step, then plot. Do not limit visuals to raw variables only when a ratio or rate is more informative.

**Diversity or "more/less over time" claims: normalize when total volume changes.** When claiming "diversity has increased" or "there are more X over time", total volume (births, population, counts) may have increased too, so raw counts can be misleading. Prefer or supplement with **normalized metrics** (e.g. distinct names per 10,000 births) so the claim reflects proportion or rate, not just scale. State in the subtitle when the metric is normalized (e.g. "Per 10,000 births").

### Look Beyond "Top N" When It Adds Insight

Do not default every chart to "top 10 by value." Often more interesting stories come from **other aspects of the data**:

- **Spikes and variation over time:** Names or series with sharp peaks, sudden rises or falls, or large variation over time (vs. stable series) can be more informative than a static "top N" snapshot. Consider highlighting spikes, turn points, or volatile vs. stable behaviour.
- **Meaningful subsets:** Mid-tier ranks, "fallen" items (e.g. formerly top that dropped), or balance-based subsets (e.g. truly gender-balanced unisex names rather than "any name with both M and F > 0") can tell a clearer story and match audience intuition.
- **Compare across categories:** When using a category, contrast items that fit the category well vs. those that don’t (e.g. balanced unisex names vs. strongly gendered names that happen to have a few of the other sex). Ensure the category definition is strict enough that the chart doesn’t contradict the label (see **Define categories so they match audience intuition**).
- **Stability vs. change:** Consider charts that emphasize which series are stable vs. which have large swings, or that compare growth rates / indexed series rather than only absolute levels.

Choose "top N" when it answers the question; otherwise consider these alternatives so the chart highlights what’s actually interesting in the data.

**Normalised or indexed series (e.g. base year = 100):** When comparing **development over time** across groups that have very different absolute levels (e.g. GDP per capita in Europe vs India), raw levels can be interesting obscure relative change. In addition to (or instead of) raw-level charts, consider **indexing each group to 100 in a reference year** and plotting the index over time. That makes it easy to see which group grew or fell more in relative terms. State the base year in the subtitle (e.g. "Index: 2016 = 100").

```r
# Example: per-capita rate alongside absolute counts
df <- df |>
  mutate(
    rate_per_100k = (events / population) * 100000,
    yoy_change    = value / lag(value) - 1
  )
```

### Consider Meaningful Grouping

When the data supports it, **consider showing variables by groups**. Ask: Is there a categorical variable (region, type, sector, income group) that might explain variation in the Y variable? Grouping by it — via color, facets, or both — often reveals the story hidden in the aggregate. Common grouping variables to consider:

- **Geography:** continent, region, country income group (LIC/MIC/HIC)
- **Time period:** before/after a policy change or event
- **Entity type:** sector, industry, party, demographic group

Do not automatically group by every variable available — only if it adds a clear insight. If grouping produces too many series to read, use `facet_wrap()` or highlight a subset with the rest in grey.

### Define Categories So They Match Audience Intuition

When a chart uses a **category label** (e.g. "high-income", "at-risk"), the definition must be **explicit and aligned with what readers expect**. Vague or overly broad definitions produce misleading charts.

- **Avoid "any presence" rules when the label implies balance or mix.** For example, "unisex names" is widely understood as names used meaningfully for both sexes. Defining it as "names with at least one birth in both M and F" pulls in names that are 99% one gender (e.g. Liam, Olivia), which readers will find wrong. Instead, define with **explicit thresholds**, e.g., but not necessarily:
  - **Balance:** e.g. neither sex accounts for more than 80% (or 70%) of that name's total in the relevant period; or the smaller share is at least 10% (or 20%).
  - **Minimum presence:** e.g. at least 50 (or 100) births in each sex for that name in the year or period.
- **Validate before plotting:** After applying the definition, spot-check that the selected items fit the label. If typical examples would surprise the reader (e.g. a "unisex" list dominated by strongly gendered names), tighten the definition or use a different label and state the rule in the subtitle (e.g. "Unisex: names with 20–80% female in this year").
- **State the definition when it's not obvious:** For any constructed category, if the threshold or rule is non-obvious, add a short note in the subtitle so the chart is self-explanatory.
- **For topics without clear intuition, ask the user for guidance.** It's preferable to have clearer instructions that make up confusing categories.
### Stacked Area and Other Composition Charts

**Stacked area charts** (`geom_area(position = "stack")`) are useful for showing how the **composition** of a total changes over time (e.g. share of value by group, or by type). Use them when:

- There are a **small number of categories** (e.g. 3–6) that sum to a meaningful total. **If very few groups dominate,** they can be shown separately with all other groups grouped into 'others', or grouped together based on another variable (e.g. emissions over time can be shown as USA, China, Europe, ROW).
- The focus is on **shares or part-of-whole over time**.
- When the main message is **absolute levels** of each category — prefer grouped lines or small multiples.

**Labels on stacked area charts must always appear on the right, inside or just outside the final stack segment — never in a legend.** Use `geom_text()` at the final x value positioned within each band, or `ggrepel::geom_label_repel()` if bands are thin. See Section 4 for the pattern.

**Stacked area: fixed category set and complete data.** Stacked area charts require a **fixed set of categories** (the fill variable). For every x value there must be **exactly one row per category** (use 0 or explicit NA for missing). If the composition set **changes over time** (e.g. "top 5 names each year" where the top 5 differ by year), do **not** use a single stacked area — you get jagged, unreadable bands because categories appear and disappear. Instead: (a) use a **fixed** set of categories (e.g. top 5 in the final year, with zeros for years before they appear), or (b) use a line chart of shares for that fixed set. Ensure one row per (x, category) after aggregation.

### Stacked Bars: When One Segment Dominates

**Do not use a stacked bar when one segment is much larger than the others** (e.g. day vs night fires where night is a tiny share). The small segment becomes unreadable and cannot be compared across groups. Prefer: **grouped (side-by-side) bars** so each segment has its own bar; **faceting by segment** (one panel per segment) if you need to compare across groups; or **percentage (100% stacked)** so all segments are on a comparable scale. See Section 2 (Never list).

### Points on Line Charts: When to Add, When to Omit

Adding `geom_point()` to a `geom_line()` is not always an improvement.

| Situation | Recommendation |
|---|---|
| Sparse data (< ~8–10 points per series) | Add `geom_point()` — individual observations deserve emphasis |
| Dense data (≥ 10 points per series, e.g., annual data over 15+ years) | **Omit points** — the line shape carries all the information; points clutter without adding insight |
| Irregular time intervals (gaps in data) | Add points to make the gaps visible |
| A single notable observation needs calling out | Use `geom_point()` only on that observation, not the entire series |

```r
# Dense annual time series — line only
geom_line(linewidth = 0.9, color = "#D55E00")

# Sparse series (< 8 points) — line + points justified
geom_line(linewidth = 0.9, color = "#D55E00") +
geom_point(size = 2.5, color = "#D55E00")
```

### When to Reach Beyond the Defaults

Before defaulting to a bar chart, line chart, or filled map, ask:

- **Is the spatial data sparse?** A filled-cell map where 95% of cells are zero is nearly unreadable. Use a point/bubble overlay — only cells with events get a mark.
- **Is one category extreme?** If one bar is 5× the next, consider a dot plot or log scale, which shows relative differences more honestly.
- **Are you comparing two time points?** `ggalt::geom_dumbbell` shows before/after per group far more clearly than grouped bars or overlapping lines.
- **Do you have many groups changing over time?** `ggbump::geom_bump` reveals rank changes without cluttered intersecting lines.
- **Is the distribution shape interesting?** A mean bar hides everything. Use `geom_violin` or `ggridges::geom_density_ridges` for many groups.

### Concrete Alternatives with Code

**Sparse spatial data: point overlay instead of filled grid**

```r
# Bad: fills every cell, 95% near-zero → unreadable
ggplot(grid_sf) +
  geom_sf(aes(fill = fatalities))

# Good: base map + points only where events occurred
events_sf <- grid_sf |> filter(fatalities > 0)

ggplot() +
  geom_sf(data = africa_sf, fill = "grey92", color = "white", linewidth = 0.2) +
  geom_sf(
    data  = events_sf,
    aes(size = fatalities, color = fatalities),
    alpha = 0.6, shape = 16
  ) +
  scale_size_continuous(
    name   = NULL,
    range  = c(0.5, 8),
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  scale_color_viridis_c(
    name   = NULL,
    trans  = "log1p",
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  guides(size = guide_legend(), color = guide_legend()) +
  coord_sf() +
  theme_void(base_family = "sans")
```

**Dominant outlier in a bar chart: dot plot with log scale**

```r
# When one bar dwarfs all others, a dot plot on log scale shows
# relative differences more honestly than raw bars
ggplot(df, aes(x = fct_reorder(country, fatalities), y = fatalities)) +
  geom_segment(aes(xend = country, y = 1, yend = fatalities),
               color = "grey70", linewidth = 0.5) +
  geom_point(size = 3, color = "#0072B2") +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  coord_flip() +
  theme_clean() +
  labs(subtitle = "Log scale; each step represents a 10× increase")
```

**Before/after comparison: dumbbell chart**

For dumbbell (and similar before/after) charts, **year or category labels (e.g. "2015", "2017") must sit at the correct endpoint**—each label next to its own dot, not in a single shared position. If you show "2015" and "2017" once, place "2015" next to the left (or first) dot and "2017" next to the right (or second) dot so it is unambiguous which dot is which year. Label color should match the dot color.

```r
library(ggalt)

df_wide <- df |>
  filter(year %in% c(2000, 2020)) |>
  tidyr::pivot_wider(names_from = year, values_from = value, names_prefix = "y")

ggplot(df_wide, aes(x = y2000, xend = y2020, y = fct_reorder(country, y2020))) +
  ggalt::geom_dumbbell(
    color       = "grey70",
    colour_x    = "#0072B2",
    colour_xend = "#D55E00",
    size        = 1.2
  ) +
  theme_clean()
```

**Many distributions: ridgeline plot**

Ridgeline plots can become messy when there are too many groups or overlapping curves. Use them only when the number of groups is small (e.g. ≤ 6–8), and tune `scale` and `rel_min_height` so ridges do not overlap excessively. If the result is cluttered, prefer **faceted densities** or **boxplots/violins** instead.

```r
library(ggridges)

ggplot(df, aes(x = value, y = fct_reorder(category, value, median),
               fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = NULL, guide = "none") +
  theme_clean()
```

**Hex and bin2d plots: keep values readable**

For `geom_hex()` and `geom_bin2d()`, the fill scale (counts or density) can make the plot unreadable if labels are huge numbers or too many bins. Use **formatted labels** (e.g. `scales::label_number(scale_cut = cut_short_scale())` or `scales::comma`) on the fill scale, a **reasonable number of bins** (e.g. 30–80 for hex so cells are distinguishable), and a **clear color scale** (e.g. viridis). If the count range is extreme, consider a log or sqrt transform on the fill and state it in the legend or subtitle.

### Scatterplots and trend lines

When using `geom_smooth()` (or any fitted line) on a scatterplot, readers should know **how the line was computed** and what the shaded band represents. An unexplained curve may look arbitrary.

- **State the method and band in the subtitle (or caption).** Examples: "LOESS smooth, 95% confidence interval"; "Linear fit; shaded area = 95% CI"; "GAM smooth, 1 SE."
- **Always set `method` explicitly on `geom_smooth()`.** The default switches from LOESS (n < 1000) to GAM (n ≥ 1000) automatically, which can produce inconsistent results. Set it explicitly every time: `geom_smooth(method = "loess", se = TRUE)` for LOESS or `method = "lm"` for a linear fit. State the method and what the band represents in the subtitle.
- **Do not add a trend line or confidence interval when the story is the path.** For scatter plots that show a **trajectory over time** (e.g. one point per year, points connected in order), the narrative is the year-by-year path. Adding `geom_smooth()` (linear fit, 95% CI, etc.) obscures that path and can imply a single relationship that is not the focus. Use trend/CI only when the goal is explicitly to show a regression or trend model.

### Bar charts when one category dominates (counts)

When a **single bar or segment is much larger than all others** (e.g. "Valid" vs "Invalid" in election votes, etc.), the smaller bars become unreadable — their lengths are hard to compare and values cannot be estimated from the axis.

- **Add direct value labels** on or at the end of each bar (`geom_text(aes(label = scales::comma(value)))`) so small categories remain interpretable even when their bars are tiny.
- **Alternatively:** show a **separate plot or inset** that zooms in on the smaller categories with an appropriate scale (e.g. bar chart of only non-dominant vote types).
- **Or:** group the smallest categories into "Other" only if that keeps "Other" among the smallest; otherwise prefer labels or a second view.
- Do **not** rely on a single sequential fill (e.g. one blue for all bars) when the goal is to compare distinct categories — use distinct colors per category or value labels so each bar is readable.

### 100% stacked bar with one dominant segment

For a **single 100% stacked bar** (one row of data), if one segment is >90% of the total, the other segments become thin slivers that are hard to see or compare.

- **Prefer a bar chart of shares (percentages)** with **direct value labels** on each bar so all categories are legible.
- **Or** use a **separate small chart or table** for the minor categories.
- **Y-axis:** If the plot is a single 100% stacked bar, the non-percentage axis (e.g. y) often has no meaningful scale — omit it or use a single category label; do not show a numeric y-axis (e.g. 0.8–1.2) that implies a scale that does not exist.

### Choosing chart type by data and goal

- **Geographic units (regions, sections, districts):** If boundary data exists, a **choropleth map** often reveals spatial patterns better than a long bar chart. Consider a map when the question is "where" as well as "how much."
- **Many categories with a long tail:** Prefer **top N** with "Other", or a **dot plot** (lollipop) for cleaner reading than many thin bars. For grouped bars (e.g. sections × parties), if most sections have small totals, consider showing only top sections or faceting.
- **Composition with one dominant category:** Prefer **bar of shares with value labels** or an **inset/separate view** for small categories; avoid a single stacked bar where small segments are invisible.
- **Two continuous variables:** Scatter with a trend line is appropriate; **always state the trend-line method and CI** in the subtitle. **Exception:** when the scatter shows a **path over time** (e.g. year-by-year), omit the trend line and CI; the story is the trajectory.

---

## 2. Visual Anti-Patterns (The "Never" List)

If the data or user request implies one of these, **suggest the alternative rather than silently complying**.

- **No 3D elements.** Depth distorts the mapping of a point to an axis.
- **No pie charts.** Humans judge length far better than angle or area. Use an ordered horizontal bar chart.
- **No dual Y-axes.** `sec_axis()` implies a relationship between two scales that is almost always arbitrary. Use two separate panels.
- **No unreadable spaghetti.** Many lines are fine *if* most are faint grey with a few colored highlights and right-margin labels. Otherwise use `facet_wrap()`.
- **No truncated Y-axes on bar charts.** `geom_col` and `geom_bar` must start at zero.
- **No rainbow color scales for continuous data.** For sequential continuous data use `scale_fill_viridis_c()` (perceptually uniform, colorblind-safe); for diverging data use `scale_fill_distiller(palette = "RdBu")`; for qualitative nominal categories use the Okabe-Ito palette (see Section 6).
- **No overplotted scatterplots.** For more than ~500 points, use `geom_hex()` or `alpha`.
- **No flat-color bar charts without hierarchy.** A ranked bar chart of 20 countries in identical grey is harder to scan. Use a sequential fill tied to value, or highlight bars of interest.
- **No chartjunk.** No decorative 3D, gradients, or redundant visual elements. Every element should earn its place.
- **No trend line or confidence interval when the story is the path.** For scatter plots that show a trajectory over time (e.g. one point per year, points connected in order), do not add `geom_smooth()`—it obscures the actual path. Use trend/CI only when the goal is explicitly a regression or trend model.
- **No legend titles unless genuinely needed.** Default to `name = NULL` across all scale types. See Section 4 for when a title is warranted (rare).
- **No mid-series labels on line or stacked area charts.** Labels must be at the rightmost data point, not floating mid-series. Maps and scatter plots use legends instead — but still without a title.
- **No tilted or rotated axis text.** This includes category names on the x- or y-axis (e.g. region names, long labels). Angled text is hard to read and looks unpolished. Use `coord_flip()`, shorten labels in the data, or show fewer categories — never `element_text(angle = 45, ...)` or similar.
- **No stacked bars when one segment dominates.** If one part is much larger than the others (e.g. day vs night fires where night is a tiny share), stacked bars make the small segments unreadable and prevent fair comparison. Use grouped (side-by-side) bars, facet by segment, or show percentage composition so segments are comparable.
- **No reusing the same color for different nominal categories.** In a bar or legend, each distinct category (e.g. party, vote type) must have a distinct color. Do not map multiple categories to the same fill when they are not grouped; use a qualitative palette with enough levels (e.g. `scale_fill_brewer(palette = "Set2")` or custom) so every series is distinguishable.
- **No mixing variables with very different scales on one plot.** Putting several control or context variables (e.g. urban share, crop share, elevation, road density) on a single bar chart with one y-axis makes most of them visually irrelevant because one scale dominates. Use **separate panels** (`facet_wrap(~ variable)`) or **separate plots** per variable, possibly with a shared x (e.g. region).

---

## 3. Typography: Read, Don't Tilt

### Orientation

Keep **all** text horizontal — including axis tick labels (categories on x or y, e.g. region names, variable names). Tilted text is a no-go.

- If X-axis labels overlap or are long, **flip the chart** so categories appear on the y-axis and read left-to-right.
- If flipping is not appropriate, **shorten labels in the data** or show fewer categories; do not rotate text.

```r
# Bad — tilted text anywhere (axis, legend, etc.)
theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Good
coord_flip()
# Or: shorten category labels and keep horizontal
```

### Bar Charts with Many Categories

When a bar chart has more than ~15 categories, or when **grouped bars** (e.g. sections × parties) create many small, hard-to-read bars:

- **Reduce the set:** Show only the top N and note exclusions in the subtitle. For grouped bars (e.g. votes by section and party), consider showing only the top N sections by total votes so bar lengths remain readable.
- **Scale chart height:** The chart height must grow with the number of categories — never shrink fonts to compensate. Minimum font floor: `rel(0.7)` (~9pt at base 13).
- **Consider a dot plot** for very long lists — it reads more cleanly at small sizes than fat bars.
- **When many bars are tiny:** If a few categories dominate and the rest are very short, add **direct value labels** on each bar (`geom_text(aes(label = value))`) so small bars remain interpretable; or show a **zoomed-in second plot** or **facet** for the smaller categories.

```r
n_cats <- n_distinct(df$country)
ggsave("output.png", width = 10, height = max(6, n_cats * 0.28), dpi = 300, bg = "white")
```

### Direct value labels on bars

When precise values matter or when some bars are very small, add **numeric labels** on or at the end of each bar. This avoids readers having to estimate from the axis and makes small segments readable.

```r
# Add value labels at end of bar (for horizontal bars, after coord_flip, x is the value)
geom_text(aes(label = scales::comma(value)), hjust = -0.15, size = 3) +
# Or for percentages
geom_text(aes(label = scales::percent(value, accuracy = 0.1)), hjust = -0.15, size = 3)
```

### Bar Charts: Combine Low-Value Categories into "Other"

Ranked bar charts with many categories often have a long tail of small values that add clutter without insight. **Combine low-value categories into a single "Other" (or "All others combined") row only when that pooled category remains among the smallest** — i.e. "Other" should appear at or near the bottom of a ranked bar chart, not at the top.

- **Do not lump into "Other" if the pooled category would become the largest (or among the largest).** If "Other" would rank first or second after reordering, the aggregation is misleading: either show more individual categories (increase top N), or use a different approach (e.g. "Top 15" with no Other, or a dot plot).
- When lumping is appropriate: define a threshold (e.g. top 8–12 by value), sum the rest into "Other," and **reorder so "Other" is last or near-last**. 
- When lumping is appropriate but "Other" group is too large - consider splitting it based on meaningful grouping values (e.g. regions).
- Mention in the subtitle that small categories are grouped (e.g. "Small categories grouped as 'Other'").

```r
# Example: keep top 10, pool the rest — only valid if "Other" ends up among smallest
top_n <- 10
df_plot <- df |>
  mutate(category = fct_lump_n(category, n = top_n, w = value, other_level = "Other")) |>
  summarise(value = sum(value), .by = category) |>
  mutate(category = fct_reorder(category, value))   # Other will be last only if its sum is small
# Before committing: check that slice_tail(df_plot, n = 1)$category == "Other" (or near-bottom)
```

### Shorten Long Labels (Axis, Legend, and On-Plot)

Long labels hurt readability everywhere: axis ticks, legend entries, and direct labels on the plot.

- **Shorten in the data or in the scale:** Prefer concise text. If a label contains parenthetical detail (e.g. "Renewables (hydro, wind, biofuels)") or is verbose, shorten it (e.g. "Renewables") and put the detail in a subtitle or caption only when necessary.
- **Avoid wrapping long labels on the axis** — it rarely looks clean. Use a short canonical label. Use **abbreviations or canonical short forms** where the audience will understand them; define any non-obvious abbreviation in the subtitle or caption if needed.
- **Prefer Title Case or sentence case for axis/legend labels** instead of ALL CAPS when the data source uses caps; ALL CAPS reduces readability for long names.
- If the full phrase is essential, use it in the title or subtitle, not as the axis tick or legend label. Apply the same rule to **numeric or value labels** placed on the plot: keep units and wording brief.
- **Domain-specific or foreign terms:** If you use terms like POSITIVO, NULO, EN BLANCO, or "mesa", define them in the subtitle (e.g. "POSITIVO = valid votes; NULO = null; EN BLANCO = blank; mesa = polling station") so the chart is self-explanatory.

```r
# Good: shorten for display
df <- df |> mutate(
  origin = case_when(
    colonial_origin == "Never colonized by a Western overseas colonial power" ~ "Never colonized",
    TRUE ~ colonial_origin
  )
)

# Good: abbreviate long names for axis; define in subtitle if needed
df <- df |> mutate(
  country_name = case_when(
    country_name == "United States of America" ~ "USA",
    country_name == "Democratic Republic of the Congo" ~ "DRC", # or DR Congo
    TRUE ~ country_name 
  )
)
```

### Safe Font Defaults

- **Always default to `base_family = "sans"`** — maps to Arial/Helvetica and never fails.
- **Do not use `theme_ipsum()` or any `hrbrthemes` function** unless the font is confirmed installed.

### Font Hierarchy

| Element | Style | Size (relative to base) |
|---|---|---|
| Title | Bold | base + 4pt |
| Subtitle | Regular | base |
| Axis tick labels | Regular | base − 1pt |
| Caption/Source | Regular, grey50 | base − 3pt |

Minimum `base_size = 12` for screen. Use 13–14 for presentations.

---

## 4. Labeling & Legends

### The Two Core Rules: Where Labels Go, and Legend Titles

**Rule 1 — Legend titles are almost always suppressed (`name = NULL`).**
Whether the legend is a color swatch, a colorbar, or a size scale, the title is almost never needed. The chart title, subtitle, and category labels already communicate what the encoding means. Only add an explicit legend title when the unit or concept is genuinely non-obvious and not stated anywhere else (e.g. a continuous colorbar for an unusual index).

```r
# Default — suppress the title in almost every case
scale_fill_manual(name = NULL, values = pal)
scale_color_viridis_c(name = NULL)
scale_size_continuous(name = NULL)

# Exception — title only when the unit is essential and absent from title/subtitle
scale_fill_viridis_c(name = "Fatalities per 100k")
```

**Rule 2 — Choose between direct right-side labels OR a legend based on chart type.** Do not mix both.

| Chart type | Preferred approach | Notes |
|---|---|---|
| Line chart (≤ 8 series) | **Direct labels at right** (max x) + no legend | Most readable; see pattern below |
| **Faceted line chart** (few series per panel) | **Direct labels at right** in each panel when feasible (when there are few facets) | Do not default to a legend just because the chart is faceted; if each panel has a clear rightmost x and few series, use direct labels there too |
| Stacked area chart | **Direct labels at right** (final band midpoints) + no legend | See stacked area pattern below |
| Map (choropleth, fill) | **Legend** (bottom, horizontal) | Spatial encoding can't use right-side labels |
| Map (qualitative, many zones) | **Legend** (bottom, multi-column) | `guide_legend(ncol = 4)` |
| Scatter / bubble | **Legend** | Points don't have a clear "end" to label |
| Bar chart (grouped) | **Legend** (top-left) | Bars are self-contained; legend is natural |
| Many lines (> 8 series) | **Legend** or facets | Too many right-side labels become unreadable |

The key distinction: **if the series has a clear rightmost endpoint (including within each facet), prefer labeling at that endpoint** over a legend. Use a legend only when there is no natural "end" (maps, scatter, bars) or when there are too many series to label cleanly (> 8).

### Direct Labeling (Preferred for Line and Stacked Charts)

Label lines and stacked bands at the end of the series for **any** multi-series chart. Use `ggrepel::geom_text_repel()`.

- **Always use `segment.size = 0`.** Do not draw grey (or any) connector lines from the line to the label; they clutter the chart and look unpolished. If labels overlap, give them more room: increase the right-side expansion and/or `nudge_x`.
- **Reserve enough space on the right** so labels are not cut off. Use at least `expansion(mult = c(0.02, 0.22))`; for many or long labels, use 0.25–0.3.
- **Suppress the legend** when direct labels are used: `theme(legend.position = "none")`.
- **Label each series at its last available data point.** When a series does not extend to the global max(x) (e.g. a name that drops out of the data), place the label at the **last x for which that series has data**, not only at `max(year)` overall. Otherwise some series have no label. Compute label data as e.g. `df |> slice_max(year, n = 1, by = series_id)` and use that for `geom_text_repel`.
- **Color labels to match the data:** Direct labels (category names or values) should use the **same color** as the line, bar, or point they refer to. Do not use uniform black for all labels when series have distinct colors—readers should not have to guess which label goes with which element.
- **Place labels at the end of the element:** Position each label immediately at (or just beyond) the end of its line, bar, or segment—e.g. at the rightmost point of a line, at the tip of a bar. Avoid placing a label in a generic position (e.g. one legend-style block) when it could be read as belonging to the wrong series or endpoint.

```r
library(ggrepel)

# Direct labels — no connector segments; legend suppressed; enough right margin
geom_text_repel(
  data         = \(d) filter(d, year == max(year)),
  aes(label    = country),
  nudge_x      = 1.5,        # in data units; scale with x range (~5–10% of span)
  direction    = "y",
  segment.size = 0,          # never use connector lines
  hjust        = 0,
  size         = 3.5
) +
scale_x_continuous(expand = expansion(mult = c(0.02, 0.25))) +
theme(legend.position = "none")
```

> ⚠️ **`nudge_x` is in data units, not pixels.** For a year axis spanning 1997–2019 (22 units), `nudge_x = 0.5` barely moves labels and risks clipping; use `nudge_x = 1.5` or more and right expansion ≥ 0.22. Calibrate to the actual x range each time — never copy-paste the same value across charts with different x scales.

### Direct Labeling for Stacked Area Charts

Stacked area charts need labels placed within or beside the final segment of each band — not in a legend. The cleanest approach is to compute label positions at the last time point and place text at the **midpoint of each band**. **Label y-positions must match the visual stack exactly** — if a label appears in the wrong band (e.g. "Olivia" floating above or below its band), the ordering used for the cumulative sum does not match `geom_area()`'s stack order.

- **Stack order is determined by the fill variable's factor level order:** the first level is the bottom band, the last level the top band. Set the fill variable to a factor with levels in the order you want the stack (e.g. smallest to largest at the final x, or a fixed order).
- **In `label_df`, use the same order:** arrange rows in the **same** order as the factor levels (e.g. `arrange(category)` if `category` is already a factor with the desired level order). Then `cumsum(value)` gives the top edge of each band from bottom to top; `midval = cumval - value / 2` is the vertical center of each band.
- **Verify:** After plotting, check that each label sits inside the correct colored band. If any label is in the wrong band, the order used in `label_df` does not match the fill factor order — fix by making the factor levels match the intended stack and using that same order in `label_df` (e.g. `arrange(category)` with no `desc()` if the first level is the bottom band).

```r
# Compute stacked positions at the final x value for label placement
library(dplyr)
library(ggrepel)

# 1. Define stack order via factor: e.g. smallest share at bottom at final year
order_at_end <- df |> filter(year == max(year)) |> arrange(value) |> pull(category)
df <- df |> mutate(category = factor(category, levels = order_at_end))
df <- df |> arrange(year, category)

# 2. Label df: same order as factor (first level = bottom band)
label_df <- df |>
  filter(year == max(year)) |>
  arrange(category) |>
  mutate(
    cumval = cumsum(value),
    midval = cumval - value / 2
  )

ggplot(df, aes(x = year, y = value, fill = category)) +
  geom_area(position = "stack", alpha = 0.85) +
  geom_text(
    data  = label_df,
    aes(x = max(df$year), y = midval, label = category, color = category),
    hjust = -0.1,
    size  = 3.5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.18))) +
  scale_fill_brewer(palette = "Set2", name = NULL, guide = "none") +
  scale_color_brewer(palette = "Set2", guide = "none") +
  theme_clean() +
  theme(legend.position = "none")
```

> ⚠️ **Stack order and label order must match.** ggplot stacks by the fill variable's factor level order. The `arrange()` in `label_df` must produce the same order (first level first, then cumsum). If you use `arrange(desc(category))`, that only matches the stack when the factor's first level is the *largest* (top band). Prefer setting the factor levels explicitly (e.g. smallest-to-largest at final x) and then `arrange(category)` so cumsum runs bottom-to-top.

> ⚠️ **Thin bands are hard to label inside.** If a band is very thin (< ~5% of the total), place the label outside the stack to the right using `geom_text_repel()` with `nudge_x` pointing right, rather than inside the band. Alternatively, merge small categories into "Other."

> **When using direct labels** (lines, stacked areas), also remove the legend body entirely since labels replace it: `theme(legend.position = "none")` or `guide = "none"` in the scale.
**


### Line Charts: Prefer Direct Labels Over a Legend

For multi-line time series (and similar line charts), **prefer direct labels at the end of each line** instead of a legend whenever the number of series is modest (≤ 8). Apply consistently: not only for "country" charts but also for "fatalities by event type," "by region," or any other categorical series. If there are many series (> 8), a legend is acceptable — but still with `name = NULL` and `legend.position = "top"` or `"bottom"`, not the default right-side float. Use `facet_wrap()` as the alternative for very many series.

### Multi-Column Legends

When a qualitative legend has many items (e.g., 15+ climate zones), a single-column layout is too tall. Use `guide_legend(ncol = ...)` to wrap into rows.

```r
scale_fill_manual(
  name   = NULL,    # still no title
  values = pal,
  guide  = guide_legend(ncol = 4, byrow = TRUE)
) +
theme(
  legend.position = "bottom",
  legend.key.size = unit(0.7, "lines"),
  legend.text     = element_text(size = rel(0.8))
)
```

### Units and Number Formatting

Always format axis numbers. Raw values like `1000000` are harder to read than `1M`.

```r
library(scales)

scale_y_continuous(labels = scales::comma)
scale_y_continuous(labels = scales::label_dollar())
scale_y_continuous(labels = scales::percent)
scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()))  # 1.2M, 3.4B
scale_x_date(date_labels = "%b %Y")
```

> ⚠️ **`scales::label_number_si()` was removed in scales 1.3.0.** Use `label_number(scale_cut = cut_short_scale())` instead.

### Axis Titles and "Explainer" Text

**Do not use axis titles.** Put variable names and units in the title or subtitle.

**Do not automatically add explainer text** (e.g., "Sum of X", "Count of Y", "Grid from X joined with Y by gid"). Technical pipeline descriptions belong in a methods section, not a chart subtitle. Only add a note when the aggregation or measurement is genuinely non-obvious.

```r
theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)
```

---

## 5. Layout: Small Multiples vs. Overlays

- **Many lines, one frame:** Faint grey context lines (`linewidth = 0.4–0.6`) with a few colored highlights (`linewidth = 0.9–1.0`) and right-margin direct labels.
- **Top-N time series:** For "top N" line charts (e.g. top 5 countries), **consider showing many series in grey as context and highlight only the top N** in color with direct labels. Plot all series (or a large subset) in grey first, then overlay the top N in bold color with right-side labels — this gives context and avoids a chart that shows only the top N with no comparison.
- **The Overlap Test:** If trends are still obscured, use `facet_wrap(~category)`.
- **Facets with natural pairs (e.g. Male/Female per entity):** When faceting by a variable that has **natural pairs** (e.g. same name Male vs Female), (a) **put paired levels side by side** — e.g. facet so "Oliver (M)" and "Oliver (F)" are adjacent, not scattered. (b) **Avoid panels that add no insight:** if one variant is negligible (e.g. "Oliver (F)" with tiny counts), consider showing only the variant where the measure is meaningful, or one panel per entity with two lines (M vs F) instead of one panel per (entity, variant).
- **Consistent scales:** Keep Y-axis scales identical across facets. Only use `scales = "free_y"` when you need to show within-panel variation (e.g. each country on its own scale). When using `scales = "free_y"`, **do not use jargon in the subtitle.** Write in plain language (e.g. "Each panel uses its own vertical scale so trends within each group are visible") rather than "Free Y-axis" or "free_y."
- **Facet strip labels:** Strip text is left-aligned and bold in `theme_clean()`. For long category names, wrap with `labeller = label_wrap_gen(width = 20)`. For panels where showing the variable name adds context, use `labeller = label_both` (e.g. "income_group: High").

```r
facet_wrap(~ country, labeller = label_wrap_gen(width = 20))
facet_wrap(~ income_group, labeller = label_both)
```
- **The sawtooth diagnostic:** Jagged `geom_line()` means the data needs aggregating — go back to Section 0, Step 3.
- **Consider transformations:** For heavily right-skewed data (conflict fatalities, population), a log scale reveals far more structure. State the transformation in the subtitle.

```r
# Log scale for skewed time series
scale_y_continuous(
  trans  = "log10",
  labels = scales::label_number(scale_cut = scales::cut_short_scale())
)

# Small multiples
ggplot(df, aes(x = year, y = value)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~country, ncol = 3) +
  theme(strip.text = element_text(face = "bold"))
```

---

## 6. Color: Meaning & Accessibility

### Palette Selection by Data Type

| Data type | Recommended scale | Example use |
|---|---|---|
| Qualitative (categories) | **Okabe-Ito palette** (see below) or `scale_fill_brewer(palette = "Set2")` for ≤ 8 categories. **Use a distinct color per category** — never reuse the same color for different nominal categories. | Country, party, sector, vote type |
| Sequential (magnitude) | `scale_fill_distiller(palette = "Blues", direction = 1)` | Population size, temperature |
| Diverging (above/below zero) | `scale_fill_distiller(palette = "RdBu")` | Growth vs. decline |
| Continuous (general) | `scale_fill_viridis_c()` | Heatmaps, density |

For diverging scales, always ensure the midpoint maps to white or light grey — not a saturated color.

### The Okabe-Ito Palette (Default for Qualitative Data)

The Okabe-Ito set is the standard colorblind-accessible qualitative palette (endorsed by Wilke's *Fundamentals of Data Visualization* and the R community). Use it as the default for any nominal categorical encoding with ≤ 8 groups.

```r
okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#000000"
)

# Apply
scale_color_manual(values = okabe_ito, name = NULL)
scale_fill_manual(values = okabe_ito, name = NULL)
```

For ≤ 3 highlighted series, prefer the three named colorblind-safe picks from the Highlight Strategy section (`#0072B2`, `#D55E00`, `#009E73`). For > 8 groups, consider collapsing smaller categories or using facets — no standard palette scales beyond 8 with sufficient contrast.

### Semantic Mapping

Match color to concept: financial loss = red, gain = green, public sector = blue. Never map "bad" outcomes to green or "good" outcomes to red (Stroop Effect).

### Redundant Encoding (Accessibility)

```r
aes(color = group, linetype = group)  # for lines
aes(color = group, shape = group)     # for points
```

### The Highlight Strategy

Grey for context, one or very few bold colors for the focus. Colorblind-safe highlights: `#0072B2` (blue), `#D55E00` (vermillion), `#009E73` (green). Often, one primary colour or very few for the main story if there are many categories in the data, everything else subdued so the eye goes to the message. Do not give every series an equally strong colour when the narrative has a specific focus. For "top N" line charts (e.g. top 5 names over time), prefer many grey context lines and only the top N in color with direct labels.

```r
df <- df |>
  mutate(highlight = ifelse(country == "Nigeria", "Nigeria", "Other"))

ggplot(mapping = aes(x = year, y = value, group = country)) +
  geom_line(data = \(d) filter(d, highlight == "Other"),   color = "grey85", linewidth = 0.5) +
  geom_line(data = \(d) filter(d, highlight == "Nigeria"), color = "#D55E00", linewidth = 0.95) +
  geom_text_repel(
    data         = \(d) filter(d, highlight == "Nigeria", year == max(year)),
    aes(label    = country),
    nudge_x      = 1.5,
    direction    = "y",
    segment.size = 0,
    hjust        = 0,
    size         = 3.5,
    color        = "#D55E00"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.25))) +
  theme(legend.position = "none")
```

The same grey/color logic applies to **bar charts** — highlight one or a few bars, grey the rest:

```r
# Single highlighted bar in a ranked bar chart
ggplot(df, aes(x = fct_reorder(country, value), y = value,
               fill = country == "Nigeria")) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("TRUE" = "#D55E00", "FALSE" = "grey75"),
                    guide = "none") +
  coord_flip() +
  theme_clean()
```

### Consistent Color Assignment Across Charts

When a report or analysis contains multiple charts that share the same categories (e.g. regions, parties, income groups), **colors must be consistent across charts**. Define a named color vector once and reuse it everywhere. This prevents the same category appearing in different colors across panels.

```r
# Define once at the top of your script
region_colors <- c(
  "Africa"  = "#0072B2",
  "Asia"    = "#D55E00",
  "Europe"  = "#009E73",
  "Americas"= "#E69F00"
)

# Reuse in every chart — ggplot matches by name
scale_fill_manual(values = region_colors, name = NULL)
scale_color_manual(values = region_colors, name = NULL)
```

Any category absent from the named vector will appear as grey, which is usually the right behaviour for "Other."

---

## 7. Titles & Context

### Editorial principles (FT, Economist, BBC, Burn-Murdoch)

- **Chart as answer:** Every chart should answer one clear question. The title should state that answer (finding-first), not just describe the axes (e.g. "Child mortality fell faster in Sub-Saharan Africa after 2000" not "Child mortality vs region over time"). 
- **Minimalism / no chartjunk:** Avoid decorative elements; include only what is necessary to tell the story. No 3D, no gratuitous gradients, no redundant grid lines.
- **Colour restraint:** Use a relatively limited palette: few primary colours for the main data, if necessary only one — especially if there are many categories in the data; greys for context and secondary series. Avoid rainbow or many equal-weight colours when one highlight colour plus grey would do.
- **Background:** For a publication-ready, warm feel, consider an off-white background instead of pure white; the Economist uses this. Default `theme_minimal()` white is fine, otherwise use `"#F9F7F4"` (warm off-white) or `"grey97"` (cooler neutral): `theme(plot.background = element_rect(fill = "#F9F7F4", color = NA))` and `ggsave(..., bg = "#F9F7F4")` — the `bg` value must match the theme fill.

### Title Hierarchy for Agents

1. **User provides a title** → use it, don't override.
2. **A clear trend is visible** → write a finding-first title: `"[Variable] has [increased/decreased] significantly since [Year]"`
3. **Exploratory / unclear** → descriptive title with an insight placeholder in the subtitle.

| Type | Example |
|---|---|
| Weak (descriptive) | "Life Expectancy vs. GDP per Capita" |
| Strong (informative) | "Higher GDP strongly correlates with longer life expectancy" |
| Strong (specific) | "Child mortality fell faster in Sub-Saharan Africa after 2000" |

### Title Length: Fit the Canvas

> **A title that overflows the right edge is silently cut off in the PNG output.** There is no error — the chart just looks broken.

At `width = 10` inches and `base_size = 13`, a title fits comfortably at roughly **80 characters or fewer**. Count before committing. For longer titles, either:

- **Shorten the wording** (preferred — forces clarity), or
- **Insert a manual line break** with `\n`.

```r
# Too long — will overflow at width = 10
title = "Nigeria has the highest average annual conflict fatalities among focus countries"

# Fixed — shortened
title = "Nigeria leads on average annual conflict fatalities"

# Fixed — manual line break
title = "Nigeria has the highest average annual conflict\nfatalities among focus countries"
```

### Title Accuracy: Verify the Claim Against the Data

> **Never write a finding-first title from assumption.** Always derive the claim from the data before writing it.

A title like "X has the highest Y", "Y fell to 21%", or "Y rose sharply after 2010" makes a factual claim. That claim must match the **actual values in the data**—and, where relevant, what the reader sees on the chart (e.g. if the title says "fell to 21%", the line or bar must actually reach or clearly show 21%). Do not state specific numbers or comparisons that the plot does not support. It's better to have a descriptive title than an incorrect one.

```r
# Derive the claim before writing the title
top_country <- focus_means |> slice_max(mean_fatalities, n = 1) |> pull(country)
# Then use top_country in the title string — not a hardcoded country name
title = paste0(top_country, " leads on average annual conflict fatalities")
```

If the data cannot be inspected at title-writing time, use a neutral descriptive title and note in the subtitle which country or value is the focus.

### Title: Colored Keywords When Few Categories

When there are **only 2–3 categories** (e.g. two years, two groups) and the title explicitly names them (e.g. "Coal supply shrank sharply from 2000 to 2020"), consider rendering those keywords in the title in the **same colors** as in the legend/plot (e.g. "2000" in blue, "2020" in red). That reinforces the link without repeating the encoding in the subtitle. Use sparingly and only when the title already mentions the categories. 

### Trend Claims: Verify Direction, Not Just Endpoints

**Never state a directional trend in the title without verifying the actual series in the plotted period.** Titles such as "X has risen since the 1980s" or "Y has fallen over the period" are factual claims about direction. If the series actually falls over that period, the title is wrong and undermines trust.

- **Before writing any "has risen", "has fallen", "increased since [year]", "declined over"**: Check the variable over the exact period shown (e.g. compare value at start vs end of the x range, or confirm the slope). If the direction is the opposite, do **not** use that title — use a neutral or accurate one (e.g. "Share of X over time" or "X has fallen since the 1990s" only if the data actually falls).
- **Time window matters:** "Since the 90s" can mean "from 1990 onward" or "starting in the late 90s." Ensure the claim matches the data in that window (e.g. if the rise starts only after 2000, avoid "rising from the 90s" unless the 1990s really show an increase).

### Subtitle and Caption Rules

- **Subtitle:** Key caveats, units, date range, and material exclusions only. Do **not** repeat information that is already clear from the plot: no color keys (e.g. "Blue = 2015, Red = 2017" when the legend or direct labels already show this), no restating axis definitions (e.g. "Final EC (ktoe) vs GDP (billion int'l USD)" when axes are labeled), and no data-pipeline or join logic. Use **plain language** — avoid jargon like "free_y" or "log1p" unless you briefly explain (e.g. "log scale").
- **Acronyms and domain terms:** If the title or axis uses acronyms (e.g. CDU, SPD) or domain-specific terms (e.g. mesa, POSITIVO, NULO), **define them once** in the subtitle or caption so the chart is self-explanatory (e.g. "CDU = Christian Democratic Party; mesa = polling station"). If there are many acronyms, do not explain all of them in the subtitle or caption.
- **Caption / Source:** Always include a source. Prefer a **human-readable source** (e.g. "Source: ACLED", "Source: World Bank Open Data") when known. **Do not hallucinate** a source; a minimal, accurate reference is better than a made-up one.

```r
labs(
  title    = "Finding-first title",
  subtitle = "Units, date range, and key caveats only",
  caption  = "Source: World Bank Open Data"
)
```

---

## 8. Annotation: Guide the Viewer

Annotations transform a chart from a data dump into a narrative.

### When to Annotate — Ask First

**Do not automatically annotate outliers.** Flag them first:

> *"I notice a large spike in [variable] around [year/value]. Do you know what caused this? If so, I can add an annotation."*

### Reference Lines and Period Bands

Reference lines and shaded bands are among the most effective annotation tools — for averages, thresholds, targets, and time periods (recessions, policy changes, events). Add them proactively when the data has a natural reference value.

```r
# Mean or threshold reference line
geom_hline(
  yintercept = mean(df$value, na.rm = TRUE),
  linetype   = "dashed", color = "grey40", linewidth = 0.4
)

# Zero line (for charts that can go negative)
geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3)

# Vertical marker for a specific event/year
geom_vline(
  xintercept = 2008,
  linetype   = "dotted", color = "grey40", linewidth = 0.4
)

# Shaded period band (e.g. crisis, intervention, COVID)
annotate(
  "rect",
  xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-01"),
  ymin = -Inf, ymax = Inf,
  fill = "grey85", alpha = 0.4
)
```

Always **add a short text annotation** explaining what the reference line or band represents — a dashed line with no label is ambiguous. Layer the reference element *before* the main geom so it sits behind the data.

### Annotation Template (Once Confirmed)

```r
peak_val <- max(df$value)   # always derive from data — never hardcode

annotate(
  "text",
  x = as.Date("2020-03-15"), y = peak_val * 1.05,
  label = "COVID-19 lockdowns begin",
  hjust = 0, vjust = 0, size = 3.2, color = "grey30"
) +
annotate(
  "curve",
  x = as.Date("2020-03-15"), y = peak_val * 1.04,
  xend = as.Date("2020-03-20"), yend = peak_val,
  curvature = -0.3,
  arrow = arrow(length = unit(2, "mm"), type = "closed"),
  color = "grey50"
)
```

---

## 9. Aspect Ratio & Export

**Publication-style background (optional):** For a warm print look, use an off-white background and set the same in `ggsave(..., bg = "...")` so the export is consistent. Do not use a coloured background unless the user requests it.

- **Scatterplots:** Use `coord_fixed()` so a 45° slope appears at 45°.
- **Bar charts:** Use `width = 0.7` in `geom_col()`.
- **Time series:** 3:2 (width:height) is a solid default.
- **Maps:** Let the geography dictate the ratio via `coord_sf()`. Never manually distort a projection.

### Bar Chart Height Scaling

```r
n_cats <- n_distinct(df$country)
ggsave("output.png", width = 10, height = max(6, n_cats * 0.28), dpi = 300, bg = "white")
```

### Always Include `ggsave()` Defaults

```r
ggsave(
  filename = "output.png",
  plot     = last_plot(),
  width    = 10,
  height   = 6,
  dpi      = 300,
  bg       = "white"   # mandatory — prevents transparent backgrounds
)
```

---

## 10. Maps

Maps deserve extra attention. Spatial data has several unique pitfalls that produce broken-looking charts even when the code appears correct.

### Layout and Background

- Use `theme_void()` — grid lines and axis ticks add no information on a map.
- **Polygon borders:** For **admin/region-level** maps (dozens to a few hundred polygons), add thin borders (`color = "white"` or `"grey80"`, `linewidth = 0.15–0.2`) to help parse geography. For **fine grids** (hundreds or thousands of small cells), **do not draw borders** — use `color = NA` and `linewidth = 0`. Otherwise the map becomes a solid white grid and fill colors disappear.
- Use `coord_sf()` if necessary (but do not use it in every map). Never use `coord_cartesian()` on spatial data.
- **Country and continent maps — background outline:** For maps of a country or continent, add a **background outline** of that geography so the outer boundary is visible even where the data layer is sparse or absent. Ideally use a **unioned** outline (e.g. `sf::st_union(region_sf)` or the boundary of the full region) and draw it with a **low linewidth** (e.g. 0.2–0.4) in a neutral color (e.g. grey40 or grey50).

```r
# Example: outline of the mapped region as background
region_outline <- region_sf |> sf::st_union() |> sf::st_boundary()
ggplot() +
  geom_sf(data = region_outline, fill = NA, color = "grey40", linewidth = 0.3) +
  geom_sf(data = data_sf, aes(fill = value), ...) +
  ...
```

### Fine grids and many small polygons

When the geometry is a **fine-resolution grid** (e.g. many thousands of small cells):

- **Omit polygon borders.** Set `color = NA` and `linewidth = 0` in `geom_sf()`. White or grey borders on every cell dominate the image and make the fill unreadable.
- Consider **aggregating to a coarser grid** if the result is still too dense to interpret at the output size.
- The two-layer pattern (base grey + data layer) and log transform for counts still apply; only the border choice changes.

### NA Fills: The White-Patch Problem

White patches on a map almost always mean `NA` — a grid cell or polygon had no matching row after a join. This is one of the most common map failures.

**Step 1 — Diagnose before plotting.**

```r
map_sf |>
  sf::st_drop_geometry() |>
  summarise(n_total = n(), n_na = sum(is.na(fill_variable)))
```

**Step 2 — Fix the join if possible.** Check for mismatched keys (trailing spaces, encoding differences, capitalisation). When joining by **region or country name** (e.g. to Natural Earth or other boundary data), **watch for name variants**: e.g. "Laos" vs "Lao PDR", "Republic of Korea" vs "South Korea". Normalise names in one table to match the other (e.g. `mutate(ADM0_EN = case_when(admin == "Lao PDR" ~ "Laos", TRUE ~ admin))`) and re-check unmatched rows. Verify that no entire country or region is missing after the join — wrong join keys often show up as one country blank or mislabelled.

```r
anti_join(map_sf |> sf::st_drop_geometry(), data_df, by = "gid")  # in map but no data
anti_join(data_df, map_sf |> sf::st_drop_geometry(), by = "gid")  # in data but no map
# For name-based joins: ensure country/region name alignment (e.g. Laos vs Lao PDR)
```

**Step 3 — If NAs are legitimate (e.g., no events recorded), set `na.value` explicitly.** Never leave it as the default white.

```r
scale_fill_viridis_c(na.value = "grey80")
scale_fill_manual(values = pal, na.value = "grey80")
```

**Step 4 — Use a two-layer pattern** so that unmatched features show as neutral land rather than disappearing into the white background.

```r
ggplot() +
  geom_sf(data = map_sf, fill = "grey88", color = "white", linewidth = 0.15) +
  geom_sf(
    data = filter(map_sf, !is.na(fill_variable)),
    aes(fill = fill_variable),
    color = "white", linewidth = 0.15
  ) +
  scale_fill_viridis_c(name = NULL, na.value = "grey88") +
  coord_sf() +
  theme_void()
```

**Step 5 — Filtered maps: distinguish in-scope NA from out-of-scope.**

- **Out of scope** (geometry excluded by the filter, e.g. outside the country): use **white**, so it reads as "not part of this map."
- **In scope but NA** (inside the filtered area, but missing value): use **grey** (e.g. `na.value = "grey80"`) so it clearly reads as "part of the map, no data."

### Handling Skewed Continuous Variables

Maps of count data almost always have extreme right skew. A linear scale maps 99% of cells to near-black. `sqrt` helps but is often insufficient — prefer `log1p` for fatality/event counts.

```r
map_sf <- map_sf |>
  mutate(log_fatalities = log1p(total_fatalities))

ggplot(map_sf) +
  geom_sf(aes(fill = log_fatalities), color = "white", linewidth = 0.15) +
  scale_fill_viridis_c(
    name     = NULL,
    labels   = \(x) scales::comma(round(expm1(x))),  # back-transform tick labels
    na.value = "grey80"
  ) +
  theme_void()
```

> ⚠️ **Always state the transformation in the legend title or subtitle.**

> ⚠️ **A map that is mostly one color is miscalibrated.** Apply a log or sqrt transform until the color range is meaningfully distributed across the map.

### Scale and Subtitle Must Match the Actual Encoding

**Do not claim a transformation that is not applied.** If the subtitle or legend says "log scale," the **axis or fill scale must actually be log (or log1p)**. Mismatched descriptions confuse readers and undermine trust.

### Semantic Color for Maps

- **Land vs. water:** Use neutral/earth tones for land. Water = muted blue-grey (`"#d0e4f0"`) or absent.
- **Sequential data (counts, densities):** `scale_fill_viridis_c()` or `scale_fill_distiller(palette = "YlOrRd", direction = 1)`.
- **Diverging data:** `scale_fill_distiller(palette = "RdBu")` with `limits` symmetric around zero.
- **Qualitative (climate zones, land cover):** `RColorBrewer::brewer.pal(n, "Set2")` or a manually curated palette for up to ~12 categories.

### Legend Placement for Maps

- `legend.position = "bottom"` with a horizontal colorbar for sequential/diverging fills.
- Multi-category qualitative legends: use `guide_legend(ncol = 4, byrow = TRUE)` to avoid a single tall column. Never let the legend overlap the map body.

```r
theme(
  legend.position   = "bottom",
  legend.direction  = "horizontal",
  legend.key.width  = unit(2, "cm"),
  legend.key.height = unit(0.3, "cm")
)
```

### Focus Regions: Show Full Geography in Grey

When highlighting a **subset of regions**, **do not plot only the subset** — draw the **full geography** in a neutral grey first, then the **focus regions** on top with data-driven fill or color.

```r
ggplot() +
  geom_sf(data = full_map_sf, fill = "grey92", color = "white", linewidth = 0.15) +
  geom_sf(data = focus_sf, aes(fill = value), color = "white", linewidth = 0.15) +
  ...
```

### Projection and Extent

- Use equal-area projections (Albers, Mollweide, Lambert) for choropleth maps where relative area matters.
- Clip to the area of interest: `coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)`.
- **CRS and coord_sf limits:** `xlim` and `ylim` in `coord_sf()` are in the **data's coordinate reference system**. If the data is projected (e.g. metres), limits must be in that CRS. To use degree-based limits, transform first: `map_sf <- map_sf |> sf::st_transform(4326)`.

### The Distant-Island Whitespace Problem

> **A large empty margin around the map almost always means a distant island or territory is stretching the bounding box.**

```r
sf::st_bbox(map_sf)   # inspect extent before finalising

# Clip explicitly if distant features inflate the box
coord_sf(xlim = c(-20, 52), ylim = c(-38, 38), expand = FALSE)
```

### Complete Map Example (Filled Choropleth)

```r
library(ggplot2)
library(sf)
library(scales)

map_sf <- map_sf |>
  mutate(log_fatalities = log1p(total_fatalities))

ggplot() +
  geom_sf(data = map_sf, fill = "grey88", color = "white", linewidth = 0.15) +
  geom_sf(
    data  = filter(map_sf, !is.na(log_fatalities)),
    aes(fill = log_fatalities),
    color = "white", linewidth = 0.15
  ) +
  scale_fill_viridis_c(
    name     = NULL,
    labels   = \(x) scales::comma(round(expm1(x))),
    na.value = "grey88",
    guide    = guide_colorbar(
      barwidth       = unit(6, "cm"),
      barheight      = unit(0.4, "cm"),
      title.position = "top"
    )
  ) +
  coord_sf() +
  theme_void(base_family = "sans") +
  theme(
    plot.title       = element_text(face = "bold", size = 17, margin = margin(b = 6)),
    plot.subtitle    = element_text(size = 13, color = "grey30", margin = margin(b = 12)),
    plot.caption     = element_text(size = 10, color = "grey50", hjust = 0, margin = margin(t = 10)),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    plot.margin      = margin(12, 16, 12, 12)
  ) +
  labs(
    title    = "Conflict fatalities are concentrated in a handful of grid cells",
    subtitle = "Cumulative fatalities 1997–2020. Color scale is log-transformed.",
    caption  = "Source: ACLED"
  )

ggsave("map_fatalities.png", width = 8, height = 9, dpi = 300, bg = "white")
```

---

## 11. The Gold Standard Template

> **Critical theme ordering rule:** Always apply `theme_clean()` *before* any additional `theme()` overrides. `theme_clean() + theme(...)` works correctly. `theme(...) + theme_clean()` will silently overwrite your overrides.

```r
library(ggplot2)
library(scales)
library(ggrepel)
library(dplyr)

theme_clean <- function(base_size = 13, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),

      plot.title    = element_text(face = "bold", size = base_size + 4,
                                   margin = margin(b = 8)),
      plot.subtitle = element_text(size = base_size, color = "grey30",
                                   margin = margin(b = 16)),
      plot.caption  = element_text(size = base_size - 3, color = "grey50",
                                   hjust = 0, margin = margin(t = 12)),

      plot.title.position   = "plot",
      plot.caption.position = "plot",

      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.line.x  = element_line(color = "grey40", linewidth = 0.4),
      axis.ticks.x = element_line(color = "grey40", linewidth = 0.4),

      legend.position      = "none",    # default off; enable per-chart only when needed
      legend.justification = "left",
      legend.title         = element_blank(),
      legend.key.size      = unit(0.8, "lines"),

      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", size = rel(0.9), hjust = 0),

      plot.margin = margin(12, 16, 12, 12)
    )
}

# Example: highlighted line chart with direct labels; no legend
df <- df |>
  mutate(highlight = ifelse(country == "Nigeria", "Nigeria", "Other"))

ggplot(df, aes(x = year, y = value, group = country, color = highlight)) +
  geom_line(
    data      = \(d) filter(d, highlight == "Other"),
    color     = "grey85",
    linewidth = 0.5
  ) +
  geom_line(
    data      = \(d) filter(d, highlight == "Nigeria"),
    color     = "#D55E00",
    linewidth = 0.95
  ) +
  geom_text_repel(
    data         = \(d) filter(d, year == max(year)),
    aes(label    = country),
    nudge_x      = 1.5,
    direction    = "y",
    segment.size = 0,
    hjust        = 0,
    size         = 3.5
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.25))) +
  scale_color_manual(values = c("Nigeria" = "#D55E00", "Other" = "grey85"), guide = "none") +
  scale_y_continuous(labels = scales::comma) +
  theme_clean() +
  labs(
    title    = "Finding-first title",
    subtitle = "Units, date range, and any key caveats",
    caption  = "Source: [Data source] | [Year of access]"
  )

ggsave("output.png", plot = last_plot(), width = 10, height = 6, dpi = 300, bg = "white")
```

---

## 12. Pre-Output Checklist

### Data Checklist — Run Before Writing Any Code
- [ ] What question does this chart answer? (Ask the user if unclear.) 
- [ ] Do I know what one row represents?
- [ ] Is the time variable parsed as `Date` or `POSIXct`, not character?
- [ ] Has the data been aggregated, or is a `summarise()` step needed?
- [ ] Are NAs handled explicitly (`na.rm = TRUE` or `filter(!is.na(x))`)?
- [ ] Are factors ordered logically (`fct_relevel` or `fct_reorder`)?
- [ ] Do I know the unit of every plotted variable?
- [ ] **Joining two or more tables?** → Join keys must have the same type (e.g. both character) in every table; coerce with `as.character()` (or one consistent type) before joining to avoid silent row drops (e.g. double vs character).
- [ ] **Is there a meaningful grouping variable** (region, type, income group) that would add insight? If so, encode via color, facets, or both.
- [ ] **Would a derived metric** (rate, share, YoY change) be more informative than the raw variable?
- [ ] **Using `cut(quantile(...))` for groups?** → Breaks must be unique; use `dplyr::ntile()` for equal-count groups, or `unique(quantile(...))` and handle fewer levels.

### Output Checklist — Run Before Returning Code
- [ ] Is it 3D? → Remove depth, use 2D.
- [ ] Is it a pie chart? → Replace with ordered horizontal bar chart.
- [ ] **Is any axis or label text tilted/rotated?** → Use `coord_flip()` or shorten labels so all text is horizontal.
- [ ] **Line chart (including faceted) with ≤ 8 series and a clear rightmost x?** → Use direct labels at the right, not a legend.
- [ ] **Stacked bar with one segment much larger than others?** → Use grouped bars, faceting by segment, or percentage composition so small segments are comparable.
- [ ] **Multiple variables with very different scales on one plot?** → Use separate panels or separate plots per variable.
- [ ] Bar chart with many categories (>15)? → Filter to top N, combine low-value categories into "Other" **only if "Other" remains among the smallest**, or scale `ggsave()` height. Never shrink labels below `rel(0.7)`.
- [ ] Bar chart with very long category labels? → Shorten in the data; avoid long tick labels.
- [ ] All bars the same flat color? → Add sequential fill or highlight bars of interest.
- [ ] Multi-series chart (lines, stacked area)? → **Labels must be on the right** (at max x, or at **last known x per series** if a series ends early). Use `geom_text_repel` with `segment.size = 0`, `hjust = 0`, and `nudge_x` proportional to the x range. Right expansion ≥ 0.22. Remove the legend body when direct labels are used.
- [ ] **Legend title visible?** → It should never be. Set `name = NULL` on every scale — for lines, maps, scatter, bars, all of them. Only keep an explicit title if the unit is essential and absent from title/subtitle (very rare).
- [ ] **Legend present when direct labels are used on lines or stacked areas?** → Remove it: `theme(legend.position = "none")` or `guide = "none"` in the scale. (Maps and scatter plots keep their legend — just without a title.)
- [ ] **Stacked area with a legend?** → Replace with right-side labels at midpoint of each final band (see Section 4).
- [ ] Bar chart Y-axis starting above zero? → Fix to start at 0.
- [ ] More than ~500 overlapping points? → Use `geom_hex()` or `alpha`. If using hex/bin2d, ensure fill scale has readable labels and sensible bin count.
- [ ] Axis titles present? → Remove via `theme(axis.title.x = element_blank(), axis.title.y = element_blank())`.
- [ ] Explainer text in subtitle? → Remove pipeline/join notes. Only include what is genuinely non-obvious.
- [ ] Numbers formatted? → Use `scales::comma`, `scales::percent`, or `label_number(scale_cut = cut_short_scale())`.
- [ ] Using deprecated `label_number_si()`? → Replace with `label_number(scale_cut = cut_short_scale())`.
- [ ] Source included? → `labs(caption = "Source: ...")`.
- [ ] Using `size` for lines? → Change to `linewidth` (ggplot2 ≥ 3.4.0).
- [ ] Suspected outliers or notable spikes? → Flag to user before annotating.
- [ ] Theme overrides in correct order? → `theme_clean() + theme(...)`.
- [ ] `ggsave()` included with `bg = "white"` and `dpi = 300`?
- [ ] Raw variable names in legend titles? → Replace with `name = NULL` (default) or human-readable label (rare exception).
- [ ] Facet subtitle mentions "free_y" or technical scale options? → Use plain language.
- [ ] Ridgeline with many groups or messy overlap? → Reduce to ≤ 6–8 groups or switch to faceted densities / boxplots.
- [ ] Source/caption invented or vague? → Use a real, human-readable source. Do not hallucinate.
- [ ] `data =` arguments use native lambda? → `\(d) filter(d, ...)` not `~`.
- [ ] **Title is finding-first** (states the answer, ideally), and **title claim matches the data and the plot** (e.g. "fell to 21%" only if the plot actually shows that). 
- [ ] **Subtitle** does not repeat legend keys or axis definitions already visible on the plot.
- [ ] **Labels:** Long labels (especially with parentheses) shortened? Direct labels colored to match their series and placed at the end of the element?
- [ ] **Scatter showing a path over time** (e.g. year-by-year)? → Do not add trend line or confidence interval; focus is the trajectory.
- [ ] **2–3 categories in title?** Consider coloring those keywords in the title to match legend/plot.
- [ ] **Title claims a trend (e.g. "has risen since the 80s")?** → Verify the actual direction in the plotted period; if the series falls or is flat, do not state that it rose. Use a neutral or accurate title (see **Trend claims: verify direction** in Section 7).
- [ ] **Category label used (e.g. "unisex", "high-income")?** → Definition must be explicit and match audience intuition; validate that plotted items fit (e.g. not "unisex" for names that are 99% one gender). State the rule in the subtitle if non-obvious (see **Define categories so they match audience intuition** in Section 1).
- [ ] **Stacked area labels:** Each label’s y-position must sit inside its band. If a label appears in the wrong band, align `label_df` ordering with the fill variable’s factor level order (see Section 4).
- [ ] **Colour restraint:** Multiple series with equal visual weight when one is the focus? → Use one or very few highlight colours + grey context.
- [ ] **Sankey/alluvial/waterfall/treemap/slope:** If used, is the chart type justified? Are flows/nodes simplified enough to read?
- [ ] **Stacked area data:** Fill variable is a **fixed set of categories** with one row per (x, category)? If the set changes over time (e.g. "top 5 each year"), do not use stacked area — use a fixed set or a line chart.
- [ ] **"Diversity" or "more over time" claim?** If total volume (births, population) also changed, consider a normalized metric (e.g. per 10k) and state it in the subtitle.
- [ ] **Top-N line chart?** Prefer many series in grey + top N highlighted in color with direct labels.
- [ ] **Facet by entity + variant (e.g. name + M/F)?** Pair same-entity panels side by side; avoid panels that are negligible or use one panel per entity with two lines.
- [ ] **Scatter with dense overplotting (discrete x or vertical bands)?** Use hex/bin2d, or highlight a subset in color and rest in grey, plus a clear trend line.
- [ ] **Bar chart with one dominant bar/segment and many tiny ones?** Add direct value labels on bars, or use a separate/inset plot for small categories.
- [ ] **Qualitative bars or legend with multiple categories?** Ensure each category has a distinct color; no reuse of the same color for different nominal categories.
- [ ] **100% stacked bar with one segment >90%?** Prefer bar of shares with value labels, or separate view for small categories; avoid unlabeled y-axis (e.g. 0.8–1.2).
- [ ] **Histogram of distribution?** Consider adding a density or frequency-polygon line so the shape is easier to read.

### Map-Specific Checklist
- [ ] Using `theme_void()`? → Required for clean map backgrounds.
- [ ] `coord_sf()` applied? → Required for all `sf`-based maps.
- [ ] **Join by region/country name?** → Verify name alignment (e.g. Laos vs Lao PDR); check that no country/region is missing or wrong after the join.
- [ ] **Join key same type in map and data?** → If joining spatial to tabular (e.g. by id/gid/ipums_id), coerce the key to the same class (e.g. `as.character()`) in both; double vs character causes silent drop of rows.
- [ ] **White patches visible?** → These are NAs. Diagnose with `sum(is.na(...))`, fix the join if possible, and set `na.value = "grey80"`.
- [ ] **Two-layer pattern applied?** → Base layer `fill = "grey88"` for all features; data layer for non-NA features only.
- [ ] Is the fill variable right-skewed (counts, totals)? → Apply `trans = "log1p"`. A map that is mostly one color is miscalibrated.
- [ ] Transformation stated in legend or subtitle? → Mandatory. Subtitle/legend must match the encoding.
- [ ] **Legend title exposes a raw variable name?** → Always replace with `name = NULL`. Maps use legends (not direct labels) but must still suppress the title.
- [ ] Multi-category qualitative legend in a single tall column? → Use `guide_legend(ncol = 4, byrow = TRUE)`.
- [ ] Legend overlapping the map body? → Move to `"bottom"` or `"right"`.
- [ ] **Excessive whitespace around the map?** → Run `sf::st_bbox(map_sf)`. Clip with `coord_sf(xlim = ..., ylim = ..., expand = FALSE)`.
- [ ] **Focus-region map?** → Draw full geography in grey first, then focus regions on top.
- [ ] **Fine grid (many small cells)?** → Use `color = NA`, `linewidth = 0` in `geom_sf()`.
- [ ] **Using degree-based xlim/ylim?** → Data must be in WGS84: `st_transform(4326)`.
- [ ] **Filtered map with NAs?** → Out-of-scope = white; in-scope but NA = grey (`na.value = "grey80"`).
- [ ] **Country or continent map?** → Add a unioned outline of the region with low linewidth so the outer boundary is visible.

### Line Chart Checklist
- [ ] Title makes a factual claim (e.g., "X has the highest Y", "fell to 21%", "X has risen since Y")? → Verify against actual data and against what the plot shows; derive programmatically where possible. For trend claims, verify direction in the plotted period (see **Trend claims: verify direction** in Section 7).
- [ ] Title longer than ~80 characters? → Shorten or insert `\n` to prevent overflow at `width = 10`.
- [ ] `geom_point()` on a dense series (10+ annual observations)? → Remove.
- [ ] **Multi-line chart (or faceted line with few lines per panel) with only a legend?** → Prefer direct end-of-line labels (`geom_text_repel` at max(x)) and remove the legend; label color should match line color; place at end of each line. Use legend only when series count > 8 or there is no clear right endpoint.
- [ ] **Direct labels:** Use `segment.size = 0` (no connector lines), `hjust = 0`. Right expansion ≥ 0.22. `nudge_x` in data units, proportional to x range. Label at **last known x per series** if a series ends before max(x) (e.g. `slice_max(year, n = 1, by = series_id)` for label data).
- [ ] `nudge_x` copied from another chart with a different x scale? → Recalibrate.
- [ ] **Legend title showing?** → Remove: `name = NULL` in the color/fill scale. This applies to all chart types — the legend title is never shown.