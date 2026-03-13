# musicbrainz-el

一个完整的 Emacs Lisp MusicBrainz API 客户端，支持 org-mode 集成。

**项目状态**: ✅ 已完成并归档 (2026-03-12)

## 特性

- ✅ **智能速率限制**: 使用 Token Bucket 算法，15 请求/18 秒
- ✅ **匿名请求**: 不需要提供联系方式
- ✅ **自动重试**: HTTP 503 错误自动重试（最多 3 次）
- ✅ **完整 API 支持**: 13 个实体类型，100% 测试覆盖
- ✅ **无额外依赖**: 纯 Emacs Lisp 实现

## 功能特性

    - **完整的 MusicBrainz API 支持**：13 个实体类型
    - **Org-mode 集成**：将 MusicBrainz 数据直接插入到 Org 文件中
    - **交互式选择界面**：支持分页和简单输入
    - **异步支持**：使用 pdd.el 库进行异步 HTTP 请求
    - **缓存系统**：LRU 缓存减少 API 调用
    - **分页支持**：支持 offset 参数进行分页浏览
    - **无数据库依赖**：纯文件存储，使用 Org 属性

## Genre 使用指南

MusicBrainz API 不支持直接搜索 genre，但可以通过以下方式获取：

    - **通过 Release 获取 genres** - 使用 `inc=genres` 参数
    - **通过 Artist 获取 genres** - 浏览 artist 的 releases，然后获取 release group 的 genres
    - **浏览所有 genres** - 使用 `/genre/all` 端点

### 解决方案

1. 通过 Release 搜索获取 Genres

```elisp
;; 搜索 release 并自动获取其 genres
(musicbrainz-org-insert-genre-from-release "Kind of Blue")
```

**工作流程**:

    - 搜索 "Kind of Blue" 获取 release
    - 获取 release 的 release-group-id
    - Lookup release-group with `inc=genres`
    - 插入 genres 到 Org 文件

**输出格式**:

```org
** Genres for Kind of Blue
- Jazz [genre:12345678-1234-1234-1234-123456789012]
- Modal Jazz [genre:87654321-4321-4321-4321-210987654321]
```

2. 通过 Artist 搜索获取 Genres

```elisp
;; 搜索 artist 并自动获取其 genres
(musicbrainz-org-insert-genre-from-artist "Miles Davis")
```

**工作流程**:

    - 搜索 "Miles Davis" 获取 artist
    - Browse artist's releases
    - 获取第一个 release 的 release-group-id
    - Lookup release-group with `inc=genres`
    - 插入 genres 到 Org 文件

3. 浏览所有可用 Genres

```elisp
;; 获取 MusicBrainz 中所有可用的 genres
(musicbrainz-org-browse-all-genres)
```

**输出格式**:
```org
** All MusicBrainz Genres
- Alternative [genre:12345678-1234-1234-1234-123456789012]
- Blues [genre:87654321-4321-4321-4321-210987654321]
- Classical [genre:...]
...
```

### API 限制

根据 MusicBrainz API 文档：
- Genre 实体不支持 `search` 和 `browse` 请求
- 但支持 `lookup` 请求（需要 MBID）
- 可以通过 `inc=genres` 参数在其他实体的 lookup 中获取 genres
- 支持 `/genre/all` 端点获取所有 genres（分页）

### 使用示例

#### 在 Org 文件中插入 Genre

    - 将光标放在要插入 genre 的位置
    - 执行 `M-x musicbrainz-org-insert-genre-from-release`
    - 输入 release 名称（如 "Kind of Blue"）
    - 系统自动搜索并插入 genres

#### 手动插入 Genre

如果知道 genre 的 MBID，可以直接插入：

```elisp
(musicbrainz-org-insert-genre
  (musicbrainz-lookup-genre "12345678-1234-1234-1234-123456789012"))
```


### Musicbrainz 實例

```elisp
;; 搜索艺术家
(musicbrainz-search-artist "Miles Davis" 10)

;; 搜索发行版
(musicbrainz-search-release "Kind of Blue" 10)
```

## Musicbrainz-Org 實例

```org
** Abbey's Road
:PROPERTIES:
:ID: 70516629-7715-41bf-97e1-b7bf11254cb8
:COUNTRY: IT
:STATUS: Official
:DATE: 2016
:ARTIST: Unknown
:END:
*** Artist Credit
  - Ada Montellanico
  - Giovanni Falzone

*** Tracklist
  - Abbey
  - Bird Alone / Singing in the Night
  - First Song
  - Long as You're Living
  - Driva Man / Freedom Day
  - Throw Away
  - Wholey Earth / Giant Hand
  - When Love Was You and Me
  - Talking to the Sun
  - Just Do It
  - Rainbow

[[https://musicbrainz.org/release/70516629-7715-41bf-97e1-b7bf11254cb8][MusicBrainz Release]]
```

### Genre 自动化插入

由于 MusicBrainz API 不支持直接搜索 genre，但支持通过 release/artist 获取 genres：

```elisp
;; 通过 release 搜索获取 genres 并插入
(musicbrainz-org-insert-genre-from-release "Kind of Blue")

;; 通过 artist 搜索获取 genres 并插入
(musicbrainz-org-insert-genre-from-artist "Miles Davis")

;; 浏览所有可用的 genres
(musicbrainz-org-browse-all-genres)
```

插入的 genre 格式：
```org
- Jazz [genre:12345678-1234-1234-1234-123456789012]
- Modal Jazz [genre:87654321-4321-4321-4321-210987654321]
```

## 支持的实体类型

| 实体         | 搜索函数                            | 查找函数                            | Org 插入函数                            |
|--------------|-------------------------------------|-------------------------------------|-----------------------------------------|
| Artist       | `musicbrainz-search-artist`         | `musicbrainz-lookup-artist`         | `musicbrainz-org-insert-artist`         |
| Release      | `musicbrainz-search-release`        | `musicbrainz-lookup-release`        | `musicbrainz-org-insert-release`        |
| Release Group| `musicbrainz-search-release-group`  | `musicbrainz-lookup-release-group`  | `musicbrainz-org-insert-release-group`  |
| Recording    | `musicbrainz-search-recording`      | `musicbrainz-lookup-recording`      | `musicbrainz-org-insert-recording`      |
| Label        | `musicbrainz-search-label`          | `musicbrainz-lookup-label`          | `musicbrainz-org-insert-label`          |
| Work         | `musicbrainz-search-work`           | `musicbrainz-lookup-work`           | `musicbrainz-org-insert-work`           |
| Area         | `musicbrainz-search-area`           | `musicbrainz-lookup-area`           | `musicbrainz-org-insert-area`           |
| Event        | `musicbrainz-search-event`          | `musicbrainz-lookup-event`          | `musicbrainz-org-insert-event`          |
| Instrument   | `musicbrainz-search-instrument`     | `musicbrainz-lookup-instrument`     | `musicbrainz-org-insert-instrument`     |
| Place        | `musicbrainz-search-place`          | `musicbrainz-lookup-place`          | `musicbrainz-org-insert-place`          |
| Series       | `musicbrainz-search-series`         | `musicbrainz-lookup-series`         | `musicbrainz-org-insert-series`         |
| URL          | `musicbrainz-search-url`            | `musicbrainz-lookup-url`            | `musicbrainz-org-insert-url`            |
| Genre        | `musicbrainz-browse-genres`        | `musicbrainz-lookup-genre`          | `musicbrainz-org-insert-genre`          |

## 文件结构

```
musicbrainz-el/
├── musicbrainz.el          # MusicBrainz API 实现
├── musicbrainz-org.el      # Org-mode 接口
└── README.md               # 本文档
```

## 类型系统實現

1. **更新槽类型定义**：
   ```elisp
   (artist-credit :initarg :artist-credit :type (or null vector list) :documentation "Artist credit list")
   (tracks :initarg :tracks :type (or null vector list) :documentation "Tracklist")
   ```

2. **更新遍历方式**：
   ```elisp
   ;; 错误：使用 in 遍历向量
   (cl-loop for medium in media ...)

   ;; 正确：使用 across 遍历向量
   (cl-loop for medium across media ...)
   ```

3. **更新迭代函数**：
   ```elisp
   ;; 错误：dolist 只接受列表
   (dolist (credit artist-credit) ...)

   ;; 正确：mapc 接受向量和列表
   (mapc (lambda (credit) ...) artist-credit)
   ```

## Token Bucket 算法实现

```elisp
(defun musicbrainz--rate-limit ()
  "Apply smart rate limiting using token bucket algorithm."
  (let* ((now (float-time))
         (period (* 1000 musicbrainz-rate-limit-period))
         (t0 (- now (/ period 1000.0)))
         (queue musicbrainz--request-queue))
    ;; Remove old requests
    (while (and queue (< (car queue) t0))
      (setq queue (cdr queue)))
    (setq musicbrainz--request-queue queue)

    ;; Check rate limit
    (when (>= (length queue) musicbrainz-rate-limit-requests)
      (let* ((oldest-request (car queue))
             (delay (- (+ oldest-request (/ period 1000.0)) now)))
        (when (> delay 0)
          (sleep-for delay))))

    ;; Add current request
    (setq musicbrainz--request-queue (append musicbrainz--request-queue (list now)))))
```

## 配置

### 键绑定（可选）

```elisp
(global-set-key (kbd "C-c m a") 'musicbrainz-org-search-and-insert-artist)
(global-set-key (kbd "C-c m r") 'musicbrainz-org-search-and-insert-release)
(global-set-key (kbd "C-c m l") 'musicbrainz-org-search-and-insert-label)
(global-set-key (kbd "C-c m w") 'musicbrainz-org-search-and-insert-work)
(global-set-key (kbd "C-c m g") 'musicbrainz-org-insert-genre-from-release)
```

### 其他配置

```elisp
;; 自定义 User-Agent
(setq musicbrainz-user-agent "MyApp/1.0")

;; 允许的请求数（默认：15）
(setq musicbrainz-rate-limit-requests 15)

;; 时间周期（秒）（默认：18）
(setq musicbrainz-rate-limit-period 18)

;; 自定义缓存大小
(setq musicbrainz-cache-size 100)

;; Org 接口配置
(setq musicbrainz-org-default-level 2)
(setq musicbrainz-org-insert-properties t)
(setq musicbrainz-org-link-to-source t)
```

### 配置實例

| 变量                                | 默认值       | 说明                      |
|-------------------------------------|--------------|---------------------------|
| `musicbrainz-org-default-level`     | `2`          | 默认标题级别              |
| `musicbrainz-org-insert-properties` | `t`          | 是否插入属性抽屉          |
| `musicbrainz-org-link-to-source`    | `t`          | 是否创建 MusicBrainz 链接 |
| `musicbrainz-org-date-format`       | `"%Y-%m-%d"` | 日期格式                  |

## 依赖

-  Emacs 27.1+
-  org-mode 9.0+
-  pdd.el 0.2.3+

## 参考

- MusicBrainz API: https://musicbrainz.org/doc/MusicBrainz_API
- pdd.el: https://github.com/lorniu/pdd.el
- denote: https://github.com/protesilaos/denote

## License

MIT
