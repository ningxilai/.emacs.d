## 关于本仓库

使用`setup.el`重构的轻量化配置，使用`lsp-mode`与`corfu`进行编程行为。

### `module`

对不同语言的支持存放在`site-lisp/langs`目录下，包括多种编程语言与Org/Markdown/LaTeX/Typst等标记语言。

对其他工具的扩展存放在`site-lisp/tools`目录下，关于`eww` `rime` `reader`等`module`。

### `lisp`

此目录下存放着`setup.el`以及`setup.el`的扩展，以便于管理配置代码段和`module`。

### `user-lisp`

~~目前尚未确定是否将其纳入稳定标准当中，对其行为的配置仍属待定。~~

使用[persistent-cached-load-filter](https://github.com/include-yy/persistent-cached-load-filter)进行了启动时速度优化。
