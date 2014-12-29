---
title: "R语言经典函数解析-do.call"
author: "By Pidong Li `r Sys.Date()` "
output: 
  rmarkdown::html_vignette: 
    css: D:/Program Files/RStudio/RFile/MarkDown/markdown.css
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

### R语言经典函数解析-do.call

> 通过传递函数的名字和参数列表，构建和执行一个函数调用

`do.call(what, args, quote = )`

#### 参数

`what` 被调用的函数或者非空字符串
`args` 被调用函数的参数名称
`quote` 逻辑值表明是否被去引用参数

#### 例子

```{r, tidy=TRUE}
# 合并数据
tmp <- expand.grid(letters[1:2], 1:3, c("+", "-"))
do.call("paste", c(tmp, sep = ""))
```












