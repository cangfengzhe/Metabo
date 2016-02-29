## 利用R语言进行代谢组学数据分析
author: Li Pidong
email: hope-dream@163.com

### 分析原有代码 MetaboAnalyst
MetaboAnalyst是http://www.metaboanalyst.ca/MetaboAnalyst/网站使用的R语言源码, 该网站使用了svm, plsda等方法进行了代谢组学数据处理, 但是缺少oplsda(正交偏最小二乘法判别分析)的代码
### oplsda(正交偏最小二乘法判别分析) 
自己编写learn/oplsda.r
