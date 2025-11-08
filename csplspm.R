library(plspm)
library(ggplot2)
library(reshape)
library(igraph)
data<- read.csv("全部数据719Z.csv", header = TRUE)
colnames(data)

# 构建潜在变量
latent_r <- list(dixin=c("SLO"),
                 qihou=c("PRE","TEM"),
                 jinji=c("POP" ,"CUL"),
                 NDVI=c("NDVI"),
                 landscape=c("CON","SHDI"),
                 CSESDR=c("CSESDR"))
latent_r
#构建模型 0表示无影响，1表示有影响

dixin <- c(0, 0, 0, 0, 0, 0)
qihou <- c(0, 0, 0, 0, 0, 0)
jinji <- c(0, 0, 0, 0, 0, 0)
NDVI <- c(1, 1, 0, 0, 0, 0)
landscape<-c(1, 1, 1, 1, 0, 0)
CSESDR <- c(1, 1, 1, 1, 1, 0)


latent_path_r <- rbind(dixin,qihou,jinji,NDVI,landscape,CSESDR)
colnames(latent_path_r) <- rownames(latent_path_r)
latent_path_r
r_B <- rep('A', 6)


#进行PLS-sem
Sample_pls <- plspm(data, latent_path_r, latent_r, modes = r_B)
summary(Sample_pls)

# 提取路径系数的索引
# 提取路径系数矩阵
path_coefs <- Sample_pls$path_coefs
edges <- which(path_coefs != 0, arr.ind = TRUE)
# 创建边列表，确保方向正确：列到行
edge_list <- apply(edges, 1, function(x) c(colnames(path_coefs)[x[2]], rownames(path_coefs)[x[1]]))
# 将edge_list转换为矩阵
edge_matrix <- matrix(unlist(edge_list), ncol = 2, byrow = TRUE)
# 创建图形对象
g <- graph_from_edgelist(edge_matrix, directed = TRUE)
# 添加权重属性
weights <- path_coefs[edges]  # 使用之前提取的索引
E(g)$weight <- weights
# 为边添加标签，这里的标签是权重（路径系数），保留两位小数
E(g)$label <- round(weights, 2)
# 设置颜色：正系数为蓝色，负系数为红色
E(g)$color <- ifelse(E(g)$weight > 0, "blue", "red")
# 设置边的宽度基于权重的绝对值
E(g)$width <- abs(E(g)$weight) * 3
# 绘图并显示路径系数
set.seed(5)
plot(g, 
     edge.arrow.size = 0.5, 
     vertex.label.color = "black", 
     vertex.color = "lightblue",
     vertex.size = 20, 
     vertex.frame.color = NA,
     edge.label = E(g)$label,  # 显示边的标签
     edge.label.cex = 0.8,     # 设置标签的字体大小
     edge.label.color = "black",  # 设置标签的颜色
     main = "Path Model ")
plot(Sample_pls,what="loadings",arr.width = 0.1, show.values = TRUE, lcol = 'black')
plot(Sample_pls,what="weights",arr.width = 0.1, show.values = TRUE, lcol = 'black')

Sample_pls_summary <- summary(Sample_pls)
Sample_pls_summary_text <- capture.output(print(Sample_pls_summary))
write.csv(Sample_pls_summary_text,"结果数据CS.csv")