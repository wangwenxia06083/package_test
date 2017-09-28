library(dplyr)
library(Lahman)
library(hflights)
library(ggplot2)
library(reshape2)
library(tidyr)
#转换数据集为tibble类型，tibble包是一个轻量级的包，它实现的data.frame的重新塑造
hflights_df <- tbl_df(hflights)
#筛选子集
filter(hflights_df, Month == 1, DayofMonth == 1)
#升序排列
arrange(hflights_df, DayofMonth, Month, Year)
#加desc降序排列
arrange(hflights_df, desc(ArrDelay))
#提取子集
select(hflights_df, Year, Month, DayOfWeek)
select(hflights_df, Year:DayOfWeek)
select(hflights_df, -(Year:DayOfWeek))
#变形：优势在于可以在同一语句中对刚增加的列进行操作:
mutate(hflights_df, 
       gain = ArrDelay - DepDelay, 
       speed = Distance / AirTime * 60)

#汇总：summarise()
summarise(hflights_df, 
          delay = mean(DepDelay, na.rm = TRUE))
#分组动作
planes <- group_by(hflights_df, TailNum)
delay <- summarise(planes, 
                   count = n(), 
                   dist = mean(Distance, na.rm = TRUE), 
                   delay = mean(ArrDelay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)
ggplot(delay, aes(dist, delay)) + 
  geom_point(aes(size = count), alpha = 1/2) + 
  geom_smooth() + 
  scale_size_area()
# n(): 计算个数
# n_distinct(): 计算 x 中唯一值的个数. (原文为 count_distinct(x), 测试无用)
# first(x), last(x) 和 nth(x, n): 返回对应秩的值, 类似于自带函数 x[1], x[length(x)], 和 x[n]
#为避免运算时频繁写变量名，可以使用within函数
within(delay,
       {delay[delay<=50]<-100
       dist[dist>=500]<-1000
       })
#创建数据框
name1 <- c("Bob","Mary","Jane","Kim")
name2 <- c("Bob","Mary","Kim","Jane")
weight <- c(60,65,45,55)
height <- c(170,165,140,135)
birth <- c("1990-1","1980-2","1995-5","1996-4")
accept <- c("no","ok","ok","no")
df1 <- data.frame(name1,weight,height)
df2 <- data.frame(name2,birth,accept)
#合并行列
bind_rows(df1,df1[c(2,4),])  
bind_cols(df1,df2)
#字段连接合并，merge函数和dplyr对比
name1 <- c("Bob","Mary","Jane","Kim","Smith")
weight <- c(60,65,45,55,60)
name2 <- c("Bob","Mary","Kim","Jane","Eric")
height <- c(170,165,140,135,170)

df11 <- data.frame(name1,weight,stringsAsFactors=F) # 加这个参数是防止字符串自动转化为因子
df33 <- data.frame(name1,height,stringsAsFactors=F)
df22 <- data.frame(name2,height,stringsAsFactors=F) # 成员与前面不完全一样

inner_join(df11,df33) # 自动根据相同的列名匹配 
full_join(df11,df22,by=c("name1"="name2"))
left_join(df11,df22,by=c("name1"="name2")) # 只保留前者的行
right_join(df11,df22,by=c("name1"="name2")) # 只保留后者的行
semi_join(df11,df22,by=c("name1"="name2")) # 保留共有的行，同时只返回前者的列
anti_join(df11,df22,by=c("name1"="name2")) # 返回后者没有的前者的行，同时只返回前者的列

#汇总计算
name1 <- c("Bob","Mary","Jane","Kim")
weight <- c(60,65,45,55)
height <- c(170,165,140,135)
weta <- 1:4
df1 <- data.frame(name1,weight,height,weta)
summarise(df1,arv_weight=mean(weight),arv_height=mean(height))
summarise_all(df1[-1],mean)
summarise_if(df1,is.numeric,mean)
#summarise_at配合vars的用法
summarise_at(df1,vars(weight,height,weta),mean) # 配合vars函数，一次选择多列
summarise_at(df1,vars(weight:weta),mean) # 从哪到哪
u <- c("weight","height")
summarise_at(df1,vars(one_of(u)),mean) # 可以接字符串向量
summarise_at(df1,u,mean) # 也可以直接接字符串向量
summarise_at(df1,u,mean,trim=1) # mean的参数可以接在后面

summarise_at(df1,vars(contains("eig")),mean) # 匹配含有的
summarise_at(df1,vars(matches(".t.")),mean) # 使用正则表达式
summarise_at(df1,vars(starts_with("w")),mean) # 匹配以此为开头的
summarise_at(df1,vars(ends_with("ht")),mean) # 匹配以此为结尾的
summarise_at(df1[,-1],vars(everything()),mean) # 选择所有列

#funs的用法
summarise_all(df1[,-1],funs(mean,sum))
summarise_all(df1[,-1],funs(sum(.*2))) # 数据用.表示
summarise_all(df1[,-1],funs(medi=median)) # 指定得到的列后缀名
summarise_all(df1[,-1],funs("in"=median)) # 或者加引号
mutate_all(df1[,-1],funs(.^2))
summarise_if(df1,is.numeric,funs(mean,sum))
summarise_at(df1,vars(ends_with("t")),funs(mean,sum))

#分组计算
name1 <- c("Bob","Mary","Jane","Kim")
weight <- c(60,65,45,55)
height <- c(170,165,140,135)
accept <- c("no","ok","ok","no")
df <- data.frame(name1,weight,height,accept)

group_df <- group_by(df,accept)
summarise(group_df,arv_height=mean(height),count=n()) # 其中n()是查数的意思
# 使用扩展函数
summarise_all(group_df[,-1],mean)
summarise_if(group_df,is.numeric,mean)
summarise_at(group_df,vars(contains("eigh")),funs(sum,mean))
#tidyr的融合和重铸
aqg <- gather(airquality,group,value,Ozone:Temp)
spread(aqg,group,value) # 输入要被转化到列名的列和值，好像一次只能转化一列作为列名
#融合重铸的应用
aqm <- melt(airquality, id=c("Month", "Day"), na.rm=TRUE)
df <- mutate(aqm, newvalue = value+rnorm(2,0,50))
colnames(df) <- c("month","day","group","value1","value2")
## 根据group分组计算两个value的均值
df_grp1 <- group_by(df,group)
summarise_at(df_grp1,vars(value1,value2),mean)
## 根据month和group分组计算两个value的均值
df_grp2 <- group_by(df,month,group)
summarise_at(df_grp2,vars(value1,value2),mean)
# 根据month和group分组计算每组个数
summarise(df_grp2,count=n())

df_melt <- melt(df,id=c("month","day","group"))
## 根据group分组计算两个value的均值
dcast(df_melt, group ~ variable, mean)
## 根据month和group分组计算两个value的均值
dcast(df_melt, month + group ~ variable, mean)
## 根据month和group分组计算每组个数
dcast(df_melt, month + group ~ variable, length)
## 如果要不区分value1和value2，算整体按照month和group分组后的均值
(mg <- dcast(df_melt, month ~ group , mean))# 上面计算结果值是一个矩阵，想要用一列表示
melt(mg, id="month")
# 按照月份拆分成多个矩阵，每一个矩阵表示group和日期的对应
u <- acast(df_melt, group ~ day ~ month) # 使用acast返回一个三维数组

#拆分合并列
library(tidyr)
name1 <- c("Bob","Mary","Jane","Kim")
birth <- c("1990-1","1980-2","1995-5","1996-4")
df <- data.frame(name1, birth)

(df1 <- separate(df,birth,into=c("year","month"),sep="-"))
separate_rows(df,birth,sep="-") # 拆分完放在一列里面显示
# 其实separate_rows相当于使用separate之后进行了融合，再更换一下顺序
separate(df,birth,into=c("year","month"),sep="-") %>% gather(group,birth,year:month)
unite(df1,birth,year,month,sep="-")





