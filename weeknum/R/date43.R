#修改不同的周别，季度周/年周/跨年周等
library(lubridate)
st = ymd("20000101")
start_year = year(st)
#计算年累计周函数
#周的起始参数为start_wd，周四=5，周五=6，周六=7，周天=1，周一=2，周二=3，周三=4
yweek = function(x,start_wd = 5){
  y = year(x)
  z = st + years(y - start_year)
  u = ifelse((wday(z) - start_wd)>=0,(wday(z) - start_wd+1),(wday(z) + (7-start_wd+1)))
  v = as.numeric(ceiling((x-z+u)/7))
  return(v)
}
#计算双周累计周函数
doubleweek = function(x,start_wd = 4){
  y = year(x)
  z = st + years(y - start_year)
  u = ifelse((wday(z) - start_wd)>=0,(wday(z) - start_wd+1),(wday(z) + (7-start_wd+1)))
  v = as.numeric(ceiling((x-z+u)/14)) 
  return(v)
}
#计算季度累计周函数
qweek = function(x,start_wd = 4){
  y = year(x)
  y2 = (quarter(x)-1)*3
  z = st + years(y - start_year)
  z = z %m+% months(y2)
  u = ifelse((wday(z) - start_wd)>=0,(wday(z) - start_wd+1),(wday(z) + (7-start_wd+1)))
  v = as.numeric(ceiling((x-z+u)/7))
  return(v)
}
#计算年份的起始值
year_start = function(x,start_wd = 5){
  y = year(x)
  z = st + years(y - start_year)
  return(z)
}
#计算季度的起始值
q_start = function(x,start_wd = 4){
  y = year(x)
  y2 = (quarter(x)-1)*3
  z = st + years(y - start_year)
  z = z %m+% months(y2)
  return(z)
}
#将季度、月份、周别修改为Q、M、W



