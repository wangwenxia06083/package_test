rm(list = ls())
source("e:\\007 日报数据-crm\\code\\crm日报-人员信息修改-ver.01.R",encoding = "utf-8")
#取出当期消耗的基础表
LT = merge(Client_info,LT,by="Client_ID")
