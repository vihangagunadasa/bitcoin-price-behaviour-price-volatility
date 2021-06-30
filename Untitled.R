#playground
A = data.frame(
  date=seq(1,10),
  valA=rep(seq(1,10,2),2)
)
A

B = data.frame(
  date=seq(2,10),
  valB=c(3,4,5,4,2,5,8,7,8)
)
B
left_join(A,B,by=c("date"))
