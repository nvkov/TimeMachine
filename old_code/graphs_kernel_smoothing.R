

suma_date <- summarise(group_date,
                       sum_return=sum(returnQuantity), 
                       sum_quant=sum(quantity), 
                       sum_order=n_distinct(orderID),
                       sum_art=n_distinct(articleID),
                       sum_vou = n_distinct(voucherID),
                       sum_g=sum(gratis)
)


#Daily sum of returns
par(mfrow=c(3,1))
with(suma_date, {
  plot(orderDate, sum_return, type="l",xaxt = "n")
  axis(side = 1, at = orderDate, labels = orderDate)
  lines(ksmooth(orderDate, sum_return, "normal", bandwidth = 50), col = 3)
})

#Daily sum of ordered Articles
with(suma_date, {
  plot(orderDate, sum_quant, type="l",xaxt = "n")
  axis(side = 1, at = orderDate, labels = orderDate)
  lines(ksmooth(orderDate, sum_quant, "normal", bandwidth = 50), col = 3)
})

# Daily return/quatity ration
with(suma_date, {
  plot(orderDate, sum_return/sum_quant, type="l",xaxt = "n")
  axis(side = 1, at = orderDate, labels = orderDate)
  lines(ksmooth(orderDate, sum_return/sum_quant, "normal", bandwidth = 50), col = 3)
})

# Daily number of GRATIS items 
with(suma_date, {
  plot(orderDate, sum_g, type="l")
  lines(ksmooth(orderDate, sum_g, "normal", bandwidth = 50), col = 3)
})








with(suma_date, {
  plot(orderDate, sum_vou, type="l")
  lines(ksmooth(orderDate, sum_vou, "normal", bandwidth = 50), col = 3)
})

