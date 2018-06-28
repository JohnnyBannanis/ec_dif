  tiempo = 0:10
  tiempoo = 1:10
  tiempoo0 = 1:200
  mem = 0:20
  
  datos1 = c(0,5,8,11,12,15,17,17,19,20,20)
  datos2 = c(0,4,6,8,12,14,15,16,17,18,19)
  datos3 = c(0,2,5,6,8,9,12,12,10,16,18)
  datos4 = c(0,1,3,3,5,7,7,6,10,11,13)
  
  datos1_2 = c(0,5,9,11,14,16,18,20,20,20,20) 
  datos2_2 = c(0,4,5,6,7,5,6,7,8,6,7) 
  datos3_2 = c(0,4,4,7,8,7,9,11,12,13,15)
  datos4_2 = c(0,2,4,6,7,10,9,11,14,15,16)
  
  val_k <- function(max, mem, t)
    return((-log(1-(mem/max)))/t)
  
  val_l <- function(k,vect_time,num)
    l = num*(1-(1/exp(k*vect_time)))
    return(l)
  
  all_k <- function(vec,num){
    a = c()
    for (i in 2:length(vec)){
      val = val_k(num,vec[i],i-1)
      if(vec[i] < num){
        a = c(a,val)  
      }
    }
    return(a)
  }
  

  #k1 = val_k(20,datos1[2],1)
  #k2 = val_k(20,datos2[2],1)
  #k3 = val_k(20,datos3[2],1)
  #k4 = val_k(20,datos4[2],1)
  
  k1 = mean(all_k(datos1,20))
  k2 = mean(all_k(datos2,20))
  k3 = mean(all_k(datos3,20))
  k4 = mean(all_k(datos4,20))
  
  k1_2 = mean(all_k(datos1_2,20))
  k2_2 = mean(all_k(datos2_2,20))
  k3_2 = mean(all_k(datos3_2,20))
  k4_2 = mean(all_k(datos4_2,20))
  
  l1 = val_l(k1,tiempoo,20)
  l4_2 = val_l(k4_2,tiempoo,20)
  l2 = val_l(k2,tiempoo,20)
  l3 = val_l(k3,tiempoo,20)
  l4 = val_l(k4,tiempoo,20)
  
  k1_50 = mean(all_k(datos1,50))
  k1_100 = mean(all_k(datos1,100))
  
  l1_20 = val_l(k1,tiempoo0,20)
  l1_50 = val_l(k1_50,tiempoo0,50)
  l1_100 = val_l(k1_100,tiempoo0,100)
  
  #grafica 3
  plot(l1_100,type="l",main="Maximo relativo de conocimiento",xlim=c(0,220), 
       ylim=c(0,100),xlab="Tiempo (min)",ylab="Memorizados",col="green",cex=3)
  lines(l1_50,col="blue",cex=3)
  lines(l1_20,col="red",cex=3)
  abline(h=20, v=18.55,col="red",lty = 3)
  abline(h=50,v=86.74,col = "blue",lty = 3)
  abline(h=100,v=210,col = "green",lty = 3)
  legend(0.01, 99, legend=c("20 datos", "50 datos", "100 datos"),
         col=c("red", "blue","green"), lty=1, cex=0.8)

  #grafica2
  plot(tiempo, datos1, type = "o",
       axes = FALSE,xlab="Tiempo (min)", 
       ylab="Memorizados",xlim=c(0,10), 
       ylim=c(0,20))
  axis(2,20,at=mem,labels=mem, col.axis="black", las=1)
  axis(1, at=tiempo,labels=tiempo, col.axis="black", las=1)
  lines(tiempo,datos4,type= "o", col="red")
  lines(l1,type = 'l',col="green")
  lines(l4,type = 'l',col="blue")
  legend("topleft",
         c("Sujeto 1","Sujeto 4","K1 = 0.2876","k4 = 0.0512"),
         fill=c("black","red","green","blue")
  )
  
  #grafica1
  par(mfrow=c(2,2))
  
  plot(tiempo, datos1,type='o',xlab = "Tiempo (min)",main = c("Sujeto 1 , K =" ,round(k1,6)), ylab = "Memorizados",col="blue")
  lines(l1,type = 'l',col="red" )
    
  plot(tiempo, datos2,type='o',xlab = "Tiempo (min)",main = c("Sujeto 2 , K =" ,round(k2,6)),ylab = "Memorizados",col="blue")
  lines(l2,type = 'l',col="red" )

  plot(tiempo, datos3,type='o',xlab = "Tiempo (min)",main = c("Sujeto 3 , K =" ,round(k3,6)),ylab = "Memorizados",col="blue")
  lines(l3,type = 'l',col="red" )

  plot(tiempo, datos4,type='o',xlab = "Tiempo (min)",main = c("Sujeto 4 , K =" ,round(k4,6)),ylab = "Memorizados",col="blue")
  lines(l4,type = 'l',col="red" )
 
  #tablas
  df = data.frame(datos1[2:length(datos1)], datos2[2:length(datos1)], 
                  datos3[2:length(datos1)],datos4[2:length(datos1)])
  colnames(df) <- c("sujeto 1","sujeto 2","sujeto 3","sujeto 4") 
  print(df)
  
  df2 = data.frame(datos1_2[2:length(datos1)], datos2_2[2:length(datos1)], 
                  datos3_2[2:length(datos1)],datos4_2[2:length(datos1)])
  colnames(df2) <- c("sujeto 1:","sujeto 2:","sujeto 3:","sujeto 4:") 
  print(df2)