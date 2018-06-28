syms x q 


k_1 = 0.2846
k_2 = 0.0711
k_3 = 0.0327

l_1 = 20*(1-(1/exp(k_1*x)))
l_2 = 50*(1-(1/exp(k_2*x)))
l_3 = 100*(1-(1/exp(k_3*x)))


hold on
ezplot(l_1,[1,100])
hold on
ezplot(l_2,[1,100])
hold on
ezplot(l_3,[1,210])

 