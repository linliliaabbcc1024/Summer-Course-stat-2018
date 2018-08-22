
n = 20
m = 1000
# normal distribution
t_test_p = c()
nonpara_t_test_p = c()
for(i in 1:m){
  group1 = rnorm(n)
  group2 = rnorm(n,mean = 1)
  t_test_p[i] = t.test(group1,group2)$p.value
  nonpara_t_test_p[i] = wilcox.test(group1,group2)$p.value
}
sum(t_test_p<0.05)
sum(nonpara_t_test_p<0.05)
# parametric is a little bit more powerful.

# log normal distribution
t_test_p = c()
nonpara_t_test_p = c()
for(i in 1:m){
  group1 = rlnorm(n)
  group2 = rlnorm(n,meanlog = 1)
  t_test_p[i] = t.test(group1,group2)$p.value
  nonpara_t_test_p[i] = wilcox.test(group1,group2)$p.value
}
sum(t_test_p<0.05)
sum(nonpara_t_test_p<0.05)
# non-parametric is more powerful.





t_test_p = c()
# multiple comparison problem
for(i in 1:(m*10)){
  group1 = rnorm(n)
  group2 = rnorm(n)
  t_test_p[i] = t.test(group1,group2)$p.value
}
sum(t_test_p<0.05)
t_test_p_adj = p.adjust(t_test_p,'fdr')
sum(t_test_p_adj<0.05)

t_test_p2 = t_test_p[t_test_p<0.05]
t_test_p_adj2 = p.adjust(t_test_p2,'fdr')
sum(t_test_p_adj2<0.05)
