ccA.irr %>% mutate(
  Score = rowSums(pick(4:58))
) %>% arrange(desc(-Score)) %>% 
  ggplot(aes(x=Score, y=brooch))+geom_smooth()+geom_point(aes(x=jitter(Score)), alpha=.2)

colMeans(ccA.irr %>% select(where(is.numeric)), na.rm=T) %>% round(3) %>% cbind
