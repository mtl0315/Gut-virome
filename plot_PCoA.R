library(tidyverse)
library(vegan)

Group <- read.delim('Group.txt', sep = '\t')
otu <- t(read.delim('all.txt', row.names = 1, sep = '\t'))
pcoa <- vegdist(otu, method = 'bray') %>% 
    cmdscale(k = (nrow(otu) - 1), eig = TRUE)

pcoa1 <- round((pcoa$eig)[1] / sum(pcoa$eig)*100,2)
pcoa2 <- round((pcoa$eig)[2] / sum(pcoa$eig)*100,2)

data.frame(pcoa$point) %>% 
    rownames_to_column("names") %>% 
    mutate(PCoA1=X1) %>% 
    mutate(PCoA2=X2) %>% 
    select(c(names, PCoA1, PCoA2)) %>% 
    left_join(Group, by='names') %>% 
    ggplot(aes(x=PCoA1, y=PCoA2, Group = Group, color = Group)) +
    labs(x=paste("PCoA1 (",pcoa1,"%)",sep="") ,y=paste("PCoA2 (",pcoa2,"%)",sep="")) +
    stat_ellipse(level = 0.95, show.legend = F) + 
    geom_point(size=4, alpha=0.6) + 
    theme_bw() +
    theme(panel.grid=element_blank())
ggsave('pcoa_all.png', width = 6, height = 5.5)
