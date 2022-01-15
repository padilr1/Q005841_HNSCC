#
#p_fc_comparison <- 
ggplot(data = combined_nsd1ko_nsd2ko_up_shrunken,aes(x = condition,y = log2FoldChange),position = "stack",color = "darkgray", size = 2, xaxt="n") +
  geom_jitter(position = position_jitter(0.2),color = "darkgray", size = 2) +
  geom_errorbar(aes(ymin = log2FoldChange-se, ymax= log2FoldChange+se),data=summary_up, width = 0.3,color="#0099ff",position=position_dodge(width=1)) +
  geom_point(aes(x = condition, y = log2FoldChange),data = summary_up,color = "#0099ff",position=position_dodge(width=1)) +
  #
  geom_jitter(data = combined_nsd1ko_nsd2ko_down_shrunken,aes(x = condition,y = log2FoldChange),position = position_jitter(0.2),color = "darkgray", size = 2) +
  geom_errorbar(aes(ymin = log2FoldChange-se, ymax= log2FoldChange+se),data=summary_down, width = 0.3,color="#0099ff",position=position_dodge(width=1)) +
  geom_point(aes(x = condition, y = log2FoldChange),data = summary_down,color = "#0099ff",position=position_dodge(width=1)) + ylab("log2FC") + xlab("Condition") + labs(title = "") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,vjust = 2.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
#
p_down <- ggplot(data = combined_nsd1ko_nsd2ko_down_shrunken,aes(x = condition,y = log2FoldChange),position = position_jitter(0.2),color = "darkgray", size = 2) +
  geom_jitter(data = combined_nsd1ko_nsd2ko_down_shrunken,aes(x = condition,y = log2FoldChange),position = position_jitter(0.2),color = "darkgray", size = 2) +
  geom_errorbar(aes(ymin = log2FoldChange-se, ymax= log2FoldChange+se),data=summary_down, width = 0.3,color="#0099ff",position=position_dodge(width=1)) +
  geom_point(aes(x = condition, y = log2FoldChange),data = summary_down,color = "#0099ff",position=position_dodge(width=1)) +
  ylab("log2FC") + xlab("") + labs(title = "") +
  scale_x_discrete(position = "top") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,vjust = 2.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x.bottom = element_blank(),
        axis.line.x.top = element_line(colour = "black"),
        axis.ticks.x.bottom = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.text.x.top = element_text())

grid.arrange(p_up_final,p_down,ncol=1,heights = c(10,10), widths=c(10))

stat.test <- compare_means(log2FoldChange ~ condition, data = combined_nsd1ko_nsd2ko_up_shrunken,paired = FALSE,p.adjust.method = "fdr", method = 't.test',method.args = list(alternative = "equal")) %>% dplyr::select(-".y.") %>% mutate(y.position = c(15))

p_up_final <- p_fc_comparison + stat_pvalue_manual(stat.test, label = "p.adj = {p.adj}")

grid.arrange(p_up_final,p_down,ncol=1,heights = c(20,10))
