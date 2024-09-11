forestPlot = function(data, cox) {
  #'
  #'@param data the dataframe used for Cox regression
  #'@param cox the output object of coxph
library(broom)
library(grid)
conf.high <- conf.low <- estimate <- NULL
coef=as.data.frame(tidy(cox, conf.int = TRUE))
gmodel=(glance(cox))

terms <- attr(cox$terms, "dataClasses")[-1]
allTerms <- lapply(seq_along(terms), function(i){
  var <- names(terms)[i]
  if (terms[i] %in% c("factor", "character")) {
    adf <- as.data.frame(table(data[, var]))
    cbind(var = var, adf, pos = 1:nrow(adf))
  }
  else if (terms[i] == "numeric") {
    data.frame(var = var, Var1 = "", Freq = nrow(data),
               pos = 1)
  }
  else {
    vars = grep(paste0("^", var, "*."), coef$term, value=TRUE)
    data.frame(var = vars, Var1 = "", Freq = nrow(data),
               pos = seq_along(vars))
  }
})
allTermsDF <- do.call(rbind, allTerms)
colnames(allTermsDF) <- c("var", "level", "N", "pos")
inds <- apply(allTermsDF[,1:2], 1, paste0, collapse="")

# use broom again to get remaining required statistics
rownames(coef) <- gsub(coef$term, pattern = "`", replacement = "")
toShow <- cbind(allTermsDF, coef[inds,])[,c("var", "level", "N", "p.value", "estimate", "conf.low", "conf.high", "pos")]

toShowExp <- toShow[,5:7]
toShowExp[is.na(toShowExp)] <- 0
toShowExp <- format(exp(toShowExp), digits=2)
toShowExpClean <- data.frame(toShow,
                             pvalue = signif(toShow[,4],2+1),
                             toShowExp)
toShowExpClean$stars <- paste0(round(toShowExpClean$p.value, 2+1), " ",
                               ifelse(toShowExpClean$p.value < 0.05, "*",""),
                               ifelse(toShowExpClean$p.value < 0.01, "*",""),
                               ifelse(toShowExpClean$p.value < 0.001, "*",""))
toShowExpClean$ci <- paste0("(",toShowExpClean[,"conf.low.1"]," - ",toShowExpClean[,"conf.high.1"],")")
toShowExpClean$estimate.1[is.na(toShowExpClean$estimate)] = "reference"
toShowExpClean$stars[which(toShowExpClean$p.value < 0.001)] = "<0.001 ***"
toShowExpClean$stars[is.na(toShowExpClean$estimate)] = ""
toShowExpClean$ci[is.na(toShowExpClean$estimate)] = ""
toShowExpClean$estimate[is.na(toShowExpClean$estimate)] = 0
toShowExpClean$var = as.character(toShowExpClean$var)
toShowExpClean$var[duplicated(toShowExpClean$var)] = ""
toShowExpClean = toShowExpClean[order(-toShowExpClean$estimate),]
# make label strings:
toShowExpClean$N <- paste0("n = ",toShowExpClean$N)

#flip order
toShowExpClean <- toShowExpClean[nrow(toShowExpClean):1, ]

rangeb <- range(toShowExpClean$conf.low, toShowExpClean$conf.high, na.rm = TRUE)
breaks <- axisTicks(rangeb/2, log = TRUE, nint = 7)
rangeplot <- rangeb
# make plot twice as wide as needed to create space for annotations
rangeplot[1] <- rangeplot[1] - diff(rangeb)
# increase white space on right for p-vals:
rangeplot[2] <- rangeplot[2] + .15 * diff(rangeb)

width <- diff(rangeplot)
# y-coordinates for labels:
y_variable <- rangeplot[1] +  c(0.02, 0.22, 0.4)[1] * width
y_nlevel <- rangeplot[1]  +  c(0.02, 0.22, 0.4)[2] * width
y_cistring <- rangeplot[1]  +  c(0.02, 0.22, 0.4)[3] * width
y_stars <- rangeb[2]
x_annotate <- seq_len(nrow(toShowExpClean))

# geom_text fontsize is in mm (https://github.com/tidyverse/ggplot2/issues/1828)
annot_size_mm <- 1.3 *
  as.numeric(convertX(unit(theme_get()$text$size, "pt"), "mm"))

p = ggplot(toShowExpClean, aes(seq_along(var), exp(estimate))) +
  geom_rect(aes(xmin = seq_along(var) - 0.5, xmax = seq_along(var) + 0.5,
                ymin = exp(rangeplot[1]), ymax = exp(rangeplot[2]),
                fill = ordered(seq_along(var) %% 2 + 1)),position = "identity") +
  scale_fill_manual(values = c("#28445533", "#E4B48533"), guide = "none") +
  geom_point(pch = 15, size = 2) +
  geom_errorbar(aes(ymin = exp(conf.low), ymax = exp(conf.high)), width = 0.15) +
  geom_hline(yintercept = 1, linetype = 3) +
  coord_flip(ylim = exp(rangeplot)) +
  ggtitle("") +
  scale_y_log10(
    name = "",
    labels = sprintf("%g", breaks),
    expand = c(0.02, 0.02),
    breaks = breaks) +
  theme_light() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        panel.border=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  annotate(geom = "text", x = x_annotate, y = exp(y_variable),
           label = toShowExpClean$var, fontface = "bold", hjust = 0,
           size = annot_size_mm) +
  annotate(geom = "text", x = x_annotate, y = exp(y_nlevel), hjust = 0,
           label = toShowExpClean$level, vjust = -0.1, size = annot_size_mm) +
  # annotate(geom = "text", x = x_annotate, y = exp(y_nlevel),
  #          label = toShowExpClean$N, fontface = "italic", hjust = 0,
  #          vjust = ifelse(toShowExpClean$level == "", .5, 1.1),
  #          size = annot_size_mm) +
  annotate(geom = "text", x = x_annotate, y = exp(y_cistring),
           label = toShowExpClean$estimate.1, size = annot_size_mm,
           vjust = ifelse(toShowExpClean$estimate.1 == "reference", .5, -0.1)) +
  annotate(geom = "text", x = x_annotate, y = exp(y_cistring),
           label = toShowExpClean$ci, size = annot_size_mm,
           vjust = 1.1,  fontface = "italic") +
  annotate(geom = "text", x = x_annotate, y = exp(y_stars),
           label = toShowExpClean$stars, size = annot_size_mm,
           hjust = -0.2,  fontface = "italic") +
  annotate(geom = "text", x = 0.5, y = exp(y_variable),
           label = paste0(toShowExpClean$N[1], "; # Events: ", gmodel$nevent, "; Global p-value (Log-Rank): ",
                          format.pval(gmodel$p.value.log, eps = ".001"), " \nAIC: ", round(gmodel$AIC,2),
                          "; Concordance Index: ", round(gmodel$concordance,2)),
           size = 0.9*annot_size_mm, hjust = 0, vjust = 1.2,  fontface = "italic")
return(p)
}
