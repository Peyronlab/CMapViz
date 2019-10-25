#' @export
#'
#' @import ggplot2
#' @import readxl
#' @import dplyr
#' @import scales
#' @title Bubble plot of CMap output table
#' @description
#' This function allows the user to represent the CMap results (broadinstitute)
#' under the form of a bubble plot representing statistics and cell lines.
#' @name bubble_plot
#' @rdname bubble_plot
#' @aliases bubble_plot
#' @usage
#' bubble_plot(path, output_path=NULL)
#' @param path path of the excel file
#' @param output_path path for the experiment output folder (default=NULL)
#'
#' @examples
#' # bubble_plot(path, output_path=NULL)
#' @return ggplot object - bubble plot



bubble_plot <- function(path, output_path = NULL) {
  # table <- read_excel("/media/Storage_HDD/C3M/RNAseq/projet_cg_hsg/Cmap/new/wt_ST3_both.xls",
  #                           sheet = "by cmap name and cell line")
  table <- read_excel(path,
    sheet = "by cmap name and cell line"
  )
  # View(table)
  HL60 <- table[which(table$...3 == "HL60"), ]
  MCF7 <- table[which(table$...3 == "MCF7"), ]
  PC3 <- table[which(table$...3 == "PC3"), ]

  HL60 <- data.frame(HL60[order(HL60$enrichment), ])
  HL60 <- HL60[which(HL60$n > 1), ]
  HL60 <- HL60[which(HL60$p < 0.05), ]
  HL60 <- HL60[which(HL60$enrichment < 0), ]
  HL60$rank <- rep(1, 12)

  MCF7 <- data.frame(MCF7[order(MCF7$enrichment), ])
  MCF7 <- MCF7[which(MCF7$n > 4), ]
  MCF7 <- MCF7[which(MCF7$p < 0.05), ]
  MCF7 <- MCF7[which(MCF7$enrichment < 0), ]
  MCF7$rank <- rep(2, length(MCF7$rank))

  PC3 <- data.frame(PC3[order(PC3$enrichment), ])
  PC3 <- PC3[which(PC3$n > 2), ]
  PC3 <- PC3[which(PC3$p < 0.05), ]
  PC3 <- PC3[which(PC3$enrichment < 0), ]
  PC3$rank <- rep(3, length(PC3$rank))

  df <- rbind(HL60, MCF7, PC3)
  df$p <- as.numeric(df$p)
  df$p[is.na(df$p)] <- mean(df$p[!is.na(df$p)])


  df2 <- df %>%
    mutate(rank2 = jitter(rank, amount = 0.4))
  df2 <- df2 %>%
    mutate(enrichment2 = jitter(df2$enrichment, amount = 0.05))
  names(df2)[7] <- "-log2(pValue)"
  df2$`-log2(pValue)` <- -log2(df2$`-log2(pValue)` + 0.01)

  unique(df2$cmap.name.and.cell.line)
  # p1 5
  # p2 2.5
  # p3 0.5
  clinical <- c(2, 10 * 2, 1, 1, 1, 39 * 2, 1, 11 * 2, 2, 26 * 2, 2, 1, 1, 1, 48 * 2, 1, 1, 1, 1, 1)
  # clinical=c(2,10*2,1,1,1,39*2,1,11*2,2,1,1,1,1,1,1,1,1,1,1,1)
  ifact <- c(3, 2, 3, 1, 1, 3, 1, 2, 3, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  s <- c(276, 386, 114, 179, 2452, 1708, 20, 3799, 664, 4158, 4024, 301, 16, 339, 5795, 3196, 543, 1, 1, 611)
  # s=c(14,76,8,9,215,4092,461,273,186,270,247,39,19,85,231,957,21,1,1,21)
  # alpha=log2(0.01/s)
  s <- (clinical * s * ifact)
  alpha <- abs(rescale(log(s), to = c(0, 1)) - 1)
  # alpha=rescale(log((alpha*clinical)+0.01),to =c(0,1))

  # alpha=abs(rescale(s,to = c(0,1))-1)
  df3 <- cbind(df2, alpha[match(df2$cmap.name.and.cell.line, unique(df2$cmap.name.and.cell.line))])
  names(df3)[12] <- "alpha"
  # df3$cmap.name.and.cell.line=gsub("alpha","α",df3$cmap.name.and.cell.line)
  # df3$cmap.name.and.cell.line=gsub("nordihydroguaiaretic.acid","nordihydroguaiaretic\nacid",df3$cmap.name.and.cell.line)
  # df3$cmap.name.and.cell.line=gsub("15-delta.prostaglandin.J2","15-delta\nprostaglandin J2",df3$cmap.name.and.cell.line)
  # df3$cmap.name.and.cell.line=gsub("-0000","",df3$cmap.name.and.cell.line)

  df3[4, ]$rank2 <- 1.2
  df3[c(14, 17, 21), 11] <- df3[c(14, 17, 21), 11] - 0.25
  df3[9, ]$rank2 <- 1.15
  df3[10, ]$rank2 <- 0.8
  df3[13, ]$enrichment2 <- -0.90

  theme_set(
    theme_bw() +
      theme(
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "bottom",
        legend.box = "vertical"
      ) + theme_void()
  )

  ggplot(df3, aes(x = rank2, y = abs(enrichment2))) +
    geom_point(aes(color = as.factor(...3), size = `-log2(pValue)`, alpha = alpha)) +
    scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name = "Cell lines") +
    scale_size(range = c(0.5, 20), name = "pValue", labels = c(0.05, 0.03, 0.02, 0.01, "<0.01")) + # Adjust the range of points size
    geom_vline(xintercept = c(1), colour = "#00AFBB", linetype = "dashed") +
    geom_vline(xintercept = 2, colour = "#E7B800", linetype = "dashed") +
    geom_vline(xintercept = 3, colour = "#FC4E07", linetype = "dashed") +
    scale_alpha("Bibliometric\nscore", labels = c("Very high", "High", "Intermediate", "Low", "Very low")) +
    scale_x_continuous(breaks = c(1:3)) +
    geom_text(aes(label = cmap.name.and.cell.line), hjust = 0.5, vjust = 0.5) +
    annotate("label", x = 0.82, y = 0.98, label = "Higher ES (1)", fill = "grey95") +
    annotate("label", x = 0.82, y = 0.15, label = "Low ES (0.4)", fill = "grey95")

  ggplot(df3, aes(x = rank2, y = abs(enrichment2))) +
    geom_point(aes(color = as.factor(...3), size = alpha, alpha = 0.8)) +
    scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name = "Cell lines") +
    scale_size(range = c(0.5, 20), name = "Bibliometric\nscore", labels = c(1, 0.8, 0.5, 0.2, 0), breaks = c(0, 0.2, 0.5, 0.8, 1)) + # Adjust the range of points size
    geom_vline(xintercept = c(1), colour = "#00AFBB", linetype = "dashed") +
    geom_vline(xintercept = 2, colour = "#E7B800", linetype = "dashed") +
    geom_vline(xintercept = 3, colour = "#FC4E07", linetype = "dashed") +
    scale_x_continuous(breaks = c(1:3)) +
    geom_text(aes(label = cmap.name.and.cell.line), hjust = 0.5, vjust = 0.5) +
    annotate("label", x = 0.82, y = 0.98, label = "Higher ES (1)", fill = "grey95") +
    annotate("label", x = 0.82, y = 0.15, label = "Low ES (0.4)", fill = "grey95")

  if (!(is.null(output_path))) {
    ggsave("../plots/bubble_cmap.png")
    save.image(file = "../plots/bubble.data.Rdata")
  }
}
