newdata <- unique(
  quantity.rand.st[,c(
    "tchiyr.std",
    "SpkrType", "nsk.std",
    "group_corpNE")])

# Estimate the predicted counts
# Conditional
X.cond <- model.matrix(
  lme4::nobars(formula(tds.rand.st.zinb)[-2]), newdata)
beta.cond <- fixef(tds.rand.st.zinb)$cond
pred.cond <- X.cond %*% beta.cond
# Zero-inflation
ziformula <- tds.rand.st.zinb$modelInfo$allForm$ziformula
X.zi <- model.matrix(lme4::nobars(ziformula), newdata)
beta.zi <- fixef(tds.rand.st.zinb)$zi
pred.zi <- X.zi %*% beta.zi
# Convert these (linear) predictions back to the response scale
pred.ucount <- exp(pred.cond) * (1 - plogis(pred.zi))

# Estimate confidence intervals
# Conditional
pred.condpar.psim <- mvrnorm(
  1000, mu = beta.cond, Sigma = vcov(tds.rand.st.zinb)$cond)
pred.cond.psim <- X.cond %*% t(pred.condpar.psim)
# Zero-inflation
pred.zipar.psim <- mvrnorm(
  1000, mu = beta.zi, Sigma = vcov(tds.rand.st.zinb)$zi)
pred.zi.psim <- X.zi %*% t(pred.zipar.psim)
# Convert these (linear) predictions back to the response scale
pred.ucount.psim <- exp(pred.cond.psim) * (1 - plogis(pred.zi.psim))
ci.ucount <- t(apply(
  pred.ucount.psim, 1, quantile, c(0.025, 0.975), na.rm = TRUE))
ci.ucount <- data.frame(ci.ucount)
names(ci.ucount) <- c("ucount.low", "ucount.high")

# Combine the predictions info
# pred.ucount.all <- data.frame(newdata, pred.ucount)
pred.ucount.all <- data.frame(newdata, pred.ucount, ci.ucount)
pred.ucount.all <- left_join(pred.ucount.all,
  select(quantity.rand.st, c("tchiyr.std", "age_mo_round"))) %>%
  filter(!(ucount.high == Inf)) %>%
  mutate(
    Sample = factor(group_corpNE, labels = c(
      "NA English", "Arg. Spanish",
      "Tseltal", "UK English", "Yélî Dnye"))) %>%
  mutate(
    Sample = factor(Sample, levels = c(
      "NA English", "UK English", "Arg. Spanish",
      "Tseltal", "Yélî Dnye")))

# Summarize the real counts
quantity.rand.st$tds_mph.st.rnd <- round(
  quantity.rand.st$tds_mph.st, 0)
real.count <- plyr::ddply(quantity.rand.st,
  ~ tchiyr.std + SpkrType +
    nsk.std + group_corpNE,
  summarize,
  m = median(tds_mph.st.rnd),
  mu = mean(tds_mph.st.rnd))
real.count <- left_join(real.count,
  select(quantity.rand.st, c("tchiyr.std", "age_mo_round"))) %>%
  mutate(
    Sample = factor(group_corpNE, labels = c(
      "NA English", "Arg. Spanish",
      "Tseltal", "UK English", "Yélî Dnye"))) %>%
  mutate(
    Sample = factor(Sample, levels = c(
      "NA English", "UK English", "Arg. Spanish",
      "Tseltal", "Yélî Dnye")))

# Plot the model estimates and real averages

# Female adult speech
pred.ucount.FAonly <- pred.ucount.all %>%
  filter(SpkrType == "Woman") %>%
  group_by(age_mo_round, Sample) %>%
  summarize(
    mean.pred.ucount = mean(pred.ucount),
    mean.ucount.low = mean(ucount.low),
    mean.ucount.high = mean(ucount.high))
real.count.FAonly <- real.count %>%
  filter(SpkrType == "Woman") %>%
  group_by(age_mo_round, Sample) %>%
  summarize(
    mean.m = mean(m),
    mean.mu = mean(mu))
tds.model.FA.plot <- ggplot(
  pred.ucount.FAonly, aes(
    x = age_mo_round, y = mean.pred.ucount,
    colour = Sample, fill = Sample)) +
  geom_smooth(aes(
    ymin = mean.ucount.low, ymax = mean.ucount.high,
    linetype = Sample),
    method = "glm", formula = y~x,
    method.args = list(family = gaussian(link = 'log')), se = FALSE) +
  geom_point(shape = 19, size = 1,
    position = position_dodge(width = 0.1), alpha = 0.2) +
  geom_errorbar(aes(
    ymin = mean.ucount.low, ymax = mean.ucount.high),
    position = position_dodge(width = 0.1), alpha = 0.2) +
  # geom_point(data = real.count.FAonly, aes(
  #   x = age_mo_round, y = mean.m,
  #   colour = Sample), shape = 0, size = 2,
  #   position = position_dodge(width = 0.1)) +
  # geom_point(data = real.count.FAonly, aes(
  #   x = age_mo_round, y = mean.mu,
  #   colour = Sample), shape = 5, size = 2) +
  theme_apa() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Women") +
  scale_x_continuous("Age (months)",
    breaks = seq(0, 36, 6), limits = c(0, 36)) +
  scale_y_continuous("Estimated TCDL min/hr",
    breaks = seq(0, 10, 2), limits = c(0, 15)) +
  coord_cartesian(ylim = c(0, 10)) +
  theme(
    legend.position = "none",
    # axis.title.y = element_blank(),
    axis.title.x = element_blank())
# tds.model.FA.plot

# Male adult speech
pred.ucount.MAonly <- pred.ucount.all %>%
  filter(SpkrType == "Man") %>%
  group_by(age_mo_round, Sample) %>%
  summarize(
    mean.pred.ucount = mean(pred.ucount),
    mean.ucount.low = mean(ucount.low),
    mean.ucount.high = mean(ucount.high))
real.count.MAonly <- real.count %>%
  filter(SpkrType == "Man") %>%
  group_by(age_mo_round, Sample) %>%
  summarize(
    mean.m = mean(m),
    mean.mu = mean(mu))
tds.model.MA.plot <- ggplot(
  pred.ucount.MAonly, aes(
    x = age_mo_round, y = mean.pred.ucount,
    colour = Sample, fill = Sample)) +
  geom_smooth(aes(
    ymin = mean.ucount.low, ymax = mean.ucount.high,
    linetype = Sample),
    method = "glm", formula = y~x,
    method.args = list(family = gaussian(link = 'log')), se = FALSE) +
  geom_point(shape = 19, size = 1,
    position = position_dodge(width = 0.1), alpha = 0.2) +
  geom_errorbar(aes(
    ymin = mean.ucount.low, ymax = mean.ucount.high),
    position = position_dodge(width = 0.1), alpha = 0.2) +
  # geom_point(data = real.count.MAonly, aes(
  #   x = age_mo_round, y = mean.m,
  #   colour = Sample), shape = 0, size = 2,
  #   position = position_dodge(width = 0.1)) +
  # geom_point(data = real.count.MAonly, aes(
  #   x = age_mo_round, y = mean.mu,
  #   colour = Sample), shape = 5, size = 2) +
  theme_apa() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Men") +
  scale_x_continuous("Age (months)",
    breaks = seq(0, 36, 6), limits = c(0, 36)) +
  scale_y_continuous("Estimated TCDL min/hr",
    breaks = seq(0, 10, 2), limits = c(0, 15)) +
  coord_cartesian(ylim = c(0, 10)) +
  theme(
    legend.position = "none",
    # axis.title.x = element_blank(),
    axis.title.y = element_blank())
# tds.model.MA.plot

# All child speech
pred.ucount.OConly <- pred.ucount.all %>%
  filter(SpkrType == "Child") %>%
  group_by(age_mo_round, Sample) %>%
  summarize(
    mean.pred.ucount = mean(pred.ucount),
    mean.ucount.low = mean(ucount.low),
    mean.ucount.high = mean(ucount.high))
real.count.OConly <- real.count %>%
  filter(SpkrType == "Child") %>%
  group_by(age_mo_round, Sample) %>%
  summarize(
    mean.m = mean(m),
    mean.mu = mean(mu))
tds.model.OC.plot <- ggplot(
  pred.ucount.OConly, aes(
    x = age_mo_round, y = mean.pred.ucount,
    colour = Sample, fill = Sample)) +
  geom_smooth(aes(
    ymin = mean.ucount.low, ymax = mean.ucount.high,
    linetype = Sample),
    method = "glm", formula = y~x,
    method.args = list(family = gaussian(link = 'log')), se = FALSE) +
  geom_point(shape = 19, size = 1,
    position = position_dodge(width = 0.1), alpha = 0.2) +
  geom_errorbar(aes(
    ymin = mean.ucount.low, ymax = mean.ucount.high),
    position = position_dodge(width = 0.1), alpha = 0.2) +
  # geom_point(data = real.count.OConly, aes(
  #   x = age_mo_round, y = mean.m,
  #   colour = Sample), shape = 0, size = 2,
  #   position = position_dodge(width = 0.1)) +
  # geom_point(data = real.count.OConly, aes(
  #   x = age_mo_round, y = mean.mu,
  #   colour = Sample), shape = 5, size = 2) +
  theme_apa() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Other children") +
  scale_x_continuous("Age (months)",
    breaks = seq(0, 36, 6), limits = c(0, 36)) +
  scale_y_continuous("Estimated TCDL min/hr",
    breaks = seq(0, 10, 2), limits = c(0, 15)) +
  coord_cartesian(ylim = c(0, 10)) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
# tds.model.OC.plot

# All speech
pred.ucount.AllSpkrs <- pred.ucount.all %>%
  group_by(age_mo_round, Sample) %>%
  summarize(
    mean.pred.ucount = mean(pred.ucount),
    mean.ucount.low = mean(ucount.low),
    mean.ucount.high = mean(ucount.high))
real.count.AllSpkrs <- real.count %>%
  group_by(age_mo_round, Sample) %>%
  summarize(
    mean.m = mean(m),
    mean.mu = mean(mu))
tds.model.AllSpkrs.plot <- ggplot(
  pred.ucount.AllSpkrs, aes(
    x = age_mo_round, y = mean.pred.ucount,
    colour = Sample, fill = Sample)) +
  geom_smooth(aes(
    ymin = mean.ucount.low, ymax = mean.ucount.high,
    linetype = Sample),
    method = "glm", formula = y~x,
    method.args = list(family = gaussian(link = 'log')), se = FALSE) +
  geom_point(shape = 19, size = 1,
    position = position_dodge(width = 0.1), alpha = 0.2) +
  geom_errorbar(aes(
    ymin = mean.ucount.low, ymax = mean.ucount.high),
    position = position_dodge(width = 0.1), alpha = 0.2) +
  # geom_point(data = real.count.AllSpkrs, aes(
  #   x = age_mo_round, y = mean.m,
  #   colour = Sample), shape = 0, size = 2,
  #   position = position_dodge(width = 0.1)) +
  # geom_point(data = real.count.AllSpkrs, aes(
  #   x = age_mo_round, y = mean.mu,
  #   colour = Sample), shape = 5, size = 2) +
  theme_apa() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("All speaker types") +
  scale_x_continuous("Age (months)",
    breaks = seq(0, 36, 6), limits = c(0, 36)) +
  scale_y_continuous("Estimated TCDL min/hr",
    breaks = seq(0, 10, 2), limits = c(0, 15)) +
  coord_cartesian(ylim = c(0, 10)) +
  theme(
    legend.position = "bottom",
    # axis.title.y = element_blank(),
    axis.title.x = element_blank())
# tds.model.AllSpkrs.plot

tds.model.AllSpkrs.plot /
(tds.model.FA.plot | tds.model.MA.plot | tds.model.OC.plot)

ggsave(
  "tdl-estimates-plot.png",
  plot = last_plot(),
  device = "png",
  path = "plots",
  scale = 1,
  width = 20,
  height = 15,
  units = "cm",
  dpi = 300)
