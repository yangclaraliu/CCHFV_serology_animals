species_inuse <- "cattle"

tab <- df %>% 
  dplyr::filter(.data[[species_inuse]] == 1) %>% 
  mutate(year = year(year_study_start)) %>% 
  dplyr::filter(!is.na(continent),
                !is.na(year),
                !is.na(bias),
                bias != 0)

print(dim(tab))

dat <- escalc(measure="PLO", 
              xi = Numerator, 
              ni = Denominator, 
              data= tab)

dat$study <- factor(dat$Title)
dat$esid  <- ave(dat$Title, dat$Title, FUN = seq_along)

stopifnot(all(is.finite(dat$yi)), all(is.finite(dat$vi)), all(dat$vi > 0))

res <- rma.mv(yi, vi, 
              data = dat, 
              random = ~ 1 | study/esid,
              mods = ~ continent + year + bias, 
              method = "REML")

dat_ag <- aggregate(dat, cluster = First.author, struct="CS", rho=0.5)
res_ag <- rma(yi, vi, data=dat_ag, method="REML")

pdf(paste0("figures/forest_plot_", species_inuse,"_SA.pdf"), width=8.5, height=0.25*nrow(dat_ag))
forest(res_ag, 
       xlab = paste0("Proportion seropositive in ", species_inuse), 
       slab = paste0(dat_ag$First.author, ", " ,dat_ag$Year.of.publication),
       atransf = transf.ilogit,   # back-transform logit to proportion
       at = transf.logit(c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75))) 
dev.off()

pdf(paste0("figures/funnel_plot_", species_inuse, "_SA.pdf"), width = 6, height = 6)
funnel(res, main = str_to_title(species_inuse))
dev.off()

summary(res)

library(broom)
library(clubSandwich)

# output
coef_tbl <- broom::tidy(res, conf.int = TRUE)
coef_rob <- clubSandwich::coef_test(res, vcov = "CR2", cluster = dat$First.author)
glance_tbl <- broom::glance(res)   # REML logLik, AIC, etc. (where available)

write_csv(coef_tbl, paste0("results/coefficients_", species_inuse, "_SA.csv"))
write_csv(coef_rob, paste0("results/coefficients_robust_", species_inuse, "_SA.csv"))
write_csv(glance_tbl, paste0("results/diagnostics_", species_inuse, "_SA.csv"))

# pooled prediction
if(species_inuse == "camel") continents_to_create <- c("Africa", "Asia")
if(species_inuse != "camel") continents_to_create <- c("Africa", "Asia", "Europe")

newdat <- expand.grid(
  continent = continents_to_create,
  year      = median(dat$year, na.rm = TRUE),
  bias1     = 0) # , bias2     = 0)

X <- model.matrix(~ continent + year + bias1, # + bias2, 
                  data = newdat)

pred_cont <- predict(res,
                     newmods = X[, -1, drop = FALSE],  # drop intercept col
                     transf = transf.ilogit) %>%  data.frame %>% 
  add_column(continent = unique(dat$continent))

pred <- predict(res_ag, transf = transf.ilogit)
pred_df <- data.frame(
  estimate = pred$pred, ci.lb = pred$ci.lb, ci.ub = pred$ci.ub,
  pi.lb = pred$pi.lb, pi.ub = pred$pi.ub
)

write_csv(pred_df, paste0("results/prediction_interval_overall_", species_inuse, "_SA.csv"))
write_csv(pred_cont, paste0("results/prediction_interval_bycontinent_", species_inuse, "_SA.csv"))
write_rds(res, paste0("results/output_", species_inuse, "_SA.rds"))