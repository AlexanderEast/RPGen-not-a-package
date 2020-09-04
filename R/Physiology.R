httkvars = function(p) {
  reths <- c("Mexican American", "Other Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Other")
  q <- data.table::copy(p)
  nreth <- rep(5,nrow(q))
  nreth[q$ethnicity=="M"] <- 1
  nreth[p$ethnicity=="O"] <- 2
  nreth[p$ethnicity=="N" & p$race=="W"] <- 3
  nreth[p$ethnicity=="N" & p$race=="B"] <- 4
  q$reth <- as.factor(reths[nreth])
  q.months <- get.randoms("months",nrow(q),g$seeds,g$var.list,0)
  q$age_months <- 12*q$age+floor(12*q.months)
  q$temp_age_months <- 12*q$age+floor(12*q.months)
  q$age_years <- q$age
  q$gender[q$gender=="M"] <- "Male"
  q$gender[q$gender=="F"] <- "Female"
  q$num <- 1:nrow(q)
  return(q)
}

random_gen_height_weight = function(hbw_dt,specs) {
  mean_logh <- g <- gender <- r <- reth <- height_spline <- NULL
  age_months <- mean_logbw <- weight_spline <- hw_kde <- nkde <- NULL
  id <- weight <- NULL
  logbw_resid <- height <- logh_resid <- NULL
  hbw_dt <- data.table::copy(hbw_dt)
  hbw_dt[, `:=`(age, age_years)]
  hbw_dt[, `:=`(age_years, pmin(age_years,99))]
  hbw_dt[, `:=`(age_months,pmin(age_months,99*12))]    # cap age at 99 years
  hbw_dt[, `:=`(mean_logh,  predict(spline_heightweight[g == gender & r == reth, height_spline][[1]], x = age_months)$y), by = list(gender, reth)]
  hbw_dt[, `:=`(mean_logbw, predict(spline_heightweight[g == gender & r == reth, weight_spline][[1]], x = age_months)$y), by = list(gender, reth)]
  spline_kde <- spline_heightweight[, list(g, r, hw_kde, nkde)]
  setnames(spline_kde, c("g", "r"), c("gender", "reth"))
  hbw_dt[, `:=`(id, 1:nrow(hbw_dt))]
  hbw_dt <- merge(hbw_dt, spline_kde, by = c("gender", "reth"))
  hbw_dt[, `:=`(q.nkde, get.randoms("nkde",nrow(hbw_dt),specs$seeds,specs$var.list,0))] 
  hbw_dt[, `:=`(q.hw1,  get.randoms("hw1", nrow(hbw_dt),specs$seeds,specs$var.list,0))] 
  hbw_dt[, `:=`(q.hw2,  get.randoms("hw2", nrow(hbw_dt),specs$seeds,specs$var.list,0))]
  hbw_dt[, `:=`(c("logbw_resid", "logh_resid"), as.data.frame(hw_kde[[1]]$x[sampleq(unique(nkde), hw_kde[[1]]$w, q.nkde), ] 
                                                              + bi_norm_cor(cbind(q.hw1,q.hw2), mean=c(0, 0), sigma=hw_kde[[1]]$H))), by = list(gender, reth)]
  hbw_dt[, `:=`(weight, pmin(exp(mean_logbw + logbw_resid),160))]   # cap at 160 kg
  hbw_dt[, `:=`(height, pmin(exp(mean_logh + logh_resid),225))]     # cap at 225 cm
  hbw_dt[, `:=`(id, NULL)]
  hbw_dt[, `:=`(hw_kde, NULL)]
  hbw_dt[, `:=`(nkde, NULL)]
  hbw_dt[, `:=`(q.nkde,NULL)]
  hbw_dt[, `:=`(q.hw1,NULL)]
  hbw_dt[, `:=`(q.hw2,NULL)]
  return(hbw_dt)
}

random_tissue_masses_flows = function (tmf_dt) {
  id <- mass_mean <- height_ref <- height <- mass_ref <- tissue <- NULL
  gender <- age_years <- age_months <- weight <- bonemass_mean <- NULL
  BSA <- mass_dist <- mass <- mass_cv <- flow_mean <- flow_ref <- NULL
  flow_frac <- flow <- flow_cv <- CO <- Adipose <- Bone <- NULL
  org_mass_sum <- Blood <- Other_mass <- Adipose_mass <- NULL
  org_flow_check <- CO_flow <- weight_adj <- BSA_adj <- NULL
  million.cells.per.gliver <- NULL
  tmf_dt <- copy(tmf_dt)
  tmf_dt[, `:=`(id, 1:nrow(tmf_dt))]
  tmp_dt <- merge(tmf_dt, mcnally_dt, by = "gender", allow.cartesian = TRUE)
  tmp_dt[, `:=`(mass_mean, tissue_scale(height_ref = height_ref, height_indiv = height, tissue_mean_ref = mass_ref))]
  tmp_dt[tissue == "Brain", `:=`(mass_mean, brain_mass(gender = gender, age_years = age_years))]
  tmp_dt[tissue == "Bone", `:=`(mass_mean, bone_mass_age(age_years = age_years, 
                                                         age_months = age_months, height = height, weight = weight, gender = gender))]
  bone_mass_mean <- tmp_dt[tissue == "Bone", list(id, mass_mean)]
  setnames(bone_mass_mean, "mass_mean", "bonemass_mean")
  tmp_dt <- merge(tmp_dt, bone_mass_mean, by = ("id"))
  tmp_dt[tissue == "Skeleton", `:=`(mass_mean, bonemass_mean/0.5)]
  tmp_dt[, `:=`(bonemass_mean, NULL)]
  rm(bone_mass_mean)
  tmp_dt[, `:=` (q.norm, get.randoms("norm",nrow(tmp_dt),g$seeds,g$var.list,0))] 
  tmp_dt[, `:=` (q.logn, get.randoms("logn",nrow(tmp_dt),g$seeds,g$var.list,0))] 
  tmp_dt[tissue == "Muscle", `:=`(mass_mean, skeletal_muscle_mass(smm = mass_mean, age_years = age_years, height = height, gender = gender))]
  tmp_dt[tissue == "Liver" & age_years <= 18, `:=`(mass_mean, liver_mass_children(height = height, weight = weight, gender = gender))]
  tmp_dt[tissue == "Kidney" & age_years <= 18, `:=`(mass_mean, kidney_mass_children(weight = weight, height = height, gender = gender))]
  tmp_dt[tissue == "Pancreas" & age_years <= 18, `:=`(mass_mean, pancreas_mass_children(height = height, weight = weight, gender = gender))]
  tmp_dt[tissue == "Spleen" & age_years <= 18, `:=`(mass_mean, spleen_mass_children(height = height, weight = weight, gender = gender))]
  tmp_dt[tissue == "Lung" & age_years <= 18, `:=`(mass_mean, lung_mass_children(height = height, weight = weight, gender = gender))]
  tmp_dt[, `:=`(BSA, body_surface_area(BW = weight, H = height, age_years = age_years))]
  tmp_dt[tissue == "Skin", `:=`(mass_mean, skin_mass_bosgra(BSA = BSA))]
  tmp_dt[tissue == "Blood", `:=`(mass_mean, blood_weight(BSA = BSA/(100^2), gender = gender))]
  tmp_dt[tissue == "Blood" & mass_mean < 0.2, `:=`(mass_mean, blood_mass_correct(blood_mass = mass_mean, age_months = age_months, 
                                                                                 age_years = age_years, gender = gender, weight = weight))]
  tmp_dt[mass_dist == "Normal", `:=`(mass, truncnorm::qtruncnorm(q.norm, a = 0, mean = mass_mean, sd = mass_cv * mass_mean))]
  tmp_dt[mass_dist == "Log-normal", `:=`(mass, exp(pnorm(q.logn, mean = log(mass_mean), sd = sqrt(log(mass_cv^2 + 1)))))]
  tmp_dt[tissue == "CO", `:=`(flow_mean, tissue_scale(height_ref = height_ref, height_indiv = height,
                                                      tissue_mean_ref = 1.05 * flow_ref) * (1 - max(0, 0.005 * (age_years - 25))))]
  CO_flow_mean <- tmp_dt[tissue == "CO", list(id, flow_mean)]
  setnames(CO_flow_mean, "flow_mean", "CO_flow_mean")
  tmp_dt <- merge(tmp_dt, CO_flow_mean, by = "id", allow.cartesian = TRUE)
  tmp_dt[tissue != "CO", `:=`(flow_mean, flow_frac * CO_flow_mean)]
  tmp_dt[, `:=` (q.flow, get.randoms("flow",nrow(tmp_dt),g$seeds,g$var.list,0))] 
  tmp_dt[tissue != "CO" & tissue != "Lung" & !is.na(flow_mean),`:=`(flow, truncnorm::qtruncnorm(q.flow, a=0, mean=flow_mean, sd=flow_cv*flow_mean))]
  tmp_dt[tissue == "Lung", `:=`(flow, flow_frac * CO_flow_mean)]
  tmp_dt[tissue == "CO", `:=`(flow, CO_flow_mean)]
  mass_cast <- data.table::dcast.data.table(tmp_dt, id ~ tissue, value.var = "mass")
  mass_cast[, `:=`(CO, NULL)]
  mass_cast[, `:=`(Adipose, NULL)]
  mass_cast[, `:=`(Bone, NULL)]
  setnames(mass_cast, names(mass_cast)[names(mass_cast) != "id"], paste(names(mass_cast)[names(mass_cast) != "id"], "mass", sep = "_"))
  mass_cast[, `:=`(org_mass_sum, Reduce("+", .SD)), .SDcols = grep(x = names(mass_cast), pattern = "mass", value = TRUE)]
  flow_cast <- data.table::dcast.data.table(tmp_dt, id ~ tissue, value.var = "flow")
  flow_cast[, `:=`(Blood, NULL)]
  flow_cast[, `:=`(Bone, NULL)]
  setnames(flow_cast, names(flow_cast)[names(flow_cast) != "id"], paste(names(flow_cast)[names(flow_cast) != "id"], "flow", sep = "_"))
  tmf_dt <- merge(tmf_dt, mass_cast, by = "id")
  tmf_dt <- merge(tmf_dt, flow_cast, by = "id")
  tmf_dt[, `:=`(Other_mass, (0.033 + 0.014) * weight)]
  tmf_dt[, `:=`(org_mass_sum, org_mass_sum + Other_mass)]
  tmf_dt[, `:=`(q.adip, get.randoms("adip",nrow(tmf_dt),g$seeds,g$var.list,0))] 
  tmf_dt[, `:=`(Adipose_mass, exp(pnorm(q.adip, mean = log(pmax(1,weight-org_mass_sum)), sd = sqrt(log(0.42^2 + 1)))))]
  tmf_dt[(weight - org_mass_sum) <= 1, `:=`(Adipose_mass, 0)]
  tmf_dt[, `:=`(org_flow_check, Reduce("+", .SD)), .SDcols = names(flow_cast)[!(names(flow_cast) %in% c("CO_flow", "id"))]]
  tmf_dt[, `:=`(org_flow_check, org_flow_check/CO_flow)]
  tmf_dt[, `:=`(weight_adj, org_mass_sum + Adipose_mass)]
  tmf_dt[, `:=`(BSA_adj, body_surface_area(BW = weight_adj, H = height, age_years = age_years))]
  mu <- log(10^(-0.66 * log10(tmf_dt[, age_years]) + 3.1))
  mu[tmf_dt[, age_years < 20]] <- log(10^(-0.66 * log10(19) + 3.1))
  sigma.total <- ((log(444) - log(99))/2 + (log(99) - log(23))/2)/2
  Fval <- qf(0.012/2, df1 = 1, df2 = 26, lower.tail = FALSE)
  R2 <- Fval/(1 + Fval)
  sigma <- sqrt((1 - R2) * sigma.total^2)
  tmf_dt[, `:=` (q.gliv, get.randoms("gliv",nrow(tmf_dt),g$seeds,g$var.list,0))] 
  tmf_dt[, `:=`(million.cells.per.gliver, exp(pnorm(q.gliv, mean = mu, sd = sigma)))]
  setnames(tmf_dt, c("Kidney_mass", "Kidney_flow", "CO_flow"), c("Kidneys_mass", "Kidneys_flow", "CO"))
  tmf_dt[, `:=`(id, NULL)]
  # tmf_dt[, `:=`(org_mass_sum, NULL)]
  return(tmf_dt)
}

