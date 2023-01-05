library(LAMRSAControl)

## basemodel
data("events")
basemodel <- interventions(events = events, intervention ="basemodel")
result_basemodel <- trajectory(run(basemodel))

## decrease finishing mixing to to 0% d34, weekly model
data("events_cf34_cfp10_fmp0")
cf34_fmp0 <- interventions(events = events_cf34_cfp10_fmp0, intervention = "mixing")
result_cf34_fmp0 <- trajectory(run(cf34_fmp0))

## clean all-in-all-out pens when they are empty
data("events")
model_AIAO <- interventions(events = events, intervention ="cleaning", clean_AIAO = 0)
result_AIAO <- trajectory(run(model_AIAO))

## clean continuous flow pens in weekly interval (based on weaning occasions)
data("events")
model_CF <- interventions(events = events, intervention ="cleaning", clean_CF = 0)
result_CF <- trajectory(run(model_CF))

## clean both AIAO and CF pens
data("events")
model_AIAO_CF <- interventions(events = events, intervention ="cleaning", clean_AIAO = 0, clean_CF = 0)
result_AIAO_CF <- trajectory(run(model_AIAO_CF))

## disease test sows before weaning (default test sensitivity is 0.7)
data("events")
model_test_sows <- interventions(events = events, intervention = "testing", test_weaning = TRUE)
result_test_sows <- trajectory(run(model_test_sows))

## disease test growing gilts before moving to breeding (default test sensitivity is 0.7)
data("events")
model_test_gilts <- interventions(events = events, intervention = "testing", test_gilts = TRUE)
result_test_gilts <- trajectory(run(model_test_gilts))

## disease test both sows and gilts(default test sensitivity is 0.7)
data("events")
model_test_both <- interventions(events = events, intervention = "testing", test_weaning = TRUE, test_gilts = TRUE)
result_test_both <- trajectory(run(model_test_both))

## tests
#####################

## check that the phi in AIAO pen is 0 when there are no animals in the pen
AIAO_pens <- result_AIAO[result_AIAO$pentype %in% 7:10 & result_AIAO$npigs == 0, ]
stopifnot(!(any(AIAO_pens$phi > 0)))

## rows where CF cleaning indicator has been triggered
trigger <- result_AIAO_CF[result_AIAO_CF$cleaning_indicator == 1, ]
## check that these are all CF pens
stopifnot(!(any(trigger$pentype %in% c(7:10))))
## check that the following day these pens are cleaned
trigger_time <- unique(trigger$time)
cleaned <- result_AIAO_CF[result_AIAO_CF$time %in% trigger_time & result_AIAO_CF$pentype %in% c(1:6, 11), ]
stopifnot(all(cleaned$phi == 0))

## check that tests have been scheduled
pos_ind <- result_test_both[result_test_both$test_indicator > 0, ]
done_test <- result_test_both[result_test_both$result_indicator > 0,]
