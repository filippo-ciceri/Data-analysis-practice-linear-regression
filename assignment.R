library(matahari)
dance_start(value = FALSE, contents = FALSE)

library(collegeIncome)
data(college)
class(college)
sapply(college, class)

college[] <- lapply(college, type.convert, as.is = TRUE)
college$major_category <- as.factor(college$major_category)
college$sample_size <- as.numeric(college$sample_size)
sapply(college, class)
college[is.na(college)] <- 0
is.finite.data.frame <- function(obj){
    sapply(obj,FUN = function(x) all(is.finite(x)))
}
college[!is.finite.data.frame(college)] <- 0

#college$major_category <- as.numeric(college$major_category)

fit<-lm(median~major_category-1,college)
summary(fit)

fit2<-update(fit, .~.+total+sample_size-1)
summary(fit2)

anova(fit,fit2)

fit3<-update(fit, .~.+perc_men-1)
summary(fit3)

anova(fit,fit2,fit3)

fit4<-update(fit, .~.+perc_college_jobs+
                       perc_non_college_jobs+
                       perc_low_wage_jobs-1)
summary(fit4)

anova(fit,fit2,fit3,fit4)

fit5<-update(fit, .~.+perc_employed+
                       perc_employed_fulltime+
                       perc_employed_parttime+
                       perc_employed_fulltime_yearround-1)
summary(fit5)
anova(fit,fit5)

plot(fit5, which = 1)
plot(fit5, which = 2)
plot(fit5, which = 3)
plot(fit5, which = 4)
plot(fit5, which = 5)
plot(fit5, which = 6)

dance_save("college_major_analysis.rds")