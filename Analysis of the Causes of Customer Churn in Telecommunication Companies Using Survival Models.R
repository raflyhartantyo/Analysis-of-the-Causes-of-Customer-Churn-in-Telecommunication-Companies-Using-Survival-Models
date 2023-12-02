library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(survminer)
library(ggfortify)
library(naniar)
library(Publish)
library(ggtext)
library(fastDummies)
df=Churn.dataset
df$Churn[df$Churn == "Yes"] <- 1 # Replace "Yes" by 1
df$Churn[df$Churn == "No"] <- 0 # Replace "No" by 0
df$Churn=as.numeric(df$Churn)
km_fit <- survfit(Surv(tenure, Churn) ~ 1, data=df)
ggsurvplot(km_fit, conf.int=TRUE, risk.table=TRUE,, ylim = c(0.6,
                                                             1))
df=subset(df, select =
            -c(customerID,PhoneService,InternetService) )
df=subset(df,MultipleLines!='No phone service' )
df=subset(df,OnlineSecurity!='No internet service' )
cox = coxph(Surv(tenure, Churn) ~., data =df)
summary(cox)
ggsurvplot(survfit(cox), data = df,break.time.by = 150,
           risk.table=TRUE)
publish(cox)
plot(publish(cox), xlim=c(0,10), cex = 0.6)
km_fit <- survfit(Surv(tenure, Churn) ~ df$PaymentMethod,
                  data=df)
ggsurvplot(km_fit, conf.int=TRUE, pval=TRUE, risk.table=TRUE,
           legend.labs=c("Bank Transfer","Credit Card",
                         "Electronic Check",'Mailed Check'), legend.title="Partner?",
           palette=c("dodgerblue2", "orchid2","brown4",'gold3'),
           main="Customer Churn by Partner")
km_fit <- survfit(Surv(tenure, Churn) ~ df$Contract, data=df)
ggsurvplot(km_fit, conf.int=TRUE, pval=TRUE, risk.table=TRUE,
           legend.labs=c("Month-to-Month", "One Year",'Two Year'), legend.title="Internet Service?",
           palette=c("dodgerblue2", "orchid2",'brown4'),
           main="Customer Churn by Partner",ylim =c(0,1))

km_fit <- survfit(Surv(tenure, Churn) ~ df$OnlineSecurity,
                  data=df)
ggsurvplot(km_fit, conf.int=TRUE, pval=TRUE, risk.table=TRUE,
           legend.labs=c("No",'Yes'), legend.title="Online Security?",
           palette=c("dodgerblue2", "orchid2"),
           main="Customer Churn by Partner",ylim =c(0.25,1))

km_fit <- survfit(Surv(tenure, Churn) ~ df$TechSupport, data=df)
ggsurvplot(km_fit, conf.int=TRUE, pval=TRUE, risk.table=TRUE,
           legend.labs=c("No",'Yes'), legend.title="Tech Support?",
           palette=c("dodgerblue2", "orchid2"),
           main="Customer Churn by Partner",ylim =c(0.3,1))

km_fit <- survfit(Surv(tenure, Churn) ~ df$StreamingTV, data=df)
ggsurvplot(km_fit, conf.int=TRUE, pval=TRUE, risk.table=TRUE,
           legend.labs=c("No",'Yes'), legend.title="Streaming TV?",
           palette=c("dodgerblue2", "orchid2"),
           main="Customer Churn by Partner",ylim =c(0.3,1))

km_fit <- survfit(Surv(tenure, Churn) ~ df$PaperlessBilling,
                  data=df)
ggsurvplot(km_fit, conf.int=TRUE, pval=TRUE, risk.table=TRUE,
           legend.labs=c("No",'Yes'), legend.title="Paperless Billing?",
           palette=c("dodgerblue2", "orchid2"),
           main="Customer Churn by Partner",ylim =c(0.3,1))
