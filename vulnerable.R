

# our most vulnerable citizens --------------------------------------------


library(readr)
library(stringr)
vulnerable <- read_csv("vulnerable.csv")

vulnerable$speech <- tolower(vulnerable$speech)

vulnerable$count <- str_count(vulnerable$speech, "our most vulnerable citizen")

vulnerable$speech_date <- as.Date(vulnerable$speech_date)

vulnerable2 <- aggregate(count ~ speech_date, data = vulnerable, FUN = sum)



vul_zoo <- zoo(vulnerable2$count, order.by = vulnerable2$speech_date)



class(vul_zoo)

summary(vul_zoo)

g <- seq(start(vul_zoo), end(vul_zoo), by = 1)
z.na <- na.locf(vul_zoo, xout = g)
rng <- range(time(vul_zoo))

vul_zoo <- merge(vul_zoo, zoo(, seq(rng[1], rng[2], by = "day")))

summary(vul_zoo)

vul_zoo <- as.data.frame(vul_zoo)

setDT(vul_zoo, keep.rownames = TRUE)[]

names(vul_zoo)[1] <- "speech_date"

vul_zoo$speech_date <- as.Date(vul_zoo$speech_date)

summary(vul_zoo)

vul_zoo$sitting <- 0

dates <- read_csv("dates.csv")

vul_zoo$sitting[vul_zoo$speech_date %in% dates$Date] <- 1

vul_zoo$sitting <- as.logical(vul_zoo$sitting)

nrow(vul_zoo)

summary(vul_zoo)

sitting_yes <- subset(vul_zoo, sitting == TRUE)

sitting_no <- subset(vul_zoo, is.na(sitting) == TRUE)

nrow(sitting_no) + nrow(sitting_yes)

summary(sitting_yes)

sitting_yes[is.na(sitting_yes)] <- 0

vull_zoo <- rbind(sitting_no, sitting_yes)

vull_zoo$sitting <- NULL

vull_zoo <- read.zoo(vull_zoo)

class(vull_zoo)

summary(vull_zoo)

vull_zoo_roll <- rollapply(vull_zoo, 365.25, FUN = mean, na.rm = TRUE, align = "right")

summary(vull_zoo_roll)

vull_zoo_gg <- as.data.frame(vull_zoo_roll)

setDT(vull_zoo_gg, keep.rownames = TRUE)[]

head(vull_zoo_gg)

summary(vull_zoo_gg)

names(vull_zoo_gg)[1] <- "Date"

vull_zoo_gg <- melt(vull_zoo_gg, id.vars = "Date")

vull_zoo_gg <- data.frame(vull_zoo_gg, stringsAsFactors = FALSE)

vull_zoo_gg$Date <- as.Date(vull_zoo_gg$Date)

class(vull_zoo_gg$variable)


fmt_dcimals <- function(decimals = 0) {
  # return a function responpsible for formatting the axis labels with a given number of decimals
  function(x) as.character(round(x, decimals))
}

summary(vull_zoo_gg)

vull_zoo_gg$variable <- as.character(vull_zoo_gg$variable)

vull_zoo_gg$variable[vull_zoo_gg$variable == "vull_zoo_roll"] <- "our most vulnerable citizen"

vull_zoo_gg$variable <- as.factor(vull_zoo_gg$variable)

### Best and most acurate plot
p3 <- ggplot(vull_zoo_gg, aes(x = Date, group = variable, col = variable))
## Need to remove days that Parliament did not sit Need to drop scientific notation
p3 + geom_smooth(aes(y = value, linetype = variable, col = variable),
                 size = 1.5, formula = y ~ log(x), se = FALSE) +
  coord_cartesian(xlim = c(as.Date("1975-01-01"), as.Date("2016-11-24"))) + 
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") + 
  scale_y_continuous(trans = log_trans(5), breaks = base_breaks(), 
                     "Mentions per Day (Logarithmic Scale)", labels = fmt_dcimals(3)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "bottom") +
  ggtitle("Average number of mentions of \"our most vulnerable citizen\", per day of debate, 1978-2016")

summary(vull_zoo_gg)


