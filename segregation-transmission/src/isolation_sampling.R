require(ggplot2)
require(packcircles)
p1 <- 0.1
p2 <- 1-p1
isolation <- 0.5
intensity <- 2

num_contacts <- 250

a_in_contact_p <- isolation
a_out_contact_p <- 1-a_in_contact_p
b_in_contact_p <- (p2-(p1*a_out_contact_p))/p2
b_out_contact_p <- 1-b_in_contact_p


## Make a dataframe with circles for the in-group
num_in <- round(num_contacts*a_in_contact_p)
num_out <- num_contacts-num_in
df <- data.frame(group = c(rep(2, num_in), rep(1, num_out)),
ingroup_ratio = c(rep(intensity, num_in),rep(1, num_out)))
df$intensity <- rbeta(nrow(df), 20,20)*df$ingroup_ratio


limits <- c(-10,10)
res <- circleRepelLayout(df$intensity, xlim = limits, ylim = limits)

dat.gg <- circleLayoutVertices(res$layout, sizetype = "radius")

df$id <- 1:nrow(df)

dat.gg <- dat.gg %>%
inner_join(select(df, id, intensity,group)) 
dat.gg$group <- as.factor(dat.gg$group)


t <- theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())

theme_set(t)

g <- ggplot(data = dat.gg, aes(x, y, group = id)) +
  geom_polygon(aes(fill=group, colour=group),alpha=0.3) +
  coord_equal(xlim=limits, ylim=limits) + 
  scale_fill_manual(breaks = c("1", "2"), 
                       values=c("red", "green")) + 
                       theme(legend.position = "none") 



ggsave("test_a.png")

## Dataframe with circles for the other group
num_in <- round(num_contacts*b_in_contact_p)
num_out <- num_contacts-num_in
df_b <- data.frame(group = c(rep(1, num_in), rep(2, num_out)))
df_b$intensity <- rbeta(nrow(df), 20,20)

res <- circleRepelLayout(df_b$intensity, xlim = limits, ylim = limits)

dat.gg <- circleLayoutVertices(res$layout, sizetype = "radius")

df_b$id <- 1:nrow(df)

dat.gg <- dat.gg %>%
inner_join(select(df_b, id, intensity,group)) 
dat.gg$group <- as.factor(dat.gg$group)


g <- ggplot(data = dat.gg, aes(x, y, group = id)) +
  geom_polygon(aes(fill=group, colour=group),alpha=0.3) +
  coord_equal(xlim=limits, ylim=limits) + 
  scale_fill_manual(breaks = c("1", "2"), 
                       values=c("red", "green")) + 
                       theme(legend.position = "none") 



ggsave("test_b.png")