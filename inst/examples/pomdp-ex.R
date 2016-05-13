# dependent functions
source("milad.R")

gdist = "ricker"
Num_s = 30
Num_a = 31
Num_z = 32
sigma_g = 0.1
sigma_m = 0.1
r = 1
K = 25
C = 10
noise_dist = "lognormal"
discount = 0.95

# Creates transition matrix
T = create_T(Num_s,Num_a,sigma_g,gdist,r,C,K,noise_dist)
# creates emission matrix
O = create_O(Num_s,Num_a,Num_z,sigma_m,noise_dist)
# creates reward function
R = create_R(Num_s,Num_a)

#R = create_R2(Num_s,Num_a,0.1)


options(mc.cores = parallel::detectCores())

system.time( out <- pomdp(T,O, R, 0.95) )

h <- out[[2]]
x <- seq_along(h)
plot(x, x - h)

## Milad R is -1 for h > x, instead of R = min(h,x)
## Milad states: 1:30, actions 0:30 (?)
## Milad transition, observation: no 'pile-on-boundary' in probabilty

out1 <- pomdp(T, O, reward, 0.95)
out2 <- pomdp(transition, O, reward, 0.95)
out3 <- pomdp(transition, observation, reward, 0.95)


library("ggplot2")
df <- data.frame(x, y = x - out[[2]], y1 = x- out1[[2]], y2 = x - out2[[2]], y3 = x - out3[[2]])
ggplot(df, aes(x)) +
  geom_line(aes(y = y)) +
  geom_point(aes(y = y1), col = "red", alpha = 0.5)  +
  geom_point(aes(y = y2), col = "blue", alpha = 0.5) +
  geom_point(aes(y = y3), col = "green", alpha = 0.5)

