# Libraries
library(fda)
library(fda.usc)
library(plotly)
library(htmlwidgets)
library(reshape2)

# Setting domain
x <- c(1:365)

##############################
# Weather Data Set           #
##############################

# Setting up data
data("daily")

# Range Function
range_01 <- function(x){(x-min(x))/(max(x)-min(x))}
beta_lm_weather <- function(x, c){
  value <- c[1] + c[2]*sin(1*2*pi*x/365) + c[3]*cos(1*2*pi*x/365) +
    c[4]*sin(2*2*pi*x/365) + c[5]*cos(2*2*pi*x/365) + c[6]*sin(3*2*pi*x/365) +
    c[7]*cos(3*2*pi*x/365) + c[8]*sin(4*2*pi*x/365) + c[9]*cos(4*2*pi*x/365) +
    c[10]*sin(5*2*pi*x/365) + c[11]*cos(5*2*pi*x/365) + c[12]*sin(6*2*pi*x/365) +
    c[13]*cos(5*2*pi*x/365)
  return(value)
}

final_beta_fnn_fourier <- function(x, d, range){
  
  # Appending on 0s
  zero_vals = rep(0, 51 - length(d))
  
  # creating c vector
  c = c(d, zero_vals)
  
  # Getting values
  value <- c[1] + c[2]*sin(1*2*pi*x/range[2]) + c[3]*cos(1*2*pi*x/range[2]) +
    c[4]*sin(2*2*pi*x/range[2]) + c[5]*cos(2*2*pi*x/range[2]) + c[6]*sin(3*2*pi*x/range[2]) + c[7]*cos(3*2*pi*x/range[2]) +
    c[8]*sin(4*2*pi*x/range[2]) + c[9]*cos(4*2*pi*x/range[2]) + c[10]*sin(5*2*pi*x/range[2]) + c[11]*cos(5*2*pi*x/range[2]) +
    c[12]*sin(6*2*pi*x/range[2]) + c[13]*cos(6*2*pi*x/range[2]) + c[14]*sin(7*2*pi*x/range[2]) + c[15]*cos(7*2*pi*x/range[2]) +
    c[16]*sin(8*2*pi*x/range[2]) + c[17]*cos(8*2*pi*x/range[2]) + c[18]*sin(9*2*pi*x/range[2]) + c[19]*cos(9*2*pi*x/range[2]) +
    c[20]*sin(10*2*pi*x/range[2]) + c[21]*cos(10*2*pi*x/range[2]) + c[22]*sin(11*2*pi*x/range[2]) + c[23]*cos(11*2*pi*x/range[2]) +
    c[24]*sin(12*2*pi*x/range[2]) + c[25]*cos(12*2*pi*x/range[2]) + c[26]*sin(13*2*pi*x/range[2]) + c[27]*cos(13*2*pi*x/range[2]) +
    c[28]*sin(14*2*pi*x/range[2]) + c[29]*cos(14*2*pi*x/range[2]) + c[30]*sin(15*2*pi*x/range[2]) + c[31]*cos(15*2*pi*x/range[2]) +
    c[32]*sin(16*2*pi*x/range[2]) + c[33]*cos(16*2*pi*x/range[2]) + c[34]*sin(17*2*pi*x/range[2]) + c[35]*cos(17*2*pi*x/range[2]) +
    c[36]*sin(18*2*pi*x/range[2]) + c[37]*cos(18*2*pi*x/range[2]) + c[38]*sin(19*2*pi*x/range[2]) + c[39]*cos(19*2*pi*x/range[2]) +
    c[40]*sin(20*2*pi*x/range[2]) + c[41]*cos(20*2*pi*x/range[2]) + c[42]*sin(21*2*pi*x/range[2]) + c[43]*cos(21*2*pi*x/range[2]) +
    c[44]*sin(22*2*pi*x/range[2]) + c[45]*cos(22*2*pi*x/range[2]) + c[46]*sin(23*2*pi*x/range[2]) + c[47]*cos(23*2*pi*x/range[2]) +
    c[48]*sin(24*2*pi*x/range[2]) + c[49]*cos(24*2*pi*x/range[2]) + c[50]*sin(25*2*pi*x/range[2]) + c[51]*cos(25*2*pi*x/range[2])
  
  # Returning
  return(value)
  
}

# Obtaining response
total_prec = range_01(apply(daily$precav, 2, sum))

# Creating functional data
tempbasis65  = create.fourier.basis(c(0,365), 25)
timepts = seq(1, 365, 1)
temp_fd = Data2fd(timepts, daily$tempav, tempbasis65)

plot(temp_fd)

# Changing into fdata
weather_fdata = fdata(daily$tempav, argvals = 1:365, rangeval = c(1, 365))

# Evaluation function
fd_eval = function(c, x){
  result = c[1] + 
    c[2]*sin((2*pi*x)/365) + 
    c[3]*cos((2*pi*x)/365) + 
    c[4]*sin((2*2*pi*x)/365) + 
    c[5]*cos((2*2*pi*x)/365) +
    c[6]*sin((2*3*pi*x)/365) + 
    c[7]*cos((2*3*pi*x)/365) + 
    c[8]*sin((2*4*pi*x)/365) + 
    c[9]*cos((2*4*pi*x)/365) +
    c[10]*sin((2*5*pi*x)/365) + 
    c[11]*cos((2*5*pi*x)/365) + 
    c[12]*sin((2*6*pi*x)/365) + 
    c[13]*cos((2*6*pi*x)/365) +
    c[14]*sin((2*7*pi*x)/365) + 
    c[15]*cos((2*7*pi*x)/365) + 
    c[16]*sin((2*8*pi*x)/365) + 
    c[17]*cos((2*8*pi*x)/365) +
    c[18]*sin((2*9*pi*x)/365) + 
    c[19]*cos((2*9*pi*x)/365) + 
    c[20]*sin((2*10*pi*x)/365) + 
    c[21]*cos((2*10*pi*x)/365) +
    c[22]*sin((2*11*pi*x)/365) + 
    c[23]*cos((2*11*pi*x)/365) + 
    c[24]*sin((2*12*pi*x)/365) + 
    c[25]*cos((2*12*pi*x)/365)
    
}

# Getting evaluations
fd_evals = apply(temp_fd$coefs[,23:27], 2, fd_eval, 1:365)
fd_ds = melt(fd_evals)
row.names(fd_ds) = NULL
colnames(fd_ds) = c("Time", "Group", "Value")

# Plotting functional observations
print(ggplot(data = fd_ds, aes(x = Time, y = Value/15, color = as.factor(Group))) + 
        geom_line(size = 1.3) +
        theme_bw() + 
        theme() +
        labs(x = "Time", y = "Temperature") +
        theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=14,face="bold")) +
    guides(color = guide_legend(title="Example Cities")))

#######################################

### Functional Linear Model (Basis) ###

# Setting up grid
l=2^(-4:10)

# Running functional linear model
func_basis = fregre.basis.cv(weather_fdata, 
                             total_prec, 
                             type.basis = "fourier",
                             lambda=l, 
                             type.CV = GCV.S, 
                             par.CV = list(trim=0.15))

# Pulling out the coefficients
coefficients_lm = func_basis$fregre.basis$coefficients

# Setting up data set
beta_coef_lm <- data.frame(time = seq(1, 365, 0.1), 
                           beta_evals = beta_lm_weather(seq(1, 365, 0.1), c(coefficients_lm[,1])))

# Functional observation values
obvs_coef <- data.frame(time = seq(1, 365, 0.1), 
                           beta_evals = scale(final_beta_fnn_fourier(seq(1, 365, 0.1), temp_fd$coefs[,26], c(1, 365))))

# Functional observation values
obvs_coef_raw <- data.frame(time = seq(1, 365, 0.1), 
                        beta_evals = final_beta_fnn_fourier(seq(1, 365, 0.1), temp_fd$coefs[,1], c(1, 365))/10)




data <- data.frame(x = beta_coef_lm$time, y = beta_coef_lm$beta_evals)

fig <- plot_ly(data, x = ~x)
fig <- fig %>% add_trace(y = ~beta_coef_lm$beta_evals, name = 'Func Coef',mode = 'lines', color = I('blue'))
fig <- fig %>% add_trace(y = ~obvs_coef$beta_evals, name = 'Func Obs', mode = 'lines', color = I('black'))
fig <- fig %>% add_trace(y = ~beta_coef_lm$beta_evals*obvs_coef$beta_evals, name = 'Effect', 
                         mode = 'lines',
                         color = I('green'))

f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)

x <- list(
  title = "Time",
  titlefont = f
)
y <- list(
  title = "Effect/Temperature",
  titlefont = f
)


fig = fig %>% layout(xaxis = x, yaxis = y)
fig = fig %>% layout(showlegend = F)
fig = fig %>% layout(hovermode = 'compare')



saveWidget(fig, "beta.html", selfcontained = F, libdir = "lib_beta")



##################### SIDE BY SIDE ####################

# Setting up data set
beta_coef_lm <- data.frame(time = seq(1, 365, 0.1), 
                           beta_evals = beta_lm_weather(seq(1, 365, 0.1), c(coefficients_lm[,1])))

# Functional observation values
obvs_coef_raw1 <- data.frame(time = seq(1, 365, 0.1), 
                            beta_evals = final_beta_fnn_fourier(seq(1, 365, 0.1), temp_fd$coefs[,1], c(1, 365))/10)

obvs_coef_raw2 <- data.frame(time = seq(1, 365, 0.1), 
                             beta_evals = final_beta_fnn_fourier(seq(1, 365, 0.1), temp_fd$coefs[,36], c(1, 365))/10)

obvs_coef_raw3 <- data.frame(time = seq(1, 365, 0.1), 
                             beta_evals = final_beta_fnn_fourier(seq(1, 365, 0.1), temp_fd$coefs[,7], c(1, 365))/10)

obvs_coef_raw4 <- data.frame(time = seq(1, 365, 0.1), 
                             beta_evals = final_beta_fnn_fourier(seq(1, 365, 0.1), temp_fd$coefs[,26], c(1, 365))/10)




data <- data.frame(x = beta_coef_lm$time, y = beta_coef_lm$beta_evals)

fig <- plot_ly(data, x = ~x)
fig1 <- fig %>% add_trace(y = ~beta_coef_lm$beta_evals*obvs_coef_raw1$beta_evals, name = 'Effect', 
                         mode = 'lines',
                         color = I('green'),
                         fill = 'tozeroy')

fig2 <- fig %>% add_trace(y = ~beta_coef_lm$beta_evals*obvs_coef_raw2$beta_evals, name = 'Effect', 
                          mode = 'lines',
                          color = I('green'),
                          fill = 'tozeroy')

fig3 <- fig %>% add_trace(y = ~beta_coef_lm$beta_evals*obvs_coef_raw3$beta_evals, name = 'Effect', 
                          mode = 'lines',
                          color = I('green'),
                          fill = 'tozeroy')

fig4 <- fig %>% add_trace(y = ~beta_coef_lm$beta_evals*obvs_coef_raw4$beta_evals, name = 'Effect', 
                          mode = 'lines',
                          color = I('green'),
                          fill = 'tozeroy')



f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)

x1 <- list(
  title = "Edmonton",
  titlefont = f
)

x2 <- list(
  title = "Calgary",
  titlefont = f
)

x3 <- list(
  title = "Kamloops",
  titlefont = f
)

x4 <- list(
  title = "Vancouver",
  titlefont = f
)
y <- list(
  title = "Effect",
  titlefont = f
)


fig1 = fig1 %>% layout(xaxis = x1, yaxis = y)
fig1 = fig1 %>% layout(showlegend = F)
fig1 = fig1 %>% layout(hovermode = 'compare')

fig2 = fig2 %>% layout(xaxis = x2, yaxis = y)
fig2 = fig2 %>% layout(showlegend = F)
fig2 = fig2 %>% layout(hovermode = 'compare')

fig3 = fig3 %>% layout(xaxis = x3, yaxis = y)
fig3 = fig3 %>% layout(showlegend = F)
fig3 = fig3 %>% layout(hovermode = 'compare')

fig4 = fig4 %>% layout(xaxis = x4, yaxis = y)
fig4 = fig4 %>% layout(showlegend = F)
fig4 = fig4 %>% layout(hovermode = 'compare')

fig_final <- subplot(fig1, fig2, fig3, fig4, nrows = 2)

?subplot
saveWidget(fig_final, "fig_sub.html", selfcontained = F, libdir = "lib_subplot")


