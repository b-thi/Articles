# Setting up functions
f1 = function(t){return(1)}
f2 = function(t){return(1 + t)}
f3 = function(t){return(1 + t + t^2)}
f4 = function(t){return(1 + t + t^2 + t^3)}
f5 = function(t){return(1 + 0.1*t + 2*t^2 + 0.8*t^3 + 0.5*t^4)}
f6 = function(t){return(1 + t + t^2 + t^3 + t^4 + t^5)}

# Domain
x = seq(-5, 5, 0.001)

# Creating data frame
df = data.frame(x = x, f1 = f1(x), f2 = f2(x), f3 = f3(x), f4 = f4(x), f5 = f5(x), f6 = f6(x))

# Plotting
for (i in 1:4) {
  a = ggplot(data = df, aes(x = x, y = df[,i + 1])) +
          theme_bw() +
          geom_line(color = "red", size = 2) +
          labs(x = "t", y = "y") +
    theme(axis.text=element_text(size=14, face = "bold"),
          axis.title=element_text(size=14,face="bold"))
  
  ggsave(path = "article_fda3", filename = paste0("basis_curve_", i, ".png"), plot = a, width = 5, height = 5)
}

