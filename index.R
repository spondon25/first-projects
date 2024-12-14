library(alr4)

Heights
plot(x=Heights$mheight, y=Heights$dheight, col="brown" ,pch=18)


data("Heights")
group_means <- aggregate(dheight ~ mheight, data = Heights, FUN = mean)

plot(Heights$mheight, Heights$dheight, 
     xlab = "mheight", 
     ylab = "dheight", 
     pch = 16, 
     col = "black")

points(group_means$mheight, group_means$dheight, 
       col = "red", 
       pch = 16, 
       cex = .5)
abline(lm(Heights$dheight~Heights$mheight), lwd=2, col="green")
abline(a=0, b=1, col="blue")

##########

Forbes

plot(x=Forbes$bp, y=Forbes$pres, col="brown" ,pch=16)
abline(lm(Forbes$pres~Forbes$bp), lwd=2)
lm(Forbes$pres~Forbes$bp)

model_original <- lm(bp ~ pres, data = Forbes)

model_transformed <- lm(bp ~ lpres, data = Forbes)


residuals_original <- residuals(model_original)
residuals_transformed <- residuals(model_transformed)


par(mfrow = c(1, 2))

plot(Forbes$bp, residuals_original,
     xlab = "Boiling Point (°F)",
     ylab = "Residuals",
     main = "Original Scale",
     pch = 19, col = "red")
abline(h = 0, col = "blue", lwd = 2)


plot(Forbes$bp, residuals_transformed,
     xlab = "Boiling Point (°F)",
     ylab = "Residuals",
     main = "Transformed Scale",
     pch = 19, col = "green")
abline(h = 0, col = "blue", lwd = 2)

par(mfrow = c(1, 1))



##########

wblake
group_mean<-aggregate(Length~Age, data = wblake, FUN = mean)
plot( x=wblake$Age, y=wblake$Length,
      pch = 19, col = "green")

points(group_mean$Age, group_mean$Length, 
       col = "red", 
       pch = 16, 
       cex = .5)
abline(lm(wblake$Length~wblake$Age), lwd=2, col="blue")





# Load ggplot2
library(ggplot2)

# Create the ggplot
ggplot(wblake, aes(x = Age, y = Length)) +
  geom_point(color = "green", size = 2) +  # Scatter points
  geom_point(data = group_mean, aes(x = Age, y = Length), 
             color = "red", size = 1.5) + # Group means
  geom_smooth(method = "loess", color = "blue", se = FALSE) + # Smooth curve
  labs(title = "Length vs Age with Smooth Curve", 
       x = "Age", y = "Length") +
  theme_minimal()


##########

ftcollinssnow

plot( x=ftcollinssnow$Early, y=ftcollinssnow$Late, pch = 19, col = "black")
abline(lm(ftcollinssnow$Late~ftcollinssnow$Early), lwd=2, col="blue")
abline(h = mean(ftcollinssnow$Late), col = "red", lwd = 2, lty = 2)

#abline(a = 0, b = 1, lwd = 2, col = "blue")


##########


library(alr4)
library(ggplot2)


##turkey$Treatment <- factor(c(
##  "Control", "Control", "Control", "Control", "Control", "new A", "new A",
##  "new A", "new A", "new B", "new B", "new B", "new B" # Match the rows
##))


turkey$Treatment <- factor(rep(c("Control","new A","new B"), times=as.vector(table(turkey$S)))) # Match the rows


#counts <- as.vector(table(turkey$S))
#turkey$Treatment <- factor(rep(c("Control", "new A", "new B"), times = counts))


ggplot(turkey, aes(x = A, y = Gain, color = Treatment)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Turkey Growth",
    x = "A (Dose)",
    y = "Gain"
  ) +
  scale_color_manual(values = c("red", "green", "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))



##########


# Box plot of Length at different Ages
boxplot(Length ~ Age, data = wblake,
        main = "Box Plot of Length at Age for Smallmouth Bass",
        xlab = "Age", ylab = "Length",
        col = "lightblue", border = "black")


# Run this if ggridges is not installed
library(ggplot2)
library(ggridges)
library(alr4)

# Load the Smallmouth Bass data (Length at Age)
data(wblake)

# Create a vertical density ridge plot of Length at different Age levels
ggplot(wblake, aes(x = Length, y = factor(Age), fill = factor(Age))) +
  geom_density_ridges(alpha = 0.6) +  # Vertical density ridge plot
  labs(title = "Vertical Density Ridge Plot of Length at Different Age Levels for Smallmouth Bass",
       x = "Length",
       y = "Age",
       fill = "Age") +
  theme_minimal() +
  scale_fill_viridis_d() # Optional: Color scale for better visualization
#+coord_flip()


##########

#ggplot(wblake, aes(y = Age, x = Length, fill = Age)) +
#  geom_density_ridges(alpha = 0.6) +  # Vertical density ridge plot
#  labs(title = "Vertical Density Ridge Plot of Length at Different Age Levels for Smallmouth Bass",
#       x = "Length",
#       y = "Age",
#       fill = "Age") +
#  theme_minimal() +
#  scale_fill_viridis_d() 

##########



# Load Anscombe's dataset
data(anscombe)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2))

# Loop over each subset of Anscombe's dataset (1 through 4)
for (i in 1:4) {
  # Create scatterplot for each dataset (response vs predictor)
  plot(anscombe[[paste0("x", i)]], anscombe[[paste0("y", i)]],
       main = paste("Dataset", i),
       xlab = paste("x", i, sep=""),
       ylab = paste("y", i, sep=""), pch=16, col="black")
  
  # Add the regression line (abline of lm)
  abline(lm(anscombe[[paste0("y", i)]] ~ anscombe[[paste0("x", i)]]), col = "blue", lwd = 2)
}



##########


fuel2001
library(GGally)

# Create an enhanced scatterplot matrix
ggpairs(fuel2001[, c("Income", "Miles", "Tax", "Drivers", "FuelC")],
        title = "Enhanced Scatterplot Matrix for fuel2001")



# Load the alr4 package
library(alr4)

# Load the fuel2001 dataset
data("fuel2001")

# Check the column names
names(fuel2001)


library(GGally)

# Create an enhanced scatterplot matrix
ggpairs(fuel2001[, c("FuelC", "Drivers", "Miles", "Income", "Tax")],
        title = "Enhanced Scatterplot Matrix for fuel2001")



##########


# Create the data
divorce_data <- data.frame(
  year = 2000:2009,
  divorce_rate_maine = c(5.0, 4.7, 4.6, 4.4, 4.3, 4.1, 4.2, 4.2, 4.2, 4.1),
  margarine_consumption = c(8.2, 7.0, 6.5, 5.3, 5.2, 4.0, 4.6, 4.5, 4.2, 3.7)
)

# Scatterplot with regression line
plot(divorce_data$margarine_consumption, divorce_data$divorce_rate_maine,
     main = "Divorce Rate vs Margarine Consumption (2000-2009)",
     xlab = "Margarine Consumption (lbs per capita)",
     ylab = "Divorce Rate (Maine)",
     pch = 19, col = "blue")

# Linear model
lm_model <- lm(divorce_rate_maine ~ margarine_consumption, data = divorce_data)

# Add regression line to the plot
abline(lm_model, col = "red", lwd = 2)

# Calculate correlation
correlation <- cor(divorce_data$margarine_consumption, divorce_data$divorce_rate_maine)
cat("Correlation coefficient:", correlation, "\n")

# Extract slope and intercept
slope <- coef(lm_model)[2]
intercept <- coef(lm_model)[1]
cat("Slope:", slope, "\n")
cat("Intercept:", intercept, "\n")

#linear_regression
