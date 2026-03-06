library(igraph)
library(smooth)
# Assuming the library for 'sma' is loaded (e.g., library(smooth) or similar)

Networks=lapply(ponza_abundance_list,tempNet)

# --- 1. Null Model Matrix Generation ---
# We want a matrix where: Rows = Time, Columns = Simulations (e.g., 1000)
N_null <- 1000 
n_time_steps <- length(Networks) #from Networks=lapply(ponza_abundance_list,tempNet)

# Function to generate ONE entire null time series (a random "alternative history")
generate_null_timeseries <- function(network_list) {
  null_Q_vector <- numeric(length(network_list))
  
  for (t in 1:length(network_list)) {
    g <- network_list[[t]]
    if (gsize(g) > 0) {
      # Rewiring while preserving degrees
      g_null <- rewire(g, with = keeping_degseq(niter = gsize(g) * 10))
      # Calculate Q
      cl <- cluster_louvain(g_null, weights = NULL)
      null_Q_vector[t] <- modularity(cl)
    } else {
      null_Q_vector[t] <- NA # Handle empty graphs
    }
  }
  return(null_Q_vector)
}

print("Generating null time series...")
set.seed(1234)
# Create a matrix: each column is a complete simulation over time
# replicate() repeats the function N_null times
Null_Matrix_Raw <- replicate(N_null, generate_null_timeseries(Networks))

# --- 2. Smoothing the Null Model ---
# Apply the SAME sma() function used for observed data to every column of the matrix

# Helper function to extract fitted values from your sma object
apply_sma <- function(x) {
  # Handle NAs if present (e.g., empty graphs)
  if(all(is.na(x))) return(rep(NA, length(x)))
  
  # Apply your sma function. 
  # NOTE: Ensure it handles NAs or impute them beforehand if necessary
  tryCatch({
    return(sma(x)$fitted) 
  }, error = function(e) return(rep(NA, length(x))))
}

# Apply smoothing to every column (2 = columns) of the raw matrix
Null_Matrix_Smoothed <- apply(Null_Matrix_Raw, 2, apply_sma)

# --- 3. Calculate Statistics on Smoothed Data ---
# Now calculate Mean and SD for each time step based on the ALREADY smoothed series
Null_Stats_Smoothed <- data.frame(
  Time = 1:n_time_steps,
  Q_mean_null = rowMeans(Null_Matrix_Smoothed, na.rm = TRUE),
  Q_sd_null = apply(Null_Matrix_Smoothed, 1, sd, na.rm = TRUE)
)

# Calculate confidence interval boundaries (Smoothed)
Null_Stats_Smoothed$Null_Lower <- Null_Stats_Smoothed$Q_mean_null - (1.96 * Null_Stats_Smoothed$Q_sd_null)
Null_Stats_Smoothed$Null_Upper <- Null_Stats_Smoothed$Q_mean_null + (1.96 * Null_Stats_Smoothed$Q_sd_null)

# --- 4. Merge with Observed Data (Already smoothed by you) ---
# Assuming 'Modularity.sma' is your dataframe with smoothed observed data
# Rename for clarity if it has only one column
Observed_Smoothed <- data.frame(Q_Observed_SMA = Modularity.sma[,1]) 

Plot_Data_SMA <- cbind(Observed_Smoothed, Null_Stats_Smoothed)

# Remove rows with NA (SMA generates NAs at the start/end of the series)
Plot_Data_SMA <- na.omit(Plot_Data_SMA)

# --- 5. Plotting (Smoothed Version) ---
library(ggplot2)

nullmodel <- ggplot(Plot_Data_SMA, aes(x = Time)) +
  # Null Confidence Band (Smoothed)
  geom_ribbon(aes(ymin = Null_Lower, ymax = Null_Upper), 
              fill = "orange", alpha = 0.3) + # Using orange to distinguish from previous plots
  
  # Null Mean (Smoothed)
  geom_line(aes(y = Q_mean_null), linetype = "dashed", color = "darkorange") +
  
  # Observed Data (Smoothed)
  geom_line(aes(y = Q_Observed_SMA), color = "darkblue", size = 1.2) +
  
  labs(title = "Smoothed modularity (SMA) vs Smoothed null model",
       #subtitle = "The Null Model underwent the same smoothing process as the observed data",
       y = "Modularity (Q - SMA)",
       x = "Time") +
  theme_minimal() +  
  theme(panel.background = element_rect(fill = NA),
        axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        plot.title = element_text(size = 22, hjust = 0.5,),
        plot.subtitle = element_text(face = "italic", size = 18),
        plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 22)) 

ggsave("nullmodel.png", nullmodel, width = 3000, height = 2000, units = "px", 
       pointsize = 22,bg = "white",dpi = 300)
