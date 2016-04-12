# Read in Barcelona data
source('barcelona/barcelona_code.R')

# Read in Paris data
source('paris/paris_code.R')

# Read in rome data
source('rome/rome_code.R')


# Combine all datasets
df <- rbind(barcelona,
            paris,
            rome)
