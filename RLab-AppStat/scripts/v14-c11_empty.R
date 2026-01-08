################################################################
# MK-002-EN Applied Statistics / MK-002 Angewandte Statistik / #
# MK-002-EN-DI Applied Statistics                              #
# *** Introduction to R ***                                    #
################################################################


# 2 Setting the working directory 
# 3 Using R scripts
#################################

# Add the path to your working directory:


# Check if the working directory is set correctly:


# Send command from script to console:



# 4 Comments in R scripts
#########################

# You can add comments to scripts by putting a # (hash
# at the beginning of the comment.
# Everything after the hash will be printed as text
# in the console.)

#****************************************************************************
# This will be evaluated:
1 + 1

#****************************************************************************
 
# This is just a comment:
# 5 + 5

#****************************************************************************

# Code and comments can be combined:

1 + 1      # add two numbers


# 5 Variables
#############




# 6 Data types
##############




# 7 Data structures
###################

# 7.1 Vectors
#############



# Calculation with vectors:



# Alphanumeric vectors:



# Accessing elements of a vector:




# 7.2 Matrices
##############



# For comparison:



# Accessing elements of a matrix:



# Vectors and matrices contain either numbers or character elements:



# 7.3 Data frames
#################

fieldtrial <- data.frame(
  plot         = c( 1,  2,  3,   4,   5,   6,  7,   8,  9,  10,   11,  12),
  variety      = c(                                                      ) ,
  fertilizer   = c(                                                      ),
  rep          = c( 1,  2,  3,   1,   2,   3,  1,   2,  3,   1,    2,   3),
  yield        = c(                                                       ))




# 8 Saving and loading data files
#################################




# 9 Reading in data from Excel or OpenOffice files
##################################################

# Read from csv file:



# 10 Cleaning up
################

rm(list = ls())