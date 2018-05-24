
# devtools::install_github("rstudio/keras")
# keras::install_keras()
# source("https://bioconductor.org/biocLite.R")
# biocLite("EBImage")

library(tidyverse)
library(magrittr)
library(jpeg)
library(EBImage)
library(keras)
library(logging)
system('set "KERAS_BACKEND=tensorflow"')
basicConfig()
# setwd("~/studies/Learning/keras")

rotate_matrix <- function(x) {
  t(apply(x, 2, rev))
}


# Process data ------------------------------------------------------------


## MNIST data
mnist <- dataset_mnist()
image(rotate_matrix(mnist$train$x[3,,]))

N <- 60000
x_train <- mnist$train$x[1:N, , ] %>%
  array_reshape(c(N, dim(mnist$train$x)[-1], 1))%>% 
  divide_by(255)
y_train <- mnist$train$y[1:N] %>% to_categorical()

## Simpsons data
## https://www.kaggle.com/alexattia/the-simpsons-characters-dataset
if (FALSE) {
  data_folder <- 'keras/the-simpsons-characters-dataset/simpsons_dataset/simpsons_dataset/'
  resolution <- 100
  images <- list.files(data_folder, full.names = TRUE, recursive = TRUE) %>%
    str_subset('jpg$') %>%
    # head(10) %>%
    map(function(x){
      print(x)
      readImage(x) %>%
        EBImage::channel('gray') %>%
        EBImage::flip() %>%
        # array_reshape(., c(dim(.)[1:3])) %>%
        EBImage::resize(w = resolution, h = resolution)
    })
  saveRDS(images, 'keras/data/simpsons-image-list.rds')
}

if (FALSE) {
  images_raw <- readRDS('keras/data/simpsons-image-list.rds')
  images <- images_raw %>% 
    map2(., seq_along(.), function(x, i){
      if (i %% 100 == 0) {
        print(i)
      }
      x <- EBImage::resize(x, w = 70, h = 70)
      array_reshape(x, c(1, dim(x), 1))
    })
  # image(images[[1]])
  # image(images[[2]])
  
  ## Note we have to use abind::abind, not EBImage::abind or it doesn't work
  simpsons_data <- images %>%
    abind::abind(along = 1)
  # simpsons_data1 <- images[1:round(length(images)/2)] %>% 
  #   map(function(x){
  #     array_reshape(x, c(1, dim(x), 1))
  #   }) %>% 
  #   abind::abind(along = 1)
  # simpsons_data2 <- images[(round(length(images)/2) + 1):length(images)] %>% 
  #   map(function(x){
  #     array_reshape(x, c(1, dim(x), 1))
  #   }) %>% 
  #   abind::abind(along = 1)
  # simpsons_data <- abind::abind(simpsons_data1, simpsons_data2, along = 1)
  saveRDS(simpsons_data, 'keras/data/simpsons_data_array_70x70.rds')
  x_train <- simpsons_data
  rm(images, simpsons_data)
}
x_train <- readRDS('keras/data/simpsons_data_array_70x70.rds')



# Network-generating functions --------------------------------------------


get_optimizer <- function() {
  optimizer_adam(lr = 0.0002, beta_1 = 0.5)
}

get_generator <- function(optimizer, input_shape) {
  keras_model_sequential() %>%
    layer_dense(256,
                input_shape = input_shape,
                kernel_initializer =initializer_random_normal(stddev = 0.02)) %>% 
    layer_activation_leaky_relu(0.2) %>% 
    layer_dense(512) %>% 
    layer_activation_leaky_relu(0.2) %>%
    layer_dense(1024) %>%
    layer_activation_leaky_relu(0.2) %>%
    layer_dense(2048) %>%
    layer_activation_leaky_relu(0.2) %>%
    layer_dense(prod(dim(x_train)[-1]), activation = 'tanh') %>% 
    layer_reshape(dim(x_train)[-1]) %>% 
    compile(loss = 'binary_crossentropy', optimizer = optimizer)
}

get_discriminator <- function(optimizer) {
  keras_model_sequential() %>% 
    layer_flatten(input_shape = dim(x_train)[-1]) %>% 
    layer_dense(2048,
                kernel_initializer =initializer_random_normal(stddev = 0.02)) %>% 
    layer_activation_leaky_relu(0.2) %>% 
    layer_dense(1024) %>% 
    layer_activation_leaky_relu(0.2) %>% 
    layer_dropout(0.3) %>% 
    layer_dense(512) %>% 
    layer_activation_leaky_relu(0.2) %>% 
    layer_dropout(0.3) %>% 
    layer_dense(256) %>% 
    layer_activation_leaky_relu(0.2) %>% 
    layer_dropout(0.3) %>% 
    layer_dense(1, activation = 'sigmoid') %>% 
    compile(loss = 'binary_crossentropy', optimizer = optimizer)
}

get_gan_network <- function(discriminator, random_shape, generator, optimizer) {
  discriminator$trainable <- FALSE
  gan_input <- layer_input(shape = random_shape)
  x <- generator(gan_input)
  gan_output <- discriminator(x)
  keras_model(inputs = gan_input, outputs = gan_output) %>% 
    compile(loss = 'binary_crossentropy', optimizer = optimizer)
}

plot_generated_images <- function(epoch, generator, random_shape, examples = 15) {
  noise <- matrix(rnorm(examples * random_shape), nrow = examples, ncol = random_shape)
  generated_images <- (1 - predict(generator, noise))/2
  col <- gray.colors(1000, 0, 1)
  layout(matrix(1:examples, nrow = floor(sqrt(examples))))
  for (i in seq_len(nrow(generated_images))) {
    image(generated_images[i, , , ], axes = FALSE, col = col, asp = 1)
  }
}

train_gan <- function(generator, discriminator, gan, epochs = 1, batch_size = 128, random_shape = 100, verbose_iter = 20, examples = 15) {
  batch_count <- round(nrow(x_train) / batch_size)
  pb <- txtProgressBar(0, batch_count, style = 3)
  for (e in seq_len(epochs)) {
    loginfo(sprintf('Epoch: %d', e))
    for (k in 1:batch_count){
      # Get a random set of input noise and images
      noise <- matrix(rnorm(batch_size * random_shape), nrow = batch_size, ncol = random_shape)
      image_batch <- x_train[sample(seq_len(nrow(x_train)), size = batch_size), , , , drop = FALSE]
      
      # Generate fake images
      generated_images <- predict(generator, noise)
      X <- abind::abind(image_batch, generated_images, along = 1)
      
      # Labels for generated and real data
      y_dis <- rep(0, 2 * batch_size)
      # One-sided label smoothing
      y_dis[1:batch_size] <- 0.9
      
      # Train discriminator
      discriminator$trainable <- TRUE
      train_on_batch(discriminator, X, y_dis)
      
      # Train generator
      noise <- matrix(rnorm(batch_size * random_shape), nrow = batch_size, ncol = random_shape)
      y_gen <- rep(1, batch_size)
      discriminator$trainable <- FALSE
      train_on_batch(gan, noise, y_gen)
      setTxtProgressBar(pb, k)
    }
    if (e == 1 || e %% verbose_iter == 0) {
       plt <- safely(plot_generated_images)(e, generator, random_shape, examples = examples)
       if (!is.null(plt$error)) {
         logwarn('Something went wrong with the plots. Resetting device...')
         safely(dev.off)()
       }
    }
  }
  list(generator = generator, discriminator = discriminator, gan = gan)
}



# Train network -----------------------------------------------------------

random_shape <- 200
adam <- get_optimizer()
generator <- get_generator(adam, input_shape = random_shape)
discriminator <- get_discriminator(adam)
gan <- get_gan_network(
  discriminator = discriminator, 
  random_shape = random_shape, 
  generator = generator, 
  optimizer = adam
)

out <- train_gan(
  generator = generator,
  discriminator = discriminator,
  gan = gan,
  epochs = 10,
  batch_size = 128,
  random_shape = random_shape,
  verbose_iter = 1,
  examples = 15
)


plot_generated_images(1, out$generator, 100, 15)


## Train some more
out <- train_gan(
  generator = out$generator,
  discriminator = out$discriminator,
  gan = out$gan,
  epochs = 100,
  batch_size = 128,
  random_shape = random_shape,
  verbose_iter = 1,
  examples = 15
)


#' TO DO
#' * Try applying some smoothing/pooling/convolution
#' * Try having bigger layers

















