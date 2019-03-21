Model Building
================

``` r
library(keras)
```

    ## Warning: package 'keras' was built under R version 3.5.3

``` r
library(dplyr)
library(magrittr)
```

2. Merging the inputs
---------------------

### 2.1 Creating the input layers

``` r
ct_input_layer <- layer_input(
  shape = c(CONSTANTS$MAX_LEN),
  dtype = "int32",
  name = "input_layer_1"
)

bn_input_layer <- layer_input(
  shape = c(CONSTANTS$MAX_LEN),
  dtype = "int32",
  name = "input_layer_2"
)

id_input_layer <- layer_input(
  shape = c(CONSTANTS$MAX_LEN),
  dtype = "int32",
  name = "input_layer_3"
)
```

### 2.2 Creating the Embedding layers

``` r
ct_aux_out_layer <- ct_input_layer %>% 
  layer_embedding(
    input_dim = CONSTANTS$MAX_WORDS,
    output_dim = 512, 
    input_length = CONSTANTS$MAX_LEN) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid", name = "aux_output_1")
  

bn_aux_out_layer <- bn_input_layer %>% 
  layer_embedding(
    input_dim = CONSTANTS$MAX_WORDS,
    output_dim = 512, 
    input_length = CONSTANTS$MAX_LEN) %>%
  layer_lstm(units = 32) %>% 
  layer_dense(units = 1, activation = "sigmoid", name = "aux_output_2")

id_aux_out_layer <- id_input_layer %>% 
  layer_embedding(
    input_dim = CONSTANTS$MAX_WORDS,
    output_dim = 512, 
    input_length = CONSTANTS$MAX_LEN) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid", name = "aux_output_3")
```

### 2.3 Merging Auxilary and Input Layers

``` r
auxiliary_input <- layer_input(shape = c(5), name = 'aux_input')

main_output <- layer_concatenate(c(ct_aux_out_layer, bn_aux_out_layer, id_aux_out_layer, auxiliary_input)) %>%  
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dense(units = 1, activation = 'sigmoid', name = 'main_output')
```

``` r
model <- keras_model(
  inputs = c(ct_input_layer, bn_input_layer, id_input_layer, auxiliary_input), 
  outputs = c(main_output, ct_aux_out_layer, bn_aux_out_layer, id_aux_out_layer)
)
```

``` r
summary(model)
```

    ## ___________________________________________________________________________
    ## Layer (type)            Output Shape     Param #  Connected to             
    ## ===========================================================================
    ## input_layer_1 (InputLay (None, 100)      0                                 
    ## ___________________________________________________________________________
    ## input_layer_2 (InputLay (None, 100)      0                                 
    ## ___________________________________________________________________________
    ## input_layer_3 (InputLay (None, 100)      0                                 
    ## ___________________________________________________________________________
    ## embedding_1 (Embedding) (None, 100, 512) 5120000  input_layer_1[0][0]      
    ## ___________________________________________________________________________
    ## embedding_2 (Embedding) (None, 100, 512) 5120000  input_layer_2[0][0]      
    ## ___________________________________________________________________________
    ## embedding_3 (Embedding) (None, 100, 512) 5120000  input_layer_3[0][0]      
    ## ___________________________________________________________________________
    ## lstm_1 (LSTM)           (None, 32)       69760    embedding_1[0][0]        
    ## ___________________________________________________________________________
    ## lstm_2 (LSTM)           (None, 32)       69760    embedding_2[0][0]        
    ## ___________________________________________________________________________
    ## lstm_3 (LSTM)           (None, 32)       69760    embedding_3[0][0]        
    ## ___________________________________________________________________________
    ## aux_output_1 (Dense)    (None, 1)        33       lstm_1[0][0]             
    ## ___________________________________________________________________________
    ## aux_output_2 (Dense)    (None, 1)        33       lstm_2[0][0]             
    ## ___________________________________________________________________________
    ## aux_output_3 (Dense)    (None, 1)        33       lstm_3[0][0]             
    ## ___________________________________________________________________________
    ## aux_input (InputLayer)  (None, 5)        0                                 
    ## ___________________________________________________________________________
    ## concatenate_1 (Concaten (None, 8)        0        aux_output_1[0][0]       
    ##                                                   aux_output_2[0][0]       
    ##                                                   aux_output_3[0][0]       
    ##                                                   aux_input[0][0]          
    ## ___________________________________________________________________________
    ## dense_1 (Dense)         (None, 64)       576      concatenate_1[0][0]      
    ## ___________________________________________________________________________
    ## dense_2 (Dense)         (None, 64)       4160     dense_1[0][0]            
    ## ___________________________________________________________________________
    ## dense_3 (Dense)         (None, 64)       4160     dense_2[0][0]            
    ## ___________________________________________________________________________
    ## main_output (Dense)     (None, 1)        65       dense_3[0][0]            
    ## ===========================================================================
    ## Total params: 15,578,340
    ## Trainable params: 15,578,340
    ## Non-trainable params: 0
    ## ___________________________________________________________________________

``` r
model %>% compile(
  optimizer = 'rmsprop',
  loss = 'binary_crossentropy',
  loss_weights = c(0.2, 1.0, 0.2, 0.2)
)
```

``` r
history <- model %>% fit(
  x = list(billing_notes_data, call_text_data, item_desc_data, type),
  y = list(labels, labels, labels, labels),
  epochs = 10,
  batch_size = 20
)
```

``` r
plot(history)
```

![](markdown_figs/model_building-unnamed-chunk-9-1.png)
