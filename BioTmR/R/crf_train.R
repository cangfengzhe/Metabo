
crf_train <- function( train_data = '/Users/lipidong/baiduyun/work/c++/crf/crf/train.data', template_data = '/Users/lipidong/baiduyun/work/c++/crf/crf/template', model_dir = getwd(), freq = '1',  max_iterations = '10000', cost = '1',  eta ='0.0001', algorithm = 'CRF',  shrinking_size = '20'){


  if(!is.data.frame(train_data) & !is.matrix(train_data) & is.character(train_data)) stop('input data must be data.frame, matrix or file path')

 if(!file.exists(model_dir)) stop('please input a valid model file path.')

  if(is.character(train_data)){
    if(file.exists(train_data)){
      path = train_data
    }else{
        stop('the train data file does not exist')
      }
  }else{
      path <- paste(c(tempdir(), 'train_data_', rnorm(1)), collapse = '')
      write.table(train_data, path, row.names = F, na="", quote = F)
    }

  model_path <- paste(c(model_dir, '/model.out'), collapse = '')
  file.create(model_path)
  crf_learn_(template_data,  path,  model_path, freq,  max_iterations, cost,  eta, algorithm,  shrinking_size)

  cat(paste(c('The model file will be saved in \n', model_path), collapse = ''))
  model_path
}
