save_varaible_code = function(){
  
  
  
  #### COPY AND PAST THIS TO SAVE VARIABLE
  
  
  save_test = F
  if(save_test == T){
    variable_list = c('full_data')
    cmd_list = save_variable_function(variable_list)
    lapply(cmd_list, function(x) eval(parse(text = x)))
    try(save_input_function(input))
    read_test = F
    if(read_test == T){
      variable_list = c(variable_list)
      cmd_list = read_variable_function(variable_list)
      for(cmd in cmd_list){
        print(cmd)
        try(eval(parse(text = cmd)))
      }
    }
  }
  
  
  
  
}

save_variable_function = function(variable_list){
  cmd_list = c()
  
  for(entry in variable_list){
    cmd = paste0("saveRDS(",entry,",'temp/",entry,".rds')")
    print(cmd)
    cmd_list = c(cmd_list,cmd)
    #eval(parse(text = cmd))
  }
  
  return(cmd_list)
  
  ### Need to run outside of function
  for(cmd in cmd_list){
    eval(parse(text = cmd))
  }
  
  ### TO GET DATA BACK OUT ###
  #cmd_list = save_variable_function(variable_list)
  #lapply(cmd_list, function(cmd) eval(parse(text = cmd)))
  #save_input_function(input)
}

save_input_function = function(input){
  save_input = list()
  input_list = names(reactiveValuesToList(input))
  
  for(entry in input_list){
    cmd = paste0("save_input[['",entry,"']] = input$",entry)
    print(cmd)
    
    eval(parse(text = cmd))
  }
  #input = readRDS('temp/save_input.rds')
  saveRDS(save_input,'temp/input.rds')
}

read_variable_function = function(variable_list){
  cmd_list = c()
  variable_list = c(variable_list,'input')
  for(entry in variable_list){
    cmd = paste0(entry, " = readRDS('temp/",entry,".rds')")
    print(cmd)
    cmd_list = c(cmd_list,cmd)
    #eval(parse(text = cmd))
  }
  #cmd_list = read_variable_function(variable_list)
  #for(cmd in cmd_list){
  #  eval(parse(text = cmd))
  #}
  return(cmd_list)
}
