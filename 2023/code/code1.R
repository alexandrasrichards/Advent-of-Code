setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

day = 1


input = as.data.frame(read.table(paste("../input/","input",day,".txt",sep=""),header=FALSE,fill=TRUE))

sum = 0

for(i in 1:nrow(input)){
  extracted = unlist(str_extract_all(input[i,], "[0-9]"))
  print(extracted)
  number = 10*as.numeric(extracted[1]) + as.numeric(extracted[length(extracted)])
  print(number)
  sum = sum + number
}

sum


str_replace("onetwo", "one", "1")


sum2 = 0

for(i in 1:nrow(input)){
    input[i,] = str_replace_all(input[i,], "fourteen", "14teen")
    input[i,] = str_replace_all(input[i,], "fifteen", "15teen")
    input[i,] = str_replace_all(input[i,], "sixteen", "16teen")
    input[i,] = str_replace_all(input[i,], "seventeen", "17teen")
    input[i,] = str_replace_all(input[i,], "eighteen", "eig18teen")
    input[i,] = str_replace_all(input[i,], "nineteen", "nin19teen")
    input[i,] = str_replace_all(input[i,], "one", "one1one")
    input[i,] = str_replace_all(input[i,], "two", "two2two")
    input[i,] = str_replace_all(input[i,], "three", "three3three")
    input[i,] = str_replace_all(input[i,], "four", "4")
    input[i,] = str_replace_all(input[i,], "five", "5five")
    input[i,] = str_replace_all(input[i,], "six", "6")
    input[i,] = str_replace_all(input[i,], "seven", "7seven")
    input[i,] = str_replace_all(input[i,], "eight", "eight8eight")
    input[i,] = str_replace_all(input[i,], "nine", "nine9nine")
    input[i,] = str_replace_all(input[i,], "ten", "ten10ten")
    input[i,] = str_replace_all(input[i,], "eleven", "eleven11eleven")
    input[i,] = str_replace_all(input[i,], "twelve", "twelve12")
    input[i,] = str_replace_all(input[i,], "thirteen", "thirteen13")
    input[i,] = str_replace_all(input[i,], "twenty", "twenty2")
    input[i,] = str_replace_all(input[i,], "thirty", "thirty3")
    input[i,] = str_replace_all(input[i,], "fourty", "4")
    input[i,] = str_replace_all(input[i,], "fifty", "5")
    input[i,] = str_replace_all(input[i,], "sixty", "6")
    input[i,] = str_replace_all(input[i,], "seventy", "7")
    input[i,] = str_replace_all(input[i,], "eighty", "eighty8")
    input[i,] = str_replace_all(input[i,], "ninety", "ninety9")
    extracted = unlist(str_extract_all(input[i,], "[0-9]"))
    print(extracted)
    if(as.numeric(extracted[1]) > 10) extracted[1] = "1"
    
    number = 10*as.numeric(unlist(strsplit(extracted[1],""))[1]) + 
        as.numeric(unlist(strsplit(extracted[length(extracted)],""))[str_length(extracted[length(extracted)])])
    print(number)
    sum2 = sum2 + number
}

sum2
