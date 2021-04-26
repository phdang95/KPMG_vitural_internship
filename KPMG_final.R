#get the default working directory of R
getwd()
rm(list = ls())
cat("\014")
dev.off()
setwd("F:/Cong viec/KPMG project/KPMG vitural internship")

#load data
data = read.csv('KPMG_data_final.csv')


summary(data)

data = data[data$gender != 'U',]
summary(data)
#exploring data


hist(data$age)
hist(data$tenure)
hist(data$past_3_years_bike_related_purchases)



#def
forward_p_value = function(dtframe, response, exclude, alpha=0.05)
{
  # Specify the columns that you want to exclude from your model
  exclude = c(exclude, response)
  cols = names(dtframe)
  min_val = 0
  sel_cols = ""
  model = NULL
  ##################################### Specifying the bigger loop
  while (min_val < alpha) {
    min_val = 100
    selected_col = "" ## reset the input
    ###################################  The small loop for running regression
    for (col in cols) {
      if (!(col %in% exclude)) {
        col2add = if (sel_cols == "") {col} else {paste(" + ", col)}
        formula = paste(response, " ~ ", sel_cols, col2add)
        model = glm(formula = formula,family=binomial(link=logit), data = dtframe)
        s = summary(model)
        ################################### finding s$coefficients
        col2retrive = col
        if (is.factor(dtframe[,col])) {
          levels = levels(dtframe[,col])
          if (length(levels) > 1){
            col2retrive =paste(col,levels[2],sep = "")
          }
        }
        if (!is.na(model$coefficients[col2retrive])){
          p_val = s$coefficients[col2retrive,4]  #t-test
          t_value = s$coefficients[col2retrive,2] #p-value
        } else { 
          p_val = 100
          t_value = 100
          cat("\nWarning:There is a strong colinearity between ", col, " and other feature. Try to remove it from dataset.\n\n")
          #N/a value
        }
        adj_r2 = s$adj.r.squared #R-squared
        if (p_val < min_val) {
          min_val = p_val
          selected_col = col
        }
        #print(s$coefficients)
        if (p_val != 100){
          cat(format(paste("+",col), width = 15), 
              " Adj. R2:", format(adj_r2,width = 15, justify = 'right'),
              "t-value:", format(t_value,width = 15, nsmall = 3, digits = 3), 
              " p-value:", p_val, "\n")
        }
      }
    }
    ############################# modifying the condition for the small loop
    if (min_val  < alpha) {
      cat("\n==> Adding ",selected_col, ": p_vlaue:", min_val, "\n\n")
      sel_cols = if (sel_cols != "") {paste(sel_cols, " + ", selected_col)} else {selected_col}
    } else {
      cat("\n==> No column to add (based on the significant level of", alpha, ")\n")
    }
    exclude = c(exclude, selected_col)
  }
  formula = paste(response, " ~ ", sel_cols)
  model = glm(formula = formula,family=binomial(link=logit), data = dtframe)
  return(model)
}  

forward_normal = forward_p_value(dtframe = data,response = 'cluster_label',exclude = c('cluster_label','X','customer_id'),alpha = 0.05)
summary(forward_normal)

#after trying fit the logit model to the data frame, with the forward selection method => no variable has been found.
# although, it seem that the state of having car might affect whether they purchase bike or not.
# the hypothesis will need more observation and variables (more information from customer) to be verified.