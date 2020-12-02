library(humanVerseWSU)
# prepareMeasureData(measure.df)
# takes the file location passed in from 'measure' and then returns a cleaned dataframe
# 411f8b5c9500b7cc928c93dd1a0006b7
#comparison = cm.df$arm.reach > cm.df$height;
#comparison[is.na(comparison)] = TRUE;
#cm.ndf = cm.df[comparison,];

prepareMeasureData = function(measure)
{
  library(measurements)
  #Convert data to cm
  # raw_measure.df = read.csv(measure, header=TRUE, sep='|')
  raw_measure.df = measure
  
  measure.df = unique(raw_measure.df)
  measure.df[measure.df$person_id == '81a9f3915f64dc6edd329b89bea87d5e', 'units'] = 'cm'
  inchNames = c('in', 'In', 'inches', 'Inch')
  rowsToConvert = gsub("\"","",measure.df$units) %in% inchNames
  columnsToConvert = colnames(measure.df[,4:26])
  measure.df[rowsToConvert,columnsToConvert] = conv_unit(measure.df[rowsToConvert,columnsToConvert],'inch','cm')
  measure.df$units = 'cm'
  
  #Clean data for the gender, writing, and swinging variables
  # measure.df[measure.df$gender %in% c('M','male','Male','m'),'gender'] = 'm'
  # measure.df[measure.df$gender %in% c('F','female','Female','f'),'gender'] = 'f'
  
  measure.df$gender = as.factor(substr(gsub("\"","",tolower(measure.df$gender)),1,1))
  measure.df[!(measure.df$gender %in% c('m','f')),'gender'] = 'nb'
  measure.df$gender = as.factor(measure.df$gender)
  measure.df$writing = as.factor(substr(gsub("\"","",tolower(measure.df$writing)),1,1))
  measure.df$swinging = as.factor(substr(gsub("\"","",tolower(measure.df$swinging)),1,1))
  
  #Clean data for the eye variable
  errorEntry = na.omit(measure.df$eye == 'brown')
  measure.df[errorEntry,'eye_color'] = 'brown'
  measure.df[errorEntry,'eye'] = 'left'
  measure.df$eye = substr(gsub("\"","",tolower(measure.df$eye)),1,1)
  measure.df[measure.df$eye %in% c('b','e'),'eye'] = 'b'
  measure.df$eye = as.factor(measure.df$eye)
  
  #Clean data for the eye color variable
  measure.df[measure.df$eye_color == 'blue/green','eye_color'] = 'blue-green'
  measure.df$eye_color = as.factor(gsub("\"","",tolower(measure.df$eye_color)))
  
  #Clean data for the ethnicity variable
  measure.df$ethnicity = gsub("\"","",tolower(measure.df$ethnicity))
  measure.df[measure.df$ethnicity == 'asain','ethnicity'] = 'asian'
  measure.df[measure.df$ethnicity == 'caucasain','ethnicity'] = 'caucasian'
  
  measure.df[measure.df$ethnicity %in% c('anglo','caucasian','white italian', 'white non-hispanic'),'ethnicity'] = 'white'
  measure.df[measure.df$ethnicity == 'african american','ethnicity'] = 'black'
  measure.df[measure.df$ethnicity %in% c('latin american', 'latino'),'ethnicity'] = 'hispanic'
  measure.df[measure.df$ethnicity %in% c('chinese','indian','japanese','korean','laotian','filipino'),'ethnicity'] = 'asian'
  measure.df[!(measure.df$ethnicity %in% c('asian','black','hispanic','native american','pacific islander','white')),'ethnicity'] = 'mixed'
  measure.df$ethnicity = as.factor(measure.df$ethnicity)
  
  n.rows = dim(measure.df)[1];
  
  getOne = c("hand.length", "hand.width", "hand.elbow", "elbow.armpit", "arm.reach", "foot.length", "floor.kneepit", "floor.hip", "floor.armpit");
  for(i in 1:n.rows) {
    for(col in getOne) {
      rightSide = paste0(col,'.right')
      leftSide = paste0(col,'.left')
      measure.df[i,col] = mean(unlist(measure.df[i,c(rightSide,leftSide)]),na.rm=TRUE)
    }
  }
  
  measure.df[,c('side','units','hand.length.left','hand.length.right','hand.width.left','hand.width.right','hand.elbow.left','hand.elbow.right','elbow.armpit.left','elbow.armpit.right','arm.reach.left','arm.reach.right','foot.length.left','foot.length.right','floor.kneepit.left','floor.kneepit.right','floor.hip.left','floor.hip.right','floor.armpit.left','floor.armpit.right')] = NULL
  
  # constructs new columns
  measure.df$shoulder.width = measure.df$arm.span - 2*(measure.df$hand.elbow + measure.df$elbow.armpit)
  measure.df$breast.head = measure.df$height - measure.df$floor.armpit
  measure.df[measure.df$breast.head < 0 & !is.na(measure.df$breast.head),'breast.head'] = NA
  measure.df[measure.df$shoulder.width < 0 & !is.na(measure.df$shoulder.width),'shoulder.width'] = NA
  # measure.df$thigh.length = measure.df$floor.hip - measure.df$floor.kneepit
  
  # reorders the columns by putting covariates at the end
  n.rows = dim(measure.df)[2]
  measure.df = measure.df[,c(1:7,18:n.rows,8:17)]
  colNumsToReplace = grep('.NA',names(measure.df))
  for (i in colNumsToReplace){
    names(measure.df)[i] = gsub('.{3}$','',names(measure.df)[i])
  }
  measure.df = measure.df[measure.df$data_collector != '411f8b5c9500b7cc928c93dd1a0006b7',]
  measure.df[measure.df$hand.length > 50,] = NA
  measure.df[measure.df$arm.reach < measure.df$height & !is.na(measure.df$arm.reach) & !is.na(measure.df$height),'arm.reach'] = NA
  measure.df[measure.df$height < 40 & !is.na(measure.df$height),c('height','arm.reach')] = NA
  measure.df[measure.df$foot.length > 35 & !is.na(measure.df$foot.length),c('foot.length','floor.kneepit')] = NA
  measure.df[measure.df$head.height > 35 & !is.na(measure.df$head.height),'head.height'] = NA
  measure.df[measure.df$floor.kneepit > 60 & !is.na(measure.df$floor.kneepit),'floor.kneepit'] = NA
  measure.df[!is.na(measure.df$arm.span) & !is.na(measure.df$height) & measure.df$arm.span < measure.df$height/2,'arm.span'] = NA
  measure.df[!is.na(measure.df$hand.length) & measure.df$hand.length <10 & measure.df$age > 10,'hand.length'] = NA 
  
  row.names(measure.df) = 1:dim(measure.df)[1]
  measure.df
}

# returns a dataframe with all of the covariates for looking at the make-up of the dataset
grabCovariates = function(measure)
{
  covariates = c('writing','eye','eye_color','swinging','age','gender','quality','minutes','ethnicity','notes')
  measure[,covariates];
}

# grabs all of the rows from the dataframe that match the gender given by the parameter of the same name
# by default it only grabs the measurements of people aged 16 or more. This way there are less outliers
grabGenderRows = function(measure, gender, minAge = 16)
{
  lastEntryToGrab = dim(measure)[2]-10
  measure[measure$gender == gender & measure$age >= minAge ,3:lastEntryToGrab];
}

constructNewVariables = function(measure)
{
  measure$shoulder.width = measure$arm.span - 2*(measure$hand.elbow + measure$elbow.armpit)
  measure$breast.head = measure$height - measure$floor.armpit
  measure$thigh.length = measure$floor.hip - measure$floor.kneepit
  measure;
}