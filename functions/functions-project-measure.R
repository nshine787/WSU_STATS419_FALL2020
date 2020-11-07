# prepareMeasureData(measure)
prepareMeasureData = function(measure)
{
  #Convert data to cm
  raw_measure = read.csv('C:/Users/Nathan/Desktop/Stat419/measure-students.txt', header=TRUE, sep='|')
  measure = unique(raw_measure)
  measure[measure$person_id == '81a9f3915f64dc6edd329b89bea87d5e', 'units'] = 'cm'
  inchNames = c('in', 'In', 'inches', 'Inch')
  rowsToConvert = measure$units %in% inchNames
  columnsToConvert = colnames(measure[,4:26])
  measure[rowsToConvert,columnsToConvert] = conv_unit(measure[rowsToConvert,columnsToConvert],'inch','cm')
  measure$units = 'cm'
  
  #Clean data for the gender, writing, and swinging variables
  measure[measure$gender %in% c('M','male','Male','m'),'gender'] = 'm'
  measure[measure$gender %in% c('F','female','Female','f'),'gender'] = 'f'
  measure[!(measure$gender %in% c('m','f')),'gender'] = 'nb'
  measure$gender = as.factor(measure$gender)
  measure$writing = as.factor(substr(tolower(measure$writing),1,1))
  measure$swinging = as.factor(substr(tolower(measure$swinging),1,1))
  
  #Clean data for the eye variable
  errorEntry = na.omit(measure$eye == 'brown')
  measure[errorEntry,'eye_color'] = 'brown'
  measure[errorEntry,'eye'] = 'left'
  measure$eye = substr(tolower(measure$eye),1,1)
  measure[measure$eye %in% c('b','e'),'eye'] = 'b'
  measure$eye = as.factor(measure$eye)
  
  #Clean data for the eye color variable
  measure[measure$eye_color == 'blue/green','eye_color'] = 'blue-green'
  measure$eye_color = as.factor(tolower(measure$eye_color))
  
  #Clean data for the ethnicity variable
  measure$ethnicity = tolower(measure$ethnicity)
  measure[measure$ethnicity == 'asain','ethnicity'] = 'asian'
  measure[measure$ethnicity == 'caucasain','ethnicity'] = 'caucasian'
  
  measure[measure$ethnicity %in% c('anglo','caucasian','white italian', 'white non-hispanic'),'ethnicity'] = 'white'
  measure[measure$ethnicity == 'african american','ethnicity'] = 'black'
  measure[measure$ethnicity %in% c('latin american', 'latino'),'ethnicity'] = 'hispanic'
  measure[measure$ethnicity %in% c('chinese','indian','japanese','korean','laotian','filipino'),'ethnicity'] = 'asian'
  measure[!(measure$ethnicity %in% c('asian','black','hispanic','native american','pacific islander','white')),'ethnicity'] = 'mixed'
  measure$ethnicity = as.factor(measure$ethnicity)
  
  measure;
}