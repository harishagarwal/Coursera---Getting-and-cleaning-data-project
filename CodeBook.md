# Introduction

The script run_analysis.Rperforms the 5 steps described in the course project's definition.

* First, all the similar data is merged using the rbind() function. 
* By similar, we address those files having the same number of columns and referring to the same entities.
* Then, only those columns with the mean and standard deviation measures are taken from the whole dataset. 
* After extracting these columns, they are given the correct names, taken from features.txt.
* As activity data is addressed with values 1:6, we take the activity names and IDs from activity_labels.txt and they are substituted in the dataset.
* On the whole dataset, those columns with vague column names are corrected.
* Finally, we generate a new dataset with all the average measures for each subject and activity type (30 subjects * 6 activities = 180 rows). The output file is called tidy.txt, and uploaded to this repository.

# Variables

* xTrain, yTrain, xTest, yTest, subjectTrain and subjectTest contain the data from the downloaded files.
* trainData, testData and finalData merge the previous datasets to further analysis.
* finalData merges trainData, testData in a big dataset.
* features dataset contains the correct names for the xTrain and xTest dataset
* Columns in all other datasets are renamed based on data they contain 
* data_mean_std contains data with column names having mean or std, along with other required variables
* The variable names are renamed with appropriate readable names
* Finally, tidy contains the relevant averages which will be later stored in a .txt file.
