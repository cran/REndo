#'@title Test scores of 3054 test scores of 1174 students in 60 schools
#'@description A dataset containing student achievement scores on a statewide mathematics test between 1994 and 2000 in Dallas, Texas.
#'             Due to privacy protection the data is a simulated dataset based on the variance-covariance matrix of the real dataset.
#'             The variables are as follows:
#'             \itemize{
#'             \item Intecept - intercept.
#'             \item SID - school ID. Runs from 1 to 60.
#'             \item CID - student ID. Runs from 1 to 1174.
#'             \item TLI - the mathematics test score.
#'             \item GRADE_3 - grade level minus 3.
#'             \item RETAINED - whether a student was retained in the same grade.
#'             \item SWITCHSC - whetehr a student switched schools during the previous year.
#'             \item S_FREELU - the proportion of students at each school who were eligible for a free or reduced-price lunch program.
#'             \item FEMALE - dummy variable, 1 if felmale student, 0 if male student.
#'             \item BLACK - dummy variable indicating whether the student is African-American.
#'             \item HISPANIC - dummy variable indicating whether the student is Hispanic.
#'             \item OTHER - dummy variable equal to 1 if the student is neither Caucasian, nor African-American, nor Hispanic.
#'             \item  C_COHORT - the cohort the student belongs to.
#'             \item T_EXPERI - represents the average years of experience of teachers in a given school.
#'             \item CLASS_SI - represents the average class size in a given school.
#'             \item P_MINORI - represents the percentage of minority students in a given school.
#'             }
#'@docType data
#'@keywords datasets
#'@name tScores
#'@usage data(tScores)
#'@format A data frame with 3054 rows and 16 variables.
"tScores"