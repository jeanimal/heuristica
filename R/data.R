#' Chicago high school dropout rates. 
#'
#' Chicago high school droput rates from 1995 and associated variables
#' like average students per teach and percent low income.
#'
#' The data is based on:
#'
#'  Morton, Felicia B. (1995). Charting a School's Course. Chicago.
#'     February, pp. 86-95.
#'
#'  Rodkin, Dennis. (1995). 10 Keys for Creating Top High Schools.
#'     Chicago. February, pp. 78-85.
#'
#' This is the data set used in simulations by the ABC (Adaptive Behavior
#' and Cognition) research group.
#'
#' @format A data frame.
#' \describe{
#'   \item{Name_of_School}{Name of School}
#'   \item{Running_Number}{Running Number}
#'   \item{Included_in_web_and_web_corrected}{Included_in_web_and_web_corrected}
#'   \item{Dropout_Rate}{Dropout rate in percent, from 0 to 100, counting all students in grades 9 through 12 who left school permanently during the 1993-4 school year}
#'   \item{completeness_of_data}{completeness_of_data}
#'   \item{Enrollment}{Enrollment as of September 30, 1993}
#'   \item{Attendance_Rate}{Attendance rate in percent, from 0 to 100, averaged over the school year}
#'   \item{Graduation_Rate}{Graduation rate in percent, from 0 to 100, based on freshmen who finished together 4 years later, in 1994}
#'   \item{Parental_Involvement_Rate}{Parental involvement rate in percent, from 0 to 100, counted as parents who had contact with teachers as a percent of students (with no firm state rules on how to measure this)}
#'   \item{Limited_English_Students}{Limited_English_Students in percent, from 0 to 100, based on the number of students found eligible for bilingual education}
#'   \item{Low_Income_Students}{Low_Income_Students in percent, from 0 to 100, based on families eligible for free or reduced price lunches or are publicly supported}
#'   \item{Average_Class_Size_Student_per_Teacher}{Average_Class_Size_Student_per_Teacher, calculated as number of students divided by number of teachers on the first day of May}
#'   \item{Percent_white_students}{Percent_white_students, from 0 to 100}
#'   \item{percent_black_students}{percent_black_students, from 0 to 100}
#'   \item{percent_hispanic_students}{percent_hispanic_students, from 0 to 100}
#'   \item{percent_asian_students}{percent_asian_students, from 0 to 100}
#'   \item{percent_minority_teacher}{percent minority teacher, from 0 to 100}
#'   \item{average_composite_ACT_Score}{average composite ACT Score}
#'   \item{Reading}{Reading score on Illinois Goal Assessment Program (IGAP)}
#'   \item{Math}{Math score on IGAP}
#'   \item{Science}{Science score on IGAP}
#'   \item{Social_Science}{Social science score on IGAP}
#'   \item{Writing}{Writing score on IGAP}
#' }
"abc_hs_dropout"
