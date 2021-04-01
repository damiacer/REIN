LIBNAME a'P:\CODES SAS-R-STATA\IMPUTATION_SAS';

DATA temp; SET a.hsb_mar_recoverd; RUN;

/*************************************************************************************************/

/* LINK : https://stats.idre.ucla.edu/sas/seminars/multiple-imputation-in-sas/mi_new_1/ */

proc format;
  value female 0 = "male"
               1= "female";
  value prog 1 = "general"
             2 = "academic"
             3 = "vocation" ;
  value race 1 = "hispanic"
             2 = "asian"
             3 = "african-amer"
             4 = "white";
  value schtyp 1 = "public"
               2 = "private";
  value ses  1 = "low"
             2 = "middle"
             3 = "high";
run;
options fmtsearch=(work);

proc means data = a.hsb_mar nmiss N min max mean std;
  var _numeric_ ;
run; 

/*************************************************************************************************/

/* COMPLETE CASE ANALYSIS */

TITLE " LISTWISE REGRESSION";
proc glm data = a.hsb_mar;
class female (ref=last) prog;
model read = write female math prog /solution ss3;
run;
quit;

		/* CREATE A DUMMY VARIABLE */
		data a.hsb_marY / view=a.hsb_marY;
		set a.hsb_mar;
		_Y = 0;
		run;
		
		/* IDRE UCLA METHOD */
		/* Because proc glm does not accept covariance matrices as data input, 
		the following example will be done with proc reg. This will require us to create dummy variables 
		for our categorical predictor prog since there is no class statement in proc reg. */
		data new2;
		set a.hsb_mar;
		if prog ^=. then do;
		if prog =1 then progcat1=1;
		else progcat1=0;
		if prog =2 then progcat2=1;
		else progcat2=0;
		end;
		run;



/* CHECK MISSING PATTERN */

proc means data=a.hsb_mar nmiss; 
var female write read math prog;
run;

data hsb_flag;
set a.hsb_mar;
if female =.  then female_flag =1; else female_flag =0;
if write  = . then write_flag  =1; else write_flag  =0;
if read   = . then read_flag   =1; else read_flag   =0;
if math   = . then math_flag   =1; else math_flag   =0;
if prog   = . then prog_flag   =1; else prog_flag   =0;
run;
proc freq data=hsb_flag;
tables female_flag write_flag read_flag math_flag prog_flag;
run;

/* Examine Missing Data Patterns among your variables of interest. 
The proc mi procedure has an ods option called misspattern that will output a 
table of the missing data patterns present in your data file. */

proc mi data=HSB_flag nimpute=0 ;
var socst write read female math prog;
ods select misspattern;
run;
