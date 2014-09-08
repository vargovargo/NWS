*Data process, read in temperature data saved as temporal dataset total;

** avg. median N STD CV **;
PROC SORT DATA=total;
  BY ID Year Month Day Hour;
RUN;

PROC MEANS DATA=total MEAN NOPRINT;
  BY ID Year Month Day Hour;
  VAR Temperature Humidity;
  OUTPUT OUT=summary MEAN(Temperature Humidity)=avg_F avg_H;
RUN;

** 5 STD exclude outlier **;

PROC MEANS DATA=summary STD mean;
  VAR avg_F avg_H;
RUN;
DATA summary5;
  SET summary;
  IF avg_F > 126.772 OR avg_F < 11.732 THEN DELETE;
RUN;

** daily coverage **;
PROC FREQ DATA=summary5 NOPRINT;
  TABLES ID* Month * Day* Year / OUT=f;
RUN;

PROC SORT DATA=summary5;
BY ID Year Month Day;
PROC SORT DATA=f;
BY ID Year Month Day;
DATA coverage;
  MERGE summary5 f;
  BY ID Year Month Day; 
RUN;

** delete date with less than 16 hour data **;

DATA coverage;
  SET coverage;
  IF COUNT<16 THEN DELETE;
RUN;

** yearly coverage **;
PROC FREQ DATA=coverage NOPRINT;
  TABLES ID*Month*Day*year/ OUT=h;
RUN;

PROC SORT DATA=h;
BY ID Year;
PROC MEANS DATA=h N NOPRINT;
  BY ID Year;
  VAR Day;
  OUTPUT OUT=i N(Day)=number;
RUN;

DATA i;
  SET i;
  Coverage=number/153;
RUN;

DATA hi;
  MERGE coverage i;
  BY ID Year;
  IF coverage<0.75 THEN DELETE;
RUN;


** Caulculate HI **;
DATA hi ;
  SET hi;
  HI=0;
  IF avg_F>80 THEN HI = -42.379 + 2.04901523*avg_F + 10.14333127*avg_H - 0.22475541*avg_F*avg_H - 0.00683783*avg_F*avg_F - 0.05481717*avg_H*avg_H + 0.00122874*avg_F*avg_F*avg_H + 0.00085282*avg_F*avg_H*avg_H - 0.00000199*avg_F*avg_F*avg_H*avg_H;
  IF avg_F>=80 AND avg_F <=112 AND avg_H<13 THEN adjustment= ((13-avg_H)/4)*SQRT((17-ABS(avg_F-95))/17); 
  ELSE IF avg_F>=80 AND avg_F<=87 AND avg_H>85 THEN adjustment=((avg_H-85)/10) * ((87-avg_F)/5); 
  ELSE adjustment=0;
  HI_a=HI+adjustment;
RUN;

** mark heat **;
DATA hi;
SET hi;
IF HI_a>=105 THEN heat=1;
ELSE HEAT=0;
RUN;

** Daily Heat **;
PROC SORT DATA=hi OUT=hi;
BY ID Year Month Day Heat;
RUN;

PROC MEANS DATA=hi NOPRINT;
  VAR Heat;
  BY ID Year Month Day;
  OUTPUT OUT=j mean(Heat)=mheat;
RUN;

DATA j;
  SET j;
  IF mheat>0 THEN Heat_Day=1;
  ELSE Heat_Day=0;
  DROP _FREQ_ Heat mheat _TYPE_;
RUN;

proc sort data=qy.Chicago;
by ID;
proc sort data=county;
by ID;
run;
data county(rename=(name=ID));
set county;
RUN;

DATA county_75;
MERGE qy.Chicago county;
BY ID;
RUN;

data county_75 (drop=heat FID_1 COUNT PERCENT FID_2 NAME_1 STATE_NAME CNTY_FIPS FIPS MSA MSAoutcome MSA_Factor);
set county_75;
RUN;


** 2Day **;
DATA atl_1;
SET county_75;
heat_before=lag1(heat_Day);
IF heat_Day=1 and heat_before=1 THEN heat_2Day=1;
ELSE heat_2Day=0;
RUN;


DATA Day2;
  set Day2;
  Heat_2Day=max(Heat,Heat_2Day);
RUN;

PROC UNIVARIATE DATA=atl_1;
VAR heat_2Day;
RUN;

libname qy'H:\Yanglab_temperature_Atlanta';

data qy.Chicago;
  set day2;
run;
