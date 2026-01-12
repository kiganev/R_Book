* Written by R;
*  write.foreign(xl_data2, "xldata.csv", "xldata.sas", package = "SAS") ;

DATA  rdata ;
INFILE  "xldata.csv" 
     DSD 
     LRECL= 25 ;
INPUT
 Index
 Value
;
RUN;
