%macro ModelSelect(library=work, dataset=, response=, inter=N, hier=Y, method=stepwise, select=aic, stop=aic, choose=aic, outputData=);
	/**class and quant have been removed as parameters and will be set globally for multiple calls with %let statements
			and are expected to be full lists (no shortcuts)**/
proc glmselect data=&library..&dataset;
	%if(&class ne ) %then %do;
		class &class;
	%end;	/**We skip the class statement if no class variables are provided...**/
	model &response = %if(&class ne ) %then %sysfunc(tranwrd(&class,%str( ),|)) |;/**and, more importantly, skip them and the | in the model**/
																					 %sysfunc(tranwrd(&quant,%str( ),|)) 
												/**in both lists, the spaces are translated to | -- %str forces the space to be treated as a literal space, not
															just spacing in the code editor (it's a macro value, so we don't use quotes)**/
										%if(%upcase(%substr(&inter,1,1)) eq Y) %then @2; 
											%else @1;/**If inter starts with y/Y, 2-way interactions are in, otherwise no interactions**/
			 /selection=&method(select=&select stop=&stop choose=&choose)
								%if(%upcase(%substr(&hier,1,1)) eq Y) %then hierarchy=single; /**If hier starts with y/Y, single is set for hierarchy
																																								otherwise it remains the default**/
			;
	ods output modelInfo=modelInfo
						NObs=Obs
						SelectionSummary=Selection
						ParameterEstimates=Estimates;				
run;

proc transpose data=modelInfo(where=(label1 in ('Selection Method','Select Criterion','Stop Criterion','Choose Criterion'))) 
		out=model(drop=_name_);
	var cValue1;
	id label1;
run;

proc sql;
	create table &outputData as
	select model.*, "&class" as class, "&quant" as quant, "%upcase(%substr(&inter,1,1))" as Interactions, "%upcase(%substr(&hier,1,1))" as Hierarchy,
		NObsRead, NObsUsed, Step, case when Parameter ne '' then Parameter else EffectEntered end as Parameter, CriterionValue,
		estimate, stdErr, StandardizedEst
	from model, obs(where=(label contains 'Read')),
		selection(rename=(SBC=CriterionValue)) left join estimates on EffectEntered eq scan(parameter,1,' ')
	order by 'Selection Method'n,'Stop Criterion'n,'Choose Criterion'n, Step, Parameter
	;
quit;
%mend;

options mprint;
ods exclude all;
/**It's a lot easier to paste lists of variables from Excel into a %let--the returns are treated like spaces when the value is defined**/
%let quant=
Cohort
GrantRate
GrantAvg
PellRate
LoanRate
LoanAvg
InDistrictT
InDistrictTDiff
InDistrictF
InDistrictFDiff
InStateT
InStateF
OutStateT
OutStateTDiff
OutStateF
OutStateFDiff
DormCapScaled
roomamt
boardamt
AvgSalary
StuFacRatio
;

%let class=
iclevel
control
hloffer
locale
instcat
c21enprf
cbsatype
Housing
board
;

%ModelSelect(dataset=regmodel,response=rate,outputData=out1);
%ModelSelect(dataset=regmodel,response=rate,choose=SBC,outputData=out2);
%ModelSelect(dataset=regmodel,response=rate,choose=SBC,outputData=out3,inter=yes,hier=no);

data modelresults;
	length class quant parameter $500;/**these could have different lengths, so we set something long prior to assembly**/
	set out1 out2 out3;
run;