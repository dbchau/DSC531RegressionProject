libname IPEDS '~/IPEDS';
options fmtsearch=(IPEDS);

proc sql;
	create view SalaryTot as
	select unitid, sum(sa09mot) as totalSalary, sum(sa09mct) as TotalFaculty
	from ipeds.salaries
	group by unitid
	;
	create view RegModelPre as
	select gradrates.unitid, Rate, Cohort, /*From GradRates*/
				iclevel, control, hloffer, locale, instcat, c21enprf, /*From Characteristics*/
				uagrntn/scfa2 as GrantRate format=percentn8.2 
						label='Percent of undergraduate students awarded federal, state, local, institutional or other sources of grant aid',
				uagrntt/scfa2 as GrantAvg  
						label='Average amount of federal, state, local, institutional or other sources of grant aid awarded to undergraduate students',
				upgrntn/scfa2 as PellRate format=percentn8.2 
						label='Percent of undergraduate students awarded Pell grants',
				ufloann/scfa2 as LoanRate format=percentn8.2 
						label='Percent of undergraduate students awarded federal student loans',		
				uagrntt/scfa2 as LoanAvg  
						label='Average amount of federal student loans awarded to undergraduate students', scfa2, /*From Aid*/
				tuition1, fee1, tuition2, fee2, tuition3, fee3, room, roomcap, board, roomamt, boardamt, /*From TuitionAndCosts*/
				totalSalary/TotalFaculty as AvgSalary label='Average Salary for 9-month faculty',
				scfa2/TotalFaculty as StuFacRatio label='Student to Faculty Ratio' format=6.1
	from ipeds.gradrates, ipeds.characteristics, ipeds.aid, ipeds.tuitionandcosts, SalaryTot
	where gradrates.unitid eq characteristics.unitid eq aid.unitid eq tuitionandcosts.unitid eq SalaryTot.unitid
	;
quit;

data regmodel;
	set regmodelpre;
	InDistrictTDiff = tuition2-tuition1;
	if tuition1 ne tuition2 then InDistrictT = 1;
		else InDistrictT = 0;
	InDistrictFDiff = fee2-fee1;
	if fee1 ne fee2 then InDistrictF = 1;
		else InDistrictF = 0;

	OutStateTDiff = tuition3-tuition2;
	if tuition3 ne tuition2 then OutStateT = 1;
		else OutStateT = 0;
	OutStateFDiff = fee3-fee2;
	if fee1 ne fee2 then OutStateF = 1;
		else OutStateF = 0;

	if room eq 2 then do;
		Housing=0;
		roomamt=0;
	end;
		else Housing=room;

	if roomcap ge 1 then ScaledHousingCap = scfa2/roomcap;				
		else ScaledHousingCap = 0;

	if board eq 3 then do;
		board = 0;
		boardamt = 0;
	end;
	rename tuition2=InStateT fee2=InStateF;
	drop tuition1 tuition3 fee1 fee3 room roomcap scfa2;
	format board 1.;
run;			
proc contents data=regmodel varnum;
run;

proc glmselect data=regmodel;
	class iclevel c21enprf board;
	model rate = cohort|iclevel|c21enprf|board|scaledHousingCap @1/selection=stepwise(select=aic stop=aic choose=aic);
	ods output modelInfo=modelInfo1
						NObs=Obs1
						SelectionSummary=Selection1
						ParameterEstimates=Estimates1;				
run;

ods trace on;
proc glmselect data=regmodel;
	class iclevel--c21enprf board;
	model rate = cohort -- scaledHousingCap/selection=stepwise(select=aic stop=aic choose=aic);
	ods output modelInfo=modelInfo1
						NObs=Obs1
						SelectionSummary=Selection1
						ParameterEstimates=Estimates1;				
run;

proc transpose data=modelInfo1(where=(label1 in ('Selection Method','Select Criterion','Stop Criterion','Choose Criterion'))) 
		out=model1(drop=_name_);
	var cValue1;
	id label1;
run;

proc glmselect data=regmodel;
	class iclevel--c21enprf board;
	model rate = cohort -- scaledHousingCap/selection=stepwise(select=aic stop=aic choose=sbc);
	ods output modelInfo=modelInfo2
						NObs=Obs2
						SelectionSummary=Selection2
						ParameterEstimates=Estimates2;				
run;

proc transpose data=modelInfo2(where=(label1 in ('Selection Method','Select Criterion','Stop Criterion','Choose Criterion'))) 
		out=model2(drop=_name_);
	var cValue1;
	id label1;
run;

proc sql;
	create view modelResults1 as
	select model1.*, NObsRead, NObsUsed, Step, Parameter, CriterionValue,
		estimate, stdErr, StandardizedEst
	from model1, obs1(where=(label contains 'Read')),selection1(rename=(AIC=CriterionValue)),estimates1
	where EffectEntered eq scan(parameter,1,' ')
	order by 'Selection Method'n,'Stop Criterion'n,'Choose Criterion'n, Step, Parameter
	;
	create view modelResults2 as
	select model2.*, NObsRead, NObsUsed, Step, case when Parameter ne '' then Parameter else EffectEntered end as Parameter, CriterionValue,
		estimate, stdErr, StandardizedEst
	from model2, obs2(where=(label contains 'Read')),
		selection2(rename=(SBC=CriterionValue)) left join estimates2 on EffectEntered eq scan(parameter,1,' ')
	order by 'Selection Method'n,'Stop Criterion'n,'Choose Criterion'n, Step, Parameter
	;
	create table modelResults as
	select * from modelResults1 
	union
	select * from modelResults2
	;
quit;


%macro ModelSelectSBC(library=work, dataset=, response=, inter=N, hier=Y, method=stepwise, select=aic, stop=aic, choose=aic, outputData=);
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
ScaledHousingCap
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
Housing
board
;

%ModelSelectSBC(dataset=regmodel,response=rate,outputData=out1);
%ModelSelectSBC(dataset=regmodel,response=rate,choose=SBC,outputData=out2);
%ModelSelectSBC(dataset=regmodel,response=rate,choose=SBC,outputData=out3,inter=yes,hier=no);

%ModelSelectSBC(dataset=regmodel, response=rate, select=aic, stop=aic, choose=aic, outputData=out4);
%ModelSelectSBC(dataset=regmodel, response=rate, select=bic, stop=bic, choose=bic, outputData=out5);
%ModelSelectSBC(dataset=regmodel, response=rate, inter=yes, hier=no, select=sbc, stop=sbc, choose=sbc, outputData=out6);
%ModelSelectSBC(dataset=regmodel, response=rate, select=aic, stop=aic, choose=aic, inter=no, hier=yes, outputData=out7);
%ModelSelectSBC(dataset=regmodel, response=rate,  select=aic, stop=aic, choose=aic, inter=yes, hier=yes, outputData=out8);


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
ScaledHousingCap
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
;
%ModelSelectSBC(dataset=regmodel,response=rate,choose=SBC,outputData=out9,inter=yes,hier=no);
%ModelSelectSBC(dataset=regmodel, response=rate, select=aic, stop=aic, choose=aic, inter=no, hier=yes, outputData=out10);

data SBCResults;
	length class quant parameter $500;/**these could have different lengths, so we set something long prior to assembly**/
	set out1 out2 out3 out4 out5 out6 out7 out8 out9 out10;
run;

/**If you sort by the criterion value you can see which has the lowest in AICResults/SBCResults to decide on a model**/

%macro ModelSelectAIC(library=work, dataset=, response=, inter=N, hier=Y, method=stepwise, select=aic, stop=aic, choose=aic, outputData=);
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
		selection(rename=(AIC=CriterionValue)) left join estimates on EffectEntered eq scan(parameter,1,' ')
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
ScaledHousingCap
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
Housing
board
;

%ModelSelectAIC(dataset=regmodel,response=rate,outputData=out11);
%ModelSelectAIC(dataset=regmodel,response=rate,choose=SBC,outputData=out12);
%ModelSelectAIC(dataset=regmodel,response=rate,choose=SBC,outputData=out13,inter=yes,hier=no);

%ModelSelectAIC(dataset=regmodel, response=rate, select=aic, stop=aic, choose=aic, outputData=out14);
%ModelSelectAIC(dataset=regmodel, response=rate, select=bic, stop=bic, choose=bic, outputData=out15);
%ModelSelectAIC(dataset=regmodel, response=rate, inter=yes, hier=no, select=sbc, stop=sbc, choose=sbc, outputData=out16);
%ModelSelectAIC(dataset=regmodel, response=rate, select=aic, stop=aic, choose=aic, inter=no, hier=yes, outputData=out17);
%ModelSelectAIC(dataset=regmodel, response=rate,  select=aic, stop=aic, choose=aic, inter=yes, hier=yes, outputData=out18);


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
ScaledHousingCap
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
;
%ModelSelectAIC(dataset=regmodel,response=rate,choose=SBC,outputData=out19,inter=yes,hier=no);
%ModelSelectAIC(dataset=regmodel, response=rate, select=aic, stop=aic, choose=aic, inter=no, hier=yes, outputData=out20);


data AICResults;
	length class quant parameter $500;/**these could have different lengths, so we set something long prior to assembly**/
	set out11 out12 out13 out14 out15 out16 out17 out18 out19 out20;
run;
/**The Models in AICResults and SBCResults are the same, just with different criterion values listed. Decide if you want the
model with the best AIC or the best SBC and choose from the tables**/