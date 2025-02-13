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

