libname IPEDS '~/IPEDS';
options fmtsearch=(IPEDS);

** Step 1: Graduation Rate Calculation **;
data GraduationRates;
  merge ipeds.graduationextended(where=(Group contains 'Incoming cohort')
                                rename=(Total=TotalEnroll grwhitt=WhiteEnroll))
        ipeds.graduationextended(where=(Group contains 'Completers within 150%')
                                rename=(Total=TotalGrad grwhitt=WhiteGrad));
  by unitid;
  drop Group;
  TotalGradRate = TotalGrad / TotalEnroll;
run;

** Step 2: Create Salary Summary Table **;
proc sql;
	create view SalaryTot as
	select unitid, sum(sa09mot) as totalSalary, sum(sa09mct) as TotalFaculty
	from ipeds.salaries
	group by unitid;
quit;

** Step 3: Build Regression Model Dataset **;
proc sql;
	create view RegModelPre as
	select grad.unitid, grad.TotalGradRate as Rate, grad.TotalEnroll as Cohort, /* Using correct Graduation Rate */
				iclevel, control, hloffer, locale, instcat, c21enprf, /*Institution characteristics*/
				uagrntn/scfa2 as GrantRate format=percentn8.2 
						label='Percent of undergraduates receiving grant aid',
				uagrntt/scfa2 as GrantAvg  
						label='Average grant aid received by undergraduates',
				upgrntn/scfa2 as PellRate format=percentn8.2 
						label='Percent of students receiving Pell grants',
				ufloann/scfa2 as LoanRate format=percentn8.2 
						label='Percent of students receiving federal student loans',		
				uagrntt/scfa2 as LoanAvg  
						label='Average amount of federal student loans received',
				scfa2, /*Total undergraduate enrollment*/
				tuition1, fee1, tuition2, fee2, tuition3, fee3, room, roomcap, board, roomamt, boardamt, /*Cost variables*/
				totalSalary/TotalFaculty as AvgSalary label='Average Salary for Faculty',
				scfa2/TotalFaculty as StuFacRatio label='Student to Faculty Ratio' format=6.1
	from GraduationRates as grad, ipeds.characteristics, ipeds.aid, ipeds.tuitionandcosts, SalaryTot
	where grad.unitid = characteristics.unitid 
	  and grad.unitid = aid.unitid 
	  and grad.unitid = tuitionandcosts.unitid 
	  and grad.unitid = SalaryTot.unitid
	  and iclevel = 3; /* Ensuring only baccalaureate institutions */
quit;

** Step 4: Process Data for Regression Model **;
data regmodel;
	set regmodelpre;

	/* Tuition and fee differences (In-State vs. Out-of-State) */
	InDistrictTDiff = tuition2 - tuition1;
	InDistrictT = (tuition1 ne tuition2);
	InDistrictFDiff = fee2 - fee1;
	InDistrictF = (fee1 ne fee2);

	OutStateTDiff = tuition3 - tuition2;
	OutStateT = (tuition3 ne tuition2);
	OutStateFDiff = fee3 - fee2;
	OutStateF = (fee1 ne fee2);

	if room = 2 then do;
		Housing = 0;
		roomamt = 0;
	end;
	else Housing = room;

	if roomcap >= 1 then ScaledHousingCap = scfa2 / roomcap;				
	else ScaledHousingCap = 0;

	if board = 3 then do;
		board = 0;
		boardamt = 0;
	end;

	rename tuition2 = InStateT fee2 = InStateF;
	drop tuition1 tuition3 fee1 fee3 room roomcap scfa2;
	format board 1.;
run;	

** Step 5: Display Data Structure **;
proc contents data=regmodel varnum;
run;

** Step 6: Model Selection Using AIC **;
ods trace on;
proc glmselect data=regmodel;
	class iclevel--c21enprf board;
	model Rate = Cohort -- ScaledHousingCap / selection=stepwise(select=aic stop=aic choose=aic);
	ods output modelInfo=modelInfo1
						NObs=Obs1
						SelectionSummary=Selection1
						ParameterEstimates=Estimates1;				
run;

** Step 7: Transform Model Selection Output for AIC **;
proc transpose data=modelInfo1(where=(label1 in ('Selection Method','Select Criterion','Stop Criterion','Choose Criterion'))) 
		out=model1(drop=_name_);
	var cValue1;
	id label1;
run;

** Step 8: Model Selection Using SBC **;
proc glmselect data=regmodel;
	class iclevel--c21enprf board;
	model Rate = Cohort -- ScaledHousingCap / selection=stepwise(select=aic stop=aic choose=sbc);
	ods output modelInfo=modelInfo2
						NObs=Obs2
						SelectionSummary=Selection2
						ParameterEstimates=Estimates2;				
run;

** Step 9: Transform Model Selection Output for SBC **;
proc transpose data=modelInfo2(where=(label1 in ('Selection Method','Select Criterion','Stop Criterion','Choose Criterion'))) 
		out=model2(drop=_name_);
	var cValue1;
	id label1;
run;





** Step 10: Drop Existing Views Before Creating Tables **;
proc sql;
	drop view work.modelResults1;
	drop view work.modelResults2;
	drop view work.modelResults;
quit;

proc datasets library=work nolist;
	delete modelResults1 modelResults2 modelResults;
quit;

proc sql;
	** Create Table for AIC-Based Model Selection **;
	create table modelResults1 as
	select model1.*, Obs1.NObsRead, Obs1.NObsUsed, Selection1.Step, 
		Selection1.AIC as CriterionValue, Estimates1.Parameter, 
		Estimates1.estimate, Estimates1.stdErr, Estimates1.StandardizedEst
	from model1
		inner join Obs1 on Obs1.label contains 'Read'
		inner join Selection1 on Selection1.Step is not null
		left join Estimates1 on Selection1.EffectEntered eq scan(Estimates1.Parameter, 1, ' ')
	order by Selection1.Step, Estimates1.Parameter;

	** Create Table for SBC-Based Model Selection **;
	create table modelResults2 as
	select model2.*, Obs2.NObsRead, Obs2.NObsUsed, Selection2.Step, 
		Selection2.SBC as CriterionValue, Estimates2.Parameter, 
		Estimates2.estimate, Estimates2.stdErr, Estimates2.StandardizedEst
	from model2
		inner join Obs2 on Obs2.label contains 'Read'
		inner join Selection2 on Selection2.Step is not null
		left join Estimates2 on Selection2.EffectEntered eq scan(Estimates2.Parameter, 1, ' ')
	order by Selection2.Step, Estimates2.Parameter;

	** Combine Both Model Selection Results into One Table **;
	create table modelResults as
	select * from modelResults1 
	union all
	select * from modelResults2;
quit;

** Step 11: Display Final Model Selection Results **;
proc print data=modelResults;
run;
