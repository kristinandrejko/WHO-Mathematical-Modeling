# WHO-Mathematical-Modeling
 To assess the contribution of mathematical modeling studies to World Health Organization (WHO) guideline recommendations, and the quality of evidence contributed by these studies

Data dictionary for  MM_TotalGuidelines_External_09-05-20.xlsx 

Column	Variable	Description

A	grc	Guideline Identification Number

B	title	Guideline Title 

C	pub_date	Publication Year of guildeline

D	topic	Guideline Topic

E	mm_studies_row	Number of Mathematical Models per Recommendation/ row (each row corresponds to 1 guideline recommendation informed by at least one guideline recommendation)

F	rec_id	Label for unique recommendation 

G	rec_text	Recommendation text from guideline 

H	evidence_strength	Strength of Recommendation

I	evidence_quality	Quality of Evidence 

J	grade_criteria	If available, the GRADE criteria the model informed within the guideline 

K	grade_eprofile	GRADE Evidence Profile: Is the model evaluated using the formal GRADE domains

grade_eprofile = “Y” : GRADE Evidence Profile/ Table framework

grade_eprofile = “Y*”: GRADE Summary of Evidence section

grade_eprofile = “N” AND grade_etd =0: GRADE supplementary section in text

L	grade_etd	GRADE Evidence to Decision (EtD) Table: Is the model evaluated in the EtD framework?

When grade_etd >0, the recommendation as cited in the EtD framework

M	model_assumptions	Does the guideline describe model assumptions and/or limitations? (1= yes, 0= no)

N	model_sensitivity	Does the guideline describe a sensitivity analysis? (1= yes, 0= no)

O	model_validation	Does the model describe cross-validation? (1= yes, 0= no)

P	model_origin	Were the models commissioned de-novo, found from existing literature, or both? 
