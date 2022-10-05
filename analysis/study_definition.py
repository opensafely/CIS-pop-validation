
from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
  Measure
)

# Import codelists from codelists.py
import codelists

# import json module
import json

# import study dates defined in "./analysis/lib/study-dates.R" script
with open("./analysis/lib/study-dates.json") as f:
  study_dates = json.load(f)

start_date = study_dates["start_date"]
end_date = study_dates["end_date"]

# Specify study definition
study = StudyDefinition(
  
  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": "2020-01-01", "latest": end_date},
    "rate": "uniform",
    "incidence": 0.2,
    "int": {"distribution": "normal", "mean": 1000, "stddev": 100},
    "float": {"distribution": "normal", "mean": 25, "stddev": 5},
  },
  
  index_date = start_date,
  
  # This line defines the study population
  population=patients.satisfying(
    """
      registered
      AND
      age >= 2 AND age <=120
      AND
      sex = "M" OR sex = "F" 
      AND
      NOT has_died
      AND 
      NOT ((care_home_tpp="care_or_nursing_home") OR (care_home_code))
      AND 
      msoa
    """,
    
    # we define baseline variables on the day _before_ the study date
    registered=patients.registered_as_of(
      "index_date",
    ),
    has_died=patients.died_from_any_cause(
      on_or_before="index_date",
      returning="binary_flag",
    ), 
    
    # patients in care or nursing homes according to patient address / residence type look up
    care_home_tpp=patients.care_home_status_as_of(
      "index_date",
      categorised_as={
          "care_or_nursing_home": "IsPotentialCareHome",
          "": "DEFAULT",  # use empty string
      },
      return_expectations={
          "category": {"ratios": {"care_or_nursing_home": 0.05, "": 0.95 }, },
          "incidence": 1,
      },
    ),

    # Patients in long-stay nursing and residential care
    care_home_code=patients.with_these_clinical_events(
      codelists.carehome,
      on_or_before="index_date",
      returning="binary_flag",
      return_expectations={"incidence": 0.01},
    ),
    
    startdate = patients.fixed_value(start_date),
    enddate = patients.fixed_value(end_date),
  ),
  
  
  ###############################################################################
  ## Demographics
  ###############################################################################
  
  age=patients.age_as_of( 
    "index_date",
  ),
  
  age_kids=patients.age_as_of(
    "first_day_of_school_year(index_date)",
  ),

  agebandCIS=patients.categorised_as(
    {
      "": "DEFAULT",
      "2-11" : "age_kids>=2 AND age_kids<=11", # ONS bases this on school year (aged 2 to to year 6)
      "12-15" : "age_kids>=12 AND age_kids<=15", # ONS bases this on school year (year 7 to year 11)
      "16-24" : "age_kids>=16 AND age_kids<=24", # ONS bases this on school year (year 12 to aged 24)
      "25-34" : "age>=25 AND age<=34",
      "35-49" : "age>=35 AND age<=49",
      "50-69" : "age>=50 AND age<=69",
      "70+" : "age>=70",
    },
    return_expectations={
      "category":{"ratios": 
        {
        "2-11"  : 0.1,
        "12-15" : 0.1, 
        "16-24" : 0.1, 
        "25-34" : 0.1,
        "35-49" : 0.2,
        "50-69" : 0.2,
        "70+"   : 0.2,
        }
      }
    },
  ),
  
  ageband5year=patients.categorised_as(
    {
      "": "DEFAULT",
      "0-4"   : "age>=0 AND age<=4", 
      "5-9"   : "age>=5 AND age<=9", 
      "10-14" : "age>=10 AND age<=14", 
      "15-19" : "age>=15 AND age<=19",
      "20-24" : "age>=20 AND age<=24",
      "25-29" : "age>=25 AND age<=29",
      "30-34" : "age>=30 AND age<=34",
      "35-39" : "age>=35 AND age<=39",
      "40-44" : "age>=40 AND age<=44",
      "45-49" : "age>=45 AND age<=49",
      "50-54" : "age>=50 AND age<=54",
      "55-59" : "age>=55 AND age<=59",
      "60-64" : "age>=60 AND age<=64",
      "65-69" : "age>=65 AND age<=69",
      "70-74" : "age>=70 AND age<=74",
      "75-79" : "age>=75 AND age<=79",
      "80-84" : "age>=80 AND age<=84",
      "85-89" : "age>=85 AND age<=89",
      "90+" : "age>=90",
    },
    return_expectations={
      "category":{"ratios": 
        {
        "0-4"   : 0.05,
        "5-9"   : 0.05,
        "10-14" : 0.05,
        "15-19" : 0.05,
        "20-24" : 0.05,
        "25-29" : 0.05,
        "30-34" : 0.05,
        "35-39" : 0.05,
        "40-44" : 0.05,
        "45-49" : 0.05,
        "50-54" : 0.1,
        "55-59" : 0.05,
        "60-64" : 0.05,
        "65-69" : 0.05,
        "70-74" : 0.05,
        "75-79" : 0.05,
        "80-84" : 0.05,
        "85-89" : 0.05,
        "90+"   : 0.05,
        }
      }
    },
  ),
  
  sex=patients.sex(
    return_expectations={
      "rate": "universal",
      "category": {"ratios": {"M": 0.49, "F": 0.51}},
      "incidence": 1,
    }
  ),
  
  # # Ethnicity in 6 categories
  # ethnicity = patients.with_these_clinical_events(
  #   codelists.ethnicity,
  #   returning="category",
  #   find_last_match_in_period=True,
  #   include_date_of_match=False,
  #   return_expectations={
  #     "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
  #     "incidence": 0.75,
  #   },
  # ),
  # 
  # # ethnicity variable that takes data from SUS
  # ethnicity_6_sus = patients.with_ethnicity_from_sus(
  #   returning="group_6",  
  #   use_most_frequent_code=True,
  #   return_expectations={
  #     "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
  #     "incidence": 0.8,
  #   },
  # ),
  
  ################################################################################################
  ## Practice and patient ID variables
  ################################################################################################

  # msoa
  msoa=patients.address_as_of(
    "index_date",
    returning="msoa",
    return_expectations={
      "rate": "universal",
      "category": {"ratios": {"E02000001": 0.0625, "E02000002": 0.0625, "E02000003": 0.0625, "E02000004": 0.0625,
        "E02000005": 0.0625, "E02000007": 0.0625, "E02000008": 0.0625, "E02000009": 0.0625, 
        "E02000010": 0.0625, "E02000011": 0.0625, "E02000012": 0.0625, "E02000013": 0.0625, 
        "E02000014": 0.0625, "E02000015": 0.0625, "E02000016": 0.0625, "E02000017": 0.0625}},
    },
  ),    
  
  # stp is an NHS administration region based on geography
  stp=patients.registered_practice_as_of(
    "index_date",
    returning="stp_code",
    return_expectations={
      "rate": "universal",
      "category": {
        "ratios": {
          "STP1": 0.1,
          "STP2": 0.1,
          "STP3": 0.1,
          "STP4": 0.1,
          "STP5": 0.1,
          "STP6": 0.1,
          "STP7": 0.1,
          "STP8": 0.1,
          "STP9": 0.1,
          "STP10": 0.1,
        }
      },
    },
  ),
  
  # NHS administrative region
  # FIXME can we get an equivalent using patient postcode not GP address?
  region=patients.registered_practice_as_of(
    "index_date",
    returning="nuts1_region_name",
    return_expectations={
      "rate": "universal",
      "category": {
        "ratios": {
          "North East": 0.1,
          "North West": 0.1,
          "Yorkshire and The Humber": 0.2,
          "East Midlands": 0.1,
          "West Midlands": 0.1,
          "East": 0.1,
          "London": 0.1,
          "South East": 0.1,
          "South West": 0.1
          #"" : 0.01
        },
      },
    },
  ),
  
  ## IMD - quintile
  # imd=patients.address_as_of(
  #   "index_date",
  #   returning="index_of_multiple_deprivation",
  #   round_to_nearest=100,
  #   return_expectations={
  #     "category": {"ratios": {c: 1/320 for c in range(100, 32100, 100)}}
  #   }
  # ),
  
  # currently in hospital on index date
  # inhospital = patients.satisfying(
  # 
  #   "discharged_date > index_date",
  #   
  #   discharged_date=patients.admitted_to_hospital(
  #     returning="date_discharged",
  #     on_or_before="index_date", #FIXME -- need to decide whether to include admissions discharged on the same day as booster dose or not
  #     # see https://github.com/opensafely-core/cohort-extractor/pull/497 for codes
  #     # see https://docs.opensafely.org/study-def-variables/#sus for more info
  #     with_patient_classification = ["1"], # ordinary admissions only
  #     #with_discharge_destination = codelists.discharged_to_hospital
  #     date_format="YYYY-MM-DD",
  #     find_last_match_in_period=True,
  #   ), 
  # ),
  # 

  ############################################################
  ## single-day events
  ## "Did any event occur on this day?"
  ############################################################

  # positive covid test
  postest_01=patients.with_test_result_in_sgss(
      pathogen="SARS-CoV-2",
      test_result="positive",
      returning="binary_flag",
      between=["index_date", "index_date"],
      find_first_match_in_period=True,
      restrict_to_earliest_specimen_date=False,
  ),
  
  # positive sympomatic covid test
  # postest_symptomatic_01=patients.satisfying(
  #   """
  #   postest_01 AND symptom_01="Y"
  #   """,
  # 
  #   symptom_01 = patients.with_test_result_in_sgss(
  #     pathogen="SARS-CoV-2",
  #     test_result="positive",
  #     returning="symptomatic",
  #     between=["index_date", "index_date"],
  #     find_first_match_in_period=True,
  #     restrict_to_earliest_specimen_date=False,
  #   ),
  # ),
  
  # positive case identification
  primary_care_covid_case_01=patients.with_these_clinical_events(
    combine_codelists( # FIXME - ask Colm about new codelists
      codelists.covid_primary_care_code,
      codelists.covid_primary_care_positive_test,
      codelists.covid_primary_care_sequelae,
    ),
    returning="binary_flag",
    date_format="YYYY-MM-DD",
    between=["index_date", "index_date"],
    find_first_match_in_period=True,
  ),
  
  # emergency attendance for covid
  covidemergency_01=patients.attended_emergency_care(
    returning="binary_flag",
    date_format="YYYY-MM-DD",
    between=["index_date", "index_date"],
    with_these_diagnoses = codelists.covid_emergency,
    find_first_match_in_period=True,
  ),
  
  # covid admission
  covidadmitted_01=patients.admitted_to_hospital(
    returning="binary_flag",
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_these_diagnoses=codelists.covid_icd10,
    between=["index_date", "index_date"],
    find_first_match_in_period=True,
  ),
  
  any_infection_or_disease_01=patients.satisfying(
    """
    postest_01
    OR primary_care_covid_case_01
    OR covidemergency_01
    OR covidadmitted_01
    """
  ),
  
  
  ############################################################
  ## 14-day events
  ## "Did any event occur within the last 14 days?"
  ############################################################

  # positive covid test
  postest_14=patients.with_test_result_in_sgss(
      pathogen="SARS-CoV-2",
      test_result="positive",
      returning="binary_flag",
      between=["index_date - 13 days", "index_date"],
      find_first_match_in_period=True,
      restrict_to_earliest_specimen_date=False,
  ),
  
  # positive sympomatic covid test
  # FIXME: note this will not pick up a symptomatic test that occurred before a subsequent non-symptomatic test within the same period
  # because we can only "return" symptomatic status, not filter on it
  # postest_symptomatic_14=patients.satisfying(
  #   """
  #   postest_14 AND symptom_14="Y"
  #   """,
  # 
  #   symptom_14 = patients.with_test_result_in_sgss(
  #     pathogen="SARS-CoV-2",
  #     test_result="positive",
  #     returning="symptomatic",
  #     between=["index_date - 13 days", "index_date"],
  #     find_first_match_in_period=True,
  #     restrict_to_earliest_specimen_date=False,
  #   ),
  # ),
  
  # Positive case identification
  primary_care_covid_case_14=patients.with_these_clinical_events(
    combine_codelists( # FIXME - ask Colm about new codelists
      codelists.covid_primary_care_code,
      codelists.covid_primary_care_positive_test,
      codelists.covid_primary_care_sequelae,
    ),
    returning="binary_flag",
    date_format="YYYY-MM-DD",
    between=["index_date - 13 days", "index_date"],
    find_first_match_in_period=True,
  ),
  
  # emergency attendance for covid
  covidemergency_14=patients.attended_emergency_care(
    returning="binary_flag",
    date_format="YYYY-MM-DD",
    between=["index_date - 13 days", "index_date"],
    with_these_diagnoses = codelists.covid_emergency,
    find_first_match_in_period=True,
  ),

  # covid admission
  covidadmitted_14=patients.admitted_to_hospital(
    returning="binary_flag",
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_these_diagnoses=codelists.covid_icd10,
    between=["index_date - 13 days", "index_date"],
    find_first_match_in_period=True,
  ),
  
  any_infection_or_disease_14=patients.satisfying(
    """
    postest_14
    OR primary_care_covid_case_14
    OR covidemergency_14
    OR covidadmitted_14
    """
  ),

  ############################################################
  ## ever-day events
  ## "Did any event occur any time up to and including this day?"
  ############################################################

  # positive covid test
  postest_ever=patients.with_test_result_in_sgss(
      pathogen="SARS-CoV-2",
      test_result="positive",
      returning="binary_flag",
      on_or_before="index_date",
      find_first_match_in_period=True,
      restrict_to_earliest_specimen_date=False,
  ),
  
  # positive symptomatic covid test
  # FIXME: note this will not pick up a symptomatic test that occurred before a subsequent non-symptomatic test within the same period
  # because we can only "return" symptomatic status, not filter on it
  # postest_symptomatic_ever=patients.satisfying(
  #   """
  #   postest_ever AND symptom_ever="Y"
  #   """,
  #   symptom_ever = patients.with_test_result_in_sgss(
  #     pathogen="SARS-CoV-2",
  #     test_result="positive",
  #     returning="symptomatic",
  #     on_or_before="index_date",
  #     find_first_match_in_period=True,
  #     restrict_to_earliest_specimen_date=False,
  #   ),
  # ),
  # 
  
  # positive case identification
  primary_care_covid_case_ever=patients.with_these_clinical_events(
    combine_codelists( # FIXME - ask Colm about new codelists
      codelists.covid_primary_care_code,
      codelists.covid_primary_care_positive_test,
      codelists.covid_primary_care_sequelae,
    ),
    returning="binary_flag",
    on_or_before="index_date",
    find_first_match_in_period=True,
  ),
  
  # emergency attendance for covid
  covidemergency_ever=patients.attended_emergency_care(
    returning="binary_flag",
    on_or_before="index_date",
    with_these_diagnoses = codelists.covid_emergency,
    find_first_match_in_period=True,
  ),
  
  # covid admission
  covidadmitted_ever=patients.admitted_to_hospital(
    returning="binary_flag",
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_these_diagnoses=codelists.covid_icd10,
    on_or_before="index_date",
    find_first_match_in_period=True,
  ),
  
  # 
  any_infection_or_disease_ever=patients.satisfying(
    """
    postest_ever
    OR primary_care_covid_case_ever
    OR covidemergency_ever
    OR covidadmitted_ever
    """
  )
  
)

measures = [

    ## single day events
    Measure(
      id="postest_01",
      numerator="postest_01",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    ),
    # Measure(
    #   id="postest_symptomatic_01",
    #   numerator="postest_symptomatic_01",
    #   denominator="population",
    #   group_by=["sex", "ageband5year", "region"],
    #   small_number_suppression=False
    # ),
    Measure(
      id="primary_care_covid_case_01",
      numerator="primary_care_covid_case_01",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    ),
    Measure(
      id="covidemergency_01",
      numerator="covidemergency_01",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    ),
    Measure(
      id="covidadmitted_01",
      numerator="covidadmitted_01",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    ),
    Measure(
      id="any_infection_or_disease_01",
      numerator="any_infection_or_disease_01",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    ),

    ## 14-day events
    Measure(
      id="postest_14",
      numerator="postest_14",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    ),
    # Measure(
    #   id="postest_symptomatic_14",
    #   numerator="postest_symptomatic_14",
    #   denominator="population",
    #   group_by=["sex", "ageband5year", "region"],
    #   small_number_suppression=False
    # ),
    Measure(
      id="primary_care_covid_case_14",
      numerator="primary_care_covid_case_14",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    ),
    Measure(
      id="covidemergency_14",
      numerator="covidemergency_14",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    ),
    Measure(
      id="covidadmitted_14",
      numerator="covidadmitted_14",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    ),
    Measure(
      id="any_infection_or_disease_14",
      numerator="any_infection_or_disease_14",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    ),

    ## ever events
    Measure(
      id="postest_ever",
      numerator="postest_ever",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    ),
    # Measure(
    #   id="postest_symptomatic_ever",
    #   numerator="postest_symptomatic_ever",
    #   denominator="population",
    #   group_by=["sex", "ageband5year", "region"],
    #   small_number_suppression=False
    # ),
    Measure(
      id="primary_care_covid_case_ever",
      numerator="primary_care_covid_case_ever",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    ),
    Measure(
      id="covidemergency_ever",
      numerator="covidemergency_ever",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    ),
    Measure(
      id="covidadmitted_ever",
      numerator="covidadmitted_ever",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    ),
    Measure(
      id="any_infection_or_disease_ever",
      numerator="any_infection_or_disease_ever",
      denominator="population",
      group_by=["sex", "ageband5year", "region"],
      small_number_suppression=False
    )

]

