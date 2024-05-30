* Encoding: UTF-8.
DATASET ACTIVATE DataSet1.

VARSTOCASES
  /MAKE rater1 FROM FB_representation 
  /KEEP=author 
  /NULL=DROP.

VARSTOCASES
  /MAKE rater1 FROM FB_level 
  /KEEP=author 
  /NULL=DROP.

VARSTOCASES
  /MAKE rater1 FROM FB_specificity 
  /KEEP=author 
  /NULL=DROP.

VARSTOCASES
  /MAKE rater1 FROM FB_tool_numbers manuscript_type 
    education setting country genre
  /KEEP=author 
  /NULL=DROP.

VARSTOCASES
  /MAKE rater2 FROM FB_representation FB_level FB_specificity FB_tool_numbers manuscript_type 
    education setting country genre
  /KEEP=author 
  /NULL=DROP.

VARSTOCASES
  /MAKE rater3 FROM FB_representation FB_order FB_specificity FB_tool_numbers manuscript_type 
    education setting country genre
  /KEEP=author 
  /NULL=DROP.

VARSTOCASES
  /MAKE rater3 FROM FB_representation FB_order FB_specificity FB_tool_numbers manuscript_type 
    education setting country genre
  /KEEP=author 
  /NULL=DROP.
