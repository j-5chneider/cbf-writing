* Encoding: UTF-8.

VARSTOCASES
  /MAKE rater1 FROM FB_representation FB_level FB_specificity FB_tool_numbers manuscript_type 
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
