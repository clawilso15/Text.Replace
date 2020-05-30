regex_replacements <- function(.data,
                               .subset = Source == 'Field Survey' & Question == 1)

  e <- substitute(.subset)
  r <- eval(e, obj, parent.frame())
# Extract comments for question 1 of the Field Survey as object `q1_DATA`
  q1_DATA1 = subset(.data, subset = r)

# Write a function to make a global substitution for a regular expression
  
# simplify the phrases before success
  out1 = text.replace("[Ii](\\s+would|'d)?(person(n)?ally)?\\s+define\\s+(su(c|cc)ess|it)?",
                      "I define success",
                      q1_DATA1,
                      column_name = 'Comments',
                      ngrams = 1)

# Fix work unit names
  out2 = text.replace("(work(/)?(\\s*)?(unit|place|area|organization)|org\\s+)",
                      "organization",
                      out1$data,
                      column_name = 'Comments',
                      ngrams = 1)

# Fix organization phrases
  out3 = text.replace("([Ff]or|([Ww]ith)?[Ii]n) (my|(y)?our|a(n)?) (unit(s)?|organization(s)? (is|as)?)",
                      "is ",
                      out2$data,
                      column_name = 'Comments',
                      ngrams = 1)

# Fix success post phrases
  out4 = text.replace("suc(c)?ess ((to|for) me (is)?|means)",
                      "is ",
                      out3$data,
                      column_name = 'Comments',
                      ngrams = 1)

# Fix warfighter phrases
  out5 = text.replace("[Ww]ar(-|\\s*)fighter",
                      "customer",
                      out4$data,
                      column_name = 'Comments',
                      ngrams = 1)


  }
