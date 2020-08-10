# convert docx into Rmd
# see: https://gist.github.com/petzi53/8120df519b86578fd7c83e4d74db273f
cd text/ecography
pandoc -f docx -t markdown+auto_identifiers -s round_1/enso_comp.docx -o round_2/enso_comp_test.Rmd --atx-headers --wrap=none --extract-media=""
pandoc -f docx -t markdown+auto_identifiers -s round_1/title_page.docx -o round_2/title_page_test.Rmd --atx-headers --wrap=none --extract-media=""
pandoc -f docx -t markdown+auto_identifiers -s round_1/supplementary_information.docx -o round_2/supplementary_information_test.Rmd --atx-headers --wrap=none --extract-media=""
