// Typst custom formats typically consist of a 'typst-template.typ' (which is
// the source code for a typst template) and a 'typst-show.typ' which calls the
// template's function (forwarding Pandoc metadata values as required)
//
// This is an example 'typst-show.typ' file (based on the default template  
// that ships with Quarto). It calls the typst function named 'article' which 
// is defined in the 'typst-template.typ' file. 
//
// If you are creating or packaging a custom typst template you will likely
// want to replace this file and 'typst-template.typ' entirely. You can find
// documentation on creating typst templates here and some examples here:
//   - https://typst.app/docs/tutorial/making-a-template/
//   - https://github.com/typst/templates

#show: doc => invoice(
  $if(company)$
    company: [$company$],
  $endif$
  $if(name)$
    name: [$name$],
  $endif$
  $if(specific_name)$
    specific_name: [$specific_name$],
  $endif$
  $if(address)$
    address: [$address$],
  $endif$
  $if(issue_date)$
    issue_date: [$issue_date$],
  $endif$
  $if(start_of_work)$
    start_of_work: [$start_of_work$],
  $endif$
  $if(end_of_work)$
    end_of_work: [$end_of_work$],
  $endif$
  $if(summary)$
    summary: [$summary$],
  $endif$
  $if(amount)$
    amount: [$amount$],
  $endif$
  //doc,
)