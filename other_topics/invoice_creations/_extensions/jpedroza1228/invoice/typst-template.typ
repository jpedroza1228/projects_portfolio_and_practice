#let invoice(
  company: none,
  name: none,
  specific_name: none,
  address: none,
  issue_date: none,
  start_of_work: none,
  end_of_work: none,
  summary: none,
  amount: none,
  //doc
) = {
  set page(
    margin: (
      top: 1in,
      bottom: 1in,
      left: .5in,
      right: .5in
    )
  )

  set text(
    font: "Source Sans Pro",
    size: 12pt
  )

  align(center)[
    #set text(
      size: 16pt,
      weight: "bold"
    )
    #company Invoice - #specific_name
  ]

  line(
    length: 100%,
    stroke: black
  )

  grid(
    columns: 1,
    [
      *Name*: #name
      #linebreak()
      *Address*: #address
      #linebreak()
      *Issue Date*: #issue_date
      #linebreak()
      *Dates of Work*: #start_of_work - #end_of_work
      #linebreak()
      *Summary of Work Completed*: #summary
      #linebreak()
      *Amount*: #amount
    ]
  )
}