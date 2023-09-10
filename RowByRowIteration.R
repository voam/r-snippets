# Load required libraries
#install.packages('tinytex')
#tinytex::install_tinytex()
library(tibble)
library(rmarkdown)
library(purrr)
library(dplyr)
library(tinytex)
library(tidyr)
# Create a tibble with sample data representing people

df.employees <- tribble (
    ~id,    ~first_name, ~last_name,  ~dob,        ~gender, ~title,
    '101A', 'Bob',      'Francis',   '1983-06-12', 'M',  'Director',
    '102C', 'Susan',    'Bluebell',  '1990-04-21', 'F',  'Assistent Director',
    '201C', 'Emily',    'Rosen',     '1971-11-07', 'F',  'CTO',
    '301X', 'Ashley',   'Emerson',   '2001-01-25', 'F',  'CFO'
)
people <- tribble(
    ~Name,  ~Age, ~City,
    "John",  30,  "New York",
    "Alice", 25,  "Los Angeles",
    "Bob",   40,  "Chicago",
    "Sam",   22,  "Paris"
)

# Create a directory for the output files
dir.create("output", showWarnings = FALSE)

outputReport <- function(tb, key) {
    
#    library(tidyr)
    print(tb$Name)
    print(tb$Age)
    output_file <- paste0("output/", tb$Name, ".pdf")
    rmarkdown::render(
        input = "person_template.Rmd",  # R Markdown template file
        output_format = "pdf_document", # Output format
        output_file = output_file,      # Output file name
        params = list(row = tb)
    )
}

# rowwise creates on group per row
# group_walk processes each group. param 1 (.x) is tibble in group, param 2 (.y) is group key
people %>%
    rowwise() %>%
    group_walk(outputReport)

people %>%
    rowwise() %>%
    group_walk(~ outputReport (.x , .y))

f2 <- function(df, idx) {
    df
    print(idx)
    idx 
}
 
process<- function(idx, data) {
   print( idx )
   print( data )
    data
}

people %>%
    mutate(idx = row_number()) %>%
    group_by(idx) %>%
    nest() %>%
    pwalk(process)

# using an anonymous function
people %>%
    mutate(idx = row_number()) %>%
    group_by(idx) %>%
    nest() %>%
    pwalk(\(idx, data) {
#        print(idx)
        print(data)
    })

#anonymous function example
people %>%
   select(Name) %>%
   pwalk(\(Name) {
      print(paste0(Name))
   }) 

# using an anonymous function
z<- people %>%
    rowwise() %>%
    group_map(\(x,y){
#        as.list(x)
        list(data=x)
    })
z %>%
    map(\(x) {
        x$data %>%
            select(Name, Age) 
    })

processRow2 <- function(dr) {
    print('A')
    str(dr)
    if (dr$Age < 30) {
        return (T)
    }
    return (F)
}
people %>%
    mutate(idx = row_number()) %>%
    group_by(idx) %>%
    
    summarize(z = pick(everything())) %>%
    mutate(z2 = )
    select(everything())
    
    names <- colnames(people)

    people %>%
        rowwise() %>%
#        mutate(row = pick(colnames(.))) %>%
        mutate(row = nest_by(Name))
        mutate(Under30 = processRow2(row))  %>%
        select(c(names, 'Under30'))
    
# Original
# this isn't a bad way to call a user-defined function on each row
        
        people %>% 
            rowwise() %>% 
            do(row = as_tibble(.)) %>%
            mutate(Under30 = processRow2(row)) %>% 
            unnest(cols = c(row)) 
            select(`EmplID (3)`, new_col)
            
# I think this is the easiest
    people %>%
        mutate(idx = row_number()) %>%
        nest_by(idx) %>%
        mutate(z = processRow2(data)) %>%
        unnest(-idx)
    
    people %>%
        rowwise() %>%
        nest()

    people %>%
        map(length)
    
       people %>%
        map(function(x) { x })
       
       people %>%
        map(\(x) length(x) )      
       
        people %>%
            rowwise(.) %>%
            map(\(x) {
                colnames(x)
                str(x)
                
                } )      
        
      people %>%
        ~ colnames (.x)

updateRecord<- function(row) {
   print(row) 
}
df.employees |>
    map(updateRecord)

df.employees |>
    purrr::map(function(x) {
        x
#       typeof(x)
    })
df.employees |>
    purrr::pmap(function(...) {
        tb <- tibble(...)
        tb$IsFemale = tb$gender == 'F'
        tb
#       typeof(x)
    }) %>%
    bind_rows()

df.employees |> 
#   purrr::pmap( .f = function( id, first_name, last_name, dob, gender, title) {
    purrr::pmap(.l = _,  .f = function(...) {
        
        tb <- tibble(...)
       #paste(...)
#        1
    })
df.result <- df.employees |> 
#   purrr::pmap( .f = function( id, first_name, last_name, dob, gender, title) {
    purrr::pmap_int(.l = _,  .f = function(...) {
#       paste(...)
        1
    })
df.result 
typeof(df.result)
glimpse(df.result)

x <- 0
x<-df.employees |>
    purrr::pmap(.l = _, function(...) {
      tb_row<- tibble(...)
#       print(tb_row)
       tb_row$first_name
    })
x
# actually this may be my new favorite
df.employees %>% 
    split(1:nrow(.)) %>%
    map_dfr(\(row) {
     #print('hi')
     #   str(x)
     print(row$first_name)
     row$result = T
     row
    }) %>%
    select(everything())
    

      people %>% 
          select(Name) %>%
          pull() %>% 
          map(\(x) {
              str(x)
              x
          })     
      
      typeof(df.employees)
      str(df.employees)
      
      