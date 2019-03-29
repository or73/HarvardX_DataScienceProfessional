# Web Scrapping
# Web Scrapping or Web Harvesting are the terms used to describe the process of extracting data
#    from a website.

# rvest
# Import the web page
library( rvest )
url <- 'https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state'
h <- read_html( url )

# Extract the table
tab <- h %>% html_nodes( 'table' )
tab
tab <- tab[[ 2 ]]
tab

# Convert HTML tables into data frames
tab <- tab %>% html_table
class( tab )
tab

# Change the names of the columns which are a little bit long
tab <- tab %>% setNames( c( 'state', 'population', 'total', 'murders', 'gun_murders', 'gun_ownership', 'total_rate', 'murder_rate', 'gun_murder_rate' ) )
head( tab )



# Question 1
# Which feature of html documents allows us to extract the table that we are interested in?
#     Select the one true statement.
#
# Html is easily converted to to xml, which can then be used for extracting tables.
# All elements in an html page are specified as “nodes”; we can use the node “tables” to identify and extract the specific table we are interested in before we do additional data cleaning.    <-*
# All tables in html documents are stored in separate files that you can download via the html code.
# Tables in html are formatted as csv tables, which we can easily copy and process in R.




# Question 2
# 1 point possible (graded)
# In the video, we use the following code to extract the murders table (tab) from our downloaded html file h:
#
# tab <- h %>% html_nodes(“table”)
# tab <- tab[[2]] %>%
#     html_table
# Wikipedia page for help.
# Why did we use the html_nodes() command instead of the html_node command?
# The html_node command only selects the first node of a specified type. In this
#    example the first “table” node is a legend table and not the actual data we
#    are interested in.   <-*
# The html_nodes command allows us to specify what type of node we want to extract,
#    while the html_node command does not.
# It does not matter; the two commands are interchangeable.
# We used html_nodes so that we could specify the second “table” element using
#    the tab[[2]] command.   <-*





# CSS Selectors
# The default look of webpage made with the most basic HTML is quite unattractive. The aesthetically pleasing pages we see today are made using CSS. CSS is used to add style to webpages. The fact that all pages for a company have the same style is usually a result that they all use the same CSS file. The general way these CSS files work is by defining how each of the elements of a webpage will look. The title, headings, itemized lists, tables, and links, for example, each receive their own style including font, color, size, and distance from the margin, among others.
#
# To do this CSS leverages patterns used to define these elements, referred to as selectors. An example of pattern we used in a previous video is table but there are many many more. If we want to grab data from a webpage and we happen to know a selector that is unique to the part of the page, we can use the html_nodes function.
#
# However, knowing which selector to use can be quite complicated. To demonstrate this we will try to extract the recipe name, total preparation time, and list of ingredients from this guacamole recipe. Looking at the code for this page, it seems that the task is impossibly complex. However, selector gadgets actually make this possible. SelectorGadget is piece of software that allows you to interactively determine what CSS selector you need to extract specific components from the webpage. If you plan on scraping data other than tables, we highly recommend you install it. A Chrome extension is available which permits you to turn on the gadget highlighting parts of the page as you click through, showing the necessary selector to extract those segments.
#
# For the guacamole recipe page we already have done this and determined that we need the following selectors:
#
#     h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
# recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
# prep_time <- h %>% html_node(".o-RecipeInfo__a-Description--Total") %>% html_text()
# ingredients <- h %>% html_nodes(".o-Ingredients__a-ListItemText") %>% html_text()
#
# You can see how complex the selectors are. In any case we are now ready to extract what we want and create a list:
#
#     guacamole <- list(recipe, prep_time, ingredients)
# guacamole
#
# Since recipe pages from this website follow this general layout, we can use this code to create a function that extracts this information:
#
#     get_recipe <- function(url){
#         h <- read_html(url)
#         recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
#         prep_time <- h %>% html_node(".o-RecipeInfo__a-Description--Total") %>% html_text()
#         ingredients <- h %>% html_nodes(".o-Ingredients__a-ListItemText") %>% html_text()
#         return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
#     }
#
# and then use it on any of their webpages:
#
#     get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")
#
# There are several other powerful tools provided by rvest. For example, the functions html_form, set_values, and submit_form permit you to query a webpage from R. This is a more advanced topic not covered here.
