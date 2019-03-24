# U.S. Bilateral Trade in Goods, by three-digit NAICS code
Pull and see year-on-year trends in Census U.S. bilateral trade in goods data by three-digit NAICS code

This program is designed as an example of how to download and format trade in goods data by three-digit NAICS code from the U.S. Census API. It allows users flexibility to specify the country and time period of interest. 

U.S. Federal statistical agencies use the North American Industry Classification System (NAICS) as a standard industry classification system. NAICS codes take a hierarchical structure: each additional digit in a NAICS code signifies a deeper level of specificity within a given sector of the economy. Two-digit NAICS codes divide the economy into sectors, three-digit NAICS codes further divide those sectors into subsectors, and four-digit NAICS codes divide the subsectors into more specific industrial groups. For a fuller definition, see the U.S. Census description of NAICS codes here: https://www.census.gov/programs-surveys/economic-census/guidance/understanding-naics.html. 

To access the U.S. Census API, first request an API key online:
http://api.census.gov/data/key_signup.html

Under "Paths and Parameters", specify the following parameters in your program: path, key, initial and final dates to pull, the country code of interest (ctry_code), and import and export variables (import_vars, export_vars).  

Note: Many fields are available for download. The program pulls "general value month" (monthly total value of general imports) and "general value year" (year-to-date total value of general imports) U.S. import fields and "all value month" (monthly total value of exports) and "all value year" (year-to-date total value of exports) U.S. export fields. Fields can be modified if desired.

For help or to learn more about international trade data available from the U.S. Census API, please see the Census guide:
https://www.census.gov/foreign-trade/reference/guides/Guide%20to%20International%20Trade%20Datasets.pdf

Import and export functions were created using a template provided by Yun Tai, CLIR Postdoctoral Fellow at the University of Virginia Library: https://data.library.virginia.edu/using-census-data-api-with-r/.  
