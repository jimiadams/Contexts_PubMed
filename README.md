# Contexts_PubMed
This is the public repo for our article on COVID-19 Science Mobilization in Contexts

This project is pulling data from PubMed to construct collaboration networks from COVID-19 publications in Dec 2019 - Jun 2020. This work is in collaboration with Ryan Light & Nicholas Theis. 

An outline of the workflow here based on the scripts located in the "script" folder:

NOTE: To comply with PubMed data use agreements, this repo does not include our raw, nor compiled datasets. We've only posted the final data objects used in the figures, as noted in italics below.

1. 1_pubmed_covid_contexts.R downloads articles from PubMed.
2. 2_name_disambiguation_contexts.R disambiguates names using place matches among other factors.
3. 3_coauthors_contexts.R builds a coauthorship network.
3a. 3a_community_countries.R connects coauthorship communities to geographic locations.
3b. 3b_community_keywords.R connects coauthorship communities to article keywords.
4. 4_make_art_map makes the map of COVID-19 papers.
5. 5_stm_contexts.R builds a structural topic model of the COVID-19 corpus.
6. 6_make_line_plot.R makes a line plot of the number of papers over time.

Note: The disparity_filter_function.R script is used in step 3 to locate the "backbone" of the network. 
